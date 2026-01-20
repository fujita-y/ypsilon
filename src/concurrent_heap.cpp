// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "concurrent_heap.h"
#include "concurrent_pool.h"
#include "concurrent_slab.h"

#define DEBUG_CONCURRENT_COLLECT 0
#define ENSURE_REALTIME          (5.0)  // in msec (1.0 == 0.001sec)
#define TIMEOUT_CHECK_EACH       (500)

#if GCDEBUG
  #define GCTRACE(fmt) \
    do {               \
      printf(fmt);     \
      fflush(stdout);  \
    } while (0)
#else
  #define GCTRACE(fmt) ((void)0)
#endif

concurrent_heap_t::concurrent_heap_t() {
  m_mark_stack_size = MARK_STACK_SIZE_INIT;
  m_mark_stack = m_mark_sp = (void**)malloc(sizeof(void*) * m_mark_stack_size);
  assert(m_mark_stack);
  m_shade_queue.init(SHADE_QUEUE_SIZE);
  m_collector_lock.init();
  m_mutator_wake.init();
  m_collector_wake.init();
  m_write_barrier = false;
  m_read_barrier = false;
  m_alloc_barrier = false;
  m_collector_kicked = false;
  m_collector_ready = false;
  m_collector_terminating = false;
  m_stop_the_world = false;
  m_mutator_stopped = false;
}

void concurrent_heap_t::init(concurrent_pool_t* pool) {
  assert(pool);
  m_concurrent_pool = pool;
  m_sweep_wavefront = (uint8_t*)m_concurrent_pool->m_pool + m_concurrent_pool->m_pool_size;
  assert(m_sweep_wavefront);
  MTVERIFY(pthread_create(&m_collector_thread, NULL, (void* (*)(void*))collector_thread, this));
}

void* concurrent_heap_t::allocate(size_t size, bool slab, bool gc) { return m_concurrent_pool->allocate(size, slab, gc); }

void concurrent_heap_t::deallocate(void* p) { m_concurrent_pool->deallocate(p); }

void concurrent_heap_t::terminate() {
  GCTRACE(";; [collector: terminating]\n");
  m_collector_lock.lock();
  m_collector_terminating = true;
  m_collector_wake.signal();
  m_collector_lock.unlock();
  MTVERIFY(pthread_join(m_collector_thread, NULL));
  GCTRACE(";; [collector: terminated]\n");
}

void concurrent_heap_t::collect() {
  if (m_collector_kicked == false) {
    m_collector_lock.lock();
    if (m_collector_kicked == false && m_collector_ready) {
      m_collector_kicked = true;
      m_collector_wake.signal();
      GCTRACE(";; [collector: running]\n");
    }
    m_collector_lock.unlock();
  }
}

void concurrent_heap_t::synchronized_collect() {
  clear_trip_bytes();
  snapshot_root();

  // mark
  assert(m_mutator_stopped == false);
  m_root_snapshot_mode = ROOT_SNAPSHOT_MODE_EVERYTHING;
  m_stop_the_world = true;
  GCTRACE(";; [collector: stop-the-world phase 1]\n");
  while (!m_mutator_stopped) {
    if (m_collector_terminating) return;
    m_collector_wake.wait(m_collector_lock);
    if (!m_mutator_stopped) {
      dequeue_root();
      m_mutator_wake.signal();
    }
  }
  double t1 = msec();
  GCTRACE(";; [collector: mark]\n");
  dequeue_root();
  while (synchronized_mark()) continue;

  // sweep
  GCTRACE(";; [collector: sweep]\n");
  m_sweep_wavefront = (uint8_t*)m_concurrent_pool->m_pool;

  update_weak_reference();
  m_read_barrier = false;

  slab_traits_t* traits = SLAB_TRAITS_OF(m_concurrent_pool->m_pool);
  for (int i = 0; i < m_concurrent_pool->m_pool_watermark; i++) {
    if (GCSLABP(m_concurrent_pool->m_pool[i])) {
      uint8_t* slab = m_concurrent_pool->m_pool + ((intptr_t)i << SLAB_SIZE_SHIFT);
      traits->cache->sweep(slab);
    }
    traits = (slab_traits_t*)((intptr_t)traits + SLAB_SIZE);
  }

  GCTRACE(";; [collector: start-the-world]\n");
  m_stop_the_world = false;
  m_sweep_wavefront = (uint8_t*)m_concurrent_pool->m_pool + m_concurrent_pool->m_pool_size;
  m_mutator_wake.signal();
  while (m_mutator_stopped) {
    if (m_collector_terminating) return;
    m_collector_wake.wait(m_collector_lock);
  }

  // end
  m_collector_kicked = false;
  GCTRACE(";; [collector: waiting]\n");
  double t3 = msec();

  m_usage.m_duration = t3 - t1;
  m_usage.m_sync1 = 0;
  m_usage.m_sync2 = 0;
  m_usage.m_recorded = true;
  m_usage.m_synchronized = true;
}

void concurrent_heap_t::concurrent_collect() {
  assert(m_mutator_stopped == false);

  // mark phase 1
  m_root_snapshot_mode = ROOT_SNAPSHOT_MODE_GLOBALS;
  m_stop_the_world = true;
  GCTRACE(";; [collector: stop-the-world]\n");
  while (!m_mutator_stopped) {
    if (m_collector_terminating) return;
    m_collector_wake.wait(m_collector_lock);
    if (!m_mutator_stopped) {
      dequeue_root();
      m_mutator_wake.signal();
    }
  }
  double t1 = msec();
  clear_trip_bytes();
  m_write_barrier = true;
  m_stop_the_world = false;
  m_mutator_wake.signal();
  GCTRACE(";; [collector: start-the-world phase 1]\n");
  while (m_mutator_stopped) {
    if (m_collector_terminating) return;
    m_collector_wake.wait(m_collector_lock);
  }
  double t2 = msec();
  GCTRACE(";; [collector: concurrent-mark phase 1]\n");

  concurrent_mark();

  // mark phase 1+
  snapshot_root();
  concurrent_mark();

  // mark phase 2
  m_root_snapshot_mode = ROOT_SNAPSHOT_MODE_LOCALS;
  m_stop_the_world = true;
  GCTRACE(";; [collector: stop-the-world phase 2]\n");
  while (!m_mutator_stopped) {
    if (m_collector_terminating) return;
    m_collector_wake.wait(m_collector_lock);
    if (!m_mutator_stopped) {
      dequeue_root();
      m_mutator_wake.signal();
    }
  }

  m_root_snapshot_mode = ROOT_SNAPSHOT_MODE_EVERYTHING;
fallback:
  m_stop_the_world = false;
  m_mutator_wake.signal();
  GCTRACE(";; [collector: start-the-world phase 2]\n");
  while (m_mutator_stopped) {
    if (m_collector_terminating) return;
    m_collector_wake.wait(m_collector_lock);
  }
  GCTRACE(";; [collector: concurrent-mark phase 2]\n");

  concurrent_mark();

#if DEBUG_CONCURRENT_COLLECT
  double t3 = msec();
#endif

  // final mark
  assert(m_mutator_stopped == false);
  m_stop_the_world = true;
  GCTRACE(";; [collector: stop-the-world final]\n");

  while (!m_mutator_stopped) {
    if (m_collector_terminating) return;
    m_collector_wake.wait(m_collector_lock);
    if (!m_mutator_stopped) {
      dequeue_root();
      m_mutator_wake.signal();
    }
  }
  double t4 = msec();
  m_write_barrier = false;
  GCTRACE(";; [collector: synchronized-mark]\n");
  dequeue_root();

#ifdef ENSURE_REALTIME
  if (synchronized_mark()) {
  #if DEBUG_CONCURRENT_COLLECT
    puts("synchronized_mark() timeout, resume mutator and restart concurrent_mark");
  #endif
    m_write_barrier = true;
    m_root_snapshot_mode = ROOT_SNAPSHOT_MODE_RETRY;
    goto fallback;
  }
#else
  while (synchronized_mark()) continue;

#endif

  // sweep
  m_sweep_wavefront = (uint8_t*)m_concurrent_pool->m_pool;
  m_alloc_barrier = true;
  m_read_barrier = true;
  m_stop_the_world = false;
  m_mutator_wake.signal();
  while (m_mutator_stopped) {
    if (m_collector_terminating) return;
    m_collector_wake.wait(m_collector_lock);  // to make mutator run now
  }
  GCTRACE(";; [collector: start-the-world]\n");
  GCTRACE(";; [collector: concurrent-sweep]\n");
  double t5 = msec();

  update_weak_reference();
  m_read_barrier = false;

  int capacity = (m_concurrent_pool->m_pool_size >> SLAB_SIZE_SHIFT);
  uint8_t* slab = m_concurrent_pool->m_pool;
  int i = 0;
  while (i < capacity) {
    int memo = m_concurrent_pool->m_pool_usage;
    if (GCSLABP(m_concurrent_pool->m_pool[i])) {
      if (SLAB_TRAITS_OF(slab)->cache == NULL) {
        GCTRACE(";; [collector: wait for mutator complete slab init]\n");
        sched_yield();
        continue;
      }
      debug_check_slab(slab);
      SLAB_TRAITS_OF(slab)->cache->sweep(slab);
      slab += SLAB_SIZE;
      i++;
    } else {
      scoped_lock lock(m_concurrent_pool->m_lock);
      if (memo != m_concurrent_pool->m_pool_usage) continue;
      do {
        if (i == m_concurrent_pool->m_pool_watermark) {
          m_sweep_wavefront = (uint8_t*)m_concurrent_pool->m_pool + m_concurrent_pool->m_pool_size;
          m_alloc_barrier = false;
          goto finish;
        }
        slab += SLAB_SIZE;
        m_sweep_wavefront = slab;
        i++;
      } while (!GCSLABP(m_concurrent_pool->m_pool[i]));
    }
  }

finish:
  m_collector_kicked = false;
  GCTRACE(";; [collector: waiting]\n");
  double t6 = msec();
  m_usage.m_duration = t6 - t1;
  m_usage.m_sync1 = t2 - t1;
  m_usage.m_sync2 = t5 - t4;
  m_usage.m_recorded = true;
  m_usage.m_synchronized = false;
#if DEBUG_CONCURRENT_COLLECT
  printf(
      ";; [        first-lock:%.2fms second-lock:%.2fms overlap:%.2fms]\n"
      ";; [        stw:%.2fms concurrent-mark:%.2fms]\n"
      ";; [        stw:%.2fms synchronized-mark:%.2fms]\n"
      ";; [        concurrent-sweep:%.2fms]\n",
      (t2 - t1), (t4 - t3) + (t5 - t4), (t3 - t2) + (t6 - t5), t2 - t1, t3 - t2, t4 - t3, t5 - t4, t6 - t5);
  fflush(stdout);
#endif
  debug_post_completation();
}

void* concurrent_heap_t::collector_thread(void* param) {
  concurrent_heap_t& concurrent_heap = *(concurrent_heap_t*)param;
  concurrent_heap.m_collector_lock.lock();
  concurrent_heap.m_collector_ready = true;
  GCTRACE(";; [collector: ready]\n");
  while (!concurrent_heap.m_collector_terminating) {
    if (concurrent_heap.m_collector_kicked == false) {
      concurrent_heap.m_collector_wake.wait(concurrent_heap.m_collector_lock);
      continue;
    }
    assert(concurrent_heap.m_mark_sp == concurrent_heap.m_mark_stack);
    if (concurrent_heap.m_mark_stack_size != MARK_STACK_SIZE_INIT) {
      concurrent_heap.m_mark_stack_size = MARK_STACK_SIZE_INIT;
      concurrent_heap.m_mark_stack = concurrent_heap.m_mark_sp =
          (void**)realloc(concurrent_heap.m_mark_stack, sizeof(void*) * concurrent_heap.m_mark_stack_size);
    }
    if (CONCURRENT_COLLECT) {
      if (concurrent_heap.m_concurrent_pool->m_pool_usage > concurrent_heap.m_concurrent_pool->m_pool_threshold) {
        concurrent_heap.synchronized_collect();
      } else {
        concurrent_heap.concurrent_collect();
      }
    } else {
      concurrent_heap.synchronized_collect();
    }
  }
  concurrent_heap.m_collector_terminating = false;
  concurrent_heap.m_collector_lock.unlock();
  return NULL;
}

void concurrent_heap_t::concurrent_mark() {
  m_collector_lock.unlock();
  do {
    while (true) {
      void* obj;
      if (m_shade_queue.try_get(&obj)) shade(obj);
      if (m_mark_sp == m_mark_stack) break;
      obj = *--m_mark_sp;
      trace(obj);
    }
  } while (m_shade_queue.count());
  m_collector_lock.lock();
}

bool concurrent_heap_t::synchronized_mark() {
#ifdef ENSURE_REALTIME
  double timeout = msec() + ENSURE_REALTIME;
  int i = 0;
  while (m_mark_sp != m_mark_stack) {
    void* obj = *--m_mark_sp;
    trace(obj);
    if (++i > TIMEOUT_CHECK_EACH) {
      i = 0;
      if (msec() > timeout) return true;
    }
  }
  return false;
#else
  while (m_mark_sp != m_mark_stack) {
    void* obj = *--m_mark_sp;
    trace(obj);
  }
  return false;
#endif
}

// Run on collector thread
void concurrent_heap_t::shade(void* obj) {
  if (SLAB_DATUM_BITS_TEST(obj)) {
    if (m_concurrent_pool->in_pool(obj)) {
      if (SLAB_TRAITS_OF(obj)->cache->state(obj) == false) {
        if (m_mark_sp < m_mark_stack + m_mark_stack_size) {
          *m_mark_sp++ = obj;
          return;
        }
        m_usage.m_expand_mark_stack++;
        int newsize = m_mark_stack_size + MARK_STACK_SIZE_GROW;
        m_mark_stack = (void**)realloc(m_mark_stack, sizeof(void*) * newsize);
        if (m_mark_stack == NULL) {
          fatal("%s:%u memory overflow on realloc mark stack", __FILE__, __LINE__);
        }
        m_mark_sp = m_mark_stack + m_mark_stack_size;
        m_mark_stack_size = newsize;
        *m_mark_sp++ = obj;
      }
    } else {
      fatal("%s:%u object not in pool %p", __FILE__, __LINE__, obj);
    }
  }
}

// Run on collector thread
void concurrent_heap_t::interior_shade(void* ref) {
  if (ref) {
    assert(m_concurrent_pool->is_collectible(ref));
    shade(SLAB_TRAITS_OF(ref)->cache->lookup(ref));
  }
}

// Run on collector thread
void concurrent_heap_t::dequeue_root() {
  void* obj;
  while (m_shade_queue.count()) {
    m_shade_queue.get(&obj);
    shade(obj);
  }
}

// Run on mutator thread
void concurrent_heap_t::enqueue_root(void* obj) {
  assert(m_stop_the_world);
  if (SLAB_DATUM_BITS_TEST(obj)) {
    if (m_concurrent_pool->in_pool(obj)) {
      while (m_shade_queue.wait_lock_try_put(obj) == false) {
        m_collector_lock.lock();
        m_collector_wake.signal();
        m_mutator_wake.wait(m_collector_lock);
        m_collector_lock.unlock();
        GCTRACE(";; [shade queue overflow while queueing root set]\n");
      }
    } else {
      fatal("%s:%u object not in pool %p", __FILE__, __LINE__, obj);
    }
  }
}

// Run on mutator thread
void concurrent_heap_t::write_barrier(void* rhs) {
  // simple (Dijkstra)
  if (m_write_barrier) {
    if (SLAB_DATUM_BITS_TEST(rhs)) {
      if (m_concurrent_pool->in_pool(rhs)) {
        if (SLAB_TRAITS_OF(rhs)->cache->state(rhs) == false) {
          while (m_shade_queue.wait_lock_try_put(rhs) == false) {
            if (SLAB_TRAITS_OF(rhs)->cache->state(rhs)) break;
            if (m_stop_the_world) {
              GCTRACE(";; [write-barrier: m_shade_queue overflow, during stop-the-world]\n");
              m_collector_lock.lock();
              m_collector_wake.signal();
              m_mutator_wake.wait(m_collector_lock);
              m_collector_lock.unlock();
            } else {
              GCTRACE(";; [write-barrier: m_shade_queue overflow, mutator sched_yield]\n");
              sched_yield();
            }
            m_usage.m_shade_queue_hazard++;
          }
          if (DETAILED_STATISTIC) m_usage.m_barriered_write++;
        }
      } else {
        fatal("%s:%u object not in pool %p", __FILE__, __LINE__, rhs);
      }
    }
  }
}