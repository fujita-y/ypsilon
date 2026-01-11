// Copyright (c) 2004-2022 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "concurrent_heap.h"
#include "object_heap.h"
#include "port.h"

#define DEBUG_CONCURRENT_COLLECT 0
#define ENSURE_REALTIME          (5.0)  // in msec (1.0 == 0.001sec)
#define TIMEOUT_CHECK_EACH       (500)

#if GCDEBUG
  #define GC_TRACE(fmt) \
    do {                \
      printf(fmt);      \
      fflush(stdout);   \
    } while (0)
#else
  #define GC_TRACE(fmt) ((void)0)
#endif

concurrent_heap_t::concurrent_heap_t(object_heap_t* heap) : m_heap(heap) {
  m_mark_stack_size = MARK_STACK_SIZE_INIT;
  m_mark_stack = m_mark_sp = (scm_obj_t*)malloc(sizeof(scm_obj_t) * m_mark_stack_size);
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

void concurrent_heap_t::init() {
  thread_start(collector_thread, this);
}

void concurrent_heap_t::terminate() {
  m_collector_lock.lock();
  m_collector_terminating = true;
  m_collector_wake.signal();
  m_collector_lock.unlock();
}

void concurrent_heap_t::collect() {
  if (m_collector_kicked == false) {
    m_collector_lock.lock();
    if (m_collector_kicked == false && m_collector_ready) {
      m_collector_kicked = true;
      m_collector_wake.signal();
      GC_TRACE(";; [collector: running]\n");
    }
    m_collector_lock.unlock();
  }
}

void concurrent_heap_t::synchronized_collect() {
  m_heap->m_trip_bytes = 0;
  m_heap->shade(m_heap->m_system_environment);
  m_heap->shade(m_heap->m_interaction_environment);
  m_heap->shade(m_heap->m_hidden_variables);
  m_heap->shade(m_heap->m_architecture_feature);
  m_heap->shade(m_heap->m_native_transcoder);
  m_heap->shade(m_heap->m_trampolines);
  for (int i = 0; i < INHERENT_TOTAL_COUNT; i++) m_heap->shade(m_heap->m_inherents[i]);

  // mark
  assert(m_mutator_stopped == false);
  m_root_snapshot = ROOT_SNAPSHOT_EVERYTHING;
  m_stop_the_world = true;
  GC_TRACE(";; [collector: stop-the-world phase 1]\n");
  while (!m_mutator_stopped) {
    m_collector_wake.wait(m_collector_lock);
    if (!m_mutator_stopped) {
      m_heap->dequeue_root();
      m_mutator_wake.signal();
    }
  }
  double t1 = msec();
  GC_TRACE(";; [collector: mark]\n");
  m_heap->dequeue_root();
  while (m_heap->serial_marking()) continue;

  // sweep
  GC_TRACE(";; [collector: sweep]\n");
  m_heap->m_sweep_wavefront = (uint8_t*)m_heap->m_pool;
  m_heap->m_symbol.sweep();
  m_heap->m_string.sweep();
  m_heap->m_weakmappings.m_lock.lock();
  if (m_heap->m_weakmappings.m_vacant) {
    object_slab_traits_t* traits = m_heap->m_weakmappings.m_vacant;
    do m_heap->break_weakmapping(traits);
    while ((traits = traits->next) != m_heap->m_weakmappings.m_vacant);
  }
  if (m_heap->m_weakmappings.m_occupied) {
    object_slab_traits_t* traits = m_heap->m_weakmappings.m_occupied;
    do m_heap->break_weakmapping(traits);
    while ((traits = traits->next) != m_heap->m_weakmappings.m_occupied);
  }
  m_heap->m_weakmappings.m_lock.unlock();
  object_slab_traits_t* traits = OBJECT_SLAB_TRAITS_OF(m_heap->m_pool);
  for (int i = 0; i < m_heap->m_pool_watermark; i++) {
    if (GCSLABP(m_heap->m_pool[i])) {
      uint8_t* slab = m_heap->m_pool + ((intptr_t)i << OBJECT_SLAB_SIZE_SHIFT);
      traits->cache->sweep(slab);
    }
    traits = (object_slab_traits_t*)((intptr_t)traits + OBJECT_SLAB_SIZE);
  }

  GC_TRACE(";; [collector: start-the-world]\n");
  m_stop_the_world = false;
  m_heap->m_sweep_wavefront = (uint8_t*)m_heap->m_pool + m_heap->m_pool_size;
  m_mutator_wake.signal();
  while (m_mutator_stopped) {
    m_collector_wake.wait(m_collector_lock);
  }

  // end
  m_collector_kicked = false;
  GC_TRACE(";; [collector: waiting]\n");
  double t3 = msec();

  m_heap->m_usage.m_duration = t3 - t1;
  m_heap->m_usage.m_sync1 = 0;
  m_heap->m_usage.m_sync2 = 0;
  m_heap->m_usage.m_recorded = true;
  m_heap->m_usage.m_synchronized = true;
}

void concurrent_heap_t::concurrent_collect() {
  assert(m_mutator_stopped == false);

  // mark phase 1
  m_root_snapshot = ROOT_SNAPSHOT_GLOBALS;
  m_stop_the_world = true;
  GC_TRACE(";; [collector: stop-the-world]\n");
  while (!m_mutator_stopped) {
    m_collector_wake.wait(m_collector_lock);
    if (!m_mutator_stopped) {
      m_heap->dequeue_root();
      m_mutator_wake.signal();
    }
  }
  double t1 = msec();
  m_heap->m_trip_bytes = 0;
  m_write_barrier = true;
  m_stop_the_world = false;
  m_mutator_wake.signal();
  GC_TRACE(";; [collector: start-the-world phase 1]\n");
  while (m_mutator_stopped) {
    m_collector_wake.wait(m_collector_lock);
  }
  double t2 = msec();
  GC_TRACE(";; [collector: concurrent-marking phase 1]\n");
  m_heap->concurrent_marking();

  // mark phase 1+
  m_heap->shade(m_heap->m_system_environment);
  m_heap->shade(m_heap->m_interaction_environment);
  m_heap->shade(m_heap->m_hidden_variables);
  m_heap->shade(m_heap->m_architecture_feature);
  m_heap->shade(m_heap->m_native_transcoder);
  m_heap->shade(m_heap->m_trampolines);

  for (int i = 0; i < INHERENT_TOTAL_COUNT; i++) m_heap->shade(m_heap->m_inherents[i]);
  m_heap->concurrent_marking();

  // mark phase 2
  m_root_snapshot = ROOT_SNAPSHOT_LOCALS;
  m_stop_the_world = true;
  GC_TRACE(";; [collector: stop-the-world phase 2]\n");
  while (!m_mutator_stopped) {
    m_collector_wake.wait(m_collector_lock);
    if (!m_mutator_stopped) {
      m_heap->dequeue_root();
      m_mutator_wake.signal();
    }
  }

  m_root_snapshot = ROOT_SNAPSHOT_EVERYTHING;
fallback:
  m_stop_the_world = false;
  m_mutator_wake.signal();
  GC_TRACE(";; [collector: start-the-world phase 2]\n");
  while (m_mutator_stopped) {
    m_collector_wake.wait(m_collector_lock);
  }
  GC_TRACE(";; [collector: concurrent-marking phase 2]\n");
  m_heap->concurrent_marking();
#if DEBUG_CONCURRENT_COLLECT
  double t3 = msec();
#endif

  // final mark
  assert(m_mutator_stopped == false);
  m_stop_the_world = true;
  GC_TRACE(";; [collector: stop-the-world final]\n");

  while (!m_mutator_stopped) {
    m_collector_wake.wait(m_collector_lock);
    if (!m_mutator_stopped) {
      m_heap->dequeue_root();
      m_mutator_wake.signal();
    }
  }
  double t4 = msec();
  m_write_barrier = false;
  GC_TRACE(";; [collector: serial-marking]\n");
  m_heap->dequeue_root();

#ifdef ENSURE_REALTIME
  if (m_heap->serial_marking()) {
  #if DEBUG_CONCURRENT_COLLECT
    puts("serial_marking() timeout, resume mutator and restart concurrent_marking");
  #endif
    m_write_barrier = true;
    m_root_snapshot = ROOT_SNAPSHOT_RETRY;
    goto fallback;
  }
#else
  while (m_heap->serial_marking()) continue;
#endif

  // sweep
  m_heap->m_sweep_wavefront = (uint8_t*)m_heap->m_pool;
  m_alloc_barrier = true;
  m_read_barrier = true;
  m_stop_the_world = false;
  m_mutator_wake.signal();
  while (m_mutator_stopped) {
    m_collector_wake.wait(m_collector_lock);  // to make mutator run now
  }
  GC_TRACE(";; [collector: start-the-world]\n");
  GC_TRACE(";; [collector: concurrent-sweeping]\n");
  double t5 = msec();
  m_heap->m_symbol.sweep();
  m_heap->m_string.sweep();
  m_read_barrier = false;
  m_heap->m_weakmappings.m_lock.lock();
  if (m_heap->m_weakmappings.m_vacant) {
    object_slab_traits_t* traits = m_heap->m_weakmappings.m_vacant;
    do m_heap->break_weakmapping(traits);
    while ((traits = traits->next) != m_heap->m_weakmappings.m_vacant);
  }
  if (m_heap->m_weakmappings.m_occupied) {
    object_slab_traits_t* traits = m_heap->m_weakmappings.m_occupied;
    do m_heap->break_weakmapping(traits);
    while ((traits = traits->next) != m_heap->m_weakmappings.m_occupied);
  }
  m_heap->m_weakmappings.m_lock.unlock();
  int capacity = (m_heap->m_pool_size >> OBJECT_SLAB_SIZE_SHIFT);
  uint8_t* slab = m_heap->m_pool;
  int i = 0;
  while (i < capacity) {
    int memo = m_heap->m_pool_usage;
    if (GCSLABP(m_heap->m_pool[i])) {
      if (OBJECT_SLAB_TRAITS_OF(slab)->cache == NULL) {
#if HPDEBUG
        printf(";; [collector: wait for mutator complete slab init]\n");
        fflush(stdout);
#endif
        thread_yield();
        continue;
      }
#if HPDEBUG
      {
        slab_cache_t* ca = OBJECT_SLAB_TRAITS_OF(slab)->cache;
        bool hit = false;
        for (int u = 0; u < array_sizeof(m_heap->m_collectibles); u++) hit |= (&m_heap->m_collectibles[u] == ca);
        hit |= (&m_heap->m_weakmappings == ca);
        hit |= (&m_heap->m_flonums == ca);
        hit |= (&m_heap->m_cons == ca);
  #if USE_CONST_LITERAL
        hit |= (&m_heap->m_immutable_cons == ca);
  #endif
        if (!hit) fatal("%s:%u concurrent_collect(): bad cache reference %p in slab %p", __FILE__, __LINE__, ca, slab);
      }
#endif
      OBJECT_SLAB_TRAITS_OF(slab)->cache->sweep(slab);
      slab += OBJECT_SLAB_SIZE;
      i++;
    } else {
      scoped_lock lock(m_heap->m_lock);
      if (memo != m_heap->m_pool_usage) continue;
      do {
        if (i == m_heap->m_pool_watermark) {
          m_heap->m_sweep_wavefront = (uint8_t*)m_heap->m_pool + m_heap->m_pool_size;
          m_alloc_barrier = false;
          goto finish;
        }
        slab += OBJECT_SLAB_SIZE;
        m_heap->m_sweep_wavefront = slab;
        i++;
      } while (!GCSLABP(m_heap->m_pool[i]));
    }
  }

finish:
  m_collector_kicked = false;
  GC_TRACE(";; [collector: waiting]\n");
  double t6 = msec();
  m_heap->m_usage.m_duration = t6 - t1;
  m_heap->m_usage.m_sync1 = t2 - t1;
  m_heap->m_usage.m_sync2 = t5 - t4;
  m_heap->m_usage.m_recorded = true;
  m_heap->m_usage.m_synchronized = false;
#if DEBUG_CONCURRENT_COLLECT
  printf(
      ";; [        first-lock:%.2fms second-lock:%.2fms overlap:%.2fms]\n"
      ";; [        stw:%.2fms concurrent-marking:%.2fms]\n"
      ";; [        stw:%.2fms serial-marking:%.2fms]\n"
      ";; [        concurrent-sweeping:%.2fms]\n",
      (t2 - t1), (t4 - t3) + (t5 - t4), (t3 - t2) + (t6 - t5), t2 - t1, t3 - t2, t4 - t3, t5 - t4, t6 - t5);
  fflush(stdout);
#endif
#if HPDEBUG
  m_heap->consistency_check();
#endif
}

thread_main_t concurrent_heap_t::collector_thread(void* param) {
  concurrent_heap_t& concurrent_heap = *(concurrent_heap_t*)param;
  concurrent_heap.m_collector_lock.lock();
  concurrent_heap.m_collector_ready = true;
  GC_TRACE(";; [collector: ready]\n");
  while (!concurrent_heap.m_collector_terminating) {
    if (concurrent_heap.m_collector_kicked == false) {
      concurrent_heap.m_collector_wake.wait(concurrent_heap.m_collector_lock);
      continue;
    }
    assert(concurrent_heap.m_mark_sp == concurrent_heap.m_mark_stack);
    if (concurrent_heap.m_mark_stack_size != MARK_STACK_SIZE_INIT) {
      concurrent_heap.m_mark_stack_size = MARK_STACK_SIZE_INIT;
      concurrent_heap.m_mark_stack = concurrent_heap.m_mark_sp = (scm_obj_t*)realloc(concurrent_heap.m_mark_stack, sizeof(scm_obj_t) * concurrent_heap.m_mark_stack_size);
    }
    if (CONCURRENT_COLLECT) {
      if (concurrent_heap.m_heap->m_pool_usage > concurrent_heap.m_heap->m_pool_threshold) {
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
