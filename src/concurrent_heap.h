// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef CONCURRENT_HEAP_H_INCLUDED
#define CONCURRENT_HEAP_H_INCLUDED

#include "core.h"
#include "object.h"
#include <functional>
#include "cond.h"
#include "mutex.h"
#include "queue.h"

#include "concurrent_pool.h"
#include "slab_cache.h"

class object_heap_t;

class collector_usage_t {
 public:
  collector_usage_t() { clear(); }
  double m_duration;
  double m_sync1;
  double m_sync2;
  double m_pause1;
  double m_pause2;
  double m_pause3;
  int m_shade_queue_hazard;
  int m_barriered_write;
  int m_barriered_read;
  int m_barriered_alloc;
  int m_expand_mark_stack;
  bool m_recorded;
  bool m_synchronized;
  void clear() {
    m_duration = 0.0;
    m_sync1 = 0.0;
    m_sync2 = 0.0;
    m_pause1 = 0.0;
    m_pause2 = 0.0;
    m_pause3 = 0.0;
    m_expand_mark_stack = 0;
    m_shade_queue_hazard = 0;
    m_barriered_write = 0;
    m_barriered_read = 0;
    m_barriered_alloc = 0;
    m_recorded = false;
    m_synchronized = false;
  }
};

class concurrent_heap_t {
 public:
  concurrent_heap_t();
  void init(object_heap_t* heap, concurrent_pool_t* pool);
  void terminate();
  void collect();
  // Bridge methods for slab_cache_t to avoid direct object_heap_t access
  void* allocate(size_t size, bool slab, bool gc);
  void deallocate(void* p);
  void finalize(void* obj);
  void shade(scm_obj_t obj);
  void interior_shade(void* ref);
  void dequeue_root();
  void enqueue_root(scm_obj_t obj);
  void write_barrier(scm_obj_t rhs);

  void set_snapshot_root_proc(std::function<void()> callback) { m_snapshot_root_proc = callback; }
  void set_trace_proc(std::function<void(void* obj)> callback) { m_trace_proc = callback; }
  void set_clear_trip_bytes_proc(std::function<void(void)> callback) { m_clear_trip_bytes_proc = callback; }

  bool in_slab(void* obj) {
    assert(obj);
    int index = ((uint8_t*)obj - m_concurrent_pool->m_pool) >> OBJECT_SLAB_SIZE_SHIFT;
    assert(index >= 0 && index < m_concurrent_pool->m_pool_watermark);
    return (m_concurrent_pool->m_pool[index] & PTAG_SLAB) != 0;
  }

  bool in_non_full_slab(void* obj) {
    assert(obj);
    int index = ((uint8_t*)obj - m_concurrent_pool->m_pool) >> OBJECT_SLAB_SIZE_SHIFT;
    assert(index >= 0 && index < m_concurrent_pool->m_pool_watermark);
    return (m_concurrent_pool->m_pool[index] & PTAG_SLAB) && OBJECT_SLAB_TRAITS_OF(obj)->free != NULL;
  }

  bool in_heap(void* obj) {
    int index = ((uint8_t*)obj - m_concurrent_pool->m_pool) >> OBJECT_SLAB_SIZE_SHIFT;
    return (index >= 0 && index < m_concurrent_pool->m_pool_watermark);
  }

  bool is_collectible(void* obj) {
    assert(obj);
    int index = ((uint8_t*)obj - m_concurrent_pool->m_pool) >> OBJECT_SLAB_SIZE_SHIFT;
    assert(index >= 0 && index < m_concurrent_pool->m_pool_watermark);
    return (m_concurrent_pool->m_pool[index] & (PTAG_SLAB | PTAG_GC)) == (PTAG_SLAB | PTAG_GC);
  }

  bool m_collector_kicked;
  bool m_mutator_stopped;
  bool m_stop_the_world;
  bool m_read_barrier;
  bool m_write_barrier;
  bool m_alloc_barrier;
  mutex_t m_collector_lock;
  cond_t m_mutator_wake;
  cond_t m_collector_wake;
  int m_root_snapshot_mode;
  uint8_t* m_sweep_wavefront;
  collector_usage_t m_usage;
  concurrent_queue_t<scm_obj_t> m_shade_queue;
  scm_obj_t* m_mark_sp;
  scm_obj_t* m_mark_stack;
  int m_mark_stack_size;

 private:
  static thread_main_t collector_thread(void* param);
  void concurrent_collect();
  void synchronized_collect();
  void concurrent_mark();
  bool synchronized_mark();
  concurrent_pool_t* m_concurrent_pool;
  object_heap_t* m_heap;
  pthread_t m_collector_thread;
  bool m_collector_ready;
  bool m_collector_terminating;

  std::function<void(void* obj)> m_trace_proc;
  std::function<void(void)> m_clear_trip_bytes_proc;
  std::function<void(void)> m_snapshot_root_proc;
  void snapshot_root() {
    if (!m_snapshot_root_proc) {
      fatal("%s:%u m_snapshot_root_proc undefined", __FILE__, __LINE__);
    }
    m_snapshot_root_proc();
  }
  void trace(void* obj) {
    if (!m_trace_proc) {
      fatal("%s:%u m_trace_proc undefined", __FILE__, __LINE__);
    }
    m_trace_proc(obj);
  }
  void clear_trip_bytes() {
    if (!m_clear_trip_bytes_proc) {
      fatal("%s:%u m_clear_trip_bytes_proc undefined", __FILE__, __LINE__);
    }
    m_clear_trip_bytes_proc();
  }
};

#endif
