// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef CONCURRENT_HEAP_H_INCLUDED
#define CONCURRENT_HEAP_H_INCLUDED

#include "core.h"
#include "cond.h"
#include "mutex.h"
#include "queue.h"

#include "concurrent_pool.h"
#include "concurrent_slab.h"

class collector_usage_t {
 public:
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
  collector_usage_t() { clear(); }
};

class concurrent_heap_t {
  friend class concurrent_slab_t;

 public:
  concurrent_heap_t();
  void init(concurrent_pool_t* pool);
  void terminate();
  void collect();
  void shade(void* obj);
  void interior_shade(void* ref);
  void dequeue_root();
  void enqueue_root(void* obj);
  void write_barrier(void* rhs);

  concurrent_queue_t<void*> m_shade_queue;
  collector_usage_t m_usage;
  cond_t m_mutator_wake;
  cond_t m_collector_wake;
  mutex_t m_collector_lock;
  void** m_mark_stack;
  int m_root_snapshot_mode;
  bool m_read_barrier;
  bool m_write_barrier;
  bool m_collector_kicked;
  bool m_mutator_stopped;
  bool m_stop_the_world;

  void set_snapshot_root_proc(std::function<void()> callback) { m_snapshot_root_proc = callback; }
  void set_trace_proc(std::function<void(void* obj)> callback) { m_trace_proc = callback; }
  void set_finalize_proc(std::function<void(void* obj)> callback) { m_finalize_proc = callback; }
  void set_clear_trip_bytes_proc(std::function<void(void)> callback) { m_clear_trip_bytes_proc = callback; }
  void set_update_weak_reference_proc(std::function<void(void)> callback) { m_update_weak_reference_proc = callback; }
  void set_debug_post_completation_proc(std::function<void(void)> callback) { m_debug_post_completation_proc = callback; }
  void set_debug_check_slab_proc(std::function<void(void* slab)> callback) { m_debug_check_slab_proc = callback; }

 private:
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
  void finalize(void* obj) {
    if (!m_finalize_proc) {
      fatal("%s:%u m_finalize_proc undefined", __FILE__, __LINE__);
    }
    m_finalize_proc(obj);
  }
  void clear_trip_bytes() {
    if (!m_clear_trip_bytes_proc) {
      fatal("%s:%u m_clear_trip_bytes_proc undefined", __FILE__, __LINE__);
    }
    m_clear_trip_bytes_proc();
  }
  void update_weak_reference() {
    if (!m_update_weak_reference_proc) {
      fatal("%s:%u m_update_weak_reference_proc undefined", __FILE__, __LINE__);
    }
    m_update_weak_reference_proc();
  }
  void debug_post_completation() {
    if (m_debug_post_completation_proc) m_debug_post_completation_proc();
  }
  void debug_check_slab(void* slab) {
    if (m_debug_check_slab_proc) m_debug_check_slab_proc(slab);
  }

  static void* collector_thread(void* param);
  void concurrent_collect();
  void synchronized_collect();
  void concurrent_mark();
  bool synchronized_mark();

  std::function<void(void* obj)> m_trace_proc;
  std::function<void(void* obj)> m_finalize_proc;
  std::function<void(void)> m_clear_trip_bytes_proc;
  std::function<void(void)> m_snapshot_root_proc;
  std::function<void(void)> m_update_weak_reference_proc;
  std::function<void(void)> m_debug_post_completation_proc;
  std::function<void(void* slab)> m_debug_check_slab_proc;
  concurrent_pool_t* m_concurrent_pool;
  void** m_mark_sp;
  uint8_t* m_sweep_wavefront;
  pthread_t m_collector_thread;
  int m_mark_stack_size;
  bool m_collector_ready;
  bool m_collector_terminating;
  bool m_alloc_barrier;
  void* allocate(size_t size, bool slab, bool gc);
  void deallocate(void* p);
};

#endif
