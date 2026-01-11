// Copyright (c) 2004-2022 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef CONCURRENT_HEAP_H_INCLUDED
#define CONCURRENT_HEAP_H_INCLUDED

#include "core.h"
#include "object.h"
#include "cond.h"
#include "mutex.h"
#include "queue.h"

class object_heap_t;

class concurrent_heap_t {
 public:
  concurrent_heap_t(object_heap_t* heap);
  void init(uint8_t* sweep_wavefront);
  void terminate();
  void collect();
  static thread_main_t collector_thread(void* param);

  bool m_collector_ready;
  bool m_collector_kicked;
  bool m_collector_terminating;
  bool m_mutator_stopped;
  bool m_stop_the_world;
  bool m_read_barrier;
  bool m_write_barrier;
  bool m_alloc_barrier;
  mutex_t m_collector_lock;
  cond_t m_mutator_wake;
  cond_t m_collector_wake;
  concurrent_queue_t<scm_obj_t> m_shade_queue;
  scm_obj_t* m_mark_sp;
  scm_obj_t* m_mark_stack;
  int m_mark_stack_size;
  int m_root_snapshot;
  uint8_t* m_sweep_wavefront;

 private:
  object_heap_t* m_heap;
  void concurrent_collect();
  void synchronized_collect();
};

#endif
