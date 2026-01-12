// Copyright (c) 2004-2022 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef OBJECT_HEAP_H_INCLUDED
#define OBJECT_HEAP_H_INCLUDED

#include "core.h"
#include "object.h"
#include "concurrent_heap.h"
#include "concurrent_pool.h"
#include "cond.h"
#include "inherent.h"
#include "mutex.h"
#include "object_set.h"
#include "queue.h"
#include "slab_cache.h"

#define STRING_TABLE_SIZE_INIT   1021
#define GLOC_TABLE_SIZE_INIT     8191
#define MARK_STACK_SIZE_INIT     16384  // 16K object, 64K/128K bytes
#define MARK_STACK_SIZE_GROW     4096   // 4K object, 16K/32K bytes
#define SHADE_QUEUE_SIZE         4096   // 4K object, 16K/32K bytes

#define ROOT_SNAPSHOT_GLOBALS    0
#define ROOT_SNAPSHOT_LOCALS     1
#define ROOT_SNAPSHOT_EVERYTHING 2
#define ROOT_SNAPSHOT_RETRY      3
#if HPDEBUG
  #define ROOT_SNAPSHOT_CONSISTENCY_CHECK 4
#endif

struct relocate_info_t;

class object_heap_t {
  friend class concurrent_heap_t;

 public:
#if ARCH_LP64
  slab_cache_t m_collectibles[8];  // 16-32-64-128-256-512-1024-2048
  slab_cache_t m_privates[8];      // 16-32-64-128-256-512-1024-2048
#else
  slab_cache_t m_collectibles[8];  // 8-16-32-64-128-256-512-1024
  slab_cache_t m_privates[8];      // 8-16-32-64-128-256-512-1024
#endif
  slab_cache_t m_cons;
  slab_cache_t m_flonums;
  slab_cache_t m_weakmappings;
#if USE_CONST_LITERAL
  slab_cache_t m_immutable_cons;
#endif

  int m_trip_bytes;
  int m_collect_trip_bytes;
  concurrent_pool_t m_concurrent_pool;
  concurrent_heap_t m_concurrent_heap;
  object_set_t m_symbol;
  object_set_t m_string;
  scm_obj_t* m_inherents;
  scm_environment_t m_interaction_environment;
  scm_environment_t m_system_environment;
  scm_weakhashtable_t m_hidden_variables;
  scm_hashtable_t m_architecture_feature;
  scm_hashtable_t m_trampolines;
  scm_bvector_t m_native_transcoder;
  int m_gensym_counter;
  mutex_t m_gensym_lock;
  object_heap_t();
  void init(size_t pool_size, size_t initial_datum_size);
  void init_heap(size_t pool_size, size_t initial_datum_size);
  void init_inherents();
  void init_architecture_feature();
  void destroy();
  void* allocate(size_t size, bool slab, bool gc);
  void deallocate(void* p);
  bool extend_pool(size_t extend_size);
  scm_obj_t allocate_collectible(size_t size);
  scm_pair_t allocate_cons();
  scm_flonum_t allocate_flonum();
  scm_weakmapping_t allocate_weakmapping();
#if USE_CONST_LITERAL
  scm_pair_t allocate_immutable_cons();
#endif
  int allocated_size(void* obj);
  void* allocate_private(size_t size);
  void deallocate_private(void* obj);
  void intern_system_environment(scm_symbol_t symbol, scm_obj_t value);
  void intern_system_subr(const char* name, subr_proc_t proc);
  scm_obj_t lookup_system_environment(scm_symbol_t symbol);

  bool in_slab(void* obj) {
    assert(obj);
    int index = ((uint8_t*)obj - m_concurrent_pool.m_pool) >> OBJECT_SLAB_SIZE_SHIFT;
    assert(index >= 0 && index < m_concurrent_pool.m_pool_watermark);
    return (m_concurrent_pool.m_pool[index] & PTAG_SLAB) != 0;
  }

  bool in_non_full_slab(void* obj) {
    assert(obj);
    int index = ((uint8_t*)obj - m_concurrent_pool.m_pool) >> OBJECT_SLAB_SIZE_SHIFT;
    assert(index >= 0 && index < m_concurrent_pool.m_pool_watermark);
    return (m_concurrent_pool.m_pool[index] & PTAG_SLAB) && OBJECT_SLAB_TRAITS_OF(obj)->free != NULL;
  }

  bool in_heap(void* obj) {
    int index = ((uint8_t*)obj - m_concurrent_pool.m_pool) >> OBJECT_SLAB_SIZE_SHIFT;
    return (index >= 0 && index < m_concurrent_pool.m_pool_watermark);
  }

  bool is_collectible(void* obj) {
    assert(obj);
    int index = ((uint8_t*)obj - m_concurrent_pool.m_pool) >> OBJECT_SLAB_SIZE_SHIFT;
    assert(index >= 0 && index < m_concurrent_pool.m_pool_watermark);
    return (m_concurrent_pool.m_pool[index] & (PTAG_SLAB | PTAG_GC)) == (PTAG_SLAB | PTAG_GC);
  }

#if USE_CONST_LITERAL
  bool is_immutable_pair(void* obj) {
    assert(PAIRP(obj));
    return (OBJECT_SLAB_TRAITS_OF(obj)->cache == &m_immutable_cons);
  }
#endif

  scm_symbol_t inherent_symbol(int code) const {
    assert(code < INHERENT_TOTAL_COUNT);
    assert(SYMBOLP(m_inherents[code]));
    return (scm_symbol_t)m_inherents[code];
  }

  // compactor
  relocate_info_t* relocate(bool pack);
  void relocate_privates(bool pack);
  void resolve(relocate_info_t* info);
  scm_obj_t forward(scm_obj_t obj);
  void* interior_forward(void* ref);
  void compact_pool();
  // collector
  void collect();

  void write_barrier(scm_obj_t rhs);
  void break_weakmapping(object_slab_traits_t* traits);
  void trace(scm_obj_t obj);
  void dequeue_root();
  void enqueue_root(scm_obj_t obj);
  // debug
  void display_object_statistics(scm_port_t port);
  void display_heap_statistics(scm_port_t port);
#if HPDEBUG
  void consistency_check();
#endif
};

#endif
