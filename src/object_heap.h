// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef OBJECT_HEAP_H_INCLUDED
#define OBJECT_HEAP_H_INCLUDED

#include "core.h"
#include "object.h"
#include "concurrent_heap.h"
#include "concurrent_pool.h"
#include "concurrent_slab.h"
#include "inherent.h"
#include "mutex.h"
#include "object_set.h"

#define STRING_TABLE_SIZE_INIT 1021
#define GLOC_TABLE_SIZE_INIT   8191

struct relocate_info_t;

class object_heap_t {
 public:
#if ARCH_LP64
  concurrent_slab_t m_collectibles[8];  // 16-32-64-128-256-512-1024-2048
  concurrent_slab_t m_privates[8];      // 16-32-64-128-256-512-1024-2048
#else
  concurrent_slab_t m_collectibles[8];  // 8-16-32-64-128-256-512-1024
  concurrent_slab_t m_privates[8];      // 8-16-32-64-128-256-512-1024
#endif
  concurrent_slab_t m_cons;
  concurrent_slab_t m_flonums;
  concurrent_slab_t m_weakmappings;
#if USE_CONST_LITERAL
  concurrent_slab_t m_immutable_cons;
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

#if USE_CONST_LITERAL
  bool is_immutable_pair(void* obj) {
    assert(PAIRP(obj));
    return (SLAB_TRAITS_OF(obj)->cache == &m_immutable_cons);
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

  void break_weakmapping(slab_traits_t* traits);
  void trace(scm_obj_t obj);
  void snapshot_root();
  void update_weak_reference();
  // debug
  void display_object_statistics(scm_port_t port);
  void display_heap_statistics(scm_port_t port);
#if HPDEBUG
  void validate_concurrent_slab(void* slab);
  void consistency_check();
#endif
};

#endif
