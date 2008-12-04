/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#ifndef OBJECT_HEAP_H_INCLUDED
#define OBJECT_HEAP_H_INCLUDED

#define KEYWORD_TABLE_SIZE_INIT     127
#define STRING_TABLE_SIZE_INIT      1021
#define GLOC_TABLE_SIZE_INIT        8191

#define MARK_STACK_SIZE_INIT        16384       // 16K object, 64K bytes
#define MARK_STACK_SIZE_GROW        4096        //  4K object, 16K bytes
#define SHADE_QUEUE_SIZE            4096        //  4K object, 16K bytes

#include "core.h"
#include "cond.h"
#include "mutex.h"
#include "queue.h"
#include "object.h"
#include "object_slab.h"
#include "object_set.h"
#include "inherent.h"
#if USE_SPINLOCK
  #include "spinlock.h"
#endif

#define ROOT_SNAPSHOT_GLOBALS           0
#define ROOT_SNAPSHOT_LOCALS            1
#define ROOT_SNAPSHOT_EVERYTHING        2
#define ROOT_SNAPSHOT_RETRY             3
#if HPDEBUG
  #define ROOT_SNAPSHOT_CONSISTENCY_CHECK 4
#endif

#define PTAG_FREE       0x00
#define PTAG_USED       0x01
#define PTAG_EXTENT     0x02
#define PTAG_SLAB       0x04
#define PTAG_GC         0x08

#define GCSLABP(tag)    (((tag) & (PTAG_SLAB | PTAG_GC)) == (PTAG_SLAB | PTAG_GC))

class collector_usage_t {
public:
    double  m_duration;
    double  m_sync1;
    double  m_sync2;
    double  m_pause1;
    double  m_pause2;
    double  m_pause3;
    int     m_shade_queue_hazard;
    int     m_barriered_write;
    int     m_barriered_read;
    int     m_barriered_alloc;
    int     m_expand_mark_stack;
    bool    m_recorded;
    bool    m_synchronized;
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

struct relocate_info_t;

class object_heap_t {
public:
    mutex_t             m_lock;
#if ARCH_LP64
    object_slab_cache_t m_collectibles[7];  //   16-32-64-128-256-512-1024
    object_slab_cache_t m_privates[7];      //   16-32-64-128-256-512-1024
#else
    object_slab_cache_t m_collectibles[8];  // 8-16-32-64-128-256-512-1024
    object_slab_cache_t m_privates[8];      // 8-16-32-64-128-256-512-1024
#endif
    object_slab_cache_t m_weakmappings;
    object_slab_cache_t m_cons;
    object_slab_cache_t m_flonums;

public:
    int                 m_trip_bytes;
    int                 m_collect_trip_bytes;
    int                 m_stop_the_world;
    uint8_t*            m_sweep_wavefront;
    int                 m_write_barrier;
    int                 m_read_barrier;
    int                 m_alloc_barrier;
    uint8_t*            m_map;
    size_t              m_map_size;
    uint8_t*            m_pool;
    size_t              m_pool_size;
    int                 m_pool_watermark;
    int                 m_pool_memo;
    int                 m_pool_usage;
    int                 m_pool_threshold;
    scm_obj_t*          m_mark_stack;
    scm_obj_t*          m_mark_sp;
    int                 m_mark_stack_size;

    mutex_t             m_collector_lock;
    cond_t              m_collector_wake;
    cond_t              m_mutator_wake;
    int                 m_mutator_stopped;
    queue_t<scm_obj_t>  m_shade_queue;
    int                 m_collector_ready;
    int                 m_collector_terminating;
    int                 m_collector_kicked;
    int                 m_root_snapshot;
    object_set_t        m_symbol;
    object_set_t        m_string;
    scm_environment_t   m_system_environment;
    scm_environment_t   m_interaction_environment;
    scm_weakhashtable_t m_hidden_variables;
    int                 m_gensym_counter;
    mutex_t             m_gensym_lock;
    scm_bvector_t       m_native_transcoder;
    scm_hashtable_t     m_architecture_feature;
    scm_hashtable_t     m_trampolines;
    scm_obj_t*          m_inherents;
    collector_usage_t   m_usage;
#if USE_PARALLEL_VM
    object_heap_t*      m_primordial;
    object_heap_t*      m_parent;
#endif

    void                init_common(size_t pool_size, size_t initial_datum_size);

public:
                        object_heap_t();
    void                init_primordial(size_t pool_size, size_t initial_datum_size);
    void                init_child(size_t pool_size, size_t initial_datum_size, object_heap_t* parent);
    void                destroy();
    void*               allocate(size_t size, bool slab, bool gc);
    void                deallocate(void* p);
    scm_obj_t           allocate_collectible(size_t size);
    scm_pair_t          allocate_cons();
    scm_flonum_t        allocate_flonum();
    scm_weakmapping_t   allocate_weakmapping();
    void*               allocate_private(size_t size);
    void                deallocate_private(void* obj);
    int                 allocated_size(void* obj);

    bool in_slab(void* obj) {
        assert(obj);
        int index = ((uint8_t*)obj - m_pool) >> OBJECT_SLAB_SIZE_SHIFT;
        assert(index >= 0 && index < m_pool_watermark);
        return (m_pool[index] & PTAG_SLAB) != 0;
    }

    bool in_non_full_slab(void* obj) {
        assert(obj);
        int index = ((uint8_t*)obj - m_pool) >> OBJECT_SLAB_SIZE_SHIFT;
        assert(index >= 0 && index < m_pool_watermark);
        return (m_pool[index] & PTAG_SLAB) && OBJECT_SLAB_TRAITS_OF(obj)->free != NULL;
    }

    bool in_heap(void* obj) { // note: include vm stack
        int index = ((uint8_t*)obj - m_pool) >> OBJECT_SLAB_SIZE_SHIFT;
        return (index >= 0 && index < m_pool_watermark);
    }

    bool is_collectible(void* obj) {
        assert(obj);
        int index = ((uint8_t*)obj - m_pool) >> OBJECT_SLAB_SIZE_SHIFT;
        assert(index >= 0 && index < m_pool_watermark);
        return (m_pool[index] & (PTAG_SLAB | PTAG_GC)) == (PTAG_SLAB | PTAG_GC);
    }

    scm_obj_t           lookup_system_environment(scm_symbol_t symbol);
    void                intern_system_environment(scm_symbol_t symbol, scm_obj_t value);
    void                intern_system_subr(const char *name, subr_proc_t proc);
    void                init_inherents();
    void                init_architecture_feature();

    scm_symbol_t inherent_symbol(int code) const {
        assert(code < INHERENT_TOTAL_COUNT);
        assert(SYMBOLP(m_inherents[code]));
        return (scm_symbol_t)m_inherents[code];
    }

public:
    relocate_info_t*    relocate(bool pack);
    void                relocate_privates(bool pack);
    void                resolve(relocate_info_t* info);
    scm_obj_t           forward(scm_obj_t obj);
    void*               interior_forward(void* ref);
    void                compact_pool();

private:
    bool                extend_pool(size_t extend_size);
    void                shade(scm_obj_t obj);
    void                interior_shade(void* obj);
    void                break_weakmapping(object_slab_traits_t* traits);

public:
    void            collect();
    void            collector_init();
    static thread_main_t collector_thread(void* param);
    static void     concurrent_collect(object_heap_t& heap);
    static void     synchronized_collect(object_heap_t& heap);
    void            concurrent_marking();
    bool            serial_marking();
    void            write_barrier(scm_obj_t rhs);
    void            trace(scm_obj_t obj);
    void            dequeue_root();
    void            enqueue_root(scm_obj_t obj);
    void            display_object_statistics(scm_port_t port);
    void            display_heap_statistics(scm_port_t port);
#if HPDEBUG
    void            consistency_check();
#endif
};

#endif
