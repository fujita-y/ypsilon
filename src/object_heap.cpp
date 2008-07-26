/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

/*
    Reference:
    Malloc(3) revisited: Paul-Henning Kamp
    The Slab Allocator: An Object-Caching Kernel Memory Allocator - Jeff Bonwick - 1994 USENIX
    Magazines and Vmem: Extending the Slab Allocator to Many CPUs and Arbitrary Resources - Jeff Bonwick, Jonathan Adams - 2001 USENIX
*/

#include "core.h"
#include "bit.h"
#include "hash.h"
#include "heap.h"
#include "port.h"
#include "subr.h"

#if GCDEBUG
  #define   GC_TRACE(fmt)   do { printf(fmt); fflush(stdout); } while (0)
#else
  #define   GC_TRACE(fmt)   ((void)0)
#endif

#define DEBUG_CONCURRENT_COLLECT    0

#define SYNCHRONIZE_THRESHOLD(x)    ((x) - (x) / 4)
#define DEFALUT_COLLECT_TRIP_BYTES  (2 * 1024 * 1024)

#define ENSURE_REALTIME             (1.0)       // in msec (1.0 for 0.0001 second)
#define TIMEOUT_CHECK_EACH          (100)

inline int
bytes_to_bucket(int x)
{
    int bucket = 0;
    if (x > 8) {
        x = x - 1;  // see clp2() in bit.cpp
        x = x | (x >>  1);
        x = x | (x >>  2);
        x = x | (x >>  4);
        x = x | (x >>  8);
        x = x | (x >> 16);
        x = (x + 1) >> 4;
        while (x) {
            bucket = bucket + 1;
            x = x >> 1;
        }
    }
    return bucket;
}

object_heap_t::object_heap_t()
    : m_pool(NULL), m_pool_size(0), m_mark_stack(NULL)
{
    m_lock.init();
    m_gensym_lock.init();
}

object_heap_t::~object_heap_t()
{
    destroy();
    m_lock.destroy();
    m_gensym_lock.destroy();
}

scm_pair_t
object_heap_t::allocate_cons()
{
    assert(m_cons.m_object_size == sizeof(scm_pair_rec_t));
    m_trip_bytes += sizeof(scm_pair_rec_t);
    if (m_trip_bytes >= m_collect_trip_bytes) collect();
    do {
        scm_pair_t obj = (scm_pair_t)m_cons.new_collectible_object();
        if (obj) return obj;
    } while (extend_pool(OBJECT_SLAB_SIZE));
    fatal("fatal: heap memory overflow (%dMB)\n[exit]\n", m_pool_size / (1024 * 1024));
    return NULL;
}

scm_flonum_t
object_heap_t::allocate_flonum()
{
    assert(m_collectibles[1].m_object_size == sizeof(scm_flonum_rec_t));
    m_trip_bytes += sizeof(scm_flonum_rec_t);
    if (m_trip_bytes >= m_collect_trip_bytes) collect();
    do {
        scm_flonum_t obj = (scm_flonum_t)m_flonums.new_collectible_object();
        if (obj) return obj;
    } while (extend_pool(OBJECT_SLAB_SIZE));
    fatal("fatal: heap memory overflow (%dMB)\n[exit]\n", m_pool_size / (1024 * 1024));
    return NULL;
}

scm_obj_t
object_heap_t::allocate_collectible(size_t size)
{
    m_trip_bytes += size;
    if (m_trip_bytes >= m_collect_trip_bytes) collect();
    int bucket = bytes_to_bucket(size);
    if (bucket < array_sizeof(m_collectibles)) {
        do {
            scm_obj_t obj = (scm_obj_t)m_collectibles[bucket].new_collectible_object();
            if (obj) return obj;
        } while (extend_pool(OBJECT_SLAB_SIZE));
        fatal("fatal: heap memory overflow (%dMB)\n[exit]\n", m_pool_size / (1024 * 1024));
    } else {
        fatal("%s:%u collectible object over %d bytes not supported but %d bytes requested", __FILE__, __LINE__, 1 << (array_sizeof(m_collectibles) + 2), size);
    }
    return NULL;
}

scm_weakmapping_t
object_heap_t::allocate_weakmapping()
{
    m_trip_bytes += m_weakmappings.m_object_size;
    if (m_trip_bytes >= m_collect_trip_bytes) collect();
    do {
        scm_weakmapping_t obj = (scm_weakmapping_t)m_weakmappings.new_collectible_object();
        if (obj) return obj;
    } while (extend_pool(OBJECT_SLAB_SIZE));
    fatal("fatal: heap memory overflow (%dMB)\n[exit]\n", m_pool_size / (1024 * 1024));
}

void*
object_heap_t::allocate_private(size_t size)
{
    m_trip_bytes += size;
    if (m_trip_bytes >= m_collect_trip_bytes) collect();
    int bucket = bytes_to_bucket(size);
    if (bucket < array_sizeof(m_privates)) {
        do {
            void* obj = m_privates[bucket].new_object();
            if (obj) return obj;
        } while (extend_pool(OBJECT_SLAB_SIZE));
        fatal("fatal: heap memory overflow (%dMB)\n[exit]\n", m_pool_size / (1024 * 1024));
    } else {
        do {
            void* obj = allocate(size, false, false);
            if (obj) return obj;
        } while (extend_pool(size));
        fatal("fatal: heap memory overflow (%dMB)\n[exit]\n", m_pool_size / (1024 * 1024));
    }
    return NULL;
}

void
object_heap_t::deallocate_private(void* obj)
{
    if (obj) {
        assert(in_heap(obj));
        assert(!is_collectible(obj));
        if (in_slab(obj)) {
            object_slab_cache_t* cache = OBJECT_SLAB_TRAITS_OF(obj)->cache;
            cache->delete_object(obj);
        } else {
            assert(!is_collectible(obj));
            deallocate(obj);
        }
    }
}

int
object_heap_t::allocated_size(void* obj)
{
    assert(in_heap(obj));
    if (in_slab(obj)) {
        assert(!is_collectible(obj));
        object_slab_cache_t* cache = OBJECT_SLAB_TRAITS_OF(obj)->cache;
        return cache->m_object_size;
    } else {
        assert(((intptr_t)obj & (OBJECT_SLAB_SIZE - 1)) == 0);
        int index = ((uint8_t*)obj - m_pool) >> OBJECT_SLAB_SIZE_SHIFT;
        assert(m_pool[index] & PTAG_USED);
        int n_page = 1;
        while (++index < m_pool_watermark) {
            if (m_pool[index] & PTAG_EXTENT) n_page++;
            else break;
        }
        return n_page * OBJECT_SLAB_SIZE;
    }
}

bool
object_heap_t::init(size_t pool_size, size_t initial_datum_size)
{
    assert(getpagesize() == OBJECT_SLAB_SIZE);                  // for optimal performance
    assert(pool_size >= OBJECT_SLAB_SIZE + OBJECT_SLAB_SIZE);   // check minimum (1 directory slab + 1 datum slab = 2)

    // pool
    if (m_pool) destroy();
    m_pool_size = (pool_size + OBJECT_SLAB_SIZE - 1) & ~(OBJECT_SLAB_SIZE - 1);
    m_pool = (uint8_t*)heap_map(NULL, m_pool_size);
    if (m_pool == HEAP_MAP_FAILED) {
        fatal("%s:%u mmap() failed: %s", __FILE__, __LINE__, strerror(errno));
        m_pool = NULL;
        return false;
    }

    // ptag
    int n_tag = m_pool_size / OBJECT_SLAB_SIZE;
    int n_slab = (n_tag + OBJECT_SLAB_SIZE - 1) / OBJECT_SLAB_SIZE;
    memset(m_pool, PTAG_FREE, n_slab * OBJECT_SLAB_SIZE);
    for (int i = 0; i < n_slab; i++) m_pool[i] = PTAG_USED;
    m_pool_watermark = (initial_datum_size >> OBJECT_SLAB_SIZE_SHIFT);
    if (m_pool_watermark <= n_slab || m_pool_watermark >= (m_pool_size >> OBJECT_SLAB_SIZE_SHIFT)) {
        fatal("%s:%u bad object_heap_t::init() parameter, pool_size:%d init_datum_size:%d", __FILE__, __LINE__, pool_size, initial_datum_size);
    }
    m_pool_memo = 0;
    m_pool_usage = 0;
    m_pool_threshold = SYNCHRONIZE_THRESHOLD(n_tag);

    // collector
    m_trip_bytes = 0;
    m_collect_trip_bytes = ((m_pool_size / 16) < DEFALUT_COLLECT_TRIP_BYTES) ?  (m_pool_size / 16) : DEFALUT_COLLECT_TRIP_BYTES;
    collector_init();

    // slab
    assert((1 << (array_sizeof(m_collectibles) + 2)) == OBJECT_SLAB_THRESHOLD);
    for (int n = 0; n < array_sizeof(m_collectibles); n++) m_collectibles[n].init(this, 1 << (n + 3), true);
    for (int n = 0; n < array_sizeof(m_privates); n++) m_privates[n].init(this, 1 << (n + 3), false);
    m_weakmappings.init(this, clp2(sizeof(scm_weakmapping_rec_t)), true);
    m_cons.init(this, clp2(sizeof(scm_pair_rec_t)), true);
    m_flonums.init(this, clp2(sizeof(scm_flonum_rec_t)), true);

    int base_cache_limit = m_collect_trip_bytes / OBJECT_SLAB_SIZE;
    m_cons.m_cache_limit = base_cache_limit;
    m_flonums.m_cache_limit = base_cache_limit >> 1;
    m_weakmappings.m_cache_limit = base_cache_limit >> 3;
    for (int n = 0; n < array_sizeof(m_collectibles); n++) m_collectibles[n].m_cache_limit = base_cache_limit >> 3;

    // hash
    m_symbol.init(this);
    m_string.init(this);

    // inherents
    for (int i = 0; i < array_sizeof(m_inherents); i++) m_inherents[i] = scm_undef;
    init_inherents();

    // global shared
    m_interaction_environment = make_environment(this, "interaction");
    m_system_environment = make_environment(this, "system");
    m_gensym_counter = 1;
    m_native_transcoder = make_bvector(this, 3);
    m_native_transcoder->elts[0] = SCM_PORT_CODEC_NATIVE;
    m_native_transcoder->elts[1] = SCM_PORT_EOL_STYLE_NATIVE;
    m_native_transcoder->elts[2] = SCM_PORT_ERROR_HANDLING_MODE_REPLACE;

    m_architecture_feature = make_hashtable(this, SCM_HASHTABLE_TYPE_EQ, lookup_mutable_hashtable_size(23));
    // edit hashtable size when add new arch parameter
    scoped_lock lock(m_architecture_feature->lock);
#define ARCH_FIXNUM_PARAM(name, value)  put_hashtable(m_architecture_feature, make_symbol(this, #name), MAKEFIXNUM(value))
#define ARCH_STRING_PARAM(name, value)  put_hashtable(m_architecture_feature, make_symbol(this, #name), make_string_literal(this, value))
    ARCH_FIXNUM_PARAM(sizeof:char,     sizeof(char));
    ARCH_FIXNUM_PARAM(sizeof:short,    sizeof(short));
    ARCH_FIXNUM_PARAM(sizeof:int,      sizeof(int));
    ARCH_FIXNUM_PARAM(sizeof:long,     sizeof(long));
    ARCH_FIXNUM_PARAM(sizeof:float,    sizeof(float));
    ARCH_FIXNUM_PARAM(sizeof:double,   sizeof(double));
    ARCH_FIXNUM_PARAM(sizeof:size_t,   sizeof(size_t));
    ARCH_FIXNUM_PARAM(sizeof:intptr_t, sizeof(intptr_t));
    ARCH_FIXNUM_PARAM(alignof:char,     ALIGNOF(char));
    ARCH_FIXNUM_PARAM(alignof:short,    ALIGNOF(short));
    ARCH_FIXNUM_PARAM(alignof:int,      ALIGNOF(int));
    ARCH_FIXNUM_PARAM(alignof:long,     ALIGNOF(long));
    ARCH_FIXNUM_PARAM(alignof:float,    ALIGNOF(float));
    ARCH_FIXNUM_PARAM(alignof:double,   ALIGNOF(double));
    ARCH_FIXNUM_PARAM(alignof:size_t,   ALIGNOF(size_t));
    ARCH_FIXNUM_PARAM(alignof:intptr_t, ALIGNOF(intptr_t));
    ARCH_FIXNUM_PARAM(alignof:int8_t,   ALIGNOF(int8_t));
    ARCH_FIXNUM_PARAM(alignof:int16_t,  ALIGNOF(int16_t));
    ARCH_FIXNUM_PARAM(alignof:int32_t,  ALIGNOF(int32_t));
    ARCH_FIXNUM_PARAM(alignof:int64_t,  ALIGNOF(int64_t));

  #if _MSC_VER
    ARCH_STRING_PARAM(operating-system, "windows");
  #else
    {
        struct utsname buf;
        uname(&buf);
        int i = 0;
        while (buf.sysname[i]) {
            buf.sysname[i] = tolower(buf.sysname[i]);
            i++;
        }
        ARCH_STRING_PARAM(operating-system, buf.sysname);
    }
  #endif
#undef ARCH_FIXNUM_PARAM
#undef ARCH_STRING_PARAM

    m_trampolines = make_hashtable(this, SCM_HASHTABLE_TYPE_EQ, lookup_mutable_hashtable_size(0));

    init_subr_base(this);
    init_subr_base_arith(this);
    init_subr_r5rs_arith(this);
    init_subr_bvector(this);
    init_subr_port(this);
    init_subr_unicode(this);
    init_subr_ffi(this);
    init_subr_bitwise(this);
    init_subr_fixnum(this);
    init_subr_flonum(this);
    init_subr_hash(this);
    init_subr_list(this);
    init_subr_others(this);
    intern_system_environment(make_symbol(this, "apply"), scm_proc_apply);
    intern_system_environment(make_symbol(this, "call-with-current-continuation"), scm_proc_callcc);
    intern_system_environment(make_symbol(this, "call/cc"), scm_proc_callcc);
    intern_system_environment(make_symbol(this, "apply-values"), scm_proc_apply_values);
    return true;
}

scm_obj_t
object_heap_t::lookup_system_environment(scm_symbol_t symbol)
{
    scoped_lock lock(m_system_environment->variable->lock);
    scm_obj_t obj = get_hashtable(m_system_environment->variable, symbol);
    if (obj != scm_undef) {
        assert(GLOCP(obj));
        return ((scm_gloc_t)obj)->value;
    }
    return scm_undef;
}

void
object_heap_t::intern_system_environment(scm_symbol_t symbol, scm_obj_t value)
{
    scm_hashtable_t ht = m_system_environment->variable;
    scoped_lock lock(ht->lock);
    scm_obj_t obj = get_hashtable(ht, symbol);
    if (obj != scm_undef) {
        assert(GLOCP(obj));
        write_barrier(value);
        ((scm_gloc_t)obj)->value = value;
        return;
    }
    scm_gloc_t gloc = make_gloc(this, m_system_environment, symbol);
    gloc->value = value;
    write_barrier(symbol);
    write_barrier(gloc);
    int nsize = put_hashtable(ht, symbol, gloc);
    if (nsize) rehash_hashtable(this, ht, nsize);
}

void
object_heap_t::intern_system_subr(const char *name, subr_proc_t proc)
{
    scm_symbol_t symbol = make_symbol(this, name);
    assert(lookup_system_environment(symbol) == scm_undef);
    intern_system_environment(symbol, make_subr(this, proc, symbol));
}

void
object_heap_t::destroy()
{
    if (m_mark_stack) free(m_mark_stack);
    m_mark_stack = NULL;
    if (m_pool) {
        heap_unmap(m_pool, m_pool_size);
        m_pool = NULL;
        m_pool_size = 0;
    }
}

void*
object_heap_t::allocate(size_t size, bool for_slab, bool for_collectible)
{
    assert(for_slab || (for_collectible == false));
    uint8_t attr = 0;
    if (for_slab) {
        if (for_collectible) attr = PTAG_SLAB | PTAG_GC;
        else attr = PTAG_SLAB;
    }
    assert(m_pool);
    int npage = (size + OBJECT_SLAB_SIZE - 1) >> OBJECT_SLAB_SIZE_SHIFT;
    scoped_lock lock(m_lock);
    if (npage == 1) {
        for (int i = m_pool_memo; i < m_pool_watermark; i++) {
            if (m_pool[i] == PTAG_FREE) {
                void* slab = m_pool + (i << OBJECT_SLAB_SIZE_SHIFT);
                if (for_collectible) OBJECT_SLAB_TRAITS_OF(slab)->cache = NULL;
                m_pool[i] = PTAG_USED | attr;
                m_pool_memo = i + 1;
                m_pool_usage = m_pool_usage + 1;
                return slab;
            }
        }
    } else {
        assert(for_collectible == false);
        int head = m_pool_memo;
        while (head < m_pool_watermark) {
            if (m_pool[head] == PTAG_FREE) {
                int found = 1;
                for (int tail = head + 1; tail < m_pool_watermark; tail++) {
                    if (m_pool[tail] == PTAG_FREE) {
                        if (++found == npage) {
                            m_pool[head] = PTAG_USED | attr;
                            for (int n = head + 1; n <= tail; n++) m_pool[n] = PTAG_EXTENT | attr;
                            m_pool_usage = m_pool_usage + npage;
                            return m_pool + (head << OBJECT_SLAB_SIZE_SHIFT);
                        }
                    } else {
                        head = tail;
                        break;
                    }
                }
            }
            head++;
        }
    }
    return NULL;
}

void
object_heap_t::deallocate(void* p)
{
    scoped_lock lock(m_lock);
    assert(p);
    assert(m_pool);
    assert(((intptr_t)p & (OBJECT_SLAB_SIZE - 1)) == 0);
    int i = ((uint8_t*)p - m_pool) >> OBJECT_SLAB_SIZE_SHIFT;
    if (i < m_pool_memo) m_pool_memo = i;
    assert(i >= 0 && i < m_pool_watermark);
    assert(m_pool[i] & PTAG_USED);
    m_pool[i] = PTAG_FREE;
    m_pool_usage = m_pool_usage - 1;
    while (++i < m_pool_watermark) {
        if (m_pool[i] & PTAG_EXTENT) {
            m_pool[i] = PTAG_FREE;
            m_pool_usage = m_pool_usage - 1;
        } else {
            break;
        }
    }
#if !defined(NDEBUG) || HPDEBUG
    memset(p, 0xBD, OBJECT_SLAB_SIZE);
#endif
}

bool
object_heap_t::extend_pool(size_t extend_size)
{
    scoped_lock lock(m_lock);
    int capacity = (m_pool_size >> OBJECT_SLAB_SIZE_SHIFT);
    if (m_pool_watermark == capacity) return false;
    m_pool_watermark += ((extend_size + OBJECT_SLAB_SIZE - 1) >> OBJECT_SLAB_SIZE_SHIFT);
    if (m_pool_watermark > capacity) m_pool_watermark = capacity;
    return true;
}

void
object_heap_t::shade(scm_obj_t obj)
{
    if (CELLP(obj)) {
        assert(obj);
        if (OBJECT_SLAB_TRAITS_OF(obj)->cache->state(obj) == false) {
            if (m_mark_sp < m_mark_stack + m_mark_stack_size) {
                *m_mark_sp++ = obj;
                return;
            }
            m_usage.m_expand_mark_stack++;
            int newsize = m_mark_stack_size + MARK_STACK_SIZE_GROW;
            m_mark_stack = (scm_obj_t*)realloc(m_mark_stack, sizeof(scm_obj_t) * newsize);
            if (m_mark_stack == NULL) {
                fatal("%s:%u memory overflow on realloc mark stack", __FILE__, __LINE__);
            }
            m_mark_sp = m_mark_stack + m_mark_stack_size;
            m_mark_stack_size = newsize;
            *m_mark_sp++ = obj;
        }
    }
}

void
object_heap_t::interior_shade(void* ref)
{
    if (ref) {
#ifndef NDEBUG
        int i = ((uint8_t*)ref - m_pool) >> OBJECT_SLAB_SIZE_SHIFT;
        assert(i >= 0 && i < m_pool_watermark);
        assert(GCSLABP(m_pool[i]));
#endif
        shade(OBJECT_SLAB_TRAITS_OF(ref)->cache->lookup(ref));
    }
}

void
object_heap_t::mark_weakmapping(object_slab_traits_t* traits)
{
    int count = traits->refc;
    int size = traits->cache->m_object_size;
    uint8_t* p = OBJECT_SLAB_TOP_OF(traits);
    while (count) {
        scm_obj_t obj = p;
        if (WEAKMAPPINGP(obj)) {
            if (traits->cache->state(obj)) {
                scm_weakmapping_t wp = (scm_weakmapping_t)obj;
                scm_obj_t key = wp->key;
                if (CELLP(key) && OBJECT_SLAB_TRAITS_OF(key)->cache->state(key) == true) shade(wp->value);
            }
            count--;
        }
        p += size;
        assert(p < (uint8_t*)traits);
    }
}

void
object_heap_t::break_weakmapping(object_slab_traits_t* traits)
{
    int count = traits->refc;
    int size = traits->cache->m_object_size;
    uint8_t* p = OBJECT_SLAB_TOP_OF(traits);
    while (count) {
        scm_obj_t obj = p;
        if (WEAKMAPPINGP(obj)) {
            if (traits->cache->state(obj)) {
                scm_weakmapping_t wp = (scm_weakmapping_t)obj;
                scm_obj_t key = wp->key;
                if (CELLP(key) && OBJECT_SLAB_TRAITS_OF(key)->cache->state(key) == false) wp->key = wp->value = scm_false;
            }
            count--;
        }
        p += size;
        assert(p < (uint8_t*)traits);
    }
}

void
object_heap_t::write_barrier(scm_obj_t rhs)
{
    // simple (Dijkstra)
    if (m_write_barrier) {
        if (CELLP(rhs)) {
            if (OBJECT_SLAB_TRAITS_OF(rhs)->cache->state(rhs) == false) {
                MEM_STORE_FENCE;
                while (m_shade_queue.wait_lock_try_put(rhs) == false) {
                    if (OBJECT_SLAB_TRAITS_OF(rhs)->cache->state(rhs)) break;
                    if (m_stop_the_world) {
                        m_collector_lock.lock();
                        m_collector_wake.signal();
                        m_collector_lock.unlock();
                    } else {
#if _MSC_VER
                        Sleep(0);
#else
                        sched_yield();
#endif
                    }
                    m_usage.m_shade_queue_hazard++;
                    if (WBDEBUG) {
                        printf(";; [write-barrier: m_shade_queue overflow, mutator sched_yield]\n");
                        fflush(stdout);
                    } else {
                        GC_TRACE(";; [write-barrier: m_shade_queue overflow, mutator sched_yield]\n");
                    }
                }
                if (DETAILED_STATISTIC) m_usage.m_barriered_write++;
            }
        }
    }
}

void
object_heap_t::collect()
{
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

void
object_heap_t::collector_init()
{
    m_mark_stack_size = MARK_STACK_SIZE_INIT;
    m_mark_stack = m_mark_sp = (scm_obj_t*)malloc(sizeof(scm_obj_t) * m_mark_stack_size);
    assert(m_mark_stack);

    m_usage.clear();

    m_shade_queue.init();
    m_collector_lock.init();
    m_mutator_wake.init();
    m_collector_wake.init();

    m_write_barrier = false;
    m_read_barrier = false;
    m_alloc_barrier = false;
    m_collector_kicked = false;
    m_collector_ready = false;
    m_stop_the_world = false;
    m_mutator_stopped = false;
    m_sweep_wavefront = (uint8_t*)m_pool + m_pool_size;
    thread_start(collector_thread, this);
}

void
object_heap_t::dequeue_root()
{
    scm_obj_t obj;
    while (m_shade_queue.count()) {
        m_shade_queue.get(&obj);
        shade(obj);
    }
}

void
object_heap_t::enqueue_root(scm_obj_t obj)
{
    assert(m_stop_the_world);
    if (CELLP(obj)) {
        if (m_shade_queue.wait_lock_try_put(obj) == false) {
            m_collector_lock.lock();
            m_collector_wake.signal(); // kick now
            m_collector_lock.unlock();
            GC_TRACE(";; [shade queue overflow while queueing root set]\n");
            m_shade_queue.put(obj);
        }
    }
}

void
object_heap_t::synchronized_collect(object_heap_t& heap)
{
    heap.m_trip_bytes = 0;
    heap.shade(heap.m_system_environment);
    heap.shade(heap.m_interaction_environment);
    heap.shade(heap.m_architecture_feature);
    heap.shade(heap.m_native_transcoder);
    heap.shade(heap.m_trampolines);
    for (int i = 0; i < array_sizeof(heap.m_inherents); i++) heap.shade(heap.m_inherents[i]);

    // mark
    assert(heap.m_mutator_stopped == false);
    heap.m_root_snapshot = ROOT_SNAPSHOT_EVERYTHING;
    heap.m_stop_the_world = true;
    GC_TRACE(";; [collector: stop-the-world]\n");
    while (!heap.m_mutator_stopped) {
        heap.m_collector_wake.wait(heap.m_collector_lock);
        if (!heap.m_mutator_stopped) {
            heap.dequeue_root();
            heap.m_mutator_wake.signal();
        }
    }
    double t1 = msec();
    GC_TRACE(";; [collector: mark]\n");
    heap.dequeue_root();
    while (heap.serial_marking()) continue;

    // sweep
    double t2 = msec();
    GC_TRACE(";; [collector: sweep]\n");
    heap.m_sweep_wavefront = (uint8_t*)heap.m_pool;
    heap.m_symbol.sweep();
    heap.m_string.sweep();
    heap.m_weakmappings.m_lock.lock();
    if (heap.m_weakmappings.m_vacant) {
        object_slab_traits_t* traits = heap.m_weakmappings.m_vacant;
        do heap.break_weakmapping(traits); while ((traits = traits->next) != heap.m_weakmappings.m_vacant);
    }
    if (heap.m_weakmappings.m_occupied) {
        object_slab_traits_t* traits = heap.m_weakmappings.m_occupied;
        do heap.break_weakmapping(traits); while ((traits = traits->next) != heap.m_weakmappings.m_occupied);
    }
    heap.m_weakmappings.m_lock.unlock();
    object_slab_traits_t* traits = OBJECT_SLAB_TRAITS_OF(heap.m_pool);
    for (int i = 0; i < heap.m_pool_watermark; i++) {
        if (GCSLABP(heap.m_pool[i])) {
            uint8_t* slab = heap.m_pool + (i << OBJECT_SLAB_SIZE_SHIFT);
            traits->cache->sweep(slab);
        }
        traits = (object_slab_traits_t*)((intptr_t)traits + OBJECT_SLAB_SIZE);
    }

    GC_TRACE(";; [collector: start-the-world]\n");
    heap.m_stop_the_world = false;
    heap.m_sweep_wavefront = (uint8_t*)heap.m_pool + heap.m_pool_size;
    heap.m_mutator_wake.signal();
    while (heap.m_mutator_stopped) {
        heap.m_collector_wake.wait(heap.m_collector_lock);
    }

    // end
    heap.m_collector_kicked = false;
    GC_TRACE(";; [collector: waiting]\n");
    double t3 = msec();

    heap.m_usage.m_duration = t3 - t1;
    heap.m_usage.m_sync1 = 0;
    heap.m_usage.m_sync2 = 0;
    heap.m_usage.m_recorded = true;
    heap.m_usage.m_synchronized = true;
}

void
object_heap_t::concurrent_collect(object_heap_t& heap)
{
    assert(heap.m_mutator_stopped == false);

    // mark phase 1
    heap.m_root_snapshot = ROOT_SNAPSHOT_GLOBALS;
    heap.m_stop_the_world = true;
    GC_TRACE(";; [collector: stop-the-world]\n");
    while (!heap.m_mutator_stopped) {
        heap.m_collector_wake.wait(heap.m_collector_lock);
        if (!heap.m_mutator_stopped) {
            heap.dequeue_root();
            heap.m_mutator_wake.signal();
        }
    }
    double t1 = msec();
    heap.m_trip_bytes = 0;
    heap.m_write_barrier = true;
    heap.m_stop_the_world = false;
    heap.m_mutator_wake.signal();
    GC_TRACE(";; [collector: start-the-world phase 1]\n");
    while (heap.m_mutator_stopped) {
        heap.m_collector_wake.wait(heap.m_collector_lock);
    }
    double t2 = msec();
    GC_TRACE(";; [collector: concurrent-marking phase 1]\n");
    heap.concurrent_marking();

    // mark phase 1+
    heap.shade(heap.m_system_environment);
    heap.shade(heap.m_interaction_environment);
    heap.shade(heap.m_architecture_feature);
    heap.shade(heap.m_native_transcoder);
    heap.shade(heap.m_trampolines);
    for (int i = 0; i < array_sizeof(heap.m_inherents); i++) heap.shade(heap.m_inherents[i]);
    heap.concurrent_marking();

    // mark phase 2
    heap.m_root_snapshot = ROOT_SNAPSHOT_LOCALS;
    heap.m_stop_the_world = true;
    GC_TRACE(";; [collector: stop-the-world phase 2]\n");
    while (!heap.m_mutator_stopped) {
        heap.m_collector_wake.wait(heap.m_collector_lock);
        if (!heap.m_mutator_stopped) {
            heap.dequeue_root();
            heap.m_mutator_wake.signal();
        }
    }

fallback:
    heap.m_stop_the_world = false;
    heap.m_mutator_wake.signal();
    GC_TRACE(";; [collector: start-the-world phase 2]\n");
    while (heap.m_mutator_stopped) {
        heap.m_collector_wake.wait(heap.m_collector_lock);
    }
    GC_TRACE(";; [collector: concurrent-marking phase 2]\n");
    heap.concurrent_marking();
    double t3 = msec();

    // final mark
    assert(heap.m_mutator_stopped == false);
    heap.m_root_snapshot = ROOT_SNAPSHOT_EVERYTHING;
    heap.m_stop_the_world = true;
    GC_TRACE(";; [collector: stop-the-world final]\n");

    while (!heap.m_mutator_stopped) {
        heap.m_collector_wake.wait(heap.m_collector_lock);
        if (!heap.m_mutator_stopped) {
            heap.dequeue_root();
            heap.m_mutator_wake.signal();
        }
    }
    double t4 = msec();
    heap.m_write_barrier = false;
    GC_TRACE(";; [collector: serial-marking]\n");
    heap.dequeue_root();

#ifdef ENSURE_REALTIME
    if (heap.serial_marking()) {
        #if DEBUG_CONCURRENT_COLLECT
        puts("serial_marking() timeout, resume mutator and restart concurrent_marking");
        #endif
        goto fallback;
    }
#else
    while (heap.serial_marking()) continue;
#endif

    // sweep
    heap.m_sweep_wavefront = (uint8_t*)heap.m_pool;
    heap.m_alloc_barrier = true;
    heap.m_read_barrier = true;
    heap.m_stop_the_world = false;
    heap.m_mutator_wake.signal();
    while (heap.m_mutator_stopped) {
        heap.m_collector_wake.wait(heap.m_collector_lock); // to make mutator run now
    }
    GC_TRACE(";; [collector: start-the-world]\n");
    GC_TRACE(";; [collector: concurrent-sweeping]\n");
    double t5 = msec();
    heap.m_symbol.sweep();
    heap.m_string.sweep();
    heap.m_read_barrier = false;
    heap.m_weakmappings.m_lock.lock();
    if (heap.m_weakmappings.m_vacant) {
        object_slab_traits_t* traits = heap.m_weakmappings.m_vacant;
        do heap.break_weakmapping(traits); while ((traits = traits->next) != heap.m_weakmappings.m_vacant);
    }
    if (heap.m_weakmappings.m_occupied) {
        object_slab_traits_t* traits = heap.m_weakmappings.m_occupied;
        do heap.break_weakmapping(traits); while ((traits = traits->next) != heap.m_weakmappings.m_occupied);
    }
    heap.m_weakmappings.m_lock.unlock();
    int capacity = (heap.m_pool_size >> OBJECT_SLAB_SIZE_SHIFT);
    uint8_t* slab = heap.m_pool;
    int i = 0;
    while (i < capacity) {
        int memo = heap.m_pool_usage;
        if (GCSLABP(heap.m_pool[i])) {
            
            if (OBJECT_SLAB_TRAITS_OF(slab)->cache == NULL) {
#if HPDEBUG
                printf(";; [collector: wait for mutator complete slab init]\n");
                fflush(stdout);
#endif                
#if _MSC_VER
                Sleep(0);
#else
                sched_yield();
#endif
                continue;
            }
#if HPDEBUG
            {
                object_slab_cache_t* ca = OBJECT_SLAB_TRAITS_OF(slab)->cache;
                bool hit = false;
                for (int u = 0; u < array_sizeof(heap.m_collectibles); u++) hit |= (&heap.m_collectibles[u] == ca);
                hit |= (&heap.m_weakmappings == ca);
                hit |= (&heap.m_flonums == ca);
                hit |= (&heap.m_cons == ca);
                if (! hit) fatal("%s:%u concurrent_collect(): bad cache reference 0x%x in slab 0x%x", __FILE__, __LINE__, ca, slab);
            }
#endif
            OBJECT_SLAB_TRAITS_OF(slab)->cache->sweep(slab);
            slab += OBJECT_SLAB_SIZE;
            i++;
        } else {
            scoped_lock lock(heap.m_lock);
            if (memo != heap.m_pool_usage) continue;
            do {
                if (i == heap.m_pool_watermark) {
                    heap.m_sweep_wavefront = (uint8_t*)heap.m_pool + heap.m_pool_size;
                    heap.m_alloc_barrier = false;
                    goto finish;
                }
                slab += OBJECT_SLAB_SIZE;
                heap.m_sweep_wavefront = slab;
                i++;
            } while (!GCSLABP(heap.m_pool[i]));
        }
    }

finish:
    heap.m_collector_kicked = false;
    GC_TRACE(";; [collector: waiting]\n");
    double t6 = msec();
    heap.m_usage.m_duration = t6 - t1;
    heap.m_usage.m_sync1 = t2 - t1;
    heap.m_usage.m_sync2 = t5 - t4;
    heap.m_usage.m_recorded = true;
    heap.m_usage.m_synchronized = false;
#if DEBUG_CONCURRENT_COLLECT
    printf( ";; [        first-lock:%.2fms second-lock:%.2fms overlap:%.2fms]\n"
            ";; [        stw:%.2fms concurrent-marking:%.2fms]\n"
            ";; [        stw:%.2fms serial-marking:%.2fms]\n"
            ";; [        concurrent-sweeping:%.2fms]\n",
            (t2 - t1), (t4 - t3) + (t5 - t4), (t3 - t2) + (t6 - t5),
            t2 - t1, t3 - t2,
            t4 - t3, t5 - t4,
            t6 - t5);
    fflush(stdout);
#endif
#if HPDEBUG
    heap.consistency_check();
#endif    
}

#if _MSC_VER
  unsigned int __stdcall
#else
  void*
#endif
object_heap_t::collector_thread(void* param)
{
    object_heap_t& heap = *(object_heap_t*)param;
    heap.m_collector_lock.lock();
    heap.m_collector_ready = true;
    GC_TRACE(";; [collector: ready]\n");
    while (true) {
        while (heap.m_collector_kicked == false) {
            heap.m_collector_wake.wait(heap.m_collector_lock);
        }
        assert(heap.m_mark_sp == heap.m_mark_stack);
        if (heap.m_mark_stack_size != MARK_STACK_SIZE_INIT) {
            heap.m_mark_stack_size = MARK_STACK_SIZE_INIT;
            heap.m_mark_stack = heap.m_mark_sp = (scm_obj_t*)realloc(heap.m_mark_stack, sizeof(scm_obj_t) * heap.m_mark_stack_size);
        }
        if (CONCURRENT_COLLECT) {
            if (heap.m_pool_usage > heap.m_pool_threshold) {
                synchronized_collect(heap);
            } else {
                concurrent_collect(heap);
            }
        } else {
            synchronized_collect(heap);
        }
    }
    return NULL;
}

void
object_heap_t::trace(scm_obj_t obj)
{
    assert(is_collectible(obj));
    object_slab_traits_t* traits = OBJECT_SLAB_TRAITS_OF(obj);
    if (traits->cache->test_and_mark(obj)) {
#if HPDEBUG
        printf(";; [collector: duplicate objects in mark stack]\n");
        fflush(stdout);
#endif                
        return;
    }
    if (PAIRP(obj)) {
        scm_pair_t pair = (scm_pair_t)obj;
        shade(pair->cdr);
        shade(pair->car);
        return;
    }
    int tc = HDR_TC(HDR(obj));
    assert(tc >= 0);
    assert(tc <= TC_MASKBITS);
    switch (tc) {
        case TC_VECTOR: {
            scm_vector_t vector = (scm_vector_t)obj;
            int count = vector->count;
            for (int i = 0; i < count; i++) shade(vector->elts[i]);
            break;
        }
        case TC_TUPLE: {
            scm_tuple_t tuple = (scm_tuple_t)obj;
            int count = HDR_TUPLE_COUNT(tuple->hdr);
            for (int i = 0; i < count; i++) shade(tuple->elts[i]);
            break;
        }
        case TC_VALUES: {
            scm_values_t values = (scm_values_t)obj;
            int count = HDR_VALUES_COUNT(values->hdr);
            for (int i = 0; i < count; i++) shade(values->elts[i]);
            break;
        }
        case TC_HASHTABLE: {
            scm_hashtable_t ht = (scm_hashtable_t)obj;
            shade(ht->handlers);
            hashtable_rec_t* ht_datum = ht->datum;
            if (ht_datum) {
                int nsize = ht_datum->capacity;
                for (int i = 0; i < nsize; i++) {
                    shade(ht_datum->elts[i]);
                }
                for (int i = 0; i < nsize; i++) {
                    shade(ht_datum->elts[i + nsize]);
                }
            }
            break;
        }
        case TC_WEAKHASHTABLE: {
            scm_weakhashtable_t ht = (scm_weakhashtable_t)obj;
            weakhashtable_rec_t* ht_datum = ht->datum;
            int nsize = ht_datum->capacity;
            for (int i = 0; i < nsize; i++) {
                if (WEAKMAPPINGP(ht_datum->elts[i])) {
                    scm_weakmapping_t wmap = (scm_weakmapping_t)ht_datum->elts[i];
                    if (wmap->key == scm_false) {
                        ht_datum->elts[i] = scm_hash_deleted;
                        ht_datum->live--;
                    } else {
                        shade(wmap);
                    }
                }
            }
            break;
        }
        case TC_PORT: {
            scm_port_t port = (scm_port_t)obj;
            shade(port->bytes);
            shade(port->name);
            shade(port->transcoder);
            shade(port->handlers);
            break;
        }
        case TC_COMPLEX: {
            scm_complex_t complex = (scm_complex_t)obj;
            shade(complex->imag);
            shade(complex->real);
            break;
        }
        case TC_RATIONAL: {
            scm_rational_t rational = (scm_rational_t)obj;
            shade(rational->nume);
            shade(rational->deno);
            break;
        }
        case TC_CLOSURE: {
            scm_closure_t closure = (scm_closure_t)obj;
            shade(closure->code);
            shade(closure->doc);
            interior_shade(closure->env);
            break;
        }
        case TC_CONT: {
            scm_cont_t cont = (scm_cont_t)obj;
            shade(cont->wind_rec);
            interior_shade(cont->cont);
            break;
        }
        case TC_HEAPENV: {
            int nbytes = HDR_HEAPENV_SIZE(HDR(obj));
            uint8_t* top = (uint8_t*)((intptr_t)obj + sizeof(scm_hdr_t));
            vm_env_t env = (vm_env_t)(top + nbytes - sizeof(vm_env_rec_t));
            interior_shade(env->up);
            for (scm_obj_t* vars = (scm_obj_t*)top; vars < (scm_obj_t*)env; vars++) shade(*vars);
            break;
        }
        case TC_HEAPCONT: {
            int nbytes = HDR_HEAPCONT_SIZE(HDR(obj));
            uint8_t* top = (uint8_t*)((intptr_t)obj + sizeof(scm_hdr_t));
            vm_cont_t cont = (vm_cont_t)(top + nbytes - sizeof(vm_cont_rec_t));
            interior_shade(cont->up);
            interior_shade(cont->env);
            shade(cont->pc);
            shade(cont->trace);
            for (scm_obj_t* args = (scm_obj_t*)top; args < (scm_obj_t*)cont; args++) shade(*args);
            break;
        }
        case TC_ENVIRONMENT: {
            scm_environment_t environment = (scm_environment_t)obj;
            shade(environment->variable);
            shade(environment->macro);
            shade(environment->name);
            break;
        }
        case TC_GLOC: {
            scm_gloc_t gloc = (scm_gloc_t)obj;
            shade(gloc->variable);
  #if GLOC_DEBUG_INFO
            shade(gloc->environment);
  #endif
            shade(gloc->value);
            break;
        }
        case TC_SUBR: {
            scm_subr_t subr = (scm_subr_t)obj;
            shade(subr->doc);
            break;
        }
        case TC_WEAKMAPPING: {
            scm_weakmapping_t wmap = (scm_weakmapping_t)obj;
            shade(wmap->value);
            break;
        }
    }
}

void
object_heap_t::concurrent_marking()
{
    scm_obj_t obj;
    do {
        while (true) {
            if (m_shade_queue.try_get(&obj)) shade(obj);
            if (m_mark_sp == m_mark_stack) break;
            obj = *--m_mark_sp;
            trace(obj);
        }
    } while (m_shade_queue.count());
}

bool
object_heap_t::serial_marking()
{
#ifdef ENSURE_REALTIME
    double timeout = msec() + ENSURE_REALTIME;
    int i = 0;
    scm_obj_t obj;
    while (m_mark_sp != m_mark_stack) {
        obj = *--m_mark_sp;
        trace(obj);
        if (++i > TIMEOUT_CHECK_EACH) {
            i = 0;
            if (msec() > timeout) return true;
        }
    }
    return false;
#else
    scm_obj_t obj;
    while (m_mark_sp != m_mark_stack) {
        obj = *--m_mark_sp;
        trace(obj);
    }
    return false;
#endif
}

typedef struct {
    int     pair;
    int     tc[TC_MASKBITS + 1];
} object_count_t;

static void
accumulate_object_count(void* obj, int size, void* refcon)
{
    object_count_t* count = (object_count_t*)refcon;
    if (PAIRP(obj)) {
        count->pair++;
    } else {
        int tc = HDR_TC(HDR(obj));
        assert(tc >= 0);
        assert(tc < array_sizeof(count->tc));
        count->tc[tc]++;
    }
}

void
object_heap_t::display_object_statistics(scm_port_t port)
{
    object_count_t count;
    memset(&count, 0, sizeof(count));
    object_slab_traits_t* traits = OBJECT_SLAB_TRAITS_OF(m_pool);
    for (int i = 0; i < m_pool_watermark; i++) {
        if (GCSLABP(m_pool[i])) {
            traits->cache->iterate(m_pool + (i << OBJECT_SLAB_SIZE_SHIFT), accumulate_object_count, &count);
        }
        traits = (object_slab_traits_t*)((intptr_t)traits + OBJECT_SLAB_SIZE);
    }
    scoped_lock lock(port->lock);

#define PRINT(name,n) port_format(port, "%15s: %9d\n",#name, count.tc[n])
    port_put_byte(port, '\n');
    port_format(port, "           pair: %9d\n", count.pair);
    PRINT(symbol, TC_SYMBOL);
    PRINT(string, TC_STRING);
    PRINT(bignum, TC_BIGNUM);
    PRINT(flonum, TC_FLONUM);
    PRINT(cont, TC_CONT);
    PRINT(closure, TC_CLOSURE);
    PRINT(subr, TC_SUBR);
    PRINT(vector, TC_VECTOR);
    PRINT(port, TC_PORT);
    PRINT(values, TC_VALUES);
    PRINT(hashtable, TC_HASHTABLE);
    PRINT(complex, TC_COMPLEX);
    PRINT(rational, TC_RATIONAL);
    PRINT(heapenv, TC_HEAPENV);
    PRINT(heapcont, TC_HEAPCONT);
    PRINT(environment, TC_ENVIRONMENT);
    PRINT(gloc, TC_GLOC);
    PRINT(tuple, TC_TUPLE);
    PRINT(weakmapping, TC_WEAKMAPPING);
    PRINT(weakhashtable, TC_WEAKHASHTABLE);
    PRINT(bvector, TC_BVECTOR);
    port_put_byte(port, '\n');
    port_flush_output(port);
#undef PRINT
}

void
object_heap_t::display_heap_statistics(scm_port_t port)
{
    int n_free = 0;
    int n_general = 0;
    int n_slab = 0;
    int n_gcslab = 0;

    scoped_lock lock(port->lock);
    port_put_byte(port, '\n');
    object_slab_traits_t* traits;
    for (int n = 0; n < m_pool_watermark; n++) {
        if ((n & 63) == 0) port_puts(port, "  |");
        switch(m_pool[n]) {
        case PTAG_FREE: port_put_byte(port, ' '); n_free++; break;
        case PTAG_USED: port_put_byte(port, 'P'); n_general++; break;
        case PTAG_USED|PTAG_SLAB:
            traits = OBJECT_SLAB_TRAITS_OF(m_pool + (n << OBJECT_SLAB_SIZE_SHIFT));
            if (traits->free) port_put_byte(port, 's');
            else port_put_byte(port, 'S');
            n_slab++;
            break;
        case PTAG_USED|PTAG_SLAB|PTAG_GC:
            traits = OBJECT_SLAB_TRAITS_OF(m_pool + (n << OBJECT_SLAB_SIZE_SHIFT));
            if (traits->refc == 0) {
                port_put_byte(port, '.');
            } else {
                if (traits->free) port_put_byte(port, 'o');
                else port_put_byte(port, 'O');
            }
            n_gcslab++;
            break;
        case PTAG_EXTENT: port_put_byte(port, '-'); n_general++; break;
        case PTAG_EXTENT|PTAG_SLAB: port_put_byte(port, '?'); n_slab++; break;
        case PTAG_EXTENT|PTAG_SLAB|PTAG_GC: port_put_byte(port, '?'); n_slab++; break;
        }
        if ((n & 63) == 63) port_puts(port, "|\n");
    }
    if ((m_pool_watermark & 63) != 0) port_puts(port, "|\n");
    port_format(port, "  object:%d static:%d page:%d free:%d", n_gcslab, n_slab, n_general, n_free);
    port_format(port, " watermark:%d limit:%d\n\n",m_pool_watermark, (m_pool_size >> OBJECT_SLAB_SIZE_SHIFT));
    port_flush_output(port);
}


void
object_heap_t::init_inherents()
{
    make_symbol_inherent(this, "const", VMOP_CONST);
    make_symbol_inherent(this, "const.unspec", VMOP_CONST_UNSPEC);
    make_symbol_inherent(this, "const.undef", VMOP_CONST_UNDEF);
    make_symbol_inherent(this, "gloc.of", VMOP_GLOC_OF);
    make_symbol_inherent(this, "gloc", VMOP_GLOC);
    make_symbol_inherent(this, "iloc", VMOP_ILOC);
    make_symbol_inherent(this, "car.iloc", VMOP_CAR_ILOC);
    make_symbol_inherent(this, "cdr.iloc", VMOP_CDR_ILOC);
    make_symbol_inherent(this, "cadr.iloc", VMOP_CADR_ILOC);
    make_symbol_inherent(this, "cddr.iloc", VMOP_CDDR_ILOC);
    make_symbol_inherent(this, "close", VMOP_CLOSE);
    make_symbol_inherent(this, "ret.const", VMOP_RET_CONST);
    make_symbol_inherent(this, "ret.const.unspec", VMOP_RET_CONST_UNSPEC);
    make_symbol_inherent(this, "ret.const.undef", VMOP_RET_CONST_UNDEF);
    make_symbol_inherent(this, "ret.gloc.of", VMOP_RET_GLOC_OF);
    make_symbol_inherent(this, "ret.gloc", VMOP_RET_GLOC);
    make_symbol_inherent(this, "ret.iloc", VMOP_RET_ILOC);
    make_symbol_inherent(this, "ret.close", VMOP_RET_CLOSE);
    make_symbol_inherent(this, "push.const", VMOP_PUSH_CONST);
    make_symbol_inherent(this, "push.const.unspec", VMOP_PUSH_CONST_UNSPEC);
    make_symbol_inherent(this, "push.const.undef", VMOP_PUSH_CONST_UNDEF);
    make_symbol_inherent(this, "push.gloc.of", VMOP_PUSH_GLOC_OF);
    make_symbol_inherent(this, "push.gloc", VMOP_PUSH_GLOC);
    make_symbol_inherent(this, "push.iloc", VMOP_PUSH_ILOC);
    make_symbol_inherent(this, "push.car.iloc", VMOP_PUSH_CAR_ILOC);
    make_symbol_inherent(this, "push.cdr.iloc", VMOP_PUSH_CDR_ILOC);
    make_symbol_inherent(this, "push.cadr.iloc", VMOP_PUSH_CADR_ILOC);
    make_symbol_inherent(this, "push.cddr.iloc", VMOP_PUSH_CDDR_ILOC);
    make_symbol_inherent(this, "push.close", VMOP_PUSH_CLOSE);
    make_symbol_inherent(this, "push.close+", VMOP_PUSH_CLOSE_LOCAL);
    make_symbol_inherent(this, "push", VMOP_PUSH);
    make_symbol_inherent(this, "set.gloc.of", VMOP_SET_GLOC_OF);
    make_symbol_inherent(this, "set.gloc", VMOP_SET_GLOC);
    make_symbol_inherent(this, "set.iloc", VMOP_SET_ILOC);
    make_symbol_inherent(this, "if.true", VMOP_IF_TRUE);
    make_symbol_inherent(this, "if.false.call", VMOP_IF_FALSE_CALL);
    make_symbol_inherent(this, "if.true.ret", VMOP_IF_TRUE_RET);
    make_symbol_inherent(this, "if.false.ret", VMOP_IF_FALSE_RET);
    make_symbol_inherent(this, "call", VMOP_CALL);
    make_symbol_inherent(this, "apply.gloc.of", VMOP_APPLY_GLOC_OF);
    make_symbol_inherent(this, "apply.gloc", VMOP_APPLY_GLOC);
    make_symbol_inherent(this, "apply.iloc", VMOP_APPLY_ILOC);
    make_symbol_inherent(this, "apply.iloc+", VMOP_APPLY_ILOC_LOCAL);
    make_symbol_inherent(this, "apply", VMOP_APPLY);
    make_symbol_inherent(this, "extend", VMOP_EXTEND);
    make_symbol_inherent(this, "enclose", VMOP_ENCLOSE);
    make_symbol_inherent(this, "touch.gloc.of", VMOP_TOUCH_GLOC_OF);
    make_symbol_inherent(this, "touch.gloc", VMOP_TOUCH_GLOC);
    make_symbol_inherent(this, "subr.gloc.of", VMOP_SUBR_GLOC_OF);
    make_symbol_inherent(this, "subr", VMOP_SUBR);
    make_symbol_inherent(this, "extend.unbound", VMOP_EXTEND_UNBOUND);
    make_symbol_inherent(this, "extend.enclose", VMOP_EXTEND_ENCLOSE);
    make_symbol_inherent(this, "extend.enclose+", VMOP_EXTEND_ENCLOSE_LOCAL);
    make_symbol_inherent(this, "vm.escape", VMOP_VM_ESCAPE);
    make_symbol_inherent(this, "push.iloc.0", VMOP_PUSH_ILOC0);
    make_symbol_inherent(this, "push.iloc.1", VMOP_PUSH_ILOC1);
    make_symbol_inherent(this, "iloc.0", VMOP_ILOC0);
    make_symbol_inherent(this, "iloc.1", VMOP_ILOC1);
    make_symbol_inherent(this, "ret.subr.gloc.of", VMOP_RET_SUBR_GLOC_OF);
    make_symbol_inherent(this, "ret.subr", VMOP_RET_SUBR);
    make_symbol_inherent(this, "push.subr", VMOP_PUSH_SUBR);
    make_symbol_inherent(this, "push.subr.gloc.of", VMOP_PUSH_SUBR_GLOC_OF);
    make_symbol_inherent(this, "if.null?", VMOP_IF_NULLP);
    make_symbol_inherent(this, "if.null?.ret.const", VMOP_IF_NULLP_RET_CONST);
    make_symbol_inherent(this, "if.not.null?.ret.const", VMOP_IF_NOT_NULLP_RET_CONST);
    make_symbol_inherent(this, "if.pair?", VMOP_IF_PAIRP);
    make_symbol_inherent(this, "if.pair?.ret.const", VMOP_IF_PAIRP_RET_CONST);
    make_symbol_inherent(this, "if.not.pair?.ret.const", VMOP_IF_NOT_PAIRP_RET_CONST);
    make_symbol_inherent(this, "if.symbol?", VMOP_IF_SYMBOLP);
    make_symbol_inherent(this, "if.symbol?.ret.const", VMOP_IF_SYMBOLP_RET_CONST);
    make_symbol_inherent(this, "if.not.symbol?.ret.const", VMOP_IF_NOT_SYMBOLP_RET_CONST);
    make_symbol_inherent(this, "if.eq?", VMOP_IF_EQP);
    make_symbol_inherent(this, "if.eq?.ret.const", VMOP_IF_EQP_RET_CONST);
    make_symbol_inherent(this, "if.not.eq?.ret.const", VMOP_IF_NOT_EQP_RET_CONST);
    make_symbol_inherent(this, "if.true.ret.const", VMOP_IF_TRUE_RET_CONST);
    make_symbol_inherent(this, "if.false.ret.const", VMOP_IF_FALSE_RET_CONST);
    make_symbol_inherent(this, "ret.cons", VMOP_RET_CONS);
    make_symbol_inherent(this, "ret.eq?", VMOP_RET_EQP);
    make_symbol_inherent(this, "ret.null?", VMOP_RET_NULLP);
    make_symbol_inherent(this, "ret.pair?", VMOP_RET_PAIRP);
    make_symbol_inherent(this, "push.cons", VMOP_PUSH_CONS);
    make_symbol_inherent(this, "push.n+.iloc", VMOP_PUSH_NADD_ILOC);
    make_symbol_inherent(this, "n+.iloc", VMOP_NADD_ILOC);
    make_symbol_inherent(this, "=n.iloc", VMOP_EQ_N_ILOC);
    make_symbol_inherent(this, "<n.iloc", VMOP_LT_N_ILOC);
    make_symbol_inherent(this, "<=n.iloc", VMOP_LE_N_ILOC);
    make_symbol_inherent(this, ">n.iloc", VMOP_GT_N_ILOC);
    make_symbol_inherent(this, ">=n.iloc", VMOP_GE_N_ILOC);
    make_symbol_inherent(this, "=.iloc", VMOP_EQ_ILOC);
    make_symbol_inherent(this, "<.iloc", VMOP_LT_ILOC);
    make_symbol_inherent(this, "<=.iloc", VMOP_LE_ILOC);
    make_symbol_inherent(this, ">.iloc", VMOP_GT_ILOC);
    make_symbol_inherent(this, ">=.iloc", VMOP_GE_ILOC);
    make_symbol_inherent(this, "little", S_CODE_LITTLE);
    make_symbol_inherent(this, "big", S_CODE_BIG);
    make_symbol_inherent(this, "quote", S_CODE_QUOTE);
    make_symbol_inherent(this, "quasiquote", S_CODE_QUASIQUOTE);
    make_symbol_inherent(this, "unquote", S_CODE_UNQUOTE);
    make_symbol_inherent(this, "unquote-splicing", S_CODE_UNQUOTE_SPLICING);
    make_symbol_inherent(this, "syntax", S_CODE_SYNTAX);
    make_symbol_inherent(this, "quasisyntax", S_CODE_QUASISYNTAX);
    make_symbol_inherent(this, "unsyntax", S_CODE_UNSYNTAX);
    make_symbol_inherent(this, "unsyntax-splicing", S_CODE_UNSYNTAX_SPLICING);
    make_symbol_inherent(this, "(", S_CODE_LPAREN);
    make_symbol_inherent(this, ")", S_CODE_RPAREN);
    make_symbol_inherent(this, "[", S_CODE_LBRACK);
    make_symbol_inherent(this, "]", S_CODE_RBRACK);
    make_symbol_inherent(this, ".", S_CODE_DOT);
    {
        scm_string_t obj = (scm_string_t)allocate_collectible(sizeof(scm_string_rec_t));
        obj->hdr = scm_hdr_string | (0 << HDR_STRING_SIZE_SHIFT);
        obj->name = (char*)allocate_private(1);
        obj->name[0] = 0;
        m_inherents[NIL_STRING] = obj;
    }
    {
        assert(INTERNAL_PRIVATE_THRESHOLD >= sizeof(scm_vector_rec_t) + sizeof(scm_obj_t));
        scm_vector_t obj = (scm_vector_t)allocate_collectible(sizeof(scm_vector_rec_t) + sizeof(scm_obj_t));
        obj->hdr = scm_hdr_vector;
        obj->count = 0;
        obj->elts = (scm_obj_t*)((uintptr_t)obj + sizeof(scm_vector_rec_t));
        obj->elts[0] = scm_unspecified;
        m_inherents[NIL_VECTOR] = obj;
    }
    {
        scm_bvector_t obj = (scm_bvector_t)allocate_collectible(sizeof(scm_bvector_rec_t));
        obj->hdr = scm_hdr_bvector;
        obj->count = 0;
        obj->elts = (uint8_t*)allocate_private(1);
        obj->elts[0] = 0;
        m_inherents[NIL_BVECTOR] = obj;
    }
    {
        assert(INTERNAL_PRIVATE_THRESHOLD >= sizeof(scm_tuple_rec_t) + sizeof(scm_obj_t));
        scm_tuple_t obj = (scm_tuple_t)allocate_collectible(sizeof(scm_tuple_rec_t) + sizeof(scm_obj_t));
        obj->hdr = scm_hdr_tuple | (0 << HDR_TUPLE_COUNT_SHIFT);
        obj->elts = (scm_obj_t*)((uintptr_t)obj + sizeof(scm_tuple_rec_t));
        obj->elts[0] = scm_unspecified;
        m_inherents[NIL_TUPLE] = obj;
    }
#if USE_FLONUM_CONST
    {
        scm_flonum_t obj = (scm_flonum_t)allocate_collectible(sizeof(scm_flonum_rec_t));
        obj->hdr = scm_hdr_flonum;
        obj->value = 0.0;
        m_inherents[FL_POSITIVE_ZERO] = obj;
    }
    {
        scm_flonum_t obj = (scm_flonum_t)allocate_collectible(sizeof(scm_flonum_rec_t));
        obj->hdr = scm_hdr_flonum;
        obj->value = - 0.0;
        m_inherents[FL_NEGATIVE_ZERO] = obj;
    }
    {
        scm_flonum_t obj = (scm_flonum_t)allocate_collectible(sizeof(scm_flonum_rec_t));
        obj->hdr = scm_hdr_flonum;
        obj->value = VALUE_NAN;
        m_inherents[FL_NAN] = obj;
    }
#endif
}

#if HPDEBUG

static const char*
verify_obj(void* obj, object_heap_t* heap)
{
    static char msg[256];
    if (CELLP(obj)) {
        if (heap->is_collectible(obj)) {
            if (PAIRP(obj)) return NULL;
            int tc = HDR_TC(HDR(obj));
            if (tc >= 0 || tc <= TC_MASKBITS) return NULL;
            snprintf(msg, sizeof(msg), "have invalid TC %d\n", tc);
            return msg;
        }
        snprintf(msg, sizeof(msg), "out of GCSLAB");
        return msg;
    }
    return NULL;
}

static const char*
verify_interior_obj(void* ref, object_heap_t* heap)
{
    if (ref == NULL) return NULL;
    return verify_obj(OBJECT_SLAB_TRAITS_OF(ref)->cache->lookup(ref), heap);
}

static void
check_collectible(void* obj, int size, void* refcon)
{
    #define VERIFY_OBJ(OBJ, SLOT) \
        do { \
            const char* msg = verify_obj((OBJ)->SLOT, heap); \
            if (msg) fatal("bad %s 0x%x %s->%s 0x%x %s\n", #OBJ, OBJ, #OBJ, #SLOT, (OBJ)->SLOT, msg); \
        } while (false);
        
    #define VERIFY_INTERIOR_OBJ(OBJ, SLOT) \
    do { \
        const char* msg = verify_interior_obj((OBJ)->SLOT, heap); \
        if (msg) fatal("bad %s 0x%x %s->%s 0x%x %s\n", #OBJ, OBJ, #OBJ, #SLOT, (OBJ)->SLOT, msg); \
    } while (false);

    #define VERIFY_ELT(OBJ, ELT) \
    do { \
        const char* msg = verify_obj(ELT, heap); \
        if (msg) fatal("bad %s 0x%x %s 0x%x %s\n", #OBJ, OBJ, #ELT, ELT, msg); \
    } while (false);
    
    object_heap_t* heap = (object_heap_t*)refcon;
    
    if (!CELLP(obj)) return;
    if (!heap->is_collectible(obj)) {
        fatal("object 0x%x out of GCSLAB\n", obj);
    }
    if (PAIRP(obj)) {
        scm_pair_t pair = (scm_pair_t)obj;
        VERIFY_OBJ(pair, car);
        VERIFY_OBJ(pair, cdr);
        return;
    }
    int tc = HDR_TC(HDR(obj));
    if (tc < 0 || tc > TC_MASKBITS) {
        fatal("object 0x%x have invalid TC %d\n", obj, tc);
    }
    switch (tc) {
        case TC_VECTOR: {
            scm_vector_t vector = (scm_vector_t)obj;
            int count = vector->count;
            for (int i = 0; i < count; i++) VERIFY_OBJ(vector, elts[i]);
        } return;
        case TC_TUPLE: {
            scm_tuple_t tuple = (scm_tuple_t)obj;
            int count = HDR_TUPLE_COUNT(tuple->hdr);
            for (int i = 0; i < count; i++) VERIFY_OBJ(tuple, elts[i]);
        } return;
        case TC_VALUES: {
            scm_values_t values = (scm_values_t)obj;
            int count = HDR_VALUES_COUNT(values->hdr);
            for (int i = 0; i < count; i++) VERIFY_OBJ(values, elts[i]);
        } return;
        case TC_HASHTABLE: {
            scm_hashtable_t ht = (scm_hashtable_t)obj;
            scoped_lock lock(ht->lock);
            hashtable_rec_t* hashtable_datum = ht->datum;
            if (hashtable_datum) {
                int nsize = hashtable_datum->capacity;
                for (int i = 0; i < nsize * 2; i++) VERIFY_OBJ(hashtable_datum, elts[i]);
            }
        } return;
        case TC_WEAKHASHTABLE: {
            scm_weakhashtable_t ht = (scm_weakhashtable_t)obj;
            scoped_lock lock(ht->lock);
            weakhashtable_rec_t* weakhashtable_datum = ht->datum;
            int nsize = weakhashtable_datum->capacity;
            for (int i = 0; i < nsize; i++) VERIFY_OBJ(weakhashtable_datum, elts[i]);
        } return;
        case TC_PORT: {
            scm_port_t port = (scm_port_t)obj;
            VERIFY_OBJ(port, bytes);
            VERIFY_OBJ(port, name);
            VERIFY_OBJ(port, transcoder);
            VERIFY_OBJ(port, handlers);
        } return;
        case TC_COMPLEX: {
            scm_complex_t complex = (scm_complex_t)obj;
            VERIFY_OBJ(complex, imag);
            VERIFY_OBJ(complex, real);
        } return;
        case TC_RATIONAL: {
            scm_rational_t rational = (scm_rational_t)obj;
            VERIFY_OBJ(rational, nume);
            VERIFY_OBJ(rational, deno);
        } return;
        case TC_CLOSURE: {
            scm_closure_t closure = (scm_closure_t)obj;
            VERIFY_OBJ(closure, code);
            VERIFY_OBJ(closure, doc);
            VERIFY_INTERIOR_OBJ(closure, env);
        } return;
        case TC_CONT: {
            scm_cont_t cont = (scm_cont_t)obj;
            VERIFY_OBJ(cont, wind_rec);
            VERIFY_INTERIOR_OBJ(cont, cont);
        } return;
        case TC_HEAPENV: {
            int nbytes = HDR(obj) >> HDR_HEAPENV_SIZE_SHIFT;
            uint8_t* top = (uint8_t*)((intptr_t)obj + sizeof(scm_hdr_t));
            vm_env_t env = (vm_env_t)(top + nbytes - sizeof(vm_env_rec_t));
            VERIFY_INTERIOR_OBJ(env, up);
            for (scm_obj_t* vars = (scm_obj_t*)top; vars < (scm_obj_t*)env; vars++) {
                VERIFY_ELT(obj, *vars);
            }
        } return;
        case TC_HEAPCONT: {
             int nbytes = HDR(obj) >> HDR_HEAPCONT_SIZE_SHIFT;
             uint8_t* top = (uint8_t*)((intptr_t)obj + sizeof(scm_hdr_t));
             vm_cont_t cont = (vm_cont_t)(top + nbytes - sizeof(vm_cont_rec_t));            
             VERIFY_INTERIOR_OBJ(cont, up);
             VERIFY_INTERIOR_OBJ(cont, env);
             VERIFY_OBJ(cont, pc);
             VERIFY_OBJ(cont, trace);
             for (scm_obj_t* args = (scm_obj_t*)top; args < (scm_obj_t*)cont; args++) {
                 VERIFY_ELT(obj, *args);
             }
        } return;
        case TC_ENVIRONMENT: {
            scm_environment_t environment = (scm_environment_t)obj;
            VERIFY_OBJ(environment, variable);
            VERIFY_OBJ(environment, macro);
            VERIFY_OBJ(environment, name);
        } return;
        case TC_GLOC: {
            scm_gloc_t gloc = (scm_gloc_t)obj;            
            VERIFY_OBJ(gloc, variable);
            VERIFY_OBJ(gloc, value);
        } return;
        case TC_SUBR: {
            scm_subr_t subr = (scm_subr_t)obj;
            VERIFY_OBJ(subr, doc);
        } return;
        case TC_WEAKMAPPING: {
            scm_weakmapping_t wmap = (scm_weakmapping_t)obj;
            VERIFY_OBJ(wmap, key);
            VERIFY_OBJ(wmap, value);
        } return;
        case TC_SYMBOL: {
            //
        } return;
        case TC_STRING: {
            //
        } return;
        case TC_BVECTOR: {
            //
        } return;
        case TC_FLONUM: {
            //
        } return;
        case TC_BIGNUM: {
            //
        } return;
        default: {
            fatal("bad object 0x%x, unknown TC %d\n", obj, tc);
            return;
        }
    }
}

void
object_heap_t::consistency_check()
{
//    puts(";; [collector: heap check]");
    m_root_snapshot = ROOT_SNAPSHOT_CONSISTENCY_CHECK;
    m_stop_the_world = true;
    while (!m_mutator_stopped) {
        m_collector_wake.wait(m_collector_lock);
        if (!m_mutator_stopped) m_mutator_wake.signal();
    }
    object_slab_traits_t* traits = OBJECT_SLAB_TRAITS_OF(m_pool);
    for (int i = 0; i < m_pool_watermark; i++) {
        if (GCSLABP(m_pool[i])) {
            traits->cache->iterate(m_pool + (i << OBJECT_SLAB_SIZE_SHIFT), check_collectible, this);
        }
        traits = (object_slab_traits_t*)((intptr_t)traits + OBJECT_SLAB_SIZE);
    }
    m_stop_the_world = false;
    m_mutator_wake.signal();
    while (m_mutator_stopped) {
        m_collector_wake.wait(m_collector_lock);
    }
}

#endif
