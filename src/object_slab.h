/*
    Ypsilon Scheme System
    Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#ifndef OBJECT_SLAB_H_INCLUDED
#define OBJECT_SLAB_H_INCLUDED

#include "core.h"
#include "object.h"

class object_heap_t;

#if PARALLEL_COLLECT
#include "interlocked.h"
#endif

#if USE_SPINLOCK
#include "spinlock.h"
#endif

#define OBJECT_SLAB_TOP_OF(obj)     ((uint8_t*)(((uintptr_t)(obj)) & ~(OBJECT_SLAB_SIZE-1)))
#define OBJECT_SLAB_TRAITS_OF(obj)  ((object_slab_traits_t*)(OBJECT_SLAB_TOP_OF(obj) + OBJECT_SLAB_SIZE - sizeof(object_slab_traits_t)))

struct object_slab_cache_t;
struct object_slab_traits_t;
struct object_freelist_t;

struct object_freelist_t {
    void*                   null;   // <- object_slab_t::deallocate(...) assign NULL to detect free cell during sweep phase
    object_freelist_t*      next;
};

struct object_slab_traits_t {       // <- locate to bottom of each slab
    intptr_t                refc;
    object_freelist_t*      free;
    object_slab_traits_t*   next;
    object_slab_traits_t*   prev;
    object_slab_cache_t*    cache;
};

typedef void (*object_iter_proc_t)(void* obj, int size, void* refcon);

struct object_slab_cache_t {
#if USE_SPINLOCK
    spinlock_t              m_lock;
#else
    mutex_t                 m_lock;
#endif
    int                     m_object_size;
    int                     m_object_size_shift;
    int                     m_bitmap_size;
    int                     m_cache_limit;
    int                     m_cache_count;
    object_slab_traits_t*   m_vacant;
    object_slab_traits_t*   m_occupied;
    object_heap_t*          m_heap;
    object_slab_cache_t();
    ~object_slab_cache_t();
    bool    init(object_heap_t* object_heap, int object_size, bool gc);
    void    destroy();
    void*   new_collectible_object();
    void*   new_object();
    void    delete_object(void* obj);
    void    attach(void* slab);
    void    detach(void* slab);
    void    sweep(void *slab);
    void    iterate(void* slab, object_iter_proc_t proc, void* desc);
    void    init_freelist(uint8_t* top, uint8_t* bottom, object_slab_traits_t* traits);
    void    unload_filled(object_slab_traits_t* traits);

    void* lookup(void* ref)
    {
        assert((uint8_t*)ref < (uint8_t*)OBJECT_SLAB_TRAITS_OF(ref) - m_bitmap_size);
        assert((uint8_t*)ref >= (uint8_t*)OBJECT_SLAB_TOP_OF(ref));
        return (void*)((intptr_t)ref & ~(m_object_size - 1));
    }

    bool state(void* obj)
    {
        assert(m_bitmap_size);
        uint8_t* bitmap = (uint8_t*)OBJECT_SLAB_TRAITS_OF(obj) - m_bitmap_size;
        int bit_n = ((intptr_t)obj & (OBJECT_SLAB_SIZE - 1)) >> m_object_size_shift;
        assert(bit_n < m_bitmap_size * 8);
        return (bitmap[bit_n >> 3] & (1 << (bit_n & 7))) != 0;
    }

    void mark(void* obj)
    {
        assert(m_bitmap_size);
        uint8_t* bitmap = (uint8_t*)OBJECT_SLAB_TRAITS_OF(obj) - m_bitmap_size;
        int bit_n = ((intptr_t)obj & (OBJECT_SLAB_SIZE - 1)) >> m_object_size_shift;
        assert(bit_n < m_bitmap_size * 8);
#if PARALLEL_COLLECT
        interlocked_or_uint8(bitmap + (bit_n >> 3), 1 << (bit_n & 7));
#else
        bitmap[bit_n >> 3] |= (1 << (bit_n & 7));
#endif
    }

    bool test_and_mark(void* obj)
    {
        assert(m_bitmap_size);
        uint8_t* bitmap = (uint8_t*)OBJECT_SLAB_TRAITS_OF(obj) - m_bitmap_size;
        int bit_n = ((intptr_t)obj & (OBJECT_SLAB_SIZE - 1)) >> m_object_size_shift;
        assert(bit_n < m_bitmap_size * 8);
        uint8_t bit = (1 << (bit_n & 7));
        uint8_t* p = bitmap + (bit_n >> 3);
        if (*p & bit) return true;
#if PARALLEL_COLLECT
        interlocked_or_uint8(p, bit);
#else
        *p |= bit;
#endif
        return false;
    }
};

#endif
