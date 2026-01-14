// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef CONCURRENT_SLAB_H_INCLUDED
#define CONCURRENT_SLAB_H_INCLUDED

#include "core.h"
#include "object.h"

class concurrent_heap_t;

#define SLAB_TOP_OF(obj)    ((uint8_t*)(((uintptr_t)(obj)) & ~(SLAB_SIZE - 1)))
#define SLAB_TRAITS_OF(obj) ((slab_traits_t*)(SLAB_TOP_OF(obj) + SLAB_SIZE - sizeof(slab_traits_t)))

struct concurrent_slab_t;
struct slab_traits_t;
struct freelist_t;

struct freelist_t {
  void* null;  // <- concurrent_slab_t::delete_object(...) assign NULL to detect free cell during sweep phase
  freelist_t* next;
};

struct slab_traits_t {  // <- locate to bottom of each slab
  intptr_t refc;
  freelist_t* free;
  slab_traits_t* next;
  slab_traits_t* prev;
  concurrent_slab_t* cache;
};

typedef void (*object_iter_proc_t)(void* obj, int size, void* refcon);

class concurrent_slab_t {
 private:
  concurrent_heap_t* m_concurrent_heap;
  int m_object_size_shift;
  int m_bitmap_size;
  int m_cache_count;
  bool m_finalize;
  void init_freelist(uint8_t* top, uint8_t* bottom, slab_traits_t* traits);
  void unload_filled(slab_traits_t* traits);

 public:
  mutex_t m_lock;
  slab_traits_t* m_vacant;
  slab_traits_t* m_occupied;
  int m_object_size;
  int m_cache_limit;
  concurrent_slab_t();
  ~concurrent_slab_t();
  bool init(concurrent_heap_t* concurrent_heap, int object_size, bool gc, bool finalize);
  void destroy();
  void* new_collectible_object();
  void* new_object();
  void delete_object(void* obj);
  void attach(void* slab);
  void detach(void* slab);
  void sweep(void* slab);
  void iterate(void* slab, object_iter_proc_t proc, void* desc);

  void* lookup(void* ref) {
    assert((uint8_t*)ref < (uint8_t*)SLAB_TRAITS_OF(ref) - m_bitmap_size);
    assert((uint8_t*)ref >= (uint8_t*)SLAB_TOP_OF(ref));
    return (void*)((intptr_t)ref & ~(m_object_size - 1));
  }

  bool state(void* obj) {
    assert(m_bitmap_size);
    uint8_t* bitmap = (uint8_t*)SLAB_TRAITS_OF(obj) - m_bitmap_size;
    int bit_n = ((intptr_t)obj & (SLAB_SIZE - 1)) >> m_object_size_shift;
    assert(bit_n < m_bitmap_size * 8);
    return (bitmap[bit_n >> 3] & (1 << (bit_n & 7))) != 0;
  }

  void mark(void* obj) {
    assert(m_bitmap_size);
    uint8_t* bitmap = (uint8_t*)SLAB_TRAITS_OF(obj) - m_bitmap_size;
    int bit_n = ((intptr_t)obj & (SLAB_SIZE - 1)) >> m_object_size_shift;
    assert(bit_n < m_bitmap_size * 8);
    bitmap[bit_n >> 3] |= (1 << (bit_n & 7));
  }

  bool test_and_mark(void* obj) {
    assert(m_bitmap_size);
    uint8_t* bitmap = (uint8_t*)SLAB_TRAITS_OF(obj) - m_bitmap_size;
    int bit_n = ((intptr_t)obj & (SLAB_SIZE - 1)) >> m_object_size_shift;
    assert(bit_n < m_bitmap_size * 8);
    uint8_t bit = (1 << (bit_n & 7));
    uint8_t* p = bitmap + (bit_n >> 3);
    if (*p & bit) return true;
    *p |= bit;
    return false;
  }
};

#endif
