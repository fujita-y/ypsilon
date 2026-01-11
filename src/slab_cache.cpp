// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

/*
  Reference:
  Malloc(3) revisited: Paul-Henning Kamp
  The Slab Allocator: An Object-Caching Kernel Memory Allocator - Jeff Bonwick - 1994 USENIX
  Magazines and Vmem: Extending the Slab Allocator to Many CPUs and Arbitrary Resources - Jeff Bonwick, Jonathan Adams - 2001 USENIX
*/

#include "core.h"
#include "heap.h"

#define SLAB_CACHE_COUNT_MAX 1024

slab_cache_t::slab_cache_t() {
  m_vacant = NULL;
  m_occupied = NULL;
  m_concurrent_heap = NULL;
  m_lock.init();
}

slab_cache_t::~slab_cache_t() { m_lock.destroy(); }

bool slab_cache_t::init(concurrent_heap_t* concurrent_heap, int object_size, bool gc) {
  assert(concurrent_heap);
  assert(object_size >= (int)sizeof(object_freelist_t));
  if (object_size & (object_size - 1)) {
    fatal("%s:%u object_size must be power of 2 but got %d", __FILE__, __LINE__, object_size);
    return false;
  }
  destroy();
  m_concurrent_heap = concurrent_heap;
  m_object_size = object_size;
  m_object_size_shift = 0;
  m_bitmap_size = 0;
  m_cache_count = 0;
  m_cache_limit = 0;
  int bits = m_object_size;
  while (bits >>= 1) m_object_size_shift++;
  if (gc) m_bitmap_size = ((OBJECT_SLAB_SIZE >> m_object_size_shift) + 7) / 8;
  return true;
}

void slab_cache_t::destroy() {
  if (m_vacant) {
    object_slab_traits_t* traits = m_vacant;
    do {
      object_slab_traits_t* elt = traits;
      traits = traits->next;
      m_concurrent_heap->deallocate(OBJECT_SLAB_TOP_OF(elt));
    } while (traits != m_vacant);
    m_vacant = NULL;
  }
  if (m_occupied) {
    object_slab_traits_t* traits = m_occupied;
    do {
      object_slab_traits_t* elt = traits;
      traits = traits->next;
      m_concurrent_heap->deallocate(OBJECT_SLAB_TOP_OF(elt));
    } while (traits != m_occupied);
    m_occupied = NULL;
  }
  m_concurrent_heap = NULL;
}

void slab_cache_t::init_freelist(uint8_t* slab, uint8_t* bottom, object_slab_traits_t* traits) {
  int step = (m_object_size + OBJECT_DATUM_ALIGN_MASK) & ~OBJECT_DATUM_ALIGN_MASK;
  uint8_t* obj = slab + step;
  traits->free = (object_freelist_t*)obj;
  while (obj + step <= bottom - step) {
    ((object_freelist_t*)obj)->null = NULL;
    ((object_freelist_t*)obj)->next = (object_freelist_t*)(obj + step);
    obj = obj + step;
  }
  ((object_freelist_t*)obj)->null = NULL;
  ((object_freelist_t*)obj)->next = NULL;
}

void slab_cache_t::unload_filled(object_slab_traits_t* traits) {
  if (traits != traits->next) {
    traits->prev->next = traits->next;
    traits->next->prev = traits->prev;
    m_vacant = traits->next;
    if (m_vacant->refc == 0) m_cache_count--;
  } else {
    m_vacant = NULL;
  }
  if (m_occupied) {
    traits->prev = m_occupied;
    traits->next = m_occupied->next;
    traits->prev->next = traits->next->prev = traits;
  } else {
    m_occupied = traits->next = traits->prev = traits;
  }
}

void* slab_cache_t::new_collectible_object() {
  assert(m_concurrent_heap);
  assert(m_bitmap_size != 0);
  bool synchronize = (m_concurrent_heap->m_alloc_barrier != 0);
  if (synchronize) {
#if LOCKFREE_ALLOC && THREAD_LOCAL_SLAB_CACHE
    object_slab_traits_t* traits = m_vacant;
    if ((uintptr_t)traits < (uintptr_t)m_concurrent_heap->m_sweep_wavefront) {
      if (traits && traits->free) {
        object_freelist_t* obj = traits->free;
        traits->free = obj->next;
        traits->refc++;
        if (traits->free == NULL) {
          m_lock.lock();
          unload_filled(traits);
          m_lock.unlock();
        }
        return obj;
      }
    }
#endif
    m_lock.lock();
  }
  if (m_vacant) {
    object_slab_traits_t* traits = m_vacant;
    object_freelist_t* obj = traits->free;
    traits->free = obj->next;
    traits->refc++;
    if (traits->free == NULL) unload_filled(traits);
    if (synchronize) {
      if ((uintptr_t)obj >= (uintptr_t)m_concurrent_heap->m_sweep_wavefront) {
        mark(obj);
        if (DETAILED_STATISTIC) m_concurrent_heap->m_usage.m_barriered_alloc++;
      }
      m_lock.unlock();
    }
    return obj;
  } else {
    uint8_t* slab = (uint8_t*)m_concurrent_heap->allocate(OBJECT_SLAB_SIZE, true, true);
    if (slab) {
      object_slab_traits_t* traits = (object_slab_traits_t*)(slab + OBJECT_SLAB_SIZE - sizeof(object_slab_traits_t));
      traits->next = traits->prev = traits;
      traits->refc = 1;
      traits->cache = this;
      uint8_t* bitmap = (uint8_t*)traits - m_bitmap_size;
      for (int i = 0; i < m_bitmap_size; i++) bitmap[i] = 0;
      init_freelist(slab, bitmap, traits);
      m_vacant = traits;
    }
    if (synchronize) {
      if ((uintptr_t)slab >= (uintptr_t)m_concurrent_heap->m_sweep_wavefront) {
        mark(slab);
        if (DETAILED_STATISTIC) m_concurrent_heap->m_usage.m_barriered_alloc++;
      }
      m_lock.unlock();
    }
    return slab;
  }
}

void* slab_cache_t::new_object() {
  assert(m_concurrent_heap);
  assert(m_bitmap_size == 0);
  m_lock.lock();
  if (m_vacant) {
    object_slab_traits_t* traits = m_vacant;
    object_freelist_t* obj = traits->free;
    traits->free = obj->next;
    traits->refc++;
    if (traits->free == NULL) unload_filled(traits);
    m_lock.unlock();
    return obj;
  } else {
    uint8_t* slab = (uint8_t*)m_concurrent_heap->allocate(OBJECT_SLAB_SIZE, true, m_bitmap_size != 0);
    if (slab) {
      object_slab_traits_t* traits = (object_slab_traits_t*)(slab + OBJECT_SLAB_SIZE - sizeof(object_slab_traits_t));
      traits->next = traits->prev = traits;
      traits->refc = 1;
      traits->cache = this;
      init_freelist(slab, (uint8_t*)traits, traits);
      m_vacant = traits;
    }
    m_lock.unlock();
    return slab;
  }
}

void slab_cache_t::delete_object(void* obj) {
  if (obj == NULL) return;
  assert(m_concurrent_heap);
  assert(m_bitmap_size == 0);
  m_lock.lock();
  object_slab_traits_t* traits = OBJECT_SLAB_TRAITS_OF(obj);
  assert(traits->refc > 0);
  object_freelist_t* first = traits->free;
  ((object_freelist_t*)obj)->null = NULL;
  ((object_freelist_t*)obj)->next = first;
  traits->free = (object_freelist_t*)obj;
  traits->refc--;
  if (first) {
    assert(m_vacant);
    if (traits->refc) {
      m_lock.unlock();
      return;
    }
    // partial->empty
    traits->prev->next = traits->next;
    traits->next->prev = traits->prev;
    if (traits == m_vacant) m_vacant = (traits == traits->next) ? NULL : traits->next;
    m_lock.unlock();
    m_concurrent_heap->deallocate(OBJECT_SLAB_TOP_OF(traits));
    return;
  } else {
    assert(m_occupied);
    // full->partial
    traits->prev->next = traits->next;
    traits->next->prev = traits->prev;
    if (traits == m_occupied) m_occupied = (traits == traits->next) ? NULL : traits->next;
    if (m_vacant) {
      traits->prev = m_vacant;
      traits->next = m_vacant->next;
      traits->prev->next = traits->next->prev = traits;
    } else {
      m_vacant = traits->next = traits->prev = traits;
    }
    m_lock.unlock();
    return;
  }
}

void slab_cache_t::attach(void* slab) {
  m_lock.lock();
  object_slab_traits_t* traits = OBJECT_SLAB_TRAITS_OF(slab);
  if (traits->free == NULL) {
    if (m_occupied) {
      traits->prev = m_occupied;
      traits->next = m_occupied->next;
      traits->prev->next = traits->next->prev = traits;
    } else {
      m_occupied = traits->next = traits->prev = traits;
    }
  } else {
    if (m_vacant) {
      traits->prev = m_vacant;
      traits->next = m_vacant->next;
      traits->prev->next = traits->next->prev = traits;
      if (traits->refc == 0) m_cache_count++;
    } else {
      m_vacant = traits->next = traits->prev = traits;
    }
  }
  m_lock.unlock();
}

void slab_cache_t::detach(void* slab) {
  m_lock.lock();
  object_slab_traits_t* traits = OBJECT_SLAB_TRAITS_OF(slab);
  traits->prev->next = traits->next;
  traits->next->prev = traits->prev;
  if (traits == m_occupied) {
    m_occupied = (traits == traits->next) ? NULL : traits->next;
  } else {
    if (traits == m_vacant) {
      m_vacant = (traits == traits->next) ? NULL : traits->next;
    } else {
      if (traits->refc == 0) m_cache_count--;
    }
  }
  traits->next = traits->prev = NULL;
  m_lock.unlock();
}

void slab_cache_t::sweep(void* slab) {
  assert(m_bitmap_size);
  assert(slab == OBJECT_SLAB_TOP_OF(slab));
  object_slab_traits_t* traits = OBJECT_SLAB_TRAITS_OF(slab);

  m_lock.lock();
  if (traits->refc == 0 && m_cache_count < m_cache_limit) {
    m_concurrent_heap->m_sweep_wavefront = (uint8_t*)slab + OBJECT_SLAB_SIZE;
    m_lock.unlock();
    return;
  }
  traits->prev->next = traits->next;
  traits->next->prev = traits->prev;
  if (traits == m_occupied) {
    m_occupied = (traits == traits->next) ? NULL : traits->next;
  } else {
    if (traits == m_vacant) {
      m_vacant = (traits == traits->next) ? NULL : traits->next;
    } else {
      if (traits->refc == 0) m_cache_count--;
    }
  }
  m_lock.unlock();
  if (traits->refc == 0) {
    if (m_cache_count > m_cache_limit) {
      m_concurrent_heap->deallocate(slab);
      m_concurrent_heap->m_sweep_wavefront = (uint8_t*)slab + OBJECT_SLAB_SIZE;
      return;
    }
  }
  size_t step = (m_object_size + OBJECT_DATUM_ALIGN_MASK) & ~OBJECT_DATUM_ALIGN_MASK;
  uint8_t* bitmap = (uint8_t*)traits - m_bitmap_size;
  uint8_t* limit = bitmap - step;
  uint8_t* obj = (uint8_t*)slab;
  uint8_t* p = bitmap;
  int refc = traits->refc;
  object_freelist_t* freelist = traits->free;
  slab_cache_t* cache = traits->cache;
#if USE_CONST_LITERAL
  if (m_concurrent_heap->is_cons_slab_cache(cache) || m_concurrent_heap->is_flonums_slab_cache(cache) ||
      m_concurrent_heap->is_immutable_cons_slab_cache(cache)) {
#else
  if (m_concurrent_heap->is_cons_slab_cache(cache) || m_concurrent_heap->is_flonums_slab_cache(cache)) {
#endif
    do {
      uint8_t bit = 1;
      uint8_t bits = p[0];
      do {
        if (((bits & bit) == 0) & (((object_freelist_t*)obj)->null != NULL)) {
          ((object_freelist_t*)obj)->null = NULL;
          ((object_freelist_t*)obj)->next = freelist;
          freelist = ((object_freelist_t*)obj);
          refc--;
        }
        obj += step;
        if (obj > limit) goto done;
        bit = bit + bit;
      } while (bit);
      p++;
    } while (obj <= limit);
  } else {
    do {
      uint8_t bit = 1;
      uint8_t bits = p[0];
      do {
        if (((bits & bit) == 0) & (((object_freelist_t*)obj)->null != NULL)) {
          m_concurrent_heap->finalize(obj);
          ((object_freelist_t*)obj)->null = NULL;
          ((object_freelist_t*)obj)->next = freelist;
          freelist = ((object_freelist_t*)obj);
          refc--;
        }
        obj += step;
        if (obj > limit) goto done;
        bit = bit + bit;
      } while (bit);
      p++;
    } while (obj <= limit);
  }

done:
  assert(refc >= 0);
  for (int i = 0; i < m_bitmap_size; i++) bitmap[i] = 0;
  traits->refc = refc;
  traits->free = freelist;
  m_concurrent_heap->m_sweep_wavefront = (uint8_t*)slab + OBJECT_SLAB_SIZE;
  attach(slab);
}

void slab_cache_t::iterate(void* slab, object_iter_proc_t proc, void* desc) {
  assert(m_bitmap_size);
  assert(slab == OBJECT_SLAB_TOP_OF(slab));
  object_slab_traits_t* traits = OBJECT_SLAB_TRAITS_OF(slab);
  size_t step = (m_object_size + OBJECT_DATUM_ALIGN_MASK) & ~OBJECT_DATUM_ALIGN_MASK;
  uint8_t* bitmap = (uint8_t*)traits - m_bitmap_size;
  uint8_t* limit = bitmap - step;
  for (uint8_t* obj = (uint8_t*)slab; obj <= limit; obj += step) {
    if (((object_freelist_t*)obj)->null != NULL) proc(obj, m_object_size, desc);
  }
}
