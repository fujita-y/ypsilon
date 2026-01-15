// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "concurrent_pool.h"
#include "concurrent_slab.h"

#define PREFERRED_MMAP_ADDRESS   ((void*)0x10000000)
#define SYNCHRONIZE_THRESHOLD(x) ((x) - (x) / 4)

concurrent_pool_t::concurrent_pool_t() {
  m_map = NULL;
  m_map_size = 0;
  m_pool = NULL;
  m_pool_size = 0;
  m_lock.init();
}

void concurrent_pool_t::init(size_t pool_size, size_t init_size) {
#ifndef NDEBUG
  printf("SLAB_SIZE:%ld\n", SLAB_SIZE);
  printf("getpagesize():%d\n", getpagesize());
#endif
  assert(getpagesize() >= SLAB_SIZE);
  assert((getpagesize() % SLAB_SIZE) == 0);  // for optimal performance
  assert(pool_size >= SLAB_SIZE * 2);        // check minimum (1 directory + 1 datum)
  pool_size = pool_size < 2 ? 2 : pool_size;
  init_size = init_size < pool_size ? init_size : pool_size;
  // pool
  assert(m_pool == NULL);
  m_pool_size = (pool_size + SLAB_SIZE - 1) & ~(SLAB_SIZE - 1);
  m_map_size = m_pool_size + SLAB_SIZE;
  m_map = (uint8_t*)heap_map(PREFERRED_MMAP_ADDRESS, m_map_size);
  if (m_map == HEAP_MAP_FAILED) {
    m_map = NULL;
    fatal("%s:%u mmap() failed: %s", __FILE__, __LINE__, strerror(errno));
  }
#ifndef NDEBUG
  printf("mmap address:0x%lx\n", (uintptr_t)m_map);
#endif
  m_pool = (uint8_t*)(((uintptr_t)m_map + SLAB_SIZE - 1) & ~(SLAB_SIZE - 1));
  assert(((uintptr_t)m_pool & (SLAB_SIZE - 1)) == 0);
  // ptag
  int n_tag = m_pool_size / SLAB_SIZE;
  int n_slab = (n_tag + SLAB_SIZE - 1) / SLAB_SIZE;
  memset(m_pool, PTAG_FREE, n_slab * SLAB_SIZE);
  for (int i = 0; i < n_slab; i++) m_pool[i] = PTAG_USED;
  m_pool_watermark = (init_size >> SLAB_SIZE_SHIFT);
  if (m_pool_watermark <= n_slab || m_pool_watermark >= (m_pool_size >> SLAB_SIZE_SHIFT)) {
    fatal("%s:%u concurrent_pool_t::init() bad parameter, pool_size:%d init_datum_size:%d", __FILE__, __LINE__, pool_size, init_size);
  }
  m_pool_memo = 0;
  m_pool_usage = 0;
  m_pool_threshold = SYNCHRONIZE_THRESHOLD(n_tag);
}

void concurrent_pool_t::destroy() {
  m_lock.destroy();
  if (m_map) {
    heap_unmap(m_map, m_map_size);
    m_map = NULL;
    m_pool = NULL;
    m_map_size = 0;
    m_pool_size = 0;
  }
}

void* concurrent_pool_t::allocate(size_t size, bool for_slab, bool for_collectible) {
  assert(for_slab || (for_collectible == false));
  uint8_t attr = 0;
  if (for_slab) {
    if (for_collectible)
      attr = PTAG_SLAB | PTAG_GC;
    else
      attr = PTAG_SLAB;
  }
  assert(m_pool);
  int npage = (size + SLAB_SIZE - 1) >> SLAB_SIZE_SHIFT;
  scoped_lock lock(m_lock);
  if (npage == 1) {
    for (int i = m_pool_memo; i < m_pool_watermark; i++) {
      if (m_pool[i] == PTAG_FREE) {
        void* slab = m_pool + ((intptr_t)i << SLAB_SIZE_SHIFT);
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
              return m_pool + ((intptr_t)head << SLAB_SIZE_SHIFT);
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

void concurrent_pool_t::deallocate(void* p) {
  scoped_lock lock(m_lock);
  assert(p);
  assert(m_pool);
  assert(((intptr_t)p & (SLAB_SIZE - 1)) == 0);
  int i = ((uint8_t*)p - m_pool) >> SLAB_SIZE_SHIFT;
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
  memset(p, 0xBD, SLAB_SIZE);
#endif
}

bool concurrent_pool_t::extend_pool(size_t extend_size) {
  scoped_lock lock(m_lock);
  int capacity = (m_pool_size >> SLAB_SIZE_SHIFT);
  if (m_pool_watermark == capacity) return false;
  m_pool_watermark += ((extend_size + SLAB_SIZE - 1) >> SLAB_SIZE_SHIFT);
  if (m_pool_watermark > capacity) m_pool_watermark = capacity;
  return true;
}
