#ifndef CONCURRENT_POOL_H_INCLUDED
#define CONCURRENT_POOL_H_INCLUDED

#include "core.h"
#include "object.h"
#include "mutex.h"

#define PTAG_FREE    0x00
#define PTAG_USED    0x01
#define PTAG_EXTENT  0x02
#define PTAG_SLAB    0x04
#define PTAG_GC      0x08

#define GCSLABP(tag) (((tag) & (PTAG_SLAB | PTAG_GC)) == (PTAG_SLAB | PTAG_GC))

class concurrent_heap_t;

class concurrent_pool_t {
  friend class concurrent_heap_t;

 private:
  uint8_t* m_map;
  size_t m_map_size;
  mutex_t m_lock;
  int m_pool_memo;
  int m_pool_usage;
  int m_pool_threshold;

 public:
  uint8_t* m_pool;
  size_t m_pool_size;
  int m_pool_watermark;

  concurrent_pool_t();
  void init(size_t pool_size, size_t init_size);
  void destroy();
  void* allocate(size_t size, bool slab, bool gc);
  void deallocate(void* p);
  bool extend_pool(size_t extend_size);
};

#endif
