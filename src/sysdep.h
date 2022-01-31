// Copyright (c) 2004-2022 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef SYSDEP_H_INCLUDED
#define SYSDEP_H_INCLUDED

#if defined(NO_TLS)
  #undef NO_TLS
  #define NO_TLS 1
#endif
#if defined(NO_POSIX_SPAWN)
  #undef NO_POSIX_SPAWN
  #define NO_POSIX_SPAWN 1
#endif

#define DECLSPEC(x)
#define ATTRIBUTE(x) __attribute__((x))
#if defined(__LITTLE_ENDIAN__)
  #define ARCH_LITTLE_ENDIAN 1
  #define ARCH_BIG_ENDIAN    0
#elif defined(__BIG_ENDIAN__)
  #define ARCH_LITTLE_ENDIAN 0
  #define ARCH_BIG_ENDIAN    1
#elif defined(__BYTE_ORDER)
  #if __BYTE_ORDER == __LITTLE_ENDIAN
    #define ARCH_LITTLE_ENDIAN 1
    #define ARCH_BIG_ENDIAN    0
  #elif __BYTE_ORDER == __BIG_ENDIAN
    #define ARCH_LITTLE_ENDIAN 0
    #define ARCH_BIG_ENDIAN    1
  #else
    #error unknown __BYTE_ORDER
  #endif
#elif defined(__x86_64__) || defined(__i386__)
  #define ARCH_LITTLE_ENDIAN 1
  #define ARCH_BIG_ENDIAN    0
#else
  #error unknown __BYTE_ORDER
#endif
#if defined(__x86_64__)
  #define ARCH_IA32  0
  #define ARCH_AMD64 1
  #define ARCH_PPC   0
  #define ARCH_ARM32 0
  #define ARCH_ARM64 0
#elif defined(__i386__)
  #define ARCH_IA32  1
  #define ARCH_AMD64 0
  #define ARCH_PPC   0
  #define ARCH_ARM32 0
  #define ARCH_ARM64 0
#elif defined(__powerpc__)
  #define ARCH_IA32  0
  #define ARCH_AMD64 0
  #define ARCH_PPC   1
  #define ARCH_ARM32 0
  #define ARCH_ARM64 0
#elif defined(__arm__)
  #define ARCH_IA32  0
  #define ARCH_AMD64 0
  #define ARCH_PPC   0
  #define ARCH_ARM32 1
  #define ARCH_ARM64 0
#elif defined(__aarch64__)
  #define ARCH_IA32  0
  #define ARCH_AMD64 0
  #define ARCH_PPC   0
  #define ARCH_ARM32 0
  #define ARCH_ARM64 1
#else
  #error unknown processor
#endif
#if defined(__LP64__)
  #define ARCH_ILP32 0
  #define ARCH_LP64  1
  #define ARCH_LLP64 0
#else
  #define ARCH_ILP32 1
  #define ARCH_LP64  0
  #define ARCH_LLP64 0
#endif

extern void fatal(const char* fmt, ...) ATTRIBUTE(noreturn);

#include <dirent.h>
#include <dlfcn.h>
#include <errno.h>
#include <netdb.h>
#include <pthread.h>
#include <stdint.h>
#include <sys/file.h>
#include <sys/mman.h>
#include <sys/param.h>
#include <sys/poll.h>
#include <sys/resource.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/times.h>
#include <sys/types.h>
#include <sys/utsname.h>
#include <sys/wait.h>
#include <unistd.h>
#if !defined(NO_POSIX_SPAWN)
  #include <spawn.h>
#endif
#if defined(__sun__)
  #include <alloca.h>
  #include <ieeefp.h>
#endif

extern char** environ;

typedef int fd_t;
#if !defined(_LARGEFILE64_SOURCE) && !defined(__off64_t_defined)
typedef off_t off64_t;
#endif

#if ARCH_LP64
typedef int int128_t __attribute__((__mode__(TI)));
typedef unsigned int uint128_t __attribute__((__mode__(TI)));
#endif

#define VALUE_NAN __builtin_nan("") /* strtod("NAN", NULL) */
#define VALUE_INF __builtin_inf()   /* strtod("INF", NULL) */

#if ARCH_PPC
  #define MEM_STORE_FENCE __asm__ __volatile__("lwsync" ::: "memory")
#elif ARCH_ARM32
  #define MEM_STORE_FENCE __asm__ __volatile__("dsb sy" ::: "memory")
#elif ARCH_ARM64
  #define MEM_STORE_FENCE __asm__ __volatile__("dsb sy" ::: "memory")
#else
  #define MEM_STORE_FENCE __asm__ __volatile__("sfence" ::: "memory")
#endif

#if defined(__sun__)
inline int isinf(double x) { return (!finite(x) && !isnan(x)); }
#endif

#define INVALID_FD     (-1)
#define INVALID_SOCKET (-1)
#define PORT_STDIN_FD  0
#define PORT_STDOUT_FD 1
#define PORT_STDERR_FD 2

#ifndef HOST_NAME_MAX
  #define HOST_NAME_MAX _POSIX_HOST_NAME_MAX
#endif

#define HEAP_MAP_FAILED   MAP_FAILED
#define HEAP_UNMAP_FAILED (-1)

inline void* heap_map(void* adrs, size_t size) { return (uint8_t*)mmap(adrs, size, (PROT_READ | PROT_WRITE), (MAP_ANON | MAP_PRIVATE), -1, 0); }

inline int heap_unmap(void* adrs, size_t size) { return munmap(adrs, size); }

inline double msec() {
  struct timeval tv;
  gettimeofday(&tv, NULL);
  return (tv.tv_sec * 1000000.0 + tv.tv_usec) / 1000.0;
}

#if MTDEBUG
  #define MTVERIFY(expr)                                                                                          \
    do {                                                                                                          \
      int __RETVAL__ = (expr);                                                                                    \
      if (__RETVAL__) fatal("error:%s:%u " #expr " %d %s", __FILE__, __LINE__, __RETVAL__, strerror(__RETVAL__)); \
    } while (0)
#else
  #define MTVERIFY(expr)                \
    do {                                \
      int __RETVAL__ = (expr);          \
      if (__RETVAL__) throw __RETVAL__; \
    } while (0)
#endif

#if defined(NO_TLS)

inline VM* current_vm() {
  extern pthread_key_t s_current_vm;
  return (VM*)pthread_getspecific(s_current_vm);
}

inline void set_current_vm(VM* vm) {
  extern pthread_key_t s_current_vm;
  MTVERIFY(pthread_setspecific(s_current_vm, vm));
}

#else

inline VM* current_vm() {
  extern __thread VM* s_current_vm;
  return s_current_vm;
}

inline void set_current_vm(VM* vm) {
  extern __thread VM* s_current_vm;
  s_current_vm = vm;
}

#endif

typedef pthread_t thread_t;
typedef void* thread_main_t;

inline void thread_start(void* (*func)(void*), void* param) {
  pthread_t th;
  MTVERIFY(pthread_create(&th, NULL, func, param));
  MTVERIFY(pthread_detach(th));
}

inline void thread_yield() { sched_yield(); }

#endif  // SYSDEP_H_INCLUDED
