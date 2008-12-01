/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#ifndef SYSDEP_H_INCLUDED
#define SYSDEP_H_INCLUDED

#if _MSC_VER
  #define DECLSPEC(x)               __declspec(x)
  #define ATTRIBUTE(x)
  #define ALIGNOF(x)                (sizeof(x) > __alignof(x) ? __alignof(x) : sizeof(x))
  #define ARCH_BIG_ENDIAN           0
  #define ARCH_LITTLE_ENDIAN        1
  #define ARCH_IA32                 1
  #define ARCH_X64                  0
  #define ARCH_ILP32                1
  #define ARCH_LP64                 0
  #define ARCH_LLP64                0
#else
  #define DECLSPEC(x)
  #define ATTRIBUTE(x)              __attribute__ ((x))
  #define ALIGNOF(x)                __alignof__(x)
  #if defined(__LITTLE_ENDIAN__)
    #define ARCH_LITTLE_ENDIAN      1
    #define ARCH_BIG_ENDIAN         0
  #elif defined(__BIG_ENDIAN__)
    #define ARCH_LITTLE_ENDIAN      0
    #define ARCH_BIG_ENDIAN         1
  #elif defined(__BYTE_ORDER)
    #if __BYTE_ORDER == __LITTLE_ENDIAN
      #define ARCH_LITTLE_ENDIAN    1
      #define ARCH_BIG_ENDIAN       0
    #elif __BYTE_ORDER == __BIG_ENDIAN
      #define ARCH_LITTLE_ENDIAN    0
      #define ARCH_BIG_ENDIAN       1
    #else
      #error unknown __BYTE_ORDER
    #endif
  #else
    #error unknown __BYTE_ORDER
  #endif
  #if defined(__x86_64__)
    #define ARCH_IA32               0
    #define ARCH_X64                1
  #elif defined(__i386__)
    #define ARCH_IA32               1
    #define ARCH_X64                0
  #else
    #error unknown processor
  #endif
  #if defined(__LP64__)
    #define ARCH_ILP32              0
    #define ARCH_LP64               1
    #define ARCH_LLP64              0
  #else
    #define ARCH_ILP32              1
    #define ARCH_LP64               0
    #define ARCH_LLP64              0
  #endif
#endif

extern void fatal(const char* fmt, ...) ATTRIBUTE(noreturn);

#if _MSC_VER

    #pragma warning(disable:4996)
    #pragma warning(disable:4146)
    #pragma warning(disable:4244)
    #pragma warning(disable:4715)
    #pragma warning(disable:4101)
    #pragma warning(disable:4018)

    #define     WIN32_LEAN_AND_MEAN
    #include    <windows.h>
    #include    <malloc.h>
    #include    <float.h>
    #include    <errno.h>
    #include    <io.h>
    #include    <fcntl.h>
    #include    <winsock2.h>
    #include    <process.h>
    #include    <xmmintrin.h>
    #include    <sys/stat.h>
    #include    <limits>
    #include    <winsock2.h>
    #include    <ws2tcpip.h>

    extern "C" void __cdecl     _dosmaperr(unsigned long);
    #define snprintf            _snprintf
    #define srandom             srand
    #define random              rand
    #define gmtime_r(A1, A2)    gmtime_s(A2, A1)
    #define localtime_r(A1, A2) localtime_s(A2, A1)

    #define VALUE_NAN           std::numeric_limits<double>::quiet_NaN()
    #define VALUE_INF           std::numeric_limits<double>::infinity()

    #define INT8_MIN            _I8_MIN
    #define INT8_MAX            _I8_MAX
    #define INT16_MIN           _I16_MIN
    #define INT16_MAX           _I16_MAX
    #define INT32_MIN           _I32_MIN
    #define INT32_MAX           _I32_MAX
    #define INT64_MIN           _I64_MIN
    #define INT64_MAX           _I64_MAX
    #define INTPTR_MIN          _I32_MIN
    #define INTPTR_MAX          _I32_MAX
    #define UINT8_MIN           _UI8_MIN
    #define UINT8_MAX           _UI8_MAX
    #define UINT16_MIN          _UI16_MIN
    #define UINT16_MAX          _UI16_MAX
    #define UINT32_MIN          _UI32_MIN
    #define UINT32_MAX          _UI32_MAX
    #define UINT64_MIN          _UI64_MIN
    #define UINT64_MAX          _UI64_MAX
    #define UINTPTR_MIN         _UI32_MIN
    #define UINTPTR_MAX         _UI32_MAX

    typedef signed char         int8_t;
    typedef short               int16_t;
    typedef int                 int32_t;
    typedef long long           int64_t;
    typedef unsigned char       uint8_t;
    typedef unsigned short      uint16_t;
    typedef unsigned int        uint32_t;
    typedef unsigned long long  uint64_t;
    typedef int                 ssize_t;
    typedef int64_t             off64_t;
    typedef HANDLE              fd_t;

    #define MEM_STORE_FENCE     _mm_sfence()
    #define INVALID_FD          INVALID_HANDLE_VALUE
    #define PORT_STDIN_FD       GetStdHandle(STD_INPUT_HANDLE)
    #define PORT_STDOUT_FD      GetStdHandle(STD_OUTPUT_HANDLE)
    #define PORT_STDERR_FD      GetStdHandle(STD_ERROR_HANDLE)

  #ifndef HOST_NAME_MAX
    #define HOST_NAME_MAX       255
  #endif

    inline int      isnan(double x) { return _isnan(x); }
    inline int      isinf(double x) { return (!_finite(x) && !_isnan(x)); }
    inline double   round(double x) { return (x >= 0.0) ? floor(x + 0.5) : ceil(x - 0.5); }
    inline double   trunc(double x) { return (x >= 0.0) ? floor(x) : ceil(x); }

    inline double msec()
    {
        FILETIME ft;
        GetSystemTimeAsFileTime(&ft);
        return ((double)ft.dwLowDateTime + (double)ft.dwHighDateTime * (double)UINT32_MAX) / 10000.0;
    }

    inline int gettimeofday(struct timeval *tv, struct timezone *tz)
    {
        FILETIME ft;
        GetSystemTimeAsFileTime(&ft);
        uint64_t ft64 = ((uint64_t)ft.dwLowDateTime + (((uint64_t)ft.dwHighDateTime) << 32)) / 10 - 11644473600000000LL;
        tv->tv_usec = ft64 % 1000000;
        tv->tv_sec = ft64 / 1000000;
        return 0;
    }

    inline int usleep(int usec)
    {
        SleepEx(usec / 1000, FALSE);
        return 0;
    }

    inline int getpagesize()
    {
        SYSTEM_INFO si;
        GetSystemInfo(&si);
        return ((int)si.dwPageSize);
    }

    #define HEAP_MAP_FAILED     0
    #define HEAP_UNMAP_FAILED   0

    inline void* heap_map(void* adrs, size_t size)
    {
        return VirtualAlloc(adrs, size, MEM_COMMIT, PAGE_READWRITE);
    }

    inline int heap_unmap(void* adrs, size_t size)
    {
        return VirtualFree(adrs, size, MEM_DECOMMIT);
    }

    inline VM* current_vm()
    {
        extern __declspec(thread) VM* s_current_vm;
        return s_current_vm;
    }

    inline void set_current_vm(VM* vm)
    {
        extern __declspec(thread) VM* s_current_vm;
        s_current_vm = vm;
    }

  #if MTDEBUG
    #define MTVERIFY(expr)                                                                                              \
        do {                                                                                                            \
            intptr_t __RETVAL__ = (intptr_t)(expr);                                                                     \
            if (__RETVAL__ == 0) fatal("error:%s:%u " #expr " %d %d", __FILE__, __LINE__, __RETVAL__, GetLastError());  \
        } while(0)
  #else
    #define MTVERIFY(expr)                              \
        do {                                            \
            intptr_t __RETVAL__ = (intptr_t)(expr);     \
            if (__RETVAL__ == 0) throw GetLastError();  \
        } while(0)
  #endif

        #define thread_main_t unsigned int __stdcall

    inline void thread_start(unsigned int (__stdcall *func)(void*), void* param)
    {
        MTVERIFY(_beginthreadex(NULL, 0, func, param, 0, NULL));
    }

    inline void thread_yield()
    {
        Sleep(0);
    }

#else

    #include <pthread.h>
    #include <sys/time.h>
    #include <sys/types.h>
    #include <sys/stat.h>
    #include <sys/mman.h>
    #include <sys/errno.h>
    #include <sys/poll.h>
    #include <sys/socket.h>
    #include <sys/param.h>
    #include <sys/times.h>
    #include <sys/resource.h>
    #include <sys/utsname.h>
    #include <sys/wait.h>
    #include <stdint.h>
    #include <unistd.h>
    #include <regex.h>
    #include <dlfcn.h>
    #include <netdb.h>
    #include <dirent.h>

    typedef int     fd_t;

  #ifndef __off64_t_defined
    typedef off_t   off64_t;
  #endif
  #if ARCH_LP64
    typedef int int128_t __attribute__((__mode__(TI)));
    typedef unsigned int uint128_t __attribute__((__mode__(TI)));
  #endif

    #define VALUE_NAN           __builtin_nan("")   /* strtod("NAN", NULL) */
    #define VALUE_INF           __builtin_inf()     /* strtod("INF", NULL) */
    #define MEM_STORE_FENCE     __asm__ __volatile__ ("sfence" ::: "memory")

    #define INVALID_FD          (-1)
    #define INVALID_SOCKET      (-1)
    #define PORT_STDIN_FD       0
    #define PORT_STDOUT_FD      1
    #define PORT_STDERR_FD      2

  #ifndef HOST_NAME_MAX
    #define HOST_NAME_MAX       _POSIX_HOST_NAME_MAX
  #endif

    #define HEAP_MAP_FAILED     MAP_FAILED
    #define HEAP_UNMAP_FAILED   (-1)

    inline void* heap_map(void* adrs, size_t size)
    {
        return (uint8_t*)mmap(adrs, size, (PROT_READ | PROT_WRITE), (MAP_ANON | MAP_PRIVATE), -1, 0);
    }

    inline int heap_unmap(void* adrs, size_t size)
    {
        return munmap(adrs, size);
    }

    inline double msec()
    {
        struct timeval tv;
        gettimeofday(&tv, NULL);
        return (tv.tv_sec * 1000000.0 + tv.tv_usec) / 1000.0;
    }

  #if MTDEBUG
    #define MTVERIFY(expr)                                                                                              \
        do {                                                                                                            \
            int __RETVAL__ = (expr);                                                                                    \
             if (__RETVAL__) fatal("error:%s:%u " #expr " %d %s", __FILE__, __LINE__, __RETVAL__, strerror(__RETVAL__));\
        } while(0)
  #else
    #define MTVERIFY(expr)                      \
        do {                                    \
            int __RETVAL__ = (expr);            \
            if (__RETVAL__) throw __RETVAL__;   \
        } while(0)
  #endif

  #if __APPLE_CC__

    inline VM* current_vm()
    {
        extern pthread_key_t s_current_vm;
        return (VM*)pthread_getspecific(s_current_vm);
    }

    inline void set_current_vm(VM* vm)
    {
        extern pthread_key_t s_current_vm;
        MTVERIFY(pthread_setspecific(s_current_vm, vm));
    }

  #else

    inline VM* current_vm()
    {
        extern __thread VM* s_current_vm;
        return s_current_vm;
    }

    inline void set_current_vm(VM* vm)
    {
        extern __thread VM* s_current_vm;
        s_current_vm = vm;
    }

  #endif

    typedef pthread_t   thread_t;
    typedef void*       thread_main_t;

    inline void thread_start(void* (*func)(void*), void* param)
    {
        pthread_t th;
        MTVERIFY(pthread_create(&th, NULL, func, param));
        MTVERIFY(pthread_detach(th));
    }

    inline void thread_yield()
    {
        sched_yield();
    }

#endif

#endif  // SYSDEP_H_INCLUDED
