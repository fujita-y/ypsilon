/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#ifndef CORE_H_INCLUDED
#define CORE_H_INCLUDED

#define __STDC_LIMIT_MACROS

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdarg.h>
#include <ctype.h>
#include <float.h>
#include <limits.h>
#include <string.h>
#include <assert.h>
#include <wctype.h>
#include <math.h>
#include <time.h>
#include <fcntl.h>
#include <signal.h>

#ifndef DEFAULT_HEAP_LIMIT
  #define DEFAULT_HEAP_LIMIT        32  // 32MB
#endif

#if _MSC_VER
  #define USE_GCC_EXTENSION         0
#else
  #define USE_GCC_EXTENSION         1
#endif

#define array_sizeof(a) ((int)(sizeof(a)/sizeof(a[0])))

#ifdef NDEBUG
    #define MTDEBUG                 0
    #define GCDEBUG                 0
    #define SCDEBUG                 0
    #define STDEBUG                 0
    #define WBDEBUG                 0
    #define HPDEBUG                 0
#else
    #define MTDEBUG                 0
    #define GCDEBUG                 0
    #define SCDEBUG                 0
    #define STDEBUG                 0
    #define WBDEBUG                 0
    #define HPDEBUG                 0
#endif

#define PROFILE_OPCODE              0
#define PROFILE_SUBR                0
#define DISABLE_FASL                0
#define DETAILED_STATISTIC          0
#define GLOC_DEBUG_INFO             0
#define PREBIND_CLOSE               1
#define THREAD_LOCAL_SLAB_CACHE     1
#define LOCKFREE_ALLOC              1
#define CONCURRENT_COLLECT          1
#define PARALLEL_COLLECT            0
#define BOOT_R6RS_COMPLIANT_SYNTAX  1

#define USE_DEBUG_BOOT              0
#define USE_DEBUG_CORE              0
#define USE_INTERNED_CORE           1  // 1
#define USE_SNPRINT_FOR_FLONUM      0
#define USE_INLINED_CXR             0
#define USE_FLONUM_CONST            1
#define USE_FAST_DYNAMIC_WIND       1
#define USE_EXTENDED_BVECTOR_SYNTAX 0
#define USE_PARALLEL_VM             0

#if USE_GCC_EXTENSION
    #define USE_SYMBOL_THREAD       0
    #define USE_FIXNUM_THREAD       0
    #define USE_DIRECT_THREAD       1
#else
    #define USE_SYMBOL_THREAD       0
    #define USE_FIXNUM_THREAD       1
    #define USE_DIRECT_THREAD       0
#endif

#if _MSC_VER
  #define USE_CRITICAL_SECTION      1
  #define USE_SPINLOCK              0
#else
  #define USE_CRITICAL_SECTION      0
  #define USE_SPINLOCK              0
#endif

#ifndef SYSTEM_SHARE_PATH
  #if _MSC_VER
    #define SYSTEM_SHARE_PATH         "C:/Program Files/Ypsilon"
  #else
    #define SYSTEM_SHARE_PATH         "/usr/local/share/ypsilon"
  #endif
#endif

class VM;

#include "sysdep.h"

extern int                  main_command_line_argc;
extern char* const*         main_command_line_argv;
extern void                 fatal(const char* fmt, ...) ATTRIBUTE(noreturn);
extern void                 warning(const char* fmt, ...);
extern void                 trace(const char* fmt, ...);
#endif
