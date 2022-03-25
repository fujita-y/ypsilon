// Copyright (c) 2004-2022 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef CORE_H_INCLUDED
#define CORE_H_INCLUDED

#ifndef __STDC_LIMIT_MACROS
  #define __STDC_LIMIT_MACROS
#endif

#include <assert.h>
#include <ctype.h>
#include <fcntl.h>
#include <float.h>
#include <limits.h>
#include <math.h>
#include <signal.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <wctype.h>

#define UNBOUND_GLOC_RETURN_UNSPEC 0  // default: 0

#ifndef DEFAULT_HEAP_LIMIT
  #define DEFAULT_HEAP_LIMIT 32  // MB
#endif

#define ENABLE_LLVM_JIT 1

#if ENABLE_LLVM_JIT
  #define ENABLE_CODEGEN_GLOC      1
  #define ENABLE_CODEGEN_REFERENCE 1
  #define ENABLE_BRANCH_WEIGHTS    1
#else
  #define ENABLE_CODEGEN_GLOC      0
  #define ENABLE_CODEGEN_REFERENCE 0
  #define ENABLE_BRANCH_WEIGHTS    0
#endif

#ifdef NDEBUG
  #define MTDEBUG 0
  #define GCDEBUG 0
  #define SCDEBUG 0
  #define STDEBUG 0
  #define WBDEBUG 0
  #define HPDEBUG 0
  #define ASDEBUG 0
#else
  #define MTDEBUG 1
  #define GCDEBUG 1
  #define SCDEBUG 1
  #define STDEBUG 1
  #define WBDEBUG 1
  #define HPDEBUG 1
  #define ASDEBUG 0
#endif

#define PROFILE_OPCODE              0
#define PROFILE_SUBR                0
#define DISABLE_FASL                0
#define DETAILED_STATISTIC          0
#define PREBIND_CLOSE               1
#define THREAD_LOCAL_SLAB_CACHE     1
#define LOCKFREE_ALLOC              1
#define CONCURRENT_COLLECT          1
#define BOOT_R6RS_COMPLIANT_SYNTAX  0

#define USE_DEBUG_BOOT              0
#define USE_DEBUG_CORE              0
#define USE_INTERNED_CORE           1
#define USE_SNPRINT_FOR_FLONUM      0
#define USE_INLINED_CXR             0
#define USE_FLONUM_CONST            1
#define USE_FAST_DYNAMIC_WIND       1
#define USE_EXTENDED_BVECTOR_SYNTAX 1
#define USE_CONST_LITERAL           1
#define USE_MULTIBYTE_READ          1
#define USE_MULTIBYTE_WRITE         1

#if defined(FD_CLOEXEC)
  #define USE_CLOEXEC 1
#else
  #define USE_CLOEXEC 0
#endif

#ifndef SYSTEM_SHARE_PATH
  #define SYSTEM_SHARE_PATH "/usr/local/share/ypsilon"
#endif

#ifndef SYSTEM_EXTENSION_PATH
  #define SYSTEM_EXTENSION_PATH "/usr/local/lib/ypsilon"
#endif

#define array_sizeof(a) ((int)(sizeof(a) / sizeof(a[0])))

class VM;

#include "sysdep.h"

extern int main_command_line_argc;
extern const char** main_command_line_argv;
extern void fatal(const char* fmt, ...) ATTRIBUTE(noreturn);
extern void warning(const char* fmt, ...);
extern void trace(const char* fmt, ...);

#endif
