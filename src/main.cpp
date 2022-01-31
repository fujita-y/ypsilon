// Copyright (c) 2004-2022 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "vm.h"

#if ENABLE_LLVM_JIT
  #include <llvm/Support/DynamicLibrary.h>
  #include <llvm/Support/InitLLVM.h>
  #include <llvm/Support/TargetSelect.h>
#endif

int main_command_line_argc;
const char** main_command_line_argv;

extern void init_c_ffi();
extern void destroy_c_ffi();

#if defined(NO_TLS)
pthread_key_t s_current_vm;
#else
__thread VM* s_current_vm;
#endif

static int opt_heap_limit(int argc, const char** argv) {
  int value = DEFAULT_HEAP_LIMIT;
  for (int i = 0; i < argc; i++) {
    const int strlen_mlimit = strlen("--heap-limit=");
    const char* opt = argv[i];
    const char* param = NULL;
    if (strcmp(opt, "--") == 0) {
      break;
    } else if ((strlen(argv[i]) >= strlen_mlimit) && (memcmp(opt, "--heap-limit=", strlen_mlimit) == 0)) {
      param = opt + strlen_mlimit;
    } else if (strcmp(opt, "--heap-limit") == 0) {
      if ((i + 1 < argc) && strcmp(argv[i + 1], "--")) {
        param = argv[i + 1];
      } else {
        fprintf(stderr, "** ERROR in option '--heap-limit': missing value\n");
        exit(EXIT_FAILURE);
      }
    }
    if (param) {
      int tmp;
      if ((sscanf(param, "%d", &tmp) == 1) && (tmp >= 16)) {
        value = tmp;
      } else {
        fprintf(stderr, "** ERROR in option '--heap-limit=%s': parameter must be a positive integer greater than 15\n", param);
        exit(EXIT_FAILURE);
      }
    }
  }
  return value;
}

static void* signal_waiter(void* param) {
  sigset_t set = *(sigset_t*)param;
  while (true) {
    int sig;
    int err = sigwait(&set, &sig);
    if (err == 0) {
      if (sig == SIGHUP) {
        fprintf(stderr, ": SIGHUP\n");
        exit(0);
      }
      if (sig == SIGTERM) {
        fprintf(stderr, ": SIGTERM\n");
        exit(0);
      }
      if (sig == SIGQUIT) {
        fprintf(stderr, ": SIGQUIT\n");
        exit(0);
      }
      if (sig == SIGKILL) {
        fprintf(stderr, ": SIGKILL\n");
        exit(0);
      }
      if (sig == SIGABRT) {
        fprintf(stderr, ": SIGABRT\n");
        exit(0);
      }
      if (sig == SIGINT) {
        fprintf(stderr, ": SIGINT\n");
        continue;
      }
      if (sig == SIGTSTP) {
        fprintf(stderr, ": SIGTSTP\n");
        continue;
      }
      if (sig == SIGCONT) {
        fprintf(stderr, ": SIGCONT\n");
        continue;
      }
      fprintf(stderr, ";; ### UNHANDLED SIGNAL %d ###\n", sig);
      exit(0);
    } else {
      if (err != EINTR) {
        fprintf(stderr, "error: sigwait() %s (%d)\n", strerror(err), err);
      }
    }
  }
  return NULL;
}

static VM s_primordial_vm;

int main(int argc, const char** argv) {
  srandom((unsigned int)fmod(msec() * 1000.0, INT_MAX));
  main_command_line_argc = argc;
  main_command_line_argv = argv;
#if ENABLE_LLVM_JIT
  llvm::InitLLVM X(argc, argv);
  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();
  llvm::sys::DynamicLibrary::LoadLibraryPermanently(nullptr);
  init_c_ffi();
#endif
#ifndef NDEBUG
  struct foo {
    char i;
  };
  struct bar {
    int i;
    struct foo o;
  };
  struct hoge {
    struct bar m;
    char k;
  };
  struct nudge {
    char c;
    double x;
  };
  printf("sizeof(int) %d\n", (int)sizeof(int));
  printf("sizeof(long) %d\n", (int)sizeof(long));
  printf("sizeof(long long) %d\n", (int)sizeof(long long));
  printf("sizeof(void*) %d\n", (int)sizeof(void*));
  printf("sizeof(foo) %d\n", (int)sizeof(foo));  // 1
  printf("sizeof(bar) %d\n", (int)sizeof(bar));  // 8
  printf("sizeof(hoge) %d\n", (int)sizeof(hoge));
  printf("sizeof(bool) %d\n", (int)sizeof(bool));
  printf("sizeof(size_t) %d\n", (int)sizeof(size_t));
  printf("__alignof__(double) %d\n", (int)__alignof__(double));  // 8
  printf("FIXNUM_MAX %ld %lx\n", (long)FIXNUM_MAX, (long)FIXNUM_MAX);
  printf("FIXNUM_MIN %ld %lx\n", (long)FIXNUM_MIN, (long)FIXNUM_MIN);
  printf("sizeof(pthread_mutex_t) %d\n", (int)sizeof(pthread_mutex_t));
  printf("sizeof(pthread_cond_t) %d\n", (int)sizeof(pthread_cond_t));
  printf("offsetof(nudge, x) %d\n", (int)offsetof(nudge, x));
#endif
#if MTDEBUG
  puts(";; MTDEBUG ON");
#endif
#if GCDEBUG
  puts(";; GCDEBUG ON");
#endif
#if SCDEBUG
  puts(";; SCDEBUG ON");
#endif
#if STDEBUG
  puts(";; STDEBUG ON");
#endif
#if HPDEBUG
  puts(";; HPDEBUG ON");
#endif
#if ASDEBUG
  puts(";; ASDEBUG ON");
#endif
  sigset_t set;
  sigemptyset(&set);
  sigaddset(&set, SIGINT);
  sigaddset(&set, SIGPIPE);
  MTVERIFY(pthread_sigmask(SIG_BLOCK, &set, NULL));
  sigemptyset(&set);
  sigaddset(&set, SIGINT);
  pthread_t tid;
  MTVERIFY(pthread_create(&tid, NULL, signal_waiter, &set));
  MTVERIFY(pthread_detach(tid));
#if defined(NO_TLS)
  MTVERIFY(pthread_key_create(&s_current_vm, NULL));
#endif
  size_t heap_limit = (size_t)opt_heap_limit(argc, argv) * 1024 * 1024;
  size_t heap_init = 4 * 1024 * 1024;
#ifndef NDEBUG
  printf("heap_limit %zu heap_init %zu\n", heap_limit, heap_init);
#endif
  object_heap_t* heap = new object_heap_t;
  heap->init(heap_limit, heap_init);
  s_primordial_vm.init(heap);
  s_primordial_vm.boot();
  set_current_vm(&s_primordial_vm);
  s_primordial_vm.standalone();
#if ENABLE_LLVM_JIT
  destroy_c_ffi();
#endif
  return 0;
}

void fatal(const char* fmt, ...) {
  fflush(stdout);
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  fprintf(stderr, "\n");
  fflush(stderr);
  exit(EXIT_FAILURE);
}

void warning(const char* fmt, ...) {
  fflush(stdout);
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  fflush(stderr);
}

void trace(const char* fmt, ...) {
  fflush(stdout);
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  fflush(stderr);
}
