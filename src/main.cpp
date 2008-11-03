/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#include "core.h"
#include "vm.h"
#include "interpreter.h"

#define MULTI_VM_TEST   0

int             main_command_line_argc;
char* const*    main_command_line_argv;

#if _MSC_VER
__declspec(thread) VM* s_current_vm;
#elif __APPLE_CC__
pthread_key_t s_current_vm;
#else
__thread VM* s_current_vm;
#endif

// --heap-limit=32   -> 32MB (default)
// --heap-limit=1024 -> 1GB

static int opt_heap_limit(int argc, char* const argv[])
{
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
            if ((sscanf(param, "%d", &tmp) == 1) && (tmp >= 16) && (tmp < 2048)) {
                value = tmp;
            } else {
                fprintf(stderr, "** ERROR in option '--heap-limit=%s': parameter should be in the range of 16 .. 2047\n", param);
                exit(EXIT_FAILURE);
            }
        }
        
    }
    return value;
}

#if MULTI_VM_TEST

static void*
multi_vm_test(void* param)
{
    VM* vm = (VM*)param;
    printf("sub_vm: %x\n", vm);
    s_current_vm = vm;
    vm->boot();
    vm->standalone();
    printf("sub_vm: %x terminated\n", vm);
    return 0;    
}

#endif

#if _MSC_VER

    int main(int argc, char* argv[])
    {
        assert(isnan(VALUE_NAN));
        assert(isinf(VALUE_INF));
        {
            WSADATA wsd;
            if(WSAStartup(MAKEWORD(2,0),&wsd) != 0) {
                fatal("WSAStartup failed");
            }
        }

        main_command_line_argc = argc;
        main_command_line_argv = argv;
                
        object_heap_t* heap = new object_heap_t;
        int heap_limit = opt_heap_limit(argc, argv) * 1024 * 1024;
        int heap_init = heap_limit > 8388608 ? 8388608 : heap_limit;
        heap->init(heap_limit, heap_init);
        VM rootVM;
        rootVM.init(heap);
        s_current_vm = &rootVM;
        rootVM.boot();
        rootVM.standalone();
        
        WSACleanup();
        return 0;
    }

#else

    static void*
    signal_waiter(void* param)
    {
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

    int main(int argc, char* const argv[])
    {
        main_command_line_argc = argc;
        main_command_line_argv = argv;
        
    #ifndef NDEBUG
        struct foo { char i; };
        struct bar { int i; struct foo o; };
        struct hoge { struct bar m; char k; };
        printf("sizeof(int) %d\n", sizeof(int));
        printf("sizeof(long) %d\n", sizeof(long));
        printf("sizeof(long long) %d\n", sizeof(long long));
        printf("sizeof(void*) %d\n", sizeof(void*));

        printf("sizeof(foo) %d\n", sizeof(foo)); // 1
        printf("sizeof(bar) %d\n", sizeof(bar)); // 8
        printf("sizeof(hoge) %d\n", sizeof(hoge));
        printf("sizeof(bool) %d\n", sizeof(bool));
        printf("sizeof(size_t) %d\n", sizeof(size_t));
        printf("__alignof__(double) %d\n", __alignof__(double)); // 8
        printf("FIXNUM_MAX %d %x\n", FIXNUM_MAX, FIXNUM_MAX);
        printf("FIXNUM_MIN %d %x\n", FIXNUM_MIN, FIXNUM_MIN);
        printf("sizeof(pthread_mutex_t) %d\n", sizeof(pthread_mutex_t));
        printf("sizeof(pthread_cond_t) %d\n", sizeof(pthread_cond_t));
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

        object_heap_t* heap = new object_heap_t;
        int heap_limit = opt_heap_limit(argc, argv) * 1024 * 1024;
        int heap_init = heap_limit > 8388608 ? 8388608 : heap_limit;
        
#ifndef NDEBUG
        printf("heap_limit %d heap_init %d\n", heap_limit, heap_init);
#endif
        
        heap->init_primordial(heap_limit, heap_init);
        VM rootVM;
        rootVM.init(heap);
#if __APPLE_CC__
        MTVERIFY(pthread_key_create(&s_current_vm, NULL));
        MTVERIFY(pthread_setspecific(s_current_vm, &rootVM));
#else
        s_current_vm = &rootVM;
#endif
        
#if MULTI_VM_TEST
        VM subVM;
        {
            object_heap_t* sub_heap = new object_heap_t;
            int heap_limit = opt_heap_limit(argc, argv) * 1024 * 1024;
            int heap_init = heap_limit > 8388608 ? 8388608 : heap_limit;
            sub_heap->init(heap_limit, heap_init);
            subVM.init(sub_heap);
            pthread_t tid;
            MTVERIFY(pthread_create(&tid, NULL, multi_vm_test, &subVM));
            MTVERIFY(pthread_detach(tid));
        }
#endif        
#if USE_PARALLEL_VM
        Interpreter interp;
        interp.init(&rootVM, 128);
        rootVM.boot();
        rootVM.standalone();
#else
        rootVM.boot();
        rootVM.standalone();
#endif
        return 0;
    }

#endif

void fatal(const char* fmt, ...)
{
    fflush(stdout);
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end(ap);
    fprintf(stderr, "\n");
    fflush(stderr);
    exit(EXIT_FAILURE);
}

void warning(const char* fmt, ...)
{
    fflush(stdout);
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end(ap);
    fflush(stderr);
}

void trace(const char* fmt, ...)
{
    fflush(stdout);
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end(ap);
    fflush(stderr);
}
