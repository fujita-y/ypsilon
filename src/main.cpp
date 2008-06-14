/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#include "core.h"
#include "vm.h"

int             main_command_line_argc;
char* const*    main_command_line_argv;
VM*             s_current_vm;

#if _MSC_VER

    int main(int argc, char* argv[])
    {
        assert(isnan(VALUE_NAN));
        assert(isinf(VALUE_INF));
    
        main_command_line_argc = argc;
        main_command_line_argv = argv;
        object_heap_t* heap = new object_heap_t;
        heap->init(OBJECT_SLAB_SIZE * 8192, OBJECT_SLAB_SIZE * 2048);
        VM rootVM;
        rootVM.init(heap);
        s_current_vm = &rootVM;
        rootVM.boot();
    }
    
#else

    void standalone_loop()
    {
        object_heap_t* heap = new object_heap_t;
        heap->init(OBJECT_SLAB_SIZE * 8192, OBJECT_SLAB_SIZE * 2048);
        VM rootVM;
        rootVM.init(heap);
        s_current_vm = &rootVM;
        rootVM.boot();
    }

    void* scm_standalone(void* arg)
    {
        standalone_loop();
        return ((void*)0);
    }

    static void*
    signal_waiter(void* param)
    {
        sigset_t* set = (sigset_t*)param;
        while (true) {
            int sig;
            sigwait(set, &sig);
            if (sig == SIGHUP) {
                fprintf(stderr,": SIGHUP\n");
                exit(0);
            }
            if (sig == SIGTERM) {
                fprintf(stderr,": SIGTERM\n");
                exit(0);
            }
            if (sig == SIGQUIT) {
                fprintf(stderr,": SIGQUIT\n");
                exit(0);
            }
            if (sig == SIGKILL) {
                fprintf(stderr,": SIGKILL\n");
                exit(0);
            }
            if (sig == SIGABRT) {
                fprintf(stderr,": SIGABRT\n");
                exit(0);
            }
            if (sig == SIGINT) {
                fprintf(stderr,": SIGINT\n");
                continue;
            }
            if (sig == SIGTSTP) {
                fprintf(stderr,": SIGTSTP\n");
                continue;
            }
            if (sig == SIGCONT) {
                fprintf(stderr,": SIGCONT\n");
                continue;
            }		
            fprintf(stderr,";; ### UNHANDLED SIGNAL %d ###\n", sig);
            exit(0);
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
        printf("sizeof(foo) %d\n", sizeof(foo)); // 1
        printf("sizeof(bar) %d\n", sizeof(bar)); // 8
        printf("sizeof(hoge) %d\n", sizeof(hoge)); 
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

        sigset_t set;
        sigemptyset(&set);
        sigaddset(&set, SIGINT);
        MTVERIFY(pthread_sigmask(SIG_BLOCK, &set, NULL));        
        {
            pthread_t tid;
            MTVERIFY(pthread_create(&tid, NULL, signal_waiter, &set));
            MTVERIFY(pthread_detach(tid));
        }
        scm_standalone(NULL);
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
