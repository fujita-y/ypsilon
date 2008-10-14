/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#include "core.h"
#include "hash.h"
#include "list.h"
#include "vm.h"

#if USE_PARALLEL_VM

#if _MSC_VER
unsigned int __stdcall
#else
void*
#endif
VM::mutator_thread(void* param)
{
    VM& vm = *(VM*)param;
    set_current_vm(&vm);
loop:
    try {
        vm.run(false);
        return vm.m_value;
    } catch (vm_exit_t& e) {
        exit(e.m_code);
    } catch (vm_exception_t& e) {
        vm.backtrace(vm.m_current_error);
    } catch (io_exception_t& e) {
        if (e.m_err == EINTR) goto loop;
        if (e.m_err == EIO) goto loop;
        fatal("fatal in thread(0x%x): unexpected io_expecption_t(%d, %s)", vm, e.m_err, e.m_message);
    } catch (reader_exception_t& e) {
        fatal("fatal in thread(0x%x): unhandled exception reader_expecption_t(%s)", vm, e.m_message);
    } catch (io_codec_exception_t& e) {
        fatal("fatal in thread(0x%x): unhandled exception io_codec_exception_t(%d, %s)", vm, e.m_operation, e.m_message);
    } catch (vm_escape_t& e) {
        fatal("fatal in thread(0x%x): unhandled exception vm_escape_t, maybe (escape) procedure in bad context", vm);
    } catch (vm_continue_t& e) {
        fatal("fatal in thread(0x%x): unhandled exception vm_continue_t, maybe (escape) procedure in bad context", vm);
    } catch (int code) {
        fatal("fatal in thread(0x%x): unexpected exception (errno %d, %s)", vm, code, strerror(code));
    } catch (...) {
        fatal("fatal in thread(0x%x): unknown exception", vm);
    }
}
 
scm_obj_t
VM::pmap(scm_closure_t func, scm_obj_t lst)
{
    int n = list_length(lst);
    pthread_t* p = (pthread_t*)alloca(sizeof(pthread_t) * n);
    int i = 0;
    while (PAIRP(lst)) {
        p[i++] = spawn(func, CAR(lst));
        lst = CDR(lst);
    }
    lst = scm_nil;
    for (int th = n - 1; th >= 0; th--) {
        void* obj = thread_join(p[th]);
        lst = make_pair(m_heap, obj, lst);
    }
    // todo: deep copy, cleanup symbols and literals
    return lst;
}

pthread_t
VM::spawn(scm_closure_t func, scm_obj_t arg)
{
    assert(CLOSUREP(func));
    VM* vm = new VM;
    
    object_heap_t* heap = new object_heap_t;
    int heap_init = 8388608;
    int heap_limit = heap_init * 2;
    heap->init(heap_limit, heap_init);
    vm->m_heap = heap;
    vm->m_heap->m_primordial_heap = m_heap->m_primordial_heap;
        
    vm->m_stack_size = VM_STACK_BYTESIZE;
    vm->m_stack_top = (scm_obj_t*)vm->m_heap->allocate(vm->m_stack_size, false, false);
    vm->m_stack_limit = (scm_obj_t*)((intptr_t)vm->m_stack_top + vm->m_stack_size);
    memset(vm->m_stack_top, 0, vm->m_stack_size);
    vm->m_to_stack_top = (scm_obj_t*)vm->m_heap->allocate(vm->m_stack_size, false, false);
    vm->m_to_stack_limit = (scm_obj_t*)((intptr_t)vm->m_to_stack_top + vm->m_stack_size);
    memset(vm->m_to_stack_top, 0, vm->m_stack_size);
    
    vm->m_bootport = (scm_port_t)scm_unspecified;
    vm->m_current_environment = m_current_environment;
    vm->m_current_input = m_current_input;
    vm->m_current_output = m_current_output;
    vm->m_current_error = m_current_error;
    vm->m_current_source_comments = scm_false;
    vm->m_current_exception_handler = scm_false;
    vm->m_current_dynamic_environment = m_current_dynamic_environment;
    vm->m_current_dynamic_wind_record = scm_nil;
    memcpy(&vm->flags, &flags, sizeof(flags));
    
    vm->run(true);    
    vm->reset();
    
    vm->m_sp[0] = arg;
    vm->m_sp++;
    vm_env_t env = (vm_env_t)vm->m_sp;
    env->count = 1;
    env->up = func->env;
    vm->m_sp = vm->m_fp = (scm_obj_t*)(env + 1);
    vm->m_pc = func->code;
    vm->m_env = &env->up;

    return thread_call(mutator_thread, vm);
}

#endif
