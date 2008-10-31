/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#include "core.h"
#include "hash.h"
#include "list.h"
#include "arith.h"
#include "interpreter.h"
#include "vm.h"
#include "port.h"
#include "printer.h"

#if USE_PARALLEL_VM

void
Interpreter::init(VM* root, int n)
{
    m_lock.init();
    m_count = n;
    m_table = new vm_table_rec_t* [m_count];
    for (int i = 0; i < m_count; i++) {
        m_table[i] = new vm_table_rec_t;
        m_table[i]->interp = this;
        m_table[i]->notify.init();
        m_table[i]->state = VM_STATE_FREE;
    }
    root->m_interp = this;
    m_table[0]->interp = this;
    m_table[0]->state = VM_STATE_RUNNING;
    m_table[0]->vm = root;
    m_table[0]->parent = VM_PARENT_NONE;
    m_table[0]->param = scm_nil;
}

int
Interpreter::vm_id(VM* vm)
{
    for (int i = 0; i < m_count; i++) {
        if (m_table[i]->state == VM_STATE_FREE) continue;
        if (vm == m_table[i]->vm) return i;
    }
    fatal("%s:%u internal error: unknown vm", __FILE__, __LINE__);
}

int
Interpreter::spawn(VM* parent, scm_closure_t func, int argc, scm_obj_t argv[])
{
    scoped_lock lock(m_lock);
    for (int i = 0; m_count; i++) {
        if (m_table[i]->state == VM_STATE_FREE) {

            object_heap_t* heap = new object_heap_t;
            int heap_limit = DEFAULT_HEAP_LIMIT * 1024 * 1024;
            int heap_init = heap_limit > 8388608 ? 8388608 : heap_limit;
            heap->init(heap_limit, heap_init, parent->m_heap);

            VM* vm = new VM;
            vm->m_heap = heap;
            vm->m_stack_size = VM_STACK_BYTESIZE;
            vm->m_stack_top = (scm_obj_t*)vm->m_heap->allocate(vm->m_stack_size, false, false);
            vm->m_stack_limit = (scm_obj_t*)((intptr_t)vm->m_stack_top + vm->m_stack_size);
            memset(vm->m_stack_top, 0, vm->m_stack_size);
            vm->m_to_stack_top = (scm_obj_t*)vm->m_heap->allocate(vm->m_stack_size, false, false);
            vm->m_to_stack_limit = (scm_obj_t*)((intptr_t)vm->m_to_stack_top + vm->m_stack_size);
            memset(vm->m_to_stack_top, 0, vm->m_stack_size);

            vm->m_interp = parent->m_interp;
            vm->m_bootport = (scm_port_t)scm_unspecified;
            vm->m_current_environment = vm->m_heap->m_interaction_environment;
            vm->m_current_input = parent->m_current_input;
            vm->m_current_output = parent->m_current_output;
            vm->m_current_error = parent->m_current_error;
            vm->m_current_source_comments = scm_false;
            vm->m_current_exception_handler = scm_false;
            vm->m_current_dynamic_environment = clone_weakhashtable(vm->m_heap, parent->m_current_dynamic_environment, false);
            vm->m_current_dynamic_wind_record = scm_nil;
            memcpy(&vm->flags, &parent->flags, sizeof(parent->flags));

            vm->run(true);
            vm->reset();

            for (int n = 0; n < argc; n++) vm->m_sp[n] = argv[n];
            vm->m_sp += argc;
            vm_env_t env = (vm_env_t)vm->m_sp;
            env->count = argc;
            env->up = func->env;
            vm->m_sp = vm->m_fp = (scm_obj_t*)(env + 1);
            vm->m_pc = func->code;
            vm->m_env = &env->up;

            m_table[i]->parent = vm_id(parent);
            m_table[i]->vm = vm;
            m_table[i]->id = i;
            m_table[i]->state = VM_STATE_START;
            thread_start(mutator_thread, m_table[i]);
            if (argc > 0) {
                scm_obj_t obj = scm_nil;
                for (int n = argc - 1; n >= 0; n--) obj = make_pair(parent->m_heap, argv[n], obj);
                m_table[i]->param = make_pair(parent->m_heap, func, obj);
            } else {
                m_table[i]->param = make_list(parent->m_heap, 1, func);
            }
            return i;
        }
    }
    fatal("%s:%u internal error: thread table overflow, expansion not implemented", __FILE__, __LINE__);
}

thread_main_t
Interpreter::mutator_thread(void* param)
{
    vm_table_rec_t* table_rec = (vm_table_rec_t*)param;
    Interpreter* interp = table_rec->interp;
    interp->m_lock.lock();
    VM& vm = *table_rec->vm;
    set_current_vm(&vm);
    table_rec->state = VM_STATE_RUNNING;
    interp->m_lock.unlock();

loop:
    try {
        vm.run(false);
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

    vm.m_heap->m_collector_lock.lock();
    vm.m_heap->m_collector_terminating = true;
    vm.m_heap->m_collector_wake.signal();
    vm.m_heap->m_collector_lock.unlock();
    while (true) {
        vm.m_heap->m_collector_lock.lock();
        if (vm.m_heap->m_collector_terminating == false) {
            vm.m_heap->m_collector_lock.unlock();
            break;
        }
        while (vm.m_heap->m_stop_the_world) {
            vm.m_heap->m_mutator_stopped = true;
            vm.m_heap->m_collector_wake.signal();
            vm.m_heap->m_mutator_wake.wait(vm.m_heap->m_collector_lock);
            vm.m_heap->m_mutator_stopped = false;
        }
        vm.m_heap->m_collector_wake.signal();
        vm.m_heap->m_collector_lock.unlock();
        thread_yield();
    }

wait_again:
    {
        scoped_lock lock(interp->m_lock);
        for (int i = 0; i < interp->m_count; i++) {
            switch (interp->m_table[i]->state) {
            case VM_STATE_FREE:
                break;
            case VM_STATE_START:
            case VM_STATE_SYNC:
            case VM_STATE_RUNNING:
                if (interp->m_table[i]->parent == table_rec->id) {
                    table_rec->state = VM_STATE_SYNC;
                    table_rec->notify.wait(interp->m_lock);
                    goto wait_again;
                }
                break;
            }
        }
    }

    vm.m_heap->destroy();
    delete vm.m_heap;
    delete &vm;

    interp->m_lock.lock();
    table_rec->state = VM_STATE_FREE;
    if (table_rec->parent != VM_PARENT_NONE) interp->m_table[table_rec->parent]->notify.signal();
    interp->m_lock.unlock();
    return NULL;
}

void
Interpreter::display_status(VM* vm)
{
    scm_port_t port = vm->m_current_output;
    scoped_lock lock1(port->lock);
    Interpreter* interp = vm->m_interp;
    scoped_lock lock2(interp->m_lock);
    Interpreter::vm_table_rec_t** table = interp->m_table;

    port_puts(port, "\n  ID ADRS       STATUS\n");
    for (int i = 0; i < interp->m_count; i++) {
        Interpreter::vm_table_rec_t* rec = table[i];
        const char* stat = "unknown";
        scm_obj_t param = scm_nil;
        switch (rec->state) {
            case VM_STATE_FREE:
                continue;
            case VM_STATE_START:
            case VM_STATE_RUNNING:
                stat = "active";
                param = rec->param;
                break;
            case VM_STATE_SYNC:
                stat = "sync";
                param = rec->param;
                break;
            default: break;
        }
        if (rec->parent == VM_PARENT_NONE) {
            port_format(port, "  %2d 0x%08lx active\n", i, rec->vm);
        } else {
            port_format(port, "  %2d 0x%08lx %-6s", i, rec->vm, stat);
            if (param != scm_nil) printer_t(vm, port).format(" ~r", param);
            port_format(port, "\n");
        }
    }
}

// make-shared-queue
// shared-queue-set!
// shared-queue-get

#endif
