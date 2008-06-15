/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#include "core.h"
#include "vm.h"

#define CONS(a, d)      make_pair(m_heap, (a), (d))
#define LIST1(e1)       CONS((e1), scm_nil)
#define LIST2(e1, e2)   CONS((e1), LIST1((e2)))

#if USE_FIXNUM_THREAD

  #define INST_CALL          (opcode_to_instruction(VMOP_CALL))
  #define INST_LOAD_CONST    (opcode_to_instruction(VMOP_CONST))
  #define INST_PUSH_CONST    (opcode_to_instruction(VMOP_PUSH_CONST))
  #define INST_APPLY         (opcode_to_instruction(VMOP_APPLY))
  #define INST_PUSH          (opcode_to_instruction(VMOP_PUSH))
  #define INST_PUSH_SUBR     (opcode_to_instruction(VMOP_PUSH_SUBR))
  #define INST_RET_CONST     (opcode_to_instruction(VMOP_RET_CONST))
  #define INST_VM_ESCAPE     (opcode_to_instruction(VMOP_VM_ESCAPE))

#else

  #define INST_CALL          (opcode_to_instruction(VMOP_CALL))
  #define INST_LOAD_CONST    (opcode_to_instruction(VMOP_CONST))
  #define INST_PUSH_CONST    (opcode_to_instruction(VMOP_PUSH_CONST))
  #define INST_APPLY         (opcode_to_instruction(VMOP_APPLY))
  #define INST_PUSH          (opcode_to_instruction(VMOP_PUSH))
  #define INST_PUSH_SUBR     (opcode_to_instruction(VMOP_PUSH_SUBR))
  #define INST_RET_CONST     (opcode_to_instruction(VMOP_RET_CONST))
  #define INST_VM_ESCAPE     (opcode_to_instruction(VMOP_VM_ESCAPE))

#endif

/*
    apply-scheme:

    tail-call via apply.*:
        (#<unspecified>
            (push.const <argv[0]>)
            (push.const <argv[1]>)
            ...
            (load.const <proc>)
            (apply))

    normal-call via subr:
        (#<unspecified>
            (call
                (push.const <argv[0]>)
                (push.const <argv[1]>)
                ...
                (load.const <proc>)
                (apply))
            #<continuation CDR(m_pc)>)

    normal-call via push.subr:
        (#<unspecified>
            (call
                (push.const <argv[0]>)
                (push.const <argv[1]>)
                ...
                (load.const <proc>)
                (apply))
            (push)
            #<continuation CDR(m_pc)>)
*/

void
VM::apply_scheme(scm_obj_t proc, int argc, ...)
{
    assert(SUBRP(proc) || CLOSUREP(proc));

    scm_obj_t code = LIST2(CONS(INST_LOAD_CONST, proc), LIST1(INST_APPLY));
    scm_obj_t args = scm_nil;

    va_list ap;
    va_start(ap, argc);
    for (int i = 0; i < argc; i++) args = CONS(va_arg(ap, scm_obj_t), args);
    va_end(ap);

    for (int i = 0; i < argc; i++) {
        code = CONS(CONS(INST_PUSH_CONST, CAR(args)), code);
        args = CDR(args);
    }

    if (CDR(m_pc) == scm_nil) {
        code = CONS(scm_unspecified, code);
    } else {
        if (PAIRP(CAR(m_pc)) && CAAR(m_pc) == INST_PUSH_SUBR) {
            code = CONS(scm_unspecified, CONS(CONS(INST_CALL, code), CONS(LIST1(INST_PUSH) , CDR(m_pc))));
        } else {
            code = CONS(scm_unspecified, CONS(CONS(INST_CALL, code), CDR(m_pc)));
        }
    }

    m_pc = code;
}

void
VM::apply_scheme_argv(scm_obj_t proc, int argc, scm_obj_t argv[])
{
    assert(SUBRP(proc) || CLOSUREP(proc));

    scm_obj_t code = LIST2(CONS(INST_LOAD_CONST, proc), LIST1(INST_APPLY));

    for (int i = 1; i <= argc; i++) {
        code = CONS(CONS(INST_PUSH_CONST, argv[argc - i]), code);
    }

    if (CDR(m_pc) == scm_nil) {
        code = CONS(scm_unspecified, code);
    } else {
        if (CAAR(m_pc) == INST_PUSH_SUBR) {
            code = CONS(scm_unspecified, CONS(CONS(INST_CALL, code), CONS(LIST1(INST_PUSH) , CDR(m_pc))));
        } else {
            code = CONS(scm_unspecified, CONS(CONS(INST_CALL, code), CDR(m_pc)));
        }
    }

    m_pc = code;
}

/*
    call-scheme

    tail-call:
        ((call
            (push.const <argv[0]>)
            (push.const <argv[1]>)
            ...
            (load.const <proc>)
            (apply))
         (vm.escape))

    normal-call:
        ((call
            (push.const <argv[0]>)
            (push.const <argv[1]>)
            ...
            (load.const <proc>)
            (apply))
         (vm.escape))

    tail-return:
        (#<unspecified> (ret.const m_value))

    normal-return:
        (#<unspecified> #<continuation CDR(m_pc)>)

*/

scm_obj_t
VM::call_scheme_stub(scm_obj_t proc, int argc, scm_obj_t argv[])
{
    assert(SUBRP(proc) || CLOSUREP(proc));

    scm_obj_t code = LIST2(CONS(INST_LOAD_CONST, proc), LIST1(INST_APPLY));

    for (int i = 1; i <= argc; i++) {
        code = CONS(CONS(INST_PUSH_CONST, argv[argc - i]), code);
    }

    if (m_sp >= m_stack_limit) collect_stack(sizeof(scm_obj_t));

    scm_obj_t cont_pc = CDR(m_pc);
    m_sp[0] = cont_pc;
    m_sp++;

    m_pc = LIST2(CONS(INST_CALL, code), LIST1(INST_VM_ESCAPE));
    run(false);

    --m_sp;
    cont_pc = m_sp[0];

    if (cont_pc == scm_nil) {
        code = LIST2(scm_false, CONS(INST_RET_CONST, m_value));
    } else {
        code = CONS(scm_false, cont_pc);
    }

    m_pc = code;
    return m_value;
}

scm_obj_t
VM::call_scheme_argv(scm_obj_t proc, int argc, scm_obj_t argv[])
{
    assert(SUBRP(proc) || CLOSUREP(proc));
    scm_obj_t assistant = lookup_system_closure(".@apply-scheme-proc-assistant");
    scm_obj_t* param = (scm_obj_t*)alloca(sizeof(scm_obj_t) * (argc + 1));
    param[0] = proc;
    for (int i = 0; i < argc; i++) param[i + 1] = argv[i];
    try {
        return call_scheme_stub(assistant, argc + 1, param);
    } catch (vm_escape_t& e) {
        if (CDR(m_pc) == scm_nil) throw vm_continue_t();
        fatal("fatal in apply: <#subr escape> should be at tail position");
    } catch (vm_continue_t& e) {
        fatal("fatal in apply: unexpected exception vm_continue_t, maybe (escape) procedure in bad context");
    } catch (vm_exception_t& e) {
        fatal("fatal in apply: unexpected exception vm_exception_t");
    } catch (reader_exception_t& e) {
        fatal("fatal in apply: unexpected exception reader_expecption_t(%s)", e.m_message);
    } catch (io_exception_t& e) {
        fatal("fatal in apply: unexpected exception io_expecption_t(%d, %s)", e.m_err, e.m_message);
    } catch (io_codec_exception_t& e) {
        fatal("fatal in apply: unexpected exception io_codec_exception_t(%d, %s)", e.m_operation, e.m_message);
    } catch (vm_exit_t& e) {
        throw;
    } catch (int code) {
        fatal("fatal in apply: unexpected exception (errno %d, %s)", code, strerror(code));
    } catch (...) {
        fatal("fatal in apply: unknown exception");
    }
}

scm_obj_t
VM::call_scheme(scm_obj_t proc, int argc, ...)
{
    assert(SUBRP(proc) || CLOSUREP(proc));
    scm_obj_t assistant = lookup_system_closure(".@apply-scheme-proc-assistant");
    scm_obj_t* param = (scm_obj_t*)alloca(sizeof(scm_obj_t) * (argc + 1));
    param[0] = proc;

    va_list ap;
    va_start(ap, argc);
    for (int i = 0; i < argc; i++) param[i + 1] = va_arg(ap, scm_obj_t);
    va_end(ap);

    try {
        return call_scheme_stub(assistant, argc + 1, param);
    } catch (vm_escape_t& e) {
        if (CDR(m_pc) == scm_nil) throw vm_continue_t();
        fatal("fatal in apply: <#subr escape> should be at tail position");
    } catch (vm_continue_t& e) {
        fatal("fatal in apply: unexpected exception vm_continue_t, maybe (escape) procedure in bad context");
    } catch (vm_exception_t& e) {
        fatal("fatal in apply: unexpected exception vm_exception_t");
    } catch (reader_exception_t& e) {
        fatal("fatal in apply: unexpected exception reader_expecption_t(%s)", e.m_message);
    } catch (io_exception_t& e) {
        fatal("fatal in apply: unexpected exception io_expecption_t(%d, %s)", e.m_err, e.m_message);
    } catch (io_codec_exception_t& e) {
        fatal("fatal in apply: unexpected exception io_codec_exception_t(%d, %s)", e.m_operation, e.m_message);
    } catch (vm_exit_t& e) {
        throw;
    } catch (int code) {
        fatal("fatal in apply: unexpected exception (errno %d, %s)", code, strerror(code));
    } catch (...) {
        fatal("fatal in apply: unknown exception");
    }
}

#if !defined(NDEBUG) || STDEBUG

    #define STACKP(p)           (((p) >= (void*)m_stack_top) && ((p) < (void*)m_stack_limit))
    #define FORWARDP(p)         ((*(intptr_t*)(p)) & 1)

    void
    VM::check_vm_env(void* lnk)
    {
        if (lnk == NULL) return;
        if (((uintptr_t)lnk) & 0x03) {
            fatal("%s:%u check_state_env(): link is not valid pointer(maybe forwarded) 0x%x, m_env 0x%x, m_cont 0x%x", __FILE__, __LINE__, lnk, m_env, m_cont);
        }
        if (!m_heap->in_heap(lnk)) {
            fatal("%s:%u check_state_env(): invalid link, out of range 0x%x, m_env 0x%x, m_cont 0x%x", __FILE__, __LINE__, lnk, m_env, m_cont);
        }
        if (m_heap->in_heap(lnk)) {
            if (!STACKP(lnk)) {
                if (!m_heap->is_collectible(lnk)) {
                    fatal("%s:%u check_state_env(): env in bad container, 0x%x, m_env 0x%x, m_cont 0x%x", __FILE__, __LINE__, lnk, m_env, m_cont);
                }
                scm_obj_t obj = OBJECT_SLAB_TRAITS_OF(lnk)->cache->lookup(lnk);
                if (!HEAPENVP(obj)) {
                    fatal("%s:%u check_state_env(): env in improper object, 0x%x, m_env 0x%x, m_cont 0x%x", __FILE__, __LINE__, lnk, m_env, m_cont);
                }
            }
        }
        vm_env_t env = (vm_env_t)((intptr_t)lnk - offsetof(vm_env_rec_t, up));
        if (env->count < 0 || env->count > 256) {
            fatal("%s:%u check_state_env(): bad env->count, 0x%x count:%d, m_env 0x%x, m_cont 0x%x", __FILE__, __LINE__, env, env->count, m_env, m_cont);
        }
        scm_obj_t* top = (scm_obj_t*)((scm_obj_t*)env - env->count);
        for (int i = 0; i < env->count; i++) {
            if (CELLP(top[i])) {
                if (!m_heap->is_collectible(top[i])) {
                    fatal("%s:%u check_state_env(): env contain bad object, 0x%x object:0x0x, m_env 0x%x, m_cont 0x%x", __FILE__, __LINE__, env, top[i], m_env, m_cont);
                }
            }
        }
    }

    void
    VM::check_vm_cont(void* lnk)
    {
        if (lnk == NULL) return;
        if (((uintptr_t)lnk) & 0x03) {
            fatal("%s:%u check_state_cont(): link is not valid pointer(maybe forwarded) 0x%x, m_env 0x%x, m_cont 0x%x", __FILE__, __LINE__, lnk, m_env, m_cont);
        }
        if (!(STACKP(lnk) || m_heap->in_heap(lnk))) {
            fatal("%s:%u check_state_cont(): invalid link, out of range 0x%x, m_env 0x%x, m_cont 0x%x", __FILE__, __LINE__, lnk, m_env, m_cont);
        }
        if (m_heap->in_heap(lnk)) {
            if (!STACKP(lnk)) {
                if (!m_heap->is_collectible(lnk)) {
                    fatal("%s:%u check_state_cont(): cont in bad container, 0x%x, m_env 0x%x, m_cont 0x%x", __FILE__, __LINE__, lnk, m_env, m_cont);
                }
                scm_obj_t obj = OBJECT_SLAB_TRAITS_OF(lnk)->cache->lookup(lnk);
                if (!HEAPCONTP(obj)) {
                    fatal("%s:%u check_state_cont(): cont in improper object, 0x%x, m_env 0x%x, m_cont 0x%x", __FILE__, __LINE__, lnk, m_env, m_cont);
                }
            }
        }
        vm_cont_t cont = (vm_cont_t)((intptr_t)lnk - offsetof(vm_cont_rec_t, up));
        void* elnk = cont->env;
        while (elnk) {
            check_vm_env(elnk);
            elnk = (*(void**)elnk);
        }
    }

    void
    VM::check_vm_state()
    {
        void* lnk;

        lnk = m_env;
        while (lnk) {
            check_vm_env(lnk);
            lnk = (*(void**)lnk);
        }

        lnk = m_cont;
        while (lnk) {
            check_vm_cont(lnk);
            lnk = (*(void**)lnk);
        }
    }

#endif
