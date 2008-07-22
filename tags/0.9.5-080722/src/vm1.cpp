/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#include "core.h"
#include "vm.h"
#include "arith.h"
#include "printer.h"
#include "violation.h"

#define FOLD_TAIL_CALL_TRACE    1
#define UNWRAP_BACKTRACE        1

#define STACKP(p)           (((p) >= (void*)m_stack_top) & ((p) < (void*)m_stack_limit))
#define FORWARDP(p)         ((*(intptr_t*)(p)) & 1)
#define FORWARD(from,to)    ((*(intptr_t*)(from)) = ((intptr_t)(to) | 1))
#define RESOLVE(p)          ((void*)((*(intptr_t*)(p)) & (~1)))

static inline void
object_copy(void* dst, const void* src, int bsize)
{
    assert(bsize % sizeof(scm_obj_t) == 0);
    int c = bsize / sizeof(scm_obj_t);
    for (int i = 0; i < c; i++) ((scm_obj_t*)dst)[i] = ((scm_obj_t*)src)[i];
}

void*
VM::save_cont(void* lnk)
{
    if (!STACKP(lnk)) return lnk;
    void* up = save_cont(*(void**)lnk);
    vm_cont_t cont = (vm_cont_t)((intptr_t)lnk - offsetof(vm_cont_rec_t, up));
    cont->env = save_env(cont->env);
    int asize = (intptr_t)cont - (intptr_t)cont->fp;
    int fsize = asize + sizeof(vm_cont_rec_t);
    void* heap_top = new_heapcont_rec(m_heap, fsize);
    object_copy(heap_top, cont->fp, fsize);
    vm_cont_t heap_cont = (vm_cont_t)((intptr_t)heap_top + asize);
    heap_cont->up = up;
    heap_cont->fp = (scm_obj_t*)heap_top;
    return &heap_cont->up;
}

void
VM::save_stack()
{
    int argc = m_sp - m_fp;
    m_cont = save_cont(m_cont);
    m_env = save_env(m_env);
    update_cont(m_cont);
    memmove(m_stack_top, m_fp, sizeof(scm_obj_t) * argc);
    m_fp = m_stack_top;
    m_sp = m_stack_top + argc;
}

void*
VM::gc_env(void* lnk)
{
    if (!STACKP(lnk)) return lnk;
    if (FORWARDP(lnk)) return RESOLVE(lnk);
    void* up = gc_env(*(void**)lnk);
    vm_env_t env = (vm_env_t)((intptr_t)lnk - offsetof(vm_env_rec_t, up));
    int bytes = env->count * sizeof(scm_obj_t) + sizeof(vm_env_rec_t);
    object_copy(m_fp, (scm_obj_t*)env - env->count, bytes);
    vm_env_t to_env = (vm_env_t)(m_fp + env->count);
    m_fp = (scm_obj_t*)((intptr_t)m_fp + bytes);
    to_env->up = up;
    FORWARD(&env->up, &to_env->up);
    return &to_env->up;
}

void*
VM::gc_cont(void* lnk)
{
    if (!STACKP(lnk)) return lnk;
    void* up = gc_cont(*(void**)lnk);
    vm_cont_t cont = (vm_cont_t)((intptr_t)lnk - offsetof(vm_cont_rec_t, up));
    cont->env = gc_env(cont->env);
    int bytes = (intptr_t)cont - (intptr_t)cont->fp + sizeof(vm_cont_rec_t);
    object_copy(m_fp, cont->fp, bytes);
    m_fp = (scm_obj_t*)((intptr_t)m_fp + bytes);
    vm_cont_t to_cont = (vm_cont_t)((intptr_t)m_fp - sizeof(vm_cont_rec_t));
    to_cont->up = up;
    to_cont->fp = (scm_obj_t*)((intptr_t)m_fp - bytes);
    return &to_cont->up;
}

void
VM::collect_stack(int acquire)
{
    if (m_stack_busy) {
        save_stack();
        if (flags.m_collect_stack_notify != scm_false) {
            scoped_lock lock(m_current_output->lock);
            printer_t prt(this, m_current_output);
            prt.format("~&;; [collect-stack: store*]~%~!");
        }
        if ((uintptr_t)m_sp + acquire > (uintptr_t)m_stack_limit) {
            int current = (uintptr_t)m_stack_limit - (uintptr_t)m_stack_top;
            backtrace(m_current_error);
            fatal("fatal: vm stack overflow: can not handle more than %d arguments under current configuration", current / sizeof(scm_obj_t));
        }
        m_stack_busy = false;

  #if STDEBUG
        check_vm_state();
  #endif

        if (m_heap->m_stop_the_world) stop();
        return;
    }

    int argc = m_sp - m_fp;
    m_fp = m_to_stack_top;
    m_cont = gc_cont(m_cont);
    m_env = gc_env(m_env);
    object_copy(m_fp, m_sp - argc, sizeof(scm_obj_t) * argc);
    m_sp = m_fp + argc;

    scm_obj_t *tmp;
    tmp = m_stack_top;
    m_stack_top = m_to_stack_top;
    m_to_stack_top = tmp;
    tmp = m_stack_limit;
    m_stack_limit = m_to_stack_limit;
    m_to_stack_limit = tmp;

    if ((uintptr_t)m_sp + acquire >= (uintptr_t)m_stack_limit) {
        save_stack();
        if (flags.m_collect_stack_notify != scm_false) {
            scoped_lock lock(m_current_output->lock);
            printer_t prt(this, m_current_output);
            prt.format("~&;; [collect-stack: store**]~%~!");
        }
        m_stack_busy = true;
    } else {

        if (flags.m_collect_stack_notify != scm_false) {
            char buf[16];
            double rate = 1.0 - ((double)(m_sp - m_stack_top) / (double)(m_stack_limit - m_stack_top));
            snprintf(buf, sizeof(buf), "%.1lf%%", rate * 100.0);
            scoped_lock lock(m_current_output->lock);
            printer_t prt(this, m_current_output);
            prt.format("~&;; [collect-stack: %s free]~%~!", buf);
        }

        m_stack_busy = (m_sp - m_stack_top) > VM_STACK_BUSY_THRESHOLD(m_stack_limit - m_stack_top);
    }

  #ifndef NDEBUG
    if ((uintptr_t)m_sp + acquire > (uintptr_t)m_stack_limit) {
        backtrace(m_current_error);
        fatal("%s:%u stack overflow", __FILE__, __LINE__);
    }
  #endif

  #if STDEBUG
    check_vm_state();
  #endif

    if (m_heap->m_stop_the_world) stop();

}

void*
VM::save_env(void* root)
{
    vm_env_t current;
    vm_env_t env;
    if (STACKP(root)) {
        if (FORWARDP(root)) return RESOLVE(root);
        current = (vm_env_t)((intptr_t)root - offsetof(vm_env_rec_t, up));
        env = current;
        int bytes = env->count * sizeof(scm_obj_t) + sizeof(vm_env_rec_t);
        scm_obj_t* stack = (scm_obj_t*)env - env->count;
        scm_obj_t* heap = (scm_obj_t*)new_heapenv_rec(m_heap, bytes);
        assert(bytes % sizeof(scm_obj_t) == 0);
        int c = bytes / sizeof(scm_obj_t);
        for (int i = 0; i < c; i++) heap[i] = stack[i];
        intptr_t offset = (intptr_t)heap - (intptr_t)stack;
        root = (void*)((intptr_t)root + offset);
        env = (vm_env_t)((intptr_t)env + offset);
    } else {
        return root;
    }
    while (STACKP(env->up)) {
        if (FORWARDP(env->up)) {
            env->up = RESOLVE(env->up);
            break;
        }
        vm_env_t parent = (vm_env_t)((intptr_t)env->up - offsetof(vm_env_rec_t, up));
        int bytes = parent->count * sizeof(scm_obj_t) + sizeof(vm_env_rec_t);
        scm_obj_t* stack = (scm_obj_t*)parent - parent->count;
        scm_obj_t* heap = (scm_obj_t*)new_heapenv_rec(m_heap, bytes);
        assert(bytes % sizeof(scm_obj_t) == 0);
        int c = bytes / sizeof(scm_obj_t);
        for (int i = 0; i < c; i++) heap[i] = stack[i];
        intptr_t offset = (intptr_t)heap - (intptr_t)stack;
        vm_env_t heap_env = (vm_env_t)((intptr_t)parent + offset);
        FORWARD(&parent->up, &heap_env->up);
        env->up = &heap_env->up;
        env = heap_env;
    }
    FORWARD(&current->up, root);
    return root;
}

void
VM::update_cont(void* lnk)
{
    while (STACKP(lnk)) {
        vm_cont_t cont = (vm_cont_t)((intptr_t)lnk - offsetof(vm_cont_rec_t, up));
        if (cont->env && FORWARDP(cont->env)) cont->env = RESOLVE(cont->env);
        lnk = (*(void**)lnk);
    }
}

scm_obj_t*
VM::lookup_iloc(scm_obj_t operands)
{
    void* lnk = m_env;
    int level = ((intptr_t)CAR(operands)) - 1;
    while (level) { lnk = *(void**)lnk; level -= 2; }
    vm_env_t env = (vm_env_t)((intptr_t)lnk - offsetof(vm_env_rec_t, up));
    int offset = FIXNUM(CDR(operands));
    return (scm_obj_t*)env - env->count + offset;
}

#if USE_GCC_EXTENSION
  #if USE_DIRECT_THREAD
    volatile void* s_volatile_stub;
    #define PIN(tag)        do { s_volatile_stub = &&tag; } while(0)
    #define CASE(code)      M_##code: \
                            __asm__ ("ud2"); \
                            __asm__ (".p2align 3"); \
                            L_##code: \
                            __asm__ ("nop"); \
                            __asm__ ("nop"); \
                            __asm__ ("nop"); \
                            __asm__ ("nop"); \
                            __asm__ ("/* "#code" */");

    #define LABEL(code)     do { assert(code < array_sizeof(m_dispatch_table)); m_dispatch_table[code] = &&L_##code; s_volatile_stub = &&M_##code;} while(0)
    #define SWITCH()        goto *instruction_to_adrs(CAAR(m_pc));
  #else
    volatile void* s_volatile_stub;
    #define CASE(code)      M_##code: __asm__ ("/* "#code" */"); L_##code:
    #define LABEL(code)     do { assert(code < array_sizeof(m_dispatch_table)); m_dispatch_table[code] = &&L_##code; s_volatile_stub = &&M_##code;} while(0)
    #define SWITCH()        goto *m_dispatch_table[instruction_to_opcode(CAAR(m_pc))];
  #endif
#else
  #define CASE(code)        case code:
  #define SWITCH()          switch (instruction_to_opcode(CAAR(m_pc)))
#endif

#define OPERANDS            (CDAR(m_pc))

/*

C-SUBR return state

m_value    CAR(m_pc)*1       special function
---------------------------------------------------
scm_undef  scm_unspecified   call-scheme-proc
...        scm_false         call-scheme-modal-proc

*1: debug info

*/

void
VM::run(bool init_dispatch_table)
{
  #if USE_GCC_EXTENSION

    if (init_dispatch_table) {
        assert(VMOP_INSTRUCTION_COUNT < 128);
    #if USE_DIRECT_THREAD
        PIN(ERROR_BAD_INSTRUCTION_ALIGN_STUB);
        PIN(APPLY_ALIGN_STUB);
        PIN(APPLY_APPLY);
        PIN(APPLY_VALUES);
        PIN(APPLY_CONT);
        PIN(APPLY_CALLCC);
        PIN(APPLY_VARIADIC);
        PIN(COLLECT_STACK_ONE);
        PIN(COLLECT_STACK_CONT_REC);
        PIN(COLLECT_STACK_ENV_REC);
        PIN(COLLECT_STACK_ENV_REC_N_ONE);
        PIN(COLLECT_STACK_ENV_REC_N_ONE_N_APPLY);
        PIN(COLLECT_STACK_ENV_REC_N_OPERAND);
        PIN(FALLBACK_PUSH_NADD_ILOC);
        PIN(FALLBACK_EQ_N_ILOC);
        PIN(FALLBACK_LT_N_ILOC);
        PIN(FALLBACK_LE_N_ILOC);
        PIN(FALLBACK_GT_N_ILOC);
        PIN(FALLBACK_GE_N_ILOC);
        PIN(THUNK_TOUCH_GLOC_OF);
        PIN(THUNK_SUBR_GLOC_OF);
        PIN(THUNK_RET_SUBR_GLOC_OF);
        PIN(THUNK_PUSH_SUBR_GLOC_OF);
        PIN(ERROR_PUSH_NADD_ILOC);
        PIN(ERROR_EQ_N_ILOC);
        PIN(ERROR_LT_N_ILOC);
        PIN(ERROR_LE_N_ILOC);
        PIN(ERROR_GT_N_ILOC);
        PIN(ERROR_GE_N_ILOC);
        PIN(ERROR_PUSH_CAR_ILOC);
        PIN(ERROR_CAR_ILOC);
        PIN(ERROR_PUSH_CDR_ILOC);
        PIN(ERROR_CDR_ILOC);
        PIN(ERROR_PUSH_CADR_ILOC);
        PIN(ERROR_CADR_ILOC);
        PIN(ERROR_PUSH_CDDR_ILOC);
        PIN(ERROR_CDDR_ILOC);
        PIN(ERROR_PUSH_GLOC);
        PIN(ERROR_GLOC);
        PIN(ERROR_RET_GLOC);
        PIN(ERROR_TOUCH_GLOC);
        PIN(ERROR_APPLY_GLOC);
        PIN(ERROR_APPLY_WRONG_NUMBER_ARGS);
        PIN(ERROR_PROC_APPLY_WRONG_NUMBER_ARGS);
        PIN(ERROR_PROC_APPLY_BAD_LAST_ARGS);
        PIN(ERROR_APPLY_VALUES_WRONG_NUMBER_ARGS);
        PIN(ERROR_CALLCC_WRONG_NUMBER_ARGS);
        PIN(ERROR_INVALID_APPLICATION);
        PIN(ERROR_BAD_INSTRUCTION);
        PIN(BACK_TO_LOOP);
        PIN(BACK_TO_TRACE_N_LOOP);
    #endif
        for (int i = 0; i < array_sizeof(m_dispatch_table); i++) m_dispatch_table[i] = &&ERROR_BAD_INSTRUCTION;
        LABEL(VMOP_EXTEND_ENCLOSE);
        LABEL(VMOP_EXTEND_ENCLOSE_LOCAL);
        LABEL(VMOP_EXTEND_UNBOUND);
        LABEL(VMOP_PUSH_CLOSE);
        LABEL(VMOP_PUSH_CLOSE_LOCAL);
        LABEL(VMOP_ENCLOSE);
        LABEL(VMOP_EXTEND);

        LABEL(VMOP_PUSH);
        LABEL(VMOP_CALL);
        LABEL(VMOP_PUSH_CONST);
        LABEL(VMOP_PUSH_GLOC);
        LABEL(VMOP_PUSH_ILOC);
        LABEL(VMOP_PUSH_ILOC0);
        LABEL(VMOP_PUSH_ILOC1);
        LABEL(VMOP_PUSH_SUBR);
        LABEL(VMOP_PUSH_CAR_ILOC);
        LABEL(VMOP_PUSH_CDR_ILOC);
        LABEL(VMOP_PUSH_CADR_ILOC);
        LABEL(VMOP_PUSH_CDDR_ILOC);
        LABEL(VMOP_PUSH_NADD_ILOC);
        LABEL(VMOP_NADD_ILOC);
        LABEL(VMOP_PUSH_CONS);
        LABEL(VMOP_RET_SUBR);
        LABEL(VMOP_APPLY_GLOC);
        LABEL(VMOP_APPLY_ILOC);
        LABEL(VMOP_APPLY_ILOC_LOCAL);
        LABEL(VMOP_RET_CONS);
        LABEL(VMOP_RET_EQP);
        LABEL(VMOP_RET_NULLP);
        LABEL(VMOP_RET_PAIRP);
        LABEL(VMOP_APPLY);

        LABEL(VMOP_CONST);
        LABEL(VMOP_SUBR);
        LABEL(VMOP_CAR_ILOC);
        LABEL(VMOP_CDR_ILOC);
        LABEL(VMOP_CADR_ILOC);
        LABEL(VMOP_CDDR_ILOC);
        LABEL(VMOP_GLOC);
        LABEL(VMOP_ILOC);
        LABEL(VMOP_ILOC0);
        LABEL(VMOP_ILOC1);

        LABEL(VMOP_IF_TRUE);
        LABEL(VMOP_IF_FALSE_CALL);
        LABEL(VMOP_IF_NULLP);
        LABEL(VMOP_IF_PAIRP);
        LABEL(VMOP_IF_SYMBOLP);
        LABEL(VMOP_IF_EQP);

        LABEL(VMOP_RET_CONST);
        LABEL(VMOP_RET_GLOC);
        LABEL(VMOP_RET_ILOC);

        LABEL(VMOP_IF_TRUE_RET);
        LABEL(VMOP_IF_FALSE_RET);

        LABEL(VMOP_IF_TRUE_RET_CONST);
        LABEL(VMOP_IF_FALSE_RET_CONST);
        LABEL(VMOP_IF_NULLP_RET_CONST);
        LABEL(VMOP_IF_NOT_NULLP_RET_CONST);
        LABEL(VMOP_IF_PAIRP_RET_CONST);
        LABEL(VMOP_IF_NOT_PAIRP_RET_CONST);
        LABEL(VMOP_IF_SYMBOLP_RET_CONST);
        LABEL(VMOP_IF_NOT_SYMBOLP_RET_CONST);
        LABEL(VMOP_IF_EQP_RET_CONST);
        LABEL(VMOP_IF_NOT_EQP_RET_CONST);

        LABEL(VMOP_EQ_N_ILOC);
        LABEL(VMOP_LT_N_ILOC);
        LABEL(VMOP_LE_N_ILOC);
        LABEL(VMOP_GT_N_ILOC);
        LABEL(VMOP_GE_N_ILOC);

        LABEL(VMOP_CLOSE);
        LABEL(VMOP_SET_ILOC);
        LABEL(VMOP_SET_GLOC);

        LABEL(VMOP_RET_CLOSE);

        LABEL(VMOP_VM_ESCAPE);
        LABEL(VMOP_TOUCH_GLOC);
        LABEL(VMOP_SUBR_GLOC_OF);
        LABEL(VMOP_RET_SUBR_GLOC_OF);
        LABEL(VMOP_PUSH_SUBR_GLOC_OF);

        LABEL(VMOP_EQ_ILOC);
        LABEL(VMOP_LT_ILOC);
        LABEL(VMOP_LE_ILOC);
        LABEL(VMOP_GT_ILOC);
        LABEL(VMOP_GE_ILOC);

        // workaround for GCC bug
        for (int i = 0; i < array_sizeof(m_dispatch_table); i++) s_volatile_stub = m_dispatch_table[i];

    #if USE_DIRECT_THREAD && !defined(NDEBUG)
        for (int i = 0; i < array_sizeof(m_dispatch_table); i++) {
            if ((uintptr_t)m_dispatch_table[i] & 1) {
                fatal("%s:%u failed to initialize virtual machine for USE_DIRECT_THREAD", __FILE__, __LINE__);
            }
        }
        if ((uintptr_t)(&&ERROR_BAD_INSTRUCTION) & 1) {
            fatal("%s:%u failed to initialize virtual machine for USE_DIRECT_THREAD", __FILE__, __LINE__);
        }
    #endif

        return;
    }
  #else
    if (init_dispatch_table) return;
  #endif
    assert(PAIRP(m_pc));

    scm_obj_t operand_trace;
    scm_obj_t obj;

    bool fresh_start = true;
    goto begin;

resume:
    fresh_start = false;

begin:
    try {

        if (fresh_start) goto loop;
        goto pop_cont;

#if USE_GCC_EXTENSION
APPLY_ALIGN_STUB:
        __asm__ (".p2align 4");
#endif

apply:
  #if USE_GCC_EXTENSION
        __asm__ ("/* VM APPLY */");
  #endif
        if (CLOSUREP(m_value)) {
            if ((uintptr_t)m_sp + sizeof(vm_env_rec_t) + sizeof(scm_obj_t) < (uintptr_t)m_stack_limit) {
                scm_closure_t closure = (scm_closure_t)m_value;
                int args = HDR_CLOSURE_ARGS(closure->hdr);
                if (m_sp - m_fp != args) goto APPLY_VARIADIC;
                vm_env_t env = (vm_env_t)m_sp;
                env->count = args;
                env->up = closure->env;
                m_sp = m_fp = (scm_obj_t*)(env + 1);
                m_pc = closure->code;
                m_env = &env->up;
                goto trace_n_loop;
            }
            goto COLLECT_STACK_ENV_REC_N_ONE_N_APPLY;
        }

        if (SUBRP(m_value)) {
            scm_subr_t subr = (scm_subr_t)m_value;
            int argc = m_sp - m_fp;
            m_value = (*subr->adrs)(this, argc, m_fp);
            if (m_value != scm_undef) goto pop_cont;
            m_sp = m_fp;
            m_pc = CDR(m_pc);
            goto trace_n_loop;
        }

        goto APPLY_SPECIAL;

trace_n_loop:

        if (operand_trace != scm_nil) {
            if (m_trace == scm_unspecified) m_trace = operand_trace;
            else m_trace_tail = operand_trace;

  #ifndef NDEBUG
    #if 0
            {
                if (CDR(operand_trace) == scm_nil) {
                    // no info
                } else if (FIXNUMP(CDR(operand_trace))) {
                    // (path . fixnum) : loaded form
                    assert(STRINGP(CAR(operand_trace)));
                    scm_string_t string = (scm_string_t)CAR(operand_trace);
                    int comment = FIXNUM(CDR(operand_trace));
                    int line = comment / MAX_SOURCE_COLUMN;
                    int column = comment % MAX_SOURCE_COLUMN;
                    scoped_lock lock(m_current_output->lock);
                    printer_t(this, m_current_output).format("trace: %s line %d column %d~%~!", string->name, line, column);
                } else {
                    // (expr path . fixnum) : repl form
                    scm_string_t string = (scm_string_t)CADR(operand_trace);
                    int comment = FIXNUM(CDDR(operand_trace));
                    int line = comment / MAX_SOURCE_COLUMN;
                    scoped_lock lock(m_current_output->lock);
                    printer_t(this, m_current_output).format("trace: ~s  ... %s line %d~%~!", CAR(operand_trace), string->name, line);
                }
            }
    #endif
  #endif

        }

        goto loop;

pop_cont:
        if (m_cont == NULL) return;
        {
            vm_cont_t cont = (vm_cont_t)((intptr_t)m_cont - offsetof(vm_cont_rec_t, up));
            m_trace = cont->trace;
            m_fp = cont->fp;
            m_pc = cont->pc;
            m_env = cont->env;
            m_cont = cont->up;
            if (STACKP(cont)) {
                m_sp = (scm_obj_t*)cont;
            } else {
                int nargs = (scm_obj_t*)cont - (scm_obj_t*)cont->fp;
                {
                    const scm_obj_t* s = (scm_obj_t*)cont->fp;
                    scm_obj_t* d = (scm_obj_t*)m_stack_top;
                    for (int i = 0; i < nargs; i++) d[i] = s[i];
                }
                m_fp = m_stack_top;
                m_sp = m_fp + nargs;
            }
        }
        m_trace_tail = scm_unspecified;
        if (m_heap->m_stop_the_world) stop();
  #if USE_GCC_EXTENSION
        __asm__ ("/* VM INST DISPATCH */");
  #endif
loop:

        assert(m_sp <= m_stack_limit);
  #if USE_DIRECT_THREAD
        assert(VMINSTP(CAAR(m_pc)));
  #endif
  #if USE_FIXNUM_THREAD
        assert(FIXNUMP(CAAR(m_pc)));
  #endif
  #if USE_SYMBOL_THREAD
        assert(OPCODESYMBOLP(CAAR(m_pc)));
  #endif


  #if PROFILE_OPCODE
        {
            static int last_opecode;
            int opcode = instruction_to_opcode(CAAR(m_pc));
            if (m_opcode_profile[opcode].count < UINT64_MAX) {
                m_opcode_profile[opcode].count++;
            }
            if (m_opcode_profile[opcode].prev[last_opecode] < UINT64_MAX) {
                m_opcode_profile[opcode].prev[last_opecode]++;
            }
            last_opecode = opcode;
        }
  #endif

        SWITCH() {

            CASE(VMOP_IF_FALSE_CALL) {
                if (m_value == scm_false) goto ENT_VMOP_CALL;
                m_pc = CDR(m_pc);
                goto loop;
            }

            CASE(VMOP_CALL) ENT_VMOP_CALL: {
                if ((uintptr_t)m_sp + sizeof(vm_cont_rec_t) < (uintptr_t)m_stack_limit) {
                    vm_cont_t cont = (vm_cont_t)m_sp;
                    cont->trace = m_trace;
                    cont->fp = m_fp;
                    cont->pc = CDR(m_pc);
                    cont->env = m_env;
                    cont->up = m_cont;
                    m_sp = m_fp = (scm_obj_t*)(cont + 1);
                    m_cont = &cont->up;
                    m_pc = OPERANDS;
                    m_trace = m_trace_tail = scm_unspecified;
                    goto loop;
                }
                goto COLLECT_STACK_CONT_REC;
            }

            CASE(VMOP_RET_GLOC) {
                assert(GLOCP(OPERANDS));
                m_value = ((scm_gloc_t)OPERANDS)->value;
                if (m_value == scm_undef) goto ERROR_RET_GLOC;
                goto pop_cont;
            }

            CASE(VMOP_RET_CONST) {
                m_value = OPERANDS;
                goto pop_cont;
            }

            CASE(VMOP_RET_ILOC) {
                m_value = *lookup_iloc(OPERANDS);
                goto pop_cont;
            }

            CASE(VMOP_PUSH_GLOC) {
                if (m_sp < m_stack_limit) {
                    assert(GLOCP(OPERANDS));
                    scm_gloc_t gloc = (scm_gloc_t)OPERANDS;
                    scm_obj_t value = gloc->value;
                    if (value == scm_undef) goto ERROR_PUSH_GLOC;
                    m_sp[0] = value;
                    m_sp++;
                    m_pc = CDR(m_pc);
                    goto loop;
                }
                goto COLLECT_STACK_ONE;
            }

            CASE(VMOP_PUSH_SUBR) {
                scm_subr_t subr = (scm_subr_t)CAR(OPERANDS);
  #if PROFILE_SUBR
                subr->c_push++;
  #endif
                assert(SUBRP(subr));
                int argc = FIXNUM(CADR(OPERANDS));
                assert(argc > 0);
                m_value = (*subr->adrs)(this, argc, m_sp - argc);
                m_sp -= argc;
                assert(m_sp >= m_fp);
                if (m_value != scm_undef) {
                    m_sp[0] = m_value;
                    m_sp++;
                }
                m_pc = CDR(m_pc);
                goto loop;
            }

            CASE(VMOP_PUSH_CAR_ILOC) {
                if (m_sp < m_stack_limit) {
                    obj = *lookup_iloc(CAR(OPERANDS));
                    if (PAIRP(obj)) {
                        m_sp[0] = CAR(obj);
                        m_sp++;
                        m_pc = CDR(m_pc);
                        goto loop;
                    }
                    goto ERROR_PUSH_CAR_ILOC;
                }
                goto COLLECT_STACK_ONE;
            }

            CASE(VMOP_PUSH_CDR_ILOC) {
                if (m_sp < m_stack_limit) {
                    obj = *lookup_iloc(CAR(OPERANDS));
                    if (PAIRP(obj)) {
                        m_sp[0] = CDR(obj);
                        m_sp++;
                        m_pc = CDR(m_pc);
                        goto loop;
                    }
                    goto ERROR_PUSH_CDR_ILOC;
                }
                goto COLLECT_STACK_ONE;
            }

            CASE(VMOP_PUSH_ILOC) {
                if (m_sp < m_stack_limit) {
                    m_sp[0] = *lookup_iloc(OPERANDS);
                    m_sp++;
                    m_pc = CDR(m_pc);
                    goto loop;
                }
                goto COLLECT_STACK_ONE;
            }

            CASE(VMOP_PUSH) {
                if (m_sp < m_stack_limit) {
                    m_sp[0] = m_value;
                    m_sp++;
                    m_pc = CDR(m_pc);
                    goto loop;
                }
                goto COLLECT_STACK_ONE;
            }

            CASE(VMOP_PUSH_CONST) {
                if (m_sp < m_stack_limit) {
                    m_sp[0] = OPERANDS;
                    m_sp++;
                    m_pc = CDR(m_pc);
                    goto loop;
                }
                goto COLLECT_STACK_ONE;
            }

            CASE(VMOP_PUSH_ILOC1) {
                if (m_sp < m_stack_limit) {
                    void* lnk = *(void**)m_env;
                    vm_env_t env = (vm_env_t)((intptr_t)lnk - offsetof(vm_env_rec_t, up));
                    m_sp[0] = *((scm_obj_t*)env - env->count + FIXNUM(OPERANDS));
                    m_sp++;
                    m_pc = CDR(m_pc);
                    goto loop;
                }
                goto COLLECT_STACK_ONE;
            }

            CASE(VMOP_PUSH_ILOC0) {
                if (m_sp < m_stack_limit) {
                    vm_env_t env = (vm_env_t)((intptr_t)m_env - offsetof(vm_env_rec_t, up));
                    m_sp[0] = *((scm_obj_t*)env - env->count + FIXNUM(OPERANDS));
                    m_sp++;
                    m_pc = CDR(m_pc);
                    goto loop;
                }
                goto COLLECT_STACK_ONE;
            }

            CASE(VMOP_APPLY_GLOC) {
                operand_trace = CDR(OPERANDS);
                assert(GLOCP(CAR(OPERANDS)));
                m_value = ((scm_gloc_t)CAR(OPERANDS))->value;
                if (m_value == scm_undef) goto ERROR_APPLY_GLOC;
                goto apply;
            }

            CASE(VMOP_RET_SUBR) {
                operand_trace = CDR(OPERANDS);
                assert(SUBRP(CAR(OPERANDS)));
                scm_subr_t subr = (scm_subr_t)CAR(OPERANDS);
  #if PROFILE_SUBR
                subr->c_apply++;
  #endif
                int argc = m_sp - m_fp;
                m_value = (*subr->adrs)(this, argc, m_fp);
                m_sp = m_fp;
                assert(m_value != scm_undef || ((m_value == scm_undef) && (CAR(m_pc) == scm_unspecified)));
                if (m_value != scm_undef) goto pop_cont;
                m_pc = CDR(m_pc);
                goto trace_n_loop;
            }

            CASE(VMOP_APPLY_ILOC) {
                operand_trace = CDR(OPERANDS);
                m_value = *lookup_iloc(CAR(OPERANDS));
                goto apply;
            }

            CASE(VMOP_APPLY_ILOC_LOCAL) {
                if ((uintptr_t)m_sp + sizeof(vm_env_rec_t) < (uintptr_t)m_stack_limit) {
                    operand_trace = CDR(OPERANDS);
                    void* lnk = m_env;
                    int level = ((intptr_t)CAAR(OPERANDS)) - 1;
                    while (level) { lnk = *(void**)lnk; level -= 2; }
                    vm_env_t cenv = (vm_env_t)((intptr_t)lnk - offsetof(vm_env_rec_t, up));
                    scm_obj_t obj = *((scm_obj_t*)cenv - cenv->count + FIXNUM(CDAR(OPERANDS)));
                    vm_env_t env = (vm_env_t)m_sp;
                    env->count = m_sp - m_fp;
                    env->up = &cenv->up;
                    m_env = &env->up;
                    m_sp = m_fp = (scm_obj_t*)(env + 1);
                    m_pc = obj;
                    goto trace_n_loop;
                }
                goto COLLECT_STACK_ENV_REC;
            }

            CASE(VMOP_APPLY) {
                operand_trace = OPERANDS;
                goto apply;
            }

            CASE(VMOP_EXTEND) {
                if ((uintptr_t)m_sp + sizeof(vm_env_rec_t) < (uintptr_t)m_stack_limit) {
                    assert(FIXNUMP(OPERANDS));
                    int argc = FIXNUM(OPERANDS);
                    assert(argc == m_sp - m_fp);
                    vm_env_t env = (vm_env_t)m_sp;
                    env->count = argc;
                    env->up = m_env;
                    m_sp = m_fp = (scm_obj_t*)(env + 1);
                    m_env = &env->up;
                    m_pc = CDR(m_pc);
                    goto loop;
                }
                goto COLLECT_STACK_ENV_REC;
            }

            CASE(VMOP_EXTEND_ENCLOSE) {
                if ((uintptr_t)m_sp + sizeof(scm_obj_t) + sizeof(vm_env_rec_t) < (uintptr_t)m_stack_limit) {
                    m_sp[0] = scm_undef;
                    m_sp++;
                    vm_env_t env = (vm_env_t)m_sp;
                    env->count = 1;
                    env->up = m_env;
                    m_sp = m_fp = (scm_obj_t*)(env + 1);
                    m_env = &env->up;
                    m_env = save_env(m_env);
                    update_cont(m_cont);
                    env = (vm_env_t)((intptr_t)m_env - offsetof(vm_env_rec_t, up));
                    scm_obj_t* slot = (scm_obj_t*)env - 1;
  #if PREBIND_CLOSE
                    *slot = make_closure(m_heap, (scm_closure_t)OPERANDS, m_env);
  #else
                    scm_obj_t spec = CAR(OPERANDS);
                    scm_obj_t code = CDR(OPERANDS);
                    scm_obj_t doc = CDDR(spec);
                    *slot = make_closure(m_heap, FIXNUM(CAR(spec)), FIXNUM(CADR(spec)), m_env, code, doc);
  #endif

                    m_pc = CDR(m_pc);
  #if STDEBUG
                    check_vm_state();
  #endif
                    goto loop;
                }
                goto COLLECT_STACK_ENV_REC_N_ONE;
            }

            CASE(VMOP_EXTEND_ENCLOSE_LOCAL) {
                if ((uintptr_t)m_sp + sizeof(scm_obj_t) + sizeof(vm_env_rec_t) < (uintptr_t)m_stack_limit) {
                    m_sp[0] = OPERANDS;
                    m_sp++;
                    vm_env_t env = (vm_env_t)m_sp;
                    env->count = 1;
                    env->up = m_env;
                    m_sp = m_fp = (scm_obj_t*)(env + 1);
                    m_env = &env->up;
                    m_pc = CDR(m_pc);
  #if STDEBUG
                    check_vm_state();
  #endif
                    goto loop;
                }
                goto COLLECT_STACK_ENV_REC_N_ONE;
            }

            CASE(VMOP_EXTEND_UNBOUND) {
                assert(FIXNUMP(OPERANDS));
                int argc = FIXNUM(OPERANDS);
                if ((uintptr_t)m_sp + sizeof(vm_env_rec_t) + sizeof(scm_obj_t*) * argc < (uintptr_t)m_stack_limit) {
                    for (int i = 0; i < argc; i++) {
                        m_sp[0] = scm_undef;
                        m_sp++;
                    }
                    vm_env_t env = (vm_env_t)m_sp;
                    env->count = argc;
                    env->up = m_env;
                    m_sp = m_fp = (scm_obj_t*)(env + 1);
                    m_env = &env->up;
                    m_pc = CDR(m_pc);
                    goto loop;
                }
                goto COLLECT_STACK_ENV_REC_N_OPERAND;
            }

            CASE(VMOP_PUSH_CLOSE) {
                if (m_sp < m_stack_limit) {
                    if (STACKP(m_env)) {
                        m_env = save_env(m_env);
                        update_cont(m_cont);
                    }
  #if PREBIND_CLOSE
                    m_sp[0] = make_closure(m_heap, (scm_closure_t)OPERANDS, m_env);
  #else
                    scm_obj_t spec = CAR(OPERANDS);
                    scm_obj_t code = CDR(OPERANDS);
                    scm_obj_t doc = CDDR(spec);
                    m_sp[0] = make_closure(m_heap, FIXNUM(CAR(spec)), FIXNUM(CADR(spec)), m_env, code, doc);
  #endif
                    m_sp++;
                    m_pc = CDR(m_pc);
  #if STDEBUG
                    check_vm_state();
  #endif
                    goto loop;
                }
                goto COLLECT_STACK_ONE;
            }

            CASE(VMOP_PUSH_CLOSE_LOCAL) {
                if (m_sp < m_stack_limit) {
                    m_sp[0] = OPERANDS;
                    m_sp++;
                    m_pc = CDR(m_pc);
                    goto loop;
                }
                goto COLLECT_STACK_ONE;
            }

            CASE(VMOP_ENCLOSE) {
                assert(FIXNUMP(OPERANDS));
                int argc = FIXNUM(OPERANDS);
                assert(m_env);
                vm_env_t env = (vm_env_t)((intptr_t)m_env - offsetof(vm_env_rec_t, up));
                scm_obj_t* dst = (scm_obj_t*)env - env->count;
                if (STACKP(env)) {
                    for (int i = 0; i < argc; i++) dst[i] = m_fp[i];
                } else {
                    for (int i = 0; i < argc; i++) {
                        dst[i] = m_fp[i];
                        m_heap->write_barrier(m_fp[i]);
                    }
                }
                m_sp = m_fp;
                m_pc = CDR(m_pc);
  #if STDEBUG
                check_vm_state();
  #endif
                goto loop;
            }

            CASE(VMOP_GLOC) {
                m_value = ((scm_gloc_t)OPERANDS)->value;
                if (m_value != scm_undef) {
                    m_pc = CDR(m_pc);
                    goto loop;
                }
                goto ERROR_GLOC;
            }

            CASE(VMOP_ILOC) {
                m_value = *lookup_iloc(OPERANDS);
                m_pc = CDR(m_pc);
                goto loop;
            }

            CASE(VMOP_CAR_ILOC) {
                obj = *lookup_iloc(CAR(OPERANDS));
                if (PAIRP(obj)) {
                    m_value = CAR(obj);
                    m_pc = CDR(m_pc);
                    goto loop;
                }
                goto ERROR_CAR_ILOC;
            }

            CASE(VMOP_CDR_ILOC) {
                obj = *lookup_iloc(CAR(OPERANDS));
                if (PAIRP(obj)) {
                    m_value = CDR(obj);
                    m_pc = CDR(m_pc);
                    goto loop;
                }
                goto ERROR_CDR_ILOC;
            }

            CASE(VMOP_CONST) {
                m_value = OPERANDS;
                m_pc = CDR(m_pc);
                goto loop;
            }

            CASE(VMOP_SUBR) {
                scm_subr_t subr = (scm_subr_t)CAR(OPERANDS);
  #if PROFILE_SUBR
                subr->c_load++;
  #endif
                assert(SUBRP(subr));
                int argc = FIXNUM(CADR(OPERANDS));
                m_value = (*subr->adrs)(this, argc, m_sp - argc);
                m_sp -= argc;
                assert(m_sp >= m_fp);
                m_pc = CDR(m_pc);
                goto loop;
            }

            CASE(VMOP_ILOC1) {
                void* lnk = *(void**)m_env;
                vm_env_t env = (vm_env_t)((intptr_t)lnk - offsetof(vm_env_rec_t, up));
                m_value = *((scm_obj_t*)env - env->count + FIXNUM(OPERANDS));
                m_pc = CDR(m_pc);
                goto loop;
            }

            CASE(VMOP_ILOC0) {
                vm_env_t env = (vm_env_t)((intptr_t)m_env - offsetof(vm_env_rec_t, up));
                m_value = *((scm_obj_t*)env - env->count + FIXNUM(OPERANDS));
                m_pc = CDR(m_pc);
                goto loop;
            }

            CASE(VMOP_IF_TRUE) {
                if (m_value != scm_false) {
                    m_pc = OPERANDS;
                    goto loop;
                }
                m_pc = CDR(m_pc);
                goto loop;
            }

            CASE(VMOP_IF_NULLP_RET_CONST) {
                if (m_value == scm_nil) {
                    m_value = OPERANDS;
                    goto pop_cont;
                }
                m_pc = CDR(m_pc);
                goto loop;
            }

            CASE(VMOP_IF_EQP) {
                m_sp--;
                assert(m_sp >= m_fp);
                if (m_sp[0] == m_value) {
                    m_pc = OPERANDS;
                    goto loop;
                }
                m_pc = CDR(m_pc);
                goto loop;
            }

            CASE(VMOP_IF_NULLP) {
                if (m_value == scm_nil) {
                    m_pc = OPERANDS;
                    goto loop;
                }
                m_pc = CDR(m_pc);
                goto loop;
            }

            CASE(VMOP_IF_PAIRP) {
                if (PAIRP(m_value)) {
                    m_pc = OPERANDS;
                    goto loop;
                }
                m_pc = CDR(m_pc);
                goto loop;
            }

            CASE(VMOP_IF_SYMBOLP) {
                if (SYMBOLP(m_value)) {
                    m_pc = OPERANDS;
                    goto loop;
                }
                m_pc = CDR(m_pc);
                goto loop;
            }

            CASE(VMOP_IF_TRUE_RET) {
                if (m_value != scm_false) goto pop_cont;
                m_pc = CDR(m_pc);
                goto loop;
            }

            CASE(VMOP_IF_FALSE_RET) {
                if (m_value == scm_false) goto pop_cont;
                m_pc = CDR(m_pc);
                goto loop;
            }

            CASE(VMOP_IF_TRUE_RET_CONST) {
                if (m_value != scm_false) {
                    m_value = OPERANDS;
                    goto pop_cont;
                }
                m_pc = CDR(m_pc);
                goto loop;
            }

            CASE(VMOP_IF_FALSE_RET_CONST) {
                if (m_value == scm_false) {
                    m_value = OPERANDS;
                    goto pop_cont;
                }
                m_pc = CDR(m_pc);
                goto loop;
            }

            CASE(VMOP_IF_EQP_RET_CONST) {
                m_sp--;
                assert(m_sp >= m_fp);
                if (m_sp[0] == m_value) {
                    m_value = OPERANDS;
                    goto pop_cont;
                }
                m_pc = CDR(m_pc);
                goto loop;
            }

            CASE(VMOP_IF_PAIRP_RET_CONST) {
                if (PAIRP(m_value)) {
                    m_value = OPERANDS;
                    goto pop_cont;
                }
                m_pc = CDR(m_pc);
                goto loop;
            }

            CASE(VMOP_IF_SYMBOLP_RET_CONST) {
                if (SYMBOLP(m_value)) {
                    m_value = OPERANDS;
                    goto pop_cont;
                }
                m_pc = CDR(m_pc);
                goto loop;
            }

            CASE(VMOP_IF_NOT_PAIRP_RET_CONST) {
                if (!PAIRP(m_value)) {
                    m_value = OPERANDS;
                    goto pop_cont;
                }
                m_pc = CDR(m_pc);
                goto loop;
            }

            CASE(VMOP_IF_NOT_NULLP_RET_CONST) {
                if (m_value != scm_nil) {
                    m_value = OPERANDS;
                    goto pop_cont;
                }
                m_pc = CDR(m_pc);
                goto loop;
            }

            CASE(VMOP_IF_NOT_EQP_RET_CONST) {
                m_sp--;
                assert(m_sp >= m_fp);
                if (m_sp[0] != m_value) {
                    m_value = OPERANDS;
                    goto pop_cont;
                }
                m_pc = CDR(m_pc);
                goto loop;
            }

            CASE(VMOP_IF_NOT_SYMBOLP_RET_CONST) {
                if (!SYMBOLP(m_value)) {
                    m_value = OPERANDS;
                    goto pop_cont;
                }
                m_pc = CDR(m_pc);
                goto loop;
            }

            CASE(VMOP_CLOSE) {
                if (STACKP(m_env)) {
                    m_env = save_env(m_env);
                    update_cont(m_cont);
                }
                scm_obj_t spec = CAR(OPERANDS);
                scm_obj_t code = CDR(OPERANDS);
                scm_obj_t doc = CDDR(spec);
                m_value = make_closure(m_heap, FIXNUM(CAR(spec)), FIXNUM(CADR(spec)), m_env, code, doc);
                m_pc = CDR(m_pc);
  #if STDEBUG
                check_vm_state();
  #endif
                goto loop;
            }

            CASE(VMOP_SET_GLOC) {
                scm_gloc_t gloc = (scm_gloc_t)OPERANDS;
                assert(GLOCP(gloc));
                m_heap->write_barrier(m_value);
                gloc->value = m_value;
                m_pc = CDR(m_pc);
                goto loop;
            }

            CASE(VMOP_SET_ILOC) {
                scm_obj_t* slot = lookup_iloc(OPERANDS);
                if (!STACKP(slot)) m_heap->write_barrier(m_value);
                *slot = m_value;
                m_pc = CDR(m_pc);
                goto loop;
            }

            CASE(VMOP_PUSH_CONS) {
                m_sp[-1] = make_pair(m_heap, m_sp[-1], m_value);
                m_pc = CDR(m_pc);
                goto loop;
            }

            CASE(VMOP_RET_CONS) {
                m_value = make_pair(m_heap, m_sp[-1], m_value);
                goto pop_cont;
            }

            CASE(VMOP_RET_EQP) {
                m_value = (m_sp[-1] == m_value) ? scm_true : scm_false;
                goto pop_cont;
            }

            CASE(VMOP_RET_NULLP) {
                m_value = (m_value == scm_nil) ? scm_true : scm_false;
                goto pop_cont;
            }

            CASE(VMOP_RET_PAIRP) {
                m_value = PAIRP(m_value) ? scm_true : scm_false;
                goto pop_cont;
            }

            CASE(VMOP_RET_CLOSE) {
                if (STACKP(m_env)) {
                    m_env = save_env(m_env);
                    update_cont(m_cont);
                }
  #if PREBIND_CLOSE
                m_value = make_closure(m_heap, (scm_closure_t)OPERANDS, m_env);
  #else
                scm_obj_t spec = CAR(OPERANDS);
                scm_obj_t code = CDR(OPERANDS);
                scm_obj_t doc = CDDR(spec);
                m_value = make_closure(m_heap, FIXNUM(CAR(spec)), FIXNUM(CADR(spec)), m_env, code, doc);
  #endif
  #if STDEBUG
                check_vm_state();
  #endif
                goto pop_cont;
            }

            CASE(VMOP_PUSH_NADD_ILOC) {
                assert(FIXNUMP(CADR(OPERANDS)));
                if (m_sp < m_stack_limit) {
                    obj = *lookup_iloc(CAR(OPERANDS));
                    if (FIXNUMP(obj)) {
                        intptr_t n = FIXNUM(obj) + FIXNUM(CADR(OPERANDS));
                        if ((n <= FIXNUM_MAX) & (n >= FIXNUM_MIN)) {
                            m_sp[0] = MAKEFIXNUM(n);
                            m_sp++;
                            m_pc = CDR(m_pc);
                            goto loop;
                        }
                    }
                    goto FALLBACK_PUSH_NADD_ILOC;
                }
                goto COLLECT_STACK_ONE;
            }

            CASE(VMOP_PUSH_CADR_ILOC) {
                if (m_sp < m_stack_limit) {
                    obj = *lookup_iloc(CAR(OPERANDS));
                    if (PAIRP(obj)) {
                        obj = CDR(obj);
                        if (PAIRP(obj)) {
                            m_sp[0] = CAR(obj);
                            m_sp++;
                            m_pc = CDR(m_pc);
                            goto loop;
                        }
                    }
                    goto ERROR_PUSH_CADR_ILOC;
                }
                goto COLLECT_STACK_ONE;
            }

            CASE(VMOP_PUSH_CDDR_ILOC) {
                if (m_sp < m_stack_limit) {
                    obj = *lookup_iloc(CAR(OPERANDS));
                    if (PAIRP(obj)) {
                        obj = CDR(obj);
                        if (PAIRP(obj)) {
                            m_sp[0] = CDR(obj);
                            m_sp++;
                            m_pc = CDR(m_pc);
                            goto loop;
                        }
                    }
                    goto ERROR_PUSH_CDDR_ILOC;
                }
                goto COLLECT_STACK_ONE;
            }

            CASE(VMOP_CADR_ILOC) {
                obj = *lookup_iloc(CAR(OPERANDS));
                if (PAIRP(obj)) {
                    obj = CDR(obj);
                    if (PAIRP(obj)) {
                        m_value = CAR(obj);
                        m_pc = CDR(m_pc);
                        goto loop;
                    }
                }
                goto ERROR_CADR_ILOC;
            }

            CASE(VMOP_CDDR_ILOC) {
                obj = *lookup_iloc(CAR(OPERANDS));
                if (PAIRP(obj)) {
                    obj = CDR(obj);
                    if (PAIRP(obj)) {
                        m_value = CDR(obj);
                        m_pc = CDR(m_pc);
                        goto loop;
                    }
                }
                goto ERROR_CDDR_ILOC;
            }

            CASE(VMOP_EQ_N_ILOC) {
                assert(FIXNUMP(CADR(OPERANDS)));
                obj = *lookup_iloc(CAR(OPERANDS));
                if (FIXNUMP(obj)) {
                    m_value = ((intptr_t)obj == (intptr_t)CADR(OPERANDS)) ? scm_true : scm_false;
                    m_pc = CDR(m_pc);
                    goto loop;
                }
                goto FALLBACK_EQ_N_ILOC;
            }

            CASE(VMOP_LT_N_ILOC) {
                obj = *lookup_iloc(CAR(OPERANDS));
                if (FIXNUMP(obj)) {
                    m_value = ((intptr_t)CADR(OPERANDS) > (intptr_t)obj) ? scm_true : scm_false;
                    m_pc = CDR(m_pc);
                    goto loop;
                }
                goto FALLBACK_LT_N_ILOC;
            }

            CASE(VMOP_GE_N_ILOC) {
                obj = *lookup_iloc(CAR(OPERANDS));
                if (FIXNUMP(obj)) {
                    m_value = ((intptr_t)CADR(OPERANDS) <= (intptr_t)obj) ? scm_true : scm_false;
                    m_pc = CDR(m_pc);
                    goto loop;
                }
                goto FALLBACK_GE_N_ILOC;
            }

            CASE(VMOP_LE_N_ILOC) {
                obj = *lookup_iloc(CAR(OPERANDS));
                if (FIXNUMP(obj)) {
                    m_value = ((intptr_t)CADR(OPERANDS) >= (intptr_t)obj) ? scm_true : scm_false;
                    m_pc = CDR(m_pc);
                    goto loop;
                }
                goto FALLBACK_LE_N_ILOC;
            }

            CASE(VMOP_GT_N_ILOC) {
                obj = *lookup_iloc(CAR(OPERANDS));
                if (FIXNUMP(obj)) {
                    m_value = ((intptr_t)CADR(OPERANDS) < (intptr_t)obj) ? scm_true : scm_false;
                    m_pc = CDR(m_pc);
                    goto loop;
                }
                goto FALLBACK_GT_N_ILOC;
            }

            CASE(VMOP_NADD_ILOC) {
                assert(FIXNUMP(CADR(OPERANDS)));
                obj = *lookup_iloc(CAR(OPERANDS));
                if (FIXNUMP(obj)) {
                    intptr_t n = FIXNUM(obj) + FIXNUM(CADR(OPERANDS));
                    if ((n <= FIXNUM_MAX) & (n >= FIXNUM_MIN)) {
                        m_value = MAKEFIXNUM(n);
                        m_pc = CDR(m_pc);
                        goto loop;
                    }
                }
                goto FALLBACK_NADD_ILOC;
            }

            CASE(VMOP_EQ_ILOC) {
                obj = *lookup_iloc(CAR(OPERANDS));
                if (FIXNUMP(m_value) & FIXNUMP(obj)) {
                    m_value = ((intptr_t)m_value == (intptr_t)obj) ? scm_true : scm_false;
                    m_pc = CDR(m_pc);
                    goto loop;
                }
                goto FALLBACK_EQ_ILOC;
            }

            CASE(VMOP_LT_ILOC) {
                obj = *lookup_iloc(CAR(OPERANDS));
                if (FIXNUMP(m_value) & FIXNUMP(obj)) {
                    m_value = ((intptr_t)m_value < (intptr_t)obj) ? scm_true : scm_false;
                    m_pc = CDR(m_pc);
                    goto loop;
                }
                goto FALLBACK_LT_ILOC;
            }

            CASE(VMOP_LE_ILOC) {
                obj = *lookup_iloc(CAR(OPERANDS));
                if (FIXNUMP(m_value) & FIXNUMP(obj)) {
                    m_value = ((intptr_t)m_value <= (intptr_t)obj) ? scm_true : scm_false;
                    m_pc = CDR(m_pc);
                    goto loop;
                }
                goto FALLBACK_LE_ILOC;
            }

            CASE(VMOP_GT_ILOC) {
                obj = *lookup_iloc(CAR(OPERANDS));
                if (FIXNUMP(m_value) & FIXNUMP(obj)) {
                    m_value = ((intptr_t)m_value > (intptr_t)obj) ? scm_true : scm_false;
                    m_pc = CDR(m_pc);
                    goto loop;
                }
                goto FALLBACK_GT_ILOC;
            }

            CASE(VMOP_GE_ILOC) {
                obj = *lookup_iloc(CAR(OPERANDS));
                if (FIXNUMP(m_value) & FIXNUMP(obj)) {
                    m_value = ((intptr_t)m_value >= (intptr_t)obj) ? scm_true : scm_false;
                    m_pc = CDR(m_pc);
                    goto loop;
                }
                goto FALLBACK_GE_ILOC;
            }

            CASE(VMOP_TOUCH_GLOC) {
                goto THUNK_TOUCH_GLOC_OF;
            }

            CASE(VMOP_SUBR_GLOC_OF) {
                goto THUNK_SUBR_GLOC_OF;
            }

            CASE(VMOP_PUSH_SUBR_GLOC_OF) {
                goto THUNK_PUSH_SUBR_GLOC_OF;
            }

            CASE(VMOP_RET_SUBR_GLOC_OF) {
                goto THUNK_RET_SUBR_GLOC_OF;
            }

            CASE(VMOP_VM_ESCAPE) {
                return;
            }

        } // END OF SWITCH()

        goto ERROR_BAD_INSTRUCTION;

APPLY_APPLY:
        if (m_sp - m_fp >= 2) {
            m_value = m_fp[0];
            m_fp++;
            obj = m_sp[-1]; // for error message
            m_sp--;
            scm_obj_t lst = obj;
            while (PAIRP(lst)) {
                if (m_sp >= m_stack_limit) collect_stack(sizeof(scm_obj_t));
                m_sp[0] = CAR(lst);
                m_sp++;
                lst = CDR(lst);
            }
            if (lst == scm_nil) goto apply;
            goto ERROR_PROC_APPLY_BAD_LAST_ARGS;
        }
        goto ERROR_PROC_APPLY_WRONG_NUMBER_ARGS;

APPLY_VALUES:
        if (m_sp - m_fp == 2) {
            m_value = m_fp[0];
            scm_obj_t args = m_fp[1];
            if (VALUESP(args)) {
                scm_values_t values = (scm_values_t)args;
                int argc = HDR_VALUES_COUNT(values->hdr);
                m_sp = m_fp;
                if (m_sp + argc >= m_stack_limit) collect_stack(sizeof(scm_obj_t) * argc);
                for (int i = 0; i < argc; i++) m_sp[i] = values->elts[i];
                m_sp += argc;
                goto apply;
            } else {
                m_fp[0] = args;
                m_sp = m_fp + 1;
                goto apply;
            }
        }
        goto ERROR_APPLY_VALUES_WRONG_NUMBER_ARGS;


#if USE_FAST_DYNAMIC_WIND
APPLY_CONT: {
            scm_cont_t cont = (scm_cont_t)m_value;
            if (cont->wind_rec == scm_unspecified || cont->wind_rec == m_current_dynamic_wind_record) {
                int argc = m_sp - m_fp;
                m_cont = cont->cont;
                if (argc == 0) {
                    m_value = scm_unspecified;
                    goto pop_cont;
                }
                if (argc == 1) {
                    m_value = *m_fp;
                    goto pop_cont;
                }
                scm_values_t values = make_values(m_heap, argc);
                for (int i = 0; i < argc; i++) values->elts[i] = m_fp[i];
                m_value = values;
                goto pop_cont;
            } else {
                scm_obj_t lst = scm_nil;
                scm_obj_t* last = m_sp;
                while (--last >= m_fp) lst = make_pair(m_heap, last[0], lst);
                scm_obj_t proc = lookup_system_closure(".@perform-dynamic-wind");
                apply_scheme(proc, 3, cont->wind_rec, make_cont(m_heap, scm_unspecified, cont->cont), lst);
                m_sp = m_fp;
                m_pc = CDR(m_pc);
                goto loop;
            }
        }
#else
APPLY_CONT: {
            int argc = m_sp - m_fp;
            scm_cont_t cont = (scm_cont_t)m_value;
            m_cont = cont->cont;
            if (argc == 0) {
                m_value = scm_unspecified;
                goto pop_cont;
            }
            if (argc == 1) {
                m_value = *m_fp;
                goto pop_cont;
            }
            scm_values_t values = make_values(m_heap, argc);
            for (int i = 0; i < argc; i++) values->elts[i] = m_fp[i];
            m_value = values;
            goto pop_cont;
        }
#endif

APPLY_CALLCC:
        if (m_sp - m_fp == 1) {
            scm_obj_t proc = m_fp[0];
            m_cont = save_cont(m_cont);
            m_env = save_env(m_env);
            update_cont(m_cont);
            m_value = proc;
            m_fp = m_stack_top;
            m_sp = m_stack_top + 1;
            m_fp[0] = make_cont(m_heap, m_current_dynamic_wind_record, m_cont);
  #if STDEBUG
            check_vm_state();
  #endif
            goto apply;
        }
        goto ERROR_CALLCC_WRONG_NUMBER_ARGS;

APPLY_SPECIAL:
        if (m_value == scm_proc_apply) goto APPLY_APPLY;
        if (m_value == scm_proc_apply_values) goto APPLY_VALUES;
        if (m_value == scm_proc_callcc) goto APPLY_CALLCC;
        if (CONTP(m_value)) goto APPLY_CONT;
        goto ERROR_INVALID_APPLICATION;

APPLY_VARIADIC: {
            scm_closure_t closure = (scm_closure_t)m_value;
            int args = HDR_CLOSURE_ARGS(closure->hdr);
            int rest = 0;
            if (args < 0) {
                args = -args - 1;
                rest = 1;
            }
            int argc = m_sp - m_fp;
            if (rest & (argc >= args)) {
                scm_obj_t opt = scm_nil;
                scm_obj_t* first = m_sp - argc + args;  // find first object of rest arg
                scm_obj_t* last = m_sp;
                while (--last >= first) opt = make_pair(m_heap, *last, opt);
                *first = opt;
                m_sp = first + 1;
                args = args + 1;
                vm_env_t env = (vm_env_t)m_sp;
                env->count = args;
                env->up = closure->env;
                m_sp = m_fp = (scm_obj_t*)(env + 1);
                m_pc = closure->code;
                m_env = &env->up;
                goto trace_n_loop;
            }
            goto ERROR_APPLY_WRONG_NUMBER_ARGS;
        }

COLLECT_STACK_ONE:
        collect_stack(sizeof(scm_obj_t));
        goto loop;

COLLECT_STACK_CONT_REC:
        collect_stack(sizeof(vm_cont_rec_t));
        goto loop;

COLLECT_STACK_ENV_REC:
        collect_stack(sizeof(vm_env_rec_t));
        goto loop;

COLLECT_STACK_ENV_REC_N_ONE:
        collect_stack(sizeof(vm_env_rec_t) + sizeof(scm_obj_t));
        goto loop;

COLLECT_STACK_ENV_REC_N_ONE_N_APPLY:
        collect_stack(sizeof(vm_env_rec_t) + sizeof(scm_obj_t));
        goto apply;

COLLECT_STACK_ENV_REC_N_OPERAND:
        collect_stack(sizeof(vm_env_rec_t) + sizeof(scm_obj_t*) * FIXNUM(OPERANDS));
        goto loop;

FALLBACK_PUSH_NADD_ILOC:
        if (number_pred(obj)) {
            m_sp[0] = arith_add(m_heap, obj, CADR(OPERANDS));
            m_sp++;
            m_pc = CDR(m_pc);
            goto loop;
        }
        goto ERROR_PUSH_NADD_ILOC;

FALLBACK_NADD_ILOC:
        if (number_pred(obj)) {
            m_value = arith_add(m_heap, obj, CADR(OPERANDS));
            m_pc = CDR(m_pc);
            goto loop;
        }
        goto ERROR_NADD_ILOC;

FALLBACK_EQ_N_ILOC:
        if (number_pred(obj)) {
            m_value = n_equal_pred(m_heap, obj, CADR(OPERANDS)) ? scm_true : scm_false;
            m_pc = CDR(m_pc);
            goto loop;
        }
        goto ERROR_EQ_N_ILOC;

FALLBACK_LT_N_ILOC:
        if (number_pred(obj)) {
            m_value = n_compare(m_heap, obj, CADR(OPERANDS)) < 0 ? scm_true : scm_false;
            m_pc = CDR(m_pc);
            goto loop;
        }
        goto ERROR_LT_N_ILOC;

FALLBACK_LE_N_ILOC:
        if (number_pred(obj)) {
            m_value = n_compare(m_heap, obj, CADR(OPERANDS)) <= 0 ? scm_true : scm_false;
            m_pc = CDR(m_pc);
            goto loop;
        }
        goto ERROR_LE_N_ILOC;

FALLBACK_GT_N_ILOC:
        if (number_pred(obj)) {
            m_value = n_compare(m_heap, obj, CADR(OPERANDS)) > 0 ? scm_true : scm_false;
            m_pc = CDR(m_pc);
            goto loop;
        }
        goto ERROR_GT_N_ILOC;

FALLBACK_GE_N_ILOC:
        if (number_pred(obj)) {
            m_value = n_compare(m_heap, obj, CADR(OPERANDS)) >= 0 ? scm_true : scm_false;
            m_pc = CDR(m_pc);
            goto loop;
        }
        goto ERROR_GE_N_ILOC;

FALLBACK_EQ_ILOC: {
            int bad;
            if (number_pred(m_value)) {
                if (number_pred(obj)) {
                    m_value = n_equal_pred(m_heap, m_value, obj) ? scm_true : scm_false;
                    m_pc = CDR(m_pc);
                    goto loop;
                }
                bad = 1;
            } else {
                bad = 0;
            }
            scm_obj_t argv[2] = { m_value, obj };
            wrong_type_argument_violation(this, "=", bad, "number", argv[bad], 2, argv);
            goto BACK_TO_LOOP;
        }

FALLBACK_LT_ILOC: {
            int bad;
            if (number_pred(m_value)) {
                if (number_pred(obj)) {
                    m_value = (n_compare(m_heap, m_value, obj) < 0) ? scm_true : scm_false;
                    m_pc = CDR(m_pc);
                    goto loop;
                }
                bad = 1;
            } else {
                bad = 0;
            }
            scm_obj_t argv[2] = { m_value, obj };
            wrong_type_argument_violation(this, "comparison(< > <= >=)", bad, "number", argv[bad], 2, argv);
            goto BACK_TO_LOOP;
        }

FALLBACK_LE_ILOC: {
            int bad;
            if (number_pred(m_value)) {
                if (number_pred(obj)) {
                    m_value = (n_compare(m_heap, m_value, obj) <= 0) ? scm_true : scm_false;
                    m_pc = CDR(m_pc);
                    goto loop;
                }
                bad = 1;
            } else {
                bad = 0;
            }
            scm_obj_t argv[2] = { m_value, obj };
            wrong_type_argument_violation(this, "comparison(< > <= >=)", bad, "number", argv[bad], 2, argv);
            goto BACK_TO_LOOP;
        }

FALLBACK_GT_ILOC: {
            int bad;
            if (number_pred(m_value)) {
                if (number_pred(obj)) {
                    m_value = (n_compare(m_heap, m_value, obj) > 0) ? scm_true : scm_false;
                    m_pc = CDR(m_pc);
                    goto loop;
                }
                bad = 1;
            } else {
                bad = 0;
            }
            scm_obj_t argv[2] = { m_value, obj };
            wrong_type_argument_violation(this, "comparison(< > <= >=)", bad, "number", argv[bad], 2, argv);
            goto BACK_TO_LOOP;
        }

FALLBACK_GE_ILOC: {
            int bad;
            if (number_pred(m_value)) {
                if (number_pred(obj)) {
                    m_value = (n_compare(m_heap, m_value, obj) >= 0) ? scm_true : scm_false;
                    m_pc = CDR(m_pc);
                    goto loop;
                }
                bad = 1;
            } else {
                bad = 0;
            }
            scm_obj_t argv[2] = { m_value, obj };
            wrong_type_argument_violation(this, "comparison(< > <= >=)", bad, "number", argv[bad], 2, argv);
            goto BACK_TO_LOOP;
        }

THUNK_TOUCH_GLOC_OF: {
            assert(GLOCP(OPERANDS));
            if (((scm_gloc_t)OPERANDS)->value != scm_undef) {
                m_heap->write_barrier(CADR(m_pc));
                m_heap->write_barrier(CDDR(m_pc));
                CAR(m_pc) = CADR(m_pc);
                CDR(m_pc) = CDDR(m_pc);
                goto loop;
            }
            goto ERROR_TOUCH_GLOC;
        }

THUNK_SUBR_GLOC_OF: {
            assert(GLOCP(CAR(OPERANDS)));
            scm_subr_t subr = (scm_subr_t)(((scm_gloc_t)CAR(OPERANDS))->value);
            if (SUBRP(subr)) {
                m_heap->write_barrier(subr);
                CAAR(m_pc) = opcode_to_instruction(VMOP_SUBR);
                CAR(OPERANDS) = subr;
                goto loop;
            }
            system_error("system error: inconsistent code in auto compile cache");
        }

THUNK_PUSH_SUBR_GLOC_OF: {
            assert(GLOCP(CAR(OPERANDS)));
            scm_subr_t subr = (scm_subr_t)(((scm_gloc_t)CAR(OPERANDS))->value);
            if (SUBRP(subr)) {
                m_heap->write_barrier(subr);
                CAAR(m_pc) = opcode_to_instruction(VMOP_PUSH_SUBR);
                CAR(OPERANDS) = subr;
                goto loop;
            }
            system_error("system error: inconsistent code in auto compile cache");
        }

THUNK_RET_SUBR_GLOC_OF: {
            assert(GLOCP(CAR(OPERANDS)));
            scm_subr_t subr = (scm_subr_t)(((scm_gloc_t)CAR(OPERANDS))->value);
            if (SUBRP(subr)) {
                m_heap->write_barrier(subr);
                CAAR(m_pc) = opcode_to_instruction(VMOP_RET_SUBR);
                CAR(OPERANDS) = subr;
                goto loop;
            }
            system_error("system error: inconsistent code in auto compile cache");
        }

ERROR_NADD_ILOC:
ERROR_PUSH_NADD_ILOC: {
            scm_obj_t argv[2] = { obj, CADR(OPERANDS) };
            wrong_type_argument_violation(this, "operator(+ -)", 0, "number", argv[0], 2, argv);
            goto BACK_TO_LOOP;
        }

ERROR_EQ_N_ILOC: {
            scm_obj_t argv[2] = { obj, CADR(OPERANDS) };
            wrong_type_argument_violation(this, "=", 0, "number", argv[0], 2, argv);
            goto BACK_TO_LOOP;
        }

ERROR_LT_N_ILOC: {
            scm_obj_t argv[2] = { obj, CADR(OPERANDS) };
            wrong_type_argument_violation(this, "comparison(< > <= >=)", 0, "number", argv[0], 2, argv);
            goto BACK_TO_LOOP;
        }

ERROR_LE_N_ILOC: {
            scm_obj_t argv[2] = { obj, CADR(OPERANDS) };
            wrong_type_argument_violation(this, "comparison(< > <= >=)", 0, "number", argv[0], 2, argv);
            goto BACK_TO_LOOP;
        }

ERROR_GT_N_ILOC: {
            scm_obj_t argv[2] = { obj, CADR(OPERANDS) };
            wrong_type_argument_violation(this, "comparison(< > <= >=)", 0, "number", argv[0], 2, argv);
            goto BACK_TO_LOOP;
        }

ERROR_GE_N_ILOC: {
            scm_obj_t argv[2] = { obj, CADR(OPERANDS) };
            wrong_type_argument_violation(this, "comparison(< > <= >=)", 0, "number", argv[0], 2, argv);
            goto BACK_TO_LOOP;
        }

ERROR_PUSH_CAR_ILOC:
ERROR_CAR_ILOC:
        wrong_type_argument_violation(this, "car", 0, "pair", obj, 1, &obj);
        goto BACK_TO_LOOP;
ERROR_PUSH_CDR_ILOC:
ERROR_CDR_ILOC:
        wrong_type_argument_violation(this, "cdr", 0, "pair", obj, 1, &obj);
        goto BACK_TO_LOOP;
ERROR_PUSH_CADR_ILOC:
ERROR_CADR_ILOC:
        wrong_type_argument_violation(this, "cadr", 0, "appropriate list structure", obj, 1, &obj);
        goto BACK_TO_LOOP;
ERROR_PUSH_CDDR_ILOC:
ERROR_CDDR_ILOC:
        wrong_type_argument_violation(this, "cddr", 0, "appropriate list structure", obj, 1, &obj);
        goto BACK_TO_LOOP;

ERROR_GLOC:
ERROR_RET_GLOC:
ERROR_PUSH_GLOC:
ERROR_TOUCH_GLOC:
        raise_undefined_violation(this, ((scm_gloc_t)OPERANDS)->variable, NULL);
        m_sp = m_fp;
        goto BACK_TO_LOOP;

ERROR_APPLY_GLOC:
        raise_undefined_violation(this, ((scm_gloc_t)CAR(OPERANDS))->variable, NULL);
        m_sp = m_fp;
        goto BACK_TO_TRACE_N_LOOP;

ERROR_APPLY_WRONG_NUMBER_ARGS:
        {
            scm_closure_t closure = (scm_closure_t)m_value;
            int args = HDR_CLOSURE_ARGS(closure->hdr);
            int rest = 0;
            if (args < 0) {
                args = -args - 1;
                rest = 1;
            }
            if (rest) wrong_number_of_arguments_violation(this, m_value, args, -1, m_sp - m_fp, m_fp);
            else wrong_number_of_arguments_violation(this, m_value, args, args, m_sp - m_fp, m_fp);
            assert(CAR(m_pc) == scm_unspecified);
            m_sp = m_fp;
            goto BACK_TO_TRACE_N_LOOP;
        }

ERROR_PROC_APPLY_WRONG_NUMBER_ARGS:
        wrong_number_of_arguments_violation(this, "apply", 2, -1, m_sp - m_fp, m_fp);
        assert(CAR(m_pc) == scm_unspecified);
        goto BACK_TO_TRACE_N_LOOP;

ERROR_PROC_APPLY_BAD_LAST_ARGS:
        wrong_type_argument_violation(this, "apply", -1, "proper list for last argument", obj, -1, NULL);
        assert(CAR(m_pc) == scm_unspecified);
        goto BACK_TO_TRACE_N_LOOP;

ERROR_APPLY_VALUES_WRONG_NUMBER_ARGS:
        wrong_number_of_arguments_violation(this, "apply-values", 2, 2, m_sp - m_fp, m_fp);
        assert(CAR(m_pc) == scm_unspecified);
        goto BACK_TO_TRACE_N_LOOP;

ERROR_CALLCC_WRONG_NUMBER_ARGS:
        wrong_number_of_arguments_violation(this, "call-with-current-continuation", 1, 1, m_sp - m_fp, m_fp);
        assert(CAR(m_pc) == scm_unspecified);
        goto BACK_TO_TRACE_N_LOOP;

ERROR_INVALID_APPLICATION:
        invalid_application_violation(this, m_value, m_sp - m_fp, m_fp);
        goto BACK_TO_TRACE_N_LOOP;

BACK_TO_LOOP:
        m_pc = CDR(m_pc);
        goto loop;

BACK_TO_TRACE_N_LOOP:
        m_sp = m_fp;
        m_pc = CDR(m_pc);
        goto trace_n_loop;

#if USE_GCC_EXTENSION
ERROR_BAD_INSTRUCTION_ALIGN_STUB:
        __asm__ ("/* ERROR_BAD_INSTRUCTION */");
        __asm__ (".p2align 3");
ERROR_BAD_INSTRUCTION:
        __asm__ ("nop");
        __asm__ ("nop");
        __asm__ ("nop");
        __asm__ ("nop");
#else
ERROR_BAD_INSTRUCTION:
#endif
        system_error("system error: invalid vm instruction %d", instruction_to_opcode(CAAR(m_pc)));

    } catch (vm_continue_t& e) {
        goto resume;
    } catch (...) {
        throw;
    }

    #undef PUSH_CONST
    #undef LOAD_CONST
    #undef RET_CONST
    #undef OPERANDS
}
