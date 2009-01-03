/*
    Ypsilon Scheme System
    Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#ifndef VM_H_INCLUDED
#define VM_H_INCLUDED

#include "core.h"
#include "heap.h"

class printer_t;

#if USE_PARALLEL_VM
class Interpreter;
#endif

class VM {
public:
    scm_obj_t           m_trace;
    scm_obj_t*          m_fp;
    scm_obj_t           m_pc;
    void*               m_env;
    void*               m_cont;

    scm_obj_t           m_trace_tail;

    scm_obj_t*          m_sp;
    scm_obj_t*          m_stack_limit;
    scm_obj_t           m_value;

    scm_obj_t*          m_stack_top;

    object_heap_t*      m_heap;

    scm_obj_t*          m_to_stack_top;
    scm_obj_t*          m_to_stack_limit;

    int                 m_stack_size;
    int                 m_stack_busy;

#if USE_GCC_EXTENSION
    void*               m_dispatch_table[VMOP_INSTRUCTION_COUNT];
#endif

    bool            init(object_heap_t* heap);
    void            boot();
    void            standalone();
    void            reset();
    void            run(bool init_dispatch_table);

    void            scheme_warning(const char* fmt, ...);
    void            scheme_error(const char* fmt, ...) ATTRIBUTE(noreturn);
    void            system_error(const char* fmt, ...) ATTRIBUTE(noreturn);

    struct {
        // note: no gc protect
        scm_obj_t   m_extend_lexical_syntax;        // #t or #f
        scm_obj_t   m_mutable_literals;             // #t ot #f
        scm_obj_t   m_collect_notify;               // #t or #f
        scm_obj_t   m_collect_stack_notify;         // #t or #f
        scm_obj_t   m_backtrace;                    // #t or #f or fixnum
        scm_obj_t   m_backtrace_line_length;        // fixnum
        scm_obj_t   m_restricted_print_line_length; // fixnum
        scm_obj_t   m_record_print_nesting_limit;   // fixnum
        scm_obj_t   m_warning_level;                // #t or #f or fixnum
    } flags;

    scm_port_t          m_bootport;
    scm_port_t          m_current_input;
    scm_port_t          m_current_output;
    scm_port_t          m_current_error;
    scm_environment_t   m_current_environment;
    scm_weakhashtable_t m_current_dynamic_environment;
    scm_obj_t           m_current_dynamic_wind_record;
    scm_obj_t           m_current_exception_handler;
    scm_obj_t           m_current_source_comments;
    int                 m_recursion_level;
    int                 m_shared_object_errno;
    uint32_t            m_shared_object_win32_lasterror;
#if USE_PARALLEL_VM
    Interpreter*        m_interp;
    VM*                 m_parent;
    int                 m_id;
    int                 m_child;
#endif

    scm_closure_t       lookup_system_closure(const char* name);
    scm_obj_t           lookup_current_environment(scm_symbol_t symbol);
    void                intern_current_environment(scm_symbol_t symbol, scm_obj_t value);
    void                prebind(scm_obj_t code);
    void                backtrace_seek();
    bool                backtrace(scm_port_t port);
    void                stop();
    void                resolve();

private:
    scm_obj_t           prebind_literal(scm_obj_t literal);
    scm_gloc_t          prebind_gloc(scm_obj_t variable);
    void                prebind_list(scm_obj_t code);

    void                backtrace_each(printer_t* prt, int n, scm_obj_t note);
    scm_obj_t           backtrace_fetch(const char* name, int line, int column);
    void                backtrace_seek_make_cont(scm_obj_t note);

    void*               save_env(void* lnk);
    void*               save_cont(void* lnk);
    void                update_cont(void* lnk);
    void                save_stack();

    void*               gc_env(void* lnk);
    void*               gc_cont(void* lnk);

    void                record_trace(scm_obj_t comment);
    scm_obj_t*          lookup_iloc(scm_obj_t operands);

    scm_obj_t           call_scheme_stub(scm_obj_t proc, int argc, scm_obj_t argv[]);

public:
    void                collect_stack(intptr_t acquire);
    void                apply_scheme(scm_obj_t proc, int argc, ...);
    void                apply_scheme_argv(scm_obj_t proc, int argc, scm_obj_t argv[]);
    scm_obj_t           call_scheme(scm_obj_t proc, int argc, ...);
    scm_obj_t           call_scheme_argv(scm_obj_t proc, int argc, scm_obj_t argv[]);

#if !defined(NDEBUG) || STDEBUG
    void check_vm_env(void* lnk);
    void check_vm_cont(void* lnk);
    void check_vm_state();
#endif

#if PROFILE_OPCODE
    struct opcode_profile_t {
        int         opcode;
        uint64_t    count;
        uint64_t    prev[VMOP_INSTRUCTION_COUNT];
    };
    opcode_profile_t m_opcode_profile[VMOP_INSTRUCTION_COUNT];
    static int comp_profile_rec(const void* a1, const void* a2);
    void display_opcode_profile();
#endif

#if PROFILE_SUBR
    void display_subr_profile();
#endif

#if USE_DIRECT_THREAD
    scm_obj_t symbol_to_instruction(scm_obj_t obj) {
        assert(OPCODESYMBOLP(obj));
        scm_symbol_t symbol = (scm_symbol_t)obj;
        int opcode = HDR_SYMBOL_CODE(symbol->hdr);
        assert(((uintptr_t)m_dispatch_table[opcode] & 7) == 0);
        return MAKEVMINST(m_dispatch_table[opcode]);
    }

    int instruction_to_opcode(scm_obj_t obj) {
        assert(VMINSTP(obj));
        uintptr_t adrs = (((uintptr_t)obj) & (~7));
        for (int i = 0; i < array_sizeof(m_dispatch_table); i++) {
            if ((uintptr_t)m_dispatch_table[i] == adrs) return i;
        }
  #ifndef NDEBUG
        printf("bad instruction 0x%p\n", obj);
  #endif
        assert(false);
        return 0;
    }

    scm_obj_t opcode_to_instruction(int opcode) {
        assert(opcode >= 0 && opcode < array_sizeof(m_dispatch_table));
        assert(((uintptr_t)m_dispatch_table[opcode] & 7) == 0);
        return MAKEVMINST(m_dispatch_table[opcode]);
    }

    void* instruction_to_adrs(scm_obj_t obj) {
        assert(VMINSTP(obj));
        return (void*)((uintptr_t)obj);
    }

#endif

#if USE_FIXNUM_THREAD
    #define FIXNUM_OPCODE_SHIFT 7

    scm_obj_t symbol_to_instruction(scm_obj_t obj) {
        assert(OPCODESYMBOLP(obj));
        scm_symbol_t symbol = (scm_symbol_t)obj;
        return MAKEFIXNUM(HDR_SYMBOL_CODE(symbol->hdr) << FIXNUM_OPCODE_SHIFT);
    }

    int instruction_to_opcode(scm_obj_t obj) {
        assert(FIXNUMP(obj));
        int opcode = (((uintptr_t)obj) & 0xff00) >> (FIXNUM_OPCODE_SHIFT + 1);
        assert(opcode >= 0 && opcode < VMOP_INSTRUCTION_COUNT);
        return opcode;
    }

    scm_obj_t opcode_to_instruction(int opcode) {
        return MAKEFIXNUM((intptr_t)opcode << FIXNUM_OPCODE_SHIFT);
    }
#endif
#if USE_SYMBOL_THREAD
    scm_obj_t symbol_to_instruction(scm_obj_t obj) {
        assert(OPCODESYMBOLP(obj));
        return obj;
    }

    int instruction_to_opcode(scm_obj_t obj) {
        assert(OPCODESYMBOLP(obj));
        return HDR_SYMBOL_CODE(((scm_symbol_t)obj)->hdr);
    }

    scm_obj_t opcode_to_instruction(int opcode) {
        assert(opcode >= 0 && opcode < VMOP_INSTRUCTION_COUNT);
        return m_heap->inherent_symbol(opcode);
    }
#endif
} ATTRIBUTE(aligned(16));

#endif
