/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#include "core.h"
#include "vm.h"
#include "hash.h"
#include "port.h"
#include "reader.h"
#include "printer.h"
#if USE_PARALLEL_VM
#include "interpreter.h"
#endif

scm_obj_t
VM::lookup_current_environment(scm_symbol_t symbol)
{
    scoped_lock lock(m_current_environment->variable->lock);
    scm_obj_t obj = get_hashtable(m_current_environment->variable, symbol);
    if (obj != scm_undef) {
        assert(GLOCP(obj));
        return ((scm_gloc_t)obj)->value;
    }
    return scm_undef;
}

scm_closure_t
VM::lookup_system_closure(const char* name)
{
    scm_obj_t proc = m_heap->lookup_system_environment(make_symbol(m_heap, name));
    if (CLOSUREP(proc)) return (scm_closure_t)proc;
    fatal("fatal: #<closure %s> not available in system environment", name);
}

void
VM::intern_current_environment(scm_symbol_t symbol, scm_obj_t value)
{
    scm_hashtable_t ht = m_current_environment->variable;
    scoped_lock lock(ht->lock);
    scm_obj_t obj = get_hashtable(ht, symbol);
    if (obj != scm_undef) {
        assert(GLOCP(obj));
#if USE_PARALLEL_VM
        if (m_interp->concurrency() > 1) {
            assert(m_heap->in_heap(obj));
            m_interp->remember(((scm_gloc_t)obj)->value, value);
        }
#endif
        m_heap->write_barrier(value);
        ((scm_gloc_t)obj)->value = value;
        return;
    }
    scm_gloc_t gloc = make_gloc(m_heap, m_current_environment, symbol);
    gloc->value = value;
    m_heap->write_barrier(symbol);
    m_heap->write_barrier(gloc);
    int nsize = put_hashtable(ht, symbol, gloc);
    if (nsize) rehash_hashtable(m_heap, ht, nsize);
}

bool
VM::init(object_heap_t* heap)
{
    try {
        m_heap = heap;
        m_stack_size = VM_STACK_BYTESIZE;
        m_stack_top = (scm_obj_t*)m_heap->allocate(m_stack_size, false, false);
        m_stack_limit = (scm_obj_t*)((intptr_t)m_stack_top + m_stack_size);
        memset(m_stack_top, 0, m_stack_size);
        m_to_stack_top = (scm_obj_t*)m_heap->allocate(m_stack_size, false, false);
        m_to_stack_limit = (scm_obj_t*)((intptr_t)m_to_stack_top + m_stack_size);
        memset(m_to_stack_top, 0, m_stack_size);
        m_current_environment = m_heap->m_system_environment;

        #if _MSC_VER
            scm_bvector_t cp932 = make_bvector(m_heap, 3);
            scm_bvector_t utf8 = make_bvector(m_heap, 3);
            cp932->elts[0] = SCM_PORT_CODEC_CP932;
            cp932->elts[1] = SCM_PORT_EOL_STYLE_CRLF;
            cp932->elts[2] = SCM_PORT_ERROR_HANDLING_MODE_IGNORE;
            utf8->elts[0] = SCM_PORT_CODEC_UTF8;
            utf8->elts[1] = SCM_PORT_EOL_STYLE_CRLF;
            utf8->elts[2] = SCM_PORT_ERROR_HANDLING_MODE_IGNORE;
            m_current_input  = make_std_port(m_heap,
                                             PORT_STDIN_FD,
                                             make_string_literal(m_heap, "/dev/stdin"),
                                             SCM_PORT_DIRECTION_IN,
                                             SCM_PORT_FILE_OPTION_NONE,
                                             SCM_PORT_BUFFER_MODE_BLOCK,
                                             (GetFileType(PORT_STDIN_FD) == FILE_TYPE_CHAR && GetConsoleCP() == 932) ? cp932 : utf8);
            m_current_output = make_std_port(m_heap,
                                             PORT_STDOUT_FD,
                                             make_string_literal(m_heap, "/dev/stdout"),
                                             SCM_PORT_DIRECTION_OUT,
                                             SCM_PORT_FILE_OPTION_NO_FAIL,
                                             SCM_PORT_BUFFER_MODE_LINE,
                                             (GetFileType(PORT_STDOUT_FD) == FILE_TYPE_CHAR && GetConsoleCP() == 932) ? cp932 : utf8);
            m_current_error  = make_std_port(m_heap,
                                             PORT_STDERR_FD,
                                             make_string_literal(m_heap, "/dev/stderr"),
                                             SCM_PORT_DIRECTION_OUT,
                                             SCM_PORT_FILE_OPTION_NO_FAIL,
                                             SCM_PORT_BUFFER_MODE_LINE,
                                             (GetFileType(PORT_STDERR_FD) == FILE_TYPE_CHAR && GetConsoleCP() == 932) ? cp932 : utf8);
        #else
            m_current_input  = make_std_port(m_heap,
                                             PORT_STDIN_FD,
                                             make_string_literal(m_heap, "/dev/stdin"),
                                             SCM_PORT_DIRECTION_IN,
                                             SCM_PORT_FILE_OPTION_NONE,
                                             SCM_PORT_BUFFER_MODE_BLOCK,
                                             scm_true);
            m_current_output = make_std_port(m_heap,
                                             PORT_STDOUT_FD,
                                             make_string_literal(m_heap, "/dev/stdout"),
                                             SCM_PORT_DIRECTION_OUT,
                                             SCM_PORT_FILE_OPTION_NO_FAIL,
                                             SCM_PORT_BUFFER_MODE_LINE,
                                             scm_true);
            m_current_error  = make_std_port(m_heap,
                                             PORT_STDERR_FD,
                                             make_string_literal(m_heap, "/dev/stderr"),
                                             SCM_PORT_DIRECTION_OUT,
                                             SCM_PORT_FILE_OPTION_NO_FAIL,
                                             SCM_PORT_BUFFER_MODE_LINE,
                                             scm_true);
        #endif

        m_current_source_comments = scm_false;
        m_current_exception_handler = scm_false;
        m_current_dynamic_environment = make_weakhashtable(m_heap, lookup_mutable_hashtable_size(0));
        m_current_dynamic_wind_record = scm_nil;
        m_recursion_level = 0;
        m_shared_object_errno = 0;
        m_shared_object_win32_lasterror = 0;
#if BOOT_R6RS_COMPLIANT_SYNTAX
        flags.m_extend_lexical_syntax = scm_false;
#else
        flags.m_extend_lexical_syntax = scm_true;
#endif
        flags.m_mutable_literals = scm_false;
        flags.m_collect_notify = scm_false;
        flags.m_collect_stack_notify = scm_false;
        flags.m_backtrace = scm_true;
        flags.m_backtrace_line_length = MAKEFIXNUM(80);
        flags.m_restricted_print_line_length = MAKEFIXNUM(40);
        flags.m_record_print_nesting_limit = MAKEFIXNUM(2);
        flags.m_warning_level = scm_false;
        run(true);
        return true;
    } catch (io_exception_t& e) {
        fatal("fatal in init-vm: unexpected io_expecption_t(%d, %s)", e.m_err, e.m_message);
    } catch (vm_exception_t& e) {
        fatal("fatal in init-vm: unexpected vm_exception_t");
    } catch (reader_exception_t& e) {
        fatal("fatal in init-vm: unexpected reader_expecption_t(%s)", e.m_message);
    } catch (io_codec_exception_t& e) {
        fatal("fatal in init-vm: unexpected io_codec_exception_t(%d, %s)", e.m_operation, e.m_message);
    } catch (vm_escape_t& e) {
        fatal("fatal in init-vm: unexpected vm_escape_t, maybe <#subr escape> in bad context");
    } catch (vm_continue_t& e) {
        fatal("fatal in init-vm: unexpected vm_continue_t, maybe <#subr escape> in bad context");
    } catch (int code) {
        fatal("fatal in init-vm: unexpected system error (errno %d, %s)", code, strerror(code));
    } catch (...) {
        fatal("fatal in init-vm: unknown exception");
    }
}

void
VM::scheme_warning(const char* fmt, ...)
{
    {
        scoped_lock lock(m_current_output->lock);
        port_flush_output(m_current_output);
    }
    va_list ap;
    va_start(ap, fmt);
    {
        scoped_lock lock(m_current_error->lock);
        printer_t prt(this, m_current_error);
        prt.format("~&~%");
        prt.format_va_list(fmt, ap);
        prt.format("~%");
        prt.flush();
    }
    va_end(ap);
}

void
VM::scheme_error(const char* fmt, ...)
{
    {
        scoped_lock lock(m_current_output->lock);
        port_flush_output(m_current_output);
    }
    {
        scoped_lock lock(m_current_input->lock);
        while (port_nonblock_byte_ready(m_current_input)) port_get_byte(m_current_input);
    }
    va_list ap;
    va_start(ap, fmt);
    {
        scoped_lock lock(m_current_error->lock);
        printer_t prt(this, m_current_error);
        prt.format("~&~%");
        prt.format_va_list(fmt, ap);
        prt.format("~%");
        prt.flush();
    }
    va_end(ap);
    throw vm_exception_t();
}

void
VM::system_error(const char* fmt, ...)
{
    {
        scoped_lock lock(m_current_output->lock);
        port_flush_output(m_current_output);
    }
    {
        scoped_lock lock(m_current_input->lock);
        while (port_nonblock_byte_ready(m_current_input)) port_get_byte(m_current_input);
    }
    va_list ap;
    va_start(ap, fmt);
    {
        scoped_lock lock(m_current_error->lock);
        printer_t prt(this, m_current_error);
        prt.format("~&~%");
        prt.format_va_list(fmt, ap);
        prt.format("~%");
        prt.flush();
    }
    va_end(ap);
    throw vm_exception_t();
}

void
VM::backtrace_seek_make_cont(scm_obj_t note)
{
    if (note != scm_unspecified) {
        scm_obj_t* argp = m_fp;
        int argc = m_sp - m_fp;
        int space = sizeof(vm_cont_rec_t) + sizeof(scm_obj_t*) * argc;
        if ((uintptr_t)m_sp + space >= (uintptr_t)m_stack_limit) collect_stack(space);
        vm_cont_t cont = (vm_cont_t)m_sp;
        cont->trace = note;
        cont->fp = m_fp;
        cont->env = m_env;
        cont->pc = scm_nil;
        cont->up = m_cont;
        m_sp = m_fp = (scm_obj_t*)(cont + 1);
        m_cont = &cont->up;
        for (int i = 0; i < argc; i++) *m_sp++ = *argp++; // move args
    }
}

void
VM::backtrace_seek()
{
    if (flags.m_backtrace != scm_false) {
        backtrace_seek_make_cont(m_trace);
        backtrace_seek_make_cont(m_trace_tail);
        m_trace = m_trace_tail = scm_unspecified;
        scm_obj_t lst = CDR(m_pc);
        while (lst != scm_nil) {
            scm_obj_t operands = (scm_obj_t)CDAR(lst);
            int opcode = instruction_to_opcode(CAAR(lst));
            switch (opcode) {
            case VMOP_RET_SUBR_GLOC_OF:
            case VMOP_APPLY_GLOC_OF:
                fatal("%s:%u internal error: backtrace_seek()", __FILE__, __LINE__);
            case VMOP_RET_SUBR:
                if (PAIRP(CDR(operands))) {
                    backtrace_seek_make_cont(CDR(operands));
                    goto more_seek;
                }
                break;
            case VMOP_APPLY_GLOC:
                if (PAIRP(CDR(operands))) {
                    backtrace_seek_make_cont(CDR(operands));
                    goto more_seek;
                }
                break;
            case VMOP_APPLY_ILOC:
                if (PAIRP(CDR(operands))) {
                    backtrace_seek_make_cont(CDR(operands));
                    goto more_seek;
                }
                break;
            case VMOP_APPLY_ILOC_LOCAL:
                if (PAIRP(CDR(operands))) {
                    backtrace_seek_make_cont(CDR(operands));
                    goto more_seek;
                }
                break;
            case VMOP_APPLY:
                if (PAIRP(operands)) {
                    backtrace_seek_make_cont(operands);
                    goto more_seek;
                }
                break;
            case VMOP_RET_CONS:
            case VMOP_RET_EQP:
            case VMOP_RET_NULLP:
            case VMOP_RET_PAIRP:
                if (PAIRP(operands)) {
                    backtrace_seek_make_cont(operands);
                    goto more_seek;
                }
                break;
            case VMOP_EXTEND:
            case VMOP_EXTEND_UNBOUND:
                goto more_seek;
            }
            lst = CDR(lst);
        }
more_seek:
        scm_obj_t lst2 = m_pc;
more_more_seek:
        if (lst2 == scm_nil) return;
        if (!PAIRP(CAR(lst2))) return;
        scm_obj_t operands = (scm_obj_t)CDAR(lst2);
        int opcode = instruction_to_opcode(CAAR(lst2));
        switch (opcode) {
        case VMOP_SUBR_GLOC_OF:
            fatal("%s:%u intern error backtrace_seek()", __FILE__, __LINE__);
        case VMOP_SUBR:
            if (PAIRP(CDDR(operands))) backtrace_seek_make_cont(CDDR(operands));
            return;
        case VMOP_EQ_ILOC:
        case VMOP_LT_ILOC:
        case VMOP_LE_ILOC:
        case VMOP_GT_ILOC:
        case VMOP_GE_ILOC:
            if (PAIRP(CDR(operands))) backtrace_seek_make_cont(CDR(operands));
            return;
        case VMOP_EQ_N_ILOC:
        case VMOP_LT_N_ILOC:
        case VMOP_LE_N_ILOC:
        case VMOP_GT_N_ILOC:
        case VMOP_GE_N_ILOC:
        case VMOP_NADD_ILOC:
        case VMOP_PUSH_NADD_ILOC:
        case VMOP_PUSH_SUBR:
            if (PAIRP(CDDR(operands))) backtrace_seek_make_cont(CDDR(operands));
            return;
        case VMOP_CAR_ILOC:
        case VMOP_CDR_ILOC:
        case VMOP_PUSH_CAR_ILOC:
        case VMOP_PUSH_CDR_ILOC:
        case VMOP_PUSH_CADR_ILOC:
        case VMOP_PUSH_CDDR_ILOC:
            if (PAIRP(CDR(operands))) backtrace_seek_make_cont(CDR(operands));
            return;
        case VMOP_CONST:
        case VMOP_GLOC:
        case VMOP_ILOC:
        case VMOP_ILOC0:
        case VMOP_ILOC1:
        case VMOP_CLOSE:
        case VMOP_CONST_UNSPEC:
        case VMOP_PUSH_CONST:
        case VMOP_PUSH_GLOC:
        case VMOP_PUSH_ILOC:
        case VMOP_PUSH_ILOC0:
        case VMOP_PUSH_ILOC1:
        case VMOP_PUSH_CLOSE:
        case VMOP_PUSH:
        case VMOP_CALL:
            lst2 = CDR(lst2);
            goto more_more_seek;
        }
    }
}

scm_obj_t
VM::backtrace_fetch(const char* name, int line, int column)
{
    try {
        scm_port_t port = make_file_port(m_heap, make_string_literal(m_heap, name), SCM_PORT_DIRECTION_IN, 0, SCM_PORT_BUFFER_MODE_BLOCK, scm_true);
        scoped_lock lock(port->lock);
        if (port_regular_file_pred(port)) {
            int c;
            do {
                if (port->line == line && port->column == column) {
                    reader_t source(this, port);
                    return source.read(NULL);
                }
            } while ((c = port_get_byte(port)) != EOF);
        }
        return scm_unspecified;
    } catch (...) {
        return scm_unspecified;
    }
}

void
VM::backtrace_each(printer_t* prt, int n, scm_obj_t note)
{
    assert(PAIRP(note));
    if (n < 10) prt->byte(' ');
    if (CDR(note) == scm_nil) {
        // (expr) : dynamic
        prt->format(" %d  ~u", n, CAR(note));
    } else if (FIXNUMP(CDR(note))) {
        // (path . fixnum) : load
        assert(STRINGP(CAR(note)));
        scm_string_t string = (scm_string_t)CAR(note);
        int comment = FIXNUM(CDR(note));
        int line = comment / MAX_SOURCE_COLUMN;
        int column = comment % MAX_SOURCE_COLUMN;
        scm_obj_t expr = backtrace_fetch(string->name, line, column);
        if (expr == scm_unspecified) {
            prt->format(" %d  --- unknown ---", n);
        } else {
            prt->format(" %d  ~u", n, expr);
        }
       prt->format("~%  ...~s line %d", string, line);
    } else {
        // (expr path . fixnum) : repl
        scm_string_t string = (scm_string_t)CADR(note);
        int comment = FIXNUM(CDDR(note));
        int line = comment / MAX_SOURCE_COLUMN;
        prt->format(" %d  ~u", n, CAR(note));
        prt->format("~%  ...~s line %d", string, line);
    }
    prt->format("~%");
}

bool
VM::backtrace(scm_port_t port)
{
    if (flags.m_backtrace == scm_false) return false;

    scoped_lock lock(port->lock);
    printer_t prt(this, port);

    scm_obj_t obj = scm_unspecified;
    if (m_trace_tail != scm_unspecified && CDR(m_trace_tail) != scm_nil) {
        obj = m_trace_tail;
    } else if (m_trace != scm_unspecified && CDR(m_trace) != scm_nil) {
        obj = m_trace;
    } else {
        void* lnk = m_cont;
        while (lnk) {
            vm_cont_t cont = (vm_cont_t)((intptr_t)lnk - offsetof(vm_cont_rec_t, up));
            if (cont->trace != scm_unspecified && CDR(cont->trace)) {
                obj = cont->trace;
                break;
            }
            lnk = (*(void**)lnk);
        }
    }
    if (obj == scm_unspecified) return false;
    int bt_level = FIXNUMP(flags.m_backtrace) ? FIXNUM(flags.m_backtrace) : FIXNUM_MAX;
    int n = 0;
    if (n == bt_level) return false;
    prt.format("~%backtrace:~%");
    prt.column_limit(FIXNUM(flags.m_backtrace_line_length));
    if (m_trace_tail != scm_unspecified) {
        backtrace_each(&prt, n++, m_trace_tail);
        if (n == bt_level) return true;
    }
    if (m_trace != scm_unspecified) {
        backtrace_each(&prt, n++, m_trace);
        if (n == bt_level) return true;
    }
    void* lnk = m_cont;
    while (lnk) {
        vm_cont_t cont = (vm_cont_t)((intptr_t)lnk - offsetof(vm_cont_rec_t, up));
        if (cont->trace != scm_unspecified) {
            backtrace_each(&prt, n++, cont->trace);
            if (n == bt_level) return true;
        }
        lnk = (*(void**)lnk);
    }
    return true;
}

void
VM::reset()
{
    m_pc = scm_nil;
    m_cont = NULL;
    m_env = NULL;
    m_sp = m_fp = m_stack_top;
    m_stack_busy = false;
    m_value = scm_unspecified;
    m_trace = scm_unspecified;
    m_trace_tail = scm_unspecified;
    errno = 0;
}

#if PROFILE_SUBR
    void
    VM::display_subr_profile()
    {
        scm_hashtable_t ht = m_heap->m_system_environment->variable;
        hashtable_rec_t* ht_datum = ht->datum;
        int n = ht_datum->capacity;
        printf("%36s: %12s %12s %12s %14s\n", "subr", "push", "load", "apply", "total");
        for (int i = 0; i < n; i++) {
            if (SYMBOLP(ht_datum->elts[i])){
                scm_symbol_t symbol = (scm_symbol_t)ht_datum->elts[i];
                scm_gloc_t gloc = (scm_gloc_t)ht_datum->elts[n + i];
                if (GLOCP(gloc)) {
                    scm_subr_t subr = (scm_subr_t)gloc->value;
                    if (SUBRP(subr)) {
                        if (subr->c_push + subr->c_load + subr->c_apply != 0) {
                            printf("%36s: %12llu %12llu %12llu %14llu\n",
                                    symbol->name,
                                    subr->c_push,
                                    subr->c_load,
                                    subr->c_apply,
                                    subr->c_push + subr->c_load + subr->c_apply);
                        }
                    }
                }
            }
        }
    }
#endif

#if PROFILE_OPCODE
    int
    VM::comp_profile_rec(const void* a1, const void* a2)
    {
        opcode_profile_t* p1 = (opcode_profile_t*)a1;
        opcode_profile_t* p2 = (opcode_profile_t*)a2;
        if (p1->count > p2->count) return -1;
        if (p1->count < p2->count) return 1;
        return 0;
    }

    void
    VM::display_opcode_profile()
    {
        for (int i = 0; i < VMOP_INSTRUCTION_COUNT; i++) m_opcode_profile[i].opcode = i;

        qsort(m_opcode_profile, array_sizeof(m_opcode_profile), sizeof(m_opcode_profile[0]), comp_profile_rec);
        for (int i = 0; i < VMOP_INSTRUCTION_COUNT; i++) {
            uint64_t m = 0;
            int prevcode = 0;
            for (int n = 0; n < VMOP_INSTRUCTION_COUNT; n++) {
                if (m < m_opcode_profile[i].prev[n]) {
                    m = m_opcode_profile[i].prev[n];
                    prevcode = n;
                }
            }
            if (m) {
                printf("%24s: %10llu    |%24s: %10llu (%.2f%%) \n",
                        m_heap->inherent_symbol(m_opcode_profile[i].opcode)->name,
                        m_opcode_profile[i].count,
                        m_heap->inherent_symbol(prevcode)->name,
                        m,
                        m * 100.0 / m_opcode_profile[i].count);
            } else {
                printf("%24s: %10llu  \n",
                        m_heap->inherent_symbol(m_opcode_profile[i].opcode)->name,
                        m_opcode_profile[i].count);
            }
        }
    }

#endif

#include "../heap/bootimage.code"
#include "../heap/coreimage.code"

void
VM::boot()
{
    try {
#if USE_DEBUG_BOOT
        {
            char load_path[] = "heap/debug-boot.vmi";
            m_bootport = make_file_port(m_heap, make_string_literal(m_heap, load_path), SCM_PORT_DIRECTION_IN, 0, SCM_PORT_BUFFER_MODE_BLOCK, scm_true);
            printf(";; loading \"%s\"\n", load_path);
            fflush(stdout);
            scoped_lock lock_port(m_bootport->lock);
            while (true) {
                reset();
                scm_obj_t obj = reader_t(this, m_bootport).read(NULL);
                if (obj == scm_eof) break;
                m_pc = obj;
                prebind(m_pc);
                run(false);
            }
            port_close(m_bootport);
        }
#else
        {
            scm_bvector_t bv = make_bvector_mapping(m_heap, (void*)s_bootimage, sizeof(s_bootimage));
            m_bootport = make_bytevector_port(m_heap, make_symbol(m_heap, "bootimage"), SCM_PORT_DIRECTION_IN, bv, scm_true);
            scoped_lock lock_port(m_bootport->lock);
            while (true) {
                reset();
                scm_obj_t obj = reader_t(this, m_bootport).read(NULL);
                if (obj == scm_eof) break;
                m_pc = obj;
                prebind(m_pc);
                run(false);
            }
            port_close(m_bootport);
        }
#endif
        m_bootport = (scm_port_t)scm_unspecified;
        m_current_environment = m_heap->m_interaction_environment;
#if USE_DEBUG_CORE
        {
            char load_path[] = "heap/debug-core.vmi";
            m_bootport = make_file_port(m_heap, make_string_literal(m_heap, load_path), SCM_PORT_DIRECTION_IN, 0, SCM_PORT_BUFFER_MODE_BLOCK, scm_true);
            printf(";; loading \"%s\"\n", load_path);
            fflush(stdout);
            scoped_lock lock_port(m_bootport->lock);
            while (true) {
                reset();
                scm_obj_t obj = reader_t(this, m_bootport).read(NULL);
                if (obj == scm_eof) break;
                m_pc = obj;
                prebind(m_pc);
                run(false);
            }
            port_close(m_bootport);
        }
#elif USE_INTERNED_CORE
        {
            scm_bvector_t bv = make_bvector_mapping(m_heap, (void*)s_coreimage, sizeof(s_coreimage));
            m_bootport = make_bytevector_port(m_heap, make_symbol(m_heap, "bootimage"), SCM_PORT_DIRECTION_IN, bv, scm_true);
            scoped_lock lock_port(m_bootport->lock);
            while (true) {
                reset();
                scm_obj_t obj = reader_t(this, m_bootport).read(NULL);
                if (obj == scm_eof) break;
                m_pc = obj;
                prebind(m_pc);
                run(false);
            }
            port_close(m_bootport);
        }
#endif
        m_bootport = (scm_port_t)scm_unspecified;
    } catch (vm_exception_t& e) {
        fatal("fatal in boot: unexpected vm_exception_t");
    } catch (reader_exception_t& e) {
        fatal("fatal in boot: unexpected reader_expecption_t(%s)", e.m_message);
    } catch (io_exception_t& e) {
        fatal("fatal in boot: unexpected io_expecption_t(%d, %s)", e.m_err, e.m_message);
    } catch (io_codec_exception_t& e) {
        fatal("fatal in boot: unexpected io_codec_exception_t(%d, %s)", e.m_operation, e.m_message);
    } catch (vm_escape_t& e) {
        fatal("fatal in boot: unexpected vm_escape_t, maybe <#subr escape> in bad context");
    } catch (vm_continue_t& e) {
        fatal("fatal in boot: unexpected vm_continue_t, maybe <#subr escape> in bad context");
    } catch (int code) {
        fatal("fatal in boot: unexpected system error (errno %d, %s)", code, strerror(code));
    } catch (...) {
        fatal("fatal in boot: unknown exception");
    }
  #if PROFILE_OPCODE
    memset(m_opcode_profile, 0, sizeof(m_opcode_profile));
  #endif
}

void
VM::standalone()
{
loop:
    try {
        reset();
        scm_closure_t closure = lookup_system_closure(".@start-scheme-session");
        m_pc = closure->code;
        prebind(m_pc);
        run(false);
    } catch (vm_exit_t& e) {
#if PROFILE_OPCODE
        display_opcode_profile();
#endif
#if PROFILE_SUBR
        display_subr_profile();
#endif
        exit(e.m_code);
    } catch (vm_exception_t& e) {
        backtrace(m_current_error);
        goto loop;
    } catch (io_exception_t& e) {
        if (e.m_err == EINTR) goto loop;
        if (e.m_err == EIO) goto loop;
        fatal("fatal in run: unexpected io_expecption_t(%d, %s)", e.m_err, e.m_message);
    } catch (reader_exception_t& e) {
        fatal("fatal in run: unhandled exception reader_expecption_t(%s)", e.m_message);
    } catch (io_codec_exception_t& e) {
        fatal("fatal in run: unhandled exception io_codec_exception_t(%d, %s)", e.m_operation, e.m_message);
    } catch (vm_escape_t& e) {
        fatal("fatal in run: unhandled exception vm_escape_t, maybe (escape) procedure in bad context");
    } catch (vm_continue_t& e) {
        fatal("fatal in run: unhandled exception vm_continue_t, maybe (escape) procedure in bad context");
    } catch (int code) {
        fatal("fatal in run: unexpected exception (errno %d, %s)", code, strerror(code));
    } catch (...) {
        fatal("fatal in run: unknown exception");
    }
}

void
VM::stop()
{
    #define ARGC_TH     8
    collector_usage_t last_usage = m_heap->m_usage;
    if (last_usage.m_recorded) m_heap->m_usage.clear();
    double t1 = msec();
#if HPDEBUG
    if (m_heap->m_root_snapshot == ROOT_SNAPSHOT_CONSISTENCY_CHECK) save_stack();
#endif
    if ((m_heap->m_root_snapshot == ROOT_SNAPSHOT_EVERYTHING) ||
        (m_heap->m_root_snapshot == ROOT_SNAPSHOT_RETRY) ||
        (m_heap->m_root_snapshot == ROOT_SNAPSHOT_GLOBALS)) {
        m_heap->enqueue_root(m_bootport);
        m_heap->enqueue_root(m_current_input);
        m_heap->enqueue_root(m_current_output);
        m_heap->enqueue_root(m_current_error);
        m_heap->enqueue_root(m_current_exception_handler);
        m_heap->enqueue_root(m_current_environment);
        m_heap->enqueue_root(m_current_dynamic_environment);
        m_heap->enqueue_root(m_current_dynamic_wind_record);
        m_heap->enqueue_root(m_current_source_comments);
    }
    if ((m_heap->m_root_snapshot == ROOT_SNAPSHOT_EVERYTHING) ||
        (m_heap->m_root_snapshot == ROOT_SNAPSHOT_RETRY) ||
        (m_heap->m_root_snapshot == ROOT_SNAPSHOT_LOCALS)) {
        save_stack();
        m_heap->enqueue_root(m_pc);
        m_heap->enqueue_root(m_value);
        m_heap->enqueue_root(m_trace);
        m_heap->enqueue_root(m_trace_tail);
        if (m_fp != m_sp) {
            int argc = m_sp - m_fp;
            if (argc > ARGC_TH) {
                scm_vector_t vector = make_vector(m_heap, argc, scm_nil);
                for (int i = 0; i < argc; i++) vector->elts[i] = m_fp[i];
                m_heap->enqueue_root(vector);
            } else {
                for (int i = 0; i < argc; i++) m_heap->enqueue_root(m_fp[i]);
            }
        }
        if (m_cont) {
            assert(m_heap->is_collectible(m_cont));
            m_heap->enqueue_root(OBJECT_SLAB_TRAITS_OF(m_cont)->cache->lookup(m_cont));
        }
        if (m_env) {
            assert(m_heap->is_collectible(m_env));
            m_heap->enqueue_root(OBJECT_SLAB_TRAITS_OF(m_env)->cache->lookup(m_env));
        }
    }
#if USE_PARALLEL_VM
    if (m_interp->concurrency() > 1) {
        if (m_heap->m_root_snapshot == ROOT_SNAPSHOT_EVERYTHING) m_interp->snapshot(this, false);
        if (m_heap->m_root_snapshot == ROOT_SNAPSHOT_RETRY) m_interp->snapshot(this, true);
    }
#endif
    m_heap->m_collector_lock.lock();
    while (m_heap->m_stop_the_world) {
        m_heap->m_mutator_stopped = true;
        m_heap->m_collector_wake.signal();
        m_heap->m_mutator_wake.wait(m_heap->m_collector_lock);
        m_heap->m_mutator_stopped = false;
    }
    m_heap->m_collector_wake.signal();
    m_heap->m_collector_lock.unlock();
    double t2 = msec();
    switch (m_heap->m_root_snapshot) {
        case ROOT_SNAPSHOT_GLOBALS:
            m_heap->m_usage.m_pause1 = t2 - t1;
            break;
        case ROOT_SNAPSHOT_LOCALS:
            m_heap->m_usage.m_pause2 = t2 - t1;
            break;
        case ROOT_SNAPSHOT_RETRY:
        case ROOT_SNAPSHOT_EVERYTHING: {
            double d = t2 - t1;
            if (d > m_heap->m_usage.m_pause3) m_heap->m_usage.m_pause3 = d;
        } break;
    }

    char usage[128];
    if (flags.m_collect_notify != scm_false) {
        if (last_usage.m_recorded) {
            if (DETAILED_STATISTIC) {
                if (last_usage.m_synchronized) {
                    snprintf(usage,
                             sizeof(usage),
                             ";; [collect synchronize: %.2fms]",
                             last_usage.m_duration);
                } else {
                    snprintf(usage,
                            sizeof(usage),
                             ";; [collect concurrent: %.2fms sync: %.2fms/%.2fms pause: %.2fms/%.2fms/%.2fms barrier: %dR/%dW/%dA]",
                             last_usage.m_duration,
                             last_usage.m_sync1,
                             last_usage.m_sync2,
                             last_usage.m_pause1,
                             last_usage.m_pause2,
                             last_usage.m_pause3,
                             last_usage.m_barriered_read,
                             last_usage.m_barriered_write,
                             last_usage.m_barriered_alloc);
                }
            } else {
                if (last_usage.m_synchronized) {
                    snprintf(usage,
                             sizeof(usage),
                             ";; [collect synchronize: %.2fms]",
                             last_usage.m_duration);
                } else {
                    snprintf(usage,
                             sizeof(usage),
                             ";; [collect concurrent: %.2fms pause: %.2fms/%.2fms/%.2fms]",
                             last_usage.m_duration,
                             last_usage.m_pause1,
                             last_usage.m_pause2,
                             last_usage.m_pause3);
                }
            }
            scoped_lock lock(m_current_output->lock);
            printer_t prt(this, m_current_output);
            prt.format("~&%s", usage);
            if (last_usage.m_shade_queue_hazard) prt.format("[shade queue overflow: %d]", last_usage.m_shade_queue_hazard);
            if (last_usage.m_expand_mark_stack) prt.format("[mark stack overflow: %d]", last_usage.m_expand_mark_stack);
            prt.format("~%~!");
        }
    } else {
        if (last_usage.m_recorded) {
            if (CONCURRENT_COLLECT) {
                if (last_usage.m_synchronized) {
                    snprintf(usage,
                             sizeof(usage),
                             "warning: low heap memory (collect: %.2fms)",
                             last_usage.m_duration);
                    scoped_lock lock(m_current_error->lock);
                    printer_t prt(this, m_current_error);
                    prt.format("~&%s~%~!", usage);
                }
            }
        }
    }
}

void
VM::resolve()
{
    save_stack();
    m_bootport = (scm_port_t)m_heap->forward(m_bootport);
    m_current_input = (scm_port_t)m_heap->forward(m_current_input);
    m_current_output = (scm_port_t)m_heap->forward(m_current_output);
    m_current_error = (scm_port_t)m_heap->forward(m_current_error);
    m_current_exception_handler = m_heap->forward(m_current_exception_handler);
    m_current_environment = (scm_environment_t)m_heap->forward(m_current_environment);
    m_current_dynamic_environment = (scm_weakhashtable_t)m_heap->forward(m_current_dynamic_environment);
    m_current_dynamic_wind_record = m_heap->forward(m_current_dynamic_wind_record);
    m_current_source_comments = m_heap->forward(m_current_source_comments);
    m_pc = m_heap->forward(m_pc);
    m_value = m_heap->forward(m_value);
    m_trace = m_heap->forward(m_trace);
    m_trace_tail = m_heap->forward(m_trace_tail);
    if (m_fp != m_sp) {
        int argc = m_sp - m_fp;
        for (int i = 0; i < argc; i++) m_fp[i] = m_heap->forward(m_fp[i]);
    }
    if (m_cont) {
        assert(m_heap->is_collectible(m_cont));
        m_cont = m_heap->interior_forward(m_cont);
    }
    if (m_env) {
        assert(m_heap->is_collectible(m_env));
        m_env = m_heap->interior_forward(m_env);
    }
}
