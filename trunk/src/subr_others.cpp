/*
  Ypsilon Scheme System
  Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
  See license.txt for terms and conditions of use
*/

#include "core.h"
#include "vm.h"
#include "violation.h"
#include "arith.h"
#include "hash.h"
#include "utf8.h"
#include "port.h"
#include "list.h"
#include "reader.h"
#include "printer.h"
#include "ioerror.h"
#if USE_PARALLEL_VM
#include "interpreter.h"
#endif

#define DEFAULT_GENSYM_PREFIX           ".G"

// core-read
scm_obj_t
subr_core_read(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc <= 3) {
        scm_hashtable_t note = NULL;
        scm_symbol_t who = make_symbol(vm->m_heap,"core-read");
        scm_port_t port;
        if (argc == 0) {
            port = vm->m_current_input;
        } else if (PORTP(argv[0])) {
            port = (scm_port_t)argv[0];
        } else {
            wrong_type_argument_violation(vm, "core-read", 0, "input port", argv[0], argc, argv);
            return scm_undef;
        }
        if (argc >= 2) {
            if (HASHTABLEP(argv[1])) {
                note = (scm_hashtable_t)argv[1];
            } else if (argv[1] != scm_false) {
                wrong_type_argument_violation(vm, "core-read", 1, "hash-table or #f", argv[1], argc, argv);
                return scm_undef;
            }
        }
        if (argc == 3) {
            if (SYMBOLP(argv[2])) {
                who = (scm_symbol_t)argv[2];
            } else {
                wrong_type_argument_violation(vm, "core-read", 2, "symbol", argv[2], argc, argv);
                return scm_undef;
            }
        }
        scoped_lock lock(port->lock);
        if (!port_input_pred(port)) {
            if (argc > 0) wrong_type_argument_violation(vm, "core-read", 0, "input port", argv[0], argc, argv);
            else invalid_object_violation(vm, "core-read", "input port", port, argc, argv);
            return scm_undef;
        }
        if (!port_open_pred(port)) {
            if (argc > 0) wrong_type_argument_violation(vm, "read", 0, "opened port", argv[0], argc, argv);
            else invalid_object_violation(vm, "core-read", "opened port", port, argc, argv);
            return scm_undef;
        }
        try {
            return reader_t(vm, port).read(note);
        } catch (reader_exception_t& exception) {
            lexical_violation(vm, who, exception.m_message);
            return scm_undef;
        } catch (io_exception_t& e) {
            raise_io_error(vm, who->name, e.m_operation, e.m_message, e.m_err, port, scm_false);
            return scm_undef;
        } catch (io_codec_exception_t& e) {
            raise_io_codec_error(vm, who->name, e.m_operation, e.m_message, port, e.m_ch);
            return scm_undef;
        }
    }
    wrong_number_of_arguments_violation(vm, "core-read", 0, 3, argc, argv);
    return scm_undef;
}

// interaction-environment
scm_obj_t
subr_interaction_environment(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) {
        return vm->m_heap->m_interaction_environment;
    }
    wrong_number_of_arguments_violation(vm, "interaction-environment", 0, 0, argc, argv);
    return scm_undef;
}

static int
fixed_format_floating_point(object_heap_t* heap, char* buf, int buflen, int precision, scm_flonum_t flonum)
{
    scm_string_t string = cnvt_flonum_to_string(heap, flonum, true);
    strncpy(buf, string->name, buflen);
    if (isinf(flonum->value)) return 0;
    if (isnan(flonum->value)) return 0;
    char* decimal_point = strchr(buf, '.');
    if (decimal_point == NULL) return 0;
    int frac = strlen(decimal_point) - 1;
    if (frac > precision) {
        decimal_point[precision + 1] = 0;
        return 0;
    }
    return precision - frac;
}

static const char* format_description =
    ";; (format [#t|#f|<port>] <format-string> <argument> ...)\n"
    ";;  ~a         display\n"
    ";;  ~s         write\n"
    ";;  ~w         write shared structure\n"
    ";;  ~d         decimal\n"
    ";;  ~x         hexadecimal\n"
    ";;  ~o         octal\n"
    ";;  ~b         binary\n"
    ";;  ~c         character\n"
    ";;  ~y         prettry print\n"
    ";;  ~?         indirection\n"
    ";;  ~k         indirection\n"
    ";;  ~[w[,d]]f  fixed format floating point\n"
    ";;  ~~         tilde\n"
    ";;  ~t         tab\n"
    ";;  ~%         newline\n"
    ";;  ~&         freshline\n"
    ";;  ~_         space\n"
    ";;  ~h         help\n";

// format
scm_obj_t
subr_format(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) {
        wrong_number_of_arguments_violation(vm, "format", 1, -1, argc, argv);
        return scm_undef;
    }

    scm_port_t port = (scm_port_t)scm_unspecified;
    int argp = 0;
    bool output_string = false;

    if (STRINGP(argv[0])) {
        port = make_bytevector_port(vm->m_heap, make_symbol(vm->m_heap, "string"), SCM_PORT_DIRECTION_OUT, scm_false, scm_true);
        output_string = true;
        goto output;
    }
    if (argv[0] == scm_false) {
        port = make_bytevector_port(vm->m_heap, make_symbol(vm->m_heap, "string"), SCM_PORT_DIRECTION_OUT, scm_false, scm_true);
        output_string = true;
        argp = 1;
        goto output;
    }
    if (PORTP(argv[0])) {
        if (argc < 2) {
            wrong_number_of_arguments_violation(vm, "format", 2, -1, argc, argv);
            return scm_undef;
        }
        port = (scm_port_t)argv[0];
        argp = 1;
        goto output;
    }
    if (argv[0] == scm_true) {
        if (argc < 2) {
            wrong_number_of_arguments_violation(vm, "format", 2, -1, argc, argv);
            return scm_undef;
        }
        port = vm->m_current_output;
        argp = 1;
        goto output;
    }
    invalid_argument_violation(vm, "format", "first argument must be either #t, #f, port or string, but got", argv[0], 0, argc, argv);
    return scm_undef;

output:
    assert(argc >= argp);
    bool flush_output = false;
    scm_port_t dst;
    if (output_string || BOOLP(port->transcoder)) {
        dst = port;
    } else {
        dst = make_bytevector_port(vm->m_heap, make_symbol(vm->m_heap, "string"), SCM_PORT_DIRECTION_OUT, scm_false, scm_true);
    }
    try {
        scoped_lock lock(dst->lock);
        printer_t prt(vm, dst);
        if (STRINGP(argv[argp])) {
            scm_string_t fmt = (scm_string_t)argv[argp++];
            const char* p = fmt->name;
            char tilded[3] = {'~', 0, 0};
            char c;
            while ((c = *p++) != 0) {
                if (c == '~') {
                    c = *p++;
                    if (c == 0) {
                        invalid_argument_violation(vm, "format", "wrong directive in control string", fmt, -1, argc, argv);
                        return scm_undef;
                    }
                    tilded[1] = c;
                    if (c == '!') {
                        flush_output = true;
                        continue;
                    }
                    if (c == '&') {
                        if (port->column != 1) prt.byte('\n');
                        continue;
                    }
                    if (strchr("hH", c)) {
                        prt.puts(format_description);
                        continue;
                    }
                    if (strchr("%~t_", c)) {
                        prt.format(tilded);
                        continue;
                    }
                    if (argp < argc) {
                        if (strchr("0123456789", c)) {
                            int width = c - '0';
                            bool ok = false;
                            while ((c = *p++) != 0) {
                                if (strchr("0123456789", c)) {
                                    width = width * 10 + c - '0';
                                    continue;
                                }
                                if (c == ',') {
                                    if (strchr("fF", p[0])) continue;
                                    int fraction = 0;
                                    while ((c = *p++) != 0) {
                                        if (strchr("0123456789", c)) {
                                            fraction = fraction * 10 + c - '0';
                                            continue;
                                        }
                                        if (!strchr("fF", c)) break;
                                        if (STRINGP(argv[argp])) {
                                            scm_string_t string = (scm_string_t)argv[argp++];
                                            int pad = width - strlen(string->name);
                                            for (int i = 0; i < pad; i++) prt.byte(' ');
                                            prt.format("~a", string);
                                            ok = true;
                                            break;
                                        }
                                        if (number_pred(argv[argp])) {
                                            scm_obj_t obj = cnvt_to_inexact(vm->m_heap, argv[argp++]);
                                            if (COMPLEXP(obj)) {
                                                scm_complex_t complex = (scm_complex_t)obj;
                                                char real_buf[512];
                                                int real_n0 = fixed_format_floating_point(vm->m_heap, real_buf, sizeof(real_buf), fraction, (scm_flonum_t)complex->real);
                                                char imag_buf[512];
                                                int imag_n0 = fixed_format_floating_point(vm->m_heap, imag_buf, sizeof(imag_buf), fraction, (scm_flonum_t)complex->imag);
                                                int pad = width - strlen(real_buf) - strlen(imag_buf) - real_n0 - imag_n0 + 2;
                                                if (imag_buf[0] != '-') pad = pad - 1;
                                                for (int i = 0; i < pad; i++) prt.byte(' ');
                                                prt.puts(real_buf);
                                                for (int i = 0; i < real_n0; i++) prt.byte('0');
                                                if (imag_buf[0] != '-') prt.byte('+');
                                                prt.puts(imag_buf);
                                                for (int i = 0; i < imag_n0; i++) prt.byte('0');
                                                prt.byte('i');
                                                ok = true;
                                                break;
                                            } else {
                                                assert(FLONUMP(obj));
                                                char buf[512];
                                                int n0 = fixed_format_floating_point(vm->m_heap, buf, sizeof(buf), fraction, (scm_flonum_t)obj);
                                                int pad = width - strlen(buf) - n0;
                                                for (int i = 0; i < pad; i++) prt.byte(' ');
                                                prt.puts(buf);
                                                for (int i = 0; i < n0; i++) prt.byte('0');
                                                ok = true;
                                                break;
                                            }
                                        }
                                    }
                                    break;
                                }
                                if (strchr("fF", c)) {
                                    scm_string_t string;
                                    if (STRINGP(argv[argp])) {
                                        string = (scm_string_t)argv[argp++];
                                        ok = true;
                                    }
                                    if (number_pred(argv[argp])) {
                                        string = (scm_string_t)cnvt_number_to_string(vm->m_heap, argv[argp++], 10);
                                        ok = true;
                                    }
                                    if (ok) {
                                        int pad = width - strlen(string->name);
                                        for (int i = 0; i < pad; i++) prt.byte(' ');
                                        prt.format("~a", string);
                                    }
                                }
                                break;
                            }
                            if (ok) continue;
                            invalid_argument_violation(vm, "format", "wrong directive in control string", fmt, -1, argc, argv);
                            return scm_undef;
                        }
                        if (strchr("fF", c)) {
                            if (STRINGP(argv[argp]) || number_pred(argv[argp])) {
                                scm_obj_t obj = argv[argp++];
                                prt.format("~a", obj);
                                continue;
                            }
                            wrong_type_argument_violation(vm, "format", argp, "number or string", argv[argp], argc, argv);
                            return scm_undef;
                        }
                        if (strchr("kK?", c)) {
                            if (STRINGP(argv[argp])) {
                                scm_string_t fmt = (scm_string_t)argv[argp++];
                                if (argp < argc) {
                                    if (listp(argv[argp])) {
                                        scm_obj_t param = argv[argp++];
                                        int count = list_length(param);
                                        scm_obj_t* args = (scm_obj_t*)alloca(sizeof(scm_obj_t) * (count + 2));
                                        args[0] = dst;
                                        args[1] = fmt;
                                        for (int n = 0; n < count; n++) {
                                            args[n + 2] = CAR(param);
                                            param = CDR(param);
                                        }
                                        scm_obj_t ans = subr_format(vm, count + 2, args);
                                        if (ans == scm_undef) return scm_undef;
                                        continue;
                                    }
                                    wrong_type_argument_violation(vm, "format", argp, "proper list", argv[argp], argc, argv);
                                    return scm_undef;
                                }
                                invalid_argument_violation(vm, "format", "too few arguments for control string", fmt, -1, argc, argv);
                                return scm_undef;
                            }
                            wrong_type_argument_violation(vm, "format", argp, "string", argv[argp], argc, argv);
                            return scm_undef;
                        }
                        if (c == '/' || c == '\\') {
                            if (STRINGP(argv[argp])) {
                                prt.format(tilded, argv[argp++]);
                                continue;
                            }
                            wrong_type_argument_violation(vm, "format", argp, "string", argv[argp], argc, argv);
                            return scm_undef;
                        }
                        if (strchr("yY", c)) {
                            vm->call_scheme(vm->lookup_system_closure(".@pretty-print"), 2, argv[argp], dst);
                            argp++;
                            continue;
                        }
                        if (strchr("cC", c)) {
                            if (CHARP(argv[argp])) {
                                prt.format(tilded, argv[argp++]);
                                continue;
                            }
                            wrong_type_argument_violation(vm, "format", argp, "char", argv[argp], argc, argv);
                            return scm_undef;
                        }
                        if (strchr("dD", c)) {
                            if (number_pred(argv[argp])) {
                                prt.format(tilded, argv[argp++]);
                                continue;
                            }
                            wrong_type_argument_violation(vm, "format", argp, "number", argv[argp], argc, argv);
                            return scm_undef;
                        }
                        if (strchr("xobXOB", c)) {
                            if (number_pred(argv[argp]) && n_exact_pred(argv[argp])) {
                                prt.format(tilded, argv[argp++]);
                                continue;
                            }
                            wrong_type_argument_violation(vm, "format", argp, "exact number", argv[argp], argc, argv);
                            return scm_undef;
                        }
                        if (strchr("asumnrwASUMNRW", c)) {
                            prt.format(tilded, argv[argp++]);
                            continue;
                        }
                        invalid_argument_violation(vm, "format", "wrong directive in control string", fmt, -1, argc, argv);
                        return scm_undef;
                    }
                    invalid_argument_violation(vm, "format", "too few arguments for control string", fmt, -1, argc, argv);
                    return scm_undef;
                } else {
                    prt.byte(c);
                }
            }
            if (argp != argc) {
                invalid_argument_violation(vm, "format", "too many arguments for control string", fmt, -1, argc, argv);
                return scm_undef;
            }
            if (output_string) {
                return port_extract_string(vm->m_heap, dst);
            }
            if (port == dst) {
                if (flush_output) port_flush_output(dst);
                return scm_unspecified;
            } else {
                scoped_lock lock2(port->lock);
                port_put_string(port, port_extract_string(vm->m_heap, dst));
                if (flush_output) port_flush_output(port);
                return scm_unspecified;
            }
        }
        wrong_type_argument_violation(vm, "format", argp, "string", argv[argp], argc, argv);
        return scm_undef;
    } catch (io_exception_t& e) {
        raise_io_error(vm, "format", e.m_operation, e.m_message, e.m_err, port, scm_false);
        return scm_undef;
    } catch (io_codec_exception_t& e) {
        raise_io_codec_error(vm, "format", e.m_operation, e.m_message, port, e.m_ch);
        return scm_undef;
    }
}

// top-level-bound? (no option environment)
scm_obj_t
subr_top_level_bound_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (SYMBOLP(argv[0])) {
            scm_symbol_t symbol = (scm_symbol_t)argv[0];
            scm_obj_t obj = vm->lookup_current_environment(symbol);
            return (obj == scm_undef) ? scm_false : scm_true;
        }
        wrong_type_argument_violation(vm, "top-level-bound?", 0, "symbol", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "top-level-bound?", 1, 1, argc, argv);
    return scm_undef;
}

// top-level-value (no option environment)
scm_obj_t
subr_top_level_value(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (SYMBOLP(argv[0])) {
            scm_symbol_t symbol = (scm_symbol_t)argv[0];
            scm_obj_t obj = vm->lookup_current_environment(symbol);
            if (obj != scm_undef) return obj;
            invalid_object_violation(vm, "top-level-value", "bound symbol", argv[0], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "top-level-value", 0, "symbol", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "top-level-value", 1, 1, argc, argv);
    return scm_undef;
}

// set-top-level-value! (no option environment)
scm_obj_t
subr_set_top_level_value(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (SYMBOLP(argv[0])) {
            scm_symbol_t symbol = (scm_symbol_t)argv[0];
            vm->intern_current_environment(symbol, argv[1]);
            return scm_unspecified;
        }
        wrong_type_argument_violation(vm, "set-top-level-value!", 0, "symbol", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "set-top-level-value!", 2, 2, argc, argv);
    return scm_undef;
}

// run-vmi
scm_obj_t
subr_run_vmi(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        vm->m_env = NULL;
        vm->m_pc = argv[0];
        vm->prebind(CDR(vm->m_pc));
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "run-vmi", 1, 1, argc, argv);
    return scm_undef;
}

// weak-mapping?
scm_obj_t
subr_weakmapping_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) return WEAKMAPPINGP(argv[0]) ? scm_true : scm_false;
    wrong_number_of_arguments_violation(vm, "weak-mapping?", 1, 1, argc, argv);
    return scm_undef;
}

// make-weak-mapping
scm_obj_t
subr_make_weakmapping(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) return make_weakmapping(vm->m_heap, argv[0], argv[1]);
    wrong_number_of_arguments_violation(vm, "make-weak-mapping", 2, 2, argc, argv);
    return scm_undef;
}

// weak-mapping-key
scm_obj_t
subr_weakmapping_key(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (WEAKMAPPINGP(argv[0])) return ((scm_weakmapping_t)argv[0])->key;
        wrong_type_argument_violation(vm, "weak-mapping-key", 0, "weak-mapping", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "weak-mapping-key", 1, 1, argc, argv);
    return scm_undef;
}

// weak-mapping-value
scm_obj_t
subr_weakmapping_value(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (WEAKMAPPINGP(argv[0])) return ((scm_weakmapping_t)argv[0])->value;
        wrong_type_argument_violation(vm, "weak-mapping-value", 0, "weak-mapping", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "weak-mapping-value", 1, 1, argc, argv);
    return scm_undef;
}

// current-source-comments
scm_obj_t
subr_current_source_comments(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        vm->m_current_source_comments = argv[0];
        return scm_unspecified;
    }
    if (argc == 0) return vm->m_current_source_comments;
    wrong_number_of_arguments_violation(vm, "current-source-comments", 0, 1, argc, argv);
    return scm_undef;
}

// current-library-infix
scm_obj_t
subr_current_library_infix(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) return MAKECHAR(IDENTIFIER_LIBRARY_INFIX);
    wrong_number_of_arguments_violation(vm, "current-library-infix", 0, 0, argc, argv);
    return scm_undef;
}

// current-library-suffix
scm_obj_t
subr_current_library_suffix(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) return MAKECHAR(IDENTIFIER_LIBRARY_SUFFIX);
    wrong_number_of_arguments_violation(vm, "current-library-suffix", 0, 0, argc, argv);
    return scm_undef;
}

// current-primitive-prefix
scm_obj_t
subr_current_primitive_prefix(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) return MAKECHAR(IDENTIFIER_PRIMITIVE_PREFIX);
    wrong_number_of_arguments_violation(vm, "current-primitive-prefix", 0, 0, argc, argv);
    return scm_undef;
}

// current-rename-delimiter
scm_obj_t
subr_current_rename_delimiter(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) return MAKECHAR(IDENTIFIER_RENAME_DELIMITER);
    wrong_number_of_arguments_violation(vm, "current-rename-delimiter", 0, 0, argc, argv);
    return scm_undef;
}

// backtrace
scm_obj_t
subr_backtrace(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (argv[0] == scm_false || argv[0] == scm_true || (FIXNUMP(argv[0]) && FIXNUM(argv[0]) >= 0)) {
            vm->flags.m_backtrace = argv[0];
            return scm_unspecified;
        } else {
            wrong_type_argument_violation(vm, "backtrace", 0, "#t, #f, or non-negative fixnum", argv[0], argc, argv);
            return scm_undef;
        }
    }
    if (argc == 0) return vm->flags.m_backtrace;
    wrong_number_of_arguments_violation(vm, "backtrace", 0, 1, argc, argv);
    return scm_undef;
}

// warning-level
scm_obj_t
subr_warning_level(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (argv[0] == scm_false || argv[0] == scm_true || (FIXNUMP(argv[0]) && FIXNUM(argv[0]) >= 0)) {
            vm->flags.m_warning_level = argv[0];
            return scm_unspecified;
        } else {
            wrong_type_argument_violation(vm, "warning-level", 0, "#t, #f, or non-negative fixnum", argv[0], argc, argv);
            return scm_undef;
        }
    }
    if (argc == 0) return vm->flags.m_warning_level;
    wrong_number_of_arguments_violation(vm, "warning-level", 0, 1, argc, argv);
    return scm_undef;
}

// extend-lexical-syntax
scm_obj_t
subr_extend_lexical_syntax(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (argv[0] == scm_false || argv[0] == scm_true) {
            vm->flags.m_extend_lexical_syntax = argv[0];
            return scm_unspecified;
        } else {
            wrong_type_argument_violation(vm, "extend-lexical-syntax", 0, "#t or #f", argv[0], argc, argv);
            return scm_undef;
        }
    }
    if (argc == 0) return vm->flags.m_extend_lexical_syntax;
    wrong_number_of_arguments_violation(vm, "extend-lexical-syntax", 0, 1, argc, argv);
    return scm_undef;
}

// mutable-literals
scm_obj_t
subr_mutable_literals(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (argv[0] == scm_false || argv[0] == scm_true) {
            vm->flags.m_mutable_literals = argv[0];
            return scm_unspecified;
        } else {
            wrong_type_argument_violation(vm, "mutable-literals", 0, "#t or #f", argv[0], argc, argv);
            return scm_undef;
        }
    }
    if (argc == 0) return vm->flags.m_mutable_literals;
    wrong_number_of_arguments_violation(vm, "mutable-literals", 0, 1, argc, argv);
    return scm_undef;
}

// display-backtrace
scm_obj_t
subr_display_backtrace(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) {
        if (vm->backtrace(vm->m_current_error)) return scm_true;
        return scm_false;
    }
    if (argc == 1) {
        if (PORTP(argv[0])) {
            scm_port_t port = (scm_port_t)argv[0];
            if (vm->backtrace(port)) return scm_true;
            return scm_false;
        }
        wrong_type_argument_violation(vm, "display-backtrace", 0, "port", argv[0], argc, argv);
    }
    wrong_number_of_arguments_violation(vm, "display-backtrace", 0, 1, argc, argv);
    return scm_undef;
}

// backtrace-line-length
scm_obj_t
subr_backtrace_line_length(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (FIXNUMP(argv[0]) && FIXNUM(argv[0]) >= 0) {
            vm->flags.m_backtrace_line_length = argv[0];
            return scm_unspecified;
        } else {
            wrong_type_argument_violation(vm, "backtrace-line-length", 0, "non-negative fixnum", argv[0], argc, argv);
            return scm_undef;
        }
    }
    if (argc == 0) return vm->flags.m_backtrace_line_length;
    wrong_number_of_arguments_violation(vm, "backtrace-line-length", 0, 1, argc, argv);
    return scm_undef;
}

// restricted-print-line-length
scm_obj_t
subr_restricted_print_line_length(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (FIXNUMP(argv[0]) && FIXNUM(argv[0]) >= 0) {
            vm->flags.m_restricted_print_line_length = argv[0];
            return scm_unspecified;
        } else {
            wrong_type_argument_violation(vm, "restricted-print-line-length", 0, "non-negative fixnum", argv[0], argc, argv);
            return scm_undef;
        }
    }
    if (argc == 0) return vm->flags.m_restricted_print_line_length;
    wrong_number_of_arguments_violation(vm, "restricted-print-line-length", 0, 1, argc, argv);
    return scm_undef;
}

// record-print-nesting-limit
scm_obj_t
subr_record_print_nesting_limit(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (FIXNUMP(argv[0]) && FIXNUM(argv[0]) >= 0) {
            vm->flags.m_record_print_nesting_limit = argv[0];
            return scm_unspecified;
        } else {
            wrong_type_argument_violation(vm, "record-print-nesting-limit", 0, "non-negative fixnum", argv[0], argc, argv);
            return scm_undef;
        }
    }
    if (argc == 0) return vm->flags.m_record_print_nesting_limit;
    wrong_number_of_arguments_violation(vm, "record-print-nesting-limit", 0, 1, argc, argv);
    return scm_undef;
}

// collect-notify
scm_obj_t
subr_collect_notify(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        vm->flags.m_collect_notify = (argv[0] != scm_false) ? scm_true : scm_false;
        return scm_unspecified;
    }
    if (argc == 0) return vm->flags.m_collect_notify;
    wrong_number_of_arguments_violation(vm, "collect-notify", 0, 1, argc, argv);
    return scm_undef;
}

// collect-stack-notify
scm_obj_t
subr_collect_stack_notify(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        vm->flags.m_collect_stack_notify = (argv[0] != scm_false) ? scm_true : scm_false;
        return scm_unspecified;
    }
    if (argc == 0) return vm->flags.m_collect_stack_notify;
    wrong_number_of_arguments_violation(vm, "collect-stack-notify", 0, 1, argc, argv);
    return scm_undef;
}

// collect
scm_obj_t
subr_collect(VM* vm, int argc, scm_obj_t argv[])
{
#if USE_PARALLEL_VM
    if (vm->m_child > 0) {
        if (argc == 0) {
            vm->m_heap->collect();
            return scm_unspecified;
        }
        wrong_number_of_arguments_violation(vm, "collect", 0, 0, argc, argv);
        return scm_undef;
    }
#endif
    bool pack = false;
    if (argc == 0 || argc == 1) {
        if (argc == 1) {
            if (BOOLP(argv[0])) {
                pack = (argv[0] == scm_true);
            } else {
                wrong_type_argument_violation(vm, "collect", 0, "#t or #f", argv[0], argc, argv);
                return scm_undef;
            }
        }
        do {
            vm->m_heap->collect();
            usleep(100);
        } while (!vm->m_heap->m_collector_kicked);
        do {
            if (vm->m_heap->m_stop_the_world) vm->stop();
            usleep(100);
        } while (vm->m_heap->m_collector_kicked);
        relocate_info_t* info = vm->m_heap->relocate(false);
        vm->resolve();
        vm->m_heap->resolve(info);
        vm->m_heap->relocate_privates(pack);
        if (pack) {
            info = vm->m_heap->relocate(true);
            vm->resolve();
            vm->m_heap->resolve(info);
            vm->m_heap->compact_pool();
        }
        return scm_unspecified;
    }
    wrong_number_of_arguments_violation(vm, "collect", 0, 1, argc, argv);
    return scm_undef;
}

// collect-trip-bytes
scm_obj_t
subr_collect_trip_bytes(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (FIXNUMP(argv[0])) {
            vm->m_heap->m_collect_trip_bytes = FIXNUM(argv[0]);
            return scm_unspecified;
        }
        wrong_type_argument_violation(vm, "collect-trip-bytes", 0, "non-negative fixnum", argv[0], argc, argv);
        return scm_undef;
    }
    if (argc == 0) return MAKEFIXNUM(vm->m_heap->m_collect_trip_bytes);
    wrong_number_of_arguments_violation(vm, "collect-trip-bytes", 0, 1, argc, argv);
    return scm_undef;
}

// display-heap-statistic
scm_obj_t
subr_display_heap_statistics(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) {
        vm->m_heap->display_heap_statistics(vm->m_current_output);
        return scm_unspecified;
    }
    wrong_number_of_arguments_violation(vm, "display-heap-statistic", 0, 0, argc, argv);
    return scm_undef;
}

// display-object-statistic
scm_obj_t
subr_display_object_statistics(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) {
        vm->m_heap->display_object_statistics(vm->m_current_output);
        return scm_unspecified;
    }
    wrong_number_of_arguments_violation(vm, "display-object-statistic", 0, 0, argc, argv);
    return scm_undef;
}

// closure-code
scm_obj_t
subr_closure_code(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (CLOSUREP(argv[0])) {
            scm_closure_t closure = (scm_closure_t)argv[0];
            return (scm_obj_t)closure->code;
        }
        wrong_type_argument_violation(vm, "closure-code", 0, "closure", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "closure-code", 1, 1, argc, argv);
    return scm_undef;
}

// closure-arity
scm_obj_t
subr_closure_arity(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (CLOSUREP(argv[0])) {
            scm_closure_t closure = (scm_closure_t)argv[0];
            scm_values_t obj = make_values(vm->m_heap, 2);
            int args = HDR_CLOSURE_ARGS(closure->hdr);
            int rest = 0;
            if (args < 0) {
                args = -args - 1;
                rest = 1;
            }
            obj->elts[0] = MAKEFIXNUM(args);
            obj->elts[1] = MAKEFIXNUM(rest);
            return obj;
        }
        scm_values_t obj = make_values(vm->m_heap, 2);
        obj->elts[0] = scm_false;
        obj->elts[1] = scm_false;
        return obj;
    }
    wrong_number_of_arguments_violation(vm, "closure-arity", 1, 1, argc, argv);
    return scm_undef;
}

// decode-flonum
scm_obj_t
subr_decode_flonum(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (FLONUMP(argv[0])) {
            return decode_flonum(vm->m_heap, (scm_flonum_t)argv[0]);
        }
        wrong_type_argument_violation(vm, "decode-flonum", 0, "flonum", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "decode-flonum", 1, 1, argc, argv);
    return scm_undef;
}

// current-environment
scm_obj_t
subr_current_environment(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (ENVIRONMENTP(argv[0])) {
            vm->m_current_environment = (scm_environment_t)argv[0];
            return scm_unspecified;
        }
        wrong_type_argument_violation(vm, "current-environment", 0, "top-level-environment", argv[0], argc, argv);
        return scm_undef;
    }
    if (argc == 0) return vm->m_current_environment;
    wrong_number_of_arguments_violation(vm, "current-environment", 0, 1, argc, argv);
    return scm_undef;
}

// current-macro-environment
scm_obj_t
subr_current_macro_environment(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (HASHTABLEP(argv[0])) {
            vm->m_current_environment->macro = (scm_hashtable_t)argv[0];
            return scm_unspecified;
        }
        wrong_type_argument_violation(vm, "current-macro-environment", 0, "hash-table", argv[0], argc, argv);
        return scm_undef;
    }
    if (argc == 0) return vm->m_current_environment->macro;
    wrong_number_of_arguments_violation(vm, "current-macro-environment", 0, 1, argc, argv);
    return scm_undef;
}

// current-variable-environment
scm_obj_t
subr_current_variable_environment(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (HASHTABLEP(argv[0])) {
            vm->m_current_environment->variable = (scm_hashtable_t)argv[0];
            return scm_unspecified;
        }
        wrong_type_argument_violation(vm, "current-variable-environment", 0, "hash-table", argv[0], argc, argv);
        return scm_undef;
    }
    if (argc == 0) return vm->m_current_environment->variable;
    wrong_number_of_arguments_violation(vm, "current-variable-environment", 0, 1, argc, argv);
    return scm_undef;
}

// current-dynamic-environment
scm_obj_t
subr_current_dynamic_environment(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (WEAKHASHTABLEP(argv[0])) {
            vm->m_current_dynamic_environment = (scm_weakhashtable_t)argv[0];
            return scm_unspecified;
        }
        wrong_type_argument_violation(vm, "current-dynamic-environment", 0, "weak-hash-table", argv[0], argc, argv);
        return scm_undef;
    }
    if (argc == 0) return vm->m_current_dynamic_environment;
    wrong_number_of_arguments_violation(vm, "current-dynamic-environment", 0, 1, argc, argv);
    return scm_undef;
}

// current-dynamic-wind-record
scm_obj_t
subr_current_dynamic_wind_record(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        vm->m_current_dynamic_wind_record = argv[0];
        return scm_unspecified;
    }
    if (argc == 0) return vm->m_current_dynamic_wind_record;
    wrong_number_of_arguments_violation(vm, "current-dynamic-wind-record", 0, 1, argc, argv);
    return scm_undef;
}

// current-exception-handler
scm_obj_t
subr_current_exception_handler(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        vm->m_current_exception_handler = argv[0];
        return scm_unspecified;
    }
    if (argc == 0) return vm->m_current_exception_handler;
    wrong_number_of_arguments_violation(vm, "current-exception-handler", 0, 1, argc, argv);
    return scm_undef;
}

// environment?
scm_obj_t
subr_environment_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) return ENVIRONMENTP(argv[0]) ? scm_true : scm_false;
    wrong_number_of_arguments_violation(vm, "environment?", 1, 1, argc, argv);
    return scm_undef;
}

// system-environment
scm_obj_t
subr_system_environment(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) return vm->m_heap->m_system_environment;
    wrong_number_of_arguments_violation(vm, "system-environment", 0, 0, argc, argv);
    return scm_undef;
}

// make-tuple
scm_obj_t
subr_make_tuple(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (FIXNUMP(argv[0]) && FIXNUM(argv[0]) >= 0) return make_tuple(vm->m_heap, FIXNUM(argv[0]), scm_unspecified);
        wrong_type_argument_violation(vm, "make-tuple", 0, "non-negative fixnum", argv[0], argc, argv);
        return scm_undef;
    }
    if (argc == 2) {
        if (FIXNUMP(argv[0]) && FIXNUM(argv[0]) >= 0) return make_tuple(vm->m_heap, FIXNUM(argv[0]), argv[1]);
        wrong_type_argument_violation(vm, "make-tuple", 0, "non-negative fixnum", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "make-tuple", 1, 2, argc, argv);
    return scm_undef;
}

// tuple->list
scm_obj_t
subr_tuple_list(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (TUPLEP(argv[0])) {
            scm_tuple_t tuple = (scm_tuple_t)argv[0];
            int n = HDR_TUPLE_COUNT(tuple->hdr);
            scm_obj_t lst = scm_nil;
            for (int i = n - 1; i >= 0 ; i--) lst = make_pair(vm->m_heap, tuple->elts[i], lst);
            return lst;
        }
        wrong_type_argument_violation(vm, "tuple->list", 0, "tuple", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "tuple->list", 1, 1, argc, argv);
    return scm_undef;
}

// exit
scm_obj_t
subr_exit(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) {
        throw vm_exit_t(EXIT_SUCCESS);
    }
    if (argc == 1) {
        if (PORTP(vm->m_current_output)) {
            scoped_lock lock(vm->m_current_output->lock);
            port_flush_output(vm->m_current_output);
        }
        if (argv[0] == scm_false) throw vm_exit_t(EXIT_FAILURE);
        if (FIXNUMP(argv[0])) throw vm_exit_t(FIXNUM(argv[0]));
        wrong_type_argument_violation(vm, "exit", 0, "fixnum", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "exit", 0, 1, argc, argv);
    return scm_undef;
}

// escape
scm_obj_t
subr_escape(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) throw vm_escape_t();
    wrong_number_of_arguments_violation(vm, "escape", 0, 0, argc, argv);
    return scm_undef;
}

// recursion-level
scm_obj_t
subr_recursion_level(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) return MAKEFIXNUM(vm->m_recursion_level);
    wrong_number_of_arguments_violation(vm, "recursion-level", 0, 0, argc, argv);
    return scm_undef;
}

// command-line
scm_obj_t
subr_command_line(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) {
        scm_obj_t lst = scm_nil;
        for (int i = main_command_line_argc - 1; i >= 0; i--) {
            lst = make_pair(vm->m_heap, make_string(vm->m_heap, main_command_line_argv[i]), lst);
        }
        return lst;
    }
    wrong_number_of_arguments_violation(vm, "command-line", 0, 0, argc, argv);
    return scm_undef;
}

// command-line-shift
scm_obj_t
subr_command_line_shift(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (FIXNUMP(argv[0]) && FIXNUM(argv[0]) >= 0) {
            int n = FIXNUM(argv[0]);
            if (n < main_command_line_argc) {
                main_command_line_argc -= n;
                main_command_line_argv += n;
            } else {
                main_command_line_argc = 0;
                main_command_line_argv = NULL;
            }
            return scm_unspecified;
        }
        wrong_type_argument_violation(vm, "command-line-shift", 0, "non-negative fixnum", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "command-line-shift", 1, 1, argc, argv);
    return scm_undef;
}

// tuple
scm_obj_t
subr_tuple(VM* vm, int argc, scm_obj_t argv[])
{
    scm_tuple_t tuple = make_tuple(vm->m_heap, argc, scm_unspecified);
    for (int i = 0; i < argc; i++) tuple->elts[i] = argv[i];
    return tuple;
}

// tuple?
scm_obj_t
subr_tuple_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) return TUPLEP(argv[0]) ? scm_true : scm_false;
    wrong_number_of_arguments_violation(vm, "tuple?", 1, 1, argc, argv);
    return scm_undef;
}

// tuple-ref
scm_obj_t
subr_tuple_ref(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (TUPLEP(argv[0])) {
            if (FIXNUMP(argv[1])) {
                scm_tuple_t tuple = (scm_tuple_t)argv[0];
                intptr_t n = FIXNUM(argv[1]);
                if (n >= 0 && n < HDR_TUPLE_COUNT(tuple->hdr)) return tuple->elts[n];
            }
        }
        return scm_false;
    }
    wrong_number_of_arguments_violation(vm, "tuple-ref", 2, 2, argc, argv);
    return scm_undef;
}

// tuple-set!
scm_obj_t
subr_tuple_set(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 3) {
        if (TUPLEP(argv[0])) {
            if (FIXNUMP(argv[1])) {
                scm_tuple_t tuple = (scm_tuple_t)argv[0];
                intptr_t n = FIXNUM(argv[1]);
                if (n >= 0 && n < HDR_TUPLE_COUNT(tuple->hdr)) {
#if USE_PARALLEL_VM
                    if (vm->m_interp->live_thread_count() > 1) {
                        if (!vm->m_heap->in_heap(tuple)) {
                            thread_object_access_violation(vm, "tuple-set!",argc, argv);
                            return scm_undef;
                        }
                        if (vm->m_child > 0) vm->m_interp->remember(tuple->elts[n], argv[2]);
                    }
#endif
                    vm->m_heap->write_barrier(argv[2]);
                    tuple->elts[n] = argv[2];
                    return scm_unspecified;
                }
                // FALL THROUGH
            }
            if (exact_non_negative_integer_pred(argv[1])) {
                invalid_argument_violation(vm, "tuple-set!", "index out of bounds,", argv[1], 1, argc, argv);
                return scm_undef;
            } else {
                wrong_type_argument_violation(vm, "tuple-set!", 1, "exact non-negative integer", argv[1], argc, argv);
                return scm_undef;
            }
        }
        wrong_type_argument_violation(vm, "tuple-set!", 0, "vector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "tuple-set!", 3, 3, argc, argv);
    return scm_undef;
}

// tuple-length
scm_obj_t
subr_tuple_length(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (TUPLEP(argv[0])) {
            scm_tuple_t tuple = (scm_tuple_t)argv[0];
            return MAKEFIXNUM(HDR_TUPLE_COUNT(tuple->hdr));
        }
        wrong_type_argument_violation(vm, "tuple-length", 0, "tuple", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "tuple-length", 1, 1, argc, argv);
    return scm_undef;
}

// tuple-index
scm_obj_t
subr_tuple_index(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc >= 2 && argc <= 4) {
        if (TUPLEP(argv[0])) {
            scm_tuple_t tuple = (scm_tuple_t)argv[0];
            int len = HDR_TUPLE_COUNT(tuple->hdr);
            int start = 0;
            int end = len;
            if (argc > 2) {
                if (FIXNUMP(argv[2]) && FIXNUM(argv[2]) >= 0) {
                    start = FIXNUM(argv[2]);
                } else {
                    wrong_type_argument_violation(vm, "tuple-index", 2, "exact non-negative integer", argv[2], argc, argv);
                    return scm_undef;
                }
            }
            if (argc > 3) {
                if (FIXNUMP(argv[3]) && FIXNUM(argv[3]) >= 0) {
                    end = FIXNUM(argv[3]);
                } else {
                    wrong_type_argument_violation(vm, "tuple-index", 3, "exact non-negative integer", argv[3], argc, argv);
                    return scm_undef;
                }
            }
            if (start <= end && end <= len) {
                for (int p = start; p < end; p++) {
                    if (tuple->elts[p] == argv[1]) return MAKEFIXNUM(p);
                }
                return scm_false;
            }
            if (end > len) {
                invalid_argument_violation(vm, "tuple-index", "index out of bounds,", argv[3], 3, argc, argv);
            } else if (start > len) {
                invalid_argument_violation(vm, "tuple-index", "index out of bounds,", argv[2], 2, argc, argv);
            } else if (start > end) {
                invalid_argument_violation(vm, "tuple-index", "indices must be start <= end", NULL, -1, argc, argv);
            }
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "tuple-index", 0, "tuple", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "tuple-index", 2, 4, argc, argv);
    return scm_undef;
}

// string-contains (return byte index)
scm_obj_t
subr_string_contains(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc >= 2 && argc <= 6) {
        if (STRINGP(argv[0])) {
            if (STRINGP(argv[1]) || CHARP(argv[1])) {
                const char* s1 = ((scm_string_t)argv[0])->name;
                const char* s2;
                int s1_size = ((scm_string_t)argv[0])->size;
                int s2_size;
                uint8_t utf8[8];
                if (STRINGP(argv[1])) {
                    s2 = ((scm_string_t)argv[1])->name;
                    s2_size = ((scm_string_t)argv[1])->size;
                } else {
                    s2 = (char*)utf8;
                    s2_size = cnvt_ucs4_to_utf8(CHAR(argv[1]), utf8);
                    utf8[s2_size] = 0;
                }
                int start1 = 0;
                int end1 = s1_size;
                int start2 = 0;
                int end2 = s2_size;
                int base = 0;
                if (argc > 2) {
                    if (FIXNUMP(argv[2])) {
                        base = FIXNUM(argv[2]);
                        start1 = utf8_char_index_to_byte_offset((uint8_t*)s1, base, s1_size + 1);
                        if (start1 < 0 || start1 > s1_size) {
                            invalid_argument_violation(vm, "string-contains", "index out of bounds,", argv[2], 2, argc, argv);
                            return scm_undef;
                        }
                    } else {
                        if (exact_non_negative_integer_pred(argv[2])) {
                            invalid_argument_violation(vm, "string-contains", "index out of bounds,", argv[2], 2, argc, argv);
                        } else {
                            wrong_type_argument_violation(vm, "string-contains", 2, "exact non-negative integer", argv[2], argc, argv);
                        }
                        return scm_undef;
                    }
                    if (argc > 3) {
                        if (FIXNUMP(argv[3])) {
                            end1 = utf8_char_index_to_byte_offset((uint8_t*)s1, FIXNUM(argv[3]), s1_size + 1);
                            if (end1 < 0 || end1 > s1_size) {
                                invalid_argument_violation(vm, "string-contains", "index out of bounds,", argv[3], 3, argc, argv);
                                return scm_undef;
                            }
                        } else {
                            if (exact_non_negative_integer_pred(argv[3])) {
                                invalid_argument_violation(vm, "string-contains", "index out of bounds,", argv[3], 3, argc, argv);
                            } else {
                                wrong_type_argument_violation(vm, "string-contains", 3, "exact non-negative integer", argv[3], argc, argv);
                            }
                            return scm_undef;
                        }
                        if (argc > 4) {
                            if (FIXNUMP(argv[4])) {
                                start2 = utf8_char_index_to_byte_offset((uint8_t*)s2, FIXNUM(argv[4]), s2_size + 1);
                                if (start2 < 0 || start2 > s2_size) {
                                    invalid_argument_violation(vm, "string-contains", "index out of bounds,", argv[4], 4, argc, argv);
                                    return scm_undef;
                                }
                            } else {
                                if (exact_non_negative_integer_pred(argv[4])) {
                                    invalid_argument_violation(vm, "string-contains", "index out of bounds,", argv[4], 4, argc, argv);
                                } else {
                                    wrong_type_argument_violation(vm, "string-contains", 4, "exact non-negative integer", argv[4], argc, argv);
                                }
                                return scm_undef;
                            }
                            if (argc > 5) {
                                if (FIXNUMP(argv[5])) {
                                    end2 = utf8_char_index_to_byte_offset((uint8_t*)s2, FIXNUM(argv[5]), s2_size + 1);
                                    if (end2 < 0 || end2 > s2_size) {
                                        invalid_argument_violation(vm, "string-contains", "index out of bounds,", argv[5], 5, argc, argv);
                                        return scm_undef;
                                    }
                                } else {
                                    if (exact_non_negative_integer_pred(argv[5])) {
                                        invalid_argument_violation(vm, "string-contains", "index out of bounds,", argv[5], 5, argc, argv);
                                    } else {
                                        wrong_type_argument_violation(vm, "string-contains", 5, "exact non-negative integer", argv[5], argc, argv);
                                    }
                                    return scm_undef;
                                }
                            }
                        }
                    }
                }
                if (start1 > end1) {
                    invalid_argument_violation(vm, "string-contains", "indices must be start1 <= end1", NULL, -1, argc, argv);
                    return scm_undef;
                }
                if (start2 > end2) {
                    invalid_argument_violation(vm, "string-contains", "indices must be start2 <= end2", NULL, -1, argc, argv);
                    return scm_undef;
                }
                assert(start1 <= end1 && start2 <= end2 && end1 <= s1_size && end2 <= s2_size);
                int span = end2 - start2;
                int to = end1 - span;
                int p = start1;
                while (p <= to) {
                    int i = 0;
                    while (i < span && s1[p + i] == s2[start2 + i]) i++;
                    if (i == span) return MAKEFIXNUM(base);
                    p = p + utf8_byte_count(s1[p]);
                    base = base + 1;
                }
                return scm_false;
            }
            wrong_type_argument_violation(vm, "string-contains", 1, "string or char", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "string-contains", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "string-contains", 2, 6, argc, argv);
    return scm_undef;
}

// symbol-contains
scm_obj_t
subr_symbol_contains(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (SYMBOLP(argv[0])) {
            if (STRINGP(argv[1]) || CHARP(argv[1])) {
                const char* s1 = ((scm_symbol_t)argv[0])->name;
                const char* s2;
                int s1_size = HDR_SYMBOL_SIZE(((scm_symbol_t)argv[0])->hdr);
                int s2_size;
                uint8_t utf8[8];
                if (STRINGP(argv[1])) {
                    s2 = ((scm_string_t)argv[1])->name;
                    s2_size = ((scm_string_t)argv[1])->size;
                } else {
                    s2 = (char*)utf8;
                    s2_size = cnvt_ucs4_to_utf8(CHAR(argv[1]), utf8);
                    utf8[s2_size] = 0;
                }
                int start1 = 0;
                int end1 = s1_size;
                int start2 = 0;
                int end2 = s2_size;
                int span = end2 - start2;
                int to = end1 - span;
                int base = 0;
                int p = start1;
                while (p <= to) {
                    int i = 0;
                    while (i < span && s1[p + i] == s2[start2 + i]) i++;
                    if (i == span) return MAKEFIXNUM(base);
                    p = p + utf8_byte_count(s1[p]);
                    base = base + 1;
                }
                return scm_false;
            }
            wrong_type_argument_violation(vm, "symbol-contains", 1, "string or char", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "symbol-contains", 0, "symbol", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "symbol-contains", 2, 2, argc, argv);
    return scm_undef;
}

// write-with-shared-structure (srfi-38)
scm_obj_t
subr_write_with_shared_structure(VM* vm, int argc, scm_obj_t argv[])
{
    scm_port_t port;
    if (argc == 1) {
        port = vm->m_current_output;
    } else if (argc == 2) {
        if (PORTP(argv[1])) {
            port = (scm_port_t)argv[1];
        } else {
            wrong_type_argument_violation(vm, "write-with-shared-structure", 1, "port", argv[1], argc, argv);
            return scm_undef;
        }
    } else {
        wrong_number_of_arguments_violation(vm, "write-with-shared-structure", 1, 2, argc, argv);
        return scm_undef;
    }
    scoped_lock lock(port->lock);
    if (port_output_pred(port)) {
        if (port_open_pred(port)) {
            printer_t(vm, port).format("~w", argv[0]);
            return scm_unspecified;
        }
        if (argc > 1) wrong_type_argument_violation(vm, "write-with-shared-structure", 1, "opened port", argv[1], argc, argv);
        else invalid_object_violation(vm, "write-with-shared-structure", "opened port", port, argc, argv);
        return scm_undef;
    }
    if (argc > 1) wrong_type_argument_violation(vm, "write-with-shared-structure", 1, "output port", argv[1], argc, argv);
    else invalid_object_violation(vm, "write-with-shared-structure", "output port", port, argc, argv);
    return scm_undef;
}

// read-with-shared-structure (srfi-38)
scm_obj_t
subr_read_with_shared_structure(VM* vm, int argc, scm_obj_t argv[])
{
    scm_port_t port;
    if (argc == 0) {
        port = vm->m_current_input;
    } else if (argc == 1) {
        if (PORTP(argv[0])) {
            port = (scm_port_t)argv[0];
        } else {
            wrong_type_argument_violation(vm, "read-with-shared-structure", 0, "port", argv[0], argc, argv);
            return scm_undef;
        }
    } else {
        wrong_number_of_arguments_violation(vm, "read-with-shared-structure", 0, 1, argc, argv);
        return scm_undef;
    }
    scoped_lock lock(port->lock);
    if (port_input_pred(port)) {
        if (port_open_pred(port)) {
            return reader_t(vm, port).read_graph(NULL);
        }
        if (argc > 0) wrong_type_argument_violation(vm, "read-with-shared-structure", 0, "opened port", argv[0], argc, argv);
        else invalid_object_violation(vm, "read-with-shared-structure", "opened port", port, argc, argv);
        return scm_undef;
    }
    if (argc > 0) wrong_type_argument_violation(vm, "read-with-shared-structure", 0, "input port", argv[0], argc, argv);
    else invalid_object_violation(vm, "read-with-shared-structure", "input port", port, argc, argv);
    return scm_undef;
}

// gensym
scm_obj_t
subr_gensym(VM* vm, int argc, scm_obj_t argv[])
{
    const char *prefix;
    if (argc < 2) {
        if (argc == 0) {
            prefix = DEFAULT_GENSYM_PREFIX;
        } else {
            if (STRINGP(argv[0])) {
                scm_string_t string = (scm_string_t)argv[0];
                prefix = string->name;
            } else {
                wrong_type_argument_violation(vm, "gensym", 0, "string", argv[0], argc, argv);
                return scm_undef;
            }
        }
        int count;
        {
            scoped_lock lock(vm->m_heap->m_gensym_lock);
            count = vm->m_heap->m_gensym_counter++;
        }
        char head[MAX_READ_SYMBOL_LENGTH];
        snprintf(head, sizeof(head), "%s%d", prefix, count);
        char buf[MAX_READ_SYMBOL_LENGTH];
        struct timeval tv;
        gettimeofday(&tv, NULL);
        snprintf(buf, sizeof(buf), "%s.%x.%x",head, (unsigned int)tv.tv_sec, (unsigned int)tv.tv_usec);
        return make_symbol_uninterned(vm->m_heap, buf, strlen(buf), strlen(head));
    }
    wrong_number_of_arguments_violation(vm, "gensym", 0, 1, argc, argv);
    return scm_undef;
}

// subr?
scm_obj_t
subr_subr_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) return SUBRP(argv[0]) ? scm_true : scm_false;
    wrong_number_of_arguments_violation(vm, "subr?", 1, 1, argc, argv);
    return scm_undef;
}

// usleep
scm_obj_t
subr_usleep(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (FIXNUMP(argv[0])) {
            usleep(FIXNUM(argv[0]));
            return scm_unspecified;
        }
        wrong_type_argument_violation(vm, "usleep", 0, "fixnum", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "usleep", 1, 1, argc, argv);
    return scm_undef;
}

// copy-environment-variables!
scm_obj_t
subr_copy_environment_variables(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 3) {
        if (ENVIRONMENTP(argv[0])) {
            if (ENVIRONMENTP(argv[1])) {
                scm_environment_t from = (scm_environment_t)argv[0];
                scm_environment_t to = (scm_environment_t)argv[1];
#if USE_PARALLEL_VM
                if (vm->m_interp->live_thread_count() > 1) {
                    if (!vm->m_heap->in_heap(to)) {
                        thread_object_access_violation(vm, "copy-environment-variables!" ,argc, argv);
                        return scm_undef;
                    }
                }
#endif
                scoped_lock lock(to->variable->lock);
                scm_obj_t lst = argv[2];
                while (PAIRP(lst)) {
                    scm_symbol_t from_symbol;
                    scm_symbol_t to_symbol;
                    if (SYMBOLP(CAR(lst))) {
                        from_symbol = to_symbol = (scm_symbol_t)CAR(lst);
                    } else if (PAIRP(CAR(lst)) && SYMBOLP(CAAR(lst)) && SYMBOLP(CDAR(lst))) {
                        from_symbol = (scm_symbol_t)CAAR(lst);
                        to_symbol = (scm_symbol_t)CDAR(lst);
                    } else {
                        wrong_type_argument_violation(vm, "copy-environment-variables!", 2, "list of identifiers", argv[2], argc, argv);
                        return scm_undef;
                    }
                    scm_obj_t obj;
                    if (from == to) {
                        obj = get_hashtable(from->variable, from_symbol);
                    } else {
                        scoped_lock lock(from->variable->lock);
                        obj = get_hashtable(from->variable, from_symbol);
                    }
                    if (GLOCP(obj)) {
                        scm_gloc_t from_gloc = (scm_gloc_t)obj;
                        scm_gloc_t to_gloc = make_gloc(vm->m_heap, to_symbol);
                        to_gloc->value = from_gloc->value;
#if USE_PARALLEL_VM
                        if (vm->m_interp->live_thread_count() > 1 && vm->m_child > 0) {
                            vm->m_interp->remember(get_hashtable(to->variable, to_symbol), to_gloc);
                        }
#endif
                        vm->m_heap->write_barrier(to_symbol);
                        vm->m_heap->write_barrier(to_gloc);
                        int nsize = put_hashtable(to->variable, to_symbol, to_gloc);
                        if (nsize) rehash_hashtable(vm->m_heap, to->variable, nsize);
                        lst = CDR(lst);
                        continue;
                    }
                    invalid_object_violation(vm, "copy-environment-variables!", "bound symbol", from_symbol, argc, argv);
                    return scm_undef;
                }
                if (lst == scm_nil) return scm_unspecified;
                wrong_type_argument_violation(vm, "copy-environment-variables!", 2, "list of identifiers", argv[2], argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "copy-environment-variables!", 1, "top-level-environment", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "copy-environment-variables!", 0, "top-level-environment", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "copy-environment-variables!", 3, 3, argc, argv);
    return scm_undef;
}

// copy-environment-macros!
scm_obj_t
subr_copy_environment_macros(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 3) {
        if (ENVIRONMENTP(argv[0])) {
            if (ENVIRONMENTP(argv[1])) {
                scm_environment_t from = (scm_environment_t)argv[0];
                scm_environment_t to = (scm_environment_t)argv[1];
#if USE_PARALLEL_VM
                if (vm->m_interp->live_thread_count() > 1) {
                    if (!vm->m_heap->in_heap(to)) {
                        thread_object_access_violation(vm, "copy-environment-macros!" ,argc, argv);
                        return scm_undef;
                    }
                }
#endif
                scoped_lock lock(to->macro->lock);
                scm_obj_t lst = argv[2];
                while (PAIRP(lst)) {
                    scm_symbol_t from_symbol;
                    scm_symbol_t to_symbol;
                    if (SYMBOLP(CAR(lst))) {
                        from_symbol = to_symbol = (scm_symbol_t)CAR(lst);
                    } else if (PAIRP(CAR(lst)) && SYMBOLP(CAAR(lst)) && SYMBOLP(CDAR(lst))) {
                        from_symbol = (scm_symbol_t)CAAR(lst);
                        to_symbol = (scm_symbol_t)CDAR(lst);
                    } else {
                        wrong_type_argument_violation(vm, "copy-environment-macros!", 2, "list of identifiers", argv[2], argc, argv);
                        return scm_undef;
                    }
                    scm_obj_t obj;
                    if (from == to) {
                        obj = get_hashtable(from->macro, from_symbol);
                    } else {
                        scoped_lock lock(from->macro->lock);
                        obj = get_hashtable(from->macro, from_symbol);
                    }
                    if (obj != scm_undef) {
#if USE_PARALLEL_VM
                        if (vm->m_interp->live_thread_count() > 1 && vm->m_child > 0) {
                            vm->m_interp->remember(get_hashtable(to->macro, to_symbol), obj);
                        }
#endif
                        vm->m_heap->write_barrier(to_symbol);
                        vm->m_heap->write_barrier(obj);
                        int nsize = put_hashtable(to->macro, to_symbol, obj);
                        if (nsize) rehash_hashtable(vm->m_heap, to->macro, nsize);
                        lst = CDR(lst);
                        continue;
                    }
                    invalid_object_violation(vm, "copy-environment-macros!", "bound symbol", from_symbol, argc, argv);
                    return scm_undef;
                }
                if (lst == scm_nil) return scm_unspecified;
                wrong_type_argument_violation(vm, "copy-environment-macros!", 2, "list of identifiers", argv[2], argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "copy-environment-macros!", 1, "top-level-environment", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "copy-environment-macros!", 0, "top-level-environment", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "copy-environment-macros!", 3, 3, argc, argv);
    return scm_undef;
}

// make-environment
scm_obj_t
subr_make_environment(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) {
        return make_environment(vm->m_heap, NULL);
    }
    if (argc == 1) {
        if (STRINGP(argv[0])) {
            scm_string_t string = (scm_string_t)argv[0];
            return make_environment(vm->m_heap, string->name);
        }
        wrong_type_argument_violation(vm, "make-environment", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "make-environment", 0, 1, argc, argv);
    return scm_undef;
}

// architecture-feature
scm_obj_t
subr_architecture_feature(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) return vm->m_heap->m_architecture_feature;
    if (argc == 1 || argc == 2) {
        if (SYMBOLP(argv[0])) {
            scoped_lock lock(vm->m_heap->m_architecture_feature->lock);
            scm_obj_t obj = get_hashtable(vm->m_heap->m_architecture_feature, argv[0]);
            if (obj == scm_undef) {
                if (argc == 2) return argv[1];
                invalid_argument_violation(vm, "architecture-feature", "undefined keyword,", argv[0], 0, argc, argv);
                return scm_undef;
            }
            return obj;
        }
        wrong_type_argument_violation(vm, "architecture-feature", 0, "symbol", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "architecture-feature", 0, 2, argc, argv);
    return scm_undef;
}

// gethostname
scm_obj_t
subr_gethostname(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) {
        char host[HOST_NAME_MAX];
        if (gethostname(host, sizeof(host))) {
            raise_error(vm, "gethostname", strerror(errno), errno);
            return scm_undef;
        }
        return make_string_literal(vm->m_heap, host);
    }
    wrong_number_of_arguments_violation(vm, "gethostname", 0, 0, argc, argv);
    return scm_undef;
}

// string->uninterned-symbol
scm_obj_t
subr_string_uninterned_symbol(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1 || argc == 2) {
        if (STRINGP(argv[0])) {
            scm_string_t string = (scm_string_t)argv[0];
            if (argc == 1) return make_symbol_uninterned(vm->m_heap, string->name, string->size);
            if (argc == 2) {
                if (FIXNUMP(argv[1])) {
                    int offset = utf8_char_index_to_byte_offset((uint8_t*)string->name, FIXNUM(argv[1]), string->size + 1);
                    return make_symbol_uninterned(vm->m_heap, string->name, string->size, offset);
                }
                wrong_type_argument_violation(vm, "string->uninterned-symbol", 1, "string", argv[1], argc, argv);
                return scm_undef;
            }
        }
        wrong_type_argument_violation(vm, "string->uninterned-symbol", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "string->uninterned-symbol", 1, 2, argc, argv);
    return scm_undef;
}

// uninterned-symbol?
scm_obj_t
subr_uninterned_symbol_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) return UNINTERNEDSYMBOLP(argv[0]) ? scm_true : scm_false;
    wrong_number_of_arguments_violation(vm, "uninterned-symbol?", 1, 1, argc, argv);
    return scm_undef;
}

// uninterned-symbol-prefix
scm_obj_t
subr_uninterned_symbol_prefix(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (UNINTERNEDSYMBOLP(argv[0])) {
            scm_symbol_t symbol = (scm_symbol_t)argv[0];
            int len = HDR_SYMBOL_SIZE(symbol->hdr);
            return make_string_literal(vm->m_heap, symbol->name, (uint8_t)symbol->name[len + 1]);
        }
        wrong_type_argument_violation(vm, "uninterned-symbol-prefix", 0, "uninterned symbol", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "uninterned-symbol-prefix", 1, 1, argc, argv);
    return scm_undef;
}

// uninterned-symbol-suffix
scm_obj_t
subr_uninterned_symbol_suffix(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (UNINTERNEDSYMBOLP(argv[0])) {
            scm_symbol_t symbol = (scm_symbol_t)argv[0];
            int len = HDR_SYMBOL_SIZE(symbol->hdr);
            int offset = (uint8_t)symbol->name[len + 1];
            return make_string_literal(vm->m_heap, symbol->name + offset , len - offset);
        }
        wrong_type_argument_violation(vm, "uninterned-symbol-suffix", 0, "uninterned symbol", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "uninterned-symbol-suffix", 1, 1, argc, argv);
    return scm_undef;
}

// make-uuid
scm_obj_t
subr_make_uuid(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) {
        char buf[64];
#if USE_PARALLEL_VM
        vm->m_interp->generate_uuid(buf, sizeof(buf));
#else
        uuid_v4(buf, sizeof(buf));
#endif
        return make_string(vm->m_heap, buf);
    }
    wrong_number_of_arguments_violation(vm, "make-uuid", 0, 0, argc, argv);
    return scm_undef;
}

// microsecond
scm_obj_t
subr_microsecond(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) {
        struct timeval tv;
        gettimeofday(&tv, NULL);
        return int64_to_integer(vm->m_heap, (int64_t)tv.tv_sec * 1000000 + tv.tv_usec);
    }
    wrong_number_of_arguments_violation(vm, "microsecond", 0, 0, argc, argv);
    return scm_undef;
}

// microsecond->utc
scm_obj_t
subr_microsecond_utc(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1 || argc == 2) {
        if (exact_non_negative_integer_pred(argv[0])) {
            int64_t usec;
            int64_t offset = 0;
            if (argc == 2) {
                if (exact_integer_pred(argv[1])) {
                    exact_integer_to_int64(argv[1], &offset);
                } else {
                    wrong_type_argument_violation(vm, "microsecond->utc", 1, "exact integer", argv[1], argc, argv);
                    return scm_undef;
                }
            }
            exact_integer_to_int64(argv[0], &usec);
            time_t sec = usec / 1000000;
            struct tm date;
            gmtime_r(&sec, &date);
            int64_t utc = usec + offset + (int64_t)(mktime(&date) - sec) * 1000000;
            return int64_to_integer(vm->m_heap, utc);
        }
        wrong_type_argument_violation(vm, "microsecond->utc", 0, "exact non-negative integer", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "microsecond->utc", 1, 2, argc, argv);
    return scm_undef;
}

// microsecond->string
scm_obj_t
subr_microsecond_string(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1 || argc == 2) {
        if (exact_non_negative_integer_pred(argv[0])) {
            int64_t usec;
            exact_integer_to_int64(argv[0], &usec);
            const char* fmt = "%c";
            if (argc == 2) {
                if (STRINGP(argv[1])) {
                    fmt = ((scm_string_t)argv[1])->name;
                } else {
                    wrong_type_argument_violation(vm, "microsecond->string", 1, "string", argv[1], argc, argv);
                    return scm_undef;
                }
            }
            time_t sec = usec / 1000000;
            struct tm date;
            localtime_r(&sec, &date);
            char* buf = NULL;
            int buflen = 0;
            int n;
            do {
                buflen += 64;
                buf = (char*)realloc(buf, buflen);
                buf[0] = '*';
                buf[1] = 0;
                n = strftime(buf, buflen, fmt, &date);
            } while (strlen(buf) != n);
            scm_obj_t obj = make_string(vm->m_heap, buf);
            free(buf);
            return obj;
        }
        wrong_type_argument_violation(vm, "microsecond->string", 0, "exact non-negative integer", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "microsecond->string", 1, 2, argc, argv);
    return scm_undef;
}

/*
// string->microsecond
scm_obj_t
subr_string_microsecond(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1 || argc == 2) {
        if (STRINGP(argv[0])) {
            scm_string_t string = (scm_string_t)argv[0];
            const char* fmt = "%c";
            if (argc == 2) {
                if (STRINGP(argv[1])) {
                    fmt = ((scm_string_t)argv[1])->name;
                } else {
                    wrong_type_argument_violation(vm, "string->microsecond", 1, "string", argv[1], argc, argv);
                    return scm_undef;
                }
            }
            struct tm date;
            time_t sec = time(NULL);
            localtime_r(&sec, &date);
            if (strptime(string->name, fmt, &date)) {
                int64_t usec = (int64_t)mktime(&date) * 1000000;
                if (usec < 0) return scm_false;
                return int64_to_integer(vm->m_heap, usec);
            }
            return scm_false;
        }
        wrong_type_argument_violation(vm, "string->microsecond", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "string->microsecond", 1, 2, argc, argv);
    return scm_undef;
}
*/

// decode-microsecond
scm_obj_t
subr_decode_microsecond(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (exact_non_negative_integer_pred(argv[0])) {
            int64_t usec;
            exact_integer_to_int64(argv[0], &usec);
            time_t sec = usec / 1000000;
            struct tm date;
            localtime_r(&sec, &date);
            return make_list(vm->m_heap, 9,
                             MAKEFIXNUM(date.tm_sec), MAKEFIXNUM(date.tm_min), MAKEFIXNUM(date.tm_hour),
                             MAKEFIXNUM(date.tm_mday), MAKEFIXNUM(date.tm_mon), MAKEFIXNUM(date.tm_year),
                             MAKEFIXNUM(date.tm_wday), MAKEFIXNUM(date.tm_yday), MAKEFIXNUM(date.tm_isdst));
        }
        wrong_type_argument_violation(vm, "decode-microsecond", 0, "exact non-negative integer", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "decode-microsecond", 1, 1, argc, argv);
    return scm_undef;
}

// encode-microsecond
scm_obj_t
subr_encode_microsecond(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 9) {
        for (int i = 0; i < 8; i++) {
            if (exact_non_negative_integer_pred(argv[i])) continue;
            wrong_type_argument_violation(vm, "encode-microsecond", i, "exact non-negative integer", argv[i], argc, argv);
            return scm_undef;
        }
        if (!exact_integer_pred(argv[8])) {
            wrong_type_argument_violation(vm, "encode-microsecond", 8, "exact integer", argv[8], argc, argv);
            return scm_undef;
        }
        struct tm date;
        date.tm_sec = FIXNUM(argv[0]);
        date.tm_min = FIXNUM(argv[1]);
        date.tm_hour = FIXNUM(argv[2]);
        date.tm_mday = FIXNUM(argv[3]);
        date.tm_mon = FIXNUM(argv[4]);
        date.tm_year = FIXNUM(argv[5]);
        date.tm_wday = FIXNUM(argv[6]);
        date.tm_yday = FIXNUM(argv[7]);
        date.tm_isdst = FIXNUM(argv[8]);
        int64_t sec = mktime(&date);
        if (sec < 0) return scm_false;
        return int64_to_integer(vm->m_heap, sec * 1000000);
    }
    wrong_number_of_arguments_violation(vm, "encode-microsecond", 9, 9, argc, argv);
    return scm_undef;
}

// time-usage
scm_obj_t
subr_time_usage(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) {
#if _MSC_VER
        FILETIME real_time;
        FILETIME creation_time;
        FILETIME exit_time;
        FILETIME kernel_time;
        FILETIME user_time;
        GetSystemTimeAsFileTime(&real_time);
        if (GetProcessTimes(GetCurrentProcess(), &creation_time, &exit_time, &kernel_time, &user_time)) {
            return make_list(vm->m_heap, 3,
                             make_flonum(vm->m_heap, ((double)real_time.dwLowDateTime
                                                       + (double)real_time.dwHighDateTime
                                                       * ((double)UINT32_MAX + 1.0)) / 10000000.0),
                             make_flonum(vm->m_heap, ((double)user_time.dwLowDateTime
                                                       + (double)user_time.dwHighDateTime
                                                       * ((double)UINT32_MAX + 1.0)) / 10000000.0),
                             make_flonum(vm->m_heap, ((double)kernel_time.dwLowDateTime
                                                       + (double)kernel_time.dwHighDateTime
                                                       * ((double)UINT32_MAX + 1.0)) / 10000000.0));
        }
        return scm_false;
#else
        struct timeval tv;
        struct rusage ru;
        gettimeofday(&tv, NULL);
        getrusage(RUSAGE_SELF, &ru);
        return make_list(vm->m_heap, 3,
                         make_flonum(vm->m_heap, (double)tv.tv_sec + tv.tv_usec / 1000000.0),
                         make_flonum(vm->m_heap, (double)ru.ru_utime.tv_sec + ru.ru_utime.tv_usec / 1000000.0),
                         make_flonum(vm->m_heap, (double)ru.ru_stime.tv_sec + ru.ru_stime.tv_usec / 1000000.0));
#endif
    }
    wrong_number_of_arguments_violation(vm, "times", 0, 0, argc, argv);
    return scm_undef;
}

// cyclic-object?
scm_obj_t
subr_cyclic_object_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) return cyclic_objectp(vm->m_heap, argv[0]) ? scm_true : scm_false;
    wrong_number_of_arguments_violation(vm, "cyclic-object?", 1, 1, argc, argv);
    return scm_undef;
}

// vector-copy
scm_obj_t
subr_vector_copy(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc >= 1 && argc <= 3) {
        if (VECTORP(argv[0])) {
            scm_vector_t src = (scm_vector_t)argv[0];
            int len = src->count;
            int end = len;
            int start = 0;
            if (argc >= 2) {
                if (FIXNUMP(argv[1])) {
                    start = FIXNUM(argv[1]);
                    if (start < 0) {
                        wrong_type_argument_violation(vm, "vector-copy", 1, "exact non-negative integer", argv[1], argc, argv);
                        return scm_undef;
                    }
                } else {
                    if (exact_non_negative_integer_pred(argv[1])) {
                        invalid_argument_violation(vm, "vector-copy", "index out of bounds,", argv[1], 1, argc, argv);
                        return scm_undef;
                    } else {
                        wrong_type_argument_violation(vm, "vector-copy", 1, "exact non-negative integer", argv[1], argc, argv);
                        return scm_undef;
                    }
                }
                if (argc == 3) {
                    if (FIXNUMP(argv[2])) {
                        end = FIXNUM(argv[2]);
                        if (end < 0) {
                            wrong_type_argument_violation(vm, "vector-copy", 2, "exact non-negative integer", argv[2], argc, argv);
                            return scm_undef;
                        }
                    } else {
                        if (exact_non_negative_integer_pred(argv[2])) {
                            invalid_argument_violation(vm, "vector-copy", "index out of bounds,", argv[2], 2, argc, argv);
                            return scm_undef;
                        } else {
                            wrong_type_argument_violation(vm, "vector-copy", 2, "exact non-negative integer", argv[2], argc, argv);
                            return scm_undef;
                        }
                    }
                }
            }
            if (start <= len && end <= len && start <= end) {
                scm_vector_t dst = make_vector(vm->m_heap, end - start, scm_unspecified);
                for (int i = start; i < end; i++) {
                    vm->m_heap->write_barrier(src->elts[i]);
                    dst->elts[i - start] = src->elts[i];
                }
                return dst;
            }
            if (start > end) {
                invalid_argument_violation(vm, "vector-copy", "indices must be start <= end", NULL, -1, argc, argv);
            } else if (start > len) {
                invalid_argument_violation(vm, "vector-copy", "index out of bounds,", argv[1], 1, argc, argv);
            } else if (end > len) {
                invalid_argument_violation(vm, "vector-copy", "index out of bounds,", argv[2], 2, argc, argv);
            }
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "vector-copy", 0, "vector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "vector-copy", 1, 3, argc, argv);
    return scm_undef;
}

// Reference:
// Seeds for Random Number Generators, George Marsaglia - Communications of the ACM, Volume 46, Issue 5, May 2003
// Reference:
// Xorshift RNGs, George Marsaglia - Jurnal of Statistical Software, Vol. 8, Issue 14, Jul 2003

#define CMWC_R  1359
static const uint32_t CMWC_A = 3636507990U;
static const uint32_t CMWC_B = 0xffffffffU;

typedef struct {
    uint32_t    i;
    uint32_t    c;
    uint32_t    q[CMWC_R];
} cmwc_status_t;

static void
cmwc_status_init(cmwc_status_t* status, uint64_t seed)
{
    uint32_t x = 123456789;
    uint32_t y = 362436069;
    uint32_t z = 521288629;
    uint32_t w = seed;
    uint32_t t;
    for (int i = 0; i < CMWC_R; i++) {
        t = (x^(x<<11));
        x = y;
        y = z;
        z = w;
        w = (w^(w>>19))^(t^(t>>8));
        status->q[i] = w;
    }
    status->i = 0;
    status->c = (seed >> 32) % CMWC_A;
}

static uint32_t
cmwc_random(cmwc_status_t* status)
{
    status->i = status->i + 1;
    if (status->i >= CMWC_R) status->i = 0;
    uint64_t t = (uint64_t)CMWC_A * status->q[status->i] + status->c;
    status->c = (uint32_t)(t >> 32);
    uint32_t x = CMWC_B - ((uint32_t)t & CMWC_B);
    status->q[status->i] = x;
    return x;
}

// make-cmwc-random-state
scm_obj_t
subr_make_cmwc_random_state(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (exact_integer_pred(argv[0])) {
            int64_t seed = coerce_exact_integer_to_int64(argv[0]);
            scm_bvector_t bvect = make_bvector(vm->m_heap, sizeof(cmwc_status_t));
            cmwc_status_t* status = (cmwc_status_t*)bvect->elts;
            cmwc_status_init(status, seed);
            return bvect;
        }
        wrong_type_argument_violation(vm, "make-cmwc-random-state", 0, "exact integer", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "make-cmwc-random-state", 1, 1, argc, argv);
    return scm_undef;
}

// cmwc-random-u32
scm_obj_t
subr_cmwc_random_u32(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (BVECTORP(argv[0])) {
            scm_bvector_t bvect = (scm_bvector_t)argv[0];
            if (bvect->count == sizeof(cmwc_status_t)) {
                cmwc_status_t* status = (cmwc_status_t*)bvect->elts;
                return uint32_to_integer(vm->m_heap, cmwc_random(status));
            }
            invalid_argument_violation(vm, "cmwc-random-u32", "invalid bytevector for random state,", argv[0], 0, argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "cmwc-random-u32", 0, "bytevector", argv[0], argc, argv);
        return scm_undef;
    }
    if (argc == 2) {
        if (BVECTORP(argv[0])) {
            if (exact_positive_integer_pred(argv[1])) {
                scm_bvector_t bvect = (scm_bvector_t)argv[0];
                if (bvect->count == sizeof(cmwc_status_t)) {
                    cmwc_status_t* status = (cmwc_status_t*)bvect->elts;
                    uint32_t u32;
                    if (exact_integer_to_uint32(argv[1], &u32)) {
                        uint32_t q = UINT32_MAX / u32;
                        uint32_t limit = q * u32;
                        uint32_t temp;
                        do { temp = cmwc_random(status); } while (temp >= limit);
                        return uint32_to_integer(vm->m_heap, temp / q);
                    }
                    uint64_t u64;
                    if (exact_integer_to_uint64(argv[1], &u64)) {
                        if (u64 == 4294967296LL) return uint32_to_integer(vm->m_heap, cmwc_random(status));
                    }
                    wrong_type_argument_violation(vm, "cmwc-random-u32", 1, "positive exact integer less than or equal to 2^32", argv[1], argc, argv);
                    return scm_undef;
                }
                invalid_argument_violation(vm, "cmwc-random-u32", "invalid bytevector for random state,", argv[0], 0, argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "cmwc-random-u32", 1, "non-negative exact integer", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "cmwc-random-u32", 0, "bytevector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "cmwc-random-u32", 1, 2, argc, argv);
    return scm_undef;
}

// cmwc-random-real
scm_obj_t
subr_cmwc_random_real(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (BVECTORP(argv[0])) {
            scm_bvector_t bvect = (scm_bvector_t)argv[0];
            if (bvect->count == sizeof(cmwc_status_t)) {
                cmwc_status_t* status = (cmwc_status_t*)bvect->elts;
                double hi = cmwc_random(status) >> 5;
                double lo = cmwc_random(status) >> 6;
                return make_flonum(vm->m_heap, (hi * 67108864.0 + lo + 1.0) * 1.1102230246251564e-16);
            }
            invalid_argument_violation(vm, "cmwc-random-real", "invalid bytevector for random state,", argv[0], 0, argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "cmwc-random-real", 0, "bytevector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "random-real", 1, 1, argc, argv);
    return scm_undef;
}

#if USE_NATIVE_CODE

static uint8_t* s_pool;
static uint8_t* s_pool_limit;

static void vmi_init_pool()
{
    int pool_alloc_size = getpagesize() * 32;
    s_pool = (uint8_t*)valloc(pool_alloc_size);
    if (mprotect(s_pool, pool_alloc_size, PROT_READ | PROT_WRITE | PROT_EXEC)) {
        fatal("%s:%u mprotect failed %d", __FILE__, __LINE__, errno);
    }
    s_pool_limit = s_pool + pool_alloc_size;
}

// vmi-native-code-address
scm_obj_t
subr_vmi_native_code_address(VM* vm, int argc, scm_obj_t argv[])
{
    if (s_pool == NULL) vmi_init_pool();
    if (argc == 0) return uintptr_to_integer(vm->m_heap, (uintptr_t)s_pool);
    wrong_number_of_arguments_violation(vm, "vmi-native-code-address", 0, 0, argc, argv);
    return scm_undef;
}

// vmi-set-native-code!
scm_obj_t
subr_vmi_set_native_code(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (CLOSUREP(argv[0])) {
            scm_closure_t closure = (scm_closure_t)argv[0];
            if (BVECTORP(argv[1])) {
                assert(VMINSTP(s_pool));
                scm_bvector_t bvect = (scm_bvector_t)argv[1];
                int bsize = bvect->count;
                if (s_pool + bsize > s_pool_limit) fatal("%s:%u native code pool overflow", __FILE__, __LINE__);
                memcpy(s_pool, bvect->elts, bsize);
                uint8_t* code = s_pool;
                s_pool += bsize;
                s_pool = (uint8_t*)(((uintptr_t)s_pool + 7) & (~7));
                closure->code = make_pair(vm->m_heap, make_pair(vm->m_heap, code, scm_nil), scm_nil);
                return scm_unspecified;
            }
            wrong_type_argument_violation(vm, "vmi-set-native-code!", 1, "bytevector", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "vmi-set-native-code!", 0, "closure", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "vmi-set-native-code!", 2, 2, argc, argv);
    return scm_undef;
}

// vmi-lookup-gloc-address
scm_obj_t
subr_vmi_lookup_gloc_address(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (SYMBOLP(argv[0])) {
            scm_symbol_t symbol = (scm_symbol_t)argv[0];
            scoped_lock lock(vm->m_current_environment->variable->lock);
            scm_obj_t obj = get_hashtable(vm->m_current_environment->variable, symbol);
            if (GLOCP(obj)) {
                scm_gloc_t gloc = (scm_gloc_t)obj;
                return uintptr_to_integer(vm->m_heap, (uintptr_t)&gloc->value);
            }
        }
        wrong_type_argument_violation(vm, "vmi-lookup-gloc-address", 0, "variable symbol", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "vmi-lookup-gloc-address", 1, 1, argc, argv);
    return scm_undef;
}

// vmi-lookup-subr-address
scm_obj_t
subr_vmi_lookup_subr_address(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (SUBRP(argv[0])) {
            scm_subr_t subr = (scm_subr_t)argv[0];
            return uintptr_to_integer(vm->m_heap, (uintptr_t)subr->adrs);
        }
        wrong_type_argument_violation(vm, "vmi-lookup-subr-address", 0, "subr", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "vmi-lookup-subr-address", 1, 1, argc, argv);
    return scm_undef;
}

// vmi-get-object-bits
scm_obj_t subr_vmi_get_object_bits(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) return uintptr_to_integer(vm->m_heap, (uintptr_t)argv[0]);
    wrong_number_of_arguments_violation(vm, "vmi-get-object-bits", 1, 1, argc, argv);
    return scm_undef;
}

extern "C" scm_obj_t vmi_stub_allocate_cons(VM* vm)
{
    return vm->m_heap->allocate_cons();
}

extern "C" void vmi_stub_collect_stack(VM* vm, int n)
{
    vm->collect_stack(n);
}

// vmi-allocate-cons-address
scm_obj_t
subr_vmi_allocate_cons_address(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) return uintptr_to_integer(vm->m_heap, (uintptr_t)&vmi_stub_allocate_cons);
    wrong_number_of_arguments_violation(vm, "vmi-allocate-cons-address", 0, 0, argc, argv);
    return scm_undef;
}

// vmi-collect-stack-address
scm_obj_t
subr_vmi_collect_stack_address(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) return uintptr_to_integer(vm->m_heap, (uintptr_t)&vmi_stub_collect_stack);
    wrong_number_of_arguments_violation(vm, "vmi-collect-stack-address", 0, 0, argc, argv);
    return scm_undef;
}

// vmi-return-loop-address
scm_obj_t
subr_vmi_return_loop_address(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) return uintptr_to_integer(vm->m_heap, (uintptr_t)VM::s_return_loop);
    wrong_number_of_arguments_violation(vm, "vmi-return-loop-address", 0, 0, argc, argv);
    return scm_undef;
}

// vmi-return-apply-address
scm_obj_t
subr_vmi_return_apply_address(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) return uintptr_to_integer(vm->m_heap, (uintptr_t)VM::s_return_apply);
    wrong_number_of_arguments_violation(vm, "vmi-return-apply-address", 0, 0, argc, argv);
    return scm_undef;
}

// vmi-return-pop-cont-address
scm_obj_t
subr_vmi_return_pop_cont_address(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) return uintptr_to_integer(vm->m_heap, (uintptr_t)VM::s_return_pop_cont);
    wrong_number_of_arguments_violation(vm, "vmi-return-pop-cont-address", 0, 0, argc, argv);
    return scm_undef;
}
#endif

void
init_subr_others(object_heap_t* heap)
{
#define DEFSUBR(SYM, FUNC)  heap->intern_system_subr(SYM, FUNC)

    DEFSUBR("run-vmi", subr_run_vmi);
    DEFSUBR("core-read", subr_core_read);
    DEFSUBR("closure-code", subr_closure_code);
    DEFSUBR("closure-arity", subr_closure_arity);
    DEFSUBR("decode-flonum", subr_decode_flonum);
    DEFSUBR("top-level-bound?", subr_top_level_bound_pred);
    DEFSUBR("top-level-value", subr_top_level_value);
    DEFSUBR("set-top-level-value!", subr_set_top_level_value);
    DEFSUBR("recursion-level", subr_recursion_level);
    DEFSUBR("escape", subr_escape);
    DEFSUBR("architecture-feature", subr_architecture_feature);
    DEFSUBR("backtrace-line-length", subr_backtrace_line_length);
    DEFSUBR("display-backtrace", subr_display_backtrace);
    DEFSUBR("collect", subr_collect);
    DEFSUBR("collect-notify", subr_collect_notify);
    DEFSUBR("collect-trip-bytes", subr_collect_trip_bytes);
    DEFSUBR("collect-stack-notify", subr_collect_stack_notify);
    DEFSUBR("display-object-statistics", subr_display_object_statistics);
    DEFSUBR("display-heap-statistics", subr_display_heap_statistics);
    DEFSUBR("backtrace", subr_backtrace);
    DEFSUBR("warning-level", subr_warning_level);
    DEFSUBR("mutable-literals", subr_mutable_literals);
    DEFSUBR("extend-lexical-syntax", subr_extend_lexical_syntax);
    DEFSUBR("record-print-nesting-limit", subr_record_print_nesting_limit);
    DEFSUBR("restricted-print-line-length", subr_restricted_print_line_length);
    DEFSUBR("current-environment", subr_current_environment);
    DEFSUBR("current-macro-environment", subr_current_macro_environment);
    DEFSUBR("current-variable-environment", subr_current_variable_environment);
    DEFSUBR("current-exception-handler", subr_current_exception_handler);
    DEFSUBR("current-dynamic-environment", subr_current_dynamic_environment);
    DEFSUBR("current-dynamic-wind-record", subr_current_dynamic_wind_record);
    DEFSUBR("current-source-comments", subr_current_source_comments);
    DEFSUBR("current-library-infix", subr_current_library_infix);
    DEFSUBR("current-library-suffix", subr_current_library_suffix);
    DEFSUBR("current-primitive-prefix", subr_current_primitive_prefix);
    DEFSUBR("current-rename-delimiter", subr_current_rename_delimiter);
    DEFSUBR("uninterned-symbol?", subr_uninterned_symbol_pred);
    DEFSUBR("uninterned-symbol-prefix", subr_uninterned_symbol_prefix);
    DEFSUBR("uninterned-symbol-suffix", subr_uninterned_symbol_suffix);
    DEFSUBR("string->uninterned-symbol", subr_string_uninterned_symbol);
    DEFSUBR("make-environment", subr_make_environment);
    DEFSUBR("environment?", subr_environment_pred);
    DEFSUBR("system-environment", subr_system_environment);
    DEFSUBR("interaction-environment", subr_interaction_environment);
    DEFSUBR("copy-environment-variables!", subr_copy_environment_variables);
    DEFSUBR("copy-environment-macros!", subr_copy_environment_macros);
    DEFSUBR("make-tuple", subr_make_tuple);
    DEFSUBR("tuple", subr_tuple);
    DEFSUBR("tuple?", subr_tuple_pred);
    DEFSUBR("tuple-ref", subr_tuple_ref);
    DEFSUBR("tuple-set!", subr_tuple_set);
    DEFSUBR("tuple-length", subr_tuple_length);
    DEFSUBR("tuple-index", subr_tuple_index);
    DEFSUBR("tuple->list", subr_tuple_list);
    DEFSUBR("make-weak-mapping", subr_make_weakmapping);
    DEFSUBR("weak-mapping?", subr_weakmapping_pred);
    DEFSUBR("weak-mapping-key", subr_weakmapping_key);
    DEFSUBR("weak-mapping-value", subr_weakmapping_value);
    DEFSUBR("make-uuid", subr_make_uuid);
    DEFSUBR("microsecond", subr_microsecond);
    DEFSUBR("microsecond->utc", subr_microsecond_utc);
    DEFSUBR("microsecond->string", subr_microsecond_string);
    DEFSUBR("decode-microsecond", subr_decode_microsecond);
    DEFSUBR("encode-microsecond", subr_encode_microsecond);
    DEFSUBR("string-contains", subr_string_contains);
    DEFSUBR("symbol-contains", subr_symbol_contains);
    DEFSUBR("vector-copy", subr_vector_copy);
    DEFSUBR("cyclic-object?", subr_cyclic_object_pred);
    DEFSUBR("gethostname", subr_gethostname);
    DEFSUBR("time-usage", subr_time_usage);
    DEFSUBR("subr?", subr_subr_pred);
    DEFSUBR("exit", subr_exit);
    DEFSUBR("usleep", subr_usleep);
    DEFSUBR("gensym", subr_gensym);
    DEFSUBR("format", subr_format);
    DEFSUBR("write-with-shared-structure", subr_write_with_shared_structure);
    DEFSUBR("read-with-shared-structure", subr_read_with_shared_structure);
    DEFSUBR("command-line", subr_command_line);
    DEFSUBR("command-line-shift", subr_command_line_shift);
    DEFSUBR("make-cmwc-random-state", subr_make_cmwc_random_state);
    DEFSUBR("cmwc-random-u32", subr_cmwc_random_u32);
    DEFSUBR("cmwc-random-real", subr_cmwc_random_real);
#if USE_NATIVE_CODE
    DEFSUBR("vmi-native-code-address", subr_vmi_native_code_address);
    DEFSUBR("vmi-set-native-code!", subr_vmi_set_native_code);
    DEFSUBR("vmi-return-loop-address", subr_vmi_return_loop_address);
    DEFSUBR("vmi-return-apply-address", subr_vmi_return_apply_address);
    DEFSUBR("vmi-return-pop-cont-address", subr_vmi_return_pop_cont_address);
    DEFSUBR("vmi-lookup-gloc-address", subr_vmi_lookup_gloc_address);
    DEFSUBR("vmi-lookup-subr-address", subr_vmi_lookup_subr_address);
    DEFSUBR("vmi-allocate-cons-address", subr_vmi_allocate_cons_address);
    DEFSUBR("vmi-collect-stack-address", subr_vmi_collect_stack_address);
    DEFSUBR("vmi-get-object-bits", subr_vmi_get_object_bits);
#endif
}
