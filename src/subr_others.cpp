/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/
#include "core.h"
#include "vm.h"
#include "file.h"
#include "fasl.h"
#include "heap.h"
#include "port.h"
#include "subr.h"
#include "ucs4.h"
#include "utf8.h"
#include "arith.h"
#include "reader.h"
#include "ioerror.h"
#include "printer.h"
#include "violation.h"

#define	DEFAULT_GENSYM_PREFIX	".G"
 
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
			raise_lexical_violation(vm, who, exception.m_message);
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
 
// format
scm_obj_t
subr_format(VM* vm, int argc, scm_obj_t argv[])
{
	if (argc == 0) {
		wrong_number_of_arguments_violation(vm, "format", 1, -1, argc, argv);
		return scm_undef;
	}
	
	scm_port_t port = (scm_port_t)scm_unspecified;
	int	argp = 0;
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
                    tilded[1] = c;
                    if (c == '!') {
                        flush_output = true;
                        continue;
                    }
                    if (c == '&') {
                        if (port->column != 1) prt.byte('\n');
                        continue;
                    }
                    if (strchr("%~t_", c)) {
                        prt.format(tilded);
                        continue;
                    }
                    if (argp < argc) {
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
                            wrong_type_argument_violation(vm, "format", argp, "exact", argv[argp], argc, argv);
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
            scm_values_t values = make_values(vm->m_heap, 3);
            values->elts[0] = make_flonum(vm->m_heap, ((double)real_time.dwLowDateTime + (double)real_time.dwHighDateTime * (double)UINT32_MAX) / 10000000.0);
            values->elts[1] = make_flonum(vm->m_heap, ((double)user_time.dwLowDateTime + (double)user_time.dwHighDateTime * (double)UINT32_MAX) / 10000000.0);
            values->elts[2] = make_flonum(vm->m_heap, ((double)kernel_time.dwLowDateTime + (double)kernel_time.dwHighDateTime * (double)UINT32_MAX) / 10000000.0);
            return values;
        }
        return scm_false;
#else
		struct timeval tv;
        struct rusage ru;
		gettimeofday(&tv, NULL);
        getrusage(RUSAGE_SELF, &ru);
        scm_values_t values = make_values(vm->m_heap, 3);
        values->elts[0] = make_flonum(vm->m_heap, (double)tv.tv_sec + tv.tv_usec / 1000000.0);
        values->elts[1] = make_flonum(vm->m_heap, (double)ru.ru_utime.tv_sec + ru.ru_utime.tv_usec / 1000000.0);
        values->elts[2] = make_flonum(vm->m_heap, (double)ru.ru_stime.tv_sec + ru.ru_stime.tv_usec / 1000000.0);
        return values;
#endif
	}
	wrong_number_of_arguments_violation(vm, "times", 0, 0, argc, argv);
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
		wrong_type_argument_violation(vm, "closure-arity", 0, "closure", argv[0], argc, argv);
		return scm_undef;
	}
	wrong_number_of_arguments_violation(vm, "closure-arity", 1, 1, argc, argv);
	return scm_undef;
}

// decode-float
scm_obj_t
subr_decode_float(VM* vm, int argc, scm_obj_t argv[])
{
	if (argc == 1) {
		if (FLONUMP(argv[0])) {
			return decode_flonum(vm->m_heap, (scm_flonum_t)argv[0]);
		}
		wrong_type_argument_violation(vm, "decode-float", 0, "flonum", argv[0], argc, argv);
		return scm_undef;
	}
	wrong_number_of_arguments_violation(vm, "decode-float", 1, 1, argc, argv);
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
				int n = FIXNUM(argv[1]);
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
				int n = FIXNUM(argv[1]);
				if (n >= 0 && n < HDR_TUPLE_COUNT(tuple->hdr)) {
					vm->m_heap->write_barrier(argv[2]);
					tuple->elts[FIXNUM(argv[1])] = argv[2];
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

// string-contains (srfi-13)
scm_obj_t
subr_string_contains(VM* vm, int argc, scm_obj_t argv[])
{
	if (argc >= 2 && argc <= 6) {
		if (STRINGP(argv[0])) {
			if (STRINGP(argv[1])) {
				const char* s1 = ((scm_string_t)argv[0])->name;
				const char* s2 = ((scm_string_t)argv[1])->name;
				int s1_size = HDR_STRING_SIZE(((scm_string_t)argv[0])->hdr);
				int s2_size = HDR_STRING_SIZE(((scm_string_t)argv[1])->hdr);
				int start1 = 0;
				int end1 = s1_size;
				int start2 = 0;
				int end2 = s2_size;
				if (argc > 2) {
					if (FIXNUMP(argv[2])) {
						start1 = FIXNUM(argv[2]);
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
				}
				if (argc > 3) {
					if (FIXNUMP(argv[3])) {
						end1 = FIXNUM(argv[3]);
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
				}
				if (argc > 4) {
					if (FIXNUMP(argv[4])) {
						start2 = FIXNUM(argv[4]);
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
				}
				if (argc > 5) {
					if (FIXNUMP(argv[5])) {
						end2 = FIXNUM(argv[5]);
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
				for (int p = start1; p <= to; p++) {
					int i = 0;
					while (i < span && s1[p+i] == s2[start2+i]) i++;
					if (i == span) return MAKEFIXNUM(p);
				}
				return scm_false;
			}
			wrong_type_argument_violation(vm, "string-contains", 1, "string", argv[1], argc, argv);
			return scm_undef;
		}
		wrong_type_argument_violation(vm, "string-contains", 0, "string", argv[0], argc, argv);
		return scm_undef;
	}
	wrong_number_of_arguments_violation(vm, "string-contains", 2, 6, argc, argv);
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

// gensym
scm_obj_t
subr_gensym(VM* vm, int argc, scm_obj_t argv[])
{
	const char *prefix;
	char buf[MAX_READ_SYMBOL_LENGTH];
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
        snprintf(buf, sizeof(buf), "%s%d", prefix, count);        
		return make_symbol(vm->m_heap, buf);
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

// file-exists?
scm_obj_t
subr_file_exists_pred(VM* vm, int argc, scm_obj_t argv[])
{
	if (argc == 1) {
		if (STRINGP(argv[0])) {
			scm_string_t string = (scm_string_t)argv[0];
            return file_exists(vm, string);
		}
		wrong_type_argument_violation(vm, "file-exists?", 0, "string", argv[0], argc, argv);
		return scm_undef;
	}
	wrong_number_of_arguments_violation(vm, "file-exists?", 1, 1, argc, argv);
	return scm_undef;
}

// delete-file
scm_obj_t
subr_delete_file(VM* vm, int argc, scm_obj_t argv[])
{
	if (argc == 1) {
		if (STRINGP(argv[0])) {
			scm_string_t string = (scm_string_t)argv[0];
            return delete_file(vm, string);
		}
		wrong_type_argument_violation(vm, "delete-file", 0, "string", argv[0], argc, argv);
		return scm_undef;
	}
	wrong_number_of_arguments_violation(vm, "delete-file", 1, 1, argc, argv);
	return scm_undef;
}

// stat-mtime
scm_obj_t
subr_stat_mtime(VM* vm, int argc, scm_obj_t argv[])
{
	if (argc == 1) {
		if (STRINGP(argv[0])) {
			scm_string_t string = (scm_string_t)argv[0];
            return stat_mtime(vm, string);
		}
		wrong_type_argument_violation(vm, "stat-mtime", 0, "string", argv[0], argc, argv);
		return scm_undef;
	}
	wrong_number_of_arguments_violation(vm, "stat-mtime", 1, 1, argc, argv);
	return scm_undef;
}

// directory-list
scm_obj_t
subr_directory_list(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (STRINGP(argv[0])) {
            scm_string_t string = (scm_string_t)argv[0];
            return directory_list(vm, string);
        }
        wrong_type_argument_violation(vm, "directory-list", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "directory-list", 1, 1, argc, argv);
    return scm_undef;
}

// current-directory
scm_obj_t
subr_current_directory(VM* vm, int argc, scm_obj_t argv[])
{
	if (argc == 1) {
		if (STRINGP(argv[0])) {
			return set_current_directory(vm, (scm_string_t)argv[0]);
		}
		wrong_type_argument_violation(vm, "current-directory", 0, "string", argv[0], argc, argv);
		return scm_undef;
	}
	if (argc == 0) return current_directory(vm);
	wrong_number_of_arguments_violation(vm, "current-directory", 0, 1, argc, argv);
	return scm_undef;
}

// create-directory
scm_obj_t
subr_create_directory(VM* vm, int argc, scm_obj_t argv[])
{
	if (argc == 1) {
		if (STRINGP(argv[0])) {
			return create_directory(vm, (scm_string_t)argv[0]);
		}
		wrong_type_argument_violation(vm, "create-directory", 0, "string", argv[0], argc, argv);
		return scm_undef;
	}
	wrong_number_of_arguments_violation(vm, "create-directory", 1, 1, argc, argv);
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
						scm_gloc_t to_gloc = make_gloc(vm->m_heap, to, to_symbol);
						to_gloc->value = from_gloc->value;
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

// system-share-path
scm_obj_t
subr_system_share_path(VM* vm, int argc, scm_obj_t argv[])
{
	if (argc == 0) {
		return make_string_literal(vm->m_heap, SYSTEM_SHARE_PATH);
	}
	wrong_number_of_arguments_violation(vm, "system-share-path", 0, 0, argc, argv);
	return scm_undef;
}

// lookup-process-environment
scm_obj_t
subr_lookup_process_environment(VM* vm, int argc, scm_obj_t argv[])
{
	if (argc == 1) {
		if (STRINGP(argv[0])) {
            scm_string_t string = (scm_string_t)argv[0];
#if _MSC_VER
            int max_count = strlen(string->name) + 1;
            wchar_t* ucs2_key = new wchar_t [max_count];
            if (MultiByteToWideChar(CP_UTF8, 0, string->name, -1, ucs2_key, max_count)) {
                const int max_bytes = 32768;
                wchar_t* ucs2_value = new wchar_t [max_bytes / sizeof(wchar_t)];
                if (GetEnvironmentVariableW(ucs2_key, ucs2_value, max_bytes)) {
                    char* utf8 = new char [max_bytes];
                    if (WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, ucs2_value, -1, utf8, max_bytes, NULL, NULL)) {
                        scm_obj_t obj = make_string_literal(vm->m_heap, utf8);
                        delete [] utf8;
                        delete [] ucs2_value;                
                        delete [] ucs2_key;
                        return obj;
                    }
                    delete [] utf8;
                }
                delete [] ucs2_value;                
            }
            delete [] ucs2_key;
            return scm_false;
#else
            const char* value = getenv(string->name);
            if (value) return make_string_literal(vm->m_heap, value);
            return scm_false;
#endif
		}
		wrong_type_argument_violation(vm, "lookup-process-environment", 0, "string", argv[0], argc, argv);
		return scm_undef;
	}
	wrong_number_of_arguments_violation(vm, "lookup-process-environment", 1, 1, argc, argv);
    return scm_undef;
}

void
init_subr_others(object_heap_t* heap)
{
	#define	DEFSUBR(SYM, FUNC)	heap->intern_system_subr(SYM, FUNC)
    
	DEFSUBR("core-read", subr_core_read);
	DEFSUBR("interaction-environment", subr_interaction_environment);
	DEFSUBR("format", subr_format);
	DEFSUBR("write-with-shared-structure", subr_write_with_shared_structure);
	DEFSUBR("top-level-bound?", subr_top_level_bound_pred);
	DEFSUBR("set-top-level-value!", subr_set_top_level_value);
	DEFSUBR("top-level-value", subr_top_level_value);
    DEFSUBR("run-vmi", subr_run_vmi);
	DEFSUBR("collect", subr_collect);
	DEFSUBR("display-object-statistics", subr_display_object_statistics);
	DEFSUBR("display-heap-statistics", subr_display_heap_statistics);
	DEFSUBR("closure-code", subr_closure_code);
	DEFSUBR("closure-arity", subr_closure_arity);
	DEFSUBR("decode-float", subr_decode_float);
	DEFSUBR("environment?", subr_environment_pred);
	DEFSUBR("system-environment", subr_system_environment);
	DEFSUBR("tuple", subr_tuple);
	DEFSUBR("tuple?", subr_tuple_pred);
	DEFSUBR("tuple-ref", subr_tuple_ref);
	DEFSUBR("tuple-set!", subr_tuple_set);
	DEFSUBR("tuple-length", subr_tuple_length);
	DEFSUBR("tuple-index", subr_tuple_index);
	DEFSUBR("tuple->list", subr_tuple_list);
	DEFSUBR("make-tuple", subr_make_tuple);
	DEFSUBR("make-weak-mapping", subr_make_weakmapping);
	DEFSUBR("weak-mapping?", subr_weakmapping_pred);
	DEFSUBR("weak-mapping-key", subr_weakmapping_key);
	DEFSUBR("weak-mapping-value", subr_weakmapping_value);
	DEFSUBR("string-contains", subr_string_contains);
	DEFSUBR("file-exists?", subr_file_exists_pred);
	DEFSUBR("delete-file", subr_delete_file);
	DEFSUBR("directory-list", subr_directory_list);
	DEFSUBR("stat-mtime", subr_stat_mtime);
	DEFSUBR("usleep", subr_usleep);
	DEFSUBR("exit", subr_exit);
	DEFSUBR("gensym", subr_gensym);
	DEFSUBR("subr?", subr_subr_pred);
	DEFSUBR("collect-notify", subr_collect_notify);
	DEFSUBR("collect-trip-bytes", subr_collect_trip_bytes);
	DEFSUBR("collect-stack-notify", subr_collect_stack_notify);
	DEFSUBR("microsecond", subr_microsecond);
    DEFSUBR("time-usage", subr_time_usage);
	DEFSUBR("backtrace", subr_backtrace);
	DEFSUBR("extend-lexical-syntax", subr_extend_lexical_syntax);
	DEFSUBR("display-backtrace", subr_display_backtrace);
	DEFSUBR("backtrace-line-length", subr_backtrace_line_length);
	DEFSUBR("restricted-print-line-length", subr_restricted_print_line_length);
	DEFSUBR("current-directory", subr_current_directory);
	DEFSUBR("create-directory", subr_create_directory);
	DEFSUBR("current-environment", subr_current_environment);
	DEFSUBR("current-macro-environment", subr_current_macro_environment);
	DEFSUBR("current-variable-environment", subr_current_variable_environment);
	DEFSUBR("current-exception-handler", subr_current_exception_handler);
	DEFSUBR("current-dynamic-environment", subr_current_dynamic_environment);
	DEFSUBR("current-dynamic-wind-record", subr_current_dynamic_wind_record);
	DEFSUBR("current-source-comments", subr_current_source_comments);
	DEFSUBR("copy-environment-variables!", subr_copy_environment_variables);
	DEFSUBR("copy-environment-macros!", subr_copy_environment_macros);
	DEFSUBR("make-environment", subr_make_environment);
	DEFSUBR("architecture-feature", subr_architecture_feature);
	DEFSUBR("lookup-process-environment", subr_lookup_process_environment);
	DEFSUBR("escape", subr_escape);
    DEFSUBR("command-line", subr_command_line);
    DEFSUBR("command-line-shift", subr_command_line_shift);
    DEFSUBR("system-share-path", subr_system_share_path);
	
	#undef DEFSUBR
	
}
