/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#include "core.h"
#include "vm.h"
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

// set-port-current-line!
scm_obj_t
subr_set_port_current_line(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (PORTP(argv[0])) {
            scm_port_t port = (scm_port_t)argv[0];
            scoped_lock lock(port->lock);
            if (port_input_pred(port)) {
                if (FIXNUMP(argv[1])) {
                    port->line = FIXNUM(argv[1]);
                    return scm_unspecified;
                }
                if (exact_integer_pred(argv[1])) {
                    invalid_argument_violation(vm, "set-port-current-line!", "line too large,", argv[1], 1, argc, argv);
                    return scm_undef;
                } else {
                    wrong_type_argument_violation(vm, "set-port-current-line!", 1, "exact integer", argv[1], argc, argv);
                    return scm_undef;
                }
            }
            /*** FALL THROUGH ***/
        }
        wrong_type_argument_violation(vm, "set-port-current-line!", 0, "input port", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "set-port-current-line!", 2, 2, argc, argv);
    return scm_undef;
}

// set-port-current-column!
scm_obj_t
subr_set_port_current_column(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (PORTP(argv[0])) {
            scm_port_t port = (scm_port_t)argv[0];
            scoped_lock lock(port->lock);
            if (port_output_pred(port)) {
                if (FIXNUMP(argv[1])) {
                    port->column = FIXNUM(argv[1]);
                    return scm_unspecified;
                }
                if (exact_integer_pred(argv[1])) {
                    invalid_argument_violation(vm, "set-port-current-column!", "column too large,", argv[1], 1, argc, argv);
                    return scm_undef;
                } else {
                    wrong_type_argument_violation(vm, "set-port-current-column!", 1, "exact integer", argv[1], argc, argv);
                    return scm_undef;
                }
            }
            /*** FALL THROUGH ***/
        }
        wrong_type_argument_violation(vm, "set-port-current-column!", 0, "output port", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "set-port-current-column!", 2, 2, argc, argv);
    return scm_undef;
}

// port?
scm_obj_t
subr_port_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (PORTP(argv[0])) return scm_true;
        return scm_false;
    }
    wrong_number_of_arguments_violation(vm, "port?", 1, 1, argc, argv);
    return scm_undef;
}

// input-port?
scm_obj_t
subr_input_port_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (PORTP(argv[0])) {
            scm_port_t port = (scm_port_t)argv[0];
            scoped_lock lock(port->lock);
            return port_input_pred(port) ? scm_true : scm_false;
        }
        return scm_false;
    }
    wrong_number_of_arguments_violation(vm, "input-port?", 1, 1, argc, argv);
    return scm_undef;
}

// output-port?
scm_obj_t
subr_output_port_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (PORTP(argv[0])) {
            scm_port_t port = (scm_port_t)argv[0];
            scoped_lock lock(port->lock);
            return port_output_pred(port) ? scm_true : scm_false;
        }
        return scm_false;
    }
    wrong_number_of_arguments_violation(vm, "output-port?", 1, 1, argc, argv);
    return scm_undef;
}

// port-closed?
scm_obj_t
subr_port_closed_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (PORTP(argv[0])) {
            scm_port_t port = (scm_port_t)argv[0];
            scoped_lock lock(port->lock);
            return port_open_pred(port) ? scm_false : scm_true;
        }
        wrong_type_argument_violation(vm, "port-closed?", 0, "port", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "port-closed?", 1, 1, argc, argv);
    return scm_undef;
}

// output-port-buffer-mode
scm_obj_t
subr_output_port_buffer_mode(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (PORTP(argv[0])) {
            scm_port_t port = (scm_port_t)argv[0];
            scoped_lock lock(port->lock);
            CHECK_OUTPUT_PORT(0, "output-port-buffer-mode");
            try {
                int i = port_output_buffer_mode(port);
                switch (i) {
                    case SCM_PORT_BUFFER_MODE_NONE: return make_symbol(vm->m_heap, "none");
                    case SCM_PORT_BUFFER_MODE_LINE: return make_symbol(vm->m_heap, "line");
                    case SCM_PORT_BUFFER_MODE_BLOCK: return make_symbol(vm->m_heap, "block");
                    default:
                        fatal("%s:%u wrong port buffer mode", __FILE__, __LINE__);
                }
            } catch (io_exception_t& e) {
                raise_io_error(vm, "output-port-buffer-mode", e.m_operation, e.m_message, e.m_err, port, scm_false);
                return scm_undef;
//            } catch (reader_exception_t& exception) {
//                raise_lexical_violation(vm, make_symbol(vm->m_heap, "output-port-buffer-mode"), exception.m_message);
//                return scm_undef;
            }
        }
    }
    wrong_number_of_arguments_violation(vm, "output-port-buffer-mode", 1, 1, argc, argv);
    return scm_undef;
}

// flush-output-port
scm_obj_t
subr_flush_output_port(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (PORTP(argv[0])) {
            scm_port_t port = (scm_port_t)argv[0];
            scoped_lock lock(port->lock);
            CHECK_OUTPUT_PORT(0, "flush-output-port");
            try {
                port_flush_output(port);
                port_sync_port_position(port);
                return scm_unspecified;
            } catch (io_exception_t& e) {
                raise_io_error(vm, "flush-output-port", e.m_operation, e.m_message, e.m_err, port, scm_false);
                return scm_undef;
//            } catch (reader_exception_t& exception) {
//                raise_lexical_violation(vm, make_symbol(vm->m_heap, "flush-output-port"), exception.m_message);
//                return scm_undef;
            }
        }
    }
    wrong_number_of_arguments_violation(vm, "flush-output-port", 1, 1, argc, argv);
    return scm_undef;
}

// close-port
scm_obj_t
subr_close_port(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (PORTP(argv[0])) {
            scm_port_t port = (scm_port_t)argv[0];
            scoped_lock lock(port->lock);
            try {
                port_close(port);
            } catch (io_exception_t& e) {
                raise_io_error(vm, "close-port", e.m_operation, e.m_message, e.m_err, port, scm_false);
                return scm_undef;
            }
            return scm_unspecified;
        }
        wrong_type_argument_violation(vm, "close-port", 0, "port", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "close-port", 1, 1, argc, argv);
    return scm_undef;
}

// eof-object
scm_obj_t
subr_eof_object(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) return scm_eof;
    wrong_number_of_arguments_violation(vm, "eof-object", 0, 0, argc, argv);
    return scm_undef;
}

// eof-object?
scm_obj_t
subr_eof_object_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) return (argv[0] == scm_eof) ? scm_true : scm_false;
    wrong_number_of_arguments_violation(vm, "eof-object?", 1, 1, argc, argv);
    return scm_undef;
}

// current-input-port
scm_obj_t
subr_current_input_port(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) return vm->m_current_input;
    wrong_number_of_arguments_violation(vm, "current-input-port", 0, 0, argc, argv);
    return scm_undef;
}

// current-output-port
scm_obj_t
subr_current_output_port(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) return vm->m_current_output;
    wrong_number_of_arguments_violation(vm, "current-output-port", 0, 0, argc, argv);
    return scm_undef;
}

// current-error-port
scm_obj_t
subr_current_error_port(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) return vm->m_current_error;
    wrong_number_of_arguments_violation(vm, "current-error-port", 0, 0, argc, argv);
    return scm_undef;
}
/*
// current-input-port
scm_obj_t
subr_current_input_port(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) return vm->m_current_input;
    if (argc == 1) {
        if (PORTP(argv[0])) {
            vm->m_current_input = (scm_port_t)argv[0];
            return scm_unspecified;
        }
        wrong_type_argument_violation(vm, "current-input-port", 0, "port", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "current-input-port", 0, 1, argc, argv);
    return scm_undef;
}

// current-output-port
scm_obj_t
subr_current_output_port(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) return vm->m_current_output;
    if (argc == 1) {
        if (PORTP(argv[0])) {
            vm->m_current_output = (scm_port_t)argv[0];
            return scm_unspecified;
        }
        wrong_type_argument_violation(vm, "current-output-port", 0, "port", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "current-output-port", 0, 1, argc, argv);
    return scm_undef;
}

// current-error-port
scm_obj_t
subr_current_error_port(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) return vm->m_current_error;
    if (argc == 1) {
        if (PORTP(argv[0])) {
            vm->m_current_error = (scm_port_t)argv[0];
            return scm_unspecified;
        }
        wrong_type_argument_violation(vm, "current-error-port", 0, "port", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "current-error-port", 0, 1, argc, argv);
    return scm_undef;
}

 */
// standard-input-port
scm_obj_t
subr_standard_input_port(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) {
        try {
            scm_port_t port;
#if _MSC_VER
            HANDLE hdl;
            if (!DuplicateHandle(GetCurrentProcess(), PORT_STDIN_FD, GetCurrentProcess(), &hdl, 0L, TRUE, DUPLICATE_SAME_ACCESS)) {
                _dosmaperr(GetLastError());
                raise_io_error(vm, "standard-input-port", SCM_PORT_OPERATION_OPEN, strerror(errno), errno, scm_false, scm_false);
                return scm_undef;
            }
            port = make_std_port(vm->m_heap, hdl, make_string_literal(vm->m_heap, "/dev/stdin"), SCM_PORT_DIRECTION_IN, 0, SCM_PORT_BUFFER_MODE_BLOCK, scm_false);
#else
            port = make_std_port(vm->m_heap, dup(PORT_STDIN_FD), make_string_literal(vm->m_heap, "/dev/stdin"), SCM_PORT_DIRECTION_IN, 0, SCM_PORT_BUFFER_MODE_BLOCK, scm_false);
#endif
            port->mark = std_port_position(PORT_STDIN_FD);
            return port;
        } catch (io_exception_t& e) {
            raise_io_error(vm, "standard-input-port", e.m_operation, e.m_message, e.m_err, scm_false, scm_false);
            return scm_undef;
        }
    }
    wrong_number_of_arguments_violation(vm, "standard-input-port", 0, 0, argc, argv);
    return scm_undef;
}

// standard-output-port
scm_obj_t
subr_standard_output_port(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) {
        try {
            scm_port_t port;
#if _MSC_VER
            HANDLE hdl;
            if (!DuplicateHandle(GetCurrentProcess(), PORT_STDOUT_FD, GetCurrentProcess(), &hdl, 0L, TRUE, DUPLICATE_SAME_ACCESS)) {
                _dosmaperr(GetLastError());
                raise_io_error(vm, "standard-output-port", SCM_PORT_OPERATION_OPEN, strerror(errno), errno, scm_false, scm_false);
                return scm_undef;
            }
            port = make_std_port(vm->m_heap, hdl, make_string_literal(vm->m_heap, "/dev/stdout"), SCM_PORT_DIRECTION_OUT, 0, SCM_PORT_BUFFER_MODE_BLOCK, scm_false);
#else
            port = make_std_port(vm->m_heap, dup(PORT_STDOUT_FD), make_string_literal(vm->m_heap, "/dev/stdout"), SCM_PORT_DIRECTION_OUT, 0, SCM_PORT_BUFFER_MODE_BLOCK, scm_false);
#endif
            port->mark = std_port_position(PORT_STDOUT_FD);
            return port;
        } catch (io_exception_t& e) {
            raise_io_error(vm, "standard-output-port", e.m_operation, e.m_message, e.m_err, scm_false, scm_false);
            return scm_undef;
        }
    }
    wrong_number_of_arguments_violation(vm, "standard-output-port", 0, 0, argc, argv);
    return scm_undef;
}

// standard-error-port
scm_obj_t
subr_standard_error_port(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) {
        try {
            scm_port_t port;
#if _MSC_VER
            HANDLE hdl;
            if (!DuplicateHandle(GetCurrentProcess(), PORT_STDERR_FD, GetCurrentProcess(), &hdl, 0L, TRUE, DUPLICATE_SAME_ACCESS)) {
                _dosmaperr(GetLastError());
                raise_io_error(vm, "standard-error-port", SCM_PORT_OPERATION_OPEN, strerror(errno), errno, scm_false, scm_false);
                return scm_undef;
            }
            port = make_std_port(vm->m_heap, hdl, make_string_literal(vm->m_heap, "/dev/stderr"), SCM_PORT_DIRECTION_OUT, 0, SCM_PORT_BUFFER_MODE_NONE, scm_false);
#else
            port = make_std_port(vm->m_heap, dup(PORT_STDERR_FD), make_string_literal(vm->m_heap, "/dev/stderr"), SCM_PORT_DIRECTION_OUT, 0, SCM_PORT_BUFFER_MODE_NONE, scm_false);
#endif
            port->mark = std_port_position(PORT_STDERR_FD);
            return port;
        } catch (io_exception_t& e) {
            raise_io_error(vm, "standard-error-port", e.m_operation, e.m_message, e.m_err, scm_false, scm_false);
            return scm_undef;
        }
    }
    wrong_number_of_arguments_violation(vm, "standard-error-port", 0, 0, argc, argv);
    return scm_undef;
}

// native-transcoder-descriptor
scm_obj_t
subr_native_transcoder_descriptor(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) {
        return vm->m_heap->m_native_transcoder;
    }
    wrong_number_of_arguments_violation(vm, "native-transcoder-descriptor", 0, 0, argc, argv);
    return scm_undef;
}

// port-transcoder-descriptor
scm_obj_t
subr_port_transcoder_descriptor(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (PORTP(argv[0])) {
            scm_port_t port = (scm_port_t)argv[0];
            scoped_lock lock(port->lock);
            return port->transcoder;
        }
        wrong_type_argument_violation(vm, "port-transcoder-descriptor", 0, "port", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "port-transcoder-descriptor", 0, 0, argc, argv);
    return scm_undef;
}

// port-device-subtype
scm_obj_t
subr_port_device_subtype(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (PORTP(argv[0])) {
            scm_port_t port = (scm_port_t)argv[0];
            scoped_lock lock(port->lock);
            switch (port->subtype) {
            case SCM_PORT_SUBTYPE_NONE:
                return make_symbol(vm->m_heap, "none");
            case SCM_PORT_SUBTYPE_CHAR_SPECIAL:
                return make_symbol(vm->m_heap, "char");
            case SCM_PORT_SUBTYPE_FIFO:
                return make_symbol(vm->m_heap, "fifo");
            default:
                fatal("%s:%u unknown port subtype", __FILE__, __LINE__);
            }
        }
        wrong_type_argument_violation(vm, "port-device-subtype", 0, "port", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "port-device-subtype", 0, 0, argc, argv);
    return scm_undef;
}

// extract-accumulated-bytevector
scm_obj_t
subr_extract_accumulated_bytevector(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (PORTP(argv[0])) {
            scm_port_t port = (scm_port_t)argv[0];
            scoped_lock lock(port->lock);
            CHECK_OPENED_PORT(0, "extract-accumulated-bytevector");
            if (port_bytevector_pred(port) && port_output_pred(port)) return port_extract_bytevector(vm->m_heap, port);
            wrong_type_argument_violation(vm, "extract-accumulated-bytevector", 0, "bytevector or string output port", argv[0], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "extract-accumulated-bytevector", 0, "bytevector or string output port", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "extract-accumulated-bytevector", 0, 0, argc, argv);
    return scm_undef;
}

// extract-accumulated-string
scm_obj_t
subr_extract_accumulated_string(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (PORTP(argv[0])) {
            scm_port_t port = (scm_port_t)argv[0];
            scoped_lock lock(port->lock);
            CHECK_OPENED_PORT(0, "extract-accumulated-string");
            if (port_bytevector_pred(port) && port_output_pred(port)) return port_extract_string(vm->m_heap, port);
            wrong_type_argument_violation(vm, "extract-accumulated-string", 0, "bytevector or string output port", argv[0], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "extract-accumulated-string", 0, "bytevector or string output port", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "extract-accumulated-string", 0, 0, argc, argv);
    return scm_undef;
}

// get-accumulated-string
scm_obj_t
subr_get_accumulated_string(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (PORTP(argv[0])) {
            scm_port_t port = (scm_port_t)argv[0];
            scoped_lock lock(port->lock);
            CHECK_OPENED_PORT(0, "get-accumulated-string");
            if (port_bytevector_pred(port) && port_output_pred(port)) return port_get_string(vm->m_heap, port);
            wrong_type_argument_violation(vm, "get-accumulated-string", 0, "bytevector or string output port", argv[0], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "get-accumulated-string", 0, "bytevector or string output port", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "get-accumulated-string", 0, 0, argc, argv);
    return scm_undef;
}

scm_obj_t
subr_make_string_output_port(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) {
        scm_bvector_t transcoder = make_bvector(vm->m_heap, 3);
        transcoder->elts[0] = SCM_PORT_CODEC_UTF8;
        transcoder->elts[1] = SCM_PORT_EOL_STYLE_NONE;
        transcoder->elts[2] = SCM_PORT_ERROR_HANDLING_MODE_IGNORE;
        return make_bytevector_port(vm->m_heap, make_symbol(vm->m_heap, "string"), SCM_PORT_DIRECTION_OUT, scm_false, transcoder);
    }
    wrong_number_of_arguments_violation(vm, "make-string-output-port", 0, 0, argc, argv);
    return scm_undef;
}

scm_obj_t
subr_make_string_input_port(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (STRINGP(argv[0])) {
            scm_bvector_t transcoder = make_bvector(vm->m_heap, 3);
            transcoder->elts[0] = SCM_PORT_CODEC_UTF8;
            transcoder->elts[1] = SCM_PORT_EOL_STYLE_NONE;
            transcoder->elts[2] = SCM_PORT_ERROR_HANDLING_MODE_IGNORE;
            scm_string_t string = (scm_string_t)argv[0];
            int size = string->size;
            scm_bvector_t bvector = make_bvector(vm->m_heap, size);
            memcpy(bvector->elts, string->name, size);
            return make_bytevector_port(vm->m_heap, make_symbol(vm->m_heap, "string"), SCM_PORT_DIRECTION_IN, bvector, transcoder);
        }
        wrong_type_argument_violation(vm, "make-string-input-port", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "make-string-input-port", 1, 1, argc, argv);
    return scm_undef;
}

// open-port
scm_obj_t
subr_open_port(VM* vm, int argc, scm_obj_t argv[])
{
    int type = 0;

    if (FIXNUMP(argv[0])) {
        type = FIXNUM(argv[0]);
        switch (type) {
            case SCM_PORT_TYPE_NAMED_FILE: break;
            case SCM_PORT_TYPE_BYTEVECTOR: break;
            case SCM_PORT_TYPE_CUSTOM: break;
            default:
                invalid_argument_violation(vm, "open-port", "bad port type,", argv[0], 0, argc, argv);
                return scm_undef;
        }
    } else {
        wrong_type_argument_violation(vm, "open-port", 0, "fixnum", argv[0], argc, argv);
        return scm_undef;
    }

    int direction = 0;

    if (FIXNUMP(argv[1])) {
        direction = FIXNUM(argv[1]);
        switch (type) {
            case SCM_PORT_DIRECTION_IN: break;
            case SCM_PORT_DIRECTION_OUT: break;
            case SCM_PORT_DIRECTION_BOTH: break;
            default:
                invalid_argument_violation(vm, "open-port", "bad port direction,", argv[1], 1, argc, argv);
                return scm_undef;
        }
    } else {
        wrong_type_argument_violation(vm, "open-port", 1, "fixnum", argv[1], argc, argv);
        return scm_undef;
    }

    scm_obj_t name = argv[2];
    if (!(STRINGP(name) || SYMBOLP(name))) {
        wrong_type_argument_violation(vm, "open-port", 2, "string or symbol", argv[2], argc, argv);
        return scm_undef;
    }

    switch (type) {

        case SCM_PORT_TYPE_NAMED_FILE: {
            if (STRINGP(name)) {

                int file_options;

                if (FIXNUMP(argv[3])) {
                    file_options = FIXNUM(argv[3]);
                } else if (argv[3] == scm_false) {
                    file_options = SCM_PORT_FILE_OPTION_NONE;
                } else {
                    wrong_type_argument_violation(vm, "open-port", 3, "#f or fixnum", argv[3], argc, argv);
                    return scm_undef;
                }
                if (file_options & ~(SCM_PORT_FILE_OPTION_NONE | SCM_PORT_FILE_OPTION_NO_CREATE | SCM_PORT_FILE_OPTION_NO_FAIL | SCM_PORT_FILE_OPTION_NO_TRUNCATE)) {
                    invalid_argument_violation(vm, "open-port", "bad file options,", argv[3], 3, argc, argv);
                    return scm_undef;
                }

                int buffer_mode;

                if (FIXNUMP(argv[4])) {
                    buffer_mode = FIXNUM(argv[4]);
                } else if (argv[4] == scm_false) {
                    buffer_mode = SCM_PORT_BUFFER_MODE_NONE;
                } else {
                    wrong_type_argument_violation(vm, "open-port", 4, "#f or fixnum", argv[4], argc, argv);
                }
                switch (buffer_mode) {
                    case SCM_PORT_BUFFER_MODE_NONE: break;
                    case SCM_PORT_BUFFER_MODE_LINE: break;
                    case SCM_PORT_BUFFER_MODE_BLOCK: break;
                    default:
                        invalid_argument_violation(vm, "open-port", "bad buffer mode,", argv[4], 4, argc, argv);
                        return scm_undef;
                }

                scm_obj_t transcoder;

                if (BOOLP(argv[5]) || BVECTORP(argv[5])) {
                    transcoder = argv[5];
                } else {
                    wrong_type_argument_violation(vm, "open-port", 5, "#f, #t, or bytevector", argv[5], argc, argv);
                    return scm_undef;
                }
                try {
                    return make_file_port(vm->m_heap, name, direction, file_options, buffer_mode, transcoder);
                } catch (io_exception_t& e) {
                    raise_io_error(vm, "open-port", e.m_operation, e.m_message, e.m_err, scm_false, name);
                    return scm_undef;
                }
                return scm_unspecified;
            }
            wrong_type_argument_violation(vm, "open-port", 2, "string", argv[2], argc, argv);
            return scm_undef;

        } break;

        case SCM_PORT_TYPE_BYTEVECTOR: {
            if (SYMBOLP(name)) {

                scm_obj_t bytes;

                if (BVECTORP(argv[3])) {
                    if (direction == SCM_PORT_DIRECTION_OUT) {
                        wrong_type_argument_violation(vm, "open-port", 3, "#f for bytevector output port", argv[3], argc, argv);
                        return scm_undef;
                    } else {
                        bytes = argv[3];
                    }
                } else if (argv[3] == scm_false) {
                    if (direction == SCM_PORT_DIRECTION_IN) {
                        wrong_type_argument_violation(vm, "open-port", 3, "bytevector for bytevector input port", argv[3], argc, argv);
                        return scm_undef;
                    } else {
                        bytes = scm_false;
                    }
                } else {
                    wrong_type_argument_violation(vm, "open-port", 3, "#f or bytevector", argv[3], argc, argv);
                    return scm_undef;
                }

                if (argv[4] != scm_false) {
                    wrong_type_argument_violation(vm, "open-port", 4, "#f for bytevector port", argv[3], argc, argv);
                    return scm_undef;
                }

                scm_obj_t transcoder;

                if (BOOLP(argv[5]) || BVECTORP(argv[5])) {
                    transcoder = argv[5];
                } else {
                    wrong_type_argument_violation(vm, "open-port", 5, "#f, #t, or bytevector", argv[5], argc, argv);
                    return scm_undef;
                }

                try {
                    return make_bytevector_port(vm->m_heap, name, direction, bytes, transcoder);
                } catch (io_exception_t& e) {
                    raise_io_error(vm, "open-port", e.m_operation, e.m_message, e.m_err, scm_false, scm_false);
                    return scm_undef;
                }
                return scm_unspecified;

            }
            wrong_type_argument_violation(vm, "open-port", 2, "symbol", argv[2], argc, argv);
            return scm_undef;
        } break;

        case SCM_PORT_TYPE_CUSTOM: {
            if (STRINGP(name)) {

                scm_obj_t handlers;

                if (VECTORP(argv[3])) {
                    handlers = argv[3];
                } else {
                    wrong_type_argument_violation(vm, "open-port", 3, "vector", argv[3], argc, argv);
                    return scm_undef;
                }

                if (argv[4] != scm_false) {
                    wrong_type_argument_violation(vm, "open-port", 4, "#f for custom port", argv[3], argc, argv);
                    return scm_undef;
                }

                scm_obj_t transcoder;

                if (BOOLP(argv[5])) {
                    transcoder = argv[5];
                } else {
                    wrong_type_argument_violation(vm, "open-port", 5, "#f or #t for custom port", argv[5], argc, argv);
                    return scm_undef;
                }

                try {
                    return make_custom_port(vm->m_heap, name, direction, handlers, transcoder);
                } catch (io_exception_t& e) {
                    raise_io_error(vm, "open-port", e.m_operation, e.m_message, e.m_err, scm_false, scm_false);
                    return scm_undef;
                }
                return scm_unspecified;

            }
            wrong_type_argument_violation(vm, "open-port", 2, "string", argv[2], argc, argv);
            return scm_undef;
        } break;

        default: assert(false);
    }
    return scm_unspecified;
}

// open-script-input-port
scm_obj_t
subr_open_script_input_port(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (STRINGP(argv[0])) {
            scm_port_t port = NULL;
            try {
                scm_bvector_t transcoder = make_bvector(vm->m_heap, 3);
                transcoder->elts[0] = SCM_PORT_CODEC_UTF8;
                transcoder->elts[1] = SCM_PORT_EOL_STYLE_LF;
                transcoder->elts[2] = SCM_PORT_ERROR_HANDLING_MODE_RAISE;
                port = make_file_port(vm->m_heap, (scm_string_t)argv[0], SCM_PORT_DIRECTION_IN, 0, SCM_PORT_BUFFER_MODE_BLOCK, transcoder);
                assert(PORTP(port));
                scoped_lock lock(port->lock);
                scm_obj_t ch = port_lookahead_utf8(port);
                if (CHARP(ch)) {
                    uint32_t ucs4 = CHAR(ch);
                    if (ucs4 == SCM_PORT_UCS4_BOM) port_get_utf8(port);
                }
                off64_t pos = port_position(port);
                ch = port_lookahead_utf8(port);
                if (CHARP(ch) && CHAR(ch) == '#') {
                    port_get_utf8(port);
                    scm_obj_t ch1 = port_get_utf8(port);
                    scm_obj_t ch2 = port_get_utf8(port);
                    if (CHARP(ch1) && CHAR(ch1) == '!' && CHARP(ch2) && (CHAR(ch2) == '/' || CHAR(ch2) == ' ')) {
                        do {
                            ch = port_get_char(port);
                            if (ch == scm_eof) break;
                        } while (CHARP(ch) && CHAR(ch) != SCM_PORT_UCS4_LF);
                    } else {
                        port_set_port_position(port, pos);
                        port->track_line_column = true;
                        port->column = 1;
                        port->line = 1;
                    }
                }
                return port;
            } catch (io_exception_t& e) {
                raise_io_error(vm, "open-script-input-port", e.m_operation, e.m_message, e.m_err, (port ? port : scm_false), argv[0]);
                return scm_undef;
            } catch (io_codec_exception_t& e) {
                raise_io_codec_error(vm, "open-script-input-port", e.m_operation, e.m_message, (port ? port : scm_false), e.m_ch);
                return scm_undef;
            }
        }
        wrong_type_argument_violation(vm, "open-script-input-port", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "open-script-input-port", 1, 1, argc, argv);
    return scm_undef;
}

// make-file-input-port
scm_obj_t
subr_make_file_input_port(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (STRINGP(argv[0])) {
            scm_port_t port = NULL;
            try {
                port = make_file_port(vm->m_heap, (scm_string_t)argv[0], SCM_PORT_DIRECTION_IN, 0, SCM_PORT_BUFFER_MODE_BLOCK, scm_true);
                assert(PORTP(port));
                return port;
            } catch (io_exception_t& e) {
                raise_io_error(vm, "make-file-input-port", e.m_operation, e.m_message, e.m_err, (port ? port : scm_false), argv[0]);
                return scm_undef;
            } catch (io_codec_exception_t& e) {
                raise_io_codec_error(vm, "make-file-input-port", e.m_operation, e.m_message, (port ? port : scm_false), e.m_ch);
                return scm_undef;
            }
        }
        wrong_type_argument_violation(vm, "make-file-input-port", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "make-file-input-port", 1, 1, argc, argv);
    return scm_undef;
}

// make-file-output-port
scm_obj_t
subr_make_file_output_port(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (STRINGP(argv[0])) {
            scm_port_t port = NULL;
            try {
                port = make_file_port(vm->m_heap, (scm_string_t)argv[0], SCM_PORT_DIRECTION_OUT, SCM_PORT_FILE_OPTION_NO_FAIL, SCM_PORT_BUFFER_MODE_BLOCK, scm_true);
                assert(PORTP(port));
                return port;
            } catch (io_exception_t& e) {
                raise_io_error(vm, "make-file-output-port", e.m_operation, e.m_message, e.m_err, (port ? port : scm_false), argv[0]);
                return scm_undef;
            } catch (io_codec_exception_t& e) {
                raise_io_codec_error(vm, "make-file-output-port", e.m_operation, e.m_message, (port ? port : scm_false), e.m_ch);
                return scm_undef;
            }
        }
        wrong_type_argument_violation(vm, "make-file-output-port", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "make-output-file-port", 1, 1, argc, argv);
    return scm_undef;
}

// make-temporary-file-port
scm_obj_t
subr_make_temporary_file_port(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (STRINGP(argv[0])) {
            scm_string_t name = (scm_string_t)argv[0];
            if (BOOLP(argv[1]) || BVECTORP(argv[1])) {
                scm_obj_t transcoder = argv[1];
                try {
                    return make_temp_file_port(vm->m_heap, name, SCM_PORT_BUFFER_MODE_BLOCK, transcoder);
                } catch (io_exception_t& e) {
                    raise_io_error(vm, "make-temporary-file-port", e.m_operation, e.m_message, e.m_err, scm_false, name);
                    return scm_undef;
                }
                return scm_unspecified;
            }
            wrong_type_argument_violation(vm, "make-temporary-file-port", 1, "#f, #t, or bytevector", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "make-temporary-file-port", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "make-temporary-file-port", 2, 2, argc, argv);
    return scm_undef;
}

// nonblock-byte-ready?
scm_obj_t
subr_nonblock_byte_ready_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (PORTP(argv[0])) {
            scm_port_t port = (scm_port_t)argv[0];
            scoped_lock lock(port->lock);
            CHECK_OPENED_INPUT_PORT(0, "nonblock-byte-ready?");
            try {
                return port_nonblock_byte_ready(port) ? scm_true : scm_false;
            } catch (io_exception_t& e) {
                raise_io_error(vm, "nonblock-byte-ready?", e.m_operation, e.m_message, e.m_err, port, scm_false);
                return scm_undef;
            }
        }
        wrong_type_argument_violation(vm, "nonblock-byte-ready?", 0, "port", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "nonblock-byte-ready?", 1, 1, argc, argv);
    return scm_undef;
}

// get-char
scm_obj_t
subr_get_char(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (PORTP(argv[0])) {
            scm_port_t port = (scm_port_t)argv[0];
            scoped_lock lock(port->lock);
            CHECK_OPENED_TEXTUAL_INPUT_PORT(0, "get-char");
            try {
                return port_get_char(port);
            } catch (io_exception_t& e) {
                raise_io_error(vm, "get-char", e.m_operation, e.m_message, e.m_err, port, scm_false);
                return scm_undef;
            } catch (io_codec_exception_t& e) {
                raise_io_codec_error(vm, "get-char", e.m_operation, e.m_message, port, e.m_ch);
                return scm_undef;
            }
        }
        wrong_type_argument_violation(vm, "get-char", 0, "port", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "get-char", 1, 1, argc, argv);
    return scm_undef;
}

// lookahead-char
scm_obj_t
subr_lookahead_char(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (PORTP(argv[0])) {
            scm_port_t port = (scm_port_t)argv[0];
            scoped_lock lock(port->lock);
            CHECK_OPENED_TEXTUAL_INPUT_PORT(0, "lookahead-char");
            try {
                return port_lookahead_char(port);
            } catch (io_exception_t& e) {
                raise_io_error(vm, "lookahead-char", e.m_operation, e.m_message, e.m_err, port, scm_false);
                return scm_undef;
            } catch (io_codec_exception_t& e) {
                raise_io_codec_error(vm, "lookahead-char", e.m_operation, e.m_message, port, e.m_ch);
                return scm_undef;
            }
        }
        wrong_type_argument_violation(vm, "lookahead-char", 0, "port", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "lookahead-char", 1, 1, argc, argv);
    return scm_undef;
}

//port-has-port-position?
scm_obj_t
subr_port_has_port_position_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (PORTP(argv[0])) {
            scm_port_t port = (scm_port_t)argv[0];
            scoped_lock lock(port->lock);
            try {
                return port_has_port_position_pred(port) ? scm_true : scm_false;
            } catch (io_exception_t& e) {
                raise_io_error(vm, "port-has-port-position?", e.m_operation, e.m_message, e.m_err, port, scm_false);
                return scm_undef;
            }
        }
        wrong_type_argument_violation(vm, "port-has-port-position?", 0, "port", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "port-has-port-position?", 1, 1, argc, argv);
    return scm_undef;
}

//port-position
scm_obj_t
subr_port_position(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (PORTP(argv[0])) {
            scm_port_t port = (scm_port_t)argv[0];
            scoped_lock lock(port->lock);
            CHECK_OPENED_PORT(0, "port-position");
            if (port_has_port_position_pred(port)) {
                try {
                    assert(sizeof(off64_t) <= sizeof(int64_t));
                    int64_t off = port_position(port);
                    return int64_to_integer(vm->m_heap, off);
                } catch (io_exception_t& e) {
                    raise_io_error(vm, "port-position", e.m_operation, e.m_message, e.m_err, port, scm_false);
                    return scm_undef;
                }
            }
            wrong_type_argument_violation(vm, "port-position", 0, "positionable port", argv[0], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "port-position", 0, "port", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "port-position", 1, 1, argc, argv);
    return scm_undef;
}

//port-has-set-port-position!?
scm_obj_t
subr_port_has_set_port_position_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (PORTP(argv[0])) {
            scm_port_t port = (scm_port_t)argv[0];
            scoped_lock lock(port->lock);
            try {
                return port_has_set_port_position_pred(port) ? scm_true : scm_false;
            } catch (io_exception_t& e) {
                raise_io_error(vm, "port-has-set-port-position!?", e.m_operation, e.m_message, e.m_err, port, scm_false);
                return scm_undef;
            }
        }
        wrong_type_argument_violation(vm, "port-has-set-port-position!?", 0, "port", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "port-has-set-port-position!?", 1, 1, argc, argv);
    return scm_undef;
}

//set-port-position!
scm_obj_t
subr_set_port_position(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (PORTP(argv[0])) {
            scm_port_t port = (scm_port_t)argv[0];
            scoped_lock lock(port->lock);
            CHECK_OPENED_PORT(0, "set-port-position!");
            if (port_has_set_port_position_pred(port)) {
                if (exact_non_negative_integer_pred(argv[1])) {
                    try {
                        assert(sizeof(off64_t) <= sizeof(int64_t));
                        int64_t off;
                        if (exact_integer_to_int64(argv[1], &off)) {
                            port_set_port_position(port, off);
                            return scm_unspecified;
                        } else {
                            invalid_argument_violation(vm, "set-port-position!", "index out of bounds,", argv[1], 1, argc, argv);
                            return scm_undef;
                       }
                    } catch (io_exception_t& e) {
                        raise_io_error(vm, "set-port-position!", e.m_operation, e.m_message, e.m_err, port, scm_false);
                        return scm_undef;
                    }
                }
                wrong_type_argument_violation(vm, "set-port-position!", 1, "exact non-negative integer", argv[1], argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "set-port-position!", 0, "positionable port", argv[0], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "set-port-position!", 0, "port", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "set-port-position!", 2, 2, argc, argv);
    return scm_undef;
}

//port-eof?
scm_obj_t
subr_port_eof_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (PORTP(argv[0])) {
            scm_port_t port = (scm_port_t)argv[0];
            scoped_lock lock(port->lock);
            CHECK_OPENED_INPUT_PORT(0, "port-eof?");
            try {
                return port_eof(port) ? scm_true : scm_false;
            } catch (io_exception_t& e) {
                raise_io_error(vm, "port-eof?", e.m_operation, e.m_message, e.m_err, port, scm_false);
                return scm_undef;
            }
        }
        wrong_type_argument_violation(vm, "port-eof?", 0, "port", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "port-eof?", 1, 1, argc, argv);
    return scm_undef;
}

// get-u8
scm_obj_t
subr_get_u8(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (PORTP(argv[0])) {
            scm_port_t port = (scm_port_t)argv[0];
            scoped_lock lock(port->lock);
            CHECK_OPENED_BINARY_INPUT_PORT(0, "get-u8");
            try {
                return port_get_u8(port);
            } catch (io_exception_t& e) {
                raise_io_error(vm, "get-u8", e.m_operation, e.m_message, e.m_err, port, scm_false);
                return scm_undef;
            }
        }
        wrong_type_argument_violation(vm, "get-u8", 0, "port", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "get-u8", 1, 1, argc, argv);
    return scm_undef;
}

// lookahead-u8
scm_obj_t
subr_lookahead_u8(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (PORTP(argv[0])) {
            scm_port_t port = (scm_port_t)argv[0];
            scoped_lock lock(port->lock);
            CHECK_OPENED_BINARY_INPUT_PORT(0, "lookahead-u8");
            try {
                return port_lookahead_u8(port);
            } catch (io_exception_t& e) {
                raise_io_error(vm, "lookahead-u8", e.m_operation, e.m_message, e.m_err, port, scm_false);
                return scm_undef;
            }
        }
        wrong_type_argument_violation(vm, "lookahead-u8", 0, "port", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "lookahead-u8", 1, 1, argc, argv);
    return scm_undef;
}

// get-byte
scm_obj_t
subr_get_byte(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (PORTP(argv[0])) {
            scm_port_t port = (scm_port_t)argv[0];
            scoped_lock lock(port->lock);
            CHECK_OPENED_INPUT_PORT(0, "get-byte");
            try {
                return port_get_u8(port);
            } catch (io_exception_t& e) {
                raise_io_error(vm, "get-byte", e.m_operation, e.m_message, e.m_err, port, scm_false);
                return scm_undef;
            }
        }
        wrong_type_argument_violation(vm, "get-byte", 0, "port", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "get-byte", 1, 1, argc, argv);
    return scm_undef;
}

// lookahead-byte
scm_obj_t
subr_lookahead_byte(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (PORTP(argv[0])) {
            scm_port_t port = (scm_port_t)argv[0];
            scoped_lock lock(port->lock);
            CHECK_OPENED_INPUT_PORT(0, "lookahead-byte");
            try {
                return port_lookahead_u8(port);
            } catch (io_exception_t& e) {
                raise_io_error(vm, "lookahead-byte", e.m_operation, e.m_message, e.m_err, port, scm_false);
                return scm_undef;
            }
        }
        wrong_type_argument_violation(vm, "lookahead-byte", 0, "port", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "lookahead-byte", 1, 1, argc, argv);
    return scm_undef;
}

//get-bytevector-n
scm_obj_t
subr_get_bytevector_n(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (PORTP(argv[0])) {
            scm_port_t port = (scm_port_t)argv[0];
            scoped_lock lock(port->lock);
            CHECK_OPENED_BINARY_INPUT_PORT(0, "get-bytevector-n");
            CHECK_NON_NEGATIVE_FIXNUM(1, "get-bytevector-n");
            int count = FIXNUM(argv[1]);
            try {
                scm_bvector_t bvector = make_bvector(vm->m_heap, count);
                for (int i = 0; i < count; i++) {
                    int c = port_get_byte(port);
                    if (c == EOF) {
                        if (i == 0) return scm_eof;
                        scm_bvector_t bvector2 = make_bvector(vm->m_heap, i);
                        memcpy(bvector2->elts, bvector->elts, i);
                        return bvector2;
                    }
                    bvector->elts[i] = c;
                }
                return bvector;
            } catch (io_exception_t& e) {
                raise_io_error(vm, "get-bytevector-n", e.m_operation, e.m_message, e.m_err, port, scm_false);
                return scm_undef;
            }
        }
        wrong_type_argument_violation(vm, "get-bytevector-n", 0, "port", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "get-bytevector-n", 2, 2, argc, argv);
    return scm_undef;
}

//get-bytevector-n!
scm_obj_t
subr_get_bytevector_n_ex(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 4) {
        if (PORTP(argv[0])) {
            scm_port_t port = (scm_port_t)argv[0];
            scoped_lock lock(port->lock);
            CHECK_OPENED_BINARY_INPUT_PORT(0, "get-bytevector-n!");
            if (BVECTORP(argv[1])) {
                scm_bvector_t bvector = (scm_bvector_t)argv[1];
                CHECK_NON_NEGATIVE_FIXNUM(2, "get-bytevector-n!");
                int start = FIXNUM(argv[2]);
                CHECK_NON_NEGATIVE_FIXNUM(3, "get-bytevector-n!");
                int count = FIXNUM(argv[3]);
                if (start + count <= bvector->count) {
                    try {
                        for (int i = 0; i < count; i++) {
                            int c = port_get_byte(port);
                            if (c == EOF) {
                                if (i == 0) return scm_eof;
                                return MAKEFIXNUM(i);
                            }
                            bvector->elts[start + i] = c;
                       }
                        return MAKEFIXNUM(count);
                    } catch (io_exception_t& e) {
                        raise_io_error(vm, "get-bytevector-n!", e.m_operation, e.m_message, e.m_err, port, scm_false);
                        return scm_undef;
                    }
                }
                if (start >= bvector->count) {
                    invalid_argument_violation(vm, "get-bytevector-n!", "index out of bounds,", argv[2], 2, argc, argv);
                    return scm_undef;
                } else {
                    invalid_argument_violation(vm, "get-bytevector-n!", "too many elements,", argv[3], 3, argc, argv);
                    return scm_undef;
                }
            }
            wrong_type_argument_violation(vm, "get-bytevector-n!", 1, "bytevector", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "get-bytevector-n!", 0, "port", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "get-bytevector-n", 4, 4, argc, argv);
    return scm_undef;
}

// get-bytevector-some
scm_obj_t
subr_get_bytevector_some(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (PORTP(argv[0])) {
            scm_port_t port = (scm_port_t)argv[0];
            scoped_lock lock1(port->lock);
            CHECK_OPENED_BINARY_INPUT_PORT(0, "get-bytevector-some");
            try {
                int c = port_lookahead_byte(port);
                if (c == EOF) return scm_eof;
                int n = port_buffered_byte_count(port);
                scm_bvector_t bvector = make_bvector(vm->m_heap, n);
                for (int i = 0; i < n; i++) bvector->elts[i] = port_get_byte(port);
                return bvector;
            } catch (io_exception_t& e) {
                raise_io_error(vm, "get-bytevector-some", e.m_operation, e.m_message, e.m_err, port, scm_false);
                return scm_undef;
            }
        }
        wrong_type_argument_violation(vm, "get-bytevector-some", 0, "port", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "get-bytevector-some", 1, 1, argc, argv);
    return scm_undef;
}

// get-bytevector-all
scm_obj_t
subr_get_bytevector_all(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (PORTP(argv[0])) {
            scm_port_t port = (scm_port_t)argv[0];
            scoped_lock lock1(port->lock);
            CHECK_OPENED_BINARY_INPUT_PORT(0, "get-bytevector-all");
            scm_port_t output = make_bytevector_port(vm->m_heap, make_symbol(vm->m_heap, "bytevector"), SCM_PORT_DIRECTION_OUT, scm_false, scm_false);
            scoped_lock lock2(output->lock);
            try {
                while (true) {
                    int b = port_get_byte(port);
                    if (b == EOF) {
                        if (port_position(output) == 0) return scm_eof;
                        return port_extract_bytevector(vm->m_heap, output);
                    }
                    port_put_byte(output, b);
                }
            } catch (io_exception_t& e) {
                raise_io_error(vm, "get-bytevector-all", e.m_operation, e.m_message, e.m_err, port, scm_false);
                return scm_undef;
            }
        }
        wrong_type_argument_violation(vm, "get-bytevector-all", 0, "port", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "get-bytevector-all", 1, 1, argc, argv);
    return scm_undef;
}

//get-string-n
scm_obj_t
subr_get_string_n(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (PORTP(argv[0])) {
            scm_port_t port = (scm_port_t)argv[0];
            scoped_lock lock(port->lock);
            CHECK_OPENED_TEXTUAL_INPUT_PORT(0, "get-string-n");
            CHECK_NON_NEGATIVE_FIXNUM(1, "get-string-n");
            int count = FIXNUM(argv[1]);
            scm_port_t output = make_bytevector_port(vm->m_heap, make_symbol(vm->m_heap, "bytevector"), SCM_PORT_DIRECTION_OUT, scm_false, scm_false);
            scoped_lock lock2(output->lock);
            try {
                for (int i = 0; i < count; i++) {
                    scm_obj_t ch = port_get_char(port);
                    if (ch == scm_eof) {
                        if (i == 0) return scm_eof;
                        return port_extract_string(vm->m_heap, output);
                    }
                    uint8_t utf8[4];
                    int n  = cnvt_ucs4_to_utf8(CHAR(ch), utf8);
                    for (int i = 0; i < n; i++) port_put_byte(output, utf8[i]);
                }
                return port_extract_string(vm->m_heap, output);
            } catch (io_exception_t& e) {
                raise_io_error(vm, "get-string-n", e.m_operation, e.m_message, e.m_err, port, scm_false);
                return scm_undef;
            } catch (io_codec_exception_t& e) {
                raise_io_codec_error(vm, "get-string-n", e.m_operation, e.m_message, port, e.m_ch);
                return scm_undef;
            }
        }
        wrong_type_argument_violation(vm, "get-string-n", 0, "port", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "get-string-n", 2, 2, argc, argv);
    return scm_undef;
}

//get-string-n!
scm_obj_t
subr_get_string_n_ex(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 4) {
        if (PORTP(argv[0])) {
            scm_port_t port = (scm_port_t)argv[0];
            scoped_lock lock(port->lock);
            CHECK_OPENED_TEXTUAL_INPUT_PORT(0, "get-string-n!");
            if (STRINGP(argv[1])) {
                scm_string_t string = (scm_string_t)argv[1];
                int length = utf8_string_length(string);
                CHECK_NON_NEGATIVE_FIXNUM(2, "get-string-n!");
                int start = FIXNUM(argv[2]);
                CHECK_NON_NEGATIVE_FIXNUM(3, "get-string-n!");
                int count = FIXNUM(argv[3]);
                if (start + count <= length) {
                    try {
                        for (int i = 0; i < count; i++) {
                            scm_obj_t ch = port_get_char(port);
                            if (ch == scm_eof) {
                                if (i == 0) return scm_eof;
                                return MAKEFIXNUM(i);
                            }
                            utf8_string_set(vm->m_heap, string, start + i, CHAR(ch));
                        }
                        return MAKEFIXNUM(count);
                    } catch (io_exception_t& e) {
                        raise_io_error(vm, "get-string-n!", e.m_operation, e.m_message, e.m_err, port, scm_false);
                        return scm_undef;
                    } catch (io_codec_exception_t& e) {
                        raise_io_codec_error(vm, "get-string-n!", e.m_operation, e.m_message, port, e.m_ch);
                        return scm_undef;
                    }
                }
                if (start >= length) {
                    invalid_argument_violation(vm, "get-string-n!", "index out of bounds,", argv[2], 2, argc, argv);
                    return scm_undef;
                } else {
                    invalid_argument_violation(vm, "get-string-n!", "too many elements,", argv[3], 3, argc, argv);
                    return scm_undef;
                }
            }
            wrong_type_argument_violation(vm, "get-string-n!", 1, "string", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "get-string-n!", 0, "port", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "get-string-n!", 4, 4, argc, argv);
    return scm_undef;
}

// get-string-all
scm_obj_t
subr_get_string_all(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (PORTP(argv[0])) {
            scm_port_t port = (scm_port_t)argv[0];
            scoped_lock lock1(port->lock);
            CHECK_OPENED_TEXTUAL_INPUT_PORT(0, "get-string-all");
            scm_port_t output = make_bytevector_port(vm->m_heap, make_symbol(vm->m_heap, "bytevector"), SCM_PORT_DIRECTION_OUT, scm_false, scm_false);
            scoped_lock lock2(output->lock);
            try {
                while (true) {
                    scm_obj_t ch = port_get_char(port);
                    if (ch == scm_eof) {
                        if (port_position(output) == 0) return scm_eof;
                        return port_extract_string(vm->m_heap, output);
                    }
                    uint8_t utf8[4];
                    int n  = cnvt_ucs4_to_utf8(CHAR(ch), utf8);
                    for (int i = 0; i < n; i++) port_put_byte(output, utf8[i]);
                }
            } catch (io_exception_t& e) {
                raise_io_error(vm, "get-string-all", e.m_operation, e.m_message, e.m_err, port, scm_false);
                return scm_undef;
            } catch (io_codec_exception_t& e) {
                raise_io_codec_error(vm, "get-string-all", e.m_operation, e.m_message, port, e.m_ch);
                return scm_undef;
            }
        }
        wrong_type_argument_violation(vm, "get-string-all", 0, "port", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "get-string-all", 1, 1, argc, argv);
    return scm_undef;
}

// get-line
scm_obj_t
subr_get_line(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (PORTP(argv[0])) {
            scm_port_t port = (scm_port_t)argv[0];
            scoped_lock lock1(port->lock);
            CHECK_OPENED_TEXTUAL_INPUT_PORT(0, "get-line");
            scm_port_t output = make_bytevector_port(vm->m_heap, make_symbol(vm->m_heap, "bytevector"), SCM_PORT_DIRECTION_OUT, scm_false, scm_false);
            scoped_lock lock2(output->lock);
            try {
                while (true) {
                    scm_obj_t ch = port_get_char(port);
                    if (ch == scm_eof) {
                        if (port_position(output) == 0) return scm_eof;
                        return port_extract_string(vm->m_heap, output);
                    }
                    if (CHAR(ch) == SCM_PORT_UCS4_LF) return port_extract_string(vm->m_heap, output);
                    uint8_t utf8[4];
                    int n  = cnvt_ucs4_to_utf8(CHAR(ch), utf8);
                    for (int i = 0; i < n; i++) port_put_byte(output, utf8[i]);
                }
            } catch (io_exception_t& e) {
                raise_io_error(vm, "get-line", e.m_operation, e.m_message, e.m_err, port, scm_false);
                return scm_undef;
            } catch (io_codec_exception_t& e) {
                raise_io_codec_error(vm, "get-line", e.m_operation, e.m_message, port, e.m_ch);
                return scm_undef;
            }
        }
        wrong_type_argument_violation(vm, "get-line", 0, "port", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "get-line", 1, 1, argc, argv);
    return scm_undef;
}

// get-datum
scm_obj_t
subr_get_datum(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (PORTP(argv[0])) {
            scm_port_t port = (scm_port_t)argv[0];
            scoped_lock lock(port->lock);
            CHECK_OPENED_TEXTUAL_INPUT_PORT(0, "get-datum");
            try {
                return reader_t(vm, port).read(NULL);
            } catch (io_exception_t& e) {
                raise_io_error(vm, "get-datum", e.m_operation, e.m_message, e.m_err, port, scm_false);
                return scm_undef;
            } catch (io_codec_exception_t& e) {
                raise_io_codec_error(vm, "get-datum", e.m_operation, e.m_message, port, e.m_ch);
                return scm_undef;
            } catch (reader_exception_t& exception) {
                lexical_violation(vm, make_symbol(vm->m_heap, "get-datum"), exception.m_message);
                return scm_undef;
            }
        }
        wrong_type_argument_violation(vm, "get-datum", 0, "port", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "get-datum", 1, 1, argc, argv);
    return scm_undef;
}

// put-u8
scm_obj_t
subr_put_u8(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (PORTP(argv[0])) {
            scm_port_t port = (scm_port_t)argv[0];
            scoped_lock lock(port->lock);
            CHECK_OPENED_BINARY_OUTPUT_PORT(0, "put-u8");
            CHECK_OCTET(1, "put-u8");
            try {
                port_put_byte(port, FIXNUM(argv[1]));
                if (port->force_sync) port_flush_output(port);
                return scm_unspecified;
            } catch (io_exception_t& e) {
                raise_io_error(vm, "put-u8", e.m_operation, e.m_message, e.m_err, port, scm_false);
                return scm_undef;
            }
        }
        wrong_type_argument_violation(vm, "put-u8", 0, "port", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "put-u8", 2, 2, argc, argv);
    return scm_undef;
}

// put-byte
scm_obj_t
subr_put_byte(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (PORTP(argv[0])) {
            scm_port_t port = (scm_port_t)argv[0];
            scoped_lock lock(port->lock);
            CHECK_OPENED_OUTPUT_PORT(0, "put-byte");
            CHECK_OCTET(1, "put-byte");
            try {
                port_put_byte(port, FIXNUM(argv[1]));
                if (port->force_sync) port_flush_output(port);
                return scm_unspecified;
            } catch (io_exception_t& e) {
                raise_io_error(vm, "put-byte", e.m_operation, e.m_message, e.m_err, port, scm_false);
                return scm_undef;
            }
        }
        wrong_type_argument_violation(vm, "put-byte", 0, "port", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "put-byte", 2, 2, argc, argv);
    return scm_undef;
}

// put-bytevector
scm_obj_t
subr_put_bytevector(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc >= 2 && argc <= 4) {
        if (PORTP(argv[0])) {
            scm_port_t port = (scm_port_t)argv[0];
            scoped_lock lock(port->lock);
            CHECK_OPENED_BINARY_OUTPUT_PORT(0, "put-bytevector");
            if (BVECTORP(argv[1])) {
                scm_bvector_t bvector = (scm_bvector_t)argv[1];
                int start = 0;
                int count = bvector->count;
                if (argc > 2) {
                    CHECK_NON_NEGATIVE_FIXNUM(2, "put-bytevector");
                    start = FIXNUM(argv[2]);
                    count = bvector->count - start;
                }
                if (argc > 3) {
                    CHECK_NON_NEGATIVE_FIXNUM(3, "put-bytevector");
                    count = FIXNUM(argv[3]);
                }
                if (start + count <= bvector->count) {
                    try {
                        for (int i = 0; i < count; i++) port_put_byte(port, bvector->elts[start + i]);
                        if (port->force_sync) port_flush_output(port);
                        return scm_unspecified;
                    } catch (io_exception_t& e) {
                        raise_io_error(vm, "put-bytevector", e.m_operation, e.m_message, e.m_err, port, scm_false);
                        return scm_undef;
                    }
                }
                if (start >= bvector->count) {
                    invalid_argument_violation(vm, "put-bytevector", "index out of bounds,", argv[2], 2, argc, argv);
                    return scm_undef;
                } else {
                    invalid_argument_violation(vm, "put-bytevector", "too many elements,", argv[3], 3, argc, argv);
                    return scm_undef;
                }
            }
            wrong_type_argument_violation(vm, "put-bytevector", 1, "bytevector", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "put-bytevector", 0, "port", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "put-bytevector", 2, 4, argc, argv);
    return scm_undef;
}

// put-char
scm_obj_t
subr_put_char(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (PORTP(argv[0])) {
            scm_port_t port = (scm_port_t)argv[0];
            scoped_lock lock(port->lock);
            CHECK_OPENED_TEXTUAL_OUTPUT_PORT(0, "put-char");
            if (CHARP(argv[1])) {
                try {
                    port_put_char(port, argv[1]);
                    if (port->force_sync) port_flush_output(port);
                    return scm_unspecified;
                } catch (io_exception_t& e) {
                    raise_io_error(vm, "put-char", e.m_operation, e.m_message, e.m_err, port, scm_false);
                    return scm_undef;
                } catch (io_codec_exception_t& e) {
                    raise_io_codec_error(vm, "put-char", e.m_operation, e.m_message, port, e.m_ch);
                    return scm_undef;
                }
            }
            wrong_type_argument_violation(vm, "put-char", 1, "char", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "put-char", 0, "port", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "put-char", 2, 2, argc, argv);
    return scm_undef;
}

// put-string
scm_obj_t
subr_put_string(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc >= 2 && argc <= 4) {
        if (PORTP(argv[0])) {
            scm_port_t port = (scm_port_t)argv[0];
            scoped_lock lock(port->lock);
            CHECK_OPENED_TEXTUAL_OUTPUT_PORT(0, "put-string");
            if (STRINGP(argv[1])) {
                scm_string_t string = (scm_string_t)argv[1];
                if (argc == 2) {
                    try {
                        port_put_string(port, string);
                        if (port->force_sync) port_flush_output(port);
                        return scm_unspecified;
                    } catch (io_exception_t& e) {
                        raise_io_error(vm, "put-string", e.m_operation, e.m_message, e.m_err, port, scm_false);
                        return scm_undef;
                    } catch (io_codec_exception_t& e) {
                        raise_io_codec_error(vm, "put-string", e.m_operation, e.m_message, port, e.m_ch);
                        return scm_undef;
                    }
                }
                int len = utf8_string_length(string);
                int start = 0;
                int count = len;
                if (argc > 2) {
                    CHECK_NON_NEGATIVE_FIXNUM(2, "put-string");
                    start = FIXNUM(argv[2]);
                    count = len - start;
                }
                if (argc > 3) {
                    CHECK_NON_NEGATIVE_FIXNUM(3, "put-string");
                    count = FIXNUM(argv[3]);
                }
                if (start + count <= len) {
                    try {
                        for (int i = 0; i < count; i++) {
                            int c = utf8_string_ref(string, start + i);
                            if (c >= 0) {
                                port_put_char(port, MAKECHAR(c));
                                continue;
                            }
                            invalid_object_violation(vm, "put-string", "properly encoded string", string, argc, argv);
                            return scm_undef;
                        }
                        if (port->force_sync) port_flush_output(port);
                        return scm_unspecified;
                    } catch (io_exception_t& e) {
                        raise_io_error(vm, "put-string", e.m_operation, e.m_message, e.m_err, port, scm_false);
                        return scm_undef;
                    } catch (io_codec_exception_t& e) {
                        raise_io_codec_error(vm, "put-string", e.m_operation, e.m_message, port, e.m_ch);
                        return scm_undef;
                    }
                }
                if (start >= len) {
                    invalid_argument_violation(vm, "put-string", "index out of bounds,", argv[2], 2, argc, argv);
                    return scm_undef;
                } else {
                    invalid_argument_violation(vm, "put-string", "too many elements,", argv[3], 3, argc, argv);
                    return scm_undef;
                }
            }
            wrong_type_argument_violation(vm, "put-string", 1, "string", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "put-string", 0, "port", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "put-string", 2, 4, argc, argv);
    return scm_undef;
}

// put-datum
scm_obj_t
subr_put_datum(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (PORTP(argv[0])) {
            scm_port_t port = (scm_port_t)argv[0];
            scoped_lock lock(port->lock);
            CHECK_OPENED_TEXTUAL_OUTPUT_PORT(0, "put-datum");
            try {
                if (BOOLP(port->transcoder)) {
                    printer_t prt(vm, port);
                    prt.format("~s", argv[1]);
                } else {
                    scm_port_t buf = make_bytevector_port(vm->m_heap, make_symbol(vm->m_heap, "string"), SCM_PORT_DIRECTION_OUT, scm_false, scm_true);
                    scoped_lock lock2(buf->lock);
                    printer_t prt(vm, buf);
                    prt.format("~s", argv[1]);
                    port_put_string(port, port_extract_string(vm->m_heap, buf));
                    if (port->force_sync) port_flush_output(port);
                }
                return scm_unspecified;
            } catch (io_exception_t& e) {
                raise_io_error(vm, "put-datum", e.m_operation, e.m_message, e.m_err, port, scm_false);
                return scm_undef;
            } catch (io_codec_exception_t& e) {
                raise_io_codec_error(vm, "put-datum", e.m_operation, e.m_message, port, e.m_ch);
                return scm_undef;
            }
        }
        wrong_type_argument_violation(vm, "put-datum", 0, "port", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "put-datum", 2, 2, argc, argv);
    return scm_undef;
}

// put-fasl
scm_obj_t
subr_put_fasl(VM* vm, int argc, scm_obj_t argv[])
{
#if DISABLE_FASL
    return subr_put_datum(vm, argc, argv);
#endif
    if (argc == 2) {
        if (PORTP(argv[0])) {
            scm_port_t port = (scm_port_t)argv[0];
            scoped_lock lock(port->lock);
            CHECK_OPENED_OUTPUT_PORT(0, "put-fasl");
            try {
                port_puts(port, "\n#!fasl0\n");
                scm_obj_t bad = fasl_printer_t(vm, port).put(argv[1]);
                if (bad) {
                    non_serializable_object_violation(vm, "put-fasl", bad, argc, argv);
                    return scm_undef;
                }
                return scm_unspecified;
            } catch (io_codec_exception_t& e) {
                raise_io_codec_error(vm, "put-fasl", e.m_operation, e.m_message, port, e.m_ch);
                return scm_undef;
            } catch (io_exception_t& e) {
                raise_io_error(vm, "put-fasl", e.m_operation, e.m_message, e.m_err, port, scm_false);
                return scm_undef;
            }
        }
        wrong_type_argument_violation(vm, "put-fasl", 0, "port", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "put-fasl", 2, 2, argc, argv);
    return scm_undef;
}

// write
scm_obj_t
subr_write(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1 || argc == 2) {
        scm_port_t port;
        if (argc == 1) {
            port = vm->m_current_output;
        } else {
            if (PORTP(argv[1])) {
                port = (scm_port_t)argv[1];
            } else {
                wrong_type_argument_violation(vm, "write", 1, "port", argv[1], argc, argv);
                return scm_undef;
            }
        }
        scoped_lock lock(port->lock);
        if (argc == 2) {
            CHECK_OPENED_TEXTUAL_OUTPUT_PORT(1, "write");
        } else {
            if (!port_open_pred(port) || !port_output_pred(port) || !port_textual_pred(port)) {
                invalid_object_violation(vm, "write", "current output port is textual and it opened for output", port, argc, argv);
                return scm_undef;
            }
        }
        try {
            if (BOOLP(port->transcoder)) {
                printer_t prt(vm, port);
                prt.format("~s", argv[0]);
            } else {
                scm_port_t buf = make_bytevector_port(vm->m_heap, make_symbol(vm->m_heap, "string"), SCM_PORT_DIRECTION_OUT, scm_false, scm_true);
                scoped_lock lock2(buf->lock);
                printer_t prt(vm, buf);
                prt.format("~s", argv[0]);
                port_put_string(port, port_extract_string(vm->m_heap, buf));
            }
            if (port->force_sync) port_flush_output(port);
            return scm_unspecified;
        } catch (io_exception_t& e) {
            raise_io_error(vm, "write", e.m_operation, e.m_message, e.m_err, port, scm_false);
            return scm_undef;
        } catch (io_codec_exception_t& e) {
            raise_io_codec_error(vm, "write", e.m_operation, e.m_message, port, e.m_ch);
            return scm_undef;
        }
    }
    wrong_number_of_arguments_violation(vm, "write", 1, 2, argc, argv);
    return scm_undef;
}

// display
scm_obj_t
subr_display(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1 || argc == 2) {
        scm_port_t port;
        if (argc == 1) {
            port = vm->m_current_output;
        } else {
            if (PORTP(argv[1])) {
                port = (scm_port_t)argv[1];
            } else {
                wrong_type_argument_violation(vm, "display", 1, "port", argv[1], argc, argv);
                return scm_undef;
            }
        }
        scoped_lock lock(port->lock);
        if (argc == 2) {
            CHECK_OPENED_TEXTUAL_OUTPUT_PORT(1, "display");
        } else {
            if (!port_open_pred(port) || !port_output_pred(port) || !port_textual_pred(port)) {
                invalid_object_violation(vm, "display", "current output port is textual and it opened for output", port, argc, argv);
                return scm_undef;
            }
        }
        try {
            if (BOOLP(port->transcoder)) {
                printer_t(vm, port).format("~a", argv[0]);
            } else {
                scm_port_t buf = make_bytevector_port(vm->m_heap, make_symbol(vm->m_heap, "string"), SCM_PORT_DIRECTION_OUT, scm_false, scm_true);
                scoped_lock lock2(buf->lock);
                printer_t(vm, buf).format("~a", argv[0]);
                port_put_string(port, port_extract_string(vm->m_heap, buf));
            }
            if (port->force_sync) port_flush_output(port);
            return scm_unspecified;
        } catch (io_exception_t& e) {
            raise_io_error(vm, "display", e.m_operation, e.m_message, e.m_err, port, scm_false);
            return scm_undef;
        } catch (io_codec_exception_t& e) {
            raise_io_codec_error(vm, "display", e.m_operation, e.m_message, port, e.m_ch);
            return scm_undef;
        }
    }
    wrong_number_of_arguments_violation(vm, "display", 1, 2, argc, argv);
    return scm_undef;
}

// newline
scm_obj_t
subr_newline(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0 || argc == 1) {
        scm_port_t port;
        if (argc == 0) {
            port = vm->m_current_output;
        } else {
            if (PORTP(argv[0])) {
                port = (scm_port_t)argv[0];
            } else {
                wrong_type_argument_violation(vm, "newline", 0, "port", argv[0], argc, argv);
                return scm_undef;
            }
        }
        scoped_lock lock(port->lock);
        if (argc == 1) {
            CHECK_OPENED_TEXTUAL_OUTPUT_PORT(0, "newline");
        } else {
            if (!port_open_pred(port) || !port_output_pred(port) || !port_textual_pred(port)) {
                invalid_object_violation(vm, "newline", "current output port is textual and it opened for output", port, argc, argv);
                return scm_undef;
            }
        }
        try {
            printer_t(vm, port).format("~%", argv[1]);
            if (port->force_sync) port_flush_output(port);
            return scm_unspecified;
        } catch (io_exception_t& e) {
            raise_io_error(vm, "newline", e.m_operation, e.m_message, e.m_err, port, scm_false);
            return scm_undef;
        } catch (io_codec_exception_t& e) {
            raise_io_codec_error(vm, "newline", e.m_operation, e.m_message, port, e.m_ch);
            return scm_undef;
        }
    }
    wrong_number_of_arguments_violation(vm, "newline", 0, 1, argc, argv);
    return scm_undef;
}

// read-char
scm_obj_t
subr_read_char(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0 || argc == 1) {
        scm_port_t port;
        if (argc == 0) {
            port = vm->m_current_input;
        } else {
            if (PORTP(argv[0])) {
                port = (scm_port_t)argv[0];
            } else {
                wrong_type_argument_violation(vm, "read-char", 0, "port", argv[0], argc, argv);
                return scm_undef;
            }
        }
        scoped_lock lock(port->lock);
        if (argc == 1) {
            CHECK_OPENED_TEXTUAL_INPUT_PORT(0, "read-char");
        } else {
            if (!port_open_pred(port) || !port_input_pred(port) || !port_textual_pred(port)) {
                invalid_object_violation(vm, "read-char", "current input port is textual and it opened for input", port, argc, argv);
                return scm_undef;
            }
        }
        try {
            return port_get_char(port);
        } catch (io_exception_t& e) {
            raise_io_error(vm, "read-char", e.m_operation, e.m_message, e.m_err, port, scm_false);
            return scm_undef;
        } catch (io_codec_exception_t& e) {
            raise_io_codec_error(vm, "read-char", e.m_operation, e.m_message, port, e.m_ch);
            return scm_undef;
        }
    }
    wrong_number_of_arguments_violation(vm, "read-char", 0, 1, argc, argv);
    return scm_undef;
}

// peek-char
scm_obj_t
subr_peek_char(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0 || argc == 1) {
        scm_port_t port;
        if (argc == 0) {
            port = vm->m_current_input;
        } else {
            if (PORTP(argv[0])) {
                port = (scm_port_t)argv[0];
            } else {
                wrong_type_argument_violation(vm, "peek-char", 0, "port", argv[0], argc, argv);
                return scm_undef;
            }
        }
        scoped_lock lock(port->lock);
        if (argc == 1) {
            CHECK_OPENED_TEXTUAL_INPUT_PORT(0, "peek-char");
        } else {
            if (!port_open_pred(port) || !port_input_pred(port) || !port_textual_pred(port)) {
                invalid_object_violation(vm, "peek-char", "current input port is textual and it opened for input", port, argc, argv);
                return scm_undef;
            }
        }
        try {
            return port_lookahead_char(port);
        } catch (io_exception_t& e) {
            raise_io_error(vm, "peek-char", e.m_operation, e.m_message, e.m_err, port, scm_false);
            return scm_undef;
        } catch (io_codec_exception_t& e) {
            raise_io_codec_error(vm, "peek-char", e.m_operation, e.m_message, port, e.m_ch);
            return scm_undef;
        }
    }
    wrong_number_of_arguments_violation(vm, "peek-char", 0, 1, argc, argv);
    return scm_undef;
}

// write-char
scm_obj_t
subr_write_char(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1 || argc == 2) {
        scm_port_t port;
        if (argc == 1) {
            port = vm->m_current_output;
        } else {
            if (PORTP(argv[1])) {
                port = (scm_port_t)argv[1];
            } else {
                wrong_type_argument_violation(vm, "write-char", 1, "port", argv[1], argc, argv);
                return scm_undef;
            }
        }
        scoped_lock lock(port->lock);
        if (argc == 2) {
            CHECK_OPENED_TEXTUAL_OUTPUT_PORT(1, "write-char");
        } else {
            if (!port_open_pred(port) || !port_output_pred(port) || !port_textual_pred(port)) {
                invalid_object_violation(vm, "write-char", "current output port is textual and it opened for output", port, argc, argv);
                return scm_undef;
            }
        }
        if (CHARP(argv[0])) {
            try {
                port_put_char(port, argv[0]);
                if (port->force_sync) port_flush_output(port);
                return scm_unspecified;
            } catch (io_exception_t& e) {
                raise_io_error(vm, "write-char", e.m_operation, e.m_message, e.m_err, port, scm_false);
                return scm_undef;
            } catch (io_codec_exception_t& e) {
                raise_io_codec_error(vm, "write-char", e.m_operation, e.m_message, port, e.m_ch);
                return scm_undef;
            }
        }
        wrong_type_argument_violation(vm, "write-char", 0, "char", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "write-char", 1, 2, argc, argv);
    return scm_undef;
}

// read
scm_obj_t
subr_read(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0 || argc == 1) {
        scm_port_t port;
        if (argc == 0) {
            port = vm->m_current_input;
        } else {
            if (PORTP(argv[0])) {
                port = (scm_port_t)argv[0];
            } else {
                wrong_type_argument_violation(vm, "read", 0, "port", argv[0], argc, argv);
                return scm_undef;
            }
        }
        scoped_lock lock(port->lock);
        if (argc == 1) {
            CHECK_OPENED_TEXTUAL_INPUT_PORT(0, "read");
        } else {
            if (!port_open_pred(port) || !port_input_pred(port) || !port_textual_pred(port)) {
                invalid_object_violation(vm, "read", "current input port is textual and it opened for input", port, argc, argv);
                return scm_undef;
            }
        }
        try {
            return reader_t(vm, port).read(NULL);
        } catch (io_exception_t& e) {
            raise_io_error(vm, "read", e.m_operation, e.m_message, e.m_err, port, scm_false);
            return scm_undef;
        } catch (io_codec_exception_t& e) {
            raise_io_codec_error(vm, "read", e.m_operation, e.m_message, port, e.m_ch);
            return scm_undef;
        } catch (reader_exception_t& exception) {
            lexical_violation(vm, make_symbol(vm->m_heap, "read"), exception.m_message);
            return scm_undef;
        }
    }
    wrong_number_of_arguments_violation(vm, "read", 0, 1, argc, argv);
    return scm_undef;
}

// make-transcoded-port
scm_obj_t
subr_make_transcoded_port(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (PORTP(argv[0])) {
            scm_port_t port = (scm_port_t)argv[0];
            scoped_lock lock(port->lock);
            CHECK_OPENED_BINARY_PORT(0, "make-transcoded-port");
            if (BVECTORP(argv[1])) {
                scm_bvector_t transcoder = (scm_bvector_t)argv[1];
                try {
                    scm_port_t textual = make_transcoded_port(vm->m_heap, make_list(vm->m_heap, 2, make_symbol(vm->m_heap, "transcoded"), port->name), port, transcoder);
                    return textual;
                } catch (io_exception_t& e) {
                    raise_io_error(vm, "make-transcoded-port", e.m_operation, e.m_message, e.m_err, port, scm_false);
                    return scm_undef;
                }
            }
            wrong_type_argument_violation(vm, "make-transcoded-port", 1, "bytevector", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "make-transcoded-port", 0, "port", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "make-transcoded-port", 2, 2, argc, argv);
    return scm_undef;
}

// set-current-input-port!
scm_obj_t
subr_set_current_input_port(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (PORTP(argv[0])) {
            vm->m_current_input = (scm_port_t)argv[0];
            return scm_unspecified;
        }
        wrong_type_argument_violation(vm, "set-current-input-port!", 0, "port", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "set-current-input-port!", 1, 1, argc, argv);
    return scm_undef;
}

// set-current-output-port!
scm_obj_t
subr_set_current_output_port(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (PORTP(argv[0])) {
            vm->m_current_output = (scm_port_t)argv[0];
            return scm_unspecified;
        }
        wrong_type_argument_violation(vm, "set-current-output-port!", 0, "port", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "set-current-output-port!", 1, 1, argc, argv);
    return scm_undef;
}

// set-current-error-port!
scm_obj_t
subr_set_current_error_port(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (PORTP(argv[0])) {
            vm->m_current_error = (scm_port_t)argv[0];
            return scm_unspecified;
        }
        wrong_type_argument_violation(vm, "set-current-error-port!", 0, "port", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "set-current-error-port!", 1, 1, argc, argv);
    return scm_undef;
}

// shutdown-output-port
scm_obj_t
subr_shutdown_output_port(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (PORTP(argv[0])) {
            scm_port_t port = (scm_port_t)argv[0];
            scoped_lock lock(port->lock);
            if (port->type == SCM_PORT_TYPE_SOCKET) {
                try {
                    port_shutdown_output(port);
                    return scm_unspecified;
                } catch (io_exception_t& e) {
                    raise_io_error(vm, "shutdown-output-port", e.m_operation, e.m_message, e.m_err, port, scm_false);
                    return scm_undef;
                }
            }
            wrong_type_argument_violation(vm, "shutdown-output-port", 0, "socket port", argv[0], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "shutdown-output-port", 0, "port", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "shutdown-output-port", 1, 1, argc, argv);
    return scm_undef;
}

void init_subr_port(object_heap_t* heap)
{
    #define DEFSUBR(SYM, FUNC)  heap->intern_system_subr(SYM, FUNC)

    DEFSUBR("port?", subr_port_pred);
    DEFSUBR("input-port?", subr_input_port_pred);
    DEFSUBR("output-port?", subr_output_port_pred);
    DEFSUBR("close-port", subr_close_port);
    DEFSUBR("eof-object", subr_eof_object);
    DEFSUBR("eof-object?", subr_eof_object_pred);
    DEFSUBR("current-input-port", subr_current_input_port);
    DEFSUBR("current-output-port", subr_current_output_port);
    DEFSUBR("current-error-port", subr_current_error_port);
    DEFSUBR("standard-input-port", subr_standard_input_port);
    DEFSUBR("standard-output-port", subr_standard_output_port);
    DEFSUBR("standard-error-port", subr_standard_error_port);

    DEFSUBR("flush-output-port", subr_flush_output_port);
    DEFSUBR("output-port-buffer-mode", subr_output_port_buffer_mode);

    DEFSUBR("set-port-current-line!", subr_set_port_current_line);
    DEFSUBR("set-port-current-column!", subr_set_port_current_column);
    DEFSUBR("port-device-subtype", subr_port_device_subtype);

    DEFSUBR("native-transcoder-descriptor", subr_native_transcoder_descriptor);
    DEFSUBR("port-transcoder-descriptor", subr_port_transcoder_descriptor);

    DEFSUBR("extract-accumulated-bytevector", subr_extract_accumulated_bytevector);
    DEFSUBR("extract-accumulated-string", subr_extract_accumulated_string);
    DEFSUBR("get-accumulated-string", subr_get_accumulated_string);
    DEFSUBR("make-string-output-port", subr_make_string_output_port);
    DEFSUBR("make-string-input-port", subr_make_string_input_port);

    DEFSUBR("open-script-input-port", subr_open_script_input_port);
    DEFSUBR("make-file-input-port", subr_make_file_input_port);
    DEFSUBR("make-file-output-port", subr_make_file_output_port);
    DEFSUBR("make-temporary-file-port", subr_make_temporary_file_port);

    DEFSUBR("open-port", subr_open_port);

    DEFSUBR("nonblock-byte-ready?", subr_nonblock_byte_ready_pred);
    DEFSUBR("get-char", subr_get_char);
    DEFSUBR("lookahead-char", subr_lookahead_char);

    DEFSUBR("port-has-port-position?", subr_port_has_port_position_pred);
    DEFSUBR("port-position", subr_port_position);
    DEFSUBR("port-has-set-port-position!?", subr_port_has_set_port_position_pred);
    DEFSUBR("set-port-position!", subr_set_port_position);
    DEFSUBR("port-eof?", subr_port_eof_pred);

    DEFSUBR("get-u8", subr_get_u8);
    DEFSUBR("get-byte", subr_get_byte);
    DEFSUBR("lookahead-u8", subr_lookahead_u8);
    DEFSUBR("lookahead-byte", subr_lookahead_byte);

    DEFSUBR("get-bytevector-n", subr_get_bytevector_n);
    DEFSUBR("get-bytevector-n!", subr_get_bytevector_n_ex);
    DEFSUBR("get-bytevector-some", subr_get_bytevector_some);
    DEFSUBR("get-bytevector-all", subr_get_bytevector_all);
    DEFSUBR("get-string-n", subr_get_string_n);
    DEFSUBR("get-string-n!", subr_get_string_n_ex);
    DEFSUBR("get-string-all", subr_get_string_all);
    DEFSUBR("get-line", subr_get_line);
    DEFSUBR("get-datum", subr_get_datum);

    DEFSUBR("put-u8", subr_put_u8);
    DEFSUBR("put-byte", subr_put_byte);
    DEFSUBR("put-bytevector", subr_put_bytevector);

    DEFSUBR("put-char", subr_put_char);
    DEFSUBR("put-string", subr_put_string);
    DEFSUBR("put-datum", subr_put_datum);
    DEFSUBR("put-fasl", subr_put_fasl);

    DEFSUBR("display", subr_display);
    DEFSUBR("write", subr_write);
    DEFSUBR("newline", subr_newline);
    DEFSUBR("peek-char", subr_peek_char);
    DEFSUBR("read-char", subr_read_char);
    DEFSUBR("write-char", subr_write_char);
    DEFSUBR("read", subr_read);

    DEFSUBR("make-transcoded-port", subr_make_transcoded_port);
    DEFSUBR("set-current-input-port!", subr_set_current_input_port);
    DEFSUBR("set-current-output-port!", subr_set_current_output_port);
    DEFSUBR("set-current-error-port!", subr_set_current_error_port);

    DEFSUBR("shutdown-output-port", subr_shutdown_output_port);
    DEFSUBR("port-closed?", subr_port_closed_pred);

}
