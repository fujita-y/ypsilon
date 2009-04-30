/*
  Ypsilon Scheme System
  Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
  See license.txt for terms and conditions of use
*/

#include "core.h"
#include "vm.h"
#include "heap.h"
#include "socket.h"
#include "subr.h"
#include "ioerror.h"
#include "violation.h"

// make-socket
scm_obj_t
subr_make_socket(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 6) {
        if (argv[0] == scm_false || STRINGP(argv[0])) {
            if (argv[1] == scm_false || STRINGP(argv[1])) {
                int family;
                int socktype;
                int protocol;
                int flags;
                CONVERT_TO_MACHINE_INT(2, "make-socket", &family);
                CONVERT_TO_MACHINE_INT(3, "make-socket", &socktype);
                CONVERT_TO_MACHINE_INT(4, "make-socket", &protocol);
                CONVERT_TO_MACHINE_INT(5, "make-socket", &flags);
                try {
                    const char* node = NULL;
                    const char* service = NULL;
                    if (STRINGP(argv[0])) node = ((scm_string_t)argv[0])->name;
                    if (STRINGP(argv[1])) service = ((scm_string_t)argv[1])->name;
                    scm_socket_t socket = make_socket(vm->m_heap, node, service, family, socktype, protocol, flags);
                    return socket;
                } catch (io_exception_t& e) {
                    raise_io_error(vm, "make-socket", e.m_operation, e.m_message, e.m_err, scm_false, scm_false);
                    return scm_undef;
                }
            }
            wrong_type_argument_violation(vm, "make-socket", 1, "string or #f", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "make-socket", 0, "string or #f", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "make-socket", 6, 6, argc, argv);
    return scm_undef;
}

// socket-shutdown
scm_obj_t
subr_socket_shutdown(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (SOCKETP(argv[0])) {
            if (FIXNUMP(argv[1])) {
                intptr_t how = FIXNUM(argv[1]);
                if (how >= 0 && how <= 2) {
                    try {
                        socket_shutdown((scm_socket_t)argv[0], FIXNUM(argv[1]));
                        return scm_unspecified;
                    } catch (io_exception_t& e) {
                        raise_io_error(vm, "socket-shutdown", e.m_operation, e.m_message, e.m_err, argv[0], scm_false);
                        return scm_undef;
                    }
                }
            }
            wrong_type_argument_violation(vm, "socket-shutdown", 1, "0, 1, or 2", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "socket-shutdown", 0, "socket", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "socket-shutdown", 2, 2, argc, argv);
    return scm_undef;
}

// socket-close
scm_obj_t
subr_socket_close(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (SOCKETP(argv[0])) {
            try {
                socket_close((scm_socket_t)argv[0]);
                return scm_unspecified;
            } catch (io_exception_t& e) {
                raise_io_error(vm, "socket-close", e.m_operation, e.m_message, e.m_err, argv[0], scm_false);
                return scm_undef;
            }
        }
        wrong_type_argument_violation(vm, "socket-close", 0, "socket", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "socket-close", 1, 1, argc, argv);
    return scm_undef;
}

// socket-send
scm_obj_t
subr_socket_send(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 3) {
        if (SOCKETP(argv[0])) {
            if (BVECTORP(argv[1])) {
                int flags;
                CONVERT_TO_MACHINE_INT(2, "socket-send", &flags);
                scm_socket_t socket = (scm_socket_t)argv[0];
                if (socket->fd != INVALID_SOCKET) {
                    try {
                        scm_bvector_t bv = (scm_bvector_t)argv[1];
                        return MAKEFIXNUM(socket_send(socket, bv->elts, bv->count, flags));
                    } catch (io_exception_t& e) {
                        raise_io_error(vm, "socket-send", e.m_operation, e.m_message, e.m_err, socket, scm_false);
                        return scm_undef;
                    }
                }
                wrong_type_argument_violation(vm, "socket-send", 0, "connected socket", argv[0], argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "socket-send", 1, "bytevector", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "socket-send", 0, "socket", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "socket-send", 3, 3, argc, argv);
    return scm_undef;
}

// socket-recv
scm_obj_t
subr_socket_recv(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 3) {
        if (SOCKETP(argv[0])) {
            int len;
            int flags;
            CONVERT_TO_MACHINE_INT(1, "socket-recv", &len);
            CONVERT_TO_MACHINE_INT(2, "socket-recv", &flags);
            scm_socket_t socket = (scm_socket_t)argv[0];
            if (socket->fd != INVALID_SOCKET) {
                try {
                    scm_bvector_t bv = make_bvector(vm->m_heap, len);
                    bool again = false;
                    int n = socket_recv(socket, bv->elts, bv->count, flags, &again);
                    if (n == 0) {
                        if (again) return scm_false;
                        if (socket->socktype == SOCK_STREAM) return scm_eof;
                        return make_bvector(vm->m_heap, 0);
                    }
                    if (n == len) return bv;
                    scm_bvector_t bv2 = make_bvector(vm->m_heap, n);
                    memcpy(bv2->elts, bv->elts, n);
                    return bv2;
                } catch (io_exception_t& e) {
                    raise_io_error(vm, "socket-recv", e.m_operation, e.m_message, e.m_err, socket, scm_false);
                    return scm_undef;
                }
            }
            wrong_type_argument_violation(vm, "socket-recv", 0, "connected socket", argv[0], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "socket-recv", 0, "socket", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "socket-recv", 2, 2, argc, argv);
    return scm_undef;
}

// socket-port
scm_obj_t
subr_socket_port(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (SOCKETP(argv[0])) {
            scm_socket_t socket = (scm_socket_t)argv[0];
            if (socket->fd != INVALID_SOCKET) {
                try {
                    socket->lock.verify_locked();
                    assert(socket->fd != INVALID_SOCKET);
                    return make_socket_port(vm->m_heap, socket, scm_false);
                } catch (io_exception_t& e) {
                    raise_io_error(vm, "socket-port", e.m_operation, e.m_message, e.m_err, socket, scm_false);
                    return scm_undef;
                }
            }
            wrong_type_argument_violation(vm, "socket-port", 0, "connected socket", argv[0], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "socket-port", 0, "socket", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "socket-port", 1, 1, argc, argv);
    return scm_undef;
}

// socket-accept
scm_obj_t
subr_socket_accept(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (SOCKETP(argv[0])) {
            scm_socket_t socket = (scm_socket_t)argv[0];
            if (socket->fd != INVALID_SOCKET) {
                try {
                    return socket_accept(vm->m_heap, socket);
                } catch (io_exception_t& e) {
                    raise_io_error(vm, "socket-accept", e.m_operation, e.m_message, e.m_err, socket, scm_false);
                    return scm_undef;
                }
            }
            wrong_type_argument_violation(vm, "socket-accept", 0, "opened socket", argv[0], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "socket-accept", 0, "socket", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "socket-accept", 1, 1, argc, argv);
    return scm_undef;
}

// socket?
scm_obj_t
subr_socket_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (SOCKETP(argv[0])) return scm_true;
        return scm_false;
    }
    wrong_number_of_arguments_violation(vm, "socket?", 1, 1, argc, argv);
    return scm_undef;
}

void init_subr_socket(object_heap_t* heap)
{
#define DEFSUBR(SYM, FUNC)  heap->intern_system_subr(SYM, FUNC)

    DEFSUBR("socket?", subr_socket_pred);
    DEFSUBR("make-socket", subr_make_socket);
    DEFSUBR("socket-send", subr_socket_send);
    DEFSUBR("socket-recv", subr_socket_recv);
    DEFSUBR("socket-close", subr_socket_close);
    DEFSUBR("socket-shutdown", subr_socket_shutdown);
    DEFSUBR("socket->port", subr_socket_port);
    DEFSUBR("socket-port", subr_socket_port);
    DEFSUBR("socket-accept", subr_socket_accept);
}
