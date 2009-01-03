/*
    Ypsilon Scheme System
    Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#ifndef SUBR_H_INCLUDED
#define SUBR_H_INCLUDED

#include "core.h"
#include "object.h"
#include "arith.h"

void init_subr_ffi(object_heap_t* heap);
void init_subr_base_arith(object_heap_t* heap);
void init_subr_r5rs_arith(object_heap_t* heap);
void init_subr_base(object_heap_t* heap);
void init_subr_bvector(object_heap_t* heap);
void init_subr_port(object_heap_t* heap);
void init_subr_socket(object_heap_t* heap);
void init_subr_bitwise(object_heap_t* heap);
void init_subr_fixnum(object_heap_t* heap);
void init_subr_flonum(object_heap_t* heap);
void init_subr_unicode(object_heap_t* heap);
void init_subr_hash(object_heap_t* heap);
void init_subr_list(object_heap_t* heap);
void init_subr_others(object_heap_t* heap);

#define CHECK_OPENED_INPUT_PORT(pos, subr)                                                              \
            do {                                                                                        \
                assert(PORTP(argv[pos]));                                                               \
                scm_port_t port = (scm_port_t)argv[pos];                                                \
                if (port_open_pred(port)) {                                                             \
                    if (port_input_pred(port)) break;                                                   \
                    wrong_type_argument_violation(vm, subr, pos, "input port", port, argc, argv);       \
                    return scm_undef;                                                                   \
                }                                                                                       \
                wrong_type_argument_violation(vm, subr, pos, "opened port", port, argc, argv);          \
                return scm_undef;                                                                       \
            } while (false)

#define CHECK_OPENED_OUTPUT_PORT(pos, subr)                                                             \
            do {                                                                                        \
                assert(PORTP(argv[pos]));                                                               \
                scm_port_t port = (scm_port_t)argv[pos];                                                \
                if (port_open_pred(port)) {                                                             \
                    if (port_output_pred(port)) break;                                                  \
                    wrong_type_argument_violation(vm, subr, pos, "output port", port, argc, argv);      \
                    return scm_undef;                                                                   \
                }                                                                                       \
                wrong_type_argument_violation(vm, subr, pos, "opened port", port, argc, argv);          \
                return scm_undef;                                                                       \
            } while (false)

#define CHECK_OPENED_TEXTUAL_INPUT_PORT(pos, subr)                                                      \
            do {                                                                                        \
                assert(PORTP(argv[pos]));                                                               \
                scm_port_t port = (scm_port_t)argv[pos];                                                \
                if (port_open_pred(port)) {                                                             \
                    if (port_input_pred(port)) {                                                        \
                        if (port_textual_pred(port)) break;                                             \
                        wrong_type_argument_violation(vm, subr, pos, "textual port", port, argc, argv); \
                        return scm_undef;                                                               \
                    }                                                                                   \
                    wrong_type_argument_violation(vm, subr, pos, "input port", port, argc, argv);       \
                    return scm_undef;                                                                   \
                }                                                                                       \
                wrong_type_argument_violation(vm, subr, pos, "opened port", port, argc, argv);          \
                return scm_undef;                                                                       \
            } while (false)

#define CHECK_OPENED_TEXTUAL_OUTPUT_PORT(pos, subr)                                                     \
            do {                                                                                        \
                assert(PORTP(argv[pos]));                                                               \
                scm_port_t port = (scm_port_t)argv[pos];                                                \
                if (port_open_pred(port)) {                                                             \
                    if (port_output_pred(port)) {                                                       \
                        if (port_textual_pred(port)) break;                                             \
                        wrong_type_argument_violation(vm, subr, pos, "textual port", port, argc, argv); \
                        return scm_undef;                                                               \
                    }                                                                                   \
                    wrong_type_argument_violation(vm, subr, pos, "output port", port, argc, argv);      \
                    return scm_undef;                                                                   \
                }                                                                                       \
                wrong_type_argument_violation(vm, subr, pos, "opened port", port, argc, argv);          \
                return scm_undef;                                                                       \
            } while (false)

#define CHECK_OPENED_BINARY_INPUT_PORT(pos, subr)                                                       \
            do {                                                                                        \
                assert(PORTP(argv[pos]));                                                               \
                scm_port_t port = (scm_port_t)argv[pos];                                                \
                if (port_open_pred(port)) {                                                             \
                    if (port_input_pred(port)) {                                                        \
                        if (port_binary_pred(port)) break;                                              \
                        wrong_type_argument_violation(vm, subr, pos, "binary port", port, argc, argv);  \
                        return scm_undef;                                                               \
                    }                                                                                   \
                    wrong_type_argument_violation(vm, subr, pos, "input port", port, argc, argv);       \
                    return scm_undef;                                                                   \
                }                                                                                       \
                wrong_type_argument_violation(vm, subr, pos, "opened port", port, argc, argv);          \
                return scm_undef;                                                                       \
            } while (false)

#define CHECK_OPENED_BINARY_OUTPUT_PORT(pos, subr)                                                      \
            do {                                                                                        \
                assert(PORTP(argv[pos]));                                                               \
                scm_port_t port = (scm_port_t)argv[pos];                                                \
                if (port_open_pred(port)) {                                                             \
                    if (port_output_pred(port)) {                                                       \
                        if (port_binary_pred(port)) break;                                              \
                        wrong_type_argument_violation(vm, subr, pos, "binary port", port, argc, argv);  \
                        return scm_undef;                                                               \
                    }                                                                                   \
                    wrong_type_argument_violation(vm, subr, pos, "output port", port, argc, argv);      \
                    return scm_undef;                                                                   \
                }                                                                                       \
                wrong_type_argument_violation(vm, subr, pos, "opened port", port, argc, argv);          \
                return scm_undef;                                                                       \
            } while (false)

#define CHECK_OPENED_PORT(pos, subr)                                                                    \
            do {                                                                                        \
                assert(PORTP(argv[pos]));                                                               \
                scm_port_t port = (scm_port_t)argv[pos];                                                \
                if (port_open_pred(port)) break;                                                        \
                wrong_type_argument_violation(vm, subr, pos, "opened port", port, argc, argv);          \
                return scm_undef;                                                                       \
            } while (false)

#define CHECK_OPENED_BINARY_PORT(pos, subr)                                                             \
            do {                                                                                        \
                assert(PORTP(argv[pos]));                                                               \
                scm_port_t port = (scm_port_t)argv[pos];                                                \
                if (port_open_pred(port)) {                                                             \
                    if (port_binary_pred(port)) break;                                                  \
                    wrong_type_argument_violation(vm, subr, pos, "binary port", port, argc, argv);      \
                    return scm_undef;                                                                   \
                }                                                                                       \
                wrong_type_argument_violation(vm, subr, pos, "opened port", port, argc, argv);          \
                return scm_undef;                                                                       \
            } while (false)

#define CHECK_OUTPUT_PORT(pos, subr)                                                                    \
            do {                                                                                        \
                assert(PORTP(argv[pos]));                                                               \
                scm_port_t port = (scm_port_t)argv[pos];                                                \
                if (port_output_pred(port)) break;                                                      \
                wrong_type_argument_violation(vm, subr, pos, "output port", port, argc, argv);          \
                return scm_undef;                                                                       \
            } while (false)

#define CHECK_NON_NEGATIVE_FIXNUM(pos, subr)                                                                \
            do {                                                                                            \
                scm_obj_t obj = argv[pos];                                                                  \
                if (FIXNUMP(obj) && FIXNUM(obj) >= 0) break;                                                \
                if (exact_non_negative_integer_pred(obj)) {                                                 \
                    invalid_argument_violation(vm, subr, "value out of bounds,", obj, pos, argc, argv);     \
                    return scm_undef;                                                                       \
                }                                                                                           \
                wrong_type_argument_violation(vm, subr, pos, "exact non-negative integer", obj, argc, argv);\
                return scm_undef;                                                                           \
            } while (false)

#define CHECK_OCTET(pos, subr)                                                                              \
            do {                                                                                            \
                scm_obj_t obj = argv[pos];                                                                  \
                if (FIXNUMP(obj) && (FIXNUM(obj) >= 0) && (FIXNUM(obj) <= UINT8_MAX)) break;                \
                wrong_type_argument_violation(vm, subr, pos, "octet", obj, argc, argv);                     \
                return scm_undef;                                                                           \
            } while (false)

#define CONVERT_TO_MACHINE_INT(pos, subr, var)                                                              \
            do {                                                                                            \
                scm_obj_t obj = argv[pos];                                                                  \
                if (exact_integer_pred(obj)) {                                                              \
                    if (exact_integer_to_int(obj, var) == false) {                                          \
                        invalid_argument_violation(vm, subr, "value out of bound,", obj, pos, argc, argv);  \
                        return scm_undef;                                                                   \
                    }                                                                                       \
                } else {                                                                                    \
                    wrong_type_argument_violation(vm, subr, pos, "exact integer", obj, argc, argv);         \
                    return scm_undef;                                                                       \
                }                                                                                           \
            } while (false)

#endif
