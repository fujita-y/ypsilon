/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#include "core.h"
#include "vm.h"
#include "ffi.h"
#include "file.h"
#include "heap.h"
#include "subr.h"
#include "arith.h"
#include "violation.h"

// load-shared-object
scm_obj_t
subr_load_shared_object(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (STRINGP(argv[0])) {
            scm_string_t string = (scm_string_t)argv[0];
            void* hdl = load_shared_object(string);
            if (hdl) return uintptr_to_integer(vm->m_heap, (uintptr_t)hdl);
            invalid_argument_violation(vm, "load-shared-object", last_shared_object_error(), NULL, -1, argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "load-shared-object", 0, "string or location", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "load-shared-object", 1, 1, argc, argv);
    return scm_undef;
}

// lookup-shared-object
scm_obj_t
subr_lookup_shared_object(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        void* hdl;
        if (exact_positive_integer_pred(argv[0])) {
            if (exact_integer_to_uintptr(argv[0], (uintptr_t*)&hdl) == false) {
                invalid_argument_violation(vm, "lookup-shared-object", "value out of bound,", argv[0], 0, argc, argv);
                return scm_undef;
            }
        } else {
            wrong_type_argument_violation(vm, "lookup-shared-object", 0, "c-function address", argv[0], argc, argv);
            return scm_undef;
        }
        if (STRINGP(argv[1]) || SYMBOLP(argv[1])) {
            uintptr_t adrs = (uintptr_t)lookup_shared_object(hdl, argv[1]);
            if (adrs == 0) return scm_false;
            return uintptr_to_integer(vm->m_heap, adrs);
        }
        wrong_type_argument_violation(vm, "lookup-shared-object", 1, "string or symbol", argv[1], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "lookup-shared-object", 2, 2, argc, argv);
    return scm_undef;
}

// call-shared-object->void
scm_obj_t
subr_call_shared_object_void(VM* vm, int argc, scm_obj_t argv[])
{
    assert(sizeof(intptr_t) == sizeof(int));
    if (argc >= 1) {
        void *func = NULL;
        if (exact_positive_integer_pred(argv[0])) {
            if (exact_integer_to_uintptr(argv[0], (uintptr_t*)&func) == false) {
                invalid_argument_violation(vm, "call-shared-object->void", "value out of bound,", argv[0], 0, argc, argv);
                return scm_undef;
            }
        } else {
            wrong_type_argument_violation(vm, "call-shared-object->void", 0, "c-function address", argv[0], argc, argv);
            return scm_undef;
        }
        if (argc - 1 <= FFI_MAX_ARGC) {
            c_stack_frame_t stack(vm);
            for (int i = 1; i < argc; i++) {
                const char* err = stack.push(argv[i]);
                if (err) {
                    wrong_type_argument_violation(vm, "call-shared-object->void", i, err, argv[i], argc, argv);
                    return scm_undef;
                }
            }
            c_func_stub_intptr(func, stack.m_count, stack.m_frame);
            return scm_unspecified;
        }
        invalid_argument_violation(vm, "call-shared-object->void", "too many arguments,", MAKEFIXNUM(argc), -1, argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "call-shared-object->void", 1, -1, argc, argv);
    return scm_undef;
}

// call-shared-object->int
scm_obj_t
subr_call_shared_object_int(VM* vm, int argc, scm_obj_t argv[])
{
    assert(sizeof(intptr_t) == sizeof(int));
    if (argc >= 1) {
        void *func = NULL;
        if (exact_positive_integer_pred(argv[0])) {
            if (exact_integer_to_uintptr(argv[0], (uintptr_t*)&func) == false) {
                invalid_argument_violation(vm, "call-shared-object->int", "value out of bound,", argv[0], 0, argc, argv);
                return scm_undef;
            }
        } else {
            wrong_type_argument_violation(vm, "call-shared-object->int", 0, "c-function address", argv[0], argc, argv);
            return scm_undef;
        }
        if (argc - 1 <= FFI_MAX_ARGC) {
            c_stack_frame_t stack(vm);
            for (int i = 1; i < argc; i++) {
                const char* err = stack.push(argv[i]);
                if (err) {
                    wrong_type_argument_violation(vm, "call-shared-object->int", i, err, argv[i], argc, argv);
                    return scm_undef;
                }
            }
            return int_to_integer(vm->m_heap, c_func_stub_intptr(func, stack.m_count, stack.m_frame));
        }
        invalid_argument_violation(vm, "call-shared-object->int", "too many arguments,", MAKEFIXNUM(argc), -1, argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "call-shared-object->int", 1, -1, argc, argv);
    return scm_undef;
}

// call-shared-object->double
scm_obj_t
subr_call_shared_object_double(VM* vm, int argc, scm_obj_t argv[])
{
    assert(sizeof(intptr_t) == sizeof(int));
    if (argc >= 1) {
        void *func = NULL;
        if (exact_positive_integer_pred(argv[0])) {
            if (exact_integer_to_uintptr(argv[0], (uintptr_t*)&func) == false) {
                invalid_argument_violation(vm, "call-shared-object->double", "value out of bound,", argv[0], 0, argc, argv);
                return scm_undef;
            }
        } else {
            wrong_type_argument_violation(vm, "call-shared-object->double", 0, "c-function address", argv[0], argc, argv);
            return scm_undef;
        }
        if (argc - 1 <= FFI_MAX_ARGC) {
            c_stack_frame_t stack(vm);
            for (int i = 1; i < argc; i++) {
                const char* err = stack.push(argv[i]);
                if (err) {
                    wrong_type_argument_violation(vm, "call-shared-object->double", i, err, argv[i], argc, argv);
                    return scm_undef;
                }
            }
            return make_flonum(vm->m_heap, c_func_stub_double(func, stack.m_count, stack.m_frame));
        }
        invalid_argument_violation(vm, "call-shared-object->double", "too many arguments,", MAKEFIXNUM(argc), -1, argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "call-shared-object->double", 1, -1, argc, argv);
    return scm_undef;
}

// call-shared-object->void*
scm_obj_t
subr_call_shared_object_intptr(VM* vm, int argc, scm_obj_t argv[])
{
    assert(sizeof(intptr_t) == sizeof(int));
    if (argc >= 1) {
        void *func = NULL;
        if (exact_positive_integer_pred(argv[0])) {
            if (exact_integer_to_uintptr(argv[0], (uintptr_t*)&func) == false) {
                invalid_argument_violation(vm, "call-shared-object->void*", "value out of bound,", argv[0], 0, argc, argv);
                return scm_undef;
            }
        } else {
            wrong_type_argument_violation(vm, "call-shared-object->void*", 0, "c-function address", argv[0], argc, argv);
            return scm_undef;
        }
        if (argc - 1 <= FFI_MAX_ARGC) {
            c_stack_frame_t stack(vm);
            for (int i = 1; i < argc; i++) {
                const char* err = stack.push(argv[i]);
                if (err) {
                    wrong_type_argument_violation(vm, "call-shared-object->void*", i, err, argv[i], argc, argv);
                    return scm_undef;
                }
            }
            intptr_t value = c_func_stub_intptr(func, stack.m_count, stack.m_frame);
            return intptr_to_integer(vm->m_heap, value);
        }
        invalid_argument_violation(vm, "call-shared-object->void*", "too many arguments,", MAKEFIXNUM(argc), -1, argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "call-shared-object->void*", 1, -1, argc, argv);
    return scm_undef;
}

#if _MSC_VER

    // stdcall-shared-object->void
    scm_obj_t
    subr_stdcall_shared_object_void(VM* vm, int argc, scm_obj_t argv[])
    {
        assert(sizeof(intptr_t) == sizeof(int));
        if (argc >= 1) {
            void *func = NULL;
            if (exact_positive_integer_pred(argv[0])) {
                if (exact_integer_to_uintptr(argv[0], (uintptr_t*)&func) == false) {
                    invalid_argument_violation(vm, "stdcall-shared-object->void", "value out of bound,", argv[0], 0, argc, argv);
                    return scm_undef;
                }
            } else {
                wrong_type_argument_violation(vm, "stdcall-shared-object->void", 0, "c-function address", argv[0], argc, argv);
                return scm_undef;
            }
            if (argc - 1 <= FFI_MAX_ARGC) {
                c_stack_frame_t stack(vm);
                for (int i = 1; i < argc; i++) {
                    const char* err = stack.push(argv[i]);
                    if (err) {
                        wrong_type_argument_violation(vm, "stdcall-shared-object->void", i, err, argv[i], argc, argv);
                        return scm_undef;
                    }
                }
                stdcall_func_stub_intptr(func, stack.m_count, stack.m_frame);
                return scm_unspecified;
            }
            invalid_argument_violation(vm, "stdcall-shared-object->void", "too many arguments,", MAKEFIXNUM(argc), -1, argc, argv);
            return scm_undef;
        }
        wrong_number_of_arguments_violation(vm, "stdcall-shared-object->void", 1, -1, argc, argv);
        return scm_undef;
    }

    // stdcall-shared-object->int
    scm_obj_t
    subr_stdcall_shared_object_int(VM* vm, int argc, scm_obj_t argv[])
    {
        assert(sizeof(intptr_t) == sizeof(int));
        if (argc >= 1) {
            void *func = NULL;
            if (exact_positive_integer_pred(argv[0])) {
                if (exact_integer_to_uintptr(argv[0], (uintptr_t*)&func) == false) {
                    invalid_argument_violation(vm, "stdcall-shared-object->int", "value out of bound,", argv[0], 0, argc, argv);
                    return scm_undef;
                }
            } else {
                wrong_type_argument_violation(vm, "stdcall-shared-object->int", 0, "c-function address", argv[0], argc, argv);
                return scm_undef;
            }
            if (argc - 1 <= FFI_MAX_ARGC) {
                c_stack_frame_t stack(vm);
                for (int i = 1; i < argc; i++) {
                    const char* err = stack.push(argv[i]);
                    if (err) {
                        wrong_type_argument_violation(vm, "stdcall-shared-object->int", i, err, argv[i], argc, argv);
                        return scm_undef;
                    }
                }
                return int_to_integer(vm->m_heap, stdcall_func_stub_intptr(func, stack.m_count, stack.m_frame));
            }
            invalid_argument_violation(vm, "stdcall-shared-object->int", "too many arguments,", MAKEFIXNUM(argc), -1, argc, argv);
            return scm_undef;
        }
        wrong_number_of_arguments_violation(vm, "stdcall-shared-object->int", 1, -1, argc, argv);
        return scm_undef;
    }

    // stdcall-shared-object->double
    scm_obj_t
    subr_stdcall_shared_object_double(VM* vm, int argc, scm_obj_t argv[])
    {
        assert(sizeof(intptr_t) == sizeof(int));
        if (argc >= 1) {
            void *func = NULL;
            if (exact_positive_integer_pred(argv[0])) {
                if (exact_integer_to_uintptr(argv[0], (uintptr_t*)&func) == false) {
                    invalid_argument_violation(vm, "stdcall-shared-object->double", "value out of bound,", argv[0], 0, argc, argv);
                    return scm_undef;
                }
            } else {
                wrong_type_argument_violation(vm, "stdcall-shared-object->double", 0, "c-function address", argv[0], argc, argv);
                return scm_undef;
            }
            if (argc - 1 <= FFI_MAX_ARGC) {
                c_stack_frame_t stack(vm);
                for (int i = 1; i < argc; i++) {
                    const char* err = stack.push(argv[i]);
                    if (err) {
                        wrong_type_argument_violation(vm, "stdcall-shared-object->double", i, err, argv[i], argc, argv);
                        return scm_undef;
                    }
                }
                return make_flonum(vm->m_heap, stdcall_func_stub_double(func, stack.m_count, stack.m_frame));
            }
            invalid_argument_violation(vm, "stdcall-shared-object->double", "too many arguments,", MAKEFIXNUM(argc), -1, argc, argv);
            return scm_undef;
        }
        wrong_number_of_arguments_violation(vm, "stdcall-shared-object->double", 1, -1, argc, argv);
        return scm_undef;
    }

    // stdcall-shared-object->void*
    scm_obj_t
    subr_stdcall_shared_object_intptr(VM* vm, int argc, scm_obj_t argv[])
    {
        assert(sizeof(intptr_t) == sizeof(int));
        if (argc >= 1) {
            void *func = NULL;
            if (exact_positive_integer_pred(argv[0])) {
                if (exact_integer_to_uintptr(argv[0], (uintptr_t*)&func) == false) {
                    invalid_argument_violation(vm, "stdcall-shared-object->void*", "value out of bound,", argv[0], 0, argc, argv);
                    return scm_undef;
                }
            } else {
                wrong_type_argument_violation(vm, "stdcall-shared-object->void*", 0, "c-function address", argv[0], argc, argv);
                return scm_undef;
            }
            if (argc - 1 <= FFI_MAX_ARGC) {
                c_stack_frame_t stack(vm);
                for (int i = 1; i < argc; i++) {
                    const char* err = stack.push(argv[i]);
                    if (err) {
                        wrong_type_argument_violation(vm, "stdcall-shared-object->void*", i, err, argv[i], argc, argv);
                        return scm_undef;
                    }
                }
                intptr_t value = stdcall_func_stub_intptr(func, stack.m_count, stack.m_frame);
                return intptr_to_integer(vm->m_heap, value);
            }
            invalid_argument_violation(vm, "stdcall-shared-object->void*", "too many arguments,", MAKEFIXNUM(argc), -1, argc, argv);
            return scm_undef;
        }
        wrong_number_of_arguments_violation(vm, "stdcall-shared-object->void*", 1, -1, argc, argv);
        return scm_undef;
    }

#endif

// make-callback
scm_obj_t
subr_make_callback(VM* vm, int argc, scm_obj_t argv[])
{
    assert(sizeof(intptr_t) == sizeof(int));
    if (argc == 3) {
        if (exact_non_negative_integer_pred(argv[0])) {
            if (exact_non_negative_integer_pred(argv[1])) {
                if (CLOSUREP(argv[2])) {
                    return make_callback(vm, FIXNUM(argv[0]), FIXNUM(argv[1]), (scm_closure_t)argv[2]);
                }
                wrong_type_argument_violation(vm, "make-callback", 2, "closure", argv[2], argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "make-callback", 1, "exact non-negative integer", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "make-callback", 0, "exact non-negative integer", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "make-callback", 3, 3, argc, argv);
    return scm_undef;
}

// flonum->float
scm_obj_t
subr_flonum_to_float(VM* vm, int argc, scm_obj_t argv[])
{
    assert(sizeof(intptr_t) == sizeof(float));
    if (argc == 1) {
        if (FLONUMP(argv[0])) {
            union {
                float f32;
                uintptr_t u32;
            } n;
            scm_flonum_t flonum = (scm_flonum_t)argv[0];
            n.f32 = flonum->value;
            return intptr_to_integer(vm->m_heap, n.u32);
        }
        wrong_type_argument_violation(vm, "flonum->float", 1, "flonum", argv[1], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "flonum->float", 1, 1, argc, argv);
    return scm_undef;
}

void init_subr_ffi(object_heap_t* heap)
{	
    #define	DEFSUBR(SYM, FUNC)	heap->intern_system_subr(SYM, FUNC)

    DEFSUBR("load-shared-object", subr_load_shared_object);
    DEFSUBR("lookup-shared-object", subr_lookup_shared_object);
    DEFSUBR("call-shared-object->void", subr_call_shared_object_void);
    DEFSUBR("call-shared-object->int", subr_call_shared_object_int);
    DEFSUBR("call-shared-object->double", subr_call_shared_object_double);
    DEFSUBR("call-shared-object->void*", subr_call_shared_object_intptr);
    DEFSUBR("call-shared-object->intptr", subr_call_shared_object_intptr);
#if _MSC_VER
    DEFSUBR("stdcall-shared-object->void", subr_stdcall_shared_object_void);
    DEFSUBR("stdcall-shared-object->int", subr_stdcall_shared_object_int);
    DEFSUBR("stdcall-shared-object->double", subr_stdcall_shared_object_double);
    DEFSUBR("stdcall-shared-object->void*", subr_stdcall_shared_object_intptr);
    DEFSUBR("stdcall-shared-object->intptr", subr_stdcall_shared_object_intptr);
#else
    DEFSUBR("stdcall-shared-object->void", subr_call_shared_object_void);
    DEFSUBR("stdcall-shared-object->int", subr_call_shared_object_int);
    DEFSUBR("stdcall-shared-object->double", subr_call_shared_object_double);
    DEFSUBR("stdcall-shared-object->void*", subr_call_shared_object_intptr);
    DEFSUBR("stdcall-shared-object->intptr", subr_call_shared_object_intptr);
#endif    
    DEFSUBR("make-callback", subr_make_callback);
    DEFSUBR("flonum->float", subr_flonum_to_float);
}
