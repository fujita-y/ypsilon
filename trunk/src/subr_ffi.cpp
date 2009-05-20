/*
    Ypsilon Scheme System
    Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
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
    if (argc == 0) {
        void* hdl = load_shared_object(NULL);
        if (hdl) return uintptr_to_integer(vm->m_heap, (uintptr_t)hdl);
        invalid_argument_violation(vm, "load-shared-object", last_shared_object_error(), NULL, -1, argc, argv);
        return scm_undef;
    }
    if (argc == 1) {
        if (STRINGP(argv[0])) {
            scm_string_t string = (scm_string_t)argv[0];
            void* hdl = load_shared_object(string);
            if (hdl) return uintptr_to_integer(vm->m_heap, (uintptr_t)hdl);
            invalid_argument_violation(vm, "load-shared-object", last_shared_object_error(), NULL, -1, argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "load-shared-object", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "load-shared-object", 0, 1, argc, argv);
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
            wrong_type_argument_violation(vm, "lookup-shared-object", 0, "shared object handle", argv[0], argc, argv);
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

// shared-object-errno
scm_obj_t
subr_shared_object_errno(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) return int_to_integer(vm->m_heap, vm->m_shared_object_errno);
    if (argc == 1) {
        if (exact_integer_pred(argv[0])) {
            int val;
            if (exact_integer_to_int(argv[0], &val)) {
                errno = val;
                vm->m_shared_object_errno = val;
                return scm_unspecified;
            }
            invalid_argument_violation(vm, "shared-object-errno", "value out of range,", argv[0], 0, argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "shared-object-errno", 0, "exact integer", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "shared-object-errno", 0, 1, argc, argv);
    return scm_undef;
}

// shared-object-win32-last-error
scm_obj_t
subr_shared_object_win32_lasterror(VM* vm, int argc, scm_obj_t argv[])
{
#if _MSC_VER
    if (argc == 0) return int_to_integer(vm->m_heap, vm->m_shared_object_win32_lasterror);
    if (argc == 1) {
        if (exact_integer_pred(argv[0])) {
            if (n_positive_pred(argv[0])) {
                uint32_t val;
                if (exact_integer_to_uint32(argv[0], &val)) {
                    SetLastError(val);
                    vm->m_shared_object_win32_lasterror = val;
                    return scm_unspecified;
                }
            } else {
                int32_t val;
                if (exact_integer_to_int32(argv[0], &val)) {
                    SetLastError(val);
                    vm->m_shared_object_win32_lasterror = val;
                    return scm_unspecified;
                }
            }
            invalid_argument_violation(vm, "shared-object-win32-lasterror", "value out of range,", argv[0], 0, argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "shared-object-win32-lasterror", 0, "exact integer", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "shared-object-win32-lasterror", 0, 1, argc, argv);
    return scm_undef;
#else
    raise_error(vm, "shared-object-win32-last-error", "operating system does not support this feature", 0, argc, argv);
    return scm_undef;
#endif
}

// win32-error->string
scm_obj_t
subr_win32_error_string(VM* vm, int argc, scm_obj_t argv[])
{
#if _MSC_VER
    if (argc == 1) {
        if (exact_integer_pred(argv[0])) {
            uint32_t val;
            if (exact_integer_to_uint32(argv[0], &val)) {
                char* message;
                FormatMessageA(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
                        NULL,
                        val,
                        MAKELANGID(LANG_ENGLISH, SUBLANG_DEFAULT),
                        (LPSTR)&message,
                        0,
                        NULL);
                int tail = strlen(message);
                while (--tail >= 0) {
                    if (message[tail] == '\r' || message[tail] == '\n') {
                        message[tail] = 0;
                        continue;
                    }
                    break;
                }
                scm_string_t obj = make_string(vm->m_heap, message);
                LocalFree(message);
                return obj;
            }
            invalid_argument_violation(vm, "win32-error->string", "value out of range,", argv[0], 0, argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "win32-error->string", 0, "exact integer", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "win32-error->string", 1, 1, argc, argv);
    return scm_undef;
#else
    raise_error(vm, "win32-error->string", "operating system does not support this feature", 0, argc, argv);
    return scm_undef;
#endif
}

// make-callback-trampoline
scm_obj_t
subr_make_callback_trampoline(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 3) {
        if (exact_non_negative_integer_pred(argv[0])) {
            if (STRINGP(argv[1])) {
                const char* signature = ((scm_string_t)argv[1])->name;
                if (CLOSUREP(argv[2])) {
                    return make_callback(vm, FIXNUM(argv[0]), signature, (scm_closure_t)argv[2]);
                }
                wrong_type_argument_violation(vm, "make-callback-trampoline", 2, "closure", argv[2], argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "make-callback-trampoline", 1, "string", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "make-callback-trampoline", 0, "exact non-negative integer", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "make-callback-trampoline", 3, 3, argc, argv);
    return scm_undef;
}

class synchronize_errno {
    VM* m_vm;
public:
    synchronize_errno(VM* vm) {
        m_vm = vm;
        errno = m_vm->m_shared_object_errno;
#if _MSC_VER
        SetLastError(m_vm->m_shared_object_win32_lasterror);
#endif
    }
    ~synchronize_errno() {
        m_vm->m_shared_object_errno = errno;
#if _MSC_VER
        m_vm->m_shared_object_win32_lasterror = GetLastError();
#endif
    }
};

static inline intptr_t
call_c_intptr(VM* vm, void* func, c_stack_frame_t& stack)
{
    synchronize_errno sync(vm);
#if ARCH_IA32
    return c_func_stub_intptr(func, stack.count(), stack.frame());
#elif ARCH_X64
    return c_func_stub_intptr_x64(func, stack.count(), stack.sse_use(), stack.frame());
#elif ARCH_PPC && ARCH_ILP32
    return c_func_stub_intptr_ppc(func, stack.count(), stack.frame());    
#else
    fatal("%s:%u ffi not supported on this build", __FILE__, __LINE__);
#endif
}

static inline int64_t
call_c_int64(VM* vm, void* func, c_stack_frame_t& stack)
{
    synchronize_errno sync(vm);
#if ARCH_IA32
    return c_func_stub_int64(func, stack.count(), stack.frame());
#elif ARCH_X64
    return (int64_t)c_func_stub_intptr_x64(func, stack.count(), stack.sse_use(), stack.frame());
#elif ARCH_PPC && ARCH_ILP32
    return c_func_stub_int64_ppc(func, stack.count(), stack.frame());    
#else
    fatal("%s:%u ffi not supported on this build", __FILE__, __LINE__);
#endif
}


static inline float
call_c_float(VM* vm, void* func, c_stack_frame_t& stack)
{
    synchronize_errno sync(vm);
#if ARCH_IA32
    return c_func_stub_double(func, stack.count(), stack.frame());
#elif ARCH_X64
    return c_func_stub_float_x64(func, stack.count(), stack.sse_use(), stack.frame());
#elif ARCH_PPC && ARCH_ILP32
    return c_func_stub_float_ppc(func, stack.count(), stack.frame());    
#else
    fatal("%s:%u ffi not supported on this build", __FILE__, __LINE__);
#endif
}

static inline double
call_c_double(VM* vm, void* func, c_stack_frame_t& stack)
{
    synchronize_errno sync(vm);
#if ARCH_IA32
    return c_func_stub_double(func, stack.count(), stack.frame());
#elif ARCH_X64
    return c_func_stub_double_x64(func, stack.count(), stack.sse_use(), stack.frame());
#elif ARCH_PPC && ARCH_ILP32
    return c_func_stub_double_ppc(func, stack.count(), stack.frame());    
#else
    fatal("%s:%u ffi not supported on this build", __FILE__, __LINE__);
#endif
}

// call-shared-object
scm_obj_t
subr_call_shared_object(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc >= 1) {
        if (!FIXNUMP(argv[0])) {
            wrong_type_argument_violation(vm, "call-shared-object", 0, "fixnum", argv[0], argc, argv);
            return scm_undef;
        }
        int type = FIXNUM(argv[0]);
        void *func = NULL;
        if (exact_positive_integer_pred(argv[1])) {
            if (exact_integer_to_uintptr(argv[1], (uintptr_t*)&func) == false) {
                invalid_argument_violation(vm, "call-shared-object", "value out of bound,", argv[1], 1, argc, argv);
                return scm_undef;
            }
        } else {
            wrong_type_argument_violation(vm, "call-shared-object", 1, "c function address", argv[1], argc, argv);
            return scm_undef;
        }
        const char* who;
        if (SYMBOLP(argv[2])) {
            who = ((scm_symbol_t)argv[2])->name;
        } else if (argv[2] == scm_false) {
            who = "call-shared-object";
        } else {
            wrong_type_argument_violation(vm, "call-shared-object", 2, "symbol or #f", argv[2], argc, argv);
            return scm_undef;
        }
        const char* signature;
        if (STRINGP(argv[3])) {
            signature = ((scm_string_t)argv[3])->name;
        } else {
            wrong_type_argument_violation(vm, "call-shared-object", 3, "string", argv[3], argc, argv);
            return scm_undef;
        }
        if (argc - 4 <= FFI_MAX_ARGC) {
            c_stack_frame_t stack(vm);
            for (int i = 4; i < argc; i++) {
                const char* err = stack.push(argv[i], signature[0]);
                if (err) {
                    wrong_type_argument_violation(vm, who, i, err, argv[i], argc, argv);
                    return scm_undef;
                }
                if (signature[0] != '*') signature++;
            }
            switch (type & FFI_RETURN_TYPE_MASK) {
                case FFI_RETURN_TYPE_VOID: {
                    call_c_intptr(vm, func, stack);
                    return scm_unspecified;
                }
                case FFI_RETURN_TYPE_BOOL: {
                    intptr_t retval;
                    retval = call_c_intptr(vm, func, stack);
                    return (retval & 0xff) ? MAKEFIXNUM(1) : MAKEFIXNUM(0);
                }
                case FFI_RETURN_TYPE_SHORT: {
                    short retval;
                    retval = (short)call_c_intptr(vm, func, stack);
                    return int_to_integer(vm->m_heap, retval);
                }
                case FFI_RETURN_TYPE_INT: {
                    int retval;
                    retval = (int)call_c_intptr(vm, func, stack);
                    return int_to_integer(vm->m_heap, retval);
                }
                case FFI_RETURN_TYPE_INTPTR: {
                    intptr_t retval;
                    retval = call_c_intptr(vm, func, stack);
                    return intptr_to_integer(vm->m_heap, retval);
                }
                case FFI_RETURN_TYPE_USHORT: {
                    unsigned short retval;
                    retval = (unsigned short)call_c_intptr(vm, func, stack);
                    return uint_to_integer(vm->m_heap, retval);
                }
                case FFI_RETURN_TYPE_UINT: {
                    unsigned int retval;
                    retval = (unsigned int)call_c_intptr(vm, func, stack);
                    return uint_to_integer(vm->m_heap, retval);
                }
                case FFI_RETURN_TYPE_UINTPTR: {
                    uintptr_t retval;
                    retval = (uintptr_t)call_c_intptr(vm, func, stack);
                    return uintptr_to_integer(vm->m_heap, retval);
                }
                case FFI_RETURN_TYPE_FLOAT: {
                    float retval;
                    retval = call_c_float(vm, func, stack);
                    return make_flonum(vm->m_heap, retval);
                }
                case FFI_RETURN_TYPE_DOUBLE: {
                    double retval;
                    retval = call_c_double(vm, func, stack);
                    return make_flonum(vm->m_heap, retval);
                }
                case FFI_RETURN_TYPE_STRING: {
                    char* p;
                    p = (char*)call_c_intptr(vm, func, stack);
                    if (p == NULL) return MAKEFIXNUM(0);
                    return make_string(vm->m_heap, p);
                }
                case FFI_RETURN_TYPE_SIZE_T: {
                    if (sizeof(size_t) == sizeof(int)) {
                        unsigned int retval;
                        retval = (unsigned int)call_c_intptr(vm, func, stack);
                        return uint_to_integer(vm->m_heap, retval);
                    }
                    uintptr_t retval;
                    retval = (uintptr_t)call_c_intptr(vm, func, stack);
                    return uintptr_to_integer(vm->m_heap, retval);
                }
                case FFI_RETURN_TYPE_INT8_T: {
                    int8_t retval;
                    retval = (int8_t)call_c_intptr(vm, func, stack);
                    return int_to_integer(vm->m_heap, retval);
                }
                case FFI_RETURN_TYPE_UINT8_T: {
                    uint8_t retval;
                    retval = (uint8_t)call_c_intptr(vm, func, stack);
                    return uint_to_integer(vm->m_heap, retval);
                }
                case FFI_RETURN_TYPE_INT16_T: {
                    int16_t retval;
                    retval = (int16_t)call_c_intptr(vm, func, stack);
                    return int_to_integer(vm->m_heap, retval);
                }
                case FFI_RETURN_TYPE_UINT16_T: {
                    uint16_t retval;
                    retval = (uint16_t)call_c_intptr(vm, func, stack);
                    return uint_to_integer(vm->m_heap, retval);
                }
                case FFI_RETURN_TYPE_INT32_T: {
                    int32_t retval;
                    retval = (int32_t)call_c_intptr(vm, func, stack);
                    return int_to_integer(vm->m_heap, retval);
                }
                case FFI_RETURN_TYPE_UINT32_T: {
                    uint32_t retval;
                    retval = (uint32_t)call_c_intptr(vm, func, stack);
                    return uint_to_integer(vm->m_heap, retval);
                }
                case FFI_RETURN_TYPE_INT64_T: {
                    int64_t retval;
                    retval = (int64_t)call_c_int64(vm, func, stack);
                    return int64_to_integer(vm->m_heap, retval);
                }
                case FFI_RETURN_TYPE_UINT64_T: {
                    uint64_t retval;
                    retval = (uint64_t)call_c_int64(vm, func, stack);
                    return uint64_to_integer(vm->m_heap, retval);
                }
            }
            invalid_argument_violation(vm, "call-shared-object", "invalid c function return type", argv[0], 0, argc, argv);
            return scm_undef;
        }
        invalid_argument_violation(vm, "call-shared-object", "too many arguments,", MAKEFIXNUM(argc), -1, argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "call-shared-object", 2, -1, argc, argv);
    return scm_undef;
}

void init_subr_ffi(object_heap_t* heap)
{
    #define DEFSUBR(SYM, FUNC)  heap->intern_system_subr(SYM, FUNC)

    DEFSUBR("load-shared-object", subr_load_shared_object);
    DEFSUBR("lookup-shared-object", subr_lookup_shared_object);
    DEFSUBR("call-shared-object", subr_call_shared_object);
    DEFSUBR("make-callback-trampoline", subr_make_callback_trampoline);
    DEFSUBR("shared-object-errno", subr_shared_object_errno);
    DEFSUBR("shared-object-win32-lasterror", subr_shared_object_win32_lasterror);
    DEFSUBR("win32-error->string", subr_win32_error_string);
}
