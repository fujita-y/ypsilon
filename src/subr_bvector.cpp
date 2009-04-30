/*
  Ypsilon Scheme System
  Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
  See license.txt for terms and conditions of use
*/

#include "core.h"
#include "vm.h"
#include "file.h"
#include "fasl.h"
#include "hash.h"
#include "heap.h"
#include "port.h"
#include "subr.h"
#include "ucs4.h"
#include "utf8.h"
#include "arith.h"
#include "equiv.h"
#include "reader.h"
#include "ioerror.h"
#include "printer.h"
#include "violation.h"

#define S_LITTLE    (vm->m_heap->inherent_symbol(S_CODE_LITTLE))
#define S_BIG       (vm->m_heap->inherent_symbol(S_CODE_BIG))

struct mutator_param_t {
    uint8_t*    bytes;
    int         little;
    bool        violation;

    mutator_param_t(const int octets, const char* subr, VM* vm, int argc, scm_obj_t argv[])
    {
        violation = false;
        little = 0;
        if (argc == 4) {
            if (BVECTORP(argv[0])) {
                if (FIXNUMP(argv[1])) {
                    scm_bvector_t bvector = (scm_bvector_t)argv[0];
#if USE_CONST_LITERAL
                    if (HDR_BVECTOR_LITERAL(bvector->hdr)) {
                        literal_constant_access_violation(vm, subr, argv[0], argc, argv);
                        violation = true;
                        return;
                    }
#endif
                    intptr_t offset = FIXNUM(argv[1]);
                    if (offset >= 0 && (offset + octets) <= bvector->count) {
                        bytes = bvector->elts + offset;
                        if (SYMBOLP(argv[3])) {
                            scm_symbol_t symbol = (scm_symbol_t)argv[3];
                            if (symbol == S_LITTLE) {
                                little = 1;
                                return;
                            }
                            if (symbol == S_BIG) return;
                        }
                        wrong_type_argument_violation(vm, subr, 3, "endianness", argv[3], argc, argv);
                        violation = true;
                        return;
                    }
                }
                if (exact_integer_pred(argv[1])) {
                    invalid_argument_violation(vm, subr, "index out of bounds,", argv[1], 1, argc, argv);
                    violation = true;
                    return;
                }
                wrong_type_argument_violation(vm, subr, 1, "exact integer", argv[1], argc, argv);
                violation = true;
                return;
            }
            wrong_type_argument_violation(vm, subr, 0, "bytevector", argv[0], argc, argv);
            violation = true;
            return;
        }
        wrong_number_of_arguments_violation(vm, subr, 4, 4, argc, argv);
        violation = true;
        return;
    }
};

struct native_mutator_param_t {
    uint8_t*    bytes;
    bool        violation;

    native_mutator_param_t(const int octets, const char* subr, VM* vm, int argc, scm_obj_t argv[])
    {
        violation = false;
        if (argc == 3) {
            if (BVECTORP(argv[0])) {
                if (FIXNUMP(argv[1])) {
                    scm_bvector_t bvector = (scm_bvector_t)argv[0];
#if USE_CONST_LITERAL
                    if (HDR_BVECTOR_LITERAL(bvector->hdr)) {
                        literal_constant_access_violation(vm, subr, argv[0], argc, argv);
                        violation = true;
                        return;
                    }
#endif
                    intptr_t offset = FIXNUM(argv[1]);
                    if (offset >= 0 && (offset + octets) <= bvector->count) {
                        if ((offset & (octets - 1)) == 0) {
                            bytes = bvector->elts + offset;
                            return;
                        }
                        invalid_argument_violation(vm, subr, "index not aligned on native boundary,", argv[1], 1, argc, argv);
                        violation = true;
                        return;
                    }
                }
                if (exact_integer_pred(argv[1])) {
                    invalid_argument_violation(vm, subr, "index out of bounds,", argv[1], 1, argc, argv);
                    violation = true;
                    return;
                }
                wrong_type_argument_violation(vm, subr, 1, "exact integer", argv[1], argc, argv);
                violation = true;
                return;
            }
            wrong_type_argument_violation(vm, subr, 0, "bytevector", argv[0], argc, argv);
            violation = true;
            return;
        }
        wrong_number_of_arguments_violation(vm, subr, 4, 4, argc, argv);
        violation = true;
        return;
    }
};

struct c_mutator_param_t {
    uint8_t*    bytes;
    bool        violation;

    c_mutator_param_t(const int octets, const char* subr, VM* vm, int argc, scm_obj_t argv[])
    {
        violation = false;
        if (argc == 3) {
            if (BVECTORP(argv[0])) {
                if (FIXNUMP(argv[1])) {
                    scm_bvector_t bvector = (scm_bvector_t)argv[0];
#if USE_CONST_LITERAL
                    if (HDR_BVECTOR_LITERAL(bvector->hdr)) {
                        literal_constant_access_violation(vm, subr, argv[0], argc, argv);
                        violation = true;
                        return;
                    }
#endif
                    intptr_t offset = FIXNUM(argv[1]);
                    if (offset >= 0 && (offset + octets) <= bvector->count) {
                        bytes = bvector->elts + offset;
                        return;
                    }
                }
                if (exact_integer_pred(argv[1])) {
                    invalid_argument_violation(vm, subr, "index out of bounds,", argv[1], 1, argc, argv);
                    violation = true;
                    return;
                }
                wrong_type_argument_violation(vm, subr, 1, "exact integer", argv[1], argc, argv);
                violation = true;
                return;
            }
            wrong_type_argument_violation(vm, subr, 0, "bytevector", argv[0], argc, argv);
            violation = true;
            return;
        }
        wrong_number_of_arguments_violation(vm, subr, 4, 4, argc, argv);
        violation = true;
        return;
    }
};

struct accessor_param_t {
    uint8_t*    bytes;
    int         little;
    bool        violation;

    accessor_param_t(const int octets, const char* subr, VM* vm, int argc, scm_obj_t argv[])
    {
        violation = false;
        little = 0;
        if (argc == 3) {
            if (BVECTORP(argv[0])) {
                if (FIXNUMP(argv[1])) {
                    scm_bvector_t bvector = (scm_bvector_t)argv[0];
                    intptr_t offset = FIXNUM(argv[1]);
                    if (offset >= 0 && (offset + octets) <= bvector->count) {
                        bytes = bvector->elts + offset;
                        if (SYMBOLP(argv[2])) {
                            scm_symbol_t symbol = (scm_symbol_t)argv[2];
                            if (symbol == S_LITTLE) {
                                little = 1;
                                return;
                            }
                            if (symbol == S_BIG) return;
                        }
                        wrong_type_argument_violation(vm, subr, 2, "endianness", argv[2], argc, argv);
                        violation = true;
                        return;
                    }
                }
                if (exact_integer_pred(argv[1])) {
                    invalid_argument_violation(vm, subr, "index out of bounds,", argv[1], 1, argc, argv);
                    violation = true;
                    return;
                }
                wrong_type_argument_violation(vm, subr, 1, "exact integer", argv[1], argc, argv);
                violation = true;
                return;
            }
            wrong_type_argument_violation(vm, subr, 0, "bytevector", argv[0], argc, argv);
            violation = true;
            return;
        }
        wrong_number_of_arguments_violation(vm, subr, 3, 3, argc, argv);
        violation = true;
        return;
    }
};

struct native_accessor_param_t {
    uint8_t*    bytes;
    bool        violation;

    native_accessor_param_t(const int octets, const char* subr, VM* vm, int argc, scm_obj_t argv[])
    {
        violation = false;
        if (argc == 2) {
            if (BVECTORP(argv[0])) {
                if (FIXNUMP(argv[1])) {
                    scm_bvector_t bvector = (scm_bvector_t)argv[0];
                    intptr_t offset = FIXNUM(argv[1]);
                    if (offset >= 0 && (offset + octets) <= bvector->count) {
                        if ((offset & (octets - 1)) == 0) {
                            bytes = bvector->elts + offset;
                            return;
                        }
                        invalid_argument_violation(vm, subr, "index not aligned on native boundary,", argv[1], 1, argc, argv);
                        violation = true;
                        return;
                    }
                }
                if (exact_integer_pred(argv[1])) {
                    invalid_argument_violation(vm, subr, "index out of bounds,", argv[1], 1, argc, argv);
                    violation = true;
                    return;
                }
                wrong_type_argument_violation(vm, subr, 1, "exact integer", argv[1], argc, argv);
                violation = true;
                return;
            }
            wrong_type_argument_violation(vm, subr, 0, "bytevector", argv[0], argc, argv);
            violation = true;
            return;
        }
        wrong_number_of_arguments_violation(vm, subr, 2, 2, argc, argv);
        violation = true;
        return;
    }
};

struct c_accessor_param_t {
    uint8_t*    bytes;
    bool        violation;

    c_accessor_param_t(const int octets, const char* subr, VM* vm, int argc, scm_obj_t argv[])
    {
        violation = false;
        if (argc == 2) {
            if (BVECTORP(argv[0])) {
                if (FIXNUMP(argv[1])) {
                    scm_bvector_t bvector = (scm_bvector_t)argv[0];
                    intptr_t offset = FIXNUM(argv[1]);
                    if (offset >= 0 && (offset + octets) <= bvector->count) {
                        bytes = bvector->elts + offset;
                        return;
                    }
                }
                if (exact_integer_pred(argv[1])) {
                    invalid_argument_violation(vm, subr, "index out of bounds,", argv[1], 1, argc, argv);
                    violation = true;
                    return;
                }
                wrong_type_argument_violation(vm, subr, 1, "exact integer", argv[1], argc, argv);
                violation = true;
                return;
            }
            wrong_type_argument_violation(vm, subr, 0, "bytevector", argv[0], argc, argv);
            violation = true;
            return;
        }
        wrong_number_of_arguments_violation(vm, subr, 2, 2, argc, argv);
        violation = true;
        return;
    }
};

static inline uint64_t LE64(uint8_t* bytes)
{
    return ((uint64_t)bytes[0])
         + ((uint64_t)bytes[1] << 8)
         + ((uint64_t)bytes[2] << 16)
         + ((uint64_t)bytes[3] << 24)
         + ((uint64_t)bytes[4] << 32)
         + ((uint64_t)bytes[5] << 40)
         + ((uint64_t)bytes[6] << 48)
         + ((uint64_t)bytes[7] << 56);
}

static inline uint64_t BE64(uint8_t* bytes)
{
    return ((uint64_t)bytes[0] << 56)
         + ((uint64_t)bytes[1] << 48)
         + ((uint64_t)bytes[2] << 40)
         + ((uint64_t)bytes[3] << 32)
         + ((uint64_t)bytes[4] << 24)
         + ((uint64_t)bytes[5] << 16)
         + ((uint64_t)bytes[6] << 8)
         + ((uint64_t)bytes[7]);
}

static inline uint32_t LE32(uint8_t* bytes)
{
    return ((uint32_t)bytes[0])
         + ((uint32_t)bytes[1] << 8)
         + ((uint32_t)bytes[2] << 16)
         + ((uint32_t)bytes[3] << 24);
}

static inline uint32_t BE32(uint8_t* bytes)
{
    return ((uint32_t)bytes[0] << 24)
         + ((uint32_t)bytes[1] << 16)
         + ((uint32_t)bytes[2] << 8)
         + ((uint32_t)bytes[3]);
}

static inline uint16_t LE16(uint8_t* bytes)
{
    return ((uint16_t)bytes[0])
         + ((uint16_t)bytes[1] << 8);
}

static inline uint16_t BE16(uint8_t* bytes)
{
    return ((uint16_t)bytes[0] << 8)
         + ((uint16_t)bytes[1]);
}

static inline void LE64(uint64_t u64, uint8_t* bytes)
{
    bytes[0] = u64;
    bytes[1] = u64 >> 8;
    bytes[2] = u64 >> 16;
    bytes[3] = u64 >> 24;
    bytes[4] = u64 >> 32;
    bytes[5] = u64 >> 40;
    bytes[6] = u64 >> 48;
    bytes[7] = u64 >> 56;
}

static inline void BE64(uint64_t u64, uint8_t* bytes)
{
    bytes[7] = u64;
    bytes[6] = u64 >> 8;
    bytes[5] = u64 >> 16;
    bytes[4] = u64 >> 24;
    bytes[3] = u64 >> 32;
    bytes[2] = u64 >> 40;
    bytes[1] = u64 >> 48;
    bytes[0] = u64 >> 56;
}

static inline void LE32(uint32_t u32, uint8_t* bytes)
{
    bytes[0] = u32;
    bytes[1] = u32 >> 8;
    bytes[2] = u32 >> 16;
    bytes[3] = u32 >> 24;
}

static inline void BE32(uint32_t u32, uint8_t* bytes)
{
    bytes[3] = u32;
    bytes[2] = u32 >> 8;
    bytes[1] = u32 >> 16;
    bytes[0] = u32 >> 24;
}

static inline void LE16(uint16_t u16, uint8_t* bytes)
{
    bytes[0] = u16;
    bytes[1] = u16 >> 8;
}

static inline void BE16(uint16_t u16, uint8_t* bytes)
{
    bytes[1] = u16;
    bytes[0] = u16 >> 8;
}

// bytevector-s8-ref
scm_obj_t
subr_bytevector_s8_ref(VM* vm, int argc, scm_obj_t argv[])
{
    native_accessor_param_t param(1, "bytevector-s8-ref", vm, argc, argv);
    if (param.violation) return scm_undef;
    return MAKEFIXNUM((int8_t)param.bytes[0]);
}

// bytevector-u8-set!
scm_obj_t
subr_bytevector_u8_set(VM* vm, int argc, scm_obj_t argv[])
{
    native_mutator_param_t param(1, "bytevector-u8-set!", vm, argc, argv);
    if (param.violation) return scm_undef;
    if (FIXNUMP(argv[2])) {
        intptr_t u8 = FIXNUM(argv[2]);
        if (u8 >= 0 && u8 <= UINT8_MAX) {
            param.bytes[0] = u8;
            return scm_unspecified;
        }
    }
    if (exact_integer_pred(argv[2])) {
        invalid_argument_violation(vm, "bytevector-u8-set!", "value out of range,", argv[2], 2, argc, argv);
        return scm_undef;
    }
    wrong_type_argument_violation(vm, "bytevector-u8-set!", 2, "exact integer", argv[2], argc, argv);
    return scm_undef;
}

// bytevector-s8-set!
scm_obj_t
subr_bytevector_s8_set(VM* vm, int argc, scm_obj_t argv[])
{
    native_mutator_param_t param(1, "bytevector-s8-set!", vm, argc, argv);
    if (param.violation) return scm_undef;
    if (FIXNUMP(argv[2])) {
        intptr_t u8 = FIXNUM(argv[2]);
        if (u8 >= INT8_MIN && u8 <= INT8_MAX) {
            param.bytes[0] = u8;
            return scm_unspecified;
        }
    }
    if (exact_integer_pred(argv[2])) {
        invalid_argument_violation(vm, "bytevector-s8-set!", "value out of range,", argv[2], 2, argc, argv);
        return scm_undef;
    }
    wrong_type_argument_violation(vm, "bytevector-s8-set!", 2, "exact integer", argv[2], argc, argv);
    return scm_undef;
}

// bytevector-u16-ref
scm_obj_t
subr_bytevector_u16_ref(VM* vm, int argc, scm_obj_t argv[])
{
    accessor_param_t param(2, "bytevector-u16-ref", vm, argc, argv);
    if (param.violation) return scm_undef;
    if (param.little) return MAKEFIXNUM(LE16(param.bytes));
    return MAKEFIXNUM(BE16(param.bytes));
}

// bytevector-s16-ref
scm_obj_t
subr_bytevector_s16_ref(VM* vm, int argc, scm_obj_t argv[])
{
    accessor_param_t param(2, "bytevector-s16-ref", vm, argc, argv);
    if (param.violation) return scm_undef;
    if (param.little) return MAKEFIXNUM((int16_t)LE16(param.bytes));
    return MAKEFIXNUM((int16_t)BE16(param.bytes));
}

// bytevector-u16-native-ref
scm_obj_t
subr_bytevector_u16_native_ref(VM* vm, int argc, scm_obj_t argv[])
{
    native_accessor_param_t param(2, "bytevector-u16-native-ref", vm, argc, argv);
    if (param.violation) return scm_undef;
    return MAKEFIXNUM(*(uint16_t*)param.bytes);
}

// bytevector-s16-native-ref
scm_obj_t
subr_bytevector_s16_native_ref(VM* vm, int argc, scm_obj_t argv[])
{
    native_accessor_param_t param(2, "bytevector-s16-native-ref", vm, argc, argv);
    if (param.violation) return scm_undef;
    return MAKEFIXNUM(*(int16_t*)param.bytes);
}

// bytevector-u16-set!
scm_obj_t
subr_bytevector_u16_set(VM* vm, int argc, scm_obj_t argv[])
{
    mutator_param_t param(2, "bytevector-u16-set!", vm, argc, argv);
    if (param.violation) return scm_undef;
    if (FIXNUMP(argv[2])) {
        intptr_t u16 = FIXNUM(argv[2]);
        if (u16 >= 0 && u16 <= UINT16_MAX) {
            if (param.little) LE16((uint16_t)u16, param.bytes);
            else BE16((uint16_t)u16, param.bytes);
            return scm_unspecified;
        }
    }
    if (exact_integer_pred(argv[2])) {
        invalid_argument_violation(vm, "bytevector-u16-set!", "value out of range,", argv[2], 2, argc, argv);
        return scm_undef;
    }
    wrong_type_argument_violation(vm, "bytevector-u16-set!", 2, "exact integer", argv[2], argc, argv);
    return scm_undef;
}

// bytevector-s16-set!
scm_obj_t
subr_bytevector_s16_set(VM* vm, int argc, scm_obj_t argv[])
{
    mutator_param_t param(2, "bytevector-s16-set!", vm, argc, argv);
    if (param.violation) return scm_undef;
    if (FIXNUMP(argv[2])) {
        intptr_t s16 = FIXNUM(argv[2]);
        if (s16 >= INT16_MIN && s16 <= INT16_MAX) {
            if (param.little) LE16((uint16_t)s16, param.bytes);
            else BE16((uint16_t)s16, param.bytes);
            return scm_unspecified;
        }
    }
    if (exact_integer_pred(argv[2])) {
        invalid_argument_violation(vm, "bytevector-s16-set!", "value out of range,", argv[2], 2, argc, argv);
        return scm_undef;
    }
    wrong_type_argument_violation(vm, "bytevector-s16-set!", 2, "exact integer", argv[2], argc, argv);
    return scm_undef;
}

// bytevector-u16-native-set!
scm_obj_t
subr_bytevector_u16_native_set(VM* vm, int argc, scm_obj_t argv[])
{
    native_mutator_param_t param(2, "bytevector-u16-native-set!", vm, argc, argv);
    if (param.violation) return scm_undef;
    if (FIXNUMP(argv[2])) {
        intptr_t u16 = FIXNUM(argv[2]);
        if (u16 >= 0 && u16 <= UINT16_MAX) {
            *(uint16_t*)param.bytes = u16;
            return scm_unspecified;
        }
    }
    if (exact_integer_pred(argv[2])) {
        invalid_argument_violation(vm, "bytevector-u16-native-set!", "value out of range,", argv[2], 2, argc, argv);
        return scm_undef;
    }
    wrong_type_argument_violation(vm, "bytevector-u16-native-set!", 2, "exact integer", argv[2], argc, argv);
    return scm_undef;
}

// bytevector-s16-native-set!
scm_obj_t
subr_bytevector_s16_native_set(VM* vm, int argc, scm_obj_t argv[])
{
    native_mutator_param_t param(2, "bytevector-s16-native-set!", vm, argc, argv);
    if (param.violation) return scm_undef;
    if (FIXNUMP(argv[2])) {
        intptr_t s16 = FIXNUM(argv[2]);
        if (s16 >= INT16_MIN && s16 <= INT16_MAX) {
            *(int16_t*)param.bytes = s16;
            return scm_unspecified;
        }
    }
    if (exact_integer_pred(argv[2])) {
        invalid_argument_violation(vm, "bytevector-s16-native-set!", "value out of range,", argv[2], 2, argc, argv);
        return scm_undef;
    }
    wrong_type_argument_violation(vm, "bytevector-s16-native-set!", 2, "exact integer", argv[2], argc, argv);
    return scm_undef;
}

// bytevector-u32-ref
scm_obj_t
subr_bytevector_u32_ref(VM* vm, int argc, scm_obj_t argv[])
{
    accessor_param_t param(4, "bytevector-u32-ref", vm, argc, argv);
    if (param.violation) return scm_undef;
    return uint32_to_integer(vm->m_heap, (param.little ? LE32(param.bytes) : BE32(param.bytes)));
}

// bytevector-s32-ref
scm_obj_t
subr_bytevector_s32_ref(VM* vm, int argc, scm_obj_t argv[])
{
    accessor_param_t param(4, "bytevector-s32-ref", vm, argc, argv);
    if (param.violation) return scm_undef;
    if (param.little) return int32_to_integer(vm->m_heap, (int32_t)LE32(param.bytes));
    return int32_to_integer(vm->m_heap, (int32_t)(param.little ? LE32(param.bytes) : BE32(param.bytes)));
}

// bytevector-u32-native-ref
scm_obj_t
subr_bytevector_u32_native_ref(VM* vm, int argc, scm_obj_t argv[])
{
    native_accessor_param_t param(4, "bytevector-u32-native-ref", vm, argc, argv);
    if (param.violation) return scm_undef;
    return uint32_to_integer(vm->m_heap, *(uint32_t*)param.bytes);
}

// bytevector-s32-native-ref
scm_obj_t
subr_bytevector_s32_native_ref(VM* vm, int argc, scm_obj_t argv[])
{
    native_accessor_param_t param(4, "bytevector-s32-native-ref", vm, argc, argv);
    if (param.violation) return scm_undef;
    return int32_to_integer(vm->m_heap, *(int32_t*)param.bytes);
}

// bytevector-u32-set!
scm_obj_t
subr_bytevector_u32_set(VM* vm, int argc, scm_obj_t argv[])
{
    mutator_param_t param(4, "bytevector-u32-set!", vm, argc, argv);
    if (param.violation) return scm_undef;
    uint32_t u32;
    if (exact_integer_to_uint32(argv[2], &u32)) {
        if (param.little) LE32(u32, param.bytes);
        else BE32(u32, param.bytes);
        return scm_unspecified;
    }
    if (exact_integer_pred(argv[2])) {
        invalid_argument_violation(vm, "bytevector-u32-set!", "value out of range,", argv[2], 2, argc, argv);
        return scm_undef;
    }
    wrong_type_argument_violation(vm, "bytevector-u32-set!", 2, "exact integer", argv[2], argc, argv);
    return scm_undef;
}

// bytevector-s32-set!
scm_obj_t
subr_bytevector_s32_set(VM* vm, int argc, scm_obj_t argv[])
{
    mutator_param_t param(4, "bytevector-s32-set!", vm, argc, argv);
    if (param.violation) return scm_undef;
    int32_t s32;
    if (exact_integer_to_int32(argv[2], &s32)) {
        if (param.little) LE32((uint32_t)s32, param.bytes);
        else BE32((uint32_t)s32, param.bytes);
        return scm_unspecified;
    }
    if (exact_integer_pred(argv[2])) {
        invalid_argument_violation(vm, "bytevector-s32-set!", "value out of range,", argv[2], 2, argc, argv);
        return scm_undef;
    }
    wrong_type_argument_violation(vm, "bytevector-s32-set!", 2, "exact integer", argv[2], argc, argv);
    return scm_undef;
}

// bytevector-u32-native-set!
scm_obj_t
subr_bytevector_u32_native_set(VM* vm, int argc, scm_obj_t argv[])
{
    native_mutator_param_t param(4, "bytevector-u32-native-set!", vm, argc, argv);
    if (param.violation) return scm_undef;
    uint32_t u32;
    if (exact_integer_to_uint32(argv[2], &u32)) {
        *(uint32_t*)param.bytes = u32;
        return scm_unspecified;
    }
    if (exact_integer_pred(argv[2])) {
        invalid_argument_violation(vm, "bytevector-u32-native-set!", "value out of range,", argv[2], 2, argc, argv);
        return scm_undef;
    }
    wrong_type_argument_violation(vm, "bytevector-u32-native-set!", 2, "exact integer", argv[2], argc, argv);
    return scm_undef;
}

// bytevector-s32-native-set!
scm_obj_t
subr_bytevector_s32_native_set(VM* vm, int argc, scm_obj_t argv[])
{
    native_mutator_param_t param(4, "bytevector-s32-native-set!", vm, argc, argv);
    if (param.violation) return scm_undef;
    int32_t s32;
    if (exact_integer_to_int32(argv[2], &s32)) {
        *(int32_t*)param.bytes = s32;
        return scm_unspecified;
    }
    if (exact_integer_pred(argv[2])) {
        invalid_argument_violation(vm, "bytevector-s32-native-set!", "value out of range,", argv[2], 2, argc, argv);
        return scm_undef;
    }
    wrong_type_argument_violation(vm, "bytevector-s32-native-set!", 2, "exact integer", argv[2], argc, argv);
    return scm_undef;
}

// bytevector-u64-ref
scm_obj_t
subr_bytevector_u64_ref(VM* vm, int argc, scm_obj_t argv[])
{
    accessor_param_t param(8, "bytevector-u64-ref", vm, argc, argv);
    if (param.violation) return scm_undef;
    return uint64_to_integer(vm->m_heap, (param.little ? LE64(param.bytes) : BE64(param.bytes)));
}

// bytevector-s64-ref
scm_obj_t
subr_bytevector_s64_ref(VM* vm, int argc, scm_obj_t argv[])
{
    accessor_param_t param(8, "bytevector-s64-ref", vm, argc, argv);
    if (param.violation) return scm_undef;
    return int64_to_integer(vm->m_heap, (int64_t)(param.little ? LE64(param.bytes) : BE64(param.bytes)));
}

// bytevector-u64-native-ref
scm_obj_t
subr_bytevector_u64_native_ref(VM* vm, int argc, scm_obj_t argv[])
{
    native_accessor_param_t param(8, "bytevector-u64-native-ref", vm, argc, argv);
    if (param.violation) return scm_undef;
    return uint64_to_integer(vm->m_heap, *(uint64_t*)param.bytes);
}

// bytevector-s64-native-ref
scm_obj_t
subr_bytevector_s64_native_ref(VM* vm, int argc, scm_obj_t argv[])
{
    native_accessor_param_t param(8, "bytevector-s64-native-ref", vm, argc, argv);
    if (param.violation) return scm_undef;
    return int64_to_integer(vm->m_heap, *(int64_t*)param.bytes);
}

// bytevector-u64-set!
scm_obj_t
subr_bytevector_u64_set(VM* vm, int argc, scm_obj_t argv[])
{
    mutator_param_t param(8, "bytevector-u64-set!", vm, argc, argv);
    if (param.violation) return scm_undef;
    uint64_t u64;
    if (exact_integer_to_uint64(argv[2], &u64)) {
        if (param.little) LE64(u64, param.bytes);
        else BE64(u64, param.bytes);
        return scm_unspecified;
    }
    if (exact_integer_pred(argv[2])) {
        invalid_argument_violation(vm, "bytevector-u64-set!", "value out of range,", argv[2], 2, argc, argv);
        return scm_undef;
    }
    wrong_type_argument_violation(vm, "bytevector-u64-set!", 2, "exact integer", argv[2], argc, argv);
    return scm_undef;
}

// bytevector-s64-set!
scm_obj_t
subr_bytevector_s64_set(VM* vm, int argc, scm_obj_t argv[])
{
    mutator_param_t param(8, "bytevector-s64-set!", vm, argc, argv);
    if (param.violation) return scm_undef;
    int64_t s64;
    if (exact_integer_to_int64(argv[2], &s64)) {
        if (param.little) LE64((uint64_t)s64, param.bytes);
        else BE32((uint64_t)s64, param.bytes);
        return scm_unspecified;
    }
    if (exact_integer_pred(argv[2])) {
        invalid_argument_violation(vm, "bytevector-s64-set!", "value out of range,", argv[2], 2, argc, argv);
        return scm_undef;
    }
    wrong_type_argument_violation(vm, "bytevector-s64-set!", 2, "exact integer", argv[2], argc, argv);
    return scm_undef;
}

// bytevector-u64-native-set!
scm_obj_t
subr_bytevector_u64_native_set(VM* vm, int argc, scm_obj_t argv[])
{
    native_mutator_param_t param(8, "bytevector-u64-native-set!", vm, argc, argv);
    if (param.violation) return scm_undef;
    uint64_t u64;
    if (exact_integer_to_uint64(argv[2], &u64)) {
        *(uint64_t*)param.bytes = u64;
        return scm_unspecified;
    }
    if (exact_integer_pred(argv[2])) {
        invalid_argument_violation(vm, "bytevector-u64-native-set!", "value out of range,", argv[2], 2, argc, argv);
        return scm_undef;
    }
    wrong_type_argument_violation(vm, "bytevector-u64-native-set!", 2, "exact integer", argv[2], argc, argv);
    return scm_undef;
}

// bytevector-s64-native-set!
scm_obj_t
subr_bytevector_s64_native_set(VM* vm, int argc, scm_obj_t argv[])
{
    native_mutator_param_t param(8, "bytevector-s64-native-set!", vm, argc, argv);
    if (param.violation) return scm_undef;
    int64_t s64;
    if (exact_integer_to_int64(argv[2], &s64)) {
        *(int64_t*)param.bytes = s64;
        return scm_unspecified;
    }
    if (exact_integer_pred(argv[2])) {
        invalid_argument_violation(vm, "bytevector-s64-native-set!", "value out of range,", argv[2], 2, argc, argv);
        return scm_undef;
    }
    wrong_type_argument_violation(vm, "bytevector-s64-native-set!", 2, "exact integer", argv[2], argc, argv);
    return scm_undef;
}

// bytevector-ieee-single-ref
scm_obj_t
subr_bytevector_ieee_single_ref(VM* vm, int argc, scm_obj_t argv[])
{
    union { float ieee_single; uint8_t bytes[4]; } datum;
    accessor_param_t param(4, "bytevector-ieee-single-ref", vm, argc, argv);
    if (param.violation) return scm_undef;
    if (param.little == ARCH_LITTLE_ENDIAN) {
        for (int i = 0; i < 4; i++) datum.bytes[i] = param.bytes[i];
        return make_flonum(vm->m_heap, datum.ieee_single);
    } else {
        for (int i = 0; i < 4; i++) datum.bytes[i] = param.bytes[4 - 1 - i];
        return make_flonum(vm->m_heap, datum.ieee_single);
    }
}

// bytevector-ieee-single-native-ref
scm_obj_t
subr_bytevector_ieee_single_native_ref(VM* vm, int argc, scm_obj_t argv[])
{
    native_accessor_param_t param(4, "bytevector-ieee-single-native-ref", vm, argc, argv);
    if (param.violation) return scm_undef;
    return double_to_inexact(vm->m_heap, *(float*)param.bytes);
}

// bytevector-ieee-single-set!
scm_obj_t
subr_bytevector_ieee_single_set(VM* vm, int argc, scm_obj_t argv[])
{
    union { float ieee_single; uint8_t bytes[4]; } datum;
    mutator_param_t param(4, "bytevector-ieee-single-set!", vm, argc, argv);
    if (param.violation) return scm_undef;
    if (real_pred(argv[2])) {
        datum.ieee_single = real_to_double(argv[2]);
        if (param.little == ARCH_LITTLE_ENDIAN) {
            for (int i = 0; i < 4; i++) param.bytes[i] = datum.bytes[i];
            return scm_unspecified;
        }
        for (int i = 0; i < 4; i++) param.bytes[i] = datum.bytes[4 - 1 - i];
        return scm_unspecified;
    }
    wrong_type_argument_violation(vm, "bytevector-ieee-single-set!", 2, "real", argv[2], argc, argv);
    return scm_undef;
}

// bytevector-ieee-single-native-set!
scm_obj_t
subr_bytevector_ieee_single_native_set(VM* vm, int argc, scm_obj_t argv[])
{
    native_mutator_param_t param(4, "bytevector-ieee-single-native-set!", vm, argc, argv);
    if (param.violation) return scm_undef;
    if (real_pred(argv[2])) {
        *(float*)param.bytes = real_to_double(argv[2]);
        return scm_unspecified;
    }
    wrong_type_argument_violation(vm, "bytevector-ieee-single-native-set!", 2, "real", argv[2], argc, argv);
    return scm_undef;
}

// bytevector-ieee-double-ref
scm_obj_t
subr_bytevector_ieee_double_ref(VM* vm, int argc, scm_obj_t argv[])
{
    union { double ieee_double; uint8_t bytes[8]; } datum;
    accessor_param_t param(8, "bytevector-ieee-double-ref", vm, argc, argv);
    if (param.violation) return scm_undef;
    if (param.little == ARCH_LITTLE_ENDIAN) {
        for (int i = 0; i < 8; i++) datum.bytes[i] = param.bytes[i];
        return make_flonum(vm->m_heap, datum.ieee_double);
    } else {
        for (int i = 0; i < 8; i++) datum.bytes[i] = param.bytes[8 - 1 - i];
        return make_flonum(vm->m_heap, datum.ieee_double);
    }
}

// bytevector-ieee-double-native-ref
scm_obj_t
subr_bytevector_ieee_double_native_ref(VM* vm, int argc, scm_obj_t argv[])
{
    native_accessor_param_t param(8, "bytevector-ieee-double-native-ref", vm, argc, argv);
    if (param.violation) return scm_undef;
    return double_to_inexact(vm->m_heap, *(double*)param.bytes);
}

// bytevector-ieee-double-set!
scm_obj_t
subr_bytevector_ieee_double_set(VM* vm, int argc, scm_obj_t argv[])
{
    union { double ieee_double; uint8_t bytes[8]; } datum;
    mutator_param_t param(8, "bytevector-ieee-double-set!", vm, argc, argv);
    if (param.violation) return scm_undef;
    if (real_pred(argv[2])) {
        datum.ieee_double = real_to_double(argv[2]);
        if (param.little == ARCH_LITTLE_ENDIAN) {
            for (int i = 0; i < 8; i++) param.bytes[i] = datum.bytes[i];
            return scm_unspecified;
        }
        for (int i = 0; i < 8; i++) param.bytes[i] = datum.bytes[8 - 1 - i];
        return scm_unspecified;
    }
    wrong_type_argument_violation(vm, "bytevector-ieee-double-set!", 2, "real", argv[2], argc, argv);
    return scm_undef;
}

// bytevector-ieee-double-native-set!
scm_obj_t
subr_bytevector_ieee_double_native_set(VM* vm, int argc, scm_obj_t argv[])
{
    native_mutator_param_t param(8, "bytevector-ieee-double-native-set!", vm, argc, argv);
    if (param.violation) return scm_undef;
    if (real_pred(argv[2])) {
        *(double*)param.bytes = real_to_double(argv[2]);
        return scm_unspecified;
    }
    wrong_type_argument_violation(vm, "bytevector-ieee-double-native-set!", 2, "real", argv[2], argc, argv);
    return scm_undef;
}

// native-endianness
scm_obj_t
subr_native_endianness(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) return ARCH_LITTLE_ENDIAN ? S_LITTLE : S_BIG;
    wrong_number_of_arguments_violation(vm, "native-endianness", 0, 0, argc, argv);
    return scm_undef;
}

// bytevector?
scm_obj_t
subr_bytevector_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) return BVECTORP(argv[0]) ? scm_true : scm_false;
    wrong_number_of_arguments_violation(vm, "bytevector?", 1, 1, argc, argv);
    return scm_undef;
}

// bytevector-mapping?
scm_obj_t
subr_bytevector_mapping_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) return BVECTORMAPPINGP(argv[0]) ? scm_true : scm_false;
    wrong_number_of_arguments_violation(vm, "bytevector-mapping?", 1, 1, argc, argv);
    return scm_undef;
}

// make-bytevector
scm_obj_t
subr_make_bytevector(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (FIXNUMP(argv[0]) && FIXNUM(argv[0]) >= 0) {
            scm_bvector_t bvector = make_bvector(vm->m_heap, FIXNUM(argv[0]));
            return bvector;
        }
        if (exact_non_negative_integer_pred(argv[0])) {
            invalid_argument_violation(vm, "make-bytevector", "too many elements,", argv[0], 0, argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "make-bytevector", 0, "exact non-negative integer", argv[0], argc, argv);
        return scm_undef;
    }
    if (argc == 2) {
        if (FIXNUMP(argv[0]) && FIXNUM(argv[0]) >= 0) {
            int count = FIXNUM(argv[0]);
            if (FIXNUMP(argv[1])) {
                intptr_t val = FIXNUM(argv[1]);
                if (val >= INT8_MIN && val <= (intptr_t)UINT8_MAX) {
                    scm_bvector_t bvector = make_bvector(vm->m_heap, count);
                    memset(bvector->elts, val & 0xff, count);
                    return bvector;
                }
            }
            if (exact_integer_pred(argv[1])) {
                invalid_argument_violation(vm, "make-bytevector", "value out of range,", argv[1], 1, argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "make-bytevector", 1, "exact integer", argv[1], argc, argv);
            return scm_undef;
        }
        if (exact_non_negative_integer_pred(argv[0])) {
            invalid_argument_violation(vm, "make-bytevector", "too many elements,", argv[0], 0, argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "make-bytevector", 0, "exact non-negative integer", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "make-bytevector", 1, 2, argc, argv);
    return scm_undef;
}

// bytevector-length
scm_obj_t
subr_bytevector_length(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (BVECTORP(argv[0])) {
            scm_bvector_t bvector = (scm_bvector_t)argv[0];
            return MAKEFIXNUM(bvector->count);
        }
        wrong_type_argument_violation(vm, "bytevector-length", 0, "bytevector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "bytevector-length", 1, 1, argc, argv);
    return scm_undef;
}

// bytevector=?
scm_obj_t
subr_bytevector_eq_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (BVECTORP(argv[0])) {
            if (BVECTORP(argv[1])) {
                scm_bvector_t bvector1 = (scm_bvector_t)argv[0];
                scm_bvector_t bvector2 = (scm_bvector_t)argv[1];
                if (bvector1->count == bvector2->count) {
                    return memcmp(bvector1->elts, bvector2->elts, bvector1->count) ? scm_false : scm_true;
                }
                return scm_false;
            }
            wrong_type_argument_violation(vm, "bytevector=?", 1, "bytevector", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "bytevector=?", 0, "bytevector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "bytevector=?", 2, 2, argc, argv);
    return scm_undef;
}

// bytevector-fill!
scm_obj_t
subr_bytevector_fill(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (BVECTORP(argv[0])) {
            if (FIXNUMP(argv[1])) {
                scm_bvector_t bvector = (scm_bvector_t)argv[0];
#if USE_CONST_LITERAL
                if (HDR_BVECTOR_LITERAL(bvector->hdr)) {
                    literal_constant_access_violation(vm, "bytevector-fill!", argv[0], argc, argv);
                    return scm_undef;
                }
#endif
                intptr_t val = FIXNUM(argv[1]);
                if (val >= INT8_MIN && val <= (intptr_t)UINT8_MAX) {
                    memset(bvector->elts, val & 0xff, bvector->count);
                    return scm_unspecified;
                }
            }
            if (exact_integer_pred(argv[1])) {
                invalid_argument_violation(vm, "bytevector-fill!", "value out of range,", argv[1], 1, argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "bytevector-fill!", 1, "exact integer", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "bytevector-fill!", 0, "bytevector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "bytevector-fill!", 2, 2, argc, argv);
    return scm_undef;
}

// bytevector-copy!
scm_obj_t
subr_bytevector_destructive_copy(VM* vm, int argc, scm_obj_t argv[])
{
    int source;
    int target;
    int size;
    if (argc == 5) {
        if (BVECTORP(argv[0])) {
            scm_bvector_t bvector1 = (scm_bvector_t)argv[0];
            if (FIXNUMP(argv[1])) {
                source = FIXNUM(argv[1]);
                if (source >= 0 && source <= bvector1->count) {
                    if (BVECTORP(argv[2])) {
                        scm_bvector_t bvector2 = (scm_bvector_t)argv[2];
#if USE_CONST_LITERAL
                        if (HDR_BVECTOR_LITERAL(bvector2->hdr)) {
                            literal_constant_access_violation(vm, "bytevector-copy!", bvector2, argc, argv);
                            return scm_undef;
                        }
#endif
                        if (FIXNUMP(argv[3])) {
                            target = FIXNUM(argv[3]);
                            if (target >= 0 && target <= bvector2->count) {
                                if (FIXNUMP(argv[4])) {
                                    size = FIXNUM(argv[4]);
                                    if (source + size <= bvector1->count && target + size <= bvector2->count) {
                                        memmove(bvector2->elts + target, bvector1->elts + source, size);
                                        return scm_unspecified;
                                    }
                                }
                                if (exact_integer_pred(argv[4])) {
                                    invalid_argument_violation(vm, "bytevector-copy!", "index out of bounds,", argv[4], 4, argc, argv);
                                    return scm_undef;
                                }
                                wrong_type_argument_violation(vm, "bytevector-copy!", 4, "exact integer", argv[4], argc, argv);
                                return scm_undef;
                            }
                        }
                        if (exact_integer_pred(argv[3])) {
                            invalid_argument_violation(vm, "bytevector-copy!", "index out of bounds,", argv[3], 3, argc, argv);
                            return scm_undef;
                        }
                        wrong_type_argument_violation(vm, "bytevector-copy!", 3, "exact integer", argv[3], argc, argv);
                        return scm_undef;
                    }
                    wrong_type_argument_violation(vm, "bytevector-copy!", 2, "bytevector", argv[2], argc, argv);
                    return scm_undef;
                }
            }
            if (exact_integer_pred(argv[1])) {
                invalid_argument_violation(vm, "bytevector-copy!", "index out of bounds,", argv[1], 1, argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "bytevector-copy!", 1, "exact integer", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "bytevector-copy!", 0, "bytevector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "bytevector-copy!", 5, 5, argc, argv);
    return scm_undef;
}

// bytevector-copy
scm_obj_t
subr_bytevector_copy(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (BVECTORP(argv[0])) {
            scm_bvector_t source = (scm_bvector_t)argv[0];
            scm_bvector_t target = make_bvector(vm->m_heap, source->count);
            memcpy(target->elts, source->elts, source->count);
            return target;
        }
        wrong_type_argument_violation(vm, "bytevector-copy", 0, "bytevector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "bytevector-copy", 1, 1, argc, argv);
    return scm_undef;
}

// bytevector->u8-list
scm_obj_t
subr_bytevector_u8_list(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (BVECTORP(argv[0])) {
            scm_bvector_t bvector = (scm_bvector_t)argv[0];
            int len = bvector->count;
            if (len) {
                scm_obj_t obj = make_pair(vm->m_heap, MAKEFIXNUM(bvector->elts[0]), scm_nil);
                scm_obj_t tail = obj;
                for (int i = 1; i < len; i++) {
                    scm_obj_t e = make_pair(vm->m_heap, MAKEFIXNUM(bvector->elts[i]), scm_nil);
                    CDR(tail) = e;
                    tail = e;
                }
                return obj;
            }
            return scm_nil;
        }
        wrong_type_argument_violation(vm, "bytevector->u8-list", 0, "bytevector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "bytevector->u8-list", 1, 1, argc, argv);
    return scm_undef;
}

// u8-list->bytevector
scm_obj_t
subr_u8_list_bytevector(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        scm_obj_t obj = (scm_obj_t)argv[0];
        if (listp(obj)) {
            int count = list_length(obj);
            scm_bvector_t bvector = make_bvector(vm->m_heap, count);
            for (int i = 0; i < count; i++) {
                scm_obj_t datum = CAR(obj);
                obj = CDR(obj);
                if (FIXNUMP(datum)) {
                    intptr_t val = FIXNUM(datum);
                    if (val >= 0 && val <= UINT8_MAX) {
                        bvector->elts[i] = val & 0xff;
                        continue;
                    }
                }
                if (exact_integer_pred(datum)) {
                    invalid_argument_violation(vm, "u8-list->bytevector", "element value out of range,", argv[0], 0, argc, argv);
                    return scm_undef;
                }
                wrong_type_argument_violation(vm, "u8-list->bytevector", 0, "list of exact integer", argv[0], argc, argv);
                return scm_undef;
            }
            return bvector;
        }
        wrong_type_argument_violation(vm, "u8-list->bytevector", 0, "proper list", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "u8-list->bytevector", 1, 1, argc, argv);
    return scm_undef;
}

// bytevector-u8-ref
scm_obj_t
subr_bytevector_u8_ref(VM* vm, int argc, scm_obj_t argv[])
{
    native_accessor_param_t param(1, "bytevector-u8-ref", vm, argc, argv);
    if (param.violation) return scm_undef;
    return MAKEFIXNUM(param.bytes[0]);
}

// string->utf8
scm_obj_t
subr_string_utf8(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (STRINGP(argv[0])) {
            scm_string_t string = (scm_string_t)argv[0];
            int size = string->size;
            scm_bvector_t bvector = make_bvector(vm->m_heap, size);
            memcpy(bvector->elts, string->name, size);
            return bvector;
        }
        wrong_type_argument_violation(vm, "string->utf8", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "string->utf8", 1, 1, argc, argv);
    return scm_undef;
}

// utf8->string
scm_obj_t
subr_utf8_string(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (BVECTORP(argv[0])) {
            scm_bvector_t bvector = (scm_bvector_t)argv[0];
            if (utf8_decode_test(bvector)) {
                int count = bvector->count;
                scm_string_t string = make_string(vm->m_heap, count, ' ');
                memcpy(string->name, bvector->elts, count);
                return string;
            }
            scm_port_t port = make_bytevector_port(vm->m_heap, make_symbol(vm->m_heap, "bytevector"), SCM_PORT_DIRECTION_OUT, scm_false, scm_false);
            scoped_lock lock(port->lock);
            uint8_t* datum = (uint8_t*)bvector->elts;
            int end = bvector->count;
            int n = 0;
            while (n < end) {
                int nbytes = utf8_byte_count(datum[n]);
                uint32_t ucs4;
                if (n + nbytes > end || cnvt_utf8_to_ucs4(datum + n, &ucs4) < 1) {
                    // U+FFFD
                    port_put_byte(port, 0xEF);
                    port_put_byte(port, 0xBF);
                    port_put_byte(port, 0xBD);
                    nbytes = 1;
                } else {
                    for (int i = 0; i < nbytes; i++) {
                        port_put_byte(port, datum[n + i]);
                    }
                }
                n += nbytes;
            }
            return port_get_string(vm->m_heap, port);
        }
        wrong_type_argument_violation(vm, "utf8->string", 0, "bytevector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "utf8->string", 1, 1, argc, argv);
    return scm_undef;
}


// string->utf8/nul
scm_obj_t
subr_string_utf8_nul(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (STRINGP(argv[0])) {
            scm_string_t string = (scm_string_t)argv[0];
            int size = string->size;
            scm_bvector_t bvector = make_bvector(vm->m_heap, size + 1);
            memcpy(bvector->elts, string->name, size + 1);
            return bvector;
        }
        wrong_type_argument_violation(vm, "string->utf8/nul", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "string->utf8/nul", 1, 1, argc, argv);
    return scm_undef;
}

// make-bytevector-mapping
scm_obj_t
subr_make_bytevector_mapping(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (FIXNUMP(argv[1])) {
            intptr_t size = FIXNUM(argv[1]);
            if (exact_integer_pred(argv[0])) {
                if (n_positive_pred(argv[0])) {
                    uintptr_t adrs;
                    if (exact_integer_to_uintptr(argv[0], &adrs)) return make_bvector_mapping(vm->m_heap, (void*)adrs, size);
                    invalid_argument_violation(vm, "make-bytevector-mapping", "value out of bounds,", argv[0], 0, argc, argv);
                    return scm_undef;
                } else {
                    intptr_t adrs;
                    if (exact_integer_to_intptr(argv[0], &adrs)) return make_bvector_mapping(vm->m_heap, (void*)adrs, size);
                    invalid_argument_violation(vm, "make-bytevector-mapping", "value out of bounds,", argv[0], 0, argc, argv);
                    return scm_undef;
                }
            }
            wrong_type_argument_violation(vm, "make-bytevector-mapping", 0, "exact integer", argv[0], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "make-bytevector-mapping", 1, "fixnum", argv[1], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "make-bytevector-mapping", 2, 2, argc, argv);
    return scm_undef;
}

static inline scm_obj_t
c_u16_ref(const char* subr, VM* vm, int argc, scm_obj_t argv[])
{
    c_accessor_param_t param(2, subr, vm, argc, argv);
    if (param.violation) return scm_undef;
    return MAKEFIXNUM(*(uint16_t*)param.bytes);
}

static inline scm_obj_t
c_u32_ref(const char* subr, VM* vm, int argc, scm_obj_t argv[])
{
    c_accessor_param_t param(4, subr, vm, argc, argv);
    if (param.violation) return scm_undef;
    return uint32_to_integer(vm->m_heap, *(uint32_t*)param.bytes);
}

static inline scm_obj_t
c_u64_ref(const char* subr, VM* vm, int argc, scm_obj_t argv[])
{
    c_accessor_param_t param(8, subr, vm, argc, argv);
    if (param.violation) return scm_undef;
    return uint64_to_integer(vm->m_heap, *(uint64_t*)param.bytes);
}

static inline scm_obj_t
c_s8_ref(const char* subr, VM* vm, int argc, scm_obj_t argv[])
{
    c_accessor_param_t param(1, subr, vm, argc, argv);
    if (param.violation) return scm_undef;
    return MAKEFIXNUM(*(int8_t*)param.bytes);
}

static inline scm_obj_t
c_s16_ref(const char* subr, VM* vm, int argc, scm_obj_t argv[])
{
    c_accessor_param_t param(2, subr, vm, argc, argv);
    if (param.violation) return scm_undef;
    return MAKEFIXNUM(*(int16_t*)param.bytes);
}

static inline scm_obj_t
c_s32_ref(const char* subr, VM* vm, int argc, scm_obj_t argv[])
{
    c_accessor_param_t param(4, subr, vm, argc, argv);
    if (param.violation) return scm_undef;
    return int32_to_integer(vm->m_heap, *(int32_t*)param.bytes);
}

static inline scm_obj_t
c_s64_ref(const char* subr, VM* vm, int argc, scm_obj_t argv[])
{
    c_accessor_param_t param(8, subr, vm, argc, argv);
    if (param.violation) return scm_undef;
    return int64_to_integer(vm->m_heap, *(int64_t*)param.bytes);
}

static inline scm_obj_t
c_n8_set(const char* subr, VM* vm, int argc, scm_obj_t argv[])
{
    c_mutator_param_t param(1, subr, vm, argc, argv);
    if (param.violation) return scm_undef;
    if (FIXNUMP(argv[2])) {
        intptr_t n8 = FIXNUM(argv[2]);
        if (n8 >= INT8_MIN && n8 <= (intptr_t)UINT16_MAX) {
            *(uint8_t*)param.bytes = n8;
            return scm_unspecified;
        }
        invalid_argument_violation(vm, subr, "value out of range,", argv[2], 2, argc, argv);
        return scm_undef;
    }
    wrong_type_argument_violation(vm, subr, 2, "exact integer", argv[2], argc, argv);
    return scm_undef;
}

static inline scm_obj_t
c_n16_set(const char* subr, VM* vm, int argc, scm_obj_t argv[])
{
    c_mutator_param_t param(2, subr, vm, argc, argv);
    if (param.violation) return scm_undef;
    if (FIXNUMP(argv[2])) {
        intptr_t n16 = FIXNUM(argv[2]);
        if (n16 >= INT16_MIN && n16 <= (intptr_t)UINT16_MAX) {
            *(uint16_t*)param.bytes = n16;
            return scm_unspecified;
        }
        invalid_argument_violation(vm, subr, "value out of range,", argv[2], 2, argc, argv);
        return scm_undef;
    }
    wrong_type_argument_violation(vm, subr, 2, "exact integer", argv[2], argc, argv);
    return scm_undef;
}

static inline scm_obj_t
c_n32_set(const char* subr, VM* vm, int argc, scm_obj_t argv[])
{
    c_mutator_param_t param(4, subr, vm, argc, argv);
    if (param.violation) return scm_undef;
    if (exact_integer_pred(argv[2])) {
        if (n_negative_pred(argv[2])) {
            int32_t s32;
            if (exact_integer_to_int32(argv[2], &s32)) {
                *(uint32_t*)param.bytes = (uint32_t)s32;
                return scm_unspecified;
            }
        } else {
            uint32_t u32;
            if (exact_integer_to_uint32(argv[2], &u32)) {
                *(uint32_t*)param.bytes = u32;
                return scm_unspecified;
            }
        }
        invalid_argument_violation(vm, subr, "value out of range,", argv[2], 2, argc, argv);
        return scm_undef;
    }
    wrong_type_argument_violation(vm, subr, 2, "exact integer", argv[2], argc, argv);
    return scm_undef;
}

static inline scm_obj_t
c_n64_set(const char* subr, VM* vm, int argc, scm_obj_t argv[])
{
    c_mutator_param_t param(8, subr, vm, argc, argv);
    if (param.violation) return scm_undef;
    if (exact_integer_pred(argv[2])) {
        if (n_negative_pred(argv[2])) {
            int64_t s64;
            if (exact_integer_to_int64(argv[2], &s64)) {
                *(uint64_t*)param.bytes = (uint64_t)s64;
                return scm_unspecified;
            }
        } else {
            uint64_t u64;
            if (exact_integer_to_uint64(argv[2], &u64)) {
                *(uint64_t*)param.bytes = u64;
                return scm_unspecified;
            }
        }
        invalid_argument_violation(vm, subr, "value out of range,", argv[2], 2, argc, argv);
        return scm_undef;
    }
    wrong_type_argument_violation(vm, subr, 2, "exact integer", argv[2], argc, argv);
    return scm_undef;
}

// bytevector-c-short-ref
scm_obj_t
subr_bytevector_c_short_ref(VM* vm, int argc, scm_obj_t argv[])
{
    assert(sizeof(short) == 2);
    return c_s16_ref("bytevector-c-short-ref", vm, argc, argv);
}

// bytevector-c-unsigend-short-ref
scm_obj_t
subr_bytevector_c_unsigned_short_ref(VM* vm, int argc, scm_obj_t argv[])
{
    assert(sizeof(unsigned short) == 2);
    return c_u16_ref("bytevector-c-unsigned-short-ref", vm, argc, argv);
}

// bytevector-c-int-ref
scm_obj_t
subr_bytevector_c_int_ref(VM* vm, int argc, scm_obj_t argv[])
{
    assert(sizeof(int) == 4 || sizeof(int) == 8);
    if (sizeof(int) == 4) return c_s32_ref("bytevector-c-int-ref", vm, argc, argv);
    return c_s64_ref("bytevector-c-int-ref", vm, argc, argv);
}

// bytevector-c-unsigend-int-ref
scm_obj_t
subr_bytevector_c_unsigned_int_ref(VM* vm, int argc, scm_obj_t argv[])
{
    assert(sizeof(unsigned int) == 4 || sizeof(unsigned int) == 8);
    if (sizeof(int) == 4) return c_u32_ref("bytevector-c-unsigned-int-ref", vm, argc, argv);
    return c_u64_ref("bytevector-c-unsigned-int-ref", vm, argc, argv);
}

// bytevector-c-long-ref
scm_obj_t
subr_bytevector_c_long_ref(VM* vm, int argc, scm_obj_t argv[])
{
    assert(sizeof(long) == 4 || sizeof(long) == 8);
    if (sizeof(long) == 4) return c_s32_ref("bytevector-c-long-ref", vm, argc, argv);
    return c_s64_ref("bytevector-c-long-ref", vm, argc, argv);
}

// bytevector-c-unsigend-long-ref
scm_obj_t
subr_bytevector_c_unsigned_long_ref(VM* vm, int argc, scm_obj_t argv[])
{
    assert(sizeof(unsigned long) == 4 || sizeof(unsigned long) == 8);
    if (sizeof(unsigned long) == 4) return c_u32_ref("bytevector-c-unsigned-long-ref", vm, argc, argv);
    return c_u64_ref("bytevector-c-unsigned-long-ref", vm, argc, argv);
}

// bytevector-c-void*-ref
scm_obj_t
subr_bytevector_c_intptr_ref(VM* vm, int argc, scm_obj_t argv[])
{
    assert(sizeof(void*) == 4 || sizeof(void*) == 8);
    if (sizeof(void*) == 4) return c_u32_ref("bytevector-c-void*-ref", vm, argc, argv);
    return c_u64_ref("bytevector-c-void*-ref", vm, argc, argv);
}

// bytevector-c-int8-ref
scm_obj_t
subr_bytevector_c_int8_ref(VM* vm, int argc, scm_obj_t argv[])
{
    native_accessor_param_t param(1, "bytevector-c-int8-ref", vm, argc, argv);
    if (param.violation) return scm_undef;
    return MAKEFIXNUM((int8_t)param.bytes[0]);
}

// bytevector-c-int16-ref
scm_obj_t
subr_bytevector_c_int16_ref(VM* vm, int argc, scm_obj_t argv[])
{
    return c_s16_ref("bytevector-c-int16-ref", vm, argc, argv);
}

// bytevector-c-int32-ref
scm_obj_t
subr_bytevector_c_int32_ref(VM* vm, int argc, scm_obj_t argv[])
{
    return c_s32_ref("bytevector-c-int32-ref", vm, argc, argv);
}

// bytevector-c-int64-ref
scm_obj_t
subr_bytevector_c_int64_ref(VM* vm, int argc, scm_obj_t argv[])
{
    return c_s64_ref("bytevector-c-int64-ref", vm, argc, argv);
}

// bytevector-c-uint8-ref
scm_obj_t
subr_bytevector_c_uint8_ref(VM* vm, int argc, scm_obj_t argv[])
{
    native_accessor_param_t param(1, "bytevector-c-uint8-ref", vm, argc, argv);
    if (param.violation) return scm_undef;
    return MAKEFIXNUM(param.bytes[0]);
}

// bytevector-c-uint16-ref
scm_obj_t
subr_bytevector_c_uint16_ref(VM* vm, int argc, scm_obj_t argv[])
{
    return c_u16_ref("bytevector-c-uint16-ref", vm, argc, argv);
}

// bytevector-c-uint32-ref
scm_obj_t
subr_bytevector_c_uint32_ref(VM* vm, int argc, scm_obj_t argv[])
{
    return c_u32_ref("bytevector-c-uint32-ref", vm, argc, argv);
}

// bytevector-c-uint64-ref
scm_obj_t
subr_bytevector_c_uint64_ref(VM* vm, int argc, scm_obj_t argv[])
{
    return c_u64_ref("bytevector-c-uint64-ref", vm, argc, argv);
}

// bytevector-c-float-ref
scm_obj_t
subr_bytevector_c_float_ref(VM* vm, int argc, scm_obj_t argv[])
{
    c_accessor_param_t param(4, "bytevector-c-float-ref", vm, argc, argv);
    if (param.violation) return scm_undef;
    return double_to_inexact(vm->m_heap, *(float*)param.bytes);
}

// bytevector-c-double-ref
scm_obj_t
subr_bytevector_c_double_ref(VM* vm, int argc, scm_obj_t argv[])
{
    c_accessor_param_t param(8, "bytevector-c-double-ref", vm, argc, argv);
    if (param.violation) return scm_undef;
    return double_to_inexact(vm->m_heap, *(double*)param.bytes);
}

// bytevector-c-short-set!
scm_obj_t
subr_bytevector_c_short_set(VM* vm, int argc, scm_obj_t argv[])
{
    assert(sizeof(short) == 2);
    return c_n16_set("bytevector-c-short-set!", vm, argc, argv);
}

// bytevector-c-int-set!
scm_obj_t
subr_bytevector_c_int_set(VM* vm, int argc, scm_obj_t argv[])
{
    assert(sizeof(int) == 4 || sizeof(int) == 8);
    if (sizeof(int) == 4) return c_n32_set("bytevector-c-int-set!", vm, argc, argv);
    return c_n64_set("bytevector-c-int-set!", vm, argc, argv);
}

// bytevector-c-long-set!
scm_obj_t
subr_bytevector_c_long_set(VM* vm, int argc, scm_obj_t argv[])
{
    assert(sizeof(long) == 4 || sizeof(long) == 8);
    if (sizeof(long) == 4) return c_n32_set("bytevector-c-long-set!", vm, argc, argv);
    return c_n64_set("bytevector-c-long-set!", vm, argc, argv);
}

// bytevector-c-void*-set!
scm_obj_t
subr_bytevector_c_intptr_set(VM* vm, int argc, scm_obj_t argv[])
{
    assert(sizeof(void*) == 4 || sizeof(void*) == 8);
    if (sizeof(void*) == 4) return c_n32_set("bytevector-c-void*-set!", vm, argc, argv);
    return c_n64_set("bytevector-c-void*-set!", vm, argc, argv);
}

// bytevector-c-int8-set!
scm_obj_t
subr_bytevector_c_int8_set(VM* vm, int argc, scm_obj_t argv[])
{
    return c_n8_set("bytevector-c-int8-set!", vm, argc, argv);
}

// bytevector-c-int16-set!
scm_obj_t
subr_bytevector_c_int16_set(VM* vm, int argc, scm_obj_t argv[])
{
    return c_n16_set("bytevector-c-int16-set!", vm, argc, argv);
}

// bytevector-c-int32-set!
scm_obj_t
subr_bytevector_c_int32_set(VM* vm, int argc, scm_obj_t argv[])
{
    return c_n32_set("bytevector-c-int32-set!", vm, argc, argv);
}

// bytevector-c-int64-set!
scm_obj_t
subr_bytevector_c_int64_set(VM* vm, int argc, scm_obj_t argv[])
{
    return c_n64_set("bytevector-c-int64-set!", vm, argc, argv);
}

// bytevector-c-float-set!
scm_obj_t
subr_bytevector_c_float_set(VM* vm, int argc, scm_obj_t argv[])
{
    c_mutator_param_t param(4, "bytevector-c-float-set!", vm, argc, argv);
    if (param.violation) return scm_undef;
    if (real_pred(argv[2])) {
        *(float*)param.bytes = real_to_double(argv[2]);
        return scm_unspecified;
    }
    wrong_type_argument_violation(vm, "bytevector-c-float-set!", 2, "real", argv[2], argc, argv);
    return scm_undef;
}

// bytevector-c-double-set!
scm_obj_t
subr_bytevector_c_double_set(VM* vm, int argc, scm_obj_t argv[])
{
    c_mutator_param_t param(8, "bytevector-c-double-set!", vm, argc, argv);
    if (param.violation) return scm_undef;
    if (real_pred(argv[2])) {
        *(double*)param.bytes = real_to_double(argv[2]);
        return scm_unspecified;
    }
    wrong_type_argument_violation(vm, "bytevector-c-double-set!", 2, "real", argv[2], argc, argv);
    return scm_undef;
}

// bytevector-c-strlen
scm_obj_t
subr_bytevector_c_strlen(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (BVECTORP(argv[0])) {
            scm_bvector_t bvector = (scm_bvector_t)argv[0];
            int end = bvector->count;
            int count = 0;
            while (count < end) {
                if (bvector->elts[count] == 0) break;
                count++;
            }
            return MAKEFIXNUM(count);
        }
        wrong_type_argument_violation(vm, "bytevector-c-strlen", 0, "bytevector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "bytevector-c-strlen", 1, 1, argc, argv);
    return scm_undef;
}

void init_subr_bvector(object_heap_t* heap)
{
#define DEFSUBR(SYM, FUNC)  heap->intern_system_subr(SYM, FUNC)

    DEFSUBR("bytevector-u8-ref", subr_bytevector_u8_ref);
    DEFSUBR("bytevector-s8-ref", subr_bytevector_s8_ref);
    DEFSUBR("bytevector-u8-set!", subr_bytevector_u8_set);
    DEFSUBR("bytevector-s8-set!", subr_bytevector_s8_set);
    DEFSUBR("bytevector-u16-ref", subr_bytevector_u16_ref);
    DEFSUBR("bytevector-s16-ref", subr_bytevector_s16_ref);
    DEFSUBR("bytevector-u16-native-ref", subr_bytevector_u16_native_ref);
    DEFSUBR("bytevector-s16-native-ref", subr_bytevector_s16_native_ref);
    DEFSUBR("bytevector-u16-set!", subr_bytevector_u16_set);
    DEFSUBR("bytevector-s16-set!", subr_bytevector_s16_set);
    DEFSUBR("bytevector-u16-native-set!", subr_bytevector_u16_native_set);
    DEFSUBR("bytevector-s16-native-set!", subr_bytevector_s16_native_set);
    DEFSUBR("bytevector-u32-ref", subr_bytevector_u32_ref);
    DEFSUBR("bytevector-s32-ref", subr_bytevector_s32_ref);
    DEFSUBR("bytevector-u32-native-ref", subr_bytevector_u32_native_ref);
    DEFSUBR("bytevector-s32-native-ref", subr_bytevector_s32_native_ref);
    DEFSUBR("bytevector-u32-set!", subr_bytevector_u32_set);
    DEFSUBR("bytevector-s32-set!", subr_bytevector_s32_set);
    DEFSUBR("bytevector-u32-native-set!", subr_bytevector_u32_native_set);
    DEFSUBR("bytevector-s32-native-set!", subr_bytevector_s32_native_set);
    DEFSUBR("bytevector-u64-ref", subr_bytevector_u64_ref);
    DEFSUBR("bytevector-s64-ref", subr_bytevector_s64_ref);
    DEFSUBR("bytevector-u64-native-ref", subr_bytevector_u64_native_ref);
    DEFSUBR("bytevector-s64-native-ref", subr_bytevector_s64_native_ref);
    DEFSUBR("bytevector-u64-set!", subr_bytevector_u64_set);
    DEFSUBR("bytevector-s64-set!", subr_bytevector_s64_set);
    DEFSUBR("bytevector-u64-native-set!", subr_bytevector_u64_native_set);
    DEFSUBR("bytevector-s64-native-set!", subr_bytevector_s64_native_set);
    DEFSUBR("bytevector-ieee-single-ref", subr_bytevector_ieee_single_ref);
    DEFSUBR("bytevector-ieee-single-native-ref", subr_bytevector_ieee_single_native_ref);
    DEFSUBR("bytevector-ieee-single-set!", subr_bytevector_ieee_single_set);
    DEFSUBR("bytevector-ieee-single-native-set!", subr_bytevector_ieee_single_native_set);
    DEFSUBR("bytevector-ieee-double-ref", subr_bytevector_ieee_double_ref);
    DEFSUBR("bytevector-ieee-double-native-ref", subr_bytevector_ieee_double_native_ref);
    DEFSUBR("bytevector-ieee-double-set!", subr_bytevector_ieee_double_set);
    DEFSUBR("bytevector-ieee-double-native-set!", subr_bytevector_ieee_double_native_set);
    DEFSUBR("native-endianness", subr_native_endianness);
    DEFSUBR("bytevector?", subr_bytevector_pred);
    DEFSUBR("make-bytevector", subr_make_bytevector);
    DEFSUBR("bytevector-length", subr_bytevector_length);
    DEFSUBR("bytevector=?", subr_bytevector_eq_pred);
    DEFSUBR("bytevector-fill!", subr_bytevector_fill);
    DEFSUBR("bytevector-copy!", subr_bytevector_destructive_copy);
    DEFSUBR("bytevector-copy", subr_bytevector_copy);
    DEFSUBR("bytevector->u8-list", subr_bytevector_u8_list);
    DEFSUBR("u8-list->bytevector", subr_u8_list_bytevector);
    DEFSUBR("string->utf8", subr_string_utf8);
    DEFSUBR("string->utf8/nul", subr_string_utf8_nul);
    DEFSUBR("utf8->string", subr_utf8_string);
    DEFSUBR("make-bytevector-mapping", subr_make_bytevector_mapping);
    DEFSUBR("bytevector-mapping?", subr_bytevector_mapping_pred);
    DEFSUBR("bytevector-c-short-ref", subr_bytevector_c_short_ref);
    DEFSUBR("bytevector-c-int-ref", subr_bytevector_c_int_ref);
    DEFSUBR("bytevector-c-long-ref", subr_bytevector_c_long_ref);
    DEFSUBR("bytevector-c-unsigned-short-ref", subr_bytevector_c_unsigned_short_ref);
    DEFSUBR("bytevector-c-unsigned-int-ref", subr_bytevector_c_unsigned_int_ref);
    DEFSUBR("bytevector-c-unsigned-long-ref", subr_bytevector_c_unsigned_long_ref);
    DEFSUBR("bytevector-c-int8-ref", subr_bytevector_c_int8_ref);
    DEFSUBR("bytevector-c-int16-ref", subr_bytevector_c_int16_ref);
    DEFSUBR("bytevector-c-int32-ref", subr_bytevector_c_int32_ref);
    DEFSUBR("bytevector-c-int64-ref", subr_bytevector_c_int64_ref);
    DEFSUBR("bytevector-c-uint8-ref", subr_bytevector_c_uint8_ref);
    DEFSUBR("bytevector-c-uint16-ref", subr_bytevector_c_uint16_ref);
    DEFSUBR("bytevector-c-uint32-ref", subr_bytevector_c_uint32_ref);
    DEFSUBR("bytevector-c-uint64-ref", subr_bytevector_c_uint64_ref);
    DEFSUBR("bytevector-c-float-ref", subr_bytevector_c_float_ref);
    DEFSUBR("bytevector-c-double-ref", subr_bytevector_c_double_ref);
    DEFSUBR("bytevector-c-short-set!", subr_bytevector_c_short_set);
    DEFSUBR("bytevector-c-int-set!", subr_bytevector_c_int_set);
    DEFSUBR("bytevector-c-long-set!", subr_bytevector_c_long_set);
    DEFSUBR("bytevector-c-void*-ref", subr_bytevector_c_intptr_ref);
    DEFSUBR("bytevector-c-void*-set!", subr_bytevector_c_intptr_set);
    DEFSUBR("bytevector-c-int8-set!", subr_bytevector_c_int8_set);
    DEFSUBR("bytevector-c-int16-set!", subr_bytevector_c_int16_set);
    DEFSUBR("bytevector-c-int32-set!", subr_bytevector_c_int32_set);
    DEFSUBR("bytevector-c-int64-set!", subr_bytevector_c_int64_set);    
    DEFSUBR("bytevector-c-float-set!", subr_bytevector_c_float_set);    
    DEFSUBR("bytevector-c-double-set!", subr_bytevector_c_double_set);    
    DEFSUBR("bytevector-c-strlen", subr_bytevector_c_strlen);    
}


