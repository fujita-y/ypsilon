/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
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

#define S_LITTLE            (vm->m_heap->inherent_symbol(S_CODE_LITTLE))
#define S_BIG               (vm->m_heap->inherent_symbol(S_CODE_BIG))
#define NATIVE_ENDIANNESS   (vm->m_heap->inherent_symbol(ARCH_LITTLE_ENDIAN ? S_CODE_LITTLE : S_CODE_BIG))

// native-endianness
scm_obj_t
subr_native_endianness(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) return NATIVE_ENDIANNESS;
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
                int val = FIXNUM(argv[1]);
                if (val >= INT8_MIN && val <= UINT8_MAX) {
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
                int val = FIXNUM(argv[1]);
                if (val >= INT8_MIN && val <= UINT8_MAX) {
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

// bytevector-u8-ref
scm_obj_t
subr_bytevector_u8_ref(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (BVECTORP(argv[0])) {
            if (FIXNUMP(argv[1])) {
                scm_bvector_t bvector = (scm_bvector_t)argv[0];
                int index = FIXNUM(argv[1]);
                if (index >= 0 && index < bvector->count) {
                    return MAKEFIXNUM(bvector->elts[index]);
                }
            }
            if (exact_integer_pred(argv[1])) {
                invalid_argument_violation(vm, "bytevector-u8-ref", "index out of bounds,", argv[1], 1, argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "bytevector-u8-ref", 1, "exact integer", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "bytevector-u8-ref", 0, "bytevector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "bytevector-u8-ref", 2, 2, argc, argv);
    return scm_undef;
}

// bytevector-s8-ref
scm_obj_t
subr_bytevector_s8_ref(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (BVECTORP(argv[0])) {
            if (FIXNUMP(argv[1])) {
                scm_bvector_t bvector = (scm_bvector_t)argv[0];
                int index = FIXNUM(argv[1]);
                if (index >= 0 && index < bvector->count) {
                    return MAKEFIXNUM((int8_t)bvector->elts[index]);
                }
            }
            if (exact_integer_pred(argv[1])) {
                invalid_argument_violation(vm, "bytevector-s8-ref", "index out of bounds,", argv[1], 1, argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "bytevector-s8-ref", 1, "exact integer", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "bytevector-s8-ref", 0, "bytevector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "bytevector-s8-ref", 2, 2, argc, argv);
    return scm_undef;
}

// bytevector-u8-set!
scm_obj_t
subr_bytevector_u8_set(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 3) {
        if (BVECTORP(argv[0])) {
            if (FIXNUMP(argv[1])) {
                scm_bvector_t bvector = (scm_bvector_t)argv[0];
                int index = FIXNUM(argv[1]);
                if (index >= 0 && index < bvector->count) {
                    if (FIXNUMP(argv[2])) {
                        int val = FIXNUM(argv[2]);
                        if (val >= 0 && val <= UINT8_MAX) {
                            bvector->elts[index] = val & 0xff;
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
            }
            if (exact_integer_pred(argv[1])) {
                invalid_argument_violation(vm, "bytevector-u8-set!", "index out of bounds,", argv[1], 1, argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "bytevector-u8-set!", 1, "exact integer", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "bytevector-u8-set!", 0, "bytevector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "bytevector-u8-set!", 3, 3, argc, argv);
    return scm_undef;
}

// bytevector-s8-set!
scm_obj_t
subr_bytevector_s8_set(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 3) {
        if (BVECTORP(argv[0])) {
            if (FIXNUMP(argv[1])) {
                scm_bvector_t bvector = (scm_bvector_t)argv[0];
                int index = FIXNUM(argv[1]);
                if (index >= 0 && index < bvector->count) {
                    if (FIXNUMP(argv[2])) {
                        int val = FIXNUM(argv[2]);
                        if (val >= INT8_MIN && val <= INT8_MAX) {
                            bvector->elts[index] = val & 0xff;
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
            }
            if (exact_integer_pred(argv[1])) {
                invalid_argument_violation(vm, "bytevector-s8-set!", "index out of bounds,", argv[1], 1, argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "bytevector-s8-set!", 1, "exact integer", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "bytevector-s8-set!", 0, "bytevector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "bytevector-s8-set!", 3, 3, argc, argv);
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
                    int val = FIXNUM(datum);
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

// bytevector-u16-ref
scm_obj_t
subr_bytevector_u16_ref(VM* vm, int argc, scm_obj_t argv[])
{
    const int size = 2;
    if (argc == 3) {
        if (BVECTORP(argv[0])) {
            if (FIXNUMP(argv[1])) {
                scm_bvector_t bvector = (scm_bvector_t)argv[0];
                int index = FIXNUM(argv[1]);
                if (index >= 0 && (index + size) <= bvector->count) {
                    if (SYMBOLP(argv[2])) {
                        scm_symbol_t symbol = (scm_symbol_t)argv[2];
                        uint16_t u16;
                        if (symbol == S_LITTLE) {
                            u16 = bvector->elts[index] + bvector->elts[index + 1] * 256;
                            return MAKEFIXNUM(u16);
                        }
                        if (symbol == S_BIG) {
                            u16 = bvector->elts[index] * 256 + bvector->elts[index + 1];
                            return MAKEFIXNUM(u16);
                        }
                    }
                    wrong_type_argument_violation(vm, "bytevector-u16-ref", 2, "endianness", argv[2], argc, argv);
                    return scm_undef;
                }
            }
            if (exact_integer_pred(argv[1])) {
                invalid_argument_violation(vm, "bytevector-u16-ref", "index out of bounds,", argv[1], 1, argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "bytevector-u16-ref", 1, "exact integer", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "bytevector-u16-ref", 0, "bytevector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "bytevector-u16-ref", 3, 3, argc, argv);
    return scm_undef;
}

// bytevector-s16-ref
scm_obj_t
subr_bytevector_s16_ref(VM* vm, int argc, scm_obj_t argv[])
{
    const int size = 2;
    if (argc == 3) {
        if (BVECTORP(argv[0])) {
            if (FIXNUMP(argv[1])) {
                scm_bvector_t bvector = (scm_bvector_t)argv[0];
                int index = FIXNUM(argv[1]);
                if (index >= 0 && (index + size) <= bvector->count) {
                    if (SYMBOLP(argv[2])) {
                        scm_symbol_t symbol = (scm_symbol_t)argv[2];
                        int16_t s16;
                        if (symbol == S_LITTLE) {
                            s16 = bvector->elts[index] + bvector->elts[index + 1] * 256;
                            return MAKEFIXNUM(s16);
                        }
                        if (symbol == S_BIG) {
                            s16 = bvector->elts[index] * 256 + bvector->elts[index + 1];
                            return MAKEFIXNUM(s16);
                        }
                    }
                    wrong_type_argument_violation(vm, "bytevector-s16-ref", 2, "endianness", argv[2], argc, argv);
                    return scm_undef;
                }
            }
            if (exact_integer_pred(argv[1])) {
                invalid_argument_violation(vm, "bytevector-s16-ref", "index out of bounds,", argv[1], 1, argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "bytevector-s16-ref", 1, "exact integer", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "bytevector-s16-ref", 0, "bytevector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "bytevector-s16-ref", 3, 3, argc, argv);
    return scm_undef;
}

// bytevector-u16-native-ref
scm_obj_t
subr_bytevector_u16_native_ref(VM* vm, int argc, scm_obj_t argv[])
{
    const int size = 2;
    if (argc == 2) {
        if (BVECTORP(argv[0])) {
            if (FIXNUMP(argv[1])) {
                scm_bvector_t bvector = (scm_bvector_t)argv[0];
                int index = FIXNUM(argv[1]);
                if (index >= 0 && (index + size) <= bvector->count) {
                    if ((index & (size - 1)) == 0) {
                        uint16_t* ref = (uint16_t*)&bvector->elts[index];
                        return MAKEFIXNUM(*ref);
                    }
                    invalid_argument_violation(vm, "bytevector-u16-native-ref", "index not aligned,", argv[1], 1, argc, argv);
                    return scm_undef;
                }
            }
            if (exact_integer_pred(argv[1])) {
                invalid_argument_violation(vm, "bytevector-u16-native-ref", "index out of bounds,", argv[1], 1, argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "bytevector-u16-native-ref", 1, "exact integer", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "bytevector-u16-native-ref", 0, "bytevector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "bytevector-u16-native-ref", 2, 2, argc, argv);
    return scm_undef;
}

// bytevector-s16-native-ref
scm_obj_t
subr_bytevector_s16_native_ref(VM* vm, int argc, scm_obj_t argv[])
{
    const int size = 2;
    if (argc == 2) {
        if (BVECTORP(argv[0])) {
            if (FIXNUMP(argv[1])) {
                scm_bvector_t bvector = (scm_bvector_t)argv[0];
                int index = FIXNUM(argv[1]);
                if (index >= 0 && (index + size) <= bvector->count) {
                    if ((index & (size - 1)) == 0) {
                        int16_t* ref = (int16_t*)&bvector->elts[index];
                        return MAKEFIXNUM(*ref);
                    }
                    invalid_argument_violation(vm, "bytevector-u16-native-ref", "index not aligned,", argv[1], 1, argc, argv);
                    return scm_undef;
                }
            }
            if (exact_integer_pred(argv[1])) {
                invalid_argument_violation(vm, "bytevector-u16-native-ref", "index out of bounds,", argv[1], 1, argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "bytevector-u16-native-ref", 1, "exact integer", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "bytevector-u16-native-ref", 0, "bytevector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "bytevector-u16-native-ref", 2, 2, argc, argv);
    return scm_undef;
}

// bytevector-u16-set!
scm_obj_t
subr_bytevector_u16_set(VM* vm, int argc, scm_obj_t argv[])
{
    const int size = 2;
    if (argc == 4) {
        if (BVECTORP(argv[0])) {
            if (FIXNUMP(argv[1])) {
                scm_bvector_t bvector = (scm_bvector_t)argv[0];
                int index = FIXNUM(argv[1]);
                if (index >= 0 && (index + size) <= bvector->count) {
                    if (FIXNUMP(argv[2])) {
                        int val = FIXNUM(argv[2]);
                        if (val >= 0 && val <= UINT16_MAX) {
                            uint16_t u16 = val;
                            if (SYMBOLP(argv[3])) {
                                scm_symbol_t symbol = (scm_symbol_t)argv[3];
                                if (symbol == S_LITTLE) {
                                    bvector->elts[index + 0] = u16 & 0xff;
                                    bvector->elts[index + 1] = u16 >> 8;
                                    return scm_unspecified;
                                }
                                if (symbol == S_BIG) {
                                    bvector->elts[index + 0] = u16 >> 8;
                                    bvector->elts[index + 1] = u16 & 0xff;
                                    return scm_unspecified;
                                }
                            }
                            wrong_type_argument_violation(vm, "bytevector-u16-set!", 3, "endianness", argv[3], argc, argv);
                            return scm_undef;
                        }
                    }
                    if (exact_integer_pred(argv[2])) {
                        invalid_argument_violation(vm, "bytevector-u16-set!", "value out of range,", argv[2], 2, argc, argv);
                        return scm_undef;
                    }
                    wrong_type_argument_violation(vm, "bytevector-u16-set!", 2, "exact integer", argv[2], argc, argv);
                    return scm_undef;
                }
            }
            if (exact_integer_pred(argv[1])) {
                invalid_argument_violation(vm, "bytevector-u16-set!", "index out of bounds,", argv[1], 1, argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "bytevector-u16-set!", 1, "exact integer", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "bytevector-u16-set!", 0, "bytevector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "bytevector-u16-set!", 4, 4, argc, argv);
    return scm_undef;
}

// bytevector-s16-set!
scm_obj_t
subr_bytevector_s16_set(VM* vm, int argc, scm_obj_t argv[])
{
    const int size = 2;
    if (argc == 4) {
        if (BVECTORP(argv[0])) {
            if (FIXNUMP(argv[1])) {
                scm_bvector_t bvector = (scm_bvector_t)argv[0];
                int index = FIXNUM(argv[1]);
                if (index >= 0 && (index + size) <= bvector->count) {
                    if (FIXNUMP(argv[2])) {
                        int val = FIXNUM(argv[2]);
                        if (val >= INT16_MIN && val <= INT16_MAX) {
                            int16_t s16 = val;
                            if (SYMBOLP(argv[3])) {
                                scm_symbol_t symbol = (scm_symbol_t)argv[3];
                                if (symbol == S_LITTLE) {
                                    bvector->elts[index + 0] = s16 & 0xff;
                                    bvector->elts[index + 1] = (uint16_t)s16 >> 8;
                                    return scm_unspecified;
                                }
                                if (symbol == S_BIG) {
                                    bvector->elts[index + 0] = (uint16_t)s16 >> 8;
                                    bvector->elts[index + 1] = s16 & 0xff;
                                    return scm_unspecified;
                                }
                            }
                            wrong_type_argument_violation(vm, "bytevector-s16-set!", 3, "endianness", argv[3], argc, argv);
                            return scm_undef;
                        }
                    }
                    if (exact_integer_pred(argv[2])) {
                        invalid_argument_violation(vm, "bytevector-s16-set!", "value out of range,", argv[2], 2, argc, argv);
                        return scm_undef;
                    }
                    wrong_type_argument_violation(vm, "bytevector-s16-set!", 2, "exact integer", argv[2], argc, argv);
                    return scm_undef;
                }
            }
            if (exact_integer_pred(argv[1])) {
                invalid_argument_violation(vm, "bytevector-s16-set!", "index out of bounds,", argv[1], 1, argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "bytevector-s16-set!", 1, "exact integer", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "bytevector-s16-set!", 0, "bytevector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "bytevector-s16-set!", 4, 4, argc, argv);
    return scm_undef;
}

// bytevector-u16-native-set!
scm_obj_t
subr_bytevector_u16_native_set(VM* vm, int argc, scm_obj_t argv[])
{
    const int size = 2;
    if (argc == 3) {
        if (BVECTORP(argv[0])) {
            if (FIXNUMP(argv[1])) {
                scm_bvector_t bvector = (scm_bvector_t)argv[0];
                int index = FIXNUM(argv[1]);
                if (index >= 0 && (index + size) <= bvector->count) {
                    if ((index & (size - 1)) == 0) {
                        if (FIXNUMP(argv[2])) {
                            int val = FIXNUM(argv[2]);
                            if (val >= 0 && val <= UINT16_MAX) {
                                uint16_t* ref = (uint16_t*)&bvector->elts[index];
                                *ref = val;
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
                    invalid_argument_violation(vm, "bytevector-u16-native-set!", "index not aligned,", argv[1], 1, argc, argv);
                    return scm_undef;
                }
            }
            if (exact_integer_pred(argv[1])) {
                invalid_argument_violation(vm, "bytevector-u16-native-set!", "index out of bounds,", argv[1], 1, argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "bytevector-u16-native-set!", 1, "exact integer", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "bytevector-u16-native-set!", 0, "bytevector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "bytevector-u16-native-set!", 3, 3, argc, argv);
    return scm_undef;
}

// bytevector-s16-native-set!
scm_obj_t
subr_bytevector_s16_native_set(VM* vm, int argc, scm_obj_t argv[])
{
    const int size = 2;
    if (argc == 3) {
        if (BVECTORP(argv[0])) {
            if (FIXNUMP(argv[1])) {
                scm_bvector_t bvector = (scm_bvector_t)argv[0];
                int index = FIXNUM(argv[1]);
                if (index >= 0 && (index + size) <= bvector->count) {
                    if ((index & (size - 1)) == 0) {
                        if (FIXNUMP(argv[2])) {
                            int val = FIXNUM(argv[2]);
                            if (val >= INT16_MIN && val <= INT16_MAX) {
                                int16_t* ref = (int16_t*)&bvector->elts[index];
                                *ref = val;
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
                    invalid_argument_violation(vm, "bytevector-s16-native-set!", "index not aligned,", argv[1], 1, argc, argv);
                    return scm_undef;
                }
            }
            if (exact_integer_pred(argv[1])) {
                invalid_argument_violation(vm, "bytevector-s16-native-set!", "index out of bounds,", argv[1], 1, argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "bytevector-s16-native-set!", 1, "exact integer", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "bytevector-s16-native-set!", 0, "bytevector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "bytevector-s16-native-set!", 3, 3, argc, argv);
    return scm_undef;
}

// bytevector-u32-ref
scm_obj_t
subr_bytevector_u32_ref(VM* vm, int argc, scm_obj_t argv[])
{
    const int size = 4;
    if (argc == 3) {
        if (BVECTORP(argv[0])) {
            if (FIXNUMP(argv[1])) {
                scm_bvector_t bvector = (scm_bvector_t)argv[0];
                int index = FIXNUM(argv[1]);
                if (index >= 0 && (index + size) <= bvector->count) {
                    if (SYMBOLP(argv[2])) {
                        scm_symbol_t symbol = (scm_symbol_t)argv[2];
                        uint32_t u32;
                        if (symbol == S_LITTLE) {
                            u32 = (bvector->elts[index + 0])
                                + (bvector->elts[index + 1] << 8)
                                + (bvector->elts[index + 2] << 16)
                                + (bvector->elts[index + 3] << 24);
                            return uint32_to_integer(vm->m_heap, u32);
                        }
                        if (symbol == S_BIG) {
                            u32 = (bvector->elts[index + 0] << 24)
                                + (bvector->elts[index + 1] << 16)
                                + (bvector->elts[index + 2] << 8)
                                + (bvector->elts[index + 3]);
                            return uint32_to_integer(vm->m_heap, u32);
                        }
                    }
                    wrong_type_argument_violation(vm, "bytevector-u32-ref", 2, "endianness", argv[2], argc, argv);
                    return scm_undef;
                }
            }
            if (exact_integer_pred(argv[1])) {
                invalid_argument_violation(vm, "bytevector-u32-ref", "index out of bounds,", argv[1], 1, argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "bytevector-u32-ref", 1, "exact integer", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "bytevector-u32-ref", 0, "bytevector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "bytevector-u32-ref", 3, 3, argc, argv);
    return scm_undef;
}

// bytevector-s32-ref
scm_obj_t
subr_bytevector_s32_ref(VM* vm, int argc, scm_obj_t argv[])
{
    const int size = 4;
    if (argc == 3) {
        if (BVECTORP(argv[0])) {
            if (FIXNUMP(argv[1])) {
                scm_bvector_t bvector = (scm_bvector_t)argv[0];
                int index = FIXNUM(argv[1]);
                if (index >= 0 && (index + size) <= bvector->count) {
                    if (SYMBOLP(argv[2])) {
                        scm_symbol_t symbol = (scm_symbol_t)argv[2];
                        int32_t s32;
                        if (symbol == S_LITTLE) {
                            s32 = (bvector->elts[index + 0])
                                + (bvector->elts[index + 1] << 8)
                                + (bvector->elts[index + 2] << 16)
                                + (bvector->elts[index + 3] << 24);
                            return int32_to_integer(vm->m_heap, s32);
                        }
                        if (symbol == S_BIG) {
                            s32 = (bvector->elts[index + 0] << 24)
                                + (bvector->elts[index + 1] << 16)
                                + (bvector->elts[index + 2] << 8)
                                + (bvector->elts[index + 3]);
                            return int32_to_integer(vm->m_heap, s32);
                        }
                    }
                    wrong_type_argument_violation(vm, "bytevector-s32-ref", 2, "endianness", argv[2], argc, argv);
                    return scm_undef;
                }
            }
            if (exact_integer_pred(argv[1])) {
                invalid_argument_violation(vm, "bytevector-s32-ref", "index out of bounds,", argv[1], 1, argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "bytevector-s32-ref", 1, "exact integer", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "bytevector-s32-ref", 0, "bytevector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "bytevector-s32-ref", 3, 3, argc, argv);
    return scm_undef;
}

// bytevector-u32-native-ref
scm_obj_t
subr_bytevector_u32_native_ref(VM* vm, int argc, scm_obj_t argv[])
{
    const int size = 4;
    if (argc == 2) {
        if (BVECTORP(argv[0])) {
            if (FIXNUMP(argv[1])) {
                scm_bvector_t bvector = (scm_bvector_t)argv[0];
                int index = FIXNUM(argv[1]);
                if (index >= 0 && (index + size) <= bvector->count) {
                    if ((index & (size - 1)) == 0) {
                        uint32_t* ref = (uint32_t*)&bvector->elts[index];
                        return uint32_to_integer(vm->m_heap, *ref);
                    }
                    invalid_argument_violation(vm, "bytevector-u32-native-ref", "index not aligned,", argv[1], 1, argc, argv);
                    return scm_undef;
                }
            }
            if (exact_integer_pred(argv[1])) {
                invalid_argument_violation(vm, "bytevector-u32-native-ref", "index out of bounds,", argv[1], 1, argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "bytevector-u32-native-ref", 1, "exact integer", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "bytevector-u32-native-ref", 0, "bytevector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "bytevector-u32-native-ref", 2, 2, argc, argv);
    return scm_undef;
}

// bytevector-s32-native-ref
scm_obj_t
subr_bytevector_s32_native_ref(VM* vm, int argc, scm_obj_t argv[])
{
    const int size = 4;
    if (argc == 2) {
        if (BVECTORP(argv[0])) {
            if (FIXNUMP(argv[1])) {
                scm_bvector_t bvector = (scm_bvector_t)argv[0];
                int index = FIXNUM(argv[1]);
                if (index >= 0 && (index + size) <= bvector->count) {
                    if ((index & (size - 1)) == 0) {
                        int32_t* ref = (int32_t*)&bvector->elts[index];
                        return int32_to_integer(vm->m_heap, *ref);
                    }
                    invalid_argument_violation(vm, "bytevector-u32-native-ref", "index not aligned,", argv[1], 1, argc, argv);
                    return scm_undef;
                }
            }
            if (exact_integer_pred(argv[1])) {
                invalid_argument_violation(vm, "bytevector-u32-native-ref", "index out of bounds,", argv[1], 1, argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "bytevector-u32-native-ref", 1, "exact integer", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "bytevector-u32-native-ref", 0, "bytevector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "bytevector-u32-native-ref", 2, 2, argc, argv);
    return scm_undef;
}

// bytevector-u32-set!
scm_obj_t
subr_bytevector_u32_set(VM* vm, int argc, scm_obj_t argv[])
{
    const int size = 4;
    if (argc == 4) {
        if (BVECTORP(argv[0])) {
            if (FIXNUMP(argv[1])) {
                scm_bvector_t bvector = (scm_bvector_t)argv[0];
                int index = FIXNUM(argv[1]);
                if (index >= 0 && (index + size) <= bvector->count) {
                    if (exact_integer_pred(argv[2])) {
                        uint32_t u32;
                        if (exact_integer_to_uint32(argv[2], &u32)) {
                            if (SYMBOLP(argv[3])) {
                                scm_symbol_t symbol = (scm_symbol_t)argv[3];
                                if (symbol == S_LITTLE) {
                                    bvector->elts[index + 0] = u32 & 0xff;
                                    bvector->elts[index + 1] = u32 >> 8;
                                    bvector->elts[index + 2] = u32 >> 16;
                                    bvector->elts[index + 3] = u32 >> 24;
                                    return scm_unspecified;
                                }
                                if (symbol == S_BIG) {
                                    bvector->elts[index + 0] = u32 >> 24;
                                    bvector->elts[index + 1] = u32 >> 16;
                                    bvector->elts[index + 2] = u32 >> 8;
                                    bvector->elts[index + 3] = u32 & 0xff;
                                    return scm_unspecified;
                                }
                            }
                            wrong_type_argument_violation(vm, "bytevector-u32-set!", 3, "endianness", argv[3], argc, argv);
                            return scm_undef;
                        }
                    }
                    if (exact_integer_pred(argv[2])) {
                        invalid_argument_violation(vm, "bytevector-u32-set!", "value out of range,", argv[2], 2, argc, argv);
                        return scm_undef;
                    }
                    wrong_type_argument_violation(vm, "bytevector-u32-set!", 2, "exact integer", argv[2], argc, argv);
                    return scm_undef;
                }
            }
            if (exact_integer_pred(argv[1])) {
                invalid_argument_violation(vm, "bytevector-u32-set!", "index out of bounds,", argv[1], 1, argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "bytevector-u32-set!", 1, "exact integer", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "bytevector-u32-set!", 0, "bytevector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "bytevector-u32-set!", 4, 4, argc, argv);
    return scm_undef;
}

// bytevector-s32-set!
scm_obj_t
subr_bytevector_s32_set(VM* vm, int argc, scm_obj_t argv[])
{
    const int size = 4;
    if (argc == 4) {
        if (BVECTORP(argv[0])) {
            if (FIXNUMP(argv[1])) {
                scm_bvector_t bvector = (scm_bvector_t)argv[0];
                int index = FIXNUM(argv[1]);
                if (index >= 0 && (index + size) <= bvector->count) {
                    if (exact_integer_pred(argv[2])) {
                        int32_t s32;
                        if (exact_integer_to_int32(argv[2], &s32)) {
                            if (SYMBOLP(argv[3])) {
                                scm_symbol_t symbol = (scm_symbol_t)argv[3];
                                if (symbol == S_LITTLE) {
                                    bvector->elts[index + 0] = s32 & 0xff;
                                    bvector->elts[index + 1] = (uint32_t)s32 >> 8;
                                    bvector->elts[index + 2] = (uint32_t)s32 >> 16;
                                    bvector->elts[index + 3] = (uint32_t)s32 >> 24;
                                    return scm_unspecified;
                                }
                                if (symbol == S_BIG) {
                                    bvector->elts[index + 0] = (uint32_t)s32 >> 24;
                                    bvector->elts[index + 1] = (uint32_t)s32 >> 16;
                                    bvector->elts[index + 2] = (uint32_t)s32 >> 8;
                                    bvector->elts[index + 3] = s32 & 0xff;
                                    return scm_unspecified;
                                }
                            }
                            wrong_type_argument_violation(vm, "bytevector-s32-set!", 3, "endianness", argv[3], argc, argv);
                            return scm_undef;
                        }
                    }
                    if (exact_integer_pred(argv[2])) {
                        invalid_argument_violation(vm, "bytevector-s32-set!", "value out of range,", argv[2], 2, argc, argv);
                        return scm_undef;
                    }
                    wrong_type_argument_violation(vm, "bytevector-s32-set!", 2, "exact integer", argv[2], argc, argv);
                    return scm_undef;
                }
            }
            if (exact_integer_pred(argv[1])) {
                invalid_argument_violation(vm, "bytevector-s32-set!", "index out of bounds,", argv[1], 1, argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "bytevector-s32-set!", 1, "exact integer", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "bytevector-s32-set!", 0, "bytevector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "bytevector-s32-set!", 4, 4, argc, argv);
    return scm_undef;
}

// bytevector-u32-native-set!
scm_obj_t
subr_bytevector_u32_native_set(VM* vm, int argc, scm_obj_t argv[])
{
    const int size = 4;
    if (argc == 3) {
        if (BVECTORP(argv[0])) {
            if (FIXNUMP(argv[1])) {
                scm_bvector_t bvector = (scm_bvector_t)argv[0];
                int index = FIXNUM(argv[1]);
                if (index >= 0 && (index + size) <= bvector->count) {
                    if ((index & (size - 1)) == 0) {
                        if (exact_integer_pred(argv[2])) {
                            uint32_t u32;
                            if (exact_integer_to_uint32(argv[2], &u32)) {
                                uint32_t* ref = (uint32_t*)&bvector->elts[index];
                                *ref = u32;
                                return scm_unspecified;
                            }
                        }
                        if (exact_integer_pred(argv[2])) {
                            invalid_argument_violation(vm, "bytevector-u32-native-set!", "value out of range,", argv[2], 2, argc, argv);
                            return scm_undef;
                        }
                        wrong_type_argument_violation(vm, "bytevector-u32-native-set!", 2, "exact integer", argv[2], argc, argv);
                        return scm_undef;
                    }
                    invalid_argument_violation(vm, "bytevector-u32-native-set!", "index not aligned,", argv[1], 1, argc, argv);
                    return scm_undef;
                }
            }
            if (exact_integer_pred(argv[1])) {
                invalid_argument_violation(vm, "bytevector-u32-native-set!", "index out of bounds,", argv[1], 1, argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "bytevector-u32-native-set!", 1, "exact integer", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "bytevector-u32-native-set!", 0, "bytevector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "bytevector-u32-native-set!", 3, 3, argc, argv);
    return scm_undef;
}

// bytevector-s32-native-set!
scm_obj_t
subr_bytevector_s32_native_set(VM* vm, int argc, scm_obj_t argv[])
{
    const int size = 4;
    if (argc == 3) {
        if (BVECTORP(argv[0])) {
            if (FIXNUMP(argv[1])) {
                scm_bvector_t bvector = (scm_bvector_t)argv[0];
                int index = FIXNUM(argv[1]);
                if (index >= 0 && (index + size) <= bvector->count) {
                    if ((index & (size - 1)) == 0) {
                        if (exact_integer_pred(argv[2])) {
                            int32_t s32;
                            if (exact_integer_to_int32(argv[2], &s32)) {
                                int32_t* ref = (int32_t*)&bvector->elts[index];
                                *ref = s32;
                                return scm_unspecified;
                            }
                        }
                        if (exact_integer_pred(argv[2])) {
                            invalid_argument_violation(vm, "bytevector-s32-native-set!", "value out of range,", argv[2], 2, argc, argv);
                            return scm_undef;
                        }
                        wrong_type_argument_violation(vm, "bytevector-s32-native-set!", 2, "exact integer", argv[2], argc, argv);
                        return scm_undef;
                    }
                    invalid_argument_violation(vm, "bytevector-s32-native-set!", "index not aligned,", argv[1], 1, argc, argv);
                    return scm_undef;
                }
            }
            if (exact_integer_pred(argv[1])) {
                invalid_argument_violation(vm, "bytevector-s32-native-set!", "index out of bounds,", argv[1], 1, argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "bytevector-s32-native-set!", 1, "exact integer", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "bytevector-s32-native-set!", 0, "bytevector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "bytevector-s32-native-set!", 3, 3, argc, argv);
    return scm_undef;
}

// bytevector-u64-ref
scm_obj_t
subr_bytevector_u64_ref(VM* vm, int argc, scm_obj_t argv[])
{
    const int size = 8;
    if (argc == 3) {
        if (BVECTORP(argv[0])) {
            if (FIXNUMP(argv[1])) {
                scm_bvector_t bvector = (scm_bvector_t)argv[0];
                int index = FIXNUM(argv[1]);
                if (index >= 0 && (index + size) <= bvector->count) {
                    if (SYMBOLP(argv[2])) {
                        scm_symbol_t symbol = (scm_symbol_t)argv[2];
                        uint64_t u64;
                        if (symbol == S_LITTLE) {
                            u64 = ((uint64_t)bvector->elts[index + 0])
                                + ((uint64_t)bvector->elts[index + 1] << 8)
                                + ((uint64_t)bvector->elts[index + 2] << 16)
                                + ((uint64_t)bvector->elts[index + 3] << 24)
                                + ((uint64_t)bvector->elts[index + 4] << 32)
                                + ((uint64_t)bvector->elts[index + 5] << 40)
                                + ((uint64_t)bvector->elts[index + 6] << 48)
                                + ((uint64_t)bvector->elts[index + 7] << 56);
                            return uint64_to_integer(vm->m_heap, u64);
                        }
                        if (symbol == S_BIG) {
                            u64 = ((uint64_t)bvector->elts[index + 0] << 56)
                                + ((uint64_t)bvector->elts[index + 1] << 48)
                                + ((uint64_t)bvector->elts[index + 2] << 40)
                                + ((uint64_t)bvector->elts[index + 3] << 32)
                                + ((uint64_t)bvector->elts[index + 4] << 24)
                                + ((uint64_t)bvector->elts[index + 5] << 16)
                                + ((uint64_t)bvector->elts[index + 6] << 8)
                                + ((uint64_t)bvector->elts[index + 7]);
                            return uint64_to_integer(vm->m_heap, u64);
                        }
                    }
                    wrong_type_argument_violation(vm, "bytevector-u64-ref", 2, "endianness", argv[2], argc, argv);
                    return scm_undef;
                }
            }
            if (exact_integer_pred(argv[1])) {
                invalid_argument_violation(vm, "bytevector-u64-ref", "index out of bounds,", argv[1], 1, argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "bytevector-u64-ref", 1, "exact integer", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "bytevector-u64-ref", 0, "bytevector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "bytevector-u64-ref", 3, 3, argc, argv);
    return scm_undef;
}

// bytevector-s64-ref
scm_obj_t
subr_bytevector_s64_ref(VM* vm, int argc, scm_obj_t argv[])
{
    const int size = 8;
    if (argc == 3) {
        if (BVECTORP(argv[0])) {
            if (FIXNUMP(argv[1])) {
                scm_bvector_t bvector = (scm_bvector_t)argv[0];
                int index = FIXNUM(argv[1]);
                if (index >= 0 && (index + size) <= bvector->count) {
                    if (SYMBOLP(argv[2])) {
                        scm_symbol_t symbol = (scm_symbol_t)argv[2];
                        int64_t s64;
                        if (symbol == S_LITTLE) {
                            s64 = ((uint64_t)bvector->elts[index + 0])
                                + ((uint64_t)bvector->elts[index + 1] << 8)
                                + ((uint64_t)bvector->elts[index + 2] << 16)
                                + ((uint64_t)bvector->elts[index + 3] << 24)
                                + ((uint64_t)bvector->elts[index + 4] << 32)
                                + ((uint64_t)bvector->elts[index + 5] << 40)
                                + ((uint64_t)bvector->elts[index + 6] << 48)
                                + ((uint64_t)bvector->elts[index + 7] << 56);
                            return int64_to_integer(vm->m_heap, s64);
                        }
                        if (symbol == S_BIG) {
                            s64 = ((uint64_t)bvector->elts[index + 0] << 56)
                                + ((uint64_t)bvector->elts[index + 1] << 48)
                                + ((uint64_t)bvector->elts[index + 2] << 40)
                                + ((uint64_t)bvector->elts[index + 3] << 32)
                                + ((uint64_t)bvector->elts[index + 4] << 24)
                                + ((uint64_t)bvector->elts[index + 5] << 16)
                                + ((uint64_t)bvector->elts[index + 6] << 8)
                                + ((uint64_t)bvector->elts[index + 7]);
                            return int64_to_integer(vm->m_heap, s64);
                        }
                    }
                    wrong_type_argument_violation(vm, "bytevector-s64-ref", 2, "endianness", argv[2], argc, argv);
                    return scm_undef;
                }
            }
            if (exact_integer_pred(argv[1])) {
                invalid_argument_violation(vm, "bytevector-s64-ref", "index out of bounds,", argv[1], 1, argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "bytevector-s64-ref", 1, "exact integer", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "bytevector-s64-ref", 0, "bytevector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "bytevector-s64-ref", 3, 3, argc, argv);
    return scm_undef;
}

// bytevector-u64-native-ref
scm_obj_t
subr_bytevector_u64_native_ref(VM* vm, int argc, scm_obj_t argv[])
{
    const int size = 8;
    if (argc == 2) {
        if (BVECTORP(argv[0])) {
            if (FIXNUMP(argv[1])) {
                scm_bvector_t bvector = (scm_bvector_t)argv[0];
                int index = FIXNUM(argv[1]);
                if (index >= 0 && (index + size) <= bvector->count) {
                    if ((index & (size - 1)) == 0) {
                        uint64_t* ref = (uint64_t*)&bvector->elts[index];
                        return uint64_to_integer(vm->m_heap, *ref);
                    }
                    invalid_argument_violation(vm, "bytevector-u64-native-ref", "index not aligned,", argv[1], 1, argc, argv);
                    return scm_undef;
                }
            }
            if (exact_integer_pred(argv[1])) {
                invalid_argument_violation(vm, "bytevector-u64-native-ref", "index out of bounds,", argv[1], 1, argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "bytevector-u64-native-ref", 1, "exact integer", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "bytevector-u64-native-ref", 0, "bytevector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "bytevector-u64-native-ref", 2, 2, argc, argv);
    return scm_undef;
}

// bytevector-s64-native-ref
scm_obj_t
subr_bytevector_s64_native_ref(VM* vm, int argc, scm_obj_t argv[])
{
    const int size = 8;
    if (argc == 2) {
        if (BVECTORP(argv[0])) {
            if (FIXNUMP(argv[1])) {
                scm_bvector_t bvector = (scm_bvector_t)argv[0];
                int index = FIXNUM(argv[1]);
                if (index >= 0 && (index + size) <= bvector->count) {
                    if ((index & (size - 1)) == 0) {
                        int64_t* ref = (int64_t*)&bvector->elts[index];
                        return int64_to_integer(vm->m_heap, *ref);
                    }
                    invalid_argument_violation(vm, "bytevector-u64-native-ref", "index not aligned,", argv[1], 1, argc, argv);
                    return scm_undef;
                }
            }
            if (exact_integer_pred(argv[1])) {
                invalid_argument_violation(vm, "bytevector-u64-native-ref", "index out of bounds,", argv[1], 1, argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "bytevector-u64-native-ref", 1, "exact integer", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "bytevector-u64-native-ref", 0, "bytevector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "bytevector-u64-native-ref", 2, 2, argc, argv);
    return scm_undef;
}

// bytevector-u64-set!
scm_obj_t
subr_bytevector_u64_set(VM* vm, int argc, scm_obj_t argv[])
{
    const int size = 8;
    if (argc == 4) {
        if (BVECTORP(argv[0])) {
            if (FIXNUMP(argv[1])) {
                scm_bvector_t bvector = (scm_bvector_t)argv[0];
                int index = FIXNUM(argv[1]);
                if (index >= 0 && (index + size) <= bvector->count) {
                    if (exact_integer_pred(argv[2])) {
                        union {
                            uint64_t u64;
                            uint8_t bytes[8];
                        } datum;
                        if (exact_integer_to_uint64(argv[2], &datum.u64)) {
                            if (SYMBOLP(argv[3])) {
                                scm_symbol_t symbol = (scm_symbol_t)argv[3];
                                if (symbol == S_BIG || symbol == S_LITTLE) {
                                    if (symbol == NATIVE_ENDIANNESS) {
                                        for (int i = 0; i < size; i++) bvector->elts[index + i] = datum.bytes[i];
                                    } else {
                                        for (int i = 0; i < size; i++) bvector->elts[index + i] = datum.bytes[size - i - 1];
                                    }
                                    return scm_unspecified;
                                }
                            }
                            wrong_type_argument_violation(vm, "bytevector-u64-set!", 3, "endianness", argv[3], argc, argv);
                            return scm_undef;
                        }
                    }
                    if (exact_integer_pred(argv[2])) {
                        invalid_argument_violation(vm, "bytevector-u64-set!", "value out of range,", argv[2], 2, argc, argv);
                        return scm_undef;
                    }
                    wrong_type_argument_violation(vm, "bytevector-u64-set!", 2, "exact integer", argv[2], argc, argv);
                    return scm_undef;
                }
            }
            if (exact_integer_pred(argv[1])) {
                invalid_argument_violation(vm, "bytevector-u64-set!", "index out of bounds,", argv[1], 1, argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "bytevector-u64-set!", 1, "exact integer", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "bytevector-u64-set!", 0, "bytevector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "bytevector-u64-set!", 4, 4, argc, argv);
    return scm_undef;
}

// bytevector-s64-set!
scm_obj_t
subr_bytevector_s64_set(VM* vm, int argc, scm_obj_t argv[])
{
    const int size = 8;
    if (argc == 4) {
        if (BVECTORP(argv[0])) {
            if (FIXNUMP(argv[1])) {
                scm_bvector_t bvector = (scm_bvector_t)argv[0];
                int index = FIXNUM(argv[1]);
                if (index >= 0 && (index + size) <= bvector->count) {
                    if (exact_integer_pred(argv[2])) {
                        union {
                            int64_t s64;
                            uint8_t bytes[8];
                        } datum;
                        if (exact_integer_to_int64(argv[2], &datum.s64)) {
                            if (SYMBOLP(argv[3])) {
                                scm_symbol_t symbol = (scm_symbol_t)argv[3];
                                if (symbol == S_BIG || symbol == S_LITTLE) {
                                    if (symbol == NATIVE_ENDIANNESS) {
                                        for (int i = 0; i < size; i++) bvector->elts[index + i] = datum.bytes[i];
                                    } else {
                                        for (int i = 0; i < size; i++) bvector->elts[index + i] = datum.bytes[size - i - 1];
                                    }
                                    return scm_unspecified;
                                }
                            }
                            wrong_type_argument_violation(vm, "bytevector-s64-set!", 3, "endianness", argv[3], argc, argv);
                            return scm_undef;
                        }
                    }
                    if (exact_integer_pred(argv[2])) {
                        invalid_argument_violation(vm, "bytevector-s64-set!", "value out of range,", argv[2], 2, argc, argv);
                        return scm_undef;
                    }
                    wrong_type_argument_violation(vm, "bytevector-s64-set!", 2, "exact integer", argv[2], argc, argv);
                    return scm_undef;
                }
            }
            if (exact_integer_pred(argv[1])) {
                invalid_argument_violation(vm, "bytevector-s64-set!", "index out of bounds,", argv[1], 1, argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "bytevector-s64-set!", 1, "exact integer", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "bytevector-s64-set!", 0, "bytevector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "bytevector-s64-set!", 4, 4, argc, argv);
    return scm_undef;
}

// bytevector-u64-native-set!
scm_obj_t
subr_bytevector_u64_native_set(VM* vm, int argc, scm_obj_t argv[])
{
    const int size = 8;
    if (argc == 3) {
        if (BVECTORP(argv[0])) {
            if (FIXNUMP(argv[1])) {
                scm_bvector_t bvector = (scm_bvector_t)argv[0];
                int index = FIXNUM(argv[1]);
                if (index >= 0 && (index + size) <= bvector->count) {
                    if ((index & (size - 1)) == 0) {
                        if (exact_integer_pred(argv[2])) {
                            uint64_t u64;
                            if (exact_integer_to_uint64(argv[2], &u64)) {
                                uint64_t* ref = (uint64_t*)&bvector->elts[index];
                                *ref = u64;
                                return scm_unspecified;
                            }
                        }
                        if (exact_integer_pred(argv[2])) {
                            invalid_argument_violation(vm, "bytevector-u64-native-set!", "value out of range,", argv[2], 2, argc, argv);
                            return scm_undef;
                        }
                        wrong_type_argument_violation(vm, "bytevector-u64-native-set!", 2, "exact integer", argv[2], argc, argv);
                        return scm_undef;
                    }
                    invalid_argument_violation(vm, "bytevector-u64-native-set!", "index not aligned,", argv[1], 1, argc, argv);
                    return scm_undef;
                }
            }
            if (exact_integer_pred(argv[1])) {
                invalid_argument_violation(vm, "bytevector-u64-native-set!", "index out of bounds,", argv[1], 1, argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "bytevector-u64-native-set!", 1, "exact integer", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "bytevector-u64-native-set!", 0, "bytevector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "bytevector-u64-native-set!", 3, 3, argc, argv);
    return scm_undef;
}

// bytevector-s64-native-set!
scm_obj_t
subr_bytevector_s64_native_set(VM* vm, int argc, scm_obj_t argv[])
{
    const int size = 8;
    if (argc == 3) {
        if (BVECTORP(argv[0])) {
            if (FIXNUMP(argv[1])) {
                scm_bvector_t bvector = (scm_bvector_t)argv[0];
                int index = FIXNUM(argv[1]);
                if (index >= 0 && (index + size) <= bvector->count) {
                    if ((index & (size - 1)) == 0) {
                        if (exact_integer_pred(argv[2])) {
                            int64_t s64;
                            if (exact_integer_to_int64(argv[2], &s64)) {
                                int64_t* ref = (int64_t*)&bvector->elts[index];
                                *ref = s64;
                                return scm_unspecified;
                            }
                        }
                        if (exact_integer_pred(argv[2])) {
                            invalid_argument_violation(vm, "bytevector-s64-native-set!", "value out of range,", argv[2], 2, argc, argv);
                            return scm_undef;
                        }
                        wrong_type_argument_violation(vm, "bytevector-s64-native-set!", 2, "exact integer", argv[2], argc, argv);
                        return scm_undef;
                    }
                    invalid_argument_violation(vm, "bytevector-s64-native-set!", "index not aligned,", argv[1], 1, argc, argv);
                    return scm_undef;
                }
            }
            if (exact_integer_pred(argv[1])) {
                invalid_argument_violation(vm, "bytevector-s64-native-set!", "index out of bounds,", argv[1], 1, argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "bytevector-s64-native-set!", 1, "exact integer", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "bytevector-s64-native-set!", 0, "bytevector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "bytevector-s64-native-set!", 3, 3, argc, argv);
    return scm_undef;
}

// bytevector-ieee-single-ref
scm_obj_t
subr_bytevector_ieee_single_ref(VM* vm, int argc, scm_obj_t argv[])
{
    const int size = 4;
    if (argc == 3) {
        if (BVECTORP(argv[0])) {
            if (FIXNUMP(argv[1])) {
                scm_bvector_t bvector = (scm_bvector_t)argv[0];
                int index = FIXNUM(argv[1]);
                if (index >= 0 && (index + size) <= bvector->count) {
                    if (SYMBOLP(argv[2])) {
                        scm_symbol_t symbol = (scm_symbol_t)argv[2];
                        union {
                            float ieee_single;
                            uint8_t bytes[4];
                        } datum;
                        if (symbol == S_BIG || symbol == S_LITTLE) {
                            if (symbol == NATIVE_ENDIANNESS) {
                                for (int i = 0; i < size; i++) datum.bytes[i] = bvector->elts[index + i];
                            } else {
                                for (int i = 0; i < size; i++) datum.bytes[i] = bvector->elts[index + size - 1 - i];
                            }
                            return make_flonum(vm->m_heap, datum.ieee_single);
                        }
                    }
                    wrong_type_argument_violation(vm, "bytevector-ieee_double-ref", 2, "endianness", argv[2], argc, argv);
                    return scm_undef;
                }
            }
            if (exact_integer_pred(argv[1])) {
                invalid_argument_violation(vm, "bytevector-ieee_double-ref", "index out of bounds,", argv[1], 1, argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "bytevector-ieee_double-ref", 1, "exact integer", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "bytevector-ieee_double-ref", 0, "bytevector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "bytevector-ieee_double-ref", 3, 3, argc, argv);
    return scm_undef;
}

// bytevector-ieee-single-native-ref
scm_obj_t
subr_bytevector_ieee_single_native_ref(VM* vm, int argc, scm_obj_t argv[])
{
    const int size = 4;
    if (argc == 2) {
        if (BVECTORP(argv[0])) {
            if (FIXNUMP(argv[1])) {
                scm_bvector_t bvector = (scm_bvector_t)argv[0];
                int index = FIXNUM(argv[1]);
                if (index >= 0 && (index + size) <= bvector->count) {
                    if ((index & (size - 1)) == 0) {
                        float* ref = (float*)&bvector->elts[index];
                        return double_to_inexact(vm->m_heap, *ref);
                    }
                    invalid_argument_violation(vm, "bytevector-ieee-single-native-ref", "index not aligned,", argv[1], 1, argc, argv);
                    return scm_undef;
                }
            }
            if (exact_integer_pred(argv[1])) {
                invalid_argument_violation(vm, "bytevector-ieee-single-native-ref", "index out of bounds,", argv[1], 1, argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "bytevector-ieee-single-native-ref", 1, "exact integer", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "bytevector-ieee-single-native-ref", 0, "bytevector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "bytevector-ieee-single-native-ref", 2, 2, argc, argv);
    return scm_undef;
}

// bytevector-ieee-single-set!
scm_obj_t
subr_bytevector_ieee_single_set(VM* vm, int argc, scm_obj_t argv[])
{
    const int size = 4;
    if (argc == 4) {
        if (BVECTORP(argv[0])) {
            if (FIXNUMP(argv[1])) {
                scm_bvector_t bvector = (scm_bvector_t)argv[0];
                int index = FIXNUM(argv[1]);
                if (index >= 0 && (index + size) <= bvector->count) {
                    if (real_pred(argv[2])) {
                        union {
                            float ieee_single;
                            uint8_t bytes[4];
                        } datum;
                        datum.ieee_single = real_to_double(argv[2]);
                        if (SYMBOLP(argv[3])) {
                            scm_symbol_t symbol = (scm_symbol_t)argv[3];
                            if (symbol == S_BIG || symbol == S_LITTLE) {
                                if (symbol == NATIVE_ENDIANNESS) {
                                    for (int i = 0; i < size; i++) bvector->elts[index + i] = datum.bytes[i];
                                } else {
                                    for (int i = 0; i < size; i++) bvector->elts[index + i] = datum.bytes[size - 1 - i];
                                }
                                return scm_unspecified;
                            }                       }
                        wrong_type_argument_violation(vm, "bytevector-ieee-single-set!", 3, "endianness", argv[3], argc, argv);
                        return scm_undef;
                    }
                    wrong_type_argument_violation(vm, "bytevector-ieee-single-set!", 2, "real", argv[2], argc, argv);
                    return scm_undef;
                }
            }
            if (exact_integer_pred(argv[1])) {
                invalid_argument_violation(vm, "bytevector-ieee-single-set!", "index out of bounds,", argv[1], 1, argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "bytevector-ieee-single-set!", 1, "exact integer", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "bytevector-ieee-single-set!", 0, "bytevector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "bytevector-ieee-single-set!", 4, 4, argc, argv);
    return scm_undef;
}

// bytevector-ieee-single-native-set!
scm_obj_t
subr_bytevector_ieee_single_native_set(VM* vm, int argc, scm_obj_t argv[])
{
    const int size = 4;
    if (argc == 3) {
        if (BVECTORP(argv[0])) {
            if (FIXNUMP(argv[1])) {
                scm_bvector_t bvector = (scm_bvector_t)argv[0];
                int index = FIXNUM(argv[1]);
                if (index >= 0 && (index + size) <= bvector->count) {
                    if ((index & (size - 1)) == 0) {
                        if (real_pred(argv[2])) {
                            float* ref = (float*)&bvector->elts[index];
                            *ref = real_to_double(argv[2]);
                            return scm_unspecified;
                        }
                        wrong_type_argument_violation(vm, "bytevector-ieee-single-native-set!", 2, "real", argv[2], argc, argv);
                        return scm_undef;
                    }
                    invalid_argument_violation(vm, "bytevector-ieee-single-native-set!", "index not aligned,", argv[1], 1, argc, argv);
                    return scm_undef;
                }
            }
            if (exact_integer_pred(argv[1])) {
                invalid_argument_violation(vm, "bytevector-ieee-single-native-set!", "index out of bounds,", argv[1], 1, argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "bytevector-ieee-single-native-set!", 1, "exact integer", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "bytevector-ieee-single-native-set!", 0, "bytevector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "bytevector-ieee-single-native-set!", 3, 3, argc, argv);
    return scm_undef;
}

// bytevector-ieee-double-ref
scm_obj_t
subr_bytevector_ieee_double_ref(VM* vm, int argc, scm_obj_t argv[])
{
    const int size = 8;
    if (argc == 3) {
        if (BVECTORP(argv[0])) {
            if (FIXNUMP(argv[1])) {
                scm_bvector_t bvector = (scm_bvector_t)argv[0];
                int index = FIXNUM(argv[1]);
                if (index >= 0 && (index + size) <= bvector->count) {
                    if (SYMBOLP(argv[2])) {
                        scm_symbol_t symbol = (scm_symbol_t)argv[2];
                        union {
                            double ieee_double;
                            uint8_t bytes[8];
                        } datum;
                        if (symbol == S_BIG || symbol == S_LITTLE) {
                            if (symbol == NATIVE_ENDIANNESS) {
                                for (int i = 0; i < size; i++) datum.bytes[i] = bvector->elts[index + i];
                            } else {
                                for (int i = 0; i < size; i++) datum.bytes[i] = bvector->elts[index + size - 1 - i];
                            }
                            return make_flonum(vm->m_heap, datum.ieee_double);
                        }
                    }
                    wrong_type_argument_violation(vm, "bytevector-ieee_double-ref", 2, "endianness", argv[2], argc, argv);
                    return scm_undef;
                }
            }
            if (exact_integer_pred(argv[1])) {
                invalid_argument_violation(vm, "bytevector-ieee_double-ref", "index out of bounds,", argv[1], 1, argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "bytevector-ieee_double-ref", 1, "exact integer", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "bytevector-ieee_double-ref", 0, "bytevector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "bytevector-ieee_double-ref", 3, 3, argc, argv);
    return scm_undef;
}

// bytevector-ieee-double-native-ref
scm_obj_t
subr_bytevector_ieee_double_native_ref(VM* vm, int argc, scm_obj_t argv[])
{
    const int size = 8;
    if (argc == 2) {
        if (BVECTORP(argv[0])) {
            if (FIXNUMP(argv[1])) {
                scm_bvector_t bvector = (scm_bvector_t)argv[0];
                int index = FIXNUM(argv[1]);
                if (index >= 0 && (index + size) <= bvector->count) {
                    if ((index & (size - 1)) == 0) {
                        double* ref = (double*)&bvector->elts[index];
                        return double_to_inexact(vm->m_heap, *ref);
                    }
                    invalid_argument_violation(vm, "bytevector-ieee-double-native-ref", "index not aligned,", argv[1], 1, argc, argv);
                    return scm_undef;
                }
            }
            if (exact_integer_pred(argv[1])) {
                invalid_argument_violation(vm, "bytevector-ieee-double-native-ref", "index out of bounds,", argv[1], 1, argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "bytevector-ieee-double-native-ref", 1, "exact integer", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "bytevector-ieee-double-native-ref", 0, "bytevector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "bytevector-ieee-double-native-ref", 2, 2, argc, argv);
    return scm_undef;
}

// bytevector-ieee-double-set!
scm_obj_t
subr_bytevector_ieee_double_set(VM* vm, int argc, scm_obj_t argv[])
{
    const int size = 8;
    if (argc == 4) {
        if (BVECTORP(argv[0])) {
            if (FIXNUMP(argv[1])) {
                scm_bvector_t bvector = (scm_bvector_t)argv[0];
                int index = FIXNUM(argv[1]);
                if (index >= 0 && (index + size) <= bvector->count) {
                    if (real_pred(argv[2])) {
                        union {
                            double ieee_double;
                            uint8_t bytes[8];
                        } datum;
                        datum.ieee_double = real_to_double(argv[2]);
                        if (SYMBOLP(argv[3])) {
                            scm_symbol_t symbol = (scm_symbol_t)argv[3];
                            if (symbol == S_BIG || symbol == S_LITTLE) {
                                if (symbol == NATIVE_ENDIANNESS) {
                                    for (int i = 0; i < size; i++) bvector->elts[index + i] = datum.bytes[i];
                                } else {
                                    for (int i = 0; i < size; i++) bvector->elts[index + i] = datum.bytes[size - 1 - i];
                                }
                                return scm_unspecified;
                            }
                        }
                        wrong_type_argument_violation(vm, "bytevector-ieee-double-set!", 3, "endianness", argv[3], argc, argv);
                        return scm_undef;
                    }
                    wrong_type_argument_violation(vm, "bytevector-ieee-double-set!", 2, "real", argv[2], argc, argv);
                    return scm_undef;
                }
            }
            if (exact_integer_pred(argv[1])) {
                invalid_argument_violation(vm, "bytevector-ieee-double-set!", "index out of bounds,", argv[1], 1, argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "bytevector-ieee-double-set!", 1, "exact integer", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "bytevector-ieee-double-set!", 0, "bytevector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "bytevector-ieee-double-set!", 4, 4, argc, argv);
    return scm_undef;
}

// bytevector-ieee-double-native-set!
scm_obj_t
subr_bytevector_ieee_double_native_set(VM* vm, int argc, scm_obj_t argv[])
{
    const int size = 8;
    if (argc == 3) {
        if (BVECTORP(argv[0])) {
            if (FIXNUMP(argv[1])) {
                scm_bvector_t bvector = (scm_bvector_t)argv[0];
                int index = FIXNUM(argv[1]);
                if (index >= 0 && (index + size) <= bvector->count) {
                    if ((index & (size - 1)) == 0) {
                        if (real_pred(argv[2])) {
                            double* ref = (double*)&bvector->elts[index];
                            *ref = real_to_double(argv[2]);
                            return scm_unspecified;
                        }
                        wrong_type_argument_violation(vm, "bytevector-ieee-double-native-set!", 2, "real", argv[2], argc, argv);
                        return scm_undef;
                    }
                    invalid_argument_violation(vm, "bytevector-ieee-double-native-set!", "index not aligned,", argv[1], 1, argc, argv);
                    return scm_undef;
                }
            }
            if (exact_integer_pred(argv[1])) {
                invalid_argument_violation(vm, "bytevector-ieee-double-native-set!", "index out of bounds,", argv[1], 1, argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "bytevector-ieee-double-native-set!", 1, "exact integer", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "bytevector-ieee-double-native-set!", 0, "bytevector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "bytevector-ieee-double-native-set!", 3, 3, argc, argv);
    return scm_undef;
}

// string->utf8
scm_obj_t
subr_string_utf8(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (STRINGP(argv[0])) {
            scm_string_t string = (scm_string_t)argv[0];
            int size = HDR_STRING_SIZE(string->hdr);
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
            int count = bvector->count;
            int n;
            for (n = 0; n < count; n++) {
                if (bvector->elts[n]) continue;
            }
            scm_string_t string = make_string(vm->m_heap, n, ' ');
            memcpy(string->name, bvector->elts, n);
            return string;
        }
        wrong_type_argument_violation(vm, "utf8->string", 0, "bytevector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "utf8->string", 1, 1, argc, argv);
    return scm_undef;
}

// string->cstring
scm_obj_t
subr_string_cstring(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (STRINGP(argv[0])) {
            scm_string_t string = (scm_string_t)argv[0];
            int size = HDR_STRING_SIZE(string->hdr);
            scm_bvector_t bvector = make_bvector(vm->m_heap, size + 1);
            memcpy(bvector->elts, string->name, size);
            bvector->elts[size] = 0;
            return bvector;
        }
        wrong_type_argument_violation(vm, "string->cstring", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "string->cstring", 1, 1, argc, argv);
    return scm_undef;
}

// cstring->string
scm_obj_t
subr_cstring_string(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (BVECTORP(argv[0])) {
            scm_bvector_t bvector = (scm_bvector_t)argv[0];
            int count = bvector->count;
            int n;
            for (n = 0; n < count; n++) {
                if (bvector->elts[n]) continue;
            }
            scm_string_t string = make_string(vm->m_heap, n, ' ');
            memcpy(string->name, bvector->elts, n);
            return string;
        }
        wrong_type_argument_violation(vm, "cstring->string", 0, "bytevector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "cstring->string", 1, 1, argc, argv);
    return scm_undef;
}

// make-bytevector-mapping
scm_obj_t
subr_make_bytevector_mapping(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (FIXNUMP(argv[1])) {
            int size = FIXNUM(argv[1]);
            if (exact_non_negative_integer_pred(argv[0])) {
                if (sizeof(intptr_t) == sizeof(uint32_t)) {
                    uint32_t adrs;
                    if (exact_integer_to_uint32(argv[0], &adrs)) return make_bvector_mapping(vm->m_heap, (void*)adrs, size);
                    invalid_argument_violation(vm, "make-bytevector-mapping", "value out of bounds,", argv[0], 0, argc, argv);
                    return scm_undef;
                }
                if (sizeof(intptr_t) == sizeof(uint64_t)) {
                    uint64_t adrs;
                    if (exact_integer_to_uint64(argv[0], &adrs)) return make_bvector_mapping(vm->m_heap, (void*)adrs, size);
                    invalid_argument_violation(vm, "make-bytevector-mapping", "value out of bounds,", argv[0], 0, argc, argv);
                    return scm_undef;
                }
                assert(false);
            }
            wrong_type_argument_violation(vm, "make-bytevector-mapping", 0, "exact non-negative integer", argv[0], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "make-bytevector-mapping", 1, "fixnum", argv[1], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "make-bytevector-mapping", 2, 2, argc, argv);
    return scm_undef;
}

void init_subr_bvector(object_heap_t* heap)
{
    #define DEFSUBR(SYM, FUNC)  heap->intern_system_subr(SYM, FUNC)

    DEFSUBR("native-endianness", subr_native_endianness);
    DEFSUBR("bytevector?", subr_bytevector_pred);
    DEFSUBR("make-bytevector", subr_make_bytevector);
    DEFSUBR("bytevector-length", subr_bytevector_length);
    DEFSUBR("bytevector=?", subr_bytevector_eq_pred);
    DEFSUBR("bytevector-fill!", subr_bytevector_fill);
    DEFSUBR("bytevector-copy!", subr_bytevector_destructive_copy);
    DEFSUBR("bytevector-copy", subr_bytevector_copy);
    DEFSUBR("bytevector-u8-ref", subr_bytevector_u8_ref);
    DEFSUBR("bytevector-s8-ref", subr_bytevector_s8_ref);
    DEFSUBR("bytevector-u8-set!", subr_bytevector_u8_set);
    DEFSUBR("bytevector-s8-set!", subr_bytevector_s8_set);
    DEFSUBR("bytevector->u8-list", subr_bytevector_u8_list);
    DEFSUBR("u8-list->bytevector", subr_u8_list_bytevector);
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

    DEFSUBR("string->cstring", subr_string_cstring);
    DEFSUBR("cstring->string", subr_cstring_string);

    DEFSUBR("string->utf8", subr_string_utf8);
    DEFSUBR("utf8->string", subr_utf8_string);

    DEFSUBR("make-bytevector-mapping", subr_make_bytevector_mapping);
}
