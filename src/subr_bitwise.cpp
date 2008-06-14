/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#include "core.h"
#include "vm.h"
#include "heap.h"
#include "subr.h"
#include "arith.h"
#include "violation.h"

// bitwise-bit-count
scm_obj_t
subr_bitwise_bit_count(VM* vm, int argc, scm_obj_t argv[])
{
	if (argc == 1) {
        if (exact_integer_pred(argv[0])) return arith_bit_count(vm->m_heap, argv[0]);
		wrong_type_argument_violation(vm, "bitwise-bit-count", 0, "exact integer", argv[0], argc, argv);
		return scm_undef;
	}
	wrong_number_of_arguments_violation(vm, "bitwise-bit-count", 1, 1, argc, argv);
	return scm_undef;
}

// bitwise-length
scm_obj_t
subr_bitwise_length(VM* vm, int argc, scm_obj_t argv[])
{
	if (argc == 1) {
        if (exact_integer_pred(argv[0])) return arith_bit_length(vm->m_heap, argv[0]);
		wrong_type_argument_violation(vm, "bitwise-length", 0, "exact integer", argv[0], argc, argv);
		return scm_undef;
	}
	wrong_number_of_arguments_violation(vm, "bitwise-length", 1, 1, argc, argv);
	return scm_undef;
}

// bitwise-first-bit-set
scm_obj_t
subr_bitwise_first_bit_set(VM* vm, int argc, scm_obj_t argv[])
{
	if (argc == 1) {
        if (exact_integer_pred(argv[0])) return arith_first_bit_set(vm->m_heap, argv[0]);
		wrong_type_argument_violation(vm, "bitwise-first-bit-set", 0, "exact integer", argv[0], argc, argv);
		return scm_undef;
	}
	wrong_number_of_arguments_violation(vm, "bitwise-first-bit-set", 1, 1, argc, argv);
	return scm_undef;
}

// bitwise-not
scm_obj_t
subr_bitwise_not(VM* vm, int argc, scm_obj_t argv[])
{
	if (argc == 1) {
        if (exact_integer_pred(argv[0])) return arith_lognot(vm->m_heap, argv[0]);
		wrong_type_argument_violation(vm, "bitwise-not", 0, "exact integer", argv[0], argc, argv);
		return scm_undef;
	}
	wrong_number_of_arguments_violation(vm, "bitwise-not", 1, 1, argc, argv);
	return scm_undef;
}

// bitwise-and
scm_obj_t
subr_bitwise_and(VM* vm, int argc, scm_obj_t argv[])
{
	if (argc == 2) {
        if (exact_integer_pred(argv[0])) {
            if (exact_integer_pred(argv[1])) {
                return arith_logand(vm->m_heap, argv[0], argv[1]);
            }
            wrong_type_argument_violation(vm, "bitwise-and", 1, "exact integer", argv[1], argc, argv);
            return scm_undef;
        }
		wrong_type_argument_violation(vm, "bitwise-and", 0, "exact integer", argv[0], argc, argv);
		return scm_undef;
	}
	if (argc == 1) {
        if (exact_integer_pred(argv[0])) {
            return argv[0];
        }
		wrong_type_argument_violation(vm, "bitwise-and", 0, "exact integer", argv[0], argc, argv);
		return scm_undef;        
	}
	if (argc == 0) return MAKEFIXNUM(-1);
	for (int i = 0; i < argc; i++) {
		if (exact_integer_pred(argv[i])) continue;
		wrong_type_argument_violation(vm, "bitwise-and", i, "exact integer", argv[i], argc, argv);
		return scm_undef;
	}
	scm_obj_t acc = argv[0];
	for (int i = 1; i < argc; i++) {
		acc = arith_logand(vm->m_heap, acc, argv[i]);
	}
	return acc;
}

// bitwise-ior
scm_obj_t
subr_bitwise_ior(VM* vm, int argc, scm_obj_t argv[])
{
	if (argc == 2) {
        if (exact_integer_pred(argv[0])) {
            if (exact_integer_pred(argv[1])) {
                return arith_logior(vm->m_heap, argv[0], argv[1]);
            }
            wrong_type_argument_violation(vm, "bitwise-ior", 1, "exact integer", argv[1], argc, argv);
            return scm_undef;
        }
		wrong_type_argument_violation(vm, "bitwise-ior", 0, "exact integer", argv[0], argc, argv);
		return scm_undef;
	}
	if (argc == 1) {
        if (exact_integer_pred(argv[0])) {
            return argv[0];
        }
		wrong_type_argument_violation(vm, "bitwise-ior", 0, "exact integer", argv[0], argc, argv);
		return scm_undef;        
	}
	if (argc == 0) return MAKEFIXNUM(0);
	for (int i = 0; i < argc; i++) {
		if (exact_integer_pred(argv[i])) continue;
		wrong_type_argument_violation(vm, "bitwise-ior", i, "exact integer", argv[i], argc, argv);
		return scm_undef;
	}
	scm_obj_t acc = argv[0];
	for (int i = 1; i < argc; i++) {
		acc = arith_logior(vm->m_heap, acc, argv[i]);
	}
	return acc;
}

// bitwise-xor
scm_obj_t
subr_bitwise_xor(VM* vm, int argc, scm_obj_t argv[])
{
	if (argc == 2) {
        if (exact_integer_pred(argv[0])) {
            if (exact_integer_pred(argv[1])) {
                return arith_logxor(vm->m_heap, argv[0], argv[1]);
            }
            wrong_type_argument_violation(vm, "bitwise-xor", 1, "exact integer", argv[1], argc, argv);
            return scm_undef;
        }
		wrong_type_argument_violation(vm, "bitwise-xor", 0, "exact integer", argv[0], argc, argv);
		return scm_undef;
	}
	if (argc == 1) {
        if (exact_integer_pred(argv[0])) {
            return argv[0];
        }
		wrong_type_argument_violation(vm, "bitwise-xor", 0, "exact integer", argv[0], argc, argv);
		return scm_undef;        
	}
	if (argc == 0) return MAKEFIXNUM(0);
	for (int i = 0; i < argc; i++) {
		if (exact_integer_pred(argv[i])) continue;
		wrong_type_argument_violation(vm, "bitwise-xor", i, "exact integer", argv[i], argc, argv);
		return scm_undef;
	}
	scm_obj_t acc = argv[0];
	for (int i = 1; i < argc; i++) {
		acc = arith_logxor(vm->m_heap, acc, argv[i]);
	}
	return acc;
}

// bitwise-arithmetic-shift
scm_obj_t
subr_bitwise_arithmetic_shift(VM* vm, int argc, scm_obj_t argv[])
{
	if (argc == 2) {
        if (exact_integer_pred(argv[0])) {
            if (FIXNUMP(argv[1])) {
                return arith_logash(vm->m_heap, argv[0], argv[1]);
            }
            wrong_type_argument_violation(vm, "bitwise-arithmetic-shift", 1, "fixnum", argv[1], argc, argv);
            return scm_undef;
        }
		wrong_type_argument_violation(vm, "bitwise-arithmetic-shift", 0, "exact integer", argv[0], argc, argv);
		return scm_undef;
	}
	wrong_number_of_arguments_violation(vm, "bitwise-arithmetic-shift", 1, 1, argc, argv);
	return scm_undef;
}

void init_subr_bitwise(object_heap_t* heap)
{
    #define	DEFSUBR(SYM, FUNC)	heap->intern_system_subr(SYM, FUNC)
	
    DEFSUBR("bitwise-not", subr_bitwise_not);
	DEFSUBR("bitwise-and", subr_bitwise_and);
	DEFSUBR("bitwise-ior", subr_bitwise_ior);
	DEFSUBR("bitwise-xor", subr_bitwise_xor);
	DEFSUBR("bitwise-arithmetic-shift", subr_bitwise_arithmetic_shift);
	DEFSUBR("bitwise-first-bit-set", subr_bitwise_first_bit_set);
	DEFSUBR("bitwise-length", subr_bitwise_length);
	DEFSUBR("bitwise-bit-count", subr_bitwise_bit_count);
}
