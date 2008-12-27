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

// fixnum?
scm_obj_t
subr_fixnum_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) return FIXNUMP(argv[0]) ? scm_true : scm_false;
    wrong_number_of_arguments_violation(vm, "fixnum?", 1, 1, argc, argv);
    return scm_undef;
}

// fixnum-width
scm_obj_t
subr_fixnum_width(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) return MAKEFIXNUM(FIXNUM_BITS);
    wrong_number_of_arguments_violation(vm, "fixnum-width", 0, 0, argc, argv);
    return scm_undef;
}

// least-fixnum
scm_obj_t
subr_least_fixnum(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) return MAKEFIXNUM(FIXNUM_MIN);
    wrong_number_of_arguments_violation(vm, "least-fixnum", 0, 0, argc, argv);
    return scm_undef;
}

// greatest-fixnum
scm_obj_t
subr_greatest_fixnum(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) return MAKEFIXNUM(FIXNUM_MAX);
    wrong_number_of_arguments_violation(vm, "greatest-fixnum", 0, 0, argc, argv);
    return scm_undef;
}

// fx=?
scm_obj_t
subr_fx_eq(VM* vm, int argc, scm_obj_t argv[])
{
    int bad;
    if (argc == 2) {
        if (FIXNUMP(argv[0]) & FIXNUMP(argv[1])) return (argv[0] == argv[1]) ? scm_true : scm_false;
        bad = FIXNUMP(argv[0]) ? 1 : 0; goto raise_bad;
    }
    if (argc == 1) {
        if (FIXNUMP(argv[0])) return scm_true;
        bad = 0; goto raise_bad;
    }
    if (argc >= 3) {
        for (int i = 0; i < argc; i++) {
            if (FIXNUMP(argv[i])) continue;
            bad = i; goto raise_bad;
        }
        for (int i = 0; i < argc - 1; i++) {
            if (argv[i] == argv[i + 1]) continue;
            return scm_false;
        }
        return scm_true;
    }
    wrong_number_of_arguments_violation(vm, "fx=?", 1, -1, argc, argv);
    return scm_undef;

raise_bad:
    wrong_type_argument_violation(vm, "fx=?", bad, "fixnum", argv[bad], argc, argv);
    return scm_undef;
}

// fx<?
scm_obj_t
subr_fx_lt(VM* vm, int argc, scm_obj_t argv[])
{
    int bad;
    if (argc == 2) {
        if (FIXNUMP(argv[0]) & FIXNUMP(argv[1])) return ((intptr_t)argv[0] < (intptr_t)argv[1]) ? scm_true : scm_false;
        bad = FIXNUMP(argv[0]) ? 1 : 0; goto raise_bad;
    }
    if (argc == 1) {
        if (FIXNUMP(argv[0])) return scm_true;
        bad = 0; goto raise_bad;
    }
    if (argc >= 3) {
        for (int i = 0; i < argc; i++) {
            if (FIXNUMP(argv[i])) continue;
            bad = i; goto raise_bad;
        }
        for (int i = 0; i < argc - 1; i++) {
            if ((intptr_t)argv[i] < (intptr_t)argv[i + 1]) continue;
            return scm_false;
        }
        return scm_true;
    }
    wrong_number_of_arguments_violation(vm, "fx<?", 1, -1, argc, argv);
    return scm_undef;

raise_bad:
    wrong_type_argument_violation(vm, "fx<?", bad, "fixnum", argv[bad], argc, argv);
    return scm_undef;
}

// fx>?
scm_obj_t
subr_fx_gt(VM* vm, int argc, scm_obj_t argv[])
{
    int bad;
    if (argc == 2) {
        if (FIXNUMP(argv[0]) & FIXNUMP(argv[1])) return ((intptr_t)argv[0] > (intptr_t)argv[1]) ? scm_true : scm_false;
        bad = FIXNUMP(argv[0]) ? 1 : 0; goto raise_bad;
    }
    if (argc == 1) {
        if (FIXNUMP(argv[0])) return scm_true;
        bad = 0; goto raise_bad;
    }
    if (argc >= 3) {
        for (int i = 0; i < argc; i++) {
            if (FIXNUMP(argv[i])) continue;
            bad = i; goto raise_bad;
        }
        for (int i = 0; i < argc - 1; i++) {
            if ((intptr_t)argv[i] > (intptr_t)argv[i + 1]) continue;
            return scm_false;
        }
        return scm_true;
    }
    wrong_number_of_arguments_violation(vm, "fx>?", 1, -1, argc, argv);
    return scm_undef;

raise_bad:
    wrong_type_argument_violation(vm, "fx>?", bad, "fixnum", argv[bad], argc, argv);
    return scm_undef;
}

// fx<=?
scm_obj_t
subr_fx_le(VM* vm, int argc, scm_obj_t argv[])
{
    int bad;
    if (argc == 2) {
        if (FIXNUMP(argv[0]) & FIXNUMP(argv[1])) return ((intptr_t)argv[0] <= (intptr_t)argv[1]) ? scm_true : scm_false;
        bad = FIXNUMP(argv[0]) ? 1 : 0; goto raise_bad;
    }
    if (argc == 1) {
        if (FIXNUMP(argv[0])) return scm_true;
        bad = 0; goto raise_bad;
    }
    if (argc >= 3) {
        for (int i = 0; i < argc; i++) {
            if (FIXNUMP(argv[i])) continue;
            bad = i; goto raise_bad;
        }
        for (int i = 0; i < argc - 1; i++) {
            if ((intptr_t)argv[i] <= (intptr_t)argv[i + 1]) continue;
            return scm_false;
        }
        return scm_true;
    }
    wrong_number_of_arguments_violation(vm, "fx<=?", 1, -1, argc, argv);
    return scm_undef;

raise_bad:
    wrong_type_argument_violation(vm, "fx<=?", bad, "fixnum", argv[bad], argc, argv);
    return scm_undef;
}

// fx>=?
scm_obj_t
subr_fx_ge(VM* vm, int argc, scm_obj_t argv[])
{
    int bad;
    if (argc == 2) {
        if (FIXNUMP(argv[0]) & FIXNUMP(argv[1])) return ((intptr_t)argv[0] >= (intptr_t)argv[1]) ? scm_true : scm_false;
        bad = FIXNUMP(argv[0]) ? 1 : 0; goto raise_bad;
    }
    if (argc == 1) {
        if (FIXNUMP(argv[0])) return scm_true;
        bad = 0; goto raise_bad;
    }
    if (argc >= 3) {
        for (int i = 0; i < argc; i++) {
            if (FIXNUMP(argv[i])) continue;
            bad = i; goto raise_bad;
        }
        for (int i = 0; i < argc - 1; i++) {
            if ((intptr_t)argv[i] >= (intptr_t)argv[i + 1]) continue;
            return scm_false;
        }
        return scm_true;
    }
    wrong_number_of_arguments_violation(vm, "fx>=?", 1, -1, argc, argv);
    return scm_undef;

raise_bad:
    wrong_type_argument_violation(vm, "fx>=?", bad, "fixnum", argv[bad], argc, argv);
    return scm_undef;
}

// fxzero?
scm_obj_t
subr_fx_zero_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (FIXNUMP(argv[0])) return (argv[0] == MAKEFIXNUM(0)) ? scm_true : scm_false;
        wrong_type_argument_violation(vm, "fxzero?", 0, "fixnum", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "fxzero?", 1, 1, argc, argv);
    return scm_undef;
}

// fxpositive?
scm_obj_t
subr_fx_positive_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (FIXNUMP(argv[0])) return (FIXNUM(argv[0]) > 0) ? scm_true : scm_false;
        wrong_type_argument_violation(vm, "fxpositive?", 0, "fixnum", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "fxpositive?", 1, 1, argc, argv);
    return scm_undef;
}

// fxnegative?
scm_obj_t
subr_fx_negative_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (FIXNUMP(argv[0])) return (FIXNUM(argv[0]) < 0) ? scm_true : scm_false;
        wrong_type_argument_violation(vm, "fxnegative?", 0, "fixnum", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "fxnegative?", 1, 1, argc, argv);
    return scm_undef;
}

// fxodd?
scm_obj_t
subr_fx_odd_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (FIXNUMP(argv[0])) return (FIXNUM(argv[0]) & 1) ? scm_true : scm_false;
        wrong_type_argument_violation(vm, "fxodd?", 0, "fixnum", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "fxodd?", 1, 1, argc, argv);
    return scm_undef;
}

// fxeven?
scm_obj_t
subr_fx_even_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (FIXNUMP(argv[0])) return (FIXNUM(argv[0]) & 1) ? scm_false : scm_true;
        wrong_type_argument_violation(vm, "fxeven?", 0, "fixnum", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "fxeven?", 1, 1, argc, argv);
    return scm_undef;
}

// fxmax
scm_obj_t
subr_fx_max(VM* vm, int argc, scm_obj_t argv[])
{
    int bad;
    if (argc == 2) {
        if (FIXNUMP(argv[0]) & FIXNUMP(argv[1])) return ((intptr_t)argv[0] > (intptr_t)argv[1]) ? argv[0] : argv[1];
        bad = FIXNUMP(argv[0]) ? 1 : 0; goto raise_bad;
    }
    if (argc == 1) {
        if (FIXNUMP(argv[0])) return argv[0];
        bad = 0; goto raise_bad;
    }
    if (argc >= 3) {
        for (int i = 0; i < argc; i++) {
            if (FIXNUMP(argv[i])) continue;
            bad = i; goto raise_bad;
        }
        intptr_t ans = FIXNUM(argv[0]);
        for (int i = 1; i < argc; i++) {
            if (ans < FIXNUM(argv[i])) ans = FIXNUM(argv[i]);
        }
        return MAKEFIXNUM(ans);
    }
    wrong_number_of_arguments_violation(vm, "fxmax", 1, -1, argc, argv);
    return scm_undef;

raise_bad:
    wrong_type_argument_violation(vm, "fxmax", bad, "fixnum", argv[bad], argc, argv);
    return scm_undef;
}

// fxmin
scm_obj_t
subr_fx_min(VM* vm, int argc, scm_obj_t argv[])
{
    int bad;
    if (argc == 2) {
        if (FIXNUMP(argv[0]) & FIXNUMP(argv[1])) return ((intptr_t)argv[0] < (intptr_t)argv[1]) ? argv[0] : argv[1];
        bad = FIXNUMP(argv[0]) ? 1 : 0; goto raise_bad;
    }
    if (argc == 1) {
        if (FIXNUMP(argv[0])) return argv[0];
        bad = 0; goto raise_bad;
    }
    if (argc >= 3) {
        for (int i = 0; i < argc; i++) {
            if (FIXNUMP(argv[i])) continue;
            bad = i; goto raise_bad;
        }
        intptr_t ans = FIXNUM(argv[0]);
        for (int i = 1; i < argc; i++) {
            if (ans > FIXNUM(argv[i])) ans = FIXNUM(argv[i]);
        }
        return MAKEFIXNUM(ans);
    }
    wrong_number_of_arguments_violation(vm, "fxmin", 1, -1, argc, argv);
    return scm_undef;

raise_bad:
    wrong_type_argument_violation(vm, "fxmin", bad, "fixnum", argv[bad], argc, argv);
    return scm_undef;
}

// fx+
scm_obj_t
subr_fx_add(VM* vm, int argc, scm_obj_t argv[])
{
    int bad;
    if (argc == 2) {
        if (FIXNUMP(argv[0]) & FIXNUMP(argv[1])) {
            intptr_t n = FIXNUM(argv[0]) + FIXNUM(argv[1]);
            if ((n >= FIXNUM_MIN) & (n <= FIXNUM_MAX)) return MAKEFIXNUM(n);
            implementation_restriction_violation(vm, "fx+", "return value out of fixnum range", intptr_to_integer(vm->m_heap, n), argc, argv);
            return scm_undef;
        }
        bad = FIXNUMP(argv[0]) ? 1 : 0; goto raise_bad;
    }
    wrong_number_of_arguments_violation(vm, "fx+", 2, 2, argc, argv);
    return scm_undef;

raise_bad:
    wrong_type_argument_violation(vm, "fx+", bad, "fixnum", argv[bad], argc, argv);
    return scm_undef;
}

// fx*
scm_obj_t
subr_fx_mul(VM* vm, int argc, scm_obj_t argv[])
{
    int bad;
    if (argc == 2) {
        if (FIXNUMP(argv[0]) & FIXNUMP(argv[1])) {
            int64_t n = (int64_t)FIXNUM(argv[0]) * FIXNUM(argv[1]);
            if ((n >= FIXNUM_MIN) & (n <= FIXNUM_MAX)) return MAKEFIXNUM(n);
            implementation_restriction_violation(vm, "fx*", "return value out of fixnum range", int64_to_integer(vm->m_heap, n), argc, argv);
            return scm_undef;
        }
        bad = FIXNUMP(argv[0]) ? 1 : 0; goto raise_bad;
    }
    wrong_number_of_arguments_violation(vm, "fx*", 2, 2, argc, argv);
    return scm_undef;

raise_bad:
    wrong_type_argument_violation(vm, "fx*", bad, "fixnum", argv[bad], argc, argv);
    return scm_undef;
}

// fx-
scm_obj_t
subr_fx_sub(VM* vm, int argc, scm_obj_t argv[])
{
    int bad;
    if (argc == 2) {
        if (FIXNUMP(argv[0]) & FIXNUMP(argv[1])) {
            intptr_t n = FIXNUM(argv[0]) - FIXNUM(argv[1]);
            if ((n >= FIXNUM_MIN) & (n <= FIXNUM_MAX)) return MAKEFIXNUM(n);
            implementation_restriction_violation(vm, "fx-", "return value out of fixnum range", intptr_to_integer(vm->m_heap, n), argc, argv);
            return scm_undef;
        }
        bad = FIXNUMP(argv[0]) ? 1 : 0; goto raise_bad;
    }
    if (argc == 1) {
        intptr_t n = FIXNUM(argv[0]);
        if (n != FIXNUM_MIN) return MAKEFIXNUM(-n);
        implementation_restriction_violation(vm, "fx-", "return value out of fixnum range", intptr_to_integer(vm->m_heap, -n), argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "fx-", 1, 2, argc, argv);
    return scm_undef;

raise_bad:
    wrong_type_argument_violation(vm, "fx-", bad, "fixnum", argv[bad], argc, argv);
    return scm_undef;
}

// fxdiv
scm_obj_t
subr_fx_div(VM* vm, int argc, scm_obj_t argv[])
{
    int bad;
    if (argc == 2) {
        if (argv[1] == MAKEFIXNUM(0)) {
            invalid_argument_violation(vm, "fxdiv", "undefined for 0", NULL, 0, argc, argv);
            return scm_undef;
        }
        if (FIXNUMP(argv[0]) & FIXNUMP(argv[1])) {
            scm_obj_t obj = arith_integer_div(vm->m_heap, argv[0], argv[1]);
            if (FIXNUMP(obj)) return obj;
            implementation_restriction_violation(vm, "fxdiv", "return value out of fixnum range", obj, argc, argv);
            return scm_undef;
        }
        bad = FIXNUMP(argv[0]) ? 1 : 0; goto raise_bad;
    }
    wrong_number_of_arguments_violation(vm, "fxdiv", 2, 2, argc, argv);
    return scm_undef;

raise_bad:
    wrong_type_argument_violation(vm, "fxdiv", bad, "fixnum", argv[bad], argc, argv);
    return scm_undef;
}

// fxdiv0
scm_obj_t
subr_fx_div0(VM* vm, int argc, scm_obj_t argv[])
{
    int bad;
    if (argc == 2) {
        if (argv[1] == MAKEFIXNUM(0)) {
            invalid_argument_violation(vm, "fxdiv0", "undefined for 0", NULL, 0, argc, argv);
            return scm_undef;
        }
        if (FIXNUMP(argv[0]) & FIXNUMP(argv[1])) return arith_integer_div0(vm->m_heap, argv[0], argv[1]);
        bad = FIXNUMP(argv[0]) ? 1 : 0; goto raise_bad;
    }
    wrong_number_of_arguments_violation(vm, "fxdiv0", 2, 2, argc, argv);
    return scm_undef;

raise_bad:
    wrong_type_argument_violation(vm, "fxdiv0", bad, "fixnum", argv[bad], argc, argv);
    return scm_undef;
}

// fxnot
scm_obj_t
subr_fx_not(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (FIXNUMP(argv[0])) return MAKEFIXNUM(~FIXNUM(argv[0]));
        wrong_type_argument_violation(vm, "fxnot", 0, "fixnum", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "fxnot", 1, 1, argc, argv);
    return scm_undef;
}

// fxand
scm_obj_t
subr_fx_and(VM* vm, int argc, scm_obj_t argv[])
{
    int bad;
    if (argc == 2) {
        if (FIXNUMP(argv[0]) & FIXNUMP(argv[1])) return MAKEFIXNUM(FIXNUM(argv[0]) & FIXNUM(argv[1]));
        bad = FIXNUMP(argv[0]) ? 1 : 0; goto raise_bad;
    }
    if (argc == 1) {
        if (FIXNUMP(argv[0])) return argv[0];
        bad = 0; goto raise_bad;
    }
    if (argc >= 3) {
        intptr_t ans = -1;
        for (int i = 0; i < argc; i++) {
            if (FIXNUMP(argv[i])) continue;
            bad = i; goto raise_bad;
        }
        for (int i = 0; i < argc; i++) ans &= FIXNUM(argv[i]);
        return MAKEFIXNUM(ans);
    }
    return MAKEFIXNUM(-1);

raise_bad:
    wrong_type_argument_violation(vm, "fxand", bad, "fixnum", argv[bad], argc, argv);
    return scm_undef;
}

// fxior
scm_obj_t
subr_fx_ior(VM* vm, int argc, scm_obj_t argv[])
{
    int bad;
    if (argc == 2) {
        if (FIXNUMP(argv[0]) & FIXNUMP(argv[1])) return MAKEFIXNUM(FIXNUM(argv[0]) | FIXNUM(argv[1]));
        bad = FIXNUMP(argv[0]) ? 1 : 0; goto raise_bad;
    }
    if (argc == 1) {
        if (FIXNUMP(argv[0])) return argv[0];
        bad = 0; goto raise_bad;
    }
    if (argc >= 3) {
        intptr_t ans = 0;
        for (int i = 0; i < argc; i++) {
            if (FIXNUMP(argv[i])) continue;
            bad = i; goto raise_bad;
        }
        for (int i = 0; i < argc; i++) ans |= FIXNUM(argv[i]);
        return MAKEFIXNUM(ans);
    }
    return MAKEFIXNUM(0);

raise_bad:
    wrong_type_argument_violation(vm, "fxior", bad, "fixnum", argv[bad], argc, argv);
    return scm_undef;
}

// fxxor
scm_obj_t
subr_fx_xor(VM* vm, int argc, scm_obj_t argv[])
{
    int bad;
    if (argc == 2) {
        if (FIXNUMP(argv[0]) & FIXNUMP(argv[1])) return MAKEFIXNUM(FIXNUM(argv[0]) ^ FIXNUM(argv[1]));
        bad = FIXNUMP(argv[0]) ? 1 : 0; goto raise_bad;
    }
    if (argc == 1) {
        if (FIXNUMP(argv[0])) return argv[0];
        bad = 0; goto raise_bad;
    }
    if (argc >= 3) {
        intptr_t ans = 0;
        for (int i = 0; i < argc; i++) {
            if (FIXNUMP(argv[i])) continue;
            bad = i; goto raise_bad;
        }
        for (int i = 0; i < argc; i++) ans ^= FIXNUM(argv[i]);
        return MAKEFIXNUM(ans);
    }
    return MAKEFIXNUM(0);

raise_bad:
    wrong_type_argument_violation(vm, "fxxor", bad, "fixnum", argv[bad], argc, argv);
    return scm_undef;
}

// fxif
scm_obj_t
subr_fx_if(VM* vm, int argc, scm_obj_t argv[])
{
    int bad;
    if (argc == 3) {
        if (FIXNUMP(argv[0]) & FIXNUMP(argv[1]) & FIXNUMP(argv[1])) {
            intptr_t fx1 = FIXNUM(argv[0]);
            intptr_t fx2 = FIXNUM(argv[1]);
            intptr_t fx3 = FIXNUM(argv[2]);
            return MAKEFIXNUM((fx1 & fx2) | ((~fx1) & fx3));
        }
        if(!FIXNUMP(argv[0])) bad = 0; goto raise_bad;
        if(!FIXNUMP(argv[1])) bad = 1; goto raise_bad;
        if(!FIXNUMP(argv[2])) bad = 2; goto raise_bad;
    }
    wrong_number_of_arguments_violation(vm, "fxif", 3, 3, argc, argv);
    return scm_undef;

raise_bad:
    wrong_type_argument_violation(vm, "fxif", bad, "fixnum", argv[bad], argc, argv);
    return scm_undef;
}

// fxbit-count
scm_obj_t
subr_fx_bit_count(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (FIXNUMP(argv[0])) return arith_bit_count(vm->m_heap, argv[0]);
        wrong_type_argument_violation(vm, "fxbit-count", 0, "fixnum", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "fxbit-count", 1, 1, argc, argv);
    return scm_undef;
}

// fxlength
scm_obj_t
subr_fx_length(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (FIXNUMP(argv[0])) return arith_bit_length(vm->m_heap, argv[0]);
        wrong_type_argument_violation(vm, "fxlength", 0, "fixnum", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "fxlength", 1, 1, argc, argv);
    return scm_undef;
}

// fxfirst-bit-set
scm_obj_t
subr_fx_first_bit_set(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (FIXNUMP(argv[0])) return arith_first_bit_set(vm->m_heap, argv[0]);
        wrong_type_argument_violation(vm, "fxfirst-bit-set", 0, "fixnum", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "fxfirst-bit-set", 1, 1, argc, argv);
    return scm_undef;
}

// fxbit-set?
scm_obj_t
subr_fx_bit_set_pred(VM* vm, int argc, scm_obj_t argv[])
{
    int bad;
    if (argc == 2) {
        if (FIXNUMP(argv[0]) & FIXNUMP(argv[1])) {
            intptr_t fx1 = FIXNUM(argv[0]);
            intptr_t fx2 = FIXNUM(argv[1]);
            if (fx2 >= 0 && fx2 < FIXNUM_BITS) return ((fx1 & ((intptr_t)1 << fx2)) != 0) ? scm_true : scm_false;
            wrong_type_argument_violation(vm, "fxbit-set?", 1, "non-negative fixnum less than fixnum width", argv[1], argc, argv);
            return scm_undef;
        }
        bad = FIXNUMP(argv[0]) ? 1 : 0; goto raise_bad;
    }
    wrong_number_of_arguments_violation(vm, "fxbit-set?", 2, 2, argc, argv);
    return scm_undef;

raise_bad:
    wrong_type_argument_violation(vm, "fxbit-set?", bad, "fixnum", argv[bad], argc, argv);
    return scm_undef;
}

// fxcopy-bit
scm_obj_t
subr_fx_copy_bit(VM* vm, int argc, scm_obj_t argv[])
{
    int bad;
    if (argc == 3) {
        if (FIXNUMP(argv[0]) & FIXNUMP(argv[1]) & FIXNUMP(argv[1])) {
            intptr_t fx1 = FIXNUM(argv[0]);
            intptr_t fx2 = FIXNUM(argv[1]);
            intptr_t fx3 = FIXNUM(argv[2]);
            if (fx2 >= 0 && fx2 < FIXNUM_BITS) {
                if ((fx3 == 0) | (fx3 == 1)) {
                    intptr_t mask = ((intptr_t)1 << fx2);
                    return MAKEFIXNUM((mask & (fx3 << fx2)) | ((~mask) & fx1));
                }
                wrong_type_argument_violation(vm, "fxcopy-bit", 2, "0 or 1", argv[2], argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "fxcopy-bit", 1, "non-negative fixnum less than fixnum width", argv[1], argc, argv);
            return scm_undef;
        }
        if(!FIXNUMP(argv[0])) bad = 0; goto raise_bad;
        if(!FIXNUMP(argv[1])) bad = 1; goto raise_bad;
        if(!FIXNUMP(argv[2])) bad = 2; goto raise_bad;
    }
    wrong_number_of_arguments_violation(vm, "fxcopy-bit", 3, 3, argc, argv);
    return scm_undef;

raise_bad:
    wrong_type_argument_violation(vm, "fxcopy-bit", bad, "fixnum", argv[bad], argc, argv);
    return scm_undef;
}

// fxarithmetic-shift
scm_obj_t
subr_fx_arithmetic_shift(VM* vm, int argc, scm_obj_t argv[])
{
    int bad;
    if (argc == 2) {
        if (FIXNUMP(argv[0]) & FIXNUMP(argv[1])) {
            intptr_t fx1 = FIXNUM(argv[0]);
            intptr_t fx2 = FIXNUM(argv[1]);
            if (fx2 > -FIXNUM_BITS && fx2 < FIXNUM_BITS) {
                intptr_t n;
                if (fx2 > 0) n = fx1 << fx2;
                else n = fx1 >> (-fx2);
                if ((n >= FIXNUM_MIN) & (n <= FIXNUM_MAX)) return MAKEFIXNUM(n);
                implementation_restriction_violation(vm,
                                                     "fxarithmetic-shift",
                                                     "return value out of fixnum range",
                                                     intptr_to_integer(vm->m_heap, n),
                                                     argc,
                                                     argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "fxarithmetic-shift", 1, "absolute value less than fixnum width", argv[1], argc, argv);
            return scm_undef;
        }
        bad = FIXNUMP(argv[0]) ? 1 : 0; goto raise_bad;
    }
    wrong_number_of_arguments_violation(vm, "fxarithmetic-shift", 2, 2, argc, argv);
    return scm_undef;

raise_bad:
    wrong_type_argument_violation(vm, "fxarithmetic-shift", bad, "fixnum", argv[bad], argc, argv);
    return scm_undef;
}

// fxarithmetic-shift-left
scm_obj_t
subr_fx_arithmetic_shift_left(VM* vm, int argc, scm_obj_t argv[])
{
    int bad;
    if (argc == 2) {
        if (FIXNUMP(argv[0]) & FIXNUMP(argv[1])) {
            intptr_t fx1 = FIXNUM(argv[0]);
            intptr_t fx2 = FIXNUM(argv[1]);
            if (fx2 >= 0 && fx2 < FIXNUM_BITS) {
                intptr_t n = fx1 << fx2;
                if ((n >= FIXNUM_MIN) & (n <= FIXNUM_MAX)) return MAKEFIXNUM(n);
                implementation_restriction_violation(vm,
                                                     "fxarithmetic-shift-left",
                                                     "return value out of fixnum range",
                                                     intptr_to_integer(vm->m_heap, n),
                                                     argc,
                                                     argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "fxarithmetic-shift-left", 1, "non-negative fixnum less than fixnum width", argv[1], argc, argv);
            return scm_undef;
        }
        bad = FIXNUMP(argv[0]) ? 1 : 0; goto raise_bad;
    }
    wrong_number_of_arguments_violation(vm, "fxarithmetic-shift-left", 2, 2, argc, argv);
    return scm_undef;

raise_bad:
    wrong_type_argument_violation(vm, "fxarithmetic-shift-left", bad, "fixnum", argv[bad], argc, argv);
    return scm_undef;
}

// fxarithmetic-shift-right
scm_obj_t
subr_fx_arithmetic_shift_right(VM* vm, int argc, scm_obj_t argv[])
{
    int bad;
    if (argc == 2) {
        if (FIXNUMP(argv[0]) & FIXNUMP(argv[1])) {
            intptr_t fx1 = FIXNUM(argv[0]);
            intptr_t fx2 = FIXNUM(argv[1]);
            if (fx2 >= 0 && fx2 < FIXNUM_BITS) {
                intptr_t n = fx1 >> fx2;
                if ((n >= FIXNUM_MIN) & (n <= FIXNUM_MAX)) return MAKEFIXNUM(n);
                implementation_restriction_violation(vm,
                                                     "fxarithmetic-shift-right",
                                                     "return value out of fixnum range",
                                                     intptr_to_integer(vm->m_heap, n),
                                                     argc,
                                                     argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "fxarithmetic-shift-right", 1, "non-negative fixnum less than fixnum width", argv[1], argc, argv);
            return scm_undef;
        }
        bad = FIXNUMP(argv[0]) ? 1 : 0; goto raise_bad;
    }
    wrong_number_of_arguments_violation(vm, "fxarithmetic-shift-right", 2, 2, argc, argv);
    return scm_undef;

raise_bad:
    wrong_type_argument_violation(vm, "fxarithmetic-shift-right", bad, "fixnum", argv[bad], argc, argv);
    return scm_undef;
}

// fxbit-field
scm_obj_t
subr_fx_bit_field(VM* vm, int argc, scm_obj_t argv[])
{
    int bad;
    if (argc == 3) {
        if (FIXNUMP(argv[0]) & FIXNUMP(argv[1]) & FIXNUMP(argv[1])) {
            intptr_t fx1 = FIXNUM(argv[0]);
            intptr_t fx2 = FIXNUM(argv[1]);
            intptr_t fx3 = FIXNUM(argv[2]);
            if (fx2 >= 0 && fx2 < FIXNUM_BITS) {
                if (fx3 >= 0 && fx3 < FIXNUM_BITS) {
                    if (fx2 <= fx3) {
                        intptr_t mask = ~((intptr_t)-1 << fx3);
                        return MAKEFIXNUM((fx1 & mask) >> fx2);
                    }
                    invalid_argument_violation(vm, "fxbit-field", "value out of range,", argv[2], 2, argc, argv);
                    return scm_undef;
                }
                wrong_type_argument_violation(vm, "fxbit-field", 2, "non-negative fixnum less than fixnum width", argv[2], argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "fxbit-field", 1, "non-negative fixnum less than fixnum width", argv[1], argc, argv);
            return scm_undef;
        }
        if(!FIXNUMP(argv[0])) bad = 0; goto raise_bad;
        if(!FIXNUMP(argv[1])) bad = 1; goto raise_bad;
        if(!FIXNUMP(argv[2])) bad = 2; goto raise_bad;
    }
    wrong_number_of_arguments_violation(vm, "fxbit-field", 3, 3, argc, argv);
    return scm_undef;

raise_bad:
    wrong_type_argument_violation(vm, "fxbit-field", bad, "fixnum", argv[bad], argc, argv);
    return scm_undef;
}

// fxcopy-bit-field
scm_obj_t
subr_fx_copy_bit_field(VM* vm, int argc, scm_obj_t argv[])
{
    int bad;
    if (argc == 4) {
        if (FIXNUMP(argv[0]) & FIXNUMP(argv[1]) & FIXNUMP(argv[1]) & FIXNUMP(argv[1])) {
            intptr_t fx1 = FIXNUM(argv[0]);
            intptr_t fx2 = FIXNUM(argv[1]);
            intptr_t fx3 = FIXNUM(argv[2]);
            intptr_t fx4 = FIXNUM(argv[3]);
            if (fx2 >= 0 && fx2 < FIXNUM_BITS) {
                if (fx3 >= 0 && fx3 < FIXNUM_BITS) {
                    if (fx2 <= fx3) {
                        intptr_t mask1 = ((intptr_t)-1 << fx2);
                        intptr_t mask2 = ~((intptr_t)-1 << fx3);
                        intptr_t mask = mask1 & mask2;
                        return MAKEFIXNUM((mask & (fx4 << fx2)) | ((~mask) & fx1));
                    }
                    invalid_argument_violation(vm, "fxcopy-bit-field", "value out of range,", argv[2], 2, argc, argv);
                    return scm_undef;
                }
                wrong_type_argument_violation(vm, "fxcopy-bit-field", 2, "non-negative fixnum less than fixnum width", argv[2], argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "fxcopy-bit-field", 1, "non-negative fixnum less than fixnum width", argv[1], argc, argv);
            return scm_undef;
        }
        if(!FIXNUMP(argv[0])) bad = 0; goto raise_bad;
        if(!FIXNUMP(argv[1])) bad = 1; goto raise_bad;
        if(!FIXNUMP(argv[2])) bad = 2; goto raise_bad;
        if(!FIXNUMP(argv[3])) bad = 3; goto raise_bad;
    }
    wrong_number_of_arguments_violation(vm, "fxcopy-bit-field", 4, 4, argc, argv);
    return scm_undef;

raise_bad:
    wrong_type_argument_violation(vm, "fxcopy-bit-field", bad, "fixnum", argv[bad], argc, argv);
    return scm_undef;
}

void init_subr_fixnum(object_heap_t* heap)
{
#define DEFSUBR(SYM, FUNC)  heap->intern_system_subr(SYM, FUNC)

    DEFSUBR("fixnum?", subr_fixnum_pred);
    DEFSUBR("fixnum-width", subr_fixnum_width);
    DEFSUBR("least-fixnum", subr_least_fixnum);
    DEFSUBR("greatest-fixnum", subr_greatest_fixnum);
    DEFSUBR("fx=?", subr_fx_eq);
    DEFSUBR("fx<?", subr_fx_lt);
    DEFSUBR("fx>?", subr_fx_gt);
    DEFSUBR("fx<=?", subr_fx_le);
    DEFSUBR("fx>=?", subr_fx_ge);
    DEFSUBR("fxzero?", subr_fx_zero_pred);
    DEFSUBR("fxpositive?", subr_fx_positive_pred);
    DEFSUBR("fxnegative?", subr_fx_negative_pred);
    DEFSUBR("fxodd?", subr_fx_odd_pred);
    DEFSUBR("fxeven?", subr_fx_even_pred);
    DEFSUBR("fxmax", subr_fx_max);
    DEFSUBR("fxmin", subr_fx_min);
    DEFSUBR("fx+", subr_fx_add);
    DEFSUBR("fx*", subr_fx_mul);
    DEFSUBR("fx-", subr_fx_sub);
    DEFSUBR("fxdiv", subr_fx_div);
    DEFSUBR("fxdiv0", subr_fx_div0);
    DEFSUBR("fxnot", subr_fx_not);
    DEFSUBR("fxand", subr_fx_and);
    DEFSUBR("fxior", subr_fx_ior);
    DEFSUBR("fxxor", subr_fx_xor);
    DEFSUBR("fxif", subr_fx_if);
    DEFSUBR("fxbit-count", subr_fx_bit_count);
    DEFSUBR("fxlength", subr_fx_length);
    DEFSUBR("fxfirst-bit-set", subr_fx_first_bit_set);
    DEFSUBR("fxbit-set?", subr_fx_bit_set_pred);
    DEFSUBR("fxcopy-bit", subr_fx_copy_bit);
    DEFSUBR("fxarithmetic-shift", subr_fx_arithmetic_shift);
    DEFSUBR("fxarithmetic-shift-left", subr_fx_arithmetic_shift_left);
    DEFSUBR("fxarithmetic-shift-right", subr_fx_arithmetic_shift_right);
    DEFSUBR("fxbit-field", subr_fx_bit_field);
    DEFSUBR("fxcopy-bit-field", subr_fx_copy_bit_field);
}
