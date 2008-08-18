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

// 9.9.4.1 Numerical type predicates

// number?
scm_obj_t
subr_number_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        return number_pred(argv[0]) ? scm_true : scm_false;
    }
    wrong_number_of_arguments_violation(vm, "number?", 1, 1, argc, argv);
    return scm_undef;
}

// complex?
scm_obj_t
subr_complex_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        return number_pred(argv[0]) ? scm_true : scm_false;
    }
    wrong_number_of_arguments_violation(vm, "complex?", 1, 1, argc, argv);
    return scm_undef;
}

// real?
scm_obj_t
subr_real_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        return real_pred(argv[0]) ? scm_true : scm_false;
    }
    wrong_number_of_arguments_violation(vm, "real?", 1, 1, argc, argv);
    return scm_undef;
}

// rational?
scm_obj_t
subr_rational_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        return rational_pred(argv[0]) ? scm_true : scm_false;
    }
    wrong_number_of_arguments_violation(vm, "rational?", 1, 1, argc, argv);
    return scm_undef;
}

// integer?
scm_obj_t
subr_integer_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        return integer_pred(argv[0]) ? scm_true : scm_false;
    }
    wrong_number_of_arguments_violation(vm, "integer?", 1, 1, argc, argv);
    return scm_undef;
}

// real-valued?
scm_obj_t
subr_real_valued_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        return real_valued_pred(argv[0]) ? scm_true : scm_false;
    }
    wrong_number_of_arguments_violation(vm, "real-valued?", 1, 1, argc, argv);
    return scm_undef;
}

// rational-valued?
scm_obj_t
subr_rational_valued_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        return rational_valued_pred(argv[0]) ? scm_true : scm_false;
    }
    wrong_number_of_arguments_violation(vm, "rational-valued?", 1, 1, argc, argv);
    return scm_undef;
}

// integer-valued?
scm_obj_t
subr_integer_valued_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        return integer_valued_pred(argv[0]) ? scm_true : scm_false;
    }
    wrong_number_of_arguments_violation(vm, "integer-valued?", 1, 1, argc, argv);
    return scm_undef;
}

// exact?
scm_obj_t
subr_exact_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (number_pred(argv[0])) return n_exact_pred(argv[0]) ? scm_true : scm_false;
        wrong_type_argument_violation(vm, "exact?", 0, "number", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "exact?", 1, 1, argc, argv);
    return scm_undef;
}

// inexact?
scm_obj_t
subr_inexact_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (number_pred(argv[0])) return n_exact_pred(argv[0]) ? scm_false : scm_true;
        wrong_type_argument_violation(vm, "inexact?", 0, "number", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "inexact?", 1, 1, argc, argv);
    return scm_undef;
}

// 9.9.4.2 Generic conversions

// inexact
scm_obj_t
subr_inexact(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (number_pred(argv[0])) return cnvt_to_inexact(vm->m_heap, argv[0]);
        wrong_type_argument_violation(vm, "inexact", 0, "number", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "inexact", 1, 1, argc, argv);
    return scm_undef;
}

// exact
scm_obj_t
subr_exact(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (number_pred(argv[0])) return cnvt_to_exact(vm->m_heap, argv[0]);
        wrong_type_argument_violation(vm, "exact", 0, "number", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "exact", 1, 1, argc, argv);
    return scm_undef;
}

// 9.9.4.3 Arithmetic operations

// =
scm_obj_t
subr_num_eq(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (FIXNUMP(argv[0]) & FIXNUMP(argv[1])) return (argv[0] == argv[1]) ? scm_true : scm_false;
        if (BOTHFLONUMP(argv[0], argv[1])) return (FLONUM(argv[0]) == FLONUM(argv[1])) ? scm_true : scm_false;
        if (number_pred(argv[0])) {
            if (number_pred(argv[1])) return n_equal_pred(vm->m_heap, argv[0], argv[1]) ? scm_true : scm_false;
            wrong_type_argument_violation(vm, "=", 1, "number", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "=", 0, "number", argv[0], argc, argv);
        return scm_undef;
    }
    if (argc == 1) {
        if (number_pred(argv[0])) return scm_true;
        wrong_type_argument_violation(vm, "=", 0, "number", argv[0], argc, argv);
        return scm_undef;
    }
    if (argc >= 3) {
        for (int i = 0; i < argc; i++) {
            if (number_pred(argv[i])) continue;
            wrong_type_argument_violation(vm, "=", i, "number", argv[i], argc, argv);
            return scm_undef;
        }
        for (int i = 0; i < argc - 1; i++) {
            if (n_equal_pred(vm->m_heap, argv[i], argv[i + 1])) continue;
            return scm_false;
        }
        return scm_true;
    }
    wrong_number_of_arguments_violation(vm, "=", 1, -1, argc, argv);
    return scm_undef;
}

// <
scm_obj_t
subr_num_lt(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (FIXNUMP(argv[0]) & FIXNUMP(argv[1])) return (FIXNUM(argv[0]) < FIXNUM(argv[1])) ? scm_true : scm_false;
        if (BOTHFLONUMP(argv[0], argv[1])) return (FLONUM(argv[0]) < FLONUM(argv[1])) ? scm_true : scm_false;
        if (real_valued_pred(argv[0])) {
            if (real_valued_pred(argv[1])) {
                if (argv[0] == vm->m_heap->m_inherents[FL_NAN]) return scm_false;
                if (argv[1] == vm->m_heap->m_inherents[FL_NAN]) return scm_false;
                return (n_compare(vm->m_heap, argv[0], argv[1]) < 0) ? scm_true : scm_false;
            }
            wrong_type_argument_violation(vm, "<", 1, "real", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "<", 0, "real", argv[0], argc, argv);
        return scm_undef;
    }
    if (argc == 1) {
        if (real_valued_pred(argv[0])) {
            if (argv[0] == vm->m_heap->m_inherents[FL_NAN]) return scm_false;
            return scm_true;
        }
        wrong_type_argument_violation(vm, "<", 0, "real", argv[0], argc, argv);
        return scm_undef;
    }
    if (argc >= 3) {
        bool has_nan = false;
        for (int i = 0; i < argc; i++) {
            if (argv[i] == vm->m_heap->m_inherents[FL_NAN]) {
                has_nan = true;
                continue;
            }
            if (real_valued_pred(argv[i])) continue;
            wrong_type_argument_violation(vm, "<", i, "real", argv[i], argc, argv);
            return scm_undef;
        }
        if (has_nan) return scm_false;
        for (int i = 0; i < argc - 1; i++) {
            if (n_compare(vm->m_heap, argv[i], argv[i + 1]) < 0) continue;
            return scm_false;
        }
        return scm_true;
    }
    wrong_number_of_arguments_violation(vm, "<", 1, -1, argc, argv);
    return scm_undef;
}

// >
scm_obj_t
subr_num_gt(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (FIXNUMP(argv[0]) & FIXNUMP(argv[1])) return (FIXNUM(argv[0]) > FIXNUM(argv[1])) ? scm_true : scm_false;
        if (BOTHFLONUMP(argv[0], argv[1])) return (FLONUM(argv[0]) > FLONUM(argv[1])) ? scm_true : scm_false;
        if (real_valued_pred(argv[0])) {
            if (real_valued_pred(argv[1])) {
                if (argv[0] == vm->m_heap->m_inherents[FL_NAN]) return scm_false;
                if (argv[1] == vm->m_heap->m_inherents[FL_NAN]) return scm_false;
                return (n_compare(vm->m_heap, argv[0], argv[1]) > 0) ? scm_true : scm_false;
            }
            wrong_type_argument_violation(vm, ">", 1, "real", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, ">", 0, "real", argv[0], argc, argv);
        return scm_undef;
    }
    if (argc == 1) {
        if (real_valued_pred(argv[0])) {
            if (argv[0] == vm->m_heap->m_inherents[FL_NAN]) return scm_false;
            return scm_true;
        }
        wrong_type_argument_violation(vm, ">", 0, "real", argv[0], argc, argv);
        return scm_undef;
    }
    if (argc >= 3) {
        bool has_nan = false;
        for (int i = 0; i < argc; i++) {
            if (argv[i] == vm->m_heap->m_inherents[FL_NAN]) {
                has_nan = true;
                continue;
            }
            if (real_valued_pred(argv[i])) continue;
            wrong_type_argument_violation(vm, ">", i, "real", argv[i], argc, argv);
            return scm_undef;
        }
        if (has_nan) return scm_false;
        for (int i = 0; i < argc - 1; i++) {
            if (n_compare(vm->m_heap, argv[i], argv[i + 1]) > 0) continue;
            return scm_false;
        }
        return scm_true;
    }
    wrong_number_of_arguments_violation(vm, ">", 1, -1, argc, argv);
    return scm_undef;
}

// <=
scm_obj_t
subr_num_le(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (FIXNUMP(argv[0]) & FIXNUMP(argv[1])) return (FIXNUM(argv[0]) <= FIXNUM(argv[1])) ? scm_true : scm_false;
        if (BOTHFLONUMP(argv[0], argv[1])) return (FLONUM(argv[0]) <= FLONUM(argv[1])) ? scm_true : scm_false;
        if (real_valued_pred(argv[0])) {
            if (real_valued_pred(argv[1])) {
                if (argv[0] == vm->m_heap->m_inherents[FL_NAN]) return scm_false;
                if (argv[1] == vm->m_heap->m_inherents[FL_NAN]) return scm_false;
                return (n_compare(vm->m_heap, argv[0], argv[1]) <= 0) ? scm_true : scm_false;
            }
            wrong_type_argument_violation(vm, "<=", 1, "real", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "<=", 0, "real", argv[0], argc, argv);
        return scm_undef;
    }
    if (argc == 1) {
        if (real_valued_pred(argv[0])) {
            if (argv[0] == vm->m_heap->m_inherents[FL_NAN]) return scm_false;
            return scm_true;
        }
        wrong_type_argument_violation(vm, "<=", 0, "real", argv[0], argc, argv);
        return scm_undef;
    }
    if (argc >= 3) {
        bool has_nan = false;
        for (int i = 0; i < argc; i++) {
            if (argv[i] == vm->m_heap->m_inherents[FL_NAN]) {
                has_nan = true;
                continue;
            }
            if (real_valued_pred(argv[i])) continue;
            wrong_type_argument_violation(vm, "<=", i, "real", argv[i], argc, argv);
            return scm_undef;
        }
        if (has_nan) return scm_false;
        for (int i = 0; i < argc - 1; i++) {
            if (n_compare(vm->m_heap, argv[i], argv[i + 1]) <= 0) continue;
            return scm_false;
        }
        return scm_true;
    }
    wrong_number_of_arguments_violation(vm, "<=", 1, -1, argc, argv);
    return scm_undef;
}

// >=
scm_obj_t
subr_num_ge(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (FIXNUMP(argv[0]) & FIXNUMP(argv[1])) return (FIXNUM(argv[0]) >= FIXNUM(argv[1])) ? scm_true : scm_false;
        if (BOTHFLONUMP(argv[0], argv[1])) return (FLONUM(argv[0]) >= FLONUM(argv[1])) ? scm_true : scm_false;
        if (real_valued_pred(argv[0])) {
            if (real_valued_pred(argv[1])) {
                if (argv[0] == vm->m_heap->m_inherents[FL_NAN]) return scm_false;
                if (argv[1] == vm->m_heap->m_inherents[FL_NAN]) return scm_false;
                return (n_compare(vm->m_heap, argv[0], argv[1]) >= 0) ? scm_true : scm_false;
            }
            wrong_type_argument_violation(vm, ">=", 1, "real", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, ">=", 0, "real", argv[0], argc, argv);
        return scm_undef;
    }
    if (argc == 1) {
        if (real_valued_pred(argv[0])) {
            if (argv[0] == vm->m_heap->m_inherents[FL_NAN]) return scm_false;
            return scm_true;
        }
        wrong_type_argument_violation(vm, ">=", 0, "real", argv[0], argc, argv);
        return scm_undef;
    }
    if (argc >= 3) {
        bool has_nan = false;
        for (int i = 0; i < argc; i++) {
            if (argv[i] == vm->m_heap->m_inherents[FL_NAN]) {
                has_nan = true;
                continue;
            }
            if (real_valued_pred(argv[i])) continue;
            wrong_type_argument_violation(vm, ">=", i, "real", argv[i], argc, argv);
            return scm_undef;
        }
        if (has_nan) return scm_false;
        for (int i = 0; i < argc - 1; i++) {
            if (n_compare(vm->m_heap, argv[i], argv[i + 1]) >= 0) continue;
            return scm_false;
        }
        return scm_true;
    }
    wrong_number_of_arguments_violation(vm, ">=", 1, -1, argc, argv);
    return scm_undef;
}

// zero?
scm_obj_t
subr_zero_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (number_pred(argv[0])) return n_zero_pred(argv[0]) ? scm_true : scm_false;
        wrong_type_argument_violation(vm, "zero?", 0, "number", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "zero?", 1, 1, argc, argv);
    return scm_undef;
}

// positive?
scm_obj_t
subr_positive_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (real_valued_pred(argv[0])) return n_positive_pred(argv[0]) ? scm_true : scm_false;
        wrong_type_argument_violation(vm, "positive?", 0, "real", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "positive?", 1, 1, argc, argv);
    return scm_undef;
}

// negative?
scm_obj_t
subr_negative_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (real_valued_pred(argv[0])) return n_negative_pred(argv[0]) ? scm_true : scm_false;
        wrong_type_argument_violation(vm, "negative?", 0, "real", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "negative?", 1, 1, argc, argv);
    return scm_undef;
}

// odd?
scm_obj_t
subr_odd_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (integer_valued_pred(argv[0])) {
            return n_even_pred(argv[0]) ? scm_false : scm_true;
        }
        wrong_type_argument_violation(vm, "odd?", 0, "integer", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "odd?", 1, 1, argc, argv);
    return scm_undef;

}

// even?
scm_obj_t
subr_even_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (integer_valued_pred(argv[0])) {
            return n_even_pred(argv[0]) ? scm_true : scm_false;
        }
        wrong_type_argument_violation(vm, "even?", 0, "integer", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "even?", 1, 1, argc, argv);
    return scm_undef;
}

// finite?
scm_obj_t
subr_finite_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (real_pred(argv[0])) {
            if (FLONUMP(argv[0])) {
                scm_flonum_t flonum = (scm_flonum_t)argv[0];
                if (isnan(flonum->value)) return scm_false;
                if (isinf(flonum->value)) return scm_false;
            }
            return scm_true;
        }
        wrong_type_argument_violation(vm, "finite?", 0, "real", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "finite?", 1, 1, argc, argv);
    return scm_undef;
}

// infinite?
scm_obj_t
subr_infinite_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (real_pred(argv[0])) {
            if (FLONUMP(argv[0])) {
                scm_flonum_t flonum = (scm_flonum_t)argv[0];
                if (isinf(flonum->value)) return scm_true;
            }
            return scm_false;
        }
        wrong_type_argument_violation(vm, "infinite?", 0, "real", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "infinite?", 1, 1, argc, argv);
    return scm_undef;
}

// nan?
scm_obj_t
subr_nan_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (real_pred(argv[0])) {
            if (FLONUMP(argv[0])) {
                scm_flonum_t flonum = (scm_flonum_t)argv[0];
                if (isnan(flonum->value)) return scm_true;
            }
            return scm_false;
        }
        wrong_type_argument_violation(vm, "nan?", 0, "real", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "nan?", 1, 1, argc, argv);
    return scm_undef;
}

// max min (r6rs-aux.scm)

// +
scm_obj_t
subr_num_add(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (FIXNUMP(argv[0]) & FIXNUMP(argv[1])) return intptr_to_integer(vm->m_heap, FIXNUM(argv[0]) + FIXNUM(argv[1]));
        if (BOTHFLONUMP(argv[0], argv[1])) return make_flonum(vm->m_heap, FLONUM(argv[0]) + FLONUM(argv[1]));
        if (number_pred(argv[0])) {
            if (number_pred(argv[1])) return arith_add(vm->m_heap, argv[0], argv[1]);
            wrong_type_argument_violation(vm, "+", 1, "number", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "+", 0, "number", argv[0], argc, argv);
        return scm_undef;
    }
    if (argc == 1) {
        if (number_pred(argv[0])) return argv[0];
        wrong_type_argument_violation(vm, "+", 0, "number", argv[0], argc, argv);
        return scm_undef;
    }
    if (argc == 0) return MAKEFIXNUM(0);
    for (int i = 0; i < argc; i++) {
        if (number_pred(argv[i])) continue;
        wrong_type_argument_violation(vm, "+", i, "number", argv[i], argc, argv);
        return scm_undef;
    }
    scm_obj_t acc = argv[0];
    for (int i = 1; i < argc; i++) {
        acc = arith_add(vm->m_heap, acc, argv[i]);
    }
    return acc;
}

// -
scm_obj_t
subr_num_sub(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (FIXNUMP(argv[0]) & FIXNUMP(argv[1])) return intptr_to_integer(vm->m_heap, FIXNUM(argv[0]) - FIXNUM(argv[1]));
        if (BOTHFLONUMP(argv[0], argv[1])) return make_flonum(vm->m_heap, FLONUM(argv[0]) - FLONUM(argv[1]));
        if (number_pred(argv[0])) {
            if (number_pred(argv[1])) return arith_sub(vm->m_heap, argv[0], argv[1]);
            wrong_type_argument_violation(vm, "-", 1, "number", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "-", 0, "number", argv[0], argc, argv);
        return scm_undef;
    }
    if (argc == 1) {
        if (number_pred(argv[0])) return arith_negate(vm->m_heap, argv[0]);
        wrong_type_argument_violation(vm, "-", 0, "number", argv[0], argc, argv);
        return scm_undef;
    }
    if (argc == 0) {
        wrong_number_of_arguments_violation(vm, "-", 1, -1, argc, argv);
        return scm_undef;
    }
    for (int i = 0; i < argc; i++) {
        if (number_pred(argv[i])) continue;
        wrong_type_argument_violation(vm, "-", i, "number", argv[i], argc, argv);
        return scm_undef;
    }
    scm_obj_t acc = argv[0];
    for (int i = 1; i < argc; i++) {
        acc = arith_sub(vm->m_heap, acc, argv[i]);
    }
    return acc;
}

// *
scm_obj_t
subr_num_mul(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (FIXNUMP(argv[0]) & FIXNUMP(argv[1])) {
            int64_t n64 = (int64_t)FIXNUM(argv[0]) * FIXNUM(argv[1]);
            if ((n64 >= FIXNUM_MIN) & (n64 <= FIXNUM_MAX)) return MAKEFIXNUM((int32_t)n64);
            return int64_to_bignum(vm->m_heap, n64);
        }
        if (BOTHFLONUMP(argv[0], argv[1])) return make_flonum(vm->m_heap, FLONUM(argv[0]) * FLONUM(argv[1]));
        if (number_pred(argv[0])) {
            if (number_pred(argv[1])) return arith_mul(vm->m_heap, argv[0], argv[1]);
            wrong_type_argument_violation(vm, "*", 1, "number", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "*", 0, "number", argv[0], argc, argv);
        return scm_undef;
    }
    if (argc > 2) {
        for (int i = 0; i < argc; i++) {
            if (number_pred(argv[i])) continue;
            wrong_type_argument_violation(vm, "*", i, "number", argv[i], argc, argv);
            return scm_undef;
        }
        scm_obj_t acc = argv[0];
        for (int i = 1; i < argc; i++) {
            acc = arith_mul(vm->m_heap, acc, argv[i]);
        }
        return acc;
    }
    if (argc == 1) {
        if (number_pred(argv[0])) return argv[0];
        wrong_type_argument_violation(vm, "*", 0, "number", argv[0], argc, argv);
        return scm_undef;
    }
    if (argc == 0) return MAKEFIXNUM(1);
}

// /
scm_obj_t
subr_num_div(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (BOTHFLONUMP(argv[0], argv[1])) return make_flonum(vm->m_heap, FLONUM(argv[0]) / FLONUM(argv[1]));
        if (argv[1] == MAKEFIXNUM(0)) {
            if (number_pred(argv[0])) {
                if (n_exact_pred(argv[0])) {
                    invalid_argument_violation(vm, "/", "undefined for 0", NULL, 0, argc, argv);
                    return scm_undef;
                }
                return arith_div(vm->m_heap, argv[0], make_flonum(vm->m_heap, 0.0));
            }
        }
        if (number_pred(argv[0])) {
            if (number_pred(argv[1])) return arith_div(vm->m_heap, argv[0], argv[1]);
            wrong_type_argument_violation(vm, "/", 1, "number", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "/", 0, "number", argv[0], argc, argv);
        return scm_undef;
    }
    if (argc == 1) {
        if (number_pred(argv[0])) {
            if (argv[0] != MAKEFIXNUM(0)) return arith_inverse(vm->m_heap, argv[0]);
            invalid_argument_violation(vm, "/", "undefined for 0", NULL, 0, argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "/", 0, "number", argv[0], argc, argv);
        return scm_undef;
    }
    if (argc == 0) {
        wrong_number_of_arguments_violation(vm, "/", 1, -1, argc, argv);
        return scm_undef;
    }
    for (int i = 0; i < argc; i++) {
        if (number_pred(argv[i])) continue;
        wrong_type_argument_violation(vm, "/", i, "number", argv[i], argc, argv);
        return scm_undef;
    }
    scm_obj_t acc = argv[0];
    for (int i = 1; i < argc; i++) {
        if (argv[i] == MAKEFIXNUM(0)) {
            if (n_exact_pred(acc)) {
                invalid_argument_violation(vm, "/", "undefined for 0", NULL, i, argc, argv);
                return scm_undef;
            }
            acc = arith_div(vm->m_heap, acc, make_flonum(vm->m_heap, 0.0));
            continue;
        }
        acc = arith_div(vm->m_heap, acc, argv[i]);
    }
    return acc;
}

// abs
scm_obj_t
subr_abs(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (real_pred(argv[0])) return arith_magnitude(vm->m_heap, argv[0]);
        wrong_type_argument_violation(vm, "abs", 0, "real", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "abs", 1, 1, argc, argv);
    return scm_undef;
}

// div-and-mod -> r6rs-aux.scm

// div
scm_obj_t
subr_int_div(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (real_pred(argv[0])) {
            if (n_finite_pred(argv[0])) {
                if (real_pred(argv[1])) {
                    if (!n_zero_pred(argv[1])) {
                        return arith_integer_div(vm->m_heap, argv[0], argv[1]);
                    }
                    invalid_argument_violation(vm, "div", "undefined for 0", NULL, 0, argc, argv);
                    return scm_undef;
                }
                wrong_type_argument_violation(vm, "div", 0, "real", argv[1], argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "div", 0, "neither infinite nor a NaN", argv[0], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "div", 0, "real", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "div", 2, 2, argc, argv);
    return scm_undef;
}

// mod div0-and-mod0 -> r6rs-aux.scm

// div0
scm_obj_t
subr_int_div0(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (real_pred(argv[0])) {
            if (n_finite_pred(argv[0])) {
                if (real_pred(argv[1])) {
                    if (!n_zero_pred(argv[1])) {
                        return arith_integer_div0(vm->m_heap, argv[0], argv[1]);
                    }
                    invalid_argument_violation(vm, "div0", "undefined for 0", NULL, 0, argc, argv);
                    return scm_undef;
                }
                wrong_type_argument_violation(vm, "div0", 0, "real", argv[1], argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "div0", 0, "neither infinite nor a NaN", argv[0], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "div0", 0, "real", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "div0", 2, 2, argc, argv);
    return scm_undef;
}

// mod0 gcm lcm -> r6rs-aux.scm

// numerator
scm_obj_t
subr_numerator(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (real_valued_pred(argv[0])) {
            if (FLONUMP(argv[0]) && FLONUM(argv[0]) == 0.0) return argv[0];
            bool inexact = FLONUMP(argv[0]);
            scm_obj_t obj = cnvt_to_exact(vm->m_heap, argv[0]);
            if (RATIONALP(obj)) {
                if (inexact) return cnvt_to_inexact(vm->m_heap, ((scm_rational_t)obj)->nume);
                return ((scm_rational_t)obj)->nume;
            }
            return inexact ? cnvt_to_inexact(vm->m_heap, obj) : obj;
        }
        wrong_type_argument_violation(vm, "numerator", 0, "real", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "numerator", 1, 1, argc, argv);
    return scm_undef;
}

// denominator
scm_obj_t
subr_denominator(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (real_valued_pred(argv[0])) {
            bool inexact = FLONUMP(argv[0]);
            scm_obj_t obj = cnvt_to_exact(vm->m_heap, argv[0]);
            if (RATIONALP(obj)) {
                if (inexact) return cnvt_to_inexact(vm->m_heap, ((scm_rational_t)obj)->deno);
                return ((scm_rational_t)obj)->deno;
            }
            return inexact ? make_flonum(vm->m_heap, 1.0) : MAKEFIXNUM(1);
        }
        wrong_type_argument_violation(vm, "denominator", 0, "real", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "denominator", 1, 1, argc, argv);
    return scm_undef;
}


// floor
scm_obj_t
subr_floor(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (COMPLEXP(argv[0])) {
            scm_complex_t cn = (scm_complex_t)argv[0];
            if (n_zero_pred(cn->imag)) {
                argv[0] = cn->real;
                /*** FALL THROUGH ***/
            } else {
                wrong_type_argument_violation(vm, "floor", 0, "real", argv[0], argc, argv);
                return scm_undef;
            }
        }
        if (FLONUMP(argv[0])) {
            double value = ((scm_flonum_t)argv[0])->value;
            return make_flonum(vm->m_heap, floor(value));
        }
        if (FIXNUMP(argv[0]) || BIGNUMP(argv[0])) return argv[0];
        if (RATIONALP(argv[0])) return arith_floor(vm->m_heap, argv[0]);
        wrong_type_argument_violation(vm, "floor", 0, "number", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "floor", 1, 1, argc, argv);
    return scm_undef;
}

// ceiling
scm_obj_t
subr_ceiling(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (COMPLEXP(argv[0])) {
            scm_complex_t cn = (scm_complex_t)argv[0];
            if (n_zero_pred(cn->imag)) {
                argv[0] = cn->real;
                /*** FALL THROUGH ***/
            } else {
                wrong_type_argument_violation(vm, "ceiling", 0, "real", argv[0], argc, argv);
                return scm_undef;
            }
        }
        if (FLONUMP(argv[0])) {
            double value = ((scm_flonum_t)argv[0])->value;
            return make_flonum(vm->m_heap, ceil(value));
        }
        if (FIXNUMP(argv[0])) return argv[0];
        if (BIGNUMP(argv[0])) return argv[0];
        if (RATIONALP(argv[0])) {
            scm_rational_t rn = (scm_rational_t)argv[0];
            if (!n_negative_pred(rn->nume)) {
                return arith_add(vm->m_heap, arith_quotient(vm->m_heap, rn->nume, rn->deno), MAKEFIXNUM(1));
            }
            return arith_quotient(vm->m_heap, rn->nume, rn->deno);
        }
        wrong_type_argument_violation(vm, "ceiling", 0, "number", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "ceiling", 1, 1, argc, argv);
    return scm_undef;
}

// truncate
scm_obj_t
subr_truncate(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (COMPLEXP(argv[0])) {
            scm_complex_t cn = (scm_complex_t)argv[0];
            if (n_zero_pred(cn->imag)) {
                argv[0] = cn->real;
                /*** FALL THROUGH ***/
            } else {
                wrong_type_argument_violation(vm, "truncate", 0, "real", argv[0], argc, argv);
                return scm_undef;
            }
        }
        if (FLONUMP(argv[0])) {
            double value = ((scm_flonum_t)argv[0])->value;
            return make_flonum(vm->m_heap, trunc(value));
        }
        if (FIXNUMP(argv[0])) return argv[0];
        if (BIGNUMP(argv[0])) return argv[0];
        if (RATIONALP(argv[0])) {
            scm_rational_t rn = (scm_rational_t)argv[0];
            return arith_quotient(vm->m_heap, rn->nume, rn->deno);
        }
        wrong_type_argument_violation(vm, "truncate", 0, "number", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "truncate", 1, 1, argc, argv);
    return scm_undef;
}

// round
scm_obj_t
subr_round(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (COMPLEXP(argv[0])) {
            scm_complex_t cn = (scm_complex_t)argv[0];
            if (n_zero_pred(cn->imag)) {
                argv[0] = cn->real;
                /*** FALL THROUGH ***/
            } else {
                wrong_type_argument_violation(vm, "round", 0, "real", argv[0], argc, argv);
                return scm_undef;
            }
        }
        if (FLONUMP(argv[0])) {
            double value = ((scm_flonum_t)argv[0])->value;
            double ans = floor(value + 0.5);
            if (ans != value + 0.5) return make_flonum(vm->m_heap, ans);
            if (ans * 0.5 == floor(ans * 0.5)) return make_flonum(vm->m_heap, ans);
            return make_flonum(vm->m_heap, ans - 1.0);
        }
        if (FIXNUMP(argv[0])) return argv[0];
        if (BIGNUMP(argv[0])) return argv[0];
        if (RATIONALP(argv[0])) {
            scm_rational_t rn = (scm_rational_t)argv[0];
            bool negative = n_negative_pred(rn->nume);

            scm_rational_rec_t half;
            half.hdr = scm_hdr_rational;
            half.nume = negative ? MAKEFIXNUM(-1) : MAKEFIXNUM(1);
            half.deno = MAKEFIXNUM(2);

            scm_obj_t n_and_half = arith_add(vm->m_heap, rn, &half);
            if (RATIONALP(n_and_half)) {
                return arith_quotient(vm->m_heap, ((scm_rational_t)n_and_half)->nume, ((scm_rational_t)n_and_half)->deno);
            } else {
                if (n_even_pred(n_and_half)) return n_and_half;
                return arith_add(vm->m_heap, n_and_half, negative ? MAKEFIXNUM(1) : MAKEFIXNUM(-1));
            }
        }
        wrong_type_argument_violation(vm, "round", 0, "number", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "round", 1, 1, argc, argv);
    return scm_undef;
}

// rationalize -> r6rs-aux.scm

// exp
scm_obj_t
subr_exp(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (number_pred(argv[0])) return arith_exp(vm->m_heap, argv[0]);
        wrong_type_argument_violation(vm, "exp", 0, "number", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "exp", 1, 1, argc, argv);
    return scm_undef;
}

// log
scm_obj_t
subr_log(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (number_pred(argv[0])) {
            if (argv[0] == MAKEFIXNUM(0)) {
                invalid_argument_violation(vm, "log", "undefined for 0", NULL, 0, argc, argv);
                return scm_undef;
            }
            return arith_log(vm->m_heap, argv[0]);
        }
        wrong_type_argument_violation(vm, "log", 0, "number", argv[0], argc, argv);
        return scm_undef;
    }
    if (argc == 2) {
        if (number_pred(argv[0])) {
            if (argv[0] == MAKEFIXNUM(0)) {
                invalid_argument_violation(vm, "log", "undefined for 0", NULL, 0, argc, argv);
                return scm_undef;
            }
            if (number_pred(argv[1])) {
                if (argv[1] == MAKEFIXNUM(0)) {
                    invalid_argument_violation(vm, "log", "undefined for base 0", NULL, 0, argc, argv);
                    return scm_undef;
                }
                return arith_div(vm->m_heap, arith_log(vm->m_heap, argv[0]), arith_log(vm->m_heap, argv[1]));
            }
            wrong_type_argument_violation(vm, "log", 1, "number", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "log", 0, "number", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "log", 1, 1, argc, argv);
    return scm_undef;
}

// sin
scm_obj_t
subr_sin(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (number_pred(argv[0])) return arith_sin(vm->m_heap, argv[0]);
        wrong_type_argument_violation(vm, "sin", 0, "number", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "sin", 1, 1, argc, argv);
    return scm_undef;
}

// cos
scm_obj_t
subr_cos(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (number_pred(argv[0])) return arith_cos(vm->m_heap, argv[0]);
        wrong_type_argument_violation(vm, "cos", 0, "number", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "cos", 1, 1, argc, argv);
    return scm_undef;
}

// tan
scm_obj_t
subr_tan(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (number_pred(argv[0])) return arith_tan(vm->m_heap, argv[0]);
        wrong_type_argument_violation(vm, "tan", 0, "number", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "tan", 1, 1, argc, argv);
    return scm_undef;
}

// asin
scm_obj_t
subr_asin(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (number_pred(argv[0])) return arith_asin(vm->m_heap, argv[0]);
        wrong_type_argument_violation(vm, "asin", 0, "number", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "asin", 1, 1, argc, argv);
    return scm_undef;
}

// acos
scm_obj_t
subr_acos(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (number_pred(argv[0])) return arith_acos(vm->m_heap, argv[0]);
        wrong_type_argument_violation(vm, "acos", 0, "number", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "acos", 1, 1, argc, argv);
    return scm_undef;
}

// atan
scm_obj_t
subr_atan(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (number_pred(argv[0])) return arith_atan(vm->m_heap, argv[0]);
        wrong_type_argument_violation(vm, "atan", 0, "number", argv[0], argc, argv);
        return scm_undef;
    }
    if (argc == 2) {
        if (real_valued_pred(argv[0])) {
            if (real_valued_pred(argv[1])) {
                return arith_atan2(vm->m_heap, argv[0], argv[1]);
            }
            wrong_type_argument_violation(vm, "atan", 1, "real", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "atan", 0, "real", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "atan", 1, 2, argc, argv);
    return scm_undef;
}

// sqrt
scm_obj_t
subr_sqrt(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (number_pred(argv[0])) return arith_sqrt(vm->m_heap, argv[0]);
        wrong_type_argument_violation(vm, "sqrt", 0, "number", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "sqrt", 1, 1, argc, argv);
    return scm_undef;
}

// exact-integer-sqrt
scm_obj_t
subr_exact_integer_sqrt(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (exact_non_negative_integer_pred(argv[0])) {
            exact_integer_sqrt_ans_t ans = arith_exact_integer_sqrt(vm->m_heap, argv[0]);
            scm_values_t obj = make_values(vm->m_heap, 2);
            obj->elts[0] = ans.s;
            obj->elts[1] = ans.r;
            return obj;
        }
        wrong_type_argument_violation(vm, "exact-integer-sqrt", 0, "non-negative exact integer", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "exact-integer-sqrt", 1, 1, argc, argv);
    return scm_undef;
}

// expt
scm_obj_t
subr_expt(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (number_pred(argv[0])) {
            if (number_pred(argv[1])) {
                if (argv[0] == MAKEFIXNUM(1)) {
                   if (n_exact_pred(argv[1])) return MAKEFIXNUM(1);
                   return make_flonum(vm->m_heap, 1.0);
                }
                if (argv[0] == MAKEFIXNUM(-1) && n_exact_pred(argv[1])) {
                    if (n_even_pred(argv[1])) return MAKEFIXNUM(1);
                    return MAKEFIXNUM(-1);
                }
                if (argv[0] == MAKEFIXNUM(0)) {
                    if (n_zero_pred(argv[1])) {
                        if (n_exact_pred(argv[1])) return MAKEFIXNUM(1);
                        return make_flonum(vm->m_heap, 1.0);
                    }
                    if (real_valued_pred(argv[1])) {
                        if (n_negative_pred(argv[1])) return make_complex(vm->m_heap, VALUE_NAN, VALUE_NAN);
                        if (n_exact_pred(argv[1])) return MAKEFIXNUM(0);
                        return make_flonum(vm->m_heap, 0.0);
                    } else {
                        assert(COMPLEXP(argv[1]));
                        if (n_positive_pred(((scm_complex_t)argv[1])->real)) return MAKEFIXNUM(0);
                        return make_complex(vm->m_heap, VALUE_NAN, VALUE_NAN);
                    }
                }
                if (n_exact_pred(argv[0]) && BIGNUMP(argv[1])) {
                    invalid_argument_violation(vm, "expt", "calculated number is too big to fit into memory", NULL, 0, argc, argv);
                    return scm_undef;
                }
                return arith_expt(vm->m_heap, argv[0], argv[1]);
            }
            wrong_type_argument_violation(vm, "expt", 1, "number", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "expt", 0, "number", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "expt", 2, 2, argc, argv);
    return scm_undef;
}

// make-rectangular
scm_obj_t
subr_make_rectangular(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (real_valued_pred(argv[0])) {
            if (real_valued_pred(argv[1])) {
                scm_obj_t real = COMPLEXP(argv[0]) ? ((scm_complex_t)argv[0])->real : argv[0];
                scm_obj_t imag = COMPLEXP(argv[1]) ? ((scm_complex_t)argv[1])->real : argv[1];
                return arith_rectangular(vm->m_heap, real, imag);
            }
            wrong_type_argument_violation(vm, "make-rectangular", 1, "real", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "make-rectangular", 0, "real", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "make-rectangular", 2, 2, argc, argv);
    return scm_undef;
}

// make-polar
scm_obj_t
subr_make_polar(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (real_valued_pred(argv[0])) {
            if (real_valued_pred(argv[1])) {
                scm_obj_t real = COMPLEXP(argv[0]) ? ((scm_complex_t)argv[0])->real : argv[0];
                scm_obj_t imag = COMPLEXP(argv[1]) ? ((scm_complex_t)argv[1])->real : argv[1];
                return arith_polar(vm->m_heap, real, imag);
            }
            wrong_type_argument_violation(vm, "make-polar", 1, "real", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "make-polar", 0, "real", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "make-polar", 2, 2, argc, argv);
    return scm_undef;
}

// real-part
scm_obj_t
subr_real_part(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (COMPLEXP(argv[0])) return ((scm_complex_t)argv[0])->real;
        if (real_valued_pred(argv[0])) return argv[0];
        wrong_type_argument_violation(vm, "real-part", 0, "number", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "real-part", 1, 1, argc, argv);
    return scm_undef;
}

// imag-part
scm_obj_t
subr_imag_part(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (COMPLEXP(argv[0])) return ((scm_complex_t)argv[0])->imag;
        if (FIXNUMP(argv[0]) || FLONUMP(argv[0]) || BIGNUMP(argv[0]) || RATIONALP(argv[0])) return MAKEFIXNUM(0);
        wrong_type_argument_violation(vm, "imag-part", 0, "number", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "imag-part", 1, 1, argc, argv);
    return scm_undef;
}

// magnitude
scm_obj_t
subr_magnitude(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (number_pred(argv[0])) return arith_magnitude(vm->m_heap, argv[0]);
        wrong_type_argument_violation(vm, "magnitude", 0, "number", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "magnitude", 1, 1, argc, argv);
    return scm_undef;
}


// angle
scm_obj_t
subr_angle(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (number_pred(argv[0])) return arith_angle(vm->m_heap, argv[0]);
        wrong_type_argument_violation(vm, "angle", 0, "number", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "angle", 1, 1, argc, argv);
    return scm_undef;
}

// 9.9.4.4 Numarical Input and Output

// number->string
scm_obj_t
subr_number_to_string(VM* vm, int argc, scm_obj_t argv[])
{
    if ((argc == 1) || (argc == 2) || (argc == 3)) {
        int radix = 10;
        if (argc >= 2) {
            if (FIXNUMP(argv[1])) {
                radix = FIXNUM(argv[1]);
                switch (radix) {
                case 2: case 8: case 10: case 16: break;
                default:
                    invalid_argument_violation(vm, "number->string", "radix must be either 2, 8, 10 or 16, but got", argv[1], 1, argc, argv);
                    return scm_undef;
                }
            } else {
                if (exact_non_negative_integer_pred(argv[1])) {
                    invalid_argument_violation(vm, "number->string", "radix must be either 2, 8, 10 or 16, but got", argv[1], 1, argc, argv);
                    return scm_undef;
                }
                wrong_type_argument_violation(vm, "number->string", 1, "exact non-negative integer", argv[1], argc, argv);
                return scm_undef;
            }
            if (argc == 3) {
                if (exact_non_negative_integer_pred(argv[2])) {
                    // precision ignored
                } else {
                    wrong_type_argument_violation(vm, "number->string", 2, "exact non-negative integer", argv[2], argc, argv);
                    return scm_undef;
                }
            }
        }
        if (number_pred(argv[0])) {
            if (radix == 10 || n_exact_pred(argv[0])) return cnvt_number_to_string(vm->m_heap, argv[0], radix);
            invalid_argument_violation(vm, "number->string", "radix must be 10 for inexact, but got", argv[1], 1, argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "number->string", 0, "number", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "number->string", 1, 3, argc, argv);
    return scm_undef;
}

// string->number
scm_obj_t
subr_string_to_number(VM* vm, int argc, scm_obj_t argv[])
{
    if ((argc == 1) || (argc == 2)) {
        int radix = 0;
        if (argc == 2) {
            if (FIXNUMP(argv[1])) {
                radix = FIXNUM(argv[1]);
                switch (radix) {
                case 2: case 8: case 10: case 16: break;
                default:
                    invalid_argument_violation(vm, "string->number", "radix must be either 2, 8, 10 or 16, but got", argv[1], 1, argc, argv);
                    return scm_undef;
                }
            } else {
                if (exact_non_negative_integer_pred(argv[1])) {
                    invalid_argument_violation(vm, "string->number", "radix must be either 2, 8, 10 or 16, but got", argv[1], 1, argc, argv);
                    return scm_undef;
                }
                wrong_type_argument_violation(vm, "string->number", 1, "exact non-negative integer", argv[1], argc, argv);
                return scm_undef;
            }
        }
        if (STRINGP(argv[0])) {
            scm_string_t string = (scm_string_t)argv[0];
            return parse_number(vm->m_heap, string->name, 0, radix);
        }
        wrong_type_argument_violation(vm, "string->number", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "string->number", 1, 2, argc, argv);
    return scm_undef;
}

///


void init_subr_base_arith(object_heap_t* heap)
{
    #define DEFSUBR(SYM, FUNC)  heap->intern_system_subr(SYM, FUNC)

    DEFSUBR("number?", subr_number_pred);
    DEFSUBR("complex?", subr_complex_pred);
    DEFSUBR("real?", subr_real_pred);
    DEFSUBR("rational?", subr_rational_pred);
    DEFSUBR("integer?", subr_integer_pred);
    DEFSUBR("real-valued?", subr_real_valued_pred);
    DEFSUBR("rational-valued?", subr_rational_valued_pred);
    DEFSUBR("integer-valued?", subr_integer_valued_pred);
    DEFSUBR("exact?", subr_exact_pred);
    DEFSUBR("inexact?", subr_inexact_pred);

    DEFSUBR("inexact", subr_inexact);
    DEFSUBR("exact", subr_exact);

    DEFSUBR("=", subr_num_eq);
    DEFSUBR("<", subr_num_lt);
    DEFSUBR(">", subr_num_gt);
    DEFSUBR("<=", subr_num_le);
    DEFSUBR(">=", subr_num_ge);
    DEFSUBR("zero?", subr_zero_pred);
    DEFSUBR("positive?", subr_positive_pred);
    DEFSUBR("negative?", subr_negative_pred);
    DEFSUBR("odd?", subr_odd_pred);
    DEFSUBR("even?", subr_even_pred);
    DEFSUBR("finite?", subr_finite_pred);
    DEFSUBR("infinite?", subr_infinite_pred);
    DEFSUBR("nan?", subr_nan_pred);
    DEFSUBR("+", subr_num_add);
    DEFSUBR("-", subr_num_sub);
    DEFSUBR("*", subr_num_mul);
    DEFSUBR("/", subr_num_div);
    DEFSUBR("div", subr_int_div);
    DEFSUBR("div0", subr_int_div0);
    DEFSUBR("abs", subr_abs);
    DEFSUBR("numerator", subr_numerator);
    DEFSUBR("denominator", subr_denominator);
    DEFSUBR("floor", subr_floor);
    DEFSUBR("ceiling", subr_ceiling);
    DEFSUBR("truncate", subr_truncate);
    DEFSUBR("round", subr_round);
    DEFSUBR("exp", subr_exp);
    DEFSUBR("log", subr_log);
    DEFSUBR("sin", subr_sin);
    DEFSUBR("cos", subr_cos);
    DEFSUBR("tan", subr_tan);
    DEFSUBR("asin", subr_asin);
    DEFSUBR("acos", subr_acos);
    DEFSUBR("atan", subr_atan);
    DEFSUBR("sqrt", subr_sqrt);
    DEFSUBR("exact-integer-sqrt", subr_exact_integer_sqrt);
    DEFSUBR("expt", subr_expt);
    DEFSUBR("make-rectangular", subr_make_rectangular);
    DEFSUBR("make-polar", subr_make_polar);
    DEFSUBR("real-part", subr_real_part);
    DEFSUBR("imag-part", subr_imag_part);
    DEFSUBR("magnitude", subr_magnitude);
    DEFSUBR("angle", subr_angle);
    DEFSUBR("number->string", subr_number_to_string);
    DEFSUBR("string->number", subr_string_to_number);
}
