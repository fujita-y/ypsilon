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

// flonum?
scm_obj_t
subr_flonum_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) return FLONUMP(argv[0]) ? scm_true : scm_false;
    wrong_number_of_arguments_violation(vm, "flonum?", 1, 1, argc, argv);
    return scm_undef;
}

// real->flonum
scm_obj_t
subr_real_to_flonum(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (real_pred(argv[0])) {
            scm_flonum_t flonum = (scm_flonum_t)cnvt_to_inexact(vm->m_heap, argv[0]);
            assert(FLONUMP(flonum));
            return flonum;
        }
        wrong_type_argument_violation(vm, "real->flonum", 0, "real", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "real->flonum", 1, 1, argc, argv);
    return scm_undef;
}

// fl=?
scm_obj_t
subr_fl_eq(VM* vm, int argc, scm_obj_t argv[])
{
    int bad;
    if (argc == 2) {
        if (BOTHFLONUMP(argv[0], argv[1])) return (FLONUM(argv[0]) == FLONUM(argv[1])) ? scm_true : scm_false;
        bad = FLONUMP(argv[0]) ? 1 : 0; goto raise_bad;
    }
    if (argc == 1) {
        if (FLONUMP(argv[0])) return scm_true;
        bad = 0; goto raise_bad;
    }
    if (argc >= 3) {
        for (int i = 0; i < argc; i++) {
            if (FLONUMP(argv[i])) continue;
            bad = i; goto raise_bad;
        }
        for (int i = 0; i < argc - 1; i++) {
            if (FLONUM(argv[i]) == FLONUM(argv[i + 1])) continue;
            return scm_false;
        }
        return scm_true;
    }
    wrong_number_of_arguments_violation(vm, "fl=?", 1, -1, argc, argv);
    return scm_undef;

raise_bad:
    wrong_type_argument_violation(vm, "fl=?", bad, "flonum", argv[bad], argc, argv);
    return scm_undef;
}

// fl<?
scm_obj_t
subr_fl_lt(VM* vm, int argc, scm_obj_t argv[])
{
    int bad;
    if (argc == 2) {
        if (BOTHFLONUMP(argv[0], argv[1])) return (FLONUM(argv[0]) < FLONUM(argv[1])) ? scm_true : scm_false;
        bad = FLONUMP(argv[0]) ? 1 : 0; goto raise_bad;
    }
    if (argc == 1) {
        if (FLONUMP(argv[0])) return scm_true;
        bad = 0; goto raise_bad;
    }
    if (argc >= 3) {
        for (int i = 0; i < argc; i++) {
            if (FLONUMP(argv[i])) continue;
            bad = i; goto raise_bad;
        }
        for (int i = 0; i < argc - 1; i++) {
            if (FLONUM(argv[i]) < FLONUM(argv[i + 1])) continue;
            return scm_false;
        }
        return scm_true;
    }
    wrong_number_of_arguments_violation(vm, "fl<?", 1, -1, argc, argv);
    return scm_undef;

raise_bad:
    wrong_type_argument_violation(vm, "fl<?", bad, "flonum", argv[bad], argc, argv);
    return scm_undef;
}

// fl>?
scm_obj_t
subr_fl_gt(VM* vm, int argc, scm_obj_t argv[])
{
    int bad;
    if (argc == 2) {
        if (BOTHFLONUMP(argv[0], argv[1])) return (FLONUM(argv[0]) > FLONUM(argv[1])) ? scm_true : scm_false;
        bad = FLONUMP(argv[0]) ? 1 : 0; goto raise_bad;
    }
    if (argc == 1) {
        if (FLONUMP(argv[0])) return scm_true;
        bad = 0; goto raise_bad;
    }
    if (argc >= 3) {
        for (int i = 0; i < argc; i++) {
            if (FLONUMP(argv[i])) continue;
            bad = i; goto raise_bad;
        }
        for (int i = 0; i < argc - 1; i++) {
            if (FLONUM(argv[i]) > FLONUM(argv[i + 1])) continue;
            return scm_false;
        }
        return scm_true;
    }
    wrong_number_of_arguments_violation(vm, "fl>?", 1, -1, argc, argv);
    return scm_undef;

raise_bad:
    wrong_type_argument_violation(vm, "fl>?", bad, "flonum", argv[bad], argc, argv);
    return scm_undef;
}

// fl<=?
scm_obj_t
subr_fl_le(VM* vm, int argc, scm_obj_t argv[])
{
    int bad;
    if (argc == 2) {
        if (BOTHFLONUMP(argv[0], argv[1])) return (FLONUM(argv[0]) <= FLONUM(argv[1])) ? scm_true : scm_false;
        bad = FLONUMP(argv[0]) ? 1 : 0; goto raise_bad;
    }
    if (argc == 1) {
        if (FLONUMP(argv[0])) return scm_true;
        bad = 0; goto raise_bad;
    }
    if (argc >= 3) {
        for (int i = 0; i < argc; i++) {
            if (FLONUMP(argv[i])) continue;
            bad = i; goto raise_bad;
        }
        for (int i = 0; i < argc - 1; i++) {
            if (FLONUM(argv[i]) <= FLONUM(argv[i + 1])) continue;
            return scm_false;
        }
        return scm_true;
    }
    wrong_number_of_arguments_violation(vm, "fl<=?", 1, -1, argc, argv);
    return scm_undef;

raise_bad:
    wrong_type_argument_violation(vm, "fl<=?", bad, "flonum", argv[bad], argc, argv);
    return scm_undef;
}

// fl>=?
scm_obj_t
subr_fl_ge(VM* vm, int argc, scm_obj_t argv[])
{
    int bad;
    if (argc == 2) {
        if (BOTHFLONUMP(argv[0], argv[1])) return (FLONUM(argv[0]) >= FLONUM(argv[1])) ? scm_true : scm_false;
        bad = FLONUMP(argv[0]) ? 1 : 0; goto raise_bad;
    }
    if (argc == 1) {
        if (FLONUMP(argv[0])) return scm_true;
        bad = 0; goto raise_bad;
    }
    if (argc >= 3) {
        for (int i = 0; i < argc; i++) {
            if (FLONUMP(argv[i])) continue;
            bad = i; goto raise_bad;
        }
        for (int i = 0; i < argc - 1; i++) {
            if (FLONUM(argv[i]) >= FLONUM(argv[i + 1])) continue;
            return scm_false;
        }
        return scm_true;
    }
    wrong_number_of_arguments_violation(vm, "fl>=?", 1, -1, argc, argv);
    return scm_undef;

raise_bad:
    wrong_type_argument_violation(vm, "fl>=?", bad, "flonum", argv[bad], argc, argv);
    return scm_undef;
}

// flinteger?
scm_obj_t
subr_fl_integer_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (FLONUMP(argv[0])) return integer_pred(argv[0]) ? scm_true : scm_false;
        wrong_type_argument_violation(vm, "flinteger?", 0, "flonum", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "flinteger?", 1, 1, argc, argv);
    return scm_undef;
}

// flzero?
scm_obj_t
subr_fl_zero_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (FLONUMP(argv[0])) return (FLONUM(argv[0]) == 0.0) ? scm_true : scm_false;
        wrong_type_argument_violation(vm, "flzero?", 0, "flonum", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "flzero?", 1, 1, argc, argv);
    return scm_undef;
}

// flpositive?
scm_obj_t
subr_fl_positive_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (FLONUMP(argv[0])) return (FLONUM(argv[0]) > 0.0) ? scm_true : scm_false;
        wrong_type_argument_violation(vm, "flpositive?", 0, "flonum", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "flpositive?", 1, 1, argc, argv);
    return scm_undef;
}

// flnegative?
scm_obj_t
subr_fl_negative_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (FLONUMP(argv[0])) return (FLONUM(argv[0]) < 0.0) ? scm_true : scm_false;
        wrong_type_argument_violation(vm, "flnegative?", 0, "flonum", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "flnegative?", 1, 1, argc, argv);
    return scm_undef;
}

// flodd?
scm_obj_t
subr_fl_odd_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (FLONUMP(argv[0]) && integer_valued_pred(argv[0])) return n_even_pred(argv[0]) ? scm_false : scm_true;
        wrong_type_argument_violation(vm, "flodd?", 0, "integer valued flonum", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "flodd?", 1, 1, argc, argv);
    return scm_undef;
}

// fleven?
scm_obj_t
subr_fl_even_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (FLONUMP(argv[0]) && integer_valued_pred(argv[0])) return n_even_pred(argv[0]) ? scm_true : scm_false;
        wrong_type_argument_violation(vm, "fleven?", 0, "integer valued flonum", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "fleven?", 1, 1, argc, argv);
    return scm_undef;
}

// flfinite?
scm_obj_t
subr_fl_finite_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (FLONUMP(argv[0])) {
            if (isnan(FLONUM(argv[0]))) return scm_false;
            if (isinf(FLONUM(argv[0]))) return scm_false;
            return scm_true;
        }
        wrong_type_argument_violation(vm, "flfinite?", 0, "flonum", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "flfinite?", 1, 1, argc, argv);
    return scm_undef;
}

// flinfinite?
scm_obj_t
subr_fl_infinite_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (FLONUMP(argv[0])) {
            return (isinf(FLONUM(argv[0]))) ? scm_true : scm_false;
        }
        wrong_type_argument_violation(vm, "flinfinite?", 0, "flonum", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "flinfinite?", 1, 1, argc, argv);
    return scm_undef;
}

// flnan?
scm_obj_t
subr_fl_nan_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (FLONUMP(argv[0])) {
            return (isnan(FLONUM(argv[0]))) ? scm_true : scm_false;
        }
        wrong_type_argument_violation(vm, "flnan?", 0, "flonum", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "flnan?", 1, 1, argc, argv);
    return scm_undef;
}

// flmax
scm_obj_t
subr_fl_max(VM* vm, int argc, scm_obj_t argv[])
{
    int bad;
    if (argc == 2) {
        if (BOTHFLONUMP(argv[0], argv[1])) return (FLONUM(argv[0]) > FLONUM(argv[1])) ? argv[0] : argv[1];
        bad = FLONUMP(argv[0]) ? 1 : 0; goto raise_bad;
    }
    if (argc == 1) {
        if (FLONUMP(argv[0])) return argv[0];
        bad = 0; goto raise_bad;
    }
    if (argc >= 3) {
        for (int i = 0; i < argc; i++) {
            if (FLONUMP(argv[i])) continue;
            bad = i; goto raise_bad;
        }
        double val = FLONUM(argv[0]);
        int n = 0;
        for (int i = 1; i < argc; i++) {
            if (val < FLONUM(argv[i])) {
                val = FLONUM(argv[i]);
                n = i;
            }
        }
        return argv[n];
    }
    wrong_number_of_arguments_violation(vm, "flmax", 1, -1, argc, argv);
    return scm_undef;

raise_bad:
    wrong_type_argument_violation(vm, "flmax", bad, "flonum", argv[bad], argc, argv);
    return scm_undef;
}

// flmin
scm_obj_t
subr_fl_min(VM* vm, int argc, scm_obj_t argv[])
{
    int bad;
    if (argc == 2) {
        if (BOTHFLONUMP(argv[0], argv[1])) return (FLONUM(argv[0]) < FLONUM(argv[1])) ? argv[0] : argv[1];
        bad = FLONUMP(argv[0]) ? 1 : 0; goto raise_bad;
    }
    if (argc == 1) {
        if (FLONUMP(argv[0])) return argv[0];
        bad = 0; goto raise_bad;
    }
    if (argc >= 3) {
        for (int i = 0; i < argc; i++) {
            if (FLONUMP(argv[i])) continue;
            bad = i; goto raise_bad;
        }
        double val = FLONUM(argv[0]);
        int n = 0;
        for (int i = 1; i < argc; i++) {
            if (val > FLONUM(argv[i])) {
                val = FLONUM(argv[i]);
                n = i;
            }
        }
        return argv[n];
    }
    wrong_number_of_arguments_violation(vm, "flmin", 1, -1, argc, argv);
    return scm_undef;

raise_bad:
    wrong_type_argument_violation(vm, "flmin", bad, "flonum", argv[bad], argc, argv);
    return scm_undef;
}

// fl+
scm_obj_t
subr_fl_add(VM* vm, int argc, scm_obj_t argv[])
{
    int bad;
    if (argc == 2) {
        if (BOTHFLONUMP(argv[0], argv[1])) return make_flonum(vm->m_heap, FLONUM(argv[0]) + FLONUM(argv[1]));
        bad = FLONUMP(argv[0]) ? 1 : 0; goto raise_bad;
    }
    if (argc == 1) {
        if (FLONUMP(argv[0])) return argv[0];
        bad = 0; goto raise_bad;
    }
    if (argc >= 3) {
        for (int i = 0; i < argc; i++) {
            if (FLONUMP(argv[i])) continue;
            bad = i; goto raise_bad;
        }
        double val = FLONUM(argv[0]);
        for (int i = 1; i < argc; i++) {
            val = val + FLONUM(argv[i]);
        }
        return make_flonum(vm->m_heap, val);
    }
    return make_flonum(vm->m_heap, 0.0);

raise_bad:
    wrong_type_argument_violation(vm, "fl+", bad, "flonum", argv[bad], argc, argv);
    return scm_undef;
}

// fl*
scm_obj_t
subr_fl_mul(VM* vm, int argc, scm_obj_t argv[])
{
    int bad;
    if (argc == 2) {
        if (BOTHFLONUMP(argv[0], argv[1])) return make_flonum(vm->m_heap, FLONUM(argv[0]) * FLONUM(argv[1]));
        bad = FLONUMP(argv[0]) ? 1 : 0; goto raise_bad;
    }
    if (argc == 1) {
        if (FLONUMP(argv[0])) return argv[0];
        bad = 0; goto raise_bad;
    }
    if (argc >= 3) {
        for (int i = 0; i < argc; i++) {
            if (FLONUMP(argv[i])) continue;
            bad = i; goto raise_bad;
        }
        double val = FLONUM(argv[0]);
        for (int i = 1; i < argc; i++) {
            val = val * FLONUM(argv[i]);
        }
        return make_flonum(vm->m_heap, val);
    }
    return make_flonum(vm->m_heap, 1.0);

raise_bad:
    wrong_type_argument_violation(vm, "fl*", bad, "flonum", argv[bad], argc, argv);
    return scm_undef;
}

// fl-
scm_obj_t
subr_fl_sub(VM* vm, int argc, scm_obj_t argv[])
{
    int bad;
    if (argc == 2) {
        if (BOTHFLONUMP(argv[0], argv[1])) return make_flonum(vm->m_heap, FLONUM(argv[0]) - FLONUM(argv[1]));
        bad = FLONUMP(argv[0]) ? 1 : 0; goto raise_bad;
    }
    if (argc == 1) {
        if (FLONUMP(argv[0])) return make_flonum(vm->m_heap, -FLONUM(argv[0]));
        bad = 0; goto raise_bad;
    }
    if (argc >= 3) {
        for (int i = 0; i < argc; i++) {
            if (FLONUMP(argv[i])) continue;
            bad = i; goto raise_bad;
        }
        double val = FLONUM(argv[0]);
        for (int i = 1; i < argc; i++) {
            val = val - FLONUM(argv[i]);
        }
        return make_flonum(vm->m_heap, val);
    }
    wrong_number_of_arguments_violation(vm, "fl-", 1, -1, argc, argv);
    return scm_undef;

raise_bad:
    wrong_type_argument_violation(vm, "fl-", bad, "flonum", argv[bad], argc, argv);
    return scm_undef;
}

// fl/
scm_obj_t
subr_fl_quotient(VM* vm, int argc, scm_obj_t argv[])
{
    int bad;
    if (argc == 2) {
        if (BOTHFLONUMP(argv[0], argv[1])) return make_flonum(vm->m_heap, FLONUM(argv[0]) / FLONUM(argv[1]));
        bad = FLONUMP(argv[0]) ? 1 : 0; goto raise_bad;
    }
    if (argc == 1) {
        if (FLONUMP(argv[0])) return make_flonum(vm->m_heap, 1.0 / FLONUM(argv[0]));
        bad = 0; goto raise_bad;
    }
    if (argc >= 3) {
        for (int i = 0; i < argc; i++) {
            if (FLONUMP(argv[i])) continue;
            bad = i; goto raise_bad;
        }
        double val = FLONUM(argv[0]);
        for (int i = 1; i < argc; i++) {
            val = val / FLONUM(argv[i]);
        }
        return make_flonum(vm->m_heap, val);
    }
    wrong_number_of_arguments_violation(vm, "fl/", 1, -1, argc, argv);
    return scm_undef;

raise_bad:
    wrong_type_argument_violation(vm, "fl/", bad, "flonum", argv[bad], argc, argv);
    return scm_undef;
}

// flabs
scm_obj_t
subr_fl_abs(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (FLONUMP(argv[0])) return make_flonum(vm->m_heap, fabs(FLONUM(argv[0])));
        wrong_type_argument_violation(vm, "flabs", 0, "flonum", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "flabs", 1, 1, argc, argv);
    return scm_undef;
}

// fldiv
scm_obj_t
subr_fl_div(VM* vm, int argc, scm_obj_t argv[])
{
    int bad;
    if (argc == 2) {
        if (BOTHFLONUMP(argv[0], argv[1])) return arith_integer_div(vm->m_heap, argv[0], argv[1]);
        bad = FLONUMP(argv[0]) ? 1 : 0; goto raise_bad;
    }
    wrong_number_of_arguments_violation(vm, "fldiv", 2, 2, argc, argv);
    return scm_undef;

raise_bad:
    wrong_type_argument_violation(vm, "fldiv", bad, "flonum", argv[bad], argc, argv);
    return scm_undef;
}

// fldiv0
scm_obj_t
subr_fl_div0(VM* vm, int argc, scm_obj_t argv[])
{
    int bad;
    if (argc == 2) {
        if (BOTHFLONUMP(argv[0], argv[1])) return arith_integer_div0(vm->m_heap, argv[0], argv[1]);
        bad = FLONUMP(argv[0]) ? 1 : 0; goto raise_bad;
    }
    wrong_number_of_arguments_violation(vm, "fldiv0", 2, 2, argc, argv);
    return scm_undef;

raise_bad:
    wrong_type_argument_violation(vm, "fldiv0", bad, "flonum", argv[bad], argc, argv);
    return scm_undef;
}

// flnumerator
scm_obj_t
subr_fl_numerator(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (FLONUMP(argv[0])) {
            if (FLONUM(argv[0]) == 0.0) return argv[0];
            scm_obj_t obj = cnvt_to_exact(vm->m_heap, argv[0]);
            if (RATIONALP(obj)) return cnvt_to_inexact(vm->m_heap, ((scm_rational_t)obj)->nume);
            return cnvt_to_inexact(vm->m_heap, obj);
        }
        wrong_type_argument_violation(vm, "flnumerator", 0, "flonum", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "flnumerator", 1, 1, argc, argv);
    return scm_undef;
}

// fldenominator
scm_obj_t
subr_fl_denominator(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (FLONUMP(argv[0])) {
            scm_obj_t obj = cnvt_to_exact(vm->m_heap, argv[0]);
            if (RATIONALP(obj)) return cnvt_to_inexact(vm->m_heap, ((scm_rational_t)obj)->deno);
            return make_flonum(vm->m_heap, 1.0);
        }
        wrong_type_argument_violation(vm, "fldenominator", 0, "flonum", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "fldenominator", 1, 1, argc, argv);
    return scm_undef;
}

// flfloor
scm_obj_t
subr_fl_floor(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (FLONUMP(argv[0])) return make_flonum(vm->m_heap, floor(FLONUM(argv[0])));
        wrong_type_argument_violation(vm, "flfloor", 0, "flonum", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "flfloor", 1, 1, argc, argv);
    return scm_undef;
}

// flceiling
scm_obj_t
subr_fl_ceiling(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (FLONUMP(argv[0])) return make_flonum(vm->m_heap, ceil(FLONUM(argv[0])));
        wrong_type_argument_violation(vm, "flceiling", 0, "flonum", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "flceiling", 1, 1, argc, argv);
    return scm_undef;
}

// fltruncate
scm_obj_t
subr_fl_truncate(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (FLONUMP(argv[0])) return make_flonum(vm->m_heap, trunc(FLONUM(argv[0])));
        wrong_type_argument_violation(vm, "fltruncate", 0, "flonum", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "fltruncate", 1, 1, argc, argv);
    return scm_undef;
}

// flround
scm_obj_t
subr_fl_round(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (FLONUMP(argv[0])) {
            double value = FLONUM(argv[0]);
            double ans = floor(value + 0.5);
            if (ans != value + 0.5) return make_flonum(vm->m_heap, ans);
            if (ans * 0.5 == floor(ans * 0.5)) return make_flonum(vm->m_heap, ans);
            return make_flonum(vm->m_heap, ans - 1.0);
        }
        wrong_type_argument_violation(vm, "flround", 0, "flonum", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "flround", 1, 1, argc, argv);
    return scm_undef;
}

// flexp
scm_obj_t
subr_fl_exp(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (FLONUMP(argv[0])) return make_flonum(vm->m_heap, exp(FLONUM(argv[0])));
        wrong_type_argument_violation(vm, "flexp", 0, "flonum", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "flexp", 1, 1, argc, argv);
    return scm_undef;
}

// flexpt
scm_obj_t
subr_fl_expt(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (FLONUMP(argv[0])) {
            if (FLONUMP(argv[1])) {
                double fl1 = FLONUM(argv[0]);
                double fl2 = FLONUM(argv[1]);
                return make_flonum(vm->m_heap, pow(fl1, fl2));
            }
            wrong_type_argument_violation(vm, "flexpt", 1, "flonum", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "flexpt", 0, "flonum", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "flexpt", 2, 2, argc, argv);
    return scm_undef;
}

// fllog
scm_obj_t
subr_fl_log(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (FLONUMP(argv[0])) return make_flonum(vm->m_heap, log(FLONUM(argv[0])));
        wrong_type_argument_violation(vm, "fllog", 0, "flonum", argv[0], argc, argv);
        return scm_undef;
    }
    if (argc == 2) {
        if (FLONUMP(argv[0]) & FLONUMP(argv[1])) return make_flonum(vm->m_heap, log(FLONUM(argv[0])) / log(FLONUM(argv[1])));
        if (FLONUMP(argv[0])) {
            wrong_type_argument_violation(vm, "fllog", 1, "flonum", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "fllog", 0, "flonum", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "fllog", 1, 2, argc, argv);
    return scm_undef;
}

// flsin
scm_obj_t
subr_fl_sin(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (FLONUMP(argv[0])) return make_flonum(vm->m_heap, sin(FLONUM(argv[0])));
        wrong_type_argument_violation(vm, "flsin", 0, "flonum", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "flsin", 1, 1, argc, argv);
    return scm_undef;
}

// flcos
scm_obj_t
subr_fl_cos(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (FLONUMP(argv[0])) return make_flonum(vm->m_heap, cos(FLONUM(argv[0])));
        wrong_type_argument_violation(vm, "flcos", 0, "flonum", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "flcos", 1, 1, argc, argv);
    return scm_undef;
}

// fltan
scm_obj_t
subr_fl_tan(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (FLONUMP(argv[0])) return make_flonum(vm->m_heap, tan(FLONUM(argv[0])));
        wrong_type_argument_violation(vm, "fltan", 0, "flonum", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "fltan", 1, 1, argc, argv);
    return scm_undef;
}

// flasin
scm_obj_t
subr_fl_asin(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (FLONUMP(argv[0])) return make_flonum(vm->m_heap, asin(FLONUM(argv[0])));
        wrong_type_argument_violation(vm, "flasin", 0, "flonum", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "flasin", 1, 1, argc, argv);
    return scm_undef;
}

// flacos
scm_obj_t
subr_fl_acos(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (FLONUMP(argv[0])) return make_flonum(vm->m_heap, acos(FLONUM(argv[0])));
        wrong_type_argument_violation(vm, "flacos", 0, "flonum", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "flacos", 1, 1, argc, argv);
    return scm_undef;
}

// flatan
scm_obj_t
subr_fl_atan(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (FLONUMP(argv[0])) return make_flonum(vm->m_heap, atan(FLONUM(argv[0])));
        wrong_type_argument_violation(vm, "flatan", 0, "flonum", argv[0], argc, argv);
        return scm_undef;
    }
    if (argc == 2) {
        if (FLONUMP(argv[0]) & FLONUMP(argv[1])) return make_flonum(vm->m_heap, atan2(FLONUM(argv[0]), FLONUM(argv[1])));
        if (FLONUMP(argv[0])) {
            wrong_type_argument_violation(vm, "flatan", 1, "flonum", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "flatan", 0, "flonum", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "flatan", 1, 1, argc, argv);
    return scm_undef;
}

// fixnum->flonum
scm_obj_t
subr_fixnum_to_flonum(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (FIXNUMP(argv[0])) return make_flonum(vm->m_heap, FIXNUM(argv[0]));
        wrong_type_argument_violation(vm, "fixnum->flonum", 0, "fixnum", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "fixnum->flonum", 1, 1, argc, argv);
    return scm_undef;
}

// flsqrt
scm_obj_t
subr_fl_sqrt(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (FLONUMP(argv[0])) return make_flonum(vm->m_heap, sqrt(FLONUM(argv[0])));
        wrong_type_argument_violation(vm, "flsqrt", 0, "flonum", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "flsqrt", 1, 1, argc, argv);
    return scm_undef;
}

void init_subr_flonum(object_heap_t* heap)
{
    #define DEFSUBR(SYM, FUNC)  heap->intern_system_subr(SYM, FUNC)

    DEFSUBR("flonum?", subr_flonum_pred);
    DEFSUBR("real->flonum", subr_real_to_flonum);
    DEFSUBR("fl=?", subr_fl_eq);
    DEFSUBR("fl<?", subr_fl_lt);
    DEFSUBR("fl>?", subr_fl_gt);
    DEFSUBR("fl<=?", subr_fl_le);
    DEFSUBR("fl>=?", subr_fl_ge);
    DEFSUBR("flinteger?", subr_fl_integer_pred);
    DEFSUBR("flzero?", subr_fl_zero_pred);
    DEFSUBR("flpositive?", subr_fl_positive_pred);
    DEFSUBR("flnegative?", subr_fl_negative_pred);
    DEFSUBR("flodd?", subr_fl_odd_pred);
    DEFSUBR("fleven?", subr_fl_even_pred);
    DEFSUBR("flfinite?", subr_fl_finite_pred);
    DEFSUBR("flinfinite?", subr_fl_infinite_pred);
    DEFSUBR("flnan?", subr_fl_nan_pred);
    DEFSUBR("flmax", subr_fl_max);
    DEFSUBR("flmin", subr_fl_min);
    DEFSUBR("fl+", subr_fl_add);
    DEFSUBR("fl*", subr_fl_mul);
    DEFSUBR("fl-", subr_fl_sub);
    DEFSUBR("fl/", subr_fl_quotient);
    DEFSUBR("fldiv", subr_fl_div);
    DEFSUBR("fldiv0", subr_fl_div0);
    DEFSUBR("flnumerator", subr_fl_numerator);
    DEFSUBR("fldenominator", subr_fl_denominator);
    DEFSUBR("flfloor", subr_fl_floor);
    DEFSUBR("flceiling", subr_fl_ceiling);
    DEFSUBR("fltruncate", subr_fl_truncate);
    DEFSUBR("flround", subr_fl_round);
    DEFSUBR("flexp", subr_fl_exp);
    DEFSUBR("flexpt", subr_fl_expt);
    DEFSUBR("fllog", subr_fl_log);
    DEFSUBR("flsin", subr_fl_sin);
    DEFSUBR("flcos", subr_fl_cos);
    DEFSUBR("fltan", subr_fl_tan);
    DEFSUBR("flasin", subr_fl_asin);
    DEFSUBR("flacos", subr_fl_acos);
    DEFSUBR("flatan", subr_fl_atan);
    DEFSUBR("flabs", subr_fl_abs);
    DEFSUBR("flsqrt", subr_fl_sqrt);
    DEFSUBR("fixnum->flonum", subr_fixnum_to_flonum);
}
