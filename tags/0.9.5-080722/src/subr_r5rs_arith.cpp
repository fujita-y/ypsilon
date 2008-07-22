/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#include "core.h"
#include "vm.h"
#include "heap.h"
#include "port.h"
#include "subr.h"
#include "ucs4.h"
#include "arith.h"
#include "violation.h"

// quotient
scm_obj_t
subr_quotient(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (integer_valued_pred(argv[0])) {
            if (integer_valued_pred(argv[1])) {
                if (!n_zero_pred(argv[1])) {
                    return arith_quotient(vm->m_heap, argv[0], argv[1]);
                }
                invalid_argument_violation(vm, "quotient", "undefined for 0", NULL, 0, argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "quotient", 0, "integer", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "quotient", 0, "integer", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "quotient", 2, 2, argc, argv);
    return scm_undef;
}

// remainder
scm_obj_t
subr_remainder(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (integer_valued_pred(argv[0])) {
            if (integer_valued_pred(argv[1])) {
                if (!n_zero_pred(argv[1])) {
                    return arith_remainder(vm->m_heap, argv[0], argv[1]);
                }
                invalid_argument_violation(vm, "remainder", "undefined for 0", NULL, 0, argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "remainder", 0, "integer", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "remainder", 0, "integer", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "remainder", 2, 2, argc, argv);
    return scm_undef;
}

// modulo
scm_obj_t
subr_modulo(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (integer_valued_pred(argv[0])) {
            if (integer_valued_pred(argv[1])) {
                if (!n_zero_pred(argv[1])) {
                    return arith_modulo(vm->m_heap, argv[0], argv[1]);
                }
                invalid_argument_violation(vm, "modulo", "undefined for 0", NULL, 0, argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "modulo", 0, "integer", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "modulo", 0, "integer", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "modulo", 2, 2, argc, argv);
    return scm_undef;
}

///

void init_subr_r5rs_arith(object_heap_t* heap)
{
    #define DEFSUBR(SYM, FUNC)  heap->intern_system_subr(SYM, FUNC)

    DEFSUBR("quotient", subr_quotient);
    DEFSUBR("remainder", subr_remainder);
    DEFSUBR("modulo", subr_modulo);
}
