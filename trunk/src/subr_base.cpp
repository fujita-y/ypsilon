/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#include "core.h"
#include "vm.h"
#include "fasl.h"
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

// 9.6 Equivalence predicates

// eq?
scm_obj_t
subr_eq_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (argv[0] == argv[1]) return scm_true;
        return scm_false;
    }
    wrong_number_of_arguments_violation(vm, "eq?", 2, 2, argc, argv);
    return scm_undef;
}

// eqv?
scm_obj_t
subr_eqv_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        return eqv_pred(argv[0], argv[1]) ? scm_true : scm_false;
    }
    wrong_number_of_arguments_violation(vm, "eqv?", 2, 2, argc, argv);
    return scm_undef;
}

// equal?
scm_obj_t
subr_equal_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        scm_hashtable_t visited = make_hashtable(vm->m_heap, SCM_HASHTABLE_TYPE_EQ, lookup_mutable_hashtable_size(0));
        scoped_lock lock(visited->lock);
        return equal_pred(vm->m_heap, visited, argv[0], argv[1]) ? scm_true : scm_false;
    }
    wrong_number_of_arguments_violation(vm, "equal?", 2, 2, argc, argv);
    return scm_undef;
}

// 9.7 Procedure predicate

// procedure?
scm_obj_t
subr_procedure_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        scm_obj_t obj = argv[0];
        if (SUBRP(obj) || CLOSUREP(obj) || CONTP(obj)) return scm_true;
        if (obj == scm_proc_apply) return scm_true;
        if (obj == scm_proc_callcc) return scm_true;
        if (obj == scm_proc_apply_values) return scm_true;
        return scm_false;
    }
    wrong_number_of_arguments_violation(vm, "procedure?", 1, 1, argc, argv);
    return scm_undef;
}

// 9.8 Unspecified value

// unspecified
scm_obj_t
subr_unspecified(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) return scm_unspecified;
    wrong_number_of_arguments_violation(vm, "unspecified", 0, 0, argc, argv);
    return scm_undef;
}

// unspecified?
scm_obj_t
subr_unspecified_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) return (argv[0] == scm_unspecified) ? scm_true : scm_false;
    wrong_number_of_arguments_violation(vm, "unspecified?", 1, 1, argc, argv);
    return scm_undef;
}

// 9.9 Generic arithmetic -> subr_base_arith.cpp

// 9.10 Booleans

// not
scm_obj_t
subr_not(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) return (argv[0] == scm_false) ? scm_true : scm_false;
    wrong_number_of_arguments_violation(vm, "not", 1, 1, argc, argv);
    return scm_undef;
}

// boolean?
scm_obj_t
subr_boolean_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) return BOOLP(argv[0]) ? scm_true : scm_false;
    wrong_number_of_arguments_violation(vm, "boolean?", 1, 1, argc, argv);
    return scm_undef;
}

// boolean=?
scm_obj_t
subr_boolean_eq_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (BOOLP(argv[0])) {
            if (BOOLP(argv[1])) {
                return (argv[0] == argv[1]) ? scm_true : scm_false;
            }
            wrong_type_argument_violation(vm, "boolean=?", 1, "boolean", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "boolean=?", 0, "boolean", argv[0], argc, argv);
        return scm_undef;
    }
    if (argc >= 3) {
        for (int i = 0; i < argc; i++) {
            if (BOOLP(argv[i])) continue;
            wrong_type_argument_violation(vm, "boolean=?", i, "boolean", argv[i], argc, argv);
            return scm_undef;
        }
        for (int i = 0; i < argc - 1; i++) {
            if (argv[i] == argv[i + 1]) continue;
            return scm_false;
        }
        return scm_true;
    }
    wrong_number_of_arguments_violation(vm, "boolean=?", 2, -1, argc, argv);
    return scm_undef;
}

// 9.12 Pairs and lists

// pair?
scm_obj_t
subr_pair_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) return PAIRP(argv[0]) ? scm_true : scm_false;
    wrong_number_of_arguments_violation(vm, "pair?", 1, 1, argc, argv);
    return scm_undef;
}

// cons
scm_obj_t
subr_cons(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) return make_pair(vm->m_heap, argv[0], argv[1]);
    wrong_number_of_arguments_violation(vm, "cons", 2, 2, argc, argv);
    return scm_undef;
}

// car
scm_obj_t
subr_car(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (PAIRP(argv[0])) return CAR(argv[0]);
        wrong_type_argument_violation(vm, "car", 0, "pair", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "car", 1, 1, argc, argv);
    return scm_undef;
}

// cdr
scm_obj_t
subr_cdr(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (PAIRP(argv[0])) return CDR(argv[0]);
        wrong_type_argument_violation(vm, "cdr", 0, "pair", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "cdr", 1, 1, argc, argv);
    return scm_undef;
}

// caar
scm_obj_t
subr_caar(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (PAIRP(argv[0]) && PAIRP(CAR(argv[0]))) return CAAR(argv[0]);
        wrong_type_argument_violation(vm, "caar", 0, "appropriate list structure", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "caar", 1, 1, argc, argv);
    return scm_undef;
}

// cdar
scm_obj_t
subr_cdar(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (PAIRP(argv[0]) && PAIRP(CAR(argv[0]))) return CDAR(argv[0]);
        wrong_type_argument_violation(vm, "cdar", 0, "appropriate list structure", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "cdar", 1, 1, argc, argv);
    return scm_undef;
}

// cadr
scm_obj_t
subr_cadr(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (PAIRP(argv[0]) && PAIRP(CDR(argv[0]))) return CADR(argv[0]);
        wrong_type_argument_violation(vm, "cadr", 0, "appropriate list structure", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "cadr", 1, 1, argc, argv);
    return scm_undef;
}

// cddr
scm_obj_t
subr_cddr(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (PAIRP(argv[0]) && PAIRP(CDR(argv[0]))) return CDDR(argv[0]);
        wrong_type_argument_violation(vm, "cddr", 0, "appropriate list structure", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "cddr", 1, 1, argc, argv);
    return scm_undef;
}

// caddr
scm_obj_t
subr_caddr(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (PAIRP(argv[0]) && PAIRP(CDR(argv[0])) && PAIRP(CDDR(argv[0]))) return CADDR(argv[0]);
        wrong_type_argument_violation(vm, "caddr", 0, "appropriate list structure", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "caddr", 1, 1, argc, argv);
    return scm_undef;
}

// null?
scm_obj_t
subr_null_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) return (argv[0] == scm_nil) ? scm_true : scm_false;
    wrong_number_of_arguments_violation(vm, "null?", 1, 1, argc, argv);
    return scm_undef;
}

// list?
scm_obj_t
subr_list_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) return listp(argv[0]) ? scm_true : scm_false;
    wrong_number_of_arguments_violation(vm, "list?", 1, 1, argc, argv);
    return scm_undef;
}

// list
scm_obj_t
subr_list(VM* vm, int argc, scm_obj_t argv[])
{
    scm_obj_t obj = scm_nil;
    for (int i = argc - 1; i >= 0; i--) obj = make_pair(vm->m_heap, argv[i], obj);
    return obj;
}

// length
scm_obj_t
subr_length(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (listp(argv[0])) return MAKEFIXNUM(list_length(argv[0]));
        wrong_type_argument_violation(vm, "length", 0, "proper list", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "length", 1, 1, argc, argv);
    return scm_undef;
}

// append

static scm_obj_t
append2(object_heap_t* heap, scm_obj_t lst1, scm_obj_t lst2)
{
    if (lst1 == scm_nil) return lst2;
    return make_pair(heap, CAR(lst1), append2(heap, CDR(lst1), lst2));
}

scm_obj_t
subr_append(VM* vm, int argc, scm_obj_t argv[])
{
    for (int i = 0; i < argc - 1; i++) {
        if (listp(argv[i])) continue;
        wrong_type_argument_violation(vm, "append", i, "proper list", argv[i], argc, argv);
        return scm_undef;
    }
    if (argc == 0) return scm_nil;
    if (argc == 1) return argv[0];
    scm_obj_t obj = scm_nil;
    for (int i = argc - 1; i >= 0; i--) {
        if (obj == scm_nil) obj = argv[i];
        else obj = append2(vm->m_heap, argv[i], obj);
    }
    return obj;
}

// reverse
scm_obj_t
subr_reverse(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (listp(argv[0])) {
            scm_obj_t lst = argv[0];
            scm_obj_t obj = scm_nil;
            while (lst != scm_nil) {
                obj = make_pair(vm->m_heap, CAR(lst), obj);
                lst = CDR(lst);
            }
            return obj;
        }
        wrong_type_argument_violation(vm, "reverse", 0, "proper list", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "reverse", 1, 1, argc, argv);
    return scm_undef;
}

// list-tail
scm_obj_t
subr_list_tail(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (PAIRP(argv[0])) {
            if (FIXNUMP(argv[1])) {
                scm_obj_t obj = list_tail(argv[0], FIXNUM(argv[1]));
                if (obj != NULL) return obj;
            }
            if (exact_non_negative_integer_pred(argv[1])) {
                invalid_argument_violation(vm, "list-tail", "index out of bounds,", argv[1], 1, argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "list-tail", 1, "exact non-negative integer", argv[1], argc, argv);
            return scm_undef;
        }
        if (FIXNUMP(argv[1]) && FIXNUM(argv[1]) == 0) return argv[0];
        if (exact_non_negative_integer_pred(argv[1])) {
            invalid_argument_violation(vm, "list-tail", "index out of bounds,", argv[1], 1, argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "list-tail", 1, "exact non-negative integer", argv[1], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "list-tail", 2, 2, argc, argv);
    return scm_undef;
}

// list-ref
scm_obj_t
subr_list_ref(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (PAIRP(argv[0])) {
            if (FIXNUMP(argv[1])) {
                scm_obj_t obj = list_ref(argv[0], FIXNUM(argv[1]));
                if (obj != NULL) return obj;
            }
            if (exact_non_negative_integer_pred(argv[1])) {
                invalid_argument_violation(vm, "list-ref", "index out of bounds,", argv[1], 1, argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "list-ref", 1, "exact non-negative integer", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "list-ref", 0, "pair", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "list-ref", 2, 2, argc, argv);
    return scm_undef;
}

// map for-each -> r6rs-aux.scm

// 9.12 Symbols

// symbol?
scm_obj_t
subr_symbol_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        return SYMBOLP(argv[0]) ? scm_true : scm_false;
    }
    wrong_number_of_arguments_violation(vm, "symbol?", 1, 1, argc, argv);
    return scm_undef;
}

// symbol=?
scm_obj_t
subr_symbol_eq_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (SYMBOLP(argv[0])) {
            if (SYMBOLP(argv[1])) {
                return (argv[0] == argv[1]) ? scm_true : scm_false;
            }
            wrong_type_argument_violation(vm, "symbol=?", 1, "symbol", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "symbol=?", 0, "symbol", argv[0], argc, argv);
        return scm_undef;
    }
    if (argc >= 3) {
        for (int i = 0; i < argc; i++) {
            if (SYMBOLP(argv[i])) continue;
            wrong_type_argument_violation(vm, "symbol=?", i, "symbol", argv[i], argc, argv);
            return scm_undef;
        }
        for (int i = 0; i < argc - 1; i++) {
            if (argv[i] == argv[i + 1]) continue;
            return scm_false;
        }
        return scm_true;
    }
    wrong_number_of_arguments_violation(vm, "symbol=?", 2, -1, argc, argv);
    return scm_undef;
}
// symbol->string
scm_obj_t
subr_symbol_string(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (SYMBOLP(argv[0])) {
            scm_symbol_t symbol = (scm_symbol_t)argv[0];
            return make_string_literal(vm->m_heap, symbol->name, HDR_SYMBOL_SIZE(symbol->hdr));
        }
        wrong_type_argument_violation(vm, "symbol->string", 0, "symbol", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "symbol->string", 1, 1, argc, argv);
    return scm_undef;
}

// string->symbol
scm_obj_t
subr_string_symbol(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (STRINGP(argv[0])) {
            scm_string_t string = (scm_string_t)argv[0];
            return make_symbol(vm->m_heap, string->name, HDR_STRING_SIZE(string->hdr));
        }
        wrong_type_argument_violation(vm, "string->symbol", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "string->symbol", 1, 1, argc, argv);
    return scm_undef;
}

// 9.13 Characters

// char?
scm_obj_t
subr_char_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) return CHARP(argv[0]) ? scm_true : scm_false;
    wrong_number_of_arguments_violation(vm, "char?", 1, 1, argc, argv);
    return scm_undef;
}

// char->integer
scm_obj_t
subr_char_integer(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (CHARP(argv[0])) return MAKEFIXNUM(CHAR(argv[0]));
        wrong_type_argument_violation(vm, "char->integer", 0, "char", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "char->integer", 1, 1, argc, argv);
    return scm_undef;
}

// integer->char
scm_obj_t
subr_integer_char(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (exact_non_negative_integer_pred(argv[0])) {
            if (FIXNUMP(argv[0])) {
                uint32_t c = FIXNUM(argv[0]);
                if (c > 0x10ffff) {
                    invalid_argument_violation(vm, "integer->char", "code point out of range,", argv[0], 0, argc, argv);
                    return scm_undef;
                }
                if (c >= 0xd800 && c <= 0xdfff) {
                    invalid_argument_violation(vm, "integer->char", "code point in excluded range,", argv[0], 0, argc, argv);
                    return scm_undef;
                }
                return MAKECHAR(c);
            }
            invalid_argument_violation(vm, "integer->char", "code point out of range,", argv[0], 0, argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "integer->char", 0, "exact non-negative integer", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "integer->char", 1, 1, argc, argv);
    return scm_undef;
}

// char=?
scm_obj_t
subr_char_eq_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (CHARP(argv[0])) {
            if (CHARP(argv[1])) {
                return (CHAR(argv[0]) == CHAR(argv[1])) ? scm_true : scm_false;
            }
            wrong_type_argument_violation(vm, "char=?", 1, "char", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "char=?", 0, "char", argv[0], argc, argv);
        return scm_undef;
    }
    if (argc >= 3) {
        for (int i = 0; i < argc; i++) {
            if (CHARP(argv[i])) continue;
            wrong_type_argument_violation(vm, "char=?", i, "char", argv[i], argc, argv);
            return scm_undef;
        }
        for (int i = 0; i < argc - 1; i++) {
            if (CHAR(argv[i]) == CHAR(argv[i + 1])) continue;
            return scm_false;
        }
        return scm_true;
    }
    wrong_number_of_arguments_violation(vm, "char=?", 2, -1, argc, argv);
    return scm_undef;
}

// char<?
scm_obj_t
subr_char_lt_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (CHARP(argv[0])) {
            if (CHARP(argv[1])) {
                return (CHAR(argv[0]) < CHAR(argv[1])) ? scm_true : scm_false;
            }
            wrong_type_argument_violation(vm, "char<?", 1, "char", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "char<?", 0, "char", argv[0], argc, argv);
        return scm_undef;
    }
    if (argc >= 3) {
        for (int i = 0; i < argc; i++) {
            if (CHARP(argv[i])) continue;
            wrong_type_argument_violation(vm, "char<?", i, "char", argv[i], argc, argv);
            return scm_undef;
        }
        for (int i = 0; i < argc - 1; i++) {
            if (CHAR(argv[i]) < CHAR(argv[i + 1])) continue;
            return scm_false;
        }
        return scm_true;
    }
    wrong_number_of_arguments_violation(vm, "char<?", 2, -1, argc, argv);
    return scm_undef;
}

// char>?
scm_obj_t
subr_char_gt_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (CHARP(argv[0])) {
            if (CHARP(argv[1])) {
                return (CHAR(argv[0]) > CHAR(argv[1])) ? scm_true : scm_false;
            }
            wrong_type_argument_violation(vm, "char>?", 1, "char", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "char>?", 0, "char", argv[0], argc, argv);
        return scm_undef;
    }
    if (argc >= 3) {
        for (int i = 0; i < argc; i++) {
            if (CHARP(argv[i])) continue;
            wrong_type_argument_violation(vm, "char>?", i, "char", argv[i], argc, argv);
            return scm_undef;
        }
        for (int i = 0; i < argc - 1; i++) {
            if (CHAR(argv[i]) > CHAR(argv[i + 1])) continue;
            return scm_false;
        }
        return scm_true;
    }
    wrong_number_of_arguments_violation(vm, "char>?", 2, -1, argc, argv);
    return scm_undef;
}

// char<=?
scm_obj_t
subr_char_le_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (CHARP(argv[0])) {
            if (CHARP(argv[1])) {
                return (CHAR(argv[0]) <= CHAR(argv[1])) ? scm_true : scm_false;
            }
            wrong_type_argument_violation(vm, "char<=?", 1, "char", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "char<=?", 0, "char", argv[0], argc, argv);
        return scm_undef;
    }
    if (argc >= 3) {
        for (int i = 0; i < argc; i++) {
            if (CHARP(argv[i])) continue;
            wrong_type_argument_violation(vm, "char<=?", i, "char", argv[i], argc, argv);
            return scm_undef;
        }
        for (int i = 0; i < argc - 1; i++) {
            if (CHAR(argv[i]) <= CHAR(argv[i + 1])) continue;
            return scm_false;
        }
        return scm_true;
    }
    wrong_number_of_arguments_violation(vm, "char<=?", 2, -1, argc, argv);
    return scm_undef;
}

// char>=?
scm_obj_t
subr_char_ge_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (CHARP(argv[0])) {
            if (CHARP(argv[1])) {
                return (CHAR(argv[0]) >= CHAR(argv[1])) ? scm_true : scm_false;
            }
            wrong_type_argument_violation(vm, "char>=?", 1, "char", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "char>=?", 0, "char", argv[0], argc, argv);
        return scm_undef;
    }
    if (argc >= 3) {
        for (int i = 0; i < argc; i++) {
            if (CHARP(argv[i])) continue;
            wrong_type_argument_violation(vm, "char>=?", i, "char", argv[i], argc, argv);
            return scm_undef;
        }
        for (int i = 0; i < argc - 1; i++) {
            if (CHAR(argv[i]) >= CHAR(argv[i + 1])) continue;
            return scm_false;
        }
        return scm_true;
    }
    wrong_number_of_arguments_violation(vm, "char>=?", 2, -1, argc, argv);
    return scm_undef;
}

// 9.14 Strings

// string?
scm_obj_t
subr_string_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) return STRINGP(argv[0]) ? scm_true : scm_false;
    wrong_number_of_arguments_violation(vm, "string?", 1, 1, argc, argv);
    return scm_undef;
}

// make-string
scm_obj_t
subr_make_string(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (FIXNUMP(argv[0]) && FIXNUM(argv[0]) >= 0) {
            scm_string_t string = make_string(vm->m_heap, FIXNUM(argv[0]), 0x20);
            if (HDR_STRING_SIZE(string->hdr) == FIXNUM(argv[0])) return string;
        }
        if (exact_non_negative_integer_pred(argv[0])) {
            invalid_argument_violation(vm, "make-string", "too many elements,", argv[0], 0, argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "make-string", 0, "exact non-negative integer", argv[0], argc, argv);
        return scm_undef;
    }
    if (argc == 2) {
        if (FIXNUMP(argv[0]) && FIXNUM(argv[0]) >= 0) {
            if (CHARP(argv[1])) {
                int c = CHAR(argv[1]);
                uint8_t utf8[4];
                int bytes = cnvt_ucs4_to_utf8(c, utf8);
                int size = FIXNUM(argv[0]) * bytes;
                scm_string_t string = make_string(vm->m_heap, size, ' ');
                if (HDR_STRING_SIZE(string->hdr) == size) {
                    for (int i = 0; i < size; i += bytes) memcpy(string->name + i, utf8, bytes);
                    return string;
                }
            } else {
                wrong_type_argument_violation(vm, "make-string", 1, "char", argv[1], argc, argv);
                return scm_undef;
            }
        }
        if (exact_non_negative_integer_pred(argv[0])) {
            invalid_argument_violation(vm, "make-string", "too many elements,", argv[0], 0, argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "make-string", 0, "exact non-negative integer", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "make-string", 1, 2, argc, argv);
    return scm_undef;
}

// string
scm_obj_t
subr_string(VM* vm, int argc, scm_obj_t argv[])
{
    int size = 0;
    for (int i = 0; i < argc; i++) {
        if (CHARP(argv[i])) {
            size = size + utf8_sizeof_ucs4(CHAR(argv[i]));
            continue;
        }
        wrong_type_argument_violation(vm, "string", i, "char", argv[i], argc, argv);
        return scm_undef;
    }
    scm_string_t string = make_string(vm->m_heap, size, ' ');
    int offset = 0;
    for (int i = 0; i < argc; i++) {
        uint8_t utf8[4];
        int n = cnvt_ucs4_to_utf8(CHAR(argv[i]), utf8);
        memcpy(string->name + offset, utf8, n);
        offset = offset + n;
    }
    return string;
}

// string-length
scm_obj_t
subr_string_length(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (STRINGP(argv[0])) {
            scm_string_t string = (scm_string_t)argv[0];
            return MAKEFIXNUM(utf8_string_length(string));
        }
        wrong_type_argument_violation(vm, "string-length", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "string-length", 1, 1, argc, argv);
    return scm_undef;
}

// string-ref
scm_obj_t
subr_string_ref(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (STRINGP(argv[0])) {
            if (FIXNUMP(argv[1])) {
                scm_string_t string = (scm_string_t)argv[0];
                int index = FIXNUM(argv[1]);
                if (index >= 0 && index < HDR_STRING_SIZE(string->hdr)) {
                    int c = utf8_string_ref(string, index);
                    if (c >= 0) return MAKECHAR(c);
                    invalid_object_violation(vm, "string-ref", "properly encoded string", string, argc, argv);
                    return scm_undef;
                }
                /*** FALL THROUGH ***/
            }
            if (exact_non_negative_integer_pred(argv[1])) {
                invalid_argument_violation(vm, "string-ref", "index out of bounds,", argv[1], 1, argc, argv);
                return scm_undef;
            } else {
                wrong_type_argument_violation(vm, "string-ref", 1, "exact non-negative integer", argv[1], argc, argv);
                return scm_undef;
            }
        }
        wrong_type_argument_violation(vm, "string-ref", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "string-ref", 2, 2, argc, argv);
    return scm_undef;
}

// string-set!
scm_obj_t
subr_string_set(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 3) {
        if (STRINGP(argv[0])) {
            if (CHARP(argv[2])) {
                if (FIXNUMP(argv[1])) {
                    scm_string_t string = (scm_string_t)argv[0];
                    int index = FIXNUM(argv[1]);
                    if (index >= 0 && index < HDR_STRING_SIZE(string->hdr)) {
                        if (HDR_STRING_LITERAL(string->hdr) == 0) {
                            if (utf8_string_set(vm->m_heap, string, index, CHAR(argv[2]))) return string;
                            if (index < utf8_string_length(string)) {
                                invalid_argument_violation(vm, "string-set!", "too many elements in string", NULL, 0, 0, NULL);
                                return scm_undef;
                            }
                            invalid_argument_violation(vm, "string-set!", "index out of bounds,", argv[1], 1, argc, argv);
                            return scm_undef;
                        }
                        invalid_argument_violation(vm, "string-set!", "immutable string,", argv[0], 0, argc, argv);
                        return scm_undef;
                    }
                    /*** FALL THROUGH ***/
                }
                if (exact_non_negative_integer_pred(argv[1])) {
                    invalid_argument_violation(vm, "string-set!", "index out of bounds,", argv[1], 1, argc, argv);
                    return scm_undef;
                } else {
                    wrong_type_argument_violation(vm, "string-set!", 1, "exact non-negative integer", argv[1], argc, argv);
                    return scm_undef;
                }
            }
            wrong_type_argument_violation(vm, "string-set!", 2, "char", argv[2], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "string-set!", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "string-set!", 3, 3, argc, argv);
    return scm_undef;
}

// string=?
scm_obj_t
subr_string_eq_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (STRINGP(argv[0])) {
            if (STRINGP(argv[1])) {
                return string_eq_pred(argv[0], argv[1]) ? scm_true : scm_false;
            }
            wrong_type_argument_violation(vm, "string=?", 1, "string", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "string=?", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    if (argc >= 3) {
        for (int i = 0; i < argc; i++) {
            if (STRINGP(argv[i])) continue;
            wrong_type_argument_violation(vm, "string=?", i, "string", argv[i], argc, argv);
            return scm_undef;
        }
        for (int i = 0; i < argc - 1; i++) {
            if (string_eq_pred(argv[i], argv[i + 1])) continue;
            return scm_false;
        }
        return scm_true;
    }
    wrong_number_of_arguments_violation(vm, "string=?", 2, -1, argc, argv);
    return scm_undef;
}

// string<?
scm_obj_t
subr_string_lt_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (STRINGP(argv[0])) {
            if (STRINGP(argv[1])) {
                return (string_compare(argv[0], argv[1]) < 0) ? scm_true : scm_false;
            }
            wrong_type_argument_violation(vm, "string<?", 1, "string", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "string<?", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    if (argc >= 3) {
        for (int i = 0; i < argc; i++) {
            if (STRINGP(argv[i])) continue;
            wrong_type_argument_violation(vm, "string<?", i, "string", argv[i], argc, argv);
            return scm_undef;
        }
        for (int i = 0; i < argc - 1; i++) {
            if (string_compare(argv[i], argv[i + 1]) < 0) continue;
            return scm_false;
        }
        return scm_true;
    }
    wrong_number_of_arguments_violation(vm, "string<?", 2, -1, argc, argv);
    return scm_undef;
}

// string>?
scm_obj_t
subr_string_gt_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (STRINGP(argv[0])) {
            if (STRINGP(argv[1])) {
                return (string_compare(argv[0], argv[1]) > 0) ? scm_true : scm_false;
            }
            wrong_type_argument_violation(vm, "string>?", 1, "string", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "string>?", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    if (argc >= 3) {
        for (int i = 0; i < argc; i++) {
            if (STRINGP(argv[i])) continue;
            wrong_type_argument_violation(vm, "string>?", i, "string", argv[i], argc, argv);
            return scm_undef;
        }
        for (int i = 0; i < argc - 1; i++) {
            if (string_compare(argv[i], argv[i + 1]) > 0) continue;
            return scm_false;
        }
        return scm_true;
    }
    wrong_number_of_arguments_violation(vm, "string>?", 2, -1, argc, argv);
    return scm_undef;
}

// string<=?
scm_obj_t
subr_string_le_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (STRINGP(argv[0])) {
            if (STRINGP(argv[1])) {
                return (string_compare(argv[0], argv[1]) <= 0) ? scm_true : scm_false;
            }
            wrong_type_argument_violation(vm, "string<=?", 1, "string", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "string<=?", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    if (argc >= 3) {
        for (int i = 0; i < argc; i++) {
            if (STRINGP(argv[i])) continue;
            wrong_type_argument_violation(vm, "string<=?", i, "string", argv[i], argc, argv);
            return scm_undef;
        }
        for (int i = 0; i < argc - 1; i++) {
            if (string_compare(argv[i], argv[i + 1]) <= 0) continue;
            return scm_false;
        }
        return scm_true;
    }
    wrong_number_of_arguments_violation(vm, "string<=?", 2, -1, argc, argv);
    return scm_undef;
}

// string>=?
scm_obj_t
subr_string_ge_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (STRINGP(argv[0])) {
            if (STRINGP(argv[1])) {
                return (string_compare(argv[0], argv[1]) >= 0) ? scm_true : scm_false;
            }
            wrong_type_argument_violation(vm, "string>=?", 1, "string", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "string>=?", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    if (argc >= 3) {
        for (int i = 0; i < argc; i++) {
            if (STRINGP(argv[i])) continue;
            wrong_type_argument_violation(vm, "string>=?", i, "string", argv[i], argc, argv);
            return scm_undef;
        }
        for (int i = 0; i < argc - 1; i++) {
            if (string_compare(argv[i], argv[i + 1]) >= 0) continue;
            return scm_false;
        }
        return scm_true;
    }
    wrong_number_of_arguments_violation(vm, "string>=?", 2, -1, argc, argv);
    return scm_undef;
}

// substring (r6rs-aux.scm)

// substring
scm_obj_t
subr_substring(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 3) {
        if (STRINGP(argv[0])) {
            scm_string_t string = (scm_string_t)argv[0];
            CHECK_NON_NEGATIVE_FIXNUM(1, "substring");
            int from = FIXNUM(argv[1]);
            CHECK_NON_NEGATIVE_FIXNUM(2, "substring");
            int to = FIXNUM(argv[2]);
            int len = utf8_string_length(string);
            if (from > len) {
                invalid_argument_violation(vm, "substring", "start index out of bounds,", argv[1], 1, argc, argv);
                return scm_undef;
            }
            if (to > len) {
                invalid_argument_violation(vm, "substring", "end index out of bounds,", argv[2], 2, argc, argv);
                return scm_undef;
            }
            if (to < from) {
                invalid_argument_violation(vm, "substring", "end index smaller than start index,", argv[2], 2, argc, argv);
                return scm_undef;
            }
            int head = 0;
            int tail = 0;
            utf8_substring(string, from, to, &head, &tail);
            scm_string_t obj = make_string(vm->m_heap, tail - head, 0);
            memcpy(obj->name, string->name + head, tail - head);
            return obj;
        }
        wrong_type_argument_violation(vm, "substring", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "substring", 3, 3, argc, argv);
    return scm_undef;
}

// string-append
scm_obj_t
subr_string_append(VM* vm, int argc, scm_obj_t argv[])
{
    int size = 0;
    for (int i = 0; i < argc; i++) {
        if (STRINGP(argv[i])) {
            scm_string_t src = (scm_string_t)argv[i];
            size = size + HDR_STRING_SIZE(src->hdr);
        } else {
            wrong_type_argument_violation(vm, "string-append", i, "string", argv[i], argc, argv);
            return scm_undef;
        }
    }
    scm_string_t string = make_string(vm->m_heap, size, 0);
    if (HDR_STRING_SIZE(string->hdr) == size) {
        int p = 0;
        for (int i = 0; i < argc; i++) {
            scm_string_t src = (scm_string_t)argv[i];
            int len = HDR_STRING_SIZE(src->hdr);
            memcpy(string->name + p, src->name, len);
            p += len;
        }
        return string;
    }
    invalid_argument_violation(vm, "string-append", "too many elements in string", NULL, 0, 0, NULL);
    return scm_undef;
}

// string-copy
scm_obj_t
subr_string_copy(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (STRINGP(argv[0])) {
            scm_string_t string = (scm_string_t)argv[0];
            return make_string(vm->m_heap, string->name, HDR_STRING_SIZE(string->hdr));
        }
        wrong_type_argument_violation(vm, "string-copy", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "string-copy",1, 1, argc, argv);
    return scm_undef;
}

// string-fill!
scm_obj_t
subr_string_fill(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (STRINGP(argv[0])) {
            if (CHARP(argv[1])) {
                scm_string_t string = (scm_string_t)argv[0];
                int ucs4 = CHAR(argv[1]);
                int len = utf8_string_length(string);
                int bsize = len * utf8_sizeof_ucs4(ucs4);
                if (vm->m_heap->allocated_size((uint8_t*)string->name) < bsize + 1) {
                    scm_hdr_t hdr2 = scm_hdr_string | (bsize << HDR_STRING_SIZE_SHIFT);
                    if (HDR_STRING_SIZE(hdr2) != bsize) {
                        invalid_argument_violation(vm, "string-fill!", "too many elements in string", NULL, 0, 0, NULL);
                        return scm_undef;
                    }
                    uint8_t* prev = (uint8_t*)string->name;
                    uint8_t* datum2 = (uint8_t*)vm->m_heap->allocate_private(bsize + 1);
                    datum2[bsize] = 0;
                    string->name = (char*)datum2;
                    string->hdr = hdr2;
                    vm->m_heap->deallocate_private(prev);
                } else {
                    string->hdr = scm_hdr_string | (bsize << HDR_STRING_SIZE_SHIFT);
                }
                uint8_t utf8[4];
                int n = cnvt_ucs4_to_utf8(ucs4, utf8);
                int p = 0;
                for (int i = 0; i < len; i++) {
                    for (int j = 0; j < n; j++) string->name[p + j] = utf8[j];
                    p += n;
                }
                string->name[p] = 0;
                return scm_unspecified;
            }
            wrong_type_argument_violation(vm, "string-fill!", 1, "char", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "string-fill!", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "string-fill!",2, 2, argc, argv);
    return scm_undef;
}

// string->list list->string string-copy string-fill! -> r6rs-aux.scm

// 9.15 Vectors

// vector?
scm_obj_t
subr_vector_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) return VECTORP(argv[0]) ? scm_true : scm_false;
    wrong_number_of_arguments_violation(vm, "vector?",1, 1, argc, argv);
    return scm_undef;
}

// make-vector
scm_obj_t
subr_make_vector(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (FIXNUMP(argv[0]) && FIXNUM(argv[0]) >= 0) {
            scm_vector_t vect = make_vector(vm->m_heap, FIXNUM(argv[0]), scm_unspecified);
            if (HDR_VECTOR_COUNT(vect->hdr) == FIXNUM(argv[0])) return vect;
        }
        if (exact_non_negative_integer_pred(argv[0])) {
            invalid_argument_violation(vm, "make-vector", "too many elements,", argv[0], 0, argc, argv);
            return scm_undef;
        } else {
            wrong_type_argument_violation(vm, "make-vector", 0, "exact non-negative integer", argv[0], argc, argv);
            return scm_undef;
        }
    }
    if (argc == 2) {
        if (FIXNUMP(argv[0]) && FIXNUM(argv[0]) >= 0) return make_vector(vm->m_heap, FIXNUM(argv[0]), argv[1]);
        if (exact_non_negative_integer_pred(argv[0])) {
            invalid_argument_violation(vm, "make-vector", "too many elements,", argv[0], 0, argc, argv);
            return scm_undef;
        } else {
            wrong_type_argument_violation(vm, "make-vector", 0, "exact non-negative integer", argv[0], argc, argv);
            return scm_undef;
        }
    }
    wrong_number_of_arguments_violation(vm, "make-vector",1, 2, argc, argv);
    return scm_undef;
}

// vector
scm_obj_t
subr_vector(VM* vm, int argc, scm_obj_t argv[])
{
    scm_vector_t vector = make_vector(vm->m_heap, argc, scm_unspecified);
    for (int i = 0; i < argc; i++) vector->elts[i] = argv[i];
    return vector;
}

// vector-length
scm_obj_t
subr_vector_length(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (VECTORP(argv[0])) {
            scm_vector_t vector = (scm_vector_t)argv[0];
            return MAKEFIXNUM(HDR_VECTOR_COUNT(vector->hdr));
        }
        wrong_type_argument_violation(vm, "vector-length", 0, "vector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "vector-length",1, 1, argc, argv);
    return scm_undef;
}

// vector-ref
scm_obj_t
subr_vector_ref(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (VECTORP(argv[0])) {
            scm_vector_t vector = (scm_vector_t)argv[0];
            if (FIXNUMP(argv[1])) {
                int n = FIXNUM(argv[1]);
                if (n >= 0 && n < HDR_VECTOR_COUNT(vector->hdr)) return vector->elts[n];
                /*** FALL THROUGH ***/
            }
            if (exact_non_negative_integer_pred(argv[1])) {
                invalid_argument_violation(vm, "vector-ref", "index out of bounds,", argv[1], 1, argc, argv);
                return scm_undef;
            } else {
                wrong_type_argument_violation(vm, "vector-ref", 1, "exact non-negative integer", argv[1], argc, argv);
                return scm_undef;
            }
        }
        wrong_type_argument_violation(vm, "vector-ref", 0, "vector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "vector-ref", 2, 2, argc, argv);
    return scm_undef;
}

// vector-set!
scm_obj_t
subr_vector_set(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 3) {
        if (VECTORP(argv[0])) {
            scm_vector_t vector = (scm_vector_t)argv[0];
            if (FIXNUMP(argv[1])) {
                int n = FIXNUM(argv[1]);
                if (n >= 0 && n < HDR_VECTOR_COUNT(vector->hdr)) {
                    vm->m_heap->write_barrier(argv[2]);
                    vector->elts[n] = argv[2];
                    return scm_unspecified;
                }
                /*** FALL THROUGH ***/
            }
            if (exact_non_negative_integer_pred(argv[1])) {
                invalid_argument_violation(vm, "vector-set!", "index out of bounds,", argv[1], 1, argc, argv);
                return scm_undef;
            } else {
                wrong_type_argument_violation(vm, "vector-set!", 1, "exact non-negative integer", argv[1], argc, argv);
                return scm_undef;
            }
        }
        wrong_type_argument_violation(vm, "vector-set!", 0, "vector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "vector-set!", 3, 3, argc, argv);
    return scm_undef;
}

// list->vector vector-map vector-for-each -> r6rs-aux.scm

// vector->list
scm_obj_t
subr_vector_list(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (VECTORP(argv[0])) {
            scm_vector_t vector = (scm_vector_t)argv[0];
            int n = HDR_VECTOR_COUNT(vector->hdr);
            scm_obj_t lst = scm_nil;
            for (int i = n - 1; i >= 0 ; i--) lst = make_pair(vm->m_heap, vector->elts[i], lst);
            return lst;
        }
        wrong_type_argument_violation(vm, "vector->list", 0, "vector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "vector->list", 1, 1, argc, argv);
    return scm_undef;
}

// vector->fill!
scm_obj_t
subr_vector_fill(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (VECTORP(argv[0])) {
            scm_vector_t vector = (scm_vector_t)argv[0];
            int n = HDR_VECTOR_COUNT(vector->hdr);
            vm->m_heap->write_barrier(argv[1]);
            for (int i = 0; i < n ; i++) vector->elts[i] = argv[1];
            return scm_unspecified;
        }
        wrong_type_argument_violation(vm, "vector-fill!", 0, "vector", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "vector-fill!", 2, 2, argc, argv);
    return scm_undef;
}

// 9.16 Errors ans violations

// error assertion-violation (exception.scm)

// 9.17 Control features

// apply (interned)
// call-with-current-continuation call/cc (dynamic-wind.scm)

scm_obj_t
subr_values(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) return scm_unspecified;
    if (argc == 1) return argv[0];
    scm_values_t values = make_values(vm->m_heap, argc);
    for (int i = 0; i < argc; i++) values->elts[i] = argv[i];
    return values;
}

// call-with-values (r6rs-aux.scm)
// dynamic-wind (dynamic-wind.scm)

////////////

// cxxxr cxxxxr

#if USE_INLINED_CXR

    #define DEF_CARS_N_CDRS3(NAME, MC)              \
    scm_obj_t                                       \
    subr_##NAME(VM* vm, int argc, scm_obj_t argv[]) \
    {                                               \
        static const int mc[] = MC;                 \
        if (argc == 1) {                            \
            scm_obj_t obj = argv[0];                \
            if (PAIRP(obj)) {                       \
                obj = (scm_obj_t)(*((scm_obj_t*)obj + mc[0]));          \
                if (PAIRP(obj)) {                                       \
                    obj = (scm_obj_t)(*((scm_obj_t*)obj + mc[1]));      \
                    if (PAIRP(obj)) {                                   \
                        obj = (scm_obj_t)(*((scm_obj_t*)obj + mc[2]));  \
                        return obj;                                     \
                    } \
                } \
            } \
            wrong_type_argument_violation(vm, #NAME, 0, "appropriate list structure", argv[0], argc, argv); \
            return scm_undef;                                               \
        }                                                                   \
        wrong_number_of_arguments_violation(vm, #NAME, 1, 1, argc, argv);   \
        return scm_undef;                                                   \
    }

    #define DEF_CARS_N_CDRS4(NAME, MC)              \
    scm_obj_t                                       \
    subr_##NAME(VM* vm, int argc, scm_obj_t argv[]) \
    {                                               \
        static const int mc[] = MC;                 \
        if (argc == 1) {                            \
            scm_obj_t obj = argv[0];                \
            if (PAIRP(obj)) {                       \
                obj = (scm_obj_t)(*((scm_obj_t*)obj + mc[0]));              \
                if (PAIRP(obj)) {                                           \
                    obj = (scm_obj_t)(*((scm_obj_t*)obj + mc[1]));          \
                    if (PAIRP(obj)) {                                       \
                        obj = (scm_obj_t)(*((scm_obj_t*)obj + mc[2]));      \
                        if (PAIRP(obj)) {                                   \
                            obj = (scm_obj_t)(*((scm_obj_t*)obj + mc[3]));  \
                            return obj;                                     \
                        } \
                    } \
                } \
            } \
            wrong_type_argument_violation(vm, #NAME, 0, "appropriate list structure", argv[0], argc, argv); \
            return scm_undef;                                               \
        }                                                                   \
        wrong_number_of_arguments_violation(vm, #NAME, 1, 1, argc, argv);   \
        return scm_undef;                                                   \
    }

    #define MCODE3(M1, M2, M3)      { M3, M2, M1 }
    #define MCODE4(M1, M2, M3, M4)  { M4, M3, M2, M1 }

    DEF_CARS_N_CDRS3( caaar  ,MCODE3(0, 0, 0) )
    DEF_CARS_N_CDRS3( caadr  ,MCODE3(0, 0, 1) )
    DEF_CARS_N_CDRS3( cadar  ,MCODE3(0, 1, 0) )
//  DEF_CARS_N_CDRS3( caddr  ,MCODE3(0, 1, 1) )
    DEF_CARS_N_CDRS3( cdaar  ,MCODE3(1, 0, 0) )
    DEF_CARS_N_CDRS3( cdadr  ,MCODE3(1, 0, 1) )
    DEF_CARS_N_CDRS3( cddar  ,MCODE3(1, 1, 0) )
    DEF_CARS_N_CDRS3( cdddr  ,MCODE3(1, 1, 1) )
    DEF_CARS_N_CDRS4( caaaar ,MCODE4(0, 0, 0, 0) )
    DEF_CARS_N_CDRS4( caaadr ,MCODE4(0, 0, 0, 1) )
    DEF_CARS_N_CDRS4( caadar ,MCODE4(0, 0, 1, 0) )
    DEF_CARS_N_CDRS4( caaddr ,MCODE4(0, 0, 1, 1) )
    DEF_CARS_N_CDRS4( cadaar ,MCODE4(0, 1, 0, 0) )
    DEF_CARS_N_CDRS4( cadadr ,MCODE4(0, 1, 0, 1) )
    DEF_CARS_N_CDRS4( caddar ,MCODE4(0, 1, 1, 0) )
    DEF_CARS_N_CDRS4( cadddr ,MCODE4(0, 1, 1, 1) )
    DEF_CARS_N_CDRS4( cdaaar ,MCODE4(1, 0, 0, 0) )
    DEF_CARS_N_CDRS4( cdaadr ,MCODE4(1, 0, 0, 1) )
    DEF_CARS_N_CDRS4( cdadar ,MCODE4(1, 0, 1, 0) )
    DEF_CARS_N_CDRS4( cdaddr ,MCODE4(1, 0, 1, 1) )
    DEF_CARS_N_CDRS4( cddaar ,MCODE4(1, 1, 0, 0) )
    DEF_CARS_N_CDRS4( cddadr ,MCODE4(1, 1, 0, 1) )
    DEF_CARS_N_CDRS4( cdddar ,MCODE4(1, 1, 1, 0) )
    DEF_CARS_N_CDRS4( cddddr ,MCODE4(1, 1, 1, 1) )

    #undef MCODE3
    #undef MCODE4
    #undef DEF_CARS_N_CDRS3
    #undef DEF_CARS_N_CDRS4

#else

    static
    scm_obj_t
    n_car_n_cdr(scm_obj_t obj, const int n, const int mc[])
    {
        for (int i = 0; i < n; i++) {
            if (PAIRP(obj)) {
                obj = (scm_obj_t)(*((scm_obj_t*)obj + mc[i]));
                continue;
            }
            return NULL;
        }
        return obj;
    }

    #define DEF_CARS_N_CDRS(NAME, MC)               \
    scm_obj_t                                       \
    subr_##NAME(VM* vm, int argc, scm_obj_t argv[]) \
    {                                               \
        static const int mc[] = MC;                 \
        if (argc == 1) {                            \
            scm_obj_t obj = n_car_n_cdr(argv[0], array_sizeof(mc), mc);     \
            if (obj != NULL) return obj;                                    \
            wrong_type_argument_violation(vm, #NAME, 0, "appropriate list structure", argv[0], argc, argv); \
            return scm_undef;                                               \
        }                                                                   \
        wrong_number_of_arguments_violation(vm, #NAME, 1, 1, argc, argv);   \
        return scm_undef;                                                   \
    }

    #define MCODE3(M1, M2, M3)      { M3, M2, M1 }
    #define MCODE4(M1, M2, M3, M4)  { M4, M3, M2, M1 }

    DEF_CARS_N_CDRS( caaar  ,MCODE3(0, 0, 0) )
    DEF_CARS_N_CDRS( caadr  ,MCODE3(0, 0, 1) )
    DEF_CARS_N_CDRS( cadar  ,MCODE3(0, 1, 0) )
//  DEF_CARS_N_CDRS( caddr  ,MCODE3(0, 1, 1) )
    DEF_CARS_N_CDRS( cdaar  ,MCODE3(1, 0, 0) )
    DEF_CARS_N_CDRS( cdadr  ,MCODE3(1, 0, 1) )
    DEF_CARS_N_CDRS( cddar  ,MCODE3(1, 1, 0) )
    DEF_CARS_N_CDRS( cdddr  ,MCODE3(1, 1, 1) )
    DEF_CARS_N_CDRS( caaaar ,MCODE4(0, 0, 0, 0) )
    DEF_CARS_N_CDRS( caaadr ,MCODE4(0, 0, 0, 1) )
    DEF_CARS_N_CDRS( caadar ,MCODE4(0, 0, 1, 0) )
    DEF_CARS_N_CDRS( caaddr ,MCODE4(0, 0, 1, 1) )
    DEF_CARS_N_CDRS( cadaar ,MCODE4(0, 1, 0, 0) )
    DEF_CARS_N_CDRS( cadadr ,MCODE4(0, 1, 0, 1) )
    DEF_CARS_N_CDRS( caddar ,MCODE4(0, 1, 1, 0) )
    DEF_CARS_N_CDRS( cadddr ,MCODE4(0, 1, 1, 1) )
    DEF_CARS_N_CDRS( cdaaar ,MCODE4(1, 0, 0, 0) )
    DEF_CARS_N_CDRS( cdaadr ,MCODE4(1, 0, 0, 1) )
    DEF_CARS_N_CDRS( cdadar ,MCODE4(1, 0, 1, 0) )
    DEF_CARS_N_CDRS( cdaddr ,MCODE4(1, 0, 1, 1) )
    DEF_CARS_N_CDRS( cddaar ,MCODE4(1, 1, 0, 0) )
    DEF_CARS_N_CDRS( cddadr ,MCODE4(1, 1, 0, 1) )
    DEF_CARS_N_CDRS( cdddar ,MCODE4(1, 1, 1, 0) )
    DEF_CARS_N_CDRS( cddddr ,MCODE4(1, 1, 1, 1) )

    #undef MCODE3
    #undef MCODE4
    #undef DEF_CARS_N_CDRS

#endif

////////////

void init_subr_base(object_heap_t* heap)
{
    #define DEFSUBR(SYM, FUNC)  heap->intern_system_subr(SYM, FUNC)

    DEFSUBR("eq?", subr_eq_pred);
    DEFSUBR("eqv?", subr_eqv_pred);
    DEFSUBR("equal?", subr_equal_pred);

    DEFSUBR("procedure?", subr_procedure_pred);

    DEFSUBR("unspecified", subr_unspecified);
    DEFSUBR("unspecified?", subr_unspecified_pred);

    DEFSUBR("not", subr_not);
    DEFSUBR("boolean?", subr_boolean_pred);
    DEFSUBR("boolean=?", subr_boolean_eq_pred);

    DEFSUBR("pair?", subr_pair_pred);
    DEFSUBR("cons", subr_cons);
    DEFSUBR("car", subr_car);
    DEFSUBR("cdr", subr_cdr);
    DEFSUBR("caar", subr_caar);
    DEFSUBR("cadr", subr_cadr);
    DEFSUBR("cdar", subr_cdar);
    DEFSUBR("cddr", subr_cddr);
    DEFSUBR("caaar", subr_caaar);
    DEFSUBR("caadr", subr_caadr);
    DEFSUBR("cadar", subr_cadar);
    DEFSUBR("caddr", subr_caddr);
    DEFSUBR("cdaar", subr_cdaar);
    DEFSUBR("cdadr", subr_cdadr);
    DEFSUBR("cddar", subr_cddar);
    DEFSUBR("cdddr", subr_cdddr);
    DEFSUBR("caaaar", subr_caaaar);
    DEFSUBR("caaadr", subr_caaadr);
    DEFSUBR("caadar", subr_caadar);
    DEFSUBR("caaddr", subr_caaddr);
    DEFSUBR("cadaar", subr_cadaar);
    DEFSUBR("cadadr", subr_cadadr);
    DEFSUBR("caddar", subr_caddar);
    DEFSUBR("cadddr", subr_cadddr);
    DEFSUBR("cdaaar", subr_cdaaar);
    DEFSUBR("cdaadr", subr_cdaadr);
    DEFSUBR("cdadar", subr_cdadar);
    DEFSUBR("cdaddr", subr_cdaddr);
    DEFSUBR("cddaar", subr_cddaar);
    DEFSUBR("cddadr", subr_cddadr);
    DEFSUBR("cdddar", subr_cdddar);
    DEFSUBR("cddddr", subr_cddddr);
    DEFSUBR("null?", subr_null_pred);
    DEFSUBR("list?", subr_list_pred);
    DEFSUBR("list", subr_list);
    DEFSUBR("length", subr_length);
    DEFSUBR("append", subr_append);
    DEFSUBR("reverse", subr_reverse);
    DEFSUBR("list-tail", subr_list_tail);
    DEFSUBR("list-ref", subr_list_ref);

    DEFSUBR("symbol?", subr_symbol_pred);
    DEFSUBR("symbol=?", subr_symbol_eq_pred);
    DEFSUBR("symbol->string", subr_symbol_string);
    DEFSUBR("string->symbol", subr_string_symbol);

    DEFSUBR("char?", subr_char_pred);
    DEFSUBR("char->integer", subr_char_integer);
    DEFSUBR("integer->char", subr_integer_char);
    DEFSUBR("char=?", subr_char_eq_pred);
    DEFSUBR("char<?", subr_char_lt_pred);
    DEFSUBR("char>?", subr_char_gt_pred);
    DEFSUBR("char<=?", subr_char_le_pred);
    DEFSUBR("char>=?", subr_char_ge_pred);

    DEFSUBR("string?", subr_string_pred);
    DEFSUBR("make-string", subr_make_string);
    DEFSUBR("string", subr_string);
    DEFSUBR("string-length", subr_string_length);
    DEFSUBR("string-ref", subr_string_ref);
    DEFSUBR("string-set!", subr_string_set);
    DEFSUBR("string=?", subr_string_eq_pred);
    DEFSUBR("string<?", subr_string_lt_pred);
    DEFSUBR("string>?", subr_string_gt_pred);
    DEFSUBR("string<=?", subr_string_le_pred);
    DEFSUBR("string>=?", subr_string_ge_pred);
    DEFSUBR("string-append", subr_string_append);
    DEFSUBR("string-copy", subr_string_copy);
    DEFSUBR("string-fill!", subr_string_fill);
    DEFSUBR("substring",subr_substring);

    DEFSUBR("vector?", subr_vector_pred);
    DEFSUBR("make-vector", subr_make_vector);
    DEFSUBR("vector", subr_vector);
    DEFSUBR("vector-length", subr_vector_length);
    DEFSUBR("vector-ref", subr_vector_ref);
    DEFSUBR("vector-set!", subr_vector_set);
    DEFSUBR("vector->list", subr_vector_list);
    DEFSUBR("vector-fill!", subr_vector_fill);
    DEFSUBR("values", subr_values);

}
