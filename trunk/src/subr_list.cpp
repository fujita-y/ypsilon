/*
  Ypsilon Scheme System
  Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
  See license.txt for terms and conditions of use
*/

#include "core.h"
#include "vm.h"
#include "file.h"
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
#include "interpreter.h"

static scm_obj_t
do_transpose(object_heap_t* heap, int each_len, int argc, scm_obj_t argv[])
{
    scm_obj_t ans = scm_nil;
    scm_obj_t ans_tail = scm_nil;
    for (int i = 0; i < each_len; i++) {
        scm_obj_t elt = make_pair(heap, CAR(argv[0]), scm_nil);
        scm_obj_t elt_tail = elt;
        argv[0] = CDR(argv[0]);
        for (int n = 1; n < argc; n++) {
            CDR(elt_tail) = make_pair(heap, CAR(argv[n]), scm_nil);
            elt_tail = CDR(elt_tail);
            argv[n] = CDR(argv[n]);
        }
        if (ans == scm_nil) {
            ans = make_pair(heap, elt, scm_nil);
            ans_tail = ans;
        } else {
            CDR(ans_tail) = make_pair(heap, elt, scm_nil);
            ans_tail = CDR(ans_tail);
        }
    }
    return ans;
}

// list-transpose
scm_obj_t
subr_list_transpose(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc >= 1) {
        if (listp(argv[0])) {
            int each_len = list_length(argv[0]);
            for (int i = 1; i < argc; i++) {
                if (listp(argv[i])) {
                    if (list_length(argv[i]) == each_len) continue;
                    invalid_argument_violation(vm, "list-transpose", "expected all lists have same length", NULL, -1, argc, argv);
                    return scm_undef;
                }
                wrong_type_argument_violation(vm, "list-transpose", i, "proper list", argv[i], argc, argv);
                return scm_undef;
            }
            return do_transpose(vm->m_heap, each_len, argc, argv);
        }
        wrong_type_argument_violation(vm, "list-transpose", 0, "proper list", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "list-transpose", 1, -1, argc, argv);
    return scm_undef;
}

// list-transpose+
scm_obj_t
subr_list_transpose_plus(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc >= 1) {
        if (listp(argv[0])) {
            int each_len = list_length(argv[0]);
            for (int i = 1; i < argc; i++) {
                if (listp(argv[i])) {
                    if (list_length(argv[i]) == each_len) continue;
                    return scm_false;
                }
                return scm_false;
            }
            return do_transpose(vm->m_heap, each_len, argc, argv);
        }
        return scm_false;
    }
    wrong_number_of_arguments_violation(vm, "list-transpose+", 1, -1, argc, argv);
    return scm_undef;
}

// list-transpose*
scm_obj_t
subr_list_transpose_ast(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc >= 1) {
        bool finite = false;
        int each_len = INT_MAX;
        for (int i = 0; i < argc; i++) {
            if (listp(argv[i])) {
                int n = list_length(argv[i]);
                if (n < each_len) each_len = n;
                finite = true;
                continue;
            }
            if (circular_listp(argv[i])) continue;
            wrong_type_argument_violation(vm, "list-transpose*", i, "list", argv[i], argc, argv);
            return scm_undef;
        }
        if (finite) return do_transpose(vm->m_heap, each_len, argc, argv);
        invalid_argument_violation(vm, "list-transpose*", "expected at least one finite list as argument", NULL, -1, argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "list-transpose*", 1, -1, argc, argv);
    return scm_undef;
}

// cons*
scm_obj_t
subr_cons_ast(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc > 0) {
        if (argc == 1) return argv[0];
        scm_obj_t obj = make_pair(vm->m_heap, argv[0], scm_nil);
        scm_obj_t tail = obj;
        for (int i = 1; i < argc - 1; i++) {
            scm_obj_t e = make_pair(vm->m_heap, argv[i], scm_nil);
            CDR(tail) = e;
            tail = e;
        }
        CDR(tail) = argv[argc - 1];
        return obj;
    }
    wrong_number_of_arguments_violation(vm, "cons*", 1, -1, argc, argv);
    return scm_undef;

}

// list-head
scm_obj_t
subr_list_head(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (PAIRP(argv[0])) {
            if (FIXNUMP(argv[1])) {
                intptr_t n = FIXNUM(argv[1]);
                scm_obj_t lst = argv[0];
                if (n >= 0) {
                    if (n == 0) return scm_nil;
                    if (PAIRP(lst)) {
                        scm_obj_t obj = make_pair(vm->m_heap, CAR(lst), scm_nil);
                        scm_obj_t tail = obj;
                        lst = CDR(lst);
                        for (int i = 2; i <= n; i++) {
                            if (PAIRP(lst)) {
                                scm_obj_t e = make_pair(vm->m_heap, CAR(lst), scm_nil);
                                CDR(tail) = e;
                                tail = e;
                                lst = CDR(lst);
                            } else {
                                obj = NULL;
                                break;
                            }
                        }
                        if (obj) {
                            CDR(tail) = scm_nil;
                            return obj;
                        }
                    }
                }
            }
            if (exact_non_negative_integer_pred(argv[1])) {
                invalid_argument_violation(vm, "list-head", "index out of bounds,", argv[1], 1, argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "list-head", 1, "exact non-negative integer", argv[1], argc, argv);
            return scm_undef;
        }
        if (argv[0] == scm_nil) {
            if (FIXNUM(argv[1]) == 0) return scm_nil;
            if (exact_non_negative_integer_pred(argv[1])) {
                invalid_argument_violation(vm, "list-head", "index out of bounds,", argv[1], 1, argc, argv);
                return scm_undef;
            }
            wrong_type_argument_violation(vm, "list-head", 1, "exact non-negative integer", argv[1], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "list-head", 0, "list", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "list-head", 2, 2, argc, argv);
    return scm_undef;
}

// set-car!
scm_obj_t
subr_set_car(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (PAIRP(argv[0])) {
#if USE_PARALLEL_VM
            if (vm->m_interp->live_thread_count() > 1) {
                if (!vm->m_heap->in_heap(argv[0])) {
                    thread_object_access_violation(vm, "set-car!" ,argc, argv);
                    return scm_undef;
                }
                if (vm->m_child > 0) vm->m_interp->remember(CAR(argv[0]), argv[1]);
            }
#endif
#if USE_CONST_LITERAL
            if (vm->m_heap->is_immutable_pair(argv[0])) {
                literal_constant_access_violation(vm, "set-car!", argv[0], argc, argv);
                return scm_undef;
            }
#endif
            vm->m_heap->write_barrier(argv[1]);
            CAR(argv[0]) = argv[1];
            return scm_unspecified;
        }
        wrong_type_argument_violation(vm, "set-car!", 0, "pair", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "set-car!", 2, 2, argc, argv);
    return scm_undef;
}

// set-cdr!
scm_obj_t
subr_set_cdr(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (PAIRP(argv[0])) {
#if USE_PARALLEL_VM
            if (vm->m_interp->live_thread_count() > 1) {
                if (!vm->m_heap->in_heap(argv[0])) {
                    thread_object_access_violation(vm, "set-cdr!" ,argc, argv);
                    return scm_undef;
                }
                if (vm->m_child > 0) vm->m_interp->remember(CDR(argv[0]), argv[1]);
            }
#endif
#if USE_CONST_LITERAL
            if (vm->m_heap->is_immutable_pair(argv[0])) {
                literal_constant_access_violation(vm, "set-cdr!", argv[0], argc, argv);
                return scm_undef;
            }
#endif
            vm->m_heap->write_barrier(argv[1]);
            CDR(argv[0]) = argv[1];
            return scm_unspecified;
        }
        wrong_type_argument_violation(vm, "set-cdr!", 0, "pair", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "set-cdr!", 2, 2, argc, argv);
    return scm_undef;
}

// memq
scm_obj_t
subr_memq(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (PAIRP(argv[1])) {
            scm_obj_t obj = argv[0];
            scm_obj_t lst = argv[1];
            while (PAIRP(lst)) {
                if (CAR(lst) != obj) lst = CDR(lst);
                else return lst;
            }
            if (lst == scm_nil) return scm_false;
            wrong_type_argument_violation(vm, "memq", 1, "list", argv[1], argc, argv);
            return scm_undef;
        }
        if (argv[1] == scm_nil) return scm_false;
        wrong_type_argument_violation(vm, "memq", 1, "list", argv[1], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "memq", 2, 2, argc, argv);
    return scm_undef;
}

// memv
scm_obj_t
subr_memv(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (PAIRP(argv[1])) {
            scm_obj_t obj = argv[0];
            scm_obj_t lst = argv[1];
            while (PAIRP(lst)) {
                if (eqv_pred(CAR(lst), obj)) return lst;
                lst = CDR(lst);
            }
            if (lst == scm_nil) return scm_false;
            wrong_type_argument_violation(vm, "memv", 1, "list", argv[1], argc, argv);
            return scm_undef;
        }
        if (argv[1] == scm_nil) return scm_false;
        wrong_type_argument_violation(vm, "memv", 1, "list", argv[1], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "memv", 2, 2, argc, argv);
    return scm_undef;
}

// member
scm_obj_t
subr_member(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (PAIRP(argv[1])) {
            scm_obj_t obj = argv[0];
            scm_obj_t lst = argv[1];
            scm_hashtable_t visited = make_hashtable(vm->m_heap, SCM_HASHTABLE_TYPE_EQ, lookup_mutable_hashtable_size(0));
            scoped_lock lock(visited->lock);
            while (PAIRP(lst)) {
                if (equal_pred(vm->m_heap, visited, obj, CAR(lst))) return lst;
                clear_volatile_hashtable(visited);
                lst = CDR(lst);
            }
            if (lst == scm_nil) return scm_false;
            wrong_type_argument_violation(vm, "member", 1, "list", argv[1], argc, argv);
            return scm_undef;
        }
        if (argv[1] == scm_nil) return scm_false;
        wrong_type_argument_violation(vm, "member", 1, "list", argv[1], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "member", 2, 2, argc, argv);
    return scm_undef;
}

// assq
scm_obj_t
subr_assq(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (PAIRP(argv[1])) {
            scm_obj_t obj = argv[0];
            scm_obj_t lst = argv[1];
            while (PAIRP(lst)) {
                if (PAIRP(CAR(lst))) {
                    if (CAAR(lst) == obj) return CAR(lst);
                    lst = CDR(lst);
                    continue;
                }
                wrong_type_argument_violation(vm, "assq", 1, "alist", argv[1], argc, argv);
                return scm_undef;
            }
            if (lst == scm_nil) return scm_false;
            wrong_type_argument_violation(vm, "assq", 1, "alist", argv[1], argc, argv);
            return scm_undef;
        }
        if (argv[1] == scm_nil) return scm_false;
        wrong_type_argument_violation(vm, "assq", 1, "alist", argv[1], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "assq", 2, 2, argc, argv);
    return scm_undef;
}

// assv
scm_obj_t
subr_assv(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (PAIRP(argv[1])) {
            scm_obj_t obj = argv[0];
            scm_obj_t lst = argv[1];
            while (PAIRP(lst)) {
                if (PAIRP(CAR(lst))) {
                    if (eqv_pred(CAAR(lst), obj)) return CAR(lst);
                } else {
                    wrong_type_argument_violation(vm, "assv", 1, "alist", argv[1], argc, argv);
                    return scm_undef;
                }
                lst = CDR(lst);
            }
            if (lst == scm_nil) return scm_false;
            wrong_type_argument_violation(vm, "assv", 1, "alist", argv[1], argc, argv);
            return scm_undef;
        }
        if (argv[1] == scm_nil) return scm_false;
        wrong_type_argument_violation(vm, "assv", 1, "alist", argv[1], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "assv", 2, 2, argc, argv);
    return scm_undef;
}

// assoc
scm_obj_t
subr_assoc(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (PAIRP(argv[1])) {
            scm_obj_t obj = argv[0];
            scm_obj_t lst = argv[1];
            scm_hashtable_t visited = make_hashtable(vm->m_heap, SCM_HASHTABLE_TYPE_EQ, lookup_mutable_hashtable_size(0));
            scoped_lock lock(visited->lock);
            while (PAIRP(lst)) {
                if (PAIRP(CAR(lst))) {
                    if (equal_pred(vm->m_heap, visited, obj, CAAR(lst))) return CAR(lst);
                    clear_volatile_hashtable(visited);
                } else {
                    wrong_type_argument_violation(vm, "assoc", 1, "alist", argv[1], argc, argv);
                    return scm_undef;
                }
                lst = CDR(lst);
            }
            if (lst == scm_nil) return scm_false;
            wrong_type_argument_violation(vm, "assoc", 1, "alist", argv[1], argc, argv);
            return scm_undef;
        }
        if (argv[1] == scm_nil) return scm_false;
        wrong_type_argument_violation(vm, "assoc", 1, "alist", argv[1], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "assoc", 2, 2, argc, argv);
    return scm_undef;
}

// circular-list?
scm_obj_t
subr_circular_list_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) return circular_listp(argv[0]) ? scm_true : scm_false;
    wrong_number_of_arguments_violation(vm, "circular-list?", 1, 1, argc, argv);
    return scm_undef;
}

// list-copy
scm_obj_t
subr_list_copy(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        scm_obj_t lst = argv[0];
        if (PAIRP(lst)) {
            if (circular_listp(lst)) {
                wrong_type_argument_violation(vm, "list-copy", 0, "finite chain of pairs", argv[0], argc, argv);
                return scm_undef;
            }
            return list_copy(vm->m_heap, lst);
        }
        return lst;
    }
    wrong_number_of_arguments_violation(vm, "list-copy", 1, 1, argc, argv);
    return scm_undef;
}


void
init_subr_list(object_heap_t* heap)
{
#define DEFSUBR(SYM, FUNC)  heap->intern_system_subr(SYM, FUNC)

    DEFSUBR("list-transpose", subr_list_transpose);
    DEFSUBR("list-transpose+", subr_list_transpose_plus);
    DEFSUBR("list-transpose*", subr_list_transpose_ast);
    DEFSUBR("list-head", subr_list_head);
    DEFSUBR("cons*", subr_cons_ast);
    DEFSUBR("set-car!", subr_set_car);
    DEFSUBR("set-cdr!", subr_set_cdr);
    DEFSUBR("memq", subr_memq);
    DEFSUBR("memv", subr_memv);
    DEFSUBR("member", subr_member);
    DEFSUBR("assq", subr_assq);
    DEFSUBR("assv", subr_assv);
    DEFSUBR("assoc", subr_assoc);
    DEFSUBR("circular-list?", subr_circular_list_pred);
    DEFSUBR("list-copy", subr_list_copy);
}
