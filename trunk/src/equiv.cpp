/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#include "core.h"
#include "hash.h"
#include "heap.h"
#include "list.h"
#include "utf8.h"
#include "arith.h"
#include "equiv.h"

#define USE_R5RS_EQUAL  0

bool
eqv_pred(scm_obj_t obj1, scm_obj_t obj2)
{
    if (obj1 == obj2) return true;
    if (!CELLP(obj1)) return false;
    if (!CELLP(obj2)) return false;
    if (number_pred(obj1)) {
        if (number_pred(obj2)) {
            if (n_exact_pred(obj1)) {
                if (n_exact_pred(obj2)) {
                    return n_exact_equal_pred(obj1, obj2);
                }
            } else {
                if (n_exact_pred(obj2)) return false;
                return n_inexact_equal_pred(obj1, obj2);
            }
        }
    }
    if (TUPLEP(obj1)) {
        if (TUPLEP(obj2)) {
            scm_tuple_t tuple1 = (scm_tuple_t)obj1;
            scm_tuple_t tuple2 = (scm_tuple_t)obj2;
            int n1 = HDR_TUPLE_COUNT(tuple1->hdr);
            int n2 = HDR_TUPLE_COUNT(tuple2->hdr);
            if (n1 == n2) {
                scm_obj_t* elts1 = tuple1->elts;
                scm_obj_t* elts2 = tuple2->elts;
                for (int i = 0; i < n1; i++) {
                    if (eqv_pred(elts1[i], elts2[i])) continue;
                    return false;
                }
                return true;
            }
        }
        return false;
    }
    return false;
}

bool
r5rs_equal_pred(scm_obj_t lst1, scm_obj_t lst2)
{
top:
    if (lst1 == lst2) return true;
    if (PAIRP(lst1)) {
        if (PAIRP(lst2)) {
            if (r5rs_equal_pred(CAR(lst1), CAR(lst2))) {
                lst1 = CDR(lst1);
                lst2 = CDR(lst2);
                goto top;
            }
        }
        return false;
    }
    if (VECTORP(lst1)) {
        if (VECTORP(lst2)) {
            scm_vector_t vector1 = (scm_vector_t)lst1;
            scm_vector_t vector2 = (scm_vector_t)lst2;
            int n1 = vector1->count;
            int n2 = vector2->count;
            if (n1 == n2) {
                scm_obj_t* elts1 = vector1->elts;
                scm_obj_t* elts2 = vector2->elts;
                for (int i = 0; i < n1; i++) {
                    if (r5rs_equal_pred(elts1[i], elts2[i])) continue;
                    return false;
                }
                return true;
            }
        }
        return false;
    }
    if (BVECTORP(lst1)) {
        if (BVECTORP(lst2)) {
            scm_bvector_t bvector1 = (scm_bvector_t)lst1;
            scm_bvector_t bvector2 = (scm_bvector_t)lst2;
            if (bvector1->count == bvector2->count) {
                return (memcmp(bvector1->elts, bvector2->elts, bvector1->count) == 0);
            }
       }
       return false;
    }
    if (string_eq_pred(lst1, lst2)) return true;
    return eqv_pred(lst1, lst2);
}

static int
terminal_listp(scm_obj_t maybe_list)
{
    int count = 1;
    if (maybe_list == scm_nil) return count;
    scm_obj_t fast = maybe_list;
    scm_obj_t slow = fast;
    while (PAIRP(fast)) {
        count++;
        fast = CDR(fast);
        if (!PAIRP(fast)) return count;
        scm_obj_t elt = CAR(fast);
        if (PAIRP(elt) || VECTORP(elt) || TUPLEP(elt)) return 0;
        fast = CDR(fast);
        slow = CDR(slow);
        if (slow == fast) return 0;
    }
    return count;
}

static bool
find_and_merge_opponent(object_heap_t* heap, scm_hashtable_t visited, scm_obj_t lst1, scm_obj_t lst2)
{
    scm_obj_t opponents = get_hashtable(visited, lst1);
    if (opponents != scm_undef) {
        scm_obj_t lst = opponents;
        while (PAIRP(lst)) {
            if (CAR(lst) != lst2) {
                lst = CDR(lst);
                continue;
            }
            return true;
        }
        int nsize = put_hashtable(visited, lst1, make_pair(heap, lst2, opponents));
        if (nsize) rehash_hashtable(heap, visited, nsize);
    } else {
        int nsize = put_hashtable(visited, lst1, scm_nil);
        if (nsize) rehash_hashtable(heap, visited, nsize);
    }
    return false;
}

bool
equal_pred(object_heap_t* heap, scm_hashtable_t visited, scm_obj_t lst1, scm_obj_t lst2)
{
#if USE_R5RS_EQUAL
    return r5rs_equal_pred(lst1, lst2);
#endif
    int c1 = terminal_listp(lst1);
    if (c1) {
        if (c1 == terminal_listp(lst2)) return r5rs_equal_pred(lst1, lst2);
        return false;
    } else {
        if (terminal_listp(lst2)) return false;
    }

top:
    if (lst1 == lst2) return true;
    if (PAIRP(lst1)) {
        if (PAIRP(lst2)) {
            if (find_and_merge_opponent(heap, visited, lst1, lst2)) return true;
            if (equal_pred(heap, visited, CAR(lst1), CAR(lst2))) {
                lst1 = CDR(lst1);
                lst2 = CDR(lst2);
                goto top;
            }
        }
        return false;
    }
    if (VECTORP(lst1)) {
        if (VECTORP(lst2)) {
            if (find_and_merge_opponent(heap, visited, lst1, lst2)) return true;
            scm_vector_t vector1 = (scm_vector_t)lst1;
            scm_vector_t vector2 = (scm_vector_t)lst2;
            int n1 = vector1->count;
            int n2 = vector2->count;
            if (n1 == n2) {
                scm_obj_t* elts1 = vector1->elts;
                scm_obj_t* elts2 = vector2->elts;
                for (int i = 0; i < n1; i++) {
                    if (equal_pred(heap, visited, elts1[i], elts2[i])) continue;
                    return false;
                }
                return true;
            }
        }
        return false;
    }
    if (BVECTORP(lst1)) {
        if (BVECTORP(lst2)) {
            scm_bvector_t bvector1 = (scm_bvector_t)lst1;
            scm_bvector_t bvector2 = (scm_bvector_t)lst2;
            if (bvector1->count == bvector2->count) {
                return (memcmp(bvector1->elts, bvector2->elts, bvector1->count) == 0);
            }
       }
       return false;
    }
    if (string_eq_pred(lst1, lst2)) return true;
    return eqv_pred(lst1, lst2);
}

/*
 R6RS
(define equal?
  (lambda (lst1 lst2)
    (let ((visited (make-core-hashtable)))
      (let loop ((lst1 lst1) (lst2 lst2))
        (or (eq? lst1 lst2)
            (cond ((pair? lst1)
                   (and (pair? lst2)
                        (cond ((core-hashtable-ref visited lst1 #f)
                               => (lambda (opponents)
                                    (cond ((memq lst2 opponents) #t)
                                          (else
                                           (core-hashtable-set! visited lst1 (cons lst2 opponents))
                                           (and (loop (car lst1) (car lst2))
                                                (loop (cdr lst1) (cdr lst2)))))))
                              (else
                               (core-hashtable-set! visited lst1 '())
                               (and (loop (car lst1) (car lst2))
                                    (loop (cdr lst1) (cdr lst2)))))))
                  ((vector? lst1)
                   (and (vector? lst2)
                        (cond ((core-hashtable-ref visited lst1 #f)
                               => (lambda (opponents)
                                    (cond ((memq lst2 opponents) #t)
                                          (else
                                           (core-hashtable-set! visited lst1 (cons lst2 opponents))
                                           (and (= (vector-length lst1) (vector-length lst2))
                                                (every2 loop (vector->list lst1) (vector->list lst2)))))))
                              (else
                               (core-hashtable-set! visited lst1 '())
                               (and (= (vector-length lst1) (vector-length lst2))
                                    (every2 loop (vector->list lst1) (vector->list lst2)))))))
                  ((string? lst1)
                   (and (string? lst2) (string=? lst1 lst2)))
                  (else
                   (eqv? lst1 lst2))))))))
 */
