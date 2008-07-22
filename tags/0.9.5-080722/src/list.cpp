/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#include "core.h"
#include "heap.h"
#include "hash.h"
#include "list.h"

int
list_length(scm_obj_t list)
{
    int n = 0;
    while (list != scm_nil) {
        list = CDR(list);
        n++;
    }
    return n;
}

scm_obj_t
list_ref(scm_obj_t list, int n)
{
    scm_obj_t obj = list;
    while (--n >= 0) {
        if (PAIRP(obj)) obj = CDR(obj);
        else return NULL;
    }
    if (PAIRP(obj)) return CAR(obj);
    return NULL;
}

scm_obj_t
list_tail(scm_obj_t list, int n)
{
    if (n < 0) return NULL;
    scm_obj_t obj = list;
    while (--n >= 0) {
        if (PAIRP(obj)) obj = CDR(obj);
        else return NULL;
    }
    return obj;
}

bool
circular_listp(scm_obj_t maybe_list)
{
    if (maybe_list == scm_nil) return false;
    scm_obj_t fast = maybe_list;
    scm_obj_t slow = fast;
    while (PAIRP(fast)) {
        fast = CDR(fast);
        if (!PAIRP(fast)) return false;
        fast = CDR(fast);
        slow = CDR(slow);
        if (slow == fast) return true;
    }
    return false;
}

bool
listp(scm_obj_t maybe_list)
{
    if (maybe_list == scm_nil) return true;
    scm_obj_t fast = maybe_list;
    scm_obj_t slow = fast;
    while (PAIRP(fast)) {
        fast = CDR(fast);
        if (!PAIRP(fast)) return fast == scm_nil;
        fast = CDR(fast);
        slow = CDR(slow);
        if (slow == fast) return false;
    }
    return fast == scm_nil;
}
/*
(define circular-tree?
  (lambda (lst)
    (let ((ht (make-core-hashtable)))
      (and (let loop ((lst lst) (ancestor '()))
             (or (and (core-hashtable-ref ht lst #f)
                      (memq lst ancestor))
                 (cond ((pair? lst)
                        (core-hashtable-set! ht lst #t)
                        (let ((ancestor (cons lst ancestor)))
                          (or (loop (car lst) ancestor)
                              (loop (cdr lst) ancestor))))
                       ((vector? lst)
                        (core-hashtable-set! ht lst #t)
                        (let ((ancestor (cons lst ancestor)))
                          (any1 (lambda (e) (loop e ancestor)) (vector->list lst))))
                       ((tuple? lst)
                        (core-hashtable-set! ht lst #t)
                        (let ((ancestor (cons lst ancestor)))
                          (any1 (lambda (e) (loop e ancestor)) (tuple->list lst))))
                       (else #f))))
           #t))))
*/

static bool
infinite_list_test(scm_obj_t lst, scm_obj_t ancestor, scm_hashtable_t ht, object_heap_t* heap)
{
top:
    if (CELLP(lst)) {
        if (get_hashtable(ht, lst) != scm_undef) {
            scm_obj_t p = ancestor;
            while (PAIRP(p)) {
                if (CAR(p) == lst) return true;
                p = CDR(p);
                continue;
            }
            return false;
        }
        if (PAIRP(lst)) {
            int nsize = put_hashtable(ht, lst, scm_true);
            if (nsize) rehash_hashtable(heap, ht, nsize);
            scm_obj_t new_ancestor = make_pair(heap, lst, ancestor);
            if (infinite_list_test(CAR(lst), new_ancestor, ht, heap)) return true;
            lst = CDR(lst);
            ancestor = new_ancestor;
            goto top;
        }
        if (VECTORP(lst)) {
            int nsize = put_hashtable(ht, lst, scm_true);
            if (nsize) rehash_hashtable(heap, ht, nsize);
            scm_obj_t new_ancestor = make_pair(heap, lst, ancestor);
            scm_vector_t vector = (scm_vector_t)lst;
            int n = HDR_VECTOR_COUNT(vector->hdr);
            if (n == 0) return false;
            for (int i = 0; i < n - 1; i++) {
                if (infinite_list_test(vector->elts[i], new_ancestor, ht, heap)) return true;
            }
            lst = vector->elts[n - 1];
            ancestor = new_ancestor;
            goto top;
        }
        if (TUPLEP(lst)) {
            int nsize = put_hashtable(ht, lst, scm_true);
            if (nsize) rehash_hashtable(heap, ht, nsize);
            scm_obj_t new_ancestor = make_pair(heap, lst, ancestor);
            scm_tuple_t tuple = (scm_tuple_t)lst;
            int n = HDR_TUPLE_COUNT(tuple->hdr);
            if (n == 0) return false;
            for (int i = 0; i < n - 1; i++) {
                if (infinite_list_test(tuple->elts[i], new_ancestor, ht, heap)) return true;
            }
            lst = tuple->elts[n - 1];
            ancestor = new_ancestor;
            goto top;
        }
    }
    return false;
}

bool
infinite_listp(object_heap_t* heap, scm_obj_t lst)
{
    scm_hashtable_t ht = make_hashtable(heap, SCM_HASHTABLE_TYPE_EQ, lookup_mutable_hashtable_size(0));
    scoped_lock lock(ht->lock);
    return infinite_list_test(lst, scm_nil, ht, heap);
}
