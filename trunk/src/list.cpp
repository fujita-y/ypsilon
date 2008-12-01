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

static inline bool
containerp(scm_obj_t obj)
{
    return PAIRP(obj) || VECTORP(obj) || TUPLEP(obj);
}

static scm_obj_t
classify_list(scm_obj_t lst)
{
    scm_obj_t fast = lst;
    scm_obj_t slow = lst;
    bool parent = false;
    while (PAIRP(fast)) {
        if (PAIRP(CDR(fast))) {
            if (CDDR(fast) == CDR(slow)) return scm_true;
            parent = parent || containerp(CAR(fast)) || containerp(CADR(fast));
            fast = CDDR(fast);
            slow = CDR(slow);
            continue;
        }
        return (parent || containerp(CAR(fast)) || containerp(CDR(fast))) ? scm_nil : scm_false;
    }
    return (parent || containerp(fast)) ? scm_nil : scm_false;
}

static bool
cyclic_object_test(scm_obj_t lst, scm_obj_t ancestor, object_heap_t* heap)
{

top:
    if (CELLP(lst)) {
        scm_obj_t p = ancestor;
        while (PAIRP(p)) {
            if (CAR(p) == lst) return true;
            p = CDR(p);
            continue;
        }
        if (PAIRP(lst)) {
            scm_obj_t type = classify_list(CAR(lst));
            if (type == scm_true) return true;
            if (type == scm_nil) {
                ancestor = make_pair(heap, lst, ancestor);
                if (cyclic_object_test(CAR(lst), ancestor, heap)) return true;
            }
            lst = CDR(lst);
            goto top;
        }
        if (VECTORP(lst)) {
            scm_vector_t vector = (scm_vector_t)lst;
            int n = vector->count;
            if (n == 0) return false;
            ancestor = make_pair(heap, lst, ancestor);
            for (int i = 0; i < n - 1; i++) {
                if (cyclic_object_test(vector->elts[i], ancestor, heap)) return true;
            }
            lst = vector->elts[n - 1];
            goto top;
        }
        if (TUPLEP(lst)) {
            scm_tuple_t tuple = (scm_tuple_t)lst;
            int n = HDR_TUPLE_COUNT(tuple->hdr);
            if (n == 0) return false;
            ancestor = make_pair(heap, lst, ancestor);
            for (int i = 0; i < n - 1; i++) {
                if (cyclic_object_test(tuple->elts[i], ancestor, heap)) return true;
            }
            lst = tuple->elts[n - 1];
            goto top;
        }
    }
    return false;
}

bool
cyclic_objectp(object_heap_t* heap, scm_obj_t lst)
{
    scm_obj_t type = classify_list(lst);
    if (type == scm_true) return true;
    if (type == scm_false) return false;
    return cyclic_object_test(lst, scm_nil, heap);
}

/* original
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
            int n = vector->count;
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
*/

/* new with hash
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
            scm_obj_t type = classify_list(CAR(lst));
            if (type == scm_true) return true;
            if (type == scm_nil) {
                int nsize = put_hashtable(ht, lst, scm_true);
                if (nsize) rehash_hashtable(heap, ht, nsize);
                ancestor = make_pair(heap, lst, ancestor);
                if (infinite_list_test(CAR(lst), ancestor, ht, heap)) return true;
            }
            lst = CDR(lst);
            goto top;
        }
        if (VECTORP(lst)) {
            scm_vector_t vector = (scm_vector_t)lst;
            int n = vector->count;
            if (n == 0) return false;
            int nsize = put_hashtable(ht, lst, scm_true);
            if (nsize) rehash_hashtable(heap, ht, nsize);
            ancestor = make_pair(heap, lst, ancestor);
            for (int i = 0; i < n - 1; i++) {
                if (infinite_list_test(vector->elts[i], ancestor, ht, heap)) return true;
            }
            lst = vector->elts[n - 1];
            goto top;
        }
        if (TUPLEP(lst)) {
            scm_tuple_t tuple = (scm_tuple_t)lst;
            int n = HDR_TUPLE_COUNT(tuple->hdr);
            if (n == 0) return false;
            int nsize = put_hashtable(ht, lst, scm_true);
            if (nsize) rehash_hashtable(heap, ht, nsize);
            ancestor = make_pair(heap, lst, ancestor);
            for (int i = 0; i < n - 1; i++) {
                if (infinite_list_test(tuple->elts[i], ancestor, ht, heap)) return true;
            }
            lst = tuple->elts[n - 1];
            goto top;
        }
    }
    return false;
}

bool
infinite_listp(object_heap_t* heap, scm_obj_t lst)
{
    scm_obj_t type = classify_list(lst);
    if (type == scm_true) return true;
    if (type == scm_false) return false;
    scm_hashtable_t ht = make_hashtable(heap, SCM_HASHTABLE_TYPE_EQ, lookup_mutable_hashtable_size(0));
    scoped_lock lock(ht->lock);
    return infinite_list_test(lst, scm_nil, ht, heap);
}

*/
