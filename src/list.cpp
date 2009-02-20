/*
    Ypsilon Scheme System
    Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#include "core.h"
#include "heap.h"
#include "hash.h"
#include "list.h"

scm_obj_t
list_ref(scm_obj_t lst, int n)
{
    scm_obj_t obj = lst;
    while (--n >= 0) {
        if (PAIRP(obj)) obj = CDR(obj);
        else return NULL;
    }
    if (PAIRP(obj)) return CAR(obj);
    return NULL;
}

scm_obj_t
list_tail(scm_obj_t lst, int n)
{
    if (n < 0) return NULL;
    scm_obj_t obj = lst;
    while (--n >= 0) {
        if (PAIRP(obj)) obj = CDR(obj);
        else return NULL;
    }
    return obj;
}

scm_obj_t
list_copy(object_heap_t* heap, scm_obj_t lst)
{
    if (PAIRP(lst)) {
        scm_obj_t obj = make_pair(heap, CAR(lst), scm_nil);
        scm_obj_t tail = obj;
        lst = CDR(lst);
        while (PAIRP(lst)) {
            scm_obj_t e = make_pair(heap, CAR(lst), scm_nil);
            CDR(tail) = e;
            tail = e;
            lst = CDR(lst);
        }
        CDR(tail) = lst;
        return obj;
    }
    return lst;
}

int
list_length(scm_obj_t lst)
{
    int n = 0;
    while (lst != scm_nil) {
        lst = CDR(lst);
        n++;
    }
    return n;
}

int
safe_list_length(scm_obj_t maybe_list)
{
    int count = 0;
    scm_obj_t fast = maybe_list;
    scm_obj_t slow = fast;
    while (PAIRP(fast)) {
        count++;
        fast = CDR(fast);
        if (fast == scm_nil) return count;
        if (!PAIRP(fast)) return -2; // improper
        count++;
        fast = CDR(fast);
        slow = CDR(slow);
        if (slow == fast) return -1; // circular
    }
    if (fast == scm_nil) return count;
    return -2; // improper
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

struct ancestor_t {
    scm_obj_t*  stack;
    int         capacity;
    scm_obj_t   buf[1024];

    ancestor_t() : stack(NULL) {
        stack = buf;
        capacity = array_sizeof(buf);
    }

    ~ancestor_t() {
        if (stack != buf) free(stack);
    }

    scm_obj_t& operator[](int depth) {
        if (depth >= capacity) {
            capacity += capacity;
            if (depth >= capacity) capacity = depth + 1;
            if (stack == buf) {
                stack = (scm_obj_t*)malloc(sizeof(scm_obj_t) * capacity);
                memcpy(stack, buf, sizeof(buf));
            } else {
                stack = (scm_obj_t*)realloc(stack, sizeof(scm_obj_t) * capacity);
                if (stack == NULL) fatal("%s:%u memory overflow", __FILE__, __LINE__);
            }
        }
        return stack[depth];
    }

    bool contains(scm_obj_t obj, int depth) {
        for (int i = 0; i < depth; i++) {
            if (stack[i] == obj) return true;
        }
        return false;
    }
};

static bool
cyclic_object_test(scm_obj_t lst, ancestor_t& ancestor, int depth)
{

top:
    if (CELLP(lst)) {
        if (ancestor.contains(lst, depth)) return true;
        if (PAIRP(lst)) {
            scm_obj_t type = classify_list(CAR(lst));
            if (type == scm_true) return true;
            if (type == scm_nil) {
                ancestor[depth++] = lst;
                if (CDR(lst) == scm_nil) {
                    lst = CAR(lst);
                    goto top;
                }
                if (cyclic_object_test(CAR(lst), ancestor, depth)) return true;
            }
            lst = CDR(lst);
            goto top;
        }
        if (VECTORP(lst)) {
            scm_vector_t vector = (scm_vector_t)lst;
            int n = vector->count;
            if (n == 0) return false;
            ancestor[depth++] = lst;
            for (int i = 0; i < n - 1; i++) {
                if (cyclic_object_test(vector->elts[i], ancestor, depth)) return true;
            }
            lst = vector->elts[n - 1];
            goto top;
        }
        if (TUPLEP(lst)) {
            scm_tuple_t tuple = (scm_tuple_t)lst;
            int n = HDR_TUPLE_COUNT(tuple->hdr);
            if (n == 0) return false;
            ancestor[depth++] = lst;
            for (int i = 0; i < n - 1; i++) {
                if (cyclic_object_test(tuple->elts[i], ancestor, depth)) return true;
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
    ancestor_t ancestor;
    return cyclic_object_test(lst, ancestor, 0);
}
