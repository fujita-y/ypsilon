/*
    Ypsilon Scheme System
    Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#ifndef LIST_H_INCLUDED
#define LIST_H_INCLUDED

#include "core.h"
#include "object.h"

class object_heap_t;

int list_length(scm_obj_t list);
int safe_list_length(scm_obj_t maybe_list);
scm_obj_t list_ref(scm_obj_t list, int n);
scm_obj_t list_tail(scm_obj_t list, int n);
scm_obj_t list_copy(object_heap_t* heap, scm_obj_t lst);
bool listp(scm_obj_t maybe_list);
bool circular_listp(scm_obj_t maybe_list);
bool cyclic_objectp(object_heap_t* heap, scm_obj_t lst);

#endif
