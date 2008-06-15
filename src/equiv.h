/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#ifndef EQUIV_H_INCLUDED
#define EQUIV_H_INCLUDED

#include "core.h"
#include "object.h"

bool eqv_pred(scm_obj_t obj1, scm_obj_t obj2);
bool r5rs_equal_pred(scm_obj_t lst1, scm_obj_t lst2);
bool equal_pred(object_heap_t* heap, scm_hashtable_t visited, scm_obj_t lst1, scm_obj_t lst2);

#endif
