// Copyright (c) 2004-2022 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef EQUIV_H_INCLUDED
#define EQUIV_H_INCLUDED

#include "core.h"
#include "object.h"

class object_heap_t;

bool eqv_pred(scm_obj_t obj1, scm_obj_t obj2);
bool r5rs_equal_pred(scm_obj_t lst1, scm_obj_t lst2);
bool equal_pred(object_heap_t* heap, scm_hashtable_t visited, scm_obj_t lst1, scm_obj_t lst2);

#endif
