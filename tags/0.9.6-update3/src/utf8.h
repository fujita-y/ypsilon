/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#ifndef UTF8_H_INCLUDED
#define UTF8_H_INCLUDED

#include "core.h"
#include "object.h"

class object_heap_t;

#define BAD_UTF8_STRING_REF_DATUM       (-1)
#define BAD_UTF8_STRING_REF_INDEX       (-2)

int utf8_byte_count(const uint8_t datum);
int cnvt_ucs4_to_utf8(uint32_t ucs4, uint8_t utf8[4]);
int cnvt_utf8_to_ucs4(const uint8_t utf8[4], uint32_t* ucs4);
int utf8_char_index_to_byte_offset(const uint8_t datum[], int index, int limit);
int utf8_string_ref(scm_string_t obj, int index);
int utf8_string_length(scm_string_t obj);
void utf8_substring(scm_string_t obj, int from, int to, int* head, int* tail);
bool utf8_string_set(object_heap_t* heap, scm_string_t obj, int index, int ch);
int utf8_sizeof_ucs4(uint32_t ucs4);
bool utf8_decode_test(scm_bvector_t obj);
bool string_eq_pred(scm_obj_t obj1, scm_obj_t obj2);
bool string_ci_eq_pred(scm_obj_t obj1, scm_obj_t obj2);
int string_compare(scm_obj_t obj1, scm_obj_t obj2);

#endif
