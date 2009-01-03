/*
  Ypsilon Scheme System
  Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
  See license.txt for terms and conditions of use
*/

#include "core.h"
#include "vm.h"
#include "heap.h"
#include "port.h"
#include "subr.h"
#include "ucs4.h"
#include "arith.h"
#include "violation.h"

#include "../unicode/case-folding.inc"
#include "../unicode/general-category-1.inc"
#include "../unicode/general-category-2.inc"
#include "../unicode/numeric-property.inc"
#include "../unicode/other-alphabetic.inc"
#include "../unicode/other-lowercase.inc"
#include "../unicode/other-uppercase.inc"
#include "../unicode/simple-lowercase.inc"
#include "../unicode/simple-titlecase.inc"
#include "../unicode/simple-uppercase.inc"
#include "../unicode/special-casing-lower.inc"
#include "../unicode/special-casing-title.inc"
#include "../unicode/special-casing-upper.inc"
#include "../unicode/canonical-class.inc"
#include "../unicode/decompose.inc"
#include "../unicode/compose.inc"
#include "../unicode/compatibility.inc"

// open-builtin-data-input-port
scm_obj_t
subr_open_builtin_data_input_port(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (STRINGP(argv[0])) {
            scm_string_t string = (scm_string_t)argv[0];
            const char* name = string->name;
            const uint8_t* datum;
            int size;
            if (strcmp(name, "case-folding") == 0) {
                datum = s_case_folding;
                size = sizeof(s_case_folding);
            } else if (strcmp(name, "general-category-1") == 0) {
                datum = s_general_category_1;
                size = sizeof(s_general_category_1);
            } else if (strcmp(name, "general-category-2") == 0) {
                datum = s_general_category_2;
                size = sizeof(s_general_category_2);
            } else if (strcmp(name, "numeric-property") == 0) {
                datum = s_numeric_property;
                size = sizeof(s_numeric_property);
            } else if (strcmp(name, "other-alphabetic") == 0) {
                datum = s_other_alphabetic;
                size = sizeof(s_other_alphabetic);
            } else if (strcmp(name, "other-lowercase") == 0) {
                datum = s_other_lowercase;
                size = sizeof(s_other_lowercase);
            } else if (strcmp(name, "other-uppercase") == 0) {
                datum = s_other_uppercase;
                size = sizeof(s_other_uppercase);
            } else if (strcmp(name, "simple-lowercase") == 0) {
                datum = s_simple_lowercase;
                size = sizeof(s_simple_lowercase);
            } else if (strcmp(name, "simple-titlecase") == 0) {
                datum = s_simple_titlecase;
                size = sizeof(s_simple_titlecase);
            } else if (strcmp(name, "simple-uppercase") == 0) {
                datum = s_simple_uppercase;
                size = sizeof(s_simple_uppercase);
            } else if (strcmp(name, "special-casing-lower") == 0) {
                datum = s_special_casing_lower;
                size = sizeof(s_special_casing_lower);
            } else if (strcmp(name, "special-casing-title") == 0) {
                datum = s_special_casing_title;
                size = sizeof(s_special_casing_title);
            } else if (strcmp(name, "special-casing-upper") == 0) {
                datum = s_special_casing_upper;
                size = sizeof(s_special_casing_upper);
            } else if (strcmp(name, "canonical-class") == 0) {
                datum = s_canonical_class;
                size = sizeof(s_canonical_class);
            } else if (strcmp(name, "decompose") == 0) {
                datum = s_decompose;
                size = sizeof(s_decompose);
            } else if (strcmp(name, "compose") == 0) {
                datum = s_compose;
                size = sizeof(s_compose);
            } else if (strcmp(name, "compatibility") == 0) {
                datum = s_compatibility;
                size = sizeof(s_compatibility);
            } else {
                wrong_type_argument_violation(vm, "open-builtin-data-input-port", 0, "builtin data name", argv[0], argc, argv);
                return scm_undef;
            }
            scm_bvector_t bv = make_bvector_mapping(vm->m_heap, (void*)datum, size);
            return make_bytevector_port(vm->m_heap, make_symbol(vm->m_heap, name), SCM_PORT_DIRECTION_IN, bv, scm_true);
        }
        wrong_type_argument_violation(vm, "open-builtin-data-input-port", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "open-builtin-data-input-port", 1, 1, argc, argv);
    return scm_undef;
}

// char-whitespace?
scm_obj_t
subr_char_whitespace_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (CHARP(argv[0])) {
            int c = CHAR(argv[0]);
            return ucs4_whitespace(c) ? scm_true : scm_false;
        }
        wrong_type_argument_violation(vm, "char-whitespace?", 0, "char", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "char-whitespace?", 1, 1, argc, argv);
    return scm_undef;
}

void init_subr_unicode(object_heap_t* heap)
{
#define DEFSUBR(SYM, FUNC)  heap->intern_system_subr(SYM, FUNC)

    DEFSUBR("char-whitespace?", subr_char_whitespace_pred);
    DEFSUBR("open-builtin-data-input-port", subr_open_builtin_data_input_port);
}
