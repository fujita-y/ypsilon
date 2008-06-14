/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#ifndef	HASH_H_INCLUDED
#define	HASH_H_INCLUDED

#include "core.h"
#include "object.h"

enum {
	SCM_HASHTABLE_TYPE_EQ	= 0,
	SCM_HASHTABLE_TYPE_EQV,
	SCM_HASHTABLE_TYPE_EQUAL,   // use r5rs style equal
	SCM_HASHTABLE_TYPE_STRING,
    SCM_HASHTABLE_TYPE_GENERIC
};

enum {
    SCM_HASHTABLE_HANDLER_SIGNATURE = 0,
    SCM_HASHTABLE_HANDLER_HASH,
    SCM_HASHTABLE_HANDLER_EQUIV,
    SCM_HASHTABLE_HANDLER_SIZE,
    SCM_HASHTABLE_HANDLER_REF,
    SCM_HASHTABLE_HANDLER_SET,
    SCM_HASHTABLE_HANDLER_DELETE,
    SCM_HASHTABLE_HANDLER_CONTAINS,
    SCM_HASHTABLE_HANDLER_COPY,
    SCM_HASHTABLE_HANDLER_CLEAR,
    SCM_HASHTABLE_HANDLER_HASH_FUNC,
    SCM_HASHTABLE_HANDLER_EQUIV_FUNC,
    SCM_HASHTABLE_HANDLER_MUTABLE,
    SCM_HASHTABLE_HANDLER_ALIST
};

int	lookup_mutable_hashtable_size(int n);
int lookup_immutable_hashtable_size(int n);

uint32_t	address_hash(void *adrs, uint32_t bound);
uint32_t	string_hash(const char *str, uint32_t bound);
uint32_t    eqv_hash(scm_obj_t obj, uint32_t bound);
uint32_t	string_hash(scm_obj_t obj, uint32_t bound);
uint32_t	equal_hash(scm_obj_t obj, uint32_t bound);
uint32_t	relocation_safe_equal_hash(scm_obj_t obj, uint32_t bound);
uint32_t	address_hash2(void *adrs, uint32_t bound);
uint32_t	string_hash2(const char *str, uint32_t bound);

bool        eqv_hash_equiv(scm_obj_t obj1, scm_obj_t obj2);
bool        equal_hash_equiv(scm_obj_t obj1, scm_obj_t obj2);
bool        string_hash_equiv(scm_obj_t obj1, scm_obj_t obj2);

int			put_hashtable(scm_hashtable_t ht, scm_obj_t key, scm_obj_t value);
scm_obj_t	get_hashtable(scm_hashtable_t ht, scm_obj_t key);
int			remove_hashtable(scm_hashtable_t ht, scm_obj_t key);

int			put_weakhashtable(scm_weakhashtable_t ht, scm_weakmapping_t wmap);
scm_obj_t	lookup_weakhashtable(scm_weakhashtable_t ht, scm_obj_t key);
int			remove_weakhashtable(scm_weakhashtable_t ht, scm_obj_t key);

#endif
