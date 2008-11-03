/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#include "core.h"
#include "vm.h"
#include "file.h"
#include "fasl.h"
#include "hash.h"
#include "heap.h"
#include "port.h"
#include "subr.h"
#include "ucs4.h"
#include "utf8.h"
#include "arith.h"
#include "equiv.h"
#include "reader.h"
#include "ioerror.h"
#include "printer.h"
#include "violation.h"

// make-weak-core-hashtable
scm_obj_t
subr_make_weak_core_hashtable(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 0) {
        int nsize = lookup_mutable_hashtable_size(0);
        return make_weakhashtable(vm->m_heap, nsize);
    }
    if (argc == 1) {
        if (FIXNUMP(argv[0]) && FIXNUM(argv[0]) >= 0) {
            int nsize = lookup_mutable_hashtable_size(FIXNUM(argv[0]));
            return make_weakhashtable(vm->m_heap, nsize);
        }
        wrong_type_argument_violation(vm, "make-weak-core-hashtable", 0, "non-negative fixnum", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "make-weak-core-hashtable", 0, 1, argc, argv);
    return scm_undef;
}

// weak-core-hashtable?
scm_obj_t
subr_weak_core_hashtable_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        return WEAKHASHTABLEP(argv[0]) ? scm_true : scm_false;
    }
    wrong_number_of_arguments_violation(vm, "weak-core-hashtable?", 1, 1, argc, argv);
    return scm_undef;
}

// make-core-hashtable
scm_obj_t
subr_make_core_hashtable(VM* vm, int argc, scm_obj_t argv[])
{
    int nsize = lookup_mutable_hashtable_size(0);
    if (argc == 0) {
        return make_hashtable(vm->m_heap, SCM_HASHTABLE_TYPE_EQ, nsize);
    }
    if (argc == 1 || argc == 2) {
        if (SYMBOLP(argv[0])) {
            if (argv[0] == make_symbol(vm->m_heap, "generic")) {
                if (argc == 2) {
                    if (VECTORP(argv[1])) {
                        scm_vector_t vector = (scm_vector_t)argv[1];
                        if (vector->count == 14) {
                            return make_generic_hashtable(vm->m_heap, (scm_vector_t)vector);
                        }
                    }
                    wrong_type_argument_violation(vm, "make-core-hashtable", 1, "14 elements vector", argv[1], argc, argv);
                    return scm_undef;
                }
                wrong_type_argument_violation(vm, "make-core-hashtable", 1, "vector", NULL, argc, argv);
                return scm_undef;
            }
            if (argc == 2) {
                if (FIXNUMP(argv[1])) {
                    nsize = lookup_mutable_hashtable_size(FIXNUM(argv[1]));
                } else {
                    wrong_type_argument_violation(vm, "make-core-hashtable", 1, "fixnum", argv[1], argc, argv);
                    return scm_undef;
                }
            }
            if (argv[0] == make_symbol(vm->m_heap, "eq?")) {
                return make_hashtable(vm->m_heap, SCM_HASHTABLE_TYPE_EQ, nsize);
            }
            if (argv[0] == make_symbol(vm->m_heap, "eqv?")) {
                return make_hashtable(vm->m_heap, SCM_HASHTABLE_TYPE_EQV, nsize);
            }
            if (argv[0] == make_symbol(vm->m_heap, "equal?")) {
                return make_hashtable(vm->m_heap, SCM_HASHTABLE_TYPE_EQUAL, nsize);
            }
            if (argv[0] == make_symbol(vm->m_heap, "string=?")) {
                return make_hashtable(vm->m_heap, SCM_HASHTABLE_TYPE_STRING, nsize);
            }
        }
        wrong_type_argument_violation(vm, "make-core-hashtable", 0, "eq?, eqv?, equal?, string=?, or generic", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "make-core-hashtable", 0, 1, argc, argv);
    return scm_undef;
}

// core-hashtable?
scm_obj_t
subr_core_hashtable_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        return (HASHTABLEP(argv[0]) || WEAKHASHTABLEP(argv[0])) ? scm_true : scm_false;
    }
    wrong_number_of_arguments_violation(vm, "core-hashtable?", 1, 1, argc, argv);
    return scm_undef;
}

// core-hashtable-mutable?
scm_obj_t
subr_core_hashtable_mutable_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (HASHTABLEP(argv[0])) {
            scm_hashtable_t ht = (scm_hashtable_t)argv[0];
            scoped_lock lock(ht->lock);
            if (ht->handlers == scm_false) {
                return HDR_HASHTABLE_IMMUTABLE(ht->hdr) ? scm_false : scm_true;
            }
            assert(VECTORP(ht->handlers));
            scm_vector_t vector = (scm_vector_t)ht->handlers;
            vm->apply_scheme_argv(vector->elts[SCM_HASHTABLE_HANDLER_MUTABLE], argc, argv);
            return scm_undef;
        }
        if (WEAKHASHTABLEP(argv[0])) {
            scm_weakhashtable_t ht = (scm_weakhashtable_t)argv[0];
            return HDR_HASHTABLE_IMMUTABLE(ht->hdr) ? scm_false : scm_true;
        }
        wrong_type_argument_violation(vm, "core-hashtable-mutable?", 0, "core-hashtable or weak-core-hashtable", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "core-hashtable-mutable?", 1, 1, argc, argv);
    return scm_undef;
}

// core-hashtable-equivalence-function
scm_obj_t
subr_core_hashtable_equivalence_function(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (HASHTABLEP(argv[0])) {
            scm_hashtable_t ht = (scm_hashtable_t)argv[0];
            scoped_lock lock(ht->lock);
            if (ht->handlers == scm_false) {
                switch (ht->type) {
                case SCM_HASHTABLE_TYPE_EQ:
                    return vm->lookup_current_environment(make_symbol(vm->m_heap, "eq?"));
                case SCM_HASHTABLE_TYPE_EQV:
                    return vm->lookup_current_environment(make_symbol(vm->m_heap, "eqv?"));
                case SCM_HASHTABLE_TYPE_EQUAL:
                    return vm->lookup_current_environment(make_symbol(vm->m_heap, "equal?"));
                case SCM_HASHTABLE_TYPE_STRING:
                    return vm->lookup_current_environment(make_symbol(vm->m_heap, "string=?"));
                }
                return scm_false;
            }
            assert(VECTORP(ht->handlers));
            scm_vector_t vector = (scm_vector_t)ht->handlers;
            vm->apply_scheme_argv(vector->elts[SCM_HASHTABLE_HANDLER_EQUIV_FUNC], argc, argv);
            return scm_undef;
        }
        if (WEAKHASHTABLEP(argv[0])) {
            return vm->lookup_current_environment(make_symbol(vm->m_heap, "eq?"));
        }
        wrong_type_argument_violation(vm, "core-hashtable-equivalence-function", 0, "core-hashtable or weak-core-hashtable", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "core-hashtable-equivalence-function", 1, 1, argc, argv);
    return scm_undef;
}

// core-hashtable-hash-function
scm_obj_t
subr_core_hashtable_hash_function(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (HASHTABLEP(argv[0])) {
            scm_hashtable_t ht = (scm_hashtable_t)argv[0];
            scoped_lock lock(ht->lock);
            if (ht->handlers == scm_false) {
                return scm_false;
            }
            assert(VECTORP(ht->handlers));
            scm_vector_t vector = (scm_vector_t)ht->handlers;
            vm->apply_scheme_argv(vector->elts[SCM_HASHTABLE_HANDLER_HASH_FUNC], argc, argv);
            return scm_undef;
        }
        if (WEAKHASHTABLEP(argv[0])) {
            return scm_false;
        }
        wrong_type_argument_violation(vm, "core-hashtable-hash-function", 0, "core-hashtable or weak-core-hashtable", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "core-hashtable-hash-function", 1, 1, argc, argv);
    return scm_undef;
}

// core-hashtable-set!
scm_obj_t
subr_core_hashtable_set(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 3) {
        if (HASHTABLEP(argv[0])) {
            scm_hashtable_t ht = (scm_hashtable_t)argv[0];
            if (!HDR_HASHTABLE_IMMUTABLE(ht->hdr)) {
#if USE_PARALLEL_VM
                if (!vm->m_heap->in_heap(ht)) {
                    thread_object_access_violation(vm, "core-hashtable-set!" ,argc, argv);
                    return scm_undef;
                }
#endif                
                vm->m_heap->write_barrier(argv[1]);
                vm->m_heap->write_barrier(argv[2]);
                scoped_lock lock(ht->lock);
                if (ht->handlers == scm_false) {
                    int nsize = put_hashtable(ht, argv[1], argv[2]);
                    if (nsize) rehash_hashtable(vm->m_heap, ht, nsize);
                    return scm_unspecified;
                }
                assert(VECTORP(ht->handlers));
                scm_vector_t vector = (scm_vector_t)ht->handlers;
                vm->apply_scheme_argv(vector->elts[SCM_HASHTABLE_HANDLER_SET], argc, argv);
                return scm_undef;
            }
            invalid_object_violation(vm, "core-hashtable-set!", "mutable hashtable", argv[0], argc, argv);
            return scm_undef;
        }
        if (WEAKHASHTABLEP(argv[0])) {
            scm_weakhashtable_t ht = (scm_weakhashtable_t)argv[0];
            if (!HDR_HASHTABLE_IMMUTABLE(ht->hdr)) {
#if USE_PARALLEL_VM
                if (!vm->m_heap->in_heap(ht)) {
                    thread_object_access_violation(vm, "core-hashtable-set!" ,argc, argv);
                    return scm_undef;
                }
#endif
                scoped_lock lock(ht->lock);
                scm_obj_t ref = lookup_weakhashtable(ht, argv[1]);
                if (ref == scm_undef) {
                    scm_weakmapping_t wmap = make_weakmapping(vm->m_heap, argv[1], argv[2]);
                    vm->m_heap->write_barrier(wmap);
                    int nsize = put_weakhashtable(ht, wmap);
                    if (nsize) rehash_weakhashtable(vm->m_heap, ht, nsize);
                } else {
                    assert(WEAKMAPPINGP(ref));
                    vm->m_heap->write_barrier(argv[2]);
                    ((scm_weakmapping_t)ref)->value = argv[2];
                }
                return scm_unspecified;
            }
            invalid_object_violation(vm, "core-hashtable-set!", "mutable hashtable", argv[0], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "core-hashtable-set!", 0, "core-hashtable or weak-core-hashtable", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "core-hashtable-set!", 3, 3, argc, argv);
    return scm_undef;
}

// core-hashtable-ref
scm_obj_t
subr_core_hashtable_ref(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 3) {
        if (HASHTABLEP(argv[0])) {
            scm_hashtable_t ht = (scm_hashtable_t)argv[0];
            scoped_lock lock(ht->lock);
            if (ht->handlers == scm_false) {
                scm_obj_t value = get_hashtable(ht, argv[1]);
                if (value != scm_undef) return value;
                return argv[2];
            }
            assert(VECTORP(ht->handlers));
            scm_vector_t vector = (scm_vector_t)ht->handlers;
            vm->apply_scheme_argv(vector->elts[SCM_HASHTABLE_HANDLER_REF], argc, argv);
            return scm_undef;
        }
        if (WEAKHASHTABLEP(argv[0])) {
            scm_weakhashtable_t ht = (scm_weakhashtable_t)argv[0];
            scoped_lock lock(ht->lock);
            scm_obj_t ref = lookup_weakhashtable(ht, argv[1]);
            if (ref == scm_undef) return argv[2];
            assert(WEAKMAPPINGP(ref));
            return ((scm_weakmapping_t)ref)->value;
        }
        wrong_type_argument_violation(vm, "core-hashtable-ref", 0, "core-hashtable or weak-core-hashtable", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "core-hashtable-ref", 3, 3, argc, argv);
    return scm_undef;
}

// core-hashtable-delete!
scm_obj_t
subr_core_hashtable_delete(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (HASHTABLEP(argv[0])) {
            scm_hashtable_t ht = (scm_hashtable_t)argv[0];
            if (!HDR_HASHTABLE_IMMUTABLE(ht->hdr)) {
#if USE_PARALLEL_VM
                if (!vm->m_heap->in_heap(ht)) {
                    thread_object_access_violation(vm, "core-hashtable-delete!" ,argc, argv);
                    return scm_undef;
                }
#endif
                scoped_lock lock(ht->lock);
                if (ht->handlers == scm_false) {
                    int nsize = remove_hashtable(ht, argv[1]);
                    if (nsize) rehash_hashtable(vm->m_heap, ht, nsize);
                    return scm_unspecified;
                }
                assert(VECTORP(ht->handlers));
                scm_vector_t vector = (scm_vector_t)ht->handlers;
                vm->apply_scheme_argv(vector->elts[SCM_HASHTABLE_HANDLER_DELETE], argc, argv);
                return scm_undef;
            }
            invalid_object_violation(vm, "core-hashtable-delete!", "mutable hashtable", argv[0], argc, argv);
            return scm_undef;
        }
        if (WEAKHASHTABLEP(argv[0])) {
            scm_weakhashtable_t ht = (scm_weakhashtable_t)argv[0];
            if (!HDR_HASHTABLE_IMMUTABLE(ht->hdr)) {
#if USE_PARALLEL_VM
                if (!vm->m_heap->in_heap(ht)) {
                    thread_object_access_violation(vm, "core-hashtable-delete!" ,argc, argv);
                    return scm_undef;
                }
#endif
                scoped_lock lock(ht->lock);
                int nsize = remove_weakhashtable(ht, argv[1]);
                if (nsize) rehash_weakhashtable(vm->m_heap, ht, nsize);
                return scm_unspecified;
            }
            invalid_object_violation(vm, "core-hashtable-delete!", "mutable hashtable", argv[0], argc, argv);
            return scm_undef;
        }
        wrong_type_argument_violation(vm, "core-hashtable-delete!", 0, "core-hashtable or weak-core-hashtable", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "core-hashtable-delete!", 2, 2, argc, argv);
    return scm_undef;
}

// core-hashtable-clear!
scm_obj_t
subr_core_hashtable_clear(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1 || argc == 2) {
        if (HASHTABLEP(argv[0]) || WEAKHASHTABLEP(argv[0])) {
            int nsize;
            if (argc == 2) {
                if (FIXNUMP(argv[1]) && FIXNUM(argv[1]) >= 0) {
                    nsize = lookup_mutable_hashtable_size(FIXNUM(argv[1]));
                } else {
                    wrong_type_argument_violation(vm, "core-hashtable-clear!", 1, "non-negative fixnum", argv[1], argc, argv);
                    return scm_undef;
                }
            } else {
                nsize = lookup_mutable_hashtable_size(0);
            }
            if (HASHTABLEP(argv[0])) {
                scm_hashtable_t ht = (scm_hashtable_t)argv[0];
                if (!HDR_HASHTABLE_IMMUTABLE(ht->hdr)) {
#if USE_PARALLEL_VM
                    if (!vm->m_heap->in_heap(ht)) {
                        thread_object_access_violation(vm, "core-hashtable-clear!" ,argc, argv);
                        return scm_undef;
                    }
#endif
                    scoped_lock lock(ht->lock);
                    if (ht->handlers == scm_false) {
                        clear_hashtable(vm->m_heap, ht, nsize);
                        return scm_unspecified;
                    }
                    assert(VECTORP(ht->handlers));
                    scm_vector_t vector = (scm_vector_t)ht->handlers;
                    vm->apply_scheme_argv(vector->elts[SCM_HASHTABLE_HANDLER_CLEAR], argc, argv);
                    return scm_undef;
                }
                invalid_object_violation(vm, "core-hashtable-clear!", "mutable hashtable", argv[0], argc, argv);
                return scm_undef;
            }
            if (WEAKHASHTABLEP(argv[0])) {
                scm_weakhashtable_t ht = (scm_weakhashtable_t)argv[0];
#if USE_PARALLEL_VM
                if (!vm->m_heap->in_heap(ht)) {
                    thread_object_access_violation(vm, "core-hashtable-clear!" ,argc, argv);
                    return scm_undef;
                }
#endif
                if (!HDR_HASHTABLE_IMMUTABLE(ht->hdr)) {
                    scoped_lock lock(ht->lock);
                    clear_weakhashtable(vm->m_heap, ht, nsize);
                    return scm_unspecified;
                }
                invalid_object_violation(vm, "core-hashtable-clear!", "mutable hashtable", argv[0], argc, argv);
                return scm_undef;
            }
        }
        wrong_type_argument_violation(vm, "core-hashtable-clear!", 0, "core-hashtable or weak-core-hashtable", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "core-hashtable-clear!", 1, 2, argc, argv);
    return scm_undef;
}

// core-hashtable-size
scm_obj_t
subr_core_hashtable_size(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (HASHTABLEP(argv[0]) || WEAKHASHTABLEP(argv[0])) {
            if (HASHTABLEP(argv[0])) {
                scm_hashtable_t ht = (scm_hashtable_t)argv[0];
                scoped_lock lock(ht->lock);
                if (ht->handlers == scm_false) {
                    assert(ht->datum);
                    return MAKEFIXNUM(ht->datum->live);
                }
                assert(VECTORP(ht->handlers));
                scm_vector_t vector = (scm_vector_t)ht->handlers;
                vm->apply_scheme_argv(vector->elts[SCM_HASHTABLE_HANDLER_SIZE], argc, argv);
                return scm_undef;
            }
            if (WEAKHASHTABLEP(argv[0])) {
                scm_weakhashtable_t ht = (scm_weakhashtable_t)argv[0];
                scoped_lock lock(ht->lock);
                return MAKEFIXNUM(ht->datum->live);
            }
        }
        wrong_type_argument_violation(vm, "core-hashtable-size", 0, "core-hashtable or weak-core-hashtable", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "core-hashtable-size", 1, 1, argc, argv);
    return scm_undef;
}


// core-hashtable-contains?
scm_obj_t
subr_core_hashtable_contains_pred(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 2) {
        if (HASHTABLEP(argv[0])) {
            scm_hashtable_t ht = (scm_hashtable_t)argv[0];
            scoped_lock lock(ht->lock);
            if (ht->handlers == scm_false) {
                scm_obj_t value = get_hashtable(ht, argv[1]);
                return (value != scm_undef) ? scm_true : scm_false;
            }
            assert(VECTORP(ht->handlers));
            scm_vector_t vector = (scm_vector_t)ht->handlers;
            vm->apply_scheme_argv(vector->elts[SCM_HASHTABLE_HANDLER_CONTAINS], argc, argv);
            return scm_undef;
        }
        if (WEAKHASHTABLEP(argv[0])) {
            scm_weakhashtable_t ht = (scm_weakhashtable_t)argv[0];
            scoped_lock lock(ht->lock);
            scm_obj_t ref = lookup_weakhashtable(ht, argv[1]);
            return (ref != scm_undef) ? scm_true : scm_false;
        }
        wrong_type_argument_violation(vm, "core-hashtable-contains?", 0, "core-hashtable or weak-core-hashtable", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "core-hashtable-contains?", 2, 2, argc, argv);
    return scm_undef;
}

// core-hashtable-copy
scm_obj_t
subr_core_hashtable_copy(VM* vm, int argc, scm_obj_t argv[])
{
    bool immutable = true;
    if (argc == 1 || argc == 2) {
        if (argc == 2 && argv[1] != scm_false) immutable = false;
        if (HASHTABLEP(argv[0])) {
            scm_hashtable_t ht = (scm_hashtable_t)argv[0];
            scoped_lock lock(ht->lock);
            if (ht->handlers == scm_false) {
                return copy_hashtable(vm->m_heap, ht, immutable);
            }
            assert(VECTORP(ht->handlers));
            scm_vector_t vector = (scm_vector_t)ht->handlers;
            vm->apply_scheme_argv(vector->elts[SCM_HASHTABLE_HANDLER_COPY], argc, argv);
            return scm_undef;
        }
        if (WEAKHASHTABLEP(argv[0])) {
            scm_weakhashtable_t ht = (scm_weakhashtable_t)argv[0];
            scoped_lock lock(ht->lock);
            return copy_weakhashtable(vm->m_heap, ht, immutable);
        }
        wrong_type_argument_violation(vm, "core-hashtable-copy", 0, "core-hashtable or weak-core-hashtable", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "core-hashtable-copy", 1, 2, argc, argv);
    return scm_undef;
}

// core-hashtable->alist
scm_obj_t
subr_core_hashtable_alist(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (HASHTABLEP(argv[0])) {
            scm_hashtable_t ht = (scm_hashtable_t)argv[0];
            scoped_lock lock(ht->lock);
            if (ht->handlers == scm_false) {
                hashtable_rec_t* ht_datum = ht->datum;
                assert(ht_datum);
                int nsize = ht_datum->capacity;
                scm_obj_t ans = scm_nil;
                for (int i = 0; i < nsize; i++) {
                    if (ht_datum->elts[i] != scm_hash_free && ht_datum->elts[i] != scm_hash_deleted) {
                        ans = make_pair(vm->m_heap, make_pair(vm->m_heap, ht_datum->elts[i], ht_datum->elts[i + nsize]) , ans);
                    }
                }
                return ans;
            }
            assert(VECTORP(ht->handlers));
            scm_vector_t vector = (scm_vector_t)ht->handlers;
            vm->apply_scheme_argv(vector->elts[SCM_HASHTABLE_HANDLER_ALIST], argc, argv);
            return scm_undef;
        }
        if (WEAKHASHTABLEP(argv[0])) {
            scm_weakhashtable_t ht = (scm_weakhashtable_t)argv[0];
            weakhashtable_rec_t* ht_datum = ht->datum;
            int nsize = ht_datum->capacity;
            scm_obj_t ans = scm_nil;
            for (int i = 0; i < nsize; i++) {
                if (ht_datum->elts[i] != scm_hash_free && ht_datum->elts[i] != scm_hash_deleted) {
                    assert(WEAKMAPPINGP(ht_datum->elts[i]));
                    scm_weakmapping_t wmap = (scm_weakmapping_t)ht_datum->elts[i];
                    ans = make_pair(vm->m_heap, make_pair(vm->m_heap, wmap->key, wmap->value) , ans);
                }
            }
            return ans;
        }
        wrong_type_argument_violation(vm, "core-hashtable->alist", 0, "core-hashtable or weak-core-hashtable", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "core-hashtable->alist", 1, 1, argc, argv);
    return scm_undef;
}

// string-hash
scm_obj_t
subr_string_hash(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (STRINGP(argv[0])) {
            scm_string_t string = (scm_string_t)argv[0];
            return MAKEFIXNUM(string_hash(string->name, HASH_BOUND_MAX));
        }
        wrong_type_argument_violation(vm, "string-hash", 0, "string", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "string-hash", 1, 1, argc, argv);
    return scm_undef;
}

// symbol-hash
scm_obj_t
subr_symbol_hash(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        if (SYMBOLP(argv[0])) {
            scm_symbol_t symbol = (scm_symbol_t)argv[0];
            return MAKEFIXNUM(string_hash(symbol->name, HASH_BOUND_MAX));
        }
        wrong_type_argument_violation(vm, "symbol-hash", 0, "symbol", argv[0], argc, argv);
        return scm_undef;
    }
    wrong_number_of_arguments_violation(vm, "symbol-hash", 1, 1, argc, argv);
    return scm_undef;
}

// equal-hash
scm_obj_t
subr_equal_hash(VM* vm, int argc, scm_obj_t argv[])
{
    if (argc == 1) {
        return MAKEFIXNUM(relocation_safe_equal_hash(argv[0], HASH_BOUND_MAX));
    }
    wrong_number_of_arguments_violation(vm, "equal-hash", 1, 1, argc, argv);
    return scm_undef;
}

void
init_subr_hash(object_heap_t* heap)
{
    #define DEFSUBR(SYM, FUNC)  heap->intern_system_subr(SYM, FUNC)

    DEFSUBR("make-core-hashtable", subr_make_core_hashtable);
    DEFSUBR("core-hashtable?", subr_core_hashtable_pred);
    DEFSUBR("core-hashtable-ref", subr_core_hashtable_ref);
    DEFSUBR("core-hashtable-set!", subr_core_hashtable_set);
    DEFSUBR("core-hashtable-delete!", subr_core_hashtable_delete);
    DEFSUBR("core-hashtable-clear!", subr_core_hashtable_clear);
    DEFSUBR("core-hashtable-size", subr_core_hashtable_size);
    DEFSUBR("core-hashtable-contains?", subr_core_hashtable_contains_pred);
    DEFSUBR("core-hashtable-copy", subr_core_hashtable_copy);
    DEFSUBR("core-hashtable-mutable?", subr_core_hashtable_mutable_pred);
    DEFSUBR("core-hashtable->alist", subr_core_hashtable_alist);
    DEFSUBR("core-hashtable-equivalence-function", subr_core_hashtable_equivalence_function);
    DEFSUBR("core-hashtable-hash-function", subr_core_hashtable_hash_function);
    DEFSUBR("string-hash", subr_string_hash);
    DEFSUBR("symbol-hash", subr_symbol_hash);
    DEFSUBR("equal-hash", subr_equal_hash);
    DEFSUBR("make-weak-core-hashtable", subr_make_weak_core_hashtable);
    DEFSUBR("weak-core-hashtable?", subr_weak_core_hashtable_pred);

}
