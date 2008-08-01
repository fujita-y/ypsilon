/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#include "core.h"
#include "hash.h"
#include "utf8.h"
#include "arith.h"
#include "equiv.h"

#define EQUAL_HASH_DEPTH_LIMIT  100

uint32_t
address_hash(void *adrs, uint32_t bound)
{
    return (((uint32_t)adrs >> 3) * 2654435761U + ((uint32_t)adrs & 7)) % bound;
}

uint32_t
address_hash2(void *adrs, uint32_t bound)
{
    uint32_t hash = (((uint32_t)adrs >> 3) * 13845163U) % bound;
    return hash + (hash == 0);
}

uint32_t
string_hash(const char *str, uint32_t bound)
{
    int hash = 0;
    while (*str) hash = hash * 32 - hash + (*str++); // hash * 31 + c
    return hash % bound;
}

uint32_t
string_hash2(const char *str, uint32_t bound)
{
    int hash = 0;
    while (*str) hash = hash * 4 + hash + (*str++); // hash * 5 + c
    hash = hash % bound;
    return hash + (hash == 0);
}

uint32_t
eqv_hash(scm_obj_t obj, uint32_t bound)
{
    if (number_pred(obj)) return n_hash(obj, bound);
    return address_hash(obj, bound);
}

uint32_t
string_hash(scm_obj_t obj, uint32_t bound)
{
    if (STRINGP(obj)) {
        scm_string_t string = (scm_string_t)obj;
        int size = HDR_STRING_SIZE(string->hdr);
        const char* datum = string->name;
        int hash = size;
        for (int i = 0; i < size; i++) hash = hash * 32 - hash + datum[i]; // hash * 31 + c
        return hash % bound;
    }
    return 1;
}

static uint32_t
equal_hash(scm_obj_t obj, uint32_t bound, int depth)
{
    if (depth > EQUAL_HASH_DEPTH_LIMIT) return 1;
    if (STRINGP(obj)) return string_hash(obj, bound);
    if (PAIRP(obj)) {
        uint32_t hash1 = equal_hash(CAR(obj), bound, depth + 1);
        uint32_t hash2 = equal_hash(CDR(obj), bound, depth + 1);
        return (hash1 + hash2 * 64 - hash2) % bound;
    }
    if (VECTORP(obj)) {
        scm_vector_t vector = (scm_vector_t)obj;
        int n = vector->count;
        scm_obj_t* elts = vector->elts;
        uint32_t hash = 1;
        for (int i = 0; i < n; i++) {
            hash = hash * 32 - hash + equal_hash(elts[i], bound, depth + 1);
        }
        return hash % bound;
    }
    return eqv_hash(obj, bound);
}

uint32_t
equal_hash(scm_obj_t obj, uint32_t bound)
{
    return equal_hash(obj, bound, 0);
}

static uint32_t
equal_hash2(scm_obj_t obj, uint32_t bound, int depth)
{
    if (depth > EQUAL_HASH_DEPTH_LIMIT) return 1;
    if (PAIRP(obj)) {
        uint32_t hash1 = equal_hash2(CAR(obj), bound, depth + 1);
        uint32_t hash2 = equal_hash2(CDR(obj), bound, depth + 1);
        return (hash1 + hash2 * 64 - hash2) % bound;
    }
    if (VECTORP(obj)) {
        scm_vector_t vector = (scm_vector_t)obj;
        int n = vector->count;
        scm_obj_t* elts = vector->elts;
        uint32_t hash = 1;
        for (int i = 0; i < n; i++) {
            hash = hash * 32 - hash + equal_hash2(elts[i], bound, depth + 1);
        }
        return hash % bound;
    }
    if (SYMBOLP(obj)) {
        return string_hash(((scm_symbol_t)obj)->name, bound);
    }
    if (STRINGP(obj)) {
        return (string_hash(obj, bound) * 3) % bound;
    }
    if (number_pred(obj)) return n_hash(obj, bound);
    if (CELLP(obj)) return HDR_TC(HDR(obj)) % bound;
    return 1;
}

uint32_t
relocation_safe_equal_hash(scm_obj_t obj, uint32_t bound)
{
    return equal_hash2(obj, bound, 0);
}

int
lookup_mutable_hashtable_size(int n)
{
    static const int primes[] = {7, 17, 31, 61, 127, 251, 509, 1021, 2039,
                                4093, 8191, 16381, 32749, 65521,
                                131071, 262147, 524309 };

    for (int i = 0; i < array_sizeof(primes); i++) {
        if (primes[i] > n) return primes[i];
    }
    assert(false);
    return primes[array_sizeof(primes) - 1];
}



int
lookup_immutable_hashtable_size(int n)
{
    static const int primes[] = {7, 17, 23, 31, 43, 53, 61, 73, 89, 97, 103, 113, 127, 139, 149, 157, 167, 173, 181, 199, 223, 233, 251,
                                 271, 293, 311, 331, 353, 379, 401, 439, 461, 487, 509, 523, 547, 563, 587, 601, 631, 659, 719, 743,
                                 761, 797, 823, 857, 883, 887, 919, 953, 983, 991, 1021, 1069, 1151, 1201, 1259, 1307, 1367, 1423, 1481, 1531,
                                 1559, 1601, 1657, 1709, 1753, 1801, 1907, 1951, 1999, 2039, 2153, 2251, 2351, 2357, 2441, 2551, 2657, 2753,
                                 2851, 2953, 3067, 3511, 4093, 4549, 5081, 5569, 6143, 6553, 7057, 7547, 8191, 9067, 10391, 11093, 12457,
                                 14621, 16381, 18217, 20233, 22291, 24533, 26731, 28687, 30313, 32749, 37517, 41201, 45293, 49031, 57709,
                                 65521, 82237, 99719, 104729, 131071, 262147, 524309 };

    for (int i = 0; i < array_sizeof(primes); i++) {
        if (primes[i] > n) return primes[i];
    }
    assert(false);
    return primes[array_sizeof(primes) - 1];
}

static int
put_eq_hashtable(scm_hashtable_t ht, scm_obj_t key, scm_obj_t value)
{
    ht->lock.verify_locked();
    hashtable_rec_t* ht_datum = ht->datum;
    int nsize = ht_datum->capacity;
    int hash = address_hash(key, nsize);
    int hash2 = address_hash2(key, nsize);
    assert(hash2);
    int index = hash;
    do {
        scm_obj_t entry = ht_datum->elts[index];
        if (entry == scm_hash_free) {
            ht_datum->live++;
            ht_datum->used++;
            goto found;
        }
        if (entry == scm_hash_deleted) {
            ht_datum->live++;
            goto found;
        }
        if (entry == key) goto found;
        index += hash2;
        if (index >= nsize) index -= nsize;
    } while (index != hash);
    fatal("%s:%u hash table full.", __FILE__, __LINE__);
found:

    ht_datum->elts[index] = key;
    ht_datum->elts[index + nsize] = value;
    if (ht_datum->used < HASH_BUSY_THRESHOLD(nsize)) return 0;
    if (ht_datum->live < HASH_DENSE_THRESHOLD(nsize)) return nsize;
    return lookup_mutable_hashtable_size(nsize);
}

static scm_obj_t
get_eq_hashtable(scm_hashtable_t ht, scm_obj_t key)
{
    ht->lock.verify_locked();
    hashtable_rec_t* ht_datum = ht->datum;
    int nsize = ht_datum->capacity;
    int hash = address_hash(key, nsize);
    int hash2 = address_hash2(key, nsize);
    assert(hash2);
    int index = hash;
    do {
        scm_obj_t entry = ht_datum->elts[index];
        if (entry == key) return ht_datum->elts[index + nsize];
        if (entry == scm_hash_free) return scm_undef;
        index += hash2;
        if (index >= nsize) index -= nsize;
    } while (index != hash);
    fatal("%s:%u hash table full.", __FILE__, __LINE__);
}

static int
remove_eq_hashtable(scm_hashtable_t ht, scm_obj_t key)
{
    ht->lock.verify_locked();
    hashtable_rec_t* ht_datum = ht->datum;
    int nsize = ht_datum->capacity;
    int hash = address_hash(key, nsize);
    int hash2 = address_hash2(key, nsize);
    assert(hash2);
    int index = hash;
    do {
        scm_obj_t entry = ht_datum->elts[index];
        if (entry == key) {
            ht_datum->elts[index] = scm_hash_deleted;
            ht_datum->elts[index + nsize] = scm_unspecified;
            ht_datum->live--;
            return ht_datum->live < HASH_SPARSE_THRESHOLD(nsize) ?
                        lookup_mutable_hashtable_size(HASH_MUTABLE_SIZE(ht_datum->live)) : 0; // set size 175% of live
        }
        if (entry == scm_hash_free) return 0;
        index += hash2;
        if (index >= nsize) index -= nsize;
    } while (index != hash);
    fatal("%s:%u hash table full.", __FILE__, __LINE__);
}

scm_obj_t
lookup_weakhashtable(scm_weakhashtable_t ht, scm_obj_t key)
{
    ht->lock.verify_locked();
    weakhashtable_rec_t* ht_datum = ht->datum;
    assert(ht_datum);
    int nsize = ht_datum->capacity;
    int hash = address_hash(key, nsize);
    int hash2 = address_hash2(key, nsize);
    assert(hash2);
    int index = hash;
    do {
        scm_obj_t entry = ht_datum->elts[index];
        if (entry == scm_hash_free) return scm_undef;
        if (entry != scm_hash_deleted) {
            assert(WEAKMAPPINGP(entry));
            scm_weakmapping_t wmap = (scm_weakmapping_t)entry;
            if (wmap->key == scm_false) {
                ht_datum->elts[index] = scm_hash_deleted;
                ht_datum->live--;
            } else {
                if (wmap->key == key) return wmap;
            }
        }
        index += hash2;
        if (index >= nsize) index -= nsize;
    } while (index != hash);
    fatal("%s:%u hash table full.", __FILE__, __LINE__);
}

int
remove_weakhashtable(scm_weakhashtable_t ht, scm_obj_t key)
{
    ht->lock.verify_locked();
    weakhashtable_rec_t* ht_datum = ht->datum;
    assert(ht_datum);
    int nsize = ht_datum->capacity;
    int hash = address_hash(key, nsize);
    int hash2 = address_hash2(key, nsize);
    assert(hash2);
    int index = hash;
    do {
        scm_obj_t entry = ht_datum->elts[index];
        if (entry == scm_hash_free) return 0;
        if (entry != scm_hash_deleted) {
            assert(WEAKMAPPINGP(entry));
            scm_weakmapping_t wmap = (scm_weakmapping_t)entry;
            if (wmap->key == scm_false) {
                ht_datum->elts[index] = scm_hash_deleted;
                ht_datum->live--;
            } else if (wmap->key == key) {
                ht_datum->elts[index] = scm_hash_deleted;
                ht_datum->live--;
                return ht_datum->live < HASH_SPARSE_THRESHOLD(nsize) ?
                            lookup_mutable_hashtable_size(HASH_MUTABLE_SIZE(ht_datum->live)) : 0; // set size 175% of live
            }
        }
        index += hash2;
        if (index >= nsize) index -= nsize;
    } while (index != hash);
    fatal("%s:%u hash table full.", __FILE__, __LINE__);
}

int
put_weakhashtable(scm_weakhashtable_t ht, scm_weakmapping_t wmap)
{
    ht->lock.verify_locked();
    scm_obj_t key = wmap->key;
    weakhashtable_rec_t* ht_datum = ht->datum;
    assert(ht_datum);
    int nsize = ht_datum->capacity;
    int hash = address_hash(key, nsize);
    int hash2 = address_hash2(key, nsize);
    assert(hash2);
    int index = hash;
    do {
        scm_obj_t entry = ht_datum->elts[index];
        if (entry == scm_hash_free) {
            ht_datum->live++;
            ht_datum->used++;
            goto found;
        }
        if (entry == scm_hash_deleted) {
            ht_datum->live++;
            goto found;
        }
        assert(WEAKMAPPINGP(entry));
        assert(((scm_weakmapping_t)entry)->key != key); // use lookup_weak_hash_table() to avoid
        index += hash2;
        if (index >= nsize) index -= nsize;
    } while (index != hash);
    fatal("%s:%u hash table full.", __FILE__, __LINE__);
found:
    ht_datum->elts[index] = wmap;
    if (ht_datum->used < HASH_BUSY_THRESHOLD(nsize)) return 0;
    if (ht_datum->live < HASH_SPARSE_THRESHOLD(nsize)) return lookup_mutable_hashtable_size(HASH_MUTABLE_SIZE(ht_datum->live)); // 175% of live
    if (ht_datum->live < HASH_DENSE_THRESHOLD(nsize)) return nsize;
    return lookup_mutable_hashtable_size(nsize);
}

int
put_hashtable(scm_hashtable_t ht, scm_obj_t key, scm_obj_t value)
{
    if (ht->type == SCM_HASHTABLE_TYPE_EQ) return put_eq_hashtable(ht, key, value);
    ht->lock.verify_locked();
    hashtable_rec_t* ht_datum = ht->datum;
    assert(ht_datum);
    int nsize = ht_datum->capacity;
    int hash = (*ht->hash)(key, nsize);
    int hash2 = 8 - (hash & 7);
    if (hash2 >= nsize) hash2 = 1;
    assert(hash2);
    int index = hash;
    do {
        scm_obj_t entry = ht_datum->elts[index];
        if (entry == scm_hash_free) {
            ht_datum->live++;
            ht_datum->used++;
            goto found;
        }
        if (entry == scm_hash_deleted) {
            ht_datum->live++;
            goto found;
        }
        if (entry == key || (*ht->equiv)(entry, key)) {
            goto found;
        }
        index += hash2;
        if (index >= nsize) index -= nsize;
    } while (index != hash);
    fatal("%s:%u hash table full.", __FILE__, __LINE__);
found:
    ht_datum->elts[index] = key;
    ht_datum->elts[index + nsize] = value;
    if (ht_datum->used < HASH_BUSY_THRESHOLD(nsize)) return 0;
    if (ht_datum->live < HASH_DENSE_THRESHOLD(nsize)) return nsize;
    return lookup_mutable_hashtable_size(nsize);
}

scm_obj_t
get_hashtable(scm_hashtable_t ht, scm_obj_t key)
{
    if (ht->type == SCM_HASHTABLE_TYPE_EQ) return get_eq_hashtable(ht, key);
    ht->lock.verify_locked();
    hashtable_rec_t* ht_datum = ht->datum;
    assert(ht_datum);
    int nsize = ht_datum->capacity;
    int hash = (*ht->hash)(key, nsize);
    int hash2 = 8 - (hash & 7);
    if (hash2 >= nsize) hash2 = 1;
    assert(hash2);
    int index = hash;
    do {
        scm_obj_t entry = ht_datum->elts[index];
        if (entry == key || (*ht->equiv)(entry, key)) return ht_datum->elts[index + nsize];
        if (entry == scm_hash_free) return scm_undef;
        index += hash2;
        if (index >= nsize) index -= nsize;
    } while (index != hash);
    fatal("%s:%u hash table full.", __FILE__, __LINE__);
}

int
remove_hashtable(scm_hashtable_t ht, scm_obj_t key)
{
    if (ht->type == SCM_HASHTABLE_TYPE_EQ) return remove_eq_hashtable(ht, key);
    ht->lock.verify_locked();
    hashtable_rec_t* ht_datum = ht->datum;
    assert(ht_datum);
    int nsize = ht_datum->capacity;
    int hash = (*ht->hash)(key, nsize);
    int hash2 = 8 - (hash & 7);
    if (hash2 >= nsize) hash2 = 1;
    assert(hash2);
    int index = hash;
    do {
        scm_obj_t entry = ht_datum->elts[index];
        if (entry == key || (*ht->equiv)(entry, key)) {
            ht_datum->elts[index] = scm_hash_deleted;
            ht_datum->elts[index + nsize] = scm_unspecified;
            ht_datum->live--;
            return ht_datum->live < HASH_SPARSE_THRESHOLD(nsize) ?
                        lookup_mutable_hashtable_size(HASH_MUTABLE_SIZE(ht_datum->live)) : 0; // set size 175% of live
        }
        if (entry == scm_hash_free) return 0;
        index += hash2;
        if (index >= nsize) index -= nsize;
    } while (index != hash);
    fatal("%s:%u hash table full.", __FILE__, __LINE__);
}

bool
eqv_hash_equiv(scm_obj_t obj1, scm_obj_t obj2)
{
    return eqv_pred(obj1, obj2);
}

bool
equal_hash_equiv(scm_obj_t obj1, scm_obj_t obj2)
{
    return r5rs_equal_pred(obj1, obj2);
}

bool
string_hash_equiv(scm_obj_t obj1, scm_obj_t obj2)
{
    return string_eq_pred(obj1, obj2);
}
