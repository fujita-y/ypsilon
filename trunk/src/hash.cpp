/*
    Ypsilon Scheme System
    Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#include "core.h"
#include "hash.h"
#include "utf8.h"
#include "arith.h"
#include "equiv.h"

#define EQUAL_HASH_DEPTH_LIMIT  100

uint32_t
address_hash1(void *adrs, uint32_t bound)
{
    return (((uintptr_t)adrs >> 3) * 2654435761U + ((uintptr_t)adrs & 7)) % bound;
}

uint32_t
address_hash2(void *adrs, uint32_t bound)
{
    uint32_t hash = (((uintptr_t)adrs >> 3) * 13845163U) % bound;
    return hash + (hash == 0);
}

uint32_t
string_hash1(const char *str, uint32_t bound)
{
    int hash = 107;
    while (*str) hash = hash * 32 - hash + (*str++);
    return hash % bound;
}

uint32_t
string_hash2(const char *str, uint32_t bound)
{
    int hash = 131;
    while (*str) hash = hash * 4 + hash + (*str++);
    hash = hash % bound;
    return hash + (hash == 0);
}

static uint32_t
eqv_hash1(scm_obj_t obj, uint32_t bound)
{
    if (CELLP(obj)) {
        if (FLONUMP(obj)) {
            scm_flonum_t flonum = (scm_flonum_t)obj;
            assert(sizeof(flonum->value) == 8);
            uint32_t* datum = (uint32_t*)(&flonum->value);
            return (datum[0] + datum[1] * 5) % bound;
        }
        if (BIGNUMP(obj)) {
            scm_bignum_t bignum = (scm_bignum_t)obj;
            int count = bn_get_count(bignum);
            uint32_t hash = bn_get_sign(bignum) + count * 5;
            if (sizeof(digit_t) == sizeof(uint32_t)) {
                for (int i = 0; i < count; i++) hash = hash * 5 + bignum->elts[i];
            } else {
                for (int i = 0; i < count; i++) {
                    hash = hash * 5 + (bignum->elts[i] & 0xffffffff) + ((uint64_t)bignum->elts[i] >> 32);
                }
            }
            return hash % bound;
        }
        if (RATIONALP(obj)) {
            scm_rational_t rational = (scm_rational_t)obj;
            uint32_t hash;
            hash = eqv_hash(rational->nume, INT32_MAX) * 5 - eqv_hash(rational->deno, INT32_MAX);
            return hash % bound;
        }
        if (COMPLEXP(obj)) {
            scm_complex_t complex = (scm_complex_t)obj;
            uint32_t hash;
            hash = eqv_hash(complex->real, INT32_MAX) * 5 + eqv_hash(complex->imag, INT32_MAX);
            return hash % bound;
        }
    }
    return address_hash1(obj, bound);
}

static uint32_t
eqv_hash2(scm_obj_t obj, uint32_t bound)
{
    if (CELLP(obj)) {
        if (FLONUMP(obj)) {
            scm_flonum_t flonum = (scm_flonum_t)obj;
            assert(sizeof(flonum->value) == 8);
            uint32_t* datum = (uint32_t*)(&flonum->value);
            uint32_t hash = (datum[0] + datum[1] * 3) % bound;
            return hash + (hash == 0);
        }
        if (BIGNUMP(obj)) {
            scm_bignum_t bignum = (scm_bignum_t)obj;
            int count = bn_get_count(bignum);
            uint32_t hash = bn_get_sign(bignum) + count * 3;
            if (sizeof(digit_t) == sizeof(uint32_t)) {
                for (int i = 0; i < count; i++) hash = hash * 3 + bignum->elts[i];
            } else {
                for (int i = 0; i < count; i++) {
                    hash = hash * 3 + (bignum->elts[i] & 0xffffffff) + ((uint64_t)bignum->elts[i] >> 32);
                }
            }
            hash = hash % bound;
            return hash + (hash == 0);
        }
        if (RATIONALP(obj)) {
            scm_rational_t rational = (scm_rational_t)obj;
            uint32_t hash = eqv_hash2(rational->nume, INT32_MAX) * 3 - eqv_hash2(rational->deno, INT32_MAX);
            hash = hash % bound;
            return hash + (hash == 0);
        }
        if (COMPLEXP(obj)) {
            scm_complex_t complex = (scm_complex_t)obj;
            uint32_t hash = eqv_hash2(complex->real, INT32_MAX) * 3 + eqv_hash2(complex->imag, INT32_MAX);
            hash = hash % bound;
            return hash + (hash == 0);
        }
    }
    return address_hash2(obj, bound);
}

static uint32_t
obj_hash(scm_obj_t obj, int depth)
{
    if (depth > EQUAL_HASH_DEPTH_LIMIT) return 1;
    if (CELLP(obj)) {
        if (PAIRP(obj)) {
            uint32_t hash1 = obj_hash(CAR(obj), depth + 1);
            uint32_t hash2 = obj_hash(CDR(obj), depth + 1);
            return (hash1 + hash2 * 64 - hash2);
        }
        if (VECTORP(obj)) {
            scm_vector_t vector = (scm_vector_t)obj;
            int n = vector->count;
            scm_obj_t* elts = vector->elts;
            uint32_t hash = 1;
            for (int i = 0; i < n; i++) {
                hash = hash * 32 - hash + obj_hash(elts[i], depth + 1);
            }
            return hash;
        }
        if (SYMBOLP(obj)) return symbol_hash((scm_symbol_t)obj, INT32_MAX);
        if (STRINGP(obj)) return string_hash((scm_string_t)obj, INT32_MAX);
        if (number_pred(obj)) return eqv_hash(obj, INT32_MAX);
        return HDR_TC(HDR(obj));
    }
    return address_hash1(obj, INT32_MAX);
}

uint32_t
eqv_hash(scm_obj_t obj, uint32_t bound)
{
    return eqv_hash1(obj, bound);
}

uint32_t
equal_hash(scm_obj_t obj, uint32_t bound)
{
    return obj_hash(obj, 0) % bound;
}

uint32_t
string_hash(scm_obj_t obj, uint32_t bound)
{
    assert(STRINGP(obj));
    scm_string_t string = (scm_string_t)obj;
    return string_hash1(string->name, bound);
}

uint32_t
symbol_hash(scm_obj_t obj, uint32_t bound)
{
    assert(SYMBOLP(obj));
    scm_symbol_t symbol = (scm_symbol_t)obj;
    return string_hash2(symbol->name, bound);
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

int
lookup_mutable_hashtable_size(int n)
{
    static const int primes[] = {
            7, 13, 29, 59, 113, 223, 431, 821, 1567, 2999, 5701, 10837, 20593, 39133, 74353,
            141277, 268439, 510047, 969097, 1841291, 3498457, 5247701, 7871573, 11807381, 17711087, 26566649, 39849977,
            59774983, 89662483, 134493731, 201740597, 302610937, 453916423, 680874641, 1021311983, 1531968019,
            2147483647 };
    for (int i = 0; i < array_sizeof(primes); i++) {
        if (primes[i] > n) return primes[i];
    }
    fatal("%s:%u internal error: hashtable too big",__FILE__ , __LINE__);
}

int
lookup_immutable_hashtable_size(int n)
{
    static const int primes[] = {
            7, 11, 13, 17, 23, 29, 37, 47, 59, 71, 89, 107, 131, 157, 191, 229, 277, 337, 409,
            491, 593, 719, 863, 1039, 1249, 1499, 1801, 2161, 2593, 3119, 3761, 4513, 5417, 6521, 7829, 9397, 11279,
            13537, 16249, 19501, 23417, 28109, 33739, 40487, 48589, 58309, 69991, 84011, 100823, 120997, 145207,
            174257, 209123, 250949, 301141, 361373, 433651, 520381, 624467, 749383, 899263, 1079123, 1294957, 1553971,
            1864769, 2237743, 2685301, 3222379, 3866857, 4640231, 5568287, 6681947, 8018347, 9622021, 11546449,
            13855747, 16626941, 19952329, 23942797, 28731359, 34477637, 41373173, 49647809, 59577379, 71492873,
            85791451, 102949741, 123539747, 148247713, 177897311, 213476789, 256172149, 307406587, 368887919,
            442665511, 531198691, 637438433, 764926171, 917911471, 1101493807, 1321792573, 1586151131,
            1903381357, 2147483647 };
    for (int i = 0; i < array_sizeof(primes); i++) {
        if (primes[i] > n) return primes[i];
    }
    fatal("%s:%u internal error: hashtable too big",__FILE__ , __LINE__);
}

static int
put_eq_hashtable(scm_hashtable_t ht, scm_obj_t key, scm_obj_t value)
{
    ht->lock.verify_locked();
    hashtable_rec_t* ht_datum = ht->datum;
    int nsize = ht_datum->capacity;
    int hash1 = address_hash1(key, nsize);
    int hash2 = address_hash2(key, nsize);
    int index = hash1;
    do {
        scm_obj_t tag = ht_datum->elts[index];
        if (tag == scm_hash_free) {
            ht_datum->live++;
            ht_datum->used++;
            goto found;
        }
        if (tag == scm_hash_deleted) {
            ht_datum->live++;
            goto found;
        }
        if (tag == key) goto found;
        index += hash2;
        if (index >= nsize) index -= nsize;
    } while (index != hash1);
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
    int hash1 = address_hash1(key, nsize);
    int hash2 = address_hash2(key, nsize);
    int index = hash1;
    do {
        scm_obj_t tag = ht_datum->elts[index];
        if (tag == key) return ht_datum->elts[index + nsize];
        if (tag == scm_hash_free) return scm_undef;
        index += hash2;
        if (index >= nsize) index -= nsize;
    } while (index != hash1);
    fatal("%s:%u hash table full.", __FILE__, __LINE__);
}

static int
remove_eq_hashtable(scm_hashtable_t ht, scm_obj_t key)
{
    ht->lock.verify_locked();
    hashtable_rec_t* ht_datum = ht->datum;
    int nsize = ht_datum->capacity;
    int hash1 = address_hash1(key, nsize);
    int hash2 = address_hash2(key, nsize);
    int index = hash1;
    do {
        scm_obj_t tag = ht_datum->elts[index];
        if (tag == key) {
            ht_datum->elts[index] = scm_hash_deleted;
            ht_datum->elts[index + nsize] = scm_unspecified;
            ht_datum->live--;
            return ht_datum->live < HASH_SPARSE_THRESHOLD(nsize) ?
                        lookup_mutable_hashtable_size(HASH_MUTABLE_SIZE(ht_datum->live)) : 0; // set size 175% of live
        }
        if (tag == scm_hash_free) return 0;
        index += hash2;
        if (index >= nsize) index -= nsize;
    } while (index != hash1);
    fatal("%s:%u hash table full.", __FILE__, __LINE__);
}

scm_obj_t
lookup_weakhashtable(scm_weakhashtable_t ht, scm_obj_t key)
{
    ht->lock.verify_locked();
    weakhashtable_rec_t* ht_datum = ht->datum;
    assert(ht_datum);
    int nsize = ht_datum->capacity;
    int hash1 = address_hash1(key, nsize);
    int hash2 = address_hash2(key, nsize);
    int index = hash1;
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
    } while (index != hash1);
    fatal("%s:%u hash table full.", __FILE__, __LINE__);
}

int
remove_weakhashtable(scm_weakhashtable_t ht, scm_obj_t key)
{
    ht->lock.verify_locked();
    weakhashtable_rec_t* ht_datum = ht->datum;
    assert(ht_datum);
    int nsize = ht_datum->capacity;
    int hash1 = address_hash1(key, nsize);
    int hash2 = address_hash2(key, nsize);
    int index = hash1;
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
    } while (index != hash1);
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
    int hash1 = address_hash1(key, nsize);
    int hash2 = address_hash2(key, nsize);
    int index = hash1;
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
        assert(((scm_weakmapping_t)entry)->key != key); // use lookup_weakhashtable() before put_weakhashtable()
        index += hash2;
        if (index >= nsize) index -= nsize;
    } while (index != hash1);
    fatal("%s:%u hash table full.", __FILE__, __LINE__);

found:
    ht_datum->elts[index] = wmap;
    if (ht_datum->used < HASH_BUSY_THRESHOLD(nsize)) return 0;
    if (ht_datum->live < HASH_SPARSE_THRESHOLD(nsize)) return lookup_mutable_hashtable_size(HASH_MUTABLE_SIZE(ht_datum->live));
    if (ht_datum->live < HASH_DENSE_THRESHOLD(nsize)) return nsize;
    return lookup_mutable_hashtable_size(nsize);
}

static uint32_t
simple_hash2(uint32_t hash, int nsize)
{
    int dist = nsize >> 6;
    dist = (dist < 8) ? ((nsize > 8) ? 8 : 1) : dist;
    int hash2 = dist - (hash % dist);
    assert(hash2 && hash2 < nsize);
    return hash2;
}

int
put_hashtable(scm_hashtable_t ht, scm_obj_t key, scm_obj_t value)
{
    if (ht->type == SCM_HASHTABLE_TYPE_EQ) return put_eq_hashtable(ht, key, value);
    ht->lock.verify_locked();
    hashtable_rec_t* ht_datum = ht->datum;
    assert(ht_datum);
    int nsize = ht_datum->capacity;
    int hash1 = (*ht->hash)(key, nsize);
    int hash2 = simple_hash2(hash1, nsize);
    int index = hash1;
    do {
        scm_obj_t tag = ht_datum->elts[index];
        if (tag == scm_hash_free) {
            ht_datum->live++;
            ht_datum->used++;
            goto found;
        }
        if (tag == scm_hash_deleted) {
            ht_datum->live++;
            goto found;
        }
        if (tag == key || (*ht->equiv)(tag, key)) goto found;
        index += hash2;
        if (index >= nsize) index -= nsize;
    } while (index != hash1);
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
    int hash1 = (*ht->hash)(key, nsize);
    int hash2 = simple_hash2(hash1, nsize);
    int index = hash1;
    do {
        scm_obj_t tag = ht_datum->elts[index];
        if (tag == key || (*ht->equiv)(tag, key)) return ht_datum->elts[index + nsize];
        if (tag == scm_hash_free) return scm_undef;
        index += hash2;
        if (index >= nsize) index -= nsize;
    } while (index != hash1);
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
    int hash1 = (*ht->hash)(key, nsize);
    int hash2 = simple_hash2(hash1, nsize);
    int index = hash1;
    do {
        scm_obj_t tag = ht_datum->elts[index];
        if (tag == key || (*ht->equiv)(tag, key)) {
            ht_datum->elts[index] = scm_hash_deleted;
            ht_datum->elts[index + nsize] = scm_unspecified;
            ht_datum->live--;
            return ht_datum->live < HASH_SPARSE_THRESHOLD(nsize) ?
                        lookup_mutable_hashtable_size(HASH_MUTABLE_SIZE(ht_datum->live)) : 0;
        }
        if (tag == scm_hash_free) return 0;
        index += hash2;
        if (index >= nsize) index -= nsize;
    } while (index != hash1);
    fatal("%s:%u hash table full.", __FILE__, __LINE__);
}
