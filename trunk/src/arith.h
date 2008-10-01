/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#ifndef ARITH_H_INCLUDED
#define ARITH_H_INCLUDED

#include "core.h"
#include "object.h"

struct exact_integer_sqrt_ans_t {
    scm_obj_t   s;
    scm_obj_t   r;
};

struct div_and_mod_ans_t {
    scm_obj_t   div;
    scm_obj_t   mod;
};

bool number_pred(scm_obj_t obj);

bool integer_pred(scm_obj_t obj);
bool rational_pred(scm_obj_t obj);
bool real_pred(scm_obj_t obj);
bool integer_valued_pred(scm_obj_t obj);
bool rational_valued_pred(scm_obj_t obj);
bool real_valued_pred(scm_obj_t obj);

bool exact_integer_pred(scm_obj_t obj);
bool exact_non_negative_integer_pred(scm_obj_t obj);
bool exact_positive_integer_pred(scm_obj_t obj);

bool n_zero_pred(scm_obj_t obj);
bool n_negative_pred(scm_obj_t obj);
bool n_positive_pred(scm_obj_t obj);
bool n_finite_pred(scm_obj_t obj);
bool n_even_pred(scm_obj_t obj);
bool n_exact_pred(scm_obj_t obj);
bool n_equal_pred(object_heap_t* heap, scm_obj_t lhs, scm_obj_t rhs);
bool n_exact_equal_pred(scm_obj_t lhs, scm_obj_t rhs);
bool n_inexact_equal_pred(scm_obj_t lhs, scm_obj_t rhs);
int  n_compare(object_heap_t* heap, scm_obj_t lhs, scm_obj_t rhs);

uint32_t  n_hash(scm_obj_t obj, uint32_t bound);
scm_obj_t int32_to_bignum(object_heap_t* heap, int32_t value);
scm_obj_t int64_to_bignum(object_heap_t* heap, int64_t value);
scm_obj_t uint64_to_integer(object_heap_t* heap, uint64_t value);
scm_obj_t uint32_to_integer(object_heap_t* heap, uint32_t value);

inline scm_obj_t
int64_to_integer(object_heap_t* heap, int64_t value)
{
    if ((value <= FIXNUM_MAX) & (value >= FIXNUM_MIN)) return MAKEFIXNUM((int32_t)value);
    return int64_to_bignum(heap, value);
}

inline scm_obj_t
int32_to_integer(object_heap_t* heap, int32_t value)
{
#if ARCH_LP64
    MAKEFIXNUM(value);
#else
    if ((value <= FIXNUM_MAX) & (value >= FIXNUM_MIN)) return MAKEFIXNUM(value);
    return int32_to_bignum(heap, value);
#endif
}

inline scm_obj_t
uint32_to_integer(object_heap_t* heap, uint32_t value)
{
#if ARCH_LP64
    return MAKEFIXNUM(value);
#else
    if (value <= FIXNUM_MAX) return MAKEFIXNUM(value);
    return uint32_to_bignum(heap, value);
#endif
}

inline scm_obj_t
int_to_integer(object_heap_t* heap, int value)
{
    if (sizeof(int) == sizeof(int32_t)) return int32_to_integer(heap, value);
    return int64_to_integer(heap, value);
}

inline scm_obj_t
uint_to_integer(object_heap_t* heap, unsigned int value)
{
    if (sizeof(unsigned int) == sizeof(uint32_t)) return uint32_to_integer(heap, value);
    return uint64_to_integer(heap, value);
}

inline scm_obj_t
intptr_to_integer(object_heap_t* heap, intptr_t value)
{
    if (sizeof(intptr_t) == sizeof(int32_t)) return int32_to_integer(heap, value);
    return int64_to_integer(heap, value);
}

inline scm_obj_t
uintptr_to_integer(object_heap_t* heap, intptr_t value)
{
    if (sizeof(uintptr_t) == sizeof(uint32_t)) return uint32_to_integer(heap, value);
    return uint64_to_integer(heap, value);
}

bool exact_integer_to_int16(scm_obj_t obj, int16_t* ans);
bool exact_integer_to_uint16(scm_obj_t obj, uint16_t* ans);
bool exact_integer_to_int32(scm_obj_t obj, int32_t* ans);
bool exact_integer_to_uint32(scm_obj_t obj, uint32_t* ans);
bool exact_integer_to_int64(scm_obj_t obj, int64_t* ans);
bool exact_integer_to_uint64(scm_obj_t obj, uint64_t* ans);

inline bool
exact_integer_to_uintptr(scm_obj_t obj, uintptr_t* ans)
{
    if (sizeof(uintptr_t) == sizeof(uint32_t)) {
        return exact_integer_to_uint32(obj, (uint32_t*)ans);
    }
    if (sizeof(uintptr_t) == sizeof(uint64_t)) {
        return exact_integer_to_uint64(obj, (uint64_t*)ans);
    }
    assert(false);
    return false;
}

inline bool
exact_integer_to_intptr(scm_obj_t obj, intptr_t* ans)
{
    if (sizeof(intptr_t) == sizeof(int32_t)) {
        return exact_integer_to_int32(obj, (int32_t*)ans);
    }
    if (sizeof(intptr_t) == sizeof(int64_t)) {
        return exact_integer_to_int64(obj, (int64_t*)ans);
    }
    assert(false);
    return false;
}

inline bool
exact_integer_to_int(scm_obj_t obj, int* ans)
{
    if (sizeof(int) == sizeof(int32_t)) {
        return exact_integer_to_int32(obj, (int32_t*)ans);
    }
    if (sizeof(int) == sizeof(int64_t)) {
        return exact_integer_to_int64(obj, (int64_t*)ans);
    }
    assert(false);
    return false;
}

scm_obj_t double_to_inexact(object_heap_t* heap, double value);
double real_to_double(scm_obj_t obj);

bool bignum_to_int32(scm_bignum_t bn, int32_t* ans);
bool bignum_to_uint32(scm_bignum_t bn, uint32_t* ans);
bool bignum_to_int64(scm_bignum_t bn, int64_t* ans);
bool bignum_to_uint64(scm_bignum_t bn, uint64_t* ans);

scm_string_t cnvt_bignum_to_string(object_heap_t* heap, scm_bignum_t bn, int radix);
scm_string_t cnvt_fixnum_to_string(object_heap_t* heap, scm_fixnum_t bn, int radix);
scm_string_t cnvt_flonum_to_string(object_heap_t* heap, scm_flonum_t bn);
scm_obj_t cnvt_number_to_string(object_heap_t* heap, scm_obj_t obj, int radix);
scm_obj_t cnvt_to_exact(object_heap_t* heap, scm_obj_t obj);
scm_obj_t cnvt_to_inexact(object_heap_t* heap, scm_obj_t obj);

scm_obj_t arith_negate(object_heap_t* heap, scm_obj_t obj);
scm_obj_t arith_inverse(object_heap_t* heap, scm_obj_t obj);
scm_obj_t arith_add(object_heap_t* heap, scm_obj_t lhs, scm_obj_t rhs);
scm_obj_t arith_sub(object_heap_t* heap, scm_obj_t lhs, scm_obj_t rhs);
scm_obj_t arith_mul(object_heap_t* heap, scm_obj_t lhs, scm_obj_t rhs);
scm_obj_t arith_div(object_heap_t* heap, scm_obj_t lhs, scm_obj_t rhs);
scm_obj_t arith_quotient(object_heap_t* heap, scm_obj_t lhs, scm_obj_t rhs);
scm_obj_t arith_remainder(object_heap_t* heap, scm_obj_t lhs, scm_obj_t rhs);
scm_obj_t arith_modulo(object_heap_t* heap, scm_obj_t lhs, scm_obj_t rhs);
scm_obj_t arith_expt(object_heap_t* heap, scm_obj_t lhs, scm_obj_t rhs);
scm_obj_t arith_exp(object_heap_t* heap, scm_obj_t obj);
scm_obj_t arith_log(object_heap_t* heap, scm_obj_t obj);
scm_obj_t arith_sin(object_heap_t* heap, scm_obj_t obj);
scm_obj_t arith_cos(object_heap_t* heap, scm_obj_t obj);
scm_obj_t arith_tan(object_heap_t* heap, scm_obj_t obj);
scm_obj_t arith_sqrt(object_heap_t* heap, scm_obj_t obj);
scm_obj_t arith_asin(object_heap_t* heap, scm_obj_t obj);
scm_obj_t arith_acos(object_heap_t* heap, scm_obj_t obj);
scm_obj_t arith_atan(object_heap_t* heap, scm_obj_t obj);
scm_obj_t arith_atan2(object_heap_t* heap, scm_obj_t lhs, scm_obj_t rhs);
scm_obj_t arith_magnitude(object_heap_t* heap, scm_obj_t obj);
scm_obj_t arith_angle(object_heap_t* heap, scm_obj_t obj);
scm_obj_t arith_rectangular(object_heap_t* heap, scm_obj_t lhs, scm_obj_t rhs);
scm_obj_t arith_polar(object_heap_t* heap, scm_obj_t lhs, scm_obj_t rhs);
scm_obj_t arith_floor(object_heap_t* heap, scm_obj_t obj);
scm_obj_t arith_integer_div(object_heap_t* heap, scm_obj_t lhs, scm_obj_t rhs);
scm_obj_t arith_integer_div0(object_heap_t* heap, scm_obj_t lhs, scm_obj_t rhs);
scm_obj_t arith_lognot(object_heap_t* heap, scm_obj_t obj);
scm_obj_t arith_logand(object_heap_t* heap, scm_obj_t lhs, scm_obj_t rhs);
scm_obj_t arith_logior(object_heap_t* heap, scm_obj_t lhs, scm_obj_t rhs);
scm_obj_t arith_logxor(object_heap_t* heap, scm_obj_t lhs, scm_obj_t rhs);
scm_obj_t arith_logash(object_heap_t* heap, scm_obj_t lhs, scm_obj_t rhs);
scm_obj_t arith_first_bit_set(object_heap_t* heap, scm_obj_t obj);
scm_obj_t arith_bit_length(object_heap_t* heap, scm_obj_t obj);
scm_obj_t arith_bit_count(object_heap_t* heap, scm_obj_t obj);
exact_integer_sqrt_ans_t arith_exact_integer_sqrt(object_heap_t* heap, scm_obj_t obj);

scm_obj_t parse_number(object_heap_t* heap, const char* s, int prefix, int radix);
scm_obj_t decode_flonum(object_heap_t* heap, scm_flonum_t n);

inline void
bn_set_zero(scm_bignum_t bn)
{
    bn->hdr = scm_hdr_bignum;
}

inline void
bn_set_count(scm_bignum_t bn, int count)
{
    assert(count >= 0);
    bn->hdr = scm_hdr_bignum | (count << HDR_BIGNUM_COUNT_SHIFT) | (bn->hdr & (0x3 << HDR_BIGNUM_SIGN_SHIFT));
}

inline int
bn_get_count(scm_bignum_t bn)
{
    return HDR_BIGNUM_COUNT(bn->hdr);
}

inline void
bn_set_sign(scm_bignum_t bn, int sign)
{
    assert(sign == 0 || sign == -1 || sign == 1);
    bn->hdr = scm_hdr_bignum | (bn->hdr & (-1 << HDR_BIGNUM_COUNT_SHIFT)) | ((sign & 0x3) << HDR_BIGNUM_SIGN_SHIFT);
}

inline int
bn_get_sign(scm_bignum_t bn)
{
    int bits = HDR_BIGNUM_SIGN(bn->hdr);
    if (bits == 0) return 0;
    return (1 - bits) | 1;
}

#endif
