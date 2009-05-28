/*
  Ypsilon Scheme System
  Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
  See license.txt for terms and conditions of use
*/

#include "core.h"
#include "arith.h"
#include "object_heap.h"
#include "object_factory.h"
#include "serialize.h"
#include "vm.h"

#define BVO_TAG_NOUSE               0
#define BVO_TAG_LOOKUP              1
#define BVO_TAG_IMMEDIATE           2
#define BVO_TAG_PLIST               3
#define BVO_TAG_DLIST               4
#define BVO_TAG_VECTOR              5
#define BVO_TAG_RATIONAL            6
#define BVO_TAG_COMPLEX             7
#define BVO_TAG_FLONUM              8
#define BVO_TAG_BIGNUM              9
#define BVO_TAG_BVECTOR             10
#define BVO_TAG_SYMBOL              11
#define BVO_TAG_STRING              12
#define BVO_TAG_UNINTERNED_SYMBOL   13
#if USE_CLOSURE_SERIALIZE
#define BVO_TAG_SUBR                14
#define BVO_TAG_GLOC                15
#define BVO_TAG_UNINTERNED_GLOC     16
#define BVO_TAG_CLOSURE             17
#define BVO_TAG_TUPLE               18
#endif
#define MAX_BUNDLE_SIZE             sizeof(uint64_t)

serializer_t::serializer_t(object_heap_t* heap)
{
    m_heap = heap;
    m_lites = make_hashtable(m_heap, SCM_HASHTABLE_TYPE_EQ, lookup_mutable_hashtable_size(0));
    int depth = 32;
    m_stack = (scm_obj_t*)m_heap->allocate_private(sizeof(scm_obj_t) * depth);
    m_stack_limit = m_stack + depth;
    m_sp = m_stack;
    m_buf_size = 128;
    m_buf_mark = 0;
    m_buf = (uint8_t*)m_heap->allocate_private(m_buf_size);
    m_bad = NULL;
}

serializer_t::~serializer_t()
{
    m_heap->deallocate_private(m_stack);
    m_heap->deallocate_private(m_buf);
}

void
serializer_t::emit_u8(uint8_t octet)
{
    m_buf[m_buf_mark] = octet;
    m_buf_mark++;
    if (m_buf_mark + MAX_BUNDLE_SIZE >= m_buf_size) expand();
}

void
serializer_t::emit_u32(uint32_t n)
{
#if ARCH_IA32 || ARCH_X64
    assert(sizeof(uint32_t) <= MAX_BUNDLE_SIZE);
    *(uint32_t*)(m_buf + m_buf_mark) = n;
    m_buf_mark += sizeof(uint32_t);
    if (m_buf_mark + MAX_BUNDLE_SIZE >= m_buf_size) expand();
#else
    emit_u8((n >> 0) & 0xff);
    emit_u8((n >> 8) & 0xff);
    emit_u8((n >> 16) & 0xff);
    emit_u8((n >> 24) & 0xff);
#endif
}

void
serializer_t::emit_u64(uint64_t n)
{
#if ARCH_IA32 || ARCH_X64
    assert(sizeof(uint64_t) <= MAX_BUNDLE_SIZE);
    *(uint64_t*)(m_buf + m_buf_mark) = n;
    m_buf_mark += sizeof(uint64_t);
    if (m_buf_mark + MAX_BUNDLE_SIZE >= m_buf_size) expand();
#else
    emit_u8((n >> 0) & 0xff);
    emit_u8((n >> 8) & 0xff);
    emit_u8((n >> 16) & 0xff);
    emit_u8((n >> 24) & 0xff);
    emit_u8((n >> 32) & 0xff);
    emit_u8((n >> 40) & 0xff);
    emit_u8((n >> 48) & 0xff);
    emit_u8((n >> 56) & 0xff);
#endif
}

void
serializer_t::emit_bytes(const uint8_t* s, int n)
{
    if (m_buf_mark + MAX_BUNDLE_SIZE + n < m_buf_size) {
        memcpy(m_buf + m_buf_mark, s, n);
        m_buf_mark += n;
    } else {
        for (int i = 0; i < n; i++) emit_u8(s[i]);
    }
}

void
serializer_t::expand()
{
    uint8_t* prev = m_buf;
    int n2 = m_buf_size + m_buf_size;
    m_buf = (uint8_t*)m_heap->allocate_private(n2);
    memcpy(m_buf, prev, m_buf_size);
    m_buf_size = n2;
    m_heap->deallocate_private(prev);
}

void
serializer_t::push(scm_obj_t obj)
{
    if (m_sp == m_stack_limit) {
        scm_obj_t* prev = m_stack;
        int n = m_stack_limit - m_stack;
        int n2 = n + n;
        m_stack = (scm_obj_t*)m_heap->allocate_private(sizeof(scm_obj_t) * n2);
        memcpy(m_stack, prev, sizeof(scm_obj_t) * n);
        m_stack_limit = m_stack + n2;
        m_sp = m_stack + n;
        m_heap->deallocate_private(prev);
    }
    m_sp[0] = obj;
    m_sp++;
}

scm_obj_t
serializer_t::pop()
{
    if (m_sp == m_stack) return NULL;
    m_sp--;
    return m_sp[0];
}

void
serializer_t::scan(scm_obj_t obj)
{

loop:
    if (CELLP(obj)) {
        if (PAIRP(obj)) {
            scan(CAR(obj));
            obj = CDR(obj);
            goto loop;
        }
        if (SYMBOLP(obj) || STRINGP(obj)) {
            if (get_hashtable(m_lites, obj) != scm_undef) return;
            int nsize = put_hashtable(m_lites, obj, MAKEFIXNUM(m_lites->datum->live));
            if (nsize) rehash_hashtable(m_heap, m_lites, nsize);
            return;
        }
        if (VECTORP(obj)) {
            scm_vector_t vector = (scm_vector_t)obj;
            int count = vector->count;
            if (count == 0) return;
            scm_obj_t* elts = vector->elts;
            for (int i = 0; i < count; i++) scan(elts[i]);
            return;
        }
#if USE_CLOSURE_SERIALIZE
        if (CLOSUREP(obj)) {
            scm_closure_t closure = (scm_closure_t)obj;
            if (closure->env == NULL) {
                scan(closure->code);
                scan(closure->doc);
                return;
            }
            if (m_bad == NULL) m_bad = obj;
            return;
        }
        if (GLOCP(obj)) {
            scm_gloc_t gloc = (scm_gloc_t)obj;
            scan(gloc->variable);
            if (UNINTERNEDGLOCP(gloc)) scan(gloc->value);
            return;
        }
        if (TUPLEP(obj)) {
            scm_tuple_t tuple = (scm_tuple_t)obj;
            int count = HDR_TUPLE_COUNT(tuple->hdr);
            if (count == 0) return;
            scm_obj_t* elts = tuple->elts;
            const char* type_name = get_tuple_type_name(tuple);
            if (type_name && strcmp(type_name, "record-type-descriptor") == 0) {
                scm_obj_t uid = tuple->elts[3];
                if (!SYMBOLP(uid)) {
                    if (m_bad == NULL) m_bad = obj;
                    return;
                }
            }
            for (int i = 0; i < count; i++) scan(elts[i]);
            return;
        }        
        if (SUBRP(obj)) return;
#endif
        if (BVECTORP(obj) || FLONUMP(obj) || BIGNUMP(obj) || RATIONALP(obj) || COMPLEXP(obj)) return;
        if (m_bad == NULL) m_bad = obj;
    }
}

void
serializer_t::put_lites()
{
    scm_obj_t* lites = (scm_obj_t*)m_heap->allocate_private(sizeof(scm_obj_t) * m_lites->datum->live);
    try {
        hashtable_rec_t* ht_datum = m_lites->datum;
        int nsize = m_lites->datum->capacity;
        for (int i = 0; i < nsize; i++) {
            scm_obj_t key = ht_datum->elts[i];
            scm_obj_t value = ht_datum->elts[i + nsize];
            if (CELLP(key)) {
                assert(FIXNUM(value) < m_lites->datum->live);
                lites[FIXNUM(value)] = key;
            }
        }
        emit_u32(m_lites->datum->live);
        for (int i = 0; i < m_lites->datum->live; i++) {
            if (SYMBOLP(lites[i])) {
                scm_symbol_t symbol = (scm_symbol_t)lites[i];
                if (UNINTERNEDSYMBOLP(symbol)) {
                    emit_u8(BVO_TAG_UNINTERNED_SYMBOL);
                    emit_u32(i);
                    int n = HDR_SYMBOL_SIZE(symbol->hdr) + 2;
                    emit_u32(n);
                    emit_bytes((uint8_t*)symbol->name, n);
                } else {
                    emit_u8(BVO_TAG_SYMBOL);
                    emit_u32(i);
                    int n = HDR_SYMBOL_SIZE(symbol->hdr);
                    emit_u32(n);
                    emit_bytes((uint8_t*)symbol->name, n);
                }
                continue;
            }
            if (STRINGP(lites[i])) {
                scm_string_t string = (scm_string_t)lites[i];
                emit_u8(BVO_TAG_STRING);
                emit_u32(i);
                int n = string->size;
                emit_u32(n);
                emit_bytes((uint8_t*)string->name, n);
                continue;
            }
        }
    } catch (...) {
        m_heap->deallocate_private(lites);
        throw;
    }
    m_heap->deallocate_private(lites);
}

void
serializer_t::put_list(scm_obj_t obj)
{
    int count = 0;
    while (PAIRP(obj)) {
        push(CAR(obj));
        obj = CDR(obj);
        count++;
    }
    if (obj == scm_nil) {
        emit_u8(BVO_TAG_PLIST);
        emit_u32(count);
    } else {
        emit_u8(BVO_TAG_DLIST);
        emit_u32(count);
        put_datum(obj);
    }
    while (count--) {
        obj = pop();
        assert(obj);
        put_datum(obj);
    }
}

void
serializer_t::put_datum(scm_obj_t obj)
{
    if (!CELLP(obj)) {
        emit_u8(BVO_TAG_IMMEDIATE);
#if ARCH_LP64
        emit_u64((uint64_t)obj);
#else
        emit_u32((uint32_t)obj);
#endif
        return;
    }
    if (PAIRP(obj)) {
        put_list(obj);
        return;
    }
    if (SYMBOLP(obj) || STRINGP(obj)) {
        scm_obj_t id = get_hashtable(m_lites, obj);
        emit_u8(BVO_TAG_LOOKUP);
        emit_u32((uint32_t)FIXNUM(id));
        return;
    }
    if (VECTORP(obj)) {
        scm_vector_t vector = (scm_vector_t)obj;
        int count = vector->count;
        emit_u8(BVO_TAG_VECTOR);
        emit_u32(count);
        scm_obj_t* elts = vector->elts;
        for (int i = 0; i < count; i++) put_datum(elts[i]);
        return;
    }
    if (BVECTORP(obj)) {
        scm_bvector_t bv = (scm_bvector_t)obj;
        int count = bv->count;
        emit_u8(BVO_TAG_BVECTOR);
        emit_u32(count);
        emit_bytes(bv->elts, count);
        return;
    }
    if (FLONUMP(obj)) {
        union {
            double      f64;
            uint64_t    u64;
        } n;
        scm_flonum_t flonum = (scm_flonum_t)obj;
        n.f64 = flonum->value;
        emit_u8(BVO_TAG_FLONUM);
        emit_u64(n.u64);
        return;
    }
    if (BIGNUMP(obj)) {
        scm_bignum_t bn = (scm_bignum_t)obj;
        assert(sizeof(bn->elts[0]) == sizeof(uint32_t));
        int sign = bn_get_sign(bn); // 0 or 1 or -1
        int count = bn_get_count(bn);
        emit_u8(BVO_TAG_BIGNUM);
        emit_u32(sign);
        emit_u32(count);
#if USE_DIGIT32
        for (int i = 0; i < count; i++) emit_u32(bn->elts[i]);
#else
        for (int i = 0; i < count; i++) emit_u64(bn->elts[i]);
#endif
        return;
    }
    if (RATIONALP(obj)) {
        scm_rational_t rat = (scm_rational_t)obj;
        emit_u8(BVO_TAG_RATIONAL);
        put_datum(rat->nume);
        put_datum(rat->deno);
        return;
    }
    if (COMPLEXP(obj)) {
        scm_complex_t comp = (scm_complex_t)obj;
        emit_u8(BVO_TAG_COMPLEX);
        put_datum(comp->real);
        put_datum(comp->imag);
        return;
    }
#if USE_CLOSURE_SERIALIZE
    if (CLOSUREP(obj)) {
        scm_closure_t closure = (scm_closure_t)obj;
        emit_u8(BVO_TAG_CLOSURE);
  #if ARCH_LP64
        emit_u64((uint64_t)closure->hdr);
  #else
        emit_u32((uint32_t)closure->hdr);
  #endif
        put_datum(closure->code);
        put_datum(closure->doc);
        return;
    }
    if (GLOCP(obj)) {
        scm_gloc_t gloc = (scm_gloc_t)obj;
        if (UNINTERNEDGLOCP(gloc)) {
            emit_u8(BVO_TAG_UNINTERNED_GLOC);
            put_datum(gloc->variable);
            put_datum(gloc->value);
            return;
        }
        emit_u8(BVO_TAG_GLOC);
        put_datum(gloc->variable);
        return;
    }
    if (SUBRP(obj)) {
        emit_u8(BVO_TAG_SUBR);
  #if ARCH_LP64
        emit_u64((uint64_t)obj);
  #else
        emit_u32((uint32_t)obj);
  #endif
        return;
    }
    if (TUPLEP(obj)) {
        scm_tuple_t tuple = (scm_tuple_t)obj;
        int count = HDR_TUPLE_COUNT(tuple->hdr);
        emit_u8(BVO_TAG_TUPLE);
        emit_u32(count);
        scm_obj_t* elts = tuple->elts;
        for (int i = 0; i < count; i++) put_datum(elts[i]);
        return;
    }            
#endif
    fatal("%s:%u internal error: datum not supported in serialized object", __FILE__, __LINE__);
}

scm_obj_t
serializer_t::translate(scm_obj_t obj)
{
    scoped_lock lock(m_lites->lock);
    scan(obj);
    if (m_bad != NULL) return m_bad;
#if ARCH_LP64
    emit_u8(64);
#else
    emit_u8(32);
#endif
    put_lites();
    put_datum(obj);
    scm_bvector_t bytes = make_bvector(m_heap, m_buf_mark);
    memcpy(bytes->elts, m_buf, m_buf_mark);
    return bytes;
}

deserializer_t::deserializer_t(object_heap_t* heap)
{
    m_heap = heap;
    m_lites = NULL;
}

deserializer_t::~deserializer_t()
{
    if (m_lites) m_heap->deallocate_private(m_lites);
}

uint8_t
deserializer_t::fetch_u8()
{
    return *m_buf++;
}

void
deserializer_t::fetch_bytes(uint8_t* p, int n)
{
    memcpy(p, m_buf, n);
    m_buf += n;
    if (m_buf > m_buf_tail) throw true;
}

uint32_t
deserializer_t::fetch_u32()
{
#if ARCH_IA32 || ARCH_X64
    uint32_t value = *(uint32_t*)m_buf;
    m_buf += sizeof(uint32_t);
    if (m_buf > m_buf_tail) throw true;
    return value;
#else
    uint32_t value = 0;
    value += ((uint32_t)fetch_u8() << 0);
    value += ((uint32_t)fetch_u8() << 8);
    value += ((uint32_t)fetch_u8() << 16);
    value += ((uint32_t)fetch_u8() << 24);
    if (m_buf > m_buf_tail) throw true;
    return value;
#endif
}

uint64_t
deserializer_t::fetch_u64()
{
#if ARCH_IA32 || ARCH_X64
    uint64_t value = *(uint64_t*)m_buf;
    m_buf += sizeof(uint64_t);
    if (m_buf > m_buf_tail) throw true;
    return value;
#else
    uint64_t value = 0;
    value += ((uint64_t)fetch_u8() << 0);
    value += ((uint64_t)fetch_u8() << 8);
    value += ((uint64_t)fetch_u8() << 16);
    value += ((uint64_t)fetch_u8() << 24);
    value += ((uint64_t)fetch_u8() << 32);
    value += ((uint64_t)fetch_u8() << 40);
    value += ((uint64_t)fetch_u8() << 48);
    value += ((uint64_t)fetch_u8() << 56);
    if (m_buf > m_buf_tail) throw true;
    return value;
#endif
}

scm_obj_t
deserializer_t::get_datum()
{
    uint8_t octet = fetch_u8();
    switch (octet) {
        case BVO_TAG_LOOKUP: {
            uint32_t uid = fetch_u32();
            return m_lites[uid];
        }
        case BVO_TAG_IMMEDIATE: {
#if ARCH_LP64
            return (scm_obj_t)fetch_u64();
#else
            return (scm_obj_t)fetch_u32();
#endif
        }
        case BVO_TAG_PLIST: {
            int count = fetch_u32();
            scm_obj_t lst = scm_nil;
            for (int i = 0; i < count; i++) {
                lst = make_pair(m_heap, get_datum(), lst);
            }
            return lst;
        }
        case BVO_TAG_DLIST: {
            int count = fetch_u32();
            scm_obj_t lst = get_datum();
            for (int i = 0; i < count; i++) {
                lst = make_pair(m_heap, get_datum(), lst);
            }
            return lst;
        }
        case BVO_TAG_VECTOR: {
            int count = fetch_u32();
            scm_vector_t vector = make_vector(m_heap, count, scm_unspecified);
            scm_obj_t* elts = vector->elts;
            for (int i = 0; i < count; i++) elts[i] = get_datum();
            return vector;
        }
        case BVO_TAG_RATIONAL: {
            scm_obj_t nume = get_datum();
            scm_obj_t deno = get_datum();
            return make_rational(m_heap, nume, deno);
        }
        case BVO_TAG_COMPLEX: {
            scm_obj_t real = get_datum();
            scm_obj_t imag = get_datum();
            return make_complex(m_heap, real, imag);
        }
        case BVO_TAG_FLONUM: {
            union { double f64; uint64_t u64; } n;
            n.u64 = fetch_u64();
            return make_flonum(m_heap, n.f64);
        }
        case BVO_TAG_BIGNUM: {
            int sign = (int32_t)fetch_u32();
            int count = fetch_u32();
            scm_bignum_t bn = make_bignum(m_heap, count);
            bn_set_sign(bn, sign);
#if USE_DIGIT32
            for (int i = 0; i < count; i++) bn->elts[i] = fetch_u32();
#else
            for (int i = 0; i < count; i++) bn->elts[i] = fetch_u64();
#endif
            return bn;
        }
        case BVO_TAG_BVECTOR: {
            int count = fetch_u32();
            scm_bvector_t bv = make_bvector(m_heap, count);
            fetch_bytes(bv->elts, count);
            return bv;
        }
#if USE_CLOSURE_SERIALIZE
        case BVO_TAG_SUBR: {
  #if ARCH_LP64
            return (scm_subr_t)fetch_u64();
  #else
            return (scm_subr_t)fetch_u32();
  #endif
        }
        case BVO_TAG_GLOC: {
            scm_symbol_t symbol = (scm_symbol_t)get_datum();
            assert(SYMBOLP(symbol));
            {
                VM* vm = current_vm();
                scm_hashtable_t ht = vm->m_current_environment->variable;
                scoped_lock lock(ht->lock);
                scm_obj_t obj = get_hashtable(ht, symbol);
                if (obj != scm_undef) return obj;
                scm_gloc_t gloc = make_gloc(m_heap, symbol);
                gloc->value = scm_undef;
                m_heap->write_barrier(symbol);
                m_heap->write_barrier(gloc);
                int nsize = put_hashtable(ht, symbol, gloc);
                if (nsize) rehash_hashtable(m_heap, ht, nsize);
                return gloc;
            }
        }
        case BVO_TAG_UNINTERNED_GLOC: {
            scm_symbol_t variable = (scm_symbol_t)get_datum();
            scm_obj_t value = get_datum();
            assert(SYMBOLP(variable));
            scm_gloc_t gloc = make_gloc_uninterned(m_heap, (scm_symbol_t)variable);
            gloc->value = value;
            return gloc;
        }
        case BVO_TAG_CLOSURE: {
  #if ARCH_LP64
            scm_hdr_t hdr = fetch_u64();
  #else
            scm_hdr_t hdr = fetch_u32();
  #endif
            scm_obj_t code = get_datum();
            scm_obj_t doc = get_datum();
            return make_closure(m_heap, hdr, NULL, code, doc);
        }
        case BVO_TAG_TUPLE: {
            int count = fetch_u32();
            scm_tuple_t tuple = make_tuple(m_heap, count, scm_unspecified);
            scm_obj_t* elts = tuple->elts;
            for (int i = 0; i < count; i++) elts[i] = get_datum();
            const char* type_name = get_tuple_type_name(tuple);
            if (type_name && strcmp(type_name, "record-type-descriptor") == 0) {
                scm_obj_t uid = tuple->elts[3];
                if (SYMBOLP(uid)) {
                    scm_hashtable_t ht = (scm_hashtable_t)m_heap->lookup_system_environment(make_symbol(m_heap, ".@nongenerative-record-types"));
                    if (ht == scm_undef) fatal("fatal: .@nongenerative-record-types not available in system environment");
                    scm_obj_t obj = get_hashtable(ht, uid);
                    if (obj != scm_undef) return obj;                       
                }
            }
            return tuple;
        }
#endif
    }
    throw true;
}

void
deserializer_t::get_lites()
{
    int buflen = 128;
    char* buf = (char*)m_heap->allocate_private(buflen + 1);
    int count = fetch_u32();
    if (count < 0) throw true;
    m_lites = (scm_obj_t*)m_heap->allocate_private(sizeof(scm_obj_t) * count);
    for (int i = 0; i < count; i++) {
        uint8_t tag = fetch_u8();
        uint32_t uid = fetch_u32();
        uint32_t len = fetch_u32();
        if (uid > count) throw true;
        if (m_buf + len > m_buf_tail) throw true;
        if (len > buflen) {
            m_heap->deallocate_private(buf);
            buf = (char*)m_heap->allocate_private(len + 1);
            buflen = len;
        }
        fetch_bytes((uint8_t*)buf, len);
        buf[len] = 0;
        switch (tag) {
            case BVO_TAG_SYMBOL:
                m_lites[uid] = make_symbol(m_heap, buf, len);
                break;
            case BVO_TAG_UNINTERNED_SYMBOL:
                m_lites[uid] = make_symbol_uninterned(m_heap, buf, len - 2, buf[len - 1]);
                break;
            case BVO_TAG_STRING:
                m_lites[uid] = make_string_literal(m_heap, buf, len);
                break;
            default:
                m_heap->deallocate_private(buf);
                throw true;
        }
    }
    m_heap->deallocate_private(buf);
}

scm_obj_t
deserializer_t::translate(scm_bvector_t obj)
{
    try {
        m_buf = obj->elts;
        m_buf_tail = m_buf + obj->count;
        if (m_buf == m_buf_tail) return NULL;
#if ARCH_LP64
        if (fetch_u8() != 64) return NULL;
#else
        if (fetch_u8() != 32) return NULL;
#endif
        get_lites();
        scm_obj_t obj = get_datum();
        if (m_buf != m_buf_tail) return NULL;
        return obj;
    } catch (...) {
        return NULL;
    }
}
