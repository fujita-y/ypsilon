/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#include "core.h"
#include "hash.h"
#include "heap.h"
#include "list.h"
#include "port.h"
#include "arith.h"

template <typename T> void swap(T& lhs, T& rhs) { T tmp = lhs; lhs = rhs; rhs = tmp; }

scm_symbol_t
make_symbol(object_heap_t* heap, const char *name, int len)
{
    heap->m_symbol.lock();
    scm_symbol_t obj = (scm_symbol_t)heap->m_symbol.get(name, len);
    if (obj == scm_undef) {
        int bytes = sizeof(scm_symbol_rec_t) + len + 1;
        if (bytes <= INTERNAL_PRIVATE_THRESHOLD) {
            obj = (scm_symbol_t)heap->allocate_collectible(bytes);
            obj->hdr = scm_hdr_symbol | (len << HDR_SYMBOL_SIZE_SHIFT) ;
            obj->name = (char*)((uintptr_t)obj + sizeof(scm_symbol_rec_t));
        } else {
            obj = (scm_symbol_t)heap->allocate_collectible(sizeof(scm_symbol_rec_t));
            obj->hdr = scm_hdr_symbol | (len << HDR_SYMBOL_SIZE_SHIFT) ;
            obj->name = (char*)heap->allocate_private(len);
        }
        memcpy(obj->name, name, len);
        obj->name[len] = 0;
        heap->m_symbol.put(obj);
    }
    heap->m_symbol.unlock();
    return obj;
}

scm_symbol_t
make_symbol(object_heap_t* heap, const char *name)
{
    return make_symbol(heap, name, strlen(name));
    /*
    heap->m_symbol.lock();
    int len = strlen(name);
    scm_symbol_t obj = (scm_symbol_t)heap->m_symbol.get(name, len);
    if (obj == scm_undef) {
        int bytes = sizeof(scm_symbol_rec_t) + len + 1;
        if (bytes <= INTERNAL_PRIVATE_THRESHOLD) {
            obj = (scm_symbol_t)heap->allocate_collectible(bytes);
            obj->hdr = scm_hdr_symbol | (len << HDR_SYMBOL_SIZE_SHIFT) ;
            obj->name = (char*)((uintptr_t)obj + sizeof(scm_symbol_rec_t));
        } else {
            obj = (scm_symbol_t)heap->allocate_collectible(sizeof(scm_symbol_rec_t));
            obj->hdr = scm_hdr_symbol | (len << HDR_SYMBOL_SIZE_SHIFT) ;
            obj->name = (char*)heap->allocate_private(len);
        }
        strcpy(obj->name, name);
        heap->m_symbol.put(obj);
    }
    heap->m_symbol.unlock();
    return obj;
    */
}

scm_symbol_t
make_symbol_inherent(object_heap_t* heap, const char* name, int code)
{
    assert(code < 256);
    assert(code < array_sizeof(heap->m_inherents));
    assert(heap->m_inherents[code] == scm_undef);
    scm_symbol_t obj = (scm_symbol_t)heap->allocate_collectible(sizeof(scm_symbol_rec_t));
    int len = strlen(name);
    obj->hdr = scm_hdr_symbol
                | (len << HDR_SYMBOL_SIZE_SHIFT)
                | (code << HDR_SYMBOL_CODE_SHIFT)
                | HDR_SYMBOL_INHERENT_BIT;
    obj->name = (char*)heap->allocate_private(len + 1);
    strcpy(obj->name, name);
    heap->m_inherents[code] = obj;
    heap->m_symbol.lock();
    assert(heap->m_symbol.get(name, len) == scm_undef);
    heap->m_symbol.put(obj);
    heap->m_symbol.unlock();
    return obj;
}

scm_string_t
make_string(object_heap_t* heap, const char *name, int len)
{
    if (len == 0) return (scm_string_t)heap->m_inherents[NIL_STRING];
    scm_string_t obj = (scm_string_t)heap->allocate_collectible(sizeof(scm_string_rec_t));
    obj->hdr = scm_hdr_string | (len << HDR_STRING_SIZE_SHIFT);
    obj->name = (char*)heap->allocate_private(len + 1);
    memcpy(obj->name, name, len);
    obj->name[len] = 0;
    return obj;
}

scm_string_t
make_string(object_heap_t* heap, const char *name)
{
    return make_string(heap, name, strlen(name));
}

scm_string_t
make_string_literal(object_heap_t* heap, const char* name, int len)
{
    heap->m_string.lock();
    scm_string_t obj = (scm_string_t)heap->m_string.get(name, len);
    if (obj == scm_undef) {
        obj = (scm_string_t)heap->allocate_collectible(sizeof(scm_string_rec_t));
        obj->hdr = scm_hdr_string
                    | (len << HDR_STRING_SIZE_SHIFT)
                    | (1 << HDR_STRING_LITERAL_SHIFT);
        obj->name = (char*)heap->allocate_private(len + 1);
        memcpy(obj->name, name, len);
        obj->name[len] = 0;
        heap->m_string.put(obj);
    }
    heap->m_string.unlock();
    return obj;
}

scm_string_t
make_string_literal(object_heap_t* heap, const char *name)
{
    return make_string_literal(heap, name, strlen(name));
}

scm_string_t
make_string(object_heap_t* heap, int n, char c)
{
    if (n == 0) return (scm_string_t)heap->m_inherents[NIL_STRING];
    scm_string_t obj = (scm_string_t)heap->allocate_collectible(sizeof(scm_string_rec_t));
    obj->hdr = scm_hdr_string | (n << HDR_STRING_SIZE_SHIFT);
    obj->name = (char*)heap->allocate_private(n + 1);
    memset(obj->name, c, n);
    obj->name[n] = 0;
    return obj;
}

scm_vector_t
make_vector(object_heap_t* heap, scm_obj_t lst)
{
    if (lst == scm_nil) return (scm_vector_t)heap->m_inherents[NIL_VECTOR];
    VERIFY_DATUM(lst);
    int n = list_length(lst);
    int bytes = sizeof(scm_vector_rec_t) + sizeof(scm_obj_t) * n;
    scm_vector_t obj;
    if (bytes <= INTERNAL_PRIVATE_THRESHOLD) {
        obj = (scm_vector_t)heap->allocate_collectible(bytes);
        obj->hdr = scm_hdr_vector | (n << HDR_VECTOR_COUNT_SHIFT);
        obj->elts = (scm_obj_t*)((uintptr_t)obj + sizeof(scm_vector_rec_t));
    } else {
        obj = (scm_vector_t)heap->allocate_collectible(sizeof(scm_vector_rec_t));
        obj->hdr = scm_hdr_vector | (n << HDR_VECTOR_COUNT_SHIFT);
        obj->elts = (scm_obj_t*)heap->allocate_private(sizeof(scm_obj_t) * n);
    }
    for (int i = 0; i < n; i++) {
        obj->elts[i] = CAR(lst);
        lst = CDR(lst);
    }
    return obj;
}

scm_vector_t
make_vector(object_heap_t* heap, int n, scm_obj_t elt)
{
    if (n == 0) return (scm_vector_t)heap->m_inherents[NIL_VECTOR];
    VERIFY_DATUM(elt);
    int bytes = sizeof(scm_vector_rec_t) + sizeof(scm_obj_t) * n;
    scm_vector_t obj;
    if (bytes <= INTERNAL_PRIVATE_THRESHOLD) {
        obj = (scm_vector_t)heap->allocate_collectible(bytes);
        obj->hdr = scm_hdr_vector | (n << HDR_VECTOR_COUNT_SHIFT);
        obj->elts = (scm_obj_t*)((uintptr_t)obj + sizeof(scm_vector_rec_t));
    } else {
        obj = (scm_vector_t)heap->allocate_collectible(sizeof(scm_vector_rec_t));
        obj->hdr = scm_hdr_vector | (n << HDR_VECTOR_COUNT_SHIFT);
        obj->elts = (scm_obj_t*)heap->allocate_private(sizeof(scm_obj_t) * n);
    }
    for (int i = 0; i < n; i++) obj->elts[i] = elt;
    return obj;
}

scm_bvector_t
make_bvector(object_heap_t* heap, int n)
{
    if (n == 0) return (scm_bvector_t)heap->m_inherents[NIL_BVECTOR];
    scm_bvector_t obj = (scm_bvector_t)heap->allocate_collectible(sizeof(scm_bvector_rec_t));
    obj->hdr = scm_hdr_bvector;
    obj->count = n;
    obj->elts = (uint8_t*)heap->allocate_private(n);
    memset(obj->elts, 0, n);
    return obj;
}

scm_bvector_t
make_bvector_mapping(object_heap_t* heap, void* p, int n)
{
    scm_bvector_t obj = (scm_bvector_t)heap->allocate_collectible(sizeof(scm_bvector_rec_t));
    obj->hdr = scm_hdr_bvector | (1 << HDR_BVECTOR_MAPPING_SHIFT);
    obj->count = n;
    obj->elts = (uint8_t*)p;
    return obj;
}

scm_port_t
make_temp_file_port(object_heap_t* heap, scm_obj_t name, int buffer_mode, scm_obj_t transcoder)
{
    VERIFY_DATUM(name);
    VERIFY_DATUM(transcoder);

    scm_port_t obj = (scm_port_t)heap->allocate_collectible(sizeof(scm_port_rec_t));
    memset(obj, 0, sizeof(scm_port_rec_t));
    obj->hdr = scm_hdr_port;
    obj->lock.init(true);
    scoped_lock lock(obj->lock);
    port_open_temp_file(obj, name, buffer_mode, transcoder);
    return obj;
}

scm_port_t
make_std_port(object_heap_t* heap, fd_t fd, scm_obj_t name, int direction, int file_options, int buffer_mode, scm_obj_t transcoder)
{
    VERIFY_DATUM(name);
    VERIFY_DATUM(transcoder);

    scm_port_t obj = (scm_port_t)heap->allocate_collectible(sizeof(scm_port_rec_t));
    memset(obj, 0, sizeof(scm_port_rec_t));
    obj->hdr = scm_hdr_port;
    obj->lock.init(true);
    scoped_lock lock(obj->lock);
    port_open_std(obj, fd, name, direction, file_options, buffer_mode, transcoder);
    return obj;
}

scm_port_t
make_file_port(object_heap_t* heap, scm_obj_t name, int direction, int file_options, int buffer_mode, scm_obj_t transcoder)
{
    VERIFY_DATUM(name);
    VERIFY_DATUM(transcoder);

    scm_port_t obj = (scm_port_t)heap->allocate_collectible(sizeof(scm_port_rec_t));
    memset(obj, 0, sizeof(scm_port_rec_t));
    obj->hdr = scm_hdr_port;
    obj->lock.init(true);
    scoped_lock lock(obj->lock);
    port_open_file(obj, name, direction, file_options, buffer_mode, transcoder);
    return obj;
}

scm_port_t
make_bytevector_port(object_heap_t* heap, scm_obj_t name, int direction, scm_obj_t bytes, scm_obj_t transcoder)
{
    VERIFY_DATUM(name);
    VERIFY_DATUM(bytes);
    VERIFY_DATUM(transcoder);
    assert(SYMBOLP(name));
    scm_port_t obj = (scm_port_t)heap->allocate_collectible(sizeof(scm_port_rec_t));
    memset(obj, 0, sizeof(scm_port_rec_t));
    obj->hdr = scm_hdr_port;
    obj->lock.init(true);
    scoped_lock lock(obj->lock);
    port_open_bytevector(obj, name, direction, bytes, transcoder);
    return obj;
}

scm_port_t
make_custom_port(object_heap_t* heap, scm_obj_t name, int direction, scm_obj_t handlers, scm_obj_t transcoder)
{
    VERIFY_DATUM(name);
    VERIFY_DATUM(handlers);
    VERIFY_DATUM(transcoder);

    scm_port_t obj = (scm_port_t)heap->allocate_collectible(sizeof(scm_port_rec_t));
    memset(obj, 0, sizeof(scm_port_rec_t));
    obj->hdr = scm_hdr_port;
    obj->lock.init(true);
    scoped_lock lock(obj->lock);
    port_make_custom_port(obj, name, direction, handlers, transcoder);
    return obj;
}


scm_port_t
make_transcoded_port(object_heap_t* heap, scm_obj_t name, scm_port_t port, scm_bvector_t transcoder)
{
    VERIFY_DATUM(name);
    VERIFY_DATUM(port);
    VERIFY_DATUM(transcoder);

    port->lock.verify_locked();
    scm_port_t obj = (scm_port_t)heap->allocate_collectible(sizeof(scm_port_rec_t));
    memset(obj, 0, sizeof(scm_port_rec_t));
    obj->hdr = scm_hdr_port;
    obj->lock.init(true);
    scoped_lock lock(obj->lock);
    port_make_transcoded_port(name, port, obj, transcoder);
    return obj;
}

scm_values_t
make_values(object_heap_t* heap, int n)
{
    int bytes = sizeof(scm_values_rec_t) + sizeof(scm_obj_t) * n;
    scm_values_t obj;
    if (bytes <= INTERNAL_PRIVATE_THRESHOLD) {
        obj = (scm_values_t)heap->allocate_collectible(bytes);
        obj->hdr = scm_hdr_values | (n << HDR_VALUES_COUNT_SHIFT);
        obj->elts = (scm_obj_t*)((uintptr_t)obj + sizeof(scm_values_rec_t));
    } else {
        obj = (scm_values_t)heap->allocate_collectible(sizeof(scm_values_rec_t));
        obj->hdr = scm_hdr_values | (n << HDR_VALUES_COUNT_SHIFT);
        obj->elts = (scm_obj_t*)heap->allocate_private(sizeof(scm_obj_t) * n);
    }
    for (int i = 0; i < n; i++) obj->elts[i] = scm_unspecified;
    return obj;
}

scm_cont_t
make_cont(object_heap_t* heap, scm_obj_t rec, void* lnk)
{
    scm_cont_t obj = (scm_cont_t)heap->allocate_collectible(sizeof(scm_cont_rec_t));
    obj->hdr = scm_hdr_cont;
    obj->wind_rec = rec;
    obj->cont = lnk;
    return obj;
}

scm_hashtable_t
make_hashtable(object_heap_t* heap, int type, int n)
{
    assert(n > 0);
    scm_hashtable_t obj = (scm_hashtable_t)heap->allocate_collectible(sizeof(scm_hashtable_rec_t));
    int datum_size = sizeof(hashtable_rec_t) + sizeof(scm_obj_t) * ((n + n) - 1);
    hashtable_rec_t* ht_datum = (hashtable_rec_t*)heap->allocate_private(datum_size);
    ht_datum->capacity = n;
    ht_datum->live = 0;
    ht_datum->used = 0;
    for (int i = 0; i < (n + n); i++) ht_datum->elts[i] = scm_hash_free;
    switch (type) {
    case SCM_HASHTABLE_TYPE_EQ:
        obj->hash = NULL;
        obj->equiv = NULL;
        break;
    case SCM_HASHTABLE_TYPE_EQV:
        obj->hash = eqv_hash;
        obj->equiv = eqv_hash_equiv;
        break;
    case SCM_HASHTABLE_TYPE_EQUAL:
        obj->hash = equal_hash;
        obj->equiv = equal_hash_equiv;
        break;
    case SCM_HASHTABLE_TYPE_STRING:
        obj->hash = string_hash;
        obj->equiv = string_hash_equiv;
        break;
    default:
        assert(false);
    }
    obj->hdr = scm_hdr_hashtable;
    obj->type = type;
    obj->handlers = scm_false;
    obj->datum = ht_datum;
    obj->lock.init();
    return obj;
}

scm_hashtable_t
make_generic_hashtable(object_heap_t* heap, scm_vector_t handlers)
{
    assert(VECTORP(handlers));
    scm_hashtable_t obj = (scm_hashtable_t)heap->allocate_collectible(sizeof(scm_hashtable_rec_t));
    obj->hdr = scm_hdr_hashtable;
    obj->type = SCM_HASHTABLE_TYPE_GENERIC;
    obj->handlers = handlers;
    obj->hash = NULL;
    obj->equiv = NULL;
    obj->datum = NULL;
    obj->lock.init();
    return obj;
}

scm_hdr_t   hdr;
    mutex_t     lock;
    int         type;
    hash_proc_t hash;
    equiv_proc_t equiv;
    hashtable_rec_t* datum; // [ key0 ... keyN val0 ... valN ]


scm_environment_t
make_environment(object_heap_t* heap, const char* name)//, bool immutable)
{
    scm_environment_t obj = (scm_environment_t)heap->allocate_collectible(sizeof(scm_environment_rec_t));
    obj->hdr = scm_hdr_environment;
    obj->variable = make_hashtable(heap, SCM_HASHTABLE_TYPE_EQ, lookup_mutable_hashtable_size(0));
    obj->macro = make_hashtable(heap, SCM_HASHTABLE_TYPE_EQ, lookup_mutable_hashtable_size(0));
    if (name) {
        obj->name = make_string_literal(heap, name);
    } else {
        char buf[32];
        snprintf(buf, sizeof(buf), "0x%x", obj);
        obj->name = make_string_literal(heap, buf);
    }
    return obj;
}

scm_subr_t
make_subr(object_heap_t* heap, subr_proc_t adrs, scm_obj_t doc)
{
    scm_subr_t obj = (scm_subr_t)heap->allocate_collectible(sizeof(scm_subr_rec_t));
    obj->hdr = scm_hdr_subr;
    obj->adrs = adrs;
    obj->doc = doc;
#if PROFILE_SUBR
    obj->c_push = 0;
    obj->c_load = 0;
    obj->c_apply = 0;
#endif
    return obj;
}

scm_closure_t
make_closure(object_heap_t* heap, int argc, int rest, void* env, scm_obj_t code, scm_obj_t doc)
{
    VERIFY_DATUM(code);
    VERIFY_DATUM(doc);
    int args = rest ? (-1 - argc) : argc;
    scm_closure_t obj = (scm_closure_t)heap->allocate_collectible(sizeof(scm_closure_rec_t));
    obj->hdr = scm_hdr_closure | (args << HDR_CLOSURE_ARGS_SHIFT);
    obj->env = env;
    obj->code = code;
    obj->doc = doc;
    return obj;
}

scm_closure_t
make_closure(object_heap_t* heap, scm_closure_t tmpl, void* env)
{
    if (env) {
        scm_closure_t obj = (scm_closure_t)heap->allocate_collectible(sizeof(scm_closure_rec_t));
        obj->hdr = tmpl->hdr;
        obj->env = env;
        obj->code = tmpl->code;
        obj->doc = tmpl->doc;
        return obj;
    }
    return tmpl;
}

scm_flonum_t
make_flonum(object_heap_t* heap, double num)
{
#if USE_FLONUM_CONST
    if (num == 0.0) {
        union { double f64; int64_t i64; } datum;
        datum.f64 = num;
        if (datum.i64 < 0) {
            return (scm_flonum_t)heap->m_inherents[FL_NEGATIVE_ZERO];
        } else {
            return (scm_flonum_t)heap->m_inherents[FL_POSITIVE_ZERO];
        }
    }
#endif
    scm_flonum_t obj = heap->allocate_flonum();
    obj->hdr = scm_hdr_flonum;
    obj->value = num;
    return obj;
}

scm_bignum_t
make_bignum(object_heap_t* heap, scm_bignum_t bn)
{
    int count = HDR_BIGNUM_COUNT(bn->hdr);
    scm_bignum_t obj = make_bignum(heap, count);
    obj->hdr = bn->hdr;
    memcpy(obj->elts, bn->elts, sizeof(uint32_t) * count);
    return obj;
}

scm_bignum_t
make_bignum(object_heap_t* heap, int n)
{
    int bytes = sizeof(scm_bignum_rec_t) + sizeof(uint32_t) * n;
    scm_bignum_t obj;
    if (bytes <= INTERNAL_PRIVATE_THRESHOLD) {
        obj = (scm_bignum_t)heap->allocate_collectible(bytes);
        obj->hdr = scm_hdr_bignum | (n << HDR_BIGNUM_COUNT_SHIFT);
        if (n) obj->elts = (uint32_t*)((uintptr_t)obj + sizeof(scm_bignum_rec_t));
        else obj->elts = NULL;
    } else {
        obj = (scm_bignum_t)heap->allocate_collectible(sizeof(scm_bignum_rec_t));
        obj->hdr = scm_hdr_bignum | (n << HDR_BIGNUM_COUNT_SHIFT);
        if (n) obj->elts = (uint32_t*)heap->allocate_private(sizeof(uint32_t) * n);
        else obj->elts = NULL;
    }
    return obj;
}

scm_complex_t
make_complex(object_heap_t* heap, double real, double imag)
{
    scm_complex_t obj = (scm_complex_t)heap->allocate_collectible(sizeof(scm_complex_rec_t));
    obj->hdr = scm_hdr_complex;
    obj->real = make_flonum(heap, real);
    obj->imag = make_flonum(heap, imag);
    return obj;
}

scm_complex_t
make_complex(object_heap_t* heap, scm_obj_t real, scm_obj_t imag)
{
    VERIFY_DATUM(real);
    VERIFY_DATUM(imag);
    assert(!COMPLEXP(real));
    assert(!COMPLEXP(imag));
    scm_complex_t obj = (scm_complex_t)heap->allocate_collectible(sizeof(scm_complex_rec_t));
    obj->hdr = scm_hdr_complex;
    obj->real = real;
    obj->imag = imag;
    return obj;
}

scm_rational_t
make_rational(object_heap_t* heap, scm_obj_t numerator, scm_obj_t denominator)
{
    VERIFY_DATUM(numerator);
    VERIFY_DATUM(denominator);
    assert(n_negative_pred(denominator) == false);
    assert(n_exact_pred(numerator));
    assert(n_exact_pred(denominator));
    scm_rational_t obj = (scm_rational_t)heap->allocate_collectible(sizeof(scm_rational_rec_t));
    obj->hdr = scm_hdr_rational;
    obj->nume = numerator;
    obj->deno = denominator;
    return obj;
}

scm_gloc_t
make_gloc(object_heap_t* heap, scm_environment_t environment, scm_symbol_t symbol)
{
    scm_gloc_t obj = (scm_gloc_t)heap->allocate_collectible(sizeof(scm_gloc_rec_t));
    obj->hdr = scm_hdr_gloc;
    obj->variable = symbol;
  #if GLOC_DEBUG_INFO
    obj->environment = environment->name;
  #endif
    obj->value = scm_undef;
    return obj;
}

scm_tuple_t
make_tuple(object_heap_t* heap, int n, scm_obj_t elt)
{
    if (n == 0) return (scm_tuple_t)heap->m_inherents[NIL_TUPLE];
    VERIFY_DATUM(elt);
    int bytes = sizeof(scm_tuple_rec_t) + sizeof(scm_obj_t) * n;
    scm_tuple_t obj;
    if (bytes <= INTERNAL_PRIVATE_THRESHOLD) {
        obj = (scm_tuple_t)heap->allocate_collectible(bytes);
        obj->hdr = scm_hdr_tuple | (n << HDR_TUPLE_COUNT_SHIFT);
        obj->elts = (scm_obj_t*)((uintptr_t)obj + sizeof(scm_tuple_rec_t));
    } else {
        obj = (scm_tuple_t)heap->allocate_collectible(sizeof(scm_tuple_rec_t));
        obj->hdr = scm_hdr_tuple | (n << HDR_TUPLE_COUNT_SHIFT);
        obj->elts = (scm_obj_t*)heap->allocate_private(sizeof(scm_obj_t) * n);
    }
    for (int i = 0; i < n; i++) obj->elts[i] = elt;
    return obj;
}

scm_weakmapping_t
make_weakmapping(object_heap_t* heap, scm_obj_t key, scm_obj_t value)
{
    scm_weakmapping_t obj = (scm_weakmapping_t)heap->allocate_weakmapping();
    obj->hdr = scm_hdr_weakmapping;
    obj->key = key;
    obj->value = value;
    return obj;
}

scm_weakhashtable_t
make_weakhashtable(object_heap_t* heap, int n)
{
    assert(n > 0);
    scm_weakhashtable_t obj = (scm_weakhashtable_t)heap->allocate_collectible(sizeof(scm_weakhashtable_rec_t));
    int datum_size = sizeof(weakhashtable_rec_t) + sizeof(scm_obj_t) * (n - 1);
    weakhashtable_rec_t* ht_datum = (weakhashtable_rec_t*)heap->allocate_private(datum_size);
    ht_datum->capacity = n;
    ht_datum->live = 0;
    ht_datum->used = 0;
    for (int i = 0; i < n; i++) ht_datum->elts[i] = scm_hash_free;
    obj->hdr = scm_hdr_weakhashtable;
    obj->datum = ht_datum;
    obj->lock.init();
    return obj;
}

scm_obj_t
make_list(object_heap_t* heap, int len, ...)
{
    va_list ap;
    va_start(ap, len);
    if (len == 0) return scm_nil;
    scm_obj_t obj = make_pair(heap, va_arg(ap, scm_obj_t), scm_nil);
    scm_obj_t tail = obj;
    for (int i = 1; i < len; i++) {
        scm_obj_t e = make_pair(heap, va_arg(ap, scm_obj_t), scm_nil);
        CDR(tail) = e;
        tail = e;
    }
    va_end(ap);
    return obj;
}

void
rehash_hashtable(object_heap_t* heap, scm_hashtable_t ht, int nsize)
{
    assert(HASHTABLEP(ht));
    ht->lock.verify_locked();
    hashtable_rec_t* ht_datum = ht->datum;
    int nelts = ht_datum->capacity;
    assert(HASH_DENSE_THRESHOLD(nsize) > ht_datum->live);
    scm_hashtable_t ht2 = make_hashtable(heap, ht->type, nsize);
    scoped_lock lock(ht2->lock);
    for (int i = 0; i < nelts; i++) {
        if (ht_datum->elts[i] == scm_hash_free) continue;
        if (ht_datum->elts[i] == scm_hash_deleted) continue;
        put_hashtable(ht2, ht_datum->elts[i], ht_datum->elts[i + nelts]);
    }
    swap(ht->datum, ht2->datum);
}

void
inplace_rehash_hashtable(object_heap_t* heap, scm_hashtable_t ht)
{
    assert(HASHTABLEP(ht));
    ht->lock.verify_locked();
    hashtable_rec_t* ht_datum = ht->datum;
    int nelts = ht_datum->capacity;
    int datum_size = sizeof(hashtable_rec_t) + sizeof(scm_obj_t) * ((nelts + nelts) - 1);
    hashtable_rec_t* save_datum = (hashtable_rec_t*)malloc(datum_size);
    memcpy(save_datum, ht_datum, datum_size);
    clear_volatile_hashtable(ht);
    for (int i = 0; i < nelts; i++) {
        if (save_datum->elts[i] == scm_hash_free) continue;
        if (save_datum->elts[i] == scm_hash_deleted) continue;
        put_hashtable(ht, save_datum->elts[i], save_datum->elts[i + nelts]);
    }
    free(save_datum);
}

void
rehash_weakhashtable(object_heap_t* heap, scm_weakhashtable_t ht, int nsize)
{
    assert(WEAKHASHTABLEP(ht));
    ht->lock.verify_locked();
    weakhashtable_rec_t* ht_datum = ht->datum;
    int nelts = ht_datum->capacity;
    assert(HASH_DENSE_THRESHOLD(nsize) > ht_datum->live);
    scm_weakhashtable_t ht2 = make_weakhashtable(heap, nsize);
    scoped_lock lock(ht2->lock);
    for (int i = 0; i < nelts; i++) {
        scm_obj_t obj = ht_datum->elts[i];
        if (obj == scm_hash_free) continue;
        if (obj == scm_hash_deleted) continue;
        assert(WEAKMAPPINGP(obj));
        if (((scm_weakmapping_t)obj)->key == scm_false) continue;
        put_weakhashtable(ht2, (scm_weakmapping_t)obj);
    }
    swap(ht->datum, ht2->datum);
}

void
inplace_rehash_weakhashtable(object_heap_t* heap, scm_weakhashtable_t ht)
{
    assert(WEAKHASHTABLEP(ht));
    ht->lock.verify_locked();
    weakhashtable_rec_t* ht_datum = ht->datum;
    int nelts = ht_datum->capacity;
    int datum_size = sizeof(weakhashtable_rec_t) + sizeof(scm_obj_t) * (nelts - 1);
    weakhashtable_rec_t* save_datum = (weakhashtable_rec_t*)malloc(datum_size);
    memcpy(save_datum, ht_datum, datum_size);
    clear_volatile_weakhashtable(ht);
    for (int i = 0; i < nelts; i++) {
        scm_obj_t obj = save_datum->elts[i];
        if (obj == scm_hash_free) continue;
        if (obj == scm_hash_deleted) continue;
        assert(WEAKMAPPINGP(obj));
        if (((scm_weakmapping_t)obj)->key == scm_false) continue;
        put_weakhashtable(ht, (scm_weakmapping_t)obj);
    }
    free(save_datum);
}

scm_hashtable_t
copy_hashtable(object_heap_t* heap, scm_hashtable_t ht, bool immutable)
{
    assert(HASHTABLEP(ht));
    ht->lock.verify_locked();
    hashtable_rec_t* ht_datum = ht->datum;
    int nelts = ht_datum->capacity;
    scm_hashtable_t ht2 = make_hashtable(heap, ht->type, lookup_immutable_hashtable_size(HASH_IMMUTABLE_SIZE(ht_datum->live)));
    scoped_lock lock(ht2->lock);
    for (int i = 0; i < nelts; i++) {
        if (ht_datum->elts[i] == scm_hash_free) continue;
        if (ht_datum->elts[i] == scm_hash_deleted) continue;
        put_hashtable(ht2, ht_datum->elts[i], ht_datum->elts[i + nelts]);
    }
    if (immutable) ht2->hdr |= (1 << HDR_HASHTABLE_IMMUTABLE_SHIFT);
    return ht2;
}

scm_weakhashtable_t
copy_weakhashtable(object_heap_t* heap, scm_weakhashtable_t ht, bool immutable)
{
    assert(WEAKHASHTABLEP(ht));
    ht->lock.verify_locked();
    weakhashtable_rec_t* ht_datum = ht->datum;
    int nelts = ht_datum->capacity;
    scm_weakhashtable_t ht2 = make_weakhashtable(heap, lookup_immutable_hashtable_size(HASH_IMMUTABLE_SIZE(ht_datum->live)));
    scoped_lock lock(ht2->lock);
    for (int i = 0; i < nelts; i++) {
        scm_obj_t obj = ht_datum->elts[i];
        if (obj == scm_hash_free) continue;
        if (obj == scm_hash_deleted) continue;
        assert(WEAKMAPPINGP(obj));
        if (((scm_weakmapping_t)obj)->key == scm_false) continue;
        put_weakhashtable(ht2, (scm_weakmapping_t)obj);
    }
    if (immutable) ht2->hdr |= (1 << HDR_WEAKHASHTABLE_IMMUTABLE_SHIFT);
    return ht2;
}

void
clear_hashtable(object_heap_t* heap, scm_hashtable_t ht, int n)
{
    assert(HASHTABLEP(ht));
    ht->lock.verify_locked();
    scm_hashtable_t ht2 = make_hashtable(heap, ht->type, n);
    swap(ht->datum, ht2->datum);
}

void
clear_weakhashtable(object_heap_t* heap, scm_weakhashtable_t ht, int n)
{
    assert(WEAKHASHTABLEP(ht));
    ht->lock.verify_locked();
    scm_weakhashtable_t ht2 = make_weakhashtable(heap, n);
    swap(ht->datum, ht2->datum);
}

void
clear_volatile_hashtable(scm_hashtable_t ht)
{
    assert(HASHTABLEP(ht));
    ht->lock.verify_locked();
    hashtable_rec_t* ht_datum = ht->datum;
    int n = ht_datum->capacity;
    ht_datum->live = 0;
    ht_datum->used = 0;
    for (int i = 0; i < (n + n); i++) ht_datum->elts[i] = scm_hash_free;
}

void
clear_volatile_weakhashtable(scm_weakhashtable_t ht)
{
    assert(WEAKHASHTABLEP(ht));
    ht->lock.verify_locked();
    weakhashtable_rec_t* ht_datum = ht->datum;
    int n = ht_datum->capacity;
    ht_datum->live = 0;
    ht_datum->used = 0;
    for (int i = 0; i < n; i++) ht_datum->elts[i] = scm_hash_free;
}

void
finalize(object_heap_t* heap, void* obj)
{
    // do not access shared object during finalize, it may collected.
    assert(heap->is_collectible(obj));
    if (PAIRP(obj)) {
        assert(false);
    }
    if (FLONUMP(obj)) {
        assert(false);
    }

    int tc = HDR_TC(HDR(obj));
    assert(tc >= 0);
    assert(tc <= TC_MASKBITS);
    switch (tc) {
        case TC_BIGNUM: {
            scm_bignum_t bignum = (scm_bignum_t)obj;
            if (bignum->elts != (uint32_t*)((uintptr_t)bignum + sizeof(scm_bignum_rec_t))) {
                heap->deallocate_private(bignum->elts);
            }
            break;
        }
        case TC_SYMBOL: {
            scm_symbol_t symbol = (scm_symbol_t)obj;
            if (symbol->name != (char*)((uintptr_t)symbol + sizeof(scm_symbol_rec_t))) {
                heap->deallocate_private(symbol->name);
            }
            break;
        }
        case TC_STRING: {
            scm_string_t string = (scm_string_t)obj;
            if (string->name != (char*)((uintptr_t)string + sizeof(scm_string_rec_t))) {
                heap->deallocate_private(string->name);
            }
            break;
        }
        case TC_VECTOR: {
            scm_vector_t vector = (scm_vector_t)obj;
            if (vector->elts != (scm_obj_t*)((uintptr_t)vector + sizeof(scm_vector_rec_t))) {
                heap->deallocate_private(vector->elts);
            }
            break;
        }
        case TC_BVECTOR: {
            scm_bvector_t bvector = (scm_bvector_t)obj;
            if (HDR_BVECTOR_MAPPING(bvector->hdr) == 0) heap->deallocate_private(bvector->elts);
            break;
        }
        case TC_TUPLE: {
            scm_tuple_t tuple = (scm_tuple_t)obj;
            if (tuple->elts != (scm_obj_t*)((uintptr_t)tuple + sizeof(scm_tuple_rec_t))) {
                heap->deallocate_private(tuple->elts);
            }
            break;
        }
        case TC_VALUES: {
            scm_values_t values = (scm_values_t)obj;
            if (values->elts != (scm_obj_t*)((uintptr_t)values + sizeof(scm_values_rec_t))) {
                heap->deallocate_private(values->elts);
            }
            break;
        }
        case TC_HASHTABLE: {
            scm_hashtable_t ht = (scm_hashtable_t)obj;
            heap->deallocate_private(ht->datum);
            ht->lock.destroy();
            break;
        }
        case TC_WEAKHASHTABLE: {
            scm_weakhashtable_t ht = (scm_weakhashtable_t)obj;
            heap->deallocate_private(ht->datum);
            ht->lock.destroy();
            break;
        }
        case TC_PORT: {
            scm_port_t port = (scm_port_t)obj;
            {
                scoped_lock lock(port->lock);
                port_close(port);
            }
            port->lock.destroy();
            break;
        }
    }
}
