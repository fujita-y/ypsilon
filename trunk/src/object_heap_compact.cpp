/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#include "core.h"
#include "heap.h"
#include "list.h"

#define PLACE_FORWARD_PTR(from, to)    ((*(intptr_t*)(from)) = MAKEHEAPFORWARDPTR(to))
#define HAVE_FORWARD_PTR(p)            HEAPFORWARDPTRP(*(intptr_t*)(p))
#define FORWARD_PTR(p)                 ((void*)HEAPFORWARDPTR(*(intptr_t*)(p)))

struct relocate_info_t {
    uint8_t*    relocated;
    int         total;

    relocate_info_t(int count) {
        total = count;
        relocated= new uint8_t [count];
        memset(relocated, 0, sizeof(uint8_t) * count);
    }

    ~relocate_info_t() {
        delete [] relocated;
        relocated = NULL;
    }
};

static inline intptr_t
forwarding_offset(scm_obj_t obj)
{
    if (CELLP(obj) && HAVE_FORWARD_PTR(obj)) {
        return (intptr_t)FORWARD_PTR(obj) - (intptr_t)obj;
    } else {
        return 0;
    }
}

scm_obj_t
object_heap_t::forward(scm_obj_t obj)
{
    assert(obj);
    if (CELLP(obj)) {
        scm_obj_t to = (scm_obj_t)((intptr_t)obj + forwarding_offset(obj));
        return to;
    }
    return obj;
}

void*
object_heap_t::interior_forward(void* ref)
{
    if (ref) {
        scm_obj_t from = OBJECT_SLAB_TRAITS_OF(ref)->cache->lookup(ref);
        scm_obj_t to = forward(from);
        intptr_t offset = (intptr_t)to - (intptr_t)from;
        return (void*)((intptr_t)ref + offset);
    }
    return ref;
}

static void
relocate_collectible(void* obj, int size, void* desc)
{
    object_heap_t* heap = (object_heap_t*)desc;
    assert(heap->is_collectible(obj));
    scm_obj_t from = (scm_obj_t)obj;
    scm_obj_t to;
    if (PAIRP(obj)) {
        assert(size == sizeof(scm_pair_rec_t));
        to = heap->allocate_cons();
    } else if (FLONUMP(obj)) {
        assert(size == sizeof(scm_flonum_rec_t));
        to = heap->allocate_flonum();
    } else if (WEAKMAPPINGP(obj)) {
        assert(size == sizeof(scm_weakmapping_rec_t));
        to = heap->allocate_weakmapping();
    } else {
        to = heap->allocate_collectible(size);
    }
    memcpy(to, from, size);
    if (!PAIRP(from)) {
        int tc = HDR_TC(HDR(from));
        assert(tc >= 0);
        assert(tc <= TC_MASKBITS);
        switch (tc) {
            case TC_BIGNUM: {
                scm_bignum_t bignum_from = (scm_bignum_t)from;
                if (bignum_from->elts == (uint32_t*)((uintptr_t)bignum_from + sizeof(scm_bignum_rec_t))) {
                    scm_bignum_t bignum_to = (scm_bignum_t)to;
                    bignum_to->elts = (uint32_t*)((uintptr_t)bignum_to + sizeof(scm_bignum_rec_t));
                }
                break;
            }
            case TC_SYMBOL: {
                scm_symbol_t symbol_from = (scm_symbol_t)from;
                if (symbol_from->name == (char*)((uintptr_t)symbol_from + sizeof(scm_symbol_rec_t))) {
                    scm_symbol_t symbol_to = (scm_symbol_t)to;
                    symbol_to->name = (char*)((uintptr_t)symbol_to + sizeof(scm_symbol_rec_t));
                }
                break;
            }
            case TC_STRING: {
                scm_string_t string_from = (scm_string_t)from;
                if (string_from->name == (char*)((uintptr_t)string_from + sizeof(scm_string_rec_t))) {
                    scm_string_t string_to = (scm_string_t)to;
                    string_to->name = (char*)((uintptr_t)string_to + sizeof(scm_string_rec_t));
                }
                break;
            }
            case TC_VECTOR: {
                scm_vector_t vector_from = (scm_vector_t)from;
                if (vector_from->elts == (scm_obj_t*)((uintptr_t)vector_from + sizeof(scm_vector_rec_t))) {
                    scm_vector_t vector_to = (scm_vector_t)to;
                    vector_to->elts = (scm_obj_t*)((uintptr_t)vector_to + sizeof(scm_vector_rec_t));
                }
                break;
            }
            case TC_TUPLE: {
                scm_tuple_t tuple_from = (scm_tuple_t)from;
                if (tuple_from->elts == (scm_obj_t*)((uintptr_t)tuple_from + sizeof(scm_tuple_rec_t))) {
                    scm_tuple_t tuple_to = (scm_tuple_t)to;
                    tuple_to->elts = (scm_obj_t*)((uintptr_t)tuple_to + sizeof(scm_tuple_rec_t));
                }
                break;
            }
            case TC_VALUES: {
                scm_values_t values_from = (scm_values_t)from;
                if (values_from->elts == (scm_obj_t*)((uintptr_t)values_from + sizeof(scm_values_rec_t))) {
                    scm_values_t values_to = (scm_values_t)to;
                    values_to->elts = (scm_obj_t*)((uintptr_t)values_to + sizeof(scm_values_rec_t));
                }
                break;
            }
        }
    }
#ifndef NDEBUG
    memset(from, 0xFE, size);
#endif
    PLACE_FORWARD_PTR(from, to);
}

static void
resolve_collectible(void* obj, int size, void* desc)
{
    object_heap_t* heap = (object_heap_t*)desc;
    assert(heap->is_collectible(obj));
    if (PAIRP(obj)) {
        scm_pair_t pair = (scm_pair_t)obj;
        pair->car = heap->forward(pair->car);
        pair->cdr = heap->forward(pair->cdr);
        return;
    }
    int tc = HDR_TC(HDR(obj));
    assert(tc >= 0 && tc <= TC_MASKBITS);
    switch (tc) {
        case TC_VECTOR: {
            scm_vector_t vector = (scm_vector_t)obj;
            int count = vector->count;
            for (int i = 0; i < count; i++) vector->elts[i] = heap->forward(vector->elts[i]);
            break;
        }
        case TC_TUPLE: {
            scm_tuple_t tuple = (scm_tuple_t)obj;
            int count = HDR_TUPLE_COUNT(tuple->hdr);
            for (int i = 0; i < count; i++) tuple->elts[i] = heap->forward(tuple->elts[i]);
            break;
        }
        case TC_VALUES: {
            scm_values_t values = (scm_values_t)obj;
            int count = HDR_VALUES_COUNT(values->hdr);
            for (int i = 0; i < count; i++) values->elts[i] = heap->forward(values->elts[i]);
            break;
        }
        case TC_HASHTABLE: {
            scm_hashtable_t ht = (scm_hashtable_t)obj;
            scoped_lock lock(ht->lock);
            ht->handlers = heap->forward(ht->handlers);
            hashtable_rec_t* ht_datum = ht->datum;
            if (ht_datum) {
                int nsize = ht_datum->capacity;
                bool rehash = false;
                for (int i = 0; i < nsize; i++) {
                    scm_obj_t p = heap->forward(ht_datum->elts[i]);
                    if (ht_datum->elts[i] != p) rehash = true;
                    ht_datum->elts[i] = p;
                }
                for (int i = 0; i < nsize; i++) {
                    scm_obj_t p = heap->forward(ht_datum->elts[i + nsize]);
                    if (ht_datum->elts[i + nsize] != p) rehash = true;
                    ht_datum->elts[i + nsize] = p;
                }
            }
            break;
        }
        case TC_WEAKHASHTABLE: {
            scm_weakhashtable_t ht = (scm_weakhashtable_t)obj;
            scoped_lock lock(ht->lock);
            weakhashtable_rec_t* ht_datum = ht->datum;
            int nsize = ht_datum->capacity;
            bool rehash = false;
            for (int i = 0; i < nsize; i++) {
                scm_obj_t p = heap->forward(ht_datum->elts[i]);
                if (ht_datum->elts[i] != p) rehash = true;
                ht_datum->elts[i] = p;
            }
            break;
        }
        case TC_PORT: {
            scm_port_t port = (scm_port_t)obj;
            port->bytes = heap->forward(port->bytes);
            port->name = heap->forward(port->name);
            port->transcoder = heap->forward(port->transcoder);
            port->handlers = heap->forward(port->handlers);
            break;
        }
        case TC_COMPLEX: {
            scm_complex_t complex = (scm_complex_t)obj;
            complex->imag = heap->forward(complex->imag);
            complex->real = heap->forward(complex->real);
            break;
        }
        case TC_RATIONAL: {
            scm_rational_t rational = (scm_rational_t)obj;
            rational->nume = heap->forward(rational->nume);
            rational->deno = heap->forward(rational->deno);
            break;
        }
        case TC_CLOSURE: {
            scm_closure_t closure = (scm_closure_t)obj;
            closure->code = heap->forward(closure->code);
            closure->doc = heap->forward(closure->doc);
            closure->env = heap->interior_forward(closure->env);
            break;
        }
        case TC_CONT: {
            scm_cont_t cont = (scm_cont_t)obj;
            cont->wind_rec = heap->forward(cont->wind_rec);
            cont->cont = heap->interior_forward(cont->cont);
            break;
        }
        case TC_HEAPENV: {
            int nbytes = HDR_HEAPENV_SIZE(HDR(obj));
            uint8_t* top = (uint8_t*)((intptr_t)obj + sizeof(scm_hdr_t));
            vm_env_t env = (vm_env_t)(top + nbytes - sizeof(vm_env_rec_t));
            env->up = heap->interior_forward(env->up);
            for (scm_obj_t* vars = (scm_obj_t*)top; vars < (scm_obj_t*)env; vars++) *vars = heap->forward(*vars);
            break;
        }
        case TC_HEAPCONT: {
            int nbytes = HDR_HEAPCONT_SIZE(HDR(obj));
            uint8_t* top = (uint8_t*)((intptr_t)obj + sizeof(scm_hdr_t));
            vm_cont_t cont = (vm_cont_t)(top + nbytes - sizeof(vm_cont_rec_t));
            cont->up = heap->interior_forward(cont->up);
            cont->env = heap->interior_forward(cont->env);
            cont->fp = (scm_obj_t*)top;
            cont->pc = heap->forward(cont->pc);
            cont->trace = heap->forward(cont->trace);
            for (scm_obj_t* args = (scm_obj_t*)top; args < (scm_obj_t*)cont; args++) *args = heap->forward(*args);
            break;
        }
        case TC_ENVIRONMENT: {
            scm_environment_t environment = (scm_environment_t)obj;
            environment->variable = (scm_hashtable_t)heap->forward(environment->variable);
            environment->macro = (scm_hashtable_t)heap->forward(environment->macro);
            environment->name = (scm_string_t)heap->forward(environment->name);
            break;
        }
        case TC_GLOC: {
            scm_gloc_t gloc = (scm_gloc_t)obj;
            gloc->variable = heap->forward(gloc->variable);
  #if GLOC_DEBUG_INFO
            gloc->environment = heap->forward(gloc->environment);
  #endif
            gloc->value = heap->forward(gloc->value);
            break;
        }
        case TC_SUBR: {
            scm_subr_t subr = (scm_subr_t)obj;
            subr->doc = heap->forward(subr->doc);
            break;
        }
        case TC_WEAKMAPPING: {
            scm_weakmapping_t wmap = (scm_weakmapping_t)obj;
            wmap->key = heap->forward(wmap->key);
            wmap->value = heap->forward(wmap->value);
            break;
        }
    }
}

static void
rehash_collectible(void* obj, int size, void* desc)
{
    object_heap_t* heap = (object_heap_t*)desc;
    assert(heap->is_collectible(obj));
    if (!PAIRP(obj)) {
        int tc = HDR_TC(HDR(obj));
        assert(tc >= 0);
        assert(tc <= TC_MASKBITS);
        switch (tc) {
            case TC_HASHTABLE: {
                scm_hashtable_t ht = (scm_hashtable_t)obj;
                scoped_lock lock(ht->lock);
                hashtable_rec_t* ht_datum = ht->datum;
                if (ht_datum && ht_datum->capacity) inplace_rehash_hashtable(heap, ht);
                break;
            }
            case TC_WEAKHASHTABLE: {
                scm_weakhashtable_t ht = (scm_weakhashtable_t)obj;
                scoped_lock lock(ht->lock);
                weakhashtable_rec_t* ht_datum = ht->datum;
                if (ht_datum && ht_datum->capacity) inplace_rehash_weakhashtable(heap, ht);
                break;
            }
        }
    }
}

relocate_info_t*
object_heap_t::relocate(bool pack)
{
    relocate_info_t* info = new relocate_info_t(m_pool_size >> OBJECT_SLAB_SIZE_SHIFT);

    object_slab_traits_t* first_traits = OBJECT_SLAB_TRAITS_OF(m_pool);
    object_slab_traits_t* traits;
    uint8_t* first_slab = m_pool;
    uint8_t* slab;

    if (pack) {
        int used_count = 0;
        for (int i = 0; i < m_pool_watermark; i++) {
            if (m_pool[i] != PTAG_FREE) used_count++;
        }
        int bound = 0;
        int hole_count = 0;
        for (int i = 0; i < m_pool_watermark; i++) {
            if (m_pool[i] == PTAG_FREE) hole_count++;
            else used_count--;
            if (hole_count >= used_count) break;
            bound++;
        }
        slab = first_slab;
        traits = first_traits;
        for (int i = 0; i < m_pool_watermark; i++) {
            if (i >= bound && GCSLABP(m_pool[i])) {
                traits->cache->detach(slab);
                info->relocated[i] = true;
            }
            slab += OBJECT_SLAB_SIZE;
            traits = (object_slab_traits_t*)((intptr_t)traits + OBJECT_SLAB_SIZE);
        }
    } else {
        slab = first_slab;
        traits = first_traits;
        for (int i = 0; i < m_pool_watermark; i++) {
            if (GCSLABP(m_pool[i]) && traits->free) {
                traits->cache->detach(slab);
                info->relocated[i] = true;
            }
            slab += OBJECT_SLAB_SIZE;
            traits = (object_slab_traits_t*)((intptr_t)traits + OBJECT_SLAB_SIZE);
        }
    }

    slab = first_slab;
    traits = first_traits;
    for (int i = 0; i < m_pool_watermark; i++) {
        if (GCSLABP(m_pool[i]) && info->relocated[i]) traits->cache->iterate(slab, relocate_collectible, this);
        slab += OBJECT_SLAB_SIZE;
        traits = (object_slab_traits_t*)((intptr_t)traits + OBJECT_SLAB_SIZE);
    }
    return info;
}

void
object_heap_t::resolve(relocate_info_t* info)
{
    object_slab_traits_t* first_traits = OBJECT_SLAB_TRAITS_OF(m_pool);
    object_slab_traits_t* traits;
    uint8_t* first_slab = m_pool;
    uint8_t* slab;

    slab = first_slab;
    traits = first_traits;
    for (int i = 0; i < m_pool_watermark; i++) {
        if (GCSLABP(m_pool[i]) && info->relocated[i] == false) traits->cache->iterate(slab, resolve_collectible, this);
        slab += OBJECT_SLAB_SIZE;
        traits = (object_slab_traits_t*)((intptr_t)traits + OBJECT_SLAB_SIZE);
    }
    m_symbol.resolve();
    m_string.resolve();

    slab = first_slab;
    traits = first_traits;
    for (int i = 0; i < m_pool_watermark; i++) {
        if (GCSLABP(m_pool[i]) && info->relocated[i] == false) traits->cache->iterate(slab, rehash_collectible, this);
        slab += OBJECT_SLAB_SIZE;
        traits = (object_slab_traits_t*)((intptr_t)traits + OBJECT_SLAB_SIZE);
    }

    m_symbol.inplace_rehash();
    m_string.inplace_rehash();

    for (int i = 0; i < INHERENT_TOTAL_COUNT; i++) m_inherents[i] = forward(m_inherents[i]);
    m_architecture_feature = (scm_hashtable_t)forward(m_architecture_feature);
    m_native_transcoder = (scm_bvector_t)forward(m_native_transcoder);
    m_trampolines = (scm_hashtable_t)forward(m_trampolines);
    //#if USE_PARALLEL_VM
    //m_thread_context = (scm_hashtable_t)forward(m_thread_context);
    //#endif
    m_system_environment = (scm_environment_t)forward(m_system_environment);
    m_interaction_environment = (scm_environment_t)forward(m_interaction_environment);
    m_hidden_variables = (scm_weakhashtable_t)forward(m_hidden_variables);

    slab = first_slab;
    traits = first_traits;
    for (int i = 0; i < m_pool_watermark; i++) {
        if (info->relocated[i]) deallocate(slab);
        slab += OBJECT_SLAB_SIZE;
        traits = (object_slab_traits_t*)((intptr_t)traits + OBJECT_SLAB_SIZE);
    }

    delete info;
}

typedef void* (*copy_proc_t)(object_heap_t* heap, void* datum);

typedef struct {
    object_heap_t* heap;
    copy_proc_t proc;
} relocate_desc_t;

static void*
copy_slab_private(object_heap_t* heap, void* datum)
{
    assert(!heap->is_collectible(datum));
    if (heap->in_non_full_slab(datum)) {
        int nbytes = heap->allocated_size(datum);
        assert(nbytes);
        void* to = heap->allocate_private(nbytes);
        memcpy(to, datum, nbytes);
        return to;
    }
    return datum;
}

static void*
copy_every_private(object_heap_t* heap, void* datum)
{
    assert(!heap->is_collectible(datum));
    if (heap->in_slab(datum)) {
        int nbytes = heap->allocated_size(datum);
        assert(nbytes);
        void* to = heap->allocate_private(nbytes);
        memcpy(to, datum, nbytes);
        return to;
    }
    assert(heap->in_heap(datum));
    int nbytes = heap->allocated_size(datum);
    assert(nbytes);
    void* to = heap->allocate_private(nbytes);
    memcpy(to, datum, nbytes);
    heap->deallocate_private(datum);
    return to;
}

static void
relocate_private(void* obj, int size, void* desc)
{
    relocate_desc_t* ctx = (relocate_desc_t*)desc;
    object_heap_t* heap = ctx->heap;
    copy_proc_t copy_proc = ctx->proc;
    assert(heap->is_collectible(obj));
    if (!PAIRP(obj)) {
        int tc = HDR_TC(HDR(obj));
        assert(tc >= 0);
        assert(tc <= TC_MASKBITS);
        switch (tc) {
            case TC_BIGNUM: {
                scm_bignum_t bignum = (scm_bignum_t)obj;
                if (bignum->elts != (uint32_t*)((uintptr_t)bignum + sizeof(scm_bignum_rec_t))) {
                    bignum->elts = (uint32_t*)copy_proc(heap, bignum->elts);
                }
                break;
            }
            case TC_SYMBOL: {
                scm_symbol_t symbol = (scm_symbol_t)obj;
                if (symbol->name != (char*)((uintptr_t)symbol + sizeof(scm_symbol_rec_t))) {
                    symbol->name = (char*)copy_proc(heap, symbol->name);
                }
                break;
            }
            case TC_STRING: {
                scm_string_t string = (scm_string_t)obj;
                if (string->name != (char*)((uintptr_t)string + sizeof(scm_string_rec_t))) {
                    string->name = (char*)copy_proc(heap, string->name);
                }
                break;
            }
            case TC_VECTOR: {
                scm_vector_t vector = (scm_vector_t)obj;
                if (vector->elts != (scm_obj_t*)((uintptr_t)vector + sizeof(scm_vector_rec_t))) {
                    vector->elts = (scm_obj_t*)copy_proc(heap, vector->elts);
                }
                break;
            }
            case TC_BVECTOR: {
                scm_bvector_t bvector = (scm_bvector_t)obj;
                if (HDR_BVECTOR_MAPPING(bvector->hdr) == 0) {
                    bvector->elts = (uint8_t*)copy_proc(heap, bvector->elts);
                }
                break;
            }
            case TC_TUPLE: {
                scm_tuple_t tuple = (scm_tuple_t)obj;
                if (tuple->elts != (scm_obj_t*)((uintptr_t)tuple + sizeof(scm_tuple_rec_t))) {
                    tuple->elts = (scm_obj_t*)copy_proc(heap, tuple->elts);
                }
                break;
            }
            case TC_VALUES: {
                scm_values_t values = (scm_values_t)obj;
                if (values->elts != (scm_obj_t*)((uintptr_t)values + sizeof(scm_values_rec_t))) {
                    values->elts = (scm_obj_t*)copy_proc(heap, values->elts);
                }
                break;
            }
            case TC_HASHTABLE: {
                scm_hashtable_t ht = (scm_hashtable_t)obj;
                ht->datum = (hashtable_rec_t*)copy_proc(heap, ht->datum);
                break;
            }
            case TC_WEAKHASHTABLE: {
                scm_weakhashtable_t ht = (scm_weakhashtable_t)obj;
                ht->datum = (weakhashtable_rec_t*)copy_proc(heap, ht->datum);
                break;
            }
        }
    }
}

void
object_heap_t::relocate_privates(bool pack)
{
    relocate_info_t* info = new relocate_info_t(m_pool_size >> OBJECT_SLAB_SIZE_SHIFT);
    object_slab_traits_t* first_traits = OBJECT_SLAB_TRAITS_OF(m_pool);
    object_slab_traits_t* traits;
    uint8_t* first_slab = m_pool;
    uint8_t* slab;

    if (pack) {
        slab = first_slab;
        traits = first_traits;
        for (int i = 0; i < m_pool_watermark; i++) {
            if (m_pool[i] == (PTAG_USED|PTAG_SLAB)) {
                traits->cache->detach(slab);
                info->relocated[i] = true;
            }
            slab += OBJECT_SLAB_SIZE;
            traits = (object_slab_traits_t*)((intptr_t)traits + OBJECT_SLAB_SIZE);
        }
    } else {
        slab = first_slab;
        traits = first_traits;
        for (int i = 0; i < m_pool_watermark; i++) {
            if (m_pool[i] == PTAG_SLAB && traits->free) {
                traits->cache->detach(slab);
                info->relocated[i] = true;
            }
            slab += OBJECT_SLAB_SIZE;
            traits = (object_slab_traits_t*)((intptr_t)traits + OBJECT_SLAB_SIZE);
        }
    }

    relocate_desc_t desc;
    desc.heap = this;
    desc.proc = (pack ? copy_every_private : copy_slab_private);
    slab = first_slab;
    traits = first_traits;
    for (int i = 0; i < m_pool_watermark; i++) {
        if (GCSLABP(m_pool[i])) traits->cache->iterate(slab, relocate_private, &desc);
        slab += OBJECT_SLAB_SIZE;
        traits = (object_slab_traits_t*)((intptr_t)traits + OBJECT_SLAB_SIZE);
    }
    m_symbol.relocate(pack);
    m_string.relocate(pack);

    slab = first_slab;
    traits = first_traits;
    for (int i = 0; i < m_pool_watermark; i++) {
        if (info->relocated[i]) deallocate(slab);
        slab += OBJECT_SLAB_SIZE;
        traits = (object_slab_traits_t*)((intptr_t)traits + OBJECT_SLAB_SIZE);
    }
}

void
object_heap_t::compact_pool()
{
    for (int i = m_pool_watermark - 1; i >= 0; i--) {
        if (m_pool[i] == PTAG_FREE) m_pool_watermark = i;
        else break;
    }
}
