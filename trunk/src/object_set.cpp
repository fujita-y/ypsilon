/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#include "core.h"
#include "hash.h"
#include "heap.h"
#include "list.h"

static inline bool
string_equiv(const char* s1, const char* s2, int len)
{
	for (int i = 0; i < len; i++) {
		if (s1[i] != s2[i]) return false;
	}
	return true;
}

static inline char*
get_name(scm_obj_t obj)
{
	if (STRINGP(obj)) return ((scm_string_t)obj)->name;
	if (SYMBOLP(obj)) return ((scm_symbol_t)obj)->name;
	return NULL;
}

static inline int
get_len(scm_obj_t obj)
{
	if (STRINGP(obj)) return HDR_STRING_SIZE(((scm_string_t)obj)->hdr);
	if (SYMBOLP(obj)) return HDR_SYMBOL_SIZE(((scm_symbol_t)obj)->hdr);
	return 0;
}

object_set_t::object_set_t()
{
	m_heap = NULL;
	m_elts = NULL;
    m_lock.init();
}

object_set_t::~object_set_t()
{
	assert(m_heap);
	assert(m_elts);
    m_lock.destroy();
}

void
object_set_t::lock()
{ 
    m_lock.lock();
}

void
object_set_t::unlock()
{
    m_lock.unlock();
}

void
object_set_t::init(object_heap_t* heap)
{
	m_heap = heap;
	m_count = lookup_mutable_hashtable_size(0);
	m_live = 0;
	m_elts = (scm_obj_t*)m_heap->allocate_private(sizeof(scm_obj_t*) * m_count);
    for (int i = 0; i < m_count; i++) m_elts[i] = scm_hash_free;
}

scm_obj_t
object_set_t::get(const char* name, int len)
{
	assert(m_heap);
    m_lock.verify_locked();
    
	int hash = string_hash(name, m_count);
	int hash2 = string_hash2(name, m_count);
	assert(hash2);
	int index = hash;
	do {
		scm_obj_t entry = m_elts[index];
		if (entry == scm_hash_free) break;
		if (entry != scm_hash_deleted) {
			assert(SYMBOLP(entry) || STRINGP(entry));
			if (get_len(entry) == len) {
				if (string_equiv(name, get_name(entry), len)) {
					if (m_heap->m_read_barrier) {
						if (DETAILED_STATISTIC) m_heap->m_usage.m_barriered_read++;
						OBJECT_SLAB_TRAITS_OF(entry)->cache->mark(entry);
					}
					return entry;
				}
			}
		}
		index += hash2;
		if (index >= m_count) index -= m_count;
	} while (index != hash);
	return scm_undef;
}

void
object_set_t::put(scm_obj_t obj)
{
	assert(m_heap);
    m_lock.verify_locked();
    
	const char* name = get_name(obj);
	int hash = string_hash(name, m_count);
	int hash2 = string_hash2(name, m_count);
	assert(hash2);
	int index = hash;
	do {
		scm_obj_t entry = m_elts[index];
		if (entry == scm_hash_free || entry == scm_hash_deleted) {
			m_elts[index] = obj;
            m_live++;
			if (m_live >= HASH_BUSY_THRESHOLD(m_count)) rehash(lookup_mutable_hashtable_size(m_count));
			return;
		}
		index += hash2;
		if (index >= m_count) index -= m_count;
	} while (index != hash);
	fatal("%s:%u HashSet overflow", __FILE__, __LINE__);
}

void
object_set_t::remove(scm_obj_t obj)
{
	assert(m_heap);
    m_lock.verify_locked();
    
	const char* name = get_name(obj);
	int hash = string_hash(name, m_count);
	int hash2 = string_hash2(name, m_count);
	assert(hash2);
	int index = hash;
	do {
		if (m_elts[index] == obj) {
			m_elts[index] = scm_hash_deleted;
			m_live--;
			if (m_live < HASH_SPARSE_THRESHOLD(m_count)) {
				rehash(lookup_mutable_hashtable_size(HASH_MUTABLE_SIZE(m_live))); // 175%
			}
			return;
		}
		index += hash2;
		if (index >= m_count) index -= m_count;
	} while (index != hash);
	fatal("%s:%u HashSet unintern failed.", __FILE__, __LINE__);
}

void
object_set_t::rehash(int ncount)
{
	assert(m_heap);
    m_lock.verify_locked();
    
    assert(ncount >= m_live);
	int save_count = m_count;
	scm_obj_t* save_elts = m_elts;
	m_count = ncount;
	m_elts = (scm_obj_t*)m_heap->allocate_private(sizeof(scm_obj_t*) * m_count);
    for (int i = 0; i < m_count; i++) m_elts[i] = scm_hash_free;
	for (int i = 0; i < save_count; i++) {
        scm_obj_t obj= save_elts[i];
		if (obj == scm_hash_free) continue;
        if (obj == scm_hash_deleted) continue;
		assert(SYMBOLP(obj) || STRINGP(obj));
        const char* name = get_name(obj);
        int hash = string_hash(name, m_count);
        int hash2 = string_hash2(name, m_count);
        int index = hash;
        assert(hash2);
        do {
            scm_obj_t entry = m_elts[index];
            if (entry == scm_hash_free) {
                m_elts[index] = obj;
                break;
            }
            index += hash2;
            if (index >= m_count) index -= m_count;
            assert(index != hash);
        } while (1);
	}
	m_heap->deallocate_private(save_elts);
}

void
object_set_t::inplace_rehash()
{
	assert(m_heap);
    scoped_lock lock(m_lock);
    scm_obj_t* save_elts = (scm_obj_t*)malloc(sizeof(scm_obj_t*) * m_count);
    memcpy(save_elts, m_elts, sizeof(scm_obj_t*) * m_count);
    for (int i = 0; i < m_count; i++) m_elts[i] = scm_hash_free;
	for (int i = 0; i < m_count; i++) {
        scm_obj_t obj= save_elts[i];
		if (obj == scm_hash_free) continue;
        if (obj == scm_hash_deleted) continue;
        const char* name = get_name(obj);
        int hash = string_hash(name, m_count);
        int hash2 = string_hash2(name, m_count);
        int index = hash;
        assert(hash2);
        do {
            scm_obj_t entry = m_elts[index];
            if (entry == scm_hash_free) {
                m_elts[index] = obj;
                break;
            }
            index += hash2;
            if (index >= m_count) index -= m_count;
            assert(index != hash);
        } while (1);
	}
    free(save_elts);
}

void
object_set_t::sweep()
{
	assert(m_heap);
    scoped_lock lock(m_lock);
	for (int i = 0; i < m_count; i++) {
        scm_obj_t obj= m_elts[i];
		if (obj == scm_hash_free) continue;
        if (obj == scm_hash_deleted) continue;
		assert(SYMBOLP(obj) || STRINGP(obj));
		object_slab_traits_t* traits = OBJECT_SLAB_TRAITS_OF(obj);
		if (traits->cache->state(obj)) continue;
        m_elts[i] = scm_hash_deleted;
		m_live--;
	}	
	if (m_count > 7 && m_live < HASH_SPARSE_THRESHOLD(m_count)) {
		rehash(lookup_mutable_hashtable_size(HASH_MUTABLE_SIZE(m_live))); // 175%
	}
}

void
object_set_t::resolve()
{
	assert(m_heap);
    scoped_lock lock(m_lock);
	for (int i = 0; i < m_count; i++) m_elts[i] = m_heap->forward(m_elts[i]);
}

void
object_set_t::relocate(bool every)
{
    if (every) {
        if (m_heap->in_slab(m_elts)) {
            int nbytes = m_heap->allocated_size(m_elts);
            assert(nbytes);
            void* to = m_heap->allocate_private(nbytes);
            memcpy(to, m_elts, nbytes);
            m_elts = (scm_obj_t*)to;
            return;
        }
        assert(m_heap->in_heap(m_elts));
        int nbytes = m_heap->allocated_size(m_elts);
        assert(nbytes);
        void* to = m_heap->allocate_private(nbytes);
        memcpy(to, m_elts, nbytes);
        m_heap->deallocate_private(m_elts);
        m_elts = (scm_obj_t*)to;
    } else {
        if (m_heap->in_non_full_slab(m_elts)) {
            int nbytes = m_heap->allocated_size(m_elts);
            assert(nbytes);
            void* to = m_heap->allocate_private(nbytes);
            memcpy(to, m_elts, nbytes);
            m_elts = (scm_obj_t*)to;
        }
    }
}
