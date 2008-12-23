/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#ifndef OBJECT_SET_H_INCLUDED
#define OBJECT_SET_H_INCLUDED

#include "core.h"
#include "object.h"

class object_set_t {
public:
    mutex_t         m_lock;
    object_heap_t*  m_heap;
    scm_obj_t*      m_elts;
    int             m_count;
    int             m_live;

    void            rehash(int ncount);

public:
                    object_set_t();
                    ~object_set_t();
    void            init(object_heap_t* heap);
    scm_obj_t       get(const char* name, int len);
    void            put(scm_obj_t obj);
    void            remove(scm_obj_t obj);
    void            lock();
    void            unlock();
    void            sweep();
    void            inplace_rehash();
    void            resolve();
    void            relocate(bool every);
};

#endif
