/*
    Ypsilon Scheme System
    Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#ifndef INTERPRETER_H_INCLUDED
#define INTERPRETER_H_INCLUDED

#include "core.h"
#include "heap.h"

#if USE_PARALLEL_VM

enum {
        VM_STATE_FREE = 0,
        VM_STATE_ACTIVE,
        VM_STATE_BLOCK,
        VM_STATE_SYNC
};

class remember_set_t {
    struct element_t {
        scm_obj_t   obj;
        uint32_t    bits;
    };
    element_t*      m_elts;
    int             m_count;
    int             m_live;

    void    rehash(int ncount);

public:
            remember_set_t();
            ~remember_set_t();
    void    init(int n);
    void    set(scm_obj_t obj, uint32_t bits);
    void    clear(uint32_t bits);
    void    snapshot(VM* vm, bool retry);
    void    cleanup(VM* vm);
    void    display_status(VM* vm);
};

class Interpreter {
    enum { VM_PARENT_NONE = -1 };
    struct vm_table_rec_t {
        Interpreter*    interp;
        cond_t          notify;
        VM*             vm;
        int             id;
        int             state;
        int             parent;
        scm_obj_t       param;
        char            name[64];
    };
    mutex_t             m_lock;
    vm_table_rec_t**    m_table;
    int                 m_capacity;
    int                 m_live;
    remember_set_t      m_remember_set;
    mutex_t             m_uuid_lock;

    static thread_main_t mutator_thread(void* param);

public:
            Interpreter() { m_table = NULL; }
    void    init(VM* root, int n);
    void    destroy();
    int     spawn(VM* parent, scm_closure_t func, int argc, scm_obj_t argv[]);
    void    update(VM* vm, int state);
    void    snapshot(VM* vm, bool retry);
    void    remember(scm_obj_t obj);
    void    remember(scm_obj_t lhs, scm_obj_t rhs);
    bool    primordial(int id);
    void    display_status(VM* vm);
    int     live_thread_count() { return m_live; }
    int     max_thread_count() { return m_capacity; }
    void    set_thread_name(int id, const char* name);
    void    get_thread_name(int id, char* name, int len);
    void    generate_uuid(char* buf, int bufsize);
};

#endif

#endif
