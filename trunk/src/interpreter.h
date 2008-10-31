/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#ifndef INTERPRETER_H_INCLUDED
#define INTERPRETER_H_INCLUDED

#include "core.h"
#include "heap.h"

#if USE_PARALLEL_VM

class Interpreter {

    struct vm_table_rec_t {
        Interpreter*    interp;
        cond_t          notify;
        VM*             vm;
        int             id;
        int             state;
        int             parent;
        scm_obj_t       param;
    };

    static thread_main_t mutator_thread(void* param);

public:
    enum { VM_PARENT_NONE = -1 };

    enum {
        VM_STATE_FREE,
        VM_STATE_START,
        VM_STATE_RUNNING,
        VM_STATE_SYNC
    };

    mutex_t m_lock;
    int m_count;
    vm_table_rec_t** m_table;

    void init(VM* root, int n);
    int  vm_id(VM* vm);
    int  spawn(VM* parent, scm_closure_t func, int argc, scm_obj_t argv[]);
    void display_status(VM* vm);
};

#endif

#endif
