/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#include "core.h"

#define FFI_MAX_ARGC    32
        
struct c_stack_frame_ia32_t {
    c_stack_frame_ia32_t(VM* vm) : m_vm(vm), m_count(0) {}
    intptr_t m_frame[FFI_MAX_ARGC];
    int m_count;
    VM* m_vm;
    const char* push(scm_obj_t obj);
};

struct c_stack_frame_x64_t {
    c_stack_frame_x64_t(VM* vm) 
        : m_vm(vm), m_count(0), m_reg_count(0), m_sse_count(0) {
        memset(m_sse, 0, sizeof(m_sse));
        memset(m_reg, 0, sizeof(m_reg));
    }
    intptr_t m_frame[FFI_MAX_ARGC + 8 + 6];
    int m_count;
    intptr_t m_sse[8];
    intptr_t m_reg[6];
    int m_reg_count;
    int m_sse_count;
    VM* m_vm;
    const char* push(scm_obj_t obj);
    void compose();
};

extern "C" {

    int         c_callback_int(uint32_t uid, uint32_t argc, uint32_t* base);

#if _MSC_VER
    double      stdcall_func_stub_double(void* func, int argc, intptr_t argv[]);
    intptr_t    stdcall_func_stub_intptr(void* func, int argc, intptr_t argv[]);
#endif

    double      c_func_stub_double(void* func, int argc, intptr_t argv[]);
    intptr_t    c_func_stub_intptr(void* func, int argc, intptr_t argv[]);
    int         c_callback_stub_int();

    intptr_t    c_func_stub_double_x64(void* func, intptr_t nstack, intptr_t nsse, intptr_t argv[]);
    intptr_t    c_func_stub_intptr_x64(void* func, intptr_t nstack, intptr_t nsse, intptr_t argv[]);
    
}

scm_obj_t make_callback(VM* vm, int type, int argc, scm_closure_t closure);

