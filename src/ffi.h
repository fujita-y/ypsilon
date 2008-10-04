/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#include "core.h"

#define FFI_MAX_ARGC    32

#if ARCH_IA32

    class c_stack_frame_t {
        intptr_t m_frame[FFI_MAX_ARGC];
        int m_count;
        VM* m_vm;
    public:
        c_stack_frame_t(VM* vm) : m_vm(vm), m_count(0) {}
        const char* push(scm_obj_t obj);
        intptr_t* frame() { return m_frame; }
        int count() { return m_count; }
    };

#endif

#if ARCH_X64

    class c_stack_frame_t {
        intptr_t m_frame[FFI_MAX_ARGC + 8 + 6];
        int m_count;
        intptr_t m_sse[8];
        intptr_t m_reg[6];
        int m_reg_count;
        int m_sse_count;
        VM* m_vm;
        void compose();
    public:
        c_stack_frame_t(VM* vm) 
            : m_vm(vm), m_count(0), m_reg_count(0), m_sse_count(0) {
            memset(m_sse, 0, sizeof(m_sse));
            memset(m_reg, 0, sizeof(m_reg));
        }
        const char* push(scm_obj_t obj);
        intptr_t* frame() { compose(); return m_frame; }
        int count() { return m_count; }
        int sse_use() { return m_sse_count; }
    };

#endif

extern "C" {
#if _MSC_VER
    double      stdcall_func_stub_double(void* func, int argc, intptr_t argv[]);
    intptr_t    stdcall_func_stub_intptr(void* func, int argc, intptr_t argv[]);
#endif
#if ARCH_IA32
    double      c_func_stub_double(void* func, int argc, intptr_t argv[]);
    intptr_t    c_func_stub_intptr(void* func, int argc, intptr_t argv[]);
    intptr_t    c_callback_stub_intptr();
    intptr_t    c_callback_intptr(intptr_t uid, intptr_t argc, intptr_t* stack);
#endif
#if ARCH_X64    
    double      c_func_stub_double_x64(void* func, intptr_t nstack, intptr_t nsse, intptr_t argv[]);
    intptr_t    c_func_stub_intptr_x64(void* func, intptr_t nstack, intptr_t nsse, intptr_t argv[]);
    intptr_t    c_callback_stub_intptr_x64();
    intptr_t    c_callback_intptr_x64(intptr_t uid, intptr_t argc, intptr_t* reg, intptr_t* stack);
#endif
}

scm_obj_t make_callback(VM* vm, int type, int argc, scm_closure_t closure);

