/*
    Ypsilon Scheme System
    Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#ifndef FFI_H_INCLUDED
#define FFI_H_INCLUDED

#include "core.h"

#if ARCH_IA32
     #define FFI_MAX_ARGC 64
     class c_stack_frame_t {
        intptr_t m_frame[FFI_MAX_ARGC];
        int m_count;
        VM* m_vm;
    public:
        c_stack_frame_t(VM* vm) : m_vm(vm), m_count(0) {}
        const char* push(scm_obj_t obj, int signature);
        intptr_t* frame() { return m_frame; }
        int count() { return m_count; }
    };
#endif

#if ARCH_X64
    #define FFI_MAX_ARGC 32
    class c_stack_frame_t {
        intptr_t m_frame[FFI_MAX_ARGC + 1 + 8 + 6];
        int m_count;
        union {
            int8_t u8[8];
            int64_t u64;
        } m_pre;
        intptr_t m_sse[8];
        intptr_t m_reg[6];
        int m_reg_count;
        int m_sse_count;
        int m_sse_float_count;
        VM* m_vm;
        void compose();
    public:
        c_stack_frame_t(VM* vm)
            : m_vm(vm), m_count(0), m_reg_count(0), m_sse_count(0), m_sse_float_count(0) {
            m_pre.u64 = 0;
            memset(m_sse, 0, sizeof(m_sse));
            memset(m_reg, 0, sizeof(m_reg));
        }
        const char* push(scm_obj_t obj, int signature);
        intptr_t* frame() { compose(); return m_frame; }
        int count() { return m_count; }
        int sse_use() { return m_sse_count; }
    };
#endif

#if ARCH_PPC
    #define FFI_MAX_ARGC 64
    class c_stack_frame_t {
    public:
        c_stack_frame_t(VM* vm) {
            fatal("%s:%u no implementation", __FILE__, __LINE__);
        }
        const char* push(scm_obj_t obj, int signature) {
            fatal("%s:%u no implementation", __FILE__, __LINE__);
        }
        intptr_t* frame() {
            fatal("%s:%u no implementation", __FILE__, __LINE__);
        }
        int count() {
            fatal("%s:%u no implementation", __FILE__, __LINE__);
        }
    };
#endif

extern "C" {
#if ARCH_IA32
    float       c_func_stub_float(void* func, int argc, intptr_t argv[]);
    double      c_func_stub_double(void* func, int argc, intptr_t argv[]);
    int64_t     c_func_stub_int64(void* func, int argc, intptr_t argv[]);
    intptr_t    c_func_stub_intptr(void* func, int argc, intptr_t argv[]);
    void        c_callback_stub_double();
    void        c_callback_stub_float();
    void        c_callback_stub_int64();
    void        c_callback_stub_intptr();
    intptr_t    c_callback_intptr(intptr_t uid, intptr_t argc, intptr_t* stack);
    int64_t     c_callback_int64(intptr_t uid, intptr_t argc, intptr_t* stack);
    float       c_callback_float(intptr_t uid, intptr_t argc, intptr_t* stack);
    double      c_callback_double(intptr_t uid, intptr_t argc, intptr_t* stack);
#endif
#if ARCH_X64
    float       c_func_stub_float_x64(void* func, intptr_t nstack, intptr_t nsse, intptr_t argv[]);
    double      c_func_stub_double_x64(void* func, intptr_t nstack, intptr_t nsse, intptr_t argv[]);
    intptr_t    c_func_stub_intptr_x64(void* func, intptr_t nstack, intptr_t nsse, intptr_t argv[]);
    void        c_callback_stub_float_x64();
    void        c_callback_stub_double_x64();
    void        c_callback_stub_intptr_x64();
    float       c_callback_float_x64(intptr_t uid, intptr_t argc, intptr_t* reg, intptr_t* stack);
    double      c_callback_double_x64(intptr_t uid, intptr_t argc, intptr_t* reg, intptr_t* stack);
    intptr_t    c_callback_intptr_x64(intptr_t uid, intptr_t argc, intptr_t* reg, intptr_t* stack);
#endif
}

scm_obj_t make_callback(VM* vm, int type, const char* signature, scm_closure_t closure);

#define FFI_RETURN_TYPE_VOID        0x0000
#define FFI_RETURN_TYPE_BOOL        0x0001
#define FFI_RETURN_TYPE_SHORT       0x0002
#define FFI_RETURN_TYPE_INT         0x0003
#define FFI_RETURN_TYPE_INTPTR      0x0004
#define FFI_RETURN_TYPE_USHORT      0x0005
#define FFI_RETURN_TYPE_UINT        0x0006
#define FFI_RETURN_TYPE_UINTPTR     0x0007
#define FFI_RETURN_TYPE_FLOAT       0x0008
#define FFI_RETURN_TYPE_DOUBLE      0x0009
#define FFI_RETURN_TYPE_STRING      0x000a
#define FFI_RETURN_TYPE_SIZE_T      0x000b
#define FFI_RETURN_TYPE_INT8_T      0x000c
#define FFI_RETURN_TYPE_UINT8_T     0x000d
#define FFI_RETURN_TYPE_INT16_T     0x000e
#define FFI_RETURN_TYPE_UINT16_T    0x000f
#define FFI_RETURN_TYPE_INT32_T     0x0010
#define FFI_RETURN_TYPE_UINT32_T    0x0011
#define FFI_RETURN_TYPE_INT64_T     0x0012
#define FFI_RETURN_TYPE_UINT64_T    0x0013
#define FFI_RETURN_TYPE_MASK        0x00ff

#define FFI_CALL_TYPE_STDCALL       0x0100
#define FFI_CALL_TYPE_MASK          0xff00

#define CALLBACK_RETURN_TYPE_INTPTR     0x0000
#define CALLBACK_RETURN_TYPE_INT64_T    0x0001
#define CALLBACK_RETURN_TYPE_FLOAT      0x0002
#define CALLBACK_RETURN_TYPE_DOUBLE     0x0003
#define CALLBACK_RETURN_TYPE_MASK       0x00ff
#define CALLBACK_CALL_TYPE_STDCALL      0x0100
#define CALLBACK_CALL_TYPE_MASK         0xff00

#endif
