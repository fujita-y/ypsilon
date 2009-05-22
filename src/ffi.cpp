/*
    Ypsilon Scheme System
    Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#include "core.h"
#include "vm.h"
#include "ffi.h"
#include "hash.h"
#include "arith.h"

#define C_STACK_COERCE_ARGUMENTS    1

#if ARCH_IA32
    const char*
    c_stack_frame_t::push(scm_obj_t obj, int signature)
    {
        if (m_count < array_sizeof(m_frame)) {
            if (FIXNUMP(obj) || BIGNUMP(obj)) {
                if (signature == 'x') {
                    union { uint64_t u64; struct { uint32_t lo; uint32_t hi; } u32; } n;
                    n.u64 = coerce_exact_integer_to_int64(obj);
                    m_frame[m_count++] = n.u32.lo;
                    m_frame[m_count++] = n.u32.hi;
                    return NULL;
                }
                if (signature == 'i' || signature == 'p' || signature == '*') {
                    m_frame[m_count++] = coerce_exact_integer_to_intptr(obj);
                    return NULL;
                }
                if (signature == 'f') {
                    union { float f32; uintptr_t u32; } n;
                    n.f32 = real_to_double(obj);
                    m_frame[m_count++] = n.u32;
                    return NULL;
                }
                if (signature == 'd') {
                    union { double f64; struct { uint32_t lo; uint32_t hi; } u32; } n;
                    n.f64 = real_to_double(obj);
                    m_frame[m_count++] = n.u32.lo;
                    m_frame[m_count++] = n.u32.hi;
                    return NULL;
                }
                goto bad_signature;
            }
            if (FLONUMP(obj)) {
                if (signature == 'f') {
                    union { float f32; uintptr_t u32; } n;
                    scm_flonum_t flonum = (scm_flonum_t)obj;
                    n.f32 = flonum->value;
                    m_frame[m_count++] = n.u32;
                    return NULL;
                }
                if (signature == 'd' || signature == '*') {
                    union { double f64; struct { uint32_t lo; uint32_t hi; } u32; } n;
                    scm_flonum_t flonum = (scm_flonum_t)obj;
                    n.f64 = flonum->value;
                    m_frame[m_count++] = n.u32.lo;
                    m_frame[m_count++] = n.u32.hi;
                    return NULL;
                }
                goto bad_signature;
            }
            if (BVECTORP(obj)) {
                if (signature != 'p' && signature != '*') goto bad_signature;
                scm_bvector_t bvector = (scm_bvector_t)obj;
                m_frame[m_count++] = (intptr_t)bvector->elts;
                return NULL;
            }
            if (VECTORP(obj)) {
                if (signature != 'c') goto bad_signature;
                scm_vector_t vector = (scm_vector_t)obj;
                int n = vector->count;
                if (n == 0) return "nonempty vector";
                assert(n);
                if (!FIXNUMP(vector->elts[0])) return "vector contains fixnum in first element";
                int ref = FIXNUM(vector->elts[0]);
                scm_bvector_t bvector = make_bvector(m_vm->m_heap, sizeof(intptr_t) * (n - 1));
                for (int i = 0; i < n - 1; i++) {
                    if (BVECTORP(vector->elts[i + 1])) {
                        *(uint8_t**)(bvector->elts + sizeof(intptr_t) * i) = ((scm_bvector_t)vector->elts[i + 1])->elts;
                    } else {
                        return "vector of bytevector";
                    }
                }
                while (ref) {
                    intptr_t datum = (intptr_t)bvector->elts;
                    bvector = make_bvector(m_vm->m_heap, sizeof(intptr_t));
                    *(intptr_t*)(bvector->elts) = datum;
                    ref--;
                }
                m_frame[m_count++] = (intptr_t)bvector->elts;
                return NULL;
            }
            goto bad_signature;
        }
        fatal("fatal: c function stack frame overflow");

    bad_signature:
        switch (signature) {
            case 'i':
            case 'x':
                return "exact integer";
            case 'p':
                return "exact integer or bytevector";
            case 'c':
                return "vector";
            case 'f':
            case 'd':
                return "real";
            case '*':
                return "exact integer, real, or bytevector";
            default:
                fatal("fatal: invalid c function argument type specifier");
        }
    }

    scm_obj_t callback_scheme(intptr_t uid, intptr_t signatures, intptr_t* stack)
    {
        VM* vm = current_vm();
        int argc = strlen((const char*)signatures);
        scm_obj_t obj;
        {
            scoped_lock lock(vm->m_heap->m_trampolines->lock);
            obj = get_hashtable(vm->m_heap->m_trampolines, MAKEFIXNUM(uid));
            if (!CLOSUREP(obj)) fatal("fatal: callback was destroyed\n[exit]\n");
        }
        try {
            scm_obj_t* argv = (scm_obj_t*)alloca(sizeof(scm_obj_t*) * argc);
            int offset = 0;
            for (int i = 0; i < argc; i++) {
                char c = *(const char*)(signatures + i);
                switch (c) {
                    case 'L': {
                        int8_t s8 = stack[offset];
                        argv[i] = s8 ? MAKEFIXNUM(1) : MAKEFIXNUM(0);
                        offset += 1;
                    } break;
                    case 'u': {
                        int8_t s8 = stack[offset];
                        argv[i] = intptr_to_integer(vm->m_heap, s8);
                        offset += 1;
                    } break;
                    case 'U': {
                        uint8_t u8 = stack[offset];
                        argv[i] = uintptr_to_integer(vm->m_heap, u8);
                        offset += 1;
                    } break;
                    case 'b': {
                        int16_t s16 = stack[offset];
                        argv[i] = intptr_to_integer(vm->m_heap, s16);
                        offset += 1;
                    } break;
                    case 'B': {
                        uint16_t u16 = stack[offset];
                        argv[i] = uintptr_to_integer(vm->m_heap, u16);
                        offset += 1;
                    } break;
                    case 'q': {
                        int32_t s32 = stack[offset];
                        argv[i] = int32_to_integer(vm->m_heap, s32);
                        offset += 1;
                    } break;
                    case 'Q': {
                        uint32_t u32 = stack[offset];
                        argv[i] = uint32_to_integer(vm->m_heap, u32);
                        offset += 1;
                    } break;
                    case 'o': {
                        int64_t* s64 = (int64_t*)(&stack[offset]);
                        argv[i] = int64_to_integer(vm->m_heap, *s64);
                        offset += 2;
                    } break;
                    case 'O': {
                        uint64_t* u64 = (uint64_t*)(&stack[offset]);
                        argv[i] = uint64_to_integer(vm->m_heap, *u64);
                        offset += 2;
                    } break;
                    case 'f': {
                        float* f32 = (float*)(&stack[offset]);
                        argv[i] = make_flonum(vm->m_heap, *f32);
                        offset += 1;
                    } break;
                    case 'd': {
                        double* f64 = (double*)(&stack[offset]);
                        argv[i] = make_flonum(vm->m_heap, *f64);
                        offset += 2;
                    } break;

                    default: fatal("fatal: invalid callback argument signature %c\n[exit]\n", c);
                }
            }
            return vm->call_scheme_argv((scm_closure_t)obj, argc, argv);
        } catch (vm_exit_t& e) {
            exit(e.m_code);
        } catch (...) {
            fatal("fatal: unhandled exception in callback\n[exit]\n");
        }
    }
#endif

#if ARCH_X64
    const char*
    c_stack_frame_t::push(scm_obj_t obj, int signature)
    {
        if (m_count < array_sizeof(m_frame) - array_sizeof(m_reg) - array_sizeof(m_sse)) {
            if (FIXNUMP(obj) || BIGNUMP(obj)) {
                if (signature == 'x' || signature == 'i' || signature == 'p' || signature == '*') {
                    intptr_t value = coerce_exact_integer_to_intptr(obj);
                    if (m_reg_count < array_sizeof(m_reg)) m_reg[m_reg_count++] = value;
                    else m_frame[m_count++] = value;
                    return NULL;
                }
                union { double f64; uint64_t u64; } n;
                n.f64 = real_to_double(obj);
                if (signature == 'f') {
                    if (m_sse_count < array_sizeof(m_sse)) {
                        m_pre.u8[m_sse_count] = 1;
                        m_sse[m_sse_count] = n.u64;
                        m_sse_float_count++;
                        m_sse_count++;
                    } else {
                        m_frame[m_count++] = n.u64;
                    }
                    return NULL;
                }
                if (signature == 'd') {
                    if (m_sse_count < array_sizeof(m_sse)) m_sse[m_sse_count++] = n.u64;
                    else m_frame[m_count++] = n.u64;
                    return NULL;
                }
                goto bad_signature;
            }
            if (FLONUMP(obj)) {
                union { double f64; uint64_t u64; } n;
                scm_flonum_t flonum = (scm_flonum_t)obj;
                n.f64 = flonum->value;
                if (signature == 'f') {
                    if (m_sse_count < array_sizeof(m_sse)) {
                        m_pre.u8[m_sse_count] = 1;
                        m_sse[m_sse_count] = n.u64;
                        m_sse_float_count++;
                        m_sse_count++;
                    } else {
                        m_frame[m_count++] = n.u64;
                    }
                    return NULL;
                }
                if (signature == 'd' || signature == '*') {
                    if (m_sse_count < array_sizeof(m_sse)) m_sse[m_sse_count++] = n.u64;
                    else m_frame[m_count++] = n.u64;
                    return NULL;
                }
                goto bad_signature;
            }
            if (BVECTORP(obj)) {
                if (signature != 'p' && signature != '*') goto bad_signature;
                scm_bvector_t bvector = (scm_bvector_t)obj;
                if (m_reg_count < array_sizeof(m_reg)) m_reg[m_reg_count++] = (intptr_t)bvector->elts;
                else m_frame[m_count++] = (intptr_t)bvector->elts;
                return NULL;
            }
            if (VECTORP(obj)) {
                if (signature != 'c') goto bad_signature;
                scm_vector_t vector = (scm_vector_t)obj;
                int n = vector->count;
                if (n == 0) return "nonempty vector";
                assert(n);
                if (!FIXNUMP(vector->elts[0])) return "vector contains fixnum in first element";
                int ref = FIXNUM(vector->elts[0]);
                scm_bvector_t bvector = make_bvector(m_vm->m_heap, sizeof(intptr_t) * (n - 1));
                for (int i = 0; i < n - 1; i++) {
                    if (!BVECTORP(vector->elts[i + 1])) return "vector of bytevector";
                    *(uint8_t**)(bvector->elts + sizeof(intptr_t) * i) = ((scm_bvector_t)vector->elts[i + 1])->elts;
                }
                while (ref) {
                    intptr_t datum = (intptr_t)bvector->elts;
                    bvector = make_bvector(m_vm->m_heap, sizeof(intptr_t));
                    *(intptr_t*)(bvector->elts) = datum;
                    ref--;
                }
                if (m_reg_count < array_sizeof(m_reg)) m_reg[m_reg_count++] = (intptr_t)bvector->elts;
                else m_frame[m_count++] = (intptr_t)bvector->elts;
                return NULL;
            }
            goto bad_signature;
        }
        fatal("fatal: c function stack frame overflow");

    bad_signature:
        switch (signature) {
            case 'i':
            case 'x':
                return "exact integer";
            case 'p':
                return "exact integer or bytevector";
            case 'c':
                return "vector";
            case 'f':
            case 'd':
                return "real";
            case '*':
                return "exact integer, real, or bytevector";
            default:
                fatal("fatal: invalid c function argument type specifier");
        }
    }

    void
    c_stack_frame_t::compose()
    {
        int dst = m_count;
        if (m_sse_count == m_sse_float_count) {
            m_frame[dst++] = -1;
        } else {
            m_frame[dst++] = m_pre.u64;
        }
        for (int i = 0; i < array_sizeof(m_sse); i++) m_frame[dst++] = m_sse[i];
        for (int i = 0; i < array_sizeof(m_reg); i++) m_frame[dst++] = m_reg[i];
    }

    scm_obj_t callback_scheme(intptr_t uid, intptr_t signatures, intptr_t* reg, intptr_t* stack)
    {
        VM* vm = current_vm();
        int argc = strlen((const char*)signatures);
        scm_obj_t obj;
        {
            scoped_lock lock(vm->m_heap->m_trampolines->lock);
            obj = get_hashtable(vm->m_heap->m_trampolines, MAKEFIXNUM(uid));
            if (!CLOSUREP(obj)) fatal("fatal: callback was destroyed\n[exit]\n");
        }
        try {
            scm_obj_t* argv = (scm_obj_t*)alloca(sizeof(scm_obj_t*) * argc);
            int reg_offset = 0;
            int sse_offset = 0;
            int stack_offset = 0;
            for (int i = 0; i < argc; i++) {
                char c = *(const char*)(signatures + i);
                switch (c) {
                    case 'L': {
                        int8_t s8;
                        if (reg_offset < 6) {
                            s8 = reg[reg_offset];
                            reg_offset += 1;
                        } else {
                            s8 = stack[stack_offset];
                            stack_offset += 1;
                        }
                        argv[i] = s8 ? MAKEFIXNUM(1) : MAKEFIXNUM(0);
                    } break;
                    case 'u': {
                        int8_t s8;
                        if (reg_offset < 6) {
                            s8 = reg[reg_offset];
                            reg_offset += 1;
                        } else {
                            s8 = stack[stack_offset];
                            stack_offset += 1;
                        }
                        argv[i] = intptr_to_integer(vm->m_heap, s8);
                    } break;
                    case 'U': {
                        uint8_t u8;
                        if (reg_offset < 6) {
                            u8 = reg[reg_offset];
                            reg_offset += 1;
                        } else {
                            u8 = stack[stack_offset];
                            stack_offset += 1;
                        }
                        argv[i] = uintptr_to_integer(vm->m_heap, u8);
                    } break;
                    case 'b': {
                        int16_t s16;
                        if (reg_offset < 6) {
                            s16 = reg[reg_offset];
                            reg_offset += 1;
                        } else {
                            s16 = stack[stack_offset];
                            stack_offset += 1;
                        }
                        argv[i] = intptr_to_integer(vm->m_heap, s16);
                    } break;
                    case 'B': {
                        uint16_t u16;
                        if (reg_offset < 6) {
                            u16 = reg[reg_offset];
                            reg_offset += 1;
                        } else {
                            u16 = stack[stack_offset];
                            stack_offset += 1;
                        }
                        argv[i] = uintptr_to_integer(vm->m_heap, u16);
                    } break;
                    case 'q': {
                        int32_t s32;
                        if (reg_offset < 6) {
                            s32 = reg[reg_offset];
                            reg_offset += 1;
                        } else {
                            s32 = stack[stack_offset];
                            stack_offset += 1;
                        }
                        argv[i] = int32_to_integer(vm->m_heap, s32);
                    } break;
                    case 'Q': {
                        uint32_t u32;
                        if (reg_offset < 6) {
                            u32 = reg[reg_offset];
                            reg_offset += 1;
                        } else {
                            u32 = stack[stack_offset];
                            stack_offset += 1;
                        }
                        argv[i] = uint32_to_integer(vm->m_heap, u32);
                    } break;
                    case 'o': {
                        int64_t s64;
                        if (reg_offset < 6) {
                            s64 = reg[reg_offset];
                            reg_offset += 1;
                        } else {
                            s64 = stack[stack_offset];
                            stack_offset += 1;
                        }
                        argv[i] = int64_to_integer(vm->m_heap, s64);
                    } break;
                    case 'O': {
                        uint64_t u64;
                        if (reg_offset < 6) {
                            u64 = reg[reg_offset];
                            reg_offset += 1;
                        } else {
                            u64 = stack[stack_offset];
                            stack_offset += 1;
                        }
                        argv[i] = uint64_to_integer(vm->m_heap, u64);
                    } break;
                    case 'f': {
                        float* f32;
                        if (sse_offset < 8) {
                            f32 = (float*)(&reg[6 + sse_offset]);
                            sse_offset += 1;
                        } else {
                            f32 = (float*)(&stack[stack_offset]);
                            stack_offset += 1;
                        }
                        argv[i] = make_flonum(vm->m_heap, *f32);
                    } break;
                    case 'd': {
                        double* f64;
                        if (sse_offset < 8) {
                            f64 = (double*)(&reg[6 + sse_offset]);
                            sse_offset += 1;
                        } else {
                            f64 = (double*)(&stack[stack_offset]);
                            stack_offset += 1;
                        }
                        argv[i] = make_flonum(vm->m_heap, *f64);
                    } break;

                    default: fatal("fatal: invalid callback argument signature %c\n[exit]\n", c);
                }
            }
            return vm->call_scheme_argv((scm_closure_t)obj, argc, argv);
        } catch (vm_exit_t& e) {
            exit(e.m_code);
        } catch (...) {
            fatal("fatal: unhandled exception in callback\n[exit]\n");
        }
    }
#endif

#if ARCH_PPC && ARCH_ILP32
    const char*
    c_stack_frame_t::push(scm_obj_t obj, int signature)
    {
        if (m_count < FFI_MAX_ARGC) {
            if (FIXNUMP(obj) || BIGNUMP(obj)) {
                if (signature == 'x') {
                    int64_t value = coerce_exact_integer_to_int64(obj);
                    if (m_gpr_count <= 6) {
                        if (m_gpr_count & 1) m_gpr_count++;
                        m_gpr[m_gpr_count++] = value >> 32;
                        m_gpr[m_gpr_count++] = value & 0xffffffff;
                        return NULL;
                    }
                    if (m_count & 1) m_count++;
                    m_frame[m_count++] = value >> 32;
                    m_frame[m_count++] = value & 0xffffffff;
                    return NULL;
                }
                if (signature == 'i' || signature == 'p' || signature == '*') {
                    intptr_t value = coerce_exact_integer_to_intptr(obj);
                    if (m_gpr_count < array_sizeof(m_gpr)) m_gpr[m_gpr_count++] = value;
                    else m_frame[m_count++] = value;
                    return NULL;
                }
                if (signature == 'f' || signature == 'd') {
                    union { double f64; uint64_t u64; } n;
                    n.f64 = real_to_double(obj);
                    if (m_fpr_count < array_sizeof(m_fpr)) {
                        m_fpr[m_fpr_count++] = n.f64;
                        return NULL;
                    }
                    if (m_count & 1) m_count++;
                    m_frame[m_count++] = n.u64 >> 32;
                    m_frame[m_count++] = n.u64 & 0xffffffff;
                    return NULL;
                }
                goto bad_signature;
            }
            if (FLONUMP(obj)) {
                union { double f64; uint64_t u64; } n;
                scm_flonum_t flonum = (scm_flonum_t)obj;
                n.f64 = flonum->value;
                if (signature == 'f' || signature == 'd' || signature == '*') {
                    if (m_fpr_count < array_sizeof(m_fpr)) {
                        m_fpr[m_fpr_count++] = n.f64;
                        return NULL;
                    }
                    if (m_count & 1) m_count++;
                    m_frame[m_count++] = n.u64 >> 32;
                    m_frame[m_count++] = n.u64 & 0xffffffff;
                    return NULL;
                }
                goto bad_signature;
            }
            if (BVECTORP(obj)) {
                if (signature != 'p' && signature != '*') goto bad_signature;
                scm_bvector_t bvector = (scm_bvector_t)obj;
                if (m_gpr_count < array_sizeof(m_gpr)) m_gpr[m_gpr_count++] = (intptr_t)bvector->elts;
                else m_frame[m_count++] = (intptr_t)bvector->elts;
                return NULL;
            }
            if (VECTORP(obj)) {
                if (signature != 'c') goto bad_signature;
                scm_vector_t vector = (scm_vector_t)obj;
                int n = vector->count;
                if (n == 0) return "nonempty vector";
                assert(n);
                if (!FIXNUMP(vector->elts[0])) return "vector contains fixnum in first element";
                int ref = FIXNUM(vector->elts[0]);
                scm_bvector_t bvector = make_bvector(m_vm->m_heap, sizeof(intptr_t) * (n - 1));
                for (int i = 0; i < n - 1; i++) {
                    if (!BVECTORP(vector->elts[i + 1])) return "vector of bytevector";
                    *(uint8_t**)(bvector->elts + sizeof(intptr_t) * i) = ((scm_bvector_t)vector->elts[i + 1])->elts;
                }
                while (ref) {
                    intptr_t datum = (intptr_t)bvector->elts;
                    bvector = make_bvector(m_vm->m_heap, sizeof(intptr_t));
                    *(intptr_t*)(bvector->elts) = datum;
                    ref--;
                }
                if (m_gpr_count < array_sizeof(m_gpr)) m_gpr[m_gpr_count++] = (intptr_t)bvector->elts;
                else m_frame[m_count++] = (intptr_t)bvector->elts;
                return NULL;
            }
            goto bad_signature;
        }
        fatal("fatal: c function stack frame overflow");

    bad_signature:
        switch (signature) {
            case 'i':
            case 'x':
                return "exact integer";
            case 'p':
                return "exact integer or bytevector";
            case 'c':
                return "vector";
            case 'f':
            case 'd':
                return "real";
            case '*':
                return "exact integer, real, or bytevector";
            default:
                fatal("fatal: invalid c function argument type specifier");
        }
    }

    void
    c_stack_frame_t::compose()
    {
        int dst = m_count;
        for (int i = 0; i < array_sizeof(m_gpr); i++) m_frame[dst++] = m_gpr[i];
        for (int i = 0; i < array_sizeof(m_fpr); i++) {
            union { double f64; uint64_t u64; } n;
            n.f64 = m_fpr[i];
            m_frame[dst++] = n.u64 >> 32;
            m_frame[dst++] = n.u64 & 0xffffffff;
        }
    }

    scm_obj_t callback_scheme(intptr_t uid, intptr_t signatures, intptr_t* reg, intptr_t* stack)
    {
        intptr_t* gpr = reg;
        double* fpr = (double*)(reg + 8);
        VM* vm = current_vm();
        int argc = strlen((const char*)signatures);
        scm_obj_t obj;
        {
            scoped_lock lock(vm->m_heap->m_trampolines->lock);
            obj = get_hashtable(vm->m_heap->m_trampolines, MAKEFIXNUM(uid));
            if (!CLOSUREP(obj)) fatal("fatal: callback was destroyed\n[exit]\n");
        }
        try {
            scm_obj_t* argv = (scm_obj_t*)alloca(sizeof(scm_obj_t*) * argc);
            int gpr_offset = 0;
            int fpr_offset = 0;
            int stack_offset = 0;
            for (int i = 0; i < argc; i++) {
                char c = *(const char*)(signatures + i);
                switch (c) {
                    case 'L': {
                        int8_t s8;
                        if (gpr_offset < 8) {
                            s8 = gpr[gpr_offset];
                            gpr_offset += 1;
                        } else {
                            s8 = stack[stack_offset];
                            stack_offset += 1;
                        }
                        argv[i] = s8 ? MAKEFIXNUM(1) : MAKEFIXNUM(0);
                    } break;
                    case 'u': {
                        int8_t s8;
                        if (gpr_offset < 8) {
                            s8 = gpr[gpr_offset];
                            gpr_offset += 1;
                        } else {
                            s8 = stack[stack_offset];
                            stack_offset += 1;
                        }
                        argv[i] = intptr_to_integer(vm->m_heap, s8);
                    } break;
                    case 'U': {
                        uint8_t u8;
                        if (gpr_offset < 8) {
                            u8 = gpr[gpr_offset];
                            gpr_offset += 1;
                        } else {
                            u8 = stack[stack_offset];
                            stack_offset += 1;
                        }
                        argv[i] = uintptr_to_integer(vm->m_heap, u8);
                    } break;
                    case 'b': {
                        int16_t s16;
                        if (gpr_offset < 8) {
                            s16 = gpr[gpr_offset];
                            gpr_offset += 1;
                        } else {
                            s16 = stack[stack_offset];
                            stack_offset += 1;
                        }
                        argv[i] = intptr_to_integer(vm->m_heap, s16);
                    } break;
                    case 'B': {
                        uint16_t u16;
                        if (gpr_offset < 8) {
                            u16 = gpr[gpr_offset];
                            gpr_offset += 1;
                        } else {
                            u16 = stack[stack_offset];
                            stack_offset += 1;
                        }
                        argv[i] = uintptr_to_integer(vm->m_heap, u16);
                    } break;
                    case 'q': {
                        int32_t s32;
                        if (gpr_offset < 8) {
                            s32 = gpr[gpr_offset];
                            gpr_offset += 1;
                        } else {
                            s32 = stack[stack_offset];
                            stack_offset += 1;
                        }
                        argv[i] = int32_to_integer(vm->m_heap, s32);
                    } break;
                    case 'Q': {
                        uint32_t u32;
                        if (gpr_offset < 8) {
                            u32 = gpr[gpr_offset];
                            gpr_offset += 1;
                        } else {
                            u32 = stack[stack_offset];
                            stack_offset += 1;
                        }
                        argv[i] = uint32_to_integer(vm->m_heap, u32);
                    } break;
                    case 'o': {
                        int64_t s64;
                        if (gpr_offset < 7) {
                            s64 = ((int64_t)gpr[gpr_offset] << 32) + gpr[gpr_offset + 1];
                            gpr_offset += 2;
                        } else {
                            gpr_offset = 8;
                            s64 = ((int64_t)stack[stack_offset] << 32) + stack[stack_offset + 1];
                        }
                        argv[i] = int64_to_integer(vm->m_heap, s64);
                    } break;
                    case 'O': {
                        uint64_t u64;
                        if (gpr_offset < 7) {
                            u64 = ((int64_t)gpr[gpr_offset] << 32) + gpr[gpr_offset + 1];
                            gpr_offset += 2;
                        } else {
                            gpr_offset = 8;
                            u64 = ((int64_t)stack[stack_offset] << 32) + stack[stack_offset + 1];
                        }
                        argv[i] = uint64_to_integer(vm->m_heap, u64);
                    } break;
                    case 'f': case 'd': {
                        double f64;
                        if (fpr_offset < 8) {
                            f64 = fpr[fpr_offset];
                            fpr_offset += 1;
                        } else {
                            if (stack_offset & 1) stack_offset++;
                            f64 = *(double*)(stack + stack_offset);
                            stack_offset += 2;
                        }
                        argv[i] = make_flonum(vm->m_heap, f64);
                    } break;

                    default: fatal("fatal: invalid callback argument signature %c\n[exit]\n", c);
                }
            }
            return vm->call_scheme_argv((scm_closure_t)obj, argc, argv);
        } catch (vm_exit_t& e) {
            exit(e.m_code);
        } catch (...) {
            fatal("fatal: unhandled exception in callback\n[exit]\n");
        }
    }

#endif

#if ARCH_PPC && ARCH_LP64
    const char*
    c_stack_frame_t::push(scm_obj_t obj, int signature)
    {
        if (m_count < FFI_MAX_ARGC) {
            if (FIXNUMP(obj) || BIGNUMP(obj)) {
                if (signature == 'x') {
                    intptr_t value = coerce_exact_integer_to_intptr(obj);
                    if (m_gpr_count < array_sizeof(m_gpr)) {
                        m_gpr[m_gpr_count++] = value;
                        m_count++;
                        return NULL;
                    }
                    m_frame[m_count++] = value;
                    return NULL;
                }
                if (signature == 'i' || signature == 'p' || signature == '*') {
                    intptr_t value = coerce_exact_integer_to_intptr(obj);
                    if (m_gpr_count < array_sizeof(m_gpr)) {
                        m_gpr[m_gpr_count++] = value;
                        m_count++;
                        return NULL;
                    }
                    m_frame[m_count++] = value;
                    return NULL;
                }
                if (signature == 'f' || signature == 'd') {
                    union { double f64; uint64_t u64; } n;
                    n.f64 = real_to_double(obj);
                    if (m_fpr_count < array_sizeof(m_fpr)) {
                        if (m_gpr_count < array_sizeof(m_gpr)) m_gpr_count++;
                        m_fpr[m_fpr_count++] = n.f64;
                        m_count++;
                        return NULL;
                    }
                    m_frame[m_count++] = n.u64;
                    return NULL;
                }
                goto bad_signature;
            }
            if (FLONUMP(obj)) {
                union { double f64; uint64_t u64; } n;
                scm_flonum_t flonum = (scm_flonum_t)obj;
                n.f64 = flonum->value;
                if (signature == 'f' || signature == 'd') {
                    if (m_fpr_count < array_sizeof(m_fpr)) {
                        if (m_gpr_count < array_sizeof(m_gpr)) m_gpr_count++;
                        m_fpr[m_fpr_count++] = n.f64;
                        m_count++;
                        return NULL;
                    }
                    m_frame[m_count++] = n.u64;
                    return NULL;
                }
                if (signature == '*') {
                    if (m_gpr_count < array_sizeof(m_gpr)) {
                        m_gpr[m_gpr_count++] = n.u64;
                        m_count++;
                        return NULL;
                    }
                    m_frame[m_count++] = n.u64;
                    return NULL;
                }
                goto bad_signature;
            }
            if (BVECTORP(obj)) {
                if (signature != 'p' && signature != '*') goto bad_signature;
                scm_bvector_t bvector = (scm_bvector_t)obj;
                if (m_gpr_count < array_sizeof(m_gpr)) {
                    m_gpr[m_gpr_count++] = (intptr_t)bvector->elts;
                    m_count++;
                    return NULL;
                }
                m_frame[m_count++] = (intptr_t)bvector->elts;
                return NULL;
            }
            if (VECTORP(obj)) {
                if (signature != 'c') goto bad_signature;
                scm_vector_t vector = (scm_vector_t)obj;
                int n = vector->count;
                if (n == 0) return "nonempty vector";
                assert(n);
                if (!FIXNUMP(vector->elts[0])) return "vector contains fixnum in first element";
                int ref = FIXNUM(vector->elts[0]);
                scm_bvector_t bvector = make_bvector(m_vm->m_heap, sizeof(intptr_t) * (n - 1));
                for (int i = 0; i < n - 1; i++) {
                    if (!BVECTORP(vector->elts[i + 1])) return "vector of bytevector";
                    *(uint8_t**)(bvector->elts + sizeof(intptr_t) * i) = ((scm_bvector_t)vector->elts[i + 1])->elts;
                }
                while (ref) {
                    intptr_t datum = (intptr_t)bvector->elts;
                    bvector = make_bvector(m_vm->m_heap, sizeof(intptr_t));
                    *(intptr_t*)(bvector->elts) = datum;
                    ref--;
                }
                if (m_gpr_count < array_sizeof(m_gpr)) {
                    m_gpr[m_gpr_count++] = (intptr_t)bvector->elts;
                    m_count++;
                    return NULL;
                }
                m_frame[m_count++] = (intptr_t)bvector->elts;
                return NULL;
            }
            goto bad_signature;
        }
        fatal("fatal: c function stack frame overflow");

    bad_signature:
        switch (signature) {
            case 'i':
            case 'x':
                return "exact integer";
            case 'p':
                return "exact integer or bytevector";
            case 'c':
                return "vector";
            case 'f':
            case 'd':
                return "real";
            case '*':
                return "exact integer, real, or bytevector";
            default:
                fatal("fatal: invalid c function argument type specifier");
        }
    }

    void
    c_stack_frame_t::compose()
    {
        int dst = m_count;
        for (int i = 0; i < array_sizeof(m_gpr); i++) m_frame[dst++] = m_gpr[i];
        for (int i = 0; i < array_sizeof(m_fpr); i++) {
            union { double f64; uint64_t u64; } n;
            n.f64 = m_fpr[i];
            m_frame[dst++] = n.u64;
        }
    }

    scm_obj_t callback_scheme(intptr_t uid, intptr_t signatures, intptr_t* reg, intptr_t* stack)
    {
        intptr_t* gpr = reg;
        double* fpr = (double*)(reg + 8);
        VM* vm = current_vm();
        int argc = strlen((const char*)signatures);
        scm_obj_t obj;
        {
            scoped_lock lock(vm->m_heap->m_trampolines->lock);
            obj = get_hashtable(vm->m_heap->m_trampolines, MAKEFIXNUM(uid));
            if (!CLOSUREP(obj)) fatal("fatal: callback was destroyed\n[exit]\n");
        }
        try {
            scm_obj_t* argv = (scm_obj_t*)alloca(sizeof(scm_obj_t*) * argc);
            int gpr_offset = 0;
            int fpr_offset = 0;
            int stack_offset = 0;
            for (int i = 0; i < argc; i++) {
                char c = *(const char*)(signatures + i);
                switch (c) {
                    case 'L': {
                        int8_t s8;
                        if (gpr_offset < 8) {
                            s8 = gpr[gpr_offset];
                            gpr_offset += 1;
                        } else {
                            s8 = stack[stack_offset];
                        }
                        stack_offset += 1;
                        argv[i] = s8 ? MAKEFIXNUM(1) : MAKEFIXNUM(0);
                    } break;
                    case 'u': {
                        int8_t s8;
                        if (gpr_offset < 8) {
                            s8 = gpr[gpr_offset];
                            gpr_offset += 1;
                        } else {
                            s8 = stack[stack_offset];
                        }
                        stack_offset += 1;
                        argv[i] = intptr_to_integer(vm->m_heap, s8);
                    } break;
                    case 'U': {
                        uint8_t u8;
                        if (gpr_offset < 8) {
                            u8 = gpr[gpr_offset];
                            gpr_offset += 1;
                        } else {
                            u8 = stack[stack_offset];
                        }
                        stack_offset += 1;
                        argv[i] = uintptr_to_integer(vm->m_heap, u8);
                    } break;
                    case 'b': {
                        int16_t s16;
                        if (gpr_offset < 8) {
                            s16 = gpr[gpr_offset];
                            gpr_offset += 1;
                        } else {
                            s16 = stack[stack_offset];
                        }
                        stack_offset += 1;
                        argv[i] = intptr_to_integer(vm->m_heap, s16);
                    } break;
                    case 'B': {
                        uint16_t u16;
                        if (gpr_offset < 8) {
                            u16 = gpr[gpr_offset];
                            gpr_offset += 1;
                        } else {
                            u16 = stack[stack_offset];
                        }
                        stack_offset += 1;
                        argv[i] = uintptr_to_integer(vm->m_heap, u16);
                    } break;
                    case 'q': {
                        int32_t s32;
                        if (gpr_offset < 8) {
                            s32 = gpr[gpr_offset];
                            gpr_offset += 1;
                        } else {
                            s32 = stack[stack_offset];
                        }
                        stack_offset += 1;
                        argv[i] = int32_to_integer(vm->m_heap, s32);
                    } break;
                    case 'Q': {
                        uint32_t u32;
                        if (gpr_offset < 8) {
                            u32 = gpr[gpr_offset];
                            gpr_offset += 1;
                        } else {
                            u32 = stack[stack_offset];
                        }
                        stack_offset += 1;
                        argv[i] = uint32_to_integer(vm->m_heap, u32);
                    } break;
                    case 'o': {
                        int64_t s64;
                        if (gpr_offset < 8) {
                            s64 = gpr[gpr_offset];
                            gpr_offset += 1;
                        } else {
                            s64 = stack[stack_offset];
                        }
                        stack_offset += 1;
                        argv[i] = int64_to_integer(vm->m_heap, s64);
                    } break;
                    case 'O': {
                        uint64_t u64;
                        if (gpr_offset < 7) {
                            u64 = gpr[gpr_offset];
                            gpr_offset += 1;
                        } else {
                            u64 = stack[stack_offset];
                        }
                        stack_offset += 1;
                        argv[i] = uint64_to_integer(vm->m_heap, u64);
                    } break;
                    case 'f': case 'd': {
                        double f64;
                        if (fpr_offset < 13) {
                            f64 = fpr[fpr_offset];
                            fpr_offset += 1;
                        } else {
                            f64 = *(double*)(stack + stack_offset);
                        }
                        stack_offset += 1;
                        argv[i] = make_flonum(vm->m_heap, f64);
                    } break;

                    default: fatal("fatal: invalid callback argument signature %c\n[exit]\n", c);
                }
            }
            return vm->call_scheme_argv((scm_closure_t)obj, argc, argv);
        } catch (vm_exit_t& e) {
            exit(e.m_code);
        } catch (...) {
            fatal("fatal: unhandled exception in callback\n[exit]\n");
        }
    }

#endif

#if _MSC_VER

    intptr_t
    c_func_stub_intptr(void* adrs, intptr_t argc, intptr_t argv[])
    {
        int bytes = (argc * sizeof(intptr_t) + 15) & ~15;
        intptr_t retval;
        __asm {
            mov     ecx, bytes
            mov     edx, esp
            sub     esp, ecx
            mov     esi, argv
            mov     edi, esp
            rep     movsb
            mov     edi, edx
            call    adrs
            mov     esp, edi
            mov     retval, eax
        }
        return retval;
    }

    int64_t
    c_func_stub_int64(void* adrs, intptr_t argc, intptr_t argv[])
    {
        int bytes = (argc * sizeof(intptr_t) + 15) & ~15;
        union {
            int64_t s64;
            struct {
                uint32_t lo;
                uint32_t hi;
            } s32;
        } retval;
        __asm {
            mov     ecx, bytes
            mov     edx, esp
            sub     esp, ecx
            mov     esi, argv
            mov     edi, esp
            rep     movsb
            mov     edi, edx
            call    adrs
            mov     esp, edi
            mov     retval.s32.lo, eax
            mov     retval.s32.hi, edx
        }
        return retval.s64;
    }

    double
    c_func_stub_double(void* adrs, intptr_t argc, intptr_t argv[])
    {
        int bytes = (argc * sizeof(intptr_t) + 15) & ~15;
        double retval;
        __asm {
            mov     ecx, bytes
            mov     edx, esp
            sub     esp, ecx
            mov     esi, argv
            mov     edi, esp
            rep     movsb
            mov     edi, edx
            call    adrs
            mov     esp, edi
            fstp    retval
        }
        return retval;
    }

    #pragma pack(push, 1)
    struct trampoline_t {
        uint8_t     mov_ecx_imm32;  // B9           : mov ecx, imm16/32
        uint32_t    imm32_uid;      // 00 00 00 00
        uint8_t     mov_eax_imm32;  // B8           ; mov eax, imm16/32
        uint32_t    imm32_stub;     // 00 00 00 00
        uint8_t     jmp_eax[2];     // FF 20        ; jmp [eax]
        uint8_t     ud2[2];         // 0F 0B

        intptr_t    m_stub;
        intptr_t    m_uid;
        intptr_t    m_signatures;
        intptr_t    m_bytes;

        char        m_buffer[FFI_MAX_ARGC];
        static uint8_t* s_pool;
        static uint8_t* s_pool_limit;
        static int s_pool_alloc_size;

        void* operator new(size_t size);
        trampoline_t(intptr_t stub, intptr_t uid, const char* signatures);
    };
    #pragma pack(pop)

    uint8_t* trampoline_t::s_pool;
    uint8_t* trampoline_t::s_pool_limit;
    int trampoline_t::s_pool_alloc_size;

    void* trampoline_t::operator new(size_t size)
    {
        if (s_pool == NULL) {
            SYSTEM_INFO info;
            GetSystemInfo(&info);
            s_pool_alloc_size = info.dwPageSize;
        }
        assert(size < s_pool_alloc_size);
        if (s_pool + size > s_pool_limit) {
            s_pool = (uint8_t*)VirtualAlloc(NULL, s_pool_alloc_size, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
            s_pool_limit = s_pool + s_pool_alloc_size;
        }
        void* p = s_pool;
        s_pool += size;
        return p;
    }

    trampoline_t::trampoline_t(intptr_t stub, intptr_t uid, const char* signatures)
    {
        strncpy(m_buffer, signatures, sizeof(m_buffer));
        m_stub = stub;
        m_uid = uid;
        m_signatures = (intptr_t)(&m_buffer[0]);
        m_bytes = 0;
        int argc = strlen(signatures);
        for (int i = 0; i < argc; i++) {
            switch (signatures[i]) {
                case 'O': case 'o': case 'd': m_bytes += 8; break;
                default: m_bytes += 4; break;
            }
        }
        mov_ecx_imm32 = 0xB9;
        imm32_uid = (uint32_t)&m_uid;
        mov_eax_imm32 = 0xB8;
        imm32_stub = (uint32_t)&m_stub;
        jmp_eax[0] = 0xFF;
        jmp_eax[1] = 0x20;
        ud2[0] = 0x0F;
        ud2[1] = 0x0B;
        MEM_STORE_FENCE;
    }

    intptr_t callback_intptr(intptr_t uid, intptr_t signatures, intptr_t* base)
    {
        scm_obj_t ans = callback_scheme(uid, signatures, base);
        if (exact_integer_pred(ans)) return coerce_exact_integer_to_intptr(ans);
        return 0;
    }

    int64_t callback_int64(intptr_t uid, intptr_t signatures, intptr_t* base)
    {
        scm_obj_t ans = callback_scheme(uid, signatures, base);
        if (exact_integer_pred(ans)) return coerce_exact_integer_to_int64(ans);
        return 0;
    }

    double callback_double(intptr_t uid, intptr_t signatures, intptr_t* base)
    {
        scm_obj_t ans = callback_scheme(uid, signatures, base);
        if (real_valued_pred(ans)) return real_to_double(ans);
        return 0.0;
    }

    void __declspec(naked) stdcall_callback_stub_intptr()
    {
        // note: uid adrs in ecx
        intptr_t*   base;
        intptr_t    uid;
        intptr_t    signatures;
        intptr_t    value;
        intptr_t    bytes;
        __asm {
            push    ebp
            mov     ebp, esp
            sub     esp, __LOCAL_SIZE
            lea     eax, [ebp + 8]
            mov     base, eax
            mov     eax, [ecx]
            mov     uid, eax
            mov     eax, [ecx + 4]
            mov     signatures, eax
            mov     eax, [ecx + 8]
            mov     bytes, eax
        }
        value = callback_intptr(uid, signatures, base);
        __asm {
            mov     edx, bytes
            mov     eax, value
            mov     esp, ebp
            pop     ebp
            pop     ecx
            add     esp, edx
            jmp     ecx
        }
    }

    void __declspec(naked) stdcall_callback_stub_int64()
    {
        // note: uid adrs in ecx
        intptr_t*   base;
        intptr_t    uid;
        intptr_t    signatures;
        union {
            int64_t n64;
            struct {
                uint32_t lo;
                uint32_t hi;
            } u32;
        } value;
        intptr_t    bytes;
        __asm {
            push    ebp
            mov     ebp, esp
            sub     esp, __LOCAL_SIZE
            lea     eax, [ebp + 8]
            mov     base, eax
            mov     eax, [ecx]
            mov     uid, eax
            mov     eax, [ecx + 4]
            mov     signatures, eax
            mov     eax, [ecx + 8]
            mov     bytes, eax
        }
        value.n64 = callback_int64(uid, signatures, base);
        __asm {
            mov     eax, value.u32.lo
            mov     edx, value.u32.hi
            mov     ecx, bytes
            mov     esp, ebp
            mov     ebp, [esp + 4]
            mov     [esp + ecx + 4], ebp
            pop     ebp
            add     esp, ecx
            ret
        }
    }

    void __declspec(naked) stdcall_callback_stub_double()
    {
        // note: uid adrs in ecx
        intptr_t*   base;
        intptr_t    uid;
        intptr_t    signatures;
        double      value;
        intptr_t    bytes;
        __asm {
            push    ebp
            mov     ebp, esp
            sub     esp, __LOCAL_SIZE
            lea     eax, [ebp + 8]
            mov     base, eax
            mov     eax, [ecx]
            mov     uid, eax
            mov     eax, [ecx + 4]
            mov     signatures, eax
            mov     eax, [ecx + 8]
            mov     bytes, eax
        }
        value = callback_double(uid, signatures, base);
        __asm {
            mov     edx, bytes
            fld     value
            mov     esp, ebp
            pop     ebp
            pop     ecx
            add     esp, edx
            jmp     ecx
        }
    }

    void __declspec(naked) c_callback_stub_intptr()
    {
        // note: uid adrs in ecx
        intptr_t*   base;
        intptr_t    uid;
        intptr_t    signatures;
        intptr_t    value;
        __asm {
            push    ebp
            mov     ebp, esp
            sub     esp, __LOCAL_SIZE
            lea     eax, [ebp + 8]
            mov     base, eax
            mov     eax, [ecx]
            mov     uid, eax
            mov     eax, [ecx + 4]
            mov     signatures, eax
        }
        value = callback_intptr(uid, signatures, base);
        __asm {
            mov     eax, value
            mov     esp, ebp
            pop     ebp
            ret
        }
    }

    void __declspec(naked) c_callback_stub_int64()
    {
        // note: uid adrs in ecx
        intptr_t*   base;
        intptr_t    uid;
        intptr_t    signatures;
        union {
            int64_t n64;
            struct {
                uint32_t lo;
                uint32_t hi;
            } u32;
        } value;
        __asm {
            push    ebp
            mov     ebp, esp
            sub     esp, __LOCAL_SIZE
            lea     eax, [ebp + 8]
            mov     base, eax
            mov     eax, [ecx]
            mov     uid, eax
            mov     eax, [ecx + 4]
            mov     signatures, eax
        }
        value.n64 = callback_int64(uid, signatures, base);
        __asm {
            mov     eax, value.u32.lo
            mov     edx, value.u32.hi
            mov     esp, ebp
            pop     ebp
            ret
        }
    }

    void __declspec(naked) c_callback_stub_double()
    {
        // note: uid adrs in ecx
        intptr_t*   base;
        intptr_t    uid;
        intptr_t    signatures;
        double      value;
        __asm {
            push    ebp
            mov     ebp, esp
            sub     esp, __LOCAL_SIZE
            lea     eax, [ebp + 8]
            mov     base, eax
            mov     eax, [ecx]
            mov     uid, eax
            mov     eax, [ecx + 4]
            mov     signatures, eax
        }
        value = callback_double(uid, signatures, base);
        __asm {
            fld     value
            mov     esp, ebp
            pop     ebp
            ret
        }
    }

    scm_obj_t make_callback(VM* vm, int type, const char* signatures, scm_closure_t closure)
    {
        scoped_lock lock(vm->m_heap->m_trampolines->lock);
        static intptr_t uid;
        trampoline_t* thunk;
        switch (type & CALLBACK_RETURN_TYPE_MASK) {
            case CALLBACK_RETURN_TYPE_INTPTR:
                if ((type & CALLBACK_CALL_TYPE_MASK) == CALLBACK_CALL_TYPE_STDCALL) {
                    thunk = new trampoline_t((intptr_t)stdcall_callback_stub_intptr, uid, signatures);
                    break;
                }
                thunk = new trampoline_t((intptr_t)c_callback_stub_intptr, uid, signatures);
                break;
            case CALLBACK_RETURN_TYPE_INT64_T:
                if ((type & CALLBACK_CALL_TYPE_MASK) == CALLBACK_CALL_TYPE_STDCALL) {
                    thunk = new trampoline_t((intptr_t)stdcall_callback_stub_int64, uid, signatures);
                    break;
                }
                thunk = new trampoline_t((intptr_t)c_callback_stub_int64, uid, signatures);
                break;
            case CALLBACK_RETURN_TYPE_FLOAT:
            case CALLBACK_RETURN_TYPE_DOUBLE:
                if ((type & CALLBACK_CALL_TYPE_MASK) == CALLBACK_CALL_TYPE_STDCALL) {
                    thunk = new trampoline_t((intptr_t)stdcall_callback_stub_double, uid, signatures);
                    break;
                }
                thunk = new trampoline_t((intptr_t)c_callback_stub_double, uid, signatures);
                break;
            default:
                fatal("%s:%u invalid callback type specifier 0x%x", __FILE__, __LINE__, type);
        }
        vm->m_heap->write_barrier(closure);
        int nsize = put_hashtable(vm->m_heap->m_trampolines, MAKEFIXNUM(uid), closure);
        if (nsize) rehash_hashtable(vm->m_heap, vm->m_heap->m_trampolines, nsize);
        uid++;
        assert(uid < FIXNUM_MAX);
        return uintptr_to_integer(vm->m_heap, (uintptr_t)thunk);
    }
#elif ARCH_IA32
    struct trampoline_t {
        uint8_t     mov_ecx_imm32;  // B9           : mov ecx, imm16/32
        uint32_t    imm32_uid;      // 00 00 00 00
        uint8_t     mov_eax_imm32;  // B8           ; mov eax, imm16/32
        uint32_t    imm32_stub;     // 00 00 00 00
        uint8_t     jmp_eax[2];     // FF 20        ; jmp [eax]
        uint8_t     ud2[2];         // 0F 0B
        intptr_t    m_stub;
        intptr_t    m_uid;
        intptr_t    m_signatures;
        char        m_buffer[FFI_MAX_ARGC];
        static uint8_t* s_pool;
        static uint8_t* s_pool_limit;
        static int s_pool_alloc_size;
        void* operator new(size_t size);
        trampoline_t(intptr_t stub, intptr_t uid, const char* signatures);
    } __attribute__((packed));

    uint8_t* trampoline_t::s_pool;
    uint8_t* trampoline_t::s_pool_limit;
    int trampoline_t::s_pool_alloc_size;

    void* trampoline_t::operator new(size_t size)
    {
        if (s_pool == NULL) {
            s_pool_alloc_size = getpagesize() * 8;
        }
        assert(size < s_pool_alloc_size);
        if (s_pool + size > s_pool_limit) {
            s_pool = (uint8_t*)mmap(NULL, s_pool_alloc_size, PROT_EXEC | PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
            if (s_pool == (uint8_t*)MAP_FAILED) fatal("%s:%u mmap failed %d", __FILE__, __LINE__, errno);
            s_pool_limit = s_pool + s_pool_alloc_size;
        }
        void* p = s_pool;
        s_pool += size;
        return p;
    }

    trampoline_t::trampoline_t(intptr_t stub, intptr_t uid, const char* signatures)
    {
        strncpy(m_buffer, signatures, sizeof(m_buffer));
        m_stub = stub;
        m_uid = uid;
        m_signatures = (intptr_t)(&m_buffer[0]);
        mov_ecx_imm32 = 0xB9;
        imm32_uid = (uint32_t)&m_uid;
        mov_eax_imm32 = 0xB8;
        imm32_stub = (uint32_t)&m_stub;
        jmp_eax[0] = 0xFF;
        jmp_eax[1] = 0x20;
        ud2[0] = 0x0F;
        ud2[1] = 0x0B;
        MEM_STORE_FENCE;
    }

    intptr_t c_callback_intptr(intptr_t uid, intptr_t signatures, intptr_t* stack)
    {
        scm_obj_t ans = callback_scheme(uid, signatures, stack);
        if (exact_integer_pred(ans)) return coerce_exact_integer_to_intptr(ans);
        return 0;
    }

    int64_t c_callback_int64(intptr_t uid, intptr_t signatures, intptr_t* stack)
    {
        scm_obj_t ans = callback_scheme(uid, signatures, stack);
        if (exact_integer_pred(ans)) return coerce_exact_integer_to_int64(ans);
        return 0;
    }

    double c_callback_double(intptr_t uid, intptr_t signatures, intptr_t* stack)
    {
        scm_obj_t ans = callback_scheme(uid, signatures, stack);
        if (real_valued_pred(ans)) return real_to_double(ans);
        return 0.0;
    }

    scm_obj_t make_callback(VM* vm, int type, const char* signatures, scm_closure_t closure)
    {
        scoped_lock lock(vm->m_heap->m_trampolines->lock);
        static intptr_t uid;
        trampoline_t* thunk;
        switch (type & CALLBACK_RETURN_TYPE_MASK) {
        case CALLBACK_RETURN_TYPE_INTPTR:
            thunk = new trampoline_t((intptr_t)c_callback_stub_intptr, uid, signatures);
            break;
        case CALLBACK_RETURN_TYPE_INT64_T:
            thunk = new trampoline_t((intptr_t)c_callback_stub_int64, uid, signatures);
            break;
        case CALLBACK_RETURN_TYPE_FLOAT:
        case CALLBACK_RETURN_TYPE_DOUBLE:
            thunk = new trampoline_t((intptr_t)c_callback_stub_double, uid, signatures);
            break;
        default:
            fatal("%s:%u invalid callback type specifier 0x%x", __FILE__, __LINE__, type);
        }
        vm->m_heap->write_barrier(closure);
        int nsize = put_hashtable(vm->m_heap->m_trampolines, MAKEFIXNUM(uid), closure);
        if (nsize) rehash_hashtable(vm->m_heap, vm->m_heap->m_trampolines, nsize);
        uid++;
        assert(uid < FIXNUM_MAX);
        return uintptr_to_integer(vm->m_heap, (uintptr_t)thunk);
    }
#elif ARCH_X64
    struct trampoline_t {
        uint8_t     mov_r10_imm64[2];   // 49 BA                    : mov r10, imm64
        uint64_t    imm64_uid;          // 00 00 00 00 00 00 00 00
        uint8_t     mov_r11_imm64[2];   // 49 BB                    : mov r11, imm64
        uint64_t    imm64_stub;         // 00 00 00 00 00 00 00 00
        uint8_t     jmp_r11[3];         // 41 FF 23                 : jmp [r11]
        uint8_t     ud2[2];             // 0F 0B
        intptr_t    m_stub;
        intptr_t    m_uid;
        intptr_t    m_signatures;
        char        m_buffer[FFI_MAX_ARGC];
        static uint8_t* s_pool;
        static uint8_t* s_pool_limit;
        static int s_pool_alloc_size;
        void* operator new(size_t size);
        trampoline_t(intptr_t stub, intptr_t uid, const char* signatures);
    } __attribute__((packed));

    uint8_t* trampoline_t::s_pool;
    uint8_t* trampoline_t::s_pool_limit;
    int trampoline_t::s_pool_alloc_size;

    void* trampoline_t::operator new(size_t size)
    {
        if (s_pool == NULL) {
            s_pool_alloc_size = getpagesize() * 8;
        }
        assert(size < s_pool_alloc_size);
        if (s_pool + size > s_pool_limit) {
            s_pool = (uint8_t*)mmap(NULL, s_pool_alloc_size, PROT_EXEC | PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
            if (s_pool == (uint8_t*)MAP_FAILED) fatal("%s:%u mmap failed %d", __FILE__, __LINE__, errno);
            s_pool_limit = s_pool + s_pool_alloc_size;
        }
        void* p = s_pool;
        s_pool += size;
        return p;
    }

    trampoline_t::trampoline_t(intptr_t stub, intptr_t uid, const char* signatures)
    {
        strncpy(m_buffer, signatures, sizeof(m_buffer));
        m_stub = stub;
        m_uid = uid;
        m_signatures = (intptr_t)(&m_buffer[0]);
        mov_r10_imm64[0] = 0x49;
        mov_r10_imm64[1] = 0xBA;
        imm64_uid = (uint64_t)&m_uid;
        mov_r11_imm64[0] = 0x49;
        mov_r11_imm64[1] = 0xBB;
        imm64_stub = (uint64_t)&m_stub;
        jmp_r11[0] = 0x41;
        jmp_r11[1] = 0xFF;
        jmp_r11[2] = 0x23;
        ud2[0] = 0x0F;
        ud2[1] = 0x0B;
        MEM_STORE_FENCE;
    }

    intptr_t c_callback_intptr_x64(intptr_t uid, intptr_t signatures, intptr_t* reg, intptr_t* stack)
    {
        scm_obj_t ans = callback_scheme(uid, signatures, reg, stack);
        if (exact_integer_pred(ans)) return coerce_exact_integer_to_intptr(ans);
        return 0;
    }

    float c_callback_float_x64(intptr_t uid, intptr_t signatures, intptr_t* reg, intptr_t* stack)
    {
        scm_obj_t ans = callback_scheme(uid, signatures, reg, stack);
        if (real_valued_pred(ans)) return real_to_double(ans);
        return 0.0;
    }

    double c_callback_double_x64(intptr_t uid, intptr_t signatures, intptr_t* reg, intptr_t* stack)
    {
        scm_obj_t ans = callback_scheme(uid, signatures, reg, stack);
        if (real_valued_pred(ans)) return real_to_double(ans);
        return 0.0;
    }

    scm_obj_t make_callback(VM* vm, int type, const char* signatures, scm_closure_t closure)
    {
        scoped_lock lock(vm->m_heap->m_trampolines->lock);
        static intptr_t uid;
        trampoline_t* thunk;
        switch (type & CALLBACK_RETURN_TYPE_MASK) {
        case CALLBACK_RETURN_TYPE_INTPTR:
        case CALLBACK_RETURN_TYPE_INT64_T:
            thunk = new trampoline_t((intptr_t)c_callback_stub_intptr_x64, uid, signatures);
            break;
        case CALLBACK_RETURN_TYPE_FLOAT:
            thunk = new trampoline_t((intptr_t)c_callback_stub_float_x64, uid, signatures);
            break;
        case CALLBACK_RETURN_TYPE_DOUBLE:
            thunk = new trampoline_t((intptr_t)c_callback_stub_double_x64, uid, signatures);
            break;
        default:
            fatal("%s:%u invalid callback type specifier 0x%x", __FILE__, __LINE__, type);
        }
        vm->m_heap->write_barrier(closure);
        int nsize = put_hashtable(vm->m_heap->m_trampolines, MAKEFIXNUM(uid), closure);
        if (nsize) rehash_hashtable(vm->m_heap, vm->m_heap->m_trampolines, nsize);
        uid++;
        assert(uid < FIXNUM_MAX);
        return uintptr_to_integer(vm->m_heap, (uintptr_t)thunk);
    }
#elif ARCH_PPC && ARCH_ILP32
    struct trampoline_t {
        uint8_t     lis_r11[2];     // 3D 60       : lis r11,imm16
        uint16_t    hi_uid;         // 00 00
        uint8_t     ori_r11[2];     // 61 6B       : ori r11,r11,imm16
        uint16_t    lo_uid;         // 00 00
        uint8_t     lis_r12[2];     // 3D 80       : lis r12,imm16
        uint16_t    hi_stub;        // 00 00
        uint8_t     ori_r12[2];     // 61 8C       : ori r12,r12,imm16
        uint16_t    lo_stub;        // 00 00
        uint8_t     lwz_r0_r12[4];  // 80 0C 00 00 : lwz r0,0(r12)
        uint8_t     mtctr_r0[4];    // 7C 09 03 A6 : mtctr r0
        uint8_t     bctr[4];        // 4E 80 04 20 : bctr
        intptr_t    m_stub;
        intptr_t    m_uid;
        intptr_t    m_signatures;
        char        m_buffer[FFI_MAX_ARGC];
        static uint8_t* s_pool;
        static uint8_t* s_pool_limit;
        static int s_pool_alloc_size;
        void* operator new(size_t size);
        trampoline_t(intptr_t stub, intptr_t uid, const char* signatures);
    } __attribute__((packed));

    uint8_t* trampoline_t::s_pool;
    uint8_t* trampoline_t::s_pool_limit;
    int trampoline_t::s_pool_alloc_size;

    void* trampoline_t::operator new(size_t size)
    {
        if (s_pool == NULL) {
            s_pool_alloc_size = getpagesize() * 8;
        }
        assert(size < s_pool_alloc_size);
        if (s_pool + size > s_pool_limit) {
            s_pool = (uint8_t*)mmap(NULL, s_pool_alloc_size, PROT_EXEC | PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
            if (s_pool == (uint8_t*)MAP_FAILED) fatal("%s:%u mmap failed %d", __FILE__, __LINE__, errno);
            s_pool_limit = s_pool + s_pool_alloc_size;
        }
        void* p = s_pool;
        s_pool += size;
        return p;
    }

    trampoline_t::trampoline_t(intptr_t stub, intptr_t uid, const char* signatures)
    {
        strncpy(m_buffer, signatures, sizeof(m_buffer));
        m_stub = stub;
        m_uid = uid;
        m_signatures = (intptr_t)(&m_buffer[0]);
        lis_r11[0] = 0x3D;
        lis_r11[1] = 0x60;
        hi_uid = ((uint32_t)&m_uid) >> 16;
        ori_r11[0] = 0x61;
        ori_r11[1] = 0x6B;
        lo_uid = ((uint32_t)&m_uid) & 0xFFFF;
        lis_r12[0] = 0x3D;
        lis_r12[1] = 0x80;
        hi_stub = ((uint32_t)&m_stub) >> 16;
        ori_r12[0] = 0x61;
        ori_r12[1] = 0x8C;
        lo_stub = ((uint32_t)&m_stub) & 0xFFFF;
        lwz_r0_r12[0] = 0x80;
        lwz_r0_r12[1] = 0x0C;
        lwz_r0_r12[2] = 0x00;
        lwz_r0_r12[3] = 0x00;
        mtctr_r0[0] = 0x7C;
        mtctr_r0[1] = 0x09;
        mtctr_r0[2] = 0x03;
        mtctr_r0[3] = 0xA6;
        bctr[0] = 0x4E;
        bctr[1] = 0x80;
        bctr[2] = 0x04;
        bctr[3] = 0x20;
        MEM_STORE_FENCE;
    }

    intptr_t c_callback_intptr_ppc(intptr_t uid, intptr_t signatures, intptr_t* reg, intptr_t* stack)
    {
        scm_obj_t ans = callback_scheme(uid, signatures, reg, stack);
        if (exact_integer_pred(ans)) return coerce_exact_integer_to_intptr(ans);
        return 0;
    }

    int64_t c_callback_int64_ppc(intptr_t uid, intptr_t signatures, intptr_t* reg, intptr_t* stack)
    {
        scm_obj_t ans = callback_scheme(uid, signatures, reg, stack);
        if (exact_integer_pred(ans)) return coerce_exact_integer_to_int64(ans);
        return 0;
    }

    double c_callback_double_ppc(intptr_t uid, intptr_t signatures, intptr_t* reg, intptr_t* stack)
    {
        scm_obj_t ans = callback_scheme(uid, signatures, reg, stack);
        if (real_valued_pred(ans)) return real_to_double(ans);
        return 0.0;
    }

    scm_obj_t make_callback(VM* vm, int type, const char* signatures, scm_closure_t closure)
    {
        scoped_lock lock(vm->m_heap->m_trampolines->lock);
        static intptr_t uid;
        trampoline_t* thunk;
        switch (type & CALLBACK_RETURN_TYPE_MASK) {
        case CALLBACK_RETURN_TYPE_INTPTR:
            thunk = new trampoline_t((intptr_t)c_callback_stub_intptr_ppc, uid, signatures);
            break;
        case CALLBACK_RETURN_TYPE_INT64_T:
            thunk = new trampoline_t((intptr_t)c_callback_stub_int64_ppc, uid, signatures);
            break;
        case CALLBACK_RETURN_TYPE_FLOAT:
        case CALLBACK_RETURN_TYPE_DOUBLE:
            thunk = new trampoline_t((intptr_t)c_callback_stub_double_ppc, uid, signatures);
            break;
        default:
            fatal("%s:%u invalid callback type specifier 0x%x", __FILE__, __LINE__, type);
        }
        vm->m_heap->write_barrier(closure);
        int nsize = put_hashtable(vm->m_heap->m_trampolines, MAKEFIXNUM(uid), closure);
        if (nsize) rehash_hashtable(vm->m_heap, vm->m_heap->m_trampolines, nsize);
        uid++;
        assert(uid < FIXNUM_MAX);
        return uintptr_to_integer(vm->m_heap, (uintptr_t)thunk);
    }
#elif ARCH_PPC && ARCH_LP64
    struct trampoline_t {
        intptr_t    descriptor[3];
        uint8_t     lis_r11[2];     // 3D 60       : lis r11,imm16
        uint16_t    hh_uid;         // 00 00
        uint8_t     ori_r11_1[2];   // 61 6B       : ori r11,r11,imm16
        uint16_t    hl_uid;         // 00 00
        uint8_t     rldicr_r11[4];  // 79 6B 07 C6 : rldicr r11,r11,32,31
        uint8_t     oris_r11[2];    // 65 6B       : oris r11,r11,imm16
        uint16_t    lh_uid;         // 00 00
        uint8_t     ori_r11_2[2];   // 61 6B       : ori r11,r11,imm16
        uint16_t    ll_uid;         // 00 00
        uint8_t     lis_r12[2];     // 3D 80       : lis r12,imm16
        uint16_t    hh_stub;        // 00 00
        uint8_t     ori_r12_1[2];   // 61 8C       : ori r12,r12,imm16
        uint16_t    hl_stub;        // 00 00
        uint8_t     rldicr_r12[4];  // 79 8C 07 C6 : rldicr r12,r12,32,31
        uint8_t     oris_r12[2];    // 65 8C       : oris r12,r12,imm16
        uint16_t    lh_stub;        // 00 00
        uint8_t     ori_r12_2[2];   // 61 8C       : ori r12,r12,imm16
        uint16_t    ll_stub;        // 00 00
        uint8_t     ld_r0_r12[4];   // E8 0C 00 00 : ld r0,0(r12)
        uint8_t     mtctr_r0[4];    // 7C 09 03 A6 : mtctr r0
        uint8_t     bctr[4];        // 4E 80 04 20 : bctr
        intptr_t    m_stub;
        intptr_t    m_uid;
        intptr_t    m_signatures;
        char        m_buffer[FFI_MAX_ARGC];
        static uint8_t* s_pool;
        static uint8_t* s_pool_limit;
        static int s_pool_alloc_size;
        void* operator new(size_t size);
        trampoline_t(intptr_t stub, intptr_t uid, const char* signatures);
    } __attribute__((packed));

    uint8_t* trampoline_t::s_pool;
    uint8_t* trampoline_t::s_pool_limit;
    int trampoline_t::s_pool_alloc_size;

    void* trampoline_t::operator new(size_t size)
    {
        if (s_pool == NULL) {
            s_pool_alloc_size = getpagesize() * 8;
        }
        assert(size < s_pool_alloc_size);
        if (s_pool + size > s_pool_limit) {
            s_pool = (uint8_t*)mmap(NULL, s_pool_alloc_size, PROT_EXEC | PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
            if (s_pool == (uint8_t*)MAP_FAILED) fatal("%s:%u mmap failed %d", __FILE__, __LINE__, errno);
            s_pool_limit = s_pool + s_pool_alloc_size;
        }
        void* p = s_pool;
        s_pool += size;
        return p;
    }

    trampoline_t::trampoline_t(intptr_t stub, intptr_t uid, const char* signatures)
    {
        union { intptr_t n64; uint16_t n16[4]; } n;
        strncpy(m_buffer, signatures, sizeof(m_buffer));
        m_stub = stub;
        m_uid = uid;
        m_signatures = (intptr_t)(&m_buffer[0]);

        intptr_t toc;
        asm volatile ("std 2,%0" : "=m" (toc) : : "memory");
        descriptor[0] = (uintptr_t)&lis_r11[0];
        descriptor[1] = toc;
        descriptor[2] = 0;

        n.n64 = (intptr_t)&m_uid;
        lis_r11[0] = 0x3D;
        lis_r11[1] = 0x60;
        hh_uid = n.n16[0];
        ori_r11_1[0] = 0x61;
        ori_r11_1[1] = 0x6B;
        hl_uid = n.n16[1];
        rldicr_r11[0] = 0x79;
        rldicr_r11[1] = 0x6B;
        rldicr_r11[2] = 0x07;
        rldicr_r11[3] = 0xC6;
        oris_r11[0] = 0x65;
        oris_r11[1] = 0x6B;
        lh_uid = n.n16[2];
        ori_r11_2[0] = 0x61;
        ori_r11_2[1] = 0x6B;
        ll_uid = n.n16[3];
        n.n64 = (intptr_t)&m_stub;
        lis_r12[0] = 0x3D;
        lis_r12[1] = 0x80;
        hh_stub = n.n16[0];
        ori_r12_1[0] = 0x61;
        ori_r12_1[1] = 0x8C;
        hl_stub = n.n16[1];
        rldicr_r12[0] = 0x79;
        rldicr_r12[1] = 0x8C;
        rldicr_r12[2] = 0x07;
        rldicr_r12[3] = 0xC6;
        oris_r12[0] = 0x65;
        oris_r12[1] = 0x8C;
        lh_stub = n.n16[2];
        ori_r12_2[0] = 0x61;
        ori_r12_2[1] = 0x8C;
        ll_stub = n.n16[3];
        ld_r0_r12[0] = 0xE8;
        ld_r0_r12[1] = 0x0C;
        ld_r0_r12[2] = 0x00;
        ld_r0_r12[3] = 0x00;
        mtctr_r0[0] = 0x7C;
        mtctr_r0[1] = 0x09;
        mtctr_r0[2] = 0x03;
        mtctr_r0[3] = 0xA6;
        bctr[0] = 0x4E;
        bctr[1] = 0x80;
        bctr[2] = 0x04;
        bctr[3] = 0x20;
        MEM_STORE_FENCE;
    }

    intptr_t c_callback_intptr_ppc64(intptr_t uid, intptr_t signatures, intptr_t* reg, intptr_t* stack)
    {
        scm_obj_t ans = callback_scheme(uid, signatures, reg, stack);
        if (exact_integer_pred(ans)) return coerce_exact_integer_to_intptr(ans);
        return 0;
    }

    double c_callback_double_ppc64(intptr_t uid, intptr_t signatures, intptr_t* reg, intptr_t* stack)
    {
        scm_obj_t ans = callback_scheme(uid, signatures, reg, stack);
        if (real_valued_pred(ans)) return real_to_double(ans);
        return 0.0;
    }

    scm_obj_t make_callback(VM* vm, int type, const char* signatures, scm_closure_t closure)
    {
        scoped_lock lock(vm->m_heap->m_trampolines->lock);
        static intptr_t uid;
        trampoline_t* thunk;
        switch (type & CALLBACK_RETURN_TYPE_MASK) {
        case CALLBACK_RETURN_TYPE_INTPTR:
        case CALLBACK_RETURN_TYPE_INT64_T:
            thunk = new trampoline_t((intptr_t)c_callback_stub_intptr_ppc64, uid, signatures);
            break;
        case CALLBACK_RETURN_TYPE_FLOAT:
        case CALLBACK_RETURN_TYPE_DOUBLE:
            thunk = new trampoline_t((intptr_t)c_callback_stub_double_ppc64, uid, signatures);
            break;
        default:
            fatal("%s:%u invalid callback type specifier 0x%x", __FILE__, __LINE__, type);
        }
        vm->m_heap->write_barrier(closure);
        int nsize = put_hashtable(vm->m_heap->m_trampolines, MAKEFIXNUM(uid), closure);
        if (nsize) rehash_hashtable(vm->m_heap, vm->m_heap->m_trampolines, nsize);
        uid++;
        assert(uid < FIXNUM_MAX);
        return uintptr_to_integer(vm->m_heap, (uintptr_t)thunk);
    }
#else
    scm_obj_t make_callback(VM* vm, int type, const char* signatures, scm_closure_t closure)
    {
        fatal("%s:%u ffi not supported on this build", __FILE__, __LINE__);
    }
#endif
