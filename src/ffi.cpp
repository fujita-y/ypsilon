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
                if (signature == 'i' || signature == 'p') {
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
                if (signature == 'd') {
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
                if (signature != 'p') goto bad_signature;
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
            default:
                fatal("fatal: invalid c function argument type specifier");
        }
    }
#endif

#if ARCH_X64
    const char*
    c_stack_frame_t::push(scm_obj_t obj, int signature)
    {
        if (m_count < array_sizeof(m_frame) - array_sizeof(m_reg) - array_sizeof(m_sse)) {
            if (FIXNUMP(obj) || BIGNUMP(obj)) {
                if (signature == 'x' || signature == 'i' || signature == 'p') {
                    intptr_t value = coerce_exact_integer_to_intptr(obj);
                    if (m_reg_count < array_sizeof(m_reg)) {
                        m_reg[m_reg_count++] = value;
                    } else {
                        m_frame[m_count++] = value;
                    }
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
                union {
                    double      f64;
                    float       f32;
                    uint64_t    u64;
                } n;
                scm_flonum_t flonum = (scm_flonum_t)obj;
                if (signature == 'f') {
                    if (m_sse_count < array_sizeof(m_sse)) {
                        n.f64 = flonum->value;
                        m_pre.u8[m_sse_count] = 1;
                        m_sse[m_sse_count] = n.u64;
                        m_sse_float_count++;
                        m_sse_count++;
                    } else {
                        n.u64 = 0;
                        n.f32 = flonum->value;
                        m_frame[m_count++] = n.u64;
                    }
                    return NULL;
                }
                if (signature == 'd') {
                    n.f64 = flonum->value;
                    if (m_sse_count < array_sizeof(m_sse)) {
                        m_sse[m_sse_count++] = n.u64;
                    } else {
                        m_frame[m_count++] = n.u64;
                    }
                    return NULL;
                }
                goto bad_signature;
            }
            if (BVECTORP(obj)) {
                if (signature != 'p') goto bad_signature;
                scm_bvector_t bvector = (scm_bvector_t)obj;
                if (m_reg_count < array_sizeof(m_reg)) {
                    m_reg[m_reg_count++] = (intptr_t)bvector->elts;
                } else {
                    m_frame[m_count++] = (intptr_t)bvector->elts;
                }
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
                if (m_reg_count < array_sizeof(m_reg)) {
                    m_reg[m_reg_count++] = (intptr_t)bvector->elts;
                } else {
                    m_frame[m_count++] = (intptr_t)bvector->elts;
                }
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
#endif

#if _MSC_VER
    intptr_t
    stdcall_func_stub_intptr(void* adrs, int argc, intptr_t argv[])
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
    stdcall_func_stub_int64(void* adrs, int argc, intptr_t argv[])
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

    float
    stdcall_func_stub_float(void* adrs, int argc, intptr_t argv[])
    {
        int bytes = (argc * sizeof(intptr_t) + 15) & ~15;
        float retval;

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

    double
    stdcall_func_stub_double(void* adrs, int argc, intptr_t argv[])
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

    intptr_t
    c_func_stub_intptr(void* adrs, int argc, intptr_t argv[])
    {
        return stdcall_func_stub_intptr(adrs, argc, argv);
    }

    int64_t
    c_func_stub_int64(void* adrs, int argc, intptr_t argv[])
    {
        return stdcall_func_stub_int64(adrs, argc, argv);
    }

    double
    c_func_stub_double(void* adrs, int argc, intptr_t argv[])
    {
        return stdcall_func_stub_double(adrs, argc, argv);
    }

    float
    c_func_stub_float(void* adrs, int argc, intptr_t argv[])
    {
        return stdcall_func_stub_float(adrs, argc, argv);
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
        uint32_t    m_uid;
        uint32_t    m_argc;

        static uint8_t* s_pool;
        static uint8_t* s_pool_limit;
        static int s_pool_alloc_size;

        void* operator new(size_t size);
        trampoline_t(intptr_t stub, intptr_t uid, int argc);
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

    trampoline_t::trampoline_t(intptr_t stub, intptr_t uid, int argc)
    {
        m_stub = stub;
        m_uid = uid;
        m_argc = argc;
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

    intptr_t callback_intptr(uint32_t uid, uint32_t argc, uint32_t* base)
    {
        scm_obj_t obj = get_hashtable(current_vm()->m_heap->m_trampolines, MAKEFIXNUM(uid));
        if (!CLOSUREP(obj)) fatal("fatal: callback was destroyed\n[exit]\n");
        scm_obj_t result;
        try {
            VM* vm = current_vm();
             scm_obj_t* argv = (scm_obj_t*)alloca(sizeof(scm_obj_t*) * argc);
            for (int i = 0; i < argc; i++) argv[i] = intptr_to_integer(vm->m_heap, base[i]);
            result = vm->call_scheme_argv((scm_closure_t)obj, argc, argv);
        } catch (vm_exit_t& e) {
            exit(e.m_code);
        } catch (...) {
            fatal("fatal: unhandled exception in callback\n[exit]\n");
        }
        if (exact_integer_pred(result)) {
            return coerce_exact_integer_to_intptr(result);
        } else {
            return 0;
        }
    }
    
    int64_t callback_int64(uint32_t uid, uint32_t argc, uint32_t* base)
    {
        scm_obj_t obj = get_hashtable(current_vm()->m_heap->m_trampolines, MAKEFIXNUM(uid));
        if (!CLOSUREP(obj)) fatal("fatal: callback was destroyed\n[exit]\n");
        scm_obj_t result;
        try {
            VM* vm = current_vm();
             scm_obj_t* argv = (scm_obj_t*)alloca(sizeof(scm_obj_t*) * argc);
            for (int i = 0; i < argc; i++) argv[i] = intptr_to_integer(vm->m_heap, base[i]);
            result = vm->call_scheme_argv((scm_closure_t)obj, argc, argv);
        } catch (vm_exit_t& e) {
            exit(e.m_code);
        } catch (...) {
            fatal("fatal: unhandled exception in callback\n[exit]\n");
        }
        if (exact_integer_pred(result)) {
            return coerce_exact_integer_to_int64(result);
        } else {
            return 0;
        }
    }
    
    float callback_float(uint32_t uid, uint32_t argc, uint32_t* base)
    {
        scm_obj_t obj = get_hashtable(current_vm()->m_heap->m_trampolines, MAKEFIXNUM(uid));
        if (!CLOSUREP(obj)) fatal("fatal: callback was destroyed\n[exit]\n");
        scm_obj_t result;
        try {
            VM* vm = current_vm();
             scm_obj_t* argv = (scm_obj_t*)alloca(sizeof(scm_obj_t*) * argc);
            for (int i = 0; i < argc; i++) argv[i] = intptr_to_integer(vm->m_heap, base[i]);
            result = vm->call_scheme_argv((scm_closure_t)obj, argc, argv);
        } catch (vm_exit_t& e) {
            exit(e.m_code);
        } catch (...) {
            fatal("fatal: unhandled exception in callback\n[exit]\n");
        }
        if (real_valued_pred(result)) {
            return real_to_double(result);
        } else {
            return 0.0;
        }
    }

    double callback_double(uint32_t uid, uint32_t argc, uint32_t* base)
    {
        scm_obj_t obj = get_hashtable(current_vm()->m_heap->m_trampolines, MAKEFIXNUM(uid));
        if (!CLOSUREP(obj)) fatal("fatal: callback was destroyed\n[exit]\n");
        scm_obj_t result;
        try {
            VM* vm = current_vm();
             scm_obj_t* argv = (scm_obj_t*)alloca(sizeof(scm_obj_t*) * argc);
            for (int i = 0; i < argc; i++) argv[i] = intptr_to_integer(vm->m_heap, base[i]);
            result = vm->call_scheme_argv((scm_closure_t)obj, argc, argv);
        } catch (vm_exit_t& e) {
            exit(e.m_code);
        } catch (...) {
            fatal("fatal: unhandled exception in callback\n[exit]\n");
        }
        if (real_valued_pred(result)) {
            return real_to_double(result);
        } else {
            return 0.0;
        }
    }    

    void __declspec(naked) c_callback_stub_intptr()
    {
        // note: uid adrs in ecx
        uint32_t*   base;
        uint32_t    uid;
        uint32_t    argc;
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
            mov     argc, eax
        }
        value = callback_intptr(uid, argc, base);
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
        uint32_t*   base;
        uint32_t    uid;
        uint32_t    argc;
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
            mov     argc, eax
        }
        value.n64 = callback_int64(uid, argc, base);
        __asm {
            mov     eax, value.u32.lo
            mov     edx, value.u32.hi
            mov     esp, ebp
            pop     ebp
            ret
        }
    }
    
    void __declspec(naked) c_callback_stub_float()
    {
        // note: uid adrs in ecx
        uint32_t*   base;
        uint32_t    uid;
        uint32_t    argc;
        float       value;
        __asm {
            push    ebp
            mov     ebp, esp
            sub     esp, __LOCAL_SIZE
            lea     eax, [ebp + 8]
            mov     base, eax
            mov     eax, [ecx]
            mov     uid, eax
            mov     eax, [ecx + 4]
            mov     argc, eax
        }
        value = callback_float(uid, argc, base);
        __asm {
            fld     value
            mov     esp, ebp
            pop     ebp
            ret
        }
    }
    
    void __declspec(naked) c_callback_stub_double()
    {
        // note: uid adrs in ecx
        uint32_t*   base;
        uint32_t    uid;
        uint32_t    argc;
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
            mov     argc, eax
        }
        value = callback_double(uid, argc, base);
        __asm {
            fld     value
            mov     esp, ebp
            pop     ebp
            ret
        }
    }
    
    void __declspec(naked) stdcall_callback_stub_intptr()
    {
        // note: uid adrs in ecx
        uint32_t*   base;
        uint32_t    uid;
        uint32_t    argc;
        intptr_t    value;
        uint32_t    bytes;
        __asm {
            push    ebp
            mov     ebp, esp
            sub     esp, __LOCAL_SIZE
            lea     eax, [ebp + 8]
            mov     base, eax
            mov     eax, [ecx]
            mov     uid, eax
            mov     eax, [ecx + 4]
            mov     argc, eax
        }
        value = callback_intptr(uid, argc, base);
        bytes = argc * 4;
        __asm {
            mov     ebx, bytes
            mov     eax, value
            mov     esp, ebp
            pop     ebp
            pop     ecx
            add     esp, ebx
            jmp     ecx
        }
    }

    void __declspec(naked) stdcall_callback_stub_int64()
    {
        // note: uid adrs in ecx
        uint32_t*   base;
        uint32_t    uid;
        uint32_t    argc;
        union {
            int64_t n64;
            struct { 
                uint32_t lo;
                uint32_t hi; 
            } u32;
        } value;
        uint32_t    bytes;
        __asm {
            push    ebp
            mov     ebp, esp
            sub     esp, __LOCAL_SIZE
            lea     eax, [ebp + 8]
            mov     base, eax
            mov     eax, [ecx]
            mov     uid, eax
            mov     eax, [ecx + 4]
            mov     argc, eax
        }
        value.n64 = callback_int64(uid, argc, base);
        bytes = argc * 4;
        __asm {
            mov     ebx, bytes
            mov     eax, value.u32.lo
            mov     edx, value.u32.hi
            mov     esp, ebp
            pop     ebp
            pop     ecx
            add     esp, ebx
            jmp     ecx
        }
    }

    void __declspec(naked) stdcall_callback_stub_float()
    {
        // note: uid adrs in ecx
        uint32_t*   base;
        uint32_t    uid;
        uint32_t    argc;
        double      value;
        uint32_t    bytes;
        __asm {
            push    ebp
            mov     ebp, esp
            sub     esp, __LOCAL_SIZE
            lea     eax, [ebp + 8]
            mov     base, eax
            mov     eax, [ecx]
            mov     uid, eax
            mov     eax, [ecx + 4]
            mov     argc, eax
        }
        value = callback_float(uid, argc, base);
        bytes = argc * 4;
        __asm {
            mov     ebx, bytes
            fld     value
            mov     esp, ebp
            pop     ebp
            pop     ecx
            add     esp, ebx
            jmp     ecx
        }
    }
    
    void __declspec(naked) stdcall_callback_stub_double()
    {
        // note: uid adrs in ecx
        uint32_t*   base;
        uint32_t    uid;
        uint32_t    argc;
        double      value;
        uint32_t    bytes;
        __asm {
            push    ebp
            mov     ebp, esp
            sub     esp, __LOCAL_SIZE
            lea     eax, [ebp + 8]
            mov     base, eax
            mov     eax, [ecx]
            mov     uid, eax
            mov     eax, [ecx + 4]
            mov     argc, eax
        }
        value = callback_float(uid, argc, base);
        bytes = argc * 4;
        __asm {
            mov     ebx, bytes
            fld     value
            mov     esp, ebp
            pop     ebp
            pop     ecx
            add     esp, ebx
            jmp     ecx
        }
    }

    scm_obj_t make_callback(VM* vm, int type, int argc, scm_closure_t closure)
    {
        static intptr_t uid;
        trampoline_t* thunk;
        if (type) {
            thunk = new trampoline_t((intptr_t)stdcall_callback_stub_intptr, uid, argc);
        } else {
            thunk = new trampoline_t((intptr_t)c_callback_stub_intptr, uid, argc);
        }
        vm->m_heap->write_barrier(closure);
        {
            scoped_lock lock(vm->m_heap->m_trampolines->lock);
            int nsize = put_hashtable(vm->m_heap->m_trampolines, MAKEFIXNUM(uid), closure);
            if (nsize) rehash_hashtable(vm->m_heap, vm->m_heap->m_trampolines, nsize);
        }
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
        uint32_t    m_uid;
        uint32_t    m_argc;
        static uint8_t* s_pool;
        static uint8_t* s_pool_limit;
        static int s_pool_alloc_size;
        void* operator new(size_t size);
        trampoline_t(intptr_t stub, intptr_t uid, int argc);
    } __attribute__((packed));

    uint8_t* trampoline_t::s_pool;
    uint8_t* trampoline_t::s_pool_limit;
    int trampoline_t::s_pool_alloc_size;

    void* trampoline_t::operator new(size_t size)
    {
        if (s_pool == NULL) {
            s_pool_alloc_size = getpagesize();
        }
        assert(size < s_pool_alloc_size);
        if (s_pool + size > s_pool_limit) {
            s_pool = (uint8_t*)valloc(s_pool_alloc_size);
            if (mprotect(s_pool, s_pool_alloc_size, PROT_READ | PROT_WRITE |PROT_EXEC)) {
                fatal("%s:%u mprotect failed %d", __FILE__, __LINE__, errno);
            }
            s_pool_limit = s_pool + s_pool_alloc_size;
        }
        void* p = s_pool;
        s_pool += size;
        return p;
    }

    trampoline_t::trampoline_t(intptr_t stub, intptr_t uid, int argc)
    {
        m_stub = stub;
        m_uid = uid;
        m_argc = argc;
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

    intptr_t c_callback_intptr(intptr_t uid, intptr_t argc, intptr_t* stack)
    {
        scm_obj_t obj = get_hashtable(current_vm()->m_heap->m_trampolines, MAKEFIXNUM(uid));
        if (!CLOSUREP(obj)) fatal("fatal: callback was destroyed\n[exit]\n");
        scm_obj_t result;
        try {
            VM* vm = current_vm();
            scm_obj_t* argv = (scm_obj_t*)alloca(sizeof(scm_obj_t*) * argc);
            for (int i = 0; i < argc; i++) argv[i] = intptr_to_integer(vm->m_heap, stack[i]);
            result = vm->call_scheme_argv((scm_closure_t)obj, argc, argv);
        } catch (vm_exit_t& e) {
            exit(e.m_code);
        } catch (...) {
            fatal("fatal: unhandled exception in callback\n[exit]\n");
        }
        if (exact_integer_pred(result)) {
            return coerce_exact_integer_to_intptr(result);
        } else {
            return 0;
        }
    }
    
    int64_t c_callback_int64(intptr_t uid, intptr_t argc, intptr_t* stack)
    {
        scm_obj_t obj = get_hashtable(current_vm()->m_heap->m_trampolines, MAKEFIXNUM(uid));
        if (!CLOSUREP(obj)) fatal("fatal: callback was destroyed\n[exit]\n");
        scm_obj_t result;
        try {
            VM* vm = current_vm();
            scm_obj_t* argv = (scm_obj_t*)alloca(sizeof(scm_obj_t*) * argc);
            for (int i = 0; i < argc; i++) argv[i] = intptr_to_integer(vm->m_heap, stack[i]);
            result = vm->call_scheme_argv((scm_closure_t)obj, argc, argv);
        } catch (vm_exit_t& e) {
            exit(e.m_code);
        } catch (...) {
            fatal("fatal: unhandled exception in callback\n[exit]\n");
        }
        if (exact_integer_pred(result)) {
            return coerce_exact_integer_to_int64(result);
        } else {
            return 0;
        }
    }
    
    float c_callback_float(intptr_t uid, intptr_t argc, intptr_t* stack)
    {
        scm_obj_t obj = get_hashtable(current_vm()->m_heap->m_trampolines, MAKEFIXNUM(uid));
        if (!CLOSUREP(obj)) fatal("fatal: callback was destroyed\n[exit]\n");
        scm_obj_t result;
        try {
            VM* vm = current_vm();
            scm_obj_t* argv = (scm_obj_t*)alloca(sizeof(scm_obj_t*) * argc);
            for (int i = 0; i < argc; i++) argv[i] = intptr_to_integer(vm->m_heap, stack[i]);
            result = vm->call_scheme_argv((scm_closure_t)obj, argc, argv);
        } catch (vm_exit_t& e) {
            exit(e.m_code);
        } catch (...) {
            fatal("fatal: unhandled exception in callback\n[exit]\n");
        }
        if (real_valued_pred(result)) {
            return real_to_double(result);
        } else {
            return 0.0;
        }
    }        
        
    double c_callback_double(intptr_t uid, intptr_t argc, intptr_t* stack)
    {
        scm_obj_t obj = get_hashtable(current_vm()->m_heap->m_trampolines, MAKEFIXNUM(uid));
        if (!CLOSUREP(obj)) fatal("fatal: callback was destroyed\n[exit]\n");
        scm_obj_t result;
        try {
            VM* vm = current_vm();
            scm_obj_t* argv = (scm_obj_t*)alloca(sizeof(scm_obj_t*) * argc);
            for (int i = 0; i < argc; i++) argv[i] = intptr_to_integer(vm->m_heap, stack[i]);
            result = vm->call_scheme_argv((scm_closure_t)obj, argc, argv);
        } catch (vm_exit_t& e) {
            exit(e.m_code);
        } catch (...) {
            fatal("fatal: unhandled exception in callback\n[exit]\n");
        }
        if (real_valued_pred(result)) {
            return real_to_double(result);
        } else {
            return 0.0;
        }
    }        

    scm_obj_t make_callback(VM* vm, int type, int argc, scm_closure_t closure)
    {
        static intptr_t uid;
        trampoline_t* thunk = new trampoline_t((intptr_t)c_callback_stub_intptr, uid, argc);
        vm->m_heap->write_barrier(closure);
        {
            scoped_lock lock(vm->m_heap->m_trampolines->lock);
            int nsize = put_hashtable(vm->m_heap->m_trampolines, MAKEFIXNUM(uid), closure);
            if (nsize) rehash_hashtable(vm->m_heap, vm->m_heap->m_trampolines, nsize);
        }
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
        uint64_t    m_uid;
        uint64_t    m_argc;
        static uint8_t* s_pool;
        static uint8_t* s_pool_limit;
        static int s_pool_alloc_size;
        void* operator new(size_t size);
        trampoline_t(intptr_t stub, intptr_t uid, int argc);
    } __attribute__((packed));

    uint8_t* trampoline_t::s_pool;
    uint8_t* trampoline_t::s_pool_limit;
    int trampoline_t::s_pool_alloc_size;

    void* trampoline_t::operator new(size_t size)
    {
        if (s_pool == NULL) {
            s_pool_alloc_size = getpagesize();
        }
        assert(size < s_pool_alloc_size);
        if (s_pool + size > s_pool_limit) {
            s_pool = (uint8_t*)valloc(s_pool_alloc_size);
            if (mprotect(s_pool, s_pool_alloc_size, PROT_READ | PROT_WRITE |PROT_EXEC)) {
                fatal("%s:%u mprotect failed %d", __FILE__, __LINE__, errno);
            }
            s_pool_limit = s_pool + s_pool_alloc_size;
        }
        void* p = s_pool;
        s_pool += size;
        return p;
    }

    trampoline_t::trampoline_t(intptr_t stub, intptr_t uid, int argc)
    {
        m_stub = stub;
        m_uid = uid;
        m_argc = argc;
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

    intptr_t c_callback_intptr_x64(intptr_t uid, intptr_t argc, intptr_t* reg, intptr_t* stack)
    {
        scm_obj_t obj = get_hashtable(current_vm()->m_heap->m_trampolines, MAKEFIXNUM(uid));
        if (!CLOSUREP(obj)) fatal("fatal: callback was destroyed\n[exit]\n");
        scm_obj_t result;
        try {
            VM* vm = current_vm();
            scm_obj_t* argv = (scm_obj_t*)alloca(sizeof(scm_obj_t*) * argc);
            for (int i = 0; i < argc; i++) {
                if (i < 6) argv[i] = intptr_to_integer(vm->m_heap, reg[i]);
                else argv[i] = intptr_to_integer(vm->m_heap, stack[i - 6]);
            }
            result = vm->call_scheme_argv((scm_closure_t)obj, argc, argv);
        } catch (vm_exit_t& e) {
            exit(e.m_code);
        } catch (...) {
            fatal("fatal: unhandled exception in callback\n[exit]\n");
        }
        if (exact_integer_pred(result)) {
            return coerce_exact_integer_to_intptr(result);
        } else {
            return 0;
        }
    }
    
    float c_callback_float_x64(intptr_t uid, intptr_t argc, intptr_t* reg, intptr_t* stack)
    {
        scm_obj_t obj = get_hashtable(current_vm()->m_heap->m_trampolines, MAKEFIXNUM(uid));
        if (!CLOSUREP(obj)) fatal("fatal: callback was destroyed\n[exit]\n");
        scm_obj_t result;
        try {
            VM* vm = current_vm();
            scm_obj_t* argv = (scm_obj_t*)alloca(sizeof(scm_obj_t*) * argc);
            for (int i = 0; i < argc; i++) {
                if (i < 6) argv[i] = intptr_to_integer(vm->m_heap, reg[i]);
                else argv[i] = intptr_to_integer(vm->m_heap, stack[i - 6]);
            }
            result = vm->call_scheme_argv((scm_closure_t)obj, argc, argv);
        } catch (vm_exit_t& e) {
            exit(e.m_code);
        } catch (...) {
            fatal("fatal: unhandled exception in callback\n[exit]\n");
        }
        if (real_valued_pred(result)) {
            return real_to_double(result);
        } else {
            return 0.0;
        }
    }

    double c_callback_double_x64(intptr_t uid, intptr_t argc, intptr_t* reg, intptr_t* stack)
    {
        scm_obj_t obj = get_hashtable(current_vm()->m_heap->m_trampolines, MAKEFIXNUM(uid));
        if (!CLOSUREP(obj)) fatal("fatal: callback was destroyed\n[exit]\n");
        scm_obj_t result;
        try {
            VM* vm = current_vm();
            scm_obj_t* argv = (scm_obj_t*)alloca(sizeof(scm_obj_t*) * argc);
            for (int i = 0; i < argc; i++) {
                if (i < 6) argv[i] = intptr_to_integer(vm->m_heap, reg[i]);
                else argv[i] = intptr_to_integer(vm->m_heap, stack[i - 6]);
            }
            result = vm->call_scheme_argv((scm_closure_t)obj, argc, argv);
        } catch (vm_exit_t& e) {
            exit(e.m_code);
        } catch (...) {
            fatal("fatal: unhandled exception in callback\n[exit]\n");
        }
        if (real_valued_pred(result)) {
            return real_to_double(result);
        } else {
            return 0.0;
        }
    }

    scm_obj_t make_callback(VM* vm, int type, int argc, scm_closure_t closure)
    {
        static intptr_t uid;
        trampoline_t* thunk = new trampoline_t((intptr_t)c_callback_stub_intptr_x64, uid, argc);
        vm->m_heap->write_barrier(closure);
        {
            scoped_lock lock(vm->m_heap->m_trampolines->lock);
            int nsize = put_hashtable(vm->m_heap->m_trampolines, MAKEFIXNUM(uid), closure);
            if (nsize) rehash_hashtable(vm->m_heap, vm->m_heap->m_trampolines, nsize);
        }
        uid++;
        assert(uid < FIXNUM_MAX);
        return uintptr_to_integer(vm->m_heap, (uintptr_t)thunk);
    }
#else
    scm_obj_t make_callback(VM* vm, int type, int argc, scm_closure_t closure)
    {
        fatal("%s:%u ffi not supported on this build", __FILE__, __LINE__);
    }

    intptr_t
    c_func_stub_intptr(void* adrs, int argc, intptr_t argv[])
    {
        fatal("%s:%u ffi not supported on this build", __FILE__, __LINE__);
    }

    double
    c_func_stub_double(void* adrs, int argc, intptr_t argv[])
    {
        fatal("%s:%u ffi not supported on this build", __FILE__, __LINE__);
    }
#endif
