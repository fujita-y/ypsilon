/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#include "core.h"
#include "vm.h"
#include "ffi.h"
#include "hash.h"
#include "arith.h"

#if ARCH_IA32

    const char*
    c_stack_frame_t::push(scm_obj_t obj)
    {
        if (m_count < array_sizeof(m_frame)) {
            if (FIXNUMP(obj) || BIGNUMP(obj)) {
                if (n_positive_pred(obj)) {
                    uintptr_t value;
                    if (exact_integer_to_uintptr(obj, &value)) {
                        m_frame[m_count++] = value;
                        return NULL;
                    }
                    return "exact integer between 0 and UINTPTR_MAX";
                } else {
                    intptr_t value;
                    if (exact_integer_to_intptr(obj, &value)) {
                        m_frame[m_count++] = value;
                        return NULL;
                    }
                    return "exact integer between INTPTR_MIN and INTPTR_MAX";
                }
            }
            if (BVECTORP(obj)) {
                scm_bvector_t bvector = (scm_bvector_t)obj;
                m_frame[m_count++] = (intptr_t)bvector->elts;
                return NULL;
            }
            if (VECTORP(obj)) {
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
            if (FLONUMP(obj)) {
                union {
                    double f64;
                    struct {
                        uint32_t lo;
                        uint32_t hi;
                    } u32;
                } n;
                scm_flonum_t flonum = (scm_flonum_t)obj;
                n.f64 = flonum->value;
                m_frame[m_count++] = n.u32.lo;
                m_frame[m_count++] = n.u32.hi;
                return NULL;
            }
            return "exact integer";
        }
        return "internal error: c function stack frame overflow";
    }
    
#endif
    
#if ARCH_X64
    
    const char*
    c_stack_frame_t::push(scm_obj_t obj)
    {
        if (m_count < array_sizeof(m_frame) - array_sizeof(m_reg) - array_sizeof(m_sse)) {
            if (FIXNUMP(obj) || BIGNUMP(obj)) {
                if (n_positive_pred(obj)) {
                    uintptr_t value;
                    if (exact_integer_to_uintptr(obj, &value)) {
                        if (m_reg_count < array_sizeof(m_reg)) {
                            m_reg[m_reg_count++] = value;
                        } else {
                            m_frame[m_count++] = value;
                        }
                        return NULL;
                    }
                    return "exact integer between 0 and UINTPTR_MAX";
                } else {
                    intptr_t value;
                    if (exact_integer_to_intptr(obj, &value)) {
                        if (m_reg_count < array_sizeof(m_reg)) {
                            m_reg[m_reg_count++] = value;
                        } else {
                            m_frame[m_count++] = value;
                        }
                        return NULL;
                    }
                    return "exact integer between INTPTR_MIN and INTPTR_MAX";
                }
            }
            if (BVECTORP(obj)) {
                scm_bvector_t bvector = (scm_bvector_t)obj;
                if (m_reg_count < array_sizeof(m_reg)) {
                    m_reg[m_reg_count++] = (intptr_t)bvector->elts;
                } else {
                    m_frame[m_count++] = (intptr_t)bvector->elts;
                }
                return NULL;
            }
            if (VECTORP(obj)) {
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
            if (FLONUMP(obj)) {
                union {
                    double f64;
                    uint64_t u64;
                } n;
                scm_flonum_t flonum = (scm_flonum_t)obj;
                n.f64 = flonum->value;
                if (m_sse_count < array_sizeof(m_sse)) {
                    m_sse[m_sse_count++] = n.u64;
                } else {
                    m_frame[m_count++] = n.u64;
                }
                return NULL;
            }
            return "exact integer";
        }
        return "internal error: c function stack frame overflow";
    }
    
    void
    c_stack_frame_t::compose()
    {
        int dst = m_count;
        for (int i = 0; i < array_sizeof(m_sse); i++) m_frame[dst++] = m_sse[i];
        for (int i = 0; i < array_sizeof(m_pre); i++) m_frame[dst++] = m_pre[i];
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

    double
    c_func_stub_double(void* adrs, int argc, intptr_t argv[])
    {
        return stdcall_func_stub_double(adrs, argc, argv);
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

    int callback_int(uint32_t uid, uint32_t argc, uint32_t* base)
    {
        scm_obj_t obj = get_hashtable(current_vm()->m_heap->m_trampolines, MAKEFIXNUM(uid));
        assert(CLOSUREP(obj));
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
            intptr_t value;
            if (exact_integer_to_intptr(result, &value) == false) return 0;
            return value;
        } else {
            return 0;
        }
    }

    int __declspec(naked) c_callback_stub_int()
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
        value = callback_int(uid, argc, base);
        __asm {
            mov     eax, value
            mov     esp, ebp
            pop     ebp
            ret
        }
    }

    int __declspec(naked) stdcall_callback_stub_int()
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
        value = callback_int(uid, argc, base);
        bytes = argc * 4;
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

    scm_obj_t make_callback(VM* vm, int type, int argc, scm_closure_t closure)
    {
        static intptr_t uid;
        trampoline_t* thunk;
        if (type) {
            thunk = new trampoline_t((intptr_t)stdcall_callback_stub_int, uid, argc);
        } else {
            thunk = new trampoline_t((intptr_t)c_callback_stub_int, uid, argc);
        }
        vm->m_heap->write_barrier(closure);
        int nsize = put_hashtable(vm->m_heap->m_trampolines, MAKEFIXNUM(uid), closure);
        if (nsize) rehash_hashtable(vm->m_heap, vm->m_heap->m_trampolines, nsize);
        uid++;
        assert(uid < FIXNUM_MAX);
        return intptr_to_integer(vm->m_heap, (intptr_t)thunk);
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
        assert(CLOSUREP(obj));
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
            intptr_t value;
            if (exact_integer_to_intptr(result, &value) == false) return 0;
            return value;
        } else {
            return 0;
        }
    }

    scm_obj_t make_callback(VM* vm, int type, int argc, scm_closure_t closure)
    {
        static intptr_t uid;
        trampoline_t* thunk = new trampoline_t((intptr_t)c_callback_stub_intptr, uid, argc);
        vm->m_heap->write_barrier(closure);
        int nsize = put_hashtable(vm->m_heap->m_trampolines, MAKEFIXNUM(uid), closure);
        if (nsize) rehash_hashtable(vm->m_heap, vm->m_heap->m_trampolines, nsize);
        uid++;
        assert(uid < FIXNUM_MAX);
        return intptr_to_integer(vm->m_heap, (intptr_t)thunk);
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
        assert(CLOSUREP(obj));
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
            intptr_t value;
            if (exact_integer_to_intptr(result, &value) == false) return 0;
            return value;
        } else {
            return 0;
        }
    }

    scm_obj_t make_callback(VM* vm, int type, int argc, scm_closure_t closure)
    {
        static intptr_t uid;
        trampoline_t* thunk = new trampoline_t((intptr_t)c_callback_stub_intptr_x64, uid, argc);
        vm->m_heap->write_barrier(closure);
        int nsize = put_hashtable(vm->m_heap->m_trampolines, MAKEFIXNUM(uid), closure);
        if (nsize) rehash_hashtable(vm->m_heap, vm->m_heap->m_trampolines, nsize);
        uid++;
        assert(uid < FIXNUM_MAX);
        return intptr_to_integer(vm->m_heap, (intptr_t)thunk);
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
