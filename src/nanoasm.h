/*
    Ypsilon Scheme System
    Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#ifndef NANOASM_H_INCLUDED
#define NANOASM_H_INCLUDED

#include "core.h"

#if ARCH_IA32 || ARCH_X64

#include <map>
#include <string>
#include <iostream>

#if ASDEBUG
  #define ASSEMBLE_ERROR(X) fatal("error:%s:%u %s", __FILE__, __LINE__, X)
#else
  #define ASSEMBLE_ERROR(X) do {} while (0)
#endif

class nanoasm_t {
public:
    nanoasm_t();

    struct reg8_t {
        const uint8_t m_regcode;
        explicit reg8_t(int value) :
            m_regcode(value) {
        }
    };

    struct reg32_t {
        const uint8_t m_regcode;
        explicit reg32_t(int value) :
            m_regcode(value) {
        }
    };

    struct reg64_t {
        const uint8_t m_regcode;
        explicit reg64_t(int value) :
            m_regcode(value) {
        }
    };

    struct symbol_t : std::string {
        explicit symbol_t() {}
        explicit symbol_t(const char* name) : std::string(name) { }
    };

#if ARCH_LP64
    const reg64_t rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi, rip;
    const reg64_t r8, r9, r10, r11, r12, r13, r14, r15;
#endif
    const reg32_t eax, ecx, edx, ebx, esp, ebp, esi, edi;
    const reg8_t al, cl, dl, bl;

private:
    enum { undefined = 0x40 };

    enum {
        regcode_rax = 0,
        regcode_rcx = 1,
        regcode_rsp = 4,
        regcode_rbp = 5,
        regcode_r12 = 12,
        regcode_r13 = 13,
        regcode_rip = 16
    };

    struct aform_t {
        uint8_t m_octets[8];
        int m_bytecount;
        void set_disp32(int i, int32_t disp) {
            *(int32_t*)(m_octets + i) = disp;
        }
    };

    struct amode_t {
        intptr_t m_disp;
        uint8_t m_base;
        amode_t() {}
        amode_t(uint8_t base, intptr_t disp) {
            m_disp = disp;
            m_base = base;
        }
        void set(uint8_t base, intptr_t disp) {
            m_disp = disp;
            m_base = base;
        }
#if ARCH_LP64
        bool direct64() const {
            return (m_base == undefined) && (m_disp < INT32_MIN || m_disp > INT32_MAX);
        }
        bool direct32() const {
            assert(false);
            return false;
        }
#else
        bool direct64() const {
            assert(false);
            return false;
        }
        bool direct32() const {
            return (m_base == undefined);
        }
#endif
    };

    struct amode_si_t {
        intptr_t m_disp;
        uint8_t m_base;
        uint8_t m_index;
        uint8_t m_scale;
        amode_si_t() {}
        amode_si_t(uint8_t base, uint8_t index, uint8_t scale, intptr_t disp) {
            m_disp = disp;
            m_base = base;
            m_index = index;
            m_scale = scale;
        }
        void set(uint8_t base, uint8_t index, uint8_t scale, intptr_t disp) {
            m_disp = disp;
            m_base = base;
            m_index = index;
            m_scale = scale;
        }
#if ARCH_LP64
        bool direct64() const {
            return (m_base == undefined) && (m_index == undefined) && (m_disp < INT32_MIN || m_disp > INT32_MAX);
        }
        bool direct32() const {
            assert(false);
            return false;
        }
#else
        bool direct64() const {
            assert(false);
            return false;
        }
        bool direct32() const {
            return (m_base == undefined) && (m_index == undefined);
        }
#endif
    };

    struct mem8_t : amode_t {};
    struct mem32_t : amode_t {};
    struct mem64_t : amode_t {};
    struct mem8si_t : amode_si_t {};
    struct mem32si_t : amode_si_t {};
    struct mem64si_t : amode_si_t {};

    struct qword_t {
        mem64_t m_mem64;
        mem64si_t m_mem64sib;
        const mem64_t& operator[](const amode_t& adrs) {
            m_mem64.set(adrs.m_base, adrs.m_disp);
            return m_mem64;
        }
        const mem64si_t& operator[](const amode_si_t& adrs) {
            m_mem64sib.set(adrs.m_base, adrs.m_index, adrs.m_scale, adrs.m_disp);
            return m_mem64sib;
        }
        const mem64_t& operator[](intptr_t direct) {
            m_mem64.set(undefined, direct);
            return m_mem64;
        }
    #if ARCH_LP64
        const mem64_t& operator[](const reg64_t& reg) {
            m_mem64.set(reg.m_regcode, 0);
            return m_mem64;
        }
    #else
        const mem64_t& operator[](const reg32_t& reg) {
            m_mem64.set(reg.m_regcode, 0);
            return m_mem64;
        }
    #endif
    };

    struct dword_t {
        mem32_t m_mem32;
        mem32si_t m_mem32sib;
        const mem32_t& operator[](const amode_t& adrs) {
            m_mem32.set(adrs.m_base, adrs.m_disp);
            return m_mem32;
        }
        const mem32si_t& operator[](const amode_si_t& adrs) {
            m_mem32sib.set(adrs.m_base, adrs.m_index, adrs.m_scale, adrs.m_disp);
            return m_mem32sib;
        }
        const mem32_t& operator[](intptr_t direct) {
            m_mem32.set(undefined, direct);
            return m_mem32;
        }
    #if ARCH_LP64
        const mem32_t& operator[](const reg64_t& reg) {
            m_mem32.set(reg.m_regcode, 0);
            return m_mem32;
        }
    #else
        const mem32_t& operator[](const reg32_t& reg) {
            m_mem32.set(reg.m_regcode, 0);
            return m_mem32;
        }
    #endif
    };

    struct byte_t {
        mem8_t m_mem8;
        mem8si_t m_mem8sib;
        const mem8_t& operator[](const amode_t& adrs) {
            m_mem8.set(adrs.m_base, adrs.m_disp);
            return m_mem8;
        }
        const mem8si_t& operator[](const amode_si_t& adrs) {
            m_mem8sib.set(adrs.m_base, adrs.m_index, adrs.m_scale, adrs.m_disp);
            return m_mem8sib;
        }
        const mem8_t& operator[](intptr_t direct) {
            m_mem8.set(undefined, direct);
            return m_mem8;
        }
    #if ARCH_LP64
        const mem8_t& operator[](const reg64_t& reg) {
            m_mem8.set(reg.m_regcode, 0);
            return m_mem8;
        }
    #else
        const mem8_t& operator[](const reg32_t& reg) {
            m_mem8.set(reg.m_regcode, 0);
            return m_mem8;
        }
    #endif
    };

    int rex(uint8_t dst, uint8_t src);
    int rex(const reg64_t& dst, const reg64_t& src);
    int rex(const reg64_t& reg, const amode_t& amode);
    int rex(const reg64_t& reg, const amode_si_t& amode);
    int rex(const reg64_t& reg);
    int rex(const amode_t& amode);
    int rex(const amode_si_t& amode);
    int mod(uint8_t dst, uint8_t src);
    int mod(uint8_t dst, const reg8_t& src);
    int mod(uint8_t dst, const reg32_t& src);
    int mod(uint8_t dst, const reg64_t& src);
    int mod(const reg8_t& dst, const reg8_t& src);
    int mod(const reg32_t& dst, const reg32_t& src);
    int mod(const reg64_t& dst, const reg64_t& src);
    aform_t mod(uint8_t reg, uint8_t base, uint8_t index, uint8_t scale, intptr_t disp);
    aform_t mod(uint8_t reg, const amode_t& amode);
    aform_t mod(uint8_t reg, const amode_si_t& amode);
    aform_t mod(const reg8_t& reg, const amode_t& amode);
    aform_t mod(const reg32_t& reg, const amode_t& amode);
    aform_t mod(const reg64_t& reg, const amode_t& amode);
    aform_t mod(const reg8_t& reg, const amode_si_t& amode);
    aform_t mod(const reg32_t& reg, const amode_si_t& amode);
    aform_t mod(const reg64_t& reg, const amode_si_t& amode);
    amode_si_t optimize_amode_si(const amode_si_t& amode);

public:
    qword_t qword;
    dword_t dword;
    byte_t byte;

#if ARCH_LP64
    friend amode_t operator+(const reg64_t&, intptr_t);
    friend amode_t operator+(const amode_t&, intptr_t);
    friend amode_t operator-(const reg64_t&, intptr_t);
    friend amode_t operator-(const amode_t&, intptr_t);
    friend amode_si_t operator*(const reg64_t&, int);
    friend amode_si_t operator+(const reg64_t&, const reg64_t&);
    friend amode_si_t operator+(const reg64_t&, const amode_si_t&);
    friend amode_si_t operator+(const amode_si_t&, const reg64_t&);
    friend amode_si_t operator+(const amode_si_t&, intptr_t);
    friend amode_si_t operator-(const amode_si_t&, intptr_t);
#else
    friend amode_t operator+(const reg32_t&, intptr_t);
    friend amode_t operator+(const amode_t&, intptr_t);
    friend amode_t operator-(const reg32_t&, intptr_t);
    friend amode_t operator-(const amode_t&, intptr_t);
    friend amode_si_t operator*(const reg32_t&, int);
    friend amode_si_t operator+(const reg32_t&, const reg32_t&);
    friend amode_si_t operator+(const reg32_t&, const amode_si_t&);
    friend amode_si_t operator+(const amode_si_t&, const reg32_t&);
    friend amode_si_t operator+(const amode_si_t&, intptr_t);
    friend amode_si_t operator-(const amode_si_t&, intptr_t);
#endif

private:
    struct reloc_t {
        reloc_t(uintptr_t target, int size, bool absolute)
            : m_target(target), m_size(size), m_absolute(absolute) { }
        uintptr_t m_target;
        int m_size;
        bool m_absolute;
    };

    typedef std::map<const symbol_t, const uintptr_t> symbol_map_t;
    typedef std::multimap<const symbol_t, const reloc_t> reloc_map_t;
    symbol_map_t m_symbol;
    reloc_map_t m_reloc_map;
    uintptr_t m_org;
    uintptr_t m_limit;
    uintptr_t m_pc;
    int m_unique_count;

    uintptr_t absolute_reloc(const symbol_t& target);
    int32_t check_reloc(int64_t rel, int size);
    int32_t relative_reloc(const symbol_t& target);
    int32_t branch_reloc(const symbol_t& target, int size);
    int32_t branch_reloc8(const symbol_t& target);
    int32_t branch_reloc32(const symbol_t& target);
    bool branch_inrel8(const symbol_t& target);
    bool branch_inrel32(const symbol_t& target);
    void resolve(const symbol_t& symbol);
    void bind(const symbol_t& symbol, uintptr_t value);
    void emit_b8(uint8_t i8);
    void emit_b32(int32_t i32);
    void emit_b64(int64_t i64);
    void emit_mod(const aform_t& aform);
    void emit_mod(uint8_t i8);

public:
    void org(void* adrs, int size);
    uintptr_t commit();
    symbol_t common(const char* name);
    symbol_t unique(const char* hint = NULL);
    void label(const symbol_t& symbol);
    void equ(const symbol_t& symbol, void* value);
    void align(int n);
    void align_bits(int width, int bits);
    void db(uint8_t u8);
    void ds(const char* s);
    void dd(uint32_t u32);
    void dq(uint64_t u64);
#if ARCH_LP64
    void dq(const symbol_t& symbol);
#else
    void dd(const symbol_t& symbol);
#endif

    #include "nanoasm.inc.h"

};

#if ARCH_LP64
    inline nanoasm_t::amode_t operator+(const nanoasm_t::reg64_t& base, intptr_t disp) {
        return nanoasm_t::amode_t(base.m_regcode, disp);
    }

    inline nanoasm_t::amode_t operator+(const nanoasm_t::amode_t& amode, intptr_t disp) {
        return nanoasm_t::amode_t(amode.m_base, amode.m_disp + disp);
    }

    inline nanoasm_t::amode_t operator-(const nanoasm_t::reg64_t& base, intptr_t disp) {
        return nanoasm_t::amode_t(base.m_regcode, - disp);
    }

    inline nanoasm_t::amode_t operator-(const nanoasm_t::amode_t& amode, intptr_t disp) {
        return nanoasm_t::amode_t(amode.m_base, amode.m_disp - disp);
    }

    inline nanoasm_t::amode_si_t operator*(const nanoasm_t::reg64_t& index, int scale) {
        return nanoasm_t::amode_si_t(nanoasm_t::undefined, index.m_regcode, scale, 0);
    }

    inline nanoasm_t::amode_si_t operator+(const nanoasm_t::reg64_t& base, const nanoasm_t::reg64_t& index) {
        return nanoasm_t::amode_si_t(base.m_regcode, index.m_regcode, 1, 0);
    }

    inline nanoasm_t::amode_si_t operator+(const nanoasm_t::reg64_t& base, const nanoasm_t::amode_si_t& amode) {
        if (amode.m_base != nanoasm_t::undefined) ASSEMBLE_ERROR("invalid base and index register combination");
        return nanoasm_t::amode_si_t(base.m_regcode, amode.m_index, amode.m_scale, amode.m_disp);
    }

    inline nanoasm_t::amode_si_t operator+(const nanoasm_t::amode_si_t& amode, const nanoasm_t::reg64_t& base) {
        if (amode.m_base != nanoasm_t::undefined) ASSEMBLE_ERROR("invalid base and index register combination");
        return nanoasm_t::amode_si_t(base.m_regcode, amode.m_index, amode.m_scale, amode.m_disp);
    }

    inline nanoasm_t::amode_si_t operator+(const nanoasm_t::amode_si_t& amode, intptr_t disp) {
        return nanoasm_t::amode_si_t(amode.m_base, amode.m_index, amode.m_scale, amode.m_disp + disp);
    }

    inline nanoasm_t::amode_si_t operator-(const nanoasm_t::amode_si_t& amode, intptr_t disp) {
        return nanoasm_t::amode_si_t(amode.m_base, amode.m_index, amode.m_scale, amode.m_disp - disp);
    }

#else
    inline nanoasm_t::amode_t operator+(const nanoasm_t::reg32_t& base, intptr_t disp) {
        return nanoasm_t::amode_t(base.m_regcode, disp);
    }

    inline nanoasm_t::amode_t operator+(const nanoasm_t::amode_t& amode, intptr_t disp) {
        return nanoasm_t::amode_t(amode.m_base, amode.m_disp + disp);
    }

    inline nanoasm_t::amode_t operator-(const nanoasm_t::reg32_t& base, intptr_t disp) {
        return nanoasm_t::amode_t(base.m_regcode, - disp);
    }

    inline nanoasm_t::amode_t operator-(const nanoasm_t::amode_t& amode, intptr_t disp) {
        return nanoasm_t::amode_t(amode.m_base, amode.m_disp - disp);
    }

    inline nanoasm_t::amode_si_t operator*(const nanoasm_t::reg32_t& index, int scale) {
        return nanoasm_t::amode_si_t(nanoasm_t::undefined, index.m_regcode, scale, 0);
    }

    inline nanoasm_t::amode_si_t operator+(const nanoasm_t::reg32_t& base, const nanoasm_t::reg32_t& index) {
        return nanoasm_t::amode_si_t(base.m_regcode, index.m_regcode, 1, 0);
    }

    inline nanoasm_t::amode_si_t operator+(const nanoasm_t::reg32_t& base, const nanoasm_t::amode_si_t& amode) {
        if (amode.m_base != nanoasm_t::undefined) ASSEMBLE_ERROR("invalid base and index register combination");
        return nanoasm_t::amode_si_t(base.m_regcode, amode.m_index, amode.m_scale, amode.m_disp);
    }

    inline nanoasm_t::amode_si_t operator+(const nanoasm_t::amode_si_t& amode, const nanoasm_t::reg32_t& base) {
        if (amode.m_base != nanoasm_t::undefined) ASSEMBLE_ERROR("invalid base and index register combination");
        return nanoasm_t::amode_si_t(base.m_regcode, amode.m_index, amode.m_scale, amode.m_disp);
    }

    inline nanoasm_t::amode_si_t operator+(const nanoasm_t::amode_si_t& amode, intptr_t disp) {
        return nanoasm_t::amode_si_t(amode.m_base, amode.m_index, amode.m_scale, amode.m_disp + disp);
    }

    inline nanoasm_t::amode_si_t operator-(const nanoasm_t::amode_si_t& amode, intptr_t disp) {
        return nanoasm_t::amode_si_t(amode.m_base, amode.m_index, amode.m_scale, amode.m_disp - disp);
    }

#endif
    
#endif
#endif
