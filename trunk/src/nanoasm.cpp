/*
    Ypsilon Scheme System
    Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#include "core.h"
#include "nanoasm.h"

nanoasm::nanoasm()
    : al(0), cl(1), dl(2), bl(3)
    , eax(0), ecx(1), edx(2), ebx(3), esp(4), ebp(5), esi(6), edi(7)
#if ARCH_LP64
    , rax(0), rcx(1), rdx(2), rbx(3), rsp(4), rbp(5), rsi(6), rdi(7)
    , r8(8), r9(9), r10(10), r11(11), r12(12), r13(13), r14(14), r15(15), rip(16)
#endif
{
    m_org = m_limit = m_pc = m_unique_count = 0;
}

void
nanoasm::emit_b8(uint8_t i8)
{
    if (m_pc == m_limit) return;
    *(uint8_t*)m_pc = i8;
    m_pc++;
}

void
nanoasm::emit_b32(int32_t i32)
{
    emit_b8(i32 & 0xff);
    emit_b8((i32 >> 8) & 0xff);
    emit_b8((i32 >> 16) & 0xff);
    emit_b8((i32 >> 24) & 0xff);
}

void
nanoasm::emit_b64(int64_t i64)
{
    emit_b8(i64 & 0xff);
    emit_b8((i64 >> 8) & 0xff);
    emit_b8((i64 >> 16) & 0xff);
    emit_b8((i64 >> 24) & 0xff);
    emit_b8((i64 >> 32) & 0xff);
    emit_b8((i64 >> 40) & 0xff);
    emit_b8((i64 >> 48) & 0xff);
    emit_b8((i64 >> 56) & 0xff);
}

void
nanoasm::emit_mod(const aform_t& aform)
{
    for (int i = 0; i < aform.m_bytecount; i++) emit_b8(aform.m_octets[i]);
}

void
nanoasm::emit_mod(uint8_t aform)
{
    emit_b8(aform);
}

int32_t
nanoasm::check_reloc(int64_t rel, int size)
{
    if (size == 1) {
        if (rel < INT8_MIN || rel > INT8_MAX) fatal("error:%s:%u reloc out of 8 bit range", __FILE__, __LINE__);
    } else {
        if (rel < INT32_MIN || rel > INT32_MAX) fatal("error:%s:%u reloc out of 32 bit range", __FILE__, __LINE__);
    }
    return rel;
}

void
nanoasm::resolve(const symbol_t& symbol)
{
    while (true) {
        reloc_map_t::iterator iter = m_reloc_map.find(symbol);
        if (iter == m_reloc_map.end()) break;
        const reloc_t* reloc = &iter->second;
        if (reloc->m_absolute) {
            assert(reloc->m_size == sizeof(uintptr_t));
            *(uintptr_t*)reloc->m_target = m_pc;
        } else {
            int size = reloc->m_size;
            int32_t rel = check_reloc((int64_t)m_pc - reloc->m_target - size, size);
            switch (size) {
                case 1:
                    *(int8_t*)reloc->m_target = rel;
                    break;
                case 4:
                    *(int32_t*)reloc->m_target = rel;
                    break;
                default:
                    ASSEMBLE_ERROR("invalid reloc m_size");
                    break;
            }
        }
        m_reloc_map.erase(iter);
        continue;
    }
}

void
nanoasm::bind(const symbol_t& symbol, uintptr_t value)
{
    symbol_map_t::iterator iter = m_symbol.find(symbol);
    if (iter == m_symbol.end()) {
        symbol_map_t::value_type item(symbol, value);
        m_symbol.insert(item);
        resolve(symbol);
        return;
    }
    fatal("error:%s:%u symbol already bound, %s", __FILE__, __LINE__, symbol.c_str());
}

int32_t
nanoasm::branch_reloc(const symbol_t& target, int size)
{
    symbol_map_t::iterator iter = m_symbol.find(target);
    if (iter == m_symbol.end()) {
        reloc_t reloc(m_pc, size, false);
        reloc_map_t::value_type item(target, reloc);
        m_reloc_map.insert(item);
        return 0;
    }
    return check_reloc((int64_t)iter->second - m_pc - size, size);
}

int32_t
nanoasm::branch_reloc8(const symbol_t& target)
{
    return branch_reloc(target, 1);
}

int32_t
nanoasm::branch_reloc32(const symbol_t& target)
{
    return branch_reloc(target, 4);
}

bool
nanoasm::branch_inrel8(const symbol_t& target)
{
    symbol_map_t::iterator iter = m_symbol.find(target);
    if (iter == m_symbol.end()) return false;
    intptr_t rel = iter->second - (m_pc + 2);
    return (rel >= INT8_MIN) & (rel <= INT8_MAX);
}

bool
nanoasm::branch_inrel32(const symbol_t& target)
{
    symbol_map_t::iterator iter = m_symbol.find(target);
    if (iter == m_symbol.end()) return false;
    intptr_t rel = iter->second - (m_pc + 6);
    return (rel >= INT32_MIN) & (rel <= INT32_MAX);
}

uintptr_t
nanoasm::absolute_reloc(const symbol_t& target)
{
    symbol_map_t::iterator iter = m_symbol.find(target);
    if (iter == m_symbol.end()) {
        reloc_t reloc(m_pc, sizeof(uintptr_t), true);
        reloc_map_t::value_type item(target, reloc);
        m_reloc_map.insert(item);
        return 0;
    }
    return iter->second;
}

int32_t
nanoasm::relative_reloc(const symbol_t& target)
{
    symbol_map_t::iterator iter = m_symbol.find(target);
    if (iter == m_symbol.end()) {
        reloc_t reloc(m_pc, sizeof(int32_t), false);
        reloc_map_t::value_type item(target, reloc);
        m_reloc_map.insert(item);
        return 0;
    }
    return check_reloc((int64_t)iter->second - m_pc - sizeof(int32_t), sizeof(int32_t));
}

nanoasm::symbol_t
nanoasm::unique(const char* info)
{
    char name[64];
    if (ASDEBUG && info) snprintf(name, sizeof(name), ".%s(%x)", info, m_unique_count);
    else snprintf(name, sizeof(name), ".%x", m_unique_count);
    m_unique_count++;
    return symbol_t(name);
}

void
nanoasm::equ(const symbol_t& symbol, void* value)
{
    bind(symbol, (uintptr_t)value);
}

void
nanoasm::label(const symbol_t& symbol)
{
    bind(symbol, m_pc);
}

uintptr_t
nanoasm::commit()
{
#if ASDEBUG
    if (m_reloc_map.empty() != true) {
        std::cerr << "error: unbound target:";
        reloc_map_t::iterator iter = m_reloc_map.begin();
        while (iter != m_reloc_map.end()) {
            std::cerr << " " << iter->first;
            iter++;
        }
        std::cerr << std::endl;
        fatal("fatal:%s:%u", __FILE__, __LINE__);
    }
#endif
    symbol_map_t::iterator iter = m_symbol.begin();
    while (iter != m_symbol.end()) {
        if (iter->first[0] == '.') m_symbol.erase(iter++);
        else iter++;
    }
    return m_pc - m_org;
}

nanoasm::amode_si_t
nanoasm::optimize_amode_si(const amode_si_t& amode)
{
    if (amode.m_scale == 1) {
        if (amode.m_base == undefined) {
            return amode_si_t(amode.m_index, undefined, 1, amode.m_disp);
        } else {
            if (amode.m_index == regcode_rsp) {
                return amode_si_t(amode.m_index, amode.m_base, 1, amode.m_disp);
            }
        }
    } else if (amode.m_scale == 2 && amode.m_base == undefined) {
        return amode_si_t(amode.m_index, amode.m_index, 1, amode.m_disp);
    }
    return amode;
}

nanoasm::aform_t
nanoasm::mod(uint8_t reg, uint8_t base, uint8_t index, uint8_t scale, intptr_t disp)
{
#if ARCH_LP64
    if (disp < INT32_MIN || disp > INT32_MAX) ASSEMBLE_ERROR("displacement out of 32 bit range");
#endif
    if (scale == 1) {
        if (base == undefined) {
            base = index;
            index = scale = undefined;
        } else {
            if (index == regcode_rsp) {
                int temp = index;
                index = scale;
                scale = temp;
            }
        }
    } else if (scale == 2 && base == undefined) {
        base = index;
        scale = 1;
    }
    aform_t aform;
    if (index == regcode_rip) ASSEMBLE_ERROR("rip can not be used as index");
    if (index != undefined || base == regcode_rsp || base == regcode_r12) {
        if (base == regcode_rip) ASSEMBLE_ERROR("invalid register combination");
        int reg_mem = ((reg & 7) << 3) + 0x04;
        if (base == undefined) {
            aform.m_octets[0] = reg_mem;
            aform.set_disp32(2, disp);
            aform.m_bytecount = 6;
        } else {
            if (disp) {
                if (disp >= -128 && disp <= 127) {
                    aform.m_octets[0] = 0x40 + reg_mem;
                    aform.m_octets[2] = disp;
                    aform.m_bytecount = 3;
                } else {
                    aform.m_octets[0] = 0x80 + reg_mem;
                    aform.set_disp32(2, disp);
                    aform.m_bytecount = 6;
                }
            } else {
                if (base == regcode_rbp || base == regcode_r13) {
                    aform.m_octets[0] = 0x40 + reg_mem;
                    aform.m_octets[2] = 0;
                    aform.m_bytecount = 3;
                } else {
                    aform.m_octets[0] = reg_mem;
                    aform.m_bytecount = 2;
                }
            }
        }
        int sib = 0;
        switch (scale) {
            case 1: break;
            case 2: sib = 0x40; break;
            case 4: sib = 0x80; break;
            case 8: sib = 0xC0; break;
            default:
                if (index != undefined) ASSEMBLE_ERROR("invalid scale");
                break;
        }
        if (index == regcode_rsp) ASSEMBLE_ERROR("esp and rsp can not be used as index");
        if (index == undefined) {
            aform.m_octets[1] = sib + 0x20 + (base & 7);
            return aform;
        } else if (base == undefined) {
            aform.m_octets[1] = sib + ((index & 7) << 3) + 0x05;
            return aform;
        } else {
            aform.m_octets[1] = sib + ((index & 7) << 3) + (base & 7);
        }
        return aform;
    }
#if ARCH_LP64
    if (base == regcode_rip && index == undefined) {
        aform.m_octets[0] = 0x05 + ((reg & 7) << 3);
        aform.set_disp32(1, disp);
        aform.m_bytecount = 5;
        return aform;
    }
    if (base == undefined && index == undefined) {
        aform.m_octets[0] = 0x04 + ((reg & 7) << 3);
        aform.m_octets[1] = 0x25;
        aform.set_disp32(2, disp);
        aform.m_bytecount = 6;
        return aform;
    }
#else
    if (base == undefined && index == undefined) {
        aform.m_octets[0] = 0x05 + ((reg & 7) << 3);
        aform.set_disp32(1, disp);
        aform.m_bytecount = 5;
        return aform;
    }
#endif
    if (reg == regcode_rip) ASSEMBLE_ERROR("rip can not be used as operand");
    int reg_mem = ((reg & 7) << 3) + (base & 7);
    if (disp) {
        if (disp >= -128 && disp <= 127) {
            aform.m_octets[0] = 0x40 + reg_mem;
            aform.m_octets[1] = disp;
            aform.m_bytecount = 2;
            return aform;
        } else {
            aform.m_octets[0] = 0x80 + reg_mem;
            aform.set_disp32(1, disp);
            aform.m_bytecount = 5;
            return aform;
        }
    }
    if (base == regcode_rbp || base == regcode_r13) {
        aform.m_octets[0] = 0x40 + reg_mem;
        aform.m_octets[1] = 0;
        aform.m_bytecount = 2;
        return aform;
    }
    aform.m_octets[0] = reg_mem;
    aform.m_bytecount = 1;
    return aform;
}

#if ARCH_LP64
#define REX(X)      if (X) emit_b8(0x40+(X))
#else
#define REX(X)      do {} while(0)
#endif
#define W           0x08
#define RXB         rex
#define DB(X)       emit_b8(X)
#define DD(X)       emit_b32(X)
#define DQ(X)       emit_b64(X)
#define MOD(X,Y)    emit_mod(mod((X), (Y)))
#define REL8(X)     emit_b8(branch_reloc8(X))
#define REL32(X)    emit_b32(branch_reloc32(X))
#define INREL8(X)   branch_inrel8(X)
#define INREL32(X)  branch_inrel32(X)
#define NA          undefined
#define RAX         regcode_rax

void nanoasm::org(void* adrs, int size)
{
    m_org = m_pc = (uintptr_t)adrs;
    m_limit = m_org + size;
}

void nanoasm::align(int n)
{
    if (n != 2 && n !=4 && n != 8 && n != 16) ASSEMBLE_ERROR("bad align value");
    int pad = ((m_pc + (n - 1)) & ~(n - 1)) - m_pc;
    for (int i = 0; i < pad; i++) emit_b8(0x90);
}

void nanoasm::align_bits(int width, int bits)
{
    int pad = 0;
    while (((m_pc + pad) & ((1 << width) - 1)) != bits) pad++;
    for (int i = 0; i < pad; i++) emit_b8(0x90);
}

void nanoasm::db(uint8_t u8)
{
    DB(u8);
}
void nanoasm::dd(uint32_t u32)
{
    DD(u32);
}
void nanoasm::dq(uint64_t u64)
{
    DQ(u64);
}
void nanoasm::ds(const char* s)
{
    while (*s) DB(*s++); DB(0);
}

#if ARCH_LP64
    void nanoasm::dq(const symbol_t& symbol)
    {
        symbol_map_t::iterator iter = m_symbol.find(symbol);
        if (iter == m_symbol.end()) {
            absolute_reloc(symbol);
            DQ(0);
        } else {
            DQ(iter->second);
        }
    }
#else
    void nanoasm::dd(const symbol_t& symbol)
    {
        symbol_map_t::iterator iter = m_symbol.find(symbol);
        if (iter == m_symbol.end()) {
            absolute_reloc(symbol);
            DD(0);
        } else {
            DD(iter->second);
        }
    }
#endif

#include "nanoasm.inc.cpp"
