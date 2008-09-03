/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#ifndef FASL_H_INCLUDED
#define FASL_H_INCLUDED

#include "core.h"
#include "hash.h"
#include "port.h"

#define FASL_DEBUG                  0

#define FASL_EOF                    0
#define FASL_TAG_LOOKUP             1
#define FASL_TAG_FIXNUM             2
#define FASL_TAG_PLIST              3
#define FASL_TAG_DLIST              4
#define FASL_TAG_VECTOR             5
#define FASL_TAG_RATIONAL           6
#define FASL_TAG_COMPLEX            7
#define FASL_TAG_FLONUM             8
#define FASL_TAG_BIGNUM             9
#define FASL_TAG_BVECTOR            10
#define FASL_TAG_CHAR               11
#define FASL_TAG_NIL                12
#define FASL_TAG_T                  13
#define FASL_TAG_F                  14
#define FASL_TAG_SYMBOL             15
#define FASL_TAG_STRING             16
#define FASL_TAG_UNINTERNED_SYMBOL  17


class fasl_printer_t {
    VM*             m_vm;
    scm_port_t      m_port;
    scm_hashtable_t m_lites;
    scm_obj_t*      m_stack;
    scm_obj_t*      m_stack_limit;
    scm_obj_t*      m_sp;

    void scan(scm_obj_t obj);
    void put_lites();
    void put_list(scm_obj_t obj);
    void put_datum(scm_obj_t obj);

    void emit_u8(uint8_t octet)
    {
        port_put_byte(m_port, octet);
    }

    void emit_u32(uint32_t n)
    {
        for (int i = 0; i < 5; i++) {
            int code = n & 0x7f;
            n = n >> 7;
            if (n == 0) {
                emit_u8(code | 0x80);
                break;
            } else {
                emit_u8(code);
            }
        }
    }

    void emit_u64(uint64_t n)
    {
        for (int i = 0; i < 8; i++) {
            emit_u8(n & 0xff);
            n = n >> 8;
        }
    }

    void emit_bytes(const char* s, int n)
    {
        for (int i = 0; i < n; i++) emit_u8(s[i]);
    }

    void push(scm_obj_t obj) {
        if (m_sp == m_stack_limit) {
            int n = m_sp - m_stack;
            int depth = (m_stack_limit - m_stack) * 2;
            m_stack = (scm_obj_t*)realloc(m_stack, sizeof(scm_obj_t) * depth);
            if (m_stack == NULL) fatal("%s:%u memory overflow on realloc fasl stack", __FILE__, __LINE__);
            m_stack_limit = m_stack + depth;
            m_sp = m_stack + n;
        }
        m_sp[0] = obj;
        m_sp++;
    }

    scm_obj_t pop() {
        if (m_sp == m_stack) return NULL;
        m_sp--;
        return m_sp[0];
    }

public:
    fasl_printer_t(VM* vm, scm_port_t port);
    ~fasl_printer_t();
    void put(scm_obj_t obj);
};

class fasl_reader_t {
    VM*             m_vm;
    scm_port_t      m_port;
    scm_obj_t*      m_lites;

    uint8_t fetch_u8()
    {
       return port_get_byte(m_port);
    }

    uint32_t fetch_u32()
    {
        uint32_t value = 0;
        int shift = 0;
        while (true) {
            uint8_t octet = port_get_byte(m_port);
            value = value + ((uint32_t)(octet & 0x7f) << shift);
            if (octet & 0x80) return value;
            shift = shift + 7;
        }
    }

    uint64_t fetch_u64()
    {
        uint64_t value = 0;
        int shift = 0;
        for (int i = 0; i < 8; i++) {
            value = value + ((uint64_t)port_get_byte(m_port) << shift);
            shift = shift + 8;
        }
        return value;
    }

    bool get_lites();
    scm_obj_t get_datum();

public:
    fasl_reader_t(VM* vm, scm_port_t port) {
        m_vm = vm;
        m_port = port;
        m_lites = NULL;
    }
    ~fasl_reader_t() {
        if (m_lites) free(m_lites);
    }

    scm_obj_t get();

};
#endif
