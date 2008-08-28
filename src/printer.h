/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#ifndef PRINTER_H_INCLUDED
#define PRINTER_H_INCLUDED

#include "core.h"
#include "object.h"
#include "list.h"

class printer_t {
public:
                    printer_t(VM* vm, scm_port_t port);
                    ~printer_t();
    void            flush();
    void            format(const char* fmt, ...);
    void            byte(uint8_t c);
    void            ucs4(uint32_t c);
    void            format_va_list(const char* fmt, va_list ap);
    void            column_limit(int limit);
    bool            r6rs(bool flag) { bool prev = m_r6rs; m_r6rs = flag; return prev; }

private:
    void            write(scm_obj_t ht, scm_obj_t obj);
    void            write_shared(scm_obj_t obj);
    void            write(scm_obj_t obj);
    void            write_r6rs_symbol(const uint8_t* utf8, int n);
    void            write_pretty_symbol(const uint8_t* utf8, int n);
    bool            write_abbreviated(scm_obj_t obj);
    void            write_string(const uint8_t* utf8, int n);
    void            write_ucs4(uint32_t c);
    void            scan(scm_hashtable_t ht, scm_obj_t obj);
    bool            symbol_need_bar(const char* s, int n);
    const char*     get_tuple_type_name(scm_obj_t obj);

    VM*             m_vm;
    scm_port_t      m_port;
    int             m_column_limit;
    int             m_tuple_nest;
    int             m_tuple_nest_limit;
    int             m_shared_tag;
    int             m_radix;
    bool            m_escape;
    bool            m_unwrap;
    bool            m_flush;
    bool            m_r6rs;

    enum {
        escape_mode_string,
        escape_mode_symbol
    };

};

#endif
