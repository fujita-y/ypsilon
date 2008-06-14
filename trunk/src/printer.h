/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#ifndef	PRINTER_H_INCLUDED
#define	PRINTER_H_INCLUDED

#include "core.h"
#include "object.h"
#include "list.h"

class printer_t {
public:
					printer_t(VM* vm, scm_port_t port);
					~printer_t();
	void			flush();
	void			format(const char* fmt, ...);
	void			byte(uint8_t c);
	void			ucs4(uint32_t c);
	void			format_va_list(const char* fmt, va_list ap);
    void            column_limit(int limit);
    
private:
	void			scan(scm_hashtable_t ht, scm_obj_t obj);
	void			write(scm_obj_t ht, scm_obj_t obj);
	void			write_shared(scm_obj_t obj);
	void			write(scm_obj_t obj);
	void			write_escaped_string(const char* s, int n);
	void			write_escaped_char(unsigned int c);
	bool			write_abbreviated(scm_obj_t obj);
	void			write_ucs4(uint32_t c);
	bool			symbol_need_bar(const char* s);
	const char*		get_tuple_type_name(scm_obj_t obj);

	VM*				m_vm;
	scm_port_t		m_port;	
    int             m_column_limit;
	int				m_shared_tag;
	int				m_radix;
	int				m_escape_mode;
	bool			m_escape;
	bool			m_unwrap;
	bool			m_flush;
	
	enum {
		escape_mode_string,
		escape_mode_symbol
    };
		
};

#endif
