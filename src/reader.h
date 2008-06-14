/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#ifndef	READER_H_INCLUDED
#define	READER_H_INCLUDED
 
#include "core.h"
#include "object.h"

class reader_t {
public:
					reader_t(VM* vm, scm_port_t input);

	scm_obj_t		read(scm_hashtable_t note);
private:

	void			lexical_error(const char* fmt, ...) ATTRIBUTE(noreturn);
	
	void			unget_ucs4();
	int				get_ucs4();
    int             lookahead_ucs4();
    void            skip_intraline_whitespace();
	int             ensure_ucs4(int ucs4);
    scm_obj_t       read_bytevector();
	void			read_thing(char *buff,size_t size);
    int             read_hex_scalar_value();
	int				read_escape_sequence();
	scm_obj_t		read_prefixed_number(int prefix, int radix);
	scm_obj_t		read_number();
	scm_obj_t		read_token();
	scm_obj_t		read_string();
	scm_obj_t		read_list(bool bracketed);
	scm_obj_t		read_quoted_symbol();
	scm_obj_t		read_symbol();
	scm_obj_t		read_char();
	scm_obj_t		read_expr();
	scm_obj_t		cons(scm_obj_t e1, scm_obj_t e2);
	scm_obj_t		list2(scm_obj_t e1, scm_obj_t e2);
	scm_obj_t		reverse_list(scm_obj_t lst, scm_obj_t tail);
	scm_obj_t		skip_line();
	scm_obj_t		skip_srfi30();
    bool            delimited(int c);
	void			put_note(scm_obj_t key, scm_obj_t value);
	void			put_note(const char* symbol_name, scm_obj_t value);
	void			parsing_range(int from, int to);
	void			parsing_line(int line);
		
	scm_hashtable_t	m_note;
	int				m_first_line;
	int				m_parsing_line_from;
	int				m_parsing_line_to;
	VM*				m_vm;
	scm_port_t		m_in;
    scm_obj_t       m_ungetbuf;
    bool            m_ungetbuf_valid;
    bool            m_file;
    
    static bool     s_char_map_ready;
    static uint8_t  s_char_map[128];
    static void     make_char_map();
};

#endif
