/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#include "core.h"
#include "vm.h"
#include "fasl.h"
#include "heap.h"
#include "port.h"
#include "ucs4.h"
#include "utf8.h"
#include "arith.h"
#include "reader.h"
#include "printer.h"

#define ENABLE_NOBACKTRACE_COMMENT      1
#define ENABLE_CORE_COMMENT             1
#define ENABLE_COMPATIBLE_COMMENT       1
#define ENABLE_R6RS_COMMENT             1

#define S_QUOTE             (m_vm->m_heap->inherent_symbol(S_CODE_QUOTE))
#define S_QUASIQUOTE        (m_vm->m_heap->inherent_symbol(S_CODE_QUASIQUOTE))
#define S_UNQUOTE           (m_vm->m_heap->inherent_symbol(S_CODE_UNQUOTE))
#define S_UNQUOTE_SPLICING  (m_vm->m_heap->inherent_symbol(S_CODE_UNQUOTE_SPLICING))
#define S_SYNTAX            (m_vm->m_heap->inherent_symbol(S_CODE_SYNTAX))
#define S_QUASISYNTAX       (m_vm->m_heap->inherent_symbol(S_CODE_QUASISYNTAX))
#define S_UNSYNTAX          (m_vm->m_heap->inherent_symbol(S_CODE_UNSYNTAX))
#define S_UNSYNTAX_SPLICING (m_vm->m_heap->inherent_symbol(S_CODE_UNSYNTAX_SPLICING))
#define S_LPAREN            (m_vm->m_heap->inherent_symbol(S_CODE_LPAREN))
#define S_RPAREN            (m_vm->m_heap->inherent_symbol(S_CODE_RPAREN))
#define S_LBRACK            (m_vm->m_heap->inherent_symbol(S_CODE_LBRACK))
#define S_RBRACK            (m_vm->m_heap->inherent_symbol(S_CODE_RBRACK))
#define S_DOT               (m_vm->m_heap->inherent_symbol(S_CODE_DOT))

bool reader_t::s_char_map_ready;
uint8_t reader_t::s_char_map[128];

#define CHAR_MAP_SYMBOL         0x01
#define CHAR_MAP_INITIAL        0x02
#define CHAR_MAP_DELIMITER      0x04

#define SYMBOL_CHARP(x)         ((s_char_map[x] & CHAR_MAP_SYMBOL) != 0)
#define INITIAL_CHARP(x)        ((s_char_map[x] & CHAR_MAP_INITIAL) != 0)
#define DELIMITER_CHARP(x)      ((s_char_map[x] & CHAR_MAP_DELIMITER) != 0)

static inline int
cnvt_hex_char_to_int(int c)
{
    if ((c >= '0') & (c <= '9')) return c - '0';
    else if ((c >= 'a') & (c <= 'f')) return c - 'a' + 10;
    else if ((c >= 'A') & (c <= 'F')) return c - 'A' + 10;
    return -1;
}

void
reader_t::make_char_map()
{
    if (s_char_map_ready) return;
    for (int i = 1; i < array_sizeof(s_char_map); i++) {
        s_char_map[i]  = ((isalnum(i) || strchr(".!?*+-/:<=>$%&@^_~", i)) ? CHAR_MAP_SYMBOL : 0);
        s_char_map[i] |= ((isalpha(i) || strchr("!?*/:<=>$%&^_~", i)) ? CHAR_MAP_INITIAL : 0);
        s_char_map[i] |= (strchr("()[]\";#", i) ? CHAR_MAP_DELIMITER : 0);
    }
    s_char_map_ready = true;
}

bool
reader_t::delimited(int c)
{
    if (ucs4_whitespace(c)) return true;
    if (c > 127) return false;
    return DELIMITER_CHARP(c);
}

reader_t::reader_t(VM* vm, scm_port_t input)
{
    m_file = port_regular_file_pred(input);
    m_graph = NULL;
    m_note = NULL;
    m_in = input;
    m_vm = vm;
    m_ungetbuf = scm_eof;
    m_ungetbuf_valid = false;
    m_graph_ref = false;
}

void
reader_t::lexical_error(const char* fmt, ...)
{
    scoped_lock lock(m_in->lock);
    while (port_nonblock_byte_ready(m_in)) {
        if (port_get_byte(m_in) == EOF) break;
        continue;
    }
    scm_port_t port = make_bytevector_port(m_vm->m_heap, make_symbol(m_vm->m_heap, "string"), SCM_PORT_DIRECTION_OUT, scm_false, scm_true);
    scoped_lock lock2(port->lock);
    printer_t prt(m_vm, port);
    va_list ap;
    va_start(ap, fmt);
    prt.format_va_list(fmt, ap);
    va_end(ap);
    prt.format("~%  ...~s ", m_in->name);
    if (m_parsing_line_from == m_parsing_line_to) {
        prt.format("line %d", m_parsing_line_from);
    } else {
        prt.format("line %d-%d", m_parsing_line_from, m_parsing_line_to);
    }
    scm_string_t message = make_string(m_vm->m_heap, (const char*)port->buf_head, port->buf_tail - port->buf_head);
    throw reader_exception_t(message);
}

int
reader_t::get_ucs4()
{
    if (m_ungetbuf_valid) {
        m_ungetbuf_valid = false;
        if (m_ungetbuf == scm_eof) return EOF;
        return CHAR(m_ungetbuf);
    } else {
        scm_obj_t ch = port_get_char(m_in);
        m_ungetbuf = ch;
        if (ch == scm_eof) return EOF;
        return CHAR(ch);
    }
}

void
reader_t::unget_ucs4()
{
    m_ungetbuf_valid = true;
}

int
reader_t::lookahead_ucs4()
{
    if (m_ungetbuf_valid) {
        if (m_ungetbuf == scm_eof) return EOF;
        return CHAR(m_ungetbuf);
    } else {
        scm_obj_t ch = port_lookahead_char(m_in);
        if (ch == scm_eof) return EOF;
        return CHAR(ch);
    }
}


scm_obj_t
reader_t::cons(scm_obj_t e1, scm_obj_t e2)
{
    return make_pair(m_vm->m_heap, e1, e2);
}

scm_obj_t
reader_t::list2(scm_obj_t e1, scm_obj_t e2)
{
    return cons(e1, cons(e2, scm_nil));
}

scm_obj_t
reader_t::reverse_list(scm_obj_t lst, scm_obj_t tail)
{
    scm_obj_t r = tail;
    while (PAIRP(lst)) {
        r = cons(CAR(lst), r);
        lst = CDR(lst);
    }
    return r;
}

scm_obj_t
reader_t::skip_line()
{
    int c;
    while ((c = get_ucs4()) != EOF) {
        if (c == '\n') return read_token();
        if (c == '\r') return read_token();
    }
    return scm_eof;
}

scm_obj_t
reader_t::skip_srfi30()
{
    int c1;
    int c2;
    int nest = 0;

seek_c1:
    c1 = get_ucs4();

seek_c2:
    c2 = get_ucs4();
    if (c2 == EOF) {
        lexical_error("unexpected end-of-file while reading comments");
    }
    if (c1 == '|' && c2 == '#') {
        if (nest == 0) return read_token();
        nest -= 1;
        goto seek_c1;
    }
    if (c1 == '#' && c2 == '|') {
        nest += 1;
        goto seek_c1;
    }
    c1 = c2;
    if (c1 == '|' || c1 == '#') goto seek_c2;
    goto seek_c1;
}

void
reader_t::read_thing(char* buf, size_t size)
{
    size_t i = 0;
    while (i + 4 < size) {
        int c = lookahead_ucs4();
        if (c == EOF) {
            buf[i] = 0;
            return;
        }
        if (delimited(c)) {
            buf[i] = 0;
            return;
        }
        get_ucs4();
        if (c < 128) buf[i++] = c;
        else i += cnvt_ucs4_to_utf8(ensure_ucs4(c), (uint8_t*)buf + i);
    }
    lexical_error("token buffer overflow while reading identifier, %s ...", buf);
}

int
reader_t::ensure_ucs4(int c)
{
    assert(c >= 0);
    if (c > 0x10ffff) lexical_error("code point out of range, U+%X", c);
    if (c >= 0xd800 && c <= 0xdfff) lexical_error("code point in excluded range, U+%X", c);
    return c;
}

int
reader_t::read_hex_scalar_value()
{
    int ucs4 = 0;
    int c = get_ucs4();
    if (c == EOF) lexical_error("unexpected end-of-file while reading hex scalar value");
    if (delimited(c)) lexical_error("expected hex digit, but got %U, while reading hex scalar value", c);
    unget_ucs4();
    while (true) {
        int c = get_ucs4();
        if (c == EOF || delimited(c)) {
            unget_ucs4();
            return ensure_ucs4(ucs4);
        }
        int n = cnvt_hex_char_to_int(c);
        if (n < 0) lexical_error("expected hex digit, but got %U, while reading hex scalar value", c);
        ucs4 = (ucs4 << 4) + n;
        if (ucs4 > 0x10ffff) lexical_error("hex scalar value out of range");
    }
}

scm_obj_t
reader_t::read_bytevector()
{
    #define CAST_REAL_TO_DOUBLE(DATUM, REF)     (*REF = real_to_double(DATUM), true)
    #define CAST_FIXNUM_TO_U8(DATUM, REF)       ((FIXNUM(datum) >= 0 && FIXNUM(datum) <= UINT8_MAX) ? (*REF = (FIXNUM(datum) & 0xFF), true) : false)
    #define CAST_FIXNUM_TO_S8(DATUM, REF)       ((FIXNUM(datum) >= INT8_MIN && FIXNUM(datum) <= INT8_MAX) ? (*REF = (FIXNUM(datum) & 0xFF), true) : false)
    #define READ_BVECTOR(S_TYPE, C_TYPE, S_TYPE_TEST, C_TYPE_CAST)              \
        do {                                                                    \
            if (strcmp(buf, S_TYPE) == 0) {                                     \
                int m = n * sizeof(C_TYPE);                                     \
                scm_bvector_t bvector = make_bvector(m_vm->m_heap, m);          \
                for (int i = 0; i < m; i += sizeof(C_TYPE)) {                   \
                    scm_obj_t datum = CAR(lst);                                 \
                    if ( S_TYPE_TEST (datum)) {                                 \
                        C_TYPE * ref = (C_TYPE *)&bvector->elts[i];             \
                        if ( C_TYPE_CAST (datum, ref)) {                        \
                            lst = CDR(lst);                                     \
                            continue;                                           \
                        }                                                       \
                    }                                                           \
                    lexical_error("expected " S_TYPE ", but got ~r", CAR(lst)); \
                }                                                               \
                return bvector;                                                 \
            }                                                                   \
        } while (0)

    char buf[16];
    read_thing(buf, sizeof(buf));
    int c = get_ucs4();
    if (c == '(') {
        int line_begin = m_in->line;
        scm_obj_t lst = read_list(false, false);
        parsing_range(line_begin, m_in->line);
        int n = list_length(lst);
        READ_BVECTOR("u8",  uint8_t,  FIXNUMP,            CAST_FIXNUM_TO_U8);
#if USE_EXTENDED_BVECTOR_SYNTAX
        if (m_vm->flags.m_extend_lexical_syntax == scm_true) {
            READ_BVECTOR("s8",  int8_t,   FIXNUMP,            CAST_FIXNUM_TO_S8);
            READ_BVECTOR("s16", int16_t,  FIXNUMP,            exact_integer_to_int16);
            READ_BVECTOR("s32", int32_t,  exact_integer_pred, exact_integer_to_int32);
            READ_BVECTOR("s64", int64_t,  exact_integer_pred, exact_integer_to_int64);
            READ_BVECTOR("u16", uint16_t, FIXNUMP,            exact_integer_to_uint16);
            READ_BVECTOR("u32", uint32_t, exact_integer_pred, exact_integer_to_uint32);
            READ_BVECTOR("u64", uint64_t, exact_integer_pred, exact_integer_to_uint64);
            READ_BVECTOR("f32", float,    real_pred,          CAST_REAL_TO_DOUBLE);
            READ_BVECTOR("f64", double,   real_pred,          CAST_REAL_TO_DOUBLE);
        }
#endif
        }
    lexical_error("invalid lexical syntax #v%s~a ...", buf, MAKECHAR(c));

    #undef CAST_REAL_TO_DOUBLE
    #undef CAST_FIXNUM_TO_U8
    #undef CAST_FIXNUM_TO_S8
    #undef READ_BVECTOR
}

scm_obj_t
reader_t::read_number()
{
    char buf[4096];
    read_thing(buf, sizeof(buf));
    scm_obj_t obj = parse_number(m_vm->m_heap, buf, 0, 0);
    if (obj != scm_false) return obj;
    if (buf[1] == 0 && buf[0] == '.') return S_DOT;
    if (m_vm->flags.m_extend_lexical_syntax != scm_true) {
        if (buf[1] == 0 && (buf[0] == '+' || buf[0] == '-')) return make_symbol(m_vm->m_heap, buf);
        if (strcmp(buf, "...") == 0) return make_symbol(m_vm->m_heap, buf);
        if (buf[0] == '-' && buf[1] == '>') {
            // todo: need check /x??;
            int i = 2;
            int c;
            while ((c = buf[i++]) != 0) {
                if (c > 127) continue;
                if (SYMBOL_CHARP(c)) continue;
                lexical_error("invalid lexical syntax %s", buf);
            }
            return make_symbol(m_vm->m_heap, buf);
        }
        lexical_error("invalid lexical syntax %s", buf);
    }
    return make_symbol(m_vm->m_heap, buf);
}

scm_obj_t
reader_t::read_prefixed_number(int exactness, int radix, bool swap)
{
    char buf[4096];
    read_thing(buf, sizeof(buf));
    scm_obj_t obj = parse_number(m_vm->m_heap, buf, exactness, radix);
    if (obj != scm_false) return obj;
    if (exactness) {
        if (radix > 32) {
            if (swap) lexical_error("invalid lexical syntax #%c#%c%s while reading number", radix, exactness, buf);
            lexical_error("invalid lexical syntax #%c#%c%s while reading number", exactness, radix, buf);
        }
        lexical_error("invalid lexical syntax #%c%s while reading number", exactness, buf);
    }
    lexical_error("invalid lexical syntax #%c%s while reading number", radix, buf);
}

int
reader_t::read_radix(int exactness)
{
    if (get_ucs4() != '#') {
        unget_ucs4();
        return 10;
    }
    int c = get_ucs4();
    switch (c) {
    case 'b': case 'B': case 'o': case 'O': case 'd': case 'D': case 'x': case 'X':
        return c;
    case EOF:
        lexical_error("unexpected end-of-file while reading number");
    default:
        lexical_error("invalid lexical syntax #%c#%c while reading number", exactness, c);
    }
}

int
reader_t::read_exactness(int radix)
{
    if (get_ucs4() != '#') {
        unget_ucs4();
        return 0;
    }
    int c = get_ucs4();
    switch (c) {
    case 'e': case 'E': case 'i': case 'I': return c;
    case EOF:
        lexical_error("unexpected end-of-file while reading number");
    default:
        lexical_error("invalid lexical syntax #%c#%c while reading number", radix, c);
    }
}

scm_obj_t
reader_t::read_char()
{
    static const struct {
        const char* name;
        int code;
    } char_name[] = {
        { "nul",        0x0000 },
        { "alarm",      0x0007 },
        { "backspace",  0x0008 },
        { "tab",        0x0009 },
        { "linefeed",   0x000A },
        { "newline",    0x000A },
        { "vtab",       0x000B },
        { "page",       0x000C },
        { "return",     0x000D },
        { "esc",        0x001B },
        { "space",      0x0020 },
        { "delete",     0x007F }
    };
    int c = get_ucs4();
    if (c == 'x') {
        c = lookahead_ucs4();
        if (c == EOF) return MAKECHAR('x');
        if (delimited(c)) return MAKECHAR('x');
        return MAKECHAR(read_hex_scalar_value());
    }
    char buf[32];
    if (c == '(') {
        c = lookahead_ucs4();
        if (c == EOF) return MAKECHAR('(');
        if (delimited(c)) return MAKECHAR('(');
        read_thing(buf, sizeof(buf));
        lexical_error("invalid lexical syntax #\\(%s", buf);
    }
    unget_ucs4();
    read_thing(buf, sizeof(buf));
    if (buf[0] == 0) {
        c = get_ucs4();
        if (c == EOF) lexical_error("unexpected end-of-file while reading character");
        return MAKECHAR(c);
    }
    if (buf[1] == 0) return MAKECHAR(buf[0]);
    for (int i = 0; i < array_sizeof(char_name); i++) {
        if (strcmp(buf, char_name[i].name) == 0) return MAKECHAR(char_name[i].code);
    }
    uint32_t ucs4;
    int n = cnvt_utf8_to_ucs4((uint8_t*)buf, &ucs4);
    if (n > 0 && buf[n] == 0) return MAKECHAR(ucs4);
    lexical_error("invalid lexical syntax #\\%s", buf);
}

int
reader_t::read_escape_sequence()
{
    int c = get_ucs4();
    switch (c) {
        case 'x':
            c = get_ucs4();
            if (c == EOF) lexical_error("unexpected end-of-file while reading escape sequence");
            unget_ucs4();
            c = read_hex_scalar_value();
            if (get_ucs4() != ';') lexical_error("inline hex escape missing terminating semi-colon");
            return c;
        case 'a':  return 0x0007;
        case 'b':  return 0x0008;
        case 't':  return 0x0009;
        case 'n':  return 0x000A;
        case 'v':  return 0x000B;
        case 'f':  return 0x000C;
        case 'r':  return 0x000D;
        case '"':  return 0x0022;
        case '\\': return 0x005C;
        case EOF: lexical_error("unexpected end-of-file while reading escape sequence");
    }
    lexical_error("invalid escape sequence, \\~a", MAKECHAR(c));
}

scm_obj_t
reader_t::read_string()
{
    char buf[MAX_READ_STRING_LENGTH];
    int i = 0;
    while (i + 4 < array_sizeof(buf)) {
        int c = get_ucs4();
        if (c == EOF) lexical_error("unexpected end-of-file while reading string");
        switch (c) {
        case SCM_PORT_UCS4_CR:
            c = get_ucs4();
            if (c != SCM_PORT_UCS4_LF && c != SCM_PORT_UCS4_NEL) unget_ucs4();
        case SCM_PORT_UCS4_LF: case SCM_PORT_UCS4_NEL: case SCM_PORT_UCS4_LS:
            buf[i++] = SCM_PORT_UCS4_LF;
            continue;
        }
        if (c == '"') {
            buf[i] = 0;
            return make_string_literal(m_vm->m_heap, buf, i);
        }
        if (c == '\\') {
            c = get_ucs4();
            if (ucs4_intraline_whitespace(c)) {
                do {
                    c = get_ucs4();
                    if (c == EOF) lexical_error("unexpected end-of-file while reading intraline whitespeace");
                } while (ucs4_intraline_whitespace(c));
                switch (c) {
                case SCM_PORT_UCS4_CR:
                    c = get_ucs4();
                    if (c != SCM_PORT_UCS4_LF && c != SCM_PORT_UCS4_NEL) unget_ucs4();
                case SCM_PORT_UCS4_LF: case SCM_PORT_UCS4_NEL: case SCM_PORT_UCS4_LS:
                    break;
                default:
                    lexical_error("unexpected charactor %U while reading intraline whitespeace", c);
                }
                do { c = get_ucs4(); } while (ucs4_intraline_whitespace(c));
                unget_ucs4();
                continue;
            }
            switch (c) {
            case SCM_PORT_UCS4_CR:
                c = get_ucs4();
                if (c != SCM_PORT_UCS4_LF && c != SCM_PORT_UCS4_NEL) unget_ucs4();
            case SCM_PORT_UCS4_LF: case SCM_PORT_UCS4_NEL: case SCM_PORT_UCS4_LS:
                do { c = get_ucs4(); } while (ucs4_intraline_whitespace(c));
                unget_ucs4();
                continue;
            }
            unget_ucs4();
            c = read_escape_sequence();
            i += cnvt_ucs4_to_utf8(ensure_ucs4(c), (uint8_t*)buf + i);
            continue;
        }
        if (c < 128) buf[i++] = c;
        else i += cnvt_ucs4_to_utf8(ensure_ucs4(c), (uint8_t*)buf + i);
    }
    lexical_error("token buffer overflow while reading string");
}

scm_obj_t
reader_t::read_quoted_symbol()
{
    if (m_vm->flags.m_extend_lexical_syntax != scm_true) {
        lexical_error("invalid lexical syntax, misplaced vertical bar(|)");
    }
    char buf[MAX_READ_SYMBOL_LENGTH];
    int i = 0;
    while (i + 4 < array_sizeof(buf)) {
        int c = get_ucs4();
        if (c == EOF) {
            lexical_error("unexpected end-of-file while reading quoted symbol");
        }
        if (c == '|') {
            buf[i] = 0;
            return make_symbol(m_vm->m_heap, buf, i);
        }
        if (c == '\\') c = get_ucs4();
        if (c < 128) buf[i++] = c;
        else i += cnvt_ucs4_to_utf8(ensure_ucs4(c), (uint8_t*)buf + i);
    }
    lexical_error("token buffer overflow while reading quoted symbol");
}

scm_obj_t
reader_t::read_symbol()
{
    char buf[MAX_READ_SYMBOL_LENGTH];
    int i = 0;
    while (i + 4 < array_sizeof(buf)) {
        int c = lookahead_ucs4();
        if (c == EOF) {
            buf[i] = 0;
            return make_symbol(m_vm->m_heap, buf, i);
        }
        if (delimited(c)) {
            buf[i] = 0;
            return make_symbol(m_vm->m_heap, buf, i);
        }
        get_ucs4();
        if (c == '\\') {
            c = get_ucs4();
            if (c == 'x') {
                unget_ucs4();
                c = read_escape_sequence();
                i += cnvt_ucs4_to_utf8(ensure_ucs4(c), (uint8_t*)buf + i);
                continue;
            } else {
                lexical_error("invalid character '\\' while reading identifier");
            }
        }
        if (c > 127) {
            ensure_ucs4(c);
            if (i == 0) {
                if (ucs4_constituent(c)) {
                    i += cnvt_ucs4_to_utf8(c, (uint8_t*)buf + i);
                    continue;
                }
            } else {
                if (ucs4_subsequent(c)) {
                    i += cnvt_ucs4_to_utf8(c, (uint8_t*)buf + i);
                    continue;
                }
            }
            lexical_error("invalid character %U while reading identifier", c);
        }
        if (m_vm->flags.m_extend_lexical_syntax == scm_true) {
            if (SYMBOL_CHARP(c)) {
                buf[i++] = c;
                continue;
            }
        } else {
            if (i == 0) {
                if (INITIAL_CHARP(c)) {
                    buf[i++] = c;
                    continue;
                }
            } else {
                if (SYMBOL_CHARP(c)) {
                    buf[i++] = c;
                    continue;
                }
            }
        }
        lexical_error("invalid character %U while reading identifier", c);
    }
    lexical_error("token buffer overflow while reading identifier");
}

static scm_obj_t
encode_source_comment(int line, int column, bool file)
{
    int comment = line * MAX_SOURCE_COLUMN + column;
    if (!file) comment = -comment;
    return MAKEFIXNUM(comment);
}

scm_obj_t
reader_t::read_list(bool bracketed, bool vector)
{
    scm_obj_t lst = scm_nil;
    scm_obj_t token;
    int line_begin = m_in->line;
    int column_begin = m_in->column - 1;
    if (column_begin < 1) column_begin = 1;
    while ((token = read_token()) != scm_eof) {
        if (token == S_RPAREN) {
            if (bracketed) {
                parsing_range(line_begin, m_in->line);
                lexical_error("bracketed list terminated by parenthesis");
            }
            lst = reverse_list(lst, scm_nil);
            if (m_note) put_note(lst, encode_source_comment(line_begin, column_begin, m_file));
            return lst;
        }
        if (token == S_RBRACK) {
            if (!bracketed) {
                parsing_range(line_begin, m_in->line);
                lexical_error("parenthesized list terminated by bracket");
            }
            lst = reverse_list(lst, scm_nil);
            if (m_note) put_note(lst, encode_source_comment(line_begin, column_begin, m_file));
            return lst;
        }
        if (token == S_LPAREN) {
            lst = cons(read_list(false, false), lst);
            continue;
        }
        if (token == S_LBRACK) {
            lst = cons(read_list(true, false), lst);
            continue;
        }
        if (token == S_DOT) {
            if (vector) {
                if (m_vm->flags.m_extend_lexical_syntax != scm_true) {
                    lexical_error("misplaced dot('.') while reading vector");
                }
            }
            if (lst == scm_nil) {
                parsing_range(line_begin, m_in->line);
                lexical_error("misplaced dot('.') while reading list");
            }
            scm_obj_t rest = read_expr();
            if (rest == S_DOT) lexical_error("misplaced dot('.') while reading list");
            token = read_token();
            if (token == S_RPAREN) {
                if (bracketed) {
                    parsing_range(line_begin, m_in->line);
                    lexical_error("bracketed list terminated by parenthesis");
                }
                lst = reverse_list(lst, rest);
                if (m_note) put_note(lst, encode_source_comment(line_begin, column_begin, m_file));
                return lst;
            }
            if (token == S_RBRACK) {
                if (!bracketed) {
                    parsing_range(line_begin, m_in->line);
                    lexical_error("parenthesized list terminated by bracket");
                }
                lst = reverse_list(lst, rest);
                if (m_note) put_note(lst, encode_source_comment(line_begin, column_begin, m_file));
                return lst;
            }
            parsing_range(line_begin, m_in->line);
            if (token == scm_eof) lexical_error("unexpected end-of-file while reading list");
            lexical_error("more than one item following dot('.') while reading list");
        }
        if (PAIRP(token)) {
            if (m_note) put_note(token, encode_source_comment(line_begin, column_begin, m_file));
        }
        lst = cons(token, lst);
    }
    parsing_range(line_begin, m_in->line);
    lexical_error("unexpected end-of-file while reading list");
}

scm_obj_t
reader_t::read_token()
{
    int c;

top:
    c = get_ucs4();
    if (c == EOF) return scm_eof;
    if (ucs4_whitespace(c)) goto top;
    parsing_line(m_in->line);
    if (c < 128 && isdigit(c)) {
        unget_ucs4();
        return read_number();
    }
    switch (c) {
        case ';':   return skip_line();
        case '"':   return read_string();
        case '|':   return read_quoted_symbol();
        case '(':   return S_LPAREN;
        case ')':   return S_RPAREN;
        case '[':   return S_LBRACK;
        case ']':   return S_RBRACK;
        case '\'': {
            scm_obj_t obj = read_expr();
            if (obj == scm_eof) lexical_error("unexpected end-of-file following quotation-mark(')");
            return list2(S_QUOTE, obj);
        }
        case '`':  {
            scm_obj_t obj = read_expr();
            if (obj == scm_eof) lexical_error("unexpected end-of-file following grave-accent(`)");
            return list2(S_QUASIQUOTE, obj);
        }
        case '+':
        case '.':
            unget_ucs4();
            return read_number();
        case '-':
            unget_ucs4();
            return read_number();
        case '#':
            c = get_ucs4();
            switch (c) {
                case EOF:
                    lexical_error("unexpected end-of-file following sharp-sign(#)");
                case '!': {
                    scm_obj_t desc = read_symbol();
                    if (SYMBOLP(desc)) {
                        const char* tag = ((scm_symbol_t)desc)->name;
                        if (strcmp(tag, "fasl0") == 0) {
                            get_ucs4(); // read delimiter
                            return fasl_reader_t(m_vm, m_in).get();
                        }
#if ENABLE_NOBACKTRACE_COMMENT
                        if (strcmp(tag, "nobacktrace") == 0) {
                            m_vm->flags.m_backtrace = scm_false;
                        }
#endif
#if ENABLE_CORE_COMMENT
                        if (strcmp(tag, "core") == 0) {
                            m_vm->flags.m_extend_lexical_syntax = scm_true;
                        }
#endif
#if ENABLE_COMPATIBLE_COMMENT
                        if (strcmp(tag, "compatible") == 0) {
                            m_vm->flags.m_extend_lexical_syntax = scm_true;
                            if (m_graph == NULL) {
                                m_graph = make_hashtable(m_vm->m_heap, SCM_HASHTABLE_TYPE_EQ, lookup_mutable_hashtable_size(0));
                            }
                        }
#endif
#if ENABLE_R6RS_COMMENT
                        if (strcmp(tag, "r6rs") == 0) {
                            m_vm->flags.m_extend_lexical_syntax = scm_false;
                        }
#endif
                    }
                    goto top;
                }
                case 'v':
                    return read_bytevector();
                case 'f': case 'F': {
                    int c2 = get_ucs4();
                    if (c2 == EOF || delimited(c2)) {
                        unget_ucs4();
                        return scm_false;
                    }
                    lexical_error("invalid lexical syntax #~a~a", MAKECHAR(c), MAKECHAR(c2));
                }
                case 't': case 'T':{
                    int c2 = get_ucs4();
                    if (c2 == EOF || delimited(c2)) {
                        unget_ucs4();
                        return scm_true;
                    }
                    lexical_error("invalid lexical syntax #~a~a", MAKECHAR(c), MAKECHAR(c2));
                }
                case '(': return make_vector(m_vm->m_heap, read_list(false, true));
                case '|': return skip_srfi30();
                case '\\': return read_char();
                case ';': read_expr(); goto top;
                case 'b': case 'B': return read_prefixed_number(read_exactness(c), c, true);
                case 'o': case 'O': return read_prefixed_number(read_exactness(c), c, true);
                case 'd': case 'D': return read_prefixed_number(read_exactness(c), c, true);
                case 'x': case 'X': return read_prefixed_number(read_exactness(c), c, true);
                case 'i': case 'I': return read_prefixed_number(c, read_radix(c), false);
                case 'e': case 'E': return read_prefixed_number(c, read_radix(c), false);
                case '\'':
                    return list2(S_SYNTAX, read_expr());
                case '`':
                    return list2(S_QUASISYNTAX, read_expr());
                case ',':
                    c = get_ucs4();
                    if (c == EOF) lexical_error("unexpected end-of-file following sharp comma(#,)");
                    if (c == '@') return list2(S_UNSYNTAX_SPLICING, read_expr());
                    unget_ucs4();
                    return list2(S_UNSYNTAX, read_expr());
                default:
                    if (m_graph == NULL) break;
                    if (c >= '0' && c <= '9') {
                        intptr_t mark = c - '0';
                        while (true) {
                            int c2 = get_ucs4();
                            if (c2 >= '0' && c2 <= '9') {
                                mark = mark * 10 + c2;
                                if (mark < 0 || mark > FIXNUM_MAX) lexical_error("invalid object tag, value out of range");
                                continue;
                            }
                            if (c2 == EOF) lexical_error("unexpected end-of-file while reading tag #%ld", mark);
                            if (c2 == '=') {
                                scm_obj_t obj = read_expr();
                                if (obj == scm_eof) lexical_error("unexpected end-of-file while reading tag #%ld=", mark);
                                if (get_hashtable(m_graph, MAKEFIXNUM(mark)) == scm_undef) {
                                    int nsize = put_hashtable(m_graph, MAKEFIXNUM(mark), obj);
                                    if (nsize) rehash_hashtable(m_vm->m_heap, m_graph, nsize);
                                    return obj;
                                }
                                lexical_error("duplicate tag #%ld=", mark);
                            }
                            if (c2 == '#') {
                                scm_tuple_t tuple = make_tuple(m_vm->m_heap, 1);
                                tuple->elts[0] = MAKEFIXNUM(mark);
                                m_graph_ref = true;
                                return tuple;
                            }
                            break;
                        }
                    }
                    break;
            }
            lexical_error("invalid lexical syntax #~a", MAKECHAR(c));
        case ',':
            c = get_ucs4();
            if (c == EOF) lexical_error("unexpected end-of-file following comma(,)");
            if (c == '@') return list2(S_UNQUOTE_SPLICING, read_expr());
            unget_ucs4();
            return list2(S_UNQUOTE, read_expr());
        default:
            unget_ucs4();
            return read_symbol();
    }
}

scm_obj_t
reader_t::read_expr()
{
    scm_obj_t token = read_token();
    if (token == S_RPAREN) lexical_error("unexpected closing parenthesis");
    if (token == S_RBRACK) lexical_error("unexpected closing bracket");
    if (token == S_LPAREN) return read_list(false, false);
    if (token == S_LBRACK) return read_list(true, false);
    return token;
}

void
reader_t::put_note(scm_obj_t key, scm_obj_t value)
{
    assert(m_note);
    assert(HASHTABLEP(m_note));
    scm_hashtable_t ht = (scm_hashtable_t)m_note;
    scoped_lock lock(ht->lock);
    m_vm->m_heap->write_barrier(key);
    m_vm->m_heap->write_barrier(value);
    int nsize = put_hashtable(ht, key, value);
    if (nsize) rehash_hashtable(m_vm->m_heap, ht, nsize);
}

void
reader_t::put_note(const char* symbol_name, scm_obj_t value)
{
    put_note(make_symbol(m_vm->m_heap, symbol_name), value);
}

void
reader_t::parsing_range(int from, int to)
{
    m_parsing_line_from = from;
    m_parsing_line_to = to;
}

void
reader_t::parsing_line(int line)
{
    parsing_range(line, line);
}

scm_obj_t
reader_t::lookup_graph(scm_tuple_t tuple)
{
    scm_obj_t obj;
    obj = get_hashtable(m_graph, tuple->elts[0]);
    if (TUPLEP(obj)) return lookup_graph((scm_tuple_t)obj);
    if (obj != scm_undef) return obj;
    lexical_error("attempt to reference undefined tag #%d#", FIXNUM(tuple->elts[0]));
}

void
reader_t::link_graph(scm_obj_t obj)
{
    if (PAIRP(obj)) {
        if (TUPLEP(CAR(obj))) {
            CAR(obj) = lookup_graph((scm_tuple_t)CAR(obj));
        } else {
            link_graph(CAR(obj));
        }
        if (TUPLEP(CDR(obj))) {
            CDR(obj) = lookup_graph((scm_tuple_t)CDR(obj));
        } else {
            link_graph(CDR(obj));
        }
        return;
    }
    if (VECTORP(obj)) {
        scm_vector_t vect = (scm_vector_t)obj;
        int n = vect->count;
        for (int i = 0; i < n; i++) {
            if (TUPLEP(vect->elts[i])) {
                vect->elts[i] = lookup_graph((scm_tuple_t)vect->elts[i]);
            } else {
                link_graph(vect->elts[i]);
            }
        }
        return;
    }
}

scm_obj_t
reader_t::read_graph(scm_hashtable_t note)
{
    m_graph = make_hashtable(m_vm->m_heap, SCM_HASHTABLE_TYPE_EQ, lookup_mutable_hashtable_size(0));
    return read(note);
}

scm_obj_t
reader_t::read(scm_hashtable_t note)
{
    make_char_map();
    m_note = note;
    if (m_note) put_note(".&SOURCE-PATH", m_in->name);
    m_first_line = m_in->line;
    if (m_vm->flags.m_extend_lexical_syntax == scm_true) {
        if (m_graph == NULL) {
            m_graph = make_hashtable(m_vm->m_heap, SCM_HASHTABLE_TYPE_EQ, lookup_mutable_hashtable_size(0));
        }
    }
    scm_obj_t obj = read_expr();
    if (obj == S_DOT) lexical_error("misplaced dot('.')");
    if (m_graph && m_graph_ref) link_graph(obj);
    parsing_range(m_first_line, m_in->line);
    return obj;
}
