/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#include "core.h"
#include "vm.h"
#include "hash.h"
#include "heap.h"
#include "port.h"
#include "socket.h"
#include "utf8.h"
#include "ucs4.h"
#include "arith.h"
#include "printer.h"

static const char  write_string_escape_codes[] = { 7, 8, 9, 10, 11, 12, 13, 92, 0};
static const char* write_string_escape_names = "abtnvfr\\";

class r6rs_param_t {
    printer_t *m_printer;
    bool m_save;
public:
    r6rs_param_t(printer_t* printer, bool mode) { m_printer = printer; m_save = printer->r6rs(mode); }
    ~r6rs_param_t() { m_printer->r6rs(m_save); }
};
    
printer_t::printer_t(VM* vm, scm_port_t port)
{
    m_vm = vm;
    m_port = port;
    m_shared_tag = 1;
    m_column_limit = 0; // no limit
    m_tuple_nest = 0;
    m_tuple_nest_limit = FIXNUM(vm->flags.m_record_print_nesting_limit);    
    m_flush = false;
    m_r6rs = true;
}

printer_t::~printer_t()
{
    if (m_flush) port_flush_output(m_port);
}

void
printer_t::flush()
{
    port_flush_output(m_port);
}

void
printer_t::column_limit(int limit)
{
    m_column_limit = limit;
}

bool
printer_t::symbol_need_bar(const char* s, int n)
{
    switch (s[0]) {
    case '@':
        return true;
    case '+':
        if (s[1] == 0) return false;
        if (s[1] == '`' && m_unwrap) return false;
        return true;
    case '-':
        if (s[1] == 0) return false;
        if (s[1] == '`' && m_unwrap) return false;
        if (s[1] != '>') return true;
        break;
    case '.':
        if (s[1] != '.') return true;
        if (s[2] != '.') return true;
        if (s[3] == 0) return false;
        if (s[3] == '`' && m_unwrap) return false;
        return true;
    }
    if (isdigit(s[0])) return true;
    char c;
    while ((c = *s++) != 0 && n--) {
        if (c < 32) continue;
        if (c == 127) continue;
        if (c & 0x80) continue;
        if (isalnum(c)) continue;
        if (strchr("!$%&/:*<=>?^_~+-.@", c)) continue;
//      if (c == '`' && m_unwrap) continue;
        return true;
    }
    return false;
}

void
printer_t::write_string(const uint8_t* utf8, int n)
{
    uint32_t ucs4;
    int i = 0;
    while (i < n) {
        if (utf8[i] < 128) {
            int c = utf8[i];
            if (c == '"') {
                port_put_byte(m_port, '\\');
                port_put_byte(m_port, c);
            } else {
                const char* p = strchr(write_string_escape_codes, c);
                if (p == NULL || p[0] == 0) {
                    if (c < 32 || c == 127) {
                        char buf[32];
                        snprintf(buf, sizeof(buf), "\\x%X;", c);
                        port_puts(m_port, buf);
                    } else {
                        port_put_byte(m_port, c);
                    }
                } else {
                    port_put_byte(m_port, '\\');
                    port_put_byte(m_port, write_string_escape_names[p - write_string_escape_codes]);
                }
            }
            i = i + 1;
            continue;
        } else {
            int bytes = cnvt_utf8_to_ucs4(utf8 + i, &ucs4);
            if (bytes < 0) fatal("%s:%u invalid utf8 encodeing in string", __FILE__, __LINE__);
            if (ucs4_subsequent(ucs4)) {
                write_ucs4(ucs4);
            } else {
                char buf[16];
                snprintf(buf, sizeof(buf), "\\x%X;", ucs4);
                port_puts(m_port, buf);
            }
            i = i + bytes;
        }
    }
}

void
printer_t::write_pretty_symbol(const uint8_t* utf8, int n)
{
    bool quote = symbol_need_bar((const char*)utf8, n);
    if (quote) port_put_byte(m_port, '|');

    uint32_t ucs4;
    int i = 0;
    while (i < n) {
        if (utf8[i] < 128) {
            int c = utf8[i];    
            if (c == '|') {
                port_put_byte(m_port, '\\');
                port_put_byte(m_port, c);
            } else if (c < 32 || c == 127) {
                char buf[16];
                snprintf(buf, sizeof(buf), "\\x%X;", c);
                port_puts(m_port, buf);
            } else {
                port_put_byte(m_port, c);
            }
            i = i + 1;
        } else {
            int bytes = cnvt_utf8_to_ucs4(utf8 + i, &ucs4);
            if (bytes < 0) fatal("%s:%u invalid utf8 encodeing in symbol", __FILE__, __LINE__);
            if (ucs4_subsequent(ucs4)) {
                write_ucs4(ucs4);
            } else {
                char buf[16];
                snprintf(buf, sizeof(buf), "\\x%X;", ucs4);
                port_puts(m_port, buf);
            }
            i = i + bytes;
        }
    }

    if (quote) port_put_byte(m_port, '|');
}

void
printer_t::write_r6rs_symbol(const uint8_t* utf8, int n)
{
    uint32_t cp;
    int i = 0;
    while (i < n) {
        int bytes = cnvt_utf8_to_ucs4(utf8 + i, &cp);
        if (bytes < 0) fatal("%s:%u invalid utf8 encodeing in symbol", __FILE__, __LINE__);
        if ((i == 0 && ucs4_constituent(cp)) || (i > 0 && ucs4_subsequent(cp))) {
            write_ucs4(cp);
        } else {
            // ... 
            if (i == 0) {
                if (n == 1) {
                    if (utf8[0] == '+' || utf8[0] == '-') {
                        port_put_byte(m_port, utf8[0]);
                        return;
                    }
                }
                if (n == 3) {
                    if (utf8[0] == '.' && utf8[1] == '.' && utf8[2] == '.') {
                        port_puts(m_port, "...");
                        return;
                    }
                }
                if (n > 2) {
                    if (utf8[0] == '-' && utf8[1] == '>') {
                        port_puts(m_port, "->");
                        i = i + 2;
                        continue;
                    }
                }
            }
            char buf[16];
            snprintf(buf, sizeof(buf), "\\x%X;", cp);
            port_puts(m_port, buf);
        }
        i = i + bytes;
    }
}

void
printer_t::byte(uint8_t c)
{
    port_put_byte(m_port, c);
}

void
printer_t::ucs4(uint32_t c)
{
    write_ucs4(c);
}

void
printer_t::format(const char* fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    format_va_list(fmt, ap);
    va_end(ap);
}

void
printer_t::format_va_list(const char* fmt, va_list ap)
{
    char buf[32];
    char c;
    while ((c = *fmt++) != 0) {
        switch (c) {
        case '~':
            c = *fmt++;
            switch (tolower(c)) {

            case 'r': // restricted
                {
                    m_escape = true;
                    m_unwrap = false;
                    m_radix = 10;
                    int save_limit = m_column_limit;
                    m_column_limit = m_port->column + FIXNUM(m_vm->flags.m_restricted_print_line_length);
                    scm_obj_t expr = va_arg(ap, scm_obj_t);
                    write(expr);
                    m_column_limit = save_limit;
                }
                break;
            case 'm': // macro form
                {
                    int save_limit = m_column_limit;
                    m_column_limit = m_column_limit ? m_column_limit : FIXNUM(m_vm->flags.m_backtrace_line_length);
                    m_escape = true;
                    m_unwrap = true;
                    m_radix = 10;
                    scm_obj_t expr = va_arg(ap, scm_obj_t);
                    write(expr);
                    if (PAIRP(expr) && m_vm->m_current_source_comments != scm_false) {
                        assert(HASHTABLEP(m_vm->m_current_source_comments));
                        scm_hashtable_t ht = (scm_hashtable_t)m_vm->m_current_source_comments;
                        scoped_lock lock(ht->lock);

                        scm_obj_t obj = get_hashtable(ht, expr);
                        if (PAIRP(obj)) {
                            port_puts(m_port, "\n  ...");
                            write(CAR(obj));
                            snprintf(buf, sizeof(buf), " line %d", abs(FIXNUM(CDR(obj))) / MAX_SOURCE_COLUMN);
                            port_puts(m_port, buf);
                        } else {

                            scm_obj_t path = get_hashtable(ht, make_symbol(m_vm->m_heap, ".&SOURCE-PATH"));
                            if (path != scm_undef) {
                                port_puts(m_port, "\n  ...");
                                write(path);
                                scm_obj_t line = get_hashtable(ht, expr);
                                if (line != scm_undef) {
                                    snprintf(buf, sizeof(buf), " line %d", abs(FIXNUM(line)) / MAX_SOURCE_COLUMN);
                                    port_puts(m_port, buf);
                                }
                            }

                        }
                    }
                    m_column_limit = save_limit;
                }
                break;

            case 'n': // line and path comment
                {
                    m_escape = true;
                    m_unwrap = true;
                    m_radix = 10;
                    scm_obj_t expr = va_arg(ap, scm_obj_t);
                    if (PAIRP(expr) && m_vm->m_current_source_comments != scm_false) {
                        assert(HASHTABLEP(m_vm->m_current_source_comments));
                        scm_hashtable_t ht = (scm_hashtable_t)m_vm->m_current_source_comments;
                        scoped_lock lock(ht->lock);

                        scm_obj_t obj = get_hashtable(ht, expr);
                        if (PAIRP(obj)) {
                            port_puts(m_port, "...");
                            write(CAR(obj));
                            snprintf(buf, sizeof(buf), " line %d", abs(FIXNUM(CDR(obj))) / MAX_SOURCE_COLUMN);
                            port_puts(m_port, buf);

                        } else {

                            scm_obj_t path = get_hashtable(ht, make_symbol(m_vm->m_heap, ".&SOURCE-PATH"));
                            if (path != scm_undef) {
                                port_puts(m_port, "...");
                                write(path);
                                scm_obj_t line = get_hashtable(ht, expr);
                                if (line != scm_undef) {
                                    snprintf(buf, sizeof(buf), " line %d", abs(FIXNUM(line)) / MAX_SOURCE_COLUMN);
                                    port_puts(m_port, buf);
                                }
                            }

                        }
                    }
                }
                break;
            case 'a':
                m_escape = false;
                m_unwrap = false;
                m_radix = 10;
                write(va_arg(ap, scm_obj_t));
                break;
            case 's':
                m_escape = true;
                m_unwrap = false;
                m_radix = 10;
                write(va_arg(ap, scm_obj_t));
                break;
            case '/': {
                scm_obj_t obj = va_arg(ap, scm_obj_t);
                if (STRINGP(obj)) {
                    scm_string_t string = (scm_string_t)obj;
                    char* path = (char*)alloca(strlen(string->name) + 1);
                    strcpy(path, string->name);
                    int i = 0;
                    while (path[i]) {
                        if (path[i] == '\\') path[i] = '/';
                        i++;
                    }
                    port_puts(m_port, path);
                } else {
                    m_escape = false;
                    m_unwrap = false;
                    m_radix = 10;
                    write(obj);
                }
            } break;
            case '\\': {
                scm_obj_t obj = va_arg(ap, scm_obj_t);
                if (STRINGP(obj)) {
                    scm_string_t string = (scm_string_t)obj;
                    char* path = (char*)alloca(strlen(string->name) + 1);
                    strcpy(path, string->name);
                    int i = 0;
                    while (path[i]) {
                        if (path[i] == '/') path[i] = '\\';
                        i++;
                    }
                    port_puts(m_port, path);
                } else {
                    m_escape = false;
                    m_unwrap = false;
                    m_radix = 10;
                    write(obj);
                }
            } break;
            case 'w':
                m_escape = true;
                m_unwrap = false;
                m_radix = 10;
                write_shared(va_arg(ap, scm_obj_t));
                break;
            case 'u':
                m_escape = true;
                m_unwrap = true;
                m_radix = 10;
                write(va_arg(ap, scm_obj_t));
                break;
            case 'c':
                m_escape = false;
                m_unwrap = false;
                m_radix = 10;
                write(va_arg(ap, scm_obj_t));
                break;
            case 'd':
                m_escape = false;
                m_unwrap = false;
                m_radix = 10;
                write(va_arg(ap, scm_obj_t));
                break;
            case 'x':
                m_escape = false;
                m_unwrap = false;
                m_radix = 16;
                write(va_arg(ap, scm_obj_t));
                break;
            case 'o':
                m_escape = false;
                m_unwrap = false;
                m_radix = 8;
                write(va_arg(ap, scm_obj_t));
                break;
            case 'b':
                m_escape = false;
                m_unwrap = false;
                m_radix = 2;
                write(va_arg(ap, scm_obj_t));
                break;
            case '%':
                port_put_byte(m_port, '\n');
                break;
            case '&':
                if (m_port->column != 1) port_put_byte(m_port, '\n');
                break;
            case '!':
                m_flush = true;
                break;
            case '~':
                port_put_byte(m_port, '~');
                break;
            case 't':
                port_put_byte(m_port, '\t');
                break;
            case '_':
                port_put_byte(m_port, ' ');
                break;
            default:
                fatal("%s:%u unrecognized format.", __FILE__, __LINE__);
                return;
            }
            break;
        case '%':
            c = *fmt++;
            switch (c) {
            case 's':
                port_puts(m_port, va_arg(ap, char*));
                break;
            case 'c':
                port_put_byte(m_port, va_arg(ap, int));
                break;
            case 'd':
                snprintf(buf, sizeof(buf), "%d", va_arg(ap, int));
                port_puts(m_port, buf);
                break;
            case 'x':
                snprintf(buf, sizeof(buf), "%x", va_arg(ap, int));
                port_puts(m_port, buf);
                break;
            case 'X':
                snprintf(buf, sizeof(buf), "%X", va_arg(ap, int));
                port_puts(m_port, buf);
                break;
            case 'U': {
                int ucs4 = va_arg(ap, int);
                if (ucs4 < 128) {
                    // put char in '~' or \tab or U+10
                    switch (ucs4) {
                    case   0: port_puts(m_port, "nul(U+0000)");         break;
                    case   7: port_puts(m_port, "alarm(U+0007)");       break;
                    case   8: port_puts(m_port, "backspace(U+0008)");   break;
                    case   9: port_puts(m_port, "tab(U+0009)");         break;
                    case  10: port_puts(m_port, "linefeed(U+000A)");    break;
                    case  11: port_puts(m_port, "vtab(U+000B)");        break;
                    case  12: port_puts(m_port, "page(U+000C)");        break;
                    case  13: port_puts(m_port, "return(U+000D)");      break;
                    case  27: port_puts(m_port, "esc(U+001B)");         break;
                    case  32: port_puts(m_port, "space(U+0020)");       break;
                    case 127: port_puts(m_port, "delete(U+007F)");      break;
                    default:
                       if (ucs4 < 32) {
                            snprintf(buf, sizeof(buf), "U+%04X", ucs4);
                            port_puts(m_port, buf);
                        } else {
                            port_put_byte(m_port, '\'');
                            m_escape = false;
                            m_unwrap = false;
                            m_radix = 10;
                            write(MAKECHAR(ucs4));
                            port_put_byte(m_port, '\'');
                        }
                        break;
                    }
                } else {
                    port_put_byte(m_port, '\'');
                    m_escape = false;
                    m_unwrap = false;
                    m_radix = 10;
                    write(MAKECHAR(ucs4));
                    port_put_byte(m_port, '\'');
                    snprintf(buf, sizeof(buf), "(U+%04X)", ucs4);
                    port_puts(m_port, buf);
                }
                break;
            }
            case 'f':
                snprintf(buf, sizeof(buf), "%f", va_arg(ap, double));
                port_puts(m_port, buf);
                break;
            case '%':
                port_put_byte(m_port, '%');
                break;
            default:
                fatal("%s:%u unrecognized format.", __FILE__, __LINE__);
                return;
            }
            break;
        default:
            port_put_byte(m_port, c);
            break;
        }
    }
}

bool
printer_t::write_abbreviated(scm_obj_t obj)
{
    assert(obj);
    if (SYMBOLP(obj)) {
        scm_symbol_t symbol = (scm_symbol_t)obj;
        if (symbol == make_symbol(m_vm->m_heap, "quote")) {
            port_put_byte(m_port, '\'');
            return true;
        } else if (symbol == make_symbol(m_vm->m_heap, "unquote")) {
            port_put_byte(m_port, ',');
            return true;
        } else if (symbol == make_symbol(m_vm->m_heap, "unquote-splicing")) {
            port_puts(m_port, ",@");
            return true;
        } else if (symbol == make_symbol(m_vm->m_heap, "quasiquote")) {
            port_put_byte(m_port, '`');
            return true;
        }
    }
    return false;
}

void
printer_t::write_ucs4(uint32_t c)
{
    char utf8[4];
    int n = cnvt_ucs4_to_utf8(c, (uint8_t*)utf8);
    for (int i = 0; i < n; i++) port_put_byte(m_port, utf8[i]);
}

const char*
printer_t::get_tuple_type_name(scm_obj_t obj)
{
    if (TUPLEP(obj)) {
        scm_tuple_t tuple = (scm_tuple_t)obj;
        scm_obj_t e0 = tuple->elts[0];
        if (SYMBOLP(e0)) {
            scm_symbol_t type = (scm_symbol_t)e0;
            if (strncmp(type->name, "type:", 5) == 0) return type->name + 5;
        }
    }
    return NULL;
}

void
printer_t::scan(scm_hashtable_t ht, scm_obj_t obj)
{
    scm_obj_t value = get_hashtable(ht, obj);
    if (value == scm_true) return;
    if (value == scm_false) {
        m_vm->m_heap->write_barrier(obj);
        int nsize = put_hashtable(ht, obj, scm_true);
        if (nsize) rehash_hashtable(m_vm->m_heap, ht, nsize);
        return;
    }

    if (PAIRP(obj)) {
        m_vm->m_heap->write_barrier(obj);
        int nsize = put_hashtable(ht, obj, scm_false);
        if (nsize) rehash_hashtable(m_vm->m_heap, ht, nsize);
        scan(ht, CAR(obj));
        scan(ht, CDR(obj));
        return;
    }
    if (VECTORP(obj)) {
        scm_vector_t vector = (scm_vector_t)obj;
        int n = vector->count;
        if (n == 0) return;
        m_vm->m_heap->write_barrier(obj);
        int nsize = put_hashtable(ht, obj, scm_false);
        if (nsize) rehash_hashtable(m_vm->m_heap, ht, nsize);
        scm_obj_t* elts = vector->elts;
        for (scm_obj_t* e = elts; e != elts + n; e++) scan(ht, *e);
        return;
    }
    if (TUPLEP(obj)) {
        scm_tuple_t tuple = (scm_tuple_t)obj;
        int n = HDR_TUPLE_COUNT(tuple->hdr);
        if (n == 0) return;
        m_vm->m_heap->write_barrier(obj);
        int nsize = put_hashtable(ht, obj, scm_false);
        if (nsize) rehash_hashtable(m_vm->m_heap, ht, nsize);
        scm_obj_t* elts = tuple->elts;
        for (scm_obj_t* e = elts; e != elts + n; e++) scan(ht, *e);
        return;
    }
}

static const char*
proc_name(scm_obj_t obj)
{
    if (SUBRP(obj)) {
        scm_subr_t subr = (scm_subr_t)obj;
        assert(SYMBOLP(subr->doc));
        scm_symbol_t symbol = (scm_symbol_t)subr->doc;
        return symbol->name;
    }
    if (CLOSUREP(obj)) {
        scm_closure_t closure = (scm_closure_t)obj;
        if (closure->doc == scm_nil) return NULL;
        assert(SYMBOLP(closure->doc));
        scm_symbol_t symbol = (scm_symbol_t)closure->doc;
        const char* s = symbol->name;
        const char* p = strchr(s, IDENTIFIER_LIBRARY_SUFFIX);
        if (p) s = p + 1;
        return s;
    }
    if (obj == scm_proc_apply) return "apply";
    if (obj == scm_proc_callcc) return "call-with-current-continuation";
    if (obj == scm_proc_apply_values) "apply-values";
    assert(false);
    return NULL;
}

void
printer_t::write(scm_obj_t ht, scm_obj_t obj)
{
    char buf[32];
    if (HASHTABLEP(ht)) {
        scm_obj_t value = get_hashtable((scm_hashtable_t)ht, obj);
        if (FIXNUMP(value)) {
            snprintf(buf, sizeof(buf), "#%d#", FIXNUM(value));
            port_puts(m_port, buf);
            return;
        }
        if (value == scm_true) {
            snprintf(buf, sizeof(buf), "#%d=", m_shared_tag);
            port_puts(m_port, buf);
            m_vm->m_heap->write_barrier(obj);
            put_hashtable((scm_hashtable_t)ht, obj, MAKEFIXNUM(m_shared_tag));
            m_shared_tag++;
        }
    }

    if (PAIRP(obj)) {
        bool abbreviated = PAIRP(CDR(obj)) && (CDDR(obj) == scm_nil) && write_abbreviated(CAR(obj));
        if (abbreviated) obj = CDR(obj);
        else port_put_byte(m_port, '(');
        for (scm_obj_t e = obj; e != scm_nil; e = CDR(e)) {
            if (e != obj) port_put_byte(m_port, ' ');
            if (PAIRP(e)) {
                if (HASHTABLEP(ht)) {
                    scm_obj_t datum = get_hashtable((scm_hashtable_t)ht, CDR(e));
                    if (datum == scm_true || FIXNUMP(datum)) {
                        write(ht, CAR(e));
                        port_puts(m_port, " . ");
                        write(ht, CDR(e));
                        break;
                    }
                }
                if (CAR(e) == make_symbol(m_vm->m_heap, "unquote")) {
                    if (PAIRP(CDR(e)) && CDDR(e) == scm_nil) {
                        port_puts(m_port, ". ,");
                        write(ht, CADR(e));
                        break;
                    }
                }
                write(ht, CAR(e));
                if (m_column_limit && m_port->column > m_column_limit) {
                    port_puts(m_port, " ...)");
                    return;
                }
            } else {
                port_puts(m_port, ". ");
                write(ht, e);
                break;
            }
        }
        if (!abbreviated) port_put_byte(m_port, ')');
        return;
    }
    if (!CELLP(obj)) {
        if (FIXNUMP(obj)) {
            if (m_radix == 10) {
                snprintf(buf, sizeof(buf), "%d", FIXNUM(obj));
                port_puts(m_port, buf);
                return;
            }
            if (m_radix == 16) {
                snprintf(buf, sizeof(buf), "%x", FIXNUM(obj));
                port_puts(m_port, buf);
                return;
            }
            scm_string_t string = cnvt_fixnum_to_string(m_vm->m_heap, (scm_fixnum_t)obj, m_radix);
            port_puts(m_port, string->name);
            return;
        }
        if (VMINSTP(obj)) {
            int opcode = m_vm->instruction_to_opcode(obj);
            port_puts(m_port, m_vm->m_heap->inherent_symbol(opcode)->name);
            return;
        }
        if (CHARP(obj)) {
            int c = CHAR(obj);
            if (m_escape) {
                port_puts(m_port, "#\\");
                switch (c) {
                    case   0: port_puts(m_port, "nul");         return;
                    case   7: port_puts(m_port, "alarm");       return;
                    case   8: port_puts(m_port, "backspace");   return;
                    case   9: port_puts(m_port, "tab");         return;
                    case  10: port_puts(m_port, "linefeed");    return;
                    case  11: port_puts(m_port, "vtab");        return;
                    case  12: port_puts(m_port, "page");        return;
                    case  13: port_puts(m_port, "return");      return;
                    case  27: port_puts(m_port, "esc");         return;
                    case  32: port_puts(m_port, "space");       return;
                    case 127: port_puts(m_port, "delete");      return;
                }
                if (c < 32) {
                    snprintf(buf, sizeof(buf), "x%X", c);
                    port_puts(m_port, buf);
                    return;
                }
            }
            write_ucs4(c);
            return;
        }
        if (obj == scm_nil)               { port_puts(m_port, "()");                        return; }
        if (obj == scm_eof)               { port_puts(m_port, "#<eof>");                    return; }
        if (obj == scm_true)              { port_puts(m_port, "#t");                        return; }
        if (obj == scm_false)             { port_puts(m_port, "#f");                        return; }
        if (obj == scm_undef)             { port_puts(m_port, "#<undefined>");              return; }
        if (obj == scm_unspecified)       { port_puts(m_port, "#<unspecified>");            return; }
        if (obj == scm_proc_apply)        { port_puts(m_port, "#<procedure apply>");        return; }
        if (obj == scm_proc_callcc)       { port_puts(m_port, "#<procedure call/cc>");      return; }
        if (obj == scm_proc_apply_values) { port_puts(m_port, "#<procedure apply-values>"); return; }
        format("#<unknown 0x%x>", obj);
        return;
    }
    int tc = HDR_TC(HDR(obj));
    assert(tc >= 0 && tc <= TC_MASKBITS);
    switch (tc) {
        case TC_BIGNUM: {
            scm_string_t string = cnvt_bignum_to_string(m_vm->m_heap, (scm_bignum_t)obj, m_radix);
            port_puts(m_port, string->name);
            return;
        }
        case TC_SYMBOL: {
            scm_symbol_t symbol = (scm_symbol_t)obj;
            const char *s = symbol->name;
            if (m_unwrap) {
                const char* e;
                if (UNINTERNEDSYMBOLP(symbol)) {
                    int len = HDR_SYMBOL_SIZE(symbol->hdr);
                    int n1 = (uint8_t)symbol->name[len + 1];
                    if (m_escape) {
                        e = s + n1;
                    } else {
                        int n2 = strlen(s);
                        e = s + (n1 < n2 ? n1 : n2);
                    }
                } else {
                    if (m_escape) e = s + HDR_SYMBOL_SIZE(symbol->hdr);
                    else e = s + strlen(s);
                }
                const char* p = strchr(s, IDENTIFIER_LIBRARY_SUFFIX);
                if (p) s = p + 1;
                if (s[0] == IDENTIFIER_PRIMITIVE_PREFIX) {
                    if (s[1] && (s[1] != IDENTIFIER_PRIMITIVE_PREFIX) && (s[1] != IDENTIFIER_CSTUB_MARK)) {
                        if (e - s < MAX_READ_SYMBOL_LENGTH) {
                            char name[MAX_READ_SYMBOL_LENGTH + 1];
                            memcpy(name, s, e - s);
                            name[e - s] = 0;
                            if (m_vm->m_heap->lookup_system_environment(make_symbol(m_vm->m_heap, name)) != scm_undef) s = s + 1;
                        }
                    }
                }
                if (m_escape) {
                    write_pretty_symbol((const uint8_t*)s, e - s);
                } else {
                    while (s != e) port_put_byte(m_port, *s++);
                }
            } else if (m_escape) {
                if (m_r6rs) write_r6rs_symbol((const uint8_t*)s, HDR_SYMBOL_SIZE(symbol->hdr));
                else write_pretty_symbol((const uint8_t*)s, HDR_SYMBOL_SIZE(symbol->hdr));
            } else {
                port_puts(m_port, s);
            }
            return;
        }
        case TC_STRING: {
            scm_string_t string = (scm_string_t)obj;
            if (m_escape) {
                port_put_byte(m_port, '"');
                write_string((const uint8_t *)string->name, string->size);
                port_put_byte(m_port, '"');
            } else {
                port_puts(m_port, string->name);
            }
            return;
        }
        case TC_VECTOR: {
            port_puts(m_port, "#(");
            scm_vector_t vector = (scm_vector_t)obj;
            int n = vector->count;
            scm_obj_t* elts = vector->elts;
            for (scm_obj_t* e = elts; e != elts + n; e++) {
                if (e != elts) port_put_byte(m_port, ' ');
                write(ht, *e);
                if (m_column_limit && m_port->column > m_column_limit) {
                    port_puts(m_port, " ...)");
                    return;
                }
            }
            port_put_byte(m_port, ')');
            return;
        }
        case TC_BVECTOR: {
            scm_bvector_t bvector = (scm_bvector_t)obj;
            if (HDR_BVECTOR_MAPPING(bvector->hdr)) {
                format("#<bytevector-mapping 0x%x %d>", bvector->elts, bvector->count);
            } else {
                port_puts(m_port, "#vu8(");
                uint8_t* u8 = (uint8_t*)bvector->elts;
                for (int i = 0; i < bvector->count; i++) {
                    if (i != 0) port_put_byte(m_port, ' ');
                    snprintf(buf, sizeof(buf), "%u", u8[i]);
                    port_puts(m_port, buf);
                    if (m_column_limit && m_port->column > m_column_limit) {
                        port_puts(m_port, " ...)");
                        return;
                    }
                }
                port_put_byte(m_port, ')');
            }
            return;
        }
        case TC_TUPLE: {
            r6rs_param_t no_r6rs(this, false);
            scm_tuple_t tuple = (scm_tuple_t)obj;            
            int n = HDR_TUPLE_COUNT(tuple->hdr);
            {
                if (TUPLEP(tuple->elts[0])) {
                    const char* type_name = get_tuple_type_name(tuple->elts[0]);
                    if (type_name) {
                        if (strcmp(type_name, "record-type-descriptor") == 0) {
                            scm_tuple_t rtd = (scm_tuple_t)tuple->elts[0];
                            scm_obj_t name = rtd->elts[1];
                            scm_obj_t opaque = rtd->elts[5];
                                                        
                            if (opaque == scm_true) {
                                format("#<opaque-record ~a>", name);
                                return;
                            }
                            if (n > 1 && m_tuple_nest > m_tuple_nest_limit) {
                                format("#<record ~a ...>", name);
                                return;
                            }
                            format("#<record ~a", name);
                            scm_obj_t* elts = tuple->elts;
                            bool save_escape = m_escape;
                            m_escape = true;
                            m_tuple_nest++;
                            for (scm_obj_t* e = elts + 1; e != elts + n; e++) {
                                port_put_byte(m_port, ' ');
                                write(ht, *e);
                            }
                            m_tuple_nest--;                                
                            m_escape = save_escape;
                            port_put_byte(m_port, '>');
                            return;
                        }
                    }
                }
                const char* type_name = get_tuple_type_name(tuple);
                if (type_name) {
#if !SCDEBUG
                    if (strcmp(type_name, "syntax") == 0) {
                        port_puts(m_port, "#<syntax ");
                        format("~r", tuple->elts[1]);
                        port_put_byte(m_port, '>');
                        return;
                    }
#endif
                    if (strcmp(type_name, "enum-set") == 0) {
                        port_puts(m_port, "#<enum-set ");
                        write(ht, tuple->elts[2]);
                        port_put_byte(m_port, '>');
                        return;
                    }
                    if (strcmp(type_name, "eval-environment") == 0) {
                        port_puts(m_port, "#<eval-environment");
                        scm_obj_t obj = tuple->elts[1];
                        if (PAIRP(obj)) {
                            port_put_byte(m_port, ' ');
                            for (scm_obj_t e = obj; e != scm_nil; e = CDR(e)) {
                                if (e != obj) port_put_byte(m_port, ' ');
                                write(ht, CAR(e));
                            }
                        }
                        port_put_byte(m_port, '>');
                        return;
                    }
                    if (n > 1 && m_tuple_nest > m_tuple_nest_limit) {
                        format("#<%s ...>", type_name);
                        return;
                    }
                    format("#<%s", type_name);
                    scm_obj_t* elts = tuple->elts;
                    m_tuple_nest++;
                    for (scm_obj_t* e = elts + 1; e != elts + n; e++) {
                        if (e != elts) port_put_byte(m_port, ' ');
                        write(ht, *e);
                    }
                    m_tuple_nest--;
                    port_put_byte(m_port, '>');
                    return;
                }
            }
            if (n == 0) {
                port_puts(m_port, "#<tuple>");
                return;
            }
            port_puts(m_port, "#<tuple ");
            scm_obj_t* elts = tuple->elts;
            for (scm_obj_t* e = elts; e != elts + n; e++) {
                if (e != elts) port_put_byte(m_port, ' ');
                write(ht, *e);
            }
            port_put_byte(m_port, '>');
            return;
        }
        case TC_VALUES: {
            port_puts(m_port, "#<values");
            scm_values_t values = (scm_values_t)obj;
            int n = HDR_VALUES_COUNT(values->hdr);
            for (int i = 0; i < n; i++) {
                port_put_byte(m_port, ' ');
                write(ht, values->elts[i]);
            }
            port_put_byte(m_port, '>');
            return;
        }
        case TC_HASHTABLE: {
            scm_hashtable_t ht = (scm_hashtable_t)obj;
            hashtable_rec_t* ht_datum = ht->datum;
            port_puts(m_port, "#<hashtable ");
            switch (ht->type) {
                case SCM_HASHTABLE_TYPE_EQ: {
                    port_puts(m_port, "eq?");
                    break;
                }
                case SCM_HASHTABLE_TYPE_EQV: {
                    port_puts(m_port, "eqv?");
                    break;
                }
                case SCM_HASHTABLE_TYPE_EQUAL: {
                    port_puts(m_port, "equal?");
                    break;
                }
                case SCM_HASHTABLE_TYPE_STRING: {
                    port_puts(m_port, "string=?");
                    break;
                }
                case SCM_HASHTABLE_TYPE_GENERIC: {
                    scm_vector_t vector = (scm_vector_t)ht->handlers;
                    const char* hash_name = proc_name(vector->elts[SCM_HASHTABLE_HANDLER_HASH]);
                    if (hash_name) format("%s ", hash_name);
                    else format("0x%x ", vector->elts[SCM_HASHTABLE_HANDLER_HASH]);
                    const char* equiv_name = proc_name(vector->elts[SCM_HASHTABLE_HANDLER_EQUIV]);
                    if (equiv_name) format("%s>", equiv_name);
                    else format("0x%x>", vector->elts[SCM_HASHTABLE_HANDLER_EQUIV]);
                    return;
                }
                default:
                    assert(false);
            }
            if (HDR_HASHTABLE_IMMUTABLE(ht->hdr)) port_puts(m_port, " immutable");
            if (ht_datum) format(" %d/%d/%d>",ht_datum->live, ht_datum->used, ht_datum->capacity);
            else port_put_byte(m_port, '>');
            return;
        }
        case TC_WEAKHASHTABLE: {
            scm_weakhashtable_t ht = (scm_weakhashtable_t)obj;
            weakhashtable_rec_t* ht_datum = ht->datum;
            format("#<weak-hashtable eq? %d/%d/%d>",ht_datum->live, ht_datum->used, ht_datum->capacity);
            return;
        }
        case TC_SOCKET: {
            scm_socket_t socket = (scm_socket_t)obj;
            scoped_lock lock(socket->lock);
            switch (socket->mode) {
                case SCM_SOCKET_MODE_CLIENT: port_puts(m_port, "#<client-socket"); break;
                case SCM_SOCKET_MODE_SERVER: port_puts(m_port, "#<server-socket"); break;
                default: port_puts(m_port, "#<socket"); break;
            }
            if (socket->fd == INVALID_SOCKET) {
                port_puts(m_port, " closed>");
                return;
            }
            struct protoent* ent = getprotobynumber(socket->protocol);
            if (ent) format(" %s", ent->p_name);    
            switch (socket->socktype) {
                case SOCK_STREAM: port_puts(m_port, " stream"); break;
                case SOCK_DGRAM: port_puts(m_port, " dgram"); break;
                case SOCK_RAW: port_puts(m_port, " raw"); break;
                default: format(" type(%d)", socket->socktype); break;
            } 
            format(" ~a>", socket_name_string(m_vm->m_heap, socket));
            return;
        }
        case TC_PORT: {
            scm_port_t port = (scm_port_t)obj;
            scoped_lock lock(port->lock);
            format("#<%s-", (port_textual_pred(port) ? "textual" : "binary"));
            if (port_input_pred(port) && port_output_pred(port)) {
                format("input/output-port ~s", port->name);
            } else {
                format("%s-port ~s", (port_input_pred(port) ? "input" : "output"), port->name);
            }
            if (port->transcoder != scm_false) {
                switch (port->codec) {
                    case SCM_PORT_CODEC_LATIN1: port_puts(m_port, " latin-1"); break;
                    case SCM_PORT_CODEC_UTF8:   port_puts(m_port, " utf-8"); break;
                    case SCM_PORT_CODEC_UTF16:  port_puts(m_port, " utf-16"); break;
#if _MSC_VER
                    case SCM_PORT_CODEC_CP932:  port_puts(m_port, " cp932"); break;
#endif
                }
            }
            if (!port_open_pred(port)) port_puts(m_port, " closed");
            port_put_byte(m_port, '>');
            return;
        }
        case TC_COMPLEX: {
            scm_obj_t ans = cnvt_number_to_string(m_vm->m_heap, obj, m_radix);
            assert(STRINGP(ans));
            port_puts(m_port, ((scm_string_t)ans)->name);
            return;
        }
        case TC_RATIONAL: {
            scm_obj_t ans = cnvt_number_to_string(m_vm->m_heap, obj, m_radix);
            assert(STRINGP(ans));
            port_puts(m_port, ((scm_string_t)ans)->name);
            return;
        }
        case TC_CLOSURE: {
            scm_closure_t closure = (scm_closure_t)obj;
#ifdef NDEBUG
            r6rs_param_t no_r6rs(this, false);
            if (closure->doc == scm_nil) format("#<closure 0x%x>", closure);
            else format("#<closure ~s>", closure->doc);
#else
            vm_env_t env = (vm_env_t)closure->env;
            if (env == NULL) {
                if (closure->doc == scm_nil) format("#<closure 0x%x>", closure->code);
                else format("#<closure ~s>", closure->doc);
            } else {
                if (closure->doc == scm_nil) format("#<closure 0x%x>", closure->code);
                else format("#<closure ~s env:0x%x count:%d up:0x%x>", closure->doc, env, env->count, env->up);
            }
#endif
            return;
        }
        case TC_CONT: {
            scm_cont_t cont = (scm_cont_t)obj;
            format("#<continuation 0x%x>", cont);
            return;
        }
        case TC_HEAPENV: {
            format("#<vm-env 0x%x>", obj);
            return;
        }
        case TC_HEAPCONT: {
            format("#<vm-cont 0x%x>", obj);
            return;
        }
        case TC_ENVIRONMENT: {
            scm_environment_t environment = (scm_environment_t)obj;
            format("#<top-level-environment %s>", environment->name->name);
            return;
        }
        case TC_GLOC: {
            scm_gloc_t gloc = (scm_gloc_t)obj;
            if (SYMBOLP(gloc->variable)) format("#<gloc ~a>", gloc->variable);
            else format("#<gloc 0x%x>", obj);
            return;
        }
        case TC_SUBR: {
            scm_subr_t subr = (scm_subr_t)obj;
            format("#<subr ~a>", subr->doc);
            return;
        }
        case TC_WEAKMAPPING: {
            scm_weakmapping_t wmap = (scm_weakmapping_t)obj;
            format("#<weak-mapping ~s ~s>", wmap->key, wmap->value);
            return;
        }
        case TC_FLONUM: {
            scm_obj_t ans = cnvt_number_to_string(m_vm->m_heap, obj, m_radix);
            assert(STRINGP(ans));
            port_puts(m_port, ((scm_string_t)ans)->name);
            return;
        }
        default:
            format("#<unknown 0x%x>", obj);
            return;
    }
}

void
printer_t::write_shared(scm_obj_t obj)
{
    scm_hashtable_t ht = make_hashtable(m_vm->m_heap, SCM_HASHTABLE_TYPE_EQ, lookup_mutable_hashtable_size(0));
    scoped_lock lock(ht->lock);
    scan(ht, obj);
    write(ht, obj);
}

void
printer_t::write(scm_obj_t obj)
{
    write(scm_false, obj);
}
