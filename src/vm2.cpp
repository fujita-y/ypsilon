/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#include "core.h"
#include "vm.h"
#include "hash.h"
#include "violation.h"

#define CONS(a, d)      make_pair(m_heap, (a), (d))

scm_gloc_t
VM::prebind_gloc(scm_obj_t variable)
{
  #ifndef NDEBUG
    if (!SYMBOLP(variable)) {
        printf("invalid gloc variable: %x\n", variable);
    }
  #endif
    assert(SYMBOLP(variable));
    scm_symbol_t symbol = (scm_symbol_t)variable;
    if (UNINTERNEDSYMBOLP(symbol)) {
        scoped_lock lock(m_heap->m_hidden_variables->lock);
        scm_obj_t obj = lookup_weakhashtable(m_heap->m_hidden_variables, symbol);
        if (obj == scm_undef) {
            scm_gloc_t gloc = make_gloc(m_heap, m_current_environment, make_symbol(m_heap, symbol->name));
            gloc->value = scm_undef;
            scm_weakmapping_t wmap = make_weakmapping(m_heap, symbol, gloc);
            m_heap->write_barrier(wmap);
            int nsize = put_weakhashtable(m_heap->m_hidden_variables, wmap);
            if (nsize) rehash_weakhashtable(m_heap, m_heap->m_hidden_variables, nsize);
            return gloc;
        } else {
            assert(WEAKMAPPINGP(obj));
            scm_weakmapping_t wmap = (scm_weakmapping_t)obj;
            assert(GLOCP(wmap->value));
            scm_gloc_t gloc = (scm_gloc_t)wmap->value;
            return gloc;
        }
    } else {
        scoped_lock lock(m_current_environment->variable->lock);
        scm_gloc_t gloc = (scm_gloc_t)get_hashtable(m_current_environment->variable, symbol);
        if (gloc == scm_undef) {
            gloc = make_gloc(m_heap, m_current_environment, symbol);
            gloc->value = scm_undef;
            m_heap->write_barrier(symbol);
            m_heap->write_barrier(gloc);
            int nsize = put_hashtable(m_current_environment->variable, symbol, gloc);
            if (nsize) rehash_hashtable(m_heap, m_current_environment->variable, nsize);
        }
        return gloc;
    }
}

static scm_obj_t
subr_warn_cache_definition_conflict(VM* vm, int argc, scm_obj_t argv[])
{
    raise_error(vm, NULL, "compiled code cache out of date", 0);
    return scm_undef;
}

void
VM::prebind_list(scm_obj_t code)
{

    while (PAIRP(code)) {

  #if USE_DIRECT_THREAD
        assert(!VMINSTP(CAAR(code)));
  #endif

  #if USE_FIXNUM_THREAD
        assert(!FIXNUMP(CAAR(code)));
  #endif

        scm_symbol_t symbol = (scm_symbol_t)CAAR(code);

  #ifndef NDEBUG
        if (!OPCODESYMBOLP(symbol)) printf("invalid instruction: %s\n", symbol->name);
  #endif

        assert(OPCODESYMBOLP(symbol));
        int opcode = HDR_SYMBOL_CODE(symbol->hdr);
        scm_obj_t operands = (scm_obj_t)CDAR(code);
        switch (opcode) {

            case VMOP_GLOC_OF: {
                scm_gloc_t gloc = prebind_gloc(CAR(operands));
                CAAR(code) = m_heap->inherent_symbol(VMOP_GLOC);
                m_heap->write_barrier(gloc);
                CDAR(code) = gloc;
            } break;

            case VMOP_RET_GLOC_OF: {
                scm_gloc_t gloc = prebind_gloc(CAR(operands));
                CAAR(code) = m_heap->inherent_symbol(VMOP_RET_GLOC);
                m_heap->write_barrier(gloc);
                CDAR(code) = gloc;
            } break;

            case VMOP_PUSH_GLOC_OF: {
                scm_gloc_t gloc = prebind_gloc(CAR(operands));
                CAAR(code) = m_heap->inherent_symbol(VMOP_PUSH_GLOC);
                m_heap->write_barrier(gloc);
                CDAR(code) = gloc;
            } break;

            case VMOP_SET_GLOC_OF: {
                scm_gloc_t gloc = prebind_gloc(CAR(operands));
                CAAR(code) = m_heap->inherent_symbol(VMOP_SET_GLOC);
                m_heap->write_barrier(gloc);
                CADAR(code) = gloc;
            } break;

            case VMOP_APPLY_GLOC_OF: {
                scm_gloc_t gloc = prebind_gloc(CAR(operands));
                CAAR(code) = m_heap->inherent_symbol(VMOP_APPLY_GLOC);
                m_heap->write_barrier(gloc);
                CAR(operands) = gloc;
            } break;

            case VMOP_TOUCH_GLOC_OF: {
                scm_gloc_t gloc = prebind_gloc(CAR(operands));
                if (gloc->value != scm_undef)   {
                    m_heap->write_barrier(CADR(code));
                    m_heap->write_barrier(CDDR(code));
                    CAR(code) = CADR(code);
                    CDR(code) = CDDR(code);
                    continue;
                }
                CAAR(code) = m_heap->inherent_symbol(VMOP_TOUCH_GLOC);
                m_heap->write_barrier(gloc);
                CDAR(code) = gloc;
            } break;

            case VMOP_PUSH_SUBR_GLOC_OF: {
                scm_gloc_t gloc = prebind_gloc(CAR(operands));
  #ifndef NDEBUG
                if (!SUBRP(gloc->value)) {
                    if (SYMBOLP(gloc->variable)) printf("** warning: expect gloc of %s contain SUBR but got %x, maybe forward reference\n", ((scm_symbol_t)gloc->variable)->name, gloc->value);
                    else printf("** warning: expect gloc %x contain SUBR but got %x, maybe forward reference\n", gloc, gloc->value);
                }
  #endif
                if (SUBRP(gloc->value)) {
                    CAAR(code) = m_heap->inherent_symbol(VMOP_PUSH_SUBR);
                    m_heap->write_barrier(gloc->value);
                    CAR(operands) = gloc->value;
                } else {
                    scm_subr_t subr = make_subr(m_heap, subr_warn_cache_definition_conflict, scm_unspecified);
                    m_heap->write_barrier(subr);
                    gloc->value = subr;
                    m_heap->write_barrier(gloc);
                    CAR(operands) = gloc;
                }
            } break;

            case VMOP_SUBR_GLOC_OF: {
                scm_gloc_t gloc = prebind_gloc(CAR(operands));
  #ifndef NDEBUG
                if (!SUBRP(gloc->value)) {
                    if (SYMBOLP(gloc->variable)) printf("** warning: expect gloc of %s contain SUBR but got %x, maybe forward reference\n", ((scm_symbol_t)gloc->variable)->name, gloc->value);
                    else printf("** warning: expect gloc %x contain SUBR but got %x, maybe forward reference\n", gloc, gloc->value);
                }
  #endif
                if (SUBRP(gloc->value)) {
                    CAAR(code) = m_heap->inherent_symbol(VMOP_SUBR);
                    m_heap->write_barrier(gloc->value);
                    CAR(operands) = gloc->value;
                } else {
                    scm_subr_t subr = make_subr(m_heap, subr_warn_cache_definition_conflict, scm_unspecified);
                    m_heap->write_barrier(subr);
                    gloc->value = subr;
                    m_heap->write_barrier(gloc);
                    CAR(operands) = gloc;
                }
            } break;

            case VMOP_RET_SUBR_GLOC_OF: {
                scm_gloc_t gloc = prebind_gloc(CAR(operands));
            #ifndef NDEBUG
                if (!SUBRP(gloc->value)) {
                    if (SYMBOLP(gloc->variable)) printf("** warning: expect gloc of %s contain SUBR but got %x, maybe forward reference\n", ((scm_symbol_t)gloc->variable)->name, gloc->value);
                    else printf("** warning: expect gloc %x contain SUBR but got %x, maybe forward reference\n", gloc, gloc->value);
                }
            #endif
                if (SUBRP(gloc->value)) {
                    CAAR(code) = m_heap->inherent_symbol(VMOP_RET_SUBR);
                    m_heap->write_barrier(gloc->value);
                    CAR(operands) = gloc->value;
                } else {
                    scm_subr_t subr = make_subr(m_heap, subr_warn_cache_definition_conflict, scm_unspecified);
                    m_heap->write_barrier(subr);
                    gloc->value = subr;
                    m_heap->write_barrier(gloc);
                    CAR(operands) = gloc;
                }
            } break;

            case VMOP_PUSH_CLOSE_LOCAL:
            case VMOP_EXTEND_ENCLOSE_LOCAL:
  #if USE_SYMBOL_THREAD
                if (SYMBOLP(CAAR(operands))) break;
  #endif
                prebind_list(CDR(operands));
                m_heap->write_barrier(CDR(operands));
                CDAR(code) = CDR(operands);
                break;

            case VMOP_CLOSE:
                prebind_list(CDR(operands));
                break;

            case VMOP_RET_CLOSE:
            case VMOP_PUSH_CLOSE:
            case VMOP_EXTEND_ENCLOSE: {
  #if USE_SYMBOL_THREAD
                if (CLOSUREP(operands)) break;
  #endif
                prebind_list(CDR(operands));
  #if PREBIND_CLOSE
                scm_obj_t spec = CAR(operands);
                scm_closure_t closure = make_closure(m_heap, FIXNUM(CAR(spec)), FIXNUM(CADR(spec)), NULL, CDR(operands), CDDR(spec));
                m_heap->write_barrier(closure);
                CDAR(code) = closure;
  #endif
            } break;

            case VMOP_IF_TRUE:
            case VMOP_IF_FALSE_CALL:
            case VMOP_IF_NULLP:
            case VMOP_IF_PAIRP:
            case VMOP_IF_SYMBOLP:
            case VMOP_IF_EQP:
            case VMOP_CALL:
                prebind_list(operands);
                break;

            case VMOP_PUSH_CONST_UNSPEC:
                CAAR(code) = m_heap->inherent_symbol(VMOP_PUSH_CONST);
                CDAR(code) = scm_unspecified;
                break;
            case VMOP_CONST_UNSPEC:
                CAAR(code) = m_heap->inherent_symbol(VMOP_CONST);
                CDAR(code) = scm_unspecified;
                break;
            case VMOP_RET_CONST_UNSPEC:
                CAAR(code) = m_heap->inherent_symbol(VMOP_RET_CONST);
                CDAR(code) = scm_unspecified;
                break;
            case VMOP_PUSH_CONST_UNDEF:
                CAAR(code) = m_heap->inherent_symbol(VMOP_PUSH_CONST);
                CDAR(code) = scm_undef;
                break;
            case VMOP_CONST_UNDEF:
                CAAR(code) = m_heap->inherent_symbol(VMOP_CONST);
                CDAR(code) = scm_undef;
                break;
            case VMOP_RET_CONST_UNDEF:
                CAAR(code) = m_heap->inherent_symbol(VMOP_RET_CONST);
                CDAR(code) = scm_undef;
                break;
        }
        CAAR(code) = symbol_to_instruction(CAAR(code));
        code = CDR(code);
    }
}


void
VM::prebind(scm_obj_t code)
{
  #if USE_DIRECT_THREAD
    if (VMINSTP(CAAR(code))) return;
  #endif

  #if USE_FIXNUM_THREAD
    if (FIXNUMP(CAAR(code))) return;
  #endif

    prebind_list(code);
//  printf("prebind: m_heap->m_hidden_variables->datum->live %d\n", m_heap->m_hidden_variables->datum->live);
}
