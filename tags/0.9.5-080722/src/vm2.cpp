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

/*
scm_gloc_t
VM::prebind_gloc(scm_obj_t variable, scm_hashtable_t ht, bool set)
{
  #ifndef NDEBUG
    if (!SYMBOLP(variable)) {
        printf("invalid gloc variable: %x\n", variable);
    }
  #endif
    assert(SYMBOLP(variable));
    scm_symbol_t symbol = (scm_symbol_t)variable;
    scoped_lock lock(m_current_environment->variable->lock);
    scm_gloc_t gloc = (scm_gloc_t)get_hashtable(m_current_environment->variable, symbol);
    if (gloc == scm_undef) {
        gloc = make_gloc(m_heap, m_current_environment, symbol);
        gloc->value = scm_undef;
        m_heap->write_barrier(symbol);
        m_heap->write_barrier(gloc);
        int nsize = put_hashtable(m_current_environment->variable, symbol, gloc);
        if (nsize) rehash_hashtable(m_heap, m_current_environment->variable, nsize);
        if (UNINTERNED_VARIABLE(symbol)) {
            int nsize = put_hashtable(ht, symbol, (set ? scm_true : scm_false));
            if (nsize) rehash_hashtable(m_heap, ht, nsize);
        }
    } else {
        if (UNINTERNED_VARIABLE(symbol) && set) {
            if (get_hashtable(ht, symbol) == scm_false) put_hashtable(ht, symbol, scm_true);
        }
    }
    return gloc;
}
*/

scm_gloc_t
VM::prebind_gloc(scm_obj_t variable, scm_hashtable_t ht, bool set)
{
  #ifndef NDEBUG
    if (!SYMBOLP(variable)) {
        printf("invalid gloc variable: %x\n", variable);
    }
  #endif
    assert(SYMBOLP(variable));
    scm_symbol_t symbol = (scm_symbol_t)variable;
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
    if (set && UNINTERNED_VARIABLE(symbol)) {
        int nsize = put_hashtable(ht, symbol, scm_true);
        if (nsize) rehash_hashtable(m_heap, ht, nsize);
    }
    return gloc;
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

    scm_hashtable_t ht = make_hashtable(m_heap, SCM_HASHTABLE_TYPE_EQ, lookup_mutable_hashtable_size(0));
    scoped_lock lock(ht->lock);
    prebind_list(code, ht);
    if (ht->datum->live) {
        int nsize = ht->datum->capacity;
        for (int i = 0; i < nsize; i++) {
            scm_obj_t obj = ht->datum->elts[i];
            if (SYMBOLP(obj)) {
                scm_symbol_t symbol = (scm_symbol_t)obj;
                scoped_lock lock(m_current_environment->variable->lock);
                scm_gloc_t gloc = (scm_gloc_t)get_hashtable(m_current_environment->variable, symbol);
                if (gloc != scm_undef) {
                    if (ht->datum->elts[i + nsize] == scm_true) {
                        gloc->variable = scm_undef;
                    }
                    remove_hashtable(m_current_environment->variable, symbol);
                }
            }
        }
    }
}

static scm_obj_t
subr_warn_cache_definition_conflict(VM* vm, int argc, scm_obj_t argv[])
{
    raise_error(vm, NULL, "compiled code cache out of date", 0);
    return scm_undef;
}

void
VM::prebind_list(scm_obj_t code, scm_hashtable_t ht)
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
                scm_gloc_t gloc = prebind_gloc(CAR(operands), ht, false);
                CAAR(code) = m_heap->inherent_symbol(VMOP_GLOC);
                m_heap->write_barrier(gloc);
                CDAR(code) = gloc;
            } break;

            case VMOP_RET_GLOC_OF: {
                scm_gloc_t gloc = prebind_gloc(CAR(operands), ht, false);
                CAAR(code) = m_heap->inherent_symbol(VMOP_RET_GLOC);
                m_heap->write_barrier(gloc);
                CDAR(code) = gloc;
            } break;

            case VMOP_PUSH_GLOC_OF: {
                scm_gloc_t gloc = prebind_gloc(CAR(operands), ht, false);
                CAAR(code) = m_heap->inherent_symbol(VMOP_PUSH_GLOC);
                m_heap->write_barrier(gloc);
                CDAR(code) = gloc;
            } break;

            case VMOP_SET_GLOC_OF: {
                scm_gloc_t gloc = prebind_gloc(CAR(operands), ht, true);
                CAAR(code) = m_heap->inherent_symbol(VMOP_SET_GLOC);
                m_heap->write_barrier(gloc);
                CDAR(code) = gloc;
            } break;

            case VMOP_APPLY_GLOC_OF: {
                scm_gloc_t gloc = prebind_gloc(CAR(operands), ht, false);
                CAAR(code) = m_heap->inherent_symbol(VMOP_APPLY_GLOC);
                m_heap->write_barrier(gloc);
                CAR(operands) = gloc;
            } break;

            case VMOP_TOUCH_GLOC_OF: {
                scm_gloc_t gloc = prebind_gloc(CAR(operands), ht, false);
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
                scm_gloc_t gloc = prebind_gloc(CAR(operands), ht, false);
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
                scm_gloc_t gloc = prebind_gloc(CAR(operands), ht, false);
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
                scm_gloc_t gloc = prebind_gloc(CAR(operands), ht, false);
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
                prebind_list(CDR(operands), ht);
                m_heap->write_barrier(CDR(operands));
                CDAR(code) = CDR(operands);
                break;

            case VMOP_CLOSE:
                prebind_list(CDR(operands), ht);
                break;

            case VMOP_RET_CLOSE:
            case VMOP_PUSH_CLOSE:
            case VMOP_EXTEND_ENCLOSE: {
  #if USE_SYMBOL_THREAD
                if (CLOSUREP(operands)) break;
  #endif
                prebind_list(CDR(operands), ht);
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
                prebind_list(operands, ht);
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
