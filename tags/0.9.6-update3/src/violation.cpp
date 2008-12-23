/*
  Ypsilon Scheme System
  Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
  See license.txt for terms and conditions of use
*/

#include "core.h"
#include "vm.h"
#include "port.h"
#include "printer.h"
#include "violation.h"

static void
raise_assertion_violation(VM* vm, scm_obj_t who, scm_string_t message, scm_obj_t irritant)
{
    assert(who);
    assert(message);
    scm_obj_t proc = vm->lookup_system_closure(".@assertion-violation");
    if (irritant != NULL) {
        vm->apply_scheme(proc, 3, who, message, irritant);
    } else {
        vm->apply_scheme(proc, 2, who, message);
    }
}

static void
raise_implementation_restriction_violation(VM* vm, scm_obj_t who, scm_string_t message, scm_obj_t irritant)
{
    assert(who);
    assert(message);
    scm_obj_t proc = vm->lookup_system_closure(".@implementation-restriction-violation");
    if (irritant != NULL) {
        vm->apply_scheme(proc, 3, who, message, irritant);
    } else {
        vm->apply_scheme(proc, 2, who, message);
    }
}

void
undefined_violation(VM* vm, scm_obj_t who, scm_string_t message)
{
    assert(who);
    vm->backtrace_seek();
#ifndef NDEBUG
    const char* var_name = NULL;
    if (STRINGP(who)) var_name = ((scm_string_t)who)->name;
    if (SYMBOLP(who)) var_name = ((scm_symbol_t)who)->name;
#endif
    if (!(STRINGP(who) || SYMBOLP(who))) who = scm_false;
    scm_obj_t proc = vm->lookup_system_closure(".@undefined-violation");
    if (message != NULL) {
        vm->apply_scheme(proc, 2, who, message);
    } else {
        vm->apply_scheme(proc, 1, who);
    }
}

void
letrec_violation(VM* vm)
{
    vm->backtrace_seek();
    scm_obj_t proc = vm->lookup_system_closure(".@assertion-violation");
    if (vm->flags.m_warning_level == scm_false) {
        vm->apply_scheme(proc, 2, scm_false, make_string_literal(vm->m_heap,
                         "binding construct attempt to reference uninitialized variable, use '--warning' to perform expansion time check"));
    } else {
        vm->apply_scheme(proc, 2, scm_false, make_string_literal(vm->m_heap,
                         "binding construct attempt to reference uninitialized variable, check warning messages"));
    }
}

void
lexical_violation(VM* vm, scm_obj_t who, scm_string_t message)
{
    assert(who);
    assert(message);
    vm->backtrace_seek();
    scm_obj_t proc = vm->lookup_system_closure(".@lexical-violation");
    vm->apply_scheme(proc, 2, who, message);
}

void
wrong_type_argument_violation(VM* vm, const char* who, int position, const char* expected, scm_obj_t got, int argc, scm_obj_t argv[])
{
    assert(who);
    assert(expected);
    assert(got);
    vm->backtrace_seek();
    scm_port_t port = make_bytevector_port(vm->m_heap, make_symbol(vm->m_heap, "string"), SCM_PORT_DIRECTION_OUT, scm_false, scm_true);
    scoped_lock lock(port->lock);
    printer_t prt(vm, port);
    if (argc < 2) {
        if (got == NULL) prt.format("expected %s, but missing", expected);
        else prt.format("expected %s, but got ~r", expected, got);
    } else {
        if (got == NULL) prt.format("expected %s, but missing for argument %d", expected, got, position + 1);
        else prt.format("expected %s, but got ~r, as argument %d", expected, got, position + 1);
    }
    scm_string_t message = port_extract_string(vm->m_heap, port);
    if (argc < 2) {
        raise_assertion_violation(vm, make_symbol(vm->m_heap, who), message, NULL);
    } else {
        scm_obj_t irritants = scm_nil;
        int last = argc;
        while (--last >= 0) irritants = make_pair(vm->m_heap, argv[last], irritants);
        raise_assertion_violation(vm, make_symbol(vm->m_heap, who), message, irritants);
    }
}

void
invalid_application_violation(VM* vm, scm_obj_t value, int argc, scm_obj_t argv[])
{
    vm->backtrace_seek();
    scm_port_t port = make_bytevector_port(vm->m_heap, make_symbol(vm->m_heap, "string"), SCM_PORT_DIRECTION_OUT, scm_false, scm_true);
    scm_obj_t form = scm_nil;
    int last = argc;
    while (--last >= 0) form = make_pair(vm->m_heap, argv[last], form);
    form = make_pair(vm->m_heap, value, form);
    scoped_lock lock(port->lock);
    printer_t prt(vm, port);
    prt.format("attempt call non-procedure: ~r", form);
    scm_string_t message = port_extract_string(vm->m_heap, port);
    raise_assertion_violation(vm, scm_false, message, NULL);
}

void
invalid_object_violation(VM* vm, const char* who, const char* expected, scm_obj_t object, int argc, scm_obj_t argv[])
{
    vm->backtrace_seek();
    scm_port_t port = make_bytevector_port(vm->m_heap, make_symbol(vm->m_heap, "string"), SCM_PORT_DIRECTION_OUT, scm_false, scm_true);
    scoped_lock lock(port->lock);
    printer_t prt(vm, port);
    prt.format("expected %s, but ~r is not", expected, object);
    scm_string_t message = port_extract_string(vm->m_heap, port);
    if (argc < 2) {
        raise_assertion_violation(vm, make_symbol(vm->m_heap, who), message, NULL);
    } else {
        scm_obj_t irritants = scm_nil;
        int last = argc;
        while (--last >= 0) irritants = make_pair(vm->m_heap, argv[last], irritants);
        raise_assertion_violation(vm, make_symbol(vm->m_heap, who), message, irritants);
    }
}

void
implementation_restriction_violation(VM* vm, const char* who, const char* description, scm_obj_t value, int argc, scm_obj_t argv[])
{
    vm->backtrace_seek();
    scm_port_t port = make_bytevector_port(vm->m_heap, make_symbol(vm->m_heap, "string"), SCM_PORT_DIRECTION_OUT, scm_false, scm_true);
    scoped_lock lock(port->lock);
    printer_t prt(vm, port);
    prt.format("%s ~r", description, value);
    scm_string_t message = port_extract_string(vm->m_heap, port);
    if (argc == 0) {
        raise_implementation_restriction_violation(vm, make_symbol(vm->m_heap, who), message, NULL);
    } else {
        scm_obj_t irritants = scm_nil;
        int last = argc;
        while (--last >= 0) irritants = make_pair(vm->m_heap, argv[last], irritants);
        raise_implementation_restriction_violation(vm, make_symbol(vm->m_heap, who), message, irritants);
    }
}

void
wrong_number_of_arguments_violation(VM* vm, scm_obj_t proc, int required_min, int required_max, int argc, scm_obj_t argv[])
{
    if (CLOSUREP(proc)) {
        scm_closure_t closure = (scm_closure_t)proc;
        if (SYMBOLP(closure->doc)) {
            scm_symbol_t symbol = (scm_symbol_t)closure->doc;
            const char* s = symbol->name;
            const char* p = strchr(s, IDENTIFIER_LIBRARY_SUFFIX);
            if (p) s = p + 1;
            wrong_number_of_arguments_violation(vm, s, required_min, required_max, argc, argv);
            return;
        }
    }
    vm->backtrace_seek();
    scm_port_t port = make_bytevector_port(vm->m_heap, make_symbol(vm->m_heap, "string"), SCM_PORT_DIRECTION_OUT, scm_false, scm_true);
    scoped_lock lock(port->lock);
    printer_t prt(vm, port);
    char plural[] = "s";
    if (argc < 2) plural[0] = 0;
    if (required_max < 0) {
        prt.format("~s required at least %d, but %d argument%s given", proc, required_min, argc, plural);
    } else if (required_min == required_max) {
        prt.format("~s expected %d, but %d argument%s given", proc, required_min, argc, plural);
    } else {
        if (required_max == required_min + 1) {
            prt.format("~s expected %d or %d, but %d argument%s given", proc, required_min, required_max, argc, plural);
        } else {
            prt.format("~s expected %d to %d, but %d argument%s given", proc, required_min, required_max, argc, plural);
        }
    }
    scm_string_t message = port_extract_string(vm->m_heap, port);
    if (argc == 0) {
        raise_assertion_violation(vm, scm_false, message, NULL);
    } else {
        scm_obj_t irritants = scm_nil;
        int last = argc;
        while (--last >= 0) irritants = make_pair(vm->m_heap, argv[last], irritants);
        raise_assertion_violation(vm, scm_false, message, irritants);
    }
}

void
wrong_number_of_arguments_violation(VM* vm, const char* who, int required_min, int required_max, int argc, scm_obj_t argv[])
{
    vm->backtrace_seek();
    scm_port_t port = make_bytevector_port(vm->m_heap, make_symbol(vm->m_heap, "string"), SCM_PORT_DIRECTION_OUT, scm_false, scm_true);
    scoped_lock lock(port->lock);
    printer_t prt(vm, port);
    char plural[] = "s";
    if (argc < 2) plural[0] = 0;
    if (required_max < 0) {
        prt.format("required at least %d, but %d argument%s given", required_min, argc, plural);
    } else if (required_min == required_max) {
        prt.format("expected %d, but %d argument%s given", required_min, argc, plural);
    } else {
        if (required_max == required_min + 1) {
            prt.format("expected %d or %d, but %d argument%s given", required_min, required_max, argc, plural);
        } else {
            prt.format("expected %d to %d, but %d argument%s given", required_min, required_max, argc, plural);
        }
    }
    scm_string_t message = port_extract_string(vm->m_heap, port);
    if (argc == 0) {
        raise_assertion_violation(vm, make_symbol(vm->m_heap, who), message, NULL);
    } else {
        scm_obj_t irritants = scm_nil;
        int last = argc;
        while (--last >= 0) irritants = make_pair(vm->m_heap, argv[last], irritants);
        raise_assertion_violation(vm, make_symbol(vm->m_heap, who), message, irritants);
    }
}

void
invalid_argument_violation(VM* vm, const char* who, const char* description, scm_obj_t value, int position, int argc, scm_obj_t argv[])
{
    vm->backtrace_seek();
    scm_port_t port = make_bytevector_port(vm->m_heap, make_symbol(vm->m_heap, "string"), SCM_PORT_DIRECTION_OUT, scm_false, scm_true);
    scoped_lock lock(port->lock);
    printer_t prt(vm, port);
    if (value == NULL) {
        prt.format("%s", description);
    } else if (position < 0) {
        prt.format("%s ~r", description, value);
    } else {
        prt.format("%s ~r as argument %d", description, value, position + 1);
    }
    scm_string_t message = port_extract_string(vm->m_heap, port);
    if (position >= 0 && argc < 2) {
        raise_assertion_violation(vm, make_symbol(vm->m_heap, who), message, NULL);
    } else {
        scm_obj_t irritants = scm_nil;
        int last = argc;
        while (--last >= 0) irritants = make_pair(vm->m_heap, argv[last], irritants);
        raise_assertion_violation(vm, make_symbol(vm->m_heap, who), message, irritants);
    }
}

void
raise_error(VM* vm, const char* who, const char* description, int code)
{
    vm->backtrace_seek();
    scm_port_t port = make_bytevector_port(vm->m_heap, make_symbol(vm->m_heap, "string"), SCM_PORT_DIRECTION_OUT, scm_false, scm_true);
    scoped_lock lock(port->lock);
    printer_t prt(vm, port);
    if (code) prt.format("%s (%d)", description, code);
    else prt.format("%s", description);
    scm_string_t message = port_extract_string(vm->m_heap, port);
    scm_obj_t proc = vm->lookup_system_closure(".@error");
    vm->apply_scheme(proc, 2, (who ? make_symbol(vm->m_heap, who) : scm_false), message);
}

void raise_error(VM* vm, const char* who, const char* description, int code, int argc, scm_obj_t argv[])
{
    vm->backtrace_seek();
    scm_port_t port = make_bytevector_port(vm->m_heap, make_symbol(vm->m_heap, "string"), SCM_PORT_DIRECTION_OUT, scm_false, scm_true);
    scoped_lock lock(port->lock);
    printer_t prt(vm, port);
    if (code) prt.format("%s (%d)", description, code);
    else prt.format("%s", description);
    scm_string_t message = port_extract_string(vm->m_heap, port);
    scm_obj_t proc = vm->lookup_system_closure(".@error");
    if (argc == 0) {
        vm->apply_scheme(proc, 2, (who ? make_symbol(vm->m_heap, who) : scm_false), message);
    } else {
        scm_obj_t irritants = scm_nil;
        int last = argc;
        while (--last >= 0) irritants = make_pair(vm->m_heap, argv[last], irritants);
        vm->apply_scheme(proc, 3, (who ? make_symbol(vm->m_heap, who) : scm_false), message, irritants);
    }
}

void thread_global_access_violation(VM* vm, scm_obj_t name, scm_obj_t value)
{
    vm->backtrace_seek();
    raise_assertion_violation(vm,
                              scm_false,
                              make_string(vm->m_heap, "child thread attempt to modify global variable"),
                              make_list(vm->m_heap, 2, name, value));
}

void thread_lexical_access_violation(VM* vm, scm_obj_t name, scm_obj_t value)
{
    vm->backtrace_seek();
    scm_obj_t irritants = NULL;
    if (name) {
        if (UNINTERNEDSYMBOLP(name)) {
            scm_symbol_t symbol = (scm_symbol_t)name;
            int len = HDR_SYMBOL_SIZE(symbol->hdr);
            irritants = make_list(vm->m_heap, 2, make_symbol(vm->m_heap, symbol->name, (uint8_t)symbol->name[len + 1]), value);
        } else {
            irritants = make_list(vm->m_heap, 2, name, value);
        }
    } else {
        irritants = make_list(vm->m_heap, 1, value);
    }
    raise_assertion_violation(vm,
                              scm_false,
                              make_string(vm->m_heap, "child thread attempt to modify immutable parent variable"),
                              irritants);
}

void thread_object_access_violation(VM* vm, const char* subr, int argc, scm_obj_t argv[])
{
    vm->backtrace_seek();
    scm_obj_t irritants = scm_nil;
    int last = argc;
    while (--last >= 0) irritants = make_pair(vm->m_heap, argv[last], irritants);
    raise_assertion_violation(vm,
                              scm_false,
                              make_string(vm->m_heap, "child thread attempt to modify parent object"),
                              make_pair(vm->m_heap, vm->m_heap->lookup_system_environment(make_symbol(vm->m_heap, subr)), irritants));
}

void non_serializable_object_violation(VM* vm, const char* who, scm_obj_t obj, int argc, scm_obj_t argv[])
{
    vm->backtrace_seek();
    scm_port_t port = make_bytevector_port(vm->m_heap, make_symbol(vm->m_heap, "string"), SCM_PORT_DIRECTION_OUT, scm_false, scm_true);
    scoped_lock lock(port->lock);
    printer_t(vm, port).format("encountered non-serializable object ~r", obj);
    scm_string_t message = port_extract_string(vm->m_heap, port);
    if (argc < 2) {
        raise_assertion_violation(vm, make_symbol(vm->m_heap, who), message, NULL);
    } else {
        scm_obj_t irritants = scm_nil;
        int last = argc;
        while (--last >= 0) irritants = make_pair(vm->m_heap, argv[last], irritants);
        raise_assertion_violation(vm, make_symbol(vm->m_heap, who), message, irritants);
    }
}

void invalid_serialized_object_violation(VM* vm, const char* who, scm_obj_t obj, int argc, scm_obj_t argv[])
{
    vm->backtrace_seek();
    scm_port_t port = make_bytevector_port(vm->m_heap, make_symbol(vm->m_heap, "string"), SCM_PORT_DIRECTION_OUT, scm_false, scm_true);
    scoped_lock lock(port->lock);
    printer_t(vm, port).format("expected serialized object data, but ~r is not", obj);
    scm_string_t message = port_extract_string(vm->m_heap, port);
    if (argc < 2) {
        raise_assertion_violation(vm, make_symbol(vm->m_heap, who), message, NULL);
    } else {
        scm_obj_t irritants = scm_nil;
        int last = argc;
        while (--last >= 0) irritants = make_pair(vm->m_heap, argv[last], irritants);
        raise_assertion_violation(vm, make_symbol(vm->m_heap, who), message, irritants);
    }
}

void literal_constant_access_violation(VM* vm, const char* who, scm_obj_t obj, int argc, scm_obj_t argv[])
{
    vm->backtrace_seek();
    scm_port_t port = make_bytevector_port(vm->m_heap, make_symbol(vm->m_heap, "string"), SCM_PORT_DIRECTION_OUT, scm_false, scm_true);
    scoped_lock lock(port->lock);
    printer_t(vm, port).format("attempt to modify literal constant ~r", obj);
    scm_string_t message = port_extract_string(vm->m_heap, port);
    scm_obj_t irritants = scm_nil;
    int last = argc;
    while (--last >= 0) irritants = make_pair(vm->m_heap, argv[last], irritants);
    raise_assertion_violation(vm,
                              make_symbol(vm->m_heap, who),
                              message,
                              irritants);
}
