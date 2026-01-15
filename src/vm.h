// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef VM_H_INCLUDED
#define VM_H_INCLUDED

#include "core.h"
#include "object_heap.h"

class printer_t;
class digamma_t;

class VM {
 public:
  scm_obj_t* m_sp;
  scm_obj_t* m_fp;
  void* m_cont;
  void* m_env;
  scm_obj_t m_value;
  scm_obj_t m_pc;
  scm_obj_t m_trace;
  scm_obj_t m_trace_tail;
  scm_obj_t* m_stack_limit;

  object_heap_t* m_heap;
  scm_obj_t* m_stack_top;
  scm_obj_t* m_to_stack_top;
  scm_obj_t* m_to_stack_limit;

  int m_stack_size;

  bool init(object_heap_t* heap);
  void boot();
  void standalone();
  void reset();
  void run();
  void loop(bool resume);

  void scheme_warning(const char* fmt, ...);
  void scheme_error(const char* fmt, ...) ATTRIBUTE(noreturn);
  void system_error(const char* fmt, ...) ATTRIBUTE(noreturn);

  struct {
    // not included in gc root
    scm_obj_t lexical_syntax_version;        // fixnum
    scm_obj_t mutable_literals;              // #t ot #f
    scm_obj_t collect_notify;                // #t or #f
    scm_obj_t collect_stack_notify;          // #t or #f
    scm_obj_t backtrace;                     // #t or #f or fixnum
    scm_obj_t backtrace_line_length;         // fixnum
    scm_obj_t restricted_print_line_length;  // fixnum
    scm_obj_t record_print_nesting_limit;    // fixnum
    scm_obj_t warning_level;                 // #t or #f or fixnum
  } m_flags;

  enum {
    native_thunk_apply = 0,
    native_thunk_pop_cont,
    native_thunk_loop,
    native_thunk_resume_loop,
    native_thunk_escape,
    native_thunk_invalid_state,
    native_thunk_unreachable,
  };

  scm_port_t m_bootport;
  scm_port_t m_current_input;
  scm_port_t m_current_output;
  scm_port_t m_current_error;
  scm_environment_t m_current_environment;
  scm_weakhashtable_t m_current_dynamic_environment;
  scm_obj_t m_current_dynamic_wind_record;
  scm_obj_t m_current_exception_handler;
  scm_obj_t m_current_source_comments;
  int m_recursion_level;
#if ENABLE_LLVM_JIT
  digamma_t* m_digamma[COMPILE_THREAD_COUNT];
#endif
  scm_closure_t lookup_system_closure(const char* name);
  scm_obj_t lookup_current_environment(scm_symbol_t symbol);
  void intern_current_environment(scm_symbol_t symbol, scm_obj_t value);
  void prebind(scm_obj_t code);
  scm_obj_t backtrace_seek_body(scm_obj_t code);
  scm_obj_t backtrace_seek_tail(scm_obj_t code);
  void backtrace_seek();
  bool backtrace(scm_port_t port);
  void stop();
  void resolve();
  void* save_env(void* lnk);
  void update_cont(void* lnk);

 private:
  scm_obj_t prebind_literal(scm_obj_t literal);
  scm_gloc_t prebind_gloc(scm_obj_t variable);
  void prebind_list(scm_obj_t code);

  bool self_modifying(scm_gloc_t gloc, scm_obj_t code);
  int choose_codegen_thread();

  void backtrace_each(printer_t* prt, int n, scm_obj_t note);
  scm_obj_t backtrace_fetch(const char* name, int line, int column);
  void backtrace_seek_make_cont(scm_obj_t note);

  int live_stack_size();
  void* save_cont(void* lnk);
  void save_stack();

  void* gc_env(void* lnk);
  void* gc_cont(void* lnk);

  void record_trace(scm_obj_t comment);
  scm_obj_t* lookup_iloc(scm_obj_t operands);

  int apply_apply_closure(scm_obj_t lastarg);
  int apply_apply_subr(scm_obj_t lastarg);

  scm_obj_t call_scheme_stub(scm_obj_t proc, int argc, scm_obj_t argv[]);

 public:
  void collect_stack(intptr_t acquire);
  void apply_scheme(scm_obj_t proc, int argc, ...);
  void apply_scheme_argv(scm_obj_t proc, int argc, scm_obj_t argv[]);
  scm_obj_t call_scheme(scm_obj_t proc, int argc, ...);
  scm_obj_t call_scheme_argv(scm_obj_t proc, int argc, scm_obj_t argv[]);

#if !defined(NDEBUG) || STDEBUG
  void check_vm_env(void* lnk);
  void check_vm_cont(void* lnk);
  void check_vm_state();
#endif

#if PROFILE_OPCODE
  struct opcode_profile_t {
    int opcode;
    uint64_t count;
    uint64_t prev[VMOP_INSTRUCTION_COUNT];
  };
  opcode_profile_t m_opcode_profile[VMOP_INSTRUCTION_COUNT];
  static int comp_profile_rec(const void* a1, const void* a2);
  void clear_opcode_profile();
  void display_opcode_profile();
#endif

#if PROFILE_SUBR
  void clear_subr_profile();
  void display_subr_profile();
#endif

  static bool closure_is_not_compiled(scm_closure_t closure) { return closure->code == NULL && !HDR_CLOSURE_CODEGEN(closure->hdr); }

  static void mark_closure_compiling(scm_closure_t closure) { closure->hdr = closure->hdr | MAKEBITS(1, HDR_CLOSURE_CODEGEN_SHIFT); }

  static int instruction_to_opcode(scm_obj_t obj) {
    assert(OPCODESYMBOLP(obj));
    return HDR_SYMBOL_CODE(((scm_symbol_t)obj)->hdr);
  }

  scm_obj_t opcode_to_instruction(int opcode) {
    assert(opcode >= 0 && opcode < VMOP_INSTRUCTION_COUNT);
    return m_heap->inherent_symbol(opcode);
  }
} ATTRIBUTE(aligned(64));

#if defined(NO_TLS)

inline VM* current_vm() {
  extern pthread_key_t s_current_vm;
  return (VM*)pthread_getspecific(s_current_vm);
}

inline void set_current_vm(VM* vm) {
  extern pthread_key_t s_current_vm;
  MTVERIFY(pthread_setspecific(s_current_vm, vm));
}

#else

inline VM* current_vm() {
  extern __thread VM* s_current_vm;
  return s_current_vm;
}

inline void set_current_vm(VM* vm) {
  extern __thread VM* s_current_vm;
  s_current_vm = vm;
}

#endif

#endif
