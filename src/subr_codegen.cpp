// Copyright (c) 2004-2022 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "digamma.h"
#include "printer.h"
#include "violation.h"
#include "vm.h"

/*
 (current-environment (system-environment)) (native-compile)
*/

// native-compile
/*
scm_obj_t
subr_native_compile(VM* vm, int argc, scm_obj_t argv[])
{
//    orcjit_compile();
    return scm_unspecified;
}
*/
/*
 (current-environment (system-environment)) (define (foo) 120) (closure-compile foo)
*/

// closure-codegen
scm_obj_t subr_closure_codegen(VM* vm, int argc, scm_obj_t argv[]) {
#if ENABLE_LLVM_JIT
  if (argc == 1) {
    if (CLOSUREP(argv[0])) {
      scm_closure_t closure = (scm_closure_t)argv[0];
      if (vm->m_digamma[0]) {
        closure->code = NULL;
        vm->m_digamma[0]->m_debug = true;
        vm->m_digamma[0]->codegen_closure(closure);
        while (closure->code == NULL) usleep(1000);
        vm->m_digamma[0]->m_debug = false;
        return scm_unspecified;
      } else {
        implementation_restriction_violation(vm, "closure-codegen", "not available on this vm", scm_undef, argc, argv);
        return scm_undef;
      }
    }
    wrong_type_argument_violation(vm, "closure-codegen", 0, "closure", argv[0], argc, argv);
    return scm_undef;
  }
  wrong_number_of_arguments_violation(vm, "closure-codegen", 1, 1, argc, argv);
  return scm_undef;
#else
  implementation_restriction_violation(vm, "closure-codegen", "not available on this vm", scm_undef, argc, argv);
  return scm_undef;
#endif
}

// display-codegen-statistics
scm_obj_t subr_display_codegen_statistics(VM* vm, int argc, scm_obj_t argv[]) {
#if ENABLE_LLVM_JIT
  if (argc == 0) {
    for (int i = 0; i < COMPILE_THREAD_COUNT; i++) {
      if (vm->m_digamma[i]) {
        vm->m_digamma[i]->display_codegen_statistics(vm->m_current_output);
      }
    }
    return scm_unspecified;
  }
  wrong_number_of_arguments_violation(vm, "display-codegen-statistics", 0, 0, argc, argv);
  return scm_undef;
#else
  implementation_restriction_violation(vm, "display-codegen-statistics", "not available on this vm", scm_undef, argc, argv);
  return scm_undef;
#endif
}

// codegen-queue-count
scm_obj_t subr_codegen_queue_count(VM* vm, int argc, scm_obj_t argv[]) {
#if ENABLE_LLVM_JIT
  if (argc == 0) {
    int count = 0;
    for (int i = 0; i < COMPILE_THREAD_COUNT; i++) {
      if (vm->m_digamma[i]) {
        scoped_lock lock(vm->m_digamma[i]->m_codegen_queue_lock);
        count += vm->m_digamma[i]->m_codegen_queue.size();
      }
    }
    return MAKEFIXNUM(count);
  }
  wrong_number_of_arguments_violation(vm, "codegen-queue-count", 0, 0, argc, argv);
  return scm_undef;
#else
  implementation_restriction_violation(vm, "codegen-queue-count", "not available on this vm", scm_undef, argc, argv);
  return scm_undef;
#endif
}

// codegen-queue-push!
scm_obj_t subr_codegen_queue_push(VM* vm, int argc, scm_obj_t argv[]) {
#if ENABLE_LLVM_JIT
  if (argc == 1) {
    if (CLOSUREP(argv[0])) {
      scm_closure_t closure = (scm_closure_t)argv[0];
      if (VM::closure_is_not_compiled(closure)) {
        VM::mark_closure_compiling(closure);
        vm->m_digamma[0]->m_usage.on_demand++;
        vm->m_digamma[0]->codegen_closure(closure);
      }
      return scm_unspecified;
    }
    wrong_type_argument_violation(vm, "closure-queue-push!", 0, "closure", argv[0], argc, argv);
    return scm_undef;
  }
  wrong_number_of_arguments_violation(vm, "codegen-queue-push!", 1, 1, argc, argv);
  return scm_undef;
#else
  implementation_restriction_violation(vm, "codegen-queue-push!", "not available on this vm", scm_undef, argc, argv);
  return scm_undef;
#endif
}

void init_subr_codegen(object_heap_t* heap) {
#define DEFSUBR(SYM, FUNC) heap->intern_system_subr(SYM, FUNC)

  DEFSUBR("display-codegen-statistics", subr_display_codegen_statistics);
  DEFSUBR("codegen-queue-count", subr_codegen_queue_count);
  DEFSUBR("codegen-queue-push!", subr_codegen_queue_push);
  DEFSUBR("closure-codegen", subr_closure_codegen);
}
