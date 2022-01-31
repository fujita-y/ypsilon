// Copyright (c) 2004-2022 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "codegen.h"
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
      if (vm->m_codegen) {
        vm->m_codegen->m_debug = true;
        vm->m_codegen->compile(closure);
        while (closure->code == NULL) usleep(1000);
        vm->m_codegen->m_debug = false;
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
    if (vm->m_codegen) {
      vm->m_codegen->display_codegen_statistics(vm->m_current_output);
      return scm_unspecified;
    } else {
      implementation_restriction_violation(vm, "display-codegen-statistics", "codegen not available on this vm", scm_undef, argc, argv);
      return scm_undef;
    }
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
    if (vm->m_codegen) {
      return MAKEFIXNUM(vm->m_codegen->m_compile_queue.size());
    } else {
      implementation_restriction_violation(vm, "codegen-queue-count", "not available on this vm", scm_undef, argc, argv);
      return scm_undef;
    }
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
      if (vm->m_codegen) {
        if (!HDR_CLOSURE_INSPECTED(closure->hdr)) {
          closure->hdr = closure->hdr | MAKEBITS(1, HDR_CLOSURE_INSPECTED_SHIFT);
          vm->m_codegen->m_usage.on_demand++;
          vm->m_codegen->compile(closure);
        }
        return scm_unspecified;
      }
      implementation_restriction_violation(vm, "codegen-queue-push!", "codegen not available on this vm", scm_undef, argc, argv);
      return scm_undef;
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
