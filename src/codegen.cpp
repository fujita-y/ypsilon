// Copyright (c) 2004-2022 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "codegen.h"
#include "arith.h"
#include "port.h"
#include "printer.h"
#include "uuid.h"
#include "violation.h"

#include <llvm/Analysis/AliasAnalysis.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Support/Error.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/InitLLVM.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>

using namespace llvm;
using namespace llvm::orc;
using namespace llvm::sys;

#define DECLEAR_COMMON_TYPES                                                                  \
  auto IntptrTy = (sizeof(intptr_t) == 4 ? Type::getInt32Ty(C) : Type::getInt64Ty(C));        \
  auto IntptrPtrTy = sizeof(intptr_t) == 4 ? Type::getInt32PtrTy(C) : Type::getInt64PtrTy(C); \
  auto VoidTy = Type::getVoidTy(C);

#define DECLEAR_CONTEXT_VARS           \
  LLVMContext& C = ctx.m_llvm_context; \
  IRBuilder<>& IRB = ctx.m_irb;        \
  Module* M = ctx.m_module;            \
  Function* F = ctx.m_function;

#if INTPTR_MAX == INT32_MAX
  #define VALUE_INTPTR(_VAL_) IRB.getInt32((intptr_t)(_VAL_))
#elif INTPTR_MAX == INT64_MAX
  #define VALUE_INTPTR(_VAL_) IRB.getInt64((intptr_t)(_VAL_))
#else
  #error unsupported intptr_t size
#endif

#define CREATE_LOAD_VM_REG(_VM_, _REG_) \
  (IRB.CreateLoad(IntptrTy, IRB.CreateGEP(IntptrTy, _VM_, IRB.getInt32(offsetof(VM, _REG_) / sizeof(intptr_t)))))

#define CREATE_STORE_VM_REG(_VM_, _REG_, _VAL_) \
  (IRB.CreateStore(_VAL_, IRB.CreateGEP(IntptrTy, _VM_, IRB.getInt32(offsetof(VM, _REG_) / sizeof(intptr_t)))))

#define CREATE_LOAD_CONT_REC(_CONT_, _REC_) \
  (IRB.CreateLoad(IntptrTy, IRB.CreateGEP(IntptrTy, _CONT_, IRB.getInt32(offsetof(vm_cont_rec_t, _REC_) / sizeof(intptr_t)))))

#define CREATE_STORE_CONT_REC(_CONT_, _REC_, _VAL_) \
  (IRB.CreateStore(_VAL_, IRB.CreateGEP(IntptrTy, _CONT_, IRB.getInt32(offsetof(vm_cont_rec_t, _REC_) / sizeof(intptr_t)))))

#define CREATE_LOAD_GLOC_REC(_GLOC_, _REC_) \
  (IRB.CreateLoad(IntptrTy, IRB.CreateGEP(IntptrTy, _GLOC_, IRB.getInt32(offsetof(scm_gloc_rec_t, _REC_) / sizeof(intptr_t)))))

#define CREATE_LOAD_ENV_REC(_ENV_, _REC_) \
  (IRB.CreateLoad(IntptrTy, IRB.CreateGEP(IntptrTy, _ENV_, IRB.getInt32(offsetof(vm_env_rec_t, _REC_) / sizeof(intptr_t)))))

#define CREATE_STORE_ENV_REC(_ENV_, _REC_, _VAL_) \
  (IRB.CreateStore(_VAL_, IRB.CreateGEP(IntptrTy, _ENV_, IRB.getInt32(offsetof(vm_env_rec_t, _REC_) / sizeof(intptr_t)))))

#define CREATE_LEA_ENV_REC(_ENV_, _REC_) \
  (IRB.CreateBitOrPointerCast(IRB.CreateGEP(IntptrTy, _ENV_, IRB.getInt32(offsetof(vm_env_rec_t, _REC_) / sizeof(intptr_t))), IntptrTy))

#define CREATE_LOAD_PAIR_REC(_PAIR_, _REC_) \
  (IRB.CreateLoad(IntptrTy, IRB.CreateGEP(IntptrTy, _PAIR_, IRB.getInt32(offsetof(scm_pair_rec_t, _REC_) / sizeof(intptr_t)))))

#define CONS(a, d)    make_pair(vm->m_heap, (a), (d))
#define LIST1(e1)     CONS((e1), scm_nil)
#define LIST2(e1, e2) CONS((e1), LIST1((e2)))
#define STACKP(p)     (((p) >= (void*)vm->m_stack_top) & ((p) < (void*)vm->m_stack_limit))

extern "C" {
  void c_collect_stack(VM* vm, intptr_t acquire) { vm->collect_stack(acquire); }

  scm_obj_t* c_lookup_iloc(VM* vm, intptr_t depth, intptr_t index) {
    void* lnk = vm->m_env;
    intptr_t level = depth;
    while (level) {
      lnk = *(void**)lnk;
      level = level - 1;
    }
    vm_env_t env = (vm_env_t)((intptr_t)lnk - offsetof(vm_env_rec_t, up));
    return (scm_obj_t*)env - env->count + index;
  }

  vm_env_t c_lookup_env(VM* vm, intptr_t depth) {
    void* lnk = vm->m_env;
    intptr_t level = depth;
    while (level) {
      lnk = *(void**)lnk;
      level = level - 1;
    }
    vm_env_t env = (vm_env_t)((intptr_t)lnk - offsetof(vm_env_rec_t, up));
    return env;
  }

  void c_letrec_violation(VM* vm) { letrec_violation(vm); }

  void c_error_push_car_iloc(VM* vm, scm_obj_t obj) { wrong_type_argument_violation(vm, "car", 0, "pair", obj, 1, &obj); }

  void c_error_push_cdr_iloc(VM* vm, scm_obj_t obj) { wrong_type_argument_violation(vm, "cdr", 0, "pair", obj, 1, &obj); }

  void c_error_car_iloc(VM* vm, scm_obj_t obj) { wrong_type_argument_violation(vm, "car", 0, "pair", obj, 1, &obj); }

  void c_error_cdr_iloc(VM* vm, scm_obj_t obj) { wrong_type_argument_violation(vm, "cdr", 0, "pair", obj, 1, &obj); }

  void c_error_cadr_iloc(VM* vm, scm_obj_t obj) { wrong_type_argument_violation(vm, "cadr", 0, "appropriate list structure", obj, 1, &obj); }

  void c_error_push_cddr_iloc(VM* vm, scm_obj_t obj) {
    wrong_type_argument_violation(vm, "cddr", 0, "appropriate list structure", obj, 1, &obj);
  }

  void c_error_push_cadr_iloc(VM* vm, scm_obj_t obj) {
    wrong_type_argument_violation(vm, "cadr", 0, "appropriate list structure", obj, 1, &obj);
  }

  void c_error_push_nadd_iloc(VM* vm, scm_obj_t obj, scm_obj_t operands) {
    if (obj == scm_undef) letrec_violation(vm);
    scm_obj_t argv[2] = {obj, operands};
    wrong_type_argument_violation(vm, "operator(+ -)", 0, "number", argv[0], 2, argv);
  }

  void c_error_apply_gloc(VM* vm, scm_obj_t operands) { undefined_violation(vm, ((scm_gloc_t)operands)->variable, NULL); }

  void c_error_push_gloc(VM* vm, scm_obj_t operands) { undefined_violation(vm, ((scm_gloc_t)operands)->variable, NULL); }

  void c_error_gloc(VM* vm, scm_obj_t operands) { undefined_violation(vm, ((scm_gloc_t)operands)->variable, NULL); }

  scm_obj_t c_make_pair(VM* vm, scm_obj_t car, scm_obj_t cdr) { return make_pair(vm->m_heap, car, cdr); }

  scm_obj_t c_arith_add(VM* vm, scm_obj_t obj, scm_obj_t operands) { return arith_add(vm->m_heap, obj, operands); }

  scm_obj_t c_lt_n_iloc(VM* vm, scm_obj_t obj, scm_obj_t operands) {
    if (real_pred(obj)) {
      return n_compare(vm->m_heap, obj, operands) < 0 ? scm_true : scm_false;
    }
    scm_obj_t argv[2] = {obj, operands};
    wrong_type_argument_violation(vm, "comparison(< > <= >=)", 0, "real", argv[0], 2, argv);
    return NULL;
  }

  scm_obj_t c_gt_n_iloc(VM* vm, scm_obj_t obj, scm_obj_t operands) {
    if (real_pred(obj)) {
      return n_compare(vm->m_heap, obj, operands) > 0 ? scm_true : scm_false;
    }
    scm_obj_t argv[2] = {obj, operands};
    wrong_type_argument_violation(vm, "comparison(< > <= >=)", 0, "real", argv[0], 2, argv);
    return NULL;
  }

  scm_obj_t c_le_n_iloc(VM* vm, scm_obj_t obj, scm_obj_t operands) {
    if (real_pred(obj)) {
      return n_compare(vm->m_heap, obj, operands) <= 0 ? scm_true : scm_false;
    }
    scm_obj_t argv[2] = {obj, operands};
    wrong_type_argument_violation(vm, "comparison(< > <= >=)", 0, "real", argv[0], 2, argv);
    return NULL;
  }

  scm_obj_t c_ge_n_iloc(VM* vm, scm_obj_t obj, scm_obj_t operands) {
    if (real_pred(obj)) {
      return n_compare(vm->m_heap, obj, operands) >= 0 ? scm_true : scm_false;
    }
    scm_obj_t argv[2] = {obj, operands};
    wrong_type_argument_violation(vm, "comparison(< > <= >=)", 0, "real", argv[0], 2, argv);
    return NULL;
  }

  scm_obj_t c_eq_n_iloc(VM* vm, scm_obj_t obj, scm_obj_t operands) {
    if (real_pred(obj)) {
      return n_compare(vm->m_heap, obj, operands) == 0 ? scm_true : scm_false;
    }
    scm_obj_t argv[2] = {obj, operands};
    wrong_type_argument_violation(vm, "comparison(< > <= >=)", 0, "real", argv[0], 2, argv);
    return NULL;
  }

  scm_obj_t c_gt_iloc(VM* vm, scm_obj_t lhs, scm_obj_t rhs) {
    int bad = 0;
    if (real_pred(lhs)) {
      if (real_pred(rhs)) {
        return (n_compare(vm->m_heap, lhs, rhs) > 0) ? scm_true : scm_false;
      }
      bad = 1;
    }
    scm_obj_t argv[2] = {lhs, rhs};
    wrong_type_argument_violation(vm, "comparison(< > <= >=)", bad, "number", argv[bad], 2, argv);
    return NULL;
  }

  scm_obj_t c_lt_iloc(VM* vm, scm_obj_t lhs, scm_obj_t rhs) {
    int bad = 0;
    if (real_pred(lhs)) {
      if (real_pred(rhs)) {
        return (n_compare(vm->m_heap, lhs, rhs) < 0) ? scm_true : scm_false;
      }
      bad = 1;
    }
    scm_obj_t argv[2] = {lhs, rhs};
    wrong_type_argument_violation(vm, "comparison(< > <= >=)", bad, "number", argv[bad], 2, argv);
    return NULL;
  }

  scm_obj_t c_ge_iloc(VM* vm, scm_obj_t lhs, scm_obj_t rhs) {
    int bad = 0;
    if (real_pred(lhs)) {
      if (real_pred(rhs)) {
        return (n_compare(vm->m_heap, lhs, rhs) >= 0) ? scm_true : scm_false;
      }
      bad = 1;
    }
    scm_obj_t argv[2] = {lhs, rhs};
    wrong_type_argument_violation(vm, "comparison(< > <= >=)", bad, "number", argv[bad], 2, argv);
    return NULL;
  }

  scm_obj_t c_le_iloc(VM* vm, scm_obj_t lhs, scm_obj_t rhs) {
    int bad = 0;
    if (real_pred(lhs)) {
      if (real_pred(rhs)) {
        return (n_compare(vm->m_heap, lhs, rhs) <= 0) ? scm_true : scm_false;
      }
      bad = 1;
    }
    scm_obj_t argv[2] = {lhs, rhs};
    wrong_type_argument_violation(vm, "comparison(< > <= >=)", bad, "number", argv[bad], 2, argv);
    return NULL;
  }

  scm_obj_t c_eq_iloc(VM* vm, scm_obj_t lhs, scm_obj_t rhs) {
    int bad = 0;
    if (real_pred(lhs)) {
      if (real_pred(rhs)) {
        return (n_compare(vm->m_heap, lhs, rhs) == 0) ? scm_true : scm_false;
      }
      bad = 1;
    }
    scm_obj_t argv[2] = {lhs, rhs};
    wrong_type_argument_violation(vm, "comparison(< > <= >=)", bad, "number", argv[bad], 2, argv);
    return NULL;
  }

  intptr_t c_number_pred(scm_obj_t obj) { return (intptr_t)number_pred(obj); }

  intptr_t c_real_pred(scm_obj_t obj) { return (intptr_t)real_pred(obj); }

  intptr_t c_n_compare(VM* vm, scm_obj_t obj, scm_obj_t operands) { return n_compare(vm->m_heap, obj, operands); }

  void c_push_close(VM* vm, scm_closure_t operands) {
    if (STACKP(vm->m_env)) {
      vm->m_env = vm->save_env(vm->m_env);
      vm->update_cont(vm->m_cont);
    }
    vm->m_sp[0] = make_closure(vm->m_heap, operands, vm->m_env);
    vm->m_sp++;
  }

  void c_close(VM* vm, scm_closure_t operands) {
    if (STACKP(vm->m_env)) {
      vm->m_env = vm->save_env(vm->m_env);
      vm->update_cont(vm->m_cont);
    }
    vm->m_value = make_closure(vm->m_heap, operands, vm->m_env);
  }

  void c_ret_close(VM* vm, scm_closure_t operands) {
    if (STACKP(vm->m_env)) {
      vm->m_env = vm->save_env(vm->m_env);
      vm->update_cont(vm->m_cont);
    }
    vm->m_value = make_closure(vm->m_heap, operands, vm->m_env);
  }

  void c_set_gloc(VM* vm, scm_closure_t operands) {
    scm_gloc_t gloc = (scm_gloc_t)CAR(operands);
    assert(GLOCP(gloc));
    vm->m_heap->write_barrier(vm->m_value);
    gloc->value = vm->m_value;
  }

  void c_set_iloc(VM* vm, scm_closure_t operands) {
    scm_obj_t loc = CAR(operands);
    scm_obj_t* slot = c_lookup_iloc(vm, FIXNUM(CAR(loc)), FIXNUM(CDR(loc)));
    if (!STACKP(slot)) {
      vm->m_heap->write_barrier(vm->m_value);
    }
    *slot = vm->m_value;
  }

  void c_enclose(VM* vm, intptr_t argc) {
    vm_env_t env = (vm_env_t)((intptr_t)vm->m_env - offsetof(vm_env_rec_t, up));
    scm_obj_t* dst = (scm_obj_t*)env - env->count;
    if (STACKP(env)) {
      for (intptr_t i = 0; i < argc; i++) dst[i] = vm->m_fp[i];
    } else {
      for (intptr_t i = 0; i < argc; i++) {
        vm->m_heap->write_barrier(vm->m_fp[i]);
        dst[i] = vm->m_fp[i];
      }
    }
    vm->m_sp = vm->m_fp;
  }

  void c_extend_enclose(VM* vm, scm_obj_t operands) {
    vm->m_sp[0] = scm_undef;
    vm->m_sp++;
    vm_env_t env = (vm_env_t)vm->m_sp;
    env->count = 1;
    env->up = vm->m_env;
    vm->m_sp = vm->m_fp = (scm_obj_t*)(env + 1);
    vm->m_env = &env->up;
    vm->m_env = vm->save_env(vm->m_env);
    vm->update_cont(vm->m_cont);
    env = (vm_env_t)((intptr_t)vm->m_env - offsetof(vm_env_rec_t, up));
    scm_obj_t* slot = (scm_obj_t*)env - 1;
    scm_closure_t closure = make_closure(vm->m_heap, (scm_closure_t)operands, vm->m_env);
    vm->m_heap->write_barrier(closure);
    *slot = closure;
  }

  scm_obj_t c_nadd_iloc(VM* vm, scm_obj_t operands) {
    scm_obj_t loc = CAR(operands);
    scm_obj_t obj = *c_lookup_iloc(vm, FIXNUM(CAR(loc)), FIXNUM(CDR(loc)));
    if (number_pred(obj)) {
      return arith_add(vm->m_heap, obj, CADR(operands));
    }
    scm_obj_t argv[2] = {obj, CADR(operands)};
    wrong_type_argument_violation(vm, "operator(+ -)", 0, "number", argv[0], 2, argv);
    return NULL;
  }
}

static ExitOnError ExitOnErr;

template <int byte_offset> llvm::Value* codegen_t::reg_cache_t<byte_offset>::load(llvm::Value* vm) {
#if USE_REG_CACHE
  if (val) return val;
  val = IRB.CreateLoad(IntptrTy, IRB.CreateGEP(IntptrTy, vm, IRB.getInt32(byte_offset / sizeof(intptr_t))));
  need_write_back = false;
  return val;
#else
  return IRB.CreateLoad(IntptrTy, IRB.CreateGEP(IntptrTy, vm, IRB.getInt32(byte_offset / sizeof(intptr_t))));
#endif
}

template <int byte_offset> void codegen_t::reg_cache_t<byte_offset>::store(llvm::Value* vm, llvm::Value* rhs) {
#if USE_REG_CACHE
  need_write_back = true;
  val = rhs;
#else
  IRB.CreateStore(rhs, IRB.CreateGEP(IntptrTy, vm, IRB.getInt32(byte_offset / sizeof(intptr_t))));
#endif
}

template <int byte_offset> void codegen_t::reg_cache_t<byte_offset>::clear() {
#if USE_REG_CACHE
  val = NULL;
  need_write_back = false;
#endif
}

template <int byte_offset> void codegen_t::reg_cache_t<byte_offset>::copy(llvm::Value* vm) {
#if USE_REG_CACHE
  if (val && need_write_back) {
    IRB.CreateStore(val, IRB.CreateGEP(IntptrTy, vm, IRB.getInt32(byte_offset / sizeof(intptr_t))));
  }
#endif
}

template <int byte_offset> void codegen_t::reg_cache_t<byte_offset>::writeback(llvm::Value* vm) {
#if USE_REG_CACHE
  if (val && need_write_back) {
    IRB.CreateStore(val, IRB.CreateGEP(IntptrTy, vm, IRB.getInt32(byte_offset / sizeof(intptr_t))));
    need_write_back = false;
  }
#endif
}

template <int byte_offset>
codegen_t::reg_cache_t<byte_offset>::reg_cache_t(codegen_t::context_t* context)
    : val(NULL), need_write_back(false), C(context->m_llvm_context), IRB(context->m_irb) {
  IntptrTy = (sizeof(intptr_t) == 4 ? llvm::Type::getInt32Ty(C) : llvm::Type::getInt64Ty(C));
}

void codegen_t::context_t::reg_cache_clear() {
  reg_fp.clear();
  reg_env.clear();
  reg_cont.clear();
  reg_sp.clear();
  reg_value.clear();
}

void codegen_t::context_t::reg_cache_clear_only_value() { reg_value.clear(); }

void codegen_t::context_t::reg_cache_clear_only_sp() { reg_sp.clear(); }

void codegen_t::context_t::reg_cache_clear_only_env_and_value() {
  reg_env.clear();
  reg_value.clear();
}

void codegen_t::context_t::reg_cache_clear_only_env_and_sp() {
  reg_env.clear();
  reg_sp.clear();
}

void codegen_t::context_t::reg_cache_clear_except_value_and_cont() {
  reg_sp.clear();
  reg_fp.clear();
  reg_env.clear();
}

void codegen_t::context_t::reg_cache_copy(llvm::Value* vm) {
  reg_fp.copy(vm);
  reg_env.copy(vm);
  reg_cont.copy(vm);
  reg_sp.copy(vm);
  reg_value.copy(vm);
}

void codegen_t::context_t::reg_cache_copy_except_sp(llvm::Value* vm) {
  reg_fp.copy(vm);
  reg_env.copy(vm);
  reg_cont.copy(vm);
  reg_value.copy(vm);
}

void codegen_t::context_t::reg_cache_copy_except_value(llvm::Value* vm) {
  reg_fp.copy(vm);
  reg_env.copy(vm);
  reg_cont.copy(vm);
  reg_sp.copy(vm);
}

void codegen_t::context_t::reg_cache_copy_except_value_and_sp(llvm::Value* vm) {
  reg_fp.copy(vm);
  reg_env.copy(vm);
  reg_cont.copy(vm);
}

void codegen_t::context_t::reg_cache_copy_except_value_and_fp(llvm::Value* vm) {
  reg_sp.copy(vm);
  reg_env.copy(vm);
  reg_cont.copy(vm);
}

void codegen_t::context_t::reg_cache_copy_only_value(llvm::Value* vm) { reg_value.copy(vm); }

void codegen_t::context_t::reg_cache_copy_only_value_and_env(llvm::Value* vm) {
  reg_env.copy(vm);
  reg_value.copy(vm);
}

void codegen_t::context_t::reg_cache_copy_only_value_and_cont(llvm::Value* vm) {
  reg_cont.copy(vm);
  reg_value.copy(vm);
}

void codegen_t::context_t::reg_cache_copy_only_env_and_cont(llvm::Value* vm) {
  reg_cont.copy(vm);
  reg_env.copy(vm);
}

void codegen_t::context_t::reg_cache_copy_only_env_and_fp(llvm::Value* vm) {
  reg_fp.copy(vm);
  reg_env.copy(vm);
}

int codegen_t::calc_iloc_index(context_t& ctx, intptr_t depth, intptr_t index) { return ((depth - (ctx.m_depth - 1)) << 16) | (index & 0xffff); }

int codegen_t::calc_iloc_index(context_t& ctx, scm_obj_t operands) {
  return calc_iloc_index(ctx, FIXNUM(CAAR(operands)), (FIXNUM(CDAR(operands))));
}

void codegen_t::context_t::set_local_var_count(int depth, int count) {
  m_local_var_count.resize(depth + 1);
  m_local_var_count[depth] = count;
}

void codegen_t::context_t::set_local_var_count(int depth, scm_closure_t closure) {
  int argc = HDR_CLOSURE_ARGS(closure->hdr);
  if (argc < 0) argc = -argc;
  set_local_var_count(depth, argc);
}

int codegen_t::context_t::get_local_var_count(int depth) {
  if (m_depth - depth - 1 >= 0) return m_local_var_count[m_depth - depth - 1];
  return 0;
}

llvm::MDNode* codegen_t::context_t::get_branch_weight(int n, int m) {
  llvm::MDBuilder MDB(m_llvm_context);
  return MDB.createBranchWeights(n, m);
}

codegen_t::codegen_t() : m_debug(false) {}

void codegen_t::init() {
  m_compile_thread_terminating = false;
  m_compile_thread_lock.init();
  m_compile_thread_wake.init();
  m_compile_queue_lock.init();
  thread_start(compile_thread, this);
}

void codegen_t::destroy() {
  m_compile_thread_terminating = true;
  {
    scoped_lock lock(m_compile_thread_lock);
    m_compile_thread_wake.signal();
  }
  while (m_compile_thread_terminating) usleep(100);
  m_compile_thread_lock.destroy();
  m_compile_thread_wake.destroy();
  m_compile_queue_lock.destroy();
}

thread_main_t codegen_t::compile_thread(void* param) {
  codegen_t& codegen = *(codegen_t*)param;
  codegen.m_compile_thread_lock.lock();
  {
    auto J = ExitOnErr(LLJITBuilder().create());
    auto D = J->getDataLayout();
    auto G = ExitOnErr(orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(D.getGlobalPrefix()));
    J->getMainJITDylib().addGenerator(std::move(G));
    codegen.m_jit = std::move(J);
    codegen.m_compile_thread_ready = true;
    while (!codegen.m_compile_thread_terminating) {
      codegen.m_compile_thread_wake.wait(codegen.m_compile_thread_lock);
      codegen.m_compile_thread_ready = false;
      while (!codegen.m_compile_thread_terminating) {
        scm_closure_t closure = NULL;
        {
          scoped_lock lock(codegen.m_compile_queue_lock);
          if (codegen.m_compile_queue.size()) {
            closure = codegen.m_compile_queue.back();
          }
        }
        if (closure) codegen.compile_each(closure);
        {
          scoped_lock lock(codegen.m_compile_queue_lock);
          codegen.m_compile_queue.erase(std::remove(codegen.m_compile_queue.begin(), codegen.m_compile_queue.end(), closure),
                                        codegen.m_compile_queue.end());
          if (codegen.m_compile_queue.size() == 0) {
            codegen.m_compile_thread_ready = true;
          }
        }
        if (codegen.m_compile_thread_ready) break;
      }
    }
#if LLVM_VERSION_MAJOR >= 12
    ExitOnErr(codegen.m_jit->getExecutionSession().endSession());
#endif
  }
  codegen.m_compile_thread_terminating = false;
  codegen.m_compile_thread_lock.unlock();
  return NULL;
}

void codegen_t::optimizeModule(Module& M) {
  LoopAnalysisManager LAM;
  FunctionAnalysisManager FAM;
  CGSCCAnalysisManager CGAM;
  ModuleAnalysisManager MAM;
  PassBuilder PB;
  FAM.registerPass([&] { return PB.buildDefaultAAPipeline(); });
  PB.registerModuleAnalyses(MAM);
  PB.registerCGSCCAnalyses(CGAM);
  PB.registerFunctionAnalyses(FAM);
  PB.registerLoopAnalyses(LAM);
  PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);
  ModulePassManager MPM = PB.buildPerModuleDefaultPipeline(PassBuilder::OptimizationLevel::O2);
  MPM.run(M, MAM);
#if PRINT_IR
  puts(";*** IR after optimize ***");
  M.print(outs(), nullptr);
#else
  if (m_debug) {
    puts(";*** IR after optimize ***");
    M.print(outs(), nullptr);
  }
#endif
}

void codegen_t::compile(scm_closure_t closure) {
  if (m_compile_thread_terminating) return;
  {
    scoped_lock lock(m_compile_queue_lock);
    if (std::find(m_compile_queue.begin(), m_compile_queue.end(), closure) != m_compile_queue.end()) return;
    m_compile_queue.push_back(closure);
  }
  if (m_compile_thread_ready) {
    scoped_lock lock(m_compile_thread_lock);
    m_compile_thread_wake.signal();
  }
}

bool codegen_t::maybe_compile(scm_closure_t closure) {
  if (closure->code == NULL && !HDR_CLOSURE_INSPECTED(closure->hdr)) {
    scoped_lock lock(m_compile_queue_lock);
    closure->hdr = closure->hdr | MAKEBITS(1, HDR_CLOSURE_INSPECTED_SHIFT);
    m_compile_queue.push_back(closure);
    return true;
  }
  return false;
}

void codegen_t::compile_each(scm_closure_t closure) {
  if (closure->code != NULL) return;
#if VERBOSE_CODEGEN
  printer_t prt(vm, vm->m_current_output);
  prt.format("generating native code: ~s~&", closure->doc);
#endif
  char module_id[40];
  uuid_v4(module_id, sizeof(module_id));
  char function_id[40];
  uuid_v4(function_id, sizeof(function_id));

  auto Context = std::make_unique<LLVMContext>();
  LLVMContext& C = *Context;
  DECLEAR_COMMON_TYPES;

  auto M = std::make_unique<Module>(module_id, C);
  M->setTargetTriple(getDefaultTargetTriple());
  M->setDataLayout(m_jit->getDataLayout());
  Function* F = Function::Create(FunctionType::get(IntptrTy, {IntptrPtrTy}, false), Function::ExternalLinkage, function_id, M.get());
#if USE_LLVM_ATTRIBUTES
  F->addFnAttr(Attribute::ArgMemOnly);
  F->addFnAttr(Attribute::NoUnwind);
  F->addFnAttr(Attribute::WillReturn);
  for (Argument& argument : F->args()) {
    argument.addAttr(Attribute::NoAlias);
    argument.addAttr(Attribute::NoCapture);
    argument.addAttr(Attribute::NoFree);
  }
#endif
  BasicBlock* ENTRY = BasicBlock::Create(C, "entry", F);
  IRBuilder<> IRB(ENTRY);
  {
    context_t context(C, IRB);
    context.m_module = M.get();
    context.m_function = F;
    context.m_top_level_closure = closure;
    context.m_top_level_function = F;
    context.set_local_var_count(0, closure);
    context.m_depth = 1;
    transform(context, closure->pc, true);
  }
  if (verifyModule(*M, &outs())) fatal("%s:%u verify module failed", __FILE__, __LINE__);
#if USE_LLVM_OPTIMIZE
  optimizeModule(*M);
#endif
  ExitOnErr(m_jit->addIRModule(std::move(ThreadSafeModule(std::move(M), std::move(Context)))));
  // m_jit->getMainJITDylib().dump(llvm::outs());

  auto symbol = ExitOnErr(m_jit->lookup(function_id));
  intptr_t (*thunk)(intptr_t) = (intptr_t(*)(intptr_t))symbol.getAddress();

  if (m_usage.min_sym == 0 || m_usage.min_sym > (uintptr_t)thunk) m_usage.min_sym = (uintptr_t)thunk;
  if (m_usage.max_sym < (uintptr_t)thunk) m_usage.max_sym = (uintptr_t)thunk;

  closure->code = (void*)thunk;
  m_lifted_functions.clear();
}

Value* codegen_t::get_function_address(context_t& ctx, scm_closure_t closure) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;

  if (closure->code == NULL) fatal("%s:%u closure is not compiled", __FILE__, __LINE__);
  intptr_t (*adrs)(intptr_t) = (intptr_t(*)(intptr_t))(closure->code);
  auto subrType = FunctionType::get(IntptrTy, {IntptrPtrTy}, false);
  return ConstantExpr::getIntToPtr(VALUE_INTPTR(adrs), subrType->getPointerTo());
}

int codegen_t::calc_stack_size(scm_obj_t inst) {
  int require = 0;
  int n = 0;
  while (inst != scm_nil) {
    switch (VM::instruction_to_opcode(CAAR(inst))) {
      case VMOP_IF_FALSE_CALL: {
        scm_obj_t operands = CDAR(inst);
        int n2 = calc_stack_size(operands);
        if (n + n2 > require) require = n + n2;
      } break;
      case VMOP_CALL: {
        n += sizeof(vm_cont_rec_t);
        scm_obj_t operands = CDAR(inst);
        int n2 = calc_stack_size(operands);
        if (n + n2 > require) require = n + n2;
      } break;
      case VMOP_PUSH_GLOC:
      case VMOP_PUSH_SUBR:
      case VMOP_PUSH_SUBR_GLOC:
      case VMOP_PUSH_CAR_ILOC:
      case VMOP_PUSH_CDR_ILOC:
      case VMOP_PUSH_ILOC0:
      case VMOP_PUSH_ILOC:
      case VMOP_PUSH:
      case VMOP_PUSH_CONST:
      case VMOP_PUSH_ILOC1: {
        n += sizeof(scm_obj_t);
      } break;
      case VMOP_APPLY_GLOC: {
        n += sizeof(vm_env_rec_t);
      } break;
      case VMOP_APPLY_ILOC: {
        n += sizeof(vm_env_rec_t);
      } break;
      case VMOP_APPLY_ILOC_LOCAL: {
        n += sizeof(vm_env_rec_t);
      } break;
      case VMOP_APPLY: {
        n += sizeof(vm_env_rec_t);
      } break;
      case VMOP_EXTEND: {
        n += sizeof(vm_env_rec_t);
      } break;
      case VMOP_EXTEND_ENCLOSE: {
        n += sizeof(scm_obj_t);
        n += sizeof(vm_env_rec_t);
      } break;
      case VMOP_EXTEND_ENCLOSE_LOCAL: {
        n += sizeof(scm_obj_t);
        n += sizeof(vm_env_rec_t);
      } break;
      case VMOP_EXTEND_UNBOUND: {
        scm_obj_t operands = CDAR(inst);
        int argc = FIXNUM(operands);
        n += sizeof(scm_obj_t) * argc;
        n += sizeof(vm_env_rec_t);
      } break;
      case VMOP_PUSH_CLOSE: {
        n += sizeof(scm_obj_t);
      } break;
      case VMOP_PUSH_CLOSE_LOCAL: {
        n += sizeof(scm_obj_t);
      } break;
      case VMOP_IF_TRUE:
      case VMOP_IF_EQP:
      case VMOP_IF_NULLP:
      case VMOP_IF_PAIRP:
      case VMOP_IF_SYMBOLP: {
        scm_obj_t operands = CDAR(inst);
        int n2 = calc_stack_size(operands);
        if (n + n2 > require) require = n + n2;
      } break;
      case VMOP_PUSH_NADD_ILOC:
      case VMOP_PUSH_CADR_ILOC:
      case VMOP_PUSH_CDDR_ILOC: {
        n += sizeof(scm_obj_t);
      } break;
      default:
        break;
    }
    if (n > require) require = n;
    inst = CDR(inst);
  }
  return require;
}

void codegen_t::transform(context_t ctx, scm_obj_t inst, bool insert_stack_check) {
  if (insert_stack_check) emit_stack_overflow_check(ctx, calc_stack_size(inst));
  while (inst != scm_nil) {
    switch (VM::instruction_to_opcode(CAAR(inst))) {
      case VMOP_IF_FALSE_CALL: {
        emit_if_false_call(ctx, inst);
      } break;
      case VMOP_CALL: {
        ctx.m_function = emit_call(ctx, inst);
      } break;
      case VMOP_RET_GLOC: {
        emit_ret_gloc(ctx, inst);
      } break;
      case VMOP_RET_CONST: {
        emit_ret_const(ctx, inst);
      } break;
      case VMOP_RET_ILOC: {
        emit_ret_iloc(ctx, inst);
      } break;
      case VMOP_PUSH_GLOC: {
        emit_push_gloc(ctx, inst);
        ctx.m_argc++;
      } break;
      case VMOP_PUSH_SUBR: {
        emit_push_subr(ctx, inst);
        intptr_t argc = FIXNUM(CADR(CDAR(inst)));
        ctx.m_argc = ctx.m_argc - argc + 1;
      } break;
      case VMOP_PUSH_CAR_ILOC: {
        emit_push_car_iloc(ctx, inst);
        ctx.m_argc++;
      } break;
      case VMOP_PUSH_CDR_ILOC: {
        emit_push_cdr_iloc(ctx, inst);
        ctx.m_argc++;
      } break;
      case VMOP_PUSH_ILOC0: {
        emit_push_iloc0(ctx, inst);
        ctx.m_argc++;
      } break;
      case VMOP_PUSH_ILOC: {
        emit_push_iloc(ctx, inst);
        ctx.m_argc++;
      } break;
      case VMOP_PUSH: {
        emit_push(ctx, inst);
        ctx.m_argc++;
      } break;
      case VMOP_PUSH_CONST: {
        emit_push_const(ctx, inst);
        ctx.m_argc++;
      } break;
      case VMOP_PUSH_ILOC1: {
        emit_push_iloc1(ctx, inst);
        ctx.m_argc++;
      } break;
      case VMOP_APPLY_GLOC: {
        emit_apply_gloc(ctx, inst);
      } break;
      case VMOP_RET_SUBR: {
        emit_ret_subr(ctx, inst);
      } break;
      case VMOP_APPLY_ILOC: {
        emit_apply_iloc(ctx, inst);
      } break;
      case VMOP_APPLY_ILOC_LOCAL: {
        emit_apply_iloc_local(ctx, inst);
      } break;
      case VMOP_APPLY: {
        emit_apply(ctx, inst);
      } break;
      case VMOP_EXTEND: {
        emit_extend(ctx, inst);
        ctx.m_argc = 0;
        ctx.m_depth++;
      } break;
      case VMOP_EXTEND_ENCLOSE: {
        emit_extend_enclose(ctx, inst);
        ctx.m_argc = 0;
        ctx.m_depth++;
      } break;
      case VMOP_EXTEND_ENCLOSE_LOCAL: {
        emit_extend_enclose_local(ctx, inst);
        ctx.m_argc = 0;
        ctx.m_depth++;
      } break;
      case VMOP_EXTEND_UNBOUND: {
        emit_extend_unbound(ctx, inst);
        ctx.m_argc = 0;
        ctx.m_depth++;
      } break;
      case VMOP_PUSH_CLOSE: {
        emit_push_close(ctx, inst);
        ctx.m_argc++;
      } break;
      case VMOP_PUSH_CLOSE_LOCAL: {
        emit_push_close_local(ctx, inst);
        ctx.m_argc++;
      } break;
      case VMOP_ENCLOSE: {
        emit_enclose(ctx, inst);
        ctx.m_argc = 0;
      } break;
      case VMOP_GLOC: {
        emit_gloc(ctx, inst);
      } break;
      case VMOP_ILOC: {
        emit_iloc(ctx, inst);
      } break;
      case VMOP_CAR_ILOC: {
        emit_car_iloc(ctx, inst);
      } break;
      case VMOP_CDR_ILOC: {
        emit_cdr_iloc(ctx, inst);
      } break;
      case VMOP_CONST: {
        emit_const(ctx, inst);
      } break;
      case VMOP_SUBR: {
        emit_subr(ctx, inst);
        intptr_t argc = FIXNUM(CADR(CDAR(inst)));
        ctx.m_argc = ctx.m_argc - argc;
      } break;
      case VMOP_ILOC1: {
        emit_iloc1(ctx, inst);
      } break;
      case VMOP_ILOC0: {
        emit_iloc0(ctx, inst);
      } break;
      case VMOP_IF_TRUE: {
        emit_if_true(ctx, inst);
      } break;
      case VMOP_IF_NULLP_RET_CONST: {
        emit_if_nullp_ret_const(ctx, inst);
      } break;
      case VMOP_IF_EQP: {
        ctx.m_argc--;
        emit_if_eqp(ctx, inst);
      } break;
      case VMOP_IF_NULLP: {
        emit_if_nullp(ctx, inst);
      } break;
      case VMOP_IF_PAIRP: {
        emit_if_pairp(ctx, inst);
      } break;
      case VMOP_IF_SYMBOLP: {
        emit_if_symbolp(ctx, inst);
      } break;
      case VMOP_IF_TRUE_RET: {
        emit_if_true_ret(ctx, inst);
      } break;
      case VMOP_IF_FALSE_RET: {
        emit_if_false_ret(ctx, inst);
      } break;
      case VMOP_IF_TRUE_RET_CONST: {
        emit_if_true_ret_const(ctx, inst);
      } break;
      case VMOP_IF_FALSE_RET_CONST: {
        emit_if_false_ret_const(ctx, inst);
      } break;
      case VMOP_IF_EQP_RET_CONST: {
        ctx.m_argc--;
        emit_if_eqp_ret_const(ctx, inst);
      } break;
      case VMOP_IF_PAIRP_RET_CONST: {
        emit_if_pairp_ret_const(ctx, inst);
      } break;
      case VMOP_IF_SYMBOLP_RET_CONST: {
        emit_if_symbolp_ret_const(ctx, inst);
      } break;
      case VMOP_IF_NOT_PAIRP_RET_CONST: {
        emit_if_not_pairp_ret_const(ctx, inst);
      } break;
      case VMOP_IF_NOT_NULLP_RET_CONST: {
        emit_if_not_nullp_ret_const(ctx, inst);
      } break;
      case VMOP_IF_NOT_EQP_RET_CONST: {
        ctx.m_argc--;
        emit_if_not_eqp_ret_const(ctx, inst);
      } break;
      case VMOP_IF_NOT_SYMBOLP_RET_CONST: {
        emit_if_not_symbolp_ret_const(ctx, inst);
      } break;
      case VMOP_CLOSE: {
        emit_close(ctx, inst);
      } break;
      case VMOP_SET_GLOC: {
        emit_set_gloc(ctx, inst);
      } break;
      case VMOP_SET_ILOC: {
        emit_set_iloc(ctx, inst);
      } break;
      case VMOP_PUSH_CONS: {
        emit_push_cons(ctx, inst);
      } break;
      case VMOP_RET_CONS: {
        emit_ret_cons(ctx, inst);
      } break;
      case VMOP_RET_EQP: {
        emit_ret_eqp(ctx, inst);
      } break;
      case VMOP_RET_NULLP: {
        emit_ret_nullp(ctx, inst);
      } break;
      case VMOP_RET_PAIRP: {
        emit_ret_pairp(ctx, inst);
      } break;
      case VMOP_RET_CLOSE: {
        emit_ret_close(ctx, inst);
      } break;
      case VMOP_PUSH_NADD_ILOC: {
        emit_push_nadd_iloc(ctx, inst);
        ctx.m_argc++;
      } break;
      case VMOP_PUSH_CADR_ILOC: {
        emit_push_cadr_iloc(ctx, inst);
        ctx.m_argc++;
      } break;
      case VMOP_PUSH_CDDR_ILOC: {
        emit_push_cddr_iloc(ctx, inst);
        ctx.m_argc++;
      } break;
      case VMOP_CADR_ILOC: {
        emit_cadr_iloc(ctx, inst);
      } break;
      case VMOP_CDDR_ILOC: {
        emit_cddr_iloc(ctx, inst);
      } break;
      case VMOP_EQ_N_ILOC: {
        emit_eq_n_iloc(ctx, inst);
      } break;
      case VMOP_LT_N_ILOC: {
        emit_lt_n_iloc(ctx, inst);
      } break;
      case VMOP_GE_N_ILOC: {
        emit_ge_n_iloc(ctx, inst);
      } break;
      case VMOP_LE_N_ILOC: {
        emit_le_n_iloc(ctx, inst);
      } break;
      case VMOP_GT_N_ILOC: {
        emit_gt_n_iloc(ctx, inst);
      } break;
      case VMOP_NADD_ILOC: {
        emit_nadd_iloc(ctx, inst);
      } break;
      case VMOP_EQ_ILOC: {
        emit_eq_iloc(ctx, inst);
      } break;
      case VMOP_LT_ILOC: {
        emit_lt_iloc(ctx, inst);
      } break;
      case VMOP_LE_ILOC: {
        emit_le_iloc(ctx, inst);
      } break;
      case VMOP_GT_ILOC: {
        emit_gt_iloc(ctx, inst);
      } break;
      case VMOP_GE_ILOC: {
        emit_ge_iloc(ctx, inst);
      } break;
      case VMOP_SUBR_GLOC: {
        emit_subr_gloc(ctx, inst);
        intptr_t argc = FIXNUM(CADR(CDAR(inst)));
        ctx.m_argc = ctx.m_argc - argc;
      } break;
      case VMOP_PUSH_SUBR_GLOC: {
        emit_push_subr_gloc(ctx, inst);
        intptr_t argc = FIXNUM(CADR(CDAR(inst)));
        ctx.m_argc = ctx.m_argc - argc + 1;
      } break;
      case VMOP_RET_SUBR_GLOC: {
        emit_ret_subr_gloc(ctx, inst);
      } break;
      case VMOP_VM_ESCAPE: {
        emit_escape(ctx, inst);
      } break;
      case VMOP_TOUCH_GLOC:
        break;
      default:
        fatal("%s:%u encounter unsupported instruction %s", __FILE__, __LINE__, ((scm_symbol_t)CAAR(inst))->name);
        break;
    }
    inst = CDR(inst);
  }
}

void codegen_t::display_codegen_statistics(scm_port_t port) {
  scoped_lock lock(port->lock);
  port_put_byte(port, '\n');
  port_format(port, "top-level apply interned : %d\n", m_usage.globals);
  port_format(port, "top-level apply lifted   : %d\n", m_usage.inners);
  port_format(port, "top-level reference      : %d\n", m_usage.refs);
  port_format(port, "closure template         : %d\n", m_usage.templates);
  port_format(port, "local loop               : %d\n", m_usage.locals);
  port_format(port, "explicit                 : %d\n", m_usage.on_demand);
  port_format(port, "skipped                  : %d\n", m_usage.skipped);
  port_format(port, "native code location     : %lx - %lx\n\n", m_usage.min_sym, m_usage.max_sym);
  port_flush_output(port);
}

llvm::AllocaInst* codegen_t::emit_alloca(context_t& ctx, llvm::Type* type) {
  DECLEAR_CONTEXT_VARS;
  IRBuilder<> TB(&F->getEntryBlock(), F->getEntryBlock().begin());
  return TB.CreateAlloca(type);
}

Function* codegen_t::emit_inner_function(context_t& ctx, scm_closure_t closure) {
  char function_id[40];
  uuid_v4(function_id, sizeof(function_id));

  LLVMContext& C = ctx.m_llvm_context;
  Module* M = ctx.m_module;

  DECLEAR_COMMON_TYPES;
  Function* F = Function::Create(FunctionType::get(IntptrTy, {IntptrPtrTy}, false), Function::PrivateLinkage, function_id, M);
#if USE_LLVM_ATTRIBUTES
  F->addFnAttr(Attribute::ArgMemOnly);
  F->addFnAttr(Attribute::NoUnwind);
  F->addFnAttr(Attribute::WillReturn);
  for (Argument& argument : F->args()) {
    argument.addAttr(Attribute::NoAlias);
    argument.addAttr(Attribute::NoCapture);
    argument.addAttr(Attribute::NoFree);
  }
#endif

  BasicBlock* ENTRY = BasicBlock::Create(C, "entry", F);
  IRBuilder<> IRB(ENTRY);

  m_lifted_functions.insert({closure, F});

  context_t context(C, IRB);
  context.m_module = M;
  context.m_function = F;
  context.m_top_level_closure = closure;
  context.m_top_level_function = F;
  context.set_local_var_count(0, closure);
  context.m_depth = 1;

  transform(context, closure->pc, true);

  return F;
}

void codegen_t::emit_stack_overflow_check(context_t& ctx, int nbytes) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  auto vm = F->arg_begin();

  if (nbytes == 0) return;
  if (nbytes >= VM_STACK_BYTESIZE) fatal("%s:%u vm stack size too small", __FILE__, __LINE__);

  auto stack_limit = CREATE_LOAD_VM_REG(vm, m_stack_limit);
  BasicBlock* stack_ok = BasicBlock::Create(C, "stack_ok", F);
  BasicBlock* stack_overflow = BasicBlock::Create(C, "stack_overflow", F);
  Value* stack_cond = IRB.CreateICmpULT(IRB.CreateAdd(CREATE_LOAD_VM_REG(vm, m_sp), VALUE_INTPTR(nbytes)), stack_limit);
  IRB.CreateCondBr(stack_cond, stack_ok, stack_overflow, ctx.likely_true);

  IRB.SetInsertPoint(stack_overflow);
  auto thunkType = FunctionType::get(VoidTy, {IntptrPtrTy, IntptrTy}, false);
  auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_collect_stack), thunkType->getPointerTo());
  IRB.CreateCall(thunkType, thunk, {vm, VALUE_INTPTR(nbytes)});

  IRB.CreateBr(stack_ok);

  IRB.SetInsertPoint(stack_ok);

  ctx.reg_cache_clear();
  ctx.m_iloc_cache.clear();
}

Value* codegen_t::emit_lookup_env(context_t& ctx, intptr_t depth) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  auto vm = F->arg_begin();

  if (depth > 4) {
    ctx.reg_env.writeback(vm);
    auto thunkType = FunctionType::get(IntptrPtrTy, {IntptrPtrTy, IntptrTy}, false);
    auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_lookup_env), thunkType->getPointerTo());
    return IRB.CreateCall(thunkType, thunk, {vm, VALUE_INTPTR(depth)});
  }
  Value* target;
  auto env0 = ctx.reg_env.load(vm);
  if (depth == 0) {
    target = env0;
  } else {
    target = IRB.CreateLoad(IntptrTy, IRB.CreateBitOrPointerCast(env0, IntptrPtrTy));
    for (int i = 1; i < depth; i++) {
      target = IRB.CreateLoad(IntptrTy, IRB.CreateBitOrPointerCast(target, IntptrPtrTy));
    }
  }
  auto env1 =
      IRB.CreateGEP(IntptrTy, IRB.CreateBitOrPointerCast(target, IntptrPtrTy), VALUE_INTPTR(-offsetof(vm_env_rec_t, up) / sizeof(intptr_t)));
  return IRB.CreateBitOrPointerCast(env1, IntptrPtrTy);
}

Value* codegen_t::emit_lookup_iloc(context_t& ctx, intptr_t depth, intptr_t index) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  auto vm = F->arg_begin();

  if (depth <= 4) {
#if USE_ILOC_OPTIMIZE
    int n = ctx.get_local_var_count(depth);
    if (n) {
      auto env = emit_lookup_env(ctx, depth);
      return IRB.CreateGEP(IntptrTy, env, VALUE_INTPTR(index - n));
    }
    auto env = emit_lookup_env(ctx, depth);
    auto count = CREATE_LOAD_ENV_REC(env, count);
    return IRB.CreateGEP(IntptrTy, env, IRB.CreateSub(VALUE_INTPTR(index), count));
#else
    auto env = emit_lookup_env(ctx, depth);
    auto count = CREATE_LOAD_ENV_REC(env, count);
    if (index == 0) return IRB.CreateGEP(IntptrTy, env, IRB.CreateNeg(count));
    return IRB.CreateGEP(IntptrTy, env, IRB.CreateSub(VALUE_INTPTR(index), count));
#endif
  }
  ctx.reg_env.writeback(vm);
  auto thunkType = FunctionType::get(IntptrPtrTy, {IntptrPtrTy, IntptrTy, IntptrTy}, false);
  auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_lookup_iloc), thunkType->getPointerTo());
  return IRB.CreateCall(thunkType, thunk, {vm, VALUE_INTPTR(depth), VALUE_INTPTR(index)});
}

Value* codegen_t::emit_lookup_iloc(context_t& ctx, scm_obj_t loc) { return emit_lookup_iloc(ctx, FIXNUM(CAR(loc)), FIXNUM(CDR(loc))); }

void codegen_t::emit_push_vm_stack(context_t& ctx, Value* val) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  auto vm = F->arg_begin();

  auto sp = ctx.reg_sp.load(vm);
  IRB.CreateStore(val, IRB.CreateBitOrPointerCast(sp, IntptrPtrTy));
  auto ea0 = IRB.CreateGEP(IntptrTy, IRB.CreateBitOrPointerCast(sp, IntptrPtrTy), VALUE_INTPTR(1));
  auto ea1 = IRB.CreateBitOrPointerCast(ea0, IntptrTy);
  ctx.reg_sp.store(vm, ea1);
}

void codegen_t::emit_prepair_apply(context_t& ctx, scm_closure_t closure) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  auto vm = F->arg_begin();

  intptr_t argc = HDR_CLOSURE_ARGS(closure->hdr);
  auto env = IRB.CreateBitOrPointerCast(ctx.reg_sp.load(vm), IntptrPtrTy);
  CREATE_STORE_ENV_REC(env, count, VALUE_INTPTR(argc));
  CREATE_STORE_ENV_REC(env, up, VALUE_INTPTR(closure->env));
  auto ea0 = IRB.CreateGEP(IntptrTy, IRB.CreateBitOrPointerCast(env, IntptrPtrTy), VALUE_INTPTR(sizeof(vm_env_rec_t) / sizeof(intptr_t)));
  auto ea1 = IRB.CreateBitOrPointerCast(ea0, IntptrTy);
  CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(closure->pc));
  ctx.reg_env.store(vm, CREATE_LEA_ENV_REC(env, up));
  ctx.reg_fp.store(vm, ea1);
  ctx.reg_sp.store(vm, ea1);
}

void codegen_t::emit_push(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  auto vm = F->arg_begin();

  emit_push_vm_stack(ctx, ctx.reg_value.load(vm));
}

void codegen_t::emit_push_const(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);

  emit_push_vm_stack(ctx, VALUE_INTPTR(operands));
}

void codegen_t::emit_push_iloc0(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);

  emit_push_vm_stack(ctx, IRB.CreateLoad(IntptrTy, emit_lookup_iloc(ctx, 0, FIXNUM(operands))));
}

void codegen_t::emit_push_iloc1(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);

  emit_push_vm_stack(ctx, IRB.CreateLoad(IntptrTy, emit_lookup_iloc(ctx, 1, FIXNUM(operands))));
}

void codegen_t::emit_push_gloc(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

#if ENABLE_COMPILE_REFERENCE
  scm_obj_t obj = ((scm_gloc_t)operands)->value;
  if (CLOSUREP(obj)) {
    if (maybe_compile((scm_closure_t)obj)) m_usage.refs++;
  }
#endif

  auto gloc = IRB.CreateBitOrPointerCast(VALUE_INTPTR(operands), IntptrPtrTy);
  auto val = CREATE_LOAD_GLOC_REC(gloc, value);
  if (((scm_gloc_t)operands)->value == scm_undef) {
    BasicBlock* undef_true = BasicBlock::Create(C, "undef_ture", F);
    BasicBlock* CONTINUE = BasicBlock::Create(C, "continue", F);
    auto undef_cond = IRB.CreateICmpEQ(val, VALUE_INTPTR(scm_undef));
    IRB.CreateCondBr(undef_cond, undef_true, CONTINUE, ctx.likely_false);
    IRB.SetInsertPoint(undef_true);
    ctx.reg_cache_copy(vm);
    CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(inst));
    auto thunkType = FunctionType::get(VoidTy, {IntptrPtrTy, IntptrTy}, false);
    auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_error_push_gloc), thunkType->getPointerTo());
    IRB.CreateCall(thunkType, thunk, {vm, VALUE_INTPTR(operands)});
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_resume_loop));
    IRB.SetInsertPoint(CONTINUE);
  }
  emit_push_vm_stack(ctx, val);
}

void codegen_t::emit_push_car_iloc(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

  auto pair = IRB.CreateLoad(IntptrTy, emit_lookup_iloc(ctx, CAR(operands)));

  // check if pair
  BasicBlock* pair_true = BasicBlock::Create(C, "pair_true", F);
  BasicBlock* pair_false = BasicBlock::Create(C, "pair_false", F);
  emit_cond_pairp(ctx, pair, pair_true, pair_false);

  // nonpair
  IRB.SetInsertPoint(pair_false);
  ctx.reg_cache_copy(vm);
  CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(inst));
  auto thunkType = FunctionType::get(VoidTy, {IntptrPtrTy, IntptrTy}, false);
  auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_error_push_car_iloc), thunkType->getPointerTo());
  IRB.CreateCall(thunkType, thunk, {vm, pair});
  IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_resume_loop));

  // pair
  IRB.SetInsertPoint(pair_true);
  emit_push_vm_stack(ctx, CREATE_LOAD_PAIR_REC(IRB.CreateBitOrPointerCast(pair, IntptrPtrTy), car));
}

void codegen_t::emit_push_cdr_iloc(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

  auto pair = IRB.CreateLoad(IntptrTy, emit_lookup_iloc(ctx, CAR(operands)));

  // check if pair
  BasicBlock* pair_true = BasicBlock::Create(C, "pair_true", F);
  BasicBlock* pair_false = BasicBlock::Create(C, "pair_false", F);
  emit_cond_pairp(ctx, pair, pair_true, pair_false);

  // nonpair
  IRB.SetInsertPoint(pair_false);
  ctx.reg_cache_copy(vm);
  CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(inst));
  auto thunkType = FunctionType::get(VoidTy, {IntptrPtrTy, IntptrTy}, false);
  auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_error_push_cdr_iloc), thunkType->getPointerTo());
  IRB.CreateCall(thunkType, thunk, {vm, pair});
  IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_resume_loop));

  // pair
  IRB.SetInsertPoint(pair_true);
  emit_push_vm_stack(ctx, CREATE_LOAD_PAIR_REC(IRB.CreateBitOrPointerCast(pair, IntptrPtrTy), cdr));
}

void codegen_t::emit_push_cddr_iloc(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

  auto pair = IRB.CreateLoad(IntptrTy, emit_lookup_iloc(ctx, CAR(operands)));

  // check if pair
  BasicBlock* pair_true = BasicBlock::Create(C, "pair_true", F);
  BasicBlock* pair_false = BasicBlock::Create(C, "pair_false", F);
  emit_cond_pairp(ctx, pair, pair_true, pair_false);

  // nonpair
  IRB.SetInsertPoint(pair_false);
  ctx.reg_cache_copy(vm);
  CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(inst));
  auto thunkType = FunctionType::get(VoidTy, {IntptrPtrTy, IntptrTy}, false);
  auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_error_push_cddr_iloc), thunkType->getPointerTo());
  IRB.CreateCall(thunkType, thunk, {vm, pair});
  IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_resume_loop));

  // pair
  IRB.SetInsertPoint(pair_true);
  auto pair2 = CREATE_LOAD_PAIR_REC(IRB.CreateBitOrPointerCast(pair, IntptrPtrTy), cdr);
  BasicBlock* pair2_true = BasicBlock::Create(C, "pair2_true", F);
  emit_cond_pairp(ctx, pair2, pair2_true, pair_false);

  // pair + pair
  IRB.SetInsertPoint(pair2_true);
  emit_push_vm_stack(ctx, CREATE_LOAD_PAIR_REC(IRB.CreateBitOrPointerCast(pair2, IntptrPtrTy), cdr));
}

void codegen_t::emit_push_cadr_iloc(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

  auto pair = IRB.CreateLoad(IntptrTy, emit_lookup_iloc(ctx, CAR(operands)));

  // check if pair
  BasicBlock* pair_true = BasicBlock::Create(C, "pair_true", F);
  BasicBlock* pair_false = BasicBlock::Create(C, "pair_false", F);
  emit_cond_pairp(ctx, pair, pair_true, pair_false);

  // nonpair
  IRB.SetInsertPoint(pair_false);
  ctx.reg_cache_copy(vm);
  CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(inst));
  auto thunkType = FunctionType::get(VoidTy, {IntptrPtrTy, IntptrTy}, false);
  auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_error_push_cadr_iloc), thunkType->getPointerTo());
  IRB.CreateCall(thunkType, thunk, {vm, pair});
  IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_resume_loop));

  // pair
  IRB.SetInsertPoint(pair_true);
  auto pair2 = CREATE_LOAD_PAIR_REC(IRB.CreateBitOrPointerCast(pair, IntptrPtrTy), cdr);
  BasicBlock* pair2_true = BasicBlock::Create(C, "pair2_true", F);
  emit_cond_pairp(ctx, pair2, pair2_true, pair_false);

  // pair - pair
  IRB.SetInsertPoint(pair2_true);
  emit_push_vm_stack(ctx, CREATE_LOAD_PAIR_REC(IRB.CreateBitOrPointerCast(pair2, IntptrPtrTy), car));
}

void codegen_t::emit_push_nadd_iloc(context_t& ctx, scm_obj_t inst) {
  emit_nadd_iloc(ctx, inst);
  emit_push(ctx, inst);
}

void codegen_t::emit_apply_iloc(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

  auto val = IRB.CreateLoad(IntptrTy, emit_lookup_iloc(ctx, CAR(operands)));
  ctx.reg_value.store(vm, val);
  ctx.reg_cache_copy(vm);
  IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_apply));
}

void codegen_t::emit_apply_gloc(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

  scm_gloc_t gloc = (scm_gloc_t)CAR(operands);
  scm_symbol_t symbol = (scm_symbol_t)gloc->variable;
  scm_obj_t obj = gloc->value;
  if (obj == ctx.m_top_level_closure && HDR_CLOSURE_ARGS(ctx.m_top_level_closure->hdr) == ctx.m_argc) {
#if VERBOSE_CODEGEN
    printf("emit_apply_gloc: self recursive: %s\n", symbol->name);
#endif
    emit_prepair_apply(ctx, ctx.m_top_level_closure);
    ctx.reg_cache_copy_except_value(vm);

    auto call2 = IRB.CreateCall(ctx.m_top_level_function, {vm});
    call2->setTailCallKind(CallInst::TCK_MustTail);
    IRB.CreateRet(call2);
    return;
  }
  if (CLOSUREP(obj) && SYMBOLP(gloc->variable)) {
    scm_closure_t closure = (scm_closure_t)obj;
    if (strchr(symbol->name, IDENTIFIER_RENAME_DELIMITER)) {
#if VERBOSE_CODEGEN
      printf("emit_apply_gloc: uninterned gloc: %s\n", symbol->name);
#endif
      if (closure->env == NULL) {
        auto found = m_lifted_functions.find(closure);
        if (found != m_lifted_functions.end()) {
#if VERBOSE_CODEGEN
          puts("emit_apply_gloc: found in m_lifted_functions, reuse Function*");
#endif
          Function* F2 = found->second;
          if (F2 == NULL) fatal("%s:%u inconsistent state", __FILE__, __LINE__);
          m_usage.inners++;
          emit_prepair_apply(ctx, closure);
          ctx.reg_cache_copy_except_value(vm);
          auto call2 = IRB.CreateCall(F2, {vm});
          call2->setTailCallKind(CallInst::TCK_MustTail);
          IRB.CreateRet(call2);
          return;
        }
#if USE_ADDRESS_TO_FUNCTION
        if (closure->code != NULL) {
  #if VERBOSE_CODEGEN
          puts("emit_apply_gloc: closure already compiled, reuse native code");
  #endif
          Value* F2 = get_function_address(ctx, closure);
          if (F2 == NULL) fatal("%s:%u inconsistent state", __FILE__, __LINE__);
          m_usage.inners++;
          emit_prepair_apply(ctx, closure);
          ctx.reg_cache_copy_except_value(vm);
          auto call2 = IRB.CreateCall(FunctionType::get(IntptrTy, {IntptrPtrTy}, false), F2, {vm});
          call2->setTailCallKind(CallInst::TCK_MustTail);
          IRB.CreateRet(call2);
          return;
        }
#endif
#if VERBOSE_CODEGEN
        puts("emit_apply_gloc: generate new native code for inner function");
#endif
        Function* F2 = emit_inner_function(ctx, closure);
        if (F2 == NULL) fatal("%s:%u inconsistent state", __FILE__, __LINE__);
        m_usage.inners++;
        emit_prepair_apply(ctx, closure);
        ctx.reg_cache_copy_except_value(vm);
        auto call2 = IRB.CreateCall(F2, {vm});
        call2->setTailCallKind(CallInst::TCK_MustTail);
        IRB.CreateRet(call2);
        return;
      } else {
        if (m_debug) {
          printf("hazard: emit_apply_gloc: closure have non NULL environment\n");
        }
#if VERBOSE_CODEGEN
        printf("emit_apply_gloc: out of top level context, closure->env != NULL: %s\n", symbol->name);
#endif
      }
    } else if (strchr(symbol->name, IDENTIFIER_LIBRARY_SUFFIX)) {
#if VERBOSE_CODEGEN
      printf("emit_apply_gloc: library top level: %s\n", symbol->name);
#endif
      if (HDR_CLOSURE_ARGS(closure->hdr) == ctx.m_argc) {
        if (closure->code) {
          auto procType = FunctionType::get(IntptrTy, {IntptrPtrTy}, false);
          auto proc = ConstantExpr::getIntToPtr(VALUE_INTPTR(closure->code), procType->getPointerTo());
          emit_prepair_apply(ctx, closure);
          ctx.reg_cache_copy_except_value(vm);
          auto call = IRB.CreateCall(procType, proc, {vm});
          call->setTailCallKind(CallInst::TCK_MustTail);
          IRB.CreateRet(call);
          return;
        } else {
#if VERBOSE_CODEGEN
          printf("emit_apply_gloc: library top level not compiled: %s\n", symbol->name);
#endif
        }
      } else {
#if VERBOSE_CODEGEN
        printf("emit_apply_gloc: library top level ctx.m_argc does not match: %s\n", symbol->name);
#endif
      }
    }
  }

#if ENABLE_COMPILE_REFERENCE
  if (CLOSUREP(obj)) {
    if (maybe_compile((scm_closure_t)obj)) m_usage.refs++;
  }
#endif

  auto val = CREATE_LOAD_GLOC_REC(IRB.CreateBitOrPointerCast(VALUE_INTPTR(gloc), IntptrPtrTy), value);
  if (gloc->value == scm_undef) {
    BasicBlock* undef_true = BasicBlock::Create(C, "undef_ture", F);
    BasicBlock* CONTINUE = BasicBlock::Create(C, "continue", F);
    auto undef_cond = IRB.CreateICmpEQ(val, VALUE_INTPTR(scm_undef));
    IRB.CreateCondBr(undef_cond, undef_true, CONTINUE, ctx.likely_false);
    IRB.SetInsertPoint(undef_true);
    ctx.reg_cache_copy(vm);
    CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(inst));
    auto thunkType = FunctionType::get(VoidTy, {IntptrPtrTy, IntptrTy}, false);
    auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_error_apply_gloc), thunkType->getPointerTo());
    IRB.CreateCall(thunkType, thunk, {vm, VALUE_INTPTR(gloc)});
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_resume_loop));
    IRB.SetInsertPoint(CONTINUE);
  }
  ctx.reg_value.store(vm, val);
  ctx.reg_cache_copy(vm);
  IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_apply));
}

void codegen_t::emit_ret_const(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

  ctx.reg_value.store(vm, VALUE_INTPTR(operands));
  ctx.reg_cache_copy_only_value_and_cont(vm);
  IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));
}

void codegen_t::emit_ret_iloc(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

  auto val = IRB.CreateLoad(IntptrTy, emit_lookup_iloc(ctx, operands));
  ctx.reg_value.store(vm, val);
  ctx.reg_cache_copy_only_value_and_cont(vm);
  IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));
}

void codegen_t::emit_ret_cons(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  auto vm = F->arg_begin();

  auto sp = ctx.reg_sp.load(vm);
  auto val = ctx.reg_value.load(vm);

  auto sp_minus_1 = IRB.CreateLoad(IntptrTy, IRB.CreateGEP(IntptrTy, IRB.CreateBitOrPointerCast(sp, IntptrPtrTy), VALUE_INTPTR(-1)));
  auto thunkType = FunctionType::get(IntptrTy, {IntptrPtrTy, IntptrTy, IntptrTy}, false);
  auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_make_pair), thunkType->getPointerTo());
  ctx.reg_value.store(vm, IRB.CreateCall(thunkType, thunk, {vm, sp_minus_1, val}));
  ctx.reg_cache_copy_only_value_and_cont(vm);
  IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));
}

void codegen_t::emit_if_true(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

  auto value = ctx.reg_value.load(vm);

  BasicBlock* f9h_true = BasicBlock::Create(C, "f9h_true", F);
  BasicBlock* f9h_false = BasicBlock::Create(C, "f9h_false", F);
  auto f9h_cond = IRB.CreateICmpEQ(value, VALUE_INTPTR(scm_false));
  IRB.CreateCondBr(f9h_cond, f9h_true, f9h_false, ctx.likely_false);

  // taken
  IRB.SetInsertPoint(f9h_false);
  transform(ctx, operands, false);

  // not taken
  IRB.SetInsertPoint(f9h_true);
}

void codegen_t::emit_if_nullp(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

  auto value = ctx.reg_value.load(vm);

  BasicBlock* taken_true = BasicBlock::Create(C, "taken_true", F);
  BasicBlock* taken_false = BasicBlock::Create(C, "taken_false", F);
  auto taken_cond = IRB.CreateICmpEQ(value, VALUE_INTPTR(scm_nil));
  IRB.CreateCondBr(taken_cond, taken_true, taken_false, ctx.likely_false);

  // taken
  IRB.SetInsertPoint(taken_true);
  transform(ctx, operands, false);

  // not taken
  IRB.SetInsertPoint(taken_false);
}

void codegen_t::emit_if_eqp(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

  auto sp = ctx.reg_sp.load(vm);
  auto ea = IRB.CreateGEP(IntptrTy, IRB.CreateBitOrPointerCast(sp, IntptrPtrTy), VALUE_INTPTR(-1));
  ctx.reg_sp.store(vm, IRB.CreateBitOrPointerCast(ea, IntptrTy));
  auto val1 = IRB.CreateLoad(IntptrTy, ea);
  auto val2 = ctx.reg_value.load(vm);

  BasicBlock* taken_true = BasicBlock::Create(C, "taken_true", F);
  BasicBlock* taken_false = BasicBlock::Create(C, "taken_false", F);
  auto taken_cond = IRB.CreateICmpEQ(val1, val2);
  IRB.CreateCondBr(taken_cond, taken_true, taken_false, ctx.likely_false);

  // taken
  IRB.SetInsertPoint(taken_true);
  transform(ctx, operands, false);

  // not taken
  IRB.SetInsertPoint(taken_false);
}

void codegen_t::emit_if_nullp_ret_const(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

  auto value = ctx.reg_value.load(vm);

  BasicBlock* taken_true = BasicBlock::Create(C, "taken_true", F);
  BasicBlock* taken_false = BasicBlock::Create(C, "taken_false", F);
  auto taken_cond = IRB.CreateICmpEQ(value, VALUE_INTPTR(scm_nil));
  IRB.CreateCondBr(taken_cond, taken_true, taken_false, ctx.likely_false);

  // taken
  IRB.SetInsertPoint(taken_true);
  ctx.reg_value.store(vm, VALUE_INTPTR(operands));
  ctx.reg_cache_copy_only_value_and_cont(vm);
  IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));

  // not taken
  IRB.SetInsertPoint(taken_false);
}

Value* codegen_t::emit_load_iloc(context_t& ctx, scm_obj_t operands) {
  return emit_load_iloc(ctx, FIXNUM(CAR(operands)), (FIXNUM(CDR(operands))));
}

Value* codegen_t::emit_load_iloc(context_t& ctx, intptr_t depth, intptr_t index) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;

  int iloc_index = calc_iloc_index(ctx, depth, index);
  auto cached = ctx.m_iloc_cache[iloc_index];
  if (cached) return cached;
  auto val = IRB.CreateLoad(IntptrTy, emit_lookup_iloc(ctx, depth, index));
  ctx.m_iloc_cache[iloc_index] = val;
  return val;
}

void codegen_t::emit_iloc(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

#if USE_ILOC_CACHE
  auto val = emit_load_iloc(ctx, operands);
#else
  auto val = IRB.CreateLoad(IntptrTy, emit_lookup_iloc(ctx, operands));
#endif

  ctx.reg_value.store(vm, val);
}

void codegen_t::emit_iloc0(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

#if USE_ILOC_CACHE
  auto val = emit_load_iloc(ctx, 0, FIXNUM(operands));
#else
  auto val = IRB.CreateLoad(IntptrTy, emit_lookup_iloc(ctx, 0, FIXNUM(operands)));
#endif
  ctx.reg_value.store(vm, val);
}

void codegen_t::emit_iloc1(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

#if USE_ILOC_CACHE
  auto val = emit_load_iloc(ctx, 1, FIXNUM(operands));
#else
  auto val = IRB.CreateLoad(IntptrTy, emit_lookup_iloc(ctx, 1, FIXNUM(operands)));
#endif
  ctx.reg_value.store(vm, val);
}

void codegen_t::emit_push_iloc(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
#if USE_ILOC_CACHE
  emit_push_vm_stack(ctx, emit_load_iloc(ctx, operands));
#else
  emit_push_vm_stack(ctx, IRB.CreateLoad(IntptrTy, emit_lookup_iloc(ctx, operands)));
#endif
}

void codegen_t::emit_if_true_ret(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  auto vm = F->arg_begin();

  auto value = ctx.reg_value.load(vm);

  BasicBlock* value_false = BasicBlock::Create(C, "value_false", F);
  BasicBlock* value_nonfalse = BasicBlock::Create(C, "value_nonfalse", F);
  auto value_cond = IRB.CreateICmpEQ(value, VALUE_INTPTR(scm_false));
  IRB.CreateCondBr(value_cond, value_false, value_nonfalse, ctx.likely_false);

  // pop
  IRB.SetInsertPoint(value_nonfalse);
  ctx.reg_cache_copy_only_value_and_cont(vm);
  IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));

  // continue
  IRB.SetInsertPoint(value_false);
}

void codegen_t::emit_if_false_ret(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  auto vm = F->arg_begin();

  auto value = ctx.reg_value.load(vm);

  BasicBlock* value_false = BasicBlock::Create(C, "value_false", F);
  BasicBlock* value_nonfalse = BasicBlock::Create(C, "value_nonfalse", F);
  auto value_cond = IRB.CreateICmpEQ(value, VALUE_INTPTR(scm_false));
  IRB.CreateCondBr(value_cond, value_false, value_nonfalse, ctx.likely_false);

  // pop
  IRB.SetInsertPoint(value_false);
  ctx.reg_cache_copy_only_value_and_cont(vm);
  IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));

  // continue
  IRB.SetInsertPoint(value_nonfalse);
}

void codegen_t::emit_if_true_ret_const(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

  auto value = ctx.reg_value.load(vm);

  BasicBlock* value_false = BasicBlock::Create(C, "value_false", F);
  BasicBlock* value_nonfalse = BasicBlock::Create(C, "value_nonfalse", F);
  auto value_cond = IRB.CreateICmpEQ(value, VALUE_INTPTR(scm_false));
  IRB.CreateCondBr(value_cond, value_false, value_nonfalse, ctx.likely_false);

  // pop
  IRB.SetInsertPoint(value_nonfalse);
  ctx.reg_value.store(vm, VALUE_INTPTR(operands));
  ctx.reg_cache_copy_only_value_and_cont(vm);
  IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));

  // continue
  IRB.SetInsertPoint(value_false);
}

Value* codegen_t::emit_cmp_inst(context_t& ctx, cc_t cc, Value* lhs, Value* rhs) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  switch (cc) {
    case LT:
      return IRB.CreateICmpSLT(lhs, rhs);
    case GT:
      return IRB.CreateICmpSGT(lhs, rhs);
    case LE:
      return IRB.CreateICmpSLE(lhs, rhs);
    case GE:
      return IRB.CreateICmpSGE(lhs, rhs);
    case EQ:
      return IRB.CreateICmpEQ(lhs, rhs);
  }
}

void codegen_t::emit_cc_n_iloc(context_t& ctx, scm_obj_t inst, cc_t cc, void* c_func) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

#if USE_ILOC_CACHE
  auto lhs = emit_load_iloc(ctx, CAR(operands));
#else
  auto lhs = IRB.CreateLoad(IntptrTy, emit_lookup_iloc(ctx, CAR(operands)));
#endif
  auto rhs = VALUE_INTPTR(CADR(operands));

  auto retval = emit_alloca(ctx, IntptrTy);

  BasicBlock* CONTINUE = BasicBlock::Create(C, "continue", F);
  BasicBlock* nonfixnum_true = BasicBlock::Create(C, "nonfixnum_true", F);
  BasicBlock* nonfixnum_false = BasicBlock::Create(C, "nonfixnum_false", F);
  auto nonfixnum_cond = IRB.CreateICmpEQ(IRB.CreateAnd(lhs, 1), VALUE_INTPTR(0));
  IRB.CreateCondBr(nonfixnum_cond, nonfixnum_true, nonfixnum_false, ctx.likely_false);

  // fixnum
  IRB.SetInsertPoint(nonfixnum_false);
  BasicBlock* cond_true = BasicBlock::Create(C, "cond_true", F);
  BasicBlock* cond_false = BasicBlock::Create(C, "cond_false", F);
  auto cond = emit_cmp_inst(ctx, cc, lhs, rhs);
  IRB.CreateCondBr(cond, cond_true, cond_false, ctx.likely_false);

  // taken
  IRB.SetInsertPoint(cond_true);
  IRB.CreateStore(VALUE_INTPTR(scm_true), retval);
  IRB.CreateBr(CONTINUE);

  // not taken
  IRB.SetInsertPoint(cond_false);
  IRB.CreateStore(VALUE_INTPTR(scm_false), retval);
  IRB.CreateBr(CONTINUE);

  // others
  IRB.SetInsertPoint(nonfixnum_true);
  ctx.reg_cache_copy_except_value(vm);
  CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(inst));
  auto thunkType = FunctionType::get(IntptrTy, {IntptrPtrTy, IntptrTy, IntptrTy}, false);
  auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_func), thunkType->getPointerTo());
  auto ans = IRB.CreateCall(thunkType, thunk, {vm, lhs, rhs});

  BasicBlock* fallback_success = BasicBlock::Create(C, "fallback_success", F);
  BasicBlock* fallback_fail = BasicBlock::Create(C, "fallback_fail", F);
  auto fallback_cond = IRB.CreateICmpNE(ans, VALUE_INTPTR(0));
  IRB.CreateCondBr(fallback_cond, fallback_success, fallback_fail, ctx.likely_true);

  IRB.SetInsertPoint(fallback_fail);
  IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_resume_loop));

  IRB.SetInsertPoint(fallback_success);
  IRB.CreateStore(ans, retval);
  IRB.CreateBr(CONTINUE);

  IRB.SetInsertPoint(CONTINUE);
  ctx.reg_value.store(vm, IRB.CreateLoad(IntptrTy, retval));
}

void codegen_t::emit_lt_n_iloc(context_t& ctx, scm_obj_t inst) { emit_cc_n_iloc(ctx, inst, LT, (void*)c_lt_n_iloc); }

void codegen_t::emit_gt_n_iloc(context_t& ctx, scm_obj_t inst) { emit_cc_n_iloc(ctx, inst, GT, (void*)c_gt_n_iloc); }

void codegen_t::emit_ge_n_iloc(context_t& ctx, scm_obj_t inst) { emit_cc_n_iloc(ctx, inst, GE, (void*)c_ge_n_iloc); }

void codegen_t::emit_le_n_iloc(context_t& ctx, scm_obj_t inst) { emit_cc_n_iloc(ctx, inst, LE, (void*)c_le_n_iloc); }

void codegen_t::emit_eq_n_iloc(context_t& ctx, scm_obj_t inst) { emit_cc_n_iloc(ctx, inst, EQ, (void*)c_eq_n_iloc); }

void codegen_t::emit_cc_iloc(context_t& ctx, scm_obj_t inst, cc_t cc, void* c_func) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

  auto lhs = ctx.reg_value.load(vm);
#if USE_ILOC_CACHE
  auto rhs = emit_load_iloc(ctx, CAR(operands));
#else
  auto rhs = IRB.CreateLoad(IntptrTy, emit_lookup_iloc(ctx, CAR(operands)));
#endif
  auto retval = emit_alloca(ctx, IntptrTy);

  BasicBlock* CONTINUE = BasicBlock::Create(C, "continue", F);
  BasicBlock* nonfixnum_true = BasicBlock::Create(C, "nonfixnum_true", F);
  BasicBlock* nonfixnum_false = BasicBlock::Create(C, "nonfixnum_false", F);
  auto nonfixnum_cond = IRB.CreateICmpEQ(IRB.CreateAnd(lhs, IRB.CreateAnd(rhs, 1)), VALUE_INTPTR(0));
  IRB.CreateCondBr(nonfixnum_cond, nonfixnum_true, nonfixnum_false, ctx.likely_false);

  // fixnum
  IRB.SetInsertPoint(nonfixnum_false);
  BasicBlock* cond_true = BasicBlock::Create(C, "cond_true", F);
  BasicBlock* cond_false = BasicBlock::Create(C, "cond_false", F);
  auto cond = emit_cmp_inst(ctx, cc, lhs, rhs);
  IRB.CreateCondBr(cond, cond_true, cond_false, ctx.likely_false);

  // taken
  IRB.SetInsertPoint(cond_true);
  IRB.CreateStore(VALUE_INTPTR(scm_true), retval);
  IRB.CreateBr(CONTINUE);

  // not taken
  IRB.SetInsertPoint(cond_false);
  IRB.CreateStore(VALUE_INTPTR(scm_false), retval);
  IRB.CreateBr(CONTINUE);

  // others
  IRB.SetInsertPoint(nonfixnum_true);
  ctx.reg_cache_copy_except_value(vm);
  CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(inst));
  auto thunkType = FunctionType::get(IntptrTy, {IntptrPtrTy, IntptrTy, IntptrTy}, false);
  auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_func), thunkType->getPointerTo());
  auto ans = IRB.CreateCall(thunkType, thunk, {vm, lhs, rhs});

  BasicBlock* fallback_success = BasicBlock::Create(C, "fallback_success", F);
  BasicBlock* fallback_fail = BasicBlock::Create(C, "fallback_fail", F);
  auto fallback_cond = IRB.CreateICmpNE(ans, VALUE_INTPTR(0));
  IRB.CreateCondBr(fallback_cond, fallback_success, fallback_fail, ctx.likely_true);

  IRB.SetInsertPoint(fallback_fail);
  IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_resume_loop));

  IRB.SetInsertPoint(fallback_success);
  IRB.CreateStore(ans, retval);
  IRB.CreateBr(CONTINUE);

  IRB.SetInsertPoint(CONTINUE);
  ctx.reg_value.store(vm, IRB.CreateLoad(IntptrTy, retval));
}

void codegen_t::emit_gt_iloc(context_t& ctx, scm_obj_t inst) { emit_cc_iloc(ctx, inst, GT, (void*)c_gt_iloc); }

void codegen_t::emit_lt_iloc(context_t& ctx, scm_obj_t inst) { emit_cc_iloc(ctx, inst, LT, (void*)c_lt_iloc); }

void codegen_t::emit_ge_iloc(context_t& ctx, scm_obj_t inst) { emit_cc_iloc(ctx, inst, GE, (void*)c_ge_iloc); }

void codegen_t::emit_le_iloc(context_t& ctx, scm_obj_t inst) { emit_cc_iloc(ctx, inst, LE, (void*)c_le_iloc); }

void codegen_t::emit_eq_iloc(context_t& ctx, scm_obj_t inst) { emit_cc_iloc(ctx, inst, EQ, (void*)c_eq_iloc); }

Function* codegen_t::emit_call(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

  char cont_id[40];
  uuid_v4(cont_id, sizeof(cont_id));

  Function* K = Function::Create(FunctionType::get(IntptrTy, {IntptrPtrTy}, false), Function::PrivateLinkage, cont_id, M);
#if USE_LLVM_ATTRIBUTES
  K->addFnAttr(Attribute::ArgMemOnly);
  K->addFnAttr(Attribute::NoUnwind);
  K->addFnAttr(Attribute::WillReturn);
  for (Argument& argument : K->args()) {
    argument.addAttr(Attribute::NoAlias);
    argument.addAttr(Attribute::NoCapture);
    argument.addAttr(Attribute::NoFree);
  }
#endif

  BasicBlock* RETURN = BasicBlock::Create(C, "entry", K);

  // vm_cont_t cont = (vm_cont_t)m_sp;
  auto cont = IRB.CreateBitOrPointerCast(ctx.reg_sp.load(vm), IntptrPtrTy);
  // cont->pc
  CREATE_STORE_CONT_REC(cont, pc, VALUE_INTPTR(CDR(inst)));
  // cont->trace
  CREATE_STORE_CONT_REC(cont, trace, VALUE_INTPTR(scm_unspecified));
  // cont->fp
  CREATE_STORE_CONT_REC(cont, fp, ctx.reg_fp.load(vm));
  // cont->env
  CREATE_STORE_CONT_REC(cont, env, ctx.reg_env.load(vm));
  // cont->code
  CREATE_STORE_CONT_REC(cont, code, IRB.CreateBitOrPointerCast(K, IntptrTy));
  // cont->up
  CREATE_STORE_CONT_REC(cont, up, ctx.reg_cont.load(vm));
  // m_sp
  auto ea1 = IRB.CreateBitOrPointerCast(IRB.CreateGEP(IntptrTy, cont, VALUE_INTPTR(sizeof(vm_cont_rec_t) / sizeof(intptr_t))), IntptrTy);
  ctx.reg_sp.store(vm, ea1);
  // m_fp
  ctx.reg_fp.store(vm, ea1);
  // m_cont
  auto ea2 = IRB.CreateBitOrPointerCast(IRB.CreateGEP(IntptrTy, cont, VALUE_INTPTR(offsetof(vm_cont_rec_t, up) / sizeof(intptr_t))), IntptrTy);
  ctx.reg_cont.store(vm, ea2);

  context_t ctx2 = ctx;
  ctx2.m_argc = 0;
  transform(ctx2, operands, false);

  IRB.SetInsertPoint(RETURN);
  ctx.reg_cache_clear();
  ctx.m_iloc_cache.clear();
  return K;
}

void codegen_t::emit_if_false_call(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

  auto value = ctx.reg_value.load(vm);

  BasicBlock* value_false = BasicBlock::Create(C, "value_false", F);
  BasicBlock* value_nonfalse = BasicBlock::Create(C, "value_nonfalse", F);
  auto value_cond = IRB.CreateICmpEQ(value, VALUE_INTPTR(scm_false));
  IRB.CreateCondBr(value_cond, value_false, value_nonfalse, ctx.likely_false);

  // taken
  IRB.SetInsertPoint(value_false);
  context_t ctx2 = ctx;
  ctx2.m_argc = 0;
  transform(ctx2, operands, false);

  // no taken
  IRB.SetInsertPoint(value_nonfalse);
}

void codegen_t::emit_extend(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

  auto argc = VALUE_INTPTR(FIXNUM(operands));
  auto env = IRB.CreateBitOrPointerCast(ctx.reg_sp.load(vm), IntptrPtrTy);
  CREATE_STORE_ENV_REC(env, count, argc);
  CREATE_STORE_ENV_REC(env, up, ctx.reg_env.load(vm));
  auto ea0 = IRB.CreateGEP(IntptrTy, IRB.CreateBitOrPointerCast(env, IntptrPtrTy), VALUE_INTPTR(sizeof(vm_env_rec_t) / sizeof(intptr_t)));
  auto ea1 = IRB.CreateBitOrPointerCast(ea0, IntptrTy);
  ctx.reg_sp.store(vm, ea1);
  ctx.reg_fp.store(vm, ea1);
  ctx.reg_env.store(vm, CREATE_LEA_ENV_REC(env, up));
  ctx.set_local_var_count(ctx.m_depth, FIXNUM(operands));
}

void codegen_t::emit_extend_enclose_local(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

  emit_push_vm_stack(ctx, VALUE_INTPTR(CDR(operands)));
  auto env = IRB.CreateBitOrPointerCast(ctx.reg_sp.load(vm), IntptrPtrTy);
  CREATE_STORE_ENV_REC(env, count, VALUE_INTPTR(1));
  CREATE_STORE_ENV_REC(env, up, ctx.reg_env.load(vm));
  auto ea0 = IRB.CreateGEP(IntptrTy, IRB.CreateBitOrPointerCast(env, IntptrPtrTy), VALUE_INTPTR(sizeof(vm_env_rec_t) / sizeof(intptr_t)));
  auto ea1 = IRB.CreateBitOrPointerCast(ea0, IntptrTy);
  ctx.reg_sp.store(vm, ea1);
  ctx.reg_fp.store(vm, ea1);
  ctx.reg_env.store(vm, CREATE_LEA_ENV_REC(env, up));

  BasicBlock* CONTINUE = BasicBlock::Create(C, "continue", F);
  IRB.CreateBr(CONTINUE);

  // continue emit code in operands
  char local_id[40];
  uuid_v4(local_id, sizeof(local_id));
  Function* L = Function::Create(FunctionType::get(IntptrTy, {IntptrPtrTy}, false), Function::PrivateLinkage, local_id, M);
#if USE_LLVM_ATTRIBUTES
  L->addFnAttr(Attribute::ArgMemOnly);
  L->addFnAttr(Attribute::NoUnwind);
  L->addFnAttr(Attribute::WillReturn);
  for (Argument& argument : L->args()) {
    argument.addAttr(Attribute::NoAlias);
    argument.addAttr(Attribute::NoCapture);
    argument.addAttr(Attribute::NoFree);
  }
#endif

  BasicBlock* LOOP = BasicBlock::Create(C, "entry", L);

  int function_index = ctx.m_depth + (0 << 16);
  ctx.m_local_functions[function_index] = L;
  ctx.set_local_var_count(ctx.m_depth, 1);
  m_usage.locals++;

  context_t ctx2 = ctx;
  ctx2.m_function = L;
  int nargs = FIXNUM(CAR(CAR(operands))) + FIXNUM(CADR(CAR(operands)));
  ctx2.set_local_var_count(ctx2.m_depth, 1);
  ctx2.set_local_var_count(ctx2.m_depth + 1, nargs);
  ctx2.m_depth += 2;
  ctx2.m_argc = 0;
  ctx2.reg_cache_clear();
  ctx2.m_iloc_cache.clear();

  IRB.SetInsertPoint(LOOP);
  transform(ctx2, CDR(operands), true);

  IRB.SetInsertPoint(CONTINUE);
}

void codegen_t::emit_apply_iloc_local(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

  int level = FIXNUM(CAAR(operands));
  int index = FIXNUM(CDAR(operands));
  int function_index = ctx.m_depth - level - 1 + (index << 16);

  if (ctx.m_depth - level - 1 < 0 || ctx.m_local_functions[function_index] == NULL) {
#if VERBOSE_CODEGEN
    printf("emit_apply_iloc_local: out of local context, level = %d index = %d ctx.m_depth = %d ctx.m_depth - level - 1 = %x \n", level, index,
           ctx.m_depth, ctx.m_depth - level - 1);
#endif
    if (m_debug) {
      if (ctx.m_depth - level - 1 < 0) {
        printf("hazard: emit_apply_iloc_local: referencing free variable (%d - %d)\n", ctx.m_depth, level);
      } else if (ctx.m_local_functions[function_index] == NULL) {
        printf("hazard: emit_apply_iloc_local: ctx.m_local_functions[%d] == NULL\n", function_index);
      }
    }
#if USE_ILOC_CACHE
    CREATE_STORE_VM_REG(vm, m_pc, emit_load_iloc(ctx, level, index));
#else
    CREATE_STORE_VM_REG(vm, m_pc, IRB.CreateLoad(IntptrTy, emit_lookup_iloc(ctx, level, index)));
#endif
    auto env2 = emit_lookup_env(ctx, level);
    auto count = CREATE_LOAD_ENV_REC(env2, count);
    IRB.CreateLoad(IntptrTy, index == 0 ? IRB.CreateGEP(IntptrTy, env2, IRB.CreateNeg(count))
                                        : IRB.CreateGEP(IntptrTy, env2, IRB.CreateSub(VALUE_INTPTR(index), count)));
    auto env = IRB.CreateBitOrPointerCast(ctx.reg_sp.load(vm), IntptrPtrTy);
    CREATE_STORE_ENV_REC(env, count, VALUE_INTPTR(ctx.m_argc));
    CREATE_STORE_ENV_REC(env, up, CREATE_LEA_ENV_REC(env2, up));
    auto ea0 = IRB.CreateGEP(IntptrTy, IRB.CreateBitOrPointerCast(env, IntptrPtrTy), VALUE_INTPTR(sizeof(vm_env_rec_t) / sizeof(intptr_t)));
    auto ea1 = IRB.CreateBitOrPointerCast(ea0, IntptrTy);
    ctx.reg_sp.store(vm, ea1);
    ctx.reg_fp.store(vm, ea1);
    ctx.reg_env.store(vm, CREATE_LEA_ENV_REC(env, up));
    ctx.reg_cache_copy(vm);
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_loop));
  } else {
    auto env2 = emit_lookup_env(ctx, level);
    auto count = CREATE_LOAD_ENV_REC(env2, count);
    IRB.CreateLoad(IntptrTy, index == 0 ? IRB.CreateGEP(IntptrTy, env2, IRB.CreateNeg(count))
                                        : IRB.CreateGEP(IntptrTy, env2, IRB.CreateSub(VALUE_INTPTR(index), count)));
    auto env = IRB.CreateBitOrPointerCast(ctx.reg_sp.load(vm), IntptrPtrTy);
    CREATE_STORE_ENV_REC(env, count, VALUE_INTPTR(ctx.m_argc));
    CREATE_STORE_ENV_REC(env, up, CREATE_LEA_ENV_REC(env2, up));
    auto ea0 = IRB.CreateGEP(IntptrTy, IRB.CreateBitOrPointerCast(env, IntptrPtrTy), VALUE_INTPTR(sizeof(vm_env_rec_t) / sizeof(intptr_t)));
    auto ea1 = IRB.CreateBitOrPointerCast(ea0, IntptrTy);
    ctx.reg_sp.store(vm, ea1);
    ctx.reg_fp.store(vm, ea1);
    ctx.reg_env.store(vm, CREATE_LEA_ENV_REC(env, up));
    ctx.reg_cache_copy_except_value(vm);
    Function* L = ctx.m_local_functions[function_index];
    if (L == NULL) {
      fatal("%s:%u emit_apply_iloc_local L = %p, level = %d index = %d ctx.m_depth = %d function_index = %x \n", __FILE__, __LINE__, L, level,
            index, ctx.m_depth, function_index);
    }
    auto call = IRB.CreateCall(L, {vm});
    call->setTailCallKind(CallInst::TCK_MustTail);
    IRB.CreateRet(call);
  }
}

void codegen_t::emit_push_cons(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  auto vm = F->arg_begin();

  auto sp = ctx.reg_sp.load(vm);
  auto val = ctx.reg_value.load(vm);

  auto ea = IRB.CreateGEP(IntptrTy, IRB.CreateBitOrPointerCast(sp, IntptrPtrTy), VALUE_INTPTR(-1));
  auto sp_minus_1 = IRB.CreateLoad(IntptrTy, ea);
  auto thunkType = FunctionType::get(IntptrTy, {IntptrPtrTy, IntptrTy, IntptrTy}, false);
  auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_make_pair), thunkType->getPointerTo());
  IRB.CreateStore(IRB.CreateCall(thunkType, thunk, {vm, sp_minus_1, val}), ea);
}

void codegen_t::emit_car_iloc(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

#if USE_ILOC_CACHE
  auto pair = emit_load_iloc(ctx, CAR(operands));
#else
  auto pair = IRB.CreateLoad(IntptrTy, emit_lookup_iloc(ctx, CAR(operands)));
#endif

  // check if pair
  BasicBlock* pair_true = BasicBlock::Create(C, "pair_true", F);
  BasicBlock* pair_false = BasicBlock::Create(C, "pair_false", F);
  emit_cond_pairp(ctx, pair, pair_true, pair_false);

  // nonpair
  IRB.SetInsertPoint(pair_false);
  ctx.reg_cache_copy(vm);
  CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(inst));
  auto thunkType = FunctionType::get(VoidTy, {IntptrPtrTy, IntptrTy}, false);
  auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_error_car_iloc), thunkType->getPointerTo());
  IRB.CreateCall(thunkType, thunk, {vm, pair});
  IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_resume_loop));

  // pair
  IRB.SetInsertPoint(pair_true);
  ctx.reg_value.store(vm, CREATE_LOAD_PAIR_REC(IRB.CreateBitOrPointerCast(pair, IntptrPtrTy), car));
}

void codegen_t::emit_cdr_iloc(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

#if USE_ILOC_CACHE
  auto pair = emit_load_iloc(ctx, CAR(operands));
#else
  auto pair = IRB.CreateLoad(IntptrTy, emit_lookup_iloc(ctx, CAR(operands)));
#endif

  // check if pair
  BasicBlock* pair_true = BasicBlock::Create(C, "pair_true", F);
  BasicBlock* pair_false = BasicBlock::Create(C, "pair_false", F);
  emit_cond_pairp(ctx, pair, pair_true, pair_false);

  // nonpair
  IRB.SetInsertPoint(pair_false);
  ctx.reg_cache_copy(vm);
  CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(inst));
  auto thunkType = FunctionType::get(VoidTy, {IntptrPtrTy, IntptrTy}, false);
  auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_error_cdr_iloc), thunkType->getPointerTo());
  IRB.CreateCall(thunkType, thunk, {vm, pair});
  IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_resume_loop));

  // pair
  IRB.SetInsertPoint(pair_true);
  ctx.reg_value.store(vm, CREATE_LOAD_PAIR_REC(IRB.CreateBitOrPointerCast(pair, IntptrPtrTy), cdr));
}

void codegen_t::emit_set_gloc(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

  ctx.reg_cache_copy_only_value(vm);
  auto thunkType = FunctionType::get(VoidTy, {IntptrPtrTy, IntptrTy}, false);
  auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_set_gloc), thunkType->getPointerTo());
  IRB.CreateCall(thunkType, thunk, {vm, VALUE_INTPTR(operands)});
}

void codegen_t::emit_const(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

  ctx.reg_value.store(vm, VALUE_INTPTR(operands));
}

void codegen_t::emit_if_pairp(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

  BasicBlock* taken_true = BasicBlock::Create(C, "taken_true", F);
  BasicBlock* taken_false = BasicBlock::Create(C, "taken_false", F);

  emit_cond_pairp(ctx, ctx.reg_value.load(vm), taken_true, taken_false);

  // taken
  IRB.SetInsertPoint(taken_true);
  transform(ctx, operands, false);

  // not taken
  IRB.SetInsertPoint(taken_false);
}

void codegen_t::emit_if_eqp_ret_const(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

  auto sp = ctx.reg_sp.load(vm);
  auto ea = IRB.CreateGEP(IntptrTy, IRB.CreateBitOrPointerCast(sp, IntptrPtrTy), VALUE_INTPTR(-1));
  ctx.reg_sp.store(vm, IRB.CreateBitOrPointerCast(ea, IntptrTy));

  auto val1 = IRB.CreateLoad(IntptrTy, ea);
  auto val2 = ctx.reg_value.load(vm);

  BasicBlock* taken_true = BasicBlock::Create(C, "taken_true", F);
  BasicBlock* taken_false = BasicBlock::Create(C, "taken_false", F);
  auto taken_cond = IRB.CreateICmpEQ(val1, val2);
  IRB.CreateCondBr(taken_cond, taken_true, taken_false, ctx.likely_false);

  // taken
  IRB.SetInsertPoint(taken_true);
  ctx.reg_value.store(vm, VALUE_INTPTR(operands));
  ctx.reg_cache_copy_only_value_and_cont(vm);
  IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));

  // not taken
  IRB.SetInsertPoint(taken_false);
}

void codegen_t::emit_cadr_iloc(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

#if USE_ILOC_CACHE
  auto pair = emit_load_iloc(ctx, CAR(operands));
#else
  auto pair = IRB.CreateLoad(IntptrTy, emit_lookup_iloc(ctx, CAR(operands)));
#endif

  // check if pair
  BasicBlock* pair_true = BasicBlock::Create(C, "pair_true", F);
  BasicBlock* pair_false = BasicBlock::Create(C, "pair_false", F);
  emit_cond_pairp(ctx, pair, pair_true, pair_false);

  // nonpair
  IRB.SetInsertPoint(pair_false);
  ctx.reg_cache_copy(vm);
  CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(inst));
  auto thunkType = FunctionType::get(VoidTy, {IntptrPtrTy, IntptrTy}, false);
  auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_error_cadr_iloc), thunkType->getPointerTo());
  IRB.CreateCall(thunkType, thunk, {vm, pair});
  IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_resume_loop));

  // pair
  IRB.SetInsertPoint(pair_true);
  auto pair2 = CREATE_LOAD_PAIR_REC(IRB.CreateBitOrPointerCast(pair, IntptrPtrTy), cdr);
  BasicBlock* pair2_true = BasicBlock::Create(C, "pair2_true", F);
  emit_cond_pairp(ctx, pair2, pair2_true, pair_false);

  // pair + pair
  IRB.SetInsertPoint(pair2_true);
  ctx.reg_value.store(vm, CREATE_LOAD_PAIR_REC(IRB.CreateBitOrPointerCast(pair2, IntptrPtrTy), car));
}

void codegen_t::emit_cddr_iloc(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

#if USE_ILOC_CACHE
  auto pair = emit_load_iloc(ctx, CAR(operands));
#else
  auto pair = IRB.CreateLoad(IntptrTy, emit_lookup_iloc(ctx, CAR(operands)));
#endif

  // check if pair
  BasicBlock* pair_true = BasicBlock::Create(C, "pair_true", F);
  BasicBlock* pair_false = BasicBlock::Create(C, "pair_false", F);
  emit_cond_pairp(ctx, pair, pair_true, pair_false);

  // nonpair
  IRB.SetInsertPoint(pair_false);
  ctx.reg_cache_copy(vm);
  CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(inst));
  auto thunkType = FunctionType::get(VoidTy, {IntptrPtrTy, IntptrTy}, false);
  auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_error_cadr_iloc), thunkType->getPointerTo());
  IRB.CreateCall(thunkType, thunk, {vm, pair});
  IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_resume_loop));

  // pair
  IRB.SetInsertPoint(pair_true);
  auto pair2 = CREATE_LOAD_PAIR_REC(IRB.CreateBitOrPointerCast(pair, IntptrPtrTy), cdr);
  BasicBlock* pair2_true = BasicBlock::Create(C, "pair2_true", F);
  emit_cond_pairp(ctx, pair2, pair2_true, pair_false);

  // pair - pair
  IRB.SetInsertPoint(pair2_true);
  ctx.reg_value.store(vm, CREATE_LOAD_PAIR_REC(IRB.CreateBitOrPointerCast(pair2, IntptrPtrTy), cdr));
}

void codegen_t::emit_if_not_eqp_ret_const(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

  auto sp = ctx.reg_sp.load(vm);

  auto ea = IRB.CreateGEP(IntptrTy, IRB.CreateBitOrPointerCast(sp, IntptrPtrTy), VALUE_INTPTR(-1));
  ctx.reg_sp.store(vm, IRB.CreateBitOrPointerCast(ea, IntptrTy));

  auto val1 = IRB.CreateLoad(IntptrTy, ea);
  auto val2 = ctx.reg_value.load(vm);

  BasicBlock* taken_true = BasicBlock::Create(C, "taken_true", F);
  BasicBlock* taken_false = BasicBlock::Create(C, "taken_false", F);
  auto taken_cond = IRB.CreateICmpNE(val1, val2);
  IRB.CreateCondBr(taken_cond, taken_true, taken_false, ctx.likely_false);

  // taken
  IRB.SetInsertPoint(taken_true);
  ctx.reg_value.store(vm, VALUE_INTPTR(operands));
  ctx.reg_cache_copy_only_value_and_cont(vm);
  IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));

  // not taken
  IRB.SetInsertPoint(taken_false);
}

void codegen_t::emit_if_not_pairp_ret_const(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

  auto value = ctx.reg_value.load(vm);

  BasicBlock* pair_true = BasicBlock::Create(C, "pair_true", F);
  BasicBlock* pair_false = BasicBlock::Create(C, "pair_false", F);
  emit_cond_pairp(ctx, value, pair_true, pair_false);

  // not pair
  IRB.SetInsertPoint(pair_false);
  ctx.reg_value.store(vm, VALUE_INTPTR(operands));
  ctx.reg_cache_copy_only_value_and_cont(vm);
  IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));

  // pair
  IRB.SetInsertPoint(pair_true);
}

void codegen_t::emit_if_false_ret_const(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

  auto value = ctx.reg_value.load(vm);

  BasicBlock* value_false = BasicBlock::Create(C, "value_false", F);
  BasicBlock* value_nonfalse = BasicBlock::Create(C, "value_nonfalse", F);
  auto value_cond = IRB.CreateICmpEQ(value, VALUE_INTPTR(scm_false));
  IRB.CreateCondBr(value_cond, value_false, value_nonfalse, ctx.likely_false);

  // pop
  IRB.SetInsertPoint(value_false);
  ctx.reg_value.store(vm, VALUE_INTPTR(operands));
  ctx.reg_cache_copy_only_value_and_cont(vm);
  IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));

  // continue
  IRB.SetInsertPoint(value_nonfalse);
}

void codegen_t::emit_ret_nullp(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  auto vm = F->arg_begin();

  auto value = ctx.reg_value.load(vm);

  BasicBlock* taken_true = BasicBlock::Create(C, "taken_true", F);
  BasicBlock* taken_false = BasicBlock::Create(C, "taken_false", F);
  auto taken_cond = IRB.CreateICmpEQ(value, VALUE_INTPTR(scm_nil));
  IRB.CreateCondBr(taken_cond, taken_true, taken_false, ctx.likely_false);

  // taken
  IRB.SetInsertPoint(taken_true);
  ctx.reg_value.store(vm, VALUE_INTPTR(scm_true));
  ctx.reg_cache_copy_only_value_and_cont(vm);
  IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));

  // not taken
  IRB.SetInsertPoint(taken_false);
  ctx.reg_value.store(vm, VALUE_INTPTR(scm_false));
  ctx.reg_cache_copy_only_value_and_cont(vm);
  IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));
}

void codegen_t::emit_ret_pairp(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  auto vm = F->arg_begin();

  auto value = ctx.reg_value.load(vm);

  BasicBlock* taken_true = BasicBlock::Create(C, "taken_true", F);
  BasicBlock* taken_false = BasicBlock::Create(C, "taken_false", F);
  emit_cond_pairp(ctx, value, taken_true, taken_false);

  // taken
  IRB.SetInsertPoint(taken_true);
  ctx.reg_value.store(vm, VALUE_INTPTR(scm_true));
  ctx.reg_cache_copy_only_value_and_cont(vm);
  IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));

  // not taken
  IRB.SetInsertPoint(taken_false);
  ctx.reg_value.store(vm, VALUE_INTPTR(scm_false));
  ctx.reg_cache_copy_only_value_and_cont(vm);
  IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));
}

void codegen_t::emit_ret_gloc(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

  auto gloc = IRB.CreateBitOrPointerCast(VALUE_INTPTR(operands), IntptrPtrTy);
  ctx.reg_value.store(vm, CREATE_LOAD_GLOC_REC(gloc, value));
  ctx.reg_cache_copy_only_value_and_cont(vm);
  IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));
}

void codegen_t::emit_ret_eqp(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  auto vm = F->arg_begin();

  auto sp = ctx.reg_sp.load(vm);
  auto ea = IRB.CreateGEP(IntptrTy, IRB.CreateBitOrPointerCast(sp, IntptrPtrTy), VALUE_INTPTR(-1));
  ctx.reg_sp.store(vm, IRB.CreateBitOrPointerCast(ea, IntptrTy));

  auto val1 = IRB.CreateLoad(IntptrTy, ea);
  auto val2 = ctx.reg_value.load(vm);

  BasicBlock* taken_true = BasicBlock::Create(C, "taken_true", F);
  BasicBlock* taken_false = BasicBlock::Create(C, "taken_false", F);
  auto taken_cond = IRB.CreateICmpEQ(val1, val2);
  IRB.CreateCondBr(taken_cond, taken_true, taken_false, ctx.likely_false);

  // taken
  IRB.SetInsertPoint(taken_true);
  ctx.reg_value.store(vm, VALUE_INTPTR(scm_true));
  ctx.reg_cache_copy_only_value_and_cont(vm);
  IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));

  // not taken
  IRB.SetInsertPoint(taken_false);
  ctx.reg_value.store(vm, VALUE_INTPTR(scm_false));
  ctx.reg_cache_copy_only_value_and_cont(vm);
  IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));
}

void codegen_t::emit_set_iloc(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

  ctx.m_iloc_cache.erase(calc_iloc_index(ctx, operands));

  ctx.reg_cache_copy_only_value_and_env(vm);
  auto thunkType = FunctionType::get(VoidTy, {IntptrPtrTy, IntptrTy}, false);
  auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_set_iloc), thunkType->getPointerTo());
  IRB.CreateCall(thunkType, thunk, {vm, VALUE_INTPTR(operands)});
}

void codegen_t::emit_extend_unbound(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

  int argc = FIXNUM(operands);
  for (intptr_t i = 0; i < argc; i++) {
    emit_push_vm_stack(ctx, VALUE_INTPTR(scm_undef));
  }
  auto env = IRB.CreateBitOrPointerCast(ctx.reg_sp.load(vm), IntptrPtrTy);
  CREATE_STORE_ENV_REC(env, count, VALUE_INTPTR(argc));
  CREATE_STORE_ENV_REC(env, up, ctx.reg_env.load(vm));
  auto ea0 = IRB.CreateGEP(IntptrTy, IRB.CreateBitOrPointerCast(env, IntptrPtrTy), VALUE_INTPTR(sizeof(vm_env_rec_t) / sizeof(intptr_t)));
  auto ea1 = IRB.CreateBitOrPointerCast(ea0, IntptrTy);
  ctx.reg_sp.store(vm, ea1);
  ctx.reg_fp.store(vm, ea1);
  ctx.reg_env.store(vm, CREATE_LEA_ENV_REC(env, up));

  ctx.set_local_var_count(ctx.m_depth, argc);
}

void codegen_t::emit_enclose(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

  int argc = FIXNUM(operands);

  ctx.reg_cache_copy_only_env_and_fp(vm);
  auto thunkType = FunctionType::get(VoidTy, {IntptrPtrTy, IntptrTy}, false);
  auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_enclose), thunkType->getPointerTo());
  IRB.CreateCall(thunkType, thunk, {vm, VALUE_INTPTR(argc)});
  ctx.reg_cache_clear_only_sp();
}

void codegen_t::emit_push_close(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

  if (maybe_compile((scm_closure_t)operands)) m_usage.templates++;

  ctx.reg_cache_copy_except_value_and_fp(vm);
  auto thunkType = FunctionType::get(VoidTy, {IntptrPtrTy, IntptrTy}, false);
  auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_push_close), thunkType->getPointerTo());
  IRB.CreateCall(thunkType, thunk, {vm, VALUE_INTPTR(operands)});
  ctx.reg_cache_clear_only_env_and_sp();
}

void codegen_t::emit_ret_close(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

  if (maybe_compile((scm_closure_t)operands)) m_usage.templates++;

  ctx.reg_cache_copy(vm);
  auto thunkType = FunctionType::get(IntptrTy, {IntptrPtrTy, IntptrTy}, false);
  auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_ret_close), thunkType->getPointerTo());
  IRB.CreateCall(thunkType, thunk, {vm, VALUE_INTPTR(operands)});
  IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));
}

void codegen_t::emit_close(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

  if (maybe_compile((scm_closure_t)operands)) m_usage.templates++;

  ctx.reg_cache_copy_only_env_and_cont(vm);
  auto thunkType = FunctionType::get(VoidTy, {IntptrPtrTy, IntptrTy}, false);
  auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_close), thunkType->getPointerTo());
  IRB.CreateCall(thunkType, thunk, {vm, VALUE_INTPTR(operands)});
  ctx.reg_cache_clear_only_env_and_value();
}

void codegen_t::emit_push_close_local(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);

  emit_push_vm_stack(ctx, VALUE_INTPTR(CDR(operands)));

  BasicBlock* CONTINUE = BasicBlock::Create(C, "continue", F);
  IRB.CreateBr(CONTINUE);

  // continue emit code in operands
  char local_id[40];
  uuid_v4(local_id, sizeof(local_id));
  Function* L = Function::Create(FunctionType::get(IntptrTy, {IntptrPtrTy}, false), Function::PrivateLinkage, local_id, M);
#if USE_LLVM_ATTRIBUTES
  L->addFnAttr(Attribute::ArgMemOnly);
  L->addFnAttr(Attribute::NoUnwind);
  L->addFnAttr(Attribute::WillReturn);
  for (Argument& argument : L->args()) {
    argument.addAttr(Attribute::NoAlias);
    argument.addAttr(Attribute::NoCapture);
    argument.addAttr(Attribute::NoFree);
  }
#endif

  BasicBlock* LOCAL = BasicBlock::Create(C, "entry", L);
  int function_index = ctx.m_depth - 1 + (ctx.m_argc << 16);
  ctx.m_local_functions[function_index] = L;
  m_usage.locals++;

#if VERBOSE_CODEGEN
  printf("emit_push_close_local level = %d index = %d function_index = %x\n", ctx.m_depth, ctx.m_argc, function_index);
#endif

  context_t ctx2 = ctx;
  ctx2.m_function = L;
  int nargs = FIXNUM(CAR(CAR(operands))) + FIXNUM(CADR(CAR(operands)));
  ctx2.set_local_var_count(ctx2.m_depth, nargs);
  ctx2.m_depth++;
  ctx2.m_argc = 0;
  ctx2.reg_cache_clear();
  ctx2.m_iloc_cache.clear();

  IRB.SetInsertPoint(LOCAL);
  transform(ctx2, CDR(operands), true);

  IRB.SetInsertPoint(CONTINUE);
}

void codegen_t::emit_gloc(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

#if ENABLE_COMPILE_REFERENCE
  scm_obj_t obj = ((scm_gloc_t)operands)->value;
  if (CLOSUREP(obj)) {
    if (maybe_compile((scm_closure_t)obj)) m_usage.refs++;
  }
#endif

  auto gloc = IRB.CreateBitOrPointerCast(VALUE_INTPTR(operands), IntptrPtrTy);
  auto val = CREATE_LOAD_GLOC_REC(gloc, value);
  if (((scm_gloc_t)operands)->value == scm_undef) {
    BasicBlock* undef_true = BasicBlock::Create(C, "undef_ture", F);
    BasicBlock* CONTINUE = BasicBlock::Create(C, "continue", F);
    auto undef_cond = IRB.CreateICmpEQ(val, VALUE_INTPTR(scm_undef));
    IRB.CreateCondBr(undef_cond, undef_true, CONTINUE, ctx.likely_false);
    IRB.SetInsertPoint(undef_true);
    ctx.reg_cache_copy(vm);
    CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(inst));
    auto thunkType = FunctionType::get(VoidTy, {IntptrPtrTy, IntptrTy}, false);
    auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_error_gloc), thunkType->getPointerTo());
    IRB.CreateCall(thunkType, thunk, {vm, VALUE_INTPTR(operands)});
    IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_resume_loop));
    IRB.SetInsertPoint(CONTINUE);
  }
  ctx.reg_value.store(vm, val);
}

void codegen_t::emit_if_symbolp_ret_const(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

  auto value = ctx.reg_value.load(vm);

  BasicBlock* symbol_true = BasicBlock::Create(C, "symbol_true", F);
  BasicBlock* symbol_false = BasicBlock::Create(C, "symbol_false", F);
  emit_cond_symbolp(ctx, value, symbol_true, symbol_false);

  // taken
  IRB.SetInsertPoint(symbol_true);
  ctx.reg_value.store(vm, VALUE_INTPTR(operands));
  ctx.reg_cache_copy_only_value_and_cont(vm);
  IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));

  // not taken
  IRB.SetInsertPoint(symbol_false);
}

void codegen_t::emit_if_pairp_ret_const(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

  auto value = ctx.reg_value.load(vm);

  BasicBlock* pair_true = BasicBlock::Create(C, "pair_true", F);
  BasicBlock* pair_false = BasicBlock::Create(C, "pair_false", F);
  emit_cond_pairp(ctx, value, pair_true, pair_false);

  // taken
  IRB.SetInsertPoint(pair_true);
  ctx.reg_value.store(vm, VALUE_INTPTR(operands));
  ctx.reg_cache_copy_only_value_and_cont(vm);
  IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));

  // not taken
  IRB.SetInsertPoint(pair_false);
}

void codegen_t::emit_if_not_nullp_ret_const(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

  auto value = ctx.reg_value.load(vm);

  BasicBlock* taken_true = BasicBlock::Create(C, "taken_true", F);
  BasicBlock* taken_false = BasicBlock::Create(C, "taken_false", F);
  auto taken_cond = IRB.CreateICmpNE(value, VALUE_INTPTR(scm_nil));
  IRB.CreateCondBr(taken_cond, taken_true, taken_false, ctx.likely_false);

  // taken
  IRB.SetInsertPoint(taken_true);
  ctx.reg_value.store(vm, VALUE_INTPTR(operands));
  ctx.reg_cache_copy_only_value_and_cont(vm);
  IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));

  // not taken
  IRB.SetInsertPoint(taken_false);
}

void codegen_t::emit_if_not_symbolp_ret_const(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

  auto value = ctx.reg_value.load(vm);

  BasicBlock* symbol_true = BasicBlock::Create(C, "symbol_true", F);
  BasicBlock* symbol_false = BasicBlock::Create(C, "symbol_false", F);
  emit_cond_symbolp(ctx, value, symbol_true, symbol_false);

  // taken
  IRB.SetInsertPoint(symbol_false);
  ctx.reg_value.store(vm, VALUE_INTPTR(operands));
  ctx.reg_cache_copy_only_value_and_cont(vm);
  IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));

  // not taken
  IRB.SetInsertPoint(symbol_true);
}

void codegen_t::emit_nadd_iloc(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

  auto retval = emit_alloca(ctx, IntptrTy);

  BasicBlock* CONTINUE = BasicBlock::Create(C, "continue", F);
  BasicBlock* fixnum_true = BasicBlock::Create(C, "fixnum_true", F);
  BasicBlock* fallback = BasicBlock::Create(C, "fallback", F);
#if USE_ILOC_CACHE
  auto val = emit_load_iloc(ctx, CAR(operands));
#else
  auto val = IRB.CreateLoad(IntptrTy, emit_lookup_iloc(ctx, CAR(operands)));
#endif
  auto fixnum_cond = IRB.CreateICmpNE(IRB.CreateAnd(val, 1), VALUE_INTPTR(0));
  IRB.CreateCondBr(fixnum_cond, fixnum_true, fallback);

  // fixnum
  IRB.SetInsertPoint(fixnum_true);
  auto intr = Intrinsic::getDeclaration(ctx.m_module, llvm::Intrinsic::ID(Intrinsic::sadd_with_overflow), {IntptrTy});
  auto rs = IRB.CreateCall(intr, {val, VALUE_INTPTR((uintptr_t)CADR(operands) - 1)});
  auto ans = IRB.CreateExtractValue(rs, {0});
  auto overflow = IRB.CreateExtractValue(rs, {1});
  auto valid_cond = IRB.CreateICmpEQ(overflow, IRB.getInt1(false));
  BasicBlock* valid_true = BasicBlock::Create(C, "valid_true", F);
  IRB.CreateCondBr(valid_cond, valid_true, fallback, ctx.likely_true);
  IRB.SetInsertPoint(valid_true);
  IRB.CreateStore(ans, retval);
  IRB.CreateBr(CONTINUE);

  // fallback
  IRB.SetInsertPoint(fallback);
  ctx.reg_cache_copy_except_value(vm);

  auto thunkType = FunctionType::get(IntptrTy, {IntptrPtrTy, IntptrTy}, false);
  auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_nadd_iloc), thunkType->getPointerTo());
  auto res = IRB.CreateCall(thunkType, thunk, {vm, VALUE_INTPTR(operands)});

  auto success_cond = IRB.CreateICmpNE(res, VALUE_INTPTR(0));
  BasicBlock* fallback_fail = BasicBlock::Create(C, "fallback_fail", F);
  BasicBlock* fallback_success = BasicBlock::Create(C, "fallback_success", F);
  IRB.CreateCondBr(success_cond, fallback_success, fallback_fail, ctx.likely_true);

  IRB.SetInsertPoint(fallback_fail);
  IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_resume_loop));

  IRB.SetInsertPoint(fallback_success);
  IRB.CreateStore(res, retval);
  IRB.CreateBr(CONTINUE);

  IRB.SetInsertPoint(CONTINUE);
  ctx.reg_value.store(vm, IRB.CreateLoad(IntptrTy, retval));
}

void codegen_t::emit_extend_enclose(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

  ctx.reg_cache_copy_except_value(vm);
  auto thunkType = FunctionType::get(VoidTy, {IntptrPtrTy, IntptrTy}, false);
  auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_extend_enclose), thunkType->getPointerTo());
  IRB.CreateCall(thunkType, thunk, {vm, VALUE_INTPTR(operands)});
  ctx.set_local_var_count(ctx.m_depth, 1);
  ctx.reg_cache_clear_except_value_and_cont();
}

void codegen_t::emit_if_symbolp(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

  BasicBlock* taken_true = BasicBlock::Create(C, "taken_true", F);
  BasicBlock* taken_false = BasicBlock::Create(C, "taken_false", F);

  emit_cond_symbolp(ctx, ctx.reg_value.load(vm), taken_true, taken_false);

  // taken
  IRB.SetInsertPoint(taken_true);
  transform(ctx, operands, false);

  // not taken
  IRB.SetInsertPoint(taken_false);
}

void codegen_t::emit_apply(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  auto vm = F->arg_begin();

  ctx.reg_cache_copy(vm);
  IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_apply));
}

void codegen_t::emit_escape(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  auto vm = F->arg_begin();

  ctx.reg_cache_copy(vm);
  ctx.reg_cache_clear();
  IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_escape));
}

void codegen_t::emit_push_subr(context_t& ctx, scm_obj_t inst, scm_subr_t subr) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

  BasicBlock* CONTINUE = BasicBlock::Create(C, "continue", F);

  intptr_t argc = FIXNUM(CADR(operands));
  auto sp = ctx.reg_sp.load(vm);
  auto argv = IRB.CreateGEP(IntptrTy, IRB.CreateBitOrPointerCast(sp, IntptrPtrTy), VALUE_INTPTR(-argc));

  CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(inst));
  auto procType = FunctionType::get(IntptrTy, {IntptrPtrTy, IntptrTy, IntptrPtrTy}, false);
  auto proc = ConstantExpr::getIntToPtr(VALUE_INTPTR(subr->adrs), procType->getPointerTo());
  auto val = IRB.CreateCall(procType, proc, {vm, VALUE_INTPTR(argc), argv});

  ctx.reg_sp.store(vm, IRB.CreateBitOrPointerCast(argv, IntptrTy));
  emit_push_vm_stack(ctx, val);

  BasicBlock* undef_true = BasicBlock::Create(C, "undef_true", F);
  auto undef_cond = IRB.CreateICmpEQ(val, VALUE_INTPTR(scm_undef));
  IRB.CreateCondBr(undef_cond, undef_true, CONTINUE, ctx.likely_false);

  // invalid
  IRB.SetInsertPoint(undef_true);
  ctx.reg_cache_copy_except_sp(vm);
  IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_resume_loop));

  IRB.SetInsertPoint(CONTINUE);
}

void codegen_t::emit_push_subr(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);

  emit_push_subr(ctx, inst, (scm_subr_t)CAR(operands));
}

void codegen_t::emit_subr(context_t& ctx, scm_obj_t inst, scm_subr_t subr) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);
  auto vm = F->arg_begin();

  BasicBlock* CONTINUE = BasicBlock::Create(C, "continue", F);

  intptr_t argc = FIXNUM(CADR(operands));
  auto sp = ctx.reg_sp.load(vm);
  auto argv = IRB.CreateGEP(IntptrTy, IRB.CreateBitOrPointerCast(sp, IntptrPtrTy), VALUE_INTPTR(-argc));

  CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(inst));
  auto procType = FunctionType::get(IntptrTy, {IntptrPtrTy, IntptrTy, IntptrPtrTy}, false);
  auto proc = ConstantExpr::getIntToPtr(VALUE_INTPTR(subr->adrs), procType->getPointerTo());
  auto val = IRB.CreateCall(procType, proc, {vm, VALUE_INTPTR(argc), argv});

  ctx.reg_sp.store(vm, IRB.CreateBitOrPointerCast(argv, IntptrTy));
  ctx.reg_value.store(vm, val);

  BasicBlock* undef_true = BasicBlock::Create(C, "undef_true", F);
  auto undef_cond = IRB.CreateICmpEQ(val, VALUE_INTPTR(scm_undef));
  IRB.CreateCondBr(undef_cond, undef_true, CONTINUE, ctx.likely_false);

  // invalid
  IRB.SetInsertPoint(undef_true);
  ctx.reg_cache_copy_except_sp(vm);
  IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_resume_loop));

  IRB.SetInsertPoint(CONTINUE);
}

void codegen_t::emit_subr(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);

  emit_subr(ctx, inst, (scm_subr_t)CAR(operands));
}

void codegen_t::emit_ret_subr(context_t& ctx, scm_obj_t inst, scm_subr_t subr) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  auto vm = F->arg_begin();

  auto fp = ctx.reg_fp.load(vm);
  auto argc = VALUE_INTPTR(ctx.m_argc);

  CREATE_STORE_VM_REG(vm, m_pc, VALUE_INTPTR(inst));
  auto procType = FunctionType::get(IntptrTy, {IntptrPtrTy, IntptrTy, IntptrTy}, false);
  auto proc = ConstantExpr::getIntToPtr(VALUE_INTPTR(subr->adrs), procType->getPointerTo());
  auto val = IRB.CreateCall(procType, proc, {vm, argc, fp});

  ctx.reg_value.store(vm, val);

  BasicBlock* undef_true = BasicBlock::Create(C, "undef_true", F);
  BasicBlock* undef_false = BasicBlock::Create(C, "undef_false", F);
  auto undef_cond = IRB.CreateICmpEQ(val, VALUE_INTPTR(scm_undef));
  IRB.CreateCondBr(undef_cond, undef_true, undef_false, ctx.likely_false);

  // valid
  IRB.SetInsertPoint(undef_false);
  ctx.reg_cache_copy_only_value_and_cont(vm);
  IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_pop_cont));

  // invalid
  IRB.SetInsertPoint(undef_true);
  ctx.reg_cache_copy_except_sp(vm);
  IRB.CreateRet(VALUE_INTPTR(VM::native_thunk_resume_loop));
}

void codegen_t::emit_ret_subr(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);

  emit_ret_subr(ctx, inst, (scm_subr_t)CAR(operands));
}

void codegen_t::emit_cond_pairp(context_t& ctx, Value* obj, BasicBlock* pair_true, BasicBlock* pair_false) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;

  BasicBlock* cond1_true = BasicBlock::Create(C, "cond1_true", F);
  auto cond1 = IRB.CreateICmpEQ(IRB.CreateAnd(obj, VALUE_INTPTR(0x7)), VALUE_INTPTR(0x0));
  IRB.CreateCondBr(cond1, cond1_true, pair_false, ctx.likely_true);
  IRB.SetInsertPoint(cond1_true);
  auto hdr = IRB.CreateLoad(IntptrTy, IRB.CreateBitOrPointerCast(obj, IntptrPtrTy));
  auto cond2 = IRB.CreateICmpNE(IRB.CreateAnd(hdr, VALUE_INTPTR(HDR_ATTR_MASKBITS)), VALUE_INTPTR(HDR_ATTR_BOXED));
  IRB.CreateCondBr(cond2, pair_true, pair_false, ctx.likely_true);
}

void codegen_t::emit_cond_symbolp(context_t& ctx, Value* obj, BasicBlock* symbol_true, BasicBlock* symbol_false) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;

  BasicBlock* cond1_true = BasicBlock::Create(C, "cond1_true", F);
  auto cond1 = IRB.CreateICmpEQ(IRB.CreateAnd(obj, VALUE_INTPTR(0x7)), VALUE_INTPTR(0x0));
  IRB.CreateCondBr(cond1, cond1_true, symbol_false, ctx.likely_true);
  IRB.SetInsertPoint(cond1_true);
  auto hdr = IRB.CreateLoad(IntptrTy, IRB.CreateBitOrPointerCast(obj, IntptrPtrTy));
  auto cond2 = IRB.CreateICmpEQ(IRB.CreateAnd(hdr, VALUE_INTPTR(HDR_TYPE_MASKBITS)), VALUE_INTPTR(scm_hdr_symbol));
  IRB.CreateCondBr(cond2, symbol_true, symbol_false, ctx.likely_true);
}

void codegen_t::emit_subr_gloc(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);

  emit_subr(ctx, inst, (scm_subr_t)(((scm_gloc_t)CAR(operands))->value));
}

void codegen_t::emit_push_subr_gloc(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);

  emit_push_subr(ctx, inst, (scm_subr_t)(((scm_gloc_t)CAR(operands))->value));
}

void codegen_t::emit_ret_subr_gloc(context_t& ctx, scm_obj_t inst) {
  DECLEAR_CONTEXT_VARS;
  DECLEAR_COMMON_TYPES;
  scm_obj_t operands = CDAR(inst);

  emit_ret_subr(ctx, inst, (scm_subr_t)(((scm_gloc_t)CAR(operands))->value));
}
