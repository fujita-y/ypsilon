// Copyright (c) 2004-2022 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "arith.h"
#include "file.h"
#include "hash.h"
#include "heap.h"
#include "subr.h"
#include "uuid.h"
#include "violation.h"
#include "vm.h"

#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/Error.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/InitLLVM.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>

#if INTPTR_MAX == INT32_MAX
  #define VALUE_INTPTR(_VAL_) IRB.getInt32((intptr_t)(_VAL_))
#elif INTPTR_MAX == INT64_MAX
  #define VALUE_INTPTR(_VAL_) IRB.getInt64((intptr_t)(_VAL_))
#else
  #error unsupported intptr_t size
#endif

using namespace llvm;
using namespace llvm::orc;
using namespace llvm::sys;

static mutex_t s_compile_lock;
static std::map<std::string, void*> s_callout_cache;
static std::unique_ptr<LLJIT> s_c_ffi;
static std::atomic<uintptr_t> s_trampoline_uid;

static ExitOnError ExitOnErr;

extern "C" {
  bool c_ffi_to_llvm_Int1Ty(VM* vm, int i, scm_obj_t obj[]) { return obj[i] == scm_true; }
  int8_t c_ffi_to_llvm_Int8Ty(VM* vm, int i, scm_obj_t obj[]) {
    if (FIXNUMP(obj[i])) return FIXNUM(obj[i]);
    if (exact_integer_pred(obj[i])) return coerce_exact_integer_to_intptr(obj[i]);
    if (real_pred(obj[i])) return real_to_double(obj[i]);
    return 0;
  }
  int16_t c_ffi_to_llvm_Int16Ty(VM* vm, int i, scm_obj_t obj[]) {
    if (FIXNUMP(obj[i])) return FIXNUM(obj[i]);
    if (exact_integer_pred(obj[i])) return coerce_exact_integer_to_intptr(obj[i]);
    if (real_pred(obj[i])) return real_to_double(obj[i]);
    return 0;
  }
  int32_t c_ffi_to_llvm_Int32Ty(VM* vm, int i, scm_obj_t obj[]) {
    if (FIXNUMP(obj[i])) return FIXNUM(obj[i]);
    if (exact_integer_pred(obj[i])) return coerce_exact_integer_to_intptr(obj[i]);
    if (real_pred(obj[i])) return real_to_double(obj[i]);
#if INTPTR_MAX == INT32_MAX
    if (BVECTORP(obj[i])) {
      scm_bvector_t bvector = (scm_bvector_t)obj[i];
      return (int64_t)bvector->elts;
    }
#endif
    return 0;
  }
  int64_t c_ffi_to_llvm_Int64Ty(VM* vm, int i, scm_obj_t obj[]) {
    if (FIXNUMP(obj[i])) return FIXNUM(obj[i]);
    if (exact_integer_pred(obj[i])) return coerce_exact_integer_to_int64(obj[i]);
    if (real_pred(obj[i])) return real_to_double(obj[i]);
#if INTPTR_MAX == INT64_MAX
    if (BVECTORP(obj[i])) {
      scm_bvector_t bvector = (scm_bvector_t)obj[i];
      return (int64_t)bvector->elts;
    }
#endif
    return 0;
  }
  float c_ffi_to_llvm_FloatTy(VM* vm, int i, scm_obj_t obj[]) {
    if (FLONUMP(obj[i])) return FLONUM(obj[i]);
    if (FIXNUMP(obj[i])) return FIXNUM(obj[i]);
    if (real_pred(obj[i])) return real_to_double(obj[i]);
    return 0;
  }
  double c_ffi_to_llvm_DoubleTy(VM* vm, int i, scm_obj_t obj[]) {
    if (FLONUMP(obj[i])) return FLONUM(obj[i]);
    if (FIXNUMP(obj[i])) return FIXNUM(obj[i]);
    if (real_pred(obj[i])) return real_to_double(obj[i]);
    return 0;
  }

  bool c_ffi_ret_llvm_Int1Ty(VM* vm, scm_obj_t obj) { return obj == scm_true; }
  int8_t c_ffi_ret_llvm_Int8Ty(VM* vm, scm_obj_t obj) {
    if (FIXNUMP(obj)) return FIXNUM(obj);
    scm_obj_t argv[] = {obj};
    return c_ffi_to_llvm_Int8Ty(vm, 0, argv);
  }
  int16_t c_ffi_ret_llvm_Int16Ty(VM* vm, scm_obj_t obj) {
    if (FIXNUMP(obj)) return FIXNUM(obj);
    scm_obj_t argv[] = {obj};
    return c_ffi_to_llvm_Int16Ty(vm, 0, argv);
  }
  int32_t c_ffi_ret_llvm_Int32Ty(VM* vm, scm_obj_t obj) {
    if (FIXNUMP(obj)) return FIXNUM(obj);
    scm_obj_t argv[] = {obj};
    return c_ffi_to_llvm_Int32Ty(vm, 0, argv);
  }
  int64_t c_ffi_ret_llvm_Int64Ty(VM* vm, scm_obj_t obj) {
    if (FIXNUMP(obj)) return FIXNUM(obj);
    scm_obj_t argv[] = {obj};
    return c_ffi_to_llvm_Int64Ty(vm, 0, argv);
  }
  float c_ffi_ret_llvm_FloatTy(VM* vm, scm_obj_t obj) {
    if (FLONUMP(obj)) return FLONUM(obj);
    if (FIXNUMP(obj)) return FIXNUM(obj);
    scm_obj_t argv[] = {obj};
    return c_ffi_to_llvm_FloatTy(vm, 0, argv);
  }
  double c_ffi_ret_llvm_DoubleTy(VM* vm, scm_obj_t obj) {
    if (FLONUMP(obj)) return FLONUM(obj);
    if (FIXNUMP(obj)) return FIXNUM(obj);
    scm_obj_t argv[] = {obj};
    return c_ffi_to_llvm_DoubleTy(vm, 0, argv);
  }

  scm_obj_t c_ffi_from_llvm_Int1Ty(VM* vm, bool val) { return val ? scm_true : scm_false; }
  scm_obj_t c_ffi_from_llvm_Int8Ty(VM* vm, int8_t val) { return MAKEFIXNUM(val); }
  scm_obj_t c_ffi_from_llvm_Int16Ty(VM* vm, int16_t val) { return MAKEFIXNUM(val); }
  scm_obj_t c_ffi_from_llvm_Int32Ty(VM* vm, int32_t val) { return int32_to_integer(vm->m_heap, val); }
  scm_obj_t c_ffi_from_llvm_Int64Ty(VM* vm, int64_t val) { return int64_to_integer(vm->m_heap, val); }
  scm_obj_t c_ffi_from_llvm_FloatTy(VM* vm, float val) { return double_to_inexact(vm->m_heap, val); }
  scm_obj_t c_ffi_from_llvm_DoubleTy(VM* vm, double val) { return double_to_inexact(vm->m_heap, val); }

  scm_obj_t c_call_scheme(VM* vm, intptr_t trampoline_uid, intptr_t argc, ...) {
    try {
      scm_obj_t* param = (scm_obj_t*)alloca(sizeof(scm_obj_t) * argc);
      va_list ap;
      va_start(ap, argc);
      for (int i = 0; i < argc; i++) param[i] = va_arg(ap, scm_obj_t);
      va_end(ap);
      scm_obj_t closure = scm_undef;
      {
        scoped_lock lock(vm->m_heap->m_trampolines->lock);
        closure = get_hashtable(vm->m_heap->m_trampolines, MAKEFIXNUM(trampoline_uid));
      }
      if (!CLOSUREP(closure)) fatal("fatal: callback exists in diffrent thread or destroyed\n[exit]\n");
      return vm->call_scheme_argv(closure, argc, param);
    } catch (...) {
      fatal("fatal: unhandled exception in callback\n[exit]\n");
    }
  }
}

void init_c_ffi() {
  s_compile_lock.init();
  auto J = ExitOnErr(LLJITBuilder().create());
  auto D = J->getDataLayout();
  auto G = ExitOnErr(DynamicLibrarySearchGenerator::GetForCurrentProcess(D.getGlobalPrefix()));
  J->getMainJITDylib().addGenerator(std::move(G));
  s_c_ffi = std::move(J);
}

void destroy_c_ffi() {
  if (s_c_ffi) {
    delete s_c_ffi.release();
    s_c_ffi = NULL;
  }
  s_callout_cache.clear();
  s_compile_lock.destroy();
}

static Type* builtin_type(LLVMContext& C, char code) {
  switch (code) {
    case 'i':
      return Type::getVoidTy(C);
    case 'b':
      return Type::getInt1Ty(C);
    case 'u':
      return Type::getInt8Ty(C);
    case 'd':
      return Type::getInt16Ty(C);
    case 'q':
      return Type::getInt32Ty(C);
    case 'o':
      return Type::getInt64Ty(C);
    case 's':
      return Type::getFloatTy(C);
    case 'x':
      return Type::getDoubleTy(C);
    default:
      fatal("%s:%u wrong type code: %c", __FILE__, __LINE__, code);
  }
}

static FunctionType* function_type(LLVMContext& C, const char* signature, bool variadic) {
  std::vector<llvm::Type*> paramTypes;
  int i = 1;
  while (signature[i]) {
    paramTypes.push_back(builtin_type(C, signature[i]));
    i++;
  }
  return FunctionType::get(builtin_type(C, signature[0]), paramTypes, variadic);
}

static llvm::FunctionCallee make_callee(IRBuilder<>& IRB, FunctionType* funcType, void* adrs) {
  return FunctionCallee(funcType, ConstantExpr::getIntToPtr(VALUE_INTPTR(adrs), funcType->getPointerTo()));
}

#define THUNK_TO_LLVM(_NAME_, _TYPE_) \
  make_callee(IRB, FunctionType::get(Type::get##_TYPE_(C), {IntptrTy, IntptrTy, IntptrTy}, false), (void*)_NAME_)
#define THUNK_RET_LLVM(_NAME_, _TYPE_)  make_callee(IRB, FunctionType::get(Type::get##_TYPE_(C), {IntptrTy, IntptrTy}, false), (void*)_NAME_)
#define THUNK_FROM_LLVM(_NAME_, _TYPE_) make_callee(IRB, FunctionType::get(IntptrTy, {IntptrTy, Type::get##_TYPE_(C)}, false), (void*)_NAME_)

static std::map<char, FunctionCallee> create_thunk_to_map(Module* M, IRBuilder<>& IRB, LLVMContext& C) {
  std::map<char, FunctionCallee> to;
  auto IntptrTy = (sizeof(intptr_t) == 4 ? Type::getInt32Ty(C) : Type::getInt64Ty(C));
  to['b'] = THUNK_TO_LLVM(c_ffi_to_llvm_Int1Ty, Int1Ty);
  to['u'] = THUNK_TO_LLVM(c_ffi_to_llvm_Int8Ty, Int8Ty);
  to['d'] = THUNK_TO_LLVM(c_ffi_to_llvm_Int16Ty, Int16Ty);
  to['q'] = THUNK_TO_LLVM(c_ffi_to_llvm_Int32Ty, Int32Ty);
  to['o'] = THUNK_TO_LLVM(c_ffi_to_llvm_Int64Ty, Int64Ty);
  to['s'] = THUNK_TO_LLVM(c_ffi_to_llvm_FloatTy, FloatTy);
  to['x'] = THUNK_TO_LLVM(c_ffi_to_llvm_DoubleTy, DoubleTy);
  return to;
}

static std::map<char, FunctionCallee> create_thunk_ret_map(Module* M, IRBuilder<>& IRB, LLVMContext& C) {
  std::map<char, FunctionCallee> ret;
  auto IntptrTy = (sizeof(intptr_t) == 4 ? Type::getInt32Ty(C) : Type::getInt64Ty(C));
  ret['b'] = THUNK_RET_LLVM(c_ffi_ret_llvm_Int1Ty, Int1Ty);
  ret['u'] = THUNK_RET_LLVM(c_ffi_ret_llvm_Int8Ty, Int8Ty);
  ret['d'] = THUNK_RET_LLVM(c_ffi_ret_llvm_Int16Ty, Int16Ty);
  ret['q'] = THUNK_RET_LLVM(c_ffi_ret_llvm_Int32Ty, Int32Ty);
  ret['o'] = THUNK_RET_LLVM(c_ffi_ret_llvm_Int64Ty, Int64Ty);
  ret['s'] = THUNK_RET_LLVM(c_ffi_ret_llvm_FloatTy, FloatTy);
  ret['x'] = THUNK_RET_LLVM(c_ffi_ret_llvm_DoubleTy, DoubleTy);
  return ret;
}

static std::map<char, FunctionCallee> create_thunk_from_map(Module* M, IRBuilder<>& IRB, LLVMContext& C) {
  std::map<char, FunctionCallee> from;
  auto IntptrTy = (sizeof(intptr_t) == 4 ? Type::getInt32Ty(C) : Type::getInt64Ty(C));
  from['b'] = THUNK_FROM_LLVM(c_ffi_from_llvm_Int1Ty, Int1Ty);
  from['u'] = THUNK_FROM_LLVM(c_ffi_from_llvm_Int8Ty, Int8Ty);
  from['d'] = THUNK_FROM_LLVM(c_ffi_from_llvm_Int16Ty, Int16Ty);
  from['q'] = THUNK_FROM_LLVM(c_ffi_from_llvm_Int32Ty, Int32Ty);
  from['o'] = THUNK_FROM_LLVM(c_ffi_from_llvm_Int64Ty, Int64Ty);
  from['s'] = THUNK_FROM_LLVM(c_ffi_from_llvm_FloatTy, FloatTy);
  from['x'] = THUNK_FROM_LLVM(c_ffi_from_llvm_DoubleTy, DoubleTy);
  return from;
}

static void* compile_callout_thunk(uintptr_t adrs, const char* caller_signature, const char* callee_signature) {
  scoped_lock lock(s_compile_lock);

  char cache_key[256];
  snprintf(cache_key, sizeof(cache_key), "%s:%s:%p", caller_signature, callee_signature, (void*)adrs);
  if (s_callout_cache[cache_key]) return s_callout_cache[cache_key];
  char module_id[40];
  char function_id[40];
  uuid_v4(module_id, sizeof(module_id));
  uuid_v4(function_id, sizeof(function_id));
  auto Context = std::make_unique<LLVMContext>();
  LLVMContext& C = *Context;
  auto M = std::make_unique<Module>(module_id, C);
  M->setTargetTriple(getDefaultTargetTriple());
  M->setDataLayout(s_c_ffi->getDataLayout());

  auto IntptrTy = (sizeof(intptr_t) == 4 ? Type::getInt32Ty(C) : Type::getInt64Ty(C));
  auto IntptrPtrTy = sizeof(intptr_t) == 4 ? Type::getInt32PtrTy(C) : Type::getInt64PtrTy(C);

  Function* F =
      Function::Create(FunctionType::get(IntptrTy, {IntptrTy, IntptrTy, IntptrTy}, false), Function::ExternalLinkage, function_id, M.get());
  BasicBlock* ENTRY = BasicBlock::Create(C, "entry", F);
  IRBuilder<> IRB(ENTRY);
  std::map<char, llvm::FunctionCallee> thunk_to = create_thunk_to_map(M.get(), IRB, C);
  std::map<char, llvm::FunctionCallee> thunk_from = create_thunk_from_map(M.get(), IRB, C);

  auto vm = F->arg_begin();
  auto argv = F->arg_begin() + 2;

  std::vector<llvm::Value*> args;
  int n = strlen(caller_signature) - 1;
  for (int i = 0; i < n; i++) {
    char code = caller_signature[i + 1];
    if (!strchr("budqosx", code)) fatal("%s:%u wrong type code: %c", __FILE__, __LINE__, code);
    Value* value = IRB.CreateCall(thunk_to[code], {vm, VALUE_INTPTR(i), argv});
    args.push_back(value);
  }

  auto procType = function_type(C, callee_signature, strcmp(caller_signature, callee_signature) != 0);
  auto proc = ConstantExpr::getIntToPtr(VALUE_INTPTR(adrs), procType->getPointerTo());
  auto retval = IRB.CreateCall(procType, proc, args);
  if (callee_signature[0] == 'i') {
    IRB.CreateRet(VALUE_INTPTR(scm_unspecified));
  } else {
    char code = callee_signature[0];
    if (!strchr("budqosx", code)) fatal("%s:%u wrong type code: %c", __FILE__, __LINE__, code);
    IRB.CreateRet(IRB.CreateCall(thunk_from[code], {vm, retval}));
  }

  if (verifyModule(*M, &outs())) fatal("%s:%u verify module failed", __FILE__, __LINE__);
  ExitOnErr(s_c_ffi->addIRModule(std::move(ThreadSafeModule(std::move(M), std::move(Context)))));
  auto symbol = ExitOnErr(s_c_ffi->lookup(function_id));
  void* ptr = (void*)symbol.getAddress();
  s_callout_cache[cache_key] = ptr;
  return ptr;
}

static void* compile_callback_thunk(VM* vm, uintptr_t trampoline_uid, const char* signature) {
  scoped_lock lock(s_compile_lock);
  char module_id[40];
  char function_id[40];
  uuid_v4(module_id, sizeof(module_id));
  uuid_v4(function_id, sizeof(function_id));
  auto Context = std::make_unique<LLVMContext>();
  LLVMContext& C = *Context;
  auto M = std::make_unique<Module>(module_id, C);
  auto IntptrTy = (sizeof(intptr_t) == 4 ? Type::getInt32Ty(C) : Type::getInt64Ty(C));
  auto IntptrPtrTy = sizeof(intptr_t) == 4 ? Type::getInt32PtrTy(C) : Type::getInt64PtrTy(C);

  auto callbackFunctionType = function_type(C, signature, false);
  Function* F = Function::Create(callbackFunctionType, Function::ExternalLinkage, function_id, M.get());
  BasicBlock* ENTRY = BasicBlock::Create(C, "entry", F);
  IRBuilder<> IRB(ENTRY);
  std::map<char, llvm::FunctionCallee> thunk_ret = create_thunk_ret_map(M.get(), IRB, C);
  std::map<char, llvm::FunctionCallee> thunk_from = create_thunk_from_map(M.get(), IRB, C);

  std::vector<llvm::Value*> args;
  args.push_back(VALUE_INTPTR(vm));
  args.push_back(VALUE_INTPTR(trampoline_uid));
  args.push_back(VALUE_INTPTR(strlen(signature) - 1));
  int n = strlen(signature) - 1;
  for (int i = 0; i < n; i++) {
    char code = signature[i + 1];
    if (!strchr("budqosx", code)) fatal("%s:%u wrong type code: %c", __FILE__, __LINE__, code);
    Value* value = IRB.CreateCall(thunk_from[code], {VALUE_INTPTR(vm), F->arg_begin() + i});
    args.push_back(value);
  }

  auto thunkType = FunctionType::get(IntptrTy, {IntptrTy, IntptrTy, IntptrTy}, true);
  auto thunk = ConstantExpr::getIntToPtr(VALUE_INTPTR(c_call_scheme), thunkType->getPointerTo());
  Value* retval = IRB.CreateCall(thunkType, thunk, args);

  if (signature[0] == 'i') {
    IRB.CreateRetVoid();
  } else {
    char code = signature[0];
    if (!strchr("budqosx", code)) fatal("%s:%u wrong type code: %c", __FILE__, __LINE__, code);
    IRB.CreateRet(IRB.CreateCall(thunk_ret[code], {VALUE_INTPTR(vm), retval}));
  }

  if (verifyModule(*M, &outs())) fatal("%s:%u verify module failed", __FILE__, __LINE__);

  // M->print(outs(), nullptr);

  ExitOnErr(s_c_ffi->addIRModule(std::move(ThreadSafeModule(std::move(M), std::move(Context)))));
  auto symbol = ExitOnErr(s_c_ffi->lookup(function_id));
  return (void*)symbol.getAddress();
}

// codegen-cdecl-callout
scm_obj_t subr_codegen_cdecl_callout(VM* vm, int argc, scm_obj_t argv[]) {
  if (argc == 2 || argc == 3) {
    if (exact_non_negative_integer_pred(argv[0])) {
      uintptr_t adrs;
      exact_integer_to_uintptr(argv[0], &adrs);
      if (adrs) {
        if (STRINGP(argv[1])) {
          const char* caller_signature = ((scm_string_t)argv[1])->name;
          if (argc == 2) {
            void* thunk = compile_callout_thunk(adrs, caller_signature, caller_signature);
            char buf[32];
            snprintf(buf, sizeof(buf), "%p", thunk);
            return make_subr(vm->m_heap, (subr_proc_t)thunk, make_symbol(vm->m_heap, buf));
          }
          if (STRINGP(argv[2])) {
            const char* callee_signature = ((scm_string_t)argv[2])->name;
            void* thunk = compile_callout_thunk(adrs, caller_signature, callee_signature);
            char buf[32];
            snprintf(buf, sizeof(buf), "%p", thunk);
            return make_subr(vm->m_heap, (subr_proc_t)thunk, make_symbol(vm->m_heap, buf));
          }
          wrong_type_argument_violation(vm, "codegen-cdecl-callout", 2, "string", argv[2], argc, argv);
          return scm_undef;
        }
        wrong_type_argument_violation(vm, "codegen-cdecl-callout", 1, "string", argv[1], argc, argv);
        return scm_undef;
      }
      wrong_type_argument_violation(vm, "codegen-cdecl-callout", 0, "c-function address", argv[1], argc, argv);
      return scm_undef;
    }
    wrong_type_argument_violation(vm, "codegen-cdecl-callout", 0, "exact non-negative integer", argv[0], argc, argv);
    return scm_undef;
  }
  wrong_number_of_arguments_violation(vm, "codegen-cdecl-callout", 2, 3, argc, argv);
  return scm_undef;
}

// codegen-cdecl-callback
scm_obj_t subr_codegen_cdecl_callback(VM* vm, int argc, scm_obj_t argv[]) {
  if (argc == 2) {
    if (CLOSUREP(argv[0])) {
      scm_closure_t closure = (scm_closure_t)argv[0];
      if (STRINGP(argv[1])) {
        uintptr_t uid = s_trampoline_uid++;
        const char* signature = ((scm_string_t)argv[1])->name;
        void* thunk = compile_callback_thunk(vm, uid, signature);
        vm->m_heap->write_barrier(closure);
        {
          scoped_lock lock(vm->m_heap->m_trampolines->lock);
          int nsize = put_hashtable(vm->m_heap->m_trampolines, MAKEFIXNUM(uid), closure);
          if (nsize) rehash_hashtable(vm->m_heap, vm->m_heap->m_trampolines, nsize);
        }
        return uintptr_to_integer(vm->m_heap, (uintptr_t)thunk);
      }
      wrong_type_argument_violation(vm, "codegen-cdecl-callback", 1, "string", argv[1], argc, argv);
      return scm_undef;
    }
    wrong_type_argument_violation(vm, "codegen-cdecl-callback", 0, "closure", argv[0], argc, argv);
    return scm_undef;
  }
  wrong_number_of_arguments_violation(vm, "codegen-cdecl-callback", 2, 2, argc, argv);
  return scm_undef;
}

// c-main-argc
scm_obj_t subr_c_main_argc(VM* vm, int argc, scm_obj_t argv[]) {
  if (argc == 0) {
    return uintptr_to_integer(vm->m_heap, (uintptr_t)main_command_line_argc);
  }
  wrong_number_of_arguments_violation(vm, "c-main-argc", 0, 0, argc, argv);
  return scm_undef;
}

// c-main-argv
scm_obj_t subr_c_main_argv(VM* vm, int argc, scm_obj_t argv[]) {
  if (argc == 0) {
    return uintptr_to_integer(vm->m_heap, (uintptr_t)main_command_line_argv);
  }
  wrong_number_of_arguments_violation(vm, "c-main-argv", 0, 0, argc, argv);
  return scm_undef;
}

void init_subr_c_ffi(object_heap_t* heap) {
#define DEFSUBR(SYM, FUNC) heap->intern_system_subr(SYM, FUNC)

  DEFSUBR("codegen-cdecl-callout", subr_codegen_cdecl_callout);
  DEFSUBR("codegen-cdecl-callback", subr_codegen_cdecl_callback);
  DEFSUBR("c-main-argc", subr_c_main_argc);
  DEFSUBR("c-main-argv", subr_c_main_argv);
}
