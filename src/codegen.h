// Copyright (c) 2004-2022 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef CODEGEN_H_INCLUDED
#define CODEGEN_H_INCLUDED

#include "core.h"
#include "object.h"
#include "vm.h"

#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/MDBuilder.h>
#include <llvm/IR/Module.h>

#define USE_LLVM_ATTRIBUTES     1
#define USE_LLVM_OPTIMIZE       1
#define USE_ADDRESS_TO_FUNCTION 1
#define USE_ILOC_OPTIMIZE       1
#define USE_ILOC_CACHE          1
#define USE_REG_CACHE           1

#define PRINT_IR                0
#define DEBUG_CODEGEN           0
#define VERBOSE_CODEGEN         0

class codegen_t {
  struct context_t;

  template <int byte_offset> struct reg_cache_t {
    llvm::Value* val;
    bool need_write_back;
    llvm::IntegerType* IntptrTy;
    llvm::LLVMContext& C;
    llvm::IRBuilder<>& IRB;
    llvm::Value* load(llvm::Value* vm);
    void store(llvm::Value* vm, llvm::Value* rhs);
    void writeback(llvm::Value* vm);
    void copy(llvm::Value* vm);
    void clear();
    reg_cache_t(codegen_t::context_t* context);
  };

  struct context_t {
    llvm::LLVMContext& m_llvm_context;
    llvm::Module* m_module;
    llvm::Function* m_function;
    llvm::IRBuilder<>& m_irb;
    llvm::Function* m_top_level_function;
    scm_closure_t m_top_level_closure;
    std::map<int, llvm::Function*> m_local_functions;
    std::vector<int> m_local_var_count;
    int m_argc;
    int m_depth;
    std::map<int, llvm::Value*> m_iloc_cache;
    reg_cache_t<offsetof(VM, m_sp)> reg_sp;
    reg_cache_t<offsetof(VM, m_fp)> reg_fp;
    reg_cache_t<offsetof(VM, m_env)> reg_env;
    reg_cache_t<offsetof(VM, m_cont)> reg_cont;
    reg_cache_t<offsetof(VM, m_value)> reg_value;
    llvm::MDNode* likely_true;
    llvm::MDNode* likely_false;
    void reg_cache_copy(llvm::Value* vm);
    void reg_cache_copy_only_value(llvm::Value* vm);
    void reg_cache_copy_only_value_and_env(llvm::Value* vm);
    void reg_cache_copy_only_value_and_cont(llvm::Value* vm);
    void reg_cache_copy_only_env_and_cont(llvm::Value* vm);
    void reg_cache_copy_only_env_and_fp(llvm::Value* vm);
    void reg_cache_copy_except_sp(llvm::Value* vm);
    void reg_cache_copy_except_value(llvm::Value* vm);
    void reg_cache_copy_except_value_and_sp(llvm::Value* vm);
    void reg_cache_copy_except_value_and_fp(llvm::Value* vm);
    void reg_cache_clear();
    void reg_cache_clear_only_value();
    void reg_cache_clear_only_env_and_value();
    void reg_cache_clear_only_sp();
    void reg_cache_clear_only_env_and_sp();
    void reg_cache_clear_except_value_and_cont();
    void set_local_var_count(int depth, int count);
    void set_local_var_count(int depth, scm_closure_t closure);
    int get_local_var_count(int depth);
    llvm::MDNode* get_branch_weight(int n, int m);
    context_t(llvm::LLVMContext& llvm_context, llvm::IRBuilder<>& irb)
        : m_llvm_context(llvm_context),
          m_irb(irb),
          m_argc(0),
          m_depth(0),
          reg_sp(this),
          reg_fp(this),
          reg_env(this),
          reg_cont(this),
          reg_value(this) {
#if ENABLE_BRANCH_WEIGHTS
      likely_true = get_branch_weight(100, 1);
      likely_false = get_branch_weight(1, 100);
#else
      likely_true = get_branch_weight(50, 50);
      likely_false = get_branch_weight(50, 50);
#endif
    }
  };

  enum cc_t {
    LT,
    GT,
    LE,
    GE,
    EQ,
  };

  std::unique_ptr<llvm::orc::LLJIT> m_jit;
  std::map<scm_closure_t, llvm::Function*> m_lifted_functions;
  mutex_t m_compile_thread_lock;
  cond_t m_compile_thread_wake;
  bool m_compile_thread_ready;
  bool m_compile_thread_terminating;
  static thread_main_t compile_thread(void* param);
  void optimizeModule(llvm::Module& M);
  void transform(context_t ctx, scm_obj_t inst, bool insert_stack_check);
  llvm::Value* get_function_address(context_t& ctx, scm_closure_t closure);

 public:
  codegen_t();
  void init();
  void destroy();
  void compile(scm_closure_t closure);
  void display_codegen_statistics(scm_port_t port);
  bool m_debug;
  std::vector<scm_closure_t> m_compile_queue;
  mutex_t m_compile_queue_lock;

  struct usage_t {
    int globals;
    int locals;
    int inners;
    int templates;
    int refs;
    int on_demand;
    int skipped;
    uintptr_t min_sym;
    uintptr_t max_sym;
    usage_t() : globals(0), locals(0), inners(0), templates(0), refs(0), on_demand(0), skipped(0), min_sym(0), max_sym(0) {}
  } m_usage;

 private:
  int calc_stack_size(scm_obj_t inst);
  int calc_iloc_index(context_t& ctx, scm_obj_t operand);
  int calc_iloc_index(context_t& ctx, intptr_t depth, intptr_t index);
  bool maybe_compile(scm_closure_t closure);
  void compile_each(scm_closure_t closure);
  llvm::AllocaInst* emit_alloca(context_t& ctx, llvm::Type* type);
  void emit_stack_overflow_check(context_t& ctx, int nbytes);
  void emit_push_vm_stack(context_t& ctx, llvm::Value* val);
  void emit_prepair_apply(context_t& ctx, scm_closure_t closure);
  void emit_cond_pairp(context_t& ctx, llvm::Value* obj, llvm::BasicBlock* pair_true, llvm::BasicBlock* pair_false);
  void emit_cond_symbolp(context_t& ctx, llvm::Value* obj, llvm::BasicBlock* symbol_true, llvm::BasicBlock* symbol_false);
  llvm::Function* emit_inner_function(context_t& ctx, scm_closure_t closure);
  llvm::Value* emit_lookup_env(context_t& ctx, intptr_t depth);
  llvm::Value* emit_load_iloc(context_t& ctx, scm_obj_t operand);
  llvm::Value* emit_load_iloc(context_t& ctx, intptr_t depth, intptr_t index);
  llvm::Value* emit_lookup_iloc(context_t& ctx, intptr_t depth, intptr_t index);
  llvm::Value* emit_lookup_iloc(context_t& ctx, scm_obj_t inst);
  llvm::Value* emit_cmp_inst(context_t& ctx, cc_t cc, llvm::Value* lhs, llvm::Value* rhs);
  void emit_cc_n_iloc(context_t& ctx, scm_obj_t inst, cc_t cc, void* c_func);
  void emit_cc_iloc(context_t& ctx, scm_obj_t inst, cc_t cc, void* c_func);
  void emit_push_subr(context_t& ctx, scm_obj_t inst, scm_subr_t subr);
  void emit_subr(context_t& ctx, scm_obj_t inst, scm_subr_t subr);
  void emit_ret_subr(context_t& ctx, scm_obj_t inst, scm_subr_t subr);
  llvm::Function* emit_call(context_t& ctx, scm_obj_t inst);
  void emit_if_false_call(context_t& ctx, scm_obj_t inst);
  void emit_subr(context_t& ctx, scm_obj_t inst);
  void emit_push(context_t& ctx, scm_obj_t inst);
  void emit_push_const(context_t& ctx, scm_obj_t inst);
  void emit_push_iloc0(context_t& ctx, scm_obj_t inst);
  void emit_push_iloc1(context_t& ctx, scm_obj_t inst);
  void emit_push_gloc(context_t& ctx, scm_obj_t inst);
  void emit_push_subr(context_t& ctx, scm_obj_t inst);
  void emit_push_car_iloc(context_t& ctx, scm_obj_t inst);
  void emit_push_cdr_iloc(context_t& ctx, scm_obj_t inst);
  void emit_push_cadr_iloc(context_t& ctx, scm_obj_t inst);
  void emit_push_cddr_iloc(context_t& ctx, scm_obj_t inst);
  void emit_push_nadd_iloc(context_t& ctx, scm_obj_t inst);
  void emit_push_iloc(context_t& ctx, scm_obj_t inst);
  void emit_push_cons(context_t& ctx, scm_obj_t inst);
  void emit_push_close(context_t& ctx, scm_obj_t inst);
  void emit_apply_iloc(context_t& ctx, scm_obj_t inst);
  void emit_apply_gloc(context_t& ctx, scm_obj_t inst);
  void emit_ret_const(context_t& ctx, scm_obj_t inst);
  void emit_ret_iloc(context_t& ctx, scm_obj_t inst);
  void emit_ret_cons(context_t& ctx, scm_obj_t inst);
  void emit_ret_subr(context_t& ctx, scm_obj_t inst);
  void emit_if_true(context_t& ctx, scm_obj_t inst);
  void emit_if_nullp(context_t& ctx, scm_obj_t inst);
  void emit_if_nullp_ret_const(context_t& ctx, scm_obj_t inst);
  void emit_if_not_nullp_ret_const(context_t& ctx, scm_obj_t inst);
  void emit_if_symbolp_ret_const(context_t& ctx, scm_obj_t inst);
  void emit_if_not_symbolp_ret_const(context_t& ctx, scm_obj_t inst);
  void emit_if_pairp_ret_const(context_t& ctx, scm_obj_t inst);
  void emit_if_eqp_ret_const(context_t& ctx, scm_obj_t inst);
  void emit_if_true_ret(context_t& ctx, scm_obj_t inst);
  void emit_if_false_ret(context_t& ctx, scm_obj_t inst);
  void emit_if_eqp(context_t& ctx, scm_obj_t inst);
  void emit_gloc(context_t& ctx, scm_obj_t inst);
  void emit_iloc(context_t& ctx, scm_obj_t inst);
  void emit_iloc0(context_t& ctx, scm_obj_t inst);
  void emit_iloc1(context_t& ctx, scm_obj_t inst);
  void emit_car_iloc(context_t& ctx, scm_obj_t inst);
  void emit_cdr_iloc(context_t& ctx, scm_obj_t inst);
  void emit_lt_n_iloc(context_t& ctx, scm_obj_t inst);
  void emit_gt_n_iloc(context_t& ctx, scm_obj_t inst);
  void emit_ge_n_iloc(context_t& ctx, scm_obj_t inst);
  void emit_le_n_iloc(context_t& ctx, scm_obj_t inst);
  void emit_eq_n_iloc(context_t& ctx, scm_obj_t inst);
  void emit_extend(context_t& ctx, scm_obj_t inst);
  void emit_extend_enclose(context_t& ctx, scm_obj_t inst);
  void emit_extend_enclose_local(context_t& ctx, scm_obj_t inst);
  void emit_apply_iloc_local(context_t& ctx, scm_obj_t inst);
  void emit_if_true_ret_const(context_t& ctx, scm_obj_t inst);
  void emit_lt_iloc(context_t& ctx, scm_obj_t inst);
  void emit_gt_iloc(context_t& ctx, scm_obj_t inst);
  void emit_le_iloc(context_t& ctx, scm_obj_t inst);
  void emit_ge_iloc(context_t& ctx, scm_obj_t inst);
  void emit_eq_iloc(context_t& ctx, scm_obj_t inst);
  void emit_set_gloc(context_t& ctx, scm_obj_t inst);
  void emit_const(context_t& ctx, scm_obj_t inst);
  void emit_if_pairp(context_t& ctx, scm_obj_t inst);
  void emit_if_symbolp(context_t& ctx, scm_obj_t inst);
  void emit_cadr_iloc(context_t& ctx, scm_obj_t inst);
  void emit_cddr_iloc(context_t& ctx, scm_obj_t inst);
  void emit_if_not_pairp_ret_const(context_t& ctx, scm_obj_t inst);
  void emit_if_not_eqp_ret_const(context_t& ctx, scm_obj_t inst);
  void emit_if_false_ret_const(context_t& ctx, scm_obj_t inst);
  void emit_ret_nullp(context_t& ctx, scm_obj_t inst);
  void emit_ret_pairp(context_t& ctx, scm_obj_t inst);
  void emit_ret_gloc(context_t& ctx, scm_obj_t inst);
  void emit_ret_eqp(context_t& ctx, scm_obj_t inst);
  void emit_set_iloc(context_t& ctx, scm_obj_t inst);
  void emit_extend_unbound(context_t& ctx, scm_obj_t inst);
  void emit_enclose(context_t& ctx, scm_obj_t inst);
  void emit_push_close_local(context_t& ctx, scm_obj_t inst);
  void emit_close(context_t& ctx, scm_obj_t inst);
  void emit_ret_close(context_t& ctx, scm_obj_t inst);
  void emit_nadd_iloc(context_t& ctx, scm_obj_t inst);
  void emit_apply(context_t& ctx, scm_obj_t inst);
  void emit_escape(context_t& ctx, scm_obj_t inst);
  void emit_push_subr_gloc(context_t& ctx, scm_obj_t inst);
  void emit_subr_gloc(context_t& ctx, scm_obj_t inst);
  void emit_ret_subr_gloc(context_t& ctx, scm_obj_t inst);
};

#endif
