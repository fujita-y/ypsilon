// Copyright (c) 2004-2022 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#include "core.h"
#include "arith.h"
#include "violation.h"
#include "vm.h"

#include "../deps/glfw/deps/linmath.h"

#define ONE_MAT4X4_OP_BEGIN(__NARGS__)           \
  if (argc == __NARGS__) {                       \
    if (BVECTORP(argv[0])) {                     \
      scm_bvector_t bv = (scm_bvector_t)argv[0]; \
      if (bv->count == sizeof(float) * 16) {
#define ONE_MAT4X4_OP_END(__NARGS__, __NAME__)                                                                  \
  }                                                                                                             \
  wrong_type_argument_violation(vm, __NAME__, 0, "bytevector length is 64", MAKEFIXNUM(bv->count), argc, argv); \
  return scm_undef;                                                                                             \
  }                                                                                                             \
  wrong_type_argument_violation(vm, __NAME__, 0, "bytevector", argv[0], argc, argv);                            \
  return scm_undef;                                                                                             \
  }                                                                                                             \
  wrong_number_of_arguments_violation(vm, __NAME__, __NARGS__, __NARGS__, argc, argv);                          \
  return scm_undef;

#define TWO_MAT4X4_OP_BEGIN(__NARGS__)              \
  if (argc == __NARGS__) {                          \
    if (BVECTORP(argv[0])) {                        \
      if (BVECTORP(argv[1])) {                      \
        scm_bvector_t dst = (scm_bvector_t)argv[0]; \
        scm_bvector_t src = (scm_bvector_t)argv[1]; \
        if (dst->count == sizeof(float) * 16) {     \
          if (src->count == sizeof(float) * 16) {
#define TWO_MAT4X4_OP_END(__NARGS__, __NAME__)                                                                  \
  }                                                                                                             \
  wrong_type_argument_violation(vm, __NAME__, 1, "bytevector length = 64", MAKEFIXNUM(src->count), argc, argv); \
  return scm_undef;                                                                                             \
  }                                                                                                             \
  wrong_type_argument_violation(vm, __NAME__, 0, "bytevector length = 64", MAKEFIXNUM(dst->count), argc, argv); \
  return scm_undef;                                                                                             \
  }                                                                                                             \
  wrong_type_argument_violation(vm, __NAME__, 1, "bytevector", argv[1], argc, argv);                            \
  return scm_undef;                                                                                             \
  }                                                                                                             \
  wrong_type_argument_violation(vm, __NAME__, 0, "bytevector", argv[0], argc, argv);                            \
  return scm_undef;                                                                                             \
  }                                                                                                             \
  wrong_number_of_arguments_violation(vm, __NAME__, __NARGS__, __NARGS__, argc, argv);                          \
  return scm_undef;

#define TWO_MAP_OP(__FUNC__, __NAME__)          \
  TWO_MAT4X4_OP_BEGIN(2);                       \
  __FUNC__((vec4*)dst->elts, (vec4*)src->elts); \
  return scm_unspecified;                       \
  TWO_MAT4X4_OP_END(2, __NAME__);

#define THREE_MAT4X4_OP(__FUNC__, __NAME__)                                                                                  \
  if (argc == 3) {                                                                                                           \
    if (BVECTORP(argv[0])) {                                                                                                 \
      if (BVECTORP(argv[1])) {                                                                                               \
        if (BVECTORP(argv[2])) {                                                                                             \
          scm_bvector_t dst = (scm_bvector_t)argv[0];                                                                        \
          scm_bvector_t src1 = (scm_bvector_t)argv[1];                                                                       \
          scm_bvector_t src2 = (scm_bvector_t)argv[2];                                                                       \
          if (dst->count == sizeof(float) * 16) {                                                                            \
            if (src1->count == sizeof(float) * 16) {                                                                         \
              if (src2->count == sizeof(float) * 16) {                                                                       \
                __FUNC__((vec4*)dst->elts, (vec4*)src1->elts, (vec4*)src2->elts);                                            \
                return scm_unspecified;                                                                                      \
              }                                                                                                              \
              wrong_type_argument_violation(vm, __NAME__, 2, "bytevector length = 64", MAKEFIXNUM(src2->count), argc, argv); \
              return scm_undef;                                                                                              \
            }                                                                                                                \
            wrong_type_argument_violation(vm, __NAME__, 1, "bytevector length = 64", MAKEFIXNUM(src1->count), argc, argv);   \
            return scm_undef;                                                                                                \
          }                                                                                                                  \
          wrong_type_argument_violation(vm, __NAME__, 0, "bytevector length = 64", MAKEFIXNUM(dst->count), argc, argv);      \
          return scm_undef;                                                                                                  \
        }                                                                                                                    \
        wrong_type_argument_violation(vm, __NAME__, 2, "bytevector", argv[2], argc, argv);                                   \
        return scm_undef;                                                                                                    \
      }                                                                                                                      \
      wrong_type_argument_violation(vm, __NAME__, 1, "bytevector", argv[1], argc, argv);                                     \
      return scm_undef;                                                                                                      \
    }                                                                                                                        \
    wrong_type_argument_violation(vm, __NAME__, 0, "bytevector", argv[0], argc, argv);                                       \
    return scm_undef;                                                                                                        \
  }                                                                                                                          \
  wrong_number_of_arguments_violation(vm, __NAME__, 3, 3, argc, argv);                                                       \
  return scm_undef;

// mat4x4-identity
scm_obj_t subr_mat4x4_identity(VM* vm, int argc, scm_obj_t argv[]) {
  ONE_MAT4X4_OP_BEGIN(1);
  mat4x4_identity((vec4*)bv->elts);
  return scm_unspecified;
  ONE_MAT4X4_OP_END(1, "mat4x4-identity");
}

// mat4x4-dup
scm_obj_t subr_mat4x4_dup(VM* vm, int argc, scm_obj_t argv[]) { TWO_MAP_OP(mat4x4_dup, "mat4x4-dup"); }

// mat4x4-transpose
scm_obj_t subr_mat4x4_transpose(VM* vm, int argc, scm_obj_t argv[]) { TWO_MAP_OP(mat4x4_transpose, "mat4x4-transpose"); }

// mat4x4-invert
scm_obj_t subr_mat4x4_invert(VM* vm, int argc, scm_obj_t argv[]) { TWO_MAP_OP(mat4x4_invert, "mat4x4-invert"); }

// mat4x4-orthonormalize
scm_obj_t subr_mat4x4_orthonormalize(VM* vm, int argc, scm_obj_t argv[]) { TWO_MAP_OP(mat4x4_invert, "mat4x4-orthonormalize"); }

// mat4x4-add
scm_obj_t subr_mat4x4_add(VM* vm, int argc, scm_obj_t argv[]) { THREE_MAT4X4_OP(mat4x4_add, "mat4x4-add"); }

// mat4x4-sub
scm_obj_t subr_mat4x4_sub(VM* vm, int argc, scm_obj_t argv[]) { THREE_MAT4X4_OP(mat4x4_sub, "mat4x4-sub"); }

// mat4x4-sub
scm_obj_t subr_mat4x4_mul(VM* vm, int argc, scm_obj_t argv[]) { THREE_MAT4X4_OP(mat4x4_mul, "mat4x4-mul"); }

// mat4x4-scale
scm_obj_t subr_mat4x4_scale(VM* vm, int argc, scm_obj_t argv[]) {
  TWO_MAT4X4_OP_BEGIN(3);
  if (real_pred(argv[2])) {
    mat4x4_scale((vec4*)dst->elts, (vec4*)src->elts, real_to_double(argv[2]));
    return scm_unspecified;
  }
  wrong_type_argument_violation(vm, "mat4x4-scale", 2, "real", argv[2], argc, argv);
  return scm_undef;
  TWO_MAT4X4_OP_END(3, "mat4x4-scale");
}

// mat4x4-translate
scm_obj_t subr_mat4x4_translate(VM* vm, int argc, scm_obj_t argv[]) {
  ONE_MAT4X4_OP_BEGIN(4);
  float param[3];
  for (int i = 1; i <= 3; i++) {
    if (real_pred(argv[i])) {
      param[i - 1] = real_to_double(argv[i]);
      continue;
    }
    wrong_type_argument_violation(vm, "mat4x4-translate", i, "real", argv[i], argc, argv);
    return scm_undef;
  }
  mat4x4_translate((vec4*)bv->elts, param[0], param[1], param[2]);
  return scm_unspecified;
  ONE_MAT4X4_OP_END(4, "mat4x4-translate");
}

// mat4x4-frustum
scm_obj_t subr_mat4x4_frustum(VM* vm, int argc, scm_obj_t argv[]) {
  ONE_MAT4X4_OP_BEGIN(7);
  float param[6];
  for (int i = 1; i <= 6; i++) {
    if (real_pred(argv[i])) {
      param[i - 1] = real_to_double(argv[i]);
      continue;
    }
    wrong_type_argument_violation(vm, "mat4x4-frustum", i, "real", argv[i], argc, argv);
    return scm_undef;
  }
  mat4x4_frustum((vec4*)bv->elts, param[0], param[1], param[2], param[3], param[4], param[5]);
  return scm_unspecified;
  ONE_MAT4X4_OP_END(7, "mat4x4-frustum");
}

// mat4x4-ortho
scm_obj_t subr_mat4x4_ortho(VM* vm, int argc, scm_obj_t argv[]) {
  ONE_MAT4X4_OP_BEGIN(7);
  float param[6];
  for (int i = 1; i <= 6; i++) {
    if (real_pred(argv[i])) {
      param[i - 1] = real_to_double(argv[i]);
      continue;
    }
    wrong_type_argument_violation(vm, "mat4x4-ortho", i, "real", argv[i], argc, argv);
    return scm_undef;
  }
  mat4x4_ortho((vec4*)bv->elts, param[0], param[1], param[2], param[3], param[4], param[5]);
  return scm_unspecified;
  ONE_MAT4X4_OP_END(7, "mat4x4-ortho");
}

// mat4x4-perspective
scm_obj_t subr_mat4x4_perspective(VM* vm, int argc, scm_obj_t argv[]) {
  ONE_MAT4X4_OP_BEGIN(5);
  float param[4];
  for (int i = 1; i <= 4; i++) {
    if (real_pred(argv[i])) {
      param[i - 1] = real_to_double(argv[i]);
      continue;
    }
    wrong_type_argument_violation(vm, "mat4x4-perspective", i, "real", argv[i], argc, argv);
    return scm_undef;
  }
  mat4x4_perspective((vec4*)bv->elts, param[0], param[1], param[2], param[3]);
  return scm_unspecified;
  ONE_MAT4X4_OP_END(5, "mat4x4-perspective");
}

// mat4x4-look-at
scm_obj_t subr_mat4x4_look_at(VM* vm, int argc, scm_obj_t argv[]) {
  ONE_MAT4X4_OP_BEGIN(10);
  float param[9];
  for (int i = 1; i <= 9; i++) {
    if (real_pred(argv[i])) {
      param[i - 1] = real_to_double(argv[i]);
      continue;
    }
    wrong_type_argument_violation(vm, "mat4x4-look-at", i, "real", argv[i], argc, argv);
    return scm_undef;
  }
  vec3 eye = {param[0], param[1], param[2]};
  vec3 center = {param[3], param[4], param[5]};
  vec3 up = {param[6], param[7], param[8]};
  mat4x4_look_at((vec4*)bv->elts, eye, center, up);
  return scm_unspecified;
  ONE_MAT4X4_OP_END(10, "mat4x4-look-at");
}

// mat4x4-rotate
scm_obj_t subr_mat4x4_rotate(VM* vm, int argc, scm_obj_t argv[]) {
  TWO_MAT4X4_OP_BEGIN(6);
  float param[4];
  for (int i = 2; i <= 5; i++) {
    if (real_pred(argv[i])) {
      param[i - 2] = real_to_double(argv[i]);
      continue;
    }
    wrong_type_argument_violation(vm, "mat4x4-rotate", i, "real", argv[i], argc, argv);
    return scm_undef;
  }
  mat4x4_rotate((vec4*)dst->elts, (vec4*)src->elts, param[0], param[1], param[2], param[3]);
  return scm_unspecified;
  TWO_MAT4X4_OP_END(6, "mat4x4-rotate");
}

void init_subr_linmath(object_heap_t* heap) {
#define DEFSUBR(SYM, FUNC) heap->intern_system_subr(SYM, FUNC)

  DEFSUBR("mat4x4-identity", subr_mat4x4_identity);
  DEFSUBR("mat4x4-dup", subr_mat4x4_dup);
  DEFSUBR("mat4x4-transpose", subr_mat4x4_transpose);
  DEFSUBR("mat4x4-invert", subr_mat4x4_invert);
  DEFSUBR("mat4x4-orthonormalize", subr_mat4x4_orthonormalize);
  DEFSUBR("mat4x4-add", subr_mat4x4_add);
  DEFSUBR("mat4x4-sub", subr_mat4x4_sub);
  DEFSUBR("mat4x4-mul", subr_mat4x4_mul);
  DEFSUBR("mat4x4-scale", subr_mat4x4_scale);
  DEFSUBR("mat4x4-translate", subr_mat4x4_translate);
  DEFSUBR("mat4x4-frustum", subr_mat4x4_frustum);
  DEFSUBR("mat4x4-ortho", subr_mat4x4_ortho);
  DEFSUBR("mat4x4-perspective", subr_mat4x4_perspective);
  DEFSUBR("mat4x4-look-at", subr_mat4x4_look_at);
  DEFSUBR("mat4x4-rotate", subr_mat4x4_rotate);
}
