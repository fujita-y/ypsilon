// Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef EXCEPTION_H_INCLUDED
#define EXCEPTION_H_INCLUDED

#include "core.h"
#include "object.h"

struct vm_exception_t {
  vm_exception_t() {}
};

struct vm_escape_t {
  vm_escape_t() {}
};

struct vm_continue_t {
  vm_continue_t() {}
};

struct io_exception_t {
  int m_operation;
  int m_err;
  const char* m_message;
  io_exception_t(int opration, int err) {
    m_operation = opration;
    m_err = err;
    m_message = strerror(err);
  }
  io_exception_t(int opration, const char* message) {
    m_operation = opration;
    m_err = 0;
    m_message = message;
  }
};

struct io_codec_exception_t {
  int m_operation;
  scm_obj_t m_ch;
  const char* m_message;
  io_codec_exception_t(int opration, const char* message, scm_obj_t ch) {
    m_operation = opration;
    m_ch = ch;
    m_message = message;
  }
};

struct reader_exception_t {
  scm_string_t m_message;
  reader_exception_t(scm_string_t message) { m_message = message; }
};

#endif
