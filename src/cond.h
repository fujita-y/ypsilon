// Copyright (c) 2004-2022 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef COND_H_INCLUDED
#define COND_H_INCLUDED

#include "core.h"
#include "mutex.h"

class cond_t {
  cond_t(const cond_t&);
  cond_t& operator=(const cond_t&);

 public:
  pthread_cond_t cv;

  cond_t() { /* should be blank */
  }

  void init() { MTVERIFY(pthread_cond_init(&cv, NULL)); }

  void destroy() { MTVERIFY(pthread_cond_destroy(&cv)); }

  void signal() { MTVERIFY(pthread_cond_signal(&cv)); }

  void wait(mutex_t& mutex) { MTVERIFY(pthread_cond_wait(&cv, &mutex.mutex)); }
};

#endif
