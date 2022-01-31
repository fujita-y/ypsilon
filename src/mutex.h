// Copyright (c) 2004-2022 Yoshikatsu Fujita / LittleWing Company Limited.
// See LICENSE file for terms and conditions of use.

#ifndef MUTEX_H_INCLUDED
#define MUTEX_H_INCLUDED

#include "core.h"

class mutex_t {
  mutex_t(const mutex_t&);
  mutex_t& operator=(const mutex_t&);

 public:
  pthread_mutex_t mutex;
#if MTDEBUG
  int lock_count;
#endif

  mutex_t() { /* should be blank */
  }

  void init(bool recursive = false) {
#if MTDEBUG
    lock_count = 0;
#endif
    if (recursive) {
      pthread_mutexattr_t attr;
      MTVERIFY(pthread_mutexattr_init(&attr));
      MTVERIFY(pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE));
      MTVERIFY(pthread_mutex_init(&mutex, &attr));
      MTVERIFY(pthread_mutexattr_destroy(&attr));
    } else {
#if MTDEBUG
      pthread_mutexattr_t attr;
      MTVERIFY(pthread_mutexattr_init(&attr));
      MTVERIFY(pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_ERRORCHECK));
      MTVERIFY(pthread_mutex_init(&mutex, &attr));
      MTVERIFY(pthread_mutexattr_destroy(&attr));
#else
      MTVERIFY(pthread_mutex_init(&mutex, NULL));
#endif
    }
  }

  void destroy() {
#if MTDEBUG
    if (lock_count) {
      fatal("internal error:%s:%u destroy() lock held(%d)", __FILE__, __LINE__, lock_count);
    }
#endif
    MTVERIFY(pthread_mutex_destroy(&mutex));
  }

  void lock() {
    MTVERIFY(pthread_mutex_lock(&mutex));
#if MTDEBUG
    lock_count++;
#endif
  }

  void unlock() {
#if MTDEBUG
    lock_count--;
    assert(lock_count >= 0);
#endif
    MTVERIFY(pthread_mutex_unlock(&mutex));
  }

  void verify_locked() {
#if MTDEBUG
    if (lock_count == 0) {
      fatal("internal error:%s:%u verify_locked() failed.", __FILE__, __LINE__);
    }
#endif
  }
};

class scoped_lock {
  scoped_lock(const scoped_lock&);
  scoped_lock& operator=(const scoped_lock&);
  mutex_t& m_lock;

 public:
  scoped_lock(mutex_t& lock) : m_lock(lock) { m_lock.lock(); }
  ~scoped_lock() { m_lock.unlock(); }
};

#endif
