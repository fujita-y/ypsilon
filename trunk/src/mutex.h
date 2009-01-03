/*
    Ypsilon Scheme System
    Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#ifndef MUTEX_H_INCLUDED
#define MUTEX_H_INCLUDED

#include "core.h"

#if _MSC_VER
  #if USE_CRITICAL_SECTION
        class mutex_t {
            mutex_t(const mutex_t&);
            mutex_t& operator=(const mutex_t&);

        public:
            CRITICAL_SECTION mutex;
  #if MTDEBUG
            int lock_count;
  #endif

            mutex_t() { /* should be blank */ }

            void init(bool recursive = false)
            {
              #if MTDEBUG
                lock_count = 0;
              #endif
                InitializeCriticalSection(&mutex);
            }

            void destroy()
            {
              #if MTDEBUG
                if (lock_count) {
                    fatal("internal error:%s:%u destroy() lock held(%d)", __FILE__, __LINE__, lock_count);
                }
              #endif
                DeleteCriticalSection(&mutex);
            }

            void lock()
            {
                EnterCriticalSection(&mutex);
  #if MTDEBUG
                lock_count++;
  #endif
            }

            void unlock()
            {
              #if MTDEBUG
                lock_count--;
                assert(lock_count >= 0);
              #endif
                LeaveCriticalSection(&mutex);
            }

            void verify_locked()
            {
              #if MTDEBUG
                if (lock_count == 0) {
                    fatal("internal error:%s:%u verify_locked() failed.", __FILE__, __LINE__);
                }
              #endif
            }
        };
  #else
        class mutex_t {
            mutex_t(const mutex_t&);
            mutex_t& operator=(const mutex_t&);

        public:
            HANDLE mutex;
  #if MTDEBUG
            int lock_count;
  #endif

            mutex_t() { /* should be blank */ }

            void init(bool recursive = false)
            {
              #if MTDEBUG
                lock_count = 0;
              #endif
                mutex = CreateMutex(NULL, FALSE, NULL);
                MTVERIFY(mutex);
            }

            void destroy()
            {
  #if MTDEBUG
                if (lock_count) {
                    fatal("internal error:%s:%u destroy() lock held(%d)", __FILE__, __LINE__, lock_count);
                }
  #endif
                MTVERIFY(CloseHandle(mutex));
            }

            void lock()
            {
                MTVERIFY(WaitForSingleObject(mutex, INFINITE) != WAIT_FAILED);
  #if MTDEBUG
                lock_count++;
  #endif
            }

            void unlock()
            {
  #if MTDEBUG
                lock_count--;
                assert(lock_count >= 0);
  #endif
                MTVERIFY(ReleaseMutex(mutex));
            }

            void verify_locked()
            {
  #if MTDEBUG
                if (lock_count == 0) {
                    fatal("internal error:%s:%u verify_locked() failed.", __FILE__, __LINE__);
                }
  #endif
            }
        };

   #endif
#else
    class mutex_t {
        mutex_t(const mutex_t&);
        mutex_t& operator=(const mutex_t&);

    public:
        pthread_mutex_t mutex;
  #if MTDEBUG
        int lock_count;
  #endif

        mutex_t() { /* should be blank */ }

        void init(bool recursive = false)
        {
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

        void destroy()
        {
  #if MTDEBUG
            if (lock_count) {
                fatal("internal error:%s:%u destroy() lock held(%d)", __FILE__, __LINE__, lock_count);
            }
  #endif
            MTVERIFY(pthread_mutex_destroy(&mutex));
        }

        void lock()
        {
            MTVERIFY(pthread_mutex_lock(&mutex));
  #if MTDEBUG
            lock_count++;
  #endif
        }

        void unlock()
        {
  #if MTDEBUG
            lock_count--;
            assert(lock_count >= 0);
  #endif
            MTVERIFY(pthread_mutex_unlock(&mutex));
        }

        void verify_locked()
        {
  #if MTDEBUG
            if (lock_count == 0) {
                fatal("internal error:%s:%u verify_locked() failed.", __FILE__, __LINE__);
            }
  #endif
        }
    };
#endif

class scoped_lock {
    scoped_lock(const scoped_lock&);
    scoped_lock& operator=(const scoped_lock&);
    mutex_t& m_lock;
public:
    scoped_lock(mutex_t& lock) : m_lock(lock) { m_lock.lock(); }
    ~scoped_lock() { m_lock.unlock(); }
};

#endif
