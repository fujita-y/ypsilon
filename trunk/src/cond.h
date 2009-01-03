/*
    Ypsilon Scheme System
    Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#ifndef COND_H_INCLUDED
#define COND_H_INCLUDED

#include "core.h"

#if _MSC_VER

    class cond_t {
        cond_t(const cond_t&);
        cond_t& operator=(const cond_t&);

    public:
        HANDLE  ev;

        cond_t() { /* should be blank */ }

        void init()
        {
            ev = CreateEvent(NULL, FALSE, FALSE, NULL);
            MTVERIFY(ev);
        }

        void destroy()
        {
            MTVERIFY(CloseHandle(ev));
        }

        void signal()
        {
            MTVERIFY(SetEvent(ev));
        }

        void wait(mutex_t& mutex)
        {
            #if USE_CRITICAL_SECTION
                mutex.unlock();
                MTVERIFY(WaitForSingleObject(ev, INFINITE) != WAIT_FAILED);
                mutex.lock();
            #else
                MTVERIFY(SignalObjectAndWait(mutex.mutex, ev, INFINITE, FALSE) != WAIT_FAILED);
                MTVERIFY(WaitForSingleObject(mutex.mutex, INFINITE) != WAIT_FAILED);
            #endif
        }
    };

#else

    class cond_t {
        cond_t(const cond_t&);
        cond_t& operator=(const cond_t&);

    public:
        pthread_cond_t  cv;

        cond_t() { /* should be blank */ }

        void init()
        {
            MTVERIFY(pthread_cond_init(&cv, NULL));
        }

        void destroy()
        {
            MTVERIFY(pthread_cond_destroy(&cv));
        }

        void signal()
        {
            MTVERIFY(pthread_cond_signal(&cv));
        }

        void wait(mutex_t& mutex)
        {
            MTVERIFY(pthread_cond_wait(&cv, &mutex.mutex));
        }
    };

#endif

#endif
