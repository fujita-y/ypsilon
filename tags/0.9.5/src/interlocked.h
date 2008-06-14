/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#ifndef	INTERLOCKED_H_INCLUDED
#define	INTERLOCKED_H_INCLUDED

#include "core.h"

inline int32_t
interlocked_exchange_int32(volatile int32_t* target, int32_t value)
{
#if _MSC_VER
#else
    __asm__ __volatile__ ("xchgl %0, %1" 
                            : "+r" (value), "+m" (*target) 
                            :
                            : "memory");
#endif
    return value;
}

/*
inline int32_t
interlocked_compare_exchange(volatile int32_t* target, int32_t exchange, int32_t compare)
{
    int32_t value;
    __asm__ __volatile__("lock; cmpxchgl %1, %2"
                            : "=a" (value)
                            : "r" (exchange), "m" (*target), "0" (compare)
                            : "memory");
    return value;
}
                     
inline void
interlocked_or_uint8(volatile uint8_t* target, uint8_t bits)
{
    __asm__ __volatile__ ("lock; orb %1, %0"
                            : "+m" (*target)
                            : "r" (bits)
                            : "memory");
}

inline void
interlocked_add_int32(volatile int32_t* target, int32_t value)
{
    __asm__ __volatile__ ("lock; addl %1, %0"
                            : "+m" (*target)
                            : "r" (value)
                            : "memory");
}
*/
#endif

/*

typedef int32_t spinlock_t;

#define SPINLOCK_LOOP_MAX   1000

inline void
spin_lock(volatile spinlock_t* lock)
{
    while (interlocked_exchange_int32(lock, 1)) {
        int n = SPINLOCK_LOOP_MAX;
        do { __asm__ ("pause"); } while ((--n) & (*lock));
        if (*lock) usleep(1);
    }
}

inline void
spin_unlock(spinlock_t* lock)
{
   *lock = 0;
}

inline int32_t
i386_atomic_exchange(volatile int32_t* the_value, int32_t new_value)
{
    int32_t old_value;
    __asm__ __volatile__ ("xchg %0,(%2)"
                            : "=r" (old_value)
                            : "0" (new_value), "r" (the_value));
  return old_value;
}

inline void
i386_atomic_or(volatile uint8_t* the_value, uint8_t bits)
{
    __asm__ __volatile__ ("lock;or %0,%1"
                            :
                            : "r" (bits), "m" (*the_value) 
                            : "memory");
}

*/

