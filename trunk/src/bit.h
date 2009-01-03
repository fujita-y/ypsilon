/*
    Ypsilon Scheme System
    Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#ifndef BIT_H_INCLUDED
#define BIT_H_INCLUDED

#include "core.h"

int clp2(uint32_t x);
int flp2(uint32_t x);
int nlz(uint32_t x);
int nlz(uint64_t x);
int ntz(uint32_t x);
int ntz(uint64_t x);
int nbits(uint32_t x);
int nbits(uint64_t x);

inline int nbits(intptr_t x) {
    if (sizeof(intptr_t) == sizeof(uint32_t)) return nbits((uint32_t)x);
    return nbits((uint64_t)x);
}

inline int ntz(intptr_t x) {
    if (sizeof(intptr_t) == sizeof(uint32_t)) return ntz((uint32_t)x);
    return ntz((uint64_t)x);
}

inline int nlz(intptr_t x) {
    if (sizeof(intptr_t) == sizeof(uint32_t)) return nlz((uint32_t)x);
    return nlz((uint64_t)x);
}


#endif
