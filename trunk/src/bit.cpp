/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

// Reference:
// PowerPC Compiler Writer's Guide
// Hacker's Delight (Addison-Wesley, 2003)

#include "core.h"
#include "bit.h"

int
clp2(uint32_t x)
{
    x = x - 1;
    x = x | (x >>  1);
    x = x | (x >>  2);
    x = x | (x >>  4);
    x = x | (x >>  8);
    x = x | (x >> 16);
    return x + 1;
}

int
flp2(uint32_t x)
{
    x = x | (x >>  1);
    x = x | (x >>  2);
    x = x | (x >>  4);
    x = x | (x >>  8);
    x = x | (x >> 16);
    return x - (x >> 1);
}

int
nbits(uint32_t x)
{
    uint32_t t;
    x = x - ((x >> 1) & 0x55555555);
    t = ((x >> 2) & 0x33333333);
    x = (x & 0x33333333) + t;
    x = (x + (x >> 4)) & 0x0F0F0F0F;
    x = x + (x << 8);
    x = x + (x << 16);
    return x >> 24;
}

int
nbits(uint64_t x)
{
    // todo: optimize
    return nbits((uint32_t)(x >> 32)) + nbits((uint32_t)(x & 0xffffffff));
}

int
nlz(uint32_t x)
{
    uint32_t t;
    int n = 32;
    t = x >> 16; if (t) { n -= 16 ; x = t; }
    t = x >>  8; if (t) { n -=  8 ; x = t; }
    t = x >>  4; if (t) { n -=  4 ; x = t; }
    t = x >>  2; if (t) { n -=  2 ; x = t; }
    t = x >>  1; if (t) { return n - 2; }
    return n - x;
}

int
nlz(uint64_t x)
{
    uint64_t t;
    int n = 64;
    t = x >> 32; if (t) { n -= 32 ; x = t; }
    t = x >> 16; if (t) { n -= 16 ; x = t; }
    t = x >>  8; if (t) { n -=  8 ; x = t; }
    t = x >>  4; if (t) { n -=  4 ; x = t; }
    t = x >>  2; if (t) { n -=  2 ; x = t; }
    t = x >>  1; if (t) { return n - 2; }
    return n - x;
}

int
ntz(uint32_t x)
{
    return nbits(~x & (x - 1));
}

int
ntz(uint64_t x)
{
    return nbits(~x & (x - 1));
}
