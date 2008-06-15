/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

/*

    gcc -O2 -fno-omit-frame-pointer ffi_stub.c

    #include "stdio.h"
    #include "stdlib.h"

    #define emit(x)  x "\n\t"

    intptr_t
    c_func_stub_intptr(void* adrs, int argc, intptr_t argv[])
    {
        int bytes = (argc * sizeof(intptr_t) + 15) & ~15;
        int retval;
        __asm__ __volatile__ (
            emit("movl    %%esp, %%edx")
            emit("subl    %1, %%esp")
            emit("movl    %%esp, %%edi")
            emit("rep movsb")
            emit("movl    %%edx, %%edi")
            emit("call    *%%eax")
            emit("movl    %%edi, %%esp")
            : "=a" (retval)
            : "c" (bytes), "S" (argv), "0" (adrs)
            : "%edi", "%edx", "memory");
        return retval;
    };

    double
    c_func_stub_double(void* adrs, int argc, intptr_t argv[])
    {
        int bytes = (argc * sizeof(intptr_t) + 15) & ~15;
        double retval;
        __asm__ __volatile__ (
            emit("movl    %%esp, %%edx")
            emit("subl    %1, %%esp")
            emit("movl    %%esp, %%edi")
            emit("rep movsb")
            emit("movl    %%edx, %%edi")
            emit("call    *%%eax")
            emit("movl    %%edi, %%esp")
            : "=t" (retval)
            : "c" (bytes), "S" (argv), "a" (adrs)
            : "%edi", "%edx", "memory");
        return retval;
    }

    int c_callback_stub_int()
    {
        int*    base;
        int uid;
        int argc;
        __asm__  __volatile__ (
            emit("leal 8(%%ebp), %%eax")
            emit("movl %%eax, %0")
            emit("movl (%%ecx), %%eax")
            emit("movl %%eax, %1")
            emit("movl 4(%%ecx), %%eax")
            emit("movl %%eax, %2")
            : "=m" (base), "=m" (uid), "=m" (argc) :: "%eax");
        return c_callback_int(uid, argc, base);
    }

*/

    .file   "ffi_stub_darwin.s"

    .text

    .align  4,0x90

    .globl  _c_func_stub_intptr
    .globl  _c_func_stub_double
    .globl  _c_callback_stub_int

_c_func_stub_intptr:
_c_func_stub_double:

    pushl   %ebp
    movl    %esp, %ebp

    subl    $8, %esp
    movl    %esi, (%esp)
    movl    %edi, 4(%esp)

    movl    8(%ebp), %eax       # adrs
    movl    12(%ebp), %ecx      # argc
    movl    16(%ebp), %esi      # argv

    leal    15(,%ecx,4), %ecx   # align to 16 byte
    andl    $-16, %ecx

    movl    %esp, %edx
    subl    %ecx, %esp
    movl    %esp, %edi
    rep movsb

    movl    %edx, %edi
    call    *%eax
    movl    %edi, %esp

    movl    (%esp), %esi
    movl    4(%esp), %edi

    movl    %ebp, %esp
    popl    %ebp
    ret

    .align  4,0x90

_c_callback_stub_int:

    pushl   %ebp
    movl    %esp, %ebp

    subl    $40, %esp

    movl    (%ecx), %eax        # uid
    movl    %eax, (%esp)

    movl    4(%ecx), %eax       # argc
    movl    %eax, 4(%esp)

    leal    8(%ebp), %eax       # base
    movl    %eax, 8(%esp)

    call    _c_callback_int

    movl    %ebp, %esp
    popl    %ebp
    ret

