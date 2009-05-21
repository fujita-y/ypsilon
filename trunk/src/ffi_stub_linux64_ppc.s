/*
    Ypsilon Scheme System
    Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

    .file           "ffi_stub_linux64_ppc.s"

    .section        ".opd","aw"
    .align 3

    .globl c_func_stub_intptr_ppc64
    .globl c_func_stub_float_ppc64
    .globl c_func_stub_double_ppc64

c_func_stub_intptr_ppc64:
    .quad .c_func_stub_intptr_ppc64, .TOC.@tocbase, 0
    .type c_func_stub_intptr_ppc64, @function

c_func_stub_float_ppc64:
    .quad .c_func_stub_float_ppc64, .TOC.@tocbase, 0
    .type c_func_stub_float_ppc64, @function

c_func_stub_double_ppc64:
    .quad .c_func_stub_double_ppc64, .TOC.@tocbase, 0
    .type c_func_stub_double_ppc64, @function

    .text
    .align 2

.c_func_stub_intptr_ppc64:
.c_func_stub_float_ppc64:
.c_func_stub_double_ppc64:

    mflr    0
    std     0,16(1)
    stdu    1,-368(1) # (112 + 32 * 8)
    std     2,40(1)

    cmpwi   0,4,0
    ble     0,.L2
    mtctr   4
    li      9,0
    addi    10,1,48
.L1:
    ldx     0,5,9
    stdx    0,10,9
    addi    9,9,8
    bdnz    .L1
.L2:

    ld      0,0(3)
    ld      2,8(3)
    ld      11,16(3)
    mtctr   0

    slwi    4,4,3
    add     12,5,4
    ld      3,0(12)
    ld      4,8(12)
    ld      5,16(12)
    ld      6,24(12)
    ld      7,32(12)
    ld      8,40(12)
    ld      9,48(12)
    ld      10,56(12)
    lfd     1,64(12)
    lfd     2,72(12)
    lfd     3,80(12)
    lfd     4,88(12)
    lfd     5,96(12)
    lfd     6,104(12)
    lfd     7,112(12)
    lfd     8,120(12)

    bctrl
    ld      2,40(1)

    addi    1,1,368
    ld      0,16(1)
    mtlr    0
    blr

.section .note.GNU-stack,"",%progbits
