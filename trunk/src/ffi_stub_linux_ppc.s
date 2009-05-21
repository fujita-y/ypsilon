/*
    Ypsilon Scheme System
    Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

    .file            "ffi_stub_linux_ppc.s"

    .gnu_attribute  4, 1
    .gnu_attribute  8, 1

    .section        ".text"
    .align          2

    .globl          c_func_stub_intptr_ppc
    .type           c_func_stub_intptr_ppc, @function
    .globl          c_func_stub_float_ppc
    .type           c_func_stub_float_ppc, @function
    .globl          c_func_stub_int64_ppc
    .type           c_func_stub_int64_ppc, @function
    .globl          c_func_stub_double_ppc
    .type           c_func_stub_double_ppc, @function

c_func_stub_intptr_ppc:
c_func_stub_float_ppc:
c_func_stub_int64_ppc:
c_func_stub_double_ppc:
    stwu    1,-144(1)
    mflr    0
    stw     0,148(1)

    cmpwi   0,4,0
    ble     0,.L2
    mtctr   4
    li      9,0
    addi    10,1,8
.L1:
    lwzx    0,5,9
    stwx    0,10,9
    addi    9,9,4
    bdnz    .L1
.L2:

    mtctr   3
    slwi    4,4,2
    add     11,5,4
    lwz     3,0(11)
    lwz     4,4(11)
    lwz     5,8(11)
    lwz     6,12(11)
    lwz     7,16(11)
    lwz     8,20(11)
    lwz     9,24(11)
    lwz     10,28(11)
    lfd     1,32(11)
    lfd     2,40(11)
    lfd     3,48(11)
    lfd     4,56(11)
    lfd     5,64(11)
    lfd     6,72(11)
    lfd     7,80(11)
    lfd     8,88(11)
    creqv   6,6,6
    bctrl
    lwz     0,148(1)
    mtlr    0
    addi    1,1,144
    blr

.section .note.GNU-stack,"",%progbits
