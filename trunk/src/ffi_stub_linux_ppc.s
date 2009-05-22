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
    .globl          c_callback_stub_intptr_ppc
    .type           c_callback_stub_intptr_ppc, @function
    .globl          c_callback_stub_int64_ppc
    .type           c_callback_stub_int64_ppc, @function
    .globl          c_callback_stub_double_ppc
    .type           c_callback_stub_double_ppc, @function

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

c_callback_stub_double_ppc:
    lis         12,c_callback_double_ppc@ha
    la          12,c_callback_double_ppc@l(12)
    mtctr       12
    b           callback_stub_common

c_callback_stub_int64_ppc:
    lis         12,c_callback_int64_ppc@ha
    la          12,c_callback_int64_ppc@l(12)
    mtctr       12
    b           callback_stub_common

c_callback_stub_intptr_ppc:
    lis         12,c_callback_intptr_ppc@ha
    la          12,c_callback_intptr_ppc@l(12)
    mtctr       12
    b           callback_stub_common

callback_stub_common:
    stwu        1,-112(1)
    mflr        0
    stw         0,116(1)
    crxor       6,6,6
    
    stw         3,8(1)
    stw         4,12(1)
    stw         5,16(1)
    stw         6,20(1)
    stw         7,24(1)
    stw         8,28(1)
    stw         9,32(1)
    stw         10,36(1)
    stfd        1,40(1)
    stfd        2,48(1)
    stfd        3,56(1)
    stfd        4,64(1)
    stfd        5,72(1)
    stfd        6,80(1)
    stfd        7,88(1)
    stfd        8,96(1)
    
    lwz         3,0(11)
    lwz         4,4(11)
    addi        5,1,8
    addi        6,1,120
    bctrl
    
    lwz         0,116(1)
    mtlr        0
    addi        1,1,112
    blr

.section .note.GNU-stack,"",%progbits
