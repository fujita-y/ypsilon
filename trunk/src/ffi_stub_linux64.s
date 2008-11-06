/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/
/*
    contents of argv[] = {
        long stack_args[nstack];
        long sse_args[8]; // xmm0 - xmm7
        long sse_prec[8]; // 0 double, 1 float
        long reg_args[6]; // rdi, rsi, rdx, rcx, r8, r9
    }

    intptr_t c_func_stub_intptr_x64(intptr_t adrs_rdi, intptr_t nstack_rsi, intptr_t nsse_rdx, intptr_t argv_rcx[])
    double   c_func_stub_double_x64(intptr_t adrs_rdi, intptr_t nstack_rsi, intptr_t nsse_rdx, intptr_t argv_rcx[])

*/

    .file   "ffi_stub.s"

    .text

    .align  4,0x90

    .globl  c_func_stub_intptr_x64
    .globl  c_func_stub_double_x64
    .globl  c_callback_stub_intptr_x64

c_func_stub_intptr_x64:
c_func_stub_double_x64:

    pushq       %rbp
    movq        %rsp, %rbp

# stack arguments
    leaq        (,%rsi,8), %rax
    andq        $-16, %rax
    subq        %rax, %rsp          # align to 16 byte
    movq        $0, %r10            # i = 0
loop:
    cmpq        %r10, %rsi
    je          done
    movq        (%rcx,%r10,8), %rax
    movq        %rax, (%rsp,%r10,8)
    addq        $1, %r10
    jmp         loop
done:
    leaq        (%rcx, %rsi, 8), %r10

# sse arguments
    movsd         (%r10), %xmm0
    movsd        8(%r10), %xmm1
    movsd       16(%r10), %xmm2
    movsd       24(%r10), %xmm3
    movsd       32(%r10), %xmm4
    movsd       40(%r10), %xmm5
    movsd       48(%r10), %xmm6
    movsd       56(%r10), %xmm7
    leaq        64(%r10), %r10

# sse precisions
L0:
    cmpq        $0, (%r10)
    je          L1
    cvtsd2ss    %xmm0, %xmm0
L1:
    cmpq        $0, 8(%r10)
    je          L2
    cvtsd2ss    %xmm1, %xmm1
L2:
    cmpq        $0, 16(%r10)
    je          L3
    cvtsd2ss    %xmm2, %xmm2
L3:
    cmpq        $0, 24(%r10)
    je          L4
    cvtsd2ss    %xmm3, %xmm3
L4:
    cmpq        $0, 32(%r10)
    je          L5
    cvtsd2ss    %xmm4, %xmm4
L5:
    cmpq        $0, 40(%r10)
    je          L6
    cvtsd2ss    %xmm5, %xmm5
L6:
    cmpq        $0, 48(%r10)
    je          L7
    cvtsd2ss    %xmm6, %xmm6
L7:
    cmpq        $0, 56(%r10)
    je          L8
    cvtsd2ss    %xmm7, %xmm7
L8:
    leaq        64(%r10), %r10

# reg argumuments
    movq        %rdx, %rax
    movq        %rdi, %r11

    movq          (%r10), %rdi
    movq         8(%r10), %rsi
    movq        16(%r10), %rdx
    movq        24(%r10), %rcx
    movq        32(%r10), %r8
    movq        40(%r10), %r9

    call    *%r11

    movq    %rbp, %rsp
    popq    %rbp
    ret

c_callback_stub_intptr_x64:
    pushq   %rbp
    movq    %rsp, %rbp
    subq    $48, %rsp
    movq    %rdi, (%rsp)
    movq    %rsi, 8(%rsp)
    movq    %rdx, 16(%rsp)
    movq    %rcx, 24(%rsp)
    movq    %r8, 32(%rsp)
    movq    %r9, 40(%rsp)

    movq    (%r10), %rdi        # uid
    movq    8(%r10), %rsi       # argc
    movq    %rsp, %rdx          # reg
    leaq    16(%rbp), %rcx      # stack

    call    c_callback_intptr_x64

    movq    %rbp, %rsp
    popq    %rbp
    ret

.section .note.GNU-stack,"",%progbits
