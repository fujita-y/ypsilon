/*
    Ypsilon Scheme System
    Copyright (c) 2004-2008 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/
/*
    argv[] : {
        long stack_args[nstack];
        long sse_args[8]; // %xmm0 - %xmm7
        long reg_args[6]; // %rdi, %rsi, %rdx, %rcx, %r8, %r9
    }
    
    long c_func_stub_intptr_x64(void* adrs:%rdi, long nstack:%rsi, long nsse:%rdx, long argv[]:%rcx)

*/

    .file   "ffi_stub.s"

    .text

    .align  4,0x90

    .globl  c_func_stub_intptr_x64
    .globl  c_func_stub_double_x64

c_func_stub_intptr_x64:
c_func_stub_double_x64:

    pushq   %rbp
    movq    %rsp, %rbp

# stack arguments
    leaq    15(,%rsi,4), %rax   
    andq    $-16, %rax  
    subq    %rax, %rsp          # align to 16 byte
    movq    $0, %r10            # i = 0
loop:
    cmpq    %r10, %rsi
    je      done
    movq   (%rcx,%r10,8), %rax    
    movq    %rax, (%rsp,%r10,8)
    addq    $1, %r10
    jmp     loop
done:
    
# sse and reg arguments
    leaq    (%rcx, %rsi, 8), %r10
    movq    %rdx, %rax
    movq    %rdi, %r11
    
    movsd      (%r10), %xmm0
    movsd     8(%r10), %xmm1
    movsd    16(%r10), %xmm2
    movsd    24(%r10), %xmm3
    movsd    32(%r10), %xmm4
    movsd    40(%r10), %xmm5
    movsd    48(%r10), %xmm6
    movsd    56(%r10), %xmm7
    movq     64(%r10), %rdi
    movq     72(%r10), %rsi
    movq     80(%r10), %rdx
    movq     88(%r10), %rcx
    movq     96(%r10), %r8
    movq    104(%r10), %r9

    call    *%r11
    leave
    ret

.section .note.GNU-stack,"",%progbits
