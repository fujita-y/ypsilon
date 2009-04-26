/*
    Ypsilon Scheme System
    Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
    See license.txt for terms and conditions of use
*/

#ifndef NANOASM_MACRO_H_INCLUDED
#define NANOASM_MACRO_H_INCLUDED

#define using_nanoasm(X)    nanoasm_t& __nas = X

#define __                  __nas .
#define GLOBAL(X,Y)         nanoasm_t::symbol_t X = __ common(#X); __ equ((X), (Y))
#define EXTERN(X)           nanoasm_t::symbol_t X = __ common(#X)
#define LOCAL(X)            nanoasm_t::symbol_t X = __ unique(#X)
#define LABEL(X)            __ label(X)
#define qword               __ qword
#define dword               __ dword
#define byte                __ byte
#define eax                 __ eax
#define ecx                 __ ecx
#define edx                 __ edx
#define ebx                 __ ebx
#define esp                 __ esp
#define ebp                 __ ebp
#define esi                 __ esi
#define edi                 __ edi
#define al                  __ al
#define cl                  __ cl
#define dl                  __ dl
#define bl                  __ bl
#if ARCH_LP64
#define rax                 __ rax
#define rcx                 __ rcx
#define rdx                 __ rdx
#define rbx                 __ rbx
#define rsp                 __ rsp
#define rbp                 __ rbp
#define rsi                 __ rsi
#define rdi                 __ rdi
#define r8                  __ r8
#define r9                  __ r9
#define r10                 __ r10
#define r11                 __ r11
#define r12                 __ r12
#define r13                 __ r13
#define r14                 __ r14
#define r15                 __ r15
#define rip                 __ rip
#endif

#endif
