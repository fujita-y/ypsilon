#define using_nanoasm   nanoasm nas
#define __              nas.
#define qword           nas.qword
#define dword           nas.dword
#define byte            nas.byte
#define eax             nas.eax
#define ecx             nas.ecx
#define edx             nas.edx
#define ebx             nas.ebx
#define esp             nas.esp
#define ebp             nas.ebp
#define esi             nas.esi
#define edi             nas.edi
#define al              nas.al
#define cl              nas.cl
#define dl              nas.dl
#define bl              nas.bl
#if ARCH_LP64
#define rax             nas.rax
#define rcx             nas.rcx
#define rdx             nas.rdx
#define rbx             nas.rbx
#define rsp             nas.rsp
#define rbp             nas.rbp
#define rsi             nas.rsi
#define rdi             nas.rdi
#define r8              nas.r8
#define r9              nas.r9
#define r10             nas.r10
#define r11             nas.r11
#define r12             nas.r12
#define r13             nas.r13
#define r14             nas.r14
#define r15             nas.r15
#define rip             nas.rip
#endif
#define LOCAL(X)        nanoasm::symbol_t X = nas.unique(#X)
#define GLOBAL(X)       nanoasm::symbol_t X (#X)
#define LABEL(X)        nas.label(X)
#define EQU(X,Y)        nas.equ((X), (Y))
