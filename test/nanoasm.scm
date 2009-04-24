;; usage: ypsilon nanoasm.scm [full]
;; requirement: linux, yasm-0.7.1, udis86-1.7
#!r6rs

(import (rnrs)
        (srfi :48)
        (ypsilon ffi)
        (ypsilon pregexp)
        (tidbits generator)
        (tidbits dotimes)
        (only (core) put-byte system))

(define quick-test (not (and (= (length (command-line)) 2)
                             (string=? (cadr (command-line)) "full"))))
(define code-chunk-count (if quick-test (if on-x64 500 100) 1000))
(define total-count 0)
(define operator-alias-alist '((or . or_) (and . and_) (xor . xor_) (not . not_)))

(define GPR64 (if quick-test
                  (if on-x64 '(rax r15) '())
                  (if on-x64 '(rax rcx rdx rbx rsp rbp rsi rdi r8 r9 r10 r11 r12 r13 r14 r15) '())))
(define GPR32 (if quick-test
                  '(eax ebx)
                  '(eax ecx edx ebx esp ebp esi edi)))
(define GPR8  (if quick-test
                  '(al bl)
                  '(al cl dl bl)))
(define BASE  (if quick-test
                  (if on-x64
                      '(rcx rsp rbp r12 r13 r15)
                      '(ecx esp ebp))
                  (if on-x64
                      '(rax rcx rdx rbx rsp rbp rsi rdi r8 r9 r10 r11 r12 r13 r14 r15)
                      '(eax ecx edx ebx esp ebp esi edi))))
(define INDEX (if quick-test
                  (if on-x64
                      '(rcx rbp r13 r15)
                      '(ecx ebp))
                  (if on-x64
                      '(rax rcx rdx rbx rbp rsi rdi r8 r9 r10 r11 r12 r13 r14 r15)
                      '(eax ecx edx ebx ebp esi edi))))

(define-generator every-memory-operands
  (lambda ()
    (for-each
     (lambda (base)
       (for-each
        (lambda (index)
          (for-each
           (lambda (scale)
             (for-each
              (lambda (disp)
                (when (and (or index (not scale)) (or base index (and disp (> disp 0))))
                  (yield
                   (format "[~a~a~a~a]"
                           (if base base "")
                           (if index (format "~a~a" (if base "+" "") index) "")
                           (if (and index scale) (format "*~a" scale) "")
                           (if disp (format "~a0x~x" (if (or base index) (if (> disp 0) "+" "-") "") (abs disp)) "")))))
              `(#f #x1C #x49ABD1 ,(- #x2F) ,(- #x37ED62))))
           '(#f 1 2 4 8)))
        `(#f ,@INDEX)))
     `(#f ,@BASE))
    (when on-x64
      (yield "[rip]")
      (yield "[rip+0x34]")
      (yield "[rip+0x62AC55]"))
    (yield #f)))

(define-generator every-combination
  (lambda (lst)
    (let loop1 ((e1 lst))
      (when (pair? e1)
        (let loop2 ((e2 lst))
          (when (pair? e2)
            (yield (cons (car e1) (car e2)))
            (loop2 (cdr e2))))
        (loop1 (cdr e1))))
    (yield #f)))

(define-generator every-combination2
  (lambda (lst1 lst2)
    (let loop1 ((e1 lst1))
      (when (pair? e1)
        (let loop2 ((e2 lst2))
          (when (pair? e2)
            (yield (cons (car e1) (car e2)))
            (loop2 (cdr e2))))
        (loop1 (cdr e1))))
    (yield #f)))

(define test-id 1)
(define nanoasm-o-filename (format "/tmp/_nanoasm.~a.o" test-id))
(define yasm-code-filename (format "/tmp/yasm.~a.code" test-id))
(define nano-code-filename (format "/tmp/nano.~a.code" test-id))
(define yasm-asm-filename (format "/tmp/yasm.~a.s" test-id))
(define nano-asm-filename (format "/tmp/nano.~a.cpp" test-id))
(define yasm-obj-filename (format "/tmp/yasm.~a.o" test-id))
(define nano-obj-filename (format "/tmp/nano.~a.o" test-id))
(define nano-prog-filename (format "/tmp/nano.~a.out" test-id))
(define yasm-disasm-filename (format "/tmp/yasm.~a.lst" test-id))
(define nano-disasm-filename (format "/tmp/nano.~a.lst" test-id))
(define diff-filename (format "/tmp/nano.~a.diff" test-id))

(define abort
  (lambda (n)
    (format #t "test abort by unexpected error (~s)\n" n)
    (exit n)))

(define-syntax call-with-code-output
  (syntax-rules ()
    ((_ body)
     (begin
       (when (file-exists? yasm-code-filename) (delete-file yasm-code-filename))
       (when (file-exists? nano-code-filename) (delete-file nano-code-filename))
       (call-with-output-file yasm-code-filename
         (lambda (yasm)
           (call-with-output-file nano-code-filename
             (lambda (nano)
               (body yasm nano)))))))))

(define-syntax call-with-code-input
  (syntax-rules ()
    ((_ body)
     (begin
       (call-with-input-file yasm-code-filename
         (lambda (yasm)
           (call-with-input-file nano-code-filename
             (lambda (nano)
               (body yasm nano)))))))))

(define-syntax call-with-script-output
  (syntax-rules ()
    ((_ body)
     (begin
       (when (file-exists? yasm-asm-filename) (delete-file yasm-asm-filename))
       (when (file-exists? nano-asm-filename) (delete-file nano-asm-filename))
       (call-with-output-file yasm-asm-filename
         (lambda (yasm)
           (call-with-output-file nano-asm-filename
             (lambda (nano)
               (body yasm nano)))))))))


(define run-test
  (lambda ()

    (define count 0)

    (define show-count
      (lambda ()
        (format #t "passed ~a" count)
        (put-byte (current-output-port) (char->integer #\return))
        (format #t "~!")))

    (define reset-count
      (lambda ()
        (set! total-count (+ total-count count))
        (format #t "                  ")
        (put-byte (current-output-port) (char->integer #\return))
        (format #t "~!")))

    (define exec-yasm
      (lambda ()
        (let ((result (system (format "yasm -o ~a /~a" yasm-obj-filename yasm-asm-filename))))
          (unless (= result 0) (abort result)))))

    (define exec-nano
      (lambda ()
        (let ((result (system (format "g++ -I../src ~a ~a -o ~a" nano-asm-filename nanoasm-o-filename nano-prog-filename))))
          (unless (= result 0) (abort result)))
        (let ((result (system nano-prog-filename)))
          (unless (= result 0) (abort result)))))

    (define exec-udcli
      (lambda ()
        (let ((result (system (format "udcli -~a ~a > ~a" (if on-x64 64 32) yasm-obj-filename yasm-disasm-filename))))
          (unless (= result 0) (abort result)))
        (let ((result (system (format "udcli -~a ~a > ~a" (if on-x64 64 32) nano-obj-filename nano-disasm-filename))))
          (unless (= result 0) (abort result)))))

    (define exec-diff
      (lambda ()
        (let ((result (system (format "diff ~a ~a > ~a" yasm-disasm-filename nano-disasm-filename diff-filename))))
          (unless (= result 0)
            (when (= result 2) (abort result))
            (call-with-input-file diff-filename
              (lambda (port)
                (let ((diff (get-string-all port)))
                  (unless (eof-object? diff)
                    (let ((yasm (pregexp-match "<.+\n" diff)) (nano (pregexp-match ">.+\n" diff)))
                      (format (current-error-port) "\n\n### TEST FAILURE ###\nyasm:\n~ananoasm:\n~a\n" (car yasm) (car nano))
                      (exit 1))))))))))

    (call-with-code-input
     (lambda (yasm-in nano-in)
       (let loop ((yasm (get-line yasm-in)) (nano (get-line nano-in)))
         (unless (eof-object? yasm)
           (assert (not (eof-object? nano)))
           (call-with-script-output
            (lambda (yasm-out nano-out)
              (format yasm-out "bits ~a\nL0:\n" (if on-x64 64 32))
              (format nano-out "~a\n" nano-template-head)
              (let loop ((n 0) (yasm yasm) (nano nano))
                (cond ((eof-object? yasm)
                       (assert (eof-object? nano))
                       (set! count (+ count n)))
                      (else
                       (format yasm-out "~a\n" yasm)
                       (format nano-out "~a\n" nano)
                       (if (< n code-chunk-count)
                           (loop (+ n 1) (get-line yasm-in) (get-line nano-in))
                           (set! count (+ count n))))))
              (format yasm-out "L1:\n")
              (format nano-out "~a\n" nano-template-tail)))
           (exec-yasm)
           (exec-nano)
           (exec-udcli)
           (exec-diff)
           (show-count)
           (loop (get-line yasm-in) (get-line nano-in))))
       (reset-count)))))

(define nano-template-head
  (format
   "#include \"nanoasm.h\"
      #include \"nanoasm_macro.h\"
      void fatal(const char* fmt, ...) {
          fflush(stdout);
          va_list ap;
          va_start(ap, fmt);
          vfprintf(stderr, fmt, ap);
          va_end(ap);
          fflush(stderr);
          exit(EXIT_FAILURE);
      }
      void assemble_test() {
          int fd = open(~s, (O_CREAT | O_WRONLY | O_TRUNC), (S_IRUSR | S_IWUSR));
          if (fd < 0) { perror(\"open\"); exit(1); }
          const int limit = 1024 * 1024 * 4;
          void* code = mmap(NULL, limit, PROT_EXEC | PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
          if (code == NULL) { perror(\"mmap\"); exit(1); }
          using_nanoasm;    
          GLOBAL(L0);
          GLOBAL(L1);
      __  org(code, limit);
      LABEL(L0);
      " nano-obj-filename))

(define nano-template-tail
  "
    LABEL(L1);   
        if (write(fd, code, nas.commit()) != nas.commit()) { perror(\"write\"); exit(1); }
        if (close(fd) < 0) { perror(\"close\"); exit(1); }
        munmap(code, limit);
    }
    int main(int argc, char** argv) { assemble_test(); return 0; }
    ")

(define put-inst-aux3
  (lambda (yasm nano op reg size mem imm)
    (let ((nano-op (cond ((assq op operator-alias-alist) => cdr) (else op))))
      (format yasm "~a ~a,~a~a,~a\n" op reg size mem imm)
      (format nano "__ ~a(~a,~a~a,~a);\n" nano-op reg size mem imm))))

(define put-inst-aux2
  (lambda (yasm nano op lhs1 lhs2 rhs1 rhs2)
    (let ((lhs1 (if lhs1 lhs1 ""))
          (lhs2 (if lhs2 lhs2 ""))
          (rhs1 (if rhs1 rhs1 ""))
          (rhs2 (if rhs2 rhs2 "")))
      (let ((nano-op (cond ((assq op operator-alias-alist) => cdr) (else op))))
        (format yasm "~a ~a~a,~a~a\n" op lhs1 lhs2 rhs1 rhs2)
        (format nano "__ ~a(~a~a,~a~a);\n" nano-op lhs1 lhs2 rhs1 rhs2)))))

(define put-inst-aux1
  (lambda (yasm nano op lhs1 lhs2)
    (let ((lhs1 (if lhs1 lhs1 ""))
          (lhs2 (if lhs2 lhs2 "")))
      (let ((nano-op (cond ((assq op operator-alias-alist) => cdr) (else op))))
        (format yasm "~a ~a~a\n" op lhs1 lhs2)
        (format nano "__ ~a(~a~a);\n" nano-op lhs1 lhs2)))))

(define put-inst-aux0
  (lambda (yasm nano op)
    (let ((nano-op (cond ((assq op operator-alias-alist) => cdr) (else op))))
      (format yasm "~a\n" op)
      (format nano "__ ~a();\n" nano-op))))

(define-syntax put-inst
  (syntax-rules (:)
    ((_ yasm nano (op reg : size mem : imm))
     (put-inst-aux3 yasm nano op reg size mem imm))
    ((_ yasm nano (op lhs1 lhs2 : rhs1 rhs2))
     (put-inst-aux2 yasm nano op lhs1 lhs2 rhs1 rhs2))
    ((_ yasm nano (op lhs1 : rhs1 rhs2))
     (put-inst-aux2 yasm nano op lhs1 #f rhs1 rhs2))
    ((_ yasm nano (op lhs1 lhs2 : rhs1))
     (put-inst-aux2 yasm nano op lhs1 lhs2 rhs1 #f))
    ((_ yasm nano (op lhs1 : rhs1))
     (put-inst-aux2 yasm nano op lhs1 #f rhs1 #f))
    ((_ yasm nano (op lhs1 lhs2))
     (put-inst-aux1 yasm nano op lhs1 lhs2))
    ((_ yasm nano (op lhs1))
     (put-inst-aux1 yasm nano op lhs1 #f))
    ((_ yasm nano (op))
     (put-inst-aux0 yasm nano op))))

;;;

(format #t ";; compile nanoasm.cpp ...\n")
(let ((result (system (format "g++ -pipe -O3 -c -I../src ../src/nanoasm.cpp -o ~a" nanoasm-o-filename))))
  (unless (= result 0) (exit result)))
(if quick-test 
    (format #t ";; *** quick-test mode ***\n")
    (format #t ";; *** full-test mode (very long) ***\n"))

;;;

(define test-mov
  (lambda ()

    (format #t ";; mov r8,r8\n")
    (call-with-code-output
     (lambda (yasm nano)
       (define generate (every-combination GPR8))
       (let loop ((reg2 (generate)))
         (when reg2
           (put-inst yasm nano ('mov (car reg2) : (cdr reg2)))
           (loop (generate))))))
    (run-test)

    (format #t ";; mov r8,m8\n")
    (call-with-code-output
     (lambda (yasm nano)
       (define generate (every-memory-operands))
       (let loop ((mem (generate)))
         (when mem
           (for-each (lambda (r8) (put-inst yasm nano ('mov r8 : 'byte mem))) GPR8)
           (loop (generate))))))
    (run-test)

    (format #t ";; mov m8,r8\n")
    (call-with-code-output
     (lambda (yasm nano)
       (define generate (every-memory-operands))
       (let loop ((mem (generate)))
         (when mem
           (for-each (lambda (r8) (put-inst yasm nano ('mov 'byte mem : r8))) GPR8)
           (loop (generate))))))
    (run-test)

    (format #t ";; mov r32,r32\n")
    (call-with-code-output
     (lambda (yasm nano)
       (define generate (every-combination GPR32))
       (let loop ((reg2 (generate)))
         (when reg2
           (put-inst yasm nano ('mov (car reg2) : (cdr reg2)))
           (loop (generate))))))
    (run-test)

    (format #t ";; mov r32,m32\n")
    (call-with-code-output
     (lambda (yasm nano)
       (define generate (every-memory-operands))
       (let loop ((mem (generate)))
         (when mem
           (for-each (lambda (r32) (put-inst yasm nano ('mov r32 : 'dword mem))) GPR32)
           (loop (generate))))))
    (run-test)

    (format #t ";; mov m32,r32\n")
    (call-with-code-output
     (lambda (yasm nano)
       (define generate (every-memory-operands))
       (let loop ((mem (generate)))
         (when mem
           (for-each (lambda (r32) (put-inst yasm nano ('mov 'dword mem : r32))) GPR32)
           (loop (generate))))))
    (run-test)

    (format #t ";; mov r8,imm8\n")
    (call-with-code-output
     (lambda (yasm nano)
       (for-each (lambda (r8) (put-inst yasm nano ('mov r8 : "0x12"))) GPR8)))
    (run-test)

    (format #t ";; mov m8,imm8\n")
    (call-with-code-output
     (lambda (yasm nano)
       (define generate (every-memory-operands))
       (let loop ((mem (generate)))
         (when mem
           (put-inst yasm nano ('mov 'byte mem : "0x12"))
           (loop (generate))))))
    (run-test)

    (format #t ";; mov r32,imm32\n")
    (call-with-code-output
     (lambda (yasm nano)
       (for-each (lambda (r32) (put-inst yasm nano ('mov r32 : "0x6A3B127"))) GPR32)))
    (run-test)

    (format #t ";; mov m32,imm32\n")
    (call-with-code-output
     (lambda (yasm nano)
       (define generate (every-memory-operands))
       (let loop ((mem (generate)))
         (when mem
           (put-inst yasm nano ('mov 'dword mem : "0x6A3B127"))
           (loop (generate))))))
    (run-test)

    (when on-x64

      (format #t ";; mov r64,r64\n")
      (call-with-code-output
       (lambda (yasm nano)
         (define generate (every-combination GPR64))
         (let loop ((reg2 (generate)))
           (when reg2
             (put-inst yasm nano ('mov (car reg2) : (cdr reg2)))
             (loop (generate))))))
      (run-test)

      (format #t ";; mov r64,m64\n")
      (call-with-code-output
       (lambda (yasm nano)
         (define generate (every-memory-operands))
         (let loop ((mem (generate)))
           (when mem
             (for-each (lambda (r64) (put-inst yasm nano ('mov r64 : 'qword mem))) GPR64)
             (loop (generate))))))
      (run-test)

      (format #t ";; mov m64,r64\n")
      (call-with-code-output
       (lambda (yasm nano)
         (define generate (every-memory-operands))
         (let loop ((mem (generate)))
           (when mem
             (for-each (lambda (r64) (put-inst yasm nano ('mov 'qword mem : r64))) GPR64)
             (loop (generate))))))
      (run-test)

      (format #t ";; mov r64,imm64\n")
      (call-with-code-output
       (lambda (yasm nano)
         (for-each (lambda (r64) (put-inst yasm nano ('mov r64 : "0x76A3B127ED5465"))) GPR64)))
      (run-test)

      (format #t ";; mov m64,imm32\n")
      (call-with-code-output
       (lambda (yasm nano)
         (define generate (every-memory-operands))
         (let loop ((mem (generate)))
           (when mem
             (put-inst yasm nano ('mov 'qword mem : "0x76A3B1"))
             (loop (generate))))))
      (run-test)

      (format #t ";; mov al,moffs8\n")
      (call-with-code-output
       (lambda (yasm nano)
         (format yasm "mov al,byte[qword 0x76A3B127ED5465]\n")
         (format nano "__ mov(al,byte[0x76A3B127ED5465]);\n")))
      (run-test)

      (format #t ";; mov moffs8,al\n")
      (call-with-code-output
       (lambda (yasm nano)
         (format yasm "mov byte[qword 0x76A3B127ED5465],al\n")
         (format nano "__ mov(byte[0x76A3B127ED5465],al);\n")))
      (run-test)

      (format #t ";; mov rax,moffs64\n")
      (call-with-code-output
       (lambda (yasm nano)
         (format yasm "mov rax,qword[qword 0x76A3B127ED5465]\n")
         (format nano "__ mov(rax,qword[0x76A3B127ED5465]);\n")))
      (run-test)

      (format #t ";; mov moffs64,rax\n")
      (call-with-code-output
       (lambda (yasm nano)
         (format yasm "mov qword[qword 0x76A3B127ED5465],rax\n")
         (format nano "__ mov(qword[0x76A3B127ED5465],rax);\n")))
      (run-test))))

(define test-add-group
  (lambda (inst)

    (format #t ";; ~a r8,r8\n" inst)
    (call-with-code-output
     (lambda (yasm nano)
       (define generate (every-combination GPR8))
       (let loop ((reg2 (generate)))
         (when reg2
           (put-inst yasm nano (inst (car reg2) : (cdr reg2)))
           (loop (generate))))))
    (run-test)

    (format #t ";; ~a m8,r8\n" inst)
    (call-with-code-output
     (lambda (yasm nano)
       (define generate (every-memory-operands))
       (let loop ((mem (generate)))
         (when mem
           (for-each (lambda (r8) (put-inst yasm nano (inst 'byte mem : r8))) GPR8)
           (loop (generate))))))
    (run-test)

    (format #t ";; ~a r8,m8\n" inst)
    (call-with-code-output
     (lambda (yasm nano)
       (define generate (every-memory-operands))
       (let loop ((mem (generate)))
         (when mem
           (for-each (lambda (r8) (put-inst yasm nano (inst r8 : 'byte mem))) GPR8)
           (loop (generate))))))
    (run-test)

    (format #t ";; ~a r8,imm8\n" inst)
    (call-with-code-output
     (lambda (yasm nano)
       (for-each (lambda (r8) (put-inst yasm nano (inst r8 : "0x43"))) GPR8)))
    (run-test)

    (format #t ";; ~a m8,imm8\n" inst)
    (call-with-code-output
     (lambda (yasm nano)
       (define generate (every-memory-operands))
       (let loop ((mem (generate)))
         (when mem
           (put-inst yasm nano (inst 'byte mem : "0x54"))
           (loop (generate))))))
    (run-test)

    (format #t ";; ~a r32,r32\n" inst)
    (call-with-code-output
     (lambda (yasm nano)
       (define generate (every-combination GPR32))
       (let loop ((reg2 (generate)))
         (when reg2
           (put-inst yasm nano (inst (car reg2) : (cdr reg2)))
           (loop (generate))))))
    (run-test)

    (format #t ";; ~a m32,r32\n" inst)
    (call-with-code-output
     (lambda (yasm nano)
       (define generate (every-memory-operands))
       (let loop ((mem (generate)))
         (when mem
           (for-each (lambda (r32) (put-inst yasm nano (inst 'dword mem : r32))) GPR32)
           (loop (generate))))))
    (run-test)

    (format #t ";; ~a r32,m32\n" inst)
    (call-with-code-output
     (lambda (yasm nano)
       (define generate (every-memory-operands))
       (let loop ((mem (generate)))
         (when mem
           (for-each (lambda (r32) (put-inst yasm nano (inst r32 : 'dword mem))) GPR32)
           (loop (generate))))))
    (run-test)

    (format #t ";; ~a r32,imm8\n" inst)
    (call-with-code-output
     (lambda (yasm nano)
       (for-each (lambda (r32) (put-inst yasm nano (inst r32 : "0x76"))) GPR32)))
    (run-test)

    (format #t ";; ~a m32,imm8\n" inst)
    (call-with-code-output
     (lambda (yasm nano)
       (define generate (every-memory-operands))
       (let loop ((mem (generate)))
         (when mem
           (put-inst yasm nano (inst 'dword mem : "0x54"))
           (loop (generate))))))
    (run-test)

    (format #t ";; ~a r32,imm32\n" inst)
    (call-with-code-output
     (lambda (yasm nano)
       (for-each (lambda (r32) (put-inst yasm nano (inst r32 : "0x42321C5"))) GPR32)))
    (run-test)

    (format #t ";; ~a m32,imm32\n" inst)
    (call-with-code-output
     (lambda (yasm nano)
       (define generate (every-memory-operands))
       (let loop ((mem (generate)))
         (when mem
           (put-inst yasm nano (inst 'dword mem : "0x1C54232"))
           (loop (generate))))))
    (run-test)

    (when on-x64

      (format #t ";; ~a r64,r64\n" inst)
      (call-with-code-output
       (lambda (yasm nano)
         (define generate (every-combination GPR64))
         (let loop ((reg2 (generate)))
           (when reg2
             (put-inst yasm nano (inst (car reg2) : (cdr reg2)))
             (loop (generate))))))
      (run-test)

      (format #t ";; ~a m64,r64\n" inst)
      (call-with-code-output
       (lambda (yasm nano)
         (define generate (every-memory-operands))
         (let loop ((mem (generate)))
           (when mem
             (for-each (lambda (r64) (put-inst yasm nano (inst 'qword mem : r64))) GPR64)
             (loop (generate))))))
      (run-test)

      (format #t ";; ~a r64,m64\n" inst)
      (call-with-code-output
       (lambda (yasm nano)
         (define generate (every-memory-operands))
         (let loop ((mem (generate)))
           (when mem
             (for-each (lambda (r64) (put-inst yasm nano (inst r64 : 'qword mem))) GPR64)
             (loop (generate))))))
      (run-test)

      (format #t ";; ~a r64,imm8\n" inst)
      (call-with-code-output
       (lambda (yasm nano)
         (for-each (lambda (r64) (put-inst yasm nano (inst r64 : "0x32"))) GPR64)))
      (run-test)

      (format #t ";; ~a m64,imm8\n" inst)
      (call-with-code-output
       (lambda (yasm nano)
         (define generate (every-memory-operands))
         (let loop ((mem (generate)))
           (when mem
             (put-inst yasm nano (inst 'qword mem : "0x54"))
             (loop (generate))))))
      (run-test)

      (format #t ";; ~a r64,imm32\n" inst)
      (call-with-code-output
       (lambda (yasm nano)
         (for-each (lambda (r64) (put-inst yasm nano (inst r64 : "0x421C532"))) GPR64)))
      (run-test)

      (format #t ";; ~a m64,imm32\n" inst)
      (call-with-code-output
       (lambda (yasm nano)
         (define generate (every-memory-operands))
         (let loop ((mem (generate)))
           (when mem
             (put-inst yasm nano (inst 'qword mem : "0x1C54232"))
             (loop (generate))))))
      (run-test))))

(define test-rol-group
  (lambda (inst)

    (format #t ";; ~a r8,1\n" inst)
    (call-with-code-output
     (lambda (yasm nano)
       (for-each (lambda (r8) (put-inst yasm nano (inst r8 : 1))) GPR8)))
    (run-test)

    (format #t ";; ~a r8,cl\n" inst)
    (call-with-code-output
     (lambda (yasm nano)
       (for-each (lambda (r8) (put-inst yasm nano (inst r8 : 'cl))) GPR8)))
    (run-test)

    (format #t ";; ~a r8,imm8\n" inst)
    (call-with-code-output
     (lambda (yasm nano)
       (for-each (lambda (r8) (put-inst yasm nano (inst r8 : "0x05"))) GPR8)))
    (run-test)

    (format #t ";; ~a m8,1\n" inst)
    (call-with-code-output
     (lambda (yasm nano)
       (define generate (every-memory-operands))
       (let loop ((mem (generate)))
         (when mem
           (put-inst yasm nano (inst 'byte mem : 1))
           (loop (generate))))))
    (run-test)

    (format #t ";; ~a m8,cl\n" inst)
    (call-with-code-output
     (lambda (yasm nano)
       (define generate (every-memory-operands))
       (let loop ((mem (generate)))
         (when mem
           (put-inst yasm nano (inst 'byte mem : 'cl))
           (loop (generate))))))
    (run-test)

    (format #t ";; ~a m8,imm8\n" inst)
    (call-with-code-output
     (lambda (yasm nano)
       (define generate (every-memory-operands))
       (let loop ((mem (generate)))
         (when mem
           (put-inst yasm nano (inst 'byte mem : "0x05"))
           (loop (generate))))))
    (run-test)

    (format #t ";; ~a r32,1\n" inst)
    (call-with-code-output
     (lambda (yasm nano)
       (for-each (lambda (r32) (put-inst yasm nano (inst r32 : 1))) GPR32)))
    (run-test)

    (format #t ";; ~a r32,cl\n" inst)
    (call-with-code-output
     (lambda (yasm nano)
       (for-each (lambda (r32) (put-inst yasm nano (inst r32 : 'cl))) GPR32)))
    (run-test)

    (format #t ";; ~a r32,imm8\n" inst)
    (call-with-code-output
     (lambda (yasm nano)
       (for-each (lambda (r32) (put-inst yasm nano (inst r32 : "0x05"))) GPR32)))
    (run-test)

    (format #t ";; ~a m32,1\n" inst)
    (call-with-code-output
     (lambda (yasm nano)
       (define generate (every-memory-operands))
       (let loop ((mem (generate)))
         (when mem
           (put-inst yasm nano (inst 'dword mem : 1))
           (loop (generate))))))
    (run-test)

    (format #t ";; ~a m32,cl\n" inst)
    (call-with-code-output
     (lambda (yasm nano)
       (define generate (every-memory-operands))
       (let loop ((mem (generate)))
         (when mem
           (put-inst yasm nano (inst 'dword mem : 'cl))
           (loop (generate))))))
    (run-test)

    (format #t ";; ~a m32,imm8\n" inst)
    (call-with-code-output
     (lambda (yasm nano)
       (define generate (every-memory-operands))
       (let loop ((mem (generate)))
         (when mem
           (put-inst yasm nano (inst 'dword mem : "0x05"))
           (loop (generate))))))
    (run-test)

    (when on-x64

      (format #t ";; ~a r64,1\n" inst)
      (call-with-code-output
       (lambda (yasm nano)
         (for-each (lambda (r64) (put-inst yasm nano (inst r64 : 1))) GPR64)))
      (run-test)

      (format #t ";; ~a r64,cl\n" inst)
      (call-with-code-output
       (lambda (yasm nano)
         (for-each (lambda (r64) (put-inst yasm nano (inst r64 : 'cl))) GPR64)))
      (run-test)

      (format #t ";; ~a r64,imm8\n" inst)
      (call-with-code-output
       (lambda (yasm nano)
         (for-each (lambda (r64) (put-inst yasm nano (inst r64 : "0x05"))) GPR64)))
      (run-test)

      (format #t ";; ~a m64,1\n" inst)
      (call-with-code-output
       (lambda (yasm nano)
         (define generate (every-memory-operands))
         (let loop ((mem (generate)))
           (when mem
             (put-inst yasm nano (inst 'qword mem : 1))
             (loop (generate))))))
      (run-test)

      (format #t ";; ~a m64,cl\n" inst)
      (call-with-code-output
       (lambda (yasm nano)
         (define generate (every-memory-operands))
         (let loop ((mem (generate)))
           (when mem
             (put-inst yasm nano (inst 'qword mem : 'cl))
             (loop (generate))))))
      (run-test)

      (format #t ";; ~a m64,imm8\n" inst)
      (call-with-code-output
       (lambda (yasm nano)
         (define generate (every-memory-operands))
         (let loop ((mem (generate)))
           (when mem
             (put-inst yasm nano (inst 'qword mem : "0x05"))
             (loop (generate))))))
      (run-test))))

(define test-inc-group
  (lambda (inst)

    (format #t ";; ~a r8\n" inst)
    (call-with-code-output
     (lambda (yasm nano)
       (for-each (lambda (r8) (put-inst yasm nano (inst r8))) GPR8)))
    (run-test)

    (format #t ";; ~a m8\n" inst)
    (call-with-code-output
     (lambda (yasm nano)
       (define generate (every-memory-operands))
       (let loop ((mem (generate)))
         (when mem
           (put-inst yasm nano (inst 'byte mem))
           (loop (generate))))))
    (run-test)

    (format #t ";; ~a r32\n" inst)
    (call-with-code-output
     (lambda (yasm nano)
       (for-each (lambda (r32) (put-inst yasm nano (inst r32))) GPR32)))
    (run-test)

    (format #t ";; ~a m32\n" inst)
    (call-with-code-output
     (lambda (yasm nano)
       (define generate (every-memory-operands))
       (let loop ((mem (generate)))
         (when mem
           (put-inst yasm nano (inst 'dword mem))
           (loop (generate))))))
    (run-test)

    (when on-x64

      (format #t ";; ~a r64\n" inst)
      (call-with-code-output
       (lambda (yasm nano)
         (for-each (lambda (r64) (put-inst yasm nano (inst r64))) GPR64)))
      (run-test)

      (format #t ";; ~a m64\n" inst)
      (call-with-code-output
       (lambda (yasm nano)
         (define generate (every-memory-operands))
         (let loop ((mem (generate)))
           (when mem
             (put-inst yasm nano (inst 'qword mem))
             (loop (generate))))))
      (run-test))))

(define test-jcc-group
  (lambda (inst)
    (format #t ";; ~a\n" inst)
    (call-with-code-output
     (lambda (yasm nano)
       (put-inst yasm nano (inst 'L0)) ; short backward
       (put-inst yasm nano (inst 'L1)) ; near forward
       (dotimes 100 (put-inst yasm nano ('dq 0)))
       (put-inst yasm nano (inst 'L0)) ; near backward     
       (format yasm "~a short L1\n" inst) ; short forward
       (format nano "__ ~a_short(L1);\n" inst))) ; short forward
    (run-test)))

(define test-push-group
  (lambda (inst)

    (when (eq? inst 'push)
      (format #t ";; ~a imm8\n" inst)
      (call-with-code-output
       (lambda (yasm nano)
         (put-inst yasm nano (inst "0x22"))))
      (run-test)

      (format #t ";; ~a imm32\n" inst)
      (call-with-code-output
       (lambda (yasm nano)
         (put-inst yasm nano (inst "0x22EE98"))))
      (run-test))

    (cond (on-x64
           (format #t ";; ~a r64\n" inst)
           (call-with-code-output
            (lambda (yasm nano)
              (for-each (lambda (r64) (put-inst yasm nano (inst r64))) GPR64)))
           (run-test)
           (format #t ";; ~a m64\n" inst)
           (call-with-code-output
            (lambda (yasm nano)
              (define generate (every-memory-operands))
              (let loop ((mem (generate)))
                (when mem
                  (put-inst yasm nano (inst 'qword mem))
                  (loop (generate))))))
           (run-test))
          (else
           (format #t ";; ~a r32\n" inst)
           (call-with-code-output
            (lambda (yasm nano)
              (for-each (lambda (r32) (put-inst yasm nano (inst r32))) GPR32)))
           (run-test)
           (format #t ";; ~a m32\n" inst)
           (call-with-code-output
            (lambda (yasm nano)
              (define generate (every-memory-operands))
              (let loop ((mem (generate)))
                (when mem
                  (put-inst yasm nano (inst 'dword mem))
                  (loop (generate))))))
           (run-test)))))

(define test-test
  (lambda ()
    (format #t ";; test r8,r8\n")
    (call-with-code-output
     (lambda (yasm nano)
       (define generate (every-combination GPR8))
       (let loop ((reg2 (generate)))
         (when reg2
           (put-inst yasm nano ('test (car reg2) : (cdr reg2)))
           (loop (generate))))))
    (run-test)

    (format #t ";; test m8,r8\n")
    (call-with-code-output
     (lambda (yasm nano)
       (define generate (every-memory-operands))
       (let loop ((mem (generate)))
         (when mem
           (for-each (lambda (r8) (put-inst yasm nano ('test 'byte mem : r8))) GPR8)
           (loop (generate))))))
    (run-test)

    (format #t ";; test r8,imm8\n")
    (call-with-code-output
     (lambda (yasm nano)
       (for-each (lambda (r8) (put-inst yasm nano ('test r8 : "0x29"))) GPR8)))
    (run-test)

    (format #t ";; test m8,imm8\n")
    (call-with-code-output
     (lambda (yasm nano)
       (define generate (every-memory-operands))
       (let loop ((mem (generate)))
         (when mem
           (put-inst yasm nano ('test 'byte mem : "0x29"))
           (loop (generate))))))
    (run-test)

    (format #t ";; test r32,r32\n")
    (call-with-code-output
     (lambda (yasm nano)
       (define generate (every-combination GPR32))
       (let loop ((reg2 (generate)))
         (when reg2
           (put-inst yasm nano ('test (car reg2) : (cdr reg2)))
           (loop (generate))))))
    (run-test)

    (format #t ";; test m32,r32\n")
    (call-with-code-output
     (lambda (yasm nano)
       (define generate (every-memory-operands))
       (let loop ((mem (generate)))
         (when mem
           (for-each (lambda (r32) (put-inst yasm nano ('test 'dword mem : r32))) GPR32)
           (loop (generate))))))
    (run-test)

    (format #t ";; test r32,imm32\n")
    (call-with-code-output
     (lambda (yasm nano)
       (for-each (lambda (r32) (put-inst yasm nano ('test r32 : "0x45DF29"))) GPR32)))
    (run-test)

    (format #t ";; test m32,imm32\n")
    (call-with-code-output
     (lambda (yasm nano)
       (define generate (every-memory-operands))
       (let loop ((mem (generate)))
         (when mem
           (put-inst yasm nano ('test 'dword mem : "0x45DF29"))
           (loop (generate))))))
    (run-test)

    (when on-x64

      (format #t ";; test r64,r64\n")
      (call-with-code-output
       (lambda (yasm nano)
         (define generate (every-combination GPR64))
         (let loop ((reg2 (generate)))
           (when reg2
             (put-inst yasm nano ('test (car reg2) : (cdr reg2)))
             (loop (generate))))))
      (run-test)

      (format #t ";; test m64,r64\n")
      (call-with-code-output
       (lambda (yasm nano)
         (define generate (every-memory-operands))
         (let loop ((mem (generate)))
           (when mem
             (for-each (lambda (r64) (put-inst yasm nano ('test 'qword mem : r64))) GPR64)
             (loop (generate))))))
      (run-test)

      (format #t ";; test r64,imm32\n")
      (call-with-code-output
       (lambda (yasm nano)
         (for-each (lambda (r64) (put-inst yasm nano ('test r64 : "0x45DF29"))) GPR64)))
      (run-test)

      (format #t ";; test m64,imm32\n")
      (call-with-code-output
       (lambda (yasm nano)
         (define generate (every-memory-operands))
         (let loop ((mem (generate)))
           (when mem
             (put-inst yasm nano ('test 'qword mem : "0x45DF29"))
             (loop (generate))))))
      (run-test))))

(define test-not-group
  (lambda (inst)

    (format #t ";; ~a r8\n" inst)
    (call-with-code-output
     (lambda (yasm nano)
       (for-each (lambda (r8) (put-inst yasm nano (inst r8))) GPR8)))
    (run-test)

    (format #t ";; ~a m8\n" inst)
    (call-with-code-output
     (lambda (yasm nano)
       (define generate (every-memory-operands))
       (let loop ((mem (generate)))
         (when mem
           (put-inst yasm nano (inst 'byte mem))
           (loop (generate))))))
    (run-test)

    (format #t ";; ~a r32\n" inst)
    (call-with-code-output
     (lambda (yasm nano)
       (for-each (lambda (r32) (put-inst yasm nano (inst r32))) GPR32)))
    (run-test)

    (format #t ";; ~a m32\n" inst)
    (call-with-code-output
     (lambda (yasm nano)
       (define generate (every-memory-operands))
       (let loop ((mem (generate)))
         (when mem
           (put-inst yasm nano (inst 'dword mem))
           (loop (generate))))))
    (run-test)

    (when on-x64

      (format #t ";; ~a r64\n" inst)
      (call-with-code-output
       (lambda (yasm nano)
         (for-each (lambda (r64) (put-inst yasm nano (inst r64))) GPR64)))
      (run-test)

      (format #t ";; ~a m64\n" inst)
      (call-with-code-output
       (lambda (yasm nano)
         (define generate (every-memory-operands))
         (let loop ((mem (generate)))
           (when mem
             (put-inst yasm nano (inst 'qword mem))
             (loop (generate))))))
      (run-test))))

(define test-imul
  (lambda ()

    (format #t ";; imul r32,imm8\n")
    (call-with-code-output
     (lambda (yasm nano)
       (for-each (lambda (r32) (put-inst yasm nano ('imul r32 : "0x20"))) GPR32)))
    (run-test)

    (format #t ";; imul r32,imm32\n")
    (call-with-code-output
     (lambda (yasm nano)
       (for-each (lambda (r32) (put-inst yasm nano ('imul r32 : "0x26782A32"))) GPR32)))
    (run-test)

    (format #t ";; imul r32,r32\n")
    (call-with-code-output
     (lambda (yasm nano)
       (define generate (every-combination GPR32))
       (let loop ((reg2 (generate)))
         (when reg2
           (put-inst yasm nano ('imul (car reg2) : (cdr reg2)))
           (loop (generate))))))
    (run-test)

    (format #t ";; imul r32,m32\n")
    (call-with-code-output
     (lambda (yasm nano)
       (define generate (every-memory-operands))
       (let loop ((mem (generate)))
         (when mem
           (for-each (lambda (r32) (put-inst yasm nano ('imul r32 : 'dword mem))) GPR32)
           (loop (generate))))))
    (run-test)

    (format #t ";; imul r32,m32,imm8\n")
    (call-with-code-output
     (lambda (yasm nano)
       (define generate (every-memory-operands))
       (let loop ((mem (generate)))
         (when mem
           (for-each (lambda (r32) (put-inst yasm nano ('imul r32 : 'dword mem : "0x57"))) GPR32)
           (loop (generate))))))
    (run-test)

    (format #t ";; imul r32,m32,imm32\n")
    (call-with-code-output
     (lambda (yasm nano)
       (define generate (every-memory-operands))
       (let loop ((mem (generate)))
         (when mem
           (for-each (lambda (r32) (put-inst yasm nano ('imul r32 : 'dword mem : "0x433357"))) GPR32)
           (loop (generate))))))
    (run-test)

    (when on-x64

      (format #t ";; imul r64,imm8\n")
      (call-with-code-output
       (lambda (yasm nano)
         (for-each (lambda (r64) (put-inst yasm nano ('imul r64 : "0x20"))) GPR64)))
      (run-test)

      (format #t ";; imul r64,imm32\n")
      (call-with-code-output
       (lambda (yasm nano)
         (for-each (lambda (r64) (put-inst yasm nano ('imul r64 : "0x26782A32"))) GPR64)))
      (run-test)

      (format #t ";; imul r64,r64\n")
      (call-with-code-output
       (lambda (yasm nano)
         (define generate (every-combination GPR64))
         (let loop ((reg2 (generate)))
           (when reg2
             (put-inst yasm nano ('imul (car reg2) : (cdr reg2)))
             (loop (generate))))))
      (run-test)

      (format #t ";; imul r64,m64\n")
      (call-with-code-output
       (lambda (yasm nano)
         (define generate (every-memory-operands))
         (let loop ((mem (generate)))
           (when mem
             (for-each (lambda (r64) (put-inst yasm nano ('imul r64 : 'qword mem))) GPR64)
             (loop (generate))))))
      (run-test)

      (format #t ";; imul r64,m64,imm8\n")
      (call-with-code-output
       (lambda (yasm nano)
         (define generate (every-memory-operands))
         (let loop ((mem (generate)))
           (when mem
             (for-each (lambda (r64) (put-inst yasm nano ('imul r64 : 'qword mem : "0x57"))) GPR64)
             (loop (generate))))))
      (run-test)

      (format #t ";; imul r64,m64,imm64\n")
      (call-with-code-output
       (lambda (yasm nano)
         (define generate (every-memory-operands))
         (let loop ((mem (generate)))
           (when mem
             (for-each (lambda (r64) (put-inst yasm nano ('imul r64 : 'qword mem : "0x433357"))) GPR64)
             (loop (generate))))))
      (run-test))))

(define test-cmov-group
  (lambda (inst)

    (format #t ";; ~a r32,r32\n" inst)
    (call-with-code-output
     (lambda (yasm nano)
       (define generate (every-combination GPR32))
       (let loop ((reg2 (generate)))
         (when reg2
           (put-inst yasm nano (inst (car reg2) : (cdr reg2)))
           (loop (generate))))))
    (run-test)

    (format #t ";; ~a r32,m32\n" inst)
    (call-with-code-output
     (lambda (yasm nano)
       (define generate (every-memory-operands))
       (let loop ((mem (generate)))
         (when mem
           (for-each (lambda (r32) (put-inst yasm nano (inst r32 : 'dword mem))) GPR32)
           (loop (generate))))))
    (run-test)

    (when on-x64

      (format #t ";; ~a r64,r64\n" inst)
      (call-with-code-output
       (lambda (yasm nano)
         (define generate (every-combination GPR64))
         (let loop ((reg2 (generate)))
           (when reg2
             (put-inst yasm nano (inst (car reg2) : (cdr reg2)))
             (loop (generate))))))
      (run-test)

      (format #t ";; ~a r64,m64\n" inst)
      (call-with-code-output
       (lambda (yasm nano)
         (define generate (every-memory-operands))
         (let loop ((mem (generate)))
           (when mem
             (for-each (lambda (r64) (put-inst yasm nano (inst r64 : 'qword mem))) GPR64)
             (loop (generate))))))
      (run-test))))

(define test-setcc-group
  (lambda (inst)

    (format #t ";; ~a r8\n" inst)
    (call-with-code-output
     (lambda (yasm nano)
       (for-each (lambda (r8) (put-inst yasm nano (inst r8))) GPR8)))
    (run-test)

    (format #t ";; ~a m8\n" inst)
    (call-with-code-output
     (lambda (yasm nano)
       (define generate (every-memory-operands))
       (let loop ((mem (generate)))
         (when mem
           (put-inst yasm nano (inst 'byte mem))
           (loop (generate))))))
    (run-test)))

(define test-movzx
  (lambda ()

    (format #t ";; movzx r32,r8\n")
    (call-with-code-output
     (lambda (yasm nano)
       (define generate (every-combination2 GPR32 GPR8))
       (let loop ((reg2 (generate)))
         (when reg2
           (put-inst yasm nano ('movzx (car reg2) : (cdr reg2)))
           (loop (generate))))))
    (run-test)

    (format #t ";; movzx r32,m8\n")
    (call-with-code-output
     (lambda (yasm nano)
       (define generate (every-memory-operands))
       (let loop ((mem (generate)))
         (when mem
           (for-each (lambda (r32) (put-inst yasm nano ('movzx r32 : 'byte mem))) GPR32)
           (loop (generate))))))
    (run-test)

    (when on-x64

      (format #t ";; movzx r64,r8\n")
      (call-with-code-output
       (lambda (yasm nano)
         (define generate (every-combination2 GPR64 GPR8))
         (let loop ((reg2 (generate)))
           (when reg2
             (put-inst yasm nano ('movzx (car reg2) : (cdr reg2)))
             (loop (generate))))))
      (run-test)

      (format #t ";; movzx r64,m8\n")
      (call-with-code-output
       (lambda (yasm nano)
         (define generate (every-memory-operands))
         (let loop ((mem (generate)))
           (when mem
             (for-each (lambda (r64) (put-inst yasm nano ('movzx r64 : 'byte mem))) GPR64)
             (loop (generate))))))
      (run-test))))



(define test-movsx
  (lambda ()

    (format #t ";; movsx r32,r8\n")
    (call-with-code-output
     (lambda (yasm nano)
       (define generate (every-combination2 GPR32 GPR8))
       (let loop ((reg2 (generate)))
         (when reg2
           (put-inst yasm nano ('movsx (car reg2) : (cdr reg2)))
           (loop (generate))))))
    (run-test)

    (format #t ";; movsx r32,m8\n")
    (call-with-code-output
     (lambda (yasm nano)
       (define generate (every-memory-operands))
       (let loop ((mem (generate)))
         (when mem
           (for-each (lambda (r32) (put-inst yasm nano ('movsx r32 : 'byte mem))) GPR32)
           (loop (generate))))))
    (run-test)

    (when on-x64

      (format #t ";; movsx r64,r8\n")
      (call-with-code-output
       (lambda (yasm nano)
         (define generate (every-combination2 GPR64 GPR8))
         (let loop ((reg2 (generate)))
           (when reg2
             (put-inst yasm nano ('movsx (car reg2) : (cdr reg2)))
             (loop (generate))))))
      (run-test)

      (format #t ";; movsx r64,m8\n")
      (call-with-code-output
       (lambda (yasm nano)
         (define generate (every-memory-operands))
         (let loop ((mem (generate)))
           (when mem
             (for-each (lambda (r64) (put-inst yasm nano ('movsx r64 : 'byte mem))) GPR64)
             (loop (generate))))))
      (run-test)

      (format #t ";; movsx r64,r32\n")
      (call-with-code-output
       (lambda (yasm nano)
         (define generate (every-combination2 GPR64 GPR32))
         (let loop ((reg2 (generate)))
           (when reg2
             (put-inst yasm nano ('movsxd (car reg2) : (cdr reg2)))
             (loop (generate))))))
      (run-test)

      (format #t ";; movsx r64,m32\n")
      (call-with-code-output
       (lambda (yasm nano)
         (define generate (every-memory-operands))
         (let loop ((mem (generate)))
           (when mem
             (for-each (lambda (r64) (put-inst yasm nano ('movsxd r64 : 'dword mem))) GPR64)
             (loop (generate))))))
      (run-test))))

(define test-lea
  (lambda ()
    (cond (on-x64
           (format #t ";; lea r64,m64\n")
           (call-with-code-output
            (lambda (yasm nano)
              (define generate (every-memory-operands))
              (let loop ((mem (generate)))
                (when mem
                  (for-each (lambda (r64) (put-inst yasm nano ('lea r64 : 'qword mem))) GPR64)
                  (loop (generate))))))
           (run-test))
          (else
           (format #t ";; lea r32,m32\n")
           (call-with-code-output
            (lambda (yasm nano)
              (define generate (every-memory-operands))
              (let loop ((mem (generate)))
                (when mem
                  (for-each (lambda (r32) (put-inst yasm nano ('lea r32 : 'dword mem))) GPR32)
                  (loop (generate))))))
           (run-test)))))

(define test-jmp
  (lambda ()

    (format #t ";; jmp rel\n")
    (call-with-code-output
     (lambda (yasm nano)
       (put-inst yasm nano ('jmp 'L0)) ; short backward
       (put-inst yasm nano ('jmp 'L1)) ; near forward
       (dotimes 100 (put-inst yasm nano ('dq 0)))
       (put-inst yasm nano ('jmp 'L0)) ; near backward     
       (format yasm "jmp short L1\n") ; short forward
       (format nano "__ jmp_short(L1);\n"))) ; short forward
    (run-test)

    (cond (on-x64
           (format #t ";; jmp r64\n")
           (call-with-code-output
            (lambda (yasm nano)
              (for-each (lambda (r64) (put-inst yasm nano ('jmp r64))) GPR64)))
           (run-test)
           (format #t ";; jmp m32\n")
           (call-with-code-output
            (lambda (yasm nano)
              (define generate (every-memory-operands))
              (let loop ((mem (generate)))
                (when mem
                  (put-inst yasm nano ('jmp 'qword mem))
                  (loop (generate))))))
           (run-test))
          (else
           (format #t ";; jmp r32\n")
           (call-with-code-output
            (lambda (yasm nano)
              (for-each (lambda (r32) (put-inst yasm nano ('jmp r32))) GPR32)))
           (run-test)
           (format #t ";; jmp m32\n")
           (call-with-code-output
            (lambda (yasm nano)
              (define generate (every-memory-operands))
              (let loop ((mem (generate)))
                (when mem
                  (put-inst yasm nano ('jmp 'dword mem))
                  (loop (generate))))))
           (run-test)))))

(define test-call
  (lambda ()

    (format #t ";; call rel\n")
    (call-with-code-output
     (lambda (yasm nano)
       (put-inst yasm nano ('call 'L0))   ; backward
       (put-inst yasm nano ('call 'L1)))) ; forward
    (run-test)

    (cond (on-x64
           (format #t ";; call r64\n")
           (call-with-code-output
            (lambda (yasm nano)
              (for-each (lambda (r64) (put-inst yasm nano ('call r64))) GPR64)))
           (run-test)
           (format #t ";; call m32\n")
           (call-with-code-output
            (lambda (yasm nano)
              (define generate (every-memory-operands))
              (let loop ((mem (generate)))
                (when mem
                  (put-inst yasm nano ('call 'qword mem))
                  (loop (generate))))))
           (run-test))
          (else
           (format #t ";; call r32\n")
           (call-with-code-output
            (lambda (yasm nano)
              (for-each (lambda (r32) (put-inst yasm nano ('call r32))) GPR32)))
           (run-test)
           (format #t ";; call m32\n")
           (call-with-code-output
            (lambda (yasm nano)
              (define generate (every-memory-operands))
              (let loop ((mem (generate)))
                (when mem
                  (put-inst yasm nano ('call 'dword mem))
                  (loop (generate))))))
           (run-test)))))

(define test-misc
  (lambda ()
    (format #t ";; ret\n")
    (call-with-code-output
     (lambda (yasm nano)
       (put-inst yasm nano ('ret))))
    (run-test)))


;;;

(for-each test-add-group
          '(add or adc sbb and sub xor cmp))
(for-each test-rol-group
          '(rol ror rcl rcr shl shr sar))
(for-each test-inc-group
          '(inc dec))
(for-each test-jcc-group
          '(ja jae jb jbe jc je jg jge jl jle jna jnae jnb jnbe jnc jne jng jnge jnl jnle jno jnp jns jnz jo jp jpe jpo js jz))
(for-each test-push-group
          '(push pop))
(for-each test-not-group
          '(not neg mul imul div idiv))
(for-each test-cmov-group
          '(cmova cmovae cmovb cmovbe cmovc cmove cmovg cmovge cmovl cmovle cmovna cmovnae cmovnb
                  cmovnbe cmovnc cmovne cmovng cmovnge cmovnl cmovnle cmovno cmovnp cmovns cmovnz
                  cmovo cmovp cmovpe cmovpo cmovs cmovz))
(for-each test-setcc-group
          '(seta setae setb setbe setc sete setg setge setl setle setna setnae setnb
                 setnbe setnc setne setng setnge setnl setnle setno setnp setns setnz
                 seto setp setpe setpo sets setz))
(test-imul)
(test-mov)
(test-movzx)
(test-movsx)
(test-test)
(test-lea)
(test-jmp)
(test-call)
(test-misc)

(format #t "total passed ~a~%" total-count)

