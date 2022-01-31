(import (core) (test-lite))

(display "More hygiene testing\n\n")

(test-begin "Check issue 1") ; http://code.google.com/p/ypsilon/issues/detail?id=1
(test-eval! (import (rnrs)))
(test-eval! (define foo
              (lambda (x)
                (define-syntax define-members
                  (lambda (x)
                    (syntax-case x ()
                      [(_ n [(c c* ...) s s* ...] ...)
                       #'(define (n e)
                           (syntax-case e ()
                             [(k c* ...)
                              (free-identifier=? #'k #'c)
                              (begin s s* ...)] ...
                             [_ #f]))])))
                (define-members parse-member
                                [(field name)       (list 'field #'name #f)]
                                [(field name init)  (list 'field #'name #'init)]
                                [(event name)       (list 'event #'name)]
                                [(ctor (arg ...) body body* ...)
                                 (list 'ctor #'ctor #'(arg ...) #'(body body* ...))]
                                [(property name)    (list 'property #'name)])
                (syntax-case x ()
                  [(e ...)
                   (map parse-member #'(e ...))]))))
(test-equal "free id 1"
            (map (lambda (a) (and a #t))
                 (foo #'((field bar)
                         (field bar #f)
                         (property baz)
                         (event foo x)
                         (ctor (a d) #f)
                         (field))))
            => (#t #t #t #f #t #f))
(test-end)

(test-begin "Check issue 10") ; http://code.google.com/p/ypsilon/issues/detail?id=10
(test-eval! (library (yp-def-dup)
              (export &A?)
              (import (rnrs))

              (define-condition-type &A &condition
                make-&A &A?)))
(test-equal #t #t)
(test-end)

(test-begin "Check issue 16") ; http://code.google.com/p/ypsilon/issues/detail?id=16
(test-eval! (library (my-define)
              (export
               (rename (my:define define)))
              (import
               (rnrs))
              (define-syntax my:define
                (lambda (stx)
                  (syntax-case stx ()
                    [(_ x . y)
                     #'(define x . y)])))))
(test-eval! (import (my-define)))
(test-eval! (define x 1))
(test-equal #t #t)
(test-end)

(test-begin "Check issue 18") ; http://code.google.com/p/ypsilon/issues/detail?id=18
(test-eval! (library (R)
              (export
               define-record-type)
              (import
               (except (rnrs) define-record-type))

              (define-syntax define-record-type
                (syntax-rules ()
                  [(_ n c ...)
                   (define (n) '#(n c ...))]))))
(test-eval! (library (I)
              (export
               insert)
              (import
               (rnrs))

              (define-syntax insert
                (lambda (stx)
                  (syntax-case stx ()
                    [(ctxt)
                     (datum->syntax #'ctxt
                                    '(define-record-type T (not normal) 123))])))))

(test-eval! (library (L)
              (export)
              (import
               (except (rnrs) define-record-type)
               (R)
               (I))

              (insert)))
(test-equal #t #t)
(test-end)

(test-begin "Check issue 22") ; http://code.google.com/p/ypsilon/issues/detail?id=22
(test-equal-evaluated (guard (ex [else
                                  (display "------------------\n")
                                  (write (simple-conditions ex))
                                  (display "\n------------------\n")])
                        (eval 'bitwise-and (environment '(rnrs arithmetic bitwise))))
                      =>
                      bitwise-and)
(test-end)

(test-begin "Check issue 29") ; http://code.google.com/p/ypsilon/issues/detail?id=29
(test-eval! (import (rnrs)))
(test-eval! (define-syntax foo
              (lambda (x)
                (define (bar-foo x)
                  (syntax-case x (bar)
                    [(bar n . e) #'#f]))
                (define bar)
                (syntax-case x ()
                  [(_ (bars ...))
                   #`(list . #,(map bar-foo #'(bars ...)))]))))
(test-syntax-violation (foo ((bar hey))))
(test-end)

(test-begin "Check issue 32") ; http://code.google.com/p/ypsilon/issues/detail?id=32
(test-eval! (import (rnrs)))
(test-eval! (define-syntax my-letrec
              (syntax-rules ()
                [(_ ([v e] ...) . b)
                 (let ()
                   (define t (list e ...))
                   (define v (let ([v (car t)]) (set! t (cdr t)) v))
                   ...
                   . b)])))
(test-equal (my-letrec ([f (lambda (x) (g x 2))]
                        [g (lambda (x y) (+ x y))])
                       (f 1))
            => 3)
(test-end)

(test-begin "Check issue 33.1") ; http://code.google.com/p/ypsilon/issues/detail?id=33
(test-eval! (import (rnrs)))
(test-eval! (library (L)
              (export s)
              (import (rnrs))
              (define-syntax s
                (lambda (x)
                  (syntax-case x (not)
                    ((_ not)
                     (syntax 'yes)))))))
(test-eval! (import (L)))
(test-equal (s not) yes)
(test-syntax-violation (let ((not 10)) (s not)))
(test-end)

(test-begin "Check issue 33.2") ; http://code.google.com/p/ypsilon/issues/detail?id=33
(test-eval! (import (rnrs)))
(test-eval! (library (L)
              (export s)
              (import (rnrs))
              (define-syntax s
                (lambda (x)
                  (syntax-case x ()
                    ((_ i)
                     (free-identifier=? #'i #'not)
                     (syntax 'yes)))))))
(test-eval! (import (L)))
(test-equal (s not) yes)
(test-syntax-violation (let ((not 10)) (s not)))
(test-end)

(test-begin "Check issue 33.3") ; http://code.google.com/p/ypsilon/issues/detail?id=33
(test-eval! (import (rnrs)))
(test-eval! (library (L)
              (export x s)
              (import (rnrs))
              (define-syntax x (lambda (_) #f))
              (define-syntax s
                (syntax-rules (x)
                  [(_ x)
                   'yes]))))
(test-eval! (import (prefix (L) L:)))
(test-equal (L:s L:x) => yes)
(test-syntax-violation (L:s x))
(test-end)

(test-begin "Check issue 54") ; http://code.google.com/p/ypsilon/issues/detail?id=54
(test-eval! (library (sweet-macros)
              ;;; Version: 0.1
              ;;; Author: Michele Simionato
              ;;; Email: michele.simionato@gmail.com
              ;;; Date: 27-Oct-2008
              ;;; Licence: BSD
              (export syntax-match def-syntax syntax-expand)
              (import (rnrs))
              ;; helper macro
              (define-syntax guarded-syntax-case
                (let ((add-clause
                       (lambda (clause acc)
                         (syntax-case clause ()
                           ((pattern skeleton . rest)
                            (syntax-case #'rest ()
                              ((cond? else1 else2 ...)
                               (cons*
                                #'(pattern cond? skeleton)
                                #'(pattern (begin else1 else2 ...))
                                acc))
                              ((cond?)
                               (cons #'(pattern cond? skeleton) acc))
                              (()
                               (cons #'(pattern skeleton) acc))
                              ))))))
                  (lambda (x)
                    (syntax-case x ()
                      ((guarded-syntax-case y (literal ...) clause ...)
                       (with-syntax
                           (((c ...) (fold-right add-clause '() #'(clause ...))))
                         #'(syntax-case y (literal ...) c ...)))))))
              (define-syntax syntax-match
                (lambda (x)
                  (syntax-case x (sub)
                    ((_ (literal ...) (sub patt skel . rest) ...)
                     #'(lambda (x)
                         (syntax-match x (literal ...)
                                       (sub patt skel . rest) ...)))
                    ((_ x (literal ...) (sub patt skel . rest) ...)
                     (and (identifier? #'x) (for-all identifier? #'(literal ...)))
                     #'(guarded-syntax-case x
                                            (<literals> <patterns> <source> <transformer> literal ...)
                                            ((ctx <literals>)
                                             #''((... (... literal)) ...))
                                            ((ctx <patterns>)
                                             #''((... (... patt)) ...))
                                            ((ctx <source>)
                                             #''(syntax-match (literal ...)
                                                              (... (... (sub patt skel . rest))) ...))
                                            ((ctx <transformer>)
                                             #'(syntax-match (literal ...)
                                                             (... (... (sub patt skel . rest))) ...))
                                            (patt skel . rest) ...)))))
              (define-syntax def-syntax
                (syntax-match ()
                              (sub (def-syntax (name . args) skel . rest)
                                   #'(define-syntax name (syntax-match () (sub (name . args) skel . rest))))
                              (sub (def-syntax name transformer)
                                   #'(define-syntax name transformer))))
              (def-syntax (syntax-expand (macro . args))
                          #'(syntax->datum ((macro <transformer>) #'(macro . args))))))
(test-eval! (library (target)
              (export check)
              (import (rnrs) (sweet-macros))
              (def-syntax (hello) #'(display "hello world!"))
              (define (check) (syntax-expand (hello)))))
(test-eval! (import (target)))
(test-equal (check) => (display "hello world!"))
(test-end)

(test-begin "Check issue 67.1") ; http://code.google.com/p/ypsilon/issues/detail?id=67
(test-eval! (library (test define-values)
              (export define-values)
              (import (for (rnrs base) run expand)
                      (for (rnrs syntax-case) run expand))
              (define-syntax define-values
                (lambda (form)
                  (syntax-case form ()
                    ((_ (id ...) exp0 exp ...)
                     ;; Mutable-ids are needed so that ids defined by
                     ;; define-values can be exported from a library (mutated
                     ;; variables cannot be exported).  This fix is due to Andre
                     ;; van Tonder.
                     (with-syntax (((mutable-id ...) (generate-temporaries (syntax (id ...))))
                                   ((result ...)     (generate-temporaries (syntax (id ...)))))
                       (syntax
                        (begin
                          (define mutable-id) ...
                          (define dummy
                            (call-with-values
                              (lambda () exp0 exp ...)
                              (lambda (result ...)
                                (set! mutable-id result) ...)))
                          (define id mutable-id) ...)))))))))
(test-eval! (library (test foo)
              (export foo-alist mogrify)
              (import (rnrs))
              (define (foo-alist)
                '((a . 42) (b . 666)))
              (define (mogrify n x)
                (apply values (let loop ((i 1) (lst '()))
                                (if (> i n)
                                    lst
                                    (loop (+ i 1) (cons (/ x i) lst))))))))
(test-eval! (library (test bar)
              (export def-expander)
              (import (for (rnrs) run expand (meta -1))
                      (for (test define-values) run expand (meta -1))
                      (for (test foo) run expand (meta -1)))
              (define (def-expander alist)
                (lambda (stx)
                  (syntax-case stx ()
                    ((k (name ...) id)
                     (let ((val (cond ((assq (syntax->datum #'id) alist) => cdr)
                                      (else #f)))
                           (l (length (syntax->datum #'(name ...)))))
                       (with-syntax ((v val)
                                     (n l))
                         #'(define-values (name ...) (mogrify n 'v))))))))))

(test-eval! (library (target)
              (export check)
              (import (rnrs)
                      (for (test foo) expand)
                      (for (test bar) expand))
              (define-syntax def (def-expander (foo-alist)))
              (def (x y z) b)
              (define (check) (list x y z))))

(test-eval! (import (target)))
(test-equal (check) => (222 333 666))
(test-end)

(test-begin "Check issue 67.2") ; http://code.google.com/p/ypsilon/issues/detail?id=67
(test-eval! (library (test foo)
              (export foo-alist mogrify)
              (import (rnrs))
              (define (foo-alist)
                '((a . 42) (b . 666)))
              (define (mogrify n x)
                (apply values (let loop ((i 1) (lst '()))
                                (if (> i n)
                                    lst
                                    (loop (+ i 1) (cons (/ x i) lst))))))))
(test-eval! (library (test bar)
              (export def-expander)
              (import (for (rnrs) run expand (meta -1))
                      (for (test foo) run expand (meta -1)))
              (define-syntax define-values
                (lambda (form)
                  (syntax-case form ()
                    ((_ (id ...) exp0 exp ...)
                     ;; Mutable-ids are needed so that ids defined by
                     ;; define-values can be exported from a library (mutated
                     ;; variables cannot be exported).  This fix is due to Andre
                     ;; van Tonder.
                     (with-syntax (((mutable-id ...) (generate-temporaries (syntax (id ...))))
                                   ((result ...)     (generate-temporaries (syntax (id ...)))))
                       (syntax
                        (begin
                          (define mutable-id) ...
                          (define dummy
                            (call-with-values
                              (lambda () exp0 exp ...)
                              (lambda (result ...)
                                (set! mutable-id result) ...)))
                          (define id mutable-id) ...)))))))
              (define (def-expander alist)
                (lambda (stx)
                  (syntax-case stx ()
                    ((k (name ...) id)
                     (let ((val (cond ((assq (syntax->datum #'id) alist) => cdr)
                                      (else #f)))
                           (l (length (syntax->datum #'(name ...)))))
                       (with-syntax ((v val)
                                     (n l))
                         #'(define-values (name ...) (mogrify n 'v))))))))))

(test-eval! (library (target)
              (export check)
              (import (rnrs)
                      (for (test foo) expand)
                      (for (test bar) expand))
              (define-syntax def (def-expander (foo-alist)))
              (def (x y z) b)
              (define (check) (list x y z))))

(test-eval! (import (target)))
(test-equal (check) => (222 333 666))
(test-end)

(test-begin "apply")
(test-equal (let ((c '(1 2 3))) (eq? c (apply (lambda x x) c))) => #f)
(test-end)

(test-begin "bad utf8")
(test-i/o-error (lookahead-char
                 (open-bytevector-input-port
                  #vu8(#xfc #x80 #x80 #x80 #x80 #x80)
                  (make-transcoder (utf-8-codec) (eol-style none) (error-handling-mode raise)))))
(test-end)

(test-begin "record")
(test-eval! (define-syntax def-foo
              (syntax-rules ()
                ((_ make pred (field accessor setter) ...)
                 (define-record-type (foo make pred)
                   (fields (mutable field accessor setter) ...))))))
(test-eval! (def-foo make-foo foo? (x foo-x foo-x-set!)))
(test-eval! (define rec (make-foo 100)))
(test-equal (foo-x rec) => 100)
(test-end)


(test-begin "sweet-macros")
(test-eval! (library (sweet-macros)
              (export syntax-match def-syntax syntax-expand sub)
              (import (rnrs))
              (define-syntax sub ; needed to make Ikarus REPL happy
                (lambda (x)
                  (syntax-violation #f "incorrect use of auxiliary keyword" x)))
              (define-syntax guarded-syntax-case
                (let ((add-clause
                       (lambda (clause acc)
                         (syntax-case clause ()
                           ((pattern skeleton . rest)
                            (syntax-case #'rest ()
                              ((cond? else1 else2 ...)
                               (cons*
                                #'(pattern cond? skeleton)
                                #'(pattern (begin else1 else2 ...))
                                acc))
                              ((cond?)
                               (cons #'(pattern cond? skeleton) acc))
                              (()
                               (cons #'(pattern skeleton) acc))
                              ))))))
                  (lambda (x)
                    (syntax-case x ()
                      ((guarded-syntax-case y (literal ...) clause ...)
                       (with-syntax (((c ...) (fold-right add-clause '() #'(clause ...))))
                         #'(syntax-case y (literal ...) c ...)))))))
              (define-syntax syntax-match
                (lambda (y)
                  (guarded-syntax-case y (sub)
                                       ((self (literal ...) (sub patt skel rest ...) ...)
                                        #'(lambda (x) (self x (literal ...) (sub patt skel rest ...) ...)))
                                       ((self x (literal ...) (sub patt skel rest ...) ...)
                                        #'(guarded-syntax-case x (<literals> <patterns> literal ...)
                                                               ((ctx <literals>) #''(literal ...))
                                                               ((ctx <patterns>) #''((... (... patt)) ...))
                                                               (patt skel rest ...)
                                                               ...)
                                        (for-all identifier? #'(literal ...))
                                        (syntax-violation 'syntax-match "Found non identifier" #'(literal ...)
                                                          (remp identifier? #'(literal ...))))
                                       )))
              (define-syntax def-syntax
                (syntax-match (extends)
                              (sub (def-syntax name (extends parent) (literal ...) clause ...)
                                   #'(def-syntax name
                                                 (syntax-match (literal ...)
                                                               clause ...
                                                               (sub x ((parent <transformer>) #'x)))))
                              (sub (def-syntax (name . args) skel rest ...)
                                   #'(def-syntax name (syntax-match () (sub (name . args) skel rest ...))))
                              (sub (def-syntax name transformer)
                                   #'(define-syntax name
                                       (lambda (x)
                                         (syntax-case x (<source> <transformer>)
                                           ((name <transformer>) #'(... (... transformer)))
                                           ((name <source>) #''(... (... transformer)))
                                           (x (transformer #'x)))))
                                   (identifier? #'name)
                                   (syntax-violation 'def-syntax "Invalid name" #'name))))
              (def-syntax (syntax-expand (macro . args))
                          #'(syntax->datum ((macro <transformer>) #'(... (... (macro . args))))))))
(test-eval! (import (rnrs)
                    (for (sweet-macros) (meta 0) (meta 1))
                    (for (only (rnrs) begin lambda display) (meta 2))))
(test-eval! (def-syntax m
                        (let ()
                          (def-syntax m2
                                      (begin
                                        (display "at metalevel 2\n")
                                        (lambda (x) "expanded-m\n")))
                          (define _ (display "at metalevel 1\n"))
                          (lambda (x) (m2)))))

(test-equal (begin (display (m)) (m)) => "expanded-m\n")
(test-end)

(test-begin "patvar out of context")
(test-syntax-violation (define-syntax foo
                         (lambda (x)
                           (syntax-case x ()
                             ((_)
                              (let ()
                                (syntax-case '(1 2) ()
                                  ((a b)
                                   (let ()
                                     (define-syntax hoge
                                       (lambda (x)
                                         (syntax-case x ()
                                           ((_) #'(list a b)))))
                                     (hoge))))))))))
(test-end)

(test-begin "library inspection")
(test-eval! (library (library-inspector)
              (export define-here)
              (import (rnrs))
              (define-syntax define-here
                (lambda (x)
                  (syntax-case x ()
                    [(_ here) (identifier? #'here)
                     #'(define-syntax here
                         (lambda (stx)
                           (syntax-case stx ()
                             [(_ expr)
                              (datum->syntax #'here
                                             (syntax->datum #'expr))])))])))))
(test-eval! (library (foo)
              (export in-foo) ;;; a and b "private"
              (import (rnrs) (library-inspector))
              (define a 12)
              (define-syntax b
                (syntax-rules ()
                  [(_ x) (+ x a)]))
              (define-here in-foo)))

(test-eval! (import (rnrs) (foo)))
(test-equal (list (in-foo a) (in-foo (b 13))) => (12 25))
(test-end)
