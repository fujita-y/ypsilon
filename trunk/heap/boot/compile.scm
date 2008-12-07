;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2008 Y.FUJITA, LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(define ht-local-closures (make-parameter #f))

(define local-closure?
  (lambda (e)
    (cond ((and (current-closure-comments) (core-hashtable-ref (current-closure-comments) e #f))
           => (lambda (e) (memq (car e) '(stack))))
          (else #f))))

(define collect-local-closure
  (lambda (b)
    (for-each (lambda (e)
                (and (pair? (cadr e))
                     (eq? (caadr e) 'lambda)
                     (local-closure? (cadr e))
                     (core-hashtable-set! (ht-local-closures) (car e) #t)))
              b)))

(define make-application-comment
  (lambda (form)
    (if (backtrace)
        (cond ((current-source-comments)
               => (lambda (ht)
                    (cond ((core-hashtable-ref (current-source-comments) form #f)
                           => (lambda (datum)
                                (if (< datum 0)
                                    `(,form ,(core-hashtable-ref (current-source-comments) '.&SOURCE-PATH #f) . ,(- datum))
                                    `(,(core-hashtable-ref (current-source-comments) '.&SOURCE-PATH #f) . ,datum))))
                          (else
                           (list form)))))
              (else
               (list form)))
        '())))

(define make-closure-comment
  (lambda (form)
    (cond ((and (current-closure-comments)
                (core-hashtable-ref (current-closure-comments) form #f))
           => cdr)
          (else '()))))

(define top-level-subr
  (lambda (e)
    (and (symbol? e)
         (top-level-bound? e)
         (subr? (top-level-value e))
         (top-level-value e))))

(define top-level-value-or-false
  (lambda (e)
    (and (symbol? e)
         (top-level-bound? e)
         (top-level-value e))))

(define formals->list
  (lambda (lst)
    (if (pair? lst)
        (cons (car lst) (formals->list (cdr lst)))
        (cond ((null? lst) '())
              (else (list lst))))))

(define make-formals-operand
  (lambda (vars formals)
    (cond ((list? formals) (list (length formals) 0))
          ((pair? formals) (list (- (length vars) 1) 1))
          (else '(0 1)))))

(define make-iloc-operand
  (lambda (e cte)
    (let loop1 ((lst1 cte) (level 0))
      (and (pair? lst1)
           (if (memq e (car lst1))
               (cons level
                     (let loop2 ((lst2 (car lst1)) (index 0))
                       (if (eq? (car lst2) e)
                           index
                           (loop2 (cdr lst2) (+ index 1)))))
               (loop1 (cdr lst1) (+ level 1)))))))


(define iloc-iota
  (lambda (depth start-index count)
    (let loop ((count (- count 1)) (lst '()))
      (cond ((< count 0) lst)
            (else (loop (- count 1) (cons (cons depth (+ start-index count)) lst)))))))

(define cte-extend-iloc
  (lambda (vars cte)
    (cons vars cte)))

(define iloc?
  (lambda (e cte)
    (and (symbol? e)
         (let loop ((lst cte))
           (and (pair? lst)
                (or (memq e (car lst))
                    (loop (cdr lst))))))))

(define immediate-literal?
  (lambda (x)
    (or (fixnum? x) (char? x) (boolean? x) (null? x)
        (and (pair? x) (eq? (car x) 'quote) (symbol? (cadr x))))))

;; todo: record local defined symbol to reduce touch
(define compile-touch
  (lambda (e cte)
    (cond ((and (symbol? e)
                (not (top-level-bound? e))
                (not (iloc? e cte)))
           `((touch.gloc.of ,e)))
          (else '()))))

(define compile-lambda-helper
  (lambda (formals body comment cte)
    (let ((vars (formals->list formals)))
      (let ((cte (cte-extend-iloc vars cte)))
        `(,(append (make-formals-operand vars formals) comment)
           ,@(compile-expression-seq body cte #f #t))))))

(define ht-special-subr-expression
  (let ((ht (make-core-hashtable)))
    (for-each (lambda (x) (core-hashtable-set! ht x #t))
              (list unspecified car cdr cadr cddr cons = < <= > >= eq? null? pair? + - eqv? equal?))
    (core-hashtable-copy ht #t)))

(define alist-special-binary-subr
  (list (cons = '=n.iloc) (cons < '<n.iloc) (cons <= '<=n.iloc) (cons > '>n.iloc) (cons >= '>=n.iloc)))

(define alist-special-binary-subr-negate
  (list (cons = '=n.iloc) (cons < '>n.iloc) (cons <= '>=n.iloc) (cons > '<n.iloc) (cons >= '<=n.iloc)))

(define alist-special-binary-subr-iloc
  (list (cons = '=.iloc) (cons < '<.iloc) (cons <= '<=.iloc) (cons > '>.iloc) (cons >= '>=.iloc)))

(define alist-special-binary-subr-iloc-negate
  (list (cons = '=.iloc) (cons < '>.iloc) (cons <= '>=.iloc) (cons > '<.iloc) (cons >= '<=.iloc)))

(define compile-subr-expression
  (lambda (form cte discard tail)

    (define compile-anonymous
      (lambda (form cte tail comment)
        (if tail
            `(,@(compile-argument-each (cdr form) cte) (ret.subr.gloc.of ,(car form) . ,comment))
            `(,@(compile-argument-each (cdr form) cte) (subr.gloc.of ,(car form) ,(length (cdr form)) . ,comment)))))

    (let ((comment (make-application-comment form)))
      (let ((subr (top-level-value (car form))))
        (if (core-hashtable-contains? ht-special-subr-expression subr)
            (cond ((null? (cdr form))
                   (cond ((eq? subr unspecified)
                          (cond (discard '())
                                (tail (list (list 'ret.const.unspec)))
                                (else (list (list 'const.unspec)))))
                         (else (compile-anonymous form cte tail comment))))
                  ((null? (cddr form))
                   (if tail
                       (cond ((eq? subr pair?)
                              `(,@(compile-expression (cadr form) cte #f #f) ,(cons 'ret.pair? comment)))
                             ((eq? subr null?)
                              `(,@(compile-expression (cadr form) cte #f #f) ,(cons 'ret.null? comment)))
                             (else
                              (compile-anonymous form cte tail comment)))
                       (if (iloc? (cadr form) cte)
                           (cond (discard '())
                                 (else
                                  (cond ((eq? subr car)
                                         `((car.iloc ,(make-iloc-operand (cadr form) cte) . ,comment)))
                                        ((eq? subr cdr)
                                         `((cdr.iloc ,(make-iloc-operand (cadr form) cte) . ,comment)))
                                        ((eq? subr cadr)
                                         `((cadr.iloc ,(make-iloc-operand (cadr form) cte) . ,comment)))
                                        ((eq? subr cddr)
                                         `((cddr.iloc ,(make-iloc-operand (cadr form) cte) . ,comment)))
                                        (else
                                         (compile-anonymous form cte tail comment)))))
                           (compile-anonymous form cte tail comment))))
                  ((null? (cdddr form))
                   (if tail
                       (cond ((eq? subr cons)
                              `(,@(compile-argument (cadr form) cte) ,@(compile-expression (caddr form) cte #f #f) ,(cons 'ret.cons comment)))
                             ((eq? subr eq?)
                              `(,@(compile-argument (cadr form) cte) ,@(compile-expression (caddr form) cte #f #f) ,(cons 'ret.eq? comment)))
                             ((eq? subr eqv?)
                              (let ((opd1 (cadr form)) (opd2 (caddr form)))
                                (if (or (immediate-literal? opd1) (immediate-literal? opd2))
                                    `(,@(compile-argument (cadr form) cte) ,@(compile-expression (caddr form) cte #f #f) ,(cons 'ret.eq? comment))
                                    (compile-anonymous form cte tail comment))))
                             (else
                              (compile-anonymous form cte tail comment)))
                       (cond ((assq subr alist-special-binary-subr)
                              (let ((opd1 (cadr form)) (opd2 (caddr form)))
                                (let-values (((lhs rhs inst)
                                              (cond ((and (fixnum? opd1) (symbol? opd2) (iloc? opd2 cte)) (values opd2 opd1 (cdr (assq subr alist-special-binary-subr-negate))))
                                                    ((and (fixnum? opd2) (symbol? opd1) (iloc? opd1 cte)) (values opd1 opd2 (cdr (assq subr alist-special-binary-subr))))
                                                    (else (values #f #f #f)))))
                                  (cond (inst `((,inst ,(make-iloc-operand lhs cte) ,rhs . ,comment)))
                                        ((iloc? opd2 cte)
                                         `(,@(compile-expression opd1 cte #f #f) (,(cdr (assq subr alist-special-binary-subr-iloc)) ,(make-iloc-operand opd2 cte) . ,comment)))
                                        ((iloc? opd1 cte)
                                         `(,@(compile-expression opd2 cte #f #f) (,(cdr (assq subr alist-special-binary-subr-iloc-negate)) ,(make-iloc-operand opd1 cte) . ,comment)))
                                        (else (compile-anonymous form cte tail comment))))))
;;;
                             ((eq? subr +)
                              (let ((opd1 (cadr form)) (opd2 (caddr form)))
                                (let-values (((lhs rhs) (cond ((and (fixnum? opd1) (symbol? opd2) (iloc? opd2 cte)) (values opd2 opd1))
                                                              ((and (fixnum? opd2) (symbol? opd1) (iloc? opd1 cte)) (values opd1 opd2))
                                                              (else (values #f #f)))))
                                  (if lhs
                                      `((n+.iloc ,(make-iloc-operand lhs cte) ,rhs . ,comment))
                                      (compile-anonymous form cte tail comment)))))
                             ((eq? subr -)
                              (let ((opd1 (cadr form)) (opd2 (caddr form)))
                                (if (and (fixnum? opd2) (symbol? opd1) (iloc? opd1 cte))
                                    `((n+.iloc ,(make-iloc-operand opd1 cte) ,(- opd2) . ,comment))
                                    (compile-anonymous form cte tail comment))))

                             (else
                              (compile-anonymous form cte tail comment)))))
                  (else
                   (compile-anonymous form cte tail comment)))
            (compile-anonymous form cte tail comment))))))

(define ht-special-subr-argument
  (let ((ht (make-core-hashtable)))
    (for-each (lambda (x) (core-hashtable-set! ht x #t))
              (list unspecified car cdr cadr cddr + - cons))
    (core-hashtable-copy ht #t)))

(define compile-subr-argument
  (lambda (form cte)

    (define compile-anonymous
      (lambda (form cte comment)
        `(,@(compile-argument-each (cdr form) cte) (push.subr.gloc.of ,(car form) ,(length (cdr form)) . ,comment))))

    (let ((comment (make-application-comment form)))
      (let ((subr (top-level-value (car form))))
        (if (core-hashtable-contains? ht-special-subr-argument subr)
            (cond ((null? (cdr form))
                   (cond ((eq? subr unspecified) (list (list 'push.const.unspec)))
                         (else
                          `((subr.gloc.of ,(car form) 0) ,(list 'push)))))
                  ((null? (cddr form))
                   (if (iloc? (cadr form) cte)
                       (cond ((eq? subr car) `((push.car.iloc ,(make-iloc-operand (cadr form) cte) . ,comment)))
                             ((eq? subr cdr) `((push.cdr.iloc ,(make-iloc-operand (cadr form) cte) . ,comment)))
                             ((eq? subr cadr) `((push.cadr.iloc ,(make-iloc-operand (cadr form) cte) . ,comment)))
                             ((eq? subr cddr) `((push.cddr.iloc ,(make-iloc-operand (cadr form) cte) . ,comment)))
                             (else (compile-anonymous form cte comment)))
                       (compile-anonymous form cte comment)))
                  ((null? (cdddr form))
                   (cond ((eq? subr cons)
                          `(,@(compile-argument (cadr form) cte) ,@(compile-expression (caddr form) cte #f #f) ,(list 'push.cons)))
                         ((eq? subr +)
                          (let ((opd1 (cadr form)) (opd2 (caddr form)))
                            (let-values (((lhs rhs) (cond ((and (iloc? opd1 cte) (fixnum? opd2)) (values opd1 opd2))
                                                          ((and (iloc? opd2 cte) (fixnum? opd1)) (values opd2 opd1))
                                                          (else (values #f #f)))))
                              (if lhs
                                  `((push.n+.iloc ,(make-iloc-operand lhs cte) ,rhs . ,comment))
                                  (compile-anonymous form cte comment)))))
                         ((eq? subr -)
                          (let ((opd1 (cadr form)) (opd2 (caddr form)))
                            (if (and (iloc? opd1 cte) (fixnum? opd2))
                                `((push.n+.iloc ,(make-iloc-operand opd1 cte) ,(- opd2) . ,comment))
                                (compile-anonymous form cte comment))))
                         (else
                          (compile-anonymous form cte comment))))
                  (else
                   (compile-anonymous form cte comment)))
            (if (null? (cdr form))
                `((subr.gloc.of ,(car form) 0) ,(list 'push))
                (compile-anonymous form cte comment)))))))

(define compile-argument-each
  (lambda (form cte)
    (let loop ((lst form) (acc '()))
      (cond ((null? lst) (apply append (reverse acc)))
            (else
             (let ((e (compile-argument (car lst) cte)))
               (loop (cdr lst) (cons e acc))))))))

(define compile-argument
  (lambda (form cte)
    (cond ((pair? form)
           (cond ((top-level-subr (car form))
                  (compile-subr-argument form cte))
                 ((eq? (car form) 'lambda)
                  (let ((code (compile-lambda-helper (cadr form) (cddr form) (make-closure-comment form) cte)))
                    (if (local-closure? form)
                        (list (cons 'push.close+ code))
                        (list (cons 'push.close code)))))
                 ((eq? (car form) 'quote)
                  `((push.const . ,(cadr form))))
                 (else
                  `(,@(compile-expression form cte #f #f) ,(list 'push)))))
          ((symbol? form)
           (cond ((eq? form '.&UNDEF) (list (list 'push.const.undef)))
                 (else
                  (if (iloc? form cte)
                      (let ((opd (make-iloc-operand form cte)))
                        (case (car opd)
                          ((0) `((push.iloc.0 . ,(cdr opd))))
                          ((1) `((push.iloc.1 . ,(cdr opd))))
                          (else `((push.iloc ,@opd)))))
                      `((push.gloc.of ,form))))))
          (else
           `((push.const . ,form))))))

(define compile-call
  (lambda (proc argc comment cte)
    (cond ((pair? proc)
           `(,@(compile-expression proc cte #f #f) ,(list 'apply)))
          ((symbol? proc)
           (cond ((iloc? proc cte)
                  (if (core-hashtable-contains? (ht-local-closures) proc)
                      `((apply.iloc+ ,(make-iloc-operand proc cte) . ,comment))
                      `((apply.iloc ,(make-iloc-operand proc cte) . ,comment))))
                 ((top-level-subr proc)
                  `((ret.subr.gloc.of ,proc . ,comment)))
                 (else
                  `((apply.gloc.of ,proc . ,comment)))))
          (else
           `((const . ,proc) (apply ,@comment)))))) ; syntax violation

(define compile-expression-begin
  (lambda (form cte discard tail)
    (let ((body (cdr form)))
      (cond ((null? body)
             (if tail (list (list 'ret.const.unspec)) '()))
            ((null? (cdr body))
             (compile-expression (car body) cte discard tail))
            (else
             (compile-expression-seq body cte discard tail))))))

(define compile-expression-quote
  (lambda (form cte discard tail)
    (let ((datum (cadr form)))
      (cond (discard '())
            (tail `((ret.const . ,datum)))
            (else `((const . ,datum)))))))

(define compile-expression-define
  (lambda (form cte discard tail)
    (let ((body (compile-expression (caddr form) cte #f #f))
          (retval (if tail (list (list 'ret.const.unspec)) '())))
      `(,@body (set.gloc.of ,(cadr form)) ,@retval))))

(define compile-expression-set!
  (lambda (form cte discard tail)
    (let ((body (compile-expression (caddr form) cte #f #f))
          (retval (if tail (list (list 'ret.const.unspec)) '())))
      (cond ((iloc? (cadr form) cte)
             `(,@body (set.iloc ,(make-iloc-operand (cadr form) cte) . ,(make-application-comment form)) ,@retval))
            (else
             (let ((touch (if (backtrace) (compile-touch (cadr form) cte) '())))
               `(,@body ,@touch (set.gloc.of ,(cadr form) . ,(make-application-comment form)) ,@retval)))))))

(define compile-expression-lambda
  (lambda (form cte discard tail)
    (let ((code (compile-lambda-helper (cadr form) (cddr form) (make-closure-comment form) cte)))
      (cond (discard '())
            (tail `((ret.close ,@code)))
            (else `((close ,@code)))))))

(define compile-expression-let
  (lambda (form cte discard tail)
    (cond ((null? (cadr form))
           (compile-expression-seq (cddr form) cte discard tail))
          (else
           (collect-local-closure (cadr form))
           (let ((vars (map car (cadr form))))
             (let ((code `(,@(compile-argument-each (map cadr (cadr form)) cte)
                             (extend . ,(length vars))
                             ,@(compile-expression-seq (cddr form) (cte-extend-iloc vars cte) #f #t))))
               (if tail code (list (cons 'call code)))))))))

(define compile-expression-letrec*
  (lambda (form cte discard tail)
    (cond ((null? (cadr form))
           (compile-expression-seq (cddr form) cte discard tail))
          ((null? (cdadr form))
           (let ((binding (caadr form)))
             (let ((cte (cte-extend-iloc (list (car binding)) cte)))
               (cond ((and (pair? (cadr binding)) (eq? (caadr binding) 'lambda))
                      (collect-local-closure (cadr form))
                      (let ((init (cadr binding)))
                        (let ((init-code (compile-lambda-helper (cadr init) (cddr init) (make-closure-comment init) cte))
                              (body-code (compile-expression-seq (cddr form) cte #f #t))
                              (inst (if (local-closure? init) 'extend.enclose+ 'extend.enclose)))
                          (let ((code `((,inst ,@init-code) ,@body-code)))
                            (if tail code (list (cons 'call code)))))))
                     (else
                      (let ((code `(,(cons 'extend.unbound 1)
                                     ,@(compile-argument (cadr binding) cte)
                                     ,(cons 'enclose 1)
                                     ,@(compile-expression-seq (cddr form) cte #f #t))))
                        (if tail code (list (cons 'call code)))))))))
          (else
           (let ((bindings (cadr form)))
             (collect-local-closure bindings)
             (let-values (((front back)
                           (let loop ((lst bindings) (front '()))
                             (if (null? lst)
                                 (values bindings '())
                                 (let ((init (cadar lst)))
                                   (if (if (pair? init) (memq (car init) '(quote lambda)) (not (symbol? init)))
                                       (loop (cdr lst) (cons (car lst) front))
                                       (values (reverse front) lst)))))))
               (let ((cte (cte-extend-iloc (map car bindings) cte)))
                 (let ((front-code (compile-argument-each (map cadr front) cte))
                       (back-code (apply append (map (lambda (iloc expr) `(,@expr (set.iloc ,iloc)))
                                                     (iloc-iota 0 (length front) (length back))
                                                     (map (lambda (e) (compile-expression (cadr e) cte #f #f)) back))))
                       (body-code (compile-expression-seq (cddr form) cte #f #t)))
                   (let ((code (if (null? front-code)
                                   `((extend.unbound . ,(length bindings)) ,@back-code ,@body-code)
                                   `((extend.unbound . ,(length bindings)) ,@front-code (enclose . ,(length front-code)) ,@back-code ,@body-code))))
                     (if tail code (list (cons 'call code))))))))))))


(define compile-expression-if
  (lambda (form cte discard tail)

    (define compile-anonymous
      (lambda (form cte discard tail)
        (let ((cond-code (compile-expression (cadr form) cte #f #f)))
          (if (and (null? (cdr cond-code)) (eq? (caar cond-code) 'const))
              (if (cdar cond-code)
                  (compile-expression (caddr form) cte discard tail)
                  (compile-expression (cadddr form) cte discard tail))
              (let ((false-code (compile-expression (cadddr form) cte #f #t))
                    (true-code (compile-expression (caddr form) cte #f #t)))
                (if (and (symbol? (cadr form)) (eq? (cadr form) (caddr form)))
                    (cond (tail `(,@cond-code ,(list 'if.true.ret) ,@false-code))
                          (else `((call ,@cond-code (if.true ,@true-code) ,@false-code))))
                    (cond (tail
                           (if (and (null? (cdr true-code)) (eq? (caar true-code) 'ret.const))
                               `(,@cond-code (if.true.ret.const . ,(cdar true-code)) ,@false-code)
                               `(,@cond-code (if.true ,@true-code) ,@false-code)))
                          (else
                           `((call ,@cond-code (if.true ,@true-code) ,@false-code))))))))))

    (define compile-unary-special
      (lambda (form cte tail inst inst-ret-const)
        (let ((arg (cadadr form))
              (true (caddr form)))
          (let ((arg-code (compile-expression arg cte #f #f))
                (true-code (compile-expression true cte #f #t))
                (false-code (compile-expression (cadddr form) cte #f #t)))
            (cond ((and (eq? inst-ret-const 'if.null?.ret.const) (symbol? arg) (eq? arg true))
                   (cond (tail `(,@arg-code (,inst-ret-const) ,@false-code))
                         (else `((call ,@arg-code (,inst-ret-const) ,@false-code)))))
                  (else
                   (if (and (null? (cdr true-code)) (memq (caar true-code) '(const ret.const)))
                       (cond (tail `(,@arg-code (,inst-ret-const . ,(cdar true-code)) ,@false-code))
                             (else `((call ,@arg-code (,inst ,@true-code) ,@false-code))))
                       (cond (tail `(,@arg-code (,inst ,@true-code) ,@false-code))
                             (else `((call ,@arg-code (,inst ,@true-code) ,@false-code)))))))))))

    (define compile-binary-special
      (lambda (form cte tail inst inst-ret-const)
        (let ((args (cdadr form))
              (true (caddr form)))
          (let ((arg-code (append (compile-argument (car args) cte)
                                  (compile-expression (cadr args) cte #f #f)))
                (true-code (compile-expression true cte #f #t))
                (false-code (compile-expression (cadddr form) cte #f #t)))
            (if (and (null? (cdr true-code)) (memq (caar true-code) '(const ret.const)))
                (cond (tail `(,@arg-code (,inst-ret-const . ,(cdar true-code)) ,@false-code))
                      (else `((call ,@arg-code (,inst ,@true-code) ,@false-code))))
                (cond (tail `(,@arg-code (,inst ,@true-code) ,@false-code))
                      (else `((call ,@arg-code (,inst ,@true-code) ,@false-code)))))))))

    #;(and discard tail (assertion-violation 'compile-expression-if "invalid flag combination" (list form cte discard tail)))

    (cond ((null? (cdddr form))
           (if discard
               (compile-expression `(and ,@(cdr form)) cte discard tail)
               (compile-expression `(if ,@(cdr form) (.unspecified)) cte discard tail)))
          (else
           (cond ((and (pair? (cadr form)) (top-level-subr (caadr form)))
                  => (lambda (subr)
                       (case (length (cadr form))
                         ((2)
                          (cond ((eq? subr null?)
                                 (compile-unary-special form cte tail 'if.null? 'if.null?.ret.const))
                                ((eq? subr pair?)
                                 (compile-unary-special form cte tail 'if.pair? 'if.pair?.ret.const))
                                ((eq? subr symbol?)
                                 (compile-unary-special form cte tail 'if.symbol? 'if.symbol?.ret.const))
                                (else
                                 (compile-anonymous form cte discard tail))))
                         ((3)
                          (cond ((eq? subr eq?)
                                 (compile-binary-special form cte tail 'if.eq? 'if.eq?.ret.const))
                                ((or (eq? subr eqv?) (eq? subr equal?))
                                 (let ((opd1 (cadr (cadr form))) (opd2 (caddr (cadr form))))
                                   (if (or (immediate-literal? opd1) (immediate-literal? opd2))
                                       (compile-binary-special form cte tail 'if.eq? 'if.eq?.ret.const)
                                       (compile-anonymous form cte discard tail))))
                                (else
                                 (compile-anonymous form cte discard tail))))
                         (else
                          (compile-anonymous form cte discard tail)))))
                 (else
                  (compile-anonymous form cte discard tail)))))))

(define compile-expression-or
  (lambda (form cte discard tail)

    (define compile-clause
      (lambda (lst cte)

        (define compile-anonymous
          (lambda (lst cte)
            `(,@(compile-expression (car lst) cte #f #f) ,(list 'if.true.ret) ,@(compile-clause (cdr lst) cte))))

        (define compile-anonymous-negate
          (lambda (lst cte)
            `(,@(compile-expression (car lst) cte #f #f) ,(cons 'if.false.ret.const #t) ,@(compile-clause (cdr lst) cte))))

        (define compile-unary-special
          (lambda (lst cte inst)
            `(,@(compile-expression (cadar lst) cte #f #f) ,(cons inst #t) ,@(compile-clause (cdr lst) cte))))

        (define compile-binary-special
          (lambda (lst cte inst)
            `(,@(compile-argument (cadar lst) cte) ,@(compile-expression (caddar lst) cte #f #f) ,(cons inst #t) ,@(compile-clause (cdr lst) cte))))

        (if (null? (cdr lst))
            (compile-expression (car lst) cte #f #t)
            (cond ((and (pair? (car lst)) (top-level-subr (caar lst)))
                   => (lambda (subr)
                        (if (eq? subr not)
                            (let ((lst (cons (cadar lst) (cdr lst))))
                              (cond ((and (pair? (car lst)) (top-level-subr (caar lst)))
                                     => (lambda (subr)
                                          (case (length (car lst))
                                            ((2)
                                             (cond ((eq? subr null?)
                                                    (compile-unary-special lst cte 'if.not.null?.ret.const))
                                                   ((eq? subr pair?)
                                                    (compile-unary-special lst cte 'if.not.pair?.ret.const))
                                                   ((eq? subr symbol?)
                                                    (compile-unary-special lst cte 'if.not.symbol?.ret.const))
                                                   (else
                                                    (compile-anonymous-negate lst cte))))
                                            ((3)
                                             (cond ((eq? subr eq?)
                                                    (compile-binary-special lst cte 'if.not.eq?.ret.const))
                                                   ((or (eq? subr eqv?) (eq? subr equal?))
                                                    (let ((opd1 (cadr (car lst))) (opd2 (caddr (car lst))))
                                                      (if (or (immediate-literal? opd1) (immediate-literal? opd2))
                                                          (compile-binary-special lst cte 'if.not.eq?.ret.const)
                                                          (compile-anonymous-negate lst cte))))
                                                   (else
                                                    (compile-anonymous-negate lst cte))))
                                            (else
                                             (compile-anonymous-negate lst cte)))))
                                    (else
                                     (compile-anonymous-negate lst cte))))
                            (case (length (car lst))
                              ((2)
                               (cond ((eq? subr null?)
                                      (compile-unary-special lst cte 'if.null?.ret.const))
                                     ((eq? subr pair?)
                                      (compile-unary-special lst cte 'if.pair?.ret.const))
                                     ((eq? subr symbol?)
                                      (compile-unary-special lst cte 'if.symbol?.ret.const))
                                     (else
                                      (compile-anonymous lst cte))))
                              ((3)
                               (cond ((eq? subr eq?)
                                      (compile-binary-special lst cte 'if.eq?.ret.const))
                                     ((or (eq? subr eqv?) (eq? subr equal?))
                                      (let ((opd1 (cadr (car lst))) (opd2 (caddr (car lst))))
                                        (if (or (immediate-literal? opd1) (immediate-literal? opd2))
                                            (compile-binary-special lst cte 'if.eq?.ret.const)
                                            (compile-anonymous lst cte))))
                                     (else
                                      (compile-anonymous lst cte))))
                              (else
                               (compile-anonymous lst cte))))))
                  (else
                   (compile-anonymous lst cte))))))

    (cond ((null? (cdr form))
           (cond (discard '())
                 (tail (list (cons 'ret.const #f)))
                 (else (list (cons 'const #f)))))
          ((null? (cddr form))
           (compile-expression (cadr form) cte discard tail))
          ((and (not tail)
                (null? (cdddr form))
                (pair? (caddr form))
                (eq? (top-level-value-or-false (caaddr form)) assertion-violation))
           `(,@(compile-expression (cadr form) cte #f #f) (if.false.call ,@(compile-expression (caddr form) cte #f #t))))
          (else
           (let ((code (compile-clause (cdr form) cte)))
             (if tail code (list (cons 'call code))))))))

(define compile-expression-and
  (lambda (form cte discard tail)

    (define compile-clause
      (lambda (lst cte)

        (define compile-anonymous
          (lambda (lst cte)
            `(,@(compile-expression (car lst) cte #f #f) ,(list 'if.false.ret) ,@(compile-clause (cdr lst) cte))))

        (define compile-anonymous-negate
          (lambda (lst cte)
            `(,@(compile-expression (car lst) cte #f #f) ,(cons 'if.true.ret.const #f) ,@(compile-clause (cdr lst) cte))))

        (define compile-unary-special
          (lambda (lst cte inst)
            `(,@(compile-expression (cadar lst) cte #f #f) ,(cons inst #f) ,@(compile-clause (cdr lst) cte))))

        (define compile-binary-special
          (lambda (lst cte inst)
            `(,@(compile-argument (cadar lst) cte) ,@(compile-expression (caddar lst) cte #f #f) ,(cons inst #f) ,@(compile-clause (cdr lst) cte))))

        (if (null? (cdr lst))
            (compile-expression (car lst) cte #f #t)
            (cond ((and (pair? (car lst)) (top-level-subr (caar lst)))
                   => (lambda (subr)
                        (if (eq? subr not)
                            (let ((lst (cons (cadar lst) (cdr lst))))
                              (cond ((and (pair? (car lst)) (top-level-subr (caar lst)))
                                     => (lambda (subr)
                                          (case (length (car lst))
                                            ((2)
                                             (cond ((eq? subr null?)
                                                    (compile-unary-special lst cte 'if.null?.ret.const))
                                                   ((eq? subr pair?)
                                                    (compile-unary-special lst cte 'if.pair?.ret.const))
                                                   ((eq? subr symbol?)
                                                    (compile-unary-special lst cte 'if.symbol?.ret.const))
                                                   (else
                                                    (compile-anonymous-negate lst cte))))
                                            ((3)
                                             (cond ((eq? subr eq?)
                                                    (compile-binary-special lst cte 'if.eq?.ret.const))
                                                   ((or (eq? subr eqv?) (eq? subr equal?))
                                                    (let ((opd1 (cadr (car lst))) (opd2 (caddr (car lst))))
                                                      (if (or (immediate-literal? opd1) (immediate-literal? opd2))
                                                          (compile-binary-special lst cte 'if.eq?.ret.const)
                                                          (compile-anonymous-negate lst cte))))
                                                   (else
                                                    (compile-anonymous-negate lst cte))))
                                            (else
                                             (compile-anonymous-negate lst cte)))))
                                    (else
                                     (compile-anonymous-negate lst cte))))
                            (case (length (car lst))
                              ((2)
                               (cond ((eq? subr null?)
                                      (compile-unary-special lst cte 'if.not.null?.ret.const))
                                     ((eq? subr pair?)
                                      (compile-unary-special lst cte 'if.not.pair?.ret.const))
                                     ((eq? subr symbol?)
                                      (compile-unary-special lst cte 'if.not.symbol?.ret.const))
                                     (else
                                      (compile-anonymous lst cte))))
                              ((3)
                               (cond ((eq? subr eq?)
                                      (compile-binary-special lst cte 'if.not.eq?.ret.const))
                                     ((or (eq? subr eqv?) (eq? subr equal?))
                                      (let ((opd1 (cadr (car lst))) (opd2 (caddr (car lst))))
                                        (if (or (immediate-literal? opd1) (immediate-literal? opd2))
                                            (compile-binary-special lst cte 'if.not.eq?.ret.const)
                                            (compile-anonymous lst cte))))
                                     (else
                                      (compile-anonymous lst cte))))
                              (else
                               (compile-anonymous lst cte))))))
                  (else
                   (compile-anonymous lst cte))))))

    (cond ((null? (cdr form))
           (cond (discard '())
                 (tail (list (cons 'ret.const #t)))
                 (else (list (cons 'const #t)))))
          ((null? (cddr form))
           (compile-expression (cadr form) cte discard tail))
          (else
           (let ((code (compile-clause (cdr form) cte)))
             (if tail code (list (cons 'call code))))))))

(define ht-dispatch-expression
  (let ((ht (make-core-hashtable)))
    (core-hashtable-set! ht 'begin compile-expression-begin)
    (core-hashtable-set! ht 'quote compile-expression-quote)
    (core-hashtable-set! ht 'define compile-expression-define)
    (core-hashtable-set! ht 'set! compile-expression-set!)
    (core-hashtable-set! ht 'lambda compile-expression-lambda)
    (core-hashtable-set! ht 'let compile-expression-let)
    (core-hashtable-set! ht 'letrec* compile-expression-letrec*)
    (core-hashtable-set! ht 'if compile-expression-if)
    (core-hashtable-set! ht 'or compile-expression-or)
    (core-hashtable-set! ht 'and compile-expression-and)
    (core-hashtable-copy ht #t)))

(define compile-expression-seq
  (lambda (form cte discard tail)

    (define reverse-append
      (lambda (lst)
        (let loop ((lst lst) (ans '()))
          (cond ((null? lst) ans)
                (else
                 (loop (cdr lst) (append (car lst) ans)))))))

    #;(and discard tail (assertion-violation 'compile-expression-seq "invalid flag combination" (list form cte discard tail)))

    (let loop ((lst form) (acc '()))
      (cond ((null? lst) (reverse-append acc))
            (else
             (let ((e (compile-expression
                       (car lst)
                       cte
                       (or discard (pair? (cdr lst)))
                       (and tail (null? (cdr lst))))))
               (loop (cdr lst) (cons e acc))))))))

(define compile-expression
  (lambda (form cte discard tail)

    #;(and discard tail (assertion-violation 'compile-expression "invalid flag combination" (list form cte discard tail)))

    (cond ((pair? form)
           (cond ((core-hashtable-ref ht-dispatch-expression (car form) #f)
                  => (lambda (proc) (proc form cte discard tail)))
                 ((top-level-subr (car form))
                  (compile-subr-expression form cte discard tail))
                 (else
                  (let ((call (compile-call (car form) (length (cdr form)) (make-application-comment form) cte))
                        (args (compile-argument-each (cdr form) cte))
                        (touch (if (backtrace) (compile-touch (car form) cte) '())))
                    (let ((code `(,@touch ,@args ,@call)))
                      (if tail code (list (cons 'call code))))))))
          (else
           (if tail
               (cond ((eq? form '.&UNDEF)
                      (list (list 'ret.const.undef)))
                     ((unspecified? form)
                      (list (list 'ret.const.unspec)))
                     ((symbol? form)
                      (if (iloc? form cte)
                          `((ret.iloc ,@(make-iloc-operand form cte)))
                          `((ret.gloc.of ,form))))
                     (else
                      `((ret.const . ,form))))
               (cond (discard '())
                     ((eq? form '.&UNDEF)
                      (list (list 'const.undef)))
                     ((unspecified? form)
                      (list (list 'const.unspec)))
                     ((symbol? form)
                      (if (iloc? form cte)
                          (let ((opd (make-iloc-operand form cte)))
                            (case (car opd)
                              ((0) `((iloc.0 . ,(cdr opd))))
                              ((1) `((iloc.1 . ,(cdr opd))))
                              (else `((iloc ,@opd)))))
                          `((gloc.of ,form))))
                     (else
                      `((const . ,form)))))))))

(define compile-coreform
  (lambda (form)
    (parameterize ((ht-local-closures (make-core-hashtable)))
      (compile-expression form '() #f #t))))
