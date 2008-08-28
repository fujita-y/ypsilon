;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2008 Y.FUJITA, LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(define annotate-bindings
  (lambda (vars inits env)
    (for-each (lambda (var init)
                (and (pair? init)
                     (denote-lambda? env (car init))
                     (set-closure-comment! init (original-id var))))
              vars inits)))

(define warning-contract-violation
  (lambda (form inits lst)
    (for-each
     (lambda (var)
       (display-warning
        (format "warning: binding construct may attempt to reference uninitialized variable ~u" var)
        form
        (any1 (lambda (e) (and (check-rec-contract-violation (list var) e) e)) inits)))
     lst)))

(define expand-let
  (lambda (form env)
    (destructuring-match form
      ((_ (? symbol? name) (? list? bindings) body ...)
       (begin
         (check-let-bindings form bindings)
         (expand-form
          (annotate
           `(.LET ,bindings (.LETREC* ((,name (.LAMBDA ,(map car bindings) ,@body))) (,name ,@(map car bindings))))
           form)
          env)))
      ((_ () body ...)
       (let ((body (expand-body form body env)))
         (if (null? body)
             (syntax-violation (car form) "empty body" form)
             (annotate `(let () ,@body) form))))
      ((_ bindings body ...)
       (begin
         (check-let-bindings form bindings)
         (let ((vars (map car bindings))
               (inits (map (lambda (a) (expand-form (cadr a) env)) bindings))
               (suffix (fresh-rename-count)))
           (let* ((renames (map (lambda (id) (rename-id id suffix)) vars))
                  (body (expand-body form (cddr form) (extend-env (map cons vars renames) env))))
             (cond ((null? body)
                    (syntax-violation (car form) "empty body" form))
                   (else
                    (annotate-bindings vars inits env)
                    (annotate `(let ,(map list renames inits) ,@body) form)))))))
      (_
       (syntax-violation (car form) "expected bindings and body" form)))))

(define expand-letrec*
  (lambda (form env)
    (destructuring-match form
      ((_ () body ...)
       (let ((body (expand-body form body env)))
         (if (null? body)
             (syntax-violation (car form) "empty body" form)
             (annotate `(let () ,@body) form))))
      ((_ bindings body ...)
       (begin
         (check-let-bindings form bindings)
         (let ((vars (map car bindings)) (suffix (fresh-rename-count)))
           (let* ((renames (map (lambda (id) (rename-id id suffix)) vars))
                  (env (extend-env (map cons vars renames) env)))
             (let ((inits (map (lambda (a) (expand-form (cadr a) env)) bindings))
                   (body (expand-body form (cddr form) env)))
               (annotate-bindings vars inits env)
               (cond ((null? body) (syntax-violation (car form) "empty body" form))
                     ((check-rec*-contract-violation renames inits)
                      => (lambda (lst)
                           (and (warning-level) (warning-contract-violation form inits lst))
                           (annotate `(letrec* ,(map list renames inits) ,@body) form)))
                     (else
                      (annotate `(letrec* ,(rewrite-letrec*-bindings (map list renames inits) env) ,@body) form)))))))))))

(define expand-letrec
  (lambda (form env)
    (destructuring-match form
      ((_ () body ...)
       (expand-form (annotate `(.LET () ,@body) form) env))
      ((_ ((var init)) body ...)
       (expand-form (annotate `(.LETREC* ((,var ,init)) ,@body) form) env))
      ((_ bindings body ...)
       (begin
         (check-let-bindings form bindings)
         (let ((vars (map car bindings)) (suffix (fresh-rename-count)))
           (let* ((renames (map (lambda (id) (rename-id id suffix)) vars))
                  (env (extend-env (map cons vars renames) env)))
             (let ((inits (map (lambda (a) (expand-form (cadr a) env)) bindings))
                   (body (expand-body form (cddr form) env)))
               (annotate-bindings vars inits env)
               (cond ((null? body) (syntax-violation (car form) "empty body" form))
                     ((check-rec-contract-violation renames inits)
                      => (lambda (lst)
                           (and (warning-level) (warning-contract-violation form inits lst)))))
               (if (every1 (lambda (e) (if (pair? e) (denote-lambda? env (car e)) (not (symbol? e)))) inits)
                   (annotate `(letrec* ,(rewrite-letrec*-bindings (map list renames inits) env) ,@body) form)
                   (let ((temps (map (lambda (_) (rename-id (generate-temporary-symbol) suffix)) bindings)))
                     `(let ,(map (lambda (e) (list e '.&UNDEF)) renames)
                        (let ,(map list temps inits)
                          ,@(map (lambda (lhs rhs) `(set! ,lhs ,rhs)) renames temps) ,@body))))))))))))

(define expand-let*
  (lambda (form env)
    (if (> (safe-length form) 2)
        (let ((bindings (cadr form)))
          (check-let*-bindings form bindings)
          (expand-form
           (annotate
            (let loop ((lst bindings))
              (if (null? lst)
                  `(.LET () ,@(cddr form))
                  `(.LET (,(car lst)) ,(loop (cdr lst)))))
            form)
           env))
        (syntax-violation (car form) "expected bindings and body" form))))

(define expand-let-values
  (lambda (form env)
    (destructuring-match form
      ((_ bindings body ...)
       (begin
         (or (list? bindings)
             (syntax-violation (car form) "malformed bindings" form))
         (and (null? body)
              (syntax-violation (car form) "expected bindings and body" form))
         (or (unique-id-list?
              (apply append
                     (map (lambda (binding)
                            (destructuring-match binding
                              ((formals init) (collect-lambda-formals formals form))
                              (_ (syntax-violation (car form) "expected each binding consist of formals and expression" form))))
                          bindings)))
             (syntax-violation (car form) "duplicate formals" form))
         (let ((init-env env))
           (annotate
            (let loop ((lst bindings) (env env))
              (cond ((null? lst)
                     (expand-form `(.LET () ,@body) env))
                    (else
                     (destructuring-match (car lst)
                       ((formals init)
                        (let ((vars (collect-lambda-formals formals form)))
                          (let* ((suffix (fresh-rename-count))
                                 (renames (map cons
                                               vars
                                               (map (lambda (id) (rename-id id suffix))
                                                    vars))))    
                            `(.call-with-values
                              (lambda () ,(expand-form init init-env))
                              (lambda ,(rename-lambda-formals formals renames)
                                ,(loop (cdr lst) (extend-env renames env)))))))
                       (_ (scheme-error "internal error: let-values: ~m" form))))))

            form))))
      (_
       (syntax-violation (car form) "expected bindings and body" form)))))

(define expand-do
  (lambda (form env)
    (destructuring-match form
      ((_ clauses (test expr ...) body ...)
       (begin
         (or (list? clauses)
             (syntax-violation (car form) "malformed (variable init update)" form clauses))
         (let ((temp (generate-temporary-symbol))
               (bindings (map (lambda (clause)
                                (destructuring-match clause
                                  (((? symbol? var) init . _) (list var init))
                                  (_ (syntax-violation (car form) "malformed (variable init update)" form clause))))
                              clauses))
               (updates (map (lambda (clause)
                               (destructuring-match clause
                                 ((var _) var)
                                 ((_ _ update) update)
                                 (_ (syntax-violation (car form) "malformed (variable init update)" form clause))))
                             clauses)))
           (expand-form
            (annotate
             `(.LET ,temp ,bindings
                    (.IF ,test
                         ,(if (null? expr)
                              '(.UNSPECIFIED)
                              `(.BEGIN ,@expr))
                         (.BEGIN ,@body (,temp ,@updates))))
             form)
            env))))
      (_
       (syntax-violation (car form) "expected (variable init update), test, and command" form)))))

(define expand-let*-values
  (lambda (form env)
    (destructuring-match form
      ((_ bindings body ...)
       (begin
         (or (list? bindings)
             (syntax-violation (car form) "malformed bindings" form))
         (and (null? body)
              (syntax-violation (car form) "missing body" form))
         (expand-form
          (annotate
           (let loop ((lst bindings))
             (cond ((null? lst) `(.LET () ,@body))
                   (else
                    (destructuring-match (car lst)
                      ((formals init)
                       `(.CALL-WITH-VALUES
                         (.LAMBDA () ,init)
                         (.LAMBDA ,formals
                           ,(loop (cdr lst)))))
                      (_
                       (syntax-violation (car form) "expected each binding consist of formals and expression" form (car lst)))))))
           form)
          env)))
      (_
       (syntax-violation (car form) "expected bindings and body" form)))))

(define expand-cond
  (lambda (form env)

    (define else? (lambda (id) (denote-else? env id)))
    (define =>? (lambda (id) (denote-=>? env id)))
    (define lambda? (lambda (id) (denote-lambda? env id)))

    (cond ((> (safe-length form) 1)
           (expand-form
            (annotate
             (let loop ((lst (cdr form)))
               (if (null? lst)
                   '(.UNSPECIFIED)
                   (let ((clause (car lst)))
                     (destructuring-match clause
                       (((? else? _) . (? pair? expr))
                        (if (null? (cdr lst))
                            `(.BEGIN ,@expr)
                            (syntax-violation (car form) "misplaced else" form clause)))
                       ((test (? =>? _) ((? lambda? _) (a) expr ...))
                        (let ((temp (generate-temporary-symbol)))
                          `(.LET ((,temp ,test))
                                 (.IF ,temp
                                      (.LET ((,a ,temp)) ,@expr)
                                      ,(loop (cdr lst))))))
                       ((test (? =>? _) result)
                        (let ((temp (generate-temporary-symbol)))
                          `(.LET ((,temp ,test))
                                 (.IF ,temp
                                      (,result ,temp)
                                      ,(loop (cdr lst))))))
                       ((test)
                        `(.OR ,test ,(loop (cdr lst))))
                       ((test expr ...)
                        `(.IF ,test
                              (.BEGIN ,@expr)
                              ,(loop (cdr lst))))
                       (_
                        (syntax-violation (car form) "malformed cond clause" form clause))))))
             form)
            env))
          (else
           (syntax-violation (car form) "expected cond clause" form)))))

(define expand-case
  (lambda (form env)

    (define else? (lambda (id) (denote-else? env id)))

    (define maplist
      (lambda (func lst)
        (cond ((null? lst) '())
              (else (cons (func lst) (maplist func (cdr lst)))))))

    (destructuring-match form
      ((_ key . (? pair? clauses))
       (let ((temp (generate-temporary-symbol)))
         (expand-form
          (annotate
           `(.LET ((,temp ,key))
                  (.COND ,@(maplist
                            (lambda (lst)
                              (destructuring-match lst
                                ((((? else? _) . (? pair? expr)) more ...)
                                 (if (null? more)
                                     `(.ELSE ,@expr)
                                     (syntax-violation (car form) "misplaced else" form (car lst))))
                                ((((datum) . (? pair? expr)) _ ...)
                                 (if (or (symbol? datum) (fixnum? datum) (char? datum) (boolean? datum) (null? datum))
                                     `((.EQ? ,temp ',datum) ,@expr)
                                     `((.EQV? ,temp ',datum) ,@expr)))
                                ((((? list? datum) . (? pair? expr)) _ ...)
                                 (cond ((null? datum) '(#f))
                                       ((every1 (lambda (e) (or (symbol? e) (fixnum? e) (char? e) (boolean? e) (null? datum))) datum)
                                        `((.MEMQ ,temp ',datum) ,@expr))
                                       (else
                                        `((.MEMV ,temp ',datum) ,@expr))))
                                (_ (syntax-violation (car form) "malformed case clause" form (car lst)))))
                            clauses)))
           form)
          env)))
      (_
       (syntax-violation (car form) "expected case clause" form)))))

(define expand-and
  (lambda (form env)
    (annotate `(and ,@(expand-each (cdr form) env)) form)))

(define expand-or
  (lambda (form env)
    (annotate `(or ,@(expand-each (cdr form) env)) form)))

(define desugar-define
  (lambda (form)
    (destructuring-match form
      ((_ (? symbol? _) _) form)
      ((_ (? symbol? name))
       (annotate `(define ,name (.UNSPECIFIED)) form))
      ((_ ((? symbol? name) . formals))
       (begin
         (collect-lambda-formals (annotate formals form) form)
         (annotate `(define ,name (.LAMBDA ,formals (.UNSPECIFIED))) form)))
      ((_ ((? symbol? name) . formals) . (? pair? body))
       (begin
         (collect-lambda-formals (annotate formals form) form)
         (annotate `(define ,name (.LAMBDA ,formals ,@body)) form)))
      ((_ (e1 . e2) . _)
       (syntax-violation (car form) "invalid syntax" form e1))
      (_
       (let ((len (- (length form) 1)))
         (case len
           ((0) (syntax-violation (car form) "expected 1 or 2, but no clause given" form))
           ((1 2) (syntax-violation (car form) "expected symbol for first clause" form))
           (else (syntax-violation (car form) (format "expected 1 or 2, but ~a clauses given" len) form))))))))

(define desugar-define-macro
  (lambda (form)
    (destructuring-match form
      ((_ (? symbol? _) _) form)
      ((_ ((? symbol? name) . formals) . (? pair? body))
       (begin
         (collect-lambda-formals (annotate formals form) form)
         (annotate `(define-macro ,name (.LAMBDA ,formals ,@body)) form)))
      ((_ (e1 . e2) . _)
       (syntax-violation (car form) "invalid syntax" form e1))
      (_
       (syntax-violation (car form) "expected symbol and expression" form)))))

(define rewrite-define-macro
  (lambda (form)
    (destructuring-match (desugar-define-macro form)
      ((_ name body)
       (let ((x (generate-temporary-symbol)))
         (annotate `(.DEFINE-SYNTAX ,name (.LAMBDA (,x) (.APPLY ,body (.CDR ,x)))) form))))))

(define expand-define-macro
  (lambda (form env)
    (and (unexpect-top-level-form)
         (syntax-violation (car form) "misplaced definition" form))
    (expand-form (rewrite-define-macro form) env)))

(define expand-identifier-syntax
  (lambda (form env)

    (define set!? (lambda (id) (denote-set!? env id)))

    (destructuring-match form
      ((_ e)
       (expand-form
        (annotate
         `(.LAMBDA (x)
            (.SYNTAX-CASE x ()
                          (id (.IDENTIFIER? (.SYNTAX id)) (.SYNTAX ,e))
                          ((_ x ...) (.SYNTAX (,e x ...)))))
         form)
        env))
      ((_ (id exp1) (((? set!? _) var val) exp2))
       (expand-form
        (annotate
         `(.MAKE-VARIABLE-TRANSFORMER
           (.LAMBDA (x)
             (.SYNTAX-CASE x (set!)
                           ((set! ,var ,val) (.SYNTAX ,exp2))
                           ((,id x ...) (.SYNTAX (,exp1 x ...)))
                           (,id (.IDENTIFIER? (.SYNTAX id)) (.SYNTAX ,exp1)))))
         form)
        env)))))

(define expand-assert
  (lambda (form env)
    (destructuring-match form
      ((_ e)
       (expand-form
        (annotate
         `(.OR ,e (assertion-violation ',(current-top-level-exterior) ,(format "assertion failed in expression ~u" e)))
         form)
        env))
      (_
       (syntax-violation (car form) "expected single expression" form)))))
