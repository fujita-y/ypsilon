;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2008 Y.FUJITA, LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(define expand-lambda
  (lambda (form env)
    (cond ((> (safe-length form) 2)
           (let ((vars (collect-lambda-formals (cadr form) form)))
             (let* ((suffix (fresh-rename-count))
                    (renames (map cons vars (map (lambda (id) (rename-variable-id id suffix)) vars)))
                    (body (expand-body form (cddr form) (extend-env renames env))))
               (if (null? body)
                   (syntax-violation 'lambda "empty body" form)
                   (annotate `(lambda ,(rename-lambda-formals (cadr form) renames) ,@body) form)))))
          (else
           (syntax-violation 'lambda "expected formals and body" form)))))

(define expand-quote
  (lambda (form env)
    (if (= (safe-length form) 2)
        (annotate `(quote ,(strip-rename-suffix (cadr form))) form)
        (syntax-violation 'quote "expected single datum" form))))

(define expand-begin
  (lambda (form env)
    (and (unexpect-top-level-form)
         (or (pair? (cdr form))
             (syntax-violation 'begin "misplaced empty begin" form)))
    (annotate `(begin ,@(flatten-begin (expand-each (cdr form) env) env)) form)))

(define expand-if
  (lambda (form env)
    (annotate (destructuring-match form
                ((_ test expr)
                 `(if ,(expand-form test env)
                      ,(expand-form expr env)))
                ((_ test expr1 expr2)
                 `(if ,(expand-form test env)
                      ,(expand-form expr1 env)
                      ,(expand-form expr2 env)))
                (_
                 (syntax-violation 'if "expected 2 or 3 expressions" form)))
              form)))

(define expand-set!
  (lambda (form env)
    (destructuring-match form
      ((_ (? symbol? name) expr)
       (let ((deno (env-lookup env name)))
         (cond ((macro-variable? deno)
                (let-values (((expr renames) (expand-macro-use form env deno)))
                  (expand-form expr (extend-env renames env))))
               ((or (special? deno) (macro? deno))
                (syntax-violation 'set! "misplaced syntactic keyword as variable" form))
               (else
                (let ((var (expand-form name env)))
                  (and (core-hashtable-contains? immutable-primitives var)
                       (syntax-violation 'set! "attempt to modify immutable variable" form))
                  (and (current-immutable-identifiers)
                       (not (renamed-id? var))
                       (core-hashtable-ref (current-immutable-identifiers) name #f)
                       (syntax-violation 'set! "attempt to modify immutable variable" form))
                  (let ((body (expand-form expr env)))
                    (and (pair? body)
                         (denote-lambda? env (car body))
                         (set-closure-comment! body (original-id var)))
                    (annotate `(set! ,var ,body) form)))))))
      (_
       (syntax-violation 'set! "expected variable and single expression" form)))))

(define expand-let-syntax
  (lambda (form env)
    (destructuring-match form
      ((_ bindings body ...)
       (begin
         (and (null? body) (syntax-violation (car form) "missing body" form))
         (check-let-bindings form bindings)
         (fresh-rename-count)
         (expand-form `(.BEGIN ,@body) (expand-let-syntax-bindings form bindings env))))
      (_
       (syntax-violation (car form) "expected bindings and body" form)))))

(define expand-letrec-syntax
  (lambda (form env)
    (destructuring-match form
      ((_ bindings body ...)
       (begin
         (and (null? body) (syntax-violation (car form) "missing body" form))
         (check-let-bindings form bindings)
         (fresh-rename-count)
         (expand-form `(.BEGIN ,@body) (expand-letrec-syntax-bindings form bindings env))))
      (_
       (syntax-violation (car form) "expected bindings and body" form)))))

(define expand-define-syntax
  (lambda (form env)
    (and (unexpect-top-level-form)
         (syntax-violation (car form) "misplaced definition" form))
    (destructuring-match form
      ((_ (? symbol? name) body)
       (begin
         (parameterize ((unexpect-top-level-form #t))
           (let-values (((code . expr) (compile-macro form body env)))
             (if (macro-variable? code)
                 (.set-top-level-macro! 'variable name (cadr code) env)
                 (.set-top-level-macro! 'syntax name code env))))
         (env-delete! env name)
         '(begin)))
      (_
       (syntax-violation (car form) "expected symbol and single expression" form)))))

(define expand-define
  (lambda (form env)

    (define immutable?
      (lambda (id)
        (or (core-hashtable-contains? immutable-primitives id)
            (memq id '(library define define-syntax
                        quote lambda if set!
                        cond case and or let let* letrec letrec*
                        let-values let*-values
                        begin quasiquote unquote unquote-splicing
                        let-syntax letrec-syntax syntax-rules identifier-syntax
                        assert else => ... _)))))

    (define let?
      (lambda (id)
        (denote-let? env id)))

    (and (unexpect-top-level-form)
         (syntax-violation (car form) "misplaced definition" form))
    (destructuring-match (desugar-define form)
      ((_ name body)
       (begin
         (and (immutable? name)
              (syntax-violation (car form) "attempt to modify immutable binding" form))
         (let ((body (parameterize ((unexpect-top-level-form #t) (current-top-level-exterior name))
                       (expand-form body env))))
           (destructuring-match body
             (((? let? _) _ e1)
              (set-closure-comment! e1 (original-id name)))
             (_
              (set-closure-comment! body (original-id name))))
           (core-hashtable-delete! (current-macro-environment) name)
           (env-delete! env name)
           (annotate `(define ,name ,body) form)))))))
