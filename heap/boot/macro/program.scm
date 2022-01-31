;;; Copyright (c) 2004-2022 Yoshikatsu Fujita / LittleWing Company Limited.
;;; See LICENSE file for terms and conditions of use.

(define expand-top-level-program
  (lambda (form env)
    (define permute-env
      (lambda (ht)
        (let loop ((lst (core-hashtable->alist ht)) (bounds '()) (unbounds '()))
          (cond ((null? lst) (append bounds unbounds))
                ((unbound? (cdar lst)) (loop (cdr lst) bounds (cons (car lst) unbounds)))
                (else (loop (cdr lst) (cons (car lst) bounds) unbounds))))))
    (define library-name '(|.TOP-LEVEL-PROGRAM|))
    (destructuring-match form
      ((('import import-spec ...) body ...)
       (let ((library-id (library-name->id form library-name))
             (library-version (library-name->version form library-name)))
         (and library-version (core-hashtable-set! (scheme-library-versions) library-id library-version))
         (let ((imports (parse-imports form import-spec))
               (depends (parse-depends form import-spec))
               (ht-immutables (make-core-hashtable))
               (ht-imports (make-core-hashtable)))
           (for-each
             (lambda (a)
               (core-hashtable-set! ht-immutables (car a) #t)
               (cond ((core-hashtable-ref ht-imports (car a) #f)
                      =>
                      (lambda (deno)
                        (or (eq? deno (cdr a))
                            (syntax-violation
                              "top-level program"
                              "duplicate import identifiers"
                              (abbreviated-take-form form 4 8)
                              (car a)))))
                     (else (core-hashtable-set! ht-imports (car a) (cdr a)))))
             imports)
           (let ((ht-env (make-shield-id-table body)) (ht-libenv (make-core-hashtable)))
             (for-each
               (lambda (a) (core-hashtable-set! ht-env (car a) (cdr a)) (core-hashtable-set! ht-libenv (car a) (cdr a)))
               (core-hashtable->alist ht-imports))
             (parameterize ((current-immutable-identifiers ht-immutables))
               (expand-top-level-program-body
                 form
                 library-id
                 library-version
                 body
                 imports
                 depends
                 (extend-env private-primitives-environment (permute-env ht-env))
                 (permute-env ht-libenv)))))))
      (_
        (syntax-violation
          "top-level program"
          "expected import form and top-level body"
          (abbreviated-take-form form 4 8))))))

(define expand-top-level-program-body
  (lambda (form library-id library-version body imports depends env libenv)
    (define initial-libenv #f)
    (define macro-defs '())
    (define extend-env!
      (lambda (datum1 datum2)
        (and (macro? datum2) (set! macro-defs (acons datum1 datum2 macro-defs)))
        (set! env (extend-env (list (cons datum1 datum2)) env))
        (for-each (lambda (a) (set-cdr! (cddr a) env)) macro-defs)))
    (define extend-libenv!
      (lambda (datum1 datum2)
        (set! libenv (extend-env (list (cons datum1 datum2)) libenv))
        (current-template-environment libenv)))
    (define rewrite-body
      (lambda (body defs macros renames)
        (define rewrite-env
          (lambda (env)
            (let loop ((lst (reverse env)) (acc '()))
              (cond ((null? lst) acc)
                    ((uninterned-symbol? (caar lst))
                     (if (assq (cdar lst) defs)
                         (loop (cdr lst) (cons (cons (caar lst) (cddr (assq (cdar lst) libenv))) acc))
                         (loop (cdr lst) (cons (car lst) acc))))
                    ((assq (caar lst) (cdr lst)) (loop (cdr lst) acc))
                    (else (loop (cdr lst) (cons (car lst) acc)))))))
        (check-duplicate-definition "top-level program" defs macros renames)
        (let ((env (rewrite-env env)))
          (let ((rewrited-body (expand-each body env)))
            (let ((rewrited-depends (map (lambda (dep) `(|.require-scheme-library| ',dep)) depends))
                  (rewrited-defs
                    (map (lambda (def)
                           (parameterize ((current-top-level-exterior (car def)))
                             (let ((lhs (cdr (assq (car def) renames))) (rhs (expand-form (cadr def) env)))
                               (set-closure-comment! rhs lhs)
                               `(define ,lhs ,rhs))))
                         defs)))
              (let ((vars (map cadr rewrited-defs)) (assignments (map caddr rewrited-defs)))
                (cond
                  ((check-rec*-contract-violation vars assignments)
                   =>
                   (lambda (var)
                     (let ((id (any1 (lambda (a) (and (eq? (cdr a) (car var)) (car a))) renames)))
                       (current-macro-expression #f)
                       (syntax-violation
                         #f
                         (format "attempt to reference uninitialized variable ~u" id)
                         (any1
                           (lambda (e) (and (check-rec-contract-violation (list id) e) (annotate `(define ,@e) e)))
                           defs)))))))
              (annotate `(begin ,@rewrited-depends ,@rewrited-defs ,@rewrited-body) form))))))
    (define ht-imported-immutables (make-core-hashtable))
    (define expression-tag (let ((num 0)) (lambda () (set! num (+ num 1)) (string->symbol (format ".e~a" num)))))
    (current-template-environment libenv)
    (for-each (lambda (b) (core-hashtable-set! ht-imported-immutables (car b) #t)) imports)
    (let loop ((body (flatten-begin body env)) (defs '()) (macros '()) (renames '()))
      (if (null? body)
          (rewrite-body body (reverse defs) (reverse macros) renames)
          (cond ((and (pair? body) (pair? (car body)) (symbol? (caar body)))
                 (let ((deno (env-lookup env (caar body))))
                   (cond ((eq? denote-begin deno) (loop (flatten-begin body env) defs macros renames))
                         ((eq? denote-define-syntax deno)
                          (destructuring-match body
                            (((_ (? symbol? org) clause) more ...)
                             (begin
                               (and (core-hashtable-contains? ht-imported-immutables org)
                                    (syntax-violation 'define-syntax "attempt to modify immutable binding" (car body)))
                               (let-values (((code . expr)
                                             (parameterize ((current-template-environment initial-libenv))
                                               (compile-macro (car body) clause env))))
                                 (let ((new (generate-global-id library-id org)))
                                   (extend-libenv! org (make-import new))
                                   (cond ((procedure? code)
                                          (extend-env! org (make-macro code env))
                                          (loop
                                            more
                                            defs
                                            (cons (list org 'procedure (car expr)) macros)
                                            (acons org new renames)))
                                         ((macro-variable? code)
                                          (extend-env! org (make-macro-variable (cadr code) env))
                                          (loop
                                            more
                                            defs
                                            (cons (list org 'variable (car expr)) macros)
                                            (acons org new renames)))
                                         (else
                                           (extend-env! org (make-macro code env))
                                           (loop
                                             more
                                             defs
                                             (cons (list org 'template code) macros)
                                             (acons org new renames))))))))
                            (_ (syntax-violation 'define-syntax "expected symbol and single expression" (car body)))))
                         ((eq? denote-define deno)
                          (let ((def (annotate (cdr (desugar-define (car body))) (car body))))
                            (and (core-hashtable-contains? ht-imported-immutables (car def))
                                 (syntax-violation 'define "attempt to modify immutable binding" (car body)))
                            (let ((org (car def)) (new (generate-global-id library-id (car def))))
                              (extend-env! org new)
                              (extend-libenv! org (make-import new))
                              (loop (cdr body) (cons def defs) macros (acons org new renames)))))
                         ((or (macro? deno) (eq? denote-let-syntax deno) (eq? denote-letrec-syntax deno))
                          (let-values (((expr new) (expand-initial-forms (car body) env)))
                            (set! env new)
                            (loop (append (flatten-begin (list expr) env) (cdr body)) defs macros renames)))
                         (else
                           (loop (cons `(|.define| ,(expression-tag) ,(car body)) (cdr body)) defs macros renames)))))
                (else (loop (cons `(|.define| ,(expression-tag) ,(car body)) (cdr body)) defs macros renames)))))))
