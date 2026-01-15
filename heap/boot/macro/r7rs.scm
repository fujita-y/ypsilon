;;; Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
;;; See LICENSE file for terms and conditions of use.

(define feature-identifiers
  (make-parameter
    (cons*
      (if (= (architecture-feature 'sizeof:void*) 64) 'lp64 'ilp32)
      (string->symbol (string-append (symbol->string (native-endianness)) "-endian"))
      '(r7rs exact-closed exact-complex ieee-float full-unicode ratios posix ypsilon))))

(define fulfill-feature-requirements?
  (lambda (form spec)
    (let loop ((spec spec))
      (destructuring-match spec
        ((? symbol? id) (memq id (feature-identifiers)))
        (('and) #t)
        (('and clause . more)
         (and (if (symbol? clause) (memq clause (feature-identifiers)) (loop clause))
              (loop `(and ,@more))))
        (('or) #f)
        (('or clause . more)
         (or (if (symbol? clause)
                 (memq clause (feature-identifiers))
                 (loop clause))
             (loop `(or ,@more))))
        (('not) (syntax-violation 'cond-expand "malformed clause" (abbreviated-take-form form 4 8) spec))
        (('not clause) (not (loop clause)))
        (('library name)
         (or (member name '((core primitives) '(core intrinsics)))
             (core-hashtable-ref (scheme-library-exports) (generate-library-id name) #f)
             (locate-library-file name)))
        (_ (syntax-violation 'cond-expand "malformed clause" (abbreviated-take-form form 4 8) spec))))))

(define expand-include
  (lambda (form env)
    `(begin ,@(apply append (map (lambda (e) (read-include-file (current-library-name) e 'include)) (cdr form))))))

(define expand-include-ci
  (lambda (form env)
    `(begin ,@(apply append (map (lambda (e) (read-include-file (current-library-name) e 'include-ci)) (cdr form))))))

(define parse-cond-expand
  (lambda (form specs)
    (let loop ((spec specs))
      (destructuring-match spec
        (() '())
        ((('else body ...)) body)
        ((('else body ...) . _)
         (syntax-violation 'cond-expand "misplaced else" (abbreviated-take-form form 4 8) (car spec)))
        (((condition body ...) . more) (if (fulfill-feature-requirements? form condition) body (loop more)))
        (_ (syntax-violation 'cond-expand "malformed clause" (abbreviated-take-form form 4 8) (car spec)))))))

(define expand-define-library
  (lambda (form env)
    (define permute-env
      (lambda (ht)
        (let loop ((lst (core-hashtable->alist ht)) (bounds '()) (unbounds '()))
          (cond ((null? lst) (append bounds unbounds))
                ((unbound? (cdar lst)) (loop (cdr lst) bounds (cons (car lst) unbounds)))
                (else (loop (cdr lst) (cons (car lst) bounds) unbounds))))))
    (parameterize ((lexical-syntax-version 7))
      (destructuring-match form
        ((_ library-name clauses ...)
         (let ((library-id (library-name->id form library-name)) (library-version (library-name->version form library-name)))
           (and library-version (core-hashtable-set! (scheme-library-versions) library-id library-version))
           (parameterize ((current-include-files (make-core-hashtable)) (current-library-name library-name))
             (let ((coreform
                     (let loop ((clauses clauses) (exports '()) (imports '()) (depends '()) (commands '()))
                       (if (null? clauses)
                           (let ((ht-immutables (make-core-hashtable))
                                 (ht-imports (make-core-hashtable))
                                 (ht-publics (make-core-hashtable)))
                             (for-each
                               (lambda (a)
                                 (and (core-hashtable-ref ht-publics (cdr a) #f)
                                      (syntax-violation
                                        'define-library
                                        "duplicate export identifier"
                                        (abbreviated-take-form form 4 8)
                                        (cdr a)))
                                 (core-hashtable-set! ht-publics (cdr a) #t)
                                 (core-hashtable-set! ht-immutables (car a) #t))
                               exports)
                             (for-each
                               (lambda (a)
                                 (core-hashtable-set! ht-immutables (car a) #t)
                                 (cond ((core-hashtable-ref ht-imports (car a) #f)
                                        =>
                                        (lambda (deno)
                                          (or (eq? deno (cdr a))
                                              (syntax-violation
                                                'define-library
                                                "duplicate import identifier"
                                                (abbreviated-take-form form 4 8)
                                                (car a)))))
                                       (else (core-hashtable-set! ht-imports (car a) (cdr a)))))
                               imports)
                             (let ((ht-env (make-shield-id-table commands)) (ht-libenv (make-core-hashtable)))
                               (for-each
                                 (lambda (a) (core-hashtable-set! ht-env (car a) (cdr a)) (core-hashtable-set! ht-libenv (car a) (cdr a)))
                                 (core-hashtable->alist ht-imports))
                               (parameterize ((current-immutable-identifiers ht-immutables))
                                 (expand-define-library-body
                                   form
                                   library-name
                                   library-id
                                   library-version
                                   commands
                                   exports
                                   imports
                                   depends
                                   (extend-env private-primitives-environment (permute-env ht-env))
                                   (permute-env ht-libenv)))))
                           (destructuring-match clauses
                             ((('export export-spec ...) more ...)
                              (loop more (append exports (parse-exports/r7rs form export-spec)) imports depends commands))
                             ((('import import-spec ...) more ...)
                              (loop
                                more
                                exports
                                (append imports (parse-imports form import-spec))
                                (append depends (parse-depends form import-spec))
                                commands))
                             ((('include path ...) more ...)
                              (every1 string? path)
                              (let ((more
                                      `((begin ,@(apply append (map (lambda (e) (read-include-file library-name e 'include)) path)))
                                        ,@more)))
                                (loop more exports imports depends commands)))
                             ((('include-ci path ...) more ...)
                              (every1 string? path)
                              (let ((more
                                      `((begin ,@(apply append (map (lambda (e) (read-include-file library-name e 'include-ci)) path)))
                                        ,@more)))
                                (loop more exports imports depends commands)))
                             ((('include-library-declarations path ...) more ...)
                              (every1 string? path)
                              (let ((more
                                      (append
                                        (apply
                                          append
                                          (map (lambda (e) (read-include-file library-name e 'include-library-declarations)) path))
                                        more)))
                                (loop more exports imports depends commands)))
                             ((('cond-expand spec ...) more ...)
                              (loop (append (parse-cond-expand form spec) more) exports imports depends commands))
                             ((('begin body ...) more ...) (loop more exports imports depends (append commands body)))
                             (_
                               (syntax-violation
                                 'define-library
                                 "malformed library declarations"
                                 (abbreviated-take-form form 4 8)
                                 (car clauses))))))))
               (or (= (core-hashtable-size (current-include-files)) 0)
                   (core-hashtable-set! library-include-dependencies library-id (current-include-files)))
               coreform))))
        (_ (syntax-violation 'define-library "expected library name and declarations" (abbreviated-take-form form 4 8)))))))

(define expand-define-library-body
  (lambda (form library-name library-id library-version body exports imports depends env libenv)
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
    (define ht-imported-immutables (make-core-hashtable))
    (define expression-tag (let ((num 0)) (lambda () (set! num (+ num 1)) (string->symbol (format ".e~a" num)))))
    (current-template-environment libenv)
    (for-each (lambda (b) (core-hashtable-set! ht-imported-immutables (car b) #t)) imports)
    (let loop ((body (flatten-begin body env)) (defs '()) (macros '()) (renames '()))
      (cond ((null? body)
             (rewrite-library-body
               form
               library-id
               library-version
               body
               (reverse defs)
               (reverse macros)
               renames
               exports
               imports
               depends
               env
               libenv))
            ((and (pair? body) (pair? (car body)) (symbol? (caar body)))
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
                     ((eq? denote-include deno)
                      (let ((expr `(begin ,@(apply append (map (lambda (e) (read-include-file library-name e 'include)) (cdar body))))))
                        (loop (append (flatten-begin (list expr) env) (cdr body)) defs macros renames)))
                     ((eq? denote-include-ci deno)
                      (let ((expr `(begin ,@(apply append (map (lambda (e) (read-include-file library-name e 'include-ci)) (cdar body))))))
                        (loop (append (flatten-begin (list expr) env) (cdr body)) defs macros renames)))
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
                     (else (loop (cons `(|.define| ,(expression-tag) ,(car body)) (cdr body)) defs macros renames)))))
            (else (loop (cons `(|.define| ,(expression-tag) ,(car body)) (cdr body)) defs macros renames))))))
