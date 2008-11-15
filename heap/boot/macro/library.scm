;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2008 Y.FUJITA, LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(define scheme-library-paths (make-parameter '()))
(define scheme-library-exports (make-parameter (make-core-hashtable)))
(define scheme-library-versions (make-parameter (make-core-hashtable)))

(define symbol-list->string
  (lambda (ref infix)
    (apply string-append
           (cdr (let loop ((lst ref))
                  (cond ((null? lst) '())
                        ((symbol? (car lst))
                         (cons infix
                               (cons (symbol->string (car lst))
                                     (loop (cdr lst)))))
                        (else
                         (loop (cdr lst)))))))))

(define generate-library-id
  (lambda (name)
    (library-name->id #f name)))

(define library-name->id
  (lambda (form name)

    (define malformed-name
      (lambda ()
        (if form
            (syntax-violation 'library "malformed library name" (abbreviated-take-form form 4 8) name)
            (syntax-violation 'library "malformed library name" name))))

    (if (and (list? name) (not (null? name)))
        (if (every1 symbol? name)
            (string->symbol (symbol-list->string name (format "~a" (current-library-infix))))
            (let ((body (list-head name (- (length name) 1))))
              (if (every1 symbol? body)
                  (string->symbol (symbol-list->string body (format "~a" (current-library-infix))))
                  (malformed-name))))
        (malformed-name))))

(define library-name->version
  (lambda (form name)

    (define malformed-version
      (lambda ()
        (if (pair? form)
            (syntax-violation 'library "malformed library version" (abbreviated-take-form form 4 8) name)
            (syntax-violation 'library "malformed library version" name))))

    (define exact-nonnegative-integer?
      (lambda (obj)
        (and (integer? obj) (exact? obj) (>= obj 0))))

    (if (and (list? name) (not (null? name)))
        (cond ((every1 symbol? name) #f)
              (else
               (let ((tail (car (list-tail name (- (length name) 1)))))
                 (cond ((null? tail) #f)
                       ((and (list? tail) (every1 exact-nonnegative-integer? tail)) tail)
                       (else (malformed-version))))))
        (malformed-version))))

(define library-reference->name
  (lambda (form lst)

    (define malformed-name
      (lambda ()
        (if (pair? form)
            (syntax-violation 'library "malformed library name" (abbreviated-take-form form 4 8) lst)
            (syntax-violation 'library "malformed library name" lst))))

    (cond ((every1 symbol? lst) lst)
          (else
           (let ((body (list-head lst (- (length lst) 1))))
             (cond ((every1 symbol? body) body)
                   (else (malformed-name))))))))

(define flatten-library-reference
  (lambda (form lst)
    (or (and (list? lst) (not (null? lst)))
        (syntax-violation 'library "malformed library name" (abbreviated-take-form form 4 8) lst))
    (destructuring-match lst
      (('library (ref ...)) (flatten-library-reference form ref))
      (('library . _) (syntax-violation 'library "malformed library name" (abbreviated-take-form form 4 8) lst))
      (_ lst))))

(define library-reference->version
  (lambda (form lst)

    (define malformed-version
      (lambda ()
        (if (pair? form)
            (syntax-violation 'import "malformed library version" (abbreviated-take-form form 4 8) lst)
            (syntax-violation 'import "malformed library version" lst))))

    (define exact-nonnegative-integer?
      (lambda (obj)
        (and (integer? obj) (exact? obj) (>= obj 0))))

    (if (and (list? lst) (not (null? lst)))
        (cond ((every1 symbol? lst) #f)
              (else (let ((tail (car (list-tail lst (- (length lst) 1)))))
                      (cond ((list? tail) tail)
                            (else (malformed-version))))))
        (malformed-version))))

(define test-library-versions
  (lambda (form spec reference current)

    (define exact-nonnegative-integer?
      (lambda (obj)
        (and (integer? obj) (exact? obj) (>= obj 0))))

    (or (let loop ((reference reference))
          (destructuring-match reference
            (('not ref) (not (loop ref)))
            (('or ref ...) (any1 (lambda (e) (loop e)) ref))
            (('and ref ...) (every1 (lambda (e) (loop e)) ref))
            (sub-reference
             (let loop ((current current) (sub-reference sub-reference))
               (or (and (list? current) (list? sub-reference))
                   (syntax-violation 'import "malformed version reference" (abbreviated-take-form form 4 8) spec))
               (or (null? sub-reference)
                   (and (>= (length current) (length sub-reference))
                        (let ((current (list-head current (length sub-reference))))
                          (every2 (lambda (c s)
                                    (destructuring-match s
                                      (('not ref) (not (loop c ref)))
                                      (('>= (? exact-nonnegative-integer? n)) (>= c n))
                                      (('<= (? exact-nonnegative-integer? n)) (<= c n))
                                      (('and ref ...) (every1 (lambda (e) (loop (list c) (list e))) ref))
                                      (('or ref ...) (any1 (lambda (e) (loop (list c) (list e))) ref))
                                      ((? exact-nonnegative-integer? n) (= c n))
                                      (else (syntax-violation 'import "malformed version reference" (abbreviated-take-form form 4 8) spec))))
                                  current sub-reference))))))))
        (syntax-violation 'import
                          (format "mismatch between version reference ~a and current version ~a" reference current)
                          (abbreviated-take-form form 4 8) spec))))

(define make-shield-id-table
  (lambda (lst)
    (let ((ht (make-core-hashtable)) (deno (make-unbound)))
      (for-each (lambda (id) (core-hashtable-set! ht id deno)) coreform-primitives)
      (let loop ((lst lst))
        (cond ((pair? lst)
               (loop (car lst))
               (loop (cdr lst)))
              ((symbol? lst)
               (core-hashtable-set! ht lst deno))
              ((vector? lst)
               (loop (vector->list lst)))))
      ht)))

(define parse-exports
  (lambda (form specs)
    (let loop ((spec specs) (exports '()))
      (destructuring-match spec
        (() (reverse exports))
        (((? symbol? id) . more)
         (loop more
               (acons id id exports)))
        ((('rename . alist) . more)
         (begin
           (or (every1 (lambda (e) (= (safe-length e) 2)) alist)
               (syntax-violation 'export "malformed export spec" (abbreviated-take-form form 4 8) (car spec)))
           (loop more
                 (append (map (lambda (e) (cons (car e) (cadr e))) alist)
                         exports))))
        (_
         (syntax-violation 'export "malformed export spec" (abbreviated-take-form form 4 8) (car spec)))))))

(define parse-imports
  (lambda (form specs)

    (define check-unbound-identifier
      (lambda (bindings ids subform)
        (for-each (lambda (id)
                    (or (assq id bindings)
                        (syntax-violation 'import (format "attempt to reference unexported identifier ~a" id) (abbreviated-take-form form 4 8) subform)))
                  ids)))

    (define check-bound-identifier
      (lambda (bindings ids subform)
        (for-each (lambda (id)
                    (and (assq id bindings)
                         (syntax-violation 'import (format "duplicate import identifiers ~a" id) (abbreviated-take-form form 4 8) subform)))
                  ids)))

    (let loop ((spec specs) (imports '()))
      (destructuring-match spec
        (() imports)
        ((('for set . phases) . more)
         (loop more
               (loop (list set) imports)))
        ((('only set . ids) . more)
         (let ((bindings (loop (list set) '())))
           (check-unbound-identifier bindings ids (car spec))
           (loop more
                 (append (filter (lambda (e) (memq (car e) ids)) bindings)
                         imports))))
        ((('except set . ids) . more)
         (let ((bindings (loop (list set) '())))
           (check-unbound-identifier bindings ids (car spec))
           (loop more
                 (append (filter (lambda (e) (not (memq (car e) ids))) bindings)
                         imports))))
        ((('rename set . alist) . more)
         (let ((bindings (loop (list set) '())))
           (or (every1 (lambda (e) (= (safe-length e) 2)) alist)
               (syntax-violation 'import "malformed import set" (abbreviated-take-form form 4 8) (car spec)))
           (or (and (unique-id-list? (map car alist))
                    (unique-id-list? (map cadr alist)))
               (syntax-violation 'import "duplicate identifers in rename specs" (abbreviated-take-form form 4 8) (car spec)))
           (check-bound-identifier bindings (map cadr alist) (car spec))
           (check-unbound-identifier bindings (map car alist) (car spec))
           (loop more
                 (append (map (lambda (e)
                                (cond ((assq (car e) alist)
                                       => (lambda (rename) (cons (cadr rename) (cdr e))))
                                      (else e)))
                              bindings)
                         imports))))
        ((('prefix set id) . more)
         (loop more
               (append (map (lambda (e)
                              (cons (string->symbol (format "~a~a" id (car e))) (cdr e)))
                            (loop (list set) '()))
                       imports)))

        ((ref . more)
         (let ((ref (flatten-library-reference form ref)))
           (let ((name (library-reference->name form ref))
                 (version (library-reference->version form ref)))
             (.require-scheme-library name)
             (let ((library-id (library-name->id form name)))
               (cond ((and version (core-hashtable-ref (scheme-library-versions) library-id #f))
                      => (lambda (current-version)
                           (test-library-versions form ref version current-version))))
               (loop more
                     (cond ((core-hashtable-ref (scheme-library-exports) library-id #f)
                            => (lambda (lst) (append lst imports)))
                           (else
                            (syntax-violation 'import (format "attempt to import undefined library ~s" name) (abbreviated-take-form form 4 8)))))))))
        (_
         (syntax-violation 'import "malformed import set" (abbreviated-take-form form 4 8) (car spec)))))))

(define parse-depends
  (lambda (form specs)
    (let loop ((spec specs) (depends '()))
      (destructuring-match spec
        (() depends)
        ((('for set . _) . more)
         (loop more (loop (list set) depends)))
        ((('only set . _) . more)
         (loop more (loop (list set) depends)))
        ((('except set . _) . more)
         (loop more (loop (list set) depends)))
        ((('rename set . _) . more)
         (loop more (loop (list set) depends)))
        ((('prefix set _) . more)
         (loop more (loop (list set) depends)))
        ((ref . more)
         (let ((ref (flatten-library-reference form ref)))
           (let ((name (library-reference->name form ref)))
             (loop more (cons name depends)))))
        (_
         (syntax-violation 'import "malformed import set" (abbreviated-take-form form 4 8) (car spec)))))))

(define expand-library
  (lambda (form env)

    (define permute-env
      (lambda (ht)
        (let loop ((lst (core-hashtable->alist ht)) (bounds '()) (unbounds '()))
          (cond ((null? lst)
                 (append bounds unbounds))
                ((unbound? (cdar lst))
                 (loop (cdr lst) bounds (cons (car lst) unbounds)))
                (else
                 (loop (cdr lst) (cons (car lst) bounds) unbounds))))))

    (destructuring-match form
      ((_ library-name ('export export-spec ...) ('import import-spec ...) body ...)
       (let ((library-id (library-name->id form library-name))
             (library-version (library-name->version form library-name)))
         (and library-version (core-hashtable-set! (scheme-library-versions) library-id library-version))
         (let ((exports (parse-exports form export-spec))
               (imports (parse-imports form import-spec))
               (depends (parse-depends form import-spec))
               (ht-immutables (make-core-hashtable))
               (ht-imports (make-core-hashtable))
               (ht-publics (make-core-hashtable)))
           (for-each (lambda (a)
                       (and (core-hashtable-ref ht-publics (cdr a) #f)
                            (syntax-violation 'library "duplicate export identifiers" (abbreviated-take-form form 4 8) (cdr a)))
                       (core-hashtable-set! ht-publics (cdr a) #t)
                       (core-hashtable-set! ht-immutables (car a) #t))
                     exports)
           (for-each (lambda (a)
                       (core-hashtable-set! ht-immutables (car a) #t)
                       (cond ((core-hashtable-ref ht-imports (car a) #f)
                              => (lambda (deno)
                                   (or (eq? deno (cdr a))
                                       (syntax-violation 'library "duplicate import identifiers" (abbreviated-take-form form 4 8) (car a)))))
                             (else
                              (core-hashtable-set! ht-imports (car a) (cdr a)))))
                     imports)
           (let ((ht-env (make-shield-id-table body)) (ht-libenv (make-core-hashtable)))
             (for-each (lambda (a)
                         (core-hashtable-set! ht-env (car a) (cdr a))
                         (core-hashtable-set! ht-libenv (car a) (cdr a)))
                       (core-hashtable->alist ht-imports))
             (parameterize ((current-immutable-identifiers ht-immutables))
               (expand-library-body form library-id library-version body exports imports depends
                                    (extend-env private-primitives-environment (permute-env ht-env))
                                    (permute-env ht-libenv)))))))
      (_
       (syntax-violation 'library "expected library name, export spec, and import spec" (abbreviated-take-form form 4 8))))))

(define expand-library-body
  (lambda (form library-id library-version body exports imports depends env libenv)

    (define internal-definition?
      (lambda (lst)
        (and (pair? lst)
             (pair? (car lst))
             (symbol? (caar lst))
             (let ((deno (env-lookup env (caar lst))))
               (or (macro? deno)
                   (eq? denote-define deno)
                   (eq? denote-define-syntax deno)
                   (eq? denote-define-macro deno)
                   (eq? denote-let-syntax deno)
                   (eq? denote-letrec-syntax deno))))))

    (define macro-defs '())

    (define extend-env!
      (lambda (datum1 datum2)
        (and (macro? datum2)
             (set! macro-defs (acons datum1 datum2 macro-defs)))
        (set! env (extend-env (list (cons datum1 datum2)) env))
        (for-each (lambda (a) (set-cdr! (cddr a) env)) macro-defs)))

    (define extend-libenv!
      (lambda (datum1 datum2)
        (set! libenv (extend-env (list (cons datum1 datum2)) libenv))))

    (define check-duplicate-definition
      (lambda (defs macros renames)
        (or (unique-id-list? (map car renames))
            (let ((id (find-duplicates (map car renames))))
              (cond ((assq id defs)
                     => (lambda (e1)
                          (let ((e2 (assq id (reverse defs))))
                            (cond ((eq? e1 e2)
                                   (let ((e2 (assq id macros)))
                                     (syntax-violation 'library "duplicate definitions"
                                                       (annotate `(define-syntax ,(car e2) ...) e2)
                                                       (annotate `(define ,@e1) e1))))
                                  (else
                                   (syntax-violation 'library "duplicate definitions"
                                                     (annotate `(define ,@e1) e1)
                                                     (annotate `(define ,@e2) e2)))))))
                    ((assq id macros)
                     => (lambda (e1)
                          (let ((e2 (assq id (reverse macros))))
                            (cond ((eq? e1 e2)
                                   (let ((e2 (assq id defs)))
                                     (syntax-violation 'library "duplicate definitions"
                                                       (annotate `(define-syntax ,(car e1) ...) e1)
                                                       (annotate `(define ,@e2) e2))))
                                  (else
                                   (syntax-violation 'library "duplicate definitions"
                                                     (annotate `(define-syntax ,(car e1) ...) e1)
                                                     (annotate `(define-syntax ,(car e2) ...) e2)))))))
                    (else
                     (syntax-violation 'library "duplicate definitions" id)))))))

    (define rewrite-body
      (lambda (body defs macros renames)
        (check-duplicate-definition defs macros renames)
        (let ((rewrited-body (expand-each body env)))
          (let* ((rewrited-depends
                  (map (lambda (dep)
                         `(.require-scheme-library ',dep))
                       depends))
                 (rewrited-defs
                  (map (lambda (def)
                         (parameterize ((current-top-level-exterior (car def)))
                           (let ((lhs (cdr (assq (car def) renames)))
                                 (rhs (expand-form (cadr def) env)))
                             (set-closure-comment! rhs lhs)
                             `(define ,lhs ,rhs))))
                       defs))
                 (rewrited-macros
                  (if (null? macros)
                      '()
                      (let ((ht-visibles (make-core-hashtable)))
                        (let loop ((lst (map caddr macros)))
                          (cond ((pair? lst) (loop (car lst)) (loop (cdr lst)))
                                ((symbol? lst) (core-hashtable-set! ht-visibles lst #t))
                                ((vector? lst) (loop (vector->list lst)))))
                        (for-each (lambda (b)
                                    (or (assq (car b) libenv)
                                        (let ((deno (env-lookup env (car b))))
                                          (if (and (symbol? deno) (not (eq? deno (car b))))
                                              (extend-libenv! (car b) (make-import deno))
                                              (or (uninterned-symbol? (car b))
                                                  (extend-libenv! (car b) (make-unbound)))))))
                                  (core-hashtable->alist ht-visibles))
                        (let ((shared-env (generate-temporary-symbol)))
                          `((let ((,shared-env
                                    ',(let ((ht (make-core-hashtable)))
                                        (for-each (lambda (a)
                                                    (and (core-hashtable-contains? ht-visibles (car a))
                                                         (core-hashtable-set! ht (car a) (cdr a))))
                                                  (reverse libenv))
                                        (core-hashtable->alist ht))))
                              ,@(map (lambda (e)
                                       (let ((id (cdr (assq (car e) renames)))
                                             (type (cadr e))
                                             (spec (caddr e)))
                                         (case type
                                           ((template) `(.set-top-level-macro! 'syntax ',id ',spec ,shared-env))
                                           ((procedure) `(.set-top-level-macro! 'syntax ',id ,spec ,shared-env))
                                           ((variable) `(.set-top-level-macro! 'variable ',id ,spec ,shared-env))
                                           (else (scheme-error "internal error in rewrite body: bad macro spec ~s" e)))))
                                     macros)))))))
                 (rewrited-exports
                  `(.intern-scheme-library
                    ',library-id
                    ',library-version
                    ',(begin
                        (map (lambda (e)
                               (cons (cdr e)
                                     (cond ((assq (car e) renames) => (lambda (a) (make-import (cdr a))))
                                           ((assq (car e) imports) => cdr)
                                           (else
                                            (current-macro-expression #f)
                                            (syntax-violation 'library
                                                              (format "attempt to export unbound identifier ~u" (car e))
                                                              (caddr form))))))
                             exports)))))
            (let ((vars (map cadr rewrited-defs))
                  (assignments (map caddr rewrited-defs)))
              (cond ((check-rec*-contract-violation vars assignments)
                     => (lambda (var)
                          (let ((id (any1 (lambda (a) (and (eq? (cdr a) (car var)) (car a))) renames)))
                            (current-macro-expression #f)
                            (syntax-violation #f
                                              (format "attempt to reference uninitialized variable ~u" id)
                                              (any1 (lambda (e)
                                                      (and (check-rec-contract-violation (list id) e)
                                                           (annotate `(define ,@e) e)))
                                                    defs)))))))
            (annotate `(begin
                         ,@rewrited-depends
                         ,@rewrited-defs
                         ,@rewrited-body
                         ,@rewrited-macros
                         ,rewrited-exports)
                      form)))))

    (define ht-imported-immutables (make-core-hashtable))

    (for-each (lambda (b) (core-hashtable-set! ht-imported-immutables (car b) #t)) imports)
    (let loop ((body (flatten-begin body env)) (defs '()) (macros '()) (renames '()))
      (cond ((and (pair? body) (pair? (car body)) (symbol? (caar body)))
             (let ((deno (env-lookup env (caar body))))
               (cond ((eq? denote-begin deno)
                      (loop (flatten-begin body env) defs macros renames))
                     ((eq? denote-define-syntax deno)
                      (destructuring-match body
                        (((_ (? symbol? org) clause) more ...)
                         (begin
                           (and (core-hashtable-contains? ht-imported-immutables org)
                                (syntax-violation 'define-syntax "attempt to modify immutable binding" (car body)))
                           (let-values (((code . expr) (compile-macro (car body) clause env)))
                             (let ((new (generate-global-id library-id org)))
                               (extend-libenv! org (make-import new))
                               (cond ((procedure? code)
                                      (extend-env! org (make-macro code env))
                                      (loop more defs (cons (list org 'procedure (car expr)) macros) (acons org new renames)))
                                     ((macro-variable? code)
                                      (extend-env! org (make-macro-variable (cadr code) env))
                                      (loop more defs (cons (list org 'variable (car expr)) macros) (acons org new renames)))
                                     (else
                                      (extend-env! org (make-macro code env))
                                      (loop more defs (cons (list org 'template code) macros) (acons org new renames))))))))
                        (_
                         (syntax-violation 'define-syntax "expected symbol and single expression" (car body)))))
                     ((eq? denote-define deno)
                      (let ((def (annotate (cdr (desugar-define (car body))) (car body))))
                        (and (core-hashtable-contains? ht-imported-immutables (car def))
                             (syntax-violation 'define "attempt to modify immutable binding" (car body)))
                        (let ((org (car def))
                              (new (generate-global-id library-id (car def))))
                          (extend-env! org new)
                          (extend-libenv! org (make-import new))
                          (loop (cdr body) (cons def defs) macros (acons org new renames)))))
                     ((eq? denote-define-macro deno)
                      (loop (cons (rewrite-define-macro (car body)) (cdr body)) defs macros renames))
                     ((or (macro? deno)
                          (eq? denote-let-syntax deno)
                          (eq? denote-letrec-syntax deno))
                      (let-values (((expr new) (expand-initial-forms (car body) env)))
                        (set! env new)
                        (let ((maybe-def (flatten-begin (list expr) env)))
                          (cond ((null? maybe-def)
                                 (loop (cdr body) defs macros renames))
                                ((internal-definition? maybe-def)
                                 (loop (append maybe-def (cdr body)) defs macros renames))
                                (else
                                 (rewrite-body body (reverse defs) (reverse macros) renames))))))
                     (else
                      (rewrite-body body (reverse defs) (reverse macros) renames)))))
            (else
             (rewrite-body body (reverse defs) (reverse macros) renames))))))

#;(define import-top-level-bindings
  (lambda (bindings)
    (for-each (lambda (binding)
                (let ((intern (car binding)) (extern (cddr binding)))
                  (or (eq? intern extern)
                      (begin
                        (core-hashtable-delete! (current-macro-environment) intern)
                        (if (top-level-bound? intern) (set-top-level-value! intern .&UNDEF))
                        (cond ((core-hashtable-ref (current-macro-environment) extern #f)
                               => (lambda (deno) (core-hashtable-set! (current-macro-environment) intern deno)))
                              (else (set-top-level-value! intern (top-level-value extern))))))))
              bindings)))

(define import-top-level-bindings
  (lambda (bindings)
    (for-each (lambda (binding)
                (let ((intern (car binding)) (extern (cddr binding)))
                  (or (eq? intern extern)
                      (cond ((core-hashtable-ref (current-macro-environment) extern #f)
                             => (lambda (deno)
                                  (if (top-level-bound? intern) (set-top-level-value! intern .&UNDEF))
                                  (core-hashtable-set! (current-macro-environment) intern deno)))
                            (else
                             (core-hashtable-delete! (current-macro-environment) intern)
                             (set-top-level-value! intern (top-level-value extern)))))))
              bindings)))

(define expand-import
  (lambda (form env)
    (and (unexpect-top-level-form)
         (syntax-violation 'import "misplaced top-level directive" form))
    (auto-compile-cache-update)
    (let ((imports (parse-imports form (cdr form))) (ht-bindings (make-core-hashtable)))
      (for-each (lambda (a)
                  (cond ((core-hashtable-ref ht-bindings (car a) #f)
                         => (lambda (deno)
                              (or (eq? deno (cdr a))
                                  (syntax-violation 'import "duplicate import identifiers" (abbreviated-take-form form 4 8) (car a)))))
                        (else
                         (core-hashtable-set! ht-bindings (car a) (cdr a)))))
                imports)
      (import-top-level-bindings (core-hashtable->alist ht-bindings)))))

(set-top-level-value! '.require-scheme-library
  (lambda (ref)
    (let ((library-id (generate-library-id ref)))
      (let ((exports (core-hashtable-ref (scheme-library-exports) library-id #f)))
        (cond ((eq? exports 'pending)
               (core-hashtable-set! (scheme-library-exports) library-id #f)
               (syntax-violation 'library "encountered cyclic dependencies" ref))
              ((eq? exports #f)
               (dynamic-wind
                (lambda ()
                  (core-hashtable-set! (scheme-library-exports) library-id 'pending))
                (lambda ()
                  (load-scheme-library ref #f))
                (lambda ()
                  (and (eq? (core-hashtable-ref (scheme-library-exports) library-id #f) 'pending)
                       (core-hashtable-set! (scheme-library-exports) library-id #f))))))))
    (unspecified)))

(define unify-import-bindings
  (let ((ht-import-bindings (make-core-hashtable 'equal?)))
    (lambda (lst)
      (map (lambda (binding)
             (cond ((core-hashtable-ref ht-import-bindings binding #f) => values)
                   (else
                    (begin (core-hashtable-set! ht-import-bindings binding binding) binding))))
           lst))))

(set-top-level-value! '.intern-scheme-library
  (lambda (library-id library-version exports)
    (and library-version (core-hashtable-set! (scheme-library-versions) library-id library-version))
    (core-hashtable-set! (scheme-library-exports) library-id (unify-import-bindings exports))))

(set-top-level-value! '.unintern-scheme-library
  (lambda (library-id)
    (core-hashtable-delete! (scheme-library-exports) library-id)))


;; todo: clean up and unify expand-top-level-program and expand-library when ERR5RS available

;; r6rs top-level program

(define expand-top-level-program
  (lambda (form env)

    (define permute-env
      (lambda (ht)
        (let loop ((lst (core-hashtable->alist ht)) (bounds '()) (unbounds '()))
          (cond ((null? lst)
                 (append bounds unbounds))
                ((unbound? (cdar lst))
                 (loop (cdr lst) bounds (cons (car lst) unbounds)))
                (else
                 (loop (cdr lst) (cons (car lst) bounds) unbounds))))))

    (define library-name '(.R6RS-TOP))

    (destructuring-match form
      ((('import import-spec ...) body ...)
       (let ((library-id (library-name->id form library-name))
             (library-version (library-name->version form library-name)))
         (and library-version (core-hashtable-set! (scheme-library-versions) library-id library-version))
         (let ((imports (parse-imports form import-spec))
               (depends (parse-depends form import-spec))
               (ht-immutables (make-core-hashtable))
               (ht-imports (make-core-hashtable)))
           (for-each (lambda (a)
                       (core-hashtable-set! ht-immutables (car a) #t)
                       (cond ((core-hashtable-ref ht-imports (car a) #f)
                              => (lambda (deno)
                                   (or (eq? deno (cdr a))
                                       (syntax-violation "top-level program" "duplicate import identifiers" (abbreviated-take-form form 4 8) (car a)))))
                             (else
                              (core-hashtable-set! ht-imports (car a) (cdr a)))))
                     imports)
           (let ((ht-env (make-shield-id-table body)) (ht-libenv (make-core-hashtable)))
             (for-each (lambda (a)
                         (core-hashtable-set! ht-env (car a) (cdr a))
                         (core-hashtable-set! ht-libenv (car a) (cdr a)))
                       (core-hashtable->alist ht-imports))
             (parameterize ((current-immutable-identifiers ht-immutables))
               (expand-top-level-program-body form library-id library-version body imports depends
                                              (extend-env private-primitives-environment (permute-env ht-env))
                                              (permute-env ht-libenv)))))))
      (_
       (syntax-violation "top-level program" "expected import form and top-level body" (abbreviated-take-form form 4 8))))))

(define expand-top-level-program-body
  (lambda (form library-id library-version body imports depends env libenv)

    (define internal-definition?
      (lambda (lst)
        (and (pair? lst)
             (pair? (car lst))
             (symbol? (caar lst))
             (let ((deno (env-lookup env (caar lst))))
               (or (macro? deno)
                   (eq? denote-define deno)
                   (eq? denote-define-syntax deno)
                   (eq? denote-define-macro deno)
                   (eq? denote-let-syntax deno)
                   (eq? denote-letrec-syntax deno))))))

    (define macro-defs '())

    (define extend-env!
      (lambda (datum1 datum2)
        (and (macro? datum2)
             (set! macro-defs (acons datum1 datum2 macro-defs)))
        (set! env (extend-env (list (cons datum1 datum2)) env))
        (for-each (lambda (a) (set-cdr! (cddr a) env)) macro-defs)))

    (define extend-libenv!
      (lambda (datum1 datum2)
        (set! libenv (extend-env (list (cons datum1 datum2)) libenv))))

    (define check-duplicate-definition
      (lambda (defs macros renames)
        (or (unique-id-list? (map car renames))
            (let ((id (find-duplicates (map car renames))))
              (cond ((assq id defs)
                     => (lambda (e1)
                          (let ((e2 (assq id (reverse defs))))
                            (cond ((eq? e1 e2)
                                   (let ((e2 (assq id macros)))
                                     (syntax-violation "top-level program" "duplicate definitions"
                                                       (annotate `(define-syntax ,(car e2) ...) e2)
                                                       (annotate `(define ,@e1) e1))))
                                  (else
                                   (syntax-violation "top-level program" "duplicate definitions"
                                                     (annotate `(define ,@e1) e1)
                                                     (annotate `(define ,@e2) e2)))))))
                    ((assq id macros)
                     => (lambda (e1)
                          (let ((e2 (assq id (reverse macros))))
                            (cond ((eq? e1 e2)
                                   (let ((e2 (assq id defs)))
                                     (syntax-violation "top-level program" "duplicate definitions"
                                                       (annotate `(define-syntax ,(car e1) ...) e1)
                                                       (annotate `(define ,@e2) e2))))
                                  (else
                                   (syntax-violation "top-level program" "duplicate definitions"
                                                     (annotate `(define-syntax ,(car e1) ...) e1)
                                                     (annotate `(define-syntax ,(car e2) ...) e2)))))))
                    (else
                     (syntax-violation "top-level program" "duplicate definitions" id)))))))

    (define rewrite-body
      (lambda (body defs macros renames)
        (check-duplicate-definition defs macros renames)
        (let ((rewrited-body (expand-each body env)))
          (let ((rewrited-depends
                 (map (lambda (dep)
                        `(.require-scheme-library ',dep))
                      depends))
                (rewrited-defs
                 (map (lambda (def)
                        (parameterize ((current-top-level-exterior (car def)))
                          (let ((lhs (cdr (assq (car def) renames)))
                                (rhs (expand-form (cadr def) env)))
                            (set-closure-comment! rhs lhs)
                            `(define ,lhs ,rhs))))
                      defs))
                (rewrited-macros
                 (if (null? macros)
                     '()
                     (let ((shared-env (generate-temporary-symbol)))
                       `((let ((,shared-env
                                 ',(let ((ht (make-core-hashtable)))
                                     (for-each (lambda (a)
                                                 (if (not (unbound? (cdr a)))
                                                     (core-hashtable-set! ht (car a) (cdr a))))
                                               (reverse libenv))
                                     (core-hashtable->alist ht))))
                           ,@(map (lambda (e)
                                    (let ((id (cdr (assq (car e) renames)))
                                          (type (cadr e))
                                          (spec (caddr e)))
                                      (case type
                                        ((template) `(.set-top-level-macro! 'syntax ',id ',spec ,shared-env))
                                        ((procedure) `(.set-top-level-macro! 'syntax ',id ,spec ,shared-env))
                                        ((variable) `(.set-top-level-macro! 'variable ',id ,spec ,shared-env))
                                        (else (scheme-error "internal error in rewrite body: bad macro spec ~s" e)))))
                                  macros)))))))
            (let ((vars (map cadr rewrited-defs))
                  (assignments (map caddr rewrited-defs)))
              (cond ((check-rec*-contract-violation vars assignments)
                     => (lambda (var)
                          (let ((id (any1 (lambda (a) (and (eq? (cdr a) (car var)) (car a))) renames)))
                            (current-macro-expression #f)
                            (syntax-violation #f
                                              (format "attempt to reference uninitialized variable ~u" id)
                                              (any1 (lambda (e)
                                                      (and (check-rec-contract-violation (list id) e)
                                                           (annotate `(define ,@e) e)))
                                                    defs)))))))
            (annotate `(begin
                         ,@rewrited-depends
                         ,@rewrited-defs
                         ,@rewrited-body
                         ,@rewrited-macros)
                      form)))))

    (define ht-imported-immutables (make-core-hashtable))

    (define expression-tag
      (let ((num 0))
        (lambda ()
          (set! num (+ num 1))
          (string->symbol (format ".e~a" num)))))

    (for-each (lambda (b) (core-hashtable-set! ht-imported-immutables (car b) #t)) imports)
    (let loop ((body (flatten-begin body env)) (defs '()) (macros '()) (renames '()))
      (if (null? body)
          (rewrite-body body (reverse defs) (reverse macros) renames)
          (cond ((and (pair? body) (pair? (car body)) (symbol? (caar body)))
                 (let ((deno (env-lookup env (caar body))))
                   (cond ((eq? denote-begin deno)
                          (loop (flatten-begin body env) defs macros renames))
                         ((eq? denote-define-syntax deno)
                          (destructuring-match body
                            (((_ (? symbol? org) clause) more ...)
                             (begin
                               (and (core-hashtable-contains? ht-imported-immutables org)
                                    (syntax-violation 'define-syntax "attempt to modify immutable binding" (car body)))
                               (let-values (((code . expr) (compile-macro (car body) clause env)))
                                 (let ((new (generate-global-id library-id org)))
                                   (extend-libenv! org (make-import new))
                                   (cond ((procedure? code)
                                          (extend-env! org (make-macro code env))
                                          (loop more defs (cons (list org 'procedure (car expr)) macros) (acons org new renames)))
                                         ((macro-variable? code)
                                          (extend-env! org (make-macro-variable (cadr code) env))
                                          (loop more defs (cons (list org 'variable (car expr)) macros) (acons org new renames)))
                                         (else
                                          (extend-env! org (make-macro code env))
                                          (loop more defs (cons (list org 'template code) macros) (acons org new renames))))))))
                            (_
                             (syntax-violation 'define-syntax "expected symbol and single expression" (car body)))))
                         ((eq? denote-define deno)
                          (let ((def (annotate (cdr (desugar-define (car body))) (car body))))
                            (and (core-hashtable-contains? ht-imported-immutables (car def))
                                 (syntax-violation 'define "attempt to modify immutable binding" (car body)))
                            (let ((org (car def))
                                  (new (generate-global-id library-id (car def))))
                              (extend-env! org new)
                              (extend-libenv! org (make-import new))
                              (loop (cdr body) (cons def defs) macros (acons org new renames)))))
                         ((eq? denote-define-macro deno)
                          (loop (cons (rewrite-define-macro (car body)) (cdr body)) defs macros renames))
                         ((or (macro? deno)
                              (eq? denote-let-syntax deno)
                              (eq? denote-letrec-syntax deno))
                          (let-values (((expr new) (expand-initial-forms (car body) env)))
                            (set! env new)
                            (loop (append (flatten-begin (list expr) env) (cdr body)) defs macros renames)))
                         (else
                          (loop (cons `(.define ,(expression-tag) ,(car body)) (cdr body)) defs macros renames)))))
                (else
                 (loop (cons `(.define ,(expression-tag) ,(car body)) (cdr body)) defs macros renames)))))))
