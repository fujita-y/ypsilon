;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2008 Y.FUJITA, LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(define formals->list
  (lambda (lst)
    (cond ((null? lst) lst)
          ((pair? lst)
           (cons (car lst) (formals->list (cdr lst))))
          (else
           (list lst)))))

(define collect-lambda-formals
  (lambda (formals form)
    (let ((vars (formals->list formals)))
      (or (every1 symbol? vars)
          (syntax-violation (car form) "malformed formals" form formals))
      (or (unique-id-list? vars)
          (syntax-violation (car form) "duplicate formals" form formals))
      vars)))

(define rename-lambda-formals
  (lambda (formals renames)
    (cond ((null? formals) formals)
          ((pair? formals)
           (cons (cdr (assq (car formals) renames))
                 (rename-lambda-formals (cdr formals) renames)))
          (else
           (cdr (assq formals renames))))))

(define check-let*-bindings
  (lambda (form bindings)
    (or (list? bindings)
        (syntax-violation (car form) "malformed bindings" form bindings))
    (for-each (lambda (binding)
                (or (and (= (safe-length binding) 2) (symbol? (car binding)))
                    (syntax-violation (car form) "expected each binding consist of symbol and expression" form binding)))
              bindings)))

(define check-let-bindings
  (lambda (form bindings)
    (or (list? bindings)
        (syntax-violation (car form) "malformed bindings" form bindings))
    (or (unique-id-list?
         (map (lambda (binding)
                (or (and (= (safe-length binding) 2) (symbol? (car binding)) (car binding))
                    (syntax-violation (car form) "expected each binding consist of symbol and expression" form binding)))
              bindings))
        (syntax-violation (car form) "duplicate bindings" form))))

(define check-internal-def-contract-violation
  (lambda (bindings lst)

    (define filter-unique-ids
      (lambda (lst)
        (let loop ((obj lst) (acc '()))
          (cond ((pair? obj)
                 (loop (cdr obj)
                       (loop (car obj) acc)))
                ((symbol? obj)
                 (cond ((memq obj acc) acc)
                       (else (cons obj acc))))
                (else acc)))))

    (define collect-ids
      (lambda (lst)
        (filter-unique-ids
         (let loop ((obj lst))
           (cond ((symbol? obj) obj)
                 ((and (pair? obj)
                       (not (eq? (car obj) 'quote)))
                  (map loop obj))
                 (else '()))))))

    (let ((ids (collect-ids lst)))
      (any1 (lambda (id) (and (memq id ids) id)) bindings))))

(define check-rec-contract-violation
  (lambda (bindings lst)

    (define filter-unique-ids
      (lambda (lst)
        (let loop ((obj lst) (acc '()))
          (cond ((pair? obj)
                 (loop (cdr obj)
                       (loop (car obj) acc)))
                ((symbol? obj)
                 (cond ((memq obj acc) acc)
                       (else (cons obj acc))))
                (else acc)))))

    (define collect-ids
      (lambda (lst)
        (filter-unique-ids
         (let loop ((obj lst))
           (cond ((symbol? obj) obj)
                 ((and (pair? obj)
                       (not (memq (car obj) '(lambda quote))))
                  (map loop obj))
                 (else '()))))))

    (let ((ids (collect-ids lst)))
      (any1 (lambda (id) (memq id ids)) bindings))))

(define check-rec*-contract-violation
  (lambda (vars forms)
    (let loop ((vars vars) (forms forms))
      (and (pair? vars)
           (or (check-rec-contract-violation vars (car forms))
               (loop (cdr vars) (cdr forms)))))))

(define rewrite-letrec*-bindings
  (lambda (form env)
    (let-values (((front back)
                  (partition (lambda (binding)
                               (cond ((not (pair? (cadr binding))))
                                     ((denote-quote? env (caadr binding)))
                                     ((denote-lambda? env (caadr binding))
                                      (begin (set-closure-comment! (cadr binding) (original-id (car binding))) #t))
                                     (else #f)))
                             form)))
      (append front back))))

(define flatten-begin
  (lambda (form env)

    (define concatenate?
      (lambda (lst)
        (and (pair? (car lst))
             (or (list? (car lst))
                 (syntax-violation #f "expression is not a proper list" (car lst)))
             (denote-begin? env (caar lst)))))

    (annotate
     (let loop ((lst form) (ans '()))
       (cond ((null? lst) ans)
             ((concatenate? lst)
              (loop (cdar lst)
                    (loop (cdr lst) ans)))
             (else
              (cond ((null? ans) lst)
                    (else
                     (append lst ans))))))
     form)))

(define flatten-top-level-begin
  (lambda (form env)

    (define concatenate?
      (lambda (lst)
        (and (pair? (car lst))
             (or (list? (car lst))
                 (syntax-violation #f "expression is not a proper list" (car lst)))
             (denote-begin? env (caar lst)))))

    (annotate
     (let loop ((lst form) (ans '()))
       (cond ((null? lst) ans)
             ((concatenate? lst)
              (loop (cdar lst)
                    (loop (cdr lst) ans)))
             (else
              (cons (car lst)
                    (loop (cdr lst) ans)))))
     form)))

(define compile-macro
  (lambda (form transformer env)

    (define compile-transformer
      (lambda (transformer env)
        (let ((out-of-context
               (let ((ht (make-core-hashtable)) (deno (make-out-of-context #f)))
                 (for-each (lambda (e)
                             (and (symbol? (car e))
                                  (renamed-id? (cdr e))
                                  (or (core-hashtable-ref ht (car e) #f)
                                      (core-hashtable-set! ht (car e) deno))))
                           env)
                 (core-hashtable->alist ht))))
          (let ((expr `(.transformer-thunk ,(expand-form transformer (extend-env out-of-context env)))))
            (let ((proc (interpret-coreform expr)))
              (cond ((procedure? proc) (values proc expr))
                    ((variable-transformer-token? proc)
                     (values (make-macro-variable (tuple-ref proc 1) env) expr))
                    (else
                     (syntax-violation (car form) "invalid transformer expression" form transformer))))))))

    (define syntax-rules?
      (lambda (id)
        (denote-syntax-rules? env id)))
    
    (destructuring-match transformer
      (((? syntax-rules? _))
       (syntax-violation 'syntax-rules "expected literals and rules" transformer))
      (((? syntax-rules? _) lites clauses ...)
       (begin
         (or (and (list? lites) (every1 symbol? lites))
             (syntax-violation 'syntax-rules "invalid literals" transformer lites))
         (or (unique-id-list? lites)
             (syntax-violation 'syntax-rules "duplicate literals" transformer lites))
         (and (memq '_ lites)
              (syntax-violation 'syntax-rules "_ in literals" transformer lites))
         (and (memq '... lites)
              (syntax-violation 'syntax-rules "... in literals" transformer lites))
         (for-each (lambda (clause)
                     (destructuring-match clause
                       ((((? symbol? _) . _) _) #t)
                       (((_ . _) _)
                        (syntax-violation 'syntax-rules "expected identifer for first subform of pattern" transformer clause))
                       ((_ _)
                        (syntax-violation 'syntax-rules "expected list for pattern" transformer clause))
                       (_
                        (syntax-violation 'syntax-rules "expected (pattern template) for each rule" transformer clause))))
                   clauses)
         (compile-syntax-rules transformer lites clauses env)))
      (_
       (compile-transformer transformer env)))))

(define expand-let-syntax-bindings
  (lambda (form bindings env)
    (let ((vars (map car bindings))
          (specs (map cadr bindings)))
      (extend-env
       (let ((macros (map (lambda (spec)
                            (let-values (((code . _) (compile-macro form spec env)))
                              (cond ((macro-variable? code) code)
                                    (else (make-macro code env)))))
                          specs)))
         (append (map (lambda (macro var) (cons macro (generate-local-macro-symbol var))) macros vars)
                 (map cons vars macros)))
       env))))

(define expand-letrec-syntax-bindings
  (lambda (form bindings env)

    (define undefined-macro
      (lambda (x)
        (syntax-violation (car form) "attempt to reference unbound syntactic keyword" form x)))

    (let ((vars (map car bindings))
          (specs (map cadr bindings)))
      (let ((unbound-macros (map (lambda (spec) (make-macro undefined-macro '())) specs)))
        (let ((env (extend-env
                    (append (map (lambda (unbound-macro var) (cons unbound-macro (generate-local-macro-symbol var))) unbound-macros vars)
                            (map cons vars unbound-macros))
                    env)))
          (let ((macros (map (lambda (spec)
                               (let-values (((code . _) (compile-macro form spec env)))
                                 (cond ((macro-variable? code) code)
                                       (else (make-macro code env)))))
                             specs)))
            (for-each (lambda (var macro) (set-cdr! (env-lookup env var) (cdr macro)))
                      vars macros))
          env)))))

(define expand-macro-use
  (lambda (form env deno)
    (fresh-rename-count)
    (current-macro-expression form)
    (parameterize ((current-expansion-environment env) (current-transformer-environment (cddr deno)))
      (if (procedure? (cadr deno))
          ((cadr deno) form)
          (transcribe-syntax-rules form (cadr deno))))))

(define expand-initial-forms
  (lambda (form env)

    (define rewrite-form-each
      (lambda (form alist)
        (let loop ((lst form))
          (cond ((null? lst) '())
                (else
                 (let ((na (rewrite-form (car lst) alist))
                       (nd (loop (cdr lst))))
                   (if (and (eq? na (car lst)) (eq? nd (cdr lst)))
                       lst
                       (cons na nd))))))))

    (define rewrite-form
      (lambda (form alist)
        (cond ((null? form) '())
              ((symbol? form)
               (cond ((assq form alist) => cdr)
                     (else form)))
              ((list? form)
               (annotate (rewrite-form-each form alist) form))
              ((vector? form)
               (list->vector (rewrite-form-each (vector->list form) alist)))
              (else form))))

    (define rewrite-let-syntax
      (lambda (form env)
        (destructuring-match form
          ((_ bindings body ...)
           (begin
             (check-let-bindings form bindings)
             (let ((vars (map car bindings))
                   (specs (map cadr bindings))
                   (suffix (fresh-rename-count)))
               (let ((renamed (map (lambda (id) (rename-id id suffix)) vars)))
                 (let ((env (expand-let-syntax-bindings form (map list renamed specs) env)))
                   (values `(begin ,@(rewrite-form body (map cons vars renamed))) env))))))
          (_
           (syntax-violation (car form) "missing clause" form)))))

    (define rewrite-letrec-syntax
      (lambda (form env)
        (destructuring-match form
          ((_ (? list? bindings) body ...)
           (begin
             (check-let-bindings form bindings)
             (let ((vars (map car bindings))
                   (specs (map cadr bindings))
                   (suffix (fresh-rename-count)))
               (let ((renames (map (lambda (id) (cons id (rename-id id suffix))) vars)))
                 (let ((env (expand-letrec-syntax-bindings form (rewrite-form bindings renames) env)))
                   (values `(begin ,@(rewrite-form body renames)) env))))))
          (_ (syntax-violation (car form) "missing clause" form)))))

    (cond ((and (pair? form) (symbol? (car form)))
           (let ((deno (env-lookup env (car form))))
             (cond ((eq? deno denote-let-syntax)
                    (rewrite-let-syntax form env))
                   ((eq? deno denote-letrec-syntax)
                    (rewrite-letrec-syntax form env))
                   ((macro? deno)
                    (let-values (((expr renames) (expand-macro-use form env deno)))
                      (values expr (extend-env renames env))))
                   (else
                    (values form env)))))
          (else
           (values form env)))))

(define expand-body
  (lambda (form form-body env)

    (define macro-defs '())
    (define macro-exprs '())
    (define list-of-lhs '())

    (define extend-env!
      (lambda (datum1 datum2)
        (set! env (extend-env (list (cons datum1 datum2)) env))
        (and (macro? datum2)
             (begin
               (set! macro-defs (acons datum1 datum2 macro-defs))
               (set! env (extend-env (list (cons datum2 (generate-local-macro-symbol datum1))) env)))) ; 071204 new
        (for-each (lambda (a) (set-cdr! (cddr a) env)) macro-defs)))

    (define internal-definition?
      (lambda (lst)
        (and (pair? lst)
             (pair? (car lst))
             (symbol? (caar lst))
             (let ((deno (env-lookup env (caar lst))))
               (or (macro? deno)
                   (eq? denote-define deno)
                   (eq? denote-define-syntax deno)
                   (eq? denote-let-syntax deno)
                   (eq? denote-letrec-syntax deno))))))

    (define rewrite-body
      (lambda (body defs renames)
        (cond ((null? body) '())
              ((null? defs)
               (annotate (expand-each body env) form-body))
              (else
               (or (unique-id-list? (map car renames))
                   (let ((id (find-duplicates (map car renames))))
                     (syntax-violation 'define
                                       "duplicate definitions"
                                       (let ((e (assq id defs))) (annotate `(define ,@e) e))
                                       (let ((e (assq id (reverse defs)))) (annotate `(define ,@e) e)))))
               (let ((lhs (map (lambda (def) (cdr (assq (car def) renames))) defs))
                     (rhs (map (lambda (def) (expand-form (cadr def) env)) defs)))
                 (cond ((check-rec*-contract-violation lhs rhs)
                        => (lambda (var)
                             (let ((id (any1 (lambda (a) (and (eq? (cdr a) (car var)) (car a))) renames)))
                               (current-macro-expression form)
                               (syntax-violation 'define
                                                 (format "attempt to reference unbound variable ~u" id)
                                                 (any1 (lambda (e)
                                                         (and (check-rec-contract-violation (list id) (cdr e))
                                                              (annotate `(define ,@e) e)))
                                                       defs)))))
                       ((check-internal-def-contract-violation (map car defs) macro-exprs)
                        => (lambda (id)
                             (current-macro-expression form)
                             (syntax-violation 'define
                                               (format "identifier ~u already used to determine the meaning of undeferred portions of definition" id)
                                               (cond ((assq id defs) => (lambda (e) (annotate `(define ,@e) e)))
                                                     (else #f)))))
                       (else
                        (annotate
                         `((letrec* ,(rewrite-letrec*-bindings (map list lhs rhs) env) ,@(expand-each body env)))
                         form-body))))))))
                     
    (let ((suffix (fresh-rename-count)))
      (let loop ((body (flatten-begin form-body env)) (defs '()) (renames '()))
        (cond ((null? body) body)
              ((and (pair? (car body)) (symbol? (caar body)))         
               (set! list-of-lhs (cons (caar body) list-of-lhs))          
               (let ((deno (env-lookup env (caar body))))
                 (cond ((eq? denote-begin deno)
                        (loop (flatten-begin body env) defs renames))
                       ((eq? denote-define-syntax deno)
                        (destructuring-match body
                          (((_ (? symbol? name) clause) more ...)
                           (begin
                             (let-values (((code . expr) (compile-macro (car body) clause env)))
                               (and (pair? expr) (set! macro-exprs (cons (car expr) macro-exprs)))
                               (cond ((macro-variable? code)
                                      (extend-env! name code))
                                     (else
                                      (extend-env! name (make-macro code env)))))
                             (loop more defs (acons name #f renames))))
                          (((_ _ _) _ ...)
                           (syntax-violation (caar body) "expected symbol for first clause" (car body)))
                          (_
                           (syntax-violation (caar body) "expected symbol and transformer expression" (car body)))))
                       ((eq? denote-define deno)
                        (let ((def (annotate (cdr (desugar-define (car body))) (car body))))
                          (let ((org (car def)) (new (rename-id (car def) suffix)))
                            (cond ((memq org list-of-lhs)
                                   => (lambda (e)
                                        (current-macro-expression form)
                                        (syntax-violation 'define
                                               (format "identifier ~u already used to determine the meaning of undeferred portions of definition" org)
                                               (car body)))))       
                            (extend-env! org new)
                            (loop (cdr body) (cons def defs) (acons org new renames)))))
                       ((or (macro? deno)
                            (eq? denote-let-syntax deno)
                            (eq? denote-letrec-syntax deno))
                        (let-values (((expr new) (expand-initial-forms (car body) env)))
                          (set! env new)
                          (let ((maybe-def (flatten-begin (list expr) env)))
                            (cond ((null? maybe-def)
                                   (loop (cdr body) defs renames))
                                  ((internal-definition? maybe-def)
                                   (loop (append maybe-def (cdr body)) defs renames))
                                  (else
                                   (rewrite-body (append maybe-def (cdr body)) (reverse defs) renames))))))
                       (else
                        (rewrite-body body (reverse defs) renames)))))
              (else
               (rewrite-body body (reverse defs) renames)))))))

(define expand-each
  (lambda (forms env)
    (let ((expr (current-macro-expression)))
      (annotate (map (lambda (form)
                       (current-macro-expression expr)
                       (expand-form form env))
                     forms)
                forms))))

(define expand-form
  (lambda (form env)
    (cond ((symbol? form)
           (let ((deno (env-lookup env form)))
             (cond ((symbol? deno) deno)
                   ((macro? deno)
                    (let-values (((expr renames)
                                  (if (< (expansion-trace-level) (expansion-backtrace))
                                      (parameterize ((expansion-trace-stack (cons (current-macro-expression) (expansion-trace-stack)))
                                                     (expansion-trace-level (+ 1 (expansion-trace-level))))
                                        (expand-macro-use form env deno))
                                      (expand-macro-use form env deno))))
                      (expand-form expr (extend-env renames env))))
                   ((unbound? deno)
                    (syntax-violation #f
                                      (format "attempt to reference unbound variable ~u" form)
                                      (current-macro-expression)))
                   ((out-of-context? deno)
                    (if (cdr deno)
                        (syntax-violation #f
                                          (format "identifer ~u out of context" form)
                                          (current-macro-expression)
                                          (annotate (list 'syntax (cdr deno)) (cdr deno)))
                        (syntax-violation #f
                                          (format "identifer ~u out of context" form)
                                          (current-macro-expression))))
                   ((special? deno)
                    (syntax-violation #f (format "misplaced syntactic keyword ~u" form) (current-macro-expression)))
                   ((pattern-variable? deno)
                    (syntax-violation #f (format "misplaced pattern variable ~u" form) (current-macro-expression)))
                   (else form))))
          ((list? form)
           (and (null? form) (syntax-violation #f "invalid expression" form))
           (current-macro-expression form)
           (cond ((symbol? (car form))
                  (let ((deno (env-lookup env (car form))))
                    (cond ((special? deno)
                           (if (or (unexpect-top-level-form)
                                   (eq? denote-begin deno)
                                   (eq? denote-define deno)
                                   (eq? denote-import deno)
                                   (eq? denote-define-macro deno)
                                   (eq? denote-define-syntax deno)
                                   (eq? denote-let-syntax deno)
                                   (eq? denote-letrec-syntax deno))
                               ((cdr deno) form env)
                               (parameterize ((unexpect-top-level-form #t))
                                 ((cdr deno) form env))))
                          ((macro? deno)
                           (let-values (((expr renames) (expand-macro-use form env deno)))
                             (if (< (expansion-trace-level) (expansion-backtrace))
                                 (parameterize ((expansion-trace-stack (cons form (expansion-trace-stack)))
                                                (expansion-trace-level (+ 1 (expansion-trace-level))))
                                   (expand-form expr (extend-env renames env)))
                                 (expand-form expr (extend-env renames env)))))
                          (else
                           (if (unexpect-top-level-form)
                               (expand-each form env)
                               (parameterize ((unexpect-top-level-form #t))
                                 (expand-each form env)))))))                        
                 (else
                  (parameterize ((unexpect-top-level-form #t))
                    (expand-each form env)))))
          ((pair? form)
           (syntax-violation #f "expression is not a proper list" form))
          ((or (boolean? form) (number? form) (char? form) (string? form) (bytevector? form)) form)
          (else 
           (syntax-violation #f "invalid expression" form)))))

(define macro-expand
  (lambda (form . mode)
    (parameterize ((current-temporary-count 0)
                   (current-rename-count 0)
                   (current-macro-expression #f)
                   (current-expansion-mode mode)
                   (current-top-level-exterior #f)
                   (unexpect-top-level-form #f)
                   (expansion-trace-level 0)
                   (expansion-trace-stack '()))
      (let ((form (if (denote-begin? '() 'begin)
                      (flatten-top-level-begin `(begin ,form) private-primitives-environment)
                      form)))
        (if (null? (cdr form))
            (unspecified)
            ((current-after-expansion-hook) (expand-form form private-primitives-environment) annotate annotate-closure))))))
