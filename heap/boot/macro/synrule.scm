;;; Copyright (c) 2004-2022 Yoshikatsu Fujita / LittleWing Company Limited.
;;; See LICENSE file for terms and conditions of use.

(define transcribe-syntax-rules
  (lambda (form spec)
    (define transcribe-compiled-templete
      (lambda (spec vars)
        (let ((template (car spec))
              (ranks (cadr spec))
              (renames (caddr spec))
              (env (current-transformer-environment))
              (suffix (current-rename-count)))
          (let ((aliases
                  (let loop ((lst renames) (ans '()))
                    (if (null? lst) ans (let ((id (car lst))) (loop (cdr lst) (acons id (rename-id id suffix) ans)))))))
            (values
              (transcribe-template template ranks vars aliases #f)
              (map (lambda (lst) (cons (cdr lst) (env-lookup env (car lst)))) aliases))))))
    (define compiled->source
      (lambda (ellipsis lites lst)
        (if (eq? ellipsis '...)
            `(syntax-rules ,lites ,@(map (lambda (e) `(,(car e) ,(cadr e))) lst))
            `(syntax-rules ,ellipsis ,lites ,@(map (lambda (e) `(,(car e) ,(cadr e))) lst)))))
    (or (pair? form) (syntax-violation form "misplaced syntactic keyword" form))
    (destructuring-match spec
      ((ellipsis lites remark rules ...)
       (parameterize ((ellipsis-id ellipsis))
         (let loop ((rules rules))
           (if (null? rules)
               (if remark
                   (syntax-violation (car form) "invalid syntax" form (put-annotation (compiled->source ellipsis lites rules) remark))
                   (syntax-violation (car form) "invalid syntax" form))
               (let* ((rule (car rules))
                      (pattern (car rule))
                      (vars
                        (and (if (and (pair? pattern) (eq? (car pattern) '_))
                                 (match-pattern? (cdr form) (cdr pattern) lites)
                                 (match-pattern? form pattern lites))
                             (bind-pattern form pattern lites '()))))
                 (if vars (transcribe-compiled-templete (cdr rule) vars) (loop (cdr rules)))))))))))

(define compile-syntax-rules
  (lambda (transformer env)
    (define make-remark
      (lambda (form)
        (cond ((get-annotation form)
               => (lambda (comment) (cons (core-hashtable-ref (current-source-comments) '|.&SOURCE-PATH| #f) comment)))
              (else #f))))
    (define parse-syntax-rules
      (lambda (lites clause)
        (let ((pattern (car clause)) (template (cadr clause)))
          (check-pattern (cdr pattern) lites)
          (let ((ranks (collect-vars-ranks pattern lites 0 '())))
            (check-template template ranks)
            (list pattern template ranks (collect-rename-ids template ranks))))))
    (define check-syntax-rules
      (lambda (clauses)
        (for-each
          (lambda (clause)
            (destructuring-match clause
              ((((? symbol? _) . _) _) #t)
              (((_ . _) _) (syntax-violation 'syntax-rules "expected identifer for first subform of pattern" transformer clause))
              ((_ _) (syntax-violation 'syntax-rules "expected list for pattern" transformer caluse))
              (_ (syntax-violation 'syntax-rules "expected (pattern template) for each rule" transformer clause))))
          clauses)))
    (define lookup-default-ellipsis
      (lambda (lites)
        (let ((ellipsis
                (cond ((renamed-variable-id? (env-lookup env '...)) #f)
                      (else
                        (let ((maybe-ellipsis (rename-id '... (current-rename-count))))
                          (if (and (eq? (env-lookup env maybe-ellipsis) denote-...)) maybe-ellipsis '...))))))
          (if (memq ellipsis lites) #f ellipsis))))
    (destructuring-match transformer
      ((_) (syntax-violation 'syntax-rules "expected literals and rules" transformer))
      ((_ body ...)
       (begin
         (let ((ellipsis (and (pair? body) (symbol? (car body)) (car body))))
           (let ((lites (if ellipsis (cadr body) (car body))) (clauses (if ellipsis (cddr body) (cdr body))))
             (or (and (list? lites) (every1 symbol? lites)) (syntax-violation 'syntax-rules "invalid literals" transformer lites))
             (or (unique-id-list? lites) (syntax-violation 'syntax-rules "duplicate literals" transformer lites))
             (or (ellipsis/underscore-in-literal) (and (memq '_ lites) (syntax-violation 'syntax-rules "_ in literals" transformer lites)))
             (check-syntax-rules clauses)
             (let ((ellipsis (or ellipsis (lookup-default-ellipsis lites))))
               (and (symbol? ellipsis)
                    (memq ellipsis lites)
                    (if (ellipsis/underscore-in-literal)
                        (set! ellipsis #f)
                        (syntax-violation 'syntax-rules "ellipsis in literals" transformer lites)))
               (parameterize ((ellipsis-id ellipsis))
                 (let ((lites (unrename-syntax lites env))
                       (clauses (unrename-syntax clauses (extend-env (list (cons ellipsis ellipsis)) env))))
                   (cons*
                     (ellipsis-id)
                     lites
                     (make-remark transformer)
                     (map (lambda (clause) (parse-syntax-rules lites clause)) clauses))))))))))))
