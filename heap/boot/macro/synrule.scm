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
                   (syntax-violation
                     (car form)
                     "invalid syntax"
                     form
                     (put-annotation (compiled->source ellipsis lites rules) remark))
                   (syntax-violation (car form) "invalid syntax" form))
               (let* ((rule (car rules))
                      (pattern (car rule))
                      (vars
                        (and (if (and (pair? pattern) (eq? (car pattern) '_))
                                 (match-pattern? (cdr form) (cdr pattern) lites)
                                 (match-pattern? form pattern lites))
                             (bind-pattern form pattern lites '()))))
                 (if vars (transcribe-compiled-templete (cdr rule) vars) (loop (cdr rules)))))))))))

(define parse-syntax-rule
  (lambda (lites clause env)
    (let ((pattern (car clause)) (template (cadr clause)))
      (check-pattern pattern lites)
      (let ((ranks (collect-vars-ranks pattern lites 0 '())))
        (check-template template ranks)
        (values pattern template ranks (collect-rename-ids template ranks))))))

(define compile-syntax-rules
  (lambda (form ellipsis lites clauses env)
    (define make-remark
      (lambda (form)
        (cond ((get-annotation form)
               =>
               (lambda (comment) (cons (core-hashtable-ref (current-source-comments) '|.&SOURCE-PATH| #f) comment)))
              (else #f))))
    (parameterize ((ellipsis-id ellipsis))
      (let ((lites (unrename-syntax lites env)) (clauses (unrename-syntax clauses env)))
        (cons*
          (ellipsis-id)
          lites
          (make-remark form)
          (map (lambda (clause)
                 (let-values (((pattern template ranks renames) (parse-syntax-rule lites clause env)))
                   (list pattern template ranks renames)))
               clauses))))))
