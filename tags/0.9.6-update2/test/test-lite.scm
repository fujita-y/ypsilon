
(library (test-lite)
  (export test-begin test-end test-section-begin test-comment test-report
          test-lexical-exception test-syntax-violation test-assertion-violation test-violation
          test-eval! test-eq test-eqv test-equal  test-equal-evaluated)
  (import (core))

  (define-record-type section
    (fields
     (mutable name)
     (mutable pass-count)
     (mutable fail-count)
     (mutable skip-count)
     (mutable skip-list)
     (mutable on-test)
     (mutable on-final)
     (mutable env)
     (mutable lib)))

  ;;
  ;; test utility
  ;;

  (define test-result (string))

  (define test-report (lambda () (format #t "~%~a~%" test-result) (set! test-result (string)) (unspecified)))

  (define copy-current-environment
    (lambda ()
      (let ((env (make-environment "testing")))
        (let ((variables (map car (core-hashtable->alist (current-variable-environment))))
              (macros (map car (core-hashtable->alist (current-macro-environment)))))
          (copy-environment-variables! (current-environment) env variables)
          (copy-environment-macros! (current-environment) env macros)
          env))))

  (define section-reset
    (lambda (sec)
      (section-name-set! sec "")
      (section-pass-count-set! sec 0)
      (section-fail-count-set! sec 0)
      (section-skip-count-set! sec 0)
      (section-skip-list-set! sec '())
      (section-on-final-set! sec (lambda x #f))
      (section-on-test-set! sec (lambda x #f))
      (section-env-set! sec (copy-current-environment))
      (section-lib-set! sec (let ((ht (make-core-hashtable)))
                              (for-each (lambda (a) (core-hashtable-set! ht (car a) (cdr a))) (core-hashtable->alist (scheme-library-exports)))
                              ht))
      sec))

  (define section-current (make-parameter #f))

  (define section-pass-count-inc!
    (lambda (sec)
      (section-pass-count-set! sec (+ (section-pass-count sec) 1))))

  (define section-fail-count-inc!
    (lambda (sec)
      (section-fail-count-set! sec (+ (section-fail-count sec) 1))))

  (define test-default-on-final-proc
    (lambda (sec)
      (let ((report (format "section ~s passed: ~a failed: ~a skipped: ~a"
                            (section-name sec)
                            (section-pass-count sec)
                            (section-fail-count sec)
                            (section-skip-count sec))))
        (and (> (section-fail-count sec) 0)
             (set! report (string-append report "  ; ### TEST FAILURE ###\n")))
        (set! test-result (string-append test-result report "\n"))
        (cond ((> (section-fail-count sec) 0)
               (newline)
               (exit #f))))))

  (define test-default-on-test-proc
    (lambda (sec test passed? form expect got)
      (cond (passed?
             (section-pass-count-inc! sec)
             (put-byte (current-output-port) #x0d)
             (format #t "Passed ~a~!" (section-pass-count sec)))
            (else
             (section-fail-count-inc! sec)
             (format #t "~&; *** ### TEST FAILURE ### ~%\
                           ; *** section: ~s~%\
                           ; *** name: ~s~%\
                           ; *** expression: ~s~%\
                           ; *** expect: ~s~%\
                           ; *** got: ~s~%~!"
                     (section-name sec) test form expect got)
             (exit #f)))))

  (define test-error-condition
    (lambda (name expr pred expect)
      (let ((sec (section-current)))
        ((lambda (got) ((section-on-test sec)
                        sec name (pred got) expr expect got))
         (guard (c (else c))
                (parameterize ((scheme-library-exports (section-lib (section-current))))
                  (eval expr (section-env (section-current)))))))))

  (define test-expression
    (lambda (name expr expect pred)
      (let ((sec (section-current)))
        ((lambda (got) ((section-on-test sec)
                        sec name (pred expect got) expr expect got))
         (guard (condition
                 (else condition))
                (parameterize ((scheme-library-exports (section-lib (section-current))))
                  (eval expr (section-env (section-current)))))))))

  (define test-begin
    (lambda (name)
      (let ((sec (section-reset (make-section #f #f #f #f #f #f #f #f #f))))
        (section-name-set! sec name)
        (section-on-test-set! sec test-default-on-test-proc)
        (section-on-final-set! sec test-default-on-final-proc)
        (section-current sec)
        (format #t "~&~a~%" name)
        sec)))

  (define test-end
    (lambda ()
      (let ((sec (section-current)))
        ((section-on-final sec) sec)
        (format #t "~%"))))

  (define test-comment
    (lambda (comment)
      (format #t "\rcomment: ~s~%" comment)))

  (define-syntax test-eval!
    (syntax-rules ()
      ((_ expr)
       (parameterize ((scheme-library-exports (section-lib (section-current))))
         (eval 'expr (section-env (section-current)))))))

  (define-syntax test-eq
    (syntax-rules (=>)
      ((_ name expr => value)
       (test-expression name 'expr 'value eq?))
      ((_ expr => value)
       (test-expression "" 'expr 'value eq?))
      ((_ expr value)
       (test-expression "" 'expr 'value eq?))))

  (define-syntax test-eqv
    (syntax-rules (=>)
      ((_ name expr => value)
       (test-expression name 'expr 'value eqv?))
      ((_ expr => value)
       (test-expression "" 'expr 'value eqv?))))

  (define-syntax test-equal
    (syntax-rules (=>)
      ((_ name expr => value)
       (test-expression name 'expr 'value equal?))
      ((_ expr => value)
       (test-expression "" 'expr 'value equal?))
      ((_ expr value)
       (test-expression "" 'expr 'value equal?))))

  (define-syntax test-equal-evaluated
    (syntax-rules (=>)
      ((_ name expr => value)
       (test-expression name
                        'expr
                        (parameterize ((scheme-library-exports (section-lib (section-current))))
                          (eval 'value (section-env (section-current))))
                        equal?))
      ((_ expr => value)
       (test-expression ""
                        'expr
                        (parameterize ((scheme-library-exports (section-lib (section-current))))
                          (eval 'value (section-env (section-current))))
                        equal?))))

  (define-syntax test-violation
    (syntax-rules ()
      ((_ name expr)
       (test-error-condition name 'expr violation? '&violation))
      ((_ expr)
       (test-error-condition "" 'expr violation? '&violation))))

  (define-syntax test-lexical-exception
    (syntax-rules ()
      ((_ name expr)
       (test-error-condition name 'expr lexical-violation? '&lexical))
      ((_ expr)
       (test-error-condition "" 'expr lexical-violation? '&lexical))))

  (define-syntax test-syntax-violation
    (syntax-rules ()
      ((_ name expr)
       (test-error-condition name 'expr syntax-violation? '&syntax))
      ((_ expr)
       (test-error-condition "" 'expr syntax-violation? '&syntax))))

  (define-syntax test-assertion-violation
    (syntax-rules ()
      ((_ name expr)
       (test-error-condition name 'expr assertion-violation? '&assertion))
      ((_ expr)
       (test-error-condition "" 'expr assertion-violation? '&assertion))))
  
  (define-syntax test-section-begin
    (syntax-rules ()
      ((_ name testcases ...)
       (begin
         (test-begin name)
         testcases ...
         (test-end)))))

  )

#|

[ ]*=>[ ]*(.+)$
 => $1)

^(\(.*)$
(test-equal "lib " $0


|#
