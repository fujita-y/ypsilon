;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2008 Y.FUJITA, LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(define parent-exception-handler (make-parameter #f))

(define raise
  (lambda (c)
    (cond ((current-exception-handler)
           => (lambda (proc)
                (proc c)
                (cond ((parent-exception-handler)
                       => (lambda (proc)
                            (proc (condition (make-non-continuable-violation)
                                             (make-who-condition 'raise)
                                             (make-message-condition "returned from non-continuable exception")
                                             (make-irritants-condition (list c)))))))
                (scheme-error "error in raise: returned from non-continuable exception~%~%irritants:~%~a" (describe-condition #f c)))))
    (scheme-error "error in raise: unhandled exception has occurred~%~%irritants:~%~a" (describe-condition #f c))))

(define raise-continuable
  (lambda (c)
    (cond ((current-exception-handler)
           => (lambda (proc) (proc c)))
          (else
           (scheme-error "error in raise-continuable: unhandled exception has occurred~%~%irritants:~%~a" (describe-condition #f c))))))

(define with-exception-handler
  (lambda (new thunk)
    (let ((parent (current-exception-handler)))
      (parameterize
          ((parent-exception-handler parent)
           (current-exception-handler
            (lambda (condition)
              (parameterize ((current-exception-handler parent))
                (new condition)))))
        (thunk)))))

(define assertion-violation
  (lambda (who message . irritants)
    (raise
     (apply condition
            (filter values
                    (list (make-assertion-violation)
                          (and who (make-who-condition who))
                          (make-message-condition message)
                          (make-irritants-condition irritants)))))))

(define undefined-violation
  (lambda (who . message)
    (raise
     (apply condition
            (filter values
                    (list (make-undefined-violation)
                          (and who (make-who-condition who))
                          (and (pair? message) (make-message-condition (car message)))))))))

(define lexical-violation
  (lambda (who . message)
    (raise
     (apply condition
            (filter values
                    (list (make-lexical-violation)
                          (and who (make-who-condition who))
                          (and (pair? message) (make-message-condition (car message)))))))))

(define syntax-violation
  (lambda (who message form . subform)
    (raise
     (apply condition
            (filter values
                    (list (make-syntax-violation form (and (pair? subform) (car subform)))
                          (if who
                              (make-who-condition who)
                              (cond ((let ((obj (if (wrapped-syntax-object? form) (unwrap-syntax form) form)))
                                       (cond ((identifier? obj) (original-id (syntax-object-expr obj)))
                                             ((and (pair? obj) (identifier? (car obj))) (original-id (syntax-object-expr (car obj))))
                                             (else #f)))
                                     => make-who-condition)
                                    (else #f)))
                          (make-message-condition message)))))))

(define error
  (lambda (who message . irritants)
    (raise
     (apply condition
            (filter values
                    (list (make-error)
                          (and who (make-who-condition who))
                          (make-message-condition message)
                          (make-irritants-condition irritants)))))))

(define implementation-restriction-violation
  (lambda (who message . irritants)
    (raise
     (apply condition
            (filter values
                    (list (make-implementation-restriction-violation)
                          (and who (make-who-condition who))
                          (make-message-condition message)
                          (and (pair? irritants) (make-irritants-condition irritants))))))))

(define undefined/syntax-violation
  (lambda (who message form . subform)
    (raise
     (apply condition
            (filter values
                    (list (make-syntax-violation form (and (pair? subform) (car subform)))
                          (make-undefined-violation)
                          (and who (make-who-condition who))
                          (make-message-condition message)))))))

(define assertion/syntax-violation
  (lambda (who message form . subform)
    (raise
     (apply condition
            (filter values
                    (list (make-syntax-violation form (and (pair? subform) (car subform)))
                          (make-assertion-violation)
                          (and who (make-who-condition who))
                          (make-message-condition message)))))))

#;(define scheme-error
  (lambda args
    (format #t "~!")
    (let ((port (current-error-port)) (proc (current-exception-handler)))
      (cond (proc (raise (apply format args)))
            (else
             (format port "~&~%")
             (apply format port args)
             (format port "~%")
             (display-backtrace)
             (format port "~%[exit]~%")
             (exit #f))))))

(define scheme-error
  (lambda args
    (format #t "~!")
    (let ((port (current-error-port)))
      (format port "~&~%")
      (apply format port args)
      (format port "~%")
      (display-backtrace)
      (format port "~%[exit]~%")
      (exit #f))))

(define raise-i/o-filename-error
  (lambda (who message filename . irritants)
    (raise
     (apply condition
            (filter values
                    (list (make-i/o-filename-error filename)
                          (and who (make-who-condition who))
                          (make-message-condition message)
                          (and (pair? irritants) (make-irritants-condition irritants))))))))

(define raise-i/o-error
  (lambda (who message . irritants)
    (raise
     (apply condition
            (filter values
                    (list (make-i/o-error)
                          (and who (make-who-condition who))
                          (make-message-condition message)
                          (and (pair? irritants) (make-irritants-condition irritants))))))))


(define raise-misc-i/o-error-with-port
  (lambda (constructor who message port . options)
    (raise
     (apply condition
            (filter values
                    (list (apply constructor options)
                          (and who (make-who-condition who))
                          (make-message-condition message)
                          (and port (make-i/o-port-error port))
                          (make-irritants-condition (cons* port options))))))))

(define raise-misc-i/o-error
  (lambda (constructor who message . options)
    (raise
     (apply condition
            (filter values
                    (list (apply constructor options)
                          (and who (make-who-condition who))
                          (make-message-condition message)
                          (and (pair? options)
                               (make-irritants-condition options))))))))

(define raise-i/o-read-error
  (lambda (who message port)
    (raise-misc-i/o-error-with-port make-i/o-read-error who message port)))

(define raise-i/o-write-error
  (lambda (who message port)
    (raise-misc-i/o-error-with-port make-i/o-write-error who message port)))

(define raise-i/o-file-protection-error
  (lambda (who message filename)
    (raise-misc-i/o-error make-i/o-file-protection-error who message filename)))

(define raise-i/o-file-is-read-only-error
  (lambda (who message port)
    (raise-misc-i/o-error-with-port make-i/o-file-is-read-only-error who message port)))

(define raise-i/o-file-already-exists-error
  (lambda (who message filename)
    (raise-misc-i/o-error make-i/o-file-already-exists-error who message filename)))

(define raise-i/o-file-does-not-exist-error
  (lambda (who message filename)
    (raise-misc-i/o-error make-i/o-file-does-not-exist-error who message filename)))

(define raise-i/o-invalid-position-error
  (lambda (who message port position)
    (raise-misc-i/o-error-with-port make-i/o-invalid-position-error who message port position)))

(define raise-i/o-decoding-error
  (lambda (who message port)
    (raise-misc-i/o-error make-i/o-decoding-error who message port)))

(define raise-i/o-encoding-error
  (lambda (who message port char)
    (raise-misc-i/o-error make-i/o-encoding-error who message port char)))
