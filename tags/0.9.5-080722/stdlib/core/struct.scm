#!core
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2008 Y.FUJITA, LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (core struct)

  (export define-struct)

  (import (core primitives)
          (core syntax-case)
          (core lists))

  (define-syntax define-struct
    (lambda (x)
      (syntax-case x ()
        ((?_ ?struct-name ?field-names)
         (let ((struct-name (syntax->datum #'?struct-name))
               (field-names (syntax->datum #'?field-names)))
           (or (symbol? struct-name)
               (syntax-violation 'define-struct "expected symbol for struct name" x struct-name))
           (let loop ((names field-names))
             (and (pair? names)
                  (if (symbol? (car names))
                      (if (memq (car names) (cdr names))
                          (syntax-violation 'define-struct "duplicate field name" x field-names)
                          (loop (cdr names)))
                      (syntax-violation 'define-struct "expected symbol for field name" x field-names))))
           (let ((make-name (string->symbol (format "make-~a" struct-name)))
                 (pred-name (string->symbol (format "~a?" struct-name)))
                 (desc-name (string->symbol (format "type:~a" struct-name)))
                 (field-count (length field-names)))
             (let ((field-refs (iota field-count 1)))
               (with-syntax
                   ((?make-name
                     (datum->syntax #'?_ make-name))
                    (?pred-name
                     (datum->syntax #'?_ pred-name))
                    (?make-rules
                     (datum->syntax #'k `(syntax-rules ()
                                           ((_ ,@field-names) (tuple ',desc-name ,@field-names))
                                           ((_) (tuple ',desc-name ,@(make-list field-count '(unspecified)))))))
                    (?pred-rules
                     (datum->syntax #'k `(syntax-rules ()
                                           ((_ obj) (and (tuple? obj) (eq? (tuple-ref obj 0) ',desc-name))))))
                    ((?getter-name ...)
                     (map (lambda (field-name)
                            (datum->syntax #'?_ (string->symbol (format "~a-~a" struct-name field-name))))
                          field-names))
                    ((?setter-name ...)
                     (map (lambda (field-name)
                            (datum->syntax #'?_ (string->symbol (format "~a-~a-set!" struct-name field-name))))
                          field-names))
                    ((?getter-rules ...)
                     (map (lambda (field-ref)
                            (datum->syntax #'k `(syntax-rules () ((_ obj) (tuple-ref obj ,field-ref)))))
                          field-refs))
                    ((?setter-rules ...)
                     (map (lambda (field-ref)
                            (datum->syntax #'k `(syntax-rules () ((_ obj value) (tuple-set! obj ,field-ref value)))))
                          field-refs)))
                 (syntax (begin
                           (define-syntax ?make-name ?make-rules)
                           (define-syntax ?pred-name ?pred-rules)
                           (define-syntax ?getter-name ?getter-rules) ...
                           (define-syntax ?setter-name ?setter-rules) ...))))))))))

  ) ;[end]


