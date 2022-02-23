#!nobacktrace
;;; Copyright (c) 2004-2022 Yoshikatsu Fujita / LittleWing Company Limited.
;;; See LICENSE file for terms and conditions of use.

(library (ypsilon c-ffi)
  (export load-shared-object
          c-function
          c-function/weak
          c-callback
          c-main-argc
          c-main-argv
          string->utf8/nul
          bytevector-mapping?
          make-bytevector-mapping
          bytevector->pinned-c-void*)

  (import (core)
          (only (ypsilon c-types) sizeof:int sizeof:long sizeof:size_t sizeof:void*))

  (define c-type-class
    `((void               . #\i)
      (bool               . #\b)
      (char               . #\u)
      (short              . #\d)
      (int                . ,(if (= sizeof:int 4) #\q #\o))
      (long               . ,(if (= sizeof:long 4) #\q #\o))
      (long-long          . #\o)
      (unsigned-short     . #\d)
      (unsigned-int       . ,(if (= sizeof:int 4) #\q #\o))
      (unsigned-long      . ,(if (= sizeof:long 4) #\q #\o))
      (unsigned-long-long . #\o)
      (int8_t             . #\u)
      (int16_t            . #\d)
      (int32_t            . #\q)
      (int64_t            . #\o)
      (uint8_t            . #\u)
      (uint16_t           . #\d)
      (uint32_t           . #\q)
      (uint64_t           . #\o)
      (float              . #\s)
      (double             . #\x)
      (size_t             . ,(if (= sizeof:size_t 4) #\q #\o))
      (void*              . ,(if (= sizeof:void* 4) #\q #\o))))

  (define make-signature
    (lambda (types)
      (apply string (map (lambda (type)
             (cond ((assq type c-type-class) => cdr)
                   (else (assertion-violation 'make-signature (format "invalid argument type ~u" type)))))
           types))))

  (define-syntax c-function
    (lambda (x)
      (syntax-case x ()
        ((_ ret name (args ...))
         (let ((signature (make-signature (syntax->datum #'(ret args ...)))))
            #`(codegen-cdecl-callout (lookup-shared-object 'name) #,signature)))
        ((_ ret name (args ...) (proto ...))
         (let ((signature1 (make-signature (syntax->datum #'(ret args ...))))
               (signature2 (make-signature (syntax->datum #'(proto ...)))))
            #`(codegen-cdecl-callout (lookup-shared-object 'name) #,signature1 #,signature2))))))

  (define-syntax c-function/weak
    (syntax-rules ()
      ((_ . args)
       (let ((thunk #f))
         (lambda e
           (cond (thunk (apply thunk e))
                 (else
                   (set! thunk (c-function . args))
                   (apply thunk e))))))))

  (define-syntax c-callback
    (lambda (x)
      (syntax-case x ()
        ((_ ret (args ...) closure)
         (let ((signature (make-signature (syntax->datum #'(ret args ...)))))
            #`(begin (codegen-queue-push! closure) (codegen-cdecl-callback closure #,signature)))))))

  ) ;[end]
