#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2008 Y.FUJITA, LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ffi)

  (export c-function c-argument
          on-windows on-darwin on-linux on-posix)

  (import (core primitives)
          (core syntax-case)
          (core bytevectors)
          (core lists))

  (define on-windows (and (string-contains (architecture-feature 'operating-system) "windows") #t))
  (define on-darwin  (and (string-contains (architecture-feature 'operating-system) "darwin")  #t))
  (define on-linux   (and (string-contains (architecture-feature 'operating-system) "linux")   #t))
  (define on-posix   (not on-windows))
  
  (define assert-int
    (lambda (name n i)
      (cond ((and (integer? i) (exact? i)) i)
            (else
             (assertion-violation name (format "expected exact integer, but got ~r, as argument ~s" i n))))))

  (define assert-float
    (lambda (name n f)
      (cond ((flonum? f) (flonum->float f))
            (else
             (assertion-violation name (format "expected flonum, but got ~r, as argument ~s" f n))))))

  (define assert-double
    (lambda (name n f)
      (cond ((flonum? f) f)
            (else
             (assertion-violation name (format "expected flonum, but got ~r, as argument ~s" f n))))))

  (define assert-string
    (lambda (name n s)
      (cond ((string? s) s)
            (else
             (assertion-violation name (format "expected string, but got ~r, as argument ~s" s n))))))

  (define assert-bytevector
    (lambda (name n b)
      (cond ((bytevector? b) b)
            (else
             (assertion-violation name (format "expected bytevector, but got ~r, as argument ~s" b n))))))

  (define assert-closure
    (lambda (name n p)
      (cond ((procedure? p) p)
            (else
             (assertion-violation name (format "expected procedure, but got ~r, as argument ~s" p n))))))

  (define assert-int-vector
    (lambda (name n vect)
      (or (vector? vect)
          (assertion-violation name (format "expected vector, but got ~r, as argument ~s" vect n)))
      (let ((lst (vector->list vect)))
        (for-each (lambda (i)
                    (or (and (integer? i) (exact? i))
                        (assertion-violation name (format "expected list of exact integer, but got ~r, as argument ~s" vect n))))
                  lst)
        lst)))

  (define assert-string-vector
    (lambda (name n vect)
      (or (vector? vect)
          (assertion-violation name (format "expected vector, but got ~r, as argument ~s" vect n)))
      (let ((lst (vector->list vect)))
        (for-each (lambda (s)
                    (or (string? s)
                        (assertion-violation name (format "expected vector of string, but got ~r, as argument ~s" vect n))))
                  lst)
        lst)))

  (define make-binary-array-of-int
    (lambda argv
      (let ((step (architecture-feature 'alignof:int))
            (proc (case (architecture-feature 'sizeof:int)
                    ((4) bytevector-s32-native-set!)
                    ((8) bytevector-s64-native-set!)
                    (else
                     (syntax-violation 'make-binary-array-of-int "byte size of int not defined")))))
        (let ((bv (make-bytevector (* step (length argv)))))
          (let loop ((offset 0) (arg argv))
            (cond ((null? arg) bv)
                  (else
                   (let ((value (car arg)))
                     (proc bv offset value)
                     (loop (+ offset step) (cdr arg))))))))))

  (define make-binary-array-of-char*
    (lambda (ref . argv)
      (apply vector
             ref
             (map (lambda (value) (string->cstring value)) argv))))

  (define-syntax c-callback-arguments
    (lambda (x)
      (syntax-case x ()
        ((_ args ...)
         (let ((lst (syntax->datum (syntax (args ...)))))
           (if (for-all (lambda (arg) (memq arg '(int void*))) lst)
               (datum->syntax #'k (length lst))
               (syntax-violation 'c-callback "expected list of int or void* for argument" x)))))))

  (define-syntax c-argument
    (syntax-rules (int void* char* byte* double float c-callback __stdcall)
      ((_ name n int var)
       (assert-int 'name n var))
      ((_ name n void* var)
       (assert-int 'name n var))
      ((_ name n float var)
       (assert-float 'name n var))
      ((_ name n double var)
       (assert-double 'name n var))
      ((_ name n byte* var)
       (assert-bytevector 'name n var))
      ((_ name n char* var)
       (string->cstring (assert-string 'name n var)))
      ((_ name n [int] var)
       (apply make-binary-array-of-int (assert-int-vector 'name n var)))
      ((_ name n [char*] var)
       (apply make-binary-array-of-char* 0 (assert-string-vector 'name n var)))
      ((_ name n (*[char*]) var)
       (apply make-binary-array-of-char* 1 (assert-string-vector 'name n var)))
      ((_ name n [c-callback void (args ...)] var)
       (make-callback 0 (c-callback-arguments args ...) (assert-closure 'name n var)))
      ((_ name n [c-callback int (args ...)] var)
       (make-callback 0 (c-callback-arguments args ...) (assert-closure 'name n var)))
      ((_ name n [c-callback void __stdcall (args ...)] var)
       (make-callback 1 (c-callback-arguments args ...) (assert-closure 'name n var)))
      ((_ name n [c-callback int __stdcall (args ...)] var)
       (make-callback 1 (c-callback-arguments args ...) (assert-closure 'name n var)))))

  (define-syntax c-function-stub
    (lambda (x)
      (syntax-case x ()
        ((_ lib-handle lib-name stub func-name types ...)
         (with-syntax (((args ...) (generate-temporaries (syntax (types ...))))
                       ((n ...) (map (lambda (e) (datum->syntax #'k e)) (iota (length (syntax (types ...))) 1))))
           (syntax (let ((loc (lookup-shared-object lib-handle 'func-name)))
                     (if loc
                         (let () (define func-name 
                                   (lambda (args ...)
                                     (stub loc (c-argument func-name n types args) ...))) func-name)
                         (let () (define func-name 
                                   (lambda x 
                                     (error 'func-name (format "function not available in ~a" lib-name)))) func-name)))))))))

  (define-syntax c-function
    (syntax-rules (__stdcall void int double void*)
      ((_ lib-handle lib-name void __stdcall func-name (types ...))
       (c-function-stub lib-handle lib-name stdcall-shared-object->void func-name types ...))
      ((_ lib-handle lib-name int __stdcall func-name (types ...))
       (c-function-stub lib-handle lib-name stdcall-shared-object->int func-name types ...))
      ((_ lib-handle lib-name double __stdcall func-name (types ...))
       (c-function-stub lib-handle lib-name stdcall-shared-object->double func-name types ...))
      ((_ lib-handle lib-name void* __stdcall func-name (types ...))
       (c-function-stub lib-handle lib-name stdcall-shared-object->intptr func-name types ...))
      ((_ lib-handle lib-name void func-name (types ...))
       (c-function-stub lib-handle lib-name call-shared-object->void func-name types ...))
      ((_ lib-handle lib-name int func-name (types ...))
       (c-function-stub lib-handle lib-name call-shared-object->int func-name types ...))
      ((_ lib-handle lib-name double func-name (types ...))
       (c-function-stub lib-handle lib-name call-shared-object->double func-name types ...))
      ((_ lib-handle lib-name void* func-name (types ...))
       (c-function-stub lib-handle lib-name call-shared-object->intptr func-name types ...))))

  ) ;[end]
