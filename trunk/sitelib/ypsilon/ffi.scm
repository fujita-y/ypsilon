#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon ffi)
  (export load-shared-object
          lookup-shared-object
          c-function
          c-function/errno
          c-function/win32-lasterror
          shared-object-errno
          shared-object-win32-lasterror
          win32-error->string
          make-cdecl-callout
          make-cdecl-callback
          make-stdcall-callout
          make-stdcall-callback
          bytevector-mapping?
          make-bytevector-mapping
          define-c-enum
          define-c-typedef
          define-c-struct-type
          define-c-struct-methods
          c-sizeof
          c-coerce-void*
          bytevector-c-bool-ref
          bytevector-c-short-ref
          bytevector-c-int-ref
          bytevector-c-long-ref
          bytevector-c-void*-ref
          bytevector-c-float-ref
          bytevector-c-double-ref
          bytevector-c-unsigned-short-ref
          bytevector-c-unsigned-int-ref
          bytevector-c-unsigned-long-ref
          bytevector-c-bool-set!
          bytevector-c-short-set!
          bytevector-c-int-set!
          bytevector-c-long-set!
          bytevector-c-void*-set!
          bytevector-c-float-set!
          bytevector-c-double-set!
          bytevector-c-int8-ref
          bytevector-c-int16-ref
          bytevector-c-int32-ref
          bytevector-c-int64-ref
          bytevector-c-uint8-ref
          bytevector-c-uint16-ref
          bytevector-c-uint32-ref
          bytevector-c-uint64-ref
          bytevector-c-int8-set!
          bytevector-c-int16-set!
          bytevector-c-int32-set!
          bytevector-c-int64-set!
          bytevector-c-strlen
          make-c-bool
          make-c-short
          make-c-int
          make-c-long
          make-c-void*
          make-c-float
          make-c-double
          make-c-int8
          make-c-int16
          make-c-int32
          make-c-int64
          make-c-string
          c-bool-ref
          c-short-ref
          c-int-ref
          c-long-ref
          c-void*-ref
          c-float-ref
          c-double-ref
          c-unsigned-short-ref
          c-unsigned-int-ref
          c-unsigned-long-ref
          c-int8-ref
          c-int16-ref
          c-int32-ref
          c-int64-ref
          c-uint8-ref
          c-uint16-ref
          c-uint32-ref
          c-uint64-ref
          c-string-ref
          c-bool-set!
          c-short-set!
          c-int-set!
          c-long-set!
          c-void*-set!
          c-float-set!
          c-double-set!
          c-int8-set!
          c-int16-set!
          c-int32-set!
          c-int64-set!
          c-string-set!
          sizeof:bool
          sizeof:short
          sizeof:int
          sizeof:long
          sizeof:void*
          sizeof:size_t
          alignof:bool
          alignof:short
          alignof:int
          alignof:long
          alignof:void*
          alignof:size_t
          alignof:float
          alignof:double
          alignof:int8_t
          alignof:int16_t
          alignof:int32_t
          alignof:int64_t
          on-sunos
          on-darwin
          on-linux
          on-freebsd
          on-openbsd
          on-windows
          on-posix
          on-ppc32
          on-ppc64
          on-ia32
          on-x64)

  (import (core) (ypsilon concurrent) (ypsilon c-types) (ypsilon assert))

  (define on-sunos         (and (string-contains (architecture-feature 'operating-system) "sunos")   #t))
  (define on-darwin        (and (string-contains (architecture-feature 'operating-system) "darwin")  #t))
  (define on-linux         (and (string-contains (architecture-feature 'operating-system) "linux")   #t))
  (define on-freebsd       (and (string-contains (architecture-feature 'operating-system) "freebsd") #t))
  (define on-openbsd       (and (string-contains (architecture-feature 'operating-system) "openbsd") #t))
  (define on-windows       (and (string-contains (architecture-feature 'operating-system) "windows") #t))
  (define on-posix         (not on-windows))
  (define on-ppc32         (and (string-contains (architecture-feature 'machine-hardware) "ppc") (= sizeof:long 4)))
  (define on-ppc64         (and (string-contains (architecture-feature 'machine-hardware) "ppc") (= sizeof:long 8)))
  (define on-x64           (and (or (string-contains (architecture-feature 'machine-hardware) "amd64")
                                    (string-contains (architecture-feature 'machine-hardware) "x86_64")
                                    (and on-sunos
                                         (string-contains (architecture-feature 'machine-hardware) "i86")
                                         (= sizeof:void* 8))) #t))
  (define on-ia32          (not (or on-x64 on-ppc32 on-ppc64)))

  (define expect-string
    (lambda (name n s)
      (cond ((eq? s 0) 0)
            ((string? s) (string->utf8/nul s))
            (else
             (assertion-violation name (format "expected string or 0, but got ~r, as argument ~s" s n))))))

  (define expect-proc
    (lambda (name n p)
      (cond ((procedure? p) p)
            (else
             (assertion-violation name (format "expected procedure, but got ~r, as argument ~s" p n))))))

  (define expect-exact-int-vector
    (lambda (name n vect)
      (or (vector? vect)
          (assertion-violation name (format "expected vector, but got ~r, as argument ~s" vect n)))
      (let ((lst (vector->list vect)))
        (for-each (lambda (i)
                    (unless (and (integer? i) (exact? i))
                      (assertion-violation name (format "expected list of exact integer, but got ~r, as argument ~s" vect n))))
                  lst)
        lst)))

  (define expect-string-vector
    (lambda (name n vect)
      (or (vector? vect)
          (assertion-violation name (format "expected vector, but got ~r, as argument ~s" vect n)))
      (let ((lst (vector->list vect)))
        (for-each (lambda (s)
                    (unless (string? s)
                      (assertion-violation name (format "expected vector of string, but got ~r, as argument ~s" vect n))))
                  lst)
        lst)))

  (define expect-arg
    (lambda (x)
      (cond ((or (bytevector? x) (flonum? x) (and (integer? x) (exact? x))) x)
            ((string? x) (string->utf8/nul x))
            (else
             (assertion-violation #f (format "expected exact integer, string, flonum, or bytevector, but got ~r" x))))))

  (define expect-args
    (lambda (name n lst)
      (let loop ((n n) (lst lst) (args '()))
        (cond ((null? lst) (reverse args))
              ((or (bytevector? (car lst))
                   (flonum? (car lst))
                   (and (integer? (car lst)) (exact? (car lst))))
               (loop (+ n 1) (cdr lst) (cons (car lst) args)))
              ((string? (car lst))
               (loop (+ n 1) (cdr lst) (cons (string->utf8/nul (car lst)) args)))
              (else
               (assertion-violation name (format "expected exact integer, string, flonum, or bytevector, but got ~r, as argument ~s" (car lst) n)))))))

  (define make-binary-array-of-int
    (lambda (argv)
      (let ((bv (make-bytevector (* alignof:int (length argv)))))
        (let loop ((offset 0) (arg argv))
          (cond ((null? arg) bv)
                (else
                 (bytevector-c-int-set! bv offset (car arg))
                 (loop (+ offset alignof:int) (cdr arg))))))))

  (define make-binary-array-of-char*
    (lambda (ref argv)
      (apply vector
             ref
             (map (lambda (value) (string->utf8/nul value)) argv))))

  (define c-function-return-type-alist
    '((void           . #x00)    ; FFI_RETURN_TYPE_VOID
      (bool           . #x01)    ; FFI_RETURN_TYPE_BOOL
      (char           . #x0d)    ; FFI_RETURN_TYPE_UINT8_T
      (short          . #x02)    ; FFI_RETURN_TYPE_SHORT
      (int            . #x03)    ; FFI_RETURN_TYPE_INT
      (long           . #x04)    ; FFI_RETURN_TYPE_INTPTR
      (unsigned-short . #x05)    ; FFI_RETURN_TYPE_USHORT
      (unsigned-int   . #x06)    ; FFI_RETURN_TYPE_UINT
      (unsigned-long  . #x07)    ; FFI_RETURN_TYPE_UINTPTR
      (float          . #x08)    ; FFI_RETURN_TYPE_FLOAT
      (double         . #x09)    ; FFI_RETURN_TYPE_DOUBLE
      (void*          . #x07)    ; FFI_RETURN_TYPE_UINTPTR
      (char*          . #x0a)    ; FFI_RETURN_TYPE_STRING
      (size_t         . #x0b)    ; FFI_RETURN_TYPE_SIZE_T
      (int8_t         . #x0c)    ; FFI_RETURN_TYPE_INT8_T
      (uint8_t        . #x0d)    ; FFI_RETURN_TYPE_UINT8_T
      (int16_t        . #x0e)    ; FFI_RETURN_TYPE_INT16_T
      (uint16_t       . #x0f)    ; FFI_RETURN_TYPE_UINT16_T
      (int32_t        . #x10)    ; FFI_RETURN_TYPE_INT32_T
      (uint32_t       . #x11)    ; FFI_RETURN_TYPE_UINT32_T
      (int64_t        . #x12)    ; FFI_RETURN_TYPE_INT64_T
      (uint64_t       . #x13)))  ; FFI_RETURN_TYPE_UINT64_T

  (define callback-return-type-alist
    '((bool           . #x00)    ; CALLBACK_RETURN_TYPE_INTPTR
      (void           . #x00)    ; CALLBACK_RETURN_TYPE_INTPTR
      (char           . #x00)    ; CALLBACK_RETURN_TYPE_INTPTR
      (short          . #x00)    ; CALLBACK_RETURN_TYPE_INTPTR
      (int            . #x00)    ; CALLBACK_RETURN_TYPE_INTPTR
      (long           . #x00)    ; CALLBACK_RETURN_TYPE_INTPTR
      (unsigned-short . #x00)    ; CALLBACK_RETURN_TYPE_INTPTR
      (unsigned-int   . #x00)    ; CALLBACK_RETURN_TYPE_INTPTR
      (unsigned-long  . #x00)    ; CALLBACK_RETURN_TYPE_INTPTR
      (int8_t         . #x00)    ; CALLBACK_RETURN_TYPE_INTPTR
      (int16_t        . #x00)    ; CALLBACK_RETURN_TYPE_INTPTR
      (int32_t        . #x00)    ; CALLBACK_RETURN_TYPE_INTPTR
      (int64_t        . #x01)    ; CALLBACK_RETURN_TYPE_INT64_T
      (uint8_t        . #x00)    ; CALLBACK_RETURN_TYPE_INTPTR
      (uint16_t       . #x00)    ; CALLBACK_RETURN_TYPE_INTPTR
      (uint32_t       . #x00)    ; CALLBACK_RETURN_TYPE_INTPTR
      (uint64_t       . #x01)    ; CALLBACK_RETURN_TYPE_INT64_T
      (float          . #x02)    ; CALLBACK_RETURN_TYPE_FLOAT
      (double         . #x03)    ; CALLBACK_RETURN_TYPE_DOUBLE
      (size_t         . #x00)    ; CALLBACK_RETURN_TYPE_INTPTR
      (void*          . #x00)))  ; CALLBACK_RETURN_TYPE_INTPTR

  (define STDCALL #x0100)

  (define handle-bool
    (lambda (x)
      (cond ((and (integer? x) (exact? x)) (if (= x 0) 0 1))
            ((unspecified? x) 0)
            (else (assertion-violation 'callback (format "expected exact integer, but got ~r, as return value" x))))))

  (define handle-int
    (lambda (x)
      (cond ((and (integer? x) (exact? x)) x)
            ((unspecified? x) 0)
            (else (assertion-violation 'callback (format "expected exact integer, but got ~r, as return value" x))))))

  (define handle-real
    (lambda (x)
      (cond ((real? x) x)
            ((unspecified? x) 0.0)
            (else (assertion-violation 'callback (format "expected real, but got ~r, as return value" x))))))

  (define callback-return-type-verifier
    (lambda (type)
      (case type
        ((void) values)
        ((bool) handle-bool)
        ((float double) handle-real)
        (else handle-int))))

  (define callback-argument-type-class
    `((bool           . #\L)
      (char           . #\U)
      (short          . #\b)
      (int            . ,(if (= sizeof:int 4) #\q #\o))
      (long           . ,(if (= sizeof:long 4) #\q #\o))
      (unsigned-short . #\B)
      (unsigned-int   . ,(if (= sizeof:int 4) #\Q #\O))
      (unsigned-long  . ,(if (= sizeof:long 4) #\Q #\O))
      (int8_t         . #\u)
      (int16_t        . #\b)
      (int32_t        . #\q)
      (int64_t        . #\o)
      (uint8_t        . #\U)
      (uint16_t       . #\B)
      (uint32_t       . #\Q)
      (uint64_t       . #\O)
      (float          . #\f)
      (double         . #\d)
      (size_t         . ,(if (= sizeof:size_t 4) #\Q #\O))
      (void*          . ,(if (= sizeof:void* 4) #\Q #\O))))

  (define-thread-variable ht-cdecl-callback-trampolines (make-parameter (make-weak-hashtable)))

  (define-thread-variable ht-stdcall-callback-trampolines (make-parameter (make-weak-hashtable)))

  (define make-callback-thunk
    (lambda (verifier callee)
      (lambda x (verifier (apply callee x)))))

  (define make-callback-signature
    (lambda (name ret args proc)
      (apply string
             (map (lambda (a)
                    (cond ((assq a callback-argument-type-class) => cdr)
                          (else (assertion-violation name (format "invalid argument type ~u" a) (list ret args proc)))))
                  args))))

  (define check-callback-param
    (lambda (name ret args proc)
      (assert-argument name 1 ret "symbol" symbol? (list ret args proc))
      (assert-argument name 2 args "list" list? (list ret args proc))
      (assert-argument name 3 proc "procedure" procedure? (list ret args proc))))

  (define-syntax make-stdcall-callback-trampoline
    (syntax-rules ()
      ((_ type args ret proc)
       (begin
         (check-callback-param 'make-stdcall-callback ret args proc)
         (make-callback-trampoline (+ (cdr type) STDCALL)
                                   (make-callback-signature 'make-stdcall-callback ret args proc)
                                   (make-callback-thunk (callback-return-type-verifier ret) proc))))))

  (define-syntax make-cdecl-callback-trampoline
    (syntax-rules ()
      ((_ type args ret proc)
       (begin
         (check-callback-param 'make-cdecl-callback ret args proc)
         (make-callback-trampoline (cdr type)
                                   (make-callback-signature 'make-cdecl-callback ret args proc)
                                   (make-callback-thunk (callback-return-type-verifier ret) proc))))))

  (define make-cdecl-callback
    (lambda (ret args proc)
      (or (cond ((hashtable-ref (ht-cdecl-callback-trampolines) proc #f)
                 => (lambda (rec)
                      (destructuring-bind (trampoline ret-memo args-memo) rec
                        (and (equal? ret ret-memo) (equal? args args-memo) trampoline))))
                (else #f))
          (cond ((assq ret callback-return-type-alist)
                 => (lambda (type)
                      (let ((trampoline (make-cdecl-callback-trampoline type args ret proc)))
                        (hashtable-set! (ht-cdecl-callback-trampolines) proc (list trampoline ret args))
                        trampoline)))
                (else
                 (assertion-violation 'make-cdecl-callback (format "invalid return type ~u" ret) (list ret args proc)))))))

  (define make-stdcall-callback
    (lambda (ret args proc)
      (or (cond ((hashtable-ref (ht-stdcall-callback-trampolines) proc #f)
                 => (lambda (rec)
                      (destructuring-bind (trampoline ret-memo args-memo) rec
                        (and (equal? ret ret-memo) (equal? args args-memo) trampoline))))
                (else #f))
          (cond ((assq ret callback-return-type-alist)
                 => (lambda (type)
                      (let ((trampoline (make-stdcall-callback-trampoline type args ret proc)))
                        (hashtable-set! (ht-stdcall-callback-trampolines) proc (list trampoline ret args))
                        trampoline)))
                (else
                 (assertion-violation 'make-stdcall-callback (format "invalid return type ~u" ret) (list ret args proc)))))))

  (define make-argument-thunk
    (lambda (name type)
      (case type
        ((...)
         (cons #\* values))
        ((bool char short int long unsigned-short unsigned-int unsigned-long int8_t int16_t int32_t uint8_t uint16_t uint32_t size_t)
         (cons #\i values))
        ((int64_t uint64_t)
         (cons #\x values))
        ((void*)
         (cons #\p values))
        ((float)
         (cons #\f values))
        ((double)
         (cons #\d values))
        ((char*)
         (cons #\p
               (lambda (x)
                 (cond ((eq? x 0) 0)
                       ((string? x) (string->utf8/nul x))
                       (else
                        (assertion-violation #f (format "c function expected string or 0, but got ~r" x)))))))
        (else
         (destructuring-match type
           (['int]
            (cons #\p
                  (lambda (x)
                    (or (vector? x) (assertion-violation #f (format "expected vector, but got ~r" x)))
                    (make-binary-array-of-int
                     (let ((lst (vector->list x)))
                       (for-each (lambda (i)
                                   (or (and (integer? i) (exact? i))
                                       (assertion-violation #f (format "expected list of exact integer, but got ~r" x))))
                                 lst)
                       lst)))))
           (['char*]
            (cons #\c
                  (lambda (x)
                    (or (vector? x) (assertion-violation #f (format "expected vector, but got ~r" x)))
                    (make-binary-array-of-char*
                     0
                     (let ((lst (vector->list x)))
                       (for-each (lambda (s)
                                   (or (string? s)
                                       (assertion-violation #f (format "expected list of string, but got ~r" x))))
                                 lst)
                       lst)))))
           (('* ['char*])
            (cons #\c
                  (lambda (x)
                    (or (vector? x) (assertion-violation #f (format "expected vector, but got ~r" x)))
                    (make-binary-array-of-char*
                     1
                     (let ((lst (vector->list x)))
                       (for-each (lambda (s)
                                   (or (string? s)
                                       (assertion-violation #f (format "expected list of string, but got ~r" x))))
                                 lst)
                       lst)))))
           (_
            (assertion-violation name (format "invalid argument type ~u" type))
            (assertion-violation 'make-cdecl-callout (format "invalid argument type ~u" type))))))))

  (define make-cdecl-callout
    (lambda (ret args addrs)

      (define make-cdecl-callout-closure
        (lambda (type addrs signature thunks)
          (lambda x
            (let loop ((in x) (thunk thunks) (out '()))
              (cond ((and (pair? in) (pair? thunk))
                     (loop (cdr in) (cdr thunk) (cons ((car thunk) (car in)) out)))
                    ((or (pair? in) (pair? thunk))
                     (assertion-violation #f (format "expected ~a, but ~a arguments given" (length thunks) (length x)) x))
                    (else
                     (apply call-shared-object type addrs #f signature (reverse out))))))))

      (define make-variadic-cdecl-callout-closure
        (lambda (type addrs signature thunks)
          (lambda x
            (let loop ((in x) (thunk thunks) (out '()))
              (cond ((and (pair? in) (pair? thunk))
                     (loop (cdr in) (cdr thunk) (cons ((car thunk) (car in)) out)))
                    ((pair? in)
                     (loop (cdr in) thunk (cons (expect-arg (car in)) out)))
                    ((pair? thunk)
                     (assertion-violation #f (format "required at least ~a, but ~a arguments given" (length thunks) (length x)) x))
                    (else
                     (apply call-shared-object type addrs #f signature (reverse out))))))))

      (assert-argument make-cdecl-callout 1 ret "symbol" symbol? (list ret args addrs))
      (assert-argument make-cdecl-callout 2 args "list" list? (list ret args addrs))
      (assert-argument make-cdecl-callout 3 addrs "c function address" (and (integer? addrs) (exact? addrs)) (list ret args addrs))
      (let ((lst (map (lambda (a) (make-argument-thunk 'make-cdecl-callout a)) args)))
        (let ((signature (apply string (map car lst))) (thunk (map cdr lst)))
          (let ((variadic (memq '... args)))
            (cond (variadic
                   (or (= (length variadic) 1)
                       (assertion-violation 'make-cdecl-callout "invalid argument type list" args))
                   (make-variadic-cdecl-callout-closure (cond ((assq ret c-function-return-type-alist) => cdr)
                                                              (else
                                                               (assertion-violation 'make-cdecl-callout
                                                                                    (format "invalid return type ~u" ret)
                                                                                    (list ret args addrs))))
                                                        addrs signature (list-head thunk (- (length thunk) 1))))
                  (else
                   (make-cdecl-callout-closure (cond ((assq ret c-function-return-type-alist) => cdr)
                                                     (else
                                                      (assertion-violation 'make-cdecl-callout
                                                                           (format "invalid return type ~u" ret)
                                                                           (list ret args addrs))))
                                               addrs signature thunk))))))))

  (define make-stdcall-callout
    (lambda (ret args addrs)

      (define make-stdcall-callout-closure
        (lambda (type addrs signature thunks)
          (lambda x
            (let loop ((in x) (thunk thunks) (out '()))
              (cond ((and (pair? in) (pair? thunk))
                     (loop (cdr in) (cdr thunk) (cons ((car thunk) (car in)) out)))
                    ((or (pair? in) (pair? thunk))
                     (assertion-violation #f (format "expected ~a, but ~a arguments given" (length thunks) (length x)) x))
                    (else
                     (apply call-shared-object (+ type STDCALL) addrs #f signature (reverse out))))))))

      (assert-argument make-cdecl-callout 1 ret "symbol" symbol? (list ret args addrs))
      (assert-argument make-cdecl-callout 2 args "list" list? (list ret args addrs))
      (assert-argument make-cdecl-callout 3 addrs "c function address" (and (integer? addrs) (exact? addrs)) (list ret args addrs))
      (and (memq '... args) (assertion-violation 'make-stdcall-callout "... in argument type list" args))
      (let ((lst (map (lambda (a) (make-argument-thunk 'make-stdcall-callout a)) args)))
        (let ((signature (apply string (map car lst))) (thunk (map cdr lst)))
          (make-stdcall-callout-closure (cond ((assq ret c-function-return-type-alist) => cdr)
                                              (else
                                               (assertion-violation 'make-stdcall-callout
                                                                    (format "invalid return type ~u" ret)
                                                                    (list ret args addrs))))
                                        addrs signature thunk)))))

  (define-syntax c-function
    (lambda (x)
      (syntax-case x ()
        ((_ lib-handle lib-name ret-type func-conv func-name (arg-types ...))
         (let ()

           (define c-callback-return
             (lambda (type)
               (if (assq type callback-return-type-alist)
                   (datum->syntax #'k type)
                   (syntax-violation 'c-callback (format "invalid return type declarator ~u" type) x))))

           (define c-callback-arguments
             (lambda (lst)
               (if (for-all (lambda (arg) (assq arg callback-argument-type-class)) lst)
                   (datum->syntax #'k lst)
                   (syntax-violation 'c-callback (format "invalid argument types declarator ~u" lst) x))))

           (define c-arguments
             (lambda (args)
               (map (lambda (type n var)
                      (with-syntax ((n n) (var var))
                        (case type
                          ((...)
                           (list #\* #'(expect-args 'func-name n var)))
                          ((bool char short int long unsigned-short unsigned-int unsigned-long int8_t int16_t int32_t uint8_t uint16_t uint32_t size_t)
                           (list #\i #'var))
                          ((int64_t uint64_t)
                           (list #\x #'var))
                          ((void*)
                           (list #\p #'var))
                          ((float)
                           (list #\f #'var))
                          ((double)
                           (list #\d #'var))
                          ((char*)
                           (list #\p #'(expect-string 'func-name n var)))
                          (else
                           (destructuring-match type
                             (['c-callback e1 (e2 ...)]
                              (with-syntax ((e1 (c-callback-return e1)) (e2 (c-callback-arguments e2)))
                                (list #\p #'(make-cdecl-callback 'e1 'e2 (expect-proc 'func-name n var)))))
                             (['c-callback e1 '__cdecl (e2 ...)]
                              (with-syntax ((e1 (c-callback-return e1)) (e2 (c-callback-arguments e2)))
                                (list #\p #'(make-cdecl-callback 'e1 'e2 (expect-proc 'func-name n var)))))
                             (['c-callback e1 '__stdcall (e2 ...)]
                              (with-syntax ((e1 (c-callback-return e1)) (e2 (c-callback-arguments e2)))
                                (list #\p #'(make-stdcall-callback 'e1 'e2 (expect-proc 'func-name n var)))))
                             (['int]
                              (list #\p #'(make-binary-array-of-int (expect-exact-int-vector 'func-name n var))))
                             (['char*]
                              (list #\c #'(make-binary-array-of-char* 0 (expect-string-vector 'func-name n var))))
                             (('* ['char*])
                              (list #\c #'(make-binary-array-of-char* 1 (expect-string-vector 'func-name n var))))
                             (_
                              (syntax-violation 'c-function (format "invalid argument type ~u" type) x)))))))
                    (datum (arg-types ...)) (iota (length args) 1) args)))

           (cond ((assq (datum ret-type) c-function-return-type-alist)
                  => (lambda (lst)
                       (let ((symbolic-arg-types (datum (arg-types ...))))
                         (let ((variadic (memq '... symbolic-arg-types)))
                           (with-syntax
                               ((type
                                 (case (datum func-conv)
                                   ((__cdecl) (cdr lst))
                                   ((__stdcall)
                                    (and variadic (syntax-violation 'c-function "invalid syntax" x))
                                    (+ (cdr lst) STDCALL))
                                   (else (syntax-violation 'c-function "invalid syntax" x))))
                                ((args ...) (generate-temporaries symbolic-arg-types)))
                             (with-syntax ((((signature thunk) ...) (c-arguments #'(args ...))))
                               (with-syntax ((signature (apply string (datum (signature ...)))))
                                 (cond (variadic
                                        (or (= (length variadic) 1) (syntax-violation 'c-function "invalid syntax" x))
                                        (with-syntax (((args ... last) #'(args ...)))
                                          #'(let ((loc (lookup-shared-object lib-handle 'func-name)))
                                              (if loc
                                                  (let ((func-name (lambda (args ... . last) (apply call-shared-object type loc 'func-name signature thunk ...)))) func-name)
                                                  (let ((func-name (lambda x (error 'func-name (format "function not available in ~a" lib-name))))) func-name)))))
                                       (else
                                        #'(let ((loc (lookup-shared-object lib-handle 'func-name)))
                                            (if loc
                                                (let ((func-name (lambda (args ...) (call-shared-object type loc 'func-name signature thunk ...)))) func-name)
                                                (let ((func-name (lambda x (error 'func-name (format "function not available in ~a" lib-name))))) func-name))))))))))))
                 (else
                  (syntax-violation 'c-function (format "invalid return type ~u" (datum ret-type)) x)))))
        ((_ lib-handle lib-name ret-type func-name (arg-types ...))
         #'(c-function lib-handle lib-name ret-type __cdecl func-name (arg-types ...)))
        (_ (syntax-violation 'c-function "invalid syntax" x)))))

  (define-syntax c-function/errno
    (syntax-rules ()
      ((_ . x)
       (let ((proc (c-function . x)))
         (lambda args
           (let* ((ret (apply proc args)) (err (shared-object-errno)))
             (values ret err)))))))

  (define-syntax c-function/win32-lasterror
    (syntax-rules ()
      ((_ . x)
       (if on-windows
           (let ((proc (c-function . x)))
             (lambda args
               (let* ((ret (apply proc args)) (err (shared-object-win32-lasterror)))
                 (values ret err))))
           (lambda x
             (error 'c-function/win32-lasterror (format "only available on windows")))))))

  ) ;[end]
