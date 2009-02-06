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
          define-c-typedef
          define-c-struct-type
          define-c-struct-methods
          c-sizeof
          c-coerce-void*
          bytevector-c-char-ref
          bytevector-c-short-ref
          bytevector-c-int-ref
          bytevector-c-long-ref
          bytevector-c-void*-ref
          bytevector-c-float-ref
          bytevector-c-double-ref
          bytevector-c-unsigned-short-ref
          bytevector-c-unsigned-int-ref
          bytevector-c-unsigned-long-ref
          bytevector-c-char-set!
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
          sizeof:short
          sizeof:int
          sizeof:long
          sizeof:void*
          sizeof:size_t
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
          on-darwin
          on-linux
          on-freebsd
          on-openbsd
          on-windows
          on-posix
          on-ia32
          on-x64)

  (import (core) (ypsilon c-types) (ypsilon assert))

  (define on-darwin        (and (string-contains (architecture-feature 'operating-system) "darwin")  #t))
  (define on-linux         (and (string-contains (architecture-feature 'operating-system) "linux")   #t))
  (define on-freebsd       (and (string-contains (architecture-feature 'operating-system) "freebsd") #t))
  (define on-openbsd       (and (string-contains (architecture-feature 'operating-system) "openbsd") #t))
  (define on-windows       (and (string-contains (architecture-feature 'operating-system) "windows") #t))
  (define on-posix         (not on-windows))
  (define on-x64           (and (or (string-contains (architecture-feature 'machine-hardware) "x86_64")
                                    (string-contains (architecture-feature 'machine-hardware) "amd64")) #t))
  (define on-ia32          (not on-x64))

  (define expect-bool
    (lambda (name n i)
      (cond ((boolean? i) (if i 1 0))
            (else
             (assertion-violation name (format "expected #t or #f, but got ~r, as argument ~s" i n))))))

  (define expect-exact-int
    (lambda (name n i)
      (cond ((and (integer? i) (exact? i)) i)
            (else
             (assertion-violation name (format "expected exact integer, but got ~r, as argument ~s" i n))))))

  (define expect-void*
    (lambda (name n i)
      (cond ((or (and (integer? i) (exact? i)) (bytevector-mapping? i)) i)
            (else
             (assertion-violation name (format "expected exact integer or bytevector-mapping object, but got ~r, as argument ~s" i n))))))
  
  (define expect-float
    (lambda (name n f)
      (cond ((flonum? f) (flonum->float f))
            (else
             (assertion-violation name (format "expected flonum, but got ~r, as argument ~s" f n))))))

  (define expect-double
    (lambda (name n f)
      (cond ((flonum? f) f)
            (else
             (assertion-violation name (format "expected flonum, but got ~r, as argument ~s" f n))))))

  (define expect-string
    (lambda (name n s)
      (cond ((string? s) (string->utf8+nul s))
            ((not s) 0)
            (else
             (assertion-violation name (format "expected string or #f(NULL), but got ~r, as argument ~s" s n))))))

  (define expect-byte*
    (lambda (name n b)
      (cond ((bytevector? b) b)
            ((not b) 0)
            (else
             (assertion-violation name (format "expected bytevector or #f(NULL), but got ~r, as argument ~s" b n))))))

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
             (map (lambda (value) (string->utf8+nul value)) argv))))

  (define string->utf8+nul
    (lambda (s)
      (string->utf8 (string-append s "\x0;"))))

  (define-syntax coerce-unsigned-exact
    (syntax-rules ()
      ((_ bytesize)
       (let ((mask-bits (- (bitwise-arithmetic-shift 1 (* bytesize 8)) 1)))
         (lambda (n)
           (bitwise-and n mask-bits))))))

  (define-syntax coerce-signed-exact
    (syntax-rules ()
      ((_ bytesize)
       (let ((sign-bit (bitwise-arithmetic-shift 1 (- (* bytesize 8) 1)))
             (mask-bits (- (bitwise-arithmetic-shift 1 (* bytesize 8)) 1)))
         (lambda (n)
           (let ((n (bitwise-and n mask-bits)))
             (if (= (bitwise-and n sign-bit) 0) n (+ (bitwise-not n) 1))))))))

  (define coerce-short (coerce-signed-exact sizeof:short))
  (define coerce-int (coerce-signed-exact sizeof:int))
  (define coerce-long (coerce-signed-exact sizeof:long))
  (define coerce-unsigned-short (coerce-unsigned-exact sizeof:short))
  (define coerce-unsigned-int (coerce-unsigned-exact sizeof:int))
  (define coerce-unsigned-long (coerce-unsigned-exact sizeof:long))
  (define coerce-void* (coerce-unsigned-exact sizeof:void*))
  (define coerce-bool (lambda (n) (= n 0)))
  
  (define c-function-return-type-alist
    `((void           . #x00)    ; FFI_RETURN_TYPE_VOID
      (bool           . #x01)    ; FFI_RETURN_TYPE_BOOL
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
      (uint8_t        . #x0d)))  ; FFI_RETURN_TYPE_UINT8_T
  
  (define ht-cdecl-callback-trampolines (make-parameter (make-weak-hashtable)))
  (define ht-stdcall-callback-trampolines (make-parameter (make-weak-hashtable)))
  (define callback-return-type-list '(void short int long unsigned-short unsigned-int unsigned-long size_t void*))
  (define callback-argument-type-list '(bool short int long unsigned-short unsigned-int unsigned-long size_t void*))

  (define make-cdecl-callback-trampoline
    (lambda (n proc)
      (make-callback 0 n proc)))

  (define make-stdcall-callback-trampoline
    (lambda (n proc)
      (make-callback 1 n proc)))

  (define make-callback-thunk
    (let ((coerce-thunk-alist
           `((bool . ,coerce-bool)
             (short . ,coerce-short)
             (int . ,(if (= sizeof:int sizeof:void*) values coerce-int))
             (long . ,(if (= sizeof:long sizeof:void*) values coerce-long))
             (size_t . ,(cond ((= sizeof:size_t sizeof:int) coerce-unsigned-int)
                              ((= sizeof:size_t sizeof:long) coerce-unsigned-long)))
             (void* . ,coerce-void*)
             (unsigned-short . ,coerce-unsigned-short)
             (unsigned-int . ,coerce-unsigned-int)
             (unsigned-long . ,coerce-unsigned-long))))

      (define callback-thunk-closure
        (lambda (callee thunks)
          (lambda x
            (let loop ((in x) (thunk thunks) (out '()))
              (if (and (pair? in) (pair? thunk))
                  (loop (cdr in) (cdr thunk) (cons ((car thunk) (car in)) out))
                  (apply callee (reverse out)))))))

      (lambda (ret args callee)
        (callback-thunk-closure callee (map (lambda (e) (cdr (assq e coerce-thunk-alist))) args)))))

  (define make-cdecl-callback
    (lambda (ret args proc)
      (or (cond ((hashtable-ref (ht-cdecl-callback-trampolines) proc #f)
                 => (lambda (rec)
                      (destructuring-bind (trampoline ret-memo args-memo) rec
                        (and (equal? ret ret-memo)
                             (equal? args args-memo)
                             trampoline))))
                (else #f))
          (begin
            (assert-argument make-cdecl-callback 1 ret "symbol" symbol? (list ret args proc))
            (assert-argument make-cdecl-callback 2 args "list" list? (list ret args proc))
            (assert-argument make-cdecl-callback 3 proc "procedure" procedure? (list ret args proc))
            (unless (memq ret callback-return-type-list)
              (assertion-violation 'make-cdecl-callback (format "invalid return type ~u" ret) (list ret args proc)))
            (for-each (lambda (a)
                        (unless (memq a callback-argument-type-list)
                          (assertion-violation 'make-cdecl-callback
                                               (format "invalid argument type ~u" a)
                                               (list ret args proc))))
                      args)
            (let ((trampoline (make-cdecl-callback-trampoline (length args) (make-callback-thunk ret args proc))))
              (hashtable-set! (ht-cdecl-callback-trampolines) proc (list trampoline ret args))
              trampoline)))))

  (define make-stdcall-callback
    (lambda (ret args proc)
      (or (cond ((hashtable-ref (ht-stdcall-callback-trampolines) proc #f)
                 => (lambda (rec)
                      (destructuring-bind (trampoline ret-memo args-memo) rec
                        (and (equal? ret ret-memo)
                             (equal? args args-memo)
                             trampoline))))
                (else #f))
          (begin
            (assert-argument make-cdecl-callback 1 ret "symbol" symbol? (list ret args proc))
            (assert-argument make-cdecl-callback 2 args "list" list? (list ret args proc))
            (assert-argument make-cdecl-callback 3 proc "procedure" procedure? (list ret args proc))
            (unless (memq ret callback-return-type-list)
              (assertion-violation 'make-stdcall-callback (format "invalid return type ~u" ret) (list ret args proc)))
            (for-each (lambda (a)
                        (unless (memq a callback-argument-type-list)
                          (assertion-violation 'make-cdecl-callback
                                               (format "invalid argument type ~u" a)
                                               (list ret args proc))))
                      args)
            (let ((trampoline (make-stdcall-callback-trampoline (length args) (make-callback-thunk ret args proc))))
              (hashtable-set! (ht-stdcall-callback-trampolines) proc (list trampoline ret args))
              trampoline)))))

  (define make-argument-thunk
    (lambda (name type)

      (define cdecl-callback-thunk
        (lambda (e1 e2)
          (lambda (x)
            (or (procedure? x) (assertion-violation #f (format "c function expected procedure, but got ~r" x)))
            (make-cdecl-callback e1 e2 x))))

      (define stdcall-callback-thunk
        (lambda (e1 e2)
          (lambda (x)
            (or (procedure? x) (assertion-violation #f (format "c function expected procedure, but got ~r" x)))
            (make-stdcall-callback e1 e2 x))))

      (case type
        ((bool)
         (lambda (x)
           (if (boolean? x)
               (if x 1 0)
               (assertion-violation #f (format "c function expected #t or #f, but got ~r" x)))))
        ((short int long unsigned-short unsigned-int unsigned-long size_t)
         (lambda (x)
           (if (and (integer? x) (exact? x))
               x
               (assertion-violation #f (format "c function expected exact integer, but got ~r" x)))))
        ((void*)
         (lambda (x)
           (if (or (and (integer? x) (exact? x)) (bytevector-mapping? x))
               x
               (assertion-violation #f (format "c function expected exact integer or bytevector-mapping object, but got ~r" x)))))
        ((byte*)
         (lambda (x)
           (cond ((bytevector? x) x)
                 ((not x) 0)
                 (else
                  (assertion-violation #f (format "c function expected bytevector or #f(NULL), but got ~r" x))))))
        ((char*)
         (lambda (x)
           (cond ((string? x) (string->utf8+nul x))
                 ((not x) 0)
                 (else
                  (assertion-violation #f (format "c function expected string or #f(NULL), but got ~r" x))))))
        ((float)
         (lambda (x)
           (if (flonum? x)
               (flonum->float x)
               (assertion-violation #f (format "c function expected flonum, but got ~r" x)))))
        ((double)
         (lambda (x)
           (if (flonum? x)
               x
               (assertion-violation #f (format "c function expected flonum, but got ~r" x)))))
        (else
         (destructuring-match type
           #;(['c-callback e1 (e2 ...)] (cdecl-callback-thunk e1 e2))
           #;(['c-callback e1 '__cdecl (e2 ...)] (cdecl-callback-thunk e1 e2))
           #;(['c-callback e1 '__stdcall (e2 ...)] (stdcall-callback-thunk e1 e2))
           (['int]
            (lambda (x)
              (or (vector? x) (assertion-violation name (format "expected vector, but got ~r" x)))
              (make-binary-array-of-int
               (let ((lst (vector->list x)))
                 (for-each (lambda (i)
                             (or (and (integer? i) (exact? i))
                                 (assertion-violation name (format "expected list of exact integer, but got ~r" x))))
                           lst)
                 lst))))
           (['char*]
            (lambda (x)
              (or (vector? x) (assertion-violation name (format "expected vector, but got ~r" x)))
              (make-binary-array-of-char*
               0
               (let ((lst (vector->list x)))
                 (for-each (lambda (s)
                             (or (string? s)
                                 (assertion-violation name (format "expected list of string, but got ~r" x))))
                           lst)
                 lst))))
           (('* ['char*])
            (lambda (x)
              (or (vector? x) (assertion-violation name (format "expected vector, but got ~r" x)))
              (make-binary-array-of-char*
               1
               (let ((lst (vector->list x)))
                 (for-each (lambda (s)
                             (or (string? s)
                                 (assertion-violation name (format "expected list of string, but got ~r" x))))
                           lst)
                 lst))))
           (_
            (assertion-violation name (format "invalid argument type ~u" type))))))))

  (define make-cdecl-callout
    (lambda (ret args addrs)

      (define make-cdecl-callout-closure
        (lambda (code addrs thunks)
          (lambda x
            (let loop ((in x) (thunk thunks) (out '()))
              (cond ((and (pair? in) (pair? thunk))
                     (loop (cdr in) (cdr thunk) (cons ((car thunk) (car in)) out)))
                    ((or (pair? in) (pair? thunk))
                     (assertion-violation #f (format "c function expected ~a, but ~a arguments given" (length thunks) (length x)) x))
                    (else
                     (apply call-shared-object code addrs (reverse out))))))))

      (assert-argument make-cdecl-callout 1 ret "symbol" symbol? (list ret args addrs))
      (assert-argument make-cdecl-callout 2 args "list" list? (list ret args addrs))
      (assert-argument make-cdecl-callout 3 addrs "c function address" (and (integer? addrs) (exact? addrs)) (list ret args addrs))
      (make-cdecl-callout-closure (cond ((assq ret c-function-return-type-alist) => cdr)
                                        (else
                                         (assertion-violation 'make-cdecl-callout
                                                              (format "invalid return type ~u" ret)
                                                              (list ret args addrs))))
                                  addrs
                                  (map (lambda (a) (make-argument-thunk 'make-cdecl-callout a)) args))))

  (define make-stdcall-callout
    (lambda (ret args addrs)

      (define make-stdcall-callout-closure
        (lambda (code addrs thunks)
          (lambda x
            (let loop ((in x) (thunk thunks) (out '()))
              (cond ((and (pair? in) (pair? thunk))
                     (loop (cdr in) (cdr thunk) (cons ((car thunk) (car in)) out)))
                    ((or (pair? in) (pair? thunk))
                     (assertion-violation #f (format "c function expected ~a, but ~a arguments given" (length thunks) (length x)) x))
                    (else
                     (apply stdcall-shared-object code addrs (reverse out))))))))

      (assert-argument make-cdecl-callout 1 ret "symbol" symbol? (list ret args addrs))
      (assert-argument make-cdecl-callout 2 args "list" list? (list ret args addrs))
      (assert-argument make-cdecl-callout 3 addrs "c function address" (and (integer? addrs) (exact? addrs)) (list ret args addrs))
      (make-stdcall-callout-closure (cond ((assq ret c-function-return-type-alist) => cdr)
                                          (else
                                           (assertion-violation 'make-cdecl-callout
                                                                (format "invalid return type ~u" ret)
                                                                (list ret args addrs))))
                                    addrs
                                    (map (lambda (a) (make-argument-thunk 'make-stdcall-callout a)) args))))

  (define-syntax c-function
    (lambda (x)
      (syntax-case x ()
        ((_ lib-handle lib-name ret-type func-conv func-name (arg-types ...))
         (let ()

           (define c-callback-return
             (lambda (type)
               (if (memq type callback-return-type-list)
                   (datum->syntax #'k type)
                   (syntax-violation 'c-callback (format "invalid return type declarator ~u" type) x))))

           (define c-callback-arguments
             (lambda (lst)
               (if (for-all (lambda (arg) (memq arg callback-argument-type-list)) lst)
                   (datum->syntax #'k lst)
                   (syntax-violation 'c-callback (format "invalid argument types declarator ~u" lst) x))))

           (define c-arguments
             (lambda (args)
               (map (lambda (type n var)
                      (with-syntax ((n n) (var var))
                        (case type
                          ((bool)
                           #'(expect-bool 'func-name n var))
                          ((short int long unsigned-short unsigned-int unsigned-long size_t)
                           #'(expect-exact-int 'func-name n var))
                          ((void*)
                           #'(expect-void* 'func-name n var))
                           ((byte*)
                           #'(expect-byte* 'func-name n var))
                          ((char*)
                           #'(expect-string 'func-name n var))
                          ((float)
                           #'(expect-float 'func-name n var))
                          ((double)
                           #'(expect-double 'func-name n var))
                          (else
                           (destructuring-match type
                             (['c-callback e1 (e2 ...)]
                              (with-syntax ((e1 (c-callback-return e1)) (e2 (c-callback-arguments e2)))
                                #'(make-cdecl-callback 'e1 'e2 (expect-proc 'func-name n var))))
                             (['c-callback e1 '__cdecl (e2 ...)]
                              (with-syntax ((e1 (c-callback-return e1)) (e2 (c-callback-arguments e2)))
                                #'(make-cdecl-callback 'e1 'e2 (expect-proc 'func-name n var))))
                             (['c-callback e1 '__stdcall (e2 ...)]
                              (with-syntax ((e1 (c-callback-return e1)) (e2 (c-callback-arguments e2)))
                                #'(make-stdcall-callback 'e1 'e2 (expect-proc 'func-name n var))))
                             (['int]
                              #'(make-binary-array-of-int (expect-exact-int-vector 'func-name n var)))
                             (['char*]
                              #'(make-binary-array-of-char* 0 (expect-string-vector 'func-name n var)))
                             (('* ['char*])
                              #'(make-binary-array-of-char* 1 (expect-string-vector 'func-name n var)))
                             (_
                              (syntax-violation 'c-function (format "invalid argument type ~u" type) x)))))))
                    (datum (arg-types ...)) (iota (length args) 1) args)))

           (cond ((assq (datum ret-type) c-function-return-type-alist)
                  => (lambda (lst)
                       (with-syntax
                           ((call
                             (case (datum func-conv)
                               ((__cdecl) #'call-shared-object)
                               ((__stdcall) #'stdcall-shared-object)
                               (else (syntax-violation 'c-function "invalid syntax" x))))
                            (cast (cdr lst))
                            ((args ...) (generate-temporaries (datum (arg-types ...))))
                            (msg (format "function not available in ~a" (datum lib-name))))
                         (with-syntax
                             (((c-args ...) (c-arguments #'(args ...))))
                           #'(let ((loc (lookup-shared-object lib-handle 'func-name)))
                               (if loc
                                   (let ((func-name (lambda (args ...) (call cast loc c-args ...)))) func-name)
                                   (let ((func-name (lambda x (error 'func-name msg)))) func-name)))))))
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
