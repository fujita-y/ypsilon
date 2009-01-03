;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(define immutable-primitives (make-core-hashtable))
(define coreform-primitives '(begin quote define set! lambda let letrec* if or and))

(let ()

  (define setup-intrinsic-procs
    (lambda (name lst)
      (copy-environment-variables! (system-environment) (interaction-environment) lst)
      (let ((ht (scheme-library-exports))
            (id (generate-library-id name))
            (aliases (map core-primitive-name lst)))
        (for-each (lambda (a e)
                    (set-top-level-value! a (top-level-value e))
                    (core-hashtable-set! immutable-primitives a #t))
                  aliases lst)
        (copy-environment-variables! (system-environment) (interaction-environment) aliases)
        (core-hashtable-set! ht id (unify-import-bindings
                                    (append (map (lambda (a e) (cons e (make-import a))) aliases lst)
                                            (core-hashtable-ref ht id '())))))))

  (define setup-intrinsic-macros
    (lambda (name lst)
      (let ((ht (scheme-library-exports))
            (id (generate-library-id name))
            (aliases (map core-primitive-name lst)))
        (copy-environment-macros! (system-environment) (interaction-environment) lst)
        (copy-environment-macros! (system-environment) (interaction-environment) aliases)
        (core-hashtable-set! ht id (unify-import-bindings
                                    (append (map (lambda (a e) (cons e (make-import a))) aliases lst)
                                            (core-hashtable-ref ht id '())))))))

  (define setup-core-primitive-procs
    (lambda (name lst)
      (let ((ht (scheme-library-exports))
            (id (generate-library-id name))
            (aliases (map core-primitive-name lst)))
        (for-each (lambda (a e)
                    (set-top-level-value! a (top-level-value e))
                    (core-hashtable-set! immutable-primitives a #t))
                  aliases lst)
        (copy-environment-variables! (system-environment) (interaction-environment) aliases)
        (core-hashtable-set! ht id (unify-import-bindings
                                    (append (map (lambda (a e) (cons e (make-import a))) aliases lst)
                                            (core-hashtable-ref ht id '())))))))

  (define setup-core-primitive-macros
    (lambda (name lst)
      (let ((ht (scheme-library-exports))
            (id (generate-library-id name))
            (aliases (map core-primitive-name lst)))
        (for-each (lambda (a e)
                    (cond ((core-hashtable-ref (current-macro-environment) e #f)
                           => (lambda (macro)
                                (core-hashtable-set! (current-macro-environment) a macro)))))
                  aliases lst)
        (copy-environment-macros! (system-environment) (interaction-environment) aliases)
        (core-hashtable-set! ht id (unify-import-bindings
                                    (append (map (lambda (a e) (cons e (make-import a))) aliases lst)
                                            (core-hashtable-ref ht id '())))))))

  (define compound-exports
    (lambda (target source)
      (let ((ht (scheme-library-exports)) (to (generate-library-id target)) (from (generate-library-id source)))
        (core-hashtable-set! ht to (append (core-hashtable-ref ht to '())
                                           (core-hashtable-ref ht from '()))))))

  (setup-intrinsic-macros
   '(core intrinsics) '( library define define-syntax
                         quote lambda if set!
                         cond case and or
                         let let* letrec letrec* let-values let*-values
                         begin
                         quasiquote unquote unquote-splicing
                         let-syntax letrec-syntax syntax-rules
                         identifier-syntax
                         assert
                         else => ... _))

  (setup-intrinsic-procs
   '(core intrinsics) '( eq?
                         eqv?
                         equal?
                         procedure?
                         number? complex? real? rational? integer?
                         real-valued? rational-valued? integer-valued?
                         exact? inexact?
                         inexact exact
                         = < > <= >=
                         zero? positive? negative? odd? even?
                         finite? infinite? nan?
                         max min + * - / abs
                         div-and-mod div mod div0-and-mod0 div0 mod0
                         gcd lcm numerator denominator
                         floor ceiling truncate round
                         rationalize
                         exp log sin cos tan asin acos atan
                         sqrt
                         exact-integer-sqrt
                         expt
                         make-rectangular make-polar real-part imag-part
                         magnitude angle
                         number->string string->number
                         not boolean? boolean=?
                         pair? cons car cdr
                         caar cadr cdar cddr caaar caadr cadar
                         caddr cdaar cdadr cddar cdddr caaaar caaadr
                         caadar caaddr cadaar cadadr caddar cadddr cdaaar
                         cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
                         null? list? list length append reverse list-tail
                         list-ref map for-each
                         symbol? symbol=? symbol->string string->symbol
                         char? char->integer integer->char
                         char=? char<? char>? char<=? char>=?
                         string? make-string string string-length string-ref
                         string=? string<? string>? string<=? string>=?
                         substring string-append string->list list->string string-copy string-for-each
                         vector? make-vector vector vector-length vector-ref vector-set!
                         vector->list list->vector vector-fill!
                         vector-map vector-for-each
                         error assertion-violation
                         apply call-with-current-continuation call/cc
                         values call-with-values dynamic-wind))

  (setup-core-primitive-macros
   '(core primitives) '( do
                           syntax-case
                         syntax
                         define-macro))

  (setup-core-primitive-procs
   '(core primitives) '(; eval
                        environment
                        eval
                        ; arith flonum
                        flonum?
                        real->flonum
                        fl=?
                        fl<?
                        fl>?
                        fl<=?
                        fl>=?
                        flinteger?
                        flzero?
                        flpositive?
                        flnegative?
                        flodd?
                        fleven?
                        flfinite?
                        flinfinite?
                        flnan?
                        flmax
                        flmin
                        fl+
                        fl*
                        fl-
                        fl/
                        fldiv
                        fldiv0
                        flnumerator
                        fldenominator
                        flfloor
                        flceiling
                        fltruncate
                        flround
                        flexp
                        flexpt
                        fllog
                        flsin
                        flcos
                        fltan
                        flasin
                        flacos
                        flatan
                        flabs
                        flsqrt
                        fixnum->flonum
                        ; arith fixnum
                        fixnum?
                        fixnum-width
                        least-fixnum
                        greatest-fixnum
                        fx=?
                        fx<?
                        fx>?
                        fx<=?
                        fx>=?
                        fxzero?
                        fxpositive?
                        fxnegative?
                        fxodd?
                        fxeven?
                        fxmax
                        fxmin
                        fx+
                        fx*
                        fx-
                        fxdiv
                        fxdiv0
                        fxnot
                        fxand
                        fxior
                        fxxor
                        fxif
                        fxbit-count
                        fxlength
                        fxfirst-bit-set
                        fxbit-set?
                        fxcopy-bit
                        fxarithmetic-shift
                        fxarithmetic-shift-left
                        fxarithmetic-shift-right
                        fxbit-field
                        fxcopy-bit-field
                        &no-infinities make-no-infinities-violation no-infinities-violation?
                        &no-nans make-no-nans-violation no-nans-violation?
                        ; arith bitwise
                        bitwise-not
                        bitwise-and
                        bitwise-ior
                        bitwise-xor
                        bitwise-arithmetic-shift
                        bitwise-first-bit-set
                        bitwise-length
                        bitwise-bit-count
                        ; r6rs syntax-case
                        make-variable-transformer
                        identifier? bound-identifier=? free-identifier=?
                        datum->syntax syntax->datum
                        generate-temporaries
                        syntax-violation
                        ; r6rs lists
                        memq memv member
                        assq assv assoc
                        cons*
                        list-head
                        ; r6rs exceptions
                        raise raise-continuable with-exception-handler
                        ; r6rs records
                        record?
                        record-rtd
                        record-type-name
                        record-type-parent
                        record-type-uid
                        record-type-generative?
                        record-type-sealed?
                        record-type-opaque?
                        record-type-field-names
                        record-field-mutable?
                        make-record-type-descriptor
                        record-type-descriptor?
                        make-record-constructor-descriptor
                        record-constructor
                        record-predicate
                        record-accessor
                        record-mutator
                        make-record-type
                        record-type?
                        record-type-rtd
                        record-type-rcd
                        ; r6rs conditions
                        condition
                        simple-conditions
                        condition?
                        condition-predicate
                        condition-accessor
                        &condition
                        &message make-message-condition message-condition? condition-message
                        &warning make-warning warning?
                        &serious make-serious-condition serious-condition?
                        &error make-error error?
                        &violation make-violation violation?
                        &assertion make-assertion-violation assertion-violation?
                        &irritants make-irritants-condition irritants-condition? condition-irritants
                        &who make-who-condition who-condition? condition-who
                        &non-continuable make-non-continuable-violation non-continuable-violation?
                        &implementation-restriction make-implementation-restriction-violation implementation-restriction-violation?
                        &lexical make-lexical-violation lexical-violation?
                        &syntax make-syntax-violation syntax-violation? syntax-violation-form syntax-violation-subform
                        &undefined make-undefined-violation undefined-violation?
                        ; r6rs mutable-pairs
                        set-car!
                        set-cdr!
                        ; r6rs mutable-strings
                        string-set!
                        string-fill!
                        ; r6rs r5rs
                        quotient
                        remainder
                        modulo
                        ; r6rs unicode
                        char-whitespace?
                        ; r6rs i/o simple
                        display
                        write
                        newline
                        read-char
                        write-char
                        ; r6rs i/o ports
                        call-with-port
                        eof-object eof-object?
                        standard-input-port standard-output-port standard-error-port
                        current-input-port current-output-port current-error-port
                        input-port? output-port? port?
                        flush-output-port
                        output-port-buffer-mode
                        close-port
                        native-transcoder-descriptor
                        port-transcoder-descriptor
                        extract-accumulated-bytevector
                        extract-accumulated-string
                        get-accumulated-string
                        open-port
                        nonblock-byte-ready?
                        lookahead-char
                        get-char
                        port-has-port-position?
                        port-position
                        port-has-set-port-position!?
                        set-port-position!
                        port-eof?
                        get-u8
                        lookahead-u8
                        get-bytevector-n
                        get-bytevector-n!
                        get-bytevector-all
                        get-bytevector-some
                        get-string-n
                        get-string-n!
                        get-string-all
                        get-line
                        get-datum
                        put-u8
                        put-bytevector
                        put-char
                        put-string
                        put-datum
                        &i/o make-i/o-error i/o-error?
                        &i/o-read make-i/o-read-error i/o-read-error?
                        &i/o-write make-i/o-write-error i/o-write-error?
                        &i/o-invalid-position make-i/o-invalid-position-error i/o-invalid-position-error? i/o-error-position
                        &i/o-filename make-i/o-filename-error i/o-filename-error? i/o-error-filename
                        &i/o-file-protection make-i/o-file-protection-error i/o-file-protection-error?
                        &i/o-file-is-read-only make-i/o-file-is-read-only-error i/o-file-is-read-only-error?
                        &i/o-file-already-exists make-i/o-file-already-exists-error i/o-file-already-exists-error?
                        &i/o-file-does-not-exist make-i/o-file-does-not-exist-error i/o-file-does-not-exist-error?
                        &i/o-port make-i/o-port-error i/o-port-error? i/o-error-port
                        &i/o-decoding make-i/o-decoding-error i/o-decoding-error?
                        &i/o-encoding make-i/o-encoding-error i/o-encoding-error? i/o-encoding-error-char
                        ; r6rs files
                        file-exists?
                        delete-file
                        ; r6rs hashtables
                        string-hash
                        symbol-hash
                        equal-hash
                        ; r6rs programs
                        command-line
                        exit
                        ; r6rs bytevector
                        native-endianness
                        bytevector?
                        make-bytevector
                        bytevector-length
                        bytevector=?
                        bytevector-fill!
                        bytevector-copy!
                        bytevector-copy
                        bytevector-u8-ref
                        bytevector-s8-ref
                        bytevector-u8-set!
                        bytevector-s8-set!
                        bytevector->u8-list
                        u8-list->bytevector
                        bytevector-u16-ref bytevector-s16-ref bytevector-u16-native-ref bytevector-s16-native-ref
                        bytevector-u16-set! bytevector-s16-set! bytevector-u16-native-set! bytevector-s16-native-set!
                        bytevector-u32-ref bytevector-s32-ref bytevector-u32-native-ref bytevector-s32-native-ref
                        bytevector-u32-set! bytevector-s32-set! bytevector-u32-native-set! bytevector-s32-native-set!
                        bytevector-u64-ref bytevector-s64-ref bytevector-u64-native-ref bytevector-s64-native-ref
                        bytevector-u64-set! bytevector-s64-set! bytevector-u64-native-set! bytevector-s64-native-set!
                        bytevector-ieee-single-ref bytevector-ieee-single-native-ref
                        bytevector-ieee-single-set! bytevector-ieee-single-native-set!
                        bytevector-ieee-double-ref bytevector-ieee-double-native-ref
                        bytevector-ieee-double-set! bytevector-ieee-double-native-set!
                        string->utf8
                        utf8->string
                        ; extensions
                        put-byte
                        make-string-output-port
                        make-string-input-port
                        make-transcoded-port
                        make-temporary-file-port
                        port-device-subtype
                        core-eval
                        command-line-shift
                        unspecified
                        unspecified?
                        generate-temporary-symbol
                        circular-list?
                        cyclic-object?
                        list-transpose
                        list-transpose+
                        list-transpose*
                        list-copy
                        vector-copy
                        make-parameter
                        gensym
                        format
                        pretty-print
                        pretty-print-line-length
                        pretty-print-initial-indent
                        pretty-print-maximum-lines
                        pretty-print-unwrap-syntax
                        peek-char
                        read
                        read-with-shared-structure
                        write-with-shared-structure
                        tuple tuple? make-tuple tuple-ref tuple-set! tuple-length tuple-index tuple->list
                        make-weak-mapping weak-mapping? weak-mapping-key weak-mapping-value
                        make-core-hashtable core-hashtable? make-shared-core-hashtable
                        make-weak-core-hashtable weak-core-hashtable? make-weak-shared-core-hashtable
                        core-hashtable-contains?
                        core-hashtable-ref core-hashtable-set! core-hashtable-delete! core-hashtable-clear!
                        core-hashtable->alist core-hashtable-size
                        core-hashtable-copy
                        core-hashtable-mutable?
                        core-hashtable-equivalence-function
                        core-hashtable-hash-function
                        usleep
                        scheme-error
                        architecture-feature
                        load-shared-object
                        lookup-shared-object
                        call-shared-object->void
                        call-shared-object->int
                        call-shared-object->intptr
                        call-shared-object->double
                        call-shared-object->char*
                        stdcall-shared-object->void
                        stdcall-shared-object->int
                        stdcall-shared-object->intptr
                        stdcall-shared-object->double
                        stdcall-shared-object->char*
                        make-callback
                        flonum->float
                        shared-object-errno
                        shared-object-win32-lasterror
                        bytevector-c-short-ref bytevector-c-unsigned-short-ref bytevector-c-short-set!
                        bytevector-c-int-ref bytevector-c-unsigned-int-ref bytevector-c-int-set!
                        bytevector-c-long-ref bytevector-c-unsigned-long-ref bytevector-c-long-set!
                        bytevector-c-void*-ref bytevector-c-void*-set!
                        bytevector-c-int8-set! bytevector-c-int16-set! bytevector-c-int32-set! bytevector-c-int64-set!
                        collect collect-notify collect-stack-notify collect-trip-bytes display-heap-statistics display-object-statistics
                        backtrace expansion-backtrace backtrace-line-length display-backtrace
                        warning-level
                        restricted-print-line-length
                        record-print-nesting-limit
                        macro-expand compile compile-coreform closure-code
                        current-environment current-macro-environment current-variable-environment current-dynamic-environment
                        system-environment interaction-environment
                        make-environment
                        copy-environment-variables! copy-environment-macros!
                        top-level-bound? top-level-value set-top-level-value!
                        core-read
                        current-source-comments
                        current-after-expansion-hook
                        string-contains symbol-contains subr?
                        make-bytevector-mapping
                        scheme-library-exports
                        scheme-library-paths
                        scheme-load-paths
                        scheme-load-verbose
                        add-load-path
                        add-library-path
                        library-extensions
                        auto-compile-verbose
                        auto-compile-cache
                        directory-list
                        current-directory
                        create-directory
                        home-directory
                        decode-flonum
                        load
                        system-share-path
                        lookup-process-environment
                        process-environment->alist
                        set-current-input-port! set-current-output-port! set-current-error-port!
                        open-builtin-data-input-port
                        current-library-infix
                        current-library-suffix
                        current-primitive-prefix
                        current-rename-delimiter
                        string->uninterned-symbol
                        uninterned-symbol?
                        uninterned-symbol-prefix
                        uninterned-symbol-suffix

                        socket?
                        make-socket socket-shutdown socket-close socket->port socket-port
                        socket-send socket-recv socket-accept
                        shutdown-output-port
                        port-closed?

                        getenv
                        gethostname
                        system
                        process
                        process-wait
                        process-shell-command

                        ;default-exception-handler
                        current-exception-printer

                        make-shared-queue
                        shared-queue?
                        shared-queue-shutdown
                        shared-queue-push!
                        shared-queue-pop!
                        make-shared-bag
                        shared-bag?
                        shared-bag-put!
                        shared-bag-get!

                        timeout-object?
                        shutdown-object?

                        spawn
                        thread-id
                        on-primordial-thread?
                        display-thread-status

                        make-uuid
                        time-usage
                        microsecond
                        microsecond->utc
                        microsecond->string
                        decode-microsecond
                        encode-microsecond
                        ))

  (compound-exports '(core primitives) '(core intrinsics)))

;; c stub
(define .@assertion-violation assertion-violation)
(define .@undefined-violation undefined-violation)
(define .@lexical-violation lexical-violation)
(define .@error error)
(define .@implementation-restriction-violation implementation-restriction-violation)
(define .@raise-i/o-read-error raise-i/o-read-error)
(define .@raise-i/o-write-error raise-i/o-write-error)
(define .@raise-i/o-file-protection-error raise-i/o-file-protection-error)
(define .@raise-i/o-file-is-read-only-error raise-i/o-file-is-read-only-error)
(define .@raise-i/o-file-already-exists-error raise-i/o-file-already-exists-error)
(define .@raise-i/o-file-does-not-exist-error raise-i/o-file-does-not-exist-error)
(define .@raise-i/o-decoding-error raise-i/o-decoding-error)
(define .@raise-i/o-encoding-error raise-i/o-encoding-error)
(define .@raise-i/o-invalid-position-error raise-i/o-invalid-position-error)
(define .@raise-i/o-filename-error raise-i/o-filename-error)
(define .@raise-i/o-error raise-i/o-error)
(define .@perform-dynamic-wind perform-dynamic-wind)
(define .@start-scheme-session start-scheme-session)
(define .@apply-scheme-proc-assistant apply-scheme-proc-assistant);
(define .@pretty-print pretty-print)

;; for debug
(set-top-level-value! '.run-vmi run-vmi)

;; top-level procs
(let ((coreform-private-procs
       '(.set-top-level-macro!
         .require-scheme-library .intern-scheme-library .unintern-scheme-library
         .vars .syntax-dispatch .flatten-syntax .transformer-thunk
         .syntax/i0n .syntax/i1n .syntax/i2n .syntax/i3n
         .syntax/c0n .syntax/c1n .syntax/c2n .syntax/c3n
         .syntax/i0e .syntax/i1e .syntax/i2e .syntax/i3e
         .syntax/c0e .syntax/c1e .syntax/c2e .syntax/c3e
         .run-vmi)))
  (for-each (lambda (a) (core-hashtable-set! immutable-primitives a #t)) coreform-private-procs)
  (copy-environment-variables! (system-environment) (interaction-environment) coreform-private-procs))

;; top-level macro
(copy-environment-macros! (system-environment) (interaction-environment) '(import))
(set! immutable-primitives (core-hashtable-copy immutable-primitives))
