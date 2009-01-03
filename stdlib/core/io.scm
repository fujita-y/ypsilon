#!core
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (core io assistants)

  (export port-type
          port-direction
          port-lookup-file-option-code
          port-lookup-buffer-mode-code
          port-lookup-codec-code
          port-lookup-eol-style-code
          port-lookup-error-handling-mode-code
          port-reverse-lookup-codec-code
          port-reverse-lookup-eol-style-code
          port-reverse-lookup-error-handling-mode-code
          make-file-options)

  (import (core primitives) (core enums))

  (define direction-codes
    '((input . 1) (output . 2) (input/output . 3)))

  (define type-codes
    '((file . 1) (bytevector . 2) (custom . 3)))

  (define file-option-codes
    '((no-create . 1) (no-fail . 2) (no-truncate . 4)))

  (define buffer-mode-codes
    '((none . 1) (line . 2) (block . 3)))

  (define codec-codes
    '((latin-1 . 1) (utf-8 . 2) (utf-16 . 3)))

  (define eol-style-codes
    '((none . 1) (lf . 2) (cr . 3) (crlf . 4) (nel . 5) (crnel . 6) (ls . 7)))

  (define error-handling-mode-codes
    '((ignore . 1) (raise . 2) (replace . 3)))

  (define flip (lambda (lst) (map (lambda (e) (cons (cdr e) (car e))) lst)))

  (define flipped-codec-codes (flip codec-codes))

  (define flipped-eol-style-codes (flip eol-style-codes))

  (define flipped-error-handling-mode-codes (flip error-handling-mode-codes))

  (define lookup (lambda (obj alist) (cond ((assq obj alist) => cdr) (else #f))))

  (define-syntax port-type
    (lambda (x)
      (syntax-case x ()
        ((_ type)
         (datum->syntax
          #'k
          (cond ((assq (syntax->datum (syntax type)) type-codes) => cdr)
                (else
                 (syntax-violation 'port-type "invalid port type" x)))))
        (_
         (syntax-violation 'port-type "invalid port type" x)))))

  (define-syntax port-direction
    (lambda (x)
      (syntax-case x (input output)
        ((_ input)
         (datum->syntax #'k (lookup 'input direction-codes)))
        ((_ output)
         (datum->syntax #'k (lookup 'output direction-codes)))
        ((_ input output)
         (datum->syntax #'k (lookup 'input/output direction-codes)))
        (_
         (syntax-violation 'port-direction "invalid port direction" x)))))

  (define port-lookup-file-option-code (lambda (obj) (lookup obj file-option-codes)))
  (define port-lookup-buffer-mode-code (lambda (obj) (lookup obj buffer-mode-codes)))
  (define port-lookup-codec-code (lambda (obj) (lookup obj codec-codes)))
  (define port-lookup-eol-style-code (lambda (obj) (lookup obj eol-style-codes)))
  (define port-lookup-error-handling-mode-code (lambda (obj) (lookup obj error-handling-mode-codes)))
  (define port-reverse-lookup-codec-code (lambda (obj) (lookup obj flipped-codec-codes)))
  (define port-reverse-lookup-eol-style-code (lambda (obj) (lookup obj flipped-eol-style-codes)))
  (define port-reverse-lookup-error-handling-mode-code (lambda (obj) (lookup obj flipped-error-handling-mode-codes)))

  (define make-file-options (enum-set-constructor (make-enumeration (map car file-option-codes))))

  ) ;[end]

(library (core io)

  (export file-options
          buffer-mode
          buffer-mode?
          utf-8-codec
          utf-16-codec
          latin-1-codec
          eol-style
          error-handling-mode
          make-transcoder
          transcoder-codec
          transcoder-eol-style
          transcoder-error-handling-mode
          native-transcoder
          native-eol-style
          bytevector->string
          string->bytevector
          eof-object
          eof-object?
          port?
          port-transcoder
          textual-port?
          binary-port?
          transcoded-port
          port-has-port-position?
          port-position
          port-has-set-port-position!?
          set-port-position!

          close-port
          call-with-port
          input-port?
          port-eof?

          open-file-input-port
          open-bytevector-input-port
          open-string-input-port
          standard-input-port
          current-input-port

          get-u8
          lookahead-u8
          get-bytevector-n
          get-bytevector-n!
          get-bytevector-some
          get-bytevector-all

          get-char
          lookahead-char
          get-string-n
          get-string-n!
          get-string-all
          get-line
          get-datum

          output-port?
          flush-output-port
          output-port-buffer-mode
          open-file-output-port
          open-bytevector-output-port
          call-with-bytevector-output-port
          open-string-output-port
          call-with-string-output-port
          standard-output-port
          standard-error-port
          current-output-port
          current-error-port

          put-u8
          put-bytevector
          put-char
          put-string
          put-datum

          open-file-input/output-port

          ; io simple
          call-with-input-file
          call-with-output-file
          with-input-from-file
          with-output-to-file
          open-input-file
          open-output-file
          close-input-port
          close-output-port
          read-char
          peek-char
          read
          write-char
          newline
          display
          write

          make-custom-binary-input-port
          make-custom-textual-input-port
          make-custom-binary-output-port
          make-custom-textual-output-port
          make-custom-binary-input/output-port
          make-custom-textual-input/output-port

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

          open-temporary-file-port
          format)

  (import (core io assistants)
          (core primitives)
          (core syntax-case)
          (core lists)
          (core conditions)
          (core bytevectors)
          (core optargs)
          (core enums))

  ;; 8.2.2  File options

  (define-syntax file-options
    (lambda (x)
      (syntax-case x ()
        ((_ options ...)
         (let ((lst (syntax->datum (syntax (options ...)))))
           (or (and (list-of-unique-symbols? lst) (for-all port-lookup-file-option-code lst))
               (syntax-violation 'file-options "invalid option" x))
           (syntax (make-file-options '(options ...)))))
        (_
         (syntax-violation 'file-options "invalid syntax" x)))))

  (define-syntax file-options->bits
    (syntax-rules ()
      ((_ x who args)
       (begin
         (or (and (enum-set? x) (enum-set-subset? x (enum-set-universe (file-options))))
             (assertion-violation 'who (format "expected file-options object, but got ~r, as argument 2" x) args))
         (apply + (map (lambda (e) (port-lookup-file-option-code e)) (enum-set->list x)))))))

  ;; 8.2.3  Buffer modes

  (define-syntax buffer-mode
    (lambda (x)
      (syntax-case x ()
        ((_ mode)
         (or (port-lookup-buffer-mode-code (syntax->datum (syntax mode)))
             (syntax-violation 'buffer-mode "invalid buffer mode" x))
         (syntax 'mode))
        (_
         (syntax-violation 'buffer-mode "invalid buffer mode" x)))))

  (define buffer-mode?
    (lambda (mode)
      (and (port-lookup-buffer-mode-code mode) #t)))

  ;; 8.2.4  Transcoders

  (define predefined-utf-8-codec (tuple 'type:codec 'utf-8))
  (define predefined-utf-16-codec (tuple 'type:codec 'utf-16))
  (define predefined-latin-1-codec (tuple 'type:codec 'latin-1))

  (define utf-8-codec (lambda () predefined-utf-8-codec))
  (define utf-16-codec (lambda () predefined-utf-16-codec))
  (define latin-1-codec (lambda () predefined-latin-1-codec))

  (define-syntax eol-style
    (lambda (x)
      (syntax-case x ()
        ((_ style)
         (or (port-lookup-eol-style-code (syntax->datum (syntax style)))
             (syntax-violation 'eol-style "invalid eol style" x))
         (syntax 'style))
        (_
         (syntax-violation 'eol-style "invalid eol style" x)))))

  (define-syntax error-handling-mode
    (lambda (x)
      (syntax-case x ()
        ((_ mode)
         (or (port-lookup-error-handling-mode-code (syntax->datum (syntax mode)))
             (syntax-violation 'error-handling-mode "invalid directive" x))
         (syntax 'mode))
        (_
         (syntax-violation 'error-handling-mode "invalid directive" x)))))

  (define make-transcoder
    (lambda (codec . options)
      (let-optionals options ((eol-style (native-eol-style)) (error-handling-mode 'replace))
        (let ((bv (make-bytevector 3)))
          (bytevector-u8-set! bv 0 (port-lookup-codec-code (tuple-ref codec 1)))
          (bytevector-u8-set! bv 1 (port-lookup-eol-style-code eol-style))
          (bytevector-u8-set! bv 2 (port-lookup-error-handling-mode-code error-handling-mode))
          (tuple 'type:transcoder bv)))))

  (define transcoder-descriptor
    (lambda (transcoder)
      (tuple-ref transcoder 1)))

  (define transcoder-codec
    (lambda (transcoder)
      (case (port-reverse-lookup-codec-code (bytevector-u8-ref (transcoder-descriptor transcoder) 0))
        ((latin-1) (latin-1-codec))
        ((utf-8) (utf-8-codec))
        ((utf-16) (utf-16-codec)))))

  (define transcoder-eol-style
    (lambda (transcoder)
      (port-reverse-lookup-eol-style-code (bytevector-u8-ref (transcoder-descriptor transcoder) 1))))

  (define transcoder-error-handling-mode
    (lambda (transcoder)
      (port-reverse-lookup-error-handling-mode-code (bytevector-u8-ref (transcoder-descriptor transcoder) 2))))

  (define native-transcoder
    (let ((transcoder (tuple 'type:transcoder (native-transcoder-descriptor))))
      (lambda () transcoder)))

  (define native-eol-style (lambda () (transcoder-eol-style (native-transcoder))))

  (define bytevector->string
    (lambda (bytes transcoder)
      (let-values (((out extract) (open-string-output-port)))
        (call-with-port
          (open-bytevector-input-port bytes transcoder)
          (lambda (in)
            (let loop ((c (get-char in)))
              (cond ((eof-object? c) (extract))
                    (else
                     (put-char out c)
                     (loop (get-char in))))))))))

  #; (define string->bytevector
    (lambda (string transcoder)
      (let-values (((out extract) (open-bytevector-output-port transcoder)))
        (call-with-port
         (open-string-input-port string)
         (lambda (in)
           (let loop ((c (get-char in)))
             (cond ((eof-object? c) (extract))
                   (else
                    (put-char out c)
                    (loop (get-char in))))))))))

  (define string->bytevector
    (lambda (string transcoder)
      (let-values (((out extract) (open-bytevector-output-port transcoder)))
        (call-with-port
          (make-string-input-port string)
          (lambda (in)
            (let loop ((c (get-char in)))
              (cond ((eof-object? c) (extract))
                    (else
                     (put-char out c)
                     (loop (get-char in))))))))))

  ;; 8.2.6  Input and output ports

  (define port-transcoder
    (lambda (port)
      (let ((desc (port-transcoder-descriptor port)))
        (and desc
             (if (eq? desc #t)
                 (native-transcoder)
                 (tuple 'type:transcoder desc))))))

  (define textual-port?
    (lambda (port)
      (and (port-transcoder-descriptor port) #t)))

  (define binary-port?
    (lambda (port)
      (not (port-transcoder-descriptor port))))

  (define transcoded-port
    (lambda (port transcoder)
      (make-transcoded-port port (transcoder-descriptor transcoder))))

  ;; 8.2.7  Input ports

  (define open-file-input-port
    (lambda (filename . options)
      (let-optionals options
          ((file-options (file-options))
           (buffer-mode 'block)
           (transcoder #f))
        (open-port (port-type file)
                   (port-direction input)
                   filename
                   (file-options->bits file-options open-file-input-port (cons* filename options))
                   (port-lookup-buffer-mode-code buffer-mode)
                   (and transcoder (transcoder-descriptor transcoder))))))

  (define open-bytevector-input-port
    (lambda (bytes . options)
      (let-optionals options ((transcoder #f))
        (open-port (port-type bytevector)
                   (port-direction input)
                   'bytevector
                   bytes
                   #f
                   (and transcoder (transcoder-descriptor transcoder))))))

  (define open-string-input-port
    (lambda (string)
      (make-string-input-port string)))

  (define make-custom-binary-input-port
    (lambda (id read! get-position set-position! close)
      (open-port (port-type custom)
                 (port-direction input)
                 id
                 (vector #f read! #f get-position set-position! close)
                 #f
                 #f)))

  (define make-custom-textual-input-port
    (lambda (id read! get-position set-position! close)
      (define port)

      (define ht-token (make-core-hashtable 'eqv?))

      (define bv-read!
        (lambda (bv start count)
          (cond ((= count 0) 0)
                (else
                 (let* ((len (div count 4))
                        (str (make-string len #\nul))
                        (count (read! str 0 len)))
                   (cond ((= count 0) 0)
                         (else
                          (let* ((bv-utf8 (string->utf8 (substring str 0 count)))
                                 (count (bytevector-length bv-utf8)))
                            (bytevector-copy! bv-utf8 0 bv start count)
                            count))))))))

      (define bv-get-position
        (lambda (token)
          (core-hashtable-set! ht-token token (get-position))
          token))

      (define bv-set-position!
        (lambda (token)
          (cond ((core-hashtable-ref ht-token token #f)
                 => (lambda (pos) (set-position! pos)))
                ((warning-level)
                 (format (current-error-port) "~&warning in set-port-position: expected return value of a call to get-position, but got ~u~%~!" token)
                 (set-position! token))
                (else
                 (set-position! token)))))

      (set! port (open-port (port-type custom)
                            (port-direction input)
                            id
                            (vector #t (and read! bv-read!) #f (and get-position bv-get-position) (and set-position! bv-set-position!) close)
                            #f
                            #t))
      port))

  ;; 8.2.10  Output ports

  (define open-file-output-port
    (lambda (filename . options)
      (let-optionals options
          ((file-options (file-options))
           (buffer-mode 'block)
           (transcoder #f))
        (open-port (port-type file)
                   (port-direction output)
                   filename
                   (file-options->bits file-options open-file-output-port (cons* filename options))
                   (port-lookup-buffer-mode-code buffer-mode)
                   (and transcoder (transcoder-descriptor transcoder))))))

  (define bytevector-output-port-values
    (lambda (port)
      (values port (lambda () (extract-accumulated-bytevector port)))))

  (define open-bytevector-output-port
    (lambda options
      (let-optionals options ((transcoder #f))
        (bytevector-output-port-values
         (open-port (port-type bytevector)
                    (port-direction output)
                    'bytevector
                    #f
                    #f
                    (and transcoder (transcoder-descriptor transcoder)))))))

  (define call-with-bytevector-output-port
    (lambda (proc . options)
      (let-optionals options ((transcoder #f))
        (let-values (((port extractor) (open-bytevector-output-port transcoder)))
          (dynamic-wind
           (lambda () #f)
           (lambda () (proc port) (extractor))
           (lambda () (close-port port)))))))

  (define call-with-string-output-port
    (lambda (proc)
      (let-values (((port extractor) (open-string-output-port)))
        (dynamic-wind
         (lambda () #f)
         (lambda () (proc port) (extractor))
         (lambda () (close-port port))))))

  (define string-output-port-values
    (lambda (port)
      (values port (lambda () (extract-accumulated-string port)))))

  (define open-string-output-port
    (lambda ()
      (string-output-port-values (make-string-output-port))))

  (define make-custom-binary-output-port
    (lambda (id write! get-position set-position! close)
      (open-port (port-type custom)
                 (port-direction output)
                 id
                 (vector #f #f write! get-position set-position! close)
                 #f
                 #f)))

  (define make-custom-textual-output-port
    (lambda (id write! get-position set-position! close)

      (define port)

      (define ht-token (make-core-hashtable 'eqv?))

      (define bv-write!
        (lambda (bv start count)
          (cond ((= count 0) (write! "" 0 0) 0)
                (else
                 (let ((bv-utf8 (make-bytevector count)))
                   (bytevector-copy! bv start bv-utf8 0 count)
                   (let* ((str (utf8->string bv-utf8))
                          (len (string-length str)))
                     (let ((written (write! str 0 len)))
                       (bytevector-length (string->utf8 (substring str 0 written))))))))))

      (define bv-get-position
        (lambda (token)
          (core-hashtable-set! ht-token token (get-position))
          token))

      (define bv-set-position!
        (lambda (token)
          (cond ((core-hashtable-ref ht-token token #f)
                 => (lambda (pos) (set-position! pos)))
                ((warning-level)
                 (format (current-error-port) "~&warning in set-port-position: expected return value of a call to get-position, but got ~u~%~!" token)
                 (set-position! token))
                (else
                 (set-position! token)))))

      (set! port (open-port (port-type custom)
                            (port-direction output)
                            id
                            (vector #t #f (and write! bv-write!) (and get-position bv-get-position) (and set-position! bv-set-position!) close)
                            #f
                            #t))
      port))

  ;; 8.2.13  Input/output ports

  (define open-file-input/output-port
    (lambda (filename . options)
      (let-optionals options
          ((file-options (file-options))
           (buffer-mode 'block)
           (transcoder #f))
        (open-port (port-type file)
                   (port-direction input output)
                   filename
                   (file-options->bits file-options open-file-input/output-port (cons* filename options))
                   (port-lookup-buffer-mode-code buffer-mode)
                   (and transcoder (transcoder-descriptor transcoder))))))

  (define make-custom-binary-input/output-port
    (lambda (id read! write! get-position set-position! close)
      (open-port (port-type custom)
                 (port-direction input output)
                 id
                 (vector #f read! write! get-position set-position! close)
                 #f
                 #f)))

  (define make-custom-textual-input/output-port
    (lambda (id read! write! get-position set-position! close)

      (define port)

      (define ht-token (make-core-hashtable 'eqv?))

      (define bv-read!
        (lambda (bv start count)
          (cond ((= count 0) 0)
                (else
                 (let* ((len (div count 4))
                        (str (make-string len #\nul))
                        (count (read! str 0 len)))
                   (cond ((= count 0) 0)
                         (else
                          (let* ((bv-utf8 (string->utf8 (substring str 0 count)))
                                 (count (bytevector-length bv-utf8)))
                            (bytevector-copy! bv-utf8 0 bv start count)
                            count))))))))

      (define bv-write!
        (lambda (bv start count)
          (cond ((= count 0) (write! "" 0 0) 0)
                (else
                 (let ((bv-utf8 (make-bytevector count)))
                   (bytevector-copy! bv start bv-utf8 0 count)
                   (let* ((str (utf8->string bv-utf8))
                          (len (string-length str)))
                     (let ((written (write! str 0 len)))
                       (bytevector-length (string->utf8 (substring str 0 written))))))))))

      (define bv-get-position
        (lambda (token)
          (core-hashtable-set! ht-token token (get-position))
          token))

      (define bv-set-position!
        (lambda (token)
          (cond ((core-hashtable-ref ht-token token #f)
                 => (lambda (pos) (set-position! pos)))
                ((warning-level)
                 (format (current-error-port) "~&warning in set-port-position: expected return value of a call to get-position, but got ~u~%~!" token)
                 (set-position! token))
                (else
                 (set-position! token)))))

      (set! port (open-port (port-type custom)
                            (port-direction input output)
                            id
                            (vector #t (and read! bv-read!) (and write! bv-write!) (and get-position bv-get-position) (and set-position! bv-set-position!) close)
                            #f
                            #t))
      port))

  ;; 8.3  Simple I/O

  (define call-with-input-file
    (lambda (filename proc)
      (call-with-port (open-input-file filename) proc)))

  (define call-with-output-file
    (lambda (filename proc)
      (call-with-port (open-output-file filename) proc)))

  (define with-input-from-file
    (lambda (filename thunk)
      (let ((port (open-input-file filename)) (save (current-input-port)))
        (dynamic-wind
         (lambda () (set-current-input-port! port))
         (lambda () (let ((ans (thunk))) (close-input-port port) ans))
         (lambda () (set-current-input-port! save))))))

  (define with-output-to-file
    (lambda (filename thunk)
      (let ((port (open-output-file filename)) (save (current-output-port)))
        (dynamic-wind
         (lambda () (set-current-output-port! port))
         (lambda () (let ((ans (thunk))) (close-output-port port) ans))
         (lambda () (set-current-output-port! save))))))

  (define open-input-file
    (lambda (filename)
      (open-file-input-port filename (file-options) (buffer-mode block) (native-transcoder))))

  (define open-output-file
    (lambda (filename)
      (open-file-output-port filename (file-options) (buffer-mode block) (native-transcoder))))

  (define close-input-port
    (lambda (port)
      (close-port port)))

  (define close-output-port
    (lambda (port)
      (close-port port)))

  ;; extension

  (define open-temporary-file-port
    (lambda options
      (let-optionals options ((name "temporary file") (transcoder #f))
        (make-temporary-file-port name (and transcoder (transcoder-descriptor transcoder))))))

  ) ;[end]
