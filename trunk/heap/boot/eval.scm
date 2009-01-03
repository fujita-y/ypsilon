;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA /  LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(define auto-compile-cache-validation-signature (string->symbol (format "ypsilon-0.9.6/r~a" (architecture-feature 'ypsilon-revision))))
(define auto-compile-verbose (make-parameter #f))
(define scheme-load-verbose (make-parameter #f))
(define scheme-load-paths (make-parameter '()))
(define library-contains-implicit-main (make-parameter #t))

(define library-extensions
  (make-parameter
   (list ".ypsilon.sls"
         ".ypsilon.ss"
         ".ypsilon.scm"
         ".sls"
         ".ss"
         ".scm")
   (lambda (value)
     (if (and (list? value) (every1 string? value))
         value
         (assertion-violation 'library-extensions (format "expected list of strings, but got ~s" value))))))

(define auto-compile-cache
  (make-parameter
   #f
   (lambda (p)
     (cond ((not p) p)
           ((and (string? p) (file-exists? (format "~//." p))) (format "~/" p))
           (else
            (format (current-error-port) "~&;; warning in auto-compile-cache: directory ~s not exist (temporary disable caching)~!~%" p)
            #f)))))

(define core-eval
  (lambda (form)
    (parameterize ((backtrace #f))
      (interpret-coreform form))))

(define compile
  (lambda (form)
    (parameterize
        ((current-closure-comments (make-core-hashtable)))
      (compile-coreform (macro-expand form)))))

(define interpret
  (lambda (form)
    (let ((code
           (parameterize
               ((current-closure-comments (make-core-hashtable)))
             (compile-coreform (macro-expand form)))))
      (run-vmi (cons '(1 . 0) code)))))

(define interpret-coreform
  (lambda (form)
    (let ((code (cons '(1 . 0) (compile-coreform form))))
      (run-vmi code))))

(define environment
  (lambda ref
    (parse-imports `(environment ,@ref) ref)
    (tuple 'type:eval-environment `,ref)))

(define eval
  (lambda (expr env)
    (or (on-primordial-thread?) (assertion-violation 'thread "invalid use of eval" expr env))
    (cond ((environment? env)
           (parameterize ((current-environment env))
             (interpret expr)))
          (else
           (or (eq? (tuple-ref env 0) 'type:eval-environment)
               (assertion-violation 'eval (format "expected environment, but got ~r, as argument 2" env)))
           (interpret `(begin
                         (library (.R6RS-EVAL)
                           (export)
                           (import (rename (only (core primitives)
                                                 set-top-level-value!
                                                 string->symbol)
                                           (set-top-level-value! .SET-TOP-LEVEL-VALUE!)
                                           (string->symbol .STRING->SYMBOL))
                                   ,@(tuple-ref env 1))
                           (.SET-TOP-LEVEL-VALUE! (.STRING->SYMBOL ".R6RS-EVAL-RESULT") ,expr))
                         (let ((result .R6RS-EVAL-RESULT))
                           (.set-top-level-value! '.R6RS-EVAL-RESULT .&UNDEF)
                           (.unintern-scheme-library ',(generate-library-id '(.R6RS-EVAL)))
                           result)))))))

(define expand-path
  (lambda (path)
    (let ((special (and (> (string-length path) 1) (memq (string-ref path 1) '(#\/ #\\)))))
      (cond ((and special (home-directory) (char=? (string-ref path 0) #\~))
             (format "~a~/" (home-directory) (substring path 1 (string-length path))))
            ((and special (char=? (string-ref path 0) #\.))
             (format "~a~/" (current-directory) (substring path 1 (string-length path))))
            ((string=? path ".")
             (format "~/" (current-directory)))
            ((and (home-directory) (string=? path "~"))
             (format "~/" (home-directory)))
            (else
             (format "~/" path))))))

(define locate-load-file
  (lambda (path)

    (define path-not-found
      (lambda (path)
        (assertion-violation 'load (format "~a~/~a not found" #\" path #\"))))

    (define confirm-path
      (lambda (path)
        (and (file-exists? path) path)))

    (cond ((= (string-length path) 0)
           (path-not-found path))
          ((or (string-contains path ":") (memq (string-ref path 0) '(#\/ #\\)))
           (or (confirm-path path) (path-not-found path)))
          ((memq (string-ref path 0) '(#\. #\~))
           (or (confirm-path (expand-path path)) (path-not-found path)))
          ((any1 (lambda (e) (confirm-path (expand-path (string-append e "/" path))))
                 (cons "." (scheme-load-paths))))
          (else
           (path-not-found path)))))

(define load-file-has-r6rs-comment?
  (lambda (path)
    (parameterize ((extend-lexical-syntax #t) (mutable-literals #f))
      (let ((port (open-script-input-port (locate-load-file path))))
        (core-read port #f 'load)
        (close-port port)
        (not (extend-lexical-syntax))))))

(define load
  (lambda (path)
    (or (on-primordial-thread?) (assertion-violation 'thread "invalid use of load" path))
    (cond ((list? path)
           (auto-compile-cache-update)
           (load-scheme-library path))
          (else
           (let ((abs-path (locate-load-file path)))
             (and (scheme-load-verbose) (format #t "~&;; loading ~s~%~!" abs-path))
             (let ((port (open-script-input-port abs-path)))
               (with-exception-handler
                (lambda (c)
                  (cond ((serious-condition? c)
                         (close-port port)
                         (raise c))
                        (else
                         (raise-continuable c))))
                (lambda ()
                  (parameterize
                      ((current-source-comments (current-source-comments))
                       (current-temporaries (current-temporaries))
                       (current-environment (current-environment))
                       (extend-lexical-syntax (extend-lexical-syntax))
                       (mutable-literals (mutable-literals))
                       (backtrace (backtrace)))
                    (current-temporaries (make-core-hashtable 'string=?))
                    (current-rename-count 0)
                    (let loop ()
                      (current-source-comments (and (backtrace) (make-core-hashtable)))
                      (let ((form (core-read port (current-source-comments) 'load)))
                        (cond ((eof-object? form)
                               (close-port port))
                              (else
                               (interpret form)
                               (loop))))))))))))))

(define load-r6rs
  (lambda (path)
    (let ((abs-path (locate-load-file path)))
      (and (scheme-load-verbose) (format #t "~&;; loading ~s~%~!" abs-path))
      (let ((port (open-script-input-port abs-path)))
        (with-exception-handler
         (lambda (c)
           (cond ((serious-condition? c)
                  (close-port port)
                  (raise c))
                 (else
                  (raise-continuable c))))
         (lambda ()
           (parameterize
               ((current-source-comments (current-source-comments))
                (current-temporaries (current-temporaries))
                (current-environment (current-environment))
                (extend-lexical-syntax (extend-lexical-syntax))
                (mutable-literals (mutable-literals))
                (backtrace (backtrace)))
             (current-source-comments (and (backtrace) (make-core-hashtable)))
             (current-temporaries (make-core-hashtable 'string=?))
             (current-rename-count 0)
             (let loop ((acc '()))
               (let ((form (core-read port (current-source-comments) 'load)))
                 (cond ((eof-object? form)
                        (close-port port)
                        (let ((program (expand-top-level-program (reverse acc) '())))
                          (current-macro-expression #f)
                          (interpret program)))
                       (else
                        (loop (cons form acc)))))))))))))

(define load-cache
  (lambda (path)
    (and (scheme-load-verbose) (format #t "~&;; loading ~s~%~!" path))
    (let ((port (open-script-input-port path)))
      (with-exception-handler
       (lambda (c)
         (close-port port)
         (raise c))
       (lambda ()
         (parameterize
             ((backtrace #f)
              (current-source-comments #f)
              (current-temporaries (make-core-hashtable 'string=?))
              (current-rename-count 0)
              (current-environment (current-environment)))
           (let loop ()
             (let ((form (core-read port #f 'load)))
               (cond ((eof-object? form)
                      (close-port port))
                     (else
                      (run-vmi (cons '(1 . 0) form))
                      (loop)))))))))))

(define auto-compile-cache-clean
  (lambda ()
    (cond ((auto-compile-cache)
           => (lambda (path)
                (let ((cache-lst (filter (lambda (s)
                                           (let ((p (string-contains s ".cache")))
                                             (and p (= (string-contains s ".cache") (- (string-length s) 6)))))
                                         (directory-list path))))
                  (for-each (lambda (cache-name)
                              (let* ((cache-path (string-append (auto-compile-cache) "/" cache-name))
                                     (timestamp-path (string-append cache-path ".time")))
                                (and (file-exists? cache-path) (delete-file cache-path))
                                (and (file-exists? timestamp-path) (delete-file timestamp-path))
                                (and (auto-compile-verbose) (format #t "~&;; clean ~s~%" cache-path))))
                            cache-lst)))))))

(define auto-compile-cache-update
  (lambda ()

    (define inconsistent-cache-state
      (lambda (cache-lst)
        (and (auto-compile-verbose)
             (format (current-error-port) "~&;; reset ~s~%" (auto-compile-cache)))
        (for-each (lambda (cache-name)
                    (let* ((cache-path (string-append (auto-compile-cache) "/" cache-name))
                           (timestamp-path (string-append cache-path ".time")))
                      (and (file-exists? cache-path) (delete-file cache-path))
                      (and (file-exists? timestamp-path) (delete-file timestamp-path))
                      (and (auto-compile-verbose) (format #t "~&;; clean ~s~%" cache-path))))
                  cache-lst)))

    (cond ((auto-compile-cache)
           => (lambda (path)
                (let ((cache-lst (filter (lambda (s)
                                           (let ((p (string-contains s ".cache")))
                                             (and p (= (string-contains s ".cache") (- (string-length s) 6)))))
                                         (directory-list path))))
                  (let loop ((lst cache-lst) (expiration #f))
                    (cond ((null? lst)
                           (and expiration
                                (for-each (lambda (cache-name)
                                            (let* ((cache-path (string-append (auto-compile-cache) "/" cache-name))
                                                   (timestamp-path (string-append cache-path ".time")))
                                              (cond ((file-exists? timestamp-path)
                                                     (call-with-port
                                                       (make-file-input-port timestamp-path)
                                                       (lambda (timestamp-port)
                                                         (let ((cache-timestamp (get-datum timestamp-port)))
                                                           (close-port timestamp-port)
                                                           (cond ((>= cache-timestamp expiration)
                                                                  (delete-file timestamp-path)
                                                                  (delete-file cache-path)
                                                                  (and (auto-compile-verbose) (format #t "~&;; clean ~s~%" cache-path))))))))
                                                    (else
                                                     (inconsistent-cache-state cache-lst)))))
                                          cache-lst))
                           (unspecified))
                          (else
                           (let* ((cache-path (string-append (auto-compile-cache) "/" (car lst)))
                                  (timestamp-path (string-append cache-path ".time")))
                             (cond ((file-exists? timestamp-path)
                                    (call-with-port
                                      (make-file-input-port timestamp-path)
                                      (lambda (timestamp-port)
                                        (let* ((cache-timestamp (get-datum timestamp-port))
                                               (source-timestamp (get-datum timestamp-port))
                                               (source-path (get-datum timestamp-port))
                                               (cache-signature (get-datum timestamp-port)))
                                          (close-port timestamp-port)
                                          (cond ((not (and (number? cache-timestamp) (number? source-timestamp) (string? source-path) (eq? cache-signature auto-compile-cache-validation-signature) (file-exists? source-path)))
                                                 (inconsistent-cache-state cache-lst))
                                                ((= (stat-mtime source-path) source-timestamp)
                                                 (loop (cdr lst) expiration))
                                                (else
                                                 (loop (cdr lst)
                                                       (cond ((not expiration) cache-timestamp)
                                                             ((< cache-timestamp expiration) cache-timestamp)
                                                             (else expiration)))))))))
                                   (else
                                    (delete-file cache-path)
                                    (and (auto-compile-verbose) (format #t "~&;; clean ~s~%" cache-path))
                                    (loop (cdr lst) expiration))))))))))
          (else
           (unspecified)))))

(define load-scheme-library
  (lambda (ref . vital)
    (let ((vital (or (not (pair? vital)) (car vital)))) ; vital default #t

      (define encode-library-ref
        (lambda (ref)
          (map (lambda (s)
                 (let ((utf8 (string->utf8 (symbol->string s))))
                   (let ((buf (make-string-output-port))
                         (end (bytevector-length utf8)))
                     (let loop ((i 0))
                       (if (= i end)
                           (string->symbol (extract-accumulated-string buf))
                           (let ((c (bytevector-u8-ref utf8 i)))
                             (cond ((or (and (> c #x60) (< c #x7b))       ; a-z
                                        (and (> c #x2f) (< c #x3a))       ; 0-9
                                        (and (> c #x40) (< c #x5b))       ; A-Z
                                        (= c #x2b) (= c #x2d) (= c #x5f)) ; + - _
                                    (put-byte buf c)
                                    (loop (+ i 1)))
                                   (else
                                    (format buf "%~x~x" (div c 16) (mod c 16))
                                    (loop (+ i 1))))))))))
               ref)))

      (define make-cache
        (lambda (src dst source-ref source-time)
          (let ((cyclic-code #f))
            (call-with-port
              (make-file-output-port dst)
              (lambda (output)
                (call-with-port
                  (open-script-input-port src)
                  (lambda (input)
                    (with-exception-handler
                     (lambda (c)
                       (close-port input)
                       (close-port output)
                       (and (file-exists? dst) (delete-file dst))
                       (raise c))
                     (lambda ()
                       (parameterize
                           ((current-source-comments (current-source-comments))
                            (current-temporaries (current-temporaries))
                            (current-environment (current-environment))
                            (extend-lexical-syntax (extend-lexical-syntax))
                            (mutable-literals (mutable-literals))
                            (backtrace (backtrace)))
                         (let loop ()
                           (current-source-comments (and (backtrace) (make-core-hashtable)))
                           (current-temporaries (make-core-hashtable 'string=?))
                           (current-rename-count 0)
                           (let ((obj (core-read input (current-source-comments) 'load)))
                             (cond ((eof-object? obj)
                                    (or cyclic-code (format output "~%")))
                                   (else
                                    (let ((code
                                           (parameterize ((current-closure-comments (make-core-hashtable)))
                                             (compile-coreform (macro-expand obj)))))
                                      (or cyclic-code
                                          (cond ((cyclic-object? code)
                                                 (close-port output)
                                                 (and (file-exists? dst) (delete-file dst))
                                                 (set! cyclic-code #t))
                                                (else
                                                 (put-fasl output code)
                                                 (format output "~%"))))
                                      (run-vmi (cons '(1 . 0) code))
                                      (loop)))))))))))))
            cyclic-code)))

      (define locate-source
        (lambda (ref)

          (define locate
            (lambda (ref)
              (let ((path (symbol-list->string ref "/")))
                (any1 (lambda (base)
                        (any1 (lambda (ext)
                                (let ((maybe-path (format "~a/~a~a" base path ext)))
                                  (and (file-exists? maybe-path) (list maybe-path base))))
                              (library-extensions)))
                      (scheme-library-paths)))))

          (let ((safe-ref (encode-library-ref ref)))
            (or (if (library-contains-implicit-main)
                    (if (eq? (list-ref safe-ref (- (length safe-ref) 1)) 'main)
                        (or (locate (append safe-ref '(main))) (locate safe-ref))
                        (or (locate safe-ref) (locate (append safe-ref '(main)))))
                    (locate safe-ref))
                (and vital (error 'load-scheme-library (format "~s not found in scheme-library-paths: ~s" path (scheme-library-paths))))))))

      (define locate-cache
        (lambda (ref source-path)
          (and (auto-compile-cache)
               (let ((cache-path (format "~a/~a.cache" (auto-compile-cache) (symbol-list->string (encode-library-ref ref) "."))))
                 (and (file-exists? cache-path)
                      (let ((timestamp-path (string-append cache-path ".time")))
                        (and (file-exists? timestamp-path)
                             (call-with-port
                               (make-file-input-port timestamp-path)
                               (lambda (timestamp-port)
                                 (get-datum timestamp-port)
                                 (get-datum timestamp-port)
                                 (cond ((equal? source-path (get-datum timestamp-port)) cache-path)
                                       (else
                                        (close-port timestamp-port)
                                        (auto-compile-cache-clean)
                                        #f))))))
                      cache-path)))))

      (define reorder-scheme-library-paths
        (lambda (top-path)
          (cons top-path
                (let loop ((lst (scheme-library-paths)))
                  (cond ((null? lst) '())
                        ((equal? (car lst) top-path) (cdr lst))
                        (else
                         (cons (car lst)
                               (loop (cdr lst)))))))))

      (or (list? ref) (scheme-error "internal error in load-scheme-library: unrecognized argument: ~s" ref))
      (cond ((locate-source ref)
             => (lambda (location)
                  (destructuring-bind (source-path base-path) location
                    (parameterize ((scheme-library-paths (reorder-scheme-library-paths base-path)))
                      (if (auto-compile-cache)
                          (let ((cache-path (locate-cache ref source-path)))
                            (if cache-path
                                (load-cache cache-path)
                                (let ((ref (encode-library-ref ref)))
                                  (let ((cache-path (format "~a/~a.cache" (auto-compile-cache) (symbol-list->string ref "."))))
                                    (and (auto-compile-verbose) (format #t "~&;; compile ~s~%~!" source-path))
                                    (if (make-cache source-path cache-path ref (stat-mtime source-path))
                                        (and (auto-compile-verbose) (format #t "~&;; delete ~s~%~!" cache-path))
                                        (let ((timestamp-path (string-append cache-path ".time")))
                                          (call-with-port
                                            (make-file-output-port timestamp-path)
                                            (lambda (output) (format output
                                                                     "~s ~s ~s ~s"
                                                                     (microsecond)
                                                                     (stat-mtime source-path)
                                                                     source-path
                                                                     auto-compile-cache-validation-signature)))))))))
                          (load source-path))))))))))
