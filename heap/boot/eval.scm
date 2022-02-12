;;; Copyright (c) 2004-2022 Yoshikatsu Fujita / LittleWing Company Limited.
;;; See LICENSE file for terms and conditions of use.

(define auto-compile-cache-validation-signature
  (string->symbol
    (format
      "ypsilon-~a.~a.~a"
      (architecture-feature 'program-version-major)
      (architecture-feature 'program-version-minor)
      (architecture-feature 'program-revision))))
(define auto-compile-cache-lock-path
  (lambda ()
    (and (auto-compile-cache)
         (format "~a/ypsilon-auto-compile-cache.lock" (auto-compile-cache)))))
(define auto-compile-verbose (make-parameter #f))
(define scheme-load-verbose (make-parameter #f))
(define scheme-load-paths (make-parameter '()))
(define library-contains-implicit-main (make-parameter #t))
(define library-include-dependencies (make-core-hashtable))
(define current-include-files (make-parameter #f))
(define current-library-name (make-parameter #f))

(define track-file-open-operation
  (lambda (path)
    (and (current-include-files)
         (core-hashtable-set! (current-include-files) path #t))))

(define library-extensions
  (make-parameter
   (list ".ypsilon.sld"
         ".ypsilon.sls"
         ".ypsilon.ss"
         ".ypsilon.scm"
         ".sld"
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
              (format
                (current-error-port)
                "~&;; warning in auto-compile-cache: directory ~s not exist (temporary disable caching)~!~%"
                p)
              #f)))))

(define core-eval
  (lambda (form)
    (parameterize ((backtrace #f))
      (interpret-coreform form))))

(define compile
  (lambda (form)
    (parameterize ((current-closure-comments (make-core-hashtable)))
      (compile-coreform (macro-expand form)))))

(define interpret
  (lambda (form)
    (let ((code
           (parameterize ((current-closure-comments (make-core-hashtable)))
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
    (cond ((environment? env) (parameterize ((current-environment env)) (interpret expr)))
          (else
            (or (eq? (tuple-ref env 0) 'type:eval-environment)
                (assertion-violation 'eval (format "expected environment, but got ~r, as argument 2" env)))
            (interpret
              `(begin
                 (library (|.&EVAL|)
                   (export)
                   (import (rename
                             (only (core primitives) set-top-level-value! string->symbol)
                             (set-top-level-value! |.&SET-TOP-LEVEL-VALUE!|)
                             (string->symbol |.&STRING->SYMBOL|))
                           ,@(tuple-ref env 1))
                   (|.&SET-TOP-LEVEL-VALUE!| (|.&STRING->SYMBOL| ".&EVAL-RESULT") ,expr))
                 (let ((result |.&EVAL-RESULT|))
                   (|.set-top-level-value!| '|.&EVAL-RESULT| |.&UNDEF|)
                   (|.unintern-scheme-library| ',(generate-library-id '(|.&EVAL|)))
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

(define locate-file
  (lambda (path form)
    (define path-not-found (lambda (path) (assertion-violation form (format "~a~/~a not found" #\" path #\"))))
    (define confirm-path (lambda (path) (and (file-exists? path) path)))
    (cond ((= (string-length path) 0) (path-not-found path))
          ((or (string-contains path ":") (memq (string-ref path 0) '(#\/ #\\)))
           (or (confirm-path path) (path-not-found path)))
          ((memq (string-ref path 0) '(#\. #\~)) (or (confirm-path (expand-path path)) (path-not-found path)))
          ((any1 (lambda (e) (confirm-path (expand-path (string-append e "/" path)))) (cons "." (scheme-load-paths))))
          (else (path-not-found path)))))

(define locate-load-file (lambda (path) (locate-file path 'load)))

(define load-file-has-r6rs-comment?
  (lambda (path)
    (parameterize ((lexical-syntax-version 7) (mutable-literals #f))
      (let ((port (open-script-input-port (locate-load-file path))))
        (core-read port #f 'load)
        (close-port port)
        (= (lexical-syntax-version) 6)))))

(define load
  (lambda (path)
    (let ((abs-path (locate-load-file path)))
      (and (scheme-load-verbose) (format #t "~&;; loading ~s~%~!" abs-path))
      (let ((port (open-script-input-port abs-path)))
        (with-exception-handler
          (lambda (c) (cond ((serious-condition? c) (close-port port) (raise c)) (else (raise-continuable c))))
          (lambda ()
            (parameterize ((current-source-comments (current-source-comments))
                            (current-temporaries (current-temporaries))
                            (current-environment (current-environment))
                            (lexical-syntax-version (lexical-syntax-version))
                            (mutable-literals (mutable-literals))
                            (backtrace (backtrace)))
              (current-temporaries (make-core-hashtable 'string=?))
              (current-rename-count 0)
              (let loop ()
                (current-source-comments (and (backtrace) (make-core-hashtable)))
                (let ((form (core-read port (current-source-comments) 'load)))
                  (cond ((eof-object? form) (close-port port)) (else (interpret form) (loop))))))))))))

(define load-top-level-program
  (lambda (path)
    (let ((abs-path (locate-load-file path)))
      (and (scheme-load-verbose) (format #t "~&;; loading ~s~%~!" abs-path))
      (let ((port (open-script-input-port abs-path)))
        (with-exception-handler
          (lambda (c) (cond ((serious-condition? c) (close-port port) (raise c)) (else (raise-continuable c))))
          (lambda ()
            (parameterize ((current-source-comments (current-source-comments))
                           (current-temporaries (current-temporaries))
                           (current-environment (current-environment))
                           (lexical-syntax-version (lexical-syntax-version))
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
                        (else (loop (cons form acc)))))))))))))

(define encode-library-ref
  (lambda (library-name)
    (map (lambda (s)
           (let ((utf8 (string->utf8 (if (symbol? s) (symbol->string s) (number->string s)))))
             (let ((buf (make-string-output-port)) (end (bytevector-length utf8)))
               (let loop ((i 0))
                 (if (= i end)
                     (string->symbol (extract-accumulated-string buf))
                     (let ((c (bytevector-u8-ref utf8 i)))
                       (cond ((or (and (> c 96) (< c 123))
                                  (and (> c 47) (< c 58))
                                  (and (> c 64) (< c 91))
                                  (= c 43)
                                  (= c 45)
                                  (= c 95))
                              (put-byte buf c)
                              (loop (+ i 1)))
                             (else (format buf "%~x~x" (div c 16) (mod c 16)) (loop (+ i 1))))))))))
         library-name)))

(define locate-include-file
  (lambda (library-name path who)
    (if library-name
        (let ((subdir
                (destructuring-match library-name
                  ((_) "")
                  ((base ... _) (id-list->string base "/")))))
          (or (any1
                (lambda (base)
                  (let ((maybe-include-path (format "~a/~a/~a" base subdir path)))
                    (and (file-exists? maybe-include-path) maybe-include-path)))
                (scheme-library-paths))
              (locate-file path who)))
        (locate-file path who))))

(define locate-library-file
  (lambda (library-name)
    (let ((path (id-list->string (encode-library-ref library-name) "/")))
      (any1
        (lambda (base)
          (any1
            (lambda (ext)
              (let ((maybe-path (format "~a/~a~a" base path ext)))
                (and (file-exists? maybe-path) (list maybe-path base))))
            (library-extensions)))
        (scheme-library-paths)))))

(define read-include-file
  (lambda (library-name path who)
    (let ((source-path (locate-include-file library-name path who)))
      (and (scheme-load-verbose) (format #t "~&;; including ~s~%~!" source-path))
      (track-file-open-operation source-path)
      (let ((port (open-script-input-port source-path)))
        (with-exception-handler
          (lambda (c) (cond ((serious-condition? c) (close-port port) (raise c)) (else (raise-continuable c))))
          (lambda ()
            (parameterize ((current-source-comments (current-source-comments)))
              (current-source-comments (and (backtrace) (make-core-hashtable)))
              (let loop ((acc '()))
                (let ((form (core-read port (current-source-comments) who)))
                  (cond ((eof-object? form) (close-port port) (reverse acc)) (else (loop (cons form acc)))))))))))))

(define load-scheme-library
  (lambda (ref . vital)
    (let ((vital (or (not (pair? vital)) (car vital))))
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
                        (parameterize ((current-source-comments (current-source-comments))
                                       (current-temporaries (current-temporaries))
                                       (current-environment (current-environment))
                                       (lexical-syntax-version (lexical-syntax-version))
                                       (mutable-literals (mutable-literals))
                                       (backtrace (backtrace)))
                          (let loop ()
                            (current-source-comments (and (backtrace) (make-core-hashtable)))
                            (current-temporaries (make-core-hashtable 'string=?))
                            (current-rename-count 0)
                            (let ((obj (core-read input (current-source-comments) 'load)))
                              (cond ((eof-object? obj) (or cyclic-code (format output "~%")))
                                    (else
                                      (let ((code
                                              (parameterize ((current-closure-comments (make-core-hashtable)))
                                                (compile-coreform (macro-expand obj)))))
                                        (or cyclic-code
                                            (cond ((cyclic-object? code)
                                                   (close-port output)
                                                   (and (file-exists? dst) (delete-file dst))
                                                   (set! cyclic-code #t))
                                                  (else (put-fasl output code) (format output "~%"))))
                                        (run-vmi (cons '(1 . 0) code))
                                        (loop)))))))))))))
            cyclic-code)))
      (define locate-file
        (lambda (ref)
          (let ((path (id-list->string ref "/")))
            (any1
              (lambda (base)
                (any1
                  (lambda (ext)
                    (let ((maybe-path (format "~a/~a~a" base path ext)))
                      (and (file-exists? maybe-path) (list maybe-path base))))
                  (library-extensions)))
              (scheme-library-paths)))))
      (define locate-source
        (lambda (ref)
          (let ((safe-ref (encode-library-ref ref)))
            (or (if (library-contains-implicit-main)
                    (if (eq? (list-ref safe-ref (- (length safe-ref) 1)) 'main)
                        (or (locate-file (append safe-ref '(main))) (locate-file safe-ref))
                        (or (locate-file safe-ref) (locate-file (append safe-ref '(main)))))
                    (locate-file safe-ref))
                (and vital
                     (error
                       'load-scheme-library
                       (format "~s not found in scheme-library-paths: ~s" path (scheme-library-paths))))))))
      (define locate-cache
        (lambda (ref source-path)
          (and (auto-compile-cache)
               (let ((cache-path
                       (format "~a/~a.cache" (auto-compile-cache) (id-list->string (encode-library-ref ref) "."))))
                 (and (file-exists? cache-path)
                      (let ((timestamp-path (string-append cache-path ".time")))
                        (and (file-exists? timestamp-path)
                             (call-with-port
                               (make-file-input-port timestamp-path)
                               (lambda (timestamp-port)
                                 (get-datum timestamp-port)
                                 (get-datum timestamp-port)
                                 (cond ((equal? source-path (get-datum timestamp-port)) cache-path)
                                       (else (close-port timestamp-port) (cache-clean) #f))))))
                      cache-path)))))
      (define reorder-scheme-library-paths
        (lambda (top-path)
          (cons top-path
                (let loop ((lst (scheme-library-paths)))
                  (cond ((null? lst) '())
                        ((equal? (car lst) top-path) (cdr lst))
                        (else (cons (car lst) (loop (cdr lst)))))))))
      (or (list? ref) (scheme-error "internal error in load-scheme-library: unrecognized argument: ~s" ref))
      (cond
        ((locate-source ref)
         =>
         (lambda (location)
           (destructuring-bind
             (source-path base-path)
             location
             (track-file-open-operation source-path)
             (parameterize ((scheme-library-paths (reorder-scheme-library-paths base-path)))
               (if (auto-compile-cache)
                   (let ((cache-path (locate-cache ref source-path)))
                     (if cache-path
                         (load-cache cache-path)
                         (let ((ref (encode-library-ref ref)) (library-id (generate-library-id ref)))
                           (let ((cache-path (format "~a/~a.cache" (auto-compile-cache) (id-list->string ref "."))))
                             (and (auto-compile-verbose) (format #t "~&;; compile ~s~%~!" source-path))
                             (if (make-cache source-path cache-path ref (file-stat-mtime source-path))
                                 (and (auto-compile-verbose) (format #t "~&;; delete ~s~%~!" cache-path))
                                 (let ((timestamp-path (string-append cache-path ".time")))
                                   (call-with-port
                                     (make-file-output-port timestamp-path)
                                     (lambda (output)
                                       (format
                                         output
                                         "~s ~s ~s ~s~%"
                                         (microsecond)
                                         (file-stat-mtime source-path)
                                         source-path
                                         auto-compile-cache-validation-signature)
                                       (cond
                                         ((core-hashtable-ref library-include-dependencies library-id #f)
                                          =>
                                          (lambda (deps)
                                            (for-each
                                              (lambda (dep)
                                                (format output "~s ~s~%" (car dep) (file-stat-mtime (car dep))))
                                              (core-hashtable->alist deps))
                                            (core-hashtable-delete! library-include-dependencies library-id))))))))))))
                   (load source-path))))))))))

(define load-cache
  (lambda (path)
    (and (scheme-load-verbose) (format #t "~&;; loading ~s~%~!" path))
    (let ((port (open-script-input-port path)))
      (with-exception-handler
        (lambda (c) (close-port port) (raise c))
        (lambda ()
          (parameterize ((backtrace #f)
                         (current-source-comments #f)
                         (current-temporaries (make-core-hashtable 'string=?))
                         (current-rename-count 0)
                         (current-environment (current-environment)))
            (let loop ()
              (let ((form (core-read port #f 'load)))
                (cond ((eof-object? form) (close-port port)) (else (run-vmi (cons '(1 . 0) form)) (loop)))))))))))

(define cache-delete-files
  (lambda (cache-lst)
    (for-each
      (lambda (cache-name)
        (let* ((cache-path (string-append (auto-compile-cache) "/" cache-name))
               (timestamp-path (string-append cache-path ".time")))
          (and (file-exists? cache-path) (delete-file cache-path))
          (and (file-exists? timestamp-path) (delete-file timestamp-path))
          (and (auto-compile-verbose) (format #t "~&;; clean ~s~%" cache-path))))
      cache-lst)))

(define cache-clean
  (lambda ()
    (cond
      ((auto-compile-cache)
       =>
       (lambda (path)
         (let ((cache-lst
                 (filter
                   (lambda (s)
                     (let ((p (string-contains s ".cache")))
                       (and p (= (string-contains s ".cache") (- (string-length s) 6)))))
                   (directory-list path))))
           (cache-delete-files cache-lst)))))))

(define cache-update
  (lambda ()
    (define inconsistent-cache-state
      (lambda (cache-lst)
        (and (auto-compile-verbose) (format #t "~&;; reset ~s~%" (auto-compile-cache)))
        (cache-delete-files cache-lst)))
    (cond ((auto-compile-cache)
           =>
           (lambda (path)
             (let ((cache-lst
                     (filter
                       (lambda (s)
                         (let ((p (string-contains s ".cache")))
                           (and p (= (string-contains s ".cache") (- (string-length s) 6)))))
                       (directory-list path))))
               (let loop ((lst cache-lst) (expiration #f))
                 (cond ((null? lst)
                        (and expiration
                             (for-each
                               (lambda (cache-name)
                                 (let* ((cache-path (string-append (auto-compile-cache) "/" cache-name))
                                        (timestamp-path (string-append cache-path ".time")))
                                   (cond ((file-exists? timestamp-path)
                                          (call-with-port
                                            (make-file-input-port timestamp-path)
                                            (lambda (timestamp-port)
                                              (let ((cache-timestamp (get-datum timestamp-port)))
                                                (close-port timestamp-port)
                                                (cond
                                                  ((>= cache-timestamp expiration)
                                                   (delete-file timestamp-path)
                                                   (delete-file cache-path)
                                                   (and (auto-compile-verbose)
                                                        (format #t "~&;; clean ~s~%" cache-path))))))))
                                         (else (inconsistent-cache-state cache-lst)))))
                               cache-lst))
                        (unspecified))
                       (else
                         (let* ((cache-path (string-append (auto-compile-cache) "/" (car lst)))
                                (timestamp-path (string-append cache-path ".time")))
                           (cond ((file-exists? timestamp-path)
                                  (call-with-port
                                    (make-file-input-port timestamp-path)
                                    (lambda (timestamp-port)
                                      (define get-dependencies-list
                                        (lambda ()
                                          (let loop ((lst '()))
                                            (let* ((dep-path (get-datum timestamp-port))
                                                   (dep-time (get-datum timestamp-port)))
                                              (if (eof-object? dep-path)
                                                  lst
                                                  (loop (cons (cons dep-path dep-time) lst)))))))
                                      (define dependencies-exists?
                                        (lambda (lst)
                                          (every1
                                            (lambda (b)
                                              (and (string? (car b)) (number? (cdr b)) (file-exists? (car b))))
                                            lst)))
                                      (define dependencies-valid?
                                        (lambda (lst) (every1 (lambda (b) (= (file-stat-mtime (car b)) (cdr b))) lst)))
                                      (let* ((cache-timestamp (get-datum timestamp-port))
                                             (source-timestamp (get-datum timestamp-port))
                                             (source-path (get-datum timestamp-port))
                                             (cache-signature (get-datum timestamp-port))
                                             (dependencies (get-dependencies-list)))
                                        (close-port timestamp-port)
                                        (cond ((not
                                                 (and (number? cache-timestamp)
                                                      (number? source-timestamp)
                                                      (string? source-path)
                                                      (eq? cache-signature auto-compile-cache-validation-signature)
                                                      (file-exists? source-path)
                                                      (dependencies-exists? dependencies)))
                                               (inconsistent-cache-state cache-lst))
                                              ((and (= (file-stat-mtime source-path) source-timestamp)
                                                    (dependencies-valid? dependencies))
                                               (loop (cdr lst) expiration))
                                              (else
                                                (loop
                                                  (cdr lst)
                                                  (cond ((not expiration) cache-timestamp)
                                                        ((< cache-timestamp expiration) cache-timestamp)
                                                        (else expiration)))))))))
                                 (else
                                   (delete-file cache-path)
                                   (and (auto-compile-verbose) (format #t "~&;; clean ~s~%" cache-path))
                                   (loop (cdr lst) expiration))))))))))
          (else (unspecified)))))

(define auto-compile-cache-clean
  (lambda ()
    (define lock-fd #f)
    (dynamic-wind
      (lambda () (and (auto-compile-cache) (set! lock-fd (acquire-lockfile (auto-compile-cache-lock-path)))))
      (lambda () (cache-clean))
      (lambda () (and (auto-compile-cache) (release-lockfile lock-fd))))))

(define auto-compile-cache-update
  (lambda ()
    (define lock-fd #f)
    (dynamic-wind
      (lambda () (and (auto-compile-cache) (set! lock-fd (acquire-lockfile (auto-compile-cache-lock-path)))))
      (lambda () (cache-update))
      (lambda () (and (auto-compile-cache) (release-lockfile lock-fd))))))
