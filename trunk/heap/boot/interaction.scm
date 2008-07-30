;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2008 Y.FUJITA, LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(define dump-condition (make-parameter #f))
(define no-letrec-check (make-parameter #t))
  
(define add-load-path
  (lambda (path)
    (cond ((string? path)
           (or (string=? path "")
               (let ((path (expand-path path)))
                 (or (member path (scheme-load-paths))
                     (scheme-load-paths (cons path (scheme-load-paths))))))
           (scheme-load-paths))
          (else
           (assertion-violation 'add-load-path (format "expected string but got ~s" path))))))

(define add-library-path
  (lambda (path)
    (cond ((string? path)
           (or (string=? path "")
               (let ((path (expand-path path)))
                 (or (member path (scheme-library-paths))
                     (scheme-library-paths (cons path (scheme-library-paths))))))
           (scheme-library-paths))
          (else
           (assertion-violation 'add-library-path (format "expected string but got ~s" path))))))

(define home-directory
  (lambda ()
    (let ((path (format
                 "~/"
                 (if (string-contains (architecture-feature 'operating-system) "windows")
                     (string-append (or (lookup-process-environment "HOMEDRIVE") "")
                                    (or (lookup-process-environment "HOMEPATH") ""))
                     (lookup-process-environment "HOME")))))
      (and (file-exists? path) path))))

(define apply-scheme-proc-assistant
  (lambda (proc . args)
    (let ((modal #t))
      (dynamic-wind
       (lambda ()
         (or modal (assertion-violation 'apply-scheme-proc-assistant "scheme continuation interleave with c/c++ continuation")))
       (lambda ()
         (let ((obj (apply proc args)))
           (begin (set! modal #f) obj))) ; normal return
       (lambda ()
         (and modal (escape))))))) ; exception raised

(define nonblock-skip-whitespace
  (lambda ()
    (and (nonblock-byte-ready? (current-input-port))
         (let ((ch (lookahead-char (current-input-port))))
           (and (not (eof-object? ch))
                (char-whitespace? ch)))
         (get-char (current-input-port))
         (nonblock-skip-whitespace))))

(define read-eval-print-loop
  (lambda ()
    (let ((plugged (or (lookup-process-environment "EMACS") (not (eq? (port-device-subtype (current-input-port)) 'char)))))
      (let loop ()
        (call-with-current-continuation
         (lambda (continue)
           (with-exception-handler
            (lambda (c)
              (flush-output-port (current-output-port))
              (default-exception-handler c continue))
            (lambda ()
              (nonblock-skip-whitespace)
              (if (eq? (current-environment) (interaction-environment))
                  (format #t "~&> ~!")
                  (format #t "~&~a: ~!" (current-environment)))
              (current-macro-expression #f)
              (current-source-comments (make-core-hashtable))
              (set-port-current-line! (current-input-port) 1)
              (set-port-current-column! (current-output-port) 1)
              (set-port-current-column! (current-error-port) 1)
              (let ((form (core-read (current-input-port) (current-source-comments) 'read)))
                (and (eof-object? form) (exit 0))
                (and plugged (format #t "~%~!"))
                (let ((ans (interpret form)))
                  (cond ((unspecified? ans))
                        (else
                         (pretty-print ans)
                         (flush-output-port (current-output-port))))))))))
        (loop)))))

(define quiet-read-eval-print-loop
  (lambda ()
    (let loop ()
      (call-with-current-continuation
       (lambda (continue)
         (with-exception-handler
          (lambda (c)
            (flush-output-port (current-output-port))
            (default-exception-handler c (lambda () (exit #f))))
          (lambda ()
            (nonblock-skip-whitespace)
            (current-macro-expression #f)
            (current-source-comments (make-core-hashtable))
            (let ((form (core-read (current-input-port) (current-source-comments) 'read)))
              (cond ((eof-object? form) (exit 0))
                    (else
                     (interpret form)
                     (flush-output-port (current-output-port)))))))))
      (loop))))

(define unrename-private-primitives
  (let ((primitive-lst '((|.LIST| . |.list|)
                         (|.CONS| . |.cons|)
                         (|.CONS*| . |.cons*|)
                         (|.APPEND| . |.append|)
                         (|.VECTOR| . |.vector|)
                         (|.LIST->VECTOR| . |.list->vector|)
                         (|.EQ?| . |.eq?|)
                         (|.EQV?| . |.eqv?|)
                         (|.MEMQ| . |.memq|)
                         (|.MEMV| . |.memv|)
                         (|.CALL-WITH-VALUES| . |.call-with-values|)
                         (|.APPLY| . |.apply|)
                         (|.CDR| . |.cdr|)
                         (|.IDENTIFIER?| . |.identifier?|)
                         (|.MAKE-VARIABLE-TRANSFORMER| . |.make-variable-transformer|)
                         (|.ASSERTION-VIOLATION| . |.assertion-violation|)
                         (|.UNSPECIFIED| . |.unspecified|)
                         (|.QUOTE| . quote)
                         (|.LET| . let)
                         (|.LETREC*| . letrec*)
                         (|.BEGIN| . begin)
                         (|.LAMBDA| . lambda)
                         (|.IF| . if)
                         (|.SET!| . set!)
                         (|.OR| . or)
                         (|.COND| . cond)
                         (|.ELSE| . else)
                         (|.DEFINE-SYNTAX| . define-syntax)
                         (|.SYNTAX| . syntax)
                         (|.SYNTAX-CASE| . syntax-case))))
    (lambda (form)
      (let loop ((lst form))
        (cond ((pair? lst)
               (cons (loop (car lst)) (loop (cdr lst))))
              ((symbol? lst)
               (cond ((assq lst primitive-lst) => cdr)
                     (else lst)))
              ((vector? lst)
               (list->vector (map loop (vector->list lst))))
              (else lst))))))

(define display-warning
  (lambda (message form subform)
    (let ((port (make-string-output-port)))
      (format port "~a" message)
      (parameterize ((pretty-print-line-length (backtrace-line-length))
                     (pretty-print-maximum-lines 10)
                     (pretty-print-unwrap-syntax #t)
                     (pretty-print-initial-indent 5))
        (cond (form
               (format port  "~%  >  ")
               (pretty-print (unrename-private-primitives form) port)
               (and (pair? form) (format port "~%  ~n" form))))
        (cond (subform
               (format port "~%  >  ")
               (pretty-print (unrename-private-primitives subform) port)
               (and (pair? subform) (format port "~%  ~n" subform))))
        (format port "~%")
        (let ((plugged (or (lookup-process-environment "EMACS") (not (eq? (port-device-subtype (current-input-port)) 'char)))))
          (if plugged
              (format (current-error-port) "~a~!" (extract-accumulated-string port))
              (format (current-error-port) "~%~a~!" (extract-accumulated-string port))))))))

(define default-exception-handler
  (lambda (condition continue)
    (current-exception-handler #f)
    (let ((port (make-string-output-port)))
      (parameterize ((pretty-print-line-length (backtrace-line-length))
                     (pretty-print-maximum-lines 10)
                     (pretty-print-unwrap-syntax #t))

        (define output-who-message
          (lambda ()
            (format port "error")
            (and (who-condition? condition)
                 (format port " in ~u" (condition-who condition)))
            (and (message-condition? condition)
                 (format port ": ~a" (condition-message condition)))))

        (define output-irritants
          (lambda ()
            (cond ((and (irritants-condition? condition) (pair? (condition-irritants condition)))
                   (format port "~%~%irritants:")
                   (for-each (lambda (e)
                               (format port "~% ")
                               (cond ((list? e)
                                      (format port " (")
                                      (let loop ((lst (map (lambda (e) (format "~r" e)) e)))
                                        (cond ((pair? lst)
                                               (format port "~a" (car lst))
                                               (cond ((pair? (cdr lst))
                                                      (format port " ")
                                                      (loop (cdr lst)))
                                                     (else
                                                      (format port ")")))))))
                                     (else (format port " ~r" e))))
                             (condition-irritants condition))))))

        (define output-expansion
          (lambda ()
            (and (expansion-backtrace)
                 (current-macro-expression)
                 (parameterize ((pretty-print-initial-indent 7))
                   (format port "~%~%expanding:~%  >  ")
                   (pretty-print (unrename-private-primitives (current-macro-expression)) port)
                   (format port "~%  ~n" (current-macro-expression))
                   (for-each (lambda (e)
                               (format port "~%  *  ")
                               (pretty-print (unrename-private-primitives e) port)
                               (format port "~%  ~n" e))
                             (expansion-trace-stack))))))

        (define output-condition
          (lambda (c)
            (and (dump-condition)
                 (format port "~%~%")
                 (describe-condition port c))))
                 
        (cond ((syntax-violation? condition)
               (output-who-message)
               (cond ((syntax-violation-form condition)
                      => (lambda (form)
                           (parameterize ((pretty-print-initial-indent 5))
                             (format port "~%  >  ")
                             (pretty-print (unrename-private-primitives form) port)
                             (and (pair? form) (format port "~%  ~n" form))))))
               (cond ((syntax-violation-subform condition)
                      => (lambda (form)
                           (parameterize ((pretty-print-initial-indent 5))
                             (format port "~%  @  ")
                             (pretty-print (unrename-private-primitives form) port)
                             (and (pair? form) (format port "~%  ~n" form))))))
               (output-condition condition)
               (or (and (null? (expansion-trace-stack))
                        (or (eq? (current-macro-expression) (syntax-violation-form condition))
                            (eq? (current-macro-expression) (syntax-violation-subform condition))))
                   (output-expansion)))

              ((undefined-violation? condition)
               (format port "error: unbound variable")
               (and (who-condition? condition)
                    (format port " ~u" (condition-who condition)))
               (and (message-condition? condition)
                    (format port ", ~a" (condition-message condition)))
               (output-irritants)
               (output-condition condition)
               (output-expansion))

              ((error? condition)
               (output-who-message)
               (output-irritants)
               (output-condition condition)
               (output-expansion))

              ((violation? condition)
               (output-who-message)
               (output-irritants)
               (output-condition condition)
               (output-expansion))

              ((warning? condition)
               (format port "warning")
               (and (who-condition? condition)
                    (format port " in ~u" (condition-who condition)))
               (and (message-condition? condition)
                    (format port ": ~a" (condition-message condition)))
               (output-irritants)
               (output-condition condition)
               (output-expansion))

              ((condition? condition)
               (format port "error: unknown type of exception caught~%~%irritants:~%~a" (describe-condition #f condition))
               (output-irritants)
               (output-expansion))

              (else
               (format port "error: unknown type of exception caught, ~a" condition)
               (output-irritants)
               (output-expansion))))

      (format port "~%")
      (let ((plugged (or (lookup-process-environment "EMACS") (not (eq? (port-device-subtype (current-input-port)) 'char)))))
        (cond ((serious-condition? condition)
               (display-backtrace port)
               (if plugged
                   (format (current-error-port) "~a~!" (extract-accumulated-string port))
                   (format (current-error-port) "~%~a~%~!" (extract-accumulated-string port)))
               (usleep 10000) ; make console happy
               (and continue (continue)))
              (else
               (if plugged
                   (format (current-error-port) "~a~!" (extract-accumulated-string port))
                   (format (current-error-port) "~%~a~!" (extract-accumulated-string port)))
               (usleep 10000))))))) ; make console happy
      
(define start-scheme-session
  (lambda ()

    (define directory-exists?
      (lambda (path)
        (file-exists? (format "~a/." path))))

    (define init-sys-sitelib
      (lambda ()
        (let ((path (format "~a/sitelib" (system-share-path))))
          (and (directory-exists? path) (add-library-path path)))))

    (define init-sys-acc
      (lambda ()
        (cond ((string-contains (architecture-feature 'operating-system) "windows")
               (cond ((lookup-process-environment "TEMP")
                      => (lambda (path)
                           (cond ((directory-exists? path)
                                  (or (directory-exists? (format "~//Ypsilon" path))
                                      (create-directory (format "~//Ypsilon" path)))
                                  (auto-compile-cache (format "~//Ypsilon" path))))))))
              (else
               (cond ((directory-exists? (format "~//.ypsilon" (home-directory)))
                      (auto-compile-cache (format "~//.ypsilon" (home-directory)))))))))

    (define init-env-acc
      (lambda ()
        (cond ((lookup-process-environment "YPSILON_ACC")
               => (lambda (path)
                    (cond ((directory-exists? (expand-path path))
                           (auto-compile-cache (expand-path path)))
                          (else
                           (format (current-error-port) "** ERROR in environment variable 'YPSILON_ACC': directory ~s not exist~%" path)
                           (auto-compile-cache #f))))))))

    (define init-env-sitelib
      (lambda ()
        (cond ((lookup-process-environment "YPSILON_SITELIB")
               => (lambda (paths)
                    (for-each
                     (lambda (path)
                       (cond ((directory-exists? (expand-path path))
                              (add-library-path (expand-path path)))
                             (else
                              (format (current-error-port) "** ERROR in environment variable 'YPSILON_SITELIB': directory ~s not exist~%" path))))
                     (reverse (if (string-contains (architecture-feature 'operating-system) "windows")
                                  (string-split paths #\;)
                                  (string-split paths #\:)))))))))

    (define init-env-loadpath
      (lambda ()
        (cond ((lookup-process-environment "YPSILON_LOADPATH")
               => (lambda (paths)
                    (for-each
                     (lambda (path)
                       (cond ((directory-exists? (expand-path path))
                              (add-load-path (expand-path path)))
                             (else
                              (format (current-error-port) "** ERROR in environment variable 'YPSILON_LOADPATH': directory ~s not exist~%" path))))
                     (reverse (if (string-contains (architecture-feature 'operating-system) "windows")
                                  (string-split paths #\;)
                                  (string-split paths #\:)))))))))

    (define add-opt-sitelib
      (lambda (paths)
        (for-each (lambda (path)
                    (cond ((directory-exists? (expand-path path))
                           (add-library-path (expand-path path)))
                          (else
                           (format (current-error-port) "** ERROR in option '--sitelib=~a': directory ~s not exist~%" paths path)
                           (exit #f))))
                  (reverse (if (string-contains (architecture-feature 'operating-system) "windows")
                               (string-split paths #\;)
                               (string-split paths #\:))))))

    (define add-opt-loadpath
      (lambda (paths)
        (for-each (lambda (path)
                    (cond ((directory-exists? (expand-path path))
                           (add-load-path (expand-path path)))
                          (else
                           (format (current-error-port) "** ERROR in option '--loadpath=~a': directory ~s not exist~%" paths path)
                           (exit #f))))
                  (reverse (if (string-contains (architecture-feature 'operating-system) "windows")
                               (string-split paths #\;)
                               (string-split paths #\:))))))

    (define set-opt-acc
      (lambda (path)
        (cond ((directory-exists? (expand-path path))
               (auto-compile-cache (expand-path path)))
              (else
               (format (current-error-port) "** ERROR in option '--acc=~a': directory ~s not exist~%" path path)
               (exit #f)))))

    (define bad-option
      (lambda (opt)
        (format (current-error-port) "** ERROR in option '~a'~%" opt)
        (show-usage)
        (exit #f)))

    (define show-usage
      (lambda ()
        (format #t "usage: ypsilon [options] [--] [file] [arguments]~%")
        (format #t "options:~%")
        (format #t "  --mute (-m)            suppresses greeting~%")
        (format #t "  --quiet (-q)           suppresses greeting, repl prompt, and repl output~%")
        (format #t "  --verbose (-v)         prints load and compile activities~%")
        (format #t "  --warning (-w)         prints warnings~%")
        (format #t "  --interactive (-i)     enters repl after running the script file~%")
        (format #t "  --r6rs (-6)            conforms r6rs lexical syntax (default)~%")
        (format #t "  --compatible (-c)      extends lexical syntax for compatibility~%")
        (format #t "  --no-letrec-check      no letrec restriction check~%")
        (format #t "  --sitelib=path         adds sitelib path (YPSILON_SITELIB)~%")
        (format #t "  --loadpath=path        adds load search path (YPSILON_LOADPATH)~%")
        (format #t "  --acc=dir              sets a auto-compile-cache directory (YPSILON_ACC)~%")
        (format #t "  --heap-limit=mbytes    sets a total heap limit in MBytes~%")
        (format #t "  --dump-condition       default exception handler dump condition~%")
        (format #t "  --disable-acc          disables auto-compile-cache~%")
        (format #t "  --clean-acc            cleans auto-compile-cache~%")
        (format #t "  --version              prints version and exit~%")
        (format #t "  --help                 prints help and exit~%")
        (format #t "  --                     indicates no more option to proceed~%")))

    (define show-banner
      (lambda ()
        (put-string (current-output-port) "Ypsilon 0.9.5-trunk Copyright (c) 2008 Y.Fujita, LittleWing Company Limited.\n")))

    (define show-info
      (lambda ()
        (show-banner)
        (cond ((lookup-process-environment "YPSILON_ACC")
               => (lambda (path) (format #t ";; YPSILON_ACC=~a~%" path)))
              (else (format #t ";; YPSILON_ACC unspecified~%")))
        (cond ((lookup-process-environment "YPSILON_SITELIB")
               => (lambda (path) (format #t ";; YPSILON_SITELIB=~a~%" path)))
              (else (format #t ";; YPSILON_SITELIB unspecified~%")))
        (cond ((lookup-process-environment "YPSILON_LOADPATH")
               => (lambda (path) (format #t ";; YPSILON_LOADPATH=~a~%" path)))
              (else (format #t ";; YPSILON_LOADPATH unspecified~%")))
        (format #t ";; (auto-compile-cache) => ~s~%" (auto-compile-cache))
        (format #t ";; (scheme-library-paths) => ~s~%" (scheme-library-paths))
        (format #t ";; (scheme-load-paths) => ~s~%" (scheme-load-paths))))

    (define exec-script
      (lambda (lst)
        (command-line-shift (- (length (command-line)) (length lst)))
        (let ((path (car lst)))
          (cond (interaction
                 (with-exception-handler
                  (lambda (c)
                    (flush-output-port (current-output-port))
                    (default-exception-handler c exec-repl))
                  (lambda ()
                    (load path))))
                (else
                 (with-exception-handler
                  (lambda (c)
                    (flush-output-port (current-output-port))
                    (default-exception-handler c (lambda () (exit #f))))
                  (lambda ()
                    (auto-compile-cache-update)
                    (if (or r6rs-program (load-file-has-r6rs-comment? path))
                        (load-r6rs path)
                        (load path))
                    (flush-output-port (current-error-port))
                    (flush-output-port (current-output-port)))))))))

    (define exec-repl
      (lambda ()
        (cond (mute)
              (verbose (show-info))
              (else (show-banner)))
        (or script (interpret '(import (core) (rnrs))))
        (if quiet
            (quiet-read-eval-print-loop)
            (read-eval-print-loop))))

    (define verbose #f)
    (define quiet #f)
    (define interaction #f)
    (define script #f)
    (define mute #f)
    (define r6rs-program #f)
      
    (define initial-command-line (command-line))

    (init-sys-acc)
    (init-env-acc)
    (init-sys-sitelib)
    (init-env-sitelib)
    (init-env-loadpath)

    (let ((lst initial-command-line))
      (and (pair? lst)
           (let loop ((lst (cdr lst)))
             (cond ((null? lst)
                    (if interaction
                        (exec-repl)
                        (or script (exec-repl))))
                   (else
                    (let ((opt (car lst)))

                      (define opt?
                        (lambda (flag rhs?)
                          (let ((n (string-contains opt flag)))
                            (cond ((not n) #f)
                                  ((not (= n 0)) #f)
                                  ((string=? opt flag) "")
                                  ((and rhs? (char=? (string-ref opt (string-length flag)) #\=))
                                   (substring opt (+ (string-length flag) 1) (string-length opt)))
                                  (else #f)))))

                      (cond ((opt? "--heap-limit" #f) (loop (cddr lst)))
                            ((opt? "--heap-limit" #t) (loop (cdr lst)))
                            ((opt? "--no-letrec-check" #f)
                             (no-letrec-check #t)
                             (loop (cdr lst)))
                            ((or (opt? "--warning" #f) (opt? "-w" #f))
                             (warning-level #t)
                             (loop (cdr lst)))
                            ((opt? "--version" #f)
                             (show-banner)
                             (exit))
                            ((opt? "--help" #f)
                             (show-usage)
                             (exit))
                            ((or (opt? "--r6rs" #f) (opt? "-6" #f))
                             (extend-lexical-syntax #f)
                             (set! r6rs-program #t)
                             (loop (cdr lst)))
                            ((or (opt? "--compatible" #f) (opt? "-c" #f))
                             (extend-lexical-syntax #t)
                             (set! r6rs-program #f)
                             (loop (cdr lst)))
                            ((or (opt? "--verbose" #f) (opt? "-v" #f))
                             (scheme-load-verbose #t)
                             (auto-compile-verbose #t)
                             (set! verbose #t)
                             (set! mute #f)
                             (loop (cdr lst)))
                            ((or (opt? "--mute" #f) (opt? "-m" #f))
                             (set! verbose #f)
                             (set! mute #t)
                             (loop (cdr lst)))
                            ((or (opt? "--quiet" #f) (opt? "-q" #f))
                             (set! verbose #f)
                             (set! mute #t)
                             (set! quiet #t)
                             (loop (cdr lst)))
                            ((or (opt? "--interactive" #f) (opt? "-i" #f))
                             (set! interaction #t)
                             (loop (cdr lst)))
                            ((opt? "--dump-condition" #f)
                             (dump-condition #t)
                             (loop (cdr lst)))
                            ((opt? "--acc" #f)
                             (or (pair? (cdr lst)) (bad-option opt))
                             (set-opt-acc (cadr lst))
                             (loop (cddr lst)))
                            ((opt? "--acc" #t)
                             => (lambda (rhs)
                                  (cond ((string=? rhs "")
                                         (bad-option opt))
                                        (else
                                         (set-opt-acc rhs)
                                         (loop (cdr lst))))))
                            ((opt? "--disable-acc" #f)
                             (auto-compile-cache #f)
                             (loop (cdr lst)))
                            ((opt? "--clean-acc" #f)
                             (auto-compile-cache-clean)
                             (loop (cdr lst)))
                            ((opt? "--sitelib" #f)
                             (or (pair? (cdr lst)) (bad-option opt))
                             (add-opt-sitelib (cadr lst))
                             (loop (cddr lst)))
                            ((opt? "--sitelib" #t)
                             => (lambda (rhs)
                                  (cond ((string=? rhs "")
                                         (bad-option opt))
                                        (else
                                         (add-opt-sitelib rhs)
                                         (loop (cdr lst))))))
                            ((opt? "--loadpath" #f)
                             (or (pair? (cdr lst)) (bad-option opt))
                             (add-opt-loadpath (cadr lst))
                             (loop (cddr lst)))
                            ((opt? "--loadpath" #t)
                             => (lambda (rhs)
                                  (cond ((string=? rhs "")
                                         (bad-option opt))
                                        (else
                                         (add-opt-loadpath rhs)
                                         (loop (cdr lst))))))
                            ((opt? "--" #f)
                             (set! script #t)
                             (exec-script (cdr lst))
                             (and interaction (exec-repl)))
                            ((char=? (string-ref opt 0) #\-)
                             (bad-option opt))
                            (else
                             (set! script #t)
                             (exec-script lst)
                             (and interaction (exec-repl))))))))))))

