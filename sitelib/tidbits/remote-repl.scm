#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2008 Y.FUJITA, LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (tidbits remote-repl)

  (export make-remote-repl
          connect-remote-repl)

  (import (core) (socket))

  (define make-remote-repl
    (lambda (service)
      (let ((server (make-server-socket service)))
        (format #t "~%remote-repl: session on ~a (~s)~%~%" (gethostname) server)
        (lambda ()
          (and (nonblock-byte-ready? (socket-port server))
               (letrec* ((in (current-input-port))
                         (out (current-output-port))
                         (err (current-error-port))
                         (restore-current-port
                          (lambda ()
                            (set-current-input-port! in)
                            (set-current-output-port! out)
                            (set-current-error-port! err)))
                         (set-remote-port!
                          (lambda (port)
                            (set-current-input-port! port)
                            (set-current-output-port! port)
                            (set-current-error-port! port))))
                 (call-with-socket (socket-accept server)
                   (lambda (socket)
                     (call/cc
                      (lambda (continue)
                        (with-exception-handler
                         (lambda (c)
                           (format #t "~%error in server: ~a (~s)~%" (gethostname) server)
                           (default-exception-handler c continue))
                         (lambda ()
                           (format #t "~&remote-repl: connect ~s~%" socket)
                           (call-with-port (transcoded-port (socket-port socket) (native-transcoder))
                             (lambda (port)
                               (set-remote-port! port)
                               (pretty-print (eval (read (open-string-input-port (get-string-all port))) (interaction-environment)))))))))))
                 (restore-current-port)))))))


  (define connect-remote-repl
    (lambda (node service)
      (format #t "~%connect to: ~a:~a (enter ^D to exit)~%~%" node service)
      (call/cc
       (lambda (done)
         (let loop ()
           (call/cc
            (lambda (continue)
              (format #t "REPL> ~!")
              (with-exception-handler
               (lambda (c)
                 (default-exception-handler c continue))
               (lambda ()
                 (let ((expr (read)))
                   (if (eof-object? expr)
                       (done)
                       (call-with-socket (make-client-socket node service)
                         (lambda (socket)
                           (call-with-port (transcoded-port (socket-port socket) (native-transcoder))
                             (lambda (port)
                               (format port "~s" expr)
                               (shutdown-output-port port)
                               (let ((ans (get-string-all port)))
                                 (if (eof-object? ans)
                                     (done)
                                     (format #t "~a~%~!" ans)))))))))))))
           (loop))))
      (format #t "~&[exit]~%")
      (unspecified)))


  ) ;[end]

#|

; server test
(import (core) (tidbits remote-repl))
(define pump-repl (make-remote-repl "6809")))
(let loop () (pump-repl) (loop)))

; client test
(import (tidbits remote-repl))
(connect-remote-repl "localhost" "6809")

|#