#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2008 Y.FUJITA, LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (tidbits remote-repl)

  (export blocking-repl-server
          blocking-repl-client)

  (import (core) (socket))

  (define blocking-repl-server
    (lambda (service)
      (define console-in)
      (define console-out)
      (define console-err)
      (define save-console-port
        (lambda ()
          (set! console-in (current-input-port))
          (set! console-out (current-output-port))
          (set! console-err (current-error-port))))
      (define restore-console-port
        (lambda ()
          (set-current-input-port! console-in)
          (set-current-output-port! console-out)
          (set-current-error-port! console-err)))
      (define set-remote-port!
        (lambda (port)
          (set-current-input-port! port)
          (set-current-output-port! port)
          (set-current-error-port! port)))
      (let ((server (make-server-socket service)))
        (format #t "~%blocking-repl-server on: ~s~%send (exit) to terminate server.~%~%" server)
        (save-console-port)
        (let loop ()
          (call/cc
           (lambda (continue)
             (restore-console-port)
             (call-with-socket (socket-accept server)
               (lambda (socket)
                 (define resume
                   (lambda ()
                     (socket-shutdown socket SHUT_RDWR)
                     (socket-close socket)
                     (continue)))
                 (define process
                   (lambda ()
                     (with-exception-handler
                      (lambda (c)
                        (format #t "~%exception in server ~s~%" server)
                        (default-exception-handler c resume))
                      (lambda ()
                        (format #t "connect: ~s~%" socket)
                        (call-with-port (transcoded-port (socket-port socket) (native-transcoder))
                          (lambda (port)
                            (set-remote-port! port)
                            (pretty-print (eval (read (open-string-input-port (get-string-all port))) (interaction-environment)))))))))
                 (process)))))
          (loop)))))

  (define blocking-repl-client
    (lambda (node service)
        (format #t "~%blocking-repl-client connect to: ~s ~s~%enter ^D to prompt to terminate client.~%~%" node service)
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
                    (exit)
                    (call-with-socket (make-client-socket node service)
                      (lambda (socket)
                        (call-with-port (transcoded-port (socket-port socket) (native-transcoder))
                          (lambda (port)
                            (format port "~s" expr)
                            (shutdown-output-port port)
                            (format #t "~a~%~!" (get-string-all port))))))))))))
        (loop))))


  ) ;[end]
#|
(import (tidbits remote-repl))
(blocking-repl-server "50000")
(import (tidbits remote-repl))
(blocking-repl-client "localhost" "50000")
|#


#|

;; server
  (import (core) (socket))
  (call-with-socket (socket-accept (make-server-socket "50000"))
    (lambda (socket)
      (call-with-port (transcoded-port (socket-port socket) (native-transcoder))
        (lambda (port)
          (format port "~s" (eval (read (open-string-input-port (get-string-all port))) (interaction-environment)))))))
  
;; client
  (import (core) (socket))
  (call-with-socket (make-client-socket "localhost" "50000")
    (lambda (socket)
      (call-with-port (transcoded-port (socket-port socket) (native-transcoder))
        (lambda (port)
          (format port "(append '(1 2 3) '(4 5 6))")
          (shutdown-output-port port)
          (read port)))))

;; repl-server
(import (core) (socket))
(let ((server (make-server-socket "50000")))
  (let loop ()
    (call-with-socket (socket-accept server)
      (lambda (socket)
        (format #t "connect: ~s~%" socket)
        (call-with-port (transcoded-port (socket-port socket) (native-transcoder))
          (lambda (port)
            (format port "~s" (eval (read (open-string-input-port (get-string-all port))) (interaction-environment)))))))
    (loop)))

;; repl-client
(import (core) (socket))
(let loop ()
  (format #t "REPL> ~!")
  (let ((expr (read)))
    (call-with-socket (make-client-socket "localhost" "50000")
      (lambda (socket)
        (call-with-port (transcoded-port (socket-port socket) (native-transcoder))
          (lambda (port)
            (format port "~s" expr)
            (shutdown-output-port port)
            (format #t "=> ~s~%~!" (read port)))))))
  (loop))

|#
