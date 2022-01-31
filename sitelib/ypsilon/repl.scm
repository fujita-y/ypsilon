#!nobacktrace
;;; Copyright (c) 2004-2022 Yoshikatsu Fujita / LittleWing Company Limited.
;;; See LICENSE file for terms and conditions of use.

(library (ypsilon repl)
  (export connect-repl make-repl-service)
  (import (core) (ypsilon socket))

  (define make-repl-service
    (lambda (socket-num)
      (let ((server (make-server-socket socket-num)))
        (format #t "~%listen: ~a:~a ~s~%~!" (gethostname) socket-num server)
        (lambda ()
          (and (nonblock-byte-ready? (socket-port server))
               (call-with-socket
                 (socket-accept server)
                 (lambda (socket)
                   (call-with-port
                     (transcoded-port (socket-port socket) (native-transcoder))
                     (lambda (port)
                       (set-current-error-port! port)
                       (set-current-input-port! port)
                       (set-current-output-port! port)
                       (call/cc
                         (lambda (continue)
                           (with-exception-handler
                             (lambda (c)
                               (format #t "~%error in ~a (~s)~%" (gethostname) server)
                               ((current-exception-printer) c)
                               (and (serious-condition? c) (continue)))
                             (lambda ()
                               (pretty-print
                                 (eval
                                   (read (open-string-input-port (get-string-all port)))
                                   (interaction-environment))))))))))))))))

  (define connect-repl
    (lambda (node socket-num)
      (format #t "~%(enter ^D to exit)~%~%")
      (call/cc
        (lambda (done)
          (let loop ()
            (call/cc
              (lambda (continue)
                (format #t "~a:~a> ~!" node socket-num)
                (with-exception-handler
                  (lambda (c)
                    ((current-exception-printer) c)
                    (and (serious-condition? c) (continue)))
                  (lambda ()
                    (let ((expr (read)))
                      (if (eof-object? expr)
                          (done)
                          (call-with-socket
                            (make-client-socket node socket-num)
                            (lambda (socket)
                              (call-with-port
                                (transcoded-port (socket-port socket) (native-transcoder))
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

;; start server
(import (core) (ypsilon repl))
(let ((tick (make-repl-service "6809")))
  (let loop () (tick) (usleep 1000) (loop)))

;; start client
(import (core) (ypsilon repl))
(connect-repl "localhost" "6809")

|#
