#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2008 Y.FUJITA, LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (socket)

  (export make-client-socket
          make-server-socket
          call-with-socket
          shutdown-output-port
          socket?
          
          make-socket
          socket-port
          socket-accept
          socket-send
          socket-recv
          socket-shutdown
          socket-close
          
          AF_UNSPEC
          AF_INET
          AF_INET6
          SOCK_STREAM
          SOCK_DGRAM
          SOCK_RAW
          SOCK_RDM
          SOCK_SEQPACKET
          AI_PASSIVE
          AI_CANONNAME
          AI_NUMERICHOST
          AI_V4MAPPED
          AI_ALL
          AI_ADDRCONFIG
          SHUT_RD
          SHUT_WR
          SHUT_RDWR
          MSG_OOB
          MSG_PEEK
          MSG_DONTROUTE
          MSG_CTRUNC
          MSG_PROBE
          MSG_TRUNC
          MSG_DONTWAIT
          MSG_EOR
          MSG_WAITALL
          MSG_FIN
          MSG_SYN
          MSG_CONFIRM
          MSG_RST
          MSG_ERRQUEUE
          MSG_NOSIGNAL
          MSG_MORE
          MSG_EOF)

  (import (core))

  (define-syntax define-param
      (syntax-rules ()
        ((_ name)
         (define name (architecture-feature 'name)))))

  (define-param AF_UNSPEC)
  (define-param AF_INET)
  (define-param AF_INET6)
  (define-param SOCK_STREAM)
  (define-param SOCK_DGRAM)
  (define-param SOCK_RAW)
  (define-param SOCK_RDM)
  (define-param SOCK_SEQPACKET)
  (define-param AI_PASSIVE)
  (define-param AI_CANONNAME)
  (define-param AI_NUMERICHOST)
  (define-param AI_V4MAPPED)
  (define-param AI_ALL)
  (define-param AI_ADDRCONFIG)
  (define-param SHUT_RD)
  (define-param SHUT_WR)
  (define-param SHUT_RDWR)
  (define-param MSG_OOB)
  (define-param MSG_PEEK)
  (define-param MSG_DONTROUTE)
  (define-param MSG_CTRUNC)
  (define-param MSG_PROBE)
  (define-param MSG_TRUNC)
  (define-param MSG_DONTWAIT)
  (define-param MSG_EOR)
  (define-param MSG_WAITALL)
  (define-param MSG_FIN)
  (define-param MSG_SYN)
  (define-param MSG_CONFIRM)
  (define-param MSG_RST)
  (define-param MSG_ERRQUEUE)
  (define-param MSG_NOSIGNAL)
  (define-param MSG_MORE)
  (define-param MSG_EOF)

  (define make-client-socket
    (lambda (node service . options)
      (let-optionals options ((ai-family AF_INET) (ai-socktype SOCK_STREAM) (ai-flags (+ AI_V4MAPPED AI_ADDRCONFIG)) (protocol 0))
        (make-socket node service ai-family ai-socktype ai-flags protocol))))

  (define make-server-socket
    (lambda (service . options)
      (let-optionals options ((ai-family AF_INET) (protocol 0))
        (make-socket #f service ai-family SOCK_STREAM AI_PASSIVE protocol))))

  (define call-with-socket
    (lambda (socket proc)
      (call-with-values
        (lambda () (proc socket))
        (lambda args
          (socket-close socket)
          (apply values args)))))

  ) ;[end]

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
