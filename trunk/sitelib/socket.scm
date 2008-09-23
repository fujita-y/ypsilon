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

  (define-syntax define-const
    (syntax-rules ()
      ((_ name)
       (define name (architecture-feature 'name)))))

  (define-const AF_UNSPEC)
  (define-const AF_INET)
  (define-const AF_INET6)
  (define-const SOCK_STREAM)
  (define-const SOCK_DGRAM)
  (define-const SOCK_RAW)
  (define-const SOCK_RDM)
  (define-const SOCK_SEQPACKET)
  (define-const AI_PASSIVE)
  (define-const AI_CANONNAME)
  (define-const AI_NUMERICHOST)
  (define-const AI_V4MAPPED)
  (define-const AI_ALL)
  (define-const AI_ADDRCONFIG)
  (define-const SHUT_RD)
  (define-const SHUT_WR)
  (define-const SHUT_RDWR)
  (define-const MSG_OOB)
  (define-const MSG_PEEK)
  (define-const MSG_DONTROUTE)
  (define-const MSG_CTRUNC)
  (define-const MSG_PROBE)
  (define-const MSG_TRUNC)
  (define-const MSG_DONTWAIT)
  (define-const MSG_EOR)
  (define-const MSG_WAITALL)
  (define-const MSG_FIN)
  (define-const MSG_SYN)
  (define-const MSG_CONFIRM)
  (define-const MSG_RST)
  (define-const MSG_ERRQUEUE)
  (define-const MSG_NOSIGNAL)
  (define-const MSG_MORE)
  (define-const MSG_EOF)

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
