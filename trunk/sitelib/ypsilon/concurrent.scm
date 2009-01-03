#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon concurrent)
  (export define-thread-variable
          future
          call-with-spawn
          mailbox?
          make-mailbox
          shutdown-mailbox
          send
          recv
          messenger-bag?
          make-messenger-bag
          messenger-bag-put!
          messenger-bag-get!
          spawn
          thread-id
          make-uuid
          timeout-object?
          shutdown-object?
          shared-queue?
          make-shared-queue
          shared-queue-push!
          shared-queue-pop!
          shared-queue-shutdown
          shared-bag?
          make-shared-bag
          shared-bag-put!
          shared-bag-get!
          current-exception-printer)
  (import (core))

  (define-syntax define-thread-variable
    (syntax-rules ()
      ((_ (e0 . e1) e2 e3 ...)
       (define-thread-variable e0 (lambda e1 e2 e3 ...)))
      ((_ e0 e1)
       (begin
         (define temp (make-parameter e1))
         (define-syntax e0
           (identifier-syntax
             (_ (temp))
             ((set! _ x) (temp x))))))))

  (define-syntax future
    (syntax-rules ()
      ((_ e0 e1 ...)
       (let ((ans (make-shared-queue)))
         (call-with-spawn
          (lambda () e0 e1 ...)
          (lambda (c)
            (unless (condition? c)
              (shared-queue-push! ans c))
            (shared-queue-shutdown ans)))
         (lambda timeout (apply shared-queue-pop! ans timeout))))))

  (define call-with-spawn
    (lambda (body finally)
      (spawn
       (lambda ()
         (finally
          (call/cc
           (lambda (resume)
             (with-exception-handler
              (lambda (c)
                ((current-exception-printer) c)
                (and (serious-condition? c) (resume c)))
              (lambda () (body))))))))))

  (define mailbox?
    (lambda (x)
      (eq? (tuple-ref x 0) 'type:mailbox)))

  (define make-mailbox
    (lambda n
      (tuple 'type:mailbox (apply make-shared-queue n))))

  (define shutdown-mailbox
    (lambda (mbox)
      (assert (mailbox? mbox))
      (shared-queue-shutdown (tuple-ref mbox 1))))

  (define send
    (lambda (mbox obj . timeout)
      (assert (mailbox? mbox))
      (let ((retval (apply shared-queue-push! (tuple-ref mbox 1) obj timeout)))
        (cond ((shutdown-object? retval)
               (error 'send "mailbox is shutdowned" (cons* mbox obj timeout)))
              ((timeout-object? retval)
               (error 'send "operation timed out" (cons* mbox obj timeout)))
              (else retval)))))

  (define recv
    (lambda (mbox . timeout)
      (assert (mailbox? mbox))
      (let ((obj (apply shared-queue-pop! (tuple-ref mbox 1) timeout)))
        (cond ((shutdown-object? obj)
               (error 'recv "mailbox is shutdowned" (cons* mbox timeout)))
              ((timeout-object? obj)
               (error 'recv "operation timed out" (cons* mbox timeout)))
              (else obj)))))

  (define messenger-bag?
    (lambda (x)
      (eq? (tuple-ref x 0) 'type:messenger-bag)))

  (define make-messenger-bag
    (lambda n
      (tuple 'type:messenger-bag (apply make-shared-bag n))))

  (define messenger-bag-put!
    (lambda (bag receiver obj . timeout)
      (assert (messenger-bag? bag))
      (assert (string? receiver))
      (let ((retval (apply shared-bag-put! (tuple-ref bag 1) receiver obj timeout)))
        (cond ((timeout-object? retval)
               (error 'messenger-bag-put! "operation timed out" (cons* bag receiver obj timeout)))
              (else retval)))))

  (define messenger-bag-get!
    (lambda (bag receiver . timeout)
      (assert (messenger-bag? bag))
      (assert (string? receiver))
      (let ((obj (apply shared-bag-get! (tuple-ref bag 1) receiver timeout)))
        (cond ((timeout-object? obj)
               (error 'messenger-bag-get! "operation timed out" (cons* bag receiver timeout)))
              (else obj)))))
  ) ;[end]

#|
(import (concurrent))
(call-with-spawn (lambda () (car 3)) (lambda () 9))
(import (concurrent))
(define bag (make-messenger-bag 3))
(messenger-bag-put! bag "c1" '(hello) 100)
;=> #t
(messenger-bag-put! bag "c1" '(world) 100)
;=> #t
(messenger-bag-put! bag "c2" '(foo) 100)
;=> #t
(messenger-bag-put! bag "c2" '(bar) 100)
;=> #t
(messenger-bag-put! bag "c2" '(hoge) 100)
;=> #t
(messenger-bag-put! bag "c2" '(more) 100)
; error in messenger-bag-put!: operation timed out
(messenger-bag-get! bag "c1" 100)
;=> (hello)
(messenger-bag-get! bag "c2" 100)
;=> (foo)
(messenger-bag-get! bag "c1" 100)
;=> (world)
(messenger-bag-get! bag "c2" 100)
;=> (bar)
(messenger-bag-get! bag "c1" 100)
; error in messenger-bag-get!: operation timed out
(messenger-bag-get! bag "c2" 100)
;=> (hoge)
(import (concurrent) (match) (only (core) format))
(define (start out)
  (assert (mailbox? out))
  (let ((mbox (make-mailbox)))
    (call-with-spawn
     (lambda ()
       (define name "reverse")
       (let loop ()
         (match (recv mbox)
           (('exit) #t)
           (('reverse lst)
            (send out (cons name (reverse lst)))
            (loop))
           (('set arg)
            (set! name arg)
            (loop))
           (else "invalid message"))))
     (lambda (retval)
       (shutdown-mailbox out)
       (shutdown-mailbox mbox)
       (format (current-error-port) "## thread terminated~%")))
    mbox))
(define answer (make-mailbox))
(define request (start answer))
(send request '(reverse (1 2 3)) 100)
(recv answer)
;=> ("reverse" 3 2 1)
(send request '(set "REVERSE"))
(send request '(reverse (a b c)))
(recv answer)
;=> ("REVERSE" c b a)
(recv answer 1000)
; error in recv: operation timed out
(send request '(reverse 1))
;=> print backtrace and "## thread terminated"
(send request '(reverse (a b c)))
; error in send: mailbox is shutdowned
(recv answer)
; error in recv: mailbox is shutdowned
(recv answer 0)
(recv answer 10)
; error in recv: mailbox is shutdowned
(display-thread-status)
|#
