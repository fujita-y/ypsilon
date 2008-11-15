#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2008 Y.FUJITA, LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (concurrent)

  (export make-shared-queue
          shared-queue-push!
          shared-queue-pop!
          make-mailbox
          shutdown-mailbox
          mailbox?
          send
          receive
          spawn
          call-with-spawn
          call-with-quiet-spawn
          current-exception-printer)

  (import (core))

  (define call-with-spawn
    (lambda (body . finally)
      (let ((finally (or (and (pair? finally) (car finally)) values)))
        (spawn
         (lambda ()
           (finally
            (call/cc
             (lambda (resume)
               (with-exception-handler
                (lambda (c)
                  ((current-exception-printer) c)
                  (and (serious-condition? c) (resume c)))
                (lambda () (body)))))))))))

  (define call-with-quiet-spawn
    (lambda (body . finally)
      (let ((finally (or (and (pair? finally) (car finally)) values)))
        (spawn
         (lambda ()
           (finally
            (call/cc
             (lambda (resume)
               (with-exception-handler
                (lambda (c)
                  (and (serious-condition? c) (resume c)))
                (lambda () (body)))))))))))

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
    (lambda (mbox obj)
      (assert (mailbox? mbox))
      (or (shared-queue-push! (tuple-ref mbox 1) (object->bytevector obj))
          (assertion-violation 'send "mailbox is shutdowned" (list mbox obj)))))

  (define receive
    (lambda (mbox . timeout)
      (assert (mailbox? mbox))
      (let ((obj (apply shared-queue-pop! (tuple-ref mbox 1) timeout)))
        (cond ((bytevector? obj) (bytevector->object obj))
              ((timeout-object? obj) obj)
              (else (assertion-violation 'receive "mailbox is shutdowned" (list mbox obj)))))))

  ) ;[end]

#|
(import (concurrent) (match) (only (core) format))
(define (start out)
  (assert (mailbox? out))
  (let ((mbox (make-mailbox)))
    (call-with-spawn
     (lambda ()
       (define name "reverse")
       (let loop ()
         (match (receive mbox)
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

(send request '(reverse (1 2 3)))
(receive answer)
;=> ("reverse" 3 2 1)
(send request '(set "REVERSE"))
(send request '(reverse (a b c)))
(receive answer) 
;=> ("REVERSE" c b a)
(receive answer 1000)
; #<timeout>
(send request '(reverse 1))
;=> print backtrace and "## thread terminated"
(send request '(reverse (a b c))) 
; error in send: mailbox is shutdowned
(receive answer)
; error in receive: mailbox is shutdowned
(receive answer 0)
(receive answer 10)
; error in receive: mailbox is shutdowned
(display-thread-status)
|#


