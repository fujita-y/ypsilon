#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2008 Y.FUJITA, LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (concurrent)

  (export make-shared-queue
          shared-queue-push!
          shared-queue-pop!
          make-mailbox
          mailbox?
          send
          receive
          spawn
          call-with-spawn
          call-with-spawn-quiet
          current-exception-printer)

  (import (core))

  (define call-with-spawn
    (lambda (call recv)
      (spawn
       (lambda ()
         (recv
          (call/cc
           (lambda (resume)
             (with-exception-handler
              (lambda (c)
                ((current-exception-printer) c)
                (and (serious-condition? c) (resume c)))
              (lambda () (call))))))))))

  (define call-with-spawn-quiet
    (lambda (call recv)
      (spawn
       (lambda ()
         (recv
          (call/cc
           (lambda (resume)
             (with-exception-handler
              (lambda (c)
                (and (serious-condition? c) (resume c)))
              (lambda () (call))))))))))

  (define make-mailbox
    (lambda n
      (tuple 'type:mailbox (apply make-shared-queue n))))

  (define mailbox?
    (lambda (x)
      (eq? (tuple-ref x 0) 'type:mailbox)))

  (define send
    (lambda (mbox obj)
      (assert (mailbox? mbox))
      (shared-queue-push! (tuple-ref mbox 1) (object->bytevector obj))))

  (define receive
    (lambda (mbox)
      (assert (mailbox? mbox))
      (bytevector->object (shared-queue-pop! (tuple-ref mbox 1)))))

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
       (close-mailbox mbox) ;; should be added
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
(send request '(reverse 1)) 
;=> print backtrace and "## thread terminated"
(send request '(reverse (a b c)))
(receive answer)
;=> wait forever (should be error reported)
|#



