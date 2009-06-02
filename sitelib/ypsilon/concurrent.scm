#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon concurrent)
  (export define-thread-variable
          define-autoload-variable
          future
          make-mailbox
          mailbox?
          send
          recv
          shutdown-mailbox
          make-messenger-bag
          messenger-bag?
          messenger-bag-put!
          messenger-bag-get!
          make-shared-queue
          shared-queue?
          shared-queue-push!
          shared-queue-pop!
          shared-queue-shutdown
          make-shared-bag
          shared-bag?
          shared-bag-put!
          shared-bag-get!
          serializable?
          timeout-object?
          shutdown-object?
          call-with-spawn
          spawn
          spawn-timeout
          spawn-heap-limit
          make-uuid
          current-exception-printer)
  (import (core))

  (define-syntax define-thread-variable
    (syntax-rules ()
      ((_ var init)
       (begin
         (define param (make-parameter (list init)))
         (define mutator (lambda (val) (param (list val))))
         (define accessor
           (lambda ()
             (let ((p (param)))
               (if (local-heap-object? p)
                   (car p)
                   (let ((val init))
                     (param (list val)) val)))))
         (define-syntax var
           (identifier-syntax
             (_ (accessor))
             ((set! _ x) (mutator x))))))))

  (define-syntax define-autoload-variable
    (syntax-rules ()
      ((_ var init)
       (begin
         (define undefined (list #f))
         (define param (make-parameter undefined))
         (define accessor
           (lambda ()
             (if (on-primordial-thread?)
                 (let ((val init))
                   (set! accessor (lambda () val)) val)
                 (let ((val (param)))
                   (if (not (eq? val undefined))
                       val
                       (let ((val init))
                         (param val) val))))))
         (define-syntax var
           (identifier-syntax
             (_ (accessor))
             ((set! var x)
              (assertion-violation 'set! (format "attempt to modify autoload variable ~u" 'var) '(set! var x)))))))))

  (define future-error-condition
    (condition
     (make-error)
     (make-who-condition 'future)
     (make-message-condition "child thread has terminated by unhandled exception")))

  (define-syntax future
    (syntax-rules ()
      ((_ e0 e1 ...)
       (let ((queue (make-shared-queue)))
         (call-with-spawn
          (lambda () e0 e1 ...)
          (lambda (ans)
            (if (condition? ans)
                (shared-queue-push! queue future-error-condition)
                (shared-queue-push! queue ans))
            (shared-queue-shutdown queue)))
         (lambda timeout
           (let ((ans (apply shared-queue-pop! queue timeout)))
             (if (condition? ans) (raise ans) ans)))))))

  (define indent2
    (lambda (s)
      (call-with-string-output-port
       (lambda (out)
         (let ((in (open-string-input-port s)))
           (or (char=? (lookahead-char in) #\linefeed) (put-string out "  "))
           (let loop ((ch (get-char in)))
             (cond ((eof-object? ch))
                   ((char=? ch #\linefeed)
                    (put-string out "\n  ")
                    (loop (get-char in)))
                   (else
                    (put-char out ch)
                    (loop (get-char in))))))))))

  (define call-with-spawn
    (lambda (body finally)
      (spawn
       (lambda ()
         (finally
          (call/cc
           (lambda (resume)
             (with-exception-handler
              (lambda (c)
                (let ((e (current-error-port)))
                  (format e "\nerror in thread: unhandled exception has occurred\n")
                  (format e (indent2 (call-with-string-output-port
                                      (lambda (s)
                                        (set-current-error-port! s)
                                        ((current-exception-printer) c)
                                        (set-current-error-port! e)))))
                  (format e "[thread exit]\n\n"))
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
    (lambda (bag tag obj . timeout)
      (assert (messenger-bag? bag))
      (assert (string? tag))
      (let ((retval (apply shared-bag-put! (tuple-ref bag 1) tag obj timeout)))
        (cond ((timeout-object? retval)
               (error 'messenger-bag-put! "operation timed out" (cons* bag tag obj timeout)))
              (else retval)))))

  (define messenger-bag-get!
    (lambda (bag tag . timeout)
      (assert (messenger-bag? bag))
      (assert (string? tag))
      (let ((obj (apply shared-bag-get! (tuple-ref bag 1) tag timeout)))
        (cond ((timeout-object? obj)
               (error 'messenger-bag-get! "operation timed out" (cons* bag tag timeout)))
              (else obj)))))
  ) ;[end]

#|
(import (ypsilon concurrent))
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
(define-autoload-variable foo (begin (display "autoload triggered:") (list 1 2 3)))
foo
;=> prints "autoload triggered:"
;=> (1 2 3)
(begin (spawn (lambda () (display foo) (newline))) (unspecified))
;=> prints "(1 2 3)"
(define-autoload-variable foo2 (begin (display "autoload triggered:") (list 1 2 3)))
(begin (spawn (lambda () (display foo2) (newline))) (unspecified))
;=> prints "autoload triggered:"
;=> prints (1 2 3)
foo2
;=> prints "autoload triggered:"
;=> (1 2 3)
foo2
;=> (1 2 3)
|#







