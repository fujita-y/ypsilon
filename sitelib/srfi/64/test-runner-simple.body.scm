;; Copyright (c) 2005, 2006, 2007, 2012, 2013 Per Bothner
;; Added "full" support for Chicken, Gauche, Guile and SISC.
;;   Alex Shinn, Copyright (c) 2005.
;; Modified for Scheme Spheres by Álvaro Castro-Castilla, Copyright (c) 2012.
;; Support for Guile 2 by Mark H Weaver <mhw@netris.org>, Copyright (c) 2014.
;; Refactored by Taylan Ulrich Bayırlı/Kammer, Copyright (c) 2014, 2015.
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Helpers

(define (string-join strings delimiter)
  (if (null? strings)
      ""
      (let loop ((result (car strings))
                 (rest (cdr strings)))
        (if (null? rest)
            result
            (loop (string-append result delimiter (car rest))
                  (cdr rest))))))

(define (truncate-string string length)
  (define (newline->space c) (if (char=? #\newline c) #\space c))
  (let* ((string (string-map newline->space string))
         (fill "...")
         (fill-len (string-length fill))
         (string-len (string-length string)))
    (if (<= string-len (+ length fill-len))
        string
        (let-values (((q r) (floor/ length 4)))
          ;; Left part gets 3/4 plus the remainder.
          (let ((left-end (+ (* q 3) r))
                (right-start (- string-len q)))
            (string-append (substring string 0 left-end)
                           fill
                           (substring string right-start string-len)))))))

(define (print runner format-string . args)
  (apply format #t format-string args)
  (let ((port (%test-runner-log-port runner)))
    (when port
      (apply format port format-string args))))

;;; Main

(define (test-runner-simple)
  (let ((runner (test-runner-null)))
    (test-runner-reset runner)
    (test-runner-on-group-begin!     runner test-on-group-begin-simple)
    (test-runner-on-group-end!       runner test-on-group-end-simple)
    (test-runner-on-final!           runner test-on-final-simple)
    (test-runner-on-test-begin!      runner test-on-test-begin-simple)
    (test-runner-on-test-end!        runner test-on-test-end-simple)
    (test-runner-on-bad-count!       runner test-on-bad-count-simple)
    (test-runner-on-bad-end-name!    runner test-on-bad-end-name-simple)
    (%test-runner-on-bad-error-type! runner on-bad-error-type)
    runner))

(when (not (test-runner-factory))
  (test-runner-factory test-runner-simple))

(define (test-on-group-begin-simple runner name count)
  (when (null? (test-runner-group-stack runner))
    (maybe-start-logging runner)
    (print runner "Test suite begin: ~a~%" name)))

(define (test-on-group-end-simple runner)
  (let ((name (car (test-runner-group-stack runner))))
    (when (= 1 (length (test-runner-group-stack runner)))
      (print runner "Test suite end: ~a~%" name))))

(define (test-on-final-simple runner)
  (print runner "Passes:            ~a\n" (test-runner-pass-count runner))
  (print runner "Expected failures: ~a\n" (test-runner-xfail-count runner))
  (print runner "Failures:          ~a\n" (test-runner-fail-count runner))
  (print runner "Unexpected passes: ~a\n" (test-runner-xpass-count runner))
  (print runner "Skipped tests:     ~a~%" (test-runner-skip-count runner))
  (maybe-finish-logging runner))

(define (maybe-start-logging runner)
  (let ((log-file (%test-runner-log-file runner)))
    (when log-file
      ;; The possible race-condition here doesn't bother us.
      (when (file-exists? log-file)
        (delete-file log-file))
      (%test-runner-log-port! runner (open-output-file log-file))
      (print runner "Writing log file: ~a~%" log-file))))

(define (maybe-finish-logging runner)
  (let ((log-file (%test-runner-log-file runner)))
    (when log-file
      (print runner "Wrote log file: ~a~%" log-file)
      (close-output-port (%test-runner-log-port runner)))))

(define (test-on-test-begin-simple runner)
  (values))

(define (test-on-test-end-simple runner)
  (let* ((result-kind (test-result-kind runner))
         (result-kind-name (case result-kind
                             ((pass) "PASS") ((fail) "FAIL")
                             ((xpass) "XPASS") ((xfail) "XFAIL")
                             ((skip) "SKIP")))
         (name (let ((name (test-runner-test-name runner)))
                 (if (string=? "" name)
                     (truncate-string
                      (format #f "~a" (test-result-ref runner 'source-form))
                      30)
                     name)))
         (label (string-join (append (test-runner-group-path runner)
                                     (list name))
                             ": ")))
    (print runner "[~a] ~a~%" result-kind-name label)
    (when (memq result-kind '(fail xpass))
      (let ((nil (cons #f #f)))
        (define (found? value)
          (not (eq? nil value)))
        (define (maybe-print value message)
          (when (found? value)
            (print runner message value)))
        (let ((file (test-result-ref runner 'source-file "(unknown file)"))
              (line (test-result-ref runner 'source-line "(unknown line)"))
              (expression (test-result-ref runner 'source-form))
              (expected-value (test-result-ref runner 'expected-value nil))
              (actual-value (test-result-ref runner 'actual-value nil))
              (expected-error (test-result-ref runner 'expected-error nil))
              (actual-error (test-result-ref runner 'actual-error nil)))
          (print runner "~a:~a: ~s~%" file line expression)
          (maybe-print expected-value "Expected value: ~s~%")
          (maybe-print expected-error "Expected error: ~a~%")
          (when (or (found? expected-value) (found? expected-error))
            (maybe-print actual-value "Returned value: ~s~%"))
          (maybe-print actual-error "Raised error: ~a~%")
          (newline))))))

(define (test-on-bad-count-simple runner count expected-count)
  (print runner "*** Total number of tests was ~a but should be ~a. ***~%"
          count expected-count)
  (print runner
         "*** Discrepancy indicates testsuite error or exceptions. ***~%"))

(define (test-on-bad-end-name-simple runner begin-name end-name)
  (error (format #f "Test-end \"~a\" does not match test-begin \"~a\"."
                 end-name begin-name)))

(define (on-bad-error-type runner type error)
  (print runner "WARNING: unknown error type predicate: ~a~%" type)
  (print runner "         error was: ~a~%" error))

;;; test-runner-simple.scm ends here
