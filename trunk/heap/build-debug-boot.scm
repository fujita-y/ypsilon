#!/usr/local/bin/runic

;; Copyright (c) 2007, 2008 Yoshikatsu Fujita. All rights reserved. 
;; runic scheme environment
;; See license.txt for terms and conditions of use.

  (begin

    (import (core primitives)
            (core destructuring)
            (core optimize)
            (core parameters)
            (core io)
            (core files))

    (define target-file-name "debug-boot.vmi")

    (define files
      '("./boot/first-load.scm"
        "./boot/r6rs-aux.scm"
        "./boot/common.scm"
        "./boot/parameter.scm"
        "./boot/macro/initial.scm"
        "./boot/macro/expand.scm"
        "./boot/macro/base.scm"
        "./boot/macro/derived.scm"
        "./boot/macro/quasi.scm"
        "./boot/macro/synpat.scm"
        "./boot/macro/syntmp.scm"
        "./boot/macro/synrule.scm"
        "./boot/macro/syncase.scm"
        "./boot/macro/library.scm"
        "./boot/macro/synenv.scm"
        "./boot/compile.scm"
        "./boot/dynamic-wind.scm"
        "./boot/exception.scm"
        "./boot/record.scm"
        "./boot/condition.scm"
        "./boot/pp.scm"
        "./boot/eval.scm"
        "./boot/interaction.scm"
        "./boot/libraries.scm"))

    (define compile-to
      (lambda (in out)
        (current-source-comments (and (backtrace) (make-core-hashtable)))
        (let ((obj (core-read in (current-source-comments) 'compile)))
          (if (eof-object? obj)
              (format out "~%;~%")
              (begin
                (set! obj (compile obj))
                (pretty-print obj out)
                (format out "~%")
                (compile-to in out))))))

    (format #t ";; build ~a/~a (debug)~!" (current-directory) target-file-name)
    (parameterize ((backtrace #t) (pretty-print-unwrap-syntax #f) (coreform-optimize #t) (extend-lexical-syntax #t))
      (call-with-port
       (open-file-output-port target-file-name (file-options no-fail) (buffer-mode block) (native-transcoder))
       (lambda (output)
         (for-each (lambda (file)
                     (let ((path file))
                       (format #t "~%;; compiling ~a~!" path)
                       (compile-to (open-input-file path) output)))
                   files)
         (format #t "~%~!"))))
    ) ;[end]

