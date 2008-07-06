;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2008 Y.FUJITA, LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

  (begin

    (import (core primitives)
            (core destructuring)
            (core optimize)
            (core parameters)
            (core io)
            (core files))


    (define target-file-name "bootimage.vmi")

    (define prefix-file '("./boot/first-load.scm"))

    (define files
      '("./boot/r6rs-aux.scm"
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
        (let ((obj (read in)))
          (or (eof-object? obj)
              (begin
                (set! obj (compile obj))
                (put-fasl out obj)
                (compile-to in out))))))

    (define concat-file
      (lambda (in out)
        (let ((obj (read in)))
          (or (eof-object? obj)
              (begin
                (put-datum out obj)
                (concat-file in out))))))

    (define temp-port (transcoded-port (open-temporary-file-port) (native-transcoder)))

    (format #t ";; build ~a/~a~!" (current-directory) target-file-name)
    (parameterize ((backtrace #f) (pretty-print-unwrap-syntax #f) (coreform-optimize #t) (extend-lexical-syntax #t))
      (call-with-port
       (open-file-output-port target-file-name (file-options no-fail) (buffer-mode block) (native-transcoder))
       (lambda (output)

         (for-each (lambda (file)
                     (let ((path file))
                       (format #t "~%;; compiling ~a~!" path)
                       (compile-to (open-input-file path) output)))
                   prefix-file)

         (format temp-port "(begin~%")
         (for-each (lambda (file)
                     (let ((path file))
                       (format #t "~%;; concat ~a~!" path)
                       (concat-file (open-input-file path) temp-port)))
                   files)
         (format temp-port ")~%")
         (set-port-position! temp-port 0)
         (format #t "~%;; compiling ... ~%~!")
         (compile-to temp-port output))))

    ) ;[end]

