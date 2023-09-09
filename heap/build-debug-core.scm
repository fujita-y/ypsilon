;;; Copyright (c) 2004-2022 Yoshikatsu Fujita / LittleWing Company Limited.
;;; See LICENSE file for terms and conditions of use.

;; if libraries have (define <id> <subr>) form and <id> is referenced within same
;; library, prebind could be screwed up, to workaround this issue,
;; replace <id> with <subr> in offending reference.
;; ex. [(define first car) ... (first '(1 2))] => [(define first car) ... (car '(1 2))]

;; to implement (compile-library)
;; check that library imported or not, if imported, abort compile with warning
;; this should avoid problem

  #|

    core library dependencies

    exceptions    :
    parameters    :
    arithmetic    :
    sorting       :
    bytevectors   :
    syntax-case   :
    control       : syntax-case
    optargs       : syntax-case
    lists         : optargs
    struct        : syntax-case lists
    destructuring : syntax-case lists
    records       : syntax-case lists destructuring
    conditions    : syntax-case records
    enums         : struct lists sorting
    io            : syntax-case lists conditions bytevectors optargs
    files         : io lists
    bytevector-transcoders : optargs io bytevectors
    unicode-assistants : io files lists bytevectors bytevector-transcoders
    unicode       : io files lists bytevectors bytevector-transcoders
    hashtables    : lists unicode optargs

    coreform optimizer dependencies

    utils            : (destructuring lists parameters)
    let-transform    : utils
    set-transform    : utils
    lambda-transform : utils
    constant-folding : utils
    local-transform  : utils constant-folding

  |#


  ;; import everything to determine more subr
  (import (core primitives)
          (core destructuring)
          (core optimize)
          (core parameters)
          (core enums)
          (core io)
          (core files)
          ;; end of vital library
          (core exceptions)
          (core arithmetic)
          (core sorting)
          (core bytevectors)
          (core syntax-case)
          (core r5rs)
          (core control)
          (core optargs)
;         (core chkarg)
          (core lists)
          (core records)
          (core conditions)
          (core bytevector-transcoders)
          (core unicode-assistants)
          (core unicode)
          (core hashtables)
          (core struct))

;  (define source-directory (format "~a/../stdlib" (current-directory)))
;  (add-library-path source-directory)
  (define source-directory "../stdlib")
  (scheme-library-paths (cons source-directory (scheme-library-paths)))

  (define target-file-name "debug-core.vmi")

  (define core-files
    '("core/exceptions.scm"
      "core/parameters.scm"
      "core/arithmetic.scm"
      "core/sorting.scm"
      "core/bytevectors.scm"
      "core/syntax-case.scm"
      "core/r5rs.scm"
      ;
      "core/control.scm"
      "core/optargs.scm"
;     "core/chkarg.scm"
      "core/lists.scm"
      "core/destructuring.scm"
      "core/records.scm"
      "core/conditions.scm"
      "core/enums.scm"
      "core/io.scm"
      "core/files.scm"
      "core/bytevector-transcoders.scm"
      "core/unicode-assistants.scm"
      "core/unicode.scm"
      "core/hashtables.scm"
      "core/struct.scm"
      "core/optimize.scm"
      "core.scm"
#|
      "./rnrs/base.scm"
      "./rnrs/unicode.scm"
      "./rnrs/bytevectors.scm"
      "./rnrs/lists.scm"
      "./rnrs/sorting.scm"
      "./rnrs/control.scm"
      "./rnrs/records/syntactic.scm"
      "./rnrs/records/procedural.scm"
      "./rnrs/records/inspection.scm"
      "./rnrs/exceptions.scm"
      "./rnrs/conditions.scm"
      "./rnrs/io/ports.scm"
      "./rnrs/io/simple.scm"
      "./rnrs/files.scm"
      "./rnrs/programs.scm"
      "./rnrs/arithmetic/fixnums.scm"
      "./rnrs/arithmetic/flonums.scm"
      "./rnrs/arithmetic/bitwise.scm"
      "./rnrs/syntax-case.scm"
      "./rnrs/hashtables.scm"
      "./rnrs/enums.scm"
      "./rnrs/r5rs.scm"
      "./rnrs/mutable-strings.scm"
      "./rnrs/mutable-pairs.scm"
      "./rnrs/eval.scm"
      "./rnrs.scm"
      "./scheme/base.scm"
      "./scheme/case-lambda.scm"
      "./scheme/char.scm"
      "./scheme/complex.scm"
      "./scheme/cxr.scm"
      "./scheme/eval.scm"
      "./scheme/file.scm"
      "./scheme/inexact.scm"
      "./scheme/lazy.scm"
      "./scheme/list.scm"
      "./scheme/load.scm"
      "./scheme/process-context.scm"
      "./scheme/r5rs.scm"
      "./scheme/read.scm"
      "./scheme/repl.scm"
      "./scheme/time.scm"
      "./scheme/write.scm"
|#
      ))

  (define compile-to
    (lambda (in out)
      (current-source-comments (and (backtrace) (make-core-hashtable)))
      (let ((obj (core-read in (current-source-comments) 'compile)))
        (if (eof-object? obj)
            (format out "~%")
            (let ((obj (compile obj)))
              (pretty-print obj out)
              (format out "~%")
              (compile-to in out))))))

  (assert (string-contains (current-directory) (home-directory)))

  (format #t ";; build ~a/~a~!" (current-directory) target-file-name)
  (parameterize ((backtrace #t) (pretty-print-unwrap-syntax #f) #;(coreform-optimize #t))
    (call-with-port
     (open-file-output-port target-file-name (file-options no-fail) (buffer-mode block) (native-transcoder))
     (lambda (out)
       (for-each (lambda (file)
                   (let ((path (string-append source-directory "/" file)))
                     (format #t "~%;; compiling ~a~!" path)
                     (compile-to (open-input-file path) out)))
                 core-files)
       (format #t "~%~!"))))
