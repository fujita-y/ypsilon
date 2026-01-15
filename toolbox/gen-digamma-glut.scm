;;; Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.
;;; See LICENSE file for terms and conditions of use.

(import (core)
        (common pregexp)
        (only (srfi srfi-13) string-join))

(define re-api-entry (pregexp "^FGAPI\\s+([a-zA-Z0-9*]+)\\s+FGAPIENTRY\\s+([a-zA-Z0-9]+)[(](.*)[)];\\s*$"))
(define re-api-entry-continue (pregexp "^FGAPI\\s+.+[,]\\s*$"))
(define re-api-const (pregexp "^#define\\s+(GLUT_[A-Z0-9_]+)\\s+(.*)$"))
(define re-api-callback (pregexp "[(][*] callback[)][(][^)]*[)]"))
(define re-trim-whitespace (pregexp "^\\s*(.+[^\\s])\\s*$"))

(define types
    '(("GLenum" . "unsigned-int")
      ("GLboolean" . "uint8_t")
      ("GLbitfield" . "unsigned-int")
      ("GLvoid" . "void")
      ("GLbyte" . "int8_t")
      ("GLubyte" . "uint8_t")
      ("GLshort" . "short")
      ("GLushort" . "unsigned-short")
      ("GLint" . "int")
      ("GLuint" . "unsigned-int")
      ("GLsizei" . "int")
      ("GLfloat" . "float")
      ("GLclampf" . "float")
      ("GLdouble" . "double")
      ("GLclampd" . "double")
      ("unsigned int" . "unsigned-int")
      ("int" . "int")
      ))

(define pregexp-substring
  (lambda (s match index)
    (let ((b (list-ref match index)))
      (substring s (car b) (cdr b)))))

(define trim-whitespace
  (lambda (s)
    (cond ((pregexp-match-positions re-trim-whitespace s)
           => (lambda (m) (pregexp-substring s m 1)))
          (else s))))

(define split-args
  (lambda (s)
    (let loop ((lst (string->list s)) (level 0) (acc '()) (ans '()))
      (cond ((null? lst)
             (reverse (cons (list->string (reverse acc)) ans)))
            ((and (= level 0) (char=? (car lst) #\,))
             (loop (cdr lst) level '() (cons (list->string (reverse acc)) ans)))
            ((= (char->integer (car lst)) 40) ;; (
             (loop (cdr lst) (+ level 1) (cons (car lst) acc) ans))
            ((= (char->integer (car lst)) 41) ;; )
             (loop (cdr lst) (- level 1) (cons (car lst) acc) ans))
            (else
             (loop (cdr lst) level (cons (car lst) acc) ans))))))

(define for-each-api-entry-line
  (lambda (proc input)
    (let loop ()
      (let ((line (get-line input)))
        (cond ((eof-object? line) (unspecified))
              ((pregexp-match re-api-entry-continue line)
               (let loop2 ((line line))
                 (let ((line2 (get-line input)))
                    (cond ((string-contains line2 #\;)
                            (proc (string-append line line2))
                            (loop))
                          (else
                            (loop2 (string-append line line2)))))))
              (else
               (proc line)
               (loop)))))))

(define convert-type
  (lambda (s)
    (cond ((string-contains s "*") "void*")
          (else
            (let loop ((type types))
              (cond ((null? type) s)
                    ((string-contains s (caar type)) (cdar type))
                    (else
                     (loop (cdr type)))))))))

(define parse-api-entry
  (lambda (path)
    (let ((acc '()))
      (call-with-port
        (open-file-input-port
          path
          (file-options)
          (buffer-mode block)
          (native-transcoder))
        (lambda (input)
          (for-each-api-entry-line
            (lambda (line)
              (cond
                ((pregexp-match-positions re-api-entry line)
                 =>
                 (lambda (m)
                   (let ((ret (pregexp-substring line m 1))
                         (name (pregexp-substring line m 2))
                         (args (pregexp-substring line m 3)))
                     (let ((args (map trim-whitespace (split-args args))))
                       (set! acc (cons (list ret name args) acc))))))))
            input)))
      (reverse acc))))

(define output-api-entry
  (lambda (port lst)
    (for-each
      (lambda (e)
        (destructuring-bind (ret name args) e
          (format port "  ;; ~a ~a(~a)~%~!" ret name (string-join args ", "))
          (let ((ret (convert-type ret)) (args (string-join (map convert-type args) " ")))
          (if (string=? args "void")
              (format port "  (define-cdecl ~a ~a ())~%~!" ret name)
              (format port "  (define-cdecl ~a ~a (~a))~%~!" ret name args)))))
      lst)))

(define parse-api-const
  (lambda (path)
    (let ((acc '()))
      (call-with-port
        (open-file-input-port path (file-options) (buffer-mode block) (native-transcoder))
        (lambda (input)
          (let loop ((line (get-line input)))
            (cond ((eof-object? line) (unspecified))
                  ((pregexp-match-positions re-api-const line)
                   =>
                   (lambda (m)
                     (let ((name (pregexp-substring line m 1)) (value (pregexp-substring line m 2)))
                       (set! acc
                             (cons (list
                                     (trim-whitespace name)
                                     (pregexp-replace "0x" (trim-whitespace value) "#x"))
                                   acc)))
                     (loop (get-line input))))
                  (else (loop (get-line input)))))))
      (reverse acc))))

(define output-api-const
  (lambda (port lst)
    (for-each
      (lambda (e)
        (destructuring-bind (name value) e
          (format port "  (define ~a ~a)~%~!" name value)))
      lst)))

(define output-exports
  (lambda (port lst)
    (let loop ((lst lst) (first #t))
      (cond ((null? lst)
             (format port ")~%"))
            (first
             (format port "  (export ~a" (car lst))
             (loop (cdr lst) #f))
            (else
             (format port "~%          ~a" (car lst))
             (loop (cdr lst) #f))))))

(define api-entry (parse-api-entry "toolbox/h/glut.h"))
(define api-const (parse-api-const "toolbox/h/glut.h"))
(define exports (append (map cadr api-entry) (map car api-const)))

(call-with-port
  (open-file-output-port "toolbox/output/glut.scm" (file-options no-fail) (buffer-mode block) (native-transcoder))
  (lambda (port)
    (format port "#!nobacktrace~%")
    (format port ";;; Copyright (c) 2004-2026 Yoshikatsu Fujita / LittleWing Company Limited.~%")
    (format port ";;; See LICENSE file for terms and conditions of use.~%~%")
    (format port "(library (ypsilon glut)~%")
    (output-exports port exports)
    (format port "  (import (core) (ypsilon c-ffi))~%")
    (format port "  (define libGLUT~%")
    (format port "    (let ((sysname (architecture-feature 'sysname)))~%")
    (format port "      (cond ((string-contains sysname \"darwin\")~%")
    (format port "             (load-shared-object \"GLUT.framework/GLUT\"))~%")
    (format port "            ((string-contains sysname \"linux\")~%")
    (format port "             (load-shared-object \"libglut.so.3\"))~%")
    (format port "            (else~%")
    (format port "              (assertion-violation 'load-shared-object \"can not load GLUT library, unknown operating system\")))))~%")
    (format port "  (define-syntax define-cdecl~%")
    (format port "    (syntax-rules ()~%")
    (format port "      ((_ ret name args)~%")
    (format port "       (define name (c-function/weak ret name args)))))~%")
    (output-api-const port api-const)
    (output-api-entry port api-entry)
    (format port ") ;[end]~%")))
