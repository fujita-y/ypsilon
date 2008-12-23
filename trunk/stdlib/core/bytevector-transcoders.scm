#!core
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2008 Y.FUJITA, LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (core bytevector-transcoders)

  (export string->utf8 utf8->string
          string->utf16 utf16->string
          string->utf32 utf32->string)

  (import (core intrinsics)
          (core optargs)
          (core io)
          (core primitives)
          (core bytevectors))

  (define string->utf32
    (lambda (str . opt)
      (let-optionals opt ((endian (endianness big)))
        (if (= (string-length str) 0)
            #vu8()
            (let ((input (make-string-input-port str))
                  (buf (make-bytevector 4)))
              (let-values (((output extract) (open-bytevector-output-port)))
                (let loop ((ch (get-char input)))
                  (cond ((eof-object? ch) (extract))
                        (else
                         (bytevector-u32-set! buf 0 (char->integer ch) endian)
                         (put-bytevector output buf)
                         (loop (get-char input)))))))))))

  (define test-utf32-bom
    (lambda (bvect)
      (and (>= (bytevector-length bvect) 4)
           (let ((bom (make-bytevector 4)))
             (bytevector-copy! bvect 0 bom 0 4)
             (cond ((equal? bom #vu8(#x00 #x00 #xFE #xFF)) (endianness big))
                   ((equal? bom #vu8(#xFF #xFE #x00 #x00)) (endianness little))
                   (else #f))))))

  (define transcode-utf32->string
    (lambda (i rest bvect endian)
      (let ((output (make-string-output-port)))
        (let loop ((i i) (rest rest))
          (cond ((= rest 0) (extract-accumulated-string output))
                ((>= rest 4)
                 (let ((sv (bytevector-u32-ref bvect i endian)))
                   (cond ((> sv #x10FFFF) (put-char output #\xFFFD))
                         ((<= #xD800 sv #xDFFF) (put-char output #\xFFFD))
                         (else (put-char output (integer->char sv)))))
                 (loop (+ i 4) (- rest 4)))
                (else
                 (put-char output #\xFFFD)
                 (extract-accumulated-string output)))))))

  (define utf32->string
    (lambda (bvect endian . opt)
      (let-optionals opt ((endianness-mandatory #f))
        (cond (endianness-mandatory
               (transcode-utf32->string 0 (bytevector-length bvect) bvect endian))
              ((test-utf32-bom bvect)
               => (lambda (bom)
                    (transcode-utf32->string 4 (- (bytevector-length bvect) 4) bvect bom)))
              (else
               (transcode-utf32->string 0 (bytevector-length bvect) bvect endian))))))

  (define encode-surrogates
    (lambda (ucs4)
      (let ((t (- ucs4 #x010000)))
        (let ((x (+ (div t 1024) #xD800))
              (y (+ (mod t 1024) #xDC00)))
          (values x y)))))

  (define decode-surrogates
    (lambda (left right)
      (let ((sv (+ (* (- left #xD800) 1024) (- right #xDC00) #x010000)))
        (cond ((> sv #x10FFFF) #\xFFFD)
              ((<= #xD800 sv #xDFFF) #\xFFFD)
              (else (integer->char sv))))))

  (define test-utf16-bom
    (lambda (bvect)
      (and (>= (bytevector-length bvect) 2)
           (let ((bom (make-bytevector 2)))
             (bytevector-copy! bvect 0 bom 0 2)
             (cond ((equal? bom #vu8(#xFE #xFF)) (endianness big))
                   ((equal? bom #vu8(#xFF #xFE)) (endianness little))
                   (else #f))))))

  (define string->utf16
    (lambda (str . opt)
      (let-optionals opt ((endian (endianness big)))
        (if (= (string-length str) 0)
            #vu8()
            (let ((input (make-string-input-port str))
                  (buf (make-bytevector 2)))
              (let-values (((output extract) (open-bytevector-output-port)))
                (let loop ((ch (get-char input)))
                  (cond ((eof-object? ch) (extract))
                        (else
                         (let ((sv (char->integer ch)))
                           (cond ((>= sv #x10000)
                                  (let-values (((left right) (encode-surrogates sv)))
                                    (bytevector-u16-set! buf 0 left endian)
                                    (put-bytevector output buf)
                                    (bytevector-u16-set! buf 0 right endian)
                                    (put-bytevector output buf))
                                  (loop (get-char input)))
                                 (else
                                  (bytevector-u16-set! buf 0 sv endian)
                                  (put-bytevector output buf)
                                  (loop (get-char input))))))))))))))

  (define transcode-utf16->string
    (lambda (i rest bvect endian)
      (let ((output (make-string-output-port)))
        (let loop ((i i) (rest rest))
          (cond ((= rest 0) (extract-accumulated-string output))
                ((>= rest 2)
                 (let ((sv (bytevector-u16-ref bvect i endian)))
                   (cond ((<= #xD800 sv #xDBFF)
                          (cond ((>= rest 4)
                                 (let ((right (bytevector-u16-ref bvect (+ i 2) endian)))
                                   (cond ((<= #xDC00 right #xDFFF)
                                          (put-char output (decode-surrogates sv right))
                                          (loop (+ i 4) (- rest 4)))
                                         (else
                                          (put-char output #\xFFFD)
                                          (loop (+ i 2) (- rest 2))))))
                                (else
                                 (put-char output #\xFFFD)
                                 (extract-accumulated-string output))))
                         ((<= #xDC00 sv #xDFFF)
                          (put-char output #\xFFFD)
                          (loop (+ i 2) (- rest 2)))
                         (else
                          (put-char output (integer->char sv))
                          (loop (+ i 2) (- rest 2))))))
                (else
                 (put-char output #\xFFFD)
                 (extract-accumulated-string output)))))))

  (define utf16->string
    (lambda (bvect endian . opt)
      (let-optionals opt ((endianness-mandatory #f))
        (cond (endianness-mandatory
               (transcode-utf16->string 0 (bytevector-length bvect) bvect endian))
              ((test-utf16-bom bvect)
               => (lambda (bom)
                    (transcode-utf16->string 2 (- (bytevector-length bvect) 2) bvect bom)))
              (else
               (transcode-utf16->string 0 (bytevector-length bvect) bvect endian))))))

  ) ;[end]
