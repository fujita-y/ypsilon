#!/usr/bin/env ypsilon
#!r6rs

;; ps3-cell-demo.scm
;; tested on PS3 Linux

(import (rnrs)
        (only (core) system format iota)
        (ypsilon ffi)
        (ypsilon concurrent)
        (ypsilon cell libspe2))

(define spe-code
  "
  #include <stdio.h>
  int main(unsigned long long spe_id, unsigned long long pdata) {
    printf(\"SPE:0x%llx sleep for 2 second\\n\", spe_id);
    sleep(2);
    printf(\"SPE:0x%llx exit\\n\", spe_id);
    return 0;
  }\n")

(define NULL 0)
(define source-file "spe_example.c")
(define object-file "spe_example")

(define make-spe-program
  (lambda ()
    (when (file-exists? source-file) (delete-file source-file))
    (call-with-port
      (open-output-file source-file)
      (lambda (port)
        (put-string port spe-code)))
    (unless (= (system (format "spu-gcc ~a -o ~a" source-file object-file)) 0)
      (error 'make-spe-program "unexpected error"))))

(make-spe-program)
(newline)

;; sequential

(let ((image (spe_image_open object-file)))
  (when (= image NULL) (error 'spe_image_open "unexpected error"))
  (for-each (lambda (n)
              (let ((context (spe_context_create (+ SPE_EVENTS_ENABLE SPE_MAP_PS) NULL)))
                (when (= context NULL) (error 'spe_context_create "unexpected error"))
                (spe_program_load context image)
                (let ((entry (make-c-int SPE_DEFAULT_ENTRY)))
                  (format #t "-- kick sequential run ~a/6\n" n)
                  (when (< (spe_context_run context entry 0 0 NULL NULL) 0)
                    (error 'spe_context_run "unexpected error"))
                  (spe_context_destroy context))))
            (iota 6))
  (spe_image_close image))

(newline)
(newline)

;; parallel

(let ((image (spe_image_open object-file)))
  (when (= image NULL) (error 'spe_image_open "unexpected error"))
  (let ((threads
         (map (lambda (n)
                (format #t "-- kick parallel run ~a/6\n" n)
                (future
                 (let ((context (spe_context_create (+ SPE_EVENTS_ENABLE SPE_MAP_PS) NULL)))
                   (when (= context NULL) (error 'spe_context_create "unexpected error"))
                   (spe_program_load context image)
                   (let ((entry (make-c-int SPE_DEFAULT_ENTRY)))
                     (when (< (spe_context_run context entry 0 0 NULL NULL) 0)
                       (error 'spe_context_run "unexpected error"))
                     (spe_context_destroy context)))))
              (iota 6))))
    (format #t "== wait for all SPE exit\n")
    (for-each (lambda (x) (x)) threads))
  (spe_image_close image))
