#!/usr/bin/env ypsilon
#!r6rs

;; cairo-demo.scm:
;;   Cairo/PostScript hello world
;;
;; Requirements:
;;   Darwin:  Cairo.framework
;;   Linux:   libcairo.so.2
;;   FreeBSD: libcairo.so
;;   OpenBSD: libcairo.so
;;   Windows: libcairo-2.dll

(import (rnrs) (ypsilon cairo) (ypsilon c-types) (srfi :48))

(define with-cairo-surface
  (lambda (surf proc)
    (proc surf)
    (cairo_surface_destroy surf)))

(define with-cairo-context
  (lambda (ctx proc)
    (proc ctx)
    (cairo_destroy ctx)))

(define print
  (lambda (ref data length)
    (format #t "~a" (utf8->string (make-bytevector-mapping data length)))))

(define-c-struct-methods cairo_text_extents_t)

(let ((text "Hello World"))
  (with-cairo-surface (cairo_ps_surface_create_for_stream print 0 320.0 160.0)
    (lambda (surf)
      (with-cairo-context (cairo_create surf)
        (lambda (cr)
          (cairo_select_font_face cr "Sans" CAIRO_FONT_SLANT_NORMAL CAIRO_FONT_WEIGHT_NORMAL)
          (cairo_set_font_size cr 32.0)
          (let ((extents (make-cairo_text_extents_t)))
            (cairo_text_extents cr text extents)
            (let ((x (- 160.0
                        (/ (cairo_text_extents_t-width extents) 2.0)
                        (cairo_text_extents_t-x_bearing extents)))
                  (y (- 80.0
                        (/ (cairo_text_extents_t-height extents) 2.0)
                        (cairo_text_extents_t-y_bearing extents))))
              (cairo_move_to cr x y)
              (cairo_set_source_rgba cr 1.0 0.7 0.5 1.0)
              (cairo_show_text cr text))))))))
