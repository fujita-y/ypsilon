#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon pango matrix)

  (export pango_matrix_concat
          pango_matrix_copy
          pango_matrix_free
          pango_matrix_get_font_scale_factor
          pango_matrix_get_type
          pango_matrix_rotate
          pango_matrix_scale
          pango_matrix_transform_distance
          pango_matrix_transform_pixel_rectangle
          pango_matrix_transform_point
          pango_matrix_transform_rectangle
          pango_matrix_translate)

  (import (rnrs) (ypsilon ffi))

  (define lib-name
    (cond (on-darwin  "Gtk.framework/Gtk")
          (on-linux   "libpango-1.0.so.0")
          (on-freebsd "libpango-1.0.so.0")
          (on-openbsd "libpango-1.0.so.0")
          (on-windows "libpango-1.0-0.dll")
          (else
           (assertion-violation #f "can not locate GDK library, unknown operating system"))))

  (define lib (load-shared-object lib-name))

  (define-syntax define-function
    (syntax-rules ()
      ((_ ret name args)
       (define name (c-function lib lib-name ret name args)))))

  (define-syntax define-variadic-function
    (syntax-rules ()
      ((_ ret name args)
      (define name (lambda x (assertion-violation 'name "variadic function not supported"))))))

  ;; void pango_matrix_concat (PangoMatrix* matrix, const PangoMatrix* new_matrix)
  (define-function void pango_matrix_concat (void* void*))

  ;; PangoMatrix* pango_matrix_copy (const PangoMatrix* matrix)
  (define-function void* pango_matrix_copy (void*))

  ;; void pango_matrix_free (PangoMatrix* matrix)
  (define-function void pango_matrix_free (void*))

  ;; double pango_matrix_get_font_scale_factor (const PangoMatrix* matrix)
  (define-function double pango_matrix_get_font_scale_factor (void*))

  ;; GType pango_matrix_get_type (void)
  (define-function unsigned-long pango_matrix_get_type ())

  ;; void pango_matrix_rotate (PangoMatrix* matrix, double degrees)
  (define-function void pango_matrix_rotate (void* double))

  ;; void pango_matrix_scale (PangoMatrix* matrix, double scale_x, double scale_y)
  (define-function void pango_matrix_scale (void* double double))

  ;; void pango_matrix_transform_distance (const PangoMatrix* matrix, double* dx, double* dy)
  (define-function void pango_matrix_transform_distance (void* void* void*))

  ;; void pango_matrix_transform_pixel_rectangle (const PangoMatrix* matrix, PangoRectangle* rect)
  (define-function void pango_matrix_transform_pixel_rectangle (void* void*))

  ;; void pango_matrix_transform_point (const PangoMatrix* matrix, double* x, double* y)
  (define-function void pango_matrix_transform_point (void* void* void*))

  ;; void pango_matrix_transform_rectangle (const PangoMatrix* matrix, PangoRectangle* rect)
  (define-function void pango_matrix_transform_rectangle (void* void*))

  ;; void pango_matrix_translate (PangoMatrix* matrix, double tx, double ty)
  (define-function void pango_matrix_translate (void* double double))

  ) ;[end]
