#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk curve)

  (export gtk_curve_get_type
          gtk_curve_get_vector
          gtk_curve_new
          gtk_curve_reset
          gtk_curve_set_curve_type
          gtk_curve_set_gamma
          gtk_curve_set_range
          gtk_curve_set_vector
          gtk_curve_type_get_type)

  (import (rnrs) (ypsilon ffi))

  (define lib-name
    (cond (on-darwin  "Gtk.framework/Gtk")
          (on-linux   "libgtk-x11-2.0.so.0")
          (on-freebsd "libgtk-x11-2.0.so.0")
          (on-openbsd "libgtk-x11-2.0.so.0")
          (on-windows "libgtk-win32-2.0-0.dll")
          (else
           (assertion-violation #f "can not locate GTK library, unknown operating system"))))

  (define lib (load-shared-object lib-name))

  (define-syntax define-function
    (syntax-rules ()
      ((_ ret name args)
       (define name (c-function lib lib-name ret name args)))))

  (define-syntax define-variadic-function
    (syntax-rules ()
      ((_ ret name args)
      (define name (lambda x (assertion-violation 'name "variadic function not supported"))))))

  ;; GType gtk_curve_get_type (void)
  (define-function unsigned-long gtk_curve_get_type ())

  ;; void gtk_curve_get_vector (GtkCurve* curve, int veclen, gfloat vector[])
  (define-function void gtk_curve_get_vector (void* int void*))

  ;; GtkWidget* gtk_curve_new (void)
  (define-function void* gtk_curve_new ())

  ;; void gtk_curve_reset (GtkCurve* curve)
  (define-function void gtk_curve_reset (void*))

  ;; void gtk_curve_set_curve_type (GtkCurve* curve, GtkCurveType type)
  (define-function void gtk_curve_set_curve_type (void* int))

  ;; void gtk_curve_set_gamma (GtkCurve* curve, gfloat gamma_)
  (define-function void gtk_curve_set_gamma (void* float))

  ;; void gtk_curve_set_range (GtkCurve* curve, gfloat min_x, gfloat max_x, gfloat min_y, gfloat max_y)
  (define-function void gtk_curve_set_range (void* float float float float))

  ;; void gtk_curve_set_vector (GtkCurve* curve, int veclen, gfloat vector[])
  (define-function void gtk_curve_set_vector (void* int void*))

  ;; GType gtk_curve_type_get_type (void)
  (define-function unsigned-long gtk_curve_type_get_type ())

  ) ;[end]
