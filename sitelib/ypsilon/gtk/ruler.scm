#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk ruler)

  (export gtk_ruler_draw_pos
          gtk_ruler_draw_ticks
          gtk_ruler_get_metric
          gtk_ruler_get_range
          gtk_ruler_get_type
          gtk_ruler_set_metric
          gtk_ruler_set_range)

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

  (define-syntax define-function/va_list
    (syntax-rules ()
      ((_ ret name args)
      (define name (lambda x (assertion-violation 'name "va_list argument not supported"))))))

  ;; void gtk_ruler_draw_pos (GtkRuler* ruler)
  (define-function void gtk_ruler_draw_pos (void*))

  ;; void gtk_ruler_draw_ticks (GtkRuler* ruler)
  (define-function void gtk_ruler_draw_ticks (void*))

  ;; GtkMetricType gtk_ruler_get_metric (GtkRuler* ruler)
  (define-function int gtk_ruler_get_metric (void*))

  ;; void gtk_ruler_get_range (GtkRuler* ruler, gdouble* lower, gdouble* upper, gdouble* position, gdouble* max_size)
  (define-function void gtk_ruler_get_range (void* void* void* void* void*))

  ;; GType gtk_ruler_get_type (void)
  (define-function unsigned-long gtk_ruler_get_type ())

  ;; void gtk_ruler_set_metric (GtkRuler* ruler, GtkMetricType metric)
  (define-function void gtk_ruler_set_metric (void* int))

  ;; void gtk_ruler_set_range (GtkRuler* ruler, gdouble lower, gdouble upper, gdouble position, gdouble max_size)
  (define-function void gtk_ruler_set_range (void* double double double double))

  ) ;[end]
