#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk hsv)

  (export gtk_hsv_get_color
          gtk_hsv_get_metrics
          gtk_hsv_get_type
          gtk_hsv_is_adjusting
          gtk_hsv_new
          gtk_hsv_set_color
          gtk_hsv_set_metrics
          gtk_hsv_to_rgb)

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

  ;; void gtk_hsv_get_color (GtkHSV* hsv, gdouble* h, gdouble* s, gdouble* v)
  (define-function void gtk_hsv_get_color (void* void* void* void*))

  ;; void gtk_hsv_get_metrics (GtkHSV* hsv, gint* size, gint* ring_width)
  (define-function void gtk_hsv_get_metrics (void* void* void*))

  ;; GType gtk_hsv_get_type (void)
  (define-function unsigned-long gtk_hsv_get_type ())

  ;; gboolean gtk_hsv_is_adjusting (GtkHSV* hsv)
  (define-function int gtk_hsv_is_adjusting (void*))

  ;; GtkWidget* gtk_hsv_new (void)
  (define-function void* gtk_hsv_new ())

  ;; void gtk_hsv_set_color (GtkHSV* hsv, double h, double s, double v)
  (define-function void gtk_hsv_set_color (void* double double double))

  ;; void gtk_hsv_set_metrics (GtkHSV* hsv, gint size, gint ring_width)
  (define-function void gtk_hsv_set_metrics (void* int int))

  ;; void gtk_hsv_to_rgb (gdouble h, gdouble s, gdouble v, gdouble* r, gdouble* g, gdouble* b)
  (define-function void gtk_hsv_to_rgb (double double double void* void* void*))

  ) ;[end]
