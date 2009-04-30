#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk hscale)

  (export gtk_hscale_get_type
          gtk_hscale_new
          gtk_hscale_new_with_range)

  (import (rnrs) (ypsilon ffi))

  (define lib-name
    (cond (on-linux   "libgtk-x11-2.0.so.0")
          (on-sunos   "libgtk-x11-2.0.so.0")
          (on-freebsd "libgtk-x11-2.0.so.0")
          (on-openbsd "libgtk-x11-2.0.so.0")
          (on-darwin  "Gtk.framework/Gtk")
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

  ;; GType gtk_hscale_get_type (void)
  (define-function unsigned-long gtk_hscale_get_type ())

  ;; GtkWidget* gtk_hscale_new (GtkAdjustment* adjustment)
  (define-function void* gtk_hscale_new (void*))

  ;; GtkWidget* gtk_hscale_new_with_range (gdouble min, gdouble max, gdouble step)
  (define-function void* gtk_hscale_new_with_range (double double double))

  ) ;[end]
