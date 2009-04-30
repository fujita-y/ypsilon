#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk arrow)

  (export gtk_arrow_get_type
          gtk_arrow_new
          gtk_arrow_placement_get_type
          gtk_arrow_set
          gtk_arrow_type_get_type)

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

  ;; GType gtk_arrow_get_type (void)
  (define-function unsigned-long gtk_arrow_get_type ())

  ;; GtkWidget* gtk_arrow_new (GtkArrowType arrow_type, GtkShadowType shadow_type)
  (define-function void* gtk_arrow_new (int int))

  ;; GType gtk_arrow_placement_get_type (void)
  (define-function unsigned-long gtk_arrow_placement_get_type ())

  ;; void gtk_arrow_set (GtkArrow* arrow, GtkArrowType arrow_type, GtkShadowType shadow_type)
  (define-function void gtk_arrow_set (void* int int))

  ;; GType gtk_arrow_type_get_type (void)
  (define-function unsigned-long gtk_arrow_type_get_type ())

  ) ;[end]
