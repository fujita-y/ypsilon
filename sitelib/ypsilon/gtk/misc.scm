#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk misc)

  (export gtk_misc_get_alignment
          gtk_misc_get_padding
          gtk_misc_get_type
          gtk_misc_set_alignment
          gtk_misc_set_padding)

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

  ;; void gtk_misc_get_alignment (GtkMisc* misc, gfloat* xalign, gfloat* yalign)
  (define-function void gtk_misc_get_alignment (void* void* void*))

  ;; void gtk_misc_get_padding (GtkMisc* misc, gint* xpad, gint* ypad)
  (define-function void gtk_misc_get_padding (void* void* void*))

  ;; GType gtk_misc_get_type (void)
  (define-function unsigned-long gtk_misc_get_type ())

  ;; void gtk_misc_set_alignment (GtkMisc* misc, gfloat xalign, gfloat yalign)
  (define-function void gtk_misc_set_alignment (void* float float))

  ;; void gtk_misc_set_padding (GtkMisc* misc, gint xpad, gint ypad)
  (define-function void gtk_misc_set_padding (void* int int))

  ) ;[end]
