#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk alignment)

  (export gtk_alignment_get_padding
          gtk_alignment_get_type
          gtk_alignment_new
          gtk_alignment_set
          gtk_alignment_set_padding)

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

  ;; void gtk_alignment_get_padding (GtkAlignment* alignment, guint* padding_top, guint* padding_bottom, guint* padding_left, guint* padding_right)
  (define-function void gtk_alignment_get_padding (void* void* void* void* void*))

  ;; GType gtk_alignment_get_type (void)
  (define-function unsigned-long gtk_alignment_get_type ())

  ;; GtkWidget* gtk_alignment_new (gfloat xalign, gfloat yalign, gfloat xscale, gfloat yscale)
  (define-function void* gtk_alignment_new (float float float float))

  ;; void gtk_alignment_set (GtkAlignment* alignment, gfloat xalign, gfloat yalign, gfloat xscale, gfloat yscale)
  (define-function void gtk_alignment_set (void* float float float float))

  ;; void gtk_alignment_set_padding (GtkAlignment* alignment, guint padding_top, guint padding_bottom, guint padding_left, guint padding_right)
  (define-function void gtk_alignment_set_padding (void* unsigned-int unsigned-int unsigned-int unsigned-int))

  ) ;[end]
