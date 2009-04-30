#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk aspect)

  (export gtk_aspect_frame_get_type
          gtk_aspect_frame_new
          gtk_aspect_frame_set)

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

  ;; GType gtk_aspect_frame_get_type (void)
  (define-function unsigned-long gtk_aspect_frame_get_type ())

  ;; GtkWidget* gtk_aspect_frame_new (const gchar* label, gfloat xalign, gfloat yalign, gfloat ratio, gboolean obey_child)
  (define-function void* gtk_aspect_frame_new (char* float float float int))

  ;; void gtk_aspect_frame_set (GtkAspectFrame* aspect_frame, gfloat xalign, gfloat yalign, gfloat ratio, gboolean obey_child)
  (define-function void gtk_aspect_frame_set (void* float float float int))

  ) ;[end]
