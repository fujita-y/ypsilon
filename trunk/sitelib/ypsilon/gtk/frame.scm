#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk frame)

  (export gtk_frame_get_label
          gtk_frame_get_label_align
          gtk_frame_get_label_widget
          gtk_frame_get_shadow_type
          gtk_frame_get_type
          gtk_frame_new
          gtk_frame_set_label
          gtk_frame_set_label_align
          gtk_frame_set_label_widget
          gtk_frame_set_shadow_type)

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

  ;; const gchar* gtk_frame_get_label (GtkFrame* frame)
  (define-function char* gtk_frame_get_label (void*))

  ;; void gtk_frame_get_label_align (GtkFrame* frame, gfloat* xalign, gfloat* yalign)
  (define-function void gtk_frame_get_label_align (void* void* void*))

  ;; GtkWidget* gtk_frame_get_label_widget (GtkFrame* frame)
  (define-function void* gtk_frame_get_label_widget (void*))

  ;; GtkShadowType gtk_frame_get_shadow_type (GtkFrame* frame)
  (define-function int gtk_frame_get_shadow_type (void*))

  ;; GType gtk_frame_get_type (void)
  (define-function unsigned-long gtk_frame_get_type ())

  ;; GtkWidget* gtk_frame_new (const gchar* label)
  (define-function void* gtk_frame_new (char*))

  ;; void gtk_frame_set_label (GtkFrame* frame, const gchar* label)
  (define-function void gtk_frame_set_label (void* char*))

  ;; void gtk_frame_set_label_align (GtkFrame* frame, gfloat xalign, gfloat yalign)
  (define-function void gtk_frame_set_label_align (void* float float))

  ;; void gtk_frame_set_label_widget (GtkFrame* frame, GtkWidget* label_widget)
  (define-function void gtk_frame_set_label_widget (void* void*))

  ;; void gtk_frame_set_shadow_type (GtkFrame* frame, GtkShadowType type)
  (define-function void gtk_frame_set_shadow_type (void* int))

  ) ;[end]
