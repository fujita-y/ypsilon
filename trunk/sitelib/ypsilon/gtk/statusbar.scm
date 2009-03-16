#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk statusbar)

  (export gtk_statusbar_get_context_id
          gtk_statusbar_get_has_resize_grip
          gtk_statusbar_get_type
          gtk_statusbar_new
          gtk_statusbar_pop
          gtk_statusbar_push
          gtk_statusbar_remove
          gtk_statusbar_set_has_resize_grip)

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

  ;; guint gtk_statusbar_get_context_id (GtkStatusbar* statusbar, const gchar* context_description)
  (define-function unsigned-int gtk_statusbar_get_context_id (void* char*))

  ;; gboolean gtk_statusbar_get_has_resize_grip (GtkStatusbar* statusbar)
  (define-function int gtk_statusbar_get_has_resize_grip (void*))

  ;; GType gtk_statusbar_get_type (void)
  (define-function unsigned-long gtk_statusbar_get_type ())

  ;; GtkWidget* gtk_statusbar_new (void)
  (define-function void* gtk_statusbar_new ())

  ;; void gtk_statusbar_pop (GtkStatusbar* statusbar, guint context_id)
  (define-function void gtk_statusbar_pop (void* unsigned-int))

  ;; guint gtk_statusbar_push (GtkStatusbar* statusbar, guint context_id, const gchar* text)
  (define-function unsigned-int gtk_statusbar_push (void* unsigned-int char*))

  ;; void gtk_statusbar_remove (GtkStatusbar* statusbar, guint context_id, guint message_id)
  (define-function void gtk_statusbar_remove (void* unsigned-int unsigned-int))

  ;; void gtk_statusbar_set_has_resize_grip (GtkStatusbar* statusbar, gboolean setting)
  (define-function void gtk_statusbar_set_has_resize_grip (void* int))

  ) ;[end]
