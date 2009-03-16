#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk size)

  (export gtk_size_group_add_widget
          gtk_size_group_get_ignore_hidden
          gtk_size_group_get_mode
          gtk_size_group_get_type
          gtk_size_group_get_widgets
          gtk_size_group_mode_get_type
          gtk_size_group_new
          gtk_size_group_remove_widget
          gtk_size_group_set_ignore_hidden
          gtk_size_group_set_mode)

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

  ;; void gtk_size_group_add_widget (GtkSizeGroup* size_group, GtkWidget* widget)
  (define-function void gtk_size_group_add_widget (void* void*))

  ;; gboolean gtk_size_group_get_ignore_hidden (GtkSizeGroup* size_group)
  (define-function int gtk_size_group_get_ignore_hidden (void*))

  ;; GtkSizeGroupMode gtk_size_group_get_mode (GtkSizeGroup* size_group)
  (define-function int gtk_size_group_get_mode (void*))

  ;; GType gtk_size_group_get_type (void)
  (define-function unsigned-long gtk_size_group_get_type ())

  ;; GSList* gtk_size_group_get_widgets (GtkSizeGroup* size_group)
  (define-function void* gtk_size_group_get_widgets (void*))

  ;; GType gtk_size_group_mode_get_type (void)
  (define-function unsigned-long gtk_size_group_mode_get_type ())

  ;; GtkSizeGroup* gtk_size_group_new (GtkSizeGroupMode mode)
  (define-function void* gtk_size_group_new (int))

  ;; void gtk_size_group_remove_widget (GtkSizeGroup* size_group, GtkWidget* widget)
  (define-function void gtk_size_group_remove_widget (void* void*))

  ;; void gtk_size_group_set_ignore_hidden (GtkSizeGroup* size_group, gboolean ignore_hidden)
  (define-function void gtk_size_group_set_ignore_hidden (void* int))

  ;; void gtk_size_group_set_mode (GtkSizeGroup* size_group, GtkSizeGroupMode mode)
  (define-function void gtk_size_group_set_mode (void* int))

  ) ;[end]
