#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk event)

  (export gtk_event_box_get_above_child
          gtk_event_box_get_type
          gtk_event_box_get_visible_window
          gtk_event_box_new
          gtk_event_box_set_above_child
          gtk_event_box_set_visible_window)

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

  ;; gboolean gtk_event_box_get_above_child (GtkEventBox* event_box)
  (define-function int gtk_event_box_get_above_child (void*))

  ;; GType gtk_event_box_get_type (void)
  (define-function unsigned-long gtk_event_box_get_type ())

  ;; gboolean gtk_event_box_get_visible_window (GtkEventBox* event_box)
  (define-function int gtk_event_box_get_visible_window (void*))

  ;; GtkWidget* gtk_event_box_new (void)
  (define-function void* gtk_event_box_new ())

  ;; void gtk_event_box_set_above_child (GtkEventBox* event_box, gboolean above_child)
  (define-function void gtk_event_box_set_above_child (void* int))

  ;; void gtk_event_box_set_visible_window (GtkEventBox* event_box, gboolean visible_window)
  (define-function void gtk_event_box_set_visible_window (void* int))

  ) ;[end]
