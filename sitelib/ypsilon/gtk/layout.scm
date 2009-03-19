#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk layout)

  (export gtk_layout_get_bin_window
          gtk_layout_get_hadjustment
          gtk_layout_get_size
          gtk_layout_get_type
          gtk_layout_get_vadjustment
          gtk_layout_move
          gtk_layout_new
          gtk_layout_put
          gtk_layout_set_hadjustment
          gtk_layout_set_size
          gtk_layout_set_vadjustment)

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

  ;; GdkWindow* gtk_layout_get_bin_window (GtkLayout* layout)
  (define-function void* gtk_layout_get_bin_window (void*))

  ;; GtkAdjustment* gtk_layout_get_hadjustment (GtkLayout* layout)
  (define-function void* gtk_layout_get_hadjustment (void*))

  ;; void gtk_layout_get_size (GtkLayout* layout, guint* width, guint* height)
  (define-function void gtk_layout_get_size (void* void* void*))

  ;; GType gtk_layout_get_type (void)
  (define-function unsigned-long gtk_layout_get_type ())

  ;; GtkAdjustment* gtk_layout_get_vadjustment (GtkLayout* layout)
  (define-function void* gtk_layout_get_vadjustment (void*))

  ;; void gtk_layout_move (GtkLayout* layout, GtkWidget* child_widget, gint x, gint y)
  (define-function void gtk_layout_move (void* void* int int))

  ;; GtkWidget* gtk_layout_new (GtkAdjustment* hadjustment, GtkAdjustment* vadjustment)
  (define-function void* gtk_layout_new (void* void*))

  ;; void gtk_layout_put (GtkLayout* layout, GtkWidget* child_widget, gint x, gint y)
  (define-function void gtk_layout_put (void* void* int int))

  ;; void gtk_layout_set_hadjustment (GtkLayout* layout, GtkAdjustment* adjustment)
  (define-function void gtk_layout_set_hadjustment (void* void*))

  ;; void gtk_layout_set_size (GtkLayout* layout, guint width, guint height)
  (define-function void gtk_layout_set_size (void* unsigned-int unsigned-int))

  ;; void gtk_layout_set_vadjustment (GtkLayout* layout, GtkAdjustment* adjustment)
  (define-function void gtk_layout_set_vadjustment (void* void*))

  ) ;[end]
