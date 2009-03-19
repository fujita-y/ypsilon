#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk fixed)

  (export gtk_fixed_get_has_window
          gtk_fixed_get_type
          gtk_fixed_move
          gtk_fixed_new
          gtk_fixed_put
          gtk_fixed_set_has_window)

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

  ;; gboolean gtk_fixed_get_has_window (GtkFixed* fixed)
  (define-function int gtk_fixed_get_has_window (void*))

  ;; GType gtk_fixed_get_type (void)
  (define-function unsigned-long gtk_fixed_get_type ())

  ;; void gtk_fixed_move (GtkFixed* fixed, GtkWidget* widget, gint x, gint y)
  (define-function void gtk_fixed_move (void* void* int int))

  ;; GtkWidget* gtk_fixed_new (void)
  (define-function void* gtk_fixed_new ())

  ;; void gtk_fixed_put (GtkFixed* fixed, GtkWidget* widget, gint x, gint y)
  (define-function void gtk_fixed_put (void* void* int int))

  ;; void gtk_fixed_set_has_window (GtkFixed* fixed, gboolean has_window)
  (define-function void gtk_fixed_set_has_window (void* int))

  ) ;[end]
