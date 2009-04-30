#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk quit)

  (export gtk_quit_add
          gtk_quit_add_destroy
          gtk_quit_add_full
          gtk_quit_remove
          gtk_quit_remove_by_data)

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

  ;; guint gtk_quit_add (guint main_level, GtkFunction function, gpointer data)
  (define-function unsigned-int gtk_quit_add (unsigned-int (c-callback int (void*)) void*))

  ;; void gtk_quit_add_destroy (guint main_level, GtkObject* object)
  (define-function void gtk_quit_add_destroy (unsigned-int void*))

  ;; guint gtk_quit_add_full (guint main_level, GtkFunction function, GtkCallbackMarshal marshal, gpointer data, GDestroyNotify destroy)
  (define-function unsigned-int gtk_quit_add_full (unsigned-int (c-callback int (void*)) (c-callback void (void* void* unsigned-int void*)) void* (c-callback void (void*))))

  ;; void gtk_quit_remove (guint quit_handler_id)
  (define-function void gtk_quit_remove (unsigned-int))

  ;; void gtk_quit_remove_by_data (gpointer data)
  (define-function void gtk_quit_remove_by_data (void*))

  ) ;[end]
