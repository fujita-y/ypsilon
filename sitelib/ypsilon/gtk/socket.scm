#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk socket)

  (export gtk_socket_add_id
          gtk_socket_get_id
          gtk_socket_get_plug_window
          gtk_socket_get_type
          gtk_socket_new)

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

  ;; void gtk_socket_add_id (GtkSocket* socket_, GdkNativeWindow window_id)
  (define-function void gtk_socket_add_id (void* uint32_t))

  ;; GdkNativeWindow gtk_socket_get_id (GtkSocket* socket_)
  (define-function uint32_t gtk_socket_get_id (void*))

  ;; GdkWindow* gtk_socket_get_plug_window (GtkSocket* socket_)
  (define-function void* gtk_socket_get_plug_window (void*))

  ;; GType gtk_socket_get_type (void)
  (define-function unsigned-long gtk_socket_get_type ())

  ;; GtkWidget* gtk_socket_new (void)
  (define-function void* gtk_socket_new ())

  ) ;[end]
