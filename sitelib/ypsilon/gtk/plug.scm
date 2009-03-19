#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk plug)

  (export gtk_plug_construct-
          gtk_plug_construct_for_display
          gtk_plug_get_embedded
          gtk_plug_get_id
          gtk_plug_get_socket_window
          gtk_plug_get_type
          gtk_plug_new
          gtk_plug_new_for_display)

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

  ;; void gtk_plug_construct (GtkPlug* plug, GdkNativeWindow socket_id)
  (define-function void gtk_plug_construct- (void* uint32_t))

  ;; void gtk_plug_construct_for_display (GtkPlug* plug, GdkDisplay* display, GdkNativeWindow socket_id)
  (define-function void gtk_plug_construct_for_display (void* void* uint32_t))

  ;; gboolean gtk_plug_get_embedded (GtkPlug* plug)
  (define-function int gtk_plug_get_embedded (void*))

  ;; GdkNativeWindow gtk_plug_get_id (GtkPlug* plug)
  (define-function uint32_t gtk_plug_get_id (void*))

  ;; GdkWindow* gtk_plug_get_socket_window (GtkPlug* plug)
  (define-function void* gtk_plug_get_socket_window (void*))

  ;; GType gtk_plug_get_type (void)
  (define-function unsigned-long gtk_plug_get_type ())

  ;; GtkWidget* gtk_plug_new (GdkNativeWindow socket_id)
  (define-function void* gtk_plug_new (uint32_t))

  ;; GtkWidget* gtk_plug_new_for_display (GdkDisplay* display, GdkNativeWindow socket_id)
  (define-function void* gtk_plug_new_for_display (void* uint32_t))

  ) ;[end]
