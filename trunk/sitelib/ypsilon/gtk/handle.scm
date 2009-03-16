#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk handle)

  (export gtk_handle_box_get_child_detached
          gtk_handle_box_get_handle_position
          gtk_handle_box_get_shadow_type
          gtk_handle_box_get_snap_edge
          gtk_handle_box_get_type
          gtk_handle_box_new
          gtk_handle_box_set_handle_position
          gtk_handle_box_set_shadow_type
          gtk_handle_box_set_snap_edge)

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

  ;; gboolean gtk_handle_box_get_child_detached (GtkHandleBox* handle_box)
  (define-function int gtk_handle_box_get_child_detached (void*))

  ;; GtkPositionType gtk_handle_box_get_handle_position(GtkHandleBox* handle_box)
  (define-function int gtk_handle_box_get_handle_position (void*))

  ;; GtkShadowType gtk_handle_box_get_shadow_type (GtkHandleBox* handle_box)
  (define-function int gtk_handle_box_get_shadow_type (void*))

  ;; GtkPositionType gtk_handle_box_get_snap_edge (GtkHandleBox* handle_box)
  (define-function int gtk_handle_box_get_snap_edge (void*))

  ;; GType gtk_handle_box_get_type (void)
  (define-function unsigned-long gtk_handle_box_get_type ())

  ;; GtkWidget* gtk_handle_box_new (void)
  (define-function void* gtk_handle_box_new ())

  ;; void gtk_handle_box_set_handle_position (GtkHandleBox* handle_box, GtkPositionType position)
  (define-function void gtk_handle_box_set_handle_position (void* int))

  ;; void gtk_handle_box_set_shadow_type (GtkHandleBox* handle_box, GtkShadowType type)
  (define-function void gtk_handle_box_set_shadow_type (void* int))

  ;; void gtk_handle_box_set_snap_edge (GtkHandleBox* handle_box, GtkPositionType edge)
  (define-function void gtk_handle_box_set_snap_edge (void* int))

  ) ;[end]
