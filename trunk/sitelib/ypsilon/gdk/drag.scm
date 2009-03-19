#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gdk drag)

  (export gdk_drag_abort
          gdk_drag_action_get_type
          gdk_drag_begin
          gdk_drag_context_get_type
          gdk_drag_context_new
          gdk_drag_drop
          gdk_drag_drop_succeeded
          gdk_drag_find_window
          gdk_drag_find_window_for_screen
          gdk_drag_get_protocol
          gdk_drag_get_protocol_for_display
          gdk_drag_get_selection
          gdk_drag_motion
          gdk_drag_protocol_get_type
          gdk_drag_status)

  (import (rnrs) (ypsilon ffi))

  (define lib-name
    (cond (on-darwin  "Gtk.framework/Gtk")
          (on-linux   "libgdk-x11-2.0.so.0")
          (on-freebsd "libgdk-x11-2.0.so.0")
          (on-openbsd "libgdk-x11-2.0.so.0")
          (on-windows "libgdk-win32-2.0-0.dll")
          (else
           (assertion-violation #f "can not locate GDK library, unknown operating system"))))

  (define lib (load-shared-object lib-name))

  (define-syntax define-function
    (syntax-rules ()
      ((_ ret name args)
       (define name (c-function lib lib-name ret name args)))))

  ;; void gdk_drag_abort (GdkDragContext* context, guint32 time_)
  (define-function void gdk_drag_abort (void* uint32_t))

  ;; GType gdk_drag_action_get_type (void)
  (define-function unsigned-long gdk_drag_action_get_type ())

  ;; GdkDragContext* gdk_drag_begin (GdkWindow* window, GList* targets)
  (define-function void* gdk_drag_begin (void* void*))

  ;; GType gdk_drag_context_get_type (void)
  (define-function unsigned-long gdk_drag_context_get_type ())

  ;; GdkDragContext* gdk_drag_context_new (void)
  (define-function void* gdk_drag_context_new ())

  ;; void gdk_drag_drop (GdkDragContext* context, guint32 time_)
  (define-function void gdk_drag_drop (void* uint32_t))

  ;; gboolean gdk_drag_drop_succeeded (GdkDragContext* context)
  (define-function int gdk_drag_drop_succeeded (void*))

  ;; void gdk_drag_find_window (GdkDragContext* context, GdkWindow* drag_window, gint x_root, gint y_root, GdkWindow** dest_window, GdkDragProtocol* protocol)
  (define-function void gdk_drag_find_window (void* void* int int void* void*))

  ;; void gdk_drag_find_window_for_screen (GdkDragContext* context, GdkWindow* drag_window, GdkScreen* screen, gint x_root, gint y_root, GdkWindow** dest_window, GdkDragProtocol* protocol)
  (define-function void gdk_drag_find_window_for_screen (void* void* void* int int void* void*))

  ;; GdkNativeWindow gdk_drag_get_protocol (GdkNativeWindow xid, GdkDragProtocol* protocol)
  (define-function uint32_t gdk_drag_get_protocol (uint32_t void*))

  ;; GdkNativeWindow gdk_drag_get_protocol_for_display (GdkDisplay* display, GdkNativeWindow xid, GdkDragProtocol* protocol)
  (define-function uint32_t gdk_drag_get_protocol_for_display (void* uint32_t void*))

  ;; GdkAtom gdk_drag_get_selection (GdkDragContext* context)
  (define-function void* gdk_drag_get_selection (void*))

  ;; gboolean gdk_drag_motion (GdkDragContext* context, GdkWindow* dest_window, GdkDragProtocol protocol, gint x_root, gint y_root, GdkDragAction suggested_action, GdkDragAction possible_actions, guint32 time_)
  (define-function int gdk_drag_motion (void* void* int int int int int uint32_t))

  ;; GType gdk_drag_protocol_get_type (void)
  (define-function unsigned-long gdk_drag_protocol_get_type ())

  ;; void gdk_drag_status (GdkDragContext* context, GdkDragAction action, guint32 time_)
  (define-function void gdk_drag_status (void* int uint32_t))

  ) ;[end]
