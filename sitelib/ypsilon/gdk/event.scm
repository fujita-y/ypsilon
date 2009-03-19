#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gdk event)

  (export gdk_event_copy
          gdk_event_free
          gdk_event_get
          gdk_event_get_axis
          gdk_event_get_coords
          gdk_event_get_graphics_expose
          gdk_event_get_root_coords
          gdk_event_get_screen
          gdk_event_get_state
          gdk_event_get_time
          gdk_event_get_type
          gdk_event_handler_set
          gdk_event_mask_get_type
          gdk_event_new
          gdk_event_peek
          gdk_event_put
          gdk_event_request_motions
          gdk_event_send_client_message
          gdk_event_send_client_message_for_display
          gdk_event_send_clientmessage_toall
          gdk_event_set_screen
          gdk_event_type_get_type)

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

  ;; GdkEvent* gdk_event_copy (const GdkEvent* event)
  (define-function void* gdk_event_copy (void*))

  ;; void gdk_event_free (GdkEvent* event)
  (define-function void gdk_event_free (void*))

  ;; GdkEvent* gdk_event_get (void)
  (define-function void* gdk_event_get ())

  ;; gboolean gdk_event_get_axis (const GdkEvent* event, GdkAxisUse axis_use, gdouble* value)
  (define-function int gdk_event_get_axis (void* int void*))

  ;; gboolean gdk_event_get_coords (const GdkEvent* event, gdouble* x_win, gdouble* y_win)
  (define-function int gdk_event_get_coords (void* void* void*))

  ;; GdkEvent* gdk_event_get_graphics_expose (GdkWindow* window)
  (define-function void* gdk_event_get_graphics_expose (void*))

  ;; gboolean gdk_event_get_root_coords (const GdkEvent* event, gdouble* x_root, gdouble* y_root)
  (define-function int gdk_event_get_root_coords (void* void* void*))

  ;; GdkScreen* gdk_event_get_screen (const GdkEvent* event)
  (define-function void* gdk_event_get_screen (void*))

  ;; gboolean gdk_event_get_state (const GdkEvent* event, GdkModifierType* state)
  (define-function int gdk_event_get_state (void* void*))

  ;; guint32 gdk_event_get_time (const GdkEvent* event)
  (define-function uint32_t gdk_event_get_time (void*))

  ;; GType gdk_event_get_type (void)
  (define-function unsigned-long gdk_event_get_type ())

  ;; void gdk_event_handler_set (GdkEventFunc func, gpointer data, GDestroyNotify notify)
  (define-function void gdk_event_handler_set ((c-callback void (void* void*)) void* (c-callback void (void*))))

  ;; GType gdk_event_mask_get_type (void)
  (define-function unsigned-long gdk_event_mask_get_type ())

  ;; GdkEvent* gdk_event_new (GdkEventType type)
  (define-function void* gdk_event_new (int))

  ;; GdkEvent* gdk_event_peek (void)
  (define-function void* gdk_event_peek ())

  ;; void gdk_event_put (const GdkEvent* event)
  (define-function void gdk_event_put (void*))

  ;; void gdk_event_request_motions (const GdkEventMotion* event)
  (define-function void gdk_event_request_motions (void*))

  ;; gboolean gdk_event_send_client_message (GdkEvent* event, GdkNativeWindow winid)
  (define-function int gdk_event_send_client_message (void* uint32_t))

  ;; gboolean gdk_event_send_client_message_for_display (GdkDisplay* display, GdkEvent* event, GdkNativeWindow winid)
  (define-function int gdk_event_send_client_message_for_display (void* void* uint32_t))

  ;; void gdk_event_send_clientmessage_toall (GdkEvent* event)
  (define-function void gdk_event_send_clientmessage_toall (void*))

  ;; void gdk_event_set_screen (GdkEvent* event, GdkScreen* screen)
  (define-function void gdk_event_set_screen (void* void*))

  ;; GType gdk_event_type_get_type (void)
  (define-function unsigned-long gdk_event_type_get_type ())

  ) ;[end]
