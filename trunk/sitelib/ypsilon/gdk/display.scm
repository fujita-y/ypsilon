#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gdk display)

  (export gdk_display_add_client_message_filter
          gdk_display_beep
          gdk_display_close
          gdk_display_flush
          gdk_display_get_core_pointer
          gdk_display_get_default
          gdk_display_get_default_cursor_size
          gdk_display_get_default_group
          gdk_display_get_default_screen
          gdk_display_get_event
          gdk_display_get_maximal_cursor_size
          gdk_display_get_n_screens
          gdk_display_get_name
          gdk_display_get_pointer
          gdk_display_get_screen
          gdk_display_get_type
          gdk_display_get_window_at_pointer
          gdk_display_keyboard_ungrab
          gdk_display_list_devices
          gdk_display_manager_get
          gdk_display_manager_get_default_display
          gdk_display_manager_get_type
          gdk_display_manager_list_displays
          gdk_display_manager_set_default_display
          gdk_display_open
          gdk_display_open_default_libgtk_only
          gdk_display_peek_event
          gdk_display_pointer_is_grabbed
          gdk_display_pointer_ungrab
          gdk_display_put_event
          gdk_display_request_selection_notification
          gdk_display_set_double_click_distance
          gdk_display_set_double_click_time
          gdk_display_set_pointer_hooks
          gdk_display_store_clipboard
          gdk_display_supports_clipboard_persistence
          gdk_display_supports_composite
          gdk_display_supports_cursor_alpha
          gdk_display_supports_cursor_color
          gdk_display_supports_input_shapes
          gdk_display_supports_selection_notification
          gdk_display_supports_shapes
          gdk_display_sync
          gdk_display_warp_pointer)

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

  (define-syntax define-variadic-function
    (syntax-rules ()
      ((_ ret name args)
      (define name (lambda x (assertion-violation 'name "variadic function not supported"))))))

  ;; void gdk_display_add_client_message_filter (GdkDisplay* display, GdkAtom message_type, GdkFilterFunc func, gpointer data)
  (define-function void gdk_display_add_client_message_filter (void* void* (c-callback int (void* void* void*)) void*))

  ;; void gdk_display_beep (GdkDisplay* display)
  (define-function void gdk_display_beep (void*))

  ;; void gdk_display_close (GdkDisplay* display)
  (define-function void gdk_display_close (void*))

  ;; void gdk_display_flush (GdkDisplay* display)
  (define-function void gdk_display_flush (void*))

  ;; GdkDevice* gdk_display_get_core_pointer (GdkDisplay* display)
  (define-function void* gdk_display_get_core_pointer (void*))

  ;; GdkDisplay* gdk_display_get_default (void)
  (define-function void* gdk_display_get_default ())

  ;; guint gdk_display_get_default_cursor_size (GdkDisplay* display)
  (define-function unsigned-int gdk_display_get_default_cursor_size (void*))

  ;; GdkWindow* gdk_display_get_default_group (GdkDisplay* display)
  (define-function void* gdk_display_get_default_group (void*))

  ;; GdkScreen* gdk_display_get_default_screen (GdkDisplay* display)
  (define-function void* gdk_display_get_default_screen (void*))

  ;; GdkEvent* gdk_display_get_event (GdkDisplay* display)
  (define-function void* gdk_display_get_event (void*))

  ;; void gdk_display_get_maximal_cursor_size (GdkDisplay* display, guint* width, guint* height)
  (define-function void gdk_display_get_maximal_cursor_size (void* void* void*))

  ;; gint gdk_display_get_n_screens (GdkDisplay* display)
  (define-function int gdk_display_get_n_screens (void*))

  ;; const gchar* gdk_display_get_name (GdkDisplay* display)
  (define-function char* gdk_display_get_name (void*))

  ;; void gdk_display_get_pointer (GdkDisplay* display, GdkScreen** screen, gint* x, gint* y, GdkModifierType* mask)
  (define-function void gdk_display_get_pointer (void* void* void* void* void*))

  ;; GdkScreen* gdk_display_get_screen (GdkDisplay* display, gint screen_num)
  (define-function void* gdk_display_get_screen (void* int))

  ;; GType gdk_display_get_type (void)
  (define-function unsigned-long gdk_display_get_type ())

  ;; GdkWindow* gdk_display_get_window_at_pointer (GdkDisplay* display, gint* win_x, gint* win_y)
  (define-function void* gdk_display_get_window_at_pointer (void* void* void*))

  ;; void gdk_display_keyboard_ungrab (GdkDisplay* display, guint32 time_)
  (define-function void gdk_display_keyboard_ungrab (void* uint32_t))

  ;; GList* gdk_display_list_devices (GdkDisplay* display)
  (define-function void* gdk_display_list_devices (void*))

  ;; GdkDisplayManager* gdk_display_manager_get (void)
  (define-function void* gdk_display_manager_get ())

  ;; GdkDisplay* gdk_display_manager_get_default_display (GdkDisplayManager* display_manager)
  (define-function void* gdk_display_manager_get_default_display (void*))

  ;; GType gdk_display_manager_get_type (void)
  (define-function unsigned-long gdk_display_manager_get_type ())

  ;; GSList* gdk_display_manager_list_displays (GdkDisplayManager* display_manager)
  (define-function void* gdk_display_manager_list_displays (void*))

  ;; void gdk_display_manager_set_default_display (GdkDisplayManager* display_manager, GdkDisplay* display)
  (define-function void gdk_display_manager_set_default_display (void* void*))

  ;; GdkDisplay* gdk_display_open (const gchar* display_name)
  (define-function void* gdk_display_open (char*))

  ;; GdkDisplay* gdk_display_open_default_libgtk_only (void)
  (define-function void* gdk_display_open_default_libgtk_only ())

  ;; GdkEvent* gdk_display_peek_event (GdkDisplay* display)
  (define-function void* gdk_display_peek_event (void*))

  ;; gboolean gdk_display_pointer_is_grabbed (GdkDisplay* display)
  (define-function int gdk_display_pointer_is_grabbed (void*))

  ;; void gdk_display_pointer_ungrab (GdkDisplay* display, guint32 time_)
  (define-function void gdk_display_pointer_ungrab (void* uint32_t))

  ;; void gdk_display_put_event (GdkDisplay* display, const GdkEvent* event)
  (define-function void gdk_display_put_event (void* void*))

  ;; gboolean gdk_display_request_selection_notification (GdkDisplay* display, GdkAtom selection)
  (define-function int gdk_display_request_selection_notification (void* void*))

  ;; void gdk_display_set_double_click_distance (GdkDisplay* display, guint distance)
  (define-function void gdk_display_set_double_click_distance (void* unsigned-int))

  ;; void gdk_display_set_double_click_time (GdkDisplay* display, guint msec)
  (define-function void gdk_display_set_double_click_time (void* unsigned-int))

  ;; GdkDisplayPointerHooks* gdk_display_set_pointer_hooks (GdkDisplay* display, const GdkDisplayPointerHooks* new_hooks)
  (define-function void* gdk_display_set_pointer_hooks (void* void*))

  ;; void gdk_display_store_clipboard (GdkDisplay* display, GdkWindow* clipboard_window, guint32 time_, const GdkAtom* targets, gint n_targets)
  (define-function void gdk_display_store_clipboard (void* void* uint32_t void* int))

  ;; gboolean gdk_display_supports_clipboard_persistence (GdkDisplay* display)
  (define-function int gdk_display_supports_clipboard_persistence (void*))

  ;; gboolean gdk_display_supports_composite (GdkDisplay* display)
  (define-function int gdk_display_supports_composite (void*))

  ;; gboolean gdk_display_supports_cursor_alpha (GdkDisplay* display)
  (define-function int gdk_display_supports_cursor_alpha (void*))

  ;; gboolean gdk_display_supports_cursor_color (GdkDisplay* display)
  (define-function int gdk_display_supports_cursor_color (void*))

  ;; gboolean gdk_display_supports_input_shapes (GdkDisplay* display)
  (define-function int gdk_display_supports_input_shapes (void*))

  ;; gboolean gdk_display_supports_selection_notification (GdkDisplay* display)
  (define-function int gdk_display_supports_selection_notification (void*))

  ;; gboolean gdk_display_supports_shapes (GdkDisplay* display)
  (define-function int gdk_display_supports_shapes (void*))

  ;; void gdk_display_sync (GdkDisplay* display)
  (define-function void gdk_display_sync (void*))

  ;; void gdk_display_warp_pointer (GdkDisplay* display, GdkScreen* screen, gint x, gint y)
  (define-function void gdk_display_warp_pointer (void* void* int int))

  ) ;[end]
