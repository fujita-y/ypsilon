#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gdk screen)

  (export gdk_screen_broadcast_client_message
          gdk_screen_get_active_window
          gdk_screen_get_default
          gdk_screen_get_default_colormap
          gdk_screen_get_display
          gdk_screen_get_font_options
          gdk_screen_get_height
          gdk_screen_get_height_mm
          gdk_screen_get_monitor_at_point
          gdk_screen_get_monitor_at_window
          gdk_screen_get_monitor_geometry
          gdk_screen_get_monitor_height_mm
          gdk_screen_get_monitor_plug_name
          gdk_screen_get_monitor_width_mm
          gdk_screen_get_n_monitors
          gdk_screen_get_number
          gdk_screen_get_resolution
          gdk_screen_get_rgb_colormap
          gdk_screen_get_rgb_visual
          gdk_screen_get_rgba_colormap
          gdk_screen_get_rgba_visual
          gdk_screen_get_root_window
          gdk_screen_get_setting
          gdk_screen_get_system_colormap
          gdk_screen_get_system_visual
          gdk_screen_get_toplevel_windows
          gdk_screen_get_type
          gdk_screen_get_width
          gdk_screen_get_width_mm
          gdk_screen_get_window_stack
          gdk_screen_height
          gdk_screen_height_mm
          gdk_screen_is_composited
          gdk_screen_list_visuals
          gdk_screen_make_display_name
          gdk_screen_set_default_colormap
          gdk_screen_set_font_options
          gdk_screen_set_resolution
          gdk_screen_width
          gdk_screen_width_mm)

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

  ;; void gdk_screen_broadcast_client_message (GdkScreen* screen, GdkEvent* event)
  (define-function void gdk_screen_broadcast_client_message (void* void*))

  ;; GdkWindow* gdk_screen_get_active_window (GdkScreen* screen)
  (define-function void* gdk_screen_get_active_window (void*))

  ;; GdkScreen* gdk_screen_get_default (void)
  (define-function void* gdk_screen_get_default ())

  ;; GdkColormap* gdk_screen_get_default_colormap (GdkScreen* screen)
  (define-function void* gdk_screen_get_default_colormap (void*))

  ;; GdkDisplay* gdk_screen_get_display (GdkScreen* screen)
  (define-function void* gdk_screen_get_display (void*))

  ;; const cairo_font_options_t* gdk_screen_get_font_options (GdkScreen* screen)
  (define-function void* gdk_screen_get_font_options (void*))

  ;; gint gdk_screen_get_height (GdkScreen* screen)
  (define-function int gdk_screen_get_height (void*))

  ;; gint gdk_screen_get_height_mm (GdkScreen* screen)
  (define-function int gdk_screen_get_height_mm (void*))

  ;; gint gdk_screen_get_monitor_at_point (GdkScreen* screen, gint x, gint y)
  (define-function int gdk_screen_get_monitor_at_point (void* int int))

  ;; gint gdk_screen_get_monitor_at_window (GdkScreen* screen, GdkWindow* window)
  (define-function int gdk_screen_get_monitor_at_window (void* void*))

  ;; void gdk_screen_get_monitor_geometry (GdkScreen* screen, gint monitor_num, GdkRectangle* dest)
  (define-function void gdk_screen_get_monitor_geometry (void* int void*))

  ;; gint gdk_screen_get_monitor_height_mm (GdkScreen* screen, gint monitor_num)
  (define-function int gdk_screen_get_monitor_height_mm (void* int))

  ;; gchar* gdk_screen_get_monitor_plug_name (GdkScreen* screen, gint monitor_num)
  (define-function char* gdk_screen_get_monitor_plug_name (void* int))

  ;; gint gdk_screen_get_monitor_width_mm (GdkScreen* screen, gint monitor_num)
  (define-function int gdk_screen_get_monitor_width_mm (void* int))

  ;; gint gdk_screen_get_n_monitors (GdkScreen* screen)
  (define-function int gdk_screen_get_n_monitors (void*))

  ;; gint gdk_screen_get_number (GdkScreen* screen)
  (define-function int gdk_screen_get_number (void*))

  ;; gdouble gdk_screen_get_resolution (GdkScreen* screen)
  (define-function double gdk_screen_get_resolution (void*))

  ;; GdkColormap* gdk_screen_get_rgb_colormap (GdkScreen* screen)
  (define-function void* gdk_screen_get_rgb_colormap (void*))

  ;; GdkVisual* gdk_screen_get_rgb_visual (GdkScreen* screen)
  (define-function void* gdk_screen_get_rgb_visual (void*))

  ;; GdkColormap* gdk_screen_get_rgba_colormap (GdkScreen* screen)
  (define-function void* gdk_screen_get_rgba_colormap (void*))

  ;; GdkVisual* gdk_screen_get_rgba_visual (GdkScreen* screen)
  (define-function void* gdk_screen_get_rgba_visual (void*))

  ;; GdkWindow* gdk_screen_get_root_window (GdkScreen* screen)
  (define-function void* gdk_screen_get_root_window (void*))

  ;; gboolean gdk_screen_get_setting (GdkScreen* screen, const gchar* name, GValue* value)
  (define-function int gdk_screen_get_setting (void* char* void*))

  ;; GdkColormap* gdk_screen_get_system_colormap (GdkScreen* screen)
  (define-function void* gdk_screen_get_system_colormap (void*))

  ;; GdkVisual* gdk_screen_get_system_visual (GdkScreen* screen)
  (define-function void* gdk_screen_get_system_visual (void*))

  ;; GList* gdk_screen_get_toplevel_windows (GdkScreen* screen)
  (define-function void* gdk_screen_get_toplevel_windows (void*))

  ;; GType gdk_screen_get_type (void)
  (define-function unsigned-long gdk_screen_get_type ())

  ;; gint gdk_screen_get_width (GdkScreen* screen)
  (define-function int gdk_screen_get_width (void*))

  ;; gint gdk_screen_get_width_mm (GdkScreen* screen)
  (define-function int gdk_screen_get_width_mm (void*))

  ;; GList* gdk_screen_get_window_stack (GdkScreen* screen)
  (define-function void* gdk_screen_get_window_stack (void*))

  ;; gint gdk_screen_height (void)
  (define-function int gdk_screen_height ())

  ;; gint gdk_screen_height_mm (void)
  (define-function int gdk_screen_height_mm ())

  ;; gboolean gdk_screen_is_composited (GdkScreen* screen)
  (define-function int gdk_screen_is_composited (void*))

  ;; GList* gdk_screen_list_visuals (GdkScreen* screen)
  (define-function void* gdk_screen_list_visuals (void*))

  ;; gchar* gdk_screen_make_display_name (GdkScreen* screen)
  (define-function char* gdk_screen_make_display_name (void*))

  ;; void gdk_screen_set_default_colormap (GdkScreen* screen, GdkColormap* colormap)
  (define-function void gdk_screen_set_default_colormap (void* void*))

  ;; void gdk_screen_set_font_options (GdkScreen* screen, const cairo_font_options_t* options)
  (define-function void gdk_screen_set_font_options (void* void*))

  ;; void gdk_screen_set_resolution (GdkScreen* screen, gdouble dpi)
  (define-function void gdk_screen_set_resolution (void* double))

  ;; gint gdk_screen_width (void)
  (define-function int gdk_screen_width ())

  ;; gint gdk_screen_width_mm (void)
  (define-function int gdk_screen_width_mm ())

  ) ;[end]
