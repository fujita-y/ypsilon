#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gdk gc)

  (export gdk_gc_copy
          gdk_gc_get_colormap
          gdk_gc_get_screen
          gdk_gc_get_type
          gdk_gc_get_values
          gdk_gc_new
          gdk_gc_new_with_values
          gdk_gc_offset
          gdk_gc_set_background
          gdk_gc_set_clip_mask
          gdk_gc_set_clip_origin
          gdk_gc_set_clip_rectangle
          gdk_gc_set_clip_region
          gdk_gc_set_colormap
          gdk_gc_set_dashes
          gdk_gc_set_exposures
          gdk_gc_set_fill
          gdk_gc_set_foreground
          gdk_gc_set_function
          gdk_gc_set_line_attributes
          gdk_gc_set_rgb_bg_color
          gdk_gc_set_rgb_fg_color
          gdk_gc_set_stipple
          gdk_gc_set_subwindow
          gdk_gc_set_tile
          gdk_gc_set_ts_origin
          gdk_gc_set_values
          gdk_gc_values_mask_get_type)

  (import (rnrs) (ypsilon ffi))

  (define lib-name
    (cond (on-darwin  "Gdk.framework/Gdk")
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

  ;; void gdk_gc_copy (GdkGC* dst_gc, GdkGC* src_gc)
  (define-function void gdk_gc_copy (void* void*))

  ;; GdkColormap* gdk_gc_get_colormap (GdkGC* gc)
  (define-function void* gdk_gc_get_colormap (void*))

  ;; GdkScreen* gdk_gc_get_screen (GdkGC* gc)
  (define-function void* gdk_gc_get_screen (void*))

  ;; GType gdk_gc_get_type (void)
  (define-function unsigned-long gdk_gc_get_type ())

  ;; void gdk_gc_get_values (GdkGC* gc, GdkGCValues* values)
  (define-function void gdk_gc_get_values (void* void*))

  ;; GdkGC* gdk_gc_new (GdkDrawable* drawable)
  (define-function void* gdk_gc_new (void*))

  ;; GdkGC* gdk_gc_new_with_values (GdkDrawable* drawable, GdkGCValues* values, GdkGCValuesMask values_mask)
  (define-function void* gdk_gc_new_with_values (void* void* int))

  ;; void gdk_gc_offset (GdkGC* gc, gint x_offset, gint y_offset)
  (define-function void gdk_gc_offset (void* int int))

  ;; void gdk_gc_set_background (GdkGC* gc, const GdkColor* color)
  (define-function void gdk_gc_set_background (void* void*))

  ;; void gdk_gc_set_clip_mask (GdkGC* gc, GdkBitmap* mask)
  (define-function void gdk_gc_set_clip_mask (void* void*))

  ;; void gdk_gc_set_clip_origin (GdkGC* gc, gint x, gint y)
  (define-function void gdk_gc_set_clip_origin (void* int int))

  ;; void gdk_gc_set_clip_rectangle (GdkGC* gc, const GdkRectangle* rectangle)
  (define-function void gdk_gc_set_clip_rectangle (void* void*))

  ;; void gdk_gc_set_clip_region (GdkGC* gc, const GdkRegion* region)
  (define-function void gdk_gc_set_clip_region (void* void*))

  ;; void gdk_gc_set_colormap (GdkGC* gc, GdkColormap* colormap)
  (define-function void gdk_gc_set_colormap (void* void*))

  ;; void gdk_gc_set_dashes (GdkGC* gc, gint dash_offset, gint8 dash_list[], gint n)
  (define-function void gdk_gc_set_dashes (void* int void* int))

  ;; void gdk_gc_set_exposures (GdkGC* gc, gboolean exposures)
  (define-function void gdk_gc_set_exposures (void* int))

  ;; void gdk_gc_set_fill (GdkGC* gc, GdkFill fill)
  (define-function void gdk_gc_set_fill (void* int))

  ;; void gdk_gc_set_foreground (GdkGC* gc, const GdkColor* color)
  (define-function void gdk_gc_set_foreground (void* void*))

  ;; void gdk_gc_set_function (GdkGC* gc, GdkFunction function)
  (define-function void gdk_gc_set_function (void* int))

  ;; void gdk_gc_set_line_attributes (GdkGC* gc, gint line_width, GdkLineStyle line_style, GdkCapStyle cap_style, GdkJoinStyle join_style)
  (define-function void gdk_gc_set_line_attributes (void* int int int int))

  ;; void gdk_gc_set_rgb_bg_color (GdkGC* gc, const GdkColor* color)
  (define-function void gdk_gc_set_rgb_bg_color (void* void*))

  ;; void gdk_gc_set_rgb_fg_color (GdkGC* gc, const GdkColor* color)
  (define-function void gdk_gc_set_rgb_fg_color (void* void*))

  ;; void gdk_gc_set_stipple (GdkGC* gc, GdkPixmap* stipple)
  (define-function void gdk_gc_set_stipple (void* void*))

  ;; void gdk_gc_set_subwindow (GdkGC* gc, GdkSubwindowMode mode)
  (define-function void gdk_gc_set_subwindow (void* int))

  ;; void gdk_gc_set_tile (GdkGC* gc, GdkPixmap* tile)
  (define-function void gdk_gc_set_tile (void* void*))

  ;; void gdk_gc_set_ts_origin (GdkGC* gc, gint x, gint y)
  (define-function void gdk_gc_set_ts_origin (void* int int))

  ;; void gdk_gc_set_values (GdkGC* gc, GdkGCValues* values, GdkGCValuesMask values_mask)
  (define-function void gdk_gc_set_values (void* void* int))

  ;; GType gdk_gc_values_mask_get_type (void)
  (define-function unsigned-long gdk_gc_values_mask_get_type ())

  ) ;[end]
