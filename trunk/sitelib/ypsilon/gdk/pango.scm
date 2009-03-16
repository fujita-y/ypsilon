#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gdk pango)

  (export gdk_pango_attr_emboss_color_new
          gdk_pango_attr_embossed_new
          gdk_pango_attr_stipple_new
          gdk_pango_context_get
          gdk_pango_context_get_for_screen
          gdk_pango_layout_get_clip_region
          gdk_pango_layout_line_get_clip_region
          gdk_pango_renderer_get_default
          gdk_pango_renderer_get_type
          gdk_pango_renderer_new
          gdk_pango_renderer_set_drawable
          gdk_pango_renderer_set_gc
          gdk_pango_renderer_set_override_color
          gdk_pango_renderer_set_stipple)

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

  ;; PangoAttribute* gdk_pango_attr_emboss_color_new (const GdkColor* color)
  (define-function void* gdk_pango_attr_emboss_color_new (void*))

  ;; PangoAttribute* gdk_pango_attr_embossed_new (gboolean embossed)
  (define-function void* gdk_pango_attr_embossed_new (int))

  ;; PangoAttribute* gdk_pango_attr_stipple_new (GdkBitmap* stipple)
  (define-function void* gdk_pango_attr_stipple_new (void*))

  ;; PangoContext* gdk_pango_context_get (void)
  (define-function void* gdk_pango_context_get ())

  ;; PangoContext* gdk_pango_context_get_for_screen (GdkScreen* screen)
  (define-function void* gdk_pango_context_get_for_screen (void*))

  ;; GdkRegion* gdk_pango_layout_get_clip_region (PangoLayout* layout, gint x_origin, gint y_origin, const gint* index_ranges, gint n_ranges)
  (define-function void* gdk_pango_layout_get_clip_region (void* int int void* int))

  ;; GdkRegion* gdk_pango_layout_line_get_clip_region (PangoLayoutLine* line, gint x_origin, gint y_origin, const gint* index_ranges, gint n_ranges)
  (define-function void* gdk_pango_layout_line_get_clip_region (void* int int void* int))

  ;; PangoRenderer* gdk_pango_renderer_get_default (GdkScreen* screen)
  (define-function void* gdk_pango_renderer_get_default (void*))

  ;; GType gdk_pango_renderer_get_type (void)
  (define-function unsigned-long gdk_pango_renderer_get_type ())

  ;; PangoRenderer* gdk_pango_renderer_new (GdkScreen* screen)
  (define-function void* gdk_pango_renderer_new (void*))

  ;; void gdk_pango_renderer_set_drawable (GdkPangoRenderer* gdk_renderer, GdkDrawable* drawable)
  (define-function void gdk_pango_renderer_set_drawable (void* void*))

  ;; void gdk_pango_renderer_set_gc (GdkPangoRenderer* gdk_renderer, GdkGC* gc)
  (define-function void gdk_pango_renderer_set_gc (void* void*))

  ;; void gdk_pango_renderer_set_override_color (GdkPangoRenderer* gdk_renderer, PangoRenderPart part, const GdkColor* color)
  (define-function void gdk_pango_renderer_set_override_color (void* int void*))

  ;; void gdk_pango_renderer_set_stipple (GdkPangoRenderer* gdk_renderer, PangoRenderPart part, GdkBitmap* stipple)
  (define-function void gdk_pango_renderer_set_stipple (void* int void*))

  ) ;[end]
