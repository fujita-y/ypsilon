#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gdk draw)

  (export gdk_draw_arc
          gdk_draw_drawable
          gdk_draw_glyphs
          gdk_draw_glyphs_transformed
          gdk_draw_gray_image
          gdk_draw_image
          gdk_draw_indexed_image
          gdk_draw_layout
          gdk_draw_layout_line
          gdk_draw_layout_line_with_colors
          gdk_draw_layout_with_colors
          gdk_draw_line
          gdk_draw_lines
          gdk_draw_pixbuf
          gdk_draw_point
          gdk_draw_points
          gdk_draw_polygon
          gdk_draw_rectangle
          gdk_draw_rgb_32_image
          gdk_draw_rgb_32_image_dithalign
          gdk_draw_rgb_image
          gdk_draw_rgb_image_dithalign
          gdk_draw_segments
          gdk_draw_trapezoids)

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

  ;; void gdk_draw_arc (GdkDrawable* drawable, GdkGC* gc, gboolean filled, gint x, gint y, gint width, gint height, gint angle1, gint angle2)
  (define-function void gdk_draw_arc (void* void* int int int int int int int))

  ;; void gdk_draw_drawable (GdkDrawable* drawable, GdkGC* gc, GdkDrawable* src, gint xsrc, gint ysrc, gint xdest, gint ydest, gint width, gint height)
  (define-function void gdk_draw_drawable (void* void* void* int int int int int int))

  ;; void gdk_draw_glyphs (GdkDrawable* drawable, GdkGC* gc, PangoFont* font, gint x, gint y, PangoGlyphString* glyphs)
  (define-function void gdk_draw_glyphs (void* void* void* int int void*))

  ;; void gdk_draw_glyphs_transformed (GdkDrawable* drawable, GdkGC* gc, const PangoMatrix* matrix, PangoFont* font, gint x, gint y, PangoGlyphString* glyphs)
  (define-function void gdk_draw_glyphs_transformed (void* void* void* void* int int void*))

  ;; void gdk_draw_gray_image (GdkDrawable* drawable, GdkGC* gc, gint x, gint y, gint width, gint height, GdkRgbDither dith, const guchar* buf, gint rowstride)
  (define-function void gdk_draw_gray_image (void* void* int int int int int void* int))

  ;; void gdk_draw_image (GdkDrawable* drawable, GdkGC* gc, GdkImage* image, gint xsrc, gint ysrc, gint xdest, gint ydest, gint width, gint height)
  (define-function void gdk_draw_image (void* void* void* int int int int int int))

  ;; void gdk_draw_indexed_image (GdkDrawable* drawable, GdkGC* gc, gint x, gint y, gint width, gint height, GdkRgbDither dith, const guchar* buf, gint rowstride, GdkRgbCmap* cmap)
  (define-function void gdk_draw_indexed_image (void* void* int int int int int void* int void*))

  ;; void gdk_draw_layout (GdkDrawable* drawable, GdkGC* gc, gint x, gint y, PangoLayout* layout)
  (define-function void gdk_draw_layout (void* void* int int void*))

  ;; void gdk_draw_layout_line (GdkDrawable* drawable, GdkGC* gc, gint x, gint y, PangoLayoutLine* line)
  (define-function void gdk_draw_layout_line (void* void* int int void*))

  ;; void gdk_draw_layout_line_with_colors (GdkDrawable* drawable, GdkGC* gc, gint x, gint y, PangoLayoutLine* line, const GdkColor* foreground, const GdkColor* background)
  (define-function void gdk_draw_layout_line_with_colors (void* void* int int void* void* void*))

  ;; void gdk_draw_layout_with_colors (GdkDrawable* drawable, GdkGC* gc, gint x, gint y, PangoLayout* layout, const GdkColor* foreground, const GdkColor* background)
  (define-function void gdk_draw_layout_with_colors (void* void* int int void* void* void*))

  ;; void gdk_draw_line (GdkDrawable* drawable, GdkGC* gc, gint x1_, gint y1_, gint x2_, gint y2_)
  (define-function void gdk_draw_line (void* void* int int int int))

  ;; void gdk_draw_lines (GdkDrawable* drawable, GdkGC* gc, const GdkPoint* points, gint n_points)
  (define-function void gdk_draw_lines (void* void* void* int))

  ;; void gdk_draw_pixbuf (GdkDrawable* drawable, GdkGC* gc, const GdkPixbuf* pixbuf, gint src_x, gint src_y, gint dest_x, gint dest_y, gint width, gint height, GdkRgbDither dither, gint x_dither, gint y_dither)
  (define-function void gdk_draw_pixbuf (void* void* void* int int int int int int int int int))

  ;; void gdk_draw_point (GdkDrawable* drawable, GdkGC* gc, gint x, gint y)
  (define-function void gdk_draw_point (void* void* int int))

  ;; void gdk_draw_points (GdkDrawable* drawable, GdkGC* gc, const GdkPoint* points, gint n_points)
  (define-function void gdk_draw_points (void* void* void* int))

  ;; void gdk_draw_polygon (GdkDrawable* drawable, GdkGC* gc, gboolean filled, const GdkPoint* points, gint n_points)
  (define-function void gdk_draw_polygon (void* void* int void* int))

  ;; void gdk_draw_rectangle (GdkDrawable* drawable, GdkGC* gc, gboolean filled, gint x, gint y, gint width, gint height)
  (define-function void gdk_draw_rectangle (void* void* int int int int int))

  ;; void gdk_draw_rgb_32_image (GdkDrawable* drawable, GdkGC* gc, gint x, gint y, gint width, gint height, GdkRgbDither dith, const guchar* buf, gint rowstride)
  (define-function void gdk_draw_rgb_32_image (void* void* int int int int int void* int))

  ;; void gdk_draw_rgb_32_image_dithalign (GdkDrawable* drawable, GdkGC* gc, gint x, gint y, gint width, gint height, GdkRgbDither dith, const guchar* buf, gint rowstride, gint xdith, gint ydith)
  (define-function void gdk_draw_rgb_32_image_dithalign (void* void* int int int int int void* int int int))

  ;; void gdk_draw_rgb_image (GdkDrawable* drawable, GdkGC* gc, gint x, gint y, gint width, gint height, GdkRgbDither dith, const guchar* rgb_buf, gint rowstride)
  (define-function void gdk_draw_rgb_image (void* void* int int int int int void* int))

  ;; void gdk_draw_rgb_image_dithalign (GdkDrawable* drawable, GdkGC* gc, gint x, gint y, gint width, gint height, GdkRgbDither dith, const guchar* rgb_buf, gint rowstride, gint xdith, gint ydith)
  (define-function void gdk_draw_rgb_image_dithalign (void* void* int int int int int void* int int int))

  ;; void gdk_draw_segments (GdkDrawable* drawable, GdkGC* gc, const GdkSegment* segs, gint n_segs)
  (define-function void gdk_draw_segments (void* void* void* int))

  ;; void gdk_draw_trapezoids (GdkDrawable* drawable, GdkGC* gc, const GdkTrapezoid* trapezoids, gint n_trapezoids)
  (define-function void gdk_draw_trapezoids (void* void* void* int))

  ) ;[end]
