#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gdk region)

  (export gdk_region_copy
          gdk_region_destroy
          gdk_region_empty
          gdk_region_equal
          gdk_region_get_clipbox
          gdk_region_get_rectangles
          gdk_region_intersect
          gdk_region_new
          gdk_region_offset
          gdk_region_point_in
          gdk_region_polygon
          gdk_region_rect_in
          gdk_region_rectangle
          gdk_region_shrink
          gdk_region_spans_intersect_foreach
          gdk_region_subtract
          gdk_region_union
          gdk_region_union_with_rect
          gdk_region_xor)

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

  ;; GdkRegion* gdk_region_copy (const GdkRegion* region)
  (define-function void* gdk_region_copy (void*))

  ;; void gdk_region_destroy (GdkRegion* region)
  (define-function void gdk_region_destroy (void*))

  ;; gboolean gdk_region_empty (const GdkRegion* region)
  (define-function int gdk_region_empty (void*))

  ;; gboolean gdk_region_equal (const GdkRegion* region1, const GdkRegion* region2)
  (define-function int gdk_region_equal (void* void*))

  ;; void gdk_region_get_clipbox (const GdkRegion* region, GdkRectangle* rectangle)
  (define-function void gdk_region_get_clipbox (void* void*))

  ;; void gdk_region_get_rectangles (const GdkRegion* region, GdkRectangle** rectangles, gint* n_rectangles)
  (define-function void gdk_region_get_rectangles (void* void* void*))

  ;; void gdk_region_intersect (GdkRegion* source1, const GdkRegion* source2)
  (define-function void gdk_region_intersect (void* void*))

  ;; GdkRegion* gdk_region_new (void)
  (define-function void* gdk_region_new ())

  ;; void gdk_region_offset (GdkRegion* region, gint dx, gint dy)
  (define-function void gdk_region_offset (void* int int))

  ;; gboolean gdk_region_point_in (const GdkRegion* region, int x, int y)
  (define-function int gdk_region_point_in (void* int int))

  ;; GdkRegion* gdk_region_polygon (const GdkPoint* points, gint n_points, GdkFillRule fill_rule)
  (define-function void* gdk_region_polygon (void* int int))

  ;; GdkOverlapType gdk_region_rect_in (const GdkRegion* region, const GdkRectangle* rectangle)
  (define-function int gdk_region_rect_in (void* void*))

  ;; GdkRegion* gdk_region_rectangle (const GdkRectangle* rectangle)
  (define-function void* gdk_region_rectangle (void*))

  ;; void gdk_region_shrink (GdkRegion* region, gint dx, gint dy)
  (define-function void gdk_region_shrink (void* int int))

  ;; void gdk_region_spans_intersect_foreach (GdkRegion* region, const GdkSpan* spans, int n_spans, gboolean sorted, GdkSpanFunc function, gpointer data)
  (define-function void gdk_region_spans_intersect_foreach (void* void* int int (c-callback void (void* void*)) void*))

  ;; void gdk_region_subtract (GdkRegion* source1, const GdkRegion* source2)
  (define-function void gdk_region_subtract (void* void*))

  ;; void gdk_region_union (GdkRegion* source1, const GdkRegion* source2)
  (define-function void gdk_region_union (void* void*))

  ;; void gdk_region_union_with_rect (GdkRegion* region, const GdkRectangle* rect)
  (define-function void gdk_region_union_with_rect (void* void*))

  ;; void gdk_region_xor (GdkRegion* source1, const GdkRegion* source2)
  (define-function void gdk_region_xor (void* void*))

  ) ;[end]
