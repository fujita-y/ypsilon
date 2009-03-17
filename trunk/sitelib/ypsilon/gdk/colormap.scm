#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gdk colormap)

  (export gdk_colormap_alloc_color
          gdk_colormap_alloc_colors
          gdk_colormap_free_colors
          gdk_colormap_get_screen
          gdk_colormap_get_system
          gdk_colormap_get_type
          gdk_colormap_get_visual
          gdk_colormap_new
          gdk_colormap_query_color)

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

  ;; gboolean gdk_colormap_alloc_color (GdkColormap* colormap, GdkColor* color, gboolean writeable, gboolean best_match)
  (define-function int gdk_colormap_alloc_color (void* void* int int))

  ;; gint gdk_colormap_alloc_colors (GdkColormap* colormap, GdkColor* colors, gint n_colors, gboolean writeable, gboolean best_match, gboolean* success)
  (define-function int gdk_colormap_alloc_colors (void* void* int int int void*))

  ;; void gdk_colormap_free_colors (GdkColormap* colormap, const GdkColor* colors, gint n_colors)
  (define-function void gdk_colormap_free_colors (void* void* int))

  ;; GdkScreen* gdk_colormap_get_screen (GdkColormap* cmap)
  (define-function void* gdk_colormap_get_screen (void*))

  ;; GdkColormap* gdk_colormap_get_system (void)
  (define-function void* gdk_colormap_get_system ())

  ;; GType gdk_colormap_get_type (void)
  (define-function unsigned-long gdk_colormap_get_type ())

  ;; GdkVisual* gdk_colormap_get_visual (GdkColormap* colormap)
  (define-function void* gdk_colormap_get_visual (void*))

  ;; GdkColormap* gdk_colormap_new (GdkVisual* visual, gboolean allocate)
  (define-function void* gdk_colormap_new (void* int))

  ;; void gdk_colormap_query_color (GdkColormap* colormap, gulong pixel, GdkColor* result)
  (define-function void gdk_colormap_query_color (void* unsigned-long void*))

  ) ;[end]
