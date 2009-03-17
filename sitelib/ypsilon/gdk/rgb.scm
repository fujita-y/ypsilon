#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gdk rgb)

  (export gdk_rgb_cmap_free
          gdk_rgb_cmap_new
          gdk_rgb_colormap_ditherable
          gdk_rgb_dither_get_type
          gdk_rgb_ditherable
          gdk_rgb_find_color
          gdk_rgb_get_colormap
          gdk_rgb_get_visual
          gdk_rgb_set_install
          gdk_rgb_set_min_colors
          gdk_rgb_set_verbose)

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

  ;; void gdk_rgb_cmap_free (GdkRgbCmap* cmap)
  (define-function void gdk_rgb_cmap_free (void*))

  ;; GdkRgbCmap* gdk_rgb_cmap_new (guint32* colors, gint n_colors)
  (define-function void* gdk_rgb_cmap_new (void* int))

  ;; gboolean gdk_rgb_colormap_ditherable (GdkColormap* cmap)
  (define-function int gdk_rgb_colormap_ditherable (void*))

  ;; GType gdk_rgb_dither_get_type (void)
  (define-function unsigned-long gdk_rgb_dither_get_type ())

  ;; gboolean gdk_rgb_ditherable (void)
  (define-function int gdk_rgb_ditherable ())

  ;; void gdk_rgb_find_color (GdkColormap* colormap, GdkColor* color)
  (define-function void gdk_rgb_find_color (void* void*))

  ;; GdkColormap* gdk_rgb_get_colormap (void)
  (define-function void* gdk_rgb_get_colormap ())

  ;; GdkVisual* gdk_rgb_get_visual (void)
  (define-function void* gdk_rgb_get_visual ())

  ;; void gdk_rgb_set_install (gboolean install)
  (define-function void gdk_rgb_set_install (int))

  ;; void gdk_rgb_set_min_colors (gint min_colors)
  (define-function void gdk_rgb_set_min_colors (int))

  ;; void gdk_rgb_set_verbose (gboolean verbose)
  (define-function void gdk_rgb_set_verbose (int))

  ) ;[end]
