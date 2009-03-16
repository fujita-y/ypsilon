#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gdk visual)

  (export gdk_visual_get_best
          gdk_visual_get_best_depth
          gdk_visual_get_best_type
          gdk_visual_get_best_with_both
          gdk_visual_get_best_with_depth
          gdk_visual_get_best_with_type
          gdk_visual_get_screen
          gdk_visual_get_system
          gdk_visual_get_type
          gdk_visual_type_get_type)

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

  ;; GdkVisual* gdk_visual_get_best (void)
  (define-function void* gdk_visual_get_best ())

  ;; gint gdk_visual_get_best_depth (void)
  (define-function int gdk_visual_get_best_depth ())

  ;; GdkVisualType gdk_visual_get_best_type (void)
  (define-function int gdk_visual_get_best_type ())

  ;; GdkVisual* gdk_visual_get_best_with_both (gint depth, GdkVisualType visual_type)
  (define-function void* gdk_visual_get_best_with_both (int int))

  ;; GdkVisual* gdk_visual_get_best_with_depth (gint depth)
  (define-function void* gdk_visual_get_best_with_depth (int))

  ;; GdkVisual* gdk_visual_get_best_with_type (GdkVisualType visual_type)
  (define-function void* gdk_visual_get_best_with_type (int))

  ;; GdkScreen* gdk_visual_get_screen (GdkVisual* visual)
  (define-function void* gdk_visual_get_screen (void*))

  ;; GdkVisual* gdk_visual_get_system (void)
  (define-function void* gdk_visual_get_system ())

  ;; GType gdk_visual_get_type (void)
  (define-function unsigned-long gdk_visual_get_type ())

  ;; GType gdk_visual_type_get_type (void)
  (define-function unsigned-long gdk_visual_type_get_type ())

  ) ;[end]
