#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gdk rectangle)

  (export gdk_rectangle_get_type
          gdk_rectangle_intersect
          gdk_rectangle_union)

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

  ;; GType gdk_rectangle_get_type (void)
  (define-function unsigned-long gdk_rectangle_get_type ())

  ;; gboolean gdk_rectangle_intersect (const GdkRectangle* src1, const GdkRectangle* src2, GdkRectangle* dest)
  (define-function int gdk_rectangle_intersect (void* void* void*))

  ;; void gdk_rectangle_union (const GdkRectangle* src1, const GdkRectangle* src2, GdkRectangle* dest)
  (define-function void gdk_rectangle_union (void* void* void*))

  ) ;[end]
