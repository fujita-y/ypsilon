#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gdk drop)

  (export gdk_drop_finish
          gdk_drop_reply)

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

  ;; void gdk_drop_finish (GdkDragContext* context, gboolean success, guint32 time_)
  (define-function void gdk_drop_finish (void* int uint32_t))

  ;; void gdk_drop_reply (GdkDragContext* context, gboolean ok, guint32 time_)
  (define-function void gdk_drop_reply (void* int uint32_t))

  ) ;[end]
