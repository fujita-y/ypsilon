#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gdk test)

  (export gdk_test_render_sync
          gdk_test_simulate_button
          gdk_test_simulate_key)

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

  ;; void gdk_test_render_sync (GdkWindow* window)
  (define-function void gdk_test_render_sync (void*))

  ;; gboolean gdk_test_simulate_button (GdkWindow* window, gint x, gint y, guint button, GdkModifierType modifiers, GdkEventType button_pressrelease)
  (define-function int gdk_test_simulate_button (void* int int unsigned-int int int))

  ;; gboolean gdk_test_simulate_key (GdkWindow* window, gint x, gint y, guint keyval, GdkModifierType modifiers, GdkEventType key_pressrelease)
  (define-function int gdk_test_simulate_key (void* int int unsigned-int int int))

  ) ;[end]
