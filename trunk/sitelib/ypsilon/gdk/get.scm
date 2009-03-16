#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gdk get)

  (export gdk_get_default_root_window
          gdk_get_display
          gdk_get_display_arg_name
          gdk_get_program_class
          gdk_get_show_events)

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

  ;; GdkWindow* gdk_get_default_root_window (void)
  (define-function void* gdk_get_default_root_window ())

  ;; gchar* gdk_get_display (void)
  (define-function char* gdk_get_display ())

  ;; const gchar* gdk_get_display_arg_name (void)
  (define-function char* gdk_get_display_arg_name ())

  ;; const char* gdk_get_program_class (void)
  (define-function char* gdk_get_program_class ())

  ;; gboolean gdk_get_show_events (void)
  (define-function int gdk_get_show_events ())

  ) ;[end]
