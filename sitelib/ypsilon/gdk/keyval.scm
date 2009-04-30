#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gdk keyval)

  (export gdk_keyval_convert_case
          gdk_keyval_from_name
          gdk_keyval_is_lower
          gdk_keyval_is_upper
          gdk_keyval_name
          gdk_keyval_to_lower
          gdk_keyval_to_unicode
          gdk_keyval_to_upper)

  (import (rnrs) (ypsilon ffi))

  (define lib-name
    (cond (on-linux   "libgdk-x11-2.0.so.0")
          (on-sunos   "libgdk-x11-2.0.so.0")
          (on-freebsd "libgdk-x11-2.0.so.0")
          (on-openbsd "libgdk-x11-2.0.so.0")
          (on-darwin  "Gtk.framework/Gtk")
          (on-windows "libgdk-win32-2.0-0.dll")
          (else
           (assertion-violation #f "can not locate GDK library, unknown operating system"))))

  (define lib (load-shared-object lib-name))

  (define-syntax define-function
    (syntax-rules ()
      ((_ ret name args)
       (define name (c-function lib lib-name ret name args)))))

  ;; void gdk_keyval_convert_case (guint symbol, guint* lower, guint* upper)
  (define-function void gdk_keyval_convert_case (unsigned-int void* void*))

  ;; guint gdk_keyval_from_name (const gchar* keyval_name)
  (define-function unsigned-int gdk_keyval_from_name (char*))

  ;; gboolean gdk_keyval_is_lower (guint keyval)
  (define-function int gdk_keyval_is_lower (unsigned-int))

  ;; gboolean gdk_keyval_is_upper (guint keyval)
  (define-function int gdk_keyval_is_upper (unsigned-int))

  ;; gchar* gdk_keyval_name (guint keyval)
  (define-function char* gdk_keyval_name (unsigned-int))

  ;; guint gdk_keyval_to_lower (guint keyval)
  (define-function unsigned-int gdk_keyval_to_lower (unsigned-int))

  ;; guint32 gdk_keyval_to_unicode (guint keyval)
  (define-function uint32_t gdk_keyval_to_unicode (unsigned-int))

  ;; guint gdk_keyval_to_upper (guint keyval)
  (define-function unsigned-int gdk_keyval_to_upper (unsigned-int))

  ) ;[end]
