#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gdk string)

  (export gdk_string_to_compound_text
          gdk_string_to_compound_text_for_display)

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

  ;; gint gdk_string_to_compound_text (const gchar* str, GdkAtom* encoding, gint* format, guchar** ctext, gint* length)
  (define-function int gdk_string_to_compound_text (char* void* void* void* void*))

  ;; gint gdk_string_to_compound_text_for_display (GdkDisplay* display, const gchar* str, GdkAtom* encoding, gint* format, guchar** ctext, gint* length)
  (define-function int gdk_string_to_compound_text_for_display (void* char* void* void* void* void*))

  ) ;[end]
