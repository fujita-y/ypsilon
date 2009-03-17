#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gdk text)

  (export gdk_text_property_to_text_list
          gdk_text_property_to_text_list_for_display
          gdk_text_property_to_utf8_list
          gdk_text_property_to_utf8_list_for_display)

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

  ;; gint gdk_text_property_to_text_list (GdkAtom encoding, gint format, const guchar* text, gint length, gchar** *list)
  (define-function int gdk_text_property_to_text_list (void* int void* int void*))

  ;; gint gdk_text_property_to_text_list_for_display (GdkDisplay* display, GdkAtom encoding, gint format, const guchar* text, gint length, gchar** *list)
  (define-function int gdk_text_property_to_text_list_for_display (void* void* int void* int void*))

  ;; gint gdk_text_property_to_utf8_list (GdkAtom encoding, gint format, const guchar* text, gint length, gchar** *list)
  (define-function int gdk_text_property_to_utf8_list (void* int void* int void*))

  ;; gint gdk_text_property_to_utf8_list_for_display (GdkDisplay* display, GdkAtom encoding, gint format, const guchar* text, gint length, gchar** *list)
  (define-function int gdk_text_property_to_utf8_list_for_display (void* void* int void* int void*))

  ) ;[end]
