#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk targets)

  (export gtk_targets_include_image
          gtk_targets_include_rich_text
          gtk_targets_include_text
          gtk_targets_include_uri)

  (import (rnrs) (ypsilon ffi))

  (define lib-name
    (cond (on-linux   "libgtk-x11-2.0.so.0")
          (on-sunos   "libgtk-x11-2.0.so.0")
          (on-freebsd "libgtk-x11-2.0.so.0")
          (on-openbsd "libgtk-x11-2.0.so.0")
          (on-darwin  "Gtk.framework/Gtk")
          (on-windows "libgtk-win32-2.0-0.dll")
          (else
           (assertion-violation #f "can not locate GTK library, unknown operating system"))))

  (define lib (load-shared-object lib-name))

  (define-syntax define-function
    (syntax-rules ()
      ((_ ret name args)
       (define name (c-function lib lib-name ret name args)))))

  (define-syntax define-function/va_list
    (syntax-rules ()
      ((_ ret name args)
      (define name (lambda x (assertion-violation 'name "va_list argument not supported"))))))

  ;; gboolean gtk_targets_include_image (GdkAtom* targets, gint n_targets, gboolean writable)
  (define-function int gtk_targets_include_image (void* int int))

  ;; gboolean gtk_targets_include_rich_text (GdkAtom* targets, gint n_targets, GtkTextBuffer* buffer)
  (define-function int gtk_targets_include_rich_text (void* int void*))

  ;; gboolean gtk_targets_include_text (GdkAtom* targets, gint n_targets)
  (define-function int gtk_targets_include_text (void* int))

  ;; gboolean gtk_targets_include_uri (GdkAtom* targets, gint n_targets)
  (define-function int gtk_targets_include_uri (void* int))

  ) ;[end]
