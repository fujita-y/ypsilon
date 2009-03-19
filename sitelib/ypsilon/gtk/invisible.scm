#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk invisible)

  (export gtk_invisible_get_screen
          gtk_invisible_get_type
          gtk_invisible_new
          gtk_invisible_new_for_screen
          gtk_invisible_set_screen)

  (import (rnrs) (ypsilon ffi))

  (define lib-name
    (cond (on-darwin  "Gtk.framework/Gtk")
          (on-linux   "libgtk-x11-2.0.so.0")
          (on-freebsd "libgtk-x11-2.0.so.0")
          (on-openbsd "libgtk-x11-2.0.so.0")
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

  ;; GdkScreen* gtk_invisible_get_screen (GtkInvisible* invisible)
  (define-function void* gtk_invisible_get_screen (void*))

  ;; GType gtk_invisible_get_type (void)
  (define-function unsigned-long gtk_invisible_get_type ())

  ;; GtkWidget* gtk_invisible_new (void)
  (define-function void* gtk_invisible_new ())

  ;; GtkWidget* gtk_invisible_new_for_screen (GdkScreen* screen)
  (define-function void* gtk_invisible_new_for_screen (void*))

  ;; void gtk_invisible_set_screen (GtkInvisible* invisible, GdkScreen* screen)
  (define-function void gtk_invisible_set_screen (void* void*))

  ) ;[end]
