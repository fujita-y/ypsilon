#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk main)

  (export gtk_main
          gtk_main_do_event
          gtk_main_iteration
          gtk_main_iteration_do
          gtk_main_level
          gtk_main_quit)

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

  ;; void gtk_main (void)
  (define-function void gtk_main ())

  ;; void gtk_main_do_event (GdkEvent* event)
  (define-function void gtk_main_do_event (void*))

  ;; gboolean gtk_main_iteration (void)
  (define-function int gtk_main_iteration ())

  ;; gboolean gtk_main_iteration_do (gboolean blocking)
  (define-function int gtk_main_iteration_do (int))

  ;; guint gtk_main_level (void)
  (define-function unsigned-int gtk_main_level ())

  ;; void gtk_main_quit (void)
  (define-function void gtk_main_quit ())

  ) ;[end]
