#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk init)

  (export gtk_init
          gtk_init_add
          gtk_init_check
          gtk_init_with_args)

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

  ;; void gtk_init (int* argc, char** *argv)
  (define-function void gtk_init ((int) (* (char*))))

  ;; void gtk_init_add (GtkFunction function, gpointer data)
  (define-function void gtk_init_add ((c-callback int (void*)) void*))

  ;; gboolean gtk_init_check (int* argc, char** *argv)
  (define-function int gtk_init_check ((int) (* (char*))))

  ;; gboolean gtk_init_with_args (int* argc, char** *argv, char* parameter_string, GOptionEntry* entries, char* translation_domain, GError** error)
  (define-function int gtk_init_with_args ((int) (* (char*)) char* void* char* void*))

  ) ;[end]
