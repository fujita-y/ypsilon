#!/usr/bin/env ypsilon
#!r6rs

;; gtk-hello.scm:
;;   GTK hello world
;;
;; Requirements:
;;   Darwin:  Gtk.framework
;;   Linux:   libgtk-x11-2.0.so.0

(import (rnrs)
        (srfi :28)
        (ypsilon ffi))

; minimal bindings for GTK hello world

(define libgtk-name (cond (on-linux "libgtk-x11-2.0.so.0")
                          (on-darwin "Gtk.framework/Gtk")
                          (on-freebsd "libgtk-x11-2.0.so")
                          (else
                           (assertion-violation #f "can not locate GTK library, unknown operating system"))))

(define libgtk (load-shared-object libgtk-name))

(define-syntax define-function
  (syntax-rules ()
    ((_ ret name args)
     (define name (c-function libgtk libgtk-name ret name args)))))

(define-function void  gtk_init ([int] (* [char*])))
(define-function void* gtk_window_new (int))
(define-function void  gtk_window_set_title (void* char*))
(define-function void  gtk_window_resize (void* int int))
(define-function void* gtk_button_new_with_label (char*))
(define-function void  gtk_widget_show (void*))
(define-function void  gtk_widget_destroy (void*))
(define-function void  gtk_main ())
(define-function void  gtk_main_quit ())
(define-function void  gtk_container_add (void* void*))
(define-function void  gtk_container_set_border_width (void* int))
(define-function int   g_signal_connect_data (void* char* [c-callback int (void* void* void*)] void* void* int))
(define-function int   g_signal_connect_object (void* char* [c-callback int (void*)] void* int))

(define GTK_WINDOW_TOPLEVEL 0)
(define G_CONNECT_SWAPPED   2)

(define g_signal_connect
  (lambda (instance detailed_signal c_handler data)
    (g_signal_connect_data instance detailed_signal c_handler data 0 0)))

(define g_signal_connect_swapped
  (lambda (instance detailed_signal c_handler object)
    (g_signal_connect_object instance detailed_signal c_handler object G_CONNECT_SWAPPED)))

; GTK hello world

(gtk_init (vector (length (command-line))) (apply vector (command-line)))
(let ((window (gtk_window_new GTK_WINDOW_TOPLEVEL))
      (button (gtk_button_new_with_label "Hello World"))
      (destroy
       (lambda x
         (format #t "[destory ~s]~%" x)
         (gtk_main_quit)))
      (clicked
       (lambda x
         (format #t "[clicked ~s]~%" x))))
  (gtk_container_set_border_width window 10)
  (gtk_window_set_title window "Hello World")
  (gtk_window_resize window 256 128)
  (g_signal_connect window "destroy" destroy 0)
  (g_signal_connect button "clicked" clicked 0)
  (g_signal_connect_swapped button "clicked" gtk_widget_destroy window)
  (gtk_container_add window button)
  (gtk_widget_show button)
  (gtk_widget_show window)
  (gtk_main))
