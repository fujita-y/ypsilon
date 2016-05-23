#!/usr/bin/env ypsilon
#!r6rs

;; gtk-hello.scm:
;;   GTK hello world
;;
;; Requirements:
;;   Darwin:  Gtk.framework
;;   Linux:   libgtk-x11-2.0.so.0
;;   FreeBSD: libgtk-x11-2.0.so
;;   OpenBSD: libgtk-x11-2.0.so

(import (rnrs)
        (srfi :48)
        (ypsilon gtk constants)
        (ypsilon gtk init)
        (ypsilon gtk main)
        (ypsilon gtk window)
        (ypsilon gtk container)
        (ypsilon gtk button)
        (ypsilon gtk widget)
        (ypsilon gobject signal)
        (ypsilon ffi))

(gtk_init (vector (length (command-line))) (apply vector (command-line)))
(let ((window (gtk_window_new GTK_WINDOW_TOPLEVEL))
      (button (gtk_button_new_with_label "Hello World"))
      (destroy
       (signal-callback gboolean (GtkObject* gpointer)
         (lambda (obj data)
           (format #t "[destory ~s ~s]~%" obj data)
           (gtk_main_quit))))
      (clicked
       (signal-callback gboolean (GtkButton* gpointer)
         (lambda (button data)
           (format #t "[clicked ~s ~s]~%" button data)))))
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
