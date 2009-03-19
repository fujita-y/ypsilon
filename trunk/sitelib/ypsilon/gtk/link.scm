#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk link)

  (export gtk_link_button_get_type
          gtk_link_button_get_uri
          gtk_link_button_get_visited
          gtk_link_button_new
          gtk_link_button_new_with_label
          gtk_link_button_set_uri
          gtk_link_button_set_uri_hook
          gtk_link_button_set_visited)

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

  ;; GType gtk_link_button_get_type (void)
  (define-function unsigned-long gtk_link_button_get_type ())

  ;; const gchar* gtk_link_button_get_uri (GtkLinkButton* link_button)
  (define-function char* gtk_link_button_get_uri (void*))

  ;; gboolean gtk_link_button_get_visited (GtkLinkButton* link_button)
  (define-function int gtk_link_button_get_visited (void*))

  ;; GtkWidget* gtk_link_button_new (const gchar* uri)
  (define-function void* gtk_link_button_new (char*))

  ;; GtkWidget* gtk_link_button_new_with_label (const gchar* uri, const gchar* label)
  (define-function void* gtk_link_button_new_with_label (char* char*))

  ;; void gtk_link_button_set_uri (GtkLinkButton* link_button, const gchar* uri)
  (define-function void gtk_link_button_set_uri (void* char*))

  ;; GtkLinkButtonUriFunc gtk_link_button_set_uri_hook (GtkLinkButtonUriFunc func, gpointer data, GDestroyNotify destroy)
  (define-function void* gtk_link_button_set_uri_hook (void* void* (c-callback void (void*))))

  ;; void gtk_link_button_set_visited (GtkLinkButton* link_button, gboolean visited)
  (define-function void gtk_link_button_set_visited (void* int))

  ) ;[end]
