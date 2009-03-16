#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk buildable)

  (export gtk_buildable_add_child
          gtk_buildable_construct_child
          gtk_buildable_custom_finished
          gtk_buildable_custom_tag_end
          gtk_buildable_custom_tag_start
          gtk_buildable_get_internal_child
          gtk_buildable_get_name
          gtk_buildable_get_type
          gtk_buildable_parser_finished
          gtk_buildable_set_buildable_property
          gtk_buildable_set_name)

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

  (define-syntax define-variadic-function
    (syntax-rules ()
      ((_ ret name args)
      (define name (lambda x (assertion-violation 'name "variadic function not supported"))))))

  ;; void gtk_buildable_add_child (GtkBuildable* buildable, GtkBuilder* builder, GObject* child, const gchar* type)
  (define-function void gtk_buildable_add_child (void* void* void* char*))

  ;; GObject* gtk_buildable_construct_child (GtkBuildable* buildable, GtkBuilder* builder, const gchar* name)
  (define-function void* gtk_buildable_construct_child (void* void* char*))

  ;; void gtk_buildable_custom_finished (GtkBuildable* buildable, GtkBuilder* builder, GObject* child, const gchar* tagname, gpointer data)
  (define-function void gtk_buildable_custom_finished (void* void* void* char* void*))

  ;; void gtk_buildable_custom_tag_end (GtkBuildable* buildable, GtkBuilder* builder, GObject* child, const gchar* tagname, gpointer* data)
  (define-function void gtk_buildable_custom_tag_end (void* void* void* char* void*))

  ;; gboolean gtk_buildable_custom_tag_start (GtkBuildable* buildable, GtkBuilder* builder, GObject* child, const gchar* tagname, GMarkupParser* parser, gpointer* data)
  (define-function int gtk_buildable_custom_tag_start (void* void* void* char* void* void*))

  ;; GObject* gtk_buildable_get_internal_child (GtkBuildable* buildable, GtkBuilder* builder, const gchar* childname)
  (define-function void* gtk_buildable_get_internal_child (void* void* char*))

  ;; const gchar* gtk_buildable_get_name (GtkBuildable* buildable)
  (define-function char* gtk_buildable_get_name (void*))

  ;; GType gtk_buildable_get_type (void)
  (define-function unsigned-long gtk_buildable_get_type ())

  ;; void gtk_buildable_parser_finished (GtkBuildable* buildable, GtkBuilder* builder)
  (define-function void gtk_buildable_parser_finished (void* void*))

  ;; void gtk_buildable_set_buildable_property (GtkBuildable* buildable, GtkBuilder* builder, const gchar* name, const GValue* value)
  (define-function void gtk_buildable_set_buildable_property (void* void* char* void*))

  ;; void gtk_buildable_set_name (GtkBuildable* buildable, const gchar* name)
  (define-function void gtk_buildable_set_name (void* char*))

  ) ;[end]
