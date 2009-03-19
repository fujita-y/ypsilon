#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk binding)

  (export gtk_binding_entry_add_signal
          gtk_binding_entry_remove
          gtk_binding_entry_skip
          gtk_binding_set_activate
          gtk_binding_set_add_path
          gtk_binding_set_by_class
          gtk_binding_set_find
          gtk_binding_set_new)

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

  ;; void gtk_binding_entry_add_signal (GtkBindingSet* binding_set, guint keyval, GdkModifierType modifiers, const gchar* signal_name, guint n_args, ...)
  (define-function void gtk_binding_entry_add_signal (void* unsigned-int int char* unsigned-int ...))

  ;; void gtk_binding_entry_remove (GtkBindingSet* binding_set, guint keyval, GdkModifierType modifiers)
  (define-function void gtk_binding_entry_remove (void* unsigned-int int))

  ;; void gtk_binding_entry_skip (GtkBindingSet* binding_set, guint keyval, GdkModifierType modifiers)
  (define-function void gtk_binding_entry_skip (void* unsigned-int int))

  ;; gboolean gtk_binding_set_activate (GtkBindingSet* binding_set, guint keyval, GdkModifierType modifiers, GtkObject* object)
  (define-function int gtk_binding_set_activate (void* unsigned-int int void*))

  ;; void gtk_binding_set_add_path (GtkBindingSet* binding_set, GtkPathType path_type, const gchar* path_pattern, GtkPathPriorityType priority)
  (define-function void gtk_binding_set_add_path (void* int char* int))

  ;; GtkBindingSet* gtk_binding_set_by_class(gpointer object_class)
  (define-function void* gtk_binding_set_by_class (void*))

  ;; GtkBindingSet* gtk_binding_set_find (const gchar* set_name)
  (define-function void* gtk_binding_set_find (char*))

  ;; GtkBindingSet* gtk_binding_set_new (const gchar* set_name)
  (define-function void* gtk_binding_set_new (char*))

  ) ;[end]
