#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk target)

  (export gtk_target_flags_get_type
          gtk_target_list_add
          gtk_target_list_add_image_targets
          gtk_target_list_add_rich_text_targets
          gtk_target_list_add_table
          gtk_target_list_add_text_targets
          gtk_target_list_add_uri_targets
          gtk_target_list_find
          gtk_target_list_get_type
          gtk_target_list_new
          gtk_target_list_ref
          gtk_target_list_remove
          gtk_target_list_unref
          gtk_target_table_free
          gtk_target_table_new_from_list)

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

  ;; GType gtk_target_flags_get_type (void)
  (define-function unsigned-long gtk_target_flags_get_type ())

  ;; void gtk_target_list_add (GtkTargetList* list, GdkAtom target, guint flags, guint info)
  (define-function void gtk_target_list_add (void* void* unsigned-int unsigned-int))

  ;; void gtk_target_list_add_image_targets (GtkTargetList* list, guint info, gboolean writable)
  (define-function void gtk_target_list_add_image_targets (void* unsigned-int int))

  ;; void gtk_target_list_add_rich_text_targets (GtkTargetList* list, guint info, gboolean deserializable, GtkTextBuffer* buffer)
  (define-function void gtk_target_list_add_rich_text_targets (void* unsigned-int int void*))

  ;; void gtk_target_list_add_table (GtkTargetList* list, const GtkTargetEntry* targets, guint ntargets)
  (define-function void gtk_target_list_add_table (void* void* unsigned-int))

  ;; void gtk_target_list_add_text_targets (GtkTargetList* list, guint info)
  (define-function void gtk_target_list_add_text_targets (void* unsigned-int))

  ;; void gtk_target_list_add_uri_targets (GtkTargetList* list, guint info)
  (define-function void gtk_target_list_add_uri_targets (void* unsigned-int))

  ;; gboolean gtk_target_list_find (GtkTargetList* list, GdkAtom target, guint* info)
  (define-function int gtk_target_list_find (void* void* void*))

  ;; GType gtk_target_list_get_type (void)
  (define-function unsigned-long gtk_target_list_get_type ())

  ;; GtkTargetList* gtk_target_list_new (const GtkTargetEntry* targets, guint ntargets)
  (define-function void* gtk_target_list_new (void* unsigned-int))

  ;; GtkTargetList* gtk_target_list_ref (GtkTargetList* list)
  (define-function void* gtk_target_list_ref (void*))

  ;; void gtk_target_list_remove (GtkTargetList* list, GdkAtom target)
  (define-function void gtk_target_list_remove (void* void*))

  ;; void gtk_target_list_unref (GtkTargetList* list)
  (define-function void gtk_target_list_unref (void*))

  ;; void gtk_target_table_free (GtkTargetEntry* targets, gint n_targets)
  (define-function void gtk_target_table_free (void* int))

  ;; GtkTargetEntry* gtk_target_table_new_from_list (GtkTargetList* list, gint* n_targets)
  (define-function void* gtk_target_table_new_from_list (void* void*))

  ) ;[end]
