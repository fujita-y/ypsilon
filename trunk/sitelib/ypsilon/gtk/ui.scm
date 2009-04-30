#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk ui)

  (export gtk_ui_manager_add_ui
          gtk_ui_manager_add_ui_from_file
          gtk_ui_manager_add_ui_from_string
          gtk_ui_manager_ensure_update
          gtk_ui_manager_get_accel_group
          gtk_ui_manager_get_action
          gtk_ui_manager_get_action_groups
          gtk_ui_manager_get_add_tearoffs
          gtk_ui_manager_get_toplevels
          gtk_ui_manager_get_type
          gtk_ui_manager_get_ui
          gtk_ui_manager_get_widget
          gtk_ui_manager_insert_action_group
          gtk_ui_manager_item_type_get_type
          gtk_ui_manager_new
          gtk_ui_manager_new_merge_id
          gtk_ui_manager_remove_action_group
          gtk_ui_manager_remove_ui
          gtk_ui_manager_set_add_tearoffs)

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

  ;; void gtk_ui_manager_add_ui (GtkUIManager* self, guint merge_id, const gchar* path, const gchar* name, const gchar* action, GtkUIManagerItemType type, gboolean top)
  (define-function void gtk_ui_manager_add_ui (void* unsigned-int char* char* char* int int))

  ;; guint gtk_ui_manager_add_ui_from_file (GtkUIManager* self, const gchar* filename, GError** error)
  (define-function unsigned-int gtk_ui_manager_add_ui_from_file (void* char* void*))

  ;; guint gtk_ui_manager_add_ui_from_string (GtkUIManager* self, const gchar* buffer, gssize length, GError** error)
  (define-function unsigned-int gtk_ui_manager_add_ui_from_string (void* char* long void*))

  ;; void gtk_ui_manager_ensure_update (GtkUIManager* self)
  (define-function void gtk_ui_manager_ensure_update (void*))

  ;; GtkAccelGroup* gtk_ui_manager_get_accel_group (GtkUIManager* self)
  (define-function void* gtk_ui_manager_get_accel_group (void*))

  ;; GtkAction* gtk_ui_manager_get_action (GtkUIManager* self, const gchar* path)
  (define-function void* gtk_ui_manager_get_action (void* char*))

  ;; GList* gtk_ui_manager_get_action_groups (GtkUIManager* self)
  (define-function void* gtk_ui_manager_get_action_groups (void*))

  ;; gboolean gtk_ui_manager_get_add_tearoffs (GtkUIManager* self)
  (define-function int gtk_ui_manager_get_add_tearoffs (void*))

  ;; GSList* gtk_ui_manager_get_toplevels (GtkUIManager* self, GtkUIManagerItemType types)
  (define-function void* gtk_ui_manager_get_toplevels (void* int))

  ;; GType gtk_ui_manager_get_type (void)
  (define-function unsigned-long gtk_ui_manager_get_type ())

  ;; gchar* gtk_ui_manager_get_ui (GtkUIManager* self)
  (define-function char* gtk_ui_manager_get_ui (void*))

  ;; GtkWidget* gtk_ui_manager_get_widget (GtkUIManager* self, const gchar* path)
  (define-function void* gtk_ui_manager_get_widget (void* char*))

  ;; void gtk_ui_manager_insert_action_group (GtkUIManager* self, GtkActionGroup* action_group, gint pos)
  (define-function void gtk_ui_manager_insert_action_group (void* void* int))

  ;; GType gtk_ui_manager_item_type_get_type (void)
  (define-function unsigned-long gtk_ui_manager_item_type_get_type ())

  ;; GtkUIManager* gtk_ui_manager_new (void)
  (define-function void* gtk_ui_manager_new ())

  ;; guint gtk_ui_manager_new_merge_id (GtkUIManager* self)
  (define-function unsigned-int gtk_ui_manager_new_merge_id (void*))

  ;; void gtk_ui_manager_remove_action_group (GtkUIManager* self, GtkActionGroup* action_group)
  (define-function void gtk_ui_manager_remove_action_group (void* void*))

  ;; void gtk_ui_manager_remove_ui (GtkUIManager* self, guint merge_id)
  (define-function void gtk_ui_manager_remove_ui (void* unsigned-int))

  ;; void gtk_ui_manager_set_add_tearoffs (GtkUIManager* self, gboolean add_tearoffs)
  (define-function void gtk_ui_manager_set_add_tearoffs (void* int))

  ) ;[end]
