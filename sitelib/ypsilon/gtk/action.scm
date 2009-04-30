#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk action)

  (export gtk_action_activate
          gtk_action_block_activate
          gtk_action_connect_accelerator
          gtk_action_create_icon
          gtk_action_create_menu
          gtk_action_create_menu_item
          gtk_action_create_tool_item
          gtk_action_disconnect_accelerator
          gtk_action_get_accel_closure
          gtk_action_get_accel_path
          gtk_action_get_gicon
          gtk_action_get_icon_name
          gtk_action_get_is_important
          gtk_action_get_label
          gtk_action_get_name
          gtk_action_get_proxies
          gtk_action_get_sensitive
          gtk_action_get_short_label
          gtk_action_get_stock_id
          gtk_action_get_tooltip
          gtk_action_get_type
          gtk_action_get_visible
          gtk_action_get_visible_horizontal
          gtk_action_get_visible_vertical
          gtk_action_group_add_action
          gtk_action_group_add_action_with_accel
          gtk_action_group_add_actions
          gtk_action_group_add_actions_full
          gtk_action_group_add_radio_actions
          gtk_action_group_add_radio_actions_full
          gtk_action_group_add_toggle_actions
          gtk_action_group_add_toggle_actions_full
          gtk_action_group_get_action
          gtk_action_group_get_name
          gtk_action_group_get_sensitive
          gtk_action_group_get_type
          gtk_action_group_get_visible
          gtk_action_group_list_actions
          gtk_action_group_new
          gtk_action_group_remove_action
          gtk_action_group_set_sensitive
          gtk_action_group_set_translate_func
          gtk_action_group_set_translation_domain
          gtk_action_group_set_visible
          gtk_action_group_translate_string
          gtk_action_is_sensitive
          gtk_action_is_visible
          gtk_action_new
          gtk_action_set_accel_group
          gtk_action_set_accel_path
          gtk_action_set_gicon
          gtk_action_set_icon_name
          gtk_action_set_is_important
          gtk_action_set_label
          gtk_action_set_sensitive
          gtk_action_set_short_label
          gtk_action_set_stock_id
          gtk_action_set_tooltip
          gtk_action_set_visible
          gtk_action_set_visible_horizontal
          gtk_action_set_visible_vertical
          gtk_action_unblock_activate)

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

  ;; void gtk_action_activate (GtkAction* action)
  (define-function void gtk_action_activate (void*))

  ;; void gtk_action_block_activate (GtkAction* action)
  (define-function void gtk_action_block_activate (void*))

  ;; void gtk_action_connect_accelerator (GtkAction* action)
  (define-function void gtk_action_connect_accelerator (void*))

  ;; GtkWidget* gtk_action_create_icon (GtkAction* action, GtkIconSize icon_size)
  (define-function void* gtk_action_create_icon (void* int))

  ;; GtkWidget* gtk_action_create_menu (GtkAction* action)
  (define-function void* gtk_action_create_menu (void*))

  ;; GtkWidget* gtk_action_create_menu_item (GtkAction* action)
  (define-function void* gtk_action_create_menu_item (void*))

  ;; GtkWidget* gtk_action_create_tool_item (GtkAction* action)
  (define-function void* gtk_action_create_tool_item (void*))

  ;; void gtk_action_disconnect_accelerator (GtkAction* action)
  (define-function void gtk_action_disconnect_accelerator (void*))

  ;; GClosure* gtk_action_get_accel_closure (GtkAction* action)
  (define-function void* gtk_action_get_accel_closure (void*))

  ;; const gchar* gtk_action_get_accel_path (GtkAction* action)
  (define-function char* gtk_action_get_accel_path (void*))

  ;; GIcon* gtk_action_get_gicon (GtkAction* action)
  (define-function void* gtk_action_get_gicon (void*))

  ;; const gchar* gtk_action_get_icon_name (GtkAction* action)
  (define-function char* gtk_action_get_icon_name (void*))

  ;; gboolean gtk_action_get_is_important (GtkAction* action)
  (define-function int gtk_action_get_is_important (void*))

  ;; const gchar* gtk_action_get_label (GtkAction* action)
  (define-function char* gtk_action_get_label (void*))

  ;; const gchar* gtk_action_get_name (GtkAction* action)
  (define-function char* gtk_action_get_name (void*))

  ;; GSList* gtk_action_get_proxies (GtkAction* action)
  (define-function void* gtk_action_get_proxies (void*))

  ;; gboolean gtk_action_get_sensitive (GtkAction* action)
  (define-function int gtk_action_get_sensitive (void*))

  ;; const gchar* gtk_action_get_short_label (GtkAction* action)
  (define-function char* gtk_action_get_short_label (void*))

  ;; const gchar* gtk_action_get_stock_id (GtkAction* action)
  (define-function char* gtk_action_get_stock_id (void*))

  ;; const gchar* gtk_action_get_tooltip (GtkAction* action)
  (define-function char* gtk_action_get_tooltip (void*))

  ;; GType gtk_action_get_type (void)
  (define-function unsigned-long gtk_action_get_type ())

  ;; gboolean gtk_action_get_visible (GtkAction* action)
  (define-function int gtk_action_get_visible (void*))

  ;; gboolean gtk_action_get_visible_horizontal (GtkAction* action)
  (define-function int gtk_action_get_visible_horizontal (void*))

  ;; gboolean gtk_action_get_visible_vertical (GtkAction* action)
  (define-function int gtk_action_get_visible_vertical (void*))

  ;; void gtk_action_group_add_action (GtkActionGroup* action_group, GtkAction* action)
  (define-function void gtk_action_group_add_action (void* void*))

  ;; void gtk_action_group_add_action_with_accel (GtkActionGroup* action_group, GtkAction* action, const gchar* accelerator)
  (define-function void gtk_action_group_add_action_with_accel (void* void* char*))

  ;; void gtk_action_group_add_actions (GtkActionGroup* action_group, const GtkActionEntry* entries, guint n_entries, gpointer user_data)
  (define-function void gtk_action_group_add_actions (void* void* unsigned-int void*))

  ;; void gtk_action_group_add_actions_full (GtkActionGroup* action_group, const GtkActionEntry* entries, guint n_entries, gpointer user_data, GDestroyNotify destroy)
  (define-function void gtk_action_group_add_actions_full (void* void* unsigned-int void* (c-callback void (void*))))

  ;; void gtk_action_group_add_radio_actions (GtkActionGroup* action_group, const GtkRadioActionEntry* entries, guint n_entries, gint value, GCallback on_change, gpointer user_data)
  (define-function void gtk_action_group_add_radio_actions (void* void* unsigned-int int (c-callback void ()) void*))

  ;; void gtk_action_group_add_radio_actions_full (GtkActionGroup* action_group, const GtkRadioActionEntry* entries, guint n_entries, gint value, GCallback on_change, gpointer user_data, GDestroyNotify destroy)
  (define-function void gtk_action_group_add_radio_actions_full (void* void* unsigned-int int (c-callback void ()) void* (c-callback void (void*))))

  ;; void gtk_action_group_add_toggle_actions (GtkActionGroup* action_group, const GtkToggleActionEntry* entries, guint n_entries, gpointer user_data)
  (define-function void gtk_action_group_add_toggle_actions (void* void* unsigned-int void*))

  ;; void gtk_action_group_add_toggle_actions_full (GtkActionGroup* action_group, const GtkToggleActionEntry* entries, guint n_entries, gpointer user_data, GDestroyNotify destroy)
  (define-function void gtk_action_group_add_toggle_actions_full (void* void* unsigned-int void* (c-callback void (void*))))

  ;; GtkAction* gtk_action_group_get_action (GtkActionGroup* action_group, const gchar* action_name)
  (define-function void* gtk_action_group_get_action (void* char*))

  ;; const gchar* gtk_action_group_get_name (GtkActionGroup* action_group)
  (define-function char* gtk_action_group_get_name (void*))

  ;; gboolean gtk_action_group_get_sensitive (GtkActionGroup* action_group)
  (define-function int gtk_action_group_get_sensitive (void*))

  ;; GType gtk_action_group_get_type (void)
  (define-function unsigned-long gtk_action_group_get_type ())

  ;; gboolean gtk_action_group_get_visible (GtkActionGroup* action_group)
  (define-function int gtk_action_group_get_visible (void*))

  ;; GList* gtk_action_group_list_actions (GtkActionGroup* action_group)
  (define-function void* gtk_action_group_list_actions (void*))

  ;; GtkActionGroup* gtk_action_group_new (const gchar* name)
  (define-function void* gtk_action_group_new (char*))

  ;; void gtk_action_group_remove_action (GtkActionGroup* action_group, GtkAction* action)
  (define-function void gtk_action_group_remove_action (void* void*))

  ;; void gtk_action_group_set_sensitive (GtkActionGroup* action_group, gboolean sensitive)
  (define-function void gtk_action_group_set_sensitive (void* int))

  ;; void gtk_action_group_set_translate_func (GtkActionGroup* action_group, GtkTranslateFunc func, gpointer data, GDestroyNotify notify)
  (define-function void gtk_action_group_set_translate_func (void* (c-callback void* (void* void*)) void* (c-callback void (void*))))

  ;; void gtk_action_group_set_translation_domain (GtkActionGroup* action_group, const gchar* domain)
  (define-function void gtk_action_group_set_translation_domain (void* char*))

  ;; void gtk_action_group_set_visible (GtkActionGroup* action_group, gboolean visible)
  (define-function void gtk_action_group_set_visible (void* int))

  ;; const gchar* gtk_action_group_translate_string (GtkActionGroup* action_group, const gchar* string)
  (define-function char* gtk_action_group_translate_string (void* char*))

  ;; gboolean gtk_action_is_sensitive (GtkAction* action)
  (define-function int gtk_action_is_sensitive (void*))

  ;; gboolean gtk_action_is_visible (GtkAction* action)
  (define-function int gtk_action_is_visible (void*))

  ;; GtkAction* gtk_action_new (const gchar* name, const gchar* label, const gchar* tooltip, const gchar* stock_id)
  (define-function void* gtk_action_new (char* char* char* char*))

  ;; void gtk_action_set_accel_group (GtkAction* action, GtkAccelGroup* accel_group)
  (define-function void gtk_action_set_accel_group (void* void*))

  ;; void gtk_action_set_accel_path (GtkAction* action, const gchar* accel_path)
  (define-function void gtk_action_set_accel_path (void* char*))

  ;; void gtk_action_set_gicon (GtkAction* action, GIcon* icon)
  (define-function void gtk_action_set_gicon (void* void*))

  ;; void gtk_action_set_icon_name (GtkAction* action, const gchar* icon_name)
  (define-function void gtk_action_set_icon_name (void* char*))

  ;; void gtk_action_set_is_important (GtkAction* action, gboolean is_important)
  (define-function void gtk_action_set_is_important (void* int))

  ;; void gtk_action_set_label (GtkAction* action, const gchar* label)
  (define-function void gtk_action_set_label (void* char*))

  ;; void gtk_action_set_sensitive (GtkAction* action, gboolean sensitive)
  (define-function void gtk_action_set_sensitive (void* int))

  ;; void gtk_action_set_short_label (GtkAction* action, const gchar* short_label)
  (define-function void gtk_action_set_short_label (void* char*))

  ;; void gtk_action_set_stock_id (GtkAction* action, const gchar* stock_id)
  (define-function void gtk_action_set_stock_id (void* char*))

  ;; void gtk_action_set_tooltip (GtkAction* action, const gchar* tooltip)
  (define-function void gtk_action_set_tooltip (void* char*))

  ;; void gtk_action_set_visible (GtkAction* action, gboolean visible)
  (define-function void gtk_action_set_visible (void* int))

  ;; void gtk_action_set_visible_horizontal (GtkAction* action, gboolean visible_horizontal)
  (define-function void gtk_action_set_visible_horizontal (void* int))

  ;; void gtk_action_set_visible_vertical (GtkAction* action, gboolean visible_vertical)
  (define-function void gtk_action_set_visible_vertical (void* int))

  ;; void gtk_action_unblock_activate (GtkAction* action)
  (define-function void gtk_action_unblock_activate (void*))

  ) ;[end]
