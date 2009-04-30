#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk recent)

  (export gtk_recent_action_get_show_numbers
          gtk_recent_action_get_type
          gtk_recent_action_new
          gtk_recent_action_new_for_manager
          gtk_recent_action_set_show_numbers
          gtk_recent_chooser_add_filter
          gtk_recent_chooser_dialog_get_type
          gtk_recent_chooser_dialog_new
          gtk_recent_chooser_dialog_new_for_manager
          gtk_recent_chooser_error_get_type
          gtk_recent_chooser_error_quark
          gtk_recent_chooser_get_current_item
          gtk_recent_chooser_get_current_uri
          gtk_recent_chooser_get_filter
          gtk_recent_chooser_get_items
          gtk_recent_chooser_get_limit
          gtk_recent_chooser_get_local_only
          gtk_recent_chooser_get_select_multiple
          gtk_recent_chooser_get_show_icons
          gtk_recent_chooser_get_show_not_found
          gtk_recent_chooser_get_show_private
          gtk_recent_chooser_get_show_tips
          gtk_recent_chooser_get_sort_type
          gtk_recent_chooser_get_type
          gtk_recent_chooser_get_uris
          gtk_recent_chooser_list_filters
          gtk_recent_chooser_menu_get_show_numbers
          gtk_recent_chooser_menu_get_type
          gtk_recent_chooser_menu_new
          gtk_recent_chooser_menu_new_for_manager
          gtk_recent_chooser_menu_set_show_numbers
          gtk_recent_chooser_remove_filter
          gtk_recent_chooser_select_all
          gtk_recent_chooser_select_uri
          gtk_recent_chooser_set_current_uri
          gtk_recent_chooser_set_filter
          gtk_recent_chooser_set_limit
          gtk_recent_chooser_set_local_only
          gtk_recent_chooser_set_select_multiple
          gtk_recent_chooser_set_show_icons
          gtk_recent_chooser_set_show_not_found
          gtk_recent_chooser_set_show_private
          gtk_recent_chooser_set_show_tips
          gtk_recent_chooser_set_sort_func
          gtk_recent_chooser_set_sort_type
          gtk_recent_chooser_unselect_all
          gtk_recent_chooser_unselect_uri
          gtk_recent_chooser_widget_get_type
          gtk_recent_chooser_widget_new
          gtk_recent_chooser_widget_new_for_manager
          gtk_recent_filter_add_age
          gtk_recent_filter_add_application
          gtk_recent_filter_add_custom
          gtk_recent_filter_add_group
          gtk_recent_filter_add_mime_type
          gtk_recent_filter_add_pattern
          gtk_recent_filter_add_pixbuf_formats
          gtk_recent_filter_filter
          gtk_recent_filter_flags_get_type
          gtk_recent_filter_get_name
          gtk_recent_filter_get_needed
          gtk_recent_filter_get_type
          gtk_recent_filter_new
          gtk_recent_filter_set_name
          gtk_recent_info_exists
          gtk_recent_info_get_added
          gtk_recent_info_get_age
          gtk_recent_info_get_application_info
          gtk_recent_info_get_applications
          gtk_recent_info_get_description
          gtk_recent_info_get_display_name
          gtk_recent_info_get_groups
          gtk_recent_info_get_icon
          gtk_recent_info_get_mime_type
          gtk_recent_info_get_modified
          gtk_recent_info_get_private_hint
          gtk_recent_info_get_short_name
          gtk_recent_info_get_type
          gtk_recent_info_get_uri
          gtk_recent_info_get_uri_display
          gtk_recent_info_get_visited
          gtk_recent_info_has_application
          gtk_recent_info_has_group
          gtk_recent_info_is_local
          gtk_recent_info_last_application
          gtk_recent_info_match
          gtk_recent_info_ref
          gtk_recent_info_unref
          gtk_recent_manager_add_full
          gtk_recent_manager_add_item
          gtk_recent_manager_error_get_type
          gtk_recent_manager_error_quark
          gtk_recent_manager_get_default
          gtk_recent_manager_get_items
          gtk_recent_manager_get_limit
          gtk_recent_manager_get_type
          gtk_recent_manager_has_item
          gtk_recent_manager_lookup_item
          gtk_recent_manager_move_item
          gtk_recent_manager_new
          gtk_recent_manager_purge_items
          gtk_recent_manager_remove_item
          gtk_recent_manager_set_limit
          gtk_recent_sort_type_get_type)

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

  ;; gboolean gtk_recent_action_get_show_numbers (GtkRecentAction* action)
  (define-function int gtk_recent_action_get_show_numbers (void*))

  ;; GType gtk_recent_action_get_type (void)
  (define-function unsigned-long gtk_recent_action_get_type ())

  ;; GtkAction* gtk_recent_action_new (const gchar* name, const gchar* label, const gchar* tooltip, const gchar* stock_id)
  (define-function void* gtk_recent_action_new (char* char* char* char*))

  ;; GtkAction* gtk_recent_action_new_for_manager (const gchar* name, const gchar* label, const gchar* tooltip, const gchar* stock_id, GtkRecentManager* manager)
  (define-function void* gtk_recent_action_new_for_manager (char* char* char* char* void*))

  ;; void gtk_recent_action_set_show_numbers (GtkRecentAction* action, gboolean show_numbers)
  (define-function void gtk_recent_action_set_show_numbers (void* int))

  ;; void gtk_recent_chooser_add_filter (GtkRecentChooser* chooser, GtkRecentFilter* filter)
  (define-function void gtk_recent_chooser_add_filter (void* void*))

  ;; GType gtk_recent_chooser_dialog_get_type (void)
  (define-function unsigned-long gtk_recent_chooser_dialog_get_type ())

  ;; GtkWidget* gtk_recent_chooser_dialog_new (const gchar* title, GtkWindow* parent, const gchar* first_button_text, ...)
  (define-function void* gtk_recent_chooser_dialog_new (char* void* char* ...))

  ;; GtkWidget* gtk_recent_chooser_dialog_new_for_manager (const gchar* title, GtkWindow* parent, GtkRecentManager* manager, const gchar* first_button_text, ...)
  (define-function void* gtk_recent_chooser_dialog_new_for_manager (char* void* void* char* ...))

  ;; GType gtk_recent_chooser_error_get_type (void)
  (define-function unsigned-long gtk_recent_chooser_error_get_type ())

  ;; GQuark gtk_recent_chooser_error_quark (void)
  (define-function uint32_t gtk_recent_chooser_error_quark ())

  ;; GtkRecentInfo* gtk_recent_chooser_get_current_item (GtkRecentChooser* chooser)
  (define-function void* gtk_recent_chooser_get_current_item (void*))

  ;; gchar* gtk_recent_chooser_get_current_uri (GtkRecentChooser* chooser)
  (define-function char* gtk_recent_chooser_get_current_uri (void*))

  ;; GtkRecentFilter* gtk_recent_chooser_get_filter (GtkRecentChooser* chooser)
  (define-function void* gtk_recent_chooser_get_filter (void*))

  ;; GList* gtk_recent_chooser_get_items (GtkRecentChooser* chooser)
  (define-function void* gtk_recent_chooser_get_items (void*))

  ;; gint gtk_recent_chooser_get_limit (GtkRecentChooser* chooser)
  (define-function int gtk_recent_chooser_get_limit (void*))

  ;; gboolean gtk_recent_chooser_get_local_only (GtkRecentChooser* chooser)
  (define-function int gtk_recent_chooser_get_local_only (void*))

  ;; gboolean gtk_recent_chooser_get_select_multiple (GtkRecentChooser* chooser)
  (define-function int gtk_recent_chooser_get_select_multiple (void*))

  ;; gboolean gtk_recent_chooser_get_show_icons (GtkRecentChooser* chooser)
  (define-function int gtk_recent_chooser_get_show_icons (void*))

  ;; gboolean gtk_recent_chooser_get_show_not_found (GtkRecentChooser* chooser)
  (define-function int gtk_recent_chooser_get_show_not_found (void*))

  ;; gboolean gtk_recent_chooser_get_show_private (GtkRecentChooser* chooser)
  (define-function int gtk_recent_chooser_get_show_private (void*))

  ;; gboolean gtk_recent_chooser_get_show_tips (GtkRecentChooser* chooser)
  (define-function int gtk_recent_chooser_get_show_tips (void*))

  ;; GtkRecentSortType gtk_recent_chooser_get_sort_type (GtkRecentChooser* chooser)
  (define-function int gtk_recent_chooser_get_sort_type (void*))

  ;; GType gtk_recent_chooser_get_type (void)
  (define-function unsigned-long gtk_recent_chooser_get_type ())

  ;; gchar** gtk_recent_chooser_get_uris (GtkRecentChooser* chooser, gsize* length)
  (define-function void* gtk_recent_chooser_get_uris (void* void*))

  ;; GSList* gtk_recent_chooser_list_filters (GtkRecentChooser* chooser)
  (define-function void* gtk_recent_chooser_list_filters (void*))

  ;; gboolean gtk_recent_chooser_menu_get_show_numbers (GtkRecentChooserMenu* menu)
  (define-function int gtk_recent_chooser_menu_get_show_numbers (void*))

  ;; GType gtk_recent_chooser_menu_get_type (void)
  (define-function unsigned-long gtk_recent_chooser_menu_get_type ())

  ;; GtkWidget* gtk_recent_chooser_menu_new (void)
  (define-function void* gtk_recent_chooser_menu_new ())

  ;; GtkWidget* gtk_recent_chooser_menu_new_for_manager (GtkRecentManager* manager)
  (define-function void* gtk_recent_chooser_menu_new_for_manager (void*))

  ;; void gtk_recent_chooser_menu_set_show_numbers (GtkRecentChooserMenu* menu, gboolean show_numbers)
  (define-function void gtk_recent_chooser_menu_set_show_numbers (void* int))

  ;; void gtk_recent_chooser_remove_filter (GtkRecentChooser* chooser, GtkRecentFilter* filter)
  (define-function void gtk_recent_chooser_remove_filter (void* void*))

  ;; void gtk_recent_chooser_select_all (GtkRecentChooser* chooser)
  (define-function void gtk_recent_chooser_select_all (void*))

  ;; gboolean gtk_recent_chooser_select_uri (GtkRecentChooser* chooser, const gchar* uri, GError** error)
  (define-function int gtk_recent_chooser_select_uri (void* char* void*))

  ;; gboolean gtk_recent_chooser_set_current_uri (GtkRecentChooser* chooser, const gchar* uri, GError** error)
  (define-function int gtk_recent_chooser_set_current_uri (void* char* void*))

  ;; void gtk_recent_chooser_set_filter (GtkRecentChooser* chooser, GtkRecentFilter* filter)
  (define-function void gtk_recent_chooser_set_filter (void* void*))

  ;; void gtk_recent_chooser_set_limit (GtkRecentChooser* chooser, gint limit)
  (define-function void gtk_recent_chooser_set_limit (void* int))

  ;; void gtk_recent_chooser_set_local_only (GtkRecentChooser* chooser, gboolean local_only)
  (define-function void gtk_recent_chooser_set_local_only (void* int))

  ;; void gtk_recent_chooser_set_select_multiple (GtkRecentChooser* chooser, gboolean select_multiple)
  (define-function void gtk_recent_chooser_set_select_multiple (void* int))

  ;; void gtk_recent_chooser_set_show_icons (GtkRecentChooser* chooser, gboolean show_icons)
  (define-function void gtk_recent_chooser_set_show_icons (void* int))

  ;; void gtk_recent_chooser_set_show_not_found (GtkRecentChooser* chooser, gboolean show_not_found)
  (define-function void gtk_recent_chooser_set_show_not_found (void* int))

  ;; void gtk_recent_chooser_set_show_private (GtkRecentChooser* chooser, gboolean show_private)
  (define-function void gtk_recent_chooser_set_show_private (void* int))

  ;; void gtk_recent_chooser_set_show_tips (GtkRecentChooser* chooser, gboolean show_tips)
  (define-function void gtk_recent_chooser_set_show_tips (void* int))

  ;; void gtk_recent_chooser_set_sort_func (GtkRecentChooser* chooser, GtkRecentSortFunc sort_func, gpointer sort_data, GDestroyNotify data_destroy)
  (define-function void gtk_recent_chooser_set_sort_func (void* (c-callback int (void* void* void*)) void* (c-callback void (void*))))

  ;; void gtk_recent_chooser_set_sort_type (GtkRecentChooser* chooser, GtkRecentSortType sort_type)
  (define-function void gtk_recent_chooser_set_sort_type (void* int))

  ;; void gtk_recent_chooser_unselect_all (GtkRecentChooser* chooser)
  (define-function void gtk_recent_chooser_unselect_all (void*))

  ;; void gtk_recent_chooser_unselect_uri (GtkRecentChooser* chooser, const gchar* uri)
  (define-function void gtk_recent_chooser_unselect_uri (void* char*))

  ;; GType gtk_recent_chooser_widget_get_type (void)
  (define-function unsigned-long gtk_recent_chooser_widget_get_type ())

  ;; GtkWidget* gtk_recent_chooser_widget_new (void)
  (define-function void* gtk_recent_chooser_widget_new ())

  ;; GtkWidget* gtk_recent_chooser_widget_new_for_manager (GtkRecentManager* manager)
  (define-function void* gtk_recent_chooser_widget_new_for_manager (void*))

  ;; void gtk_recent_filter_add_age (GtkRecentFilter* filter, gint days)
  (define-function void gtk_recent_filter_add_age (void* int))

  ;; void gtk_recent_filter_add_application (GtkRecentFilter* filter, const gchar* application)
  (define-function void gtk_recent_filter_add_application (void* char*))

  ;; void gtk_recent_filter_add_custom (GtkRecentFilter* filter, GtkRecentFilterFlags needed, GtkRecentFilterFunc func, gpointer data, GDestroyNotify data_destroy)
  (define-function void gtk_recent_filter_add_custom (void* int (c-callback int (void* void*)) void* (c-callback void (void*))))

  ;; void gtk_recent_filter_add_group (GtkRecentFilter* filter, const gchar* group)
  (define-function void gtk_recent_filter_add_group (void* char*))

  ;; void gtk_recent_filter_add_mime_type (GtkRecentFilter* filter, const gchar* mime_type)
  (define-function void gtk_recent_filter_add_mime_type (void* char*))

  ;; void gtk_recent_filter_add_pattern (GtkRecentFilter* filter, const gchar* pattern)
  (define-function void gtk_recent_filter_add_pattern (void* char*))

  ;; void gtk_recent_filter_add_pixbuf_formats (GtkRecentFilter* filter)
  (define-function void gtk_recent_filter_add_pixbuf_formats (void*))

  ;; gboolean gtk_recent_filter_filter (GtkRecentFilter* filter, const GtkRecentFilterInfo* filter_info)
  (define-function int gtk_recent_filter_filter (void* void*))

  ;; GType gtk_recent_filter_flags_get_type (void)
  (define-function unsigned-long gtk_recent_filter_flags_get_type ())

  ;; const gchar* gtk_recent_filter_get_name (GtkRecentFilter* filter)
  (define-function char* gtk_recent_filter_get_name (void*))

  ;; GtkRecentFilterFlags gtk_recent_filter_get_needed (GtkRecentFilter* filter)
  (define-function int gtk_recent_filter_get_needed (void*))

  ;; GType gtk_recent_filter_get_type (void)
  (define-function unsigned-long gtk_recent_filter_get_type ())

  ;; GtkRecentFilter* gtk_recent_filter_new (void)
  (define-function void* gtk_recent_filter_new ())

  ;; void gtk_recent_filter_set_name (GtkRecentFilter* filter, const gchar* name)
  (define-function void gtk_recent_filter_set_name (void* char*))

  ;; gboolean gtk_recent_info_exists (GtkRecentInfo* info)
  (define-function int gtk_recent_info_exists (void*))

  ;; time_t gtk_recent_info_get_added (GtkRecentInfo* info)
  (define-function long gtk_recent_info_get_added (void*))

  ;; gint gtk_recent_info_get_age (GtkRecentInfo* info)
  (define-function int gtk_recent_info_get_age (void*))

  ;; gboolean gtk_recent_info_get_application_info (GtkRecentInfo* info, const gchar* app_name, gchar** app_exec, guint* count, time_t* time_)
  (define-function int gtk_recent_info_get_application_info (void* char* void* void* void*))

  ;; gchar** gtk_recent_info_get_applications (GtkRecentInfo* info, gsize* length)
  (define-function void* gtk_recent_info_get_applications (void* void*))

  ;; const gchar* gtk_recent_info_get_description (GtkRecentInfo* info)
  (define-function char* gtk_recent_info_get_description (void*))

  ;; const gchar* gtk_recent_info_get_display_name (GtkRecentInfo* info)
  (define-function char* gtk_recent_info_get_display_name (void*))

  ;; gchar** gtk_recent_info_get_groups (GtkRecentInfo* info, gsize* length)
  (define-function void* gtk_recent_info_get_groups (void* void*))

  ;; GdkPixbuf* gtk_recent_info_get_icon (GtkRecentInfo* info, gint size)
  (define-function void* gtk_recent_info_get_icon (void* int))

  ;; const gchar* gtk_recent_info_get_mime_type (GtkRecentInfo* info)
  (define-function char* gtk_recent_info_get_mime_type (void*))

  ;; time_t gtk_recent_info_get_modified (GtkRecentInfo* info)
  (define-function long gtk_recent_info_get_modified (void*))

  ;; gboolean gtk_recent_info_get_private_hint (GtkRecentInfo* info)
  (define-function int gtk_recent_info_get_private_hint (void*))

  ;; gchar* gtk_recent_info_get_short_name (GtkRecentInfo* info)
  (define-function char* gtk_recent_info_get_short_name (void*))

  ;; GType gtk_recent_info_get_type (void)
  (define-function unsigned-long gtk_recent_info_get_type ())

  ;; const gchar* gtk_recent_info_get_uri (GtkRecentInfo* info)
  (define-function char* gtk_recent_info_get_uri (void*))

  ;; gchar* gtk_recent_info_get_uri_display (GtkRecentInfo* info)
  (define-function char* gtk_recent_info_get_uri_display (void*))

  ;; time_t gtk_recent_info_get_visited (GtkRecentInfo* info)
  (define-function long gtk_recent_info_get_visited (void*))

  ;; gboolean gtk_recent_info_has_application (GtkRecentInfo* info, const gchar* app_name)
  (define-function int gtk_recent_info_has_application (void* char*))

  ;; gboolean gtk_recent_info_has_group (GtkRecentInfo* info, const gchar* group_name)
  (define-function int gtk_recent_info_has_group (void* char*))

  ;; gboolean gtk_recent_info_is_local (GtkRecentInfo* info)
  (define-function int gtk_recent_info_is_local (void*))

  ;; gchar* gtk_recent_info_last_application (GtkRecentInfo* info)
  (define-function char* gtk_recent_info_last_application (void*))

  ;; gboolean gtk_recent_info_match (GtkRecentInfo* info_a, GtkRecentInfo* info_b)
  (define-function int gtk_recent_info_match (void* void*))

  ;; GtkRecentInfo* gtk_recent_info_ref (GtkRecentInfo* info)
  (define-function void* gtk_recent_info_ref (void*))

  ;; void gtk_recent_info_unref (GtkRecentInfo* info)
  (define-function void gtk_recent_info_unref (void*))

  ;; gboolean gtk_recent_manager_add_full (GtkRecentManager* manager, const gchar* uri, const GtkRecentData* recent_data)
  (define-function int gtk_recent_manager_add_full (void* char* void*))

  ;; gboolean gtk_recent_manager_add_item (GtkRecentManager* manager, const gchar* uri)
  (define-function int gtk_recent_manager_add_item (void* char*))

  ;; GType gtk_recent_manager_error_get_type (void)
  (define-function unsigned-long gtk_recent_manager_error_get_type ())

  ;; GQuark gtk_recent_manager_error_quark (void)
  (define-function uint32_t gtk_recent_manager_error_quark ())

  ;; GtkRecentManager* gtk_recent_manager_get_default (void)
  (define-function void* gtk_recent_manager_get_default ())

  ;; GList* gtk_recent_manager_get_items (GtkRecentManager* manager)
  (define-function void* gtk_recent_manager_get_items (void*))

  ;; gint gtk_recent_manager_get_limit (GtkRecentManager* manager)
  (define-function int gtk_recent_manager_get_limit (void*))

  ;; GType gtk_recent_manager_get_type (void)
  (define-function unsigned-long gtk_recent_manager_get_type ())

  ;; gboolean gtk_recent_manager_has_item (GtkRecentManager* manager, const gchar* uri)
  (define-function int gtk_recent_manager_has_item (void* char*))

  ;; GtkRecentInfo* gtk_recent_manager_lookup_item (GtkRecentManager* manager, const gchar* uri, GError** error)
  (define-function void* gtk_recent_manager_lookup_item (void* char* void*))

  ;; gboolean gtk_recent_manager_move_item (GtkRecentManager* manager, const gchar* uri, const gchar* new_uri, GError** error)
  (define-function int gtk_recent_manager_move_item (void* char* char* void*))

  ;; GtkRecentManager* gtk_recent_manager_new (void)
  (define-function void* gtk_recent_manager_new ())

  ;; gint gtk_recent_manager_purge_items (GtkRecentManager* manager, GError** error)
  (define-function int gtk_recent_manager_purge_items (void* void*))

  ;; gboolean gtk_recent_manager_remove_item (GtkRecentManager* manager, const gchar* uri, GError** error)
  (define-function int gtk_recent_manager_remove_item (void* char* void*))

  ;; void gtk_recent_manager_set_limit (GtkRecentManager* manager, gint limit)
  (define-function void gtk_recent_manager_set_limit (void* int))

  ;; GType gtk_recent_sort_type_get_type (void)
  (define-function unsigned-long gtk_recent_sort_type_get_type ())

  ) ;[end]
