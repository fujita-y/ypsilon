#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk file)

  (export gtk_file_chooser_action_get_type
          gtk_file_chooser_add_filter
          gtk_file_chooser_add_shortcut_folder
          gtk_file_chooser_add_shortcut_folder_uri
          gtk_file_chooser_button_get_focus_on_click
          gtk_file_chooser_button_get_title
          gtk_file_chooser_button_get_type
          gtk_file_chooser_button_get_width_chars
          gtk_file_chooser_button_new
          gtk_file_chooser_button_new_with_dialog
          gtk_file_chooser_button_set_focus_on_click
          gtk_file_chooser_button_set_title
          gtk_file_chooser_button_set_width_chars
          gtk_file_chooser_confirmation_get_type
          gtk_file_chooser_dialog_get_type
          gtk_file_chooser_dialog_new
          gtk_file_chooser_error_get_type
          gtk_file_chooser_error_quark
          gtk_file_chooser_get_action
          gtk_file_chooser_get_current_folder
          gtk_file_chooser_get_current_folder_file
          gtk_file_chooser_get_current_folder_uri
          gtk_file_chooser_get_do_overwrite_confirmation
          gtk_file_chooser_get_extra_widget
          gtk_file_chooser_get_file
          gtk_file_chooser_get_filename
          gtk_file_chooser_get_filenames
          gtk_file_chooser_get_files
          gtk_file_chooser_get_filter
          gtk_file_chooser_get_local_only
          gtk_file_chooser_get_preview_file
          gtk_file_chooser_get_preview_filename
          gtk_file_chooser_get_preview_uri
          gtk_file_chooser_get_preview_widget
          gtk_file_chooser_get_preview_widget_active
          gtk_file_chooser_get_select_multiple
          gtk_file_chooser_get_show_hidden
          gtk_file_chooser_get_type
          gtk_file_chooser_get_uri
          gtk_file_chooser_get_uris
          gtk_file_chooser_get_use_preview_label
          gtk_file_chooser_list_filters
          gtk_file_chooser_list_shortcut_folder_uris
          gtk_file_chooser_list_shortcut_folders
          gtk_file_chooser_remove_filter
          gtk_file_chooser_remove_shortcut_folder
          gtk_file_chooser_remove_shortcut_folder_uri
          gtk_file_chooser_select_all
          gtk_file_chooser_select_file
          gtk_file_chooser_select_filename
          gtk_file_chooser_select_uri
          gtk_file_chooser_set_action
          gtk_file_chooser_set_current_folder
          gtk_file_chooser_set_current_folder_file
          gtk_file_chooser_set_current_folder_uri
          gtk_file_chooser_set_current_name
          gtk_file_chooser_set_do_overwrite_confirmation
          gtk_file_chooser_set_extra_widget
          gtk_file_chooser_set_file
          gtk_file_chooser_set_filename
          gtk_file_chooser_set_filter
          gtk_file_chooser_set_local_only
          gtk_file_chooser_set_preview_widget
          gtk_file_chooser_set_preview_widget_active
          gtk_file_chooser_set_select_multiple
          gtk_file_chooser_set_show_hidden
          gtk_file_chooser_set_uri
          gtk_file_chooser_set_use_preview_label
          gtk_file_chooser_unselect_all
          gtk_file_chooser_unselect_file
          gtk_file_chooser_unselect_filename
          gtk_file_chooser_unselect_uri
          gtk_file_chooser_widget_get_type
          gtk_file_chooser_widget_new
          gtk_file_filter_add_custom
          gtk_file_filter_add_mime_type
          gtk_file_filter_add_pattern
          gtk_file_filter_add_pixbuf_formats
          gtk_file_filter_filter
          gtk_file_filter_flags_get_type
          gtk_file_filter_get_name
          gtk_file_filter_get_needed
          gtk_file_filter_get_type
          gtk_file_filter_new
          gtk_file_filter_set_name)

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

  ;; GType gtk_file_chooser_action_get_type (void)
  (define-function unsigned-long gtk_file_chooser_action_get_type ())

  ;; void gtk_file_chooser_add_filter (GtkFileChooser* chooser, GtkFileFilter* filter)
  (define-function void gtk_file_chooser_add_filter (void* void*))

  ;; gboolean gtk_file_chooser_add_shortcut_folder (GtkFileChooser* chooser, const char* folder, GError** error)
  (define-function int gtk_file_chooser_add_shortcut_folder (void* char* void*))

  ;; gboolean gtk_file_chooser_add_shortcut_folder_uri (GtkFileChooser* chooser, const char* uri, GError** error)
  (define-function int gtk_file_chooser_add_shortcut_folder_uri (void* char* void*))

  ;; gboolean gtk_file_chooser_button_get_focus_on_click (GtkFileChooserButton* button)
  (define-function int gtk_file_chooser_button_get_focus_on_click (void*))

  ;; const gchar* gtk_file_chooser_button_get_title (GtkFileChooserButton* button)
  (define-function char* gtk_file_chooser_button_get_title (void*))

  ;; GType gtk_file_chooser_button_get_type (void)
  (define-function unsigned-long gtk_file_chooser_button_get_type ())

  ;; gint gtk_file_chooser_button_get_width_chars (GtkFileChooserButton* button)
  (define-function int gtk_file_chooser_button_get_width_chars (void*))

  ;; GtkWidget* gtk_file_chooser_button_new (const gchar* title, GtkFileChooserAction action)
  (define-function void* gtk_file_chooser_button_new (char* int))

  ;; GtkWidget* gtk_file_chooser_button_new_with_dialog (GtkWidget* dialog)
  (define-function void* gtk_file_chooser_button_new_with_dialog (void*))

  ;; void gtk_file_chooser_button_set_focus_on_click (GtkFileChooserButton* button, gboolean focus_on_click)
  (define-function void gtk_file_chooser_button_set_focus_on_click (void* int))

  ;; void gtk_file_chooser_button_set_title (GtkFileChooserButton* button, const gchar* title)
  (define-function void gtk_file_chooser_button_set_title (void* char*))

  ;; void gtk_file_chooser_button_set_width_chars (GtkFileChooserButton* button, gint n_chars)
  (define-function void gtk_file_chooser_button_set_width_chars (void* int))

  ;; GType gtk_file_chooser_confirmation_get_type (void)
  (define-function unsigned-long gtk_file_chooser_confirmation_get_type ())

  ;; GType gtk_file_chooser_dialog_get_type (void)
  (define-function unsigned-long gtk_file_chooser_dialog_get_type ())

  ;; GtkWidget* gtk_file_chooser_dialog_new (const gchar* title, GtkWindow* parent, GtkFileChooserAction action, const gchar* first_button_text, ...)
  (define-function void* gtk_file_chooser_dialog_new (char* void* int char* ...))

  ;; GType gtk_file_chooser_error_get_type (void)
  (define-function unsigned-long gtk_file_chooser_error_get_type ())

  ;; GQuark gtk_file_chooser_error_quark (void)
  (define-function uint32_t gtk_file_chooser_error_quark ())

  ;; GtkFileChooserAction gtk_file_chooser_get_action (GtkFileChooser* chooser)
  (define-function int gtk_file_chooser_get_action (void*))

  ;; gchar* gtk_file_chooser_get_current_folder (GtkFileChooser* chooser)
  (define-function char* gtk_file_chooser_get_current_folder (void*))

  ;; GFile* gtk_file_chooser_get_current_folder_file (GtkFileChooser* chooser)
  (define-function void* gtk_file_chooser_get_current_folder_file (void*))

  ;; gchar* gtk_file_chooser_get_current_folder_uri (GtkFileChooser* chooser)
  (define-function char* gtk_file_chooser_get_current_folder_uri (void*))

  ;; gboolean gtk_file_chooser_get_do_overwrite_confirmation (GtkFileChooser* chooser)
  (define-function int gtk_file_chooser_get_do_overwrite_confirmation (void*))

  ;; GtkWidget* gtk_file_chooser_get_extra_widget (GtkFileChooser* chooser)
  (define-function void* gtk_file_chooser_get_extra_widget (void*))

  ;; GFile* gtk_file_chooser_get_file (GtkFileChooser* chooser)
  (define-function void* gtk_file_chooser_get_file (void*))

  ;; gchar* gtk_file_chooser_get_filename (GtkFileChooser* chooser)
  (define-function char* gtk_file_chooser_get_filename (void*))

  ;; GSList* gtk_file_chooser_get_filenames (GtkFileChooser* chooser)
  (define-function void* gtk_file_chooser_get_filenames (void*))

  ;; GSList* gtk_file_chooser_get_files (GtkFileChooser* chooser)
  (define-function void* gtk_file_chooser_get_files (void*))

  ;; GtkFileFilter* gtk_file_chooser_get_filter (GtkFileChooser* chooser)
  (define-function void* gtk_file_chooser_get_filter (void*))

  ;; gboolean gtk_file_chooser_get_local_only (GtkFileChooser* chooser)
  (define-function int gtk_file_chooser_get_local_only (void*))

  ;; GFile* gtk_file_chooser_get_preview_file (GtkFileChooser* chooser)
  (define-function void* gtk_file_chooser_get_preview_file (void*))

  ;; char* gtk_file_chooser_get_preview_filename (GtkFileChooser* chooser)
  (define-function char* gtk_file_chooser_get_preview_filename (void*))

  ;; char* gtk_file_chooser_get_preview_uri (GtkFileChooser* chooser)
  (define-function char* gtk_file_chooser_get_preview_uri (void*))

  ;; GtkWidget* gtk_file_chooser_get_preview_widget (GtkFileChooser* chooser)
  (define-function void* gtk_file_chooser_get_preview_widget (void*))

  ;; gboolean gtk_file_chooser_get_preview_widget_active (GtkFileChooser* chooser)
  (define-function int gtk_file_chooser_get_preview_widget_active (void*))

  ;; gboolean gtk_file_chooser_get_select_multiple (GtkFileChooser* chooser)
  (define-function int gtk_file_chooser_get_select_multiple (void*))

  ;; gboolean gtk_file_chooser_get_show_hidden (GtkFileChooser* chooser)
  (define-function int gtk_file_chooser_get_show_hidden (void*))

  ;; GType gtk_file_chooser_get_type (void)
  (define-function unsigned-long gtk_file_chooser_get_type ())

  ;; gchar* gtk_file_chooser_get_uri (GtkFileChooser* chooser)
  (define-function char* gtk_file_chooser_get_uri (void*))

  ;; GSList* gtk_file_chooser_get_uris (GtkFileChooser* chooser)
  (define-function void* gtk_file_chooser_get_uris (void*))

  ;; gboolean gtk_file_chooser_get_use_preview_label (GtkFileChooser* chooser)
  (define-function int gtk_file_chooser_get_use_preview_label (void*))

  ;; GSList* gtk_file_chooser_list_filters (GtkFileChooser* chooser)
  (define-function void* gtk_file_chooser_list_filters (void*))

  ;; GSList* gtk_file_chooser_list_shortcut_folder_uris (GtkFileChooser* chooser)
  (define-function void* gtk_file_chooser_list_shortcut_folder_uris (void*))

  ;; GSList* gtk_file_chooser_list_shortcut_folders (GtkFileChooser* chooser)
  (define-function void* gtk_file_chooser_list_shortcut_folders (void*))

  ;; void gtk_file_chooser_remove_filter (GtkFileChooser* chooser, GtkFileFilter* filter)
  (define-function void gtk_file_chooser_remove_filter (void* void*))

  ;; gboolean gtk_file_chooser_remove_shortcut_folder (GtkFileChooser* chooser, const char* folder, GError** error)
  (define-function int gtk_file_chooser_remove_shortcut_folder (void* char* void*))

  ;; gboolean gtk_file_chooser_remove_shortcut_folder_uri (GtkFileChooser* chooser, const char* uri, GError** error)
  (define-function int gtk_file_chooser_remove_shortcut_folder_uri (void* char* void*))

  ;; void gtk_file_chooser_select_all (GtkFileChooser* chooser)
  (define-function void gtk_file_chooser_select_all (void*))

  ;; gboolean gtk_file_chooser_select_file (GtkFileChooser* chooser, GFile* file, GError** error)
  (define-function int gtk_file_chooser_select_file (void* void* void*))

  ;; gboolean gtk_file_chooser_select_filename (GtkFileChooser* chooser, const char* filename)
  (define-function int gtk_file_chooser_select_filename (void* char*))

  ;; gboolean gtk_file_chooser_select_uri (GtkFileChooser* chooser, const char* uri)
  (define-function int gtk_file_chooser_select_uri (void* char*))

  ;; void gtk_file_chooser_set_action (GtkFileChooser* chooser, GtkFileChooserAction action)
  (define-function void gtk_file_chooser_set_action (void* int))

  ;; gboolean gtk_file_chooser_set_current_folder (GtkFileChooser* chooser, const gchar* filename)
  (define-function int gtk_file_chooser_set_current_folder (void* char*))

  ;; gboolean gtk_file_chooser_set_current_folder_file (GtkFileChooser* chooser, GFile* file, GError** error)
  (define-function int gtk_file_chooser_set_current_folder_file (void* void* void*))

  ;; gboolean gtk_file_chooser_set_current_folder_uri (GtkFileChooser* chooser, const gchar* uri)
  (define-function int gtk_file_chooser_set_current_folder_uri (void* char*))

  ;; void gtk_file_chooser_set_current_name (GtkFileChooser* chooser, const gchar* name)
  (define-function void gtk_file_chooser_set_current_name (void* char*))

  ;; void gtk_file_chooser_set_do_overwrite_confirmation (GtkFileChooser* chooser, gboolean do_overwrite_confirmation)
  (define-function void gtk_file_chooser_set_do_overwrite_confirmation (void* int))

  ;; void gtk_file_chooser_set_extra_widget (GtkFileChooser* chooser, GtkWidget* extra_widget)
  (define-function void gtk_file_chooser_set_extra_widget (void* void*))

  ;; gboolean gtk_file_chooser_set_file (GtkFileChooser* chooser, GFile* file, GError** error)
  (define-function int gtk_file_chooser_set_file (void* void* void*))

  ;; gboolean gtk_file_chooser_set_filename (GtkFileChooser* chooser, const char* filename)
  (define-function int gtk_file_chooser_set_filename (void* char*))

  ;; void gtk_file_chooser_set_filter (GtkFileChooser* chooser, GtkFileFilter* filter)
  (define-function void gtk_file_chooser_set_filter (void* void*))

  ;; void gtk_file_chooser_set_local_only (GtkFileChooser* chooser, gboolean local_only)
  (define-function void gtk_file_chooser_set_local_only (void* int))

  ;; void gtk_file_chooser_set_preview_widget (GtkFileChooser* chooser, GtkWidget* preview_widget)
  (define-function void gtk_file_chooser_set_preview_widget (void* void*))

  ;; void gtk_file_chooser_set_preview_widget_active (GtkFileChooser* chooser, gboolean active)
  (define-function void gtk_file_chooser_set_preview_widget_active (void* int))

  ;; void gtk_file_chooser_set_select_multiple (GtkFileChooser* chooser, gboolean select_multiple)
  (define-function void gtk_file_chooser_set_select_multiple (void* int))

  ;; void gtk_file_chooser_set_show_hidden (GtkFileChooser* chooser, gboolean show_hidden)
  (define-function void gtk_file_chooser_set_show_hidden (void* int))

  ;; gboolean gtk_file_chooser_set_uri (GtkFileChooser* chooser, const char* uri)
  (define-function int gtk_file_chooser_set_uri (void* char*))

  ;; void gtk_file_chooser_set_use_preview_label (GtkFileChooser* chooser, gboolean use_label)
  (define-function void gtk_file_chooser_set_use_preview_label (void* int))

  ;; void gtk_file_chooser_unselect_all (GtkFileChooser* chooser)
  (define-function void gtk_file_chooser_unselect_all (void*))

  ;; void gtk_file_chooser_unselect_file (GtkFileChooser* chooser, GFile* file)
  (define-function void gtk_file_chooser_unselect_file (void* void*))

  ;; void gtk_file_chooser_unselect_filename (GtkFileChooser* chooser, const char* filename)
  (define-function void gtk_file_chooser_unselect_filename (void* char*))

  ;; void gtk_file_chooser_unselect_uri (GtkFileChooser* chooser, const char* uri)
  (define-function void gtk_file_chooser_unselect_uri (void* char*))

  ;; GType gtk_file_chooser_widget_get_type (void)
  (define-function unsigned-long gtk_file_chooser_widget_get_type ())

  ;; GtkWidget* gtk_file_chooser_widget_new (GtkFileChooserAction action)
  (define-function void* gtk_file_chooser_widget_new (int))

  ;; void gtk_file_filter_add_custom (GtkFileFilter* filter, GtkFileFilterFlags needed, GtkFileFilterFunc func, gpointer data, GDestroyNotify notify)
  (define-function void gtk_file_filter_add_custom (void* int (c-callback int (void* void*)) void* (c-callback void (void*))))

  ;; void gtk_file_filter_add_mime_type (GtkFileFilter* filter, const gchar* mime_type)
  (define-function void gtk_file_filter_add_mime_type (void* char*))

  ;; void gtk_file_filter_add_pattern (GtkFileFilter* filter, const gchar* pattern)
  (define-function void gtk_file_filter_add_pattern (void* char*))

  ;; void gtk_file_filter_add_pixbuf_formats (GtkFileFilter* filter)
  (define-function void gtk_file_filter_add_pixbuf_formats (void*))

  ;; gboolean gtk_file_filter_filter (GtkFileFilter* filter, const GtkFileFilterInfo* filter_info)
  (define-function int gtk_file_filter_filter (void* void*))

  ;; GType gtk_file_filter_flags_get_type (void)
  (define-function unsigned-long gtk_file_filter_flags_get_type ())

  ;; const gchar* gtk_file_filter_get_name (GtkFileFilter* filter)
  (define-function char* gtk_file_filter_get_name (void*))

  ;; GtkFileFilterFlags gtk_file_filter_get_needed (GtkFileFilter* filter)
  (define-function int gtk_file_filter_get_needed (void*))

  ;; GType gtk_file_filter_get_type (void)
  (define-function unsigned-long gtk_file_filter_get_type ())

  ;; GtkFileFilter* gtk_file_filter_new (void)
  (define-function void* gtk_file_filter_new ())

  ;; void gtk_file_filter_set_name (GtkFileFilter* filter, const gchar* name)
  (define-function void gtk_file_filter_set_name (void* char*))

  ) ;[end]
