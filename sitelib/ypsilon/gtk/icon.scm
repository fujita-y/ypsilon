#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk icon)

  (export gtk_icon_factory_add
          gtk_icon_factory_add_default
          gtk_icon_factory_get_type
          gtk_icon_factory_lookup
          gtk_icon_factory_lookup_default
          gtk_icon_factory_new
          gtk_icon_factory_remove_default
          gtk_icon_info_copy
          gtk_icon_info_free
          gtk_icon_info_get_attach_points
          gtk_icon_info_get_base_size
          gtk_icon_info_get_builtin_pixbuf
          gtk_icon_info_get_display_name
          gtk_icon_info_get_embedded_rect
          gtk_icon_info_get_filename
          gtk_icon_info_get_type
          gtk_icon_info_load_icon
          gtk_icon_info_new_for_pixbuf
          gtk_icon_info_set_raw_coordinates
          gtk_icon_lookup_flags_get_type
          gtk_icon_set_add_source
          gtk_icon_set_copy
          gtk_icon_set_get_sizes
          gtk_icon_set_get_type
          gtk_icon_set_new
          gtk_icon_set_new_from_pixbuf
          gtk_icon_set_ref
          gtk_icon_set_render_icon
          gtk_icon_set_unref
          gtk_icon_size_from_name
          gtk_icon_size_get_name
          gtk_icon_size_get_type
          gtk_icon_size_lookup
          gtk_icon_size_lookup_for_settings
          gtk_icon_size_register
          gtk_icon_size_register_alias
          gtk_icon_source_copy
          gtk_icon_source_free
          gtk_icon_source_get_direction
          gtk_icon_source_get_direction_wildcarded
          gtk_icon_source_get_filename
          gtk_icon_source_get_icon_name
          gtk_icon_source_get_pixbuf
          gtk_icon_source_get_size
          gtk_icon_source_get_size_wildcarded
          gtk_icon_source_get_state
          gtk_icon_source_get_state_wildcarded
          gtk_icon_source_get_type
          gtk_icon_source_new
          gtk_icon_source_set_direction
          gtk_icon_source_set_direction_wildcarded
          gtk_icon_source_set_filename
          gtk_icon_source_set_icon_name
          gtk_icon_source_set_pixbuf
          gtk_icon_source_set_size
          gtk_icon_source_set_size_wildcarded
          gtk_icon_source_set_state
          gtk_icon_source_set_state_wildcarded
          gtk_icon_theme_add_builtin_icon
          gtk_icon_theme_append_search_path
          gtk_icon_theme_choose_icon
          gtk_icon_theme_error_get_type
          gtk_icon_theme_error_quark
          gtk_icon_theme_get_default
          gtk_icon_theme_get_example_icon_name
          gtk_icon_theme_get_for_screen
          gtk_icon_theme_get_icon_sizes
          gtk_icon_theme_get_search_path
          gtk_icon_theme_get_type
          gtk_icon_theme_has_icon
          gtk_icon_theme_list_contexts
          gtk_icon_theme_list_icons
          gtk_icon_theme_load_icon
          gtk_icon_theme_lookup_by_gicon
          gtk_icon_theme_lookup_icon
          gtk_icon_theme_new
          gtk_icon_theme_prepend_search_path
          gtk_icon_theme_rescan_if_needed
          gtk_icon_theme_set_custom_theme
          gtk_icon_theme_set_screen
          gtk_icon_theme_set_search_path
          gtk_icon_view_convert_widget_to_bin_window_coords
          gtk_icon_view_create_drag_icon
          gtk_icon_view_drop_position_get_type
          gtk_icon_view_enable_model_drag_dest
          gtk_icon_view_enable_model_drag_source
          gtk_icon_view_get_column_spacing
          gtk_icon_view_get_columns
          gtk_icon_view_get_cursor
          gtk_icon_view_get_dest_item_at_pos
          gtk_icon_view_get_drag_dest_item
          gtk_icon_view_get_item_at_pos
          gtk_icon_view_get_item_width
          gtk_icon_view_get_margin
          gtk_icon_view_get_markup_column
          gtk_icon_view_get_model
          gtk_icon_view_get_orientation
          gtk_icon_view_get_path_at_pos
          gtk_icon_view_get_pixbuf_column
          gtk_icon_view_get_reorderable
          gtk_icon_view_get_row_spacing
          gtk_icon_view_get_selected_items
          gtk_icon_view_get_selection_mode
          gtk_icon_view_get_spacing
          gtk_icon_view_get_text_column
          gtk_icon_view_get_tooltip_column
          gtk_icon_view_get_tooltip_context
          gtk_icon_view_get_type
          gtk_icon_view_get_visible_range
          gtk_icon_view_item_activated
          gtk_icon_view_new
          gtk_icon_view_new_with_model
          gtk_icon_view_path_is_selected
          gtk_icon_view_scroll_to_path
          gtk_icon_view_select_all
          gtk_icon_view_select_path
          gtk_icon_view_selected_foreach
          gtk_icon_view_set_column_spacing
          gtk_icon_view_set_columns
          gtk_icon_view_set_cursor
          gtk_icon_view_set_drag_dest_item
          gtk_icon_view_set_item_width
          gtk_icon_view_set_margin
          gtk_icon_view_set_markup_column
          gtk_icon_view_set_model
          gtk_icon_view_set_orientation
          gtk_icon_view_set_pixbuf_column
          gtk_icon_view_set_reorderable
          gtk_icon_view_set_row_spacing
          gtk_icon_view_set_selection_mode
          gtk_icon_view_set_spacing
          gtk_icon_view_set_text_column
          gtk_icon_view_set_tooltip_cell
          gtk_icon_view_set_tooltip_column
          gtk_icon_view_set_tooltip_item
          gtk_icon_view_unselect_all
          gtk_icon_view_unselect_path
          gtk_icon_view_unset_model_drag_dest
          gtk_icon_view_unset_model_drag_source)

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

  ;; void gtk_icon_factory_add (GtkIconFactory* factory, const gchar* stock_id, GtkIconSet* icon_set)
  (define-function void gtk_icon_factory_add (void* char* void*))

  ;; void gtk_icon_factory_add_default (GtkIconFactory* factory)
  (define-function void gtk_icon_factory_add_default (void*))

  ;; GType gtk_icon_factory_get_type (void)
  (define-function unsigned-long gtk_icon_factory_get_type ())

  ;; GtkIconSet* gtk_icon_factory_lookup (GtkIconFactory* factory, const gchar* stock_id)
  (define-function void* gtk_icon_factory_lookup (void* char*))

  ;; GtkIconSet* gtk_icon_factory_lookup_default (const gchar* stock_id)
  (define-function void* gtk_icon_factory_lookup_default (char*))

  ;; GtkIconFactory* gtk_icon_factory_new (void)
  (define-function void* gtk_icon_factory_new ())

  ;; void gtk_icon_factory_remove_default (GtkIconFactory* factory)
  (define-function void gtk_icon_factory_remove_default (void*))

  ;; GtkIconInfo* gtk_icon_info_copy (GtkIconInfo* icon_info)
  (define-function void* gtk_icon_info_copy (void*))

  ;; void gtk_icon_info_free (GtkIconInfo* icon_info)
  (define-function void gtk_icon_info_free (void*))

  ;; gboolean gtk_icon_info_get_attach_points (GtkIconInfo* icon_info, GdkPoint** points, gint* n_points)
  (define-function int gtk_icon_info_get_attach_points (void* void* void*))

  ;; gint gtk_icon_info_get_base_size (GtkIconInfo* icon_info)
  (define-function int gtk_icon_info_get_base_size (void*))

  ;; GdkPixbuf* gtk_icon_info_get_builtin_pixbuf (GtkIconInfo* icon_info)
  (define-function void* gtk_icon_info_get_builtin_pixbuf (void*))

  ;; const gchar* gtk_icon_info_get_display_name (GtkIconInfo* icon_info)
  (define-function char* gtk_icon_info_get_display_name (void*))

  ;; gboolean gtk_icon_info_get_embedded_rect (GtkIconInfo* icon_info, GdkRectangle* rectangle)
  (define-function int gtk_icon_info_get_embedded_rect (void* void*))

  ;; const gchar* gtk_icon_info_get_filename (GtkIconInfo* icon_info)
  (define-function char* gtk_icon_info_get_filename (void*))

  ;; GType gtk_icon_info_get_type (void)
  (define-function unsigned-long gtk_icon_info_get_type ())

  ;; GdkPixbuf* gtk_icon_info_load_icon (GtkIconInfo* icon_info, GError** error)
  (define-function void* gtk_icon_info_load_icon (void* void*))

  ;; GtkIconInfo* gtk_icon_info_new_for_pixbuf (GtkIconTheme* icon_theme, GdkPixbuf* pixbuf)
  (define-function void* gtk_icon_info_new_for_pixbuf (void* void*))

  ;; void gtk_icon_info_set_raw_coordinates (GtkIconInfo* icon_info, gboolean raw_coordinates)
  (define-function void gtk_icon_info_set_raw_coordinates (void* int))

  ;; GType gtk_icon_lookup_flags_get_type (void)
  (define-function unsigned-long gtk_icon_lookup_flags_get_type ())

  ;; void gtk_icon_set_add_source (GtkIconSet* icon_set, const GtkIconSource* source)
  (define-function void gtk_icon_set_add_source (void* void*))

  ;; GtkIconSet* gtk_icon_set_copy (GtkIconSet* icon_set)
  (define-function void* gtk_icon_set_copy (void*))

  ;; void gtk_icon_set_get_sizes (GtkIconSet* icon_set, GtkIconSize** sizes, gint* n_sizes)
  (define-function void gtk_icon_set_get_sizes (void* void* void*))

  ;; GType gtk_icon_set_get_type (void)
  (define-function unsigned-long gtk_icon_set_get_type ())

  ;; GtkIconSet* gtk_icon_set_new (void)
  (define-function void* gtk_icon_set_new ())

  ;; GtkIconSet* gtk_icon_set_new_from_pixbuf (GdkPixbuf* pixbuf)
  (define-function void* gtk_icon_set_new_from_pixbuf (void*))

  ;; GtkIconSet* gtk_icon_set_ref (GtkIconSet* icon_set)
  (define-function void* gtk_icon_set_ref (void*))

  ;; GdkPixbuf* gtk_icon_set_render_icon (GtkIconSet* icon_set, GtkStyle* style, GtkTextDirection direction, GtkStateType state, GtkIconSize size, GtkWidget* widget, const char* detail)
  (define-function void* gtk_icon_set_render_icon (void* void* int int int void* char*))

  ;; void gtk_icon_set_unref (GtkIconSet* icon_set)
  (define-function void gtk_icon_set_unref (void*))

  ;; GtkIconSize gtk_icon_size_from_name (const gchar* name)
  (define-function int gtk_icon_size_from_name (char*))

  ;; const gchar* gtk_icon_size_get_name (GtkIconSize size)
  (define-function char* gtk_icon_size_get_name (int))

  ;; GType gtk_icon_size_get_type (void)
  (define-function unsigned-long gtk_icon_size_get_type ())

  ;; gboolean gtk_icon_size_lookup (GtkIconSize size, gint* width, gint* height)
  (define-function int gtk_icon_size_lookup (int void* void*))

  ;; gboolean gtk_icon_size_lookup_for_settings (GtkSettings* settings, GtkIconSize size, gint* width, gint* height)
  (define-function int gtk_icon_size_lookup_for_settings (void* int void* void*))

  ;; GtkIconSize gtk_icon_size_register (const gchar* name, gint width, gint height)
  (define-function int gtk_icon_size_register (char* int int))

  ;; void gtk_icon_size_register_alias (const gchar* alias, GtkIconSize target)
  (define-function void gtk_icon_size_register_alias (char* int))

  ;; GtkIconSource* gtk_icon_source_copy (const GtkIconSource* source)
  (define-function void* gtk_icon_source_copy (void*))

  ;; void gtk_icon_source_free (GtkIconSource* source)
  (define-function void gtk_icon_source_free (void*))

  ;; GtkTextDirection gtk_icon_source_get_direction (const GtkIconSource* source)
  (define-function int gtk_icon_source_get_direction (void*))

  ;; gboolean gtk_icon_source_get_direction_wildcarded (const GtkIconSource* source)
  (define-function int gtk_icon_source_get_direction_wildcarded (void*))

  ;; const gchar* gtk_icon_source_get_filename (const GtkIconSource* source)
  (define-function char* gtk_icon_source_get_filename (void*))

  ;; const gchar* gtk_icon_source_get_icon_name (const GtkIconSource* source)
  (define-function char* gtk_icon_source_get_icon_name (void*))

  ;; GdkPixbuf* gtk_icon_source_get_pixbuf (const GtkIconSource* source)
  (define-function void* gtk_icon_source_get_pixbuf (void*))

  ;; GtkIconSize gtk_icon_source_get_size (const GtkIconSource* source)
  (define-function int gtk_icon_source_get_size (void*))

  ;; gboolean gtk_icon_source_get_size_wildcarded (const GtkIconSource* source)
  (define-function int gtk_icon_source_get_size_wildcarded (void*))

  ;; GtkStateType gtk_icon_source_get_state (const GtkIconSource* source)
  (define-function int gtk_icon_source_get_state (void*))

  ;; gboolean gtk_icon_source_get_state_wildcarded (const GtkIconSource* source)
  (define-function int gtk_icon_source_get_state_wildcarded (void*))

  ;; GType gtk_icon_source_get_type (void)
  (define-function unsigned-long gtk_icon_source_get_type ())

  ;; GtkIconSource* gtk_icon_source_new (void)
  (define-function void* gtk_icon_source_new ())

  ;; void gtk_icon_source_set_direction (GtkIconSource* source, GtkTextDirection direction)
  (define-function void gtk_icon_source_set_direction (void* int))

  ;; void gtk_icon_source_set_direction_wildcarded (GtkIconSource* source, gboolean setting)
  (define-function void gtk_icon_source_set_direction_wildcarded (void* int))

  ;; void gtk_icon_source_set_filename (GtkIconSource* source, const gchar* filename)
  (define-function void gtk_icon_source_set_filename (void* char*))

  ;; void gtk_icon_source_set_icon_name (GtkIconSource* source, const gchar* icon_name)
  (define-function void gtk_icon_source_set_icon_name (void* char*))

  ;; void gtk_icon_source_set_pixbuf (GtkIconSource* source, GdkPixbuf* pixbuf)
  (define-function void gtk_icon_source_set_pixbuf (void* void*))

  ;; void gtk_icon_source_set_size (GtkIconSource* source, GtkIconSize size)
  (define-function void gtk_icon_source_set_size (void* int))

  ;; void gtk_icon_source_set_size_wildcarded (GtkIconSource* source, gboolean setting)
  (define-function void gtk_icon_source_set_size_wildcarded (void* int))

  ;; void gtk_icon_source_set_state (GtkIconSource* source, GtkStateType state)
  (define-function void gtk_icon_source_set_state (void* int))

  ;; void gtk_icon_source_set_state_wildcarded (GtkIconSource* source, gboolean setting)
  (define-function void gtk_icon_source_set_state_wildcarded (void* int))

  ;; void gtk_icon_theme_add_builtin_icon (const gchar* icon_name, gint size, GdkPixbuf* pixbuf)
  (define-function void gtk_icon_theme_add_builtin_icon (char* int void*))

  ;; void gtk_icon_theme_append_search_path (GtkIconTheme* icon_theme, const gchar* path)
  (define-function void gtk_icon_theme_append_search_path (void* char*))

  ;; GtkIconInfo* gtk_icon_theme_choose_icon (GtkIconTheme* icon_theme, const gchar* icon_names[], gint size, GtkIconLookupFlags flags)
  (define-function void* gtk_icon_theme_choose_icon (void* void* int int))

  ;; GType gtk_icon_theme_error_get_type (void)
  (define-function unsigned-long gtk_icon_theme_error_get_type ())

  ;; GQuark gtk_icon_theme_error_quark (void)
  (define-function uint32_t gtk_icon_theme_error_quark ())

  ;; GtkIconTheme* gtk_icon_theme_get_default (void)
  (define-function void* gtk_icon_theme_get_default ())

  ;; char* gtk_icon_theme_get_example_icon_name (GtkIconTheme* icon_theme)
  (define-function char* gtk_icon_theme_get_example_icon_name (void*))

  ;; GtkIconTheme* gtk_icon_theme_get_for_screen (GdkScreen* screen)
  (define-function void* gtk_icon_theme_get_for_screen (void*))

  ;; gint* gtk_icon_theme_get_icon_sizes (GtkIconTheme* icon_theme, const gchar* icon_name)
  (define-function void* gtk_icon_theme_get_icon_sizes (void* char*))

  ;; void gtk_icon_theme_get_search_path (GtkIconTheme* icon_theme, gchar** path[], gint* n_elements)
  (define-function void gtk_icon_theme_get_search_path (void* void* void*))

  ;; GType gtk_icon_theme_get_type (void)
  (define-function unsigned-long gtk_icon_theme_get_type ())

  ;; gboolean gtk_icon_theme_has_icon (GtkIconTheme* icon_theme, const gchar* icon_name)
  (define-function int gtk_icon_theme_has_icon (void* char*))

  ;; GList* gtk_icon_theme_list_contexts (GtkIconTheme* icon_theme)
  (define-function void* gtk_icon_theme_list_contexts (void*))

  ;; GList* gtk_icon_theme_list_icons (GtkIconTheme* icon_theme, const gchar* context)
  (define-function void* gtk_icon_theme_list_icons (void* char*))

  ;; GdkPixbuf* gtk_icon_theme_load_icon (GtkIconTheme* icon_theme, const gchar* icon_name, gint size, GtkIconLookupFlags flags, GError** error)
  (define-function void* gtk_icon_theme_load_icon (void* char* int int void*))

  ;; GtkIconInfo* gtk_icon_theme_lookup_by_gicon (GtkIconTheme* icon_theme, GIcon* icon, gint size, GtkIconLookupFlags flags)
  (define-function void* gtk_icon_theme_lookup_by_gicon (void* void* int int))

  ;; GtkIconInfo* gtk_icon_theme_lookup_icon (GtkIconTheme* icon_theme, const gchar* icon_name, gint size, GtkIconLookupFlags flags)
  (define-function void* gtk_icon_theme_lookup_icon (void* char* int int))

  ;; GtkIconTheme* gtk_icon_theme_new (void)
  (define-function void* gtk_icon_theme_new ())

  ;; void gtk_icon_theme_prepend_search_path (GtkIconTheme* icon_theme, const gchar* path)
  (define-function void gtk_icon_theme_prepend_search_path (void* char*))

  ;; gboolean gtk_icon_theme_rescan_if_needed (GtkIconTheme* icon_theme)
  (define-function int gtk_icon_theme_rescan_if_needed (void*))

  ;; void gtk_icon_theme_set_custom_theme (GtkIconTheme* icon_theme, const gchar* theme_name)
  (define-function void gtk_icon_theme_set_custom_theme (void* char*))

  ;; void gtk_icon_theme_set_screen (GtkIconTheme* icon_theme, GdkScreen* screen)
  (define-function void gtk_icon_theme_set_screen (void* void*))

  ;; void gtk_icon_theme_set_search_path (GtkIconTheme* icon_theme, const gchar* path[], gint n_elements)
  (define-function void gtk_icon_theme_set_search_path (void* void* int))

  ;; void gtk_icon_view_convert_widget_to_bin_window_coords (GtkIconView* icon_view, gint wx, gint wy, gint* bx, gint* by)
  (define-function void gtk_icon_view_convert_widget_to_bin_window_coords (void* int int void* void*))

  ;; GdkPixmap* gtk_icon_view_create_drag_icon (GtkIconView* icon_view, GtkTreePath* path)
  (define-function void* gtk_icon_view_create_drag_icon (void* void*))

  ;; GType gtk_icon_view_drop_position_get_type (void)
  (define-function unsigned-long gtk_icon_view_drop_position_get_type ())

  ;; void gtk_icon_view_enable_model_drag_dest (GtkIconView* icon_view, const GtkTargetEntry* targets, gint n_targets, GdkDragAction actions)
  (define-function void gtk_icon_view_enable_model_drag_dest (void* void* int int))

  ;; void gtk_icon_view_enable_model_drag_source (GtkIconView* icon_view, GdkModifierType start_button_mask, const GtkTargetEntry* targets, gint n_targets, GdkDragAction actions)
  (define-function void gtk_icon_view_enable_model_drag_source (void* int void* int int))

  ;; gint gtk_icon_view_get_column_spacing (GtkIconView* icon_view)
  (define-function int gtk_icon_view_get_column_spacing (void*))

  ;; gint gtk_icon_view_get_columns (GtkIconView* icon_view)
  (define-function int gtk_icon_view_get_columns (void*))

  ;; gboolean gtk_icon_view_get_cursor (GtkIconView* icon_view, GtkTreePath** path, GtkCellRenderer** cell)
  (define-function int gtk_icon_view_get_cursor (void* void* void*))

  ;; gboolean gtk_icon_view_get_dest_item_at_pos (GtkIconView* icon_view, gint drag_x, gint drag_y, GtkTreePath** path, GtkIconViewDropPosition* pos)
  (define-function int gtk_icon_view_get_dest_item_at_pos (void* int int void* void*))

  ;; void gtk_icon_view_get_drag_dest_item (GtkIconView* icon_view, GtkTreePath** path, GtkIconViewDropPosition* pos)
  (define-function void gtk_icon_view_get_drag_dest_item (void* void* void*))

  ;; gboolean gtk_icon_view_get_item_at_pos (GtkIconView* icon_view, gint x, gint y, GtkTreePath** path, GtkCellRenderer** cell)
  (define-function int gtk_icon_view_get_item_at_pos (void* int int void* void*))

  ;; gint gtk_icon_view_get_item_width (GtkIconView* icon_view)
  (define-function int gtk_icon_view_get_item_width (void*))

  ;; gint gtk_icon_view_get_margin (GtkIconView* icon_view)
  (define-function int gtk_icon_view_get_margin (void*))

  ;; gint gtk_icon_view_get_markup_column (GtkIconView* icon_view)
  (define-function int gtk_icon_view_get_markup_column (void*))

  ;; GtkTreeModel* gtk_icon_view_get_model (GtkIconView* icon_view)
  (define-function void* gtk_icon_view_get_model (void*))

  ;; GtkOrientation gtk_icon_view_get_orientation (GtkIconView* icon_view)
  (define-function int gtk_icon_view_get_orientation (void*))

  ;; GtkTreePath* gtk_icon_view_get_path_at_pos (GtkIconView* icon_view, gint x, gint y)
  (define-function void* gtk_icon_view_get_path_at_pos (void* int int))

  ;; gint gtk_icon_view_get_pixbuf_column (GtkIconView* icon_view)
  (define-function int gtk_icon_view_get_pixbuf_column (void*))

  ;; gboolean gtk_icon_view_get_reorderable (GtkIconView* icon_view)
  (define-function int gtk_icon_view_get_reorderable (void*))

  ;; gint gtk_icon_view_get_row_spacing (GtkIconView* icon_view)
  (define-function int gtk_icon_view_get_row_spacing (void*))

  ;; GList* gtk_icon_view_get_selected_items (GtkIconView* icon_view)
  (define-function void* gtk_icon_view_get_selected_items (void*))

  ;; GtkSelectionMode gtk_icon_view_get_selection_mode (GtkIconView* icon_view)
  (define-function int gtk_icon_view_get_selection_mode (void*))

  ;; gint gtk_icon_view_get_spacing (GtkIconView* icon_view)
  (define-function int gtk_icon_view_get_spacing (void*))

  ;; gint gtk_icon_view_get_text_column (GtkIconView* icon_view)
  (define-function int gtk_icon_view_get_text_column (void*))

  ;; gint gtk_icon_view_get_tooltip_column (GtkIconView* icon_view)
  (define-function int gtk_icon_view_get_tooltip_column (void*))

  ;; gboolean gtk_icon_view_get_tooltip_context (GtkIconView* icon_view, gint* x, gint* y, gboolean keyboard_tip, GtkTreeModel** model, GtkTreePath** path, GtkTreeIter* iter)
  (define-function int gtk_icon_view_get_tooltip_context (void* void* void* int void* void* void*))

  ;; GType gtk_icon_view_get_type (void)
  (define-function unsigned-long gtk_icon_view_get_type ())

  ;; gboolean gtk_icon_view_get_visible_range (GtkIconView* icon_view, GtkTreePath** start_path, GtkTreePath** end_path)
  (define-function int gtk_icon_view_get_visible_range (void* void* void*))

  ;; void gtk_icon_view_item_activated (GtkIconView* icon_view, GtkTreePath* path)
  (define-function void gtk_icon_view_item_activated (void* void*))

  ;; GtkWidget* gtk_icon_view_new (void)
  (define-function void* gtk_icon_view_new ())

  ;; GtkWidget* gtk_icon_view_new_with_model (GtkTreeModel* model)
  (define-function void* gtk_icon_view_new_with_model (void*))

  ;; gboolean gtk_icon_view_path_is_selected (GtkIconView* icon_view, GtkTreePath* path)
  (define-function int gtk_icon_view_path_is_selected (void* void*))

  ;; void gtk_icon_view_scroll_to_path (GtkIconView* icon_view, GtkTreePath* path, gboolean use_align, gfloat row_align, gfloat col_align)
  (define-function void gtk_icon_view_scroll_to_path (void* void* int float float))

  ;; void gtk_icon_view_select_all (GtkIconView* icon_view)
  (define-function void gtk_icon_view_select_all (void*))

  ;; void gtk_icon_view_select_path (GtkIconView* icon_view, GtkTreePath* path)
  (define-function void gtk_icon_view_select_path (void* void*))

  ;; void gtk_icon_view_selected_foreach (GtkIconView* icon_view, GtkIconViewForeachFunc func, gpointer data)
  (define-function void gtk_icon_view_selected_foreach (void* (c-callback void (void* void* void*)) void*))

  ;; void gtk_icon_view_set_column_spacing (GtkIconView* icon_view, gint column_spacing)
  (define-function void gtk_icon_view_set_column_spacing (void* int))

  ;; void gtk_icon_view_set_columns (GtkIconView* icon_view, gint columns)
  (define-function void gtk_icon_view_set_columns (void* int))

  ;; void gtk_icon_view_set_cursor (GtkIconView* icon_view, GtkTreePath* path, GtkCellRenderer* cell, gboolean start_editing)
  (define-function void gtk_icon_view_set_cursor (void* void* void* int))

  ;; void gtk_icon_view_set_drag_dest_item (GtkIconView* icon_view, GtkTreePath* path, GtkIconViewDropPosition pos)
  (define-function void gtk_icon_view_set_drag_dest_item (void* void* int))

  ;; void gtk_icon_view_set_item_width (GtkIconView* icon_view, gint item_width)
  (define-function void gtk_icon_view_set_item_width (void* int))

  ;; void gtk_icon_view_set_margin (GtkIconView* icon_view, gint margin)
  (define-function void gtk_icon_view_set_margin (void* int))

  ;; void gtk_icon_view_set_markup_column (GtkIconView* icon_view, gint column)
  (define-function void gtk_icon_view_set_markup_column (void* int))

  ;; void gtk_icon_view_set_model (GtkIconView* icon_view, GtkTreeModel* model)
  (define-function void gtk_icon_view_set_model (void* void*))

  ;; void gtk_icon_view_set_orientation (GtkIconView* icon_view, GtkOrientation orientation)
  (define-function void gtk_icon_view_set_orientation (void* int))

  ;; void gtk_icon_view_set_pixbuf_column (GtkIconView* icon_view, gint column)
  (define-function void gtk_icon_view_set_pixbuf_column (void* int))

  ;; void gtk_icon_view_set_reorderable (GtkIconView* icon_view, gboolean reorderable)
  (define-function void gtk_icon_view_set_reorderable (void* int))

  ;; void gtk_icon_view_set_row_spacing (GtkIconView* icon_view, gint row_spacing)
  (define-function void gtk_icon_view_set_row_spacing (void* int))

  ;; void gtk_icon_view_set_selection_mode (GtkIconView* icon_view, GtkSelectionMode mode)
  (define-function void gtk_icon_view_set_selection_mode (void* int))

  ;; void gtk_icon_view_set_spacing (GtkIconView* icon_view, gint spacing)
  (define-function void gtk_icon_view_set_spacing (void* int))

  ;; void gtk_icon_view_set_text_column (GtkIconView* icon_view, gint column)
  (define-function void gtk_icon_view_set_text_column (void* int))

  ;; void gtk_icon_view_set_tooltip_cell (GtkIconView* icon_view, GtkTooltip* tooltip, GtkTreePath* path, GtkCellRenderer* cell)
  (define-function void gtk_icon_view_set_tooltip_cell (void* void* void* void*))

  ;; void gtk_icon_view_set_tooltip_column (GtkIconView* icon_view, gint column)
  (define-function void gtk_icon_view_set_tooltip_column (void* int))

  ;; void gtk_icon_view_set_tooltip_item (GtkIconView* icon_view, GtkTooltip* tooltip, GtkTreePath* path)
  (define-function void gtk_icon_view_set_tooltip_item (void* void* void*))

  ;; void gtk_icon_view_unselect_all (GtkIconView* icon_view)
  (define-function void gtk_icon_view_unselect_all (void*))

  ;; void gtk_icon_view_unselect_path (GtkIconView* icon_view, GtkTreePath* path)
  (define-function void gtk_icon_view_unselect_path (void* void*))

  ;; void gtk_icon_view_unset_model_drag_dest (GtkIconView* icon_view)
  (define-function void gtk_icon_view_unset_model_drag_dest (void*))

  ;; void gtk_icon_view_unset_model_drag_source (GtkIconView* icon_view)
  (define-function void gtk_icon_view_unset_model_drag_source (void*))

  ) ;[end]
