#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk tree)

  (export gtk_tree_drag_dest_drag_data_received
          gtk_tree_drag_dest_get_type
          gtk_tree_drag_dest_row_drop_possible
          gtk_tree_drag_source_drag_data_delete
          gtk_tree_drag_source_drag_data_get
          gtk_tree_drag_source_get_type
          gtk_tree_drag_source_row_draggable
          gtk_tree_get_row_drag_data
          gtk_tree_iter_copy
          gtk_tree_iter_free
          gtk_tree_iter_get_type
          gtk_tree_model_filter_clear_cache
          gtk_tree_model_filter_convert_child_iter_to_iter
          gtk_tree_model_filter_convert_child_path_to_path
          gtk_tree_model_filter_convert_iter_to_child_iter
          gtk_tree_model_filter_convert_path_to_child_path
          gtk_tree_model_filter_get_model
          gtk_tree_model_filter_get_type
          gtk_tree_model_filter_new
          gtk_tree_model_filter_refilter
          gtk_tree_model_filter_set_modify_func
          gtk_tree_model_filter_set_visible_column
          gtk_tree_model_filter_set_visible_func
          gtk_tree_model_flags_get_type
          gtk_tree_model_foreach
          gtk_tree_model_get
          gtk_tree_model_get_column_type
          gtk_tree_model_get_flags
          gtk_tree_model_get_iter
          gtk_tree_model_get_iter_first
          gtk_tree_model_get_iter_from_string
          gtk_tree_model_get_n_columns
          gtk_tree_model_get_path
          gtk_tree_model_get_string_from_iter
          gtk_tree_model_get_type
          gtk_tree_model_get_valist
          gtk_tree_model_get_value
          gtk_tree_model_iter_children
          gtk_tree_model_iter_has_child
          gtk_tree_model_iter_n_children
          gtk_tree_model_iter_next
          gtk_tree_model_iter_nth_child
          gtk_tree_model_iter_parent
          gtk_tree_model_ref_node
          gtk_tree_model_row_changed
          gtk_tree_model_row_deleted
          gtk_tree_model_row_has_child_toggled
          gtk_tree_model_row_inserted
          gtk_tree_model_rows_reordered
          gtk_tree_model_sort_clear_cache
          gtk_tree_model_sort_convert_child_iter_to_iter
          gtk_tree_model_sort_convert_child_path_to_path
          gtk_tree_model_sort_convert_iter_to_child_iter
          gtk_tree_model_sort_convert_path_to_child_path
          gtk_tree_model_sort_get_model
          gtk_tree_model_sort_get_type
          gtk_tree_model_sort_iter_is_valid
          gtk_tree_model_sort_new_with_model
          gtk_tree_model_sort_reset_default_sort_func
          gtk_tree_model_unref_node
          gtk_tree_path_append_index
          gtk_tree_path_compare
          gtk_tree_path_copy
          gtk_tree_path_down
          gtk_tree_path_free
          gtk_tree_path_get_depth
          gtk_tree_path_get_indices
          gtk_tree_path_get_type
          gtk_tree_path_is_ancestor
          gtk_tree_path_is_descendant
          gtk_tree_path_new
          gtk_tree_path_new_first
          gtk_tree_path_new_from_indices
          gtk_tree_path_new_from_string
          gtk_tree_path_next
          gtk_tree_path_prepend_index
          gtk_tree_path_prev
          gtk_tree_path_to_string
          gtk_tree_path_up
          gtk_tree_row_reference_copy
          gtk_tree_row_reference_deleted
          gtk_tree_row_reference_free
          gtk_tree_row_reference_get_model
          gtk_tree_row_reference_get_path
          gtk_tree_row_reference_get_type
          gtk_tree_row_reference_inserted
          gtk_tree_row_reference_new
          gtk_tree_row_reference_new_proxy
          gtk_tree_row_reference_reordered
          gtk_tree_row_reference_valid
          gtk_tree_selection_count_selected_rows
          gtk_tree_selection_get_mode
          gtk_tree_selection_get_select_function
          gtk_tree_selection_get_selected
          gtk_tree_selection_get_selected_rows
          gtk_tree_selection_get_tree_view
          gtk_tree_selection_get_type
          gtk_tree_selection_get_user_data
          gtk_tree_selection_iter_is_selected
          gtk_tree_selection_path_is_selected
          gtk_tree_selection_select_all
          gtk_tree_selection_select_iter
          gtk_tree_selection_select_path
          gtk_tree_selection_select_range
          gtk_tree_selection_selected_foreach
          gtk_tree_selection_set_mode
          gtk_tree_selection_set_select_function
          gtk_tree_selection_unselect_all
          gtk_tree_selection_unselect_iter
          gtk_tree_selection_unselect_path
          gtk_tree_selection_unselect_range
          gtk_tree_set_row_drag_data
          gtk_tree_sortable_get_sort_column_id
          gtk_tree_sortable_get_type
          gtk_tree_sortable_has_default_sort_func
          gtk_tree_sortable_set_default_sort_func
          gtk_tree_sortable_set_sort_column_id
          gtk_tree_sortable_set_sort_func
          gtk_tree_sortable_sort_column_changed
          gtk_tree_store_append
          gtk_tree_store_clear
          gtk_tree_store_get_type
          gtk_tree_store_insert
          gtk_tree_store_insert_after
          gtk_tree_store_insert_before
          gtk_tree_store_insert_with_values
          gtk_tree_store_insert_with_valuesv
          gtk_tree_store_is_ancestor
          gtk_tree_store_iter_depth
          gtk_tree_store_iter_is_valid
          gtk_tree_store_move_after
          gtk_tree_store_move_before
          gtk_tree_store_new
          gtk_tree_store_newv
          gtk_tree_store_prepend
          gtk_tree_store_remove
          gtk_tree_store_reorder
          gtk_tree_store_set
          gtk_tree_store_set_column_types
          gtk_tree_store_set_valist
          gtk_tree_store_set_value
          gtk_tree_store_set_valuesv
          gtk_tree_store_swap
          gtk_tree_view_append_column
          gtk_tree_view_collapse_all
          gtk_tree_view_collapse_row
          gtk_tree_view_column_add_attribute
          gtk_tree_view_column_cell_get_position
          gtk_tree_view_column_cell_get_size
          gtk_tree_view_column_cell_is_visible
          gtk_tree_view_column_cell_set_cell_data
          gtk_tree_view_column_clear
          gtk_tree_view_column_clear_attributes
          gtk_tree_view_column_clicked
          gtk_tree_view_column_focus_cell
          gtk_tree_view_column_get_alignment
          gtk_tree_view_column_get_cell_renderers
          gtk_tree_view_column_get_clickable
          gtk_tree_view_column_get_expand
          gtk_tree_view_column_get_fixed_width
          gtk_tree_view_column_get_max_width
          gtk_tree_view_column_get_min_width
          gtk_tree_view_column_get_reorderable
          gtk_tree_view_column_get_resizable
          gtk_tree_view_column_get_sizing
          gtk_tree_view_column_get_sort_column_id
          gtk_tree_view_column_get_sort_indicator
          gtk_tree_view_column_get_sort_order
          gtk_tree_view_column_get_spacing
          gtk_tree_view_column_get_title
          gtk_tree_view_column_get_tree_view
          gtk_tree_view_column_get_type
          gtk_tree_view_column_get_visible
          gtk_tree_view_column_get_widget
          gtk_tree_view_column_get_width
          gtk_tree_view_column_new
          gtk_tree_view_column_new_with_attributes
          gtk_tree_view_column_pack_end
          gtk_tree_view_column_pack_start
          gtk_tree_view_column_queue_resize
          gtk_tree_view_column_set_alignment
          gtk_tree_view_column_set_attributes
          gtk_tree_view_column_set_cell_data_func
          gtk_tree_view_column_set_clickable
          gtk_tree_view_column_set_expand
          gtk_tree_view_column_set_fixed_width
          gtk_tree_view_column_set_max_width
          gtk_tree_view_column_set_min_width
          gtk_tree_view_column_set_reorderable
          gtk_tree_view_column_set_resizable
          gtk_tree_view_column_set_sizing
          gtk_tree_view_column_set_sort_column_id
          gtk_tree_view_column_set_sort_indicator
          gtk_tree_view_column_set_sort_order
          gtk_tree_view_column_set_spacing
          gtk_tree_view_column_set_title
          gtk_tree_view_column_set_visible
          gtk_tree_view_column_set_widget
          gtk_tree_view_column_sizing_get_type
          gtk_tree_view_columns_autosize
          gtk_tree_view_convert_bin_window_to_tree_coords
          gtk_tree_view_convert_bin_window_to_widget_coords
          gtk_tree_view_convert_tree_to_bin_window_coords
          gtk_tree_view_convert_tree_to_widget_coords
          gtk_tree_view_convert_widget_to_bin_window_coords
          gtk_tree_view_convert_widget_to_tree_coords
          gtk_tree_view_create_row_drag_icon
          gtk_tree_view_drop_position_get_type
          gtk_tree_view_enable_model_drag_dest
          gtk_tree_view_enable_model_drag_source
          gtk_tree_view_expand_all
          gtk_tree_view_expand_row
          gtk_tree_view_expand_to_path
          gtk_tree_view_get_background_area
          gtk_tree_view_get_bin_window
          gtk_tree_view_get_cell_area
          gtk_tree_view_get_column
          gtk_tree_view_get_columns
          gtk_tree_view_get_cursor
          gtk_tree_view_get_dest_row_at_pos
          gtk_tree_view_get_drag_dest_row
          gtk_tree_view_get_enable_search
          gtk_tree_view_get_enable_tree_lines
          gtk_tree_view_get_expander_column
          gtk_tree_view_get_fixed_height_mode
          gtk_tree_view_get_grid_lines
          gtk_tree_view_get_hadjustment
          gtk_tree_view_get_headers_clickable
          gtk_tree_view_get_headers_visible
          gtk_tree_view_get_hover_expand
          gtk_tree_view_get_hover_selection
          gtk_tree_view_get_level_indentation
          gtk_tree_view_get_model
          gtk_tree_view_get_path_at_pos
          gtk_tree_view_get_reorderable
          gtk_tree_view_get_row_separator_func
          gtk_tree_view_get_rubber_banding
          gtk_tree_view_get_rules_hint
          gtk_tree_view_get_search_column
          gtk_tree_view_get_search_entry
          gtk_tree_view_get_search_equal_func
          gtk_tree_view_get_search_position_func
          gtk_tree_view_get_selection
          gtk_tree_view_get_show_expanders
          gtk_tree_view_get_tooltip_column
          gtk_tree_view_get_tooltip_context
          gtk_tree_view_get_type
          gtk_tree_view_get_vadjustment
          gtk_tree_view_get_visible_range
          gtk_tree_view_get_visible_rect
          gtk_tree_view_grid_lines_get_type
          gtk_tree_view_insert_column
          gtk_tree_view_insert_column_with_attributes
          gtk_tree_view_insert_column_with_data_func
          gtk_tree_view_is_rubber_banding_active
          gtk_tree_view_map_expanded_rows
          gtk_tree_view_mode_get_type
          gtk_tree_view_move_column_after
          gtk_tree_view_new
          gtk_tree_view_new_with_model
          gtk_tree_view_remove_column
          gtk_tree_view_row_activated
          gtk_tree_view_row_expanded
          gtk_tree_view_scroll_to_cell
          gtk_tree_view_scroll_to_point
          gtk_tree_view_set_column_drag_function
          gtk_tree_view_set_cursor
          gtk_tree_view_set_cursor_on_cell
          gtk_tree_view_set_destroy_count_func
          gtk_tree_view_set_drag_dest_row
          gtk_tree_view_set_enable_search
          gtk_tree_view_set_enable_tree_lines
          gtk_tree_view_set_expander_column
          gtk_tree_view_set_fixed_height_mode
          gtk_tree_view_set_grid_lines
          gtk_tree_view_set_hadjustment
          gtk_tree_view_set_headers_clickable
          gtk_tree_view_set_headers_visible
          gtk_tree_view_set_hover_expand
          gtk_tree_view_set_hover_selection
          gtk_tree_view_set_level_indentation
          gtk_tree_view_set_model
          gtk_tree_view_set_reorderable
          gtk_tree_view_set_row_separator_func
          gtk_tree_view_set_rubber_banding
          gtk_tree_view_set_rules_hint
          gtk_tree_view_set_search_column
          gtk_tree_view_set_search_entry
          gtk_tree_view_set_search_equal_func
          gtk_tree_view_set_search_position_func
          gtk_tree_view_set_show_expanders
          gtk_tree_view_set_tooltip_cell
          gtk_tree_view_set_tooltip_column
          gtk_tree_view_set_tooltip_row
          gtk_tree_view_set_vadjustment
          gtk_tree_view_unset_rows_drag_dest
          gtk_tree_view_unset_rows_drag_source)

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

  ;; gboolean gtk_tree_drag_dest_drag_data_received (GtkTreeDragDest* drag_dest, GtkTreePath* dest, GtkSelectionData* selection_data)
  (define-function int gtk_tree_drag_dest_drag_data_received (void* void* void*))

  ;; GType gtk_tree_drag_dest_get_type (void)
  (define-function unsigned-long gtk_tree_drag_dest_get_type ())

  ;; gboolean gtk_tree_drag_dest_row_drop_possible (GtkTreeDragDest* drag_dest, GtkTreePath* dest_path, GtkSelectionData* selection_data)
  (define-function int gtk_tree_drag_dest_row_drop_possible (void* void* void*))

  ;; gboolean gtk_tree_drag_source_drag_data_delete (GtkTreeDragSource* drag_source, GtkTreePath* path)
  (define-function int gtk_tree_drag_source_drag_data_delete (void* void*))

  ;; gboolean gtk_tree_drag_source_drag_data_get (GtkTreeDragSource* drag_source, GtkTreePath* path, GtkSelectionData* selection_data)
  (define-function int gtk_tree_drag_source_drag_data_get (void* void* void*))

  ;; GType gtk_tree_drag_source_get_type (void)
  (define-function unsigned-long gtk_tree_drag_source_get_type ())

  ;; gboolean gtk_tree_drag_source_row_draggable (GtkTreeDragSource* drag_source, GtkTreePath* path)
  (define-function int gtk_tree_drag_source_row_draggable (void* void*))

  ;; gboolean gtk_tree_get_row_drag_data (GtkSelectionData* selection_data, GtkTreeModel** tree_model, GtkTreePath** path)
  (define-function int gtk_tree_get_row_drag_data (void* void* void*))

  ;; GtkTreeIter* gtk_tree_iter_copy (GtkTreeIter* iter)
  (define-function void* gtk_tree_iter_copy (void*))

  ;; void gtk_tree_iter_free (GtkTreeIter* iter)
  (define-function void gtk_tree_iter_free (void*))

  ;; GType gtk_tree_iter_get_type (void)
  (define-function unsigned-long gtk_tree_iter_get_type ())

  ;; void gtk_tree_model_filter_clear_cache (GtkTreeModelFilter* filter)
  (define-function void gtk_tree_model_filter_clear_cache (void*))

  ;; gboolean gtk_tree_model_filter_convert_child_iter_to_iter (GtkTreeModelFilter* filter, GtkTreeIter* filter_iter, GtkTreeIter* child_iter)
  (define-function int gtk_tree_model_filter_convert_child_iter_to_iter (void* void* void*))

  ;; GtkTreePath* gtk_tree_model_filter_convert_child_path_to_path (GtkTreeModelFilter* filter, GtkTreePath* child_path)
  (define-function void* gtk_tree_model_filter_convert_child_path_to_path (void* void*))

  ;; void gtk_tree_model_filter_convert_iter_to_child_iter (GtkTreeModelFilter* filter, GtkTreeIter* child_iter, GtkTreeIter* filter_iter)
  (define-function void gtk_tree_model_filter_convert_iter_to_child_iter (void* void* void*))

  ;; GtkTreePath* gtk_tree_model_filter_convert_path_to_child_path (GtkTreeModelFilter* filter, GtkTreePath* filter_path)
  (define-function void* gtk_tree_model_filter_convert_path_to_child_path (void* void*))

  ;; GtkTreeModel* gtk_tree_model_filter_get_model (GtkTreeModelFilter* filter)
  (define-function void* gtk_tree_model_filter_get_model (void*))

  ;; GType gtk_tree_model_filter_get_type (void)
  (define-function unsigned-long gtk_tree_model_filter_get_type ())

  ;; GtkTreeModel* gtk_tree_model_filter_new (GtkTreeModel* child_model, GtkTreePath* root)
  (define-function void* gtk_tree_model_filter_new (void* void*))

  ;; void gtk_tree_model_filter_refilter (GtkTreeModelFilter* filter)
  (define-function void gtk_tree_model_filter_refilter (void*))

  ;; void gtk_tree_model_filter_set_modify_func (GtkTreeModelFilter* filter, gint n_columns, GType* types, GtkTreeModelFilterModifyFunc func, gpointer data, GDestroyNotify destroy)
  (define-function void gtk_tree_model_filter_set_modify_func (void* int void* (c-callback void (void* void* void* int void*)) void* (c-callback void (void*))))

  ;; void gtk_tree_model_filter_set_visible_column (GtkTreeModelFilter* filter, gint column)
  (define-function void gtk_tree_model_filter_set_visible_column (void* int))

  ;; void gtk_tree_model_filter_set_visible_func (GtkTreeModelFilter* filter, GtkTreeModelFilterVisibleFunc func, gpointer data, GDestroyNotify destroy)
  (define-function void gtk_tree_model_filter_set_visible_func (void* (c-callback int (void* void* void*)) void* (c-callback void (void*))))

  ;; GType gtk_tree_model_flags_get_type (void)
  (define-function unsigned-long gtk_tree_model_flags_get_type ())

  ;; void gtk_tree_model_foreach (GtkTreeModel* model, GtkTreeModelForeachFunc func, gpointer user_data)
  (define-function void gtk_tree_model_foreach (void* (c-callback int (void* void* void* void*)) void*))

  ;; void gtk_tree_model_get (GtkTreeModel* tree_model, GtkTreeIter* iter, ...)
  (define-function void gtk_tree_model_get (void* void* ...))

  ;; GType gtk_tree_model_get_column_type (GtkTreeModel* tree_model, gint index_)
  (define-function unsigned-long gtk_tree_model_get_column_type (void* int))

  ;; GtkTreeModelFlags gtk_tree_model_get_flags (GtkTreeModel* tree_model)
  (define-function int gtk_tree_model_get_flags (void*))

  ;; gboolean gtk_tree_model_get_iter (GtkTreeModel* tree_model, GtkTreeIter* iter, GtkTreePath* path)
  (define-function int gtk_tree_model_get_iter (void* void* void*))

  ;; gboolean gtk_tree_model_get_iter_first (GtkTreeModel* tree_model, GtkTreeIter* iter)
  (define-function int gtk_tree_model_get_iter_first (void* void*))

  ;; gboolean gtk_tree_model_get_iter_from_string (GtkTreeModel* tree_model, GtkTreeIter* iter, const gchar* path_string)
  (define-function int gtk_tree_model_get_iter_from_string (void* void* char*))

  ;; gint gtk_tree_model_get_n_columns (GtkTreeModel* tree_model)
  (define-function int gtk_tree_model_get_n_columns (void*))

  ;; GtkTreePath* gtk_tree_model_get_path (GtkTreeModel* tree_model, GtkTreeIter* iter)
  (define-function void* gtk_tree_model_get_path (void* void*))

  ;; gchar* gtk_tree_model_get_string_from_iter (GtkTreeModel* tree_model, GtkTreeIter* iter)
  (define-function char* gtk_tree_model_get_string_from_iter (void* void*))

  ;; GType gtk_tree_model_get_type (void)
  (define-function unsigned-long gtk_tree_model_get_type ())

  ;; void gtk_tree_model_get_valist (GtkTreeModel* tree_model, GtkTreeIter* iter, va_list var_args)
  (define-function/va_list void gtk_tree_model_get_valist (void* void* va_list))

  ;; void gtk_tree_model_get_value (GtkTreeModel* tree_model, GtkTreeIter* iter, gint column, GValue* value)
  (define-function void gtk_tree_model_get_value (void* void* int void*))

  ;; gboolean gtk_tree_model_iter_children (GtkTreeModel* tree_model, GtkTreeIter* iter, GtkTreeIter* parent)
  (define-function int gtk_tree_model_iter_children (void* void* void*))

  ;; gboolean gtk_tree_model_iter_has_child (GtkTreeModel* tree_model, GtkTreeIter* iter)
  (define-function int gtk_tree_model_iter_has_child (void* void*))

  ;; gint gtk_tree_model_iter_n_children (GtkTreeModel* tree_model, GtkTreeIter* iter)
  (define-function int gtk_tree_model_iter_n_children (void* void*))

  ;; gboolean gtk_tree_model_iter_next (GtkTreeModel* tree_model, GtkTreeIter* iter)
  (define-function int gtk_tree_model_iter_next (void* void*))

  ;; gboolean gtk_tree_model_iter_nth_child (GtkTreeModel* tree_model, GtkTreeIter* iter, GtkTreeIter* parent, gint n)
  (define-function int gtk_tree_model_iter_nth_child (void* void* void* int))

  ;; gboolean gtk_tree_model_iter_parent (GtkTreeModel* tree_model, GtkTreeIter* iter, GtkTreeIter* child)
  (define-function int gtk_tree_model_iter_parent (void* void* void*))

  ;; void gtk_tree_model_ref_node (GtkTreeModel* tree_model, GtkTreeIter* iter)
  (define-function void gtk_tree_model_ref_node (void* void*))

  ;; void gtk_tree_model_row_changed (GtkTreeModel* tree_model, GtkTreePath* path, GtkTreeIter* iter)
  (define-function void gtk_tree_model_row_changed (void* void* void*))

  ;; void gtk_tree_model_row_deleted (GtkTreeModel* tree_model, GtkTreePath* path)
  (define-function void gtk_tree_model_row_deleted (void* void*))

  ;; void gtk_tree_model_row_has_child_toggled (GtkTreeModel* tree_model, GtkTreePath* path, GtkTreeIter* iter)
  (define-function void gtk_tree_model_row_has_child_toggled (void* void* void*))

  ;; void gtk_tree_model_row_inserted (GtkTreeModel* tree_model, GtkTreePath* path, GtkTreeIter* iter)
  (define-function void gtk_tree_model_row_inserted (void* void* void*))

  ;; void gtk_tree_model_rows_reordered (GtkTreeModel* tree_model, GtkTreePath* path, GtkTreeIter* iter, gint* new_order)
  (define-function void gtk_tree_model_rows_reordered (void* void* void* void*))

  ;; void gtk_tree_model_sort_clear_cache (GtkTreeModelSort* tree_model_sort)
  (define-function void gtk_tree_model_sort_clear_cache (void*))

  ;; gboolean gtk_tree_model_sort_convert_child_iter_to_iter (GtkTreeModelSort* tree_model_sort, GtkTreeIter* sort_iter, GtkTreeIter* child_iter)
  (define-function int gtk_tree_model_sort_convert_child_iter_to_iter (void* void* void*))

  ;; GtkTreePath* gtk_tree_model_sort_convert_child_path_to_path (GtkTreeModelSort* tree_model_sort, GtkTreePath* child_path)
  (define-function void* gtk_tree_model_sort_convert_child_path_to_path (void* void*))

  ;; void gtk_tree_model_sort_convert_iter_to_child_iter (GtkTreeModelSort* tree_model_sort, GtkTreeIter* child_iter, GtkTreeIter* sorted_iter)
  (define-function void gtk_tree_model_sort_convert_iter_to_child_iter (void* void* void*))

  ;; GtkTreePath* gtk_tree_model_sort_convert_path_to_child_path (GtkTreeModelSort* tree_model_sort, GtkTreePath* sorted_path)
  (define-function void* gtk_tree_model_sort_convert_path_to_child_path (void* void*))

  ;; GtkTreeModel* gtk_tree_model_sort_get_model (GtkTreeModelSort* tree_model)
  (define-function void* gtk_tree_model_sort_get_model (void*))

  ;; GType gtk_tree_model_sort_get_type (void)
  (define-function unsigned-long gtk_tree_model_sort_get_type ())

  ;; gboolean gtk_tree_model_sort_iter_is_valid (GtkTreeModelSort* tree_model_sort, GtkTreeIter* iter)
  (define-function int gtk_tree_model_sort_iter_is_valid (void* void*))

  ;; GtkTreeModel* gtk_tree_model_sort_new_with_model (GtkTreeModel* child_model)
  (define-function void* gtk_tree_model_sort_new_with_model (void*))

  ;; void gtk_tree_model_sort_reset_default_sort_func (GtkTreeModelSort* tree_model_sort)
  (define-function void gtk_tree_model_sort_reset_default_sort_func (void*))

  ;; void gtk_tree_model_unref_node (GtkTreeModel* tree_model, GtkTreeIter* iter)
  (define-function void gtk_tree_model_unref_node (void* void*))

  ;; void gtk_tree_path_append_index (GtkTreePath* path, gint index_)
  (define-function void gtk_tree_path_append_index (void* int))

  ;; gint gtk_tree_path_compare (const GtkTreePath* a, const GtkTreePath* b)
  (define-function int gtk_tree_path_compare (void* void*))

  ;; GtkTreePath* gtk_tree_path_copy (const GtkTreePath* path)
  (define-function void* gtk_tree_path_copy (void*))

  ;; void gtk_tree_path_down (GtkTreePath* path)
  (define-function void gtk_tree_path_down (void*))

  ;; void gtk_tree_path_free (GtkTreePath* path)
  (define-function void gtk_tree_path_free (void*))

  ;; gint gtk_tree_path_get_depth (GtkTreePath* path)
  (define-function int gtk_tree_path_get_depth (void*))

  ;; gint* gtk_tree_path_get_indices (GtkTreePath* path)
  (define-function void* gtk_tree_path_get_indices (void*))

  ;; GType gtk_tree_path_get_type (void)
  (define-function unsigned-long gtk_tree_path_get_type ())

  ;; gboolean gtk_tree_path_is_ancestor (GtkTreePath* path, GtkTreePath* descendant)
  (define-function int gtk_tree_path_is_ancestor (void* void*))

  ;; gboolean gtk_tree_path_is_descendant (GtkTreePath* path, GtkTreePath* ancestor)
  (define-function int gtk_tree_path_is_descendant (void* void*))

  ;; GtkTreePath* gtk_tree_path_new (void)
  (define-function void* gtk_tree_path_new ())

  ;; GtkTreePath* gtk_tree_path_new_first (void)
  (define-function void* gtk_tree_path_new_first ())

  ;; GtkTreePath* gtk_tree_path_new_from_indices (gint first_index, ...)
  (define-function void* gtk_tree_path_new_from_indices (int ...))

  ;; GtkTreePath* gtk_tree_path_new_from_string (const gchar* path)
  (define-function void* gtk_tree_path_new_from_string (char*))

  ;; void gtk_tree_path_next (GtkTreePath* path)
  (define-function void gtk_tree_path_next (void*))

  ;; void gtk_tree_path_prepend_index (GtkTreePath* path, gint index_)
  (define-function void gtk_tree_path_prepend_index (void* int))

  ;; gboolean gtk_tree_path_prev (GtkTreePath* path)
  (define-function int gtk_tree_path_prev (void*))

  ;; gchar* gtk_tree_path_to_string (GtkTreePath* path)
  (define-function char* gtk_tree_path_to_string (void*))

  ;; gboolean gtk_tree_path_up (GtkTreePath* path)
  (define-function int gtk_tree_path_up (void*))

  ;; GtkTreeRowReference* gtk_tree_row_reference_copy (GtkTreeRowReference* reference)
  (define-function void* gtk_tree_row_reference_copy (void*))

  ;; void gtk_tree_row_reference_deleted (GObject* proxy, GtkTreePath* path)
  (define-function void gtk_tree_row_reference_deleted (void* void*))

  ;; void gtk_tree_row_reference_free (GtkTreeRowReference* reference)
  (define-function void gtk_tree_row_reference_free (void*))

  ;; GtkTreeModel* gtk_tree_row_reference_get_model (GtkTreeRowReference* reference)
  (define-function void* gtk_tree_row_reference_get_model (void*))

  ;; GtkTreePath* gtk_tree_row_reference_get_path (GtkTreeRowReference* reference)
  (define-function void* gtk_tree_row_reference_get_path (void*))

  ;; GType gtk_tree_row_reference_get_type (void)
  (define-function unsigned-long gtk_tree_row_reference_get_type ())

  ;; void gtk_tree_row_reference_inserted (GObject* proxy, GtkTreePath* path)
  (define-function void gtk_tree_row_reference_inserted (void* void*))

  ;; GtkTreeRowReference* gtk_tree_row_reference_new (GtkTreeModel* model, GtkTreePath* path)
  (define-function void* gtk_tree_row_reference_new (void* void*))

  ;; GtkTreeRowReference* gtk_tree_row_reference_new_proxy (GObject* proxy, GtkTreeModel* model, GtkTreePath* path)
  (define-function void* gtk_tree_row_reference_new_proxy (void* void* void*))

  ;; void gtk_tree_row_reference_reordered (GObject* proxy, GtkTreePath* path, GtkTreeIter* iter, gint* new_order)
  (define-function void gtk_tree_row_reference_reordered (void* void* void* void*))

  ;; gboolean gtk_tree_row_reference_valid (GtkTreeRowReference* reference)
  (define-function int gtk_tree_row_reference_valid (void*))

  ;; gint gtk_tree_selection_count_selected_rows (GtkTreeSelection* selection)
  (define-function int gtk_tree_selection_count_selected_rows (void*))

  ;; GtkSelectionMode gtk_tree_selection_get_mode (GtkTreeSelection* selection)
  (define-function int gtk_tree_selection_get_mode (void*))

  ;; GtkTreeSelectionFunc gtk_tree_selection_get_select_function (GtkTreeSelection* selection)
  (define-function void* gtk_tree_selection_get_select_function (void*))

  ;; gboolean gtk_tree_selection_get_selected (GtkTreeSelection* selection, GtkTreeModel** model, GtkTreeIter* iter)
  (define-function int gtk_tree_selection_get_selected (void* void* void*))

  ;; GList* gtk_tree_selection_get_selected_rows (GtkTreeSelection* selection, GtkTreeModel** model)
  (define-function void* gtk_tree_selection_get_selected_rows (void* void*))

  ;; GtkTreeView* gtk_tree_selection_get_tree_view (GtkTreeSelection* selection)
  (define-function void* gtk_tree_selection_get_tree_view (void*))

  ;; GType gtk_tree_selection_get_type (void)
  (define-function unsigned-long gtk_tree_selection_get_type ())

  ;; gpointer gtk_tree_selection_get_user_data (GtkTreeSelection* selection)
  (define-function void* gtk_tree_selection_get_user_data (void*))

  ;; gboolean gtk_tree_selection_iter_is_selected (GtkTreeSelection* selection, GtkTreeIter* iter)
  (define-function int gtk_tree_selection_iter_is_selected (void* void*))

  ;; gboolean gtk_tree_selection_path_is_selected (GtkTreeSelection* selection, GtkTreePath* path)
  (define-function int gtk_tree_selection_path_is_selected (void* void*))

  ;; void gtk_tree_selection_select_all (GtkTreeSelection* selection)
  (define-function void gtk_tree_selection_select_all (void*))

  ;; void gtk_tree_selection_select_iter (GtkTreeSelection* selection, GtkTreeIter* iter)
  (define-function void gtk_tree_selection_select_iter (void* void*))

  ;; void gtk_tree_selection_select_path (GtkTreeSelection* selection, GtkTreePath* path)
  (define-function void gtk_tree_selection_select_path (void* void*))

  ;; void gtk_tree_selection_select_range (GtkTreeSelection* selection, GtkTreePath* start_path, GtkTreePath* end_path)
  (define-function void gtk_tree_selection_select_range (void* void* void*))

  ;; void gtk_tree_selection_selected_foreach (GtkTreeSelection* selection, GtkTreeSelectionForeachFunc func, gpointer data)
  (define-function void gtk_tree_selection_selected_foreach (void* (c-callback void (void* void* void* void*)) void*))

  ;; void gtk_tree_selection_set_mode (GtkTreeSelection* selection, GtkSelectionMode type)
  (define-function void gtk_tree_selection_set_mode (void* int))

  ;; void gtk_tree_selection_set_select_function (GtkTreeSelection* selection, GtkTreeSelectionFunc func, gpointer data, GDestroyNotify destroy)
  (define-function void gtk_tree_selection_set_select_function (void* void* void* (c-callback void (void*))))

  ;; void gtk_tree_selection_unselect_all (GtkTreeSelection* selection)
  (define-function void gtk_tree_selection_unselect_all (void*))

  ;; void gtk_tree_selection_unselect_iter (GtkTreeSelection* selection, GtkTreeIter* iter)
  (define-function void gtk_tree_selection_unselect_iter (void* void*))

  ;; void gtk_tree_selection_unselect_path (GtkTreeSelection* selection, GtkTreePath* path)
  (define-function void gtk_tree_selection_unselect_path (void* void*))

  ;; void gtk_tree_selection_unselect_range (GtkTreeSelection* selection, GtkTreePath* start_path, GtkTreePath* end_path)
  (define-function void gtk_tree_selection_unselect_range (void* void* void*))

  ;; gboolean gtk_tree_set_row_drag_data (GtkSelectionData* selection_data, GtkTreeModel* tree_model, GtkTreePath* path)
  (define-function int gtk_tree_set_row_drag_data (void* void* void*))

  ;; gboolean gtk_tree_sortable_get_sort_column_id (GtkTreeSortable* sortable, gint* sort_column_id, GtkSortType* order)
  (define-function int gtk_tree_sortable_get_sort_column_id (void* void* void*))

  ;; GType gtk_tree_sortable_get_type (void)
  (define-function unsigned-long gtk_tree_sortable_get_type ())

  ;; gboolean gtk_tree_sortable_has_default_sort_func (GtkTreeSortable* sortable)
  (define-function int gtk_tree_sortable_has_default_sort_func (void*))

  ;; void gtk_tree_sortable_set_default_sort_func (GtkTreeSortable* sortable, GtkTreeIterCompareFunc sort_func, gpointer user_data, GDestroyNotify destroy)
  (define-function void gtk_tree_sortable_set_default_sort_func (void* (c-callback int (void* void* void* void*)) void* (c-callback void (void*))))

  ;; void gtk_tree_sortable_set_sort_column_id (GtkTreeSortable* sortable, gint sort_column_id, GtkSortType order)
  (define-function void gtk_tree_sortable_set_sort_column_id (void* int int))

  ;; void gtk_tree_sortable_set_sort_func (GtkTreeSortable* sortable, gint sort_column_id, GtkTreeIterCompareFunc sort_func, gpointer user_data, GDestroyNotify destroy)
  (define-function void gtk_tree_sortable_set_sort_func (void* int (c-callback int (void* void* void* void*)) void* (c-callback void (void*))))

  ;; void gtk_tree_sortable_sort_column_changed (GtkTreeSortable* sortable)
  (define-function void gtk_tree_sortable_sort_column_changed (void*))

  ;; void gtk_tree_store_append (GtkTreeStore* tree_store, GtkTreeIter* iter, GtkTreeIter* parent)
  (define-function void gtk_tree_store_append (void* void* void*))

  ;; void gtk_tree_store_clear (GtkTreeStore* tree_store)
  (define-function void gtk_tree_store_clear (void*))

  ;; GType gtk_tree_store_get_type (void)
  (define-function unsigned-long gtk_tree_store_get_type ())

  ;; void gtk_tree_store_insert (GtkTreeStore* tree_store, GtkTreeIter* iter, GtkTreeIter* parent, gint position)
  (define-function void gtk_tree_store_insert (void* void* void* int))

  ;; void gtk_tree_store_insert_after (GtkTreeStore* tree_store, GtkTreeIter* iter, GtkTreeIter* parent, GtkTreeIter* sibling)
  (define-function void gtk_tree_store_insert_after (void* void* void* void*))

  ;; void gtk_tree_store_insert_before (GtkTreeStore* tree_store, GtkTreeIter* iter, GtkTreeIter* parent, GtkTreeIter* sibling)
  (define-function void gtk_tree_store_insert_before (void* void* void* void*))

  ;; void gtk_tree_store_insert_with_values (GtkTreeStore* tree_store, GtkTreeIter* iter, GtkTreeIter* parent, gint position, ...)
  (define-function void gtk_tree_store_insert_with_values (void* void* void* int ...))

  ;; void gtk_tree_store_insert_with_valuesv (GtkTreeStore* tree_store, GtkTreeIter* iter, GtkTreeIter* parent, gint position, gint* columns, GValue* values, gint n_values)
  (define-function void gtk_tree_store_insert_with_valuesv (void* void* void* int void* void* int))

  ;; gboolean gtk_tree_store_is_ancestor (GtkTreeStore* tree_store, GtkTreeIter* iter, GtkTreeIter* descendant)
  (define-function int gtk_tree_store_is_ancestor (void* void* void*))

  ;; gint gtk_tree_store_iter_depth (GtkTreeStore* tree_store, GtkTreeIter* iter)
  (define-function int gtk_tree_store_iter_depth (void* void*))

  ;; gboolean gtk_tree_store_iter_is_valid (GtkTreeStore* tree_store, GtkTreeIter* iter)
  (define-function int gtk_tree_store_iter_is_valid (void* void*))

  ;; void gtk_tree_store_move_after (GtkTreeStore* tree_store, GtkTreeIter* iter, GtkTreeIter* position)
  (define-function void gtk_tree_store_move_after (void* void* void*))

  ;; void gtk_tree_store_move_before (GtkTreeStore* tree_store, GtkTreeIter* iter, GtkTreeIter* position)
  (define-function void gtk_tree_store_move_before (void* void* void*))

  ;; GtkTreeStore* gtk_tree_store_new (gint n_columns, ...)
  (define-function void* gtk_tree_store_new (int ...))

  ;; GtkTreeStore* gtk_tree_store_newv (gint n_columns, GType* types)
  (define-function void* gtk_tree_store_newv (int void*))

  ;; void gtk_tree_store_prepend (GtkTreeStore* tree_store, GtkTreeIter* iter, GtkTreeIter* parent)
  (define-function void gtk_tree_store_prepend (void* void* void*))

  ;; gboolean gtk_tree_store_remove (GtkTreeStore* tree_store, GtkTreeIter* iter)
  (define-function int gtk_tree_store_remove (void* void*))

  ;; void gtk_tree_store_reorder (GtkTreeStore* tree_store, GtkTreeIter* parent, gint* new_order)
  (define-function void gtk_tree_store_reorder (void* void* void*))

  ;; void gtk_tree_store_set (GtkTreeStore* tree_store, GtkTreeIter* iter, ...)
  (define-function void gtk_tree_store_set (void* void* ...))

  ;; void gtk_tree_store_set_column_types (GtkTreeStore* tree_store, gint n_columns, GType* types)
  (define-function void gtk_tree_store_set_column_types (void* int void*))

  ;; void gtk_tree_store_set_valist (GtkTreeStore* tree_store, GtkTreeIter* iter, va_list var_args)
  (define-function/va_list void gtk_tree_store_set_valist (void* void* va_list))

  ;; void gtk_tree_store_set_value (GtkTreeStore* tree_store, GtkTreeIter* iter, gint column, GValue* value)
  (define-function void gtk_tree_store_set_value (void* void* int void*))

  ;; void gtk_tree_store_set_valuesv (GtkTreeStore* tree_store, GtkTreeIter* iter, gint* columns, GValue* values, gint n_values)
  (define-function void gtk_tree_store_set_valuesv (void* void* void* void* int))

  ;; void gtk_tree_store_swap (GtkTreeStore* tree_store, GtkTreeIter* a, GtkTreeIter* b)
  (define-function void gtk_tree_store_swap (void* void* void*))

  ;; gint gtk_tree_view_append_column (GtkTreeView* tree_view, GtkTreeViewColumn* column)
  (define-function int gtk_tree_view_append_column (void* void*))

  ;; void gtk_tree_view_collapse_all (GtkTreeView* tree_view)
  (define-function void gtk_tree_view_collapse_all (void*))

  ;; gboolean gtk_tree_view_collapse_row (GtkTreeView* tree_view, GtkTreePath* path)
  (define-function int gtk_tree_view_collapse_row (void* void*))

  ;; void gtk_tree_view_column_add_attribute (GtkTreeViewColumn* tree_column, GtkCellRenderer* cell_renderer, const gchar* attribute, gint column)
  (define-function void gtk_tree_view_column_add_attribute (void* void* char* int))

  ;; gboolean gtk_tree_view_column_cell_get_position (GtkTreeViewColumn* tree_column, GtkCellRenderer* cell_renderer, gint* start_pos, gint* width)
  (define-function int gtk_tree_view_column_cell_get_position (void* void* void* void*))

  ;; void gtk_tree_view_column_cell_get_size (GtkTreeViewColumn* tree_column, const GdkRectangle* cell_area, gint* x_offset, gint* y_offset, gint* width, gint* height)
  (define-function void gtk_tree_view_column_cell_get_size (void* void* void* void* void* void*))

  ;; gboolean gtk_tree_view_column_cell_is_visible (GtkTreeViewColumn* tree_column)
  (define-function int gtk_tree_view_column_cell_is_visible (void*))

  ;; void gtk_tree_view_column_cell_set_cell_data (GtkTreeViewColumn* tree_column, GtkTreeModel* tree_model, GtkTreeIter* iter, gboolean is_expander, gboolean is_expanded)
  (define-function void gtk_tree_view_column_cell_set_cell_data (void* void* void* int int))

  ;; void gtk_tree_view_column_clear (GtkTreeViewColumn* tree_column)
  (define-function void gtk_tree_view_column_clear (void*))

  ;; void gtk_tree_view_column_clear_attributes (GtkTreeViewColumn* tree_column, GtkCellRenderer* cell_renderer)
  (define-function void gtk_tree_view_column_clear_attributes (void* void*))

  ;; void gtk_tree_view_column_clicked (GtkTreeViewColumn* tree_column)
  (define-function void gtk_tree_view_column_clicked (void*))

  ;; void gtk_tree_view_column_focus_cell (GtkTreeViewColumn* tree_column, GtkCellRenderer* cell)
  (define-function void gtk_tree_view_column_focus_cell (void* void*))

  ;; gfloat gtk_tree_view_column_get_alignment (GtkTreeViewColumn* tree_column)
  (define-function float gtk_tree_view_column_get_alignment (void*))

  ;; GList* gtk_tree_view_column_get_cell_renderers (GtkTreeViewColumn* tree_column)
  (define-function void* gtk_tree_view_column_get_cell_renderers (void*))

  ;; gboolean gtk_tree_view_column_get_clickable (GtkTreeViewColumn* tree_column)
  (define-function int gtk_tree_view_column_get_clickable (void*))

  ;; gboolean gtk_tree_view_column_get_expand (GtkTreeViewColumn* tree_column)
  (define-function int gtk_tree_view_column_get_expand (void*))

  ;; gint gtk_tree_view_column_get_fixed_width (GtkTreeViewColumn* tree_column)
  (define-function int gtk_tree_view_column_get_fixed_width (void*))

  ;; gint gtk_tree_view_column_get_max_width (GtkTreeViewColumn* tree_column)
  (define-function int gtk_tree_view_column_get_max_width (void*))

  ;; gint gtk_tree_view_column_get_min_width (GtkTreeViewColumn* tree_column)
  (define-function int gtk_tree_view_column_get_min_width (void*))

  ;; gboolean gtk_tree_view_column_get_reorderable (GtkTreeViewColumn* tree_column)
  (define-function int gtk_tree_view_column_get_reorderable (void*))

  ;; gboolean gtk_tree_view_column_get_resizable (GtkTreeViewColumn* tree_column)
  (define-function int gtk_tree_view_column_get_resizable (void*))

  ;; GtkTreeViewColumnSizing gtk_tree_view_column_get_sizing (GtkTreeViewColumn* tree_column)
  (define-function int gtk_tree_view_column_get_sizing (void*))

  ;; gint gtk_tree_view_column_get_sort_column_id (GtkTreeViewColumn* tree_column)
  (define-function int gtk_tree_view_column_get_sort_column_id (void*))

  ;; gboolean gtk_tree_view_column_get_sort_indicator (GtkTreeViewColumn* tree_column)
  (define-function int gtk_tree_view_column_get_sort_indicator (void*))

  ;; GtkSortType gtk_tree_view_column_get_sort_order (GtkTreeViewColumn* tree_column)
  (define-function int gtk_tree_view_column_get_sort_order (void*))

  ;; gint gtk_tree_view_column_get_spacing (GtkTreeViewColumn* tree_column)
  (define-function int gtk_tree_view_column_get_spacing (void*))

  ;; const gchar* gtk_tree_view_column_get_title (GtkTreeViewColumn* tree_column)
  (define-function char* gtk_tree_view_column_get_title (void*))

  ;; GtkWidget* gtk_tree_view_column_get_tree_view (GtkTreeViewColumn* tree_column)
  (define-function void* gtk_tree_view_column_get_tree_view (void*))

  ;; GType gtk_tree_view_column_get_type (void)
  (define-function unsigned-long gtk_tree_view_column_get_type ())

  ;; gboolean gtk_tree_view_column_get_visible (GtkTreeViewColumn* tree_column)
  (define-function int gtk_tree_view_column_get_visible (void*))

  ;; GtkWidget* gtk_tree_view_column_get_widget (GtkTreeViewColumn* tree_column)
  (define-function void* gtk_tree_view_column_get_widget (void*))

  ;; gint gtk_tree_view_column_get_width (GtkTreeViewColumn* tree_column)
  (define-function int gtk_tree_view_column_get_width (void*))

  ;; GtkTreeViewColumn* gtk_tree_view_column_new (void)
  (define-function void* gtk_tree_view_column_new ())

  ;; GtkTreeViewColumn* gtk_tree_view_column_new_with_attributes (const gchar* title, GtkCellRenderer* cell, ...)
  (define-function void* gtk_tree_view_column_new_with_attributes (char* void* ...))

  ;; void gtk_tree_view_column_pack_end (GtkTreeViewColumn* tree_column, GtkCellRenderer* cell, gboolean expand)
  (define-function void gtk_tree_view_column_pack_end (void* void* int))

  ;; void gtk_tree_view_column_pack_start (GtkTreeViewColumn* tree_column, GtkCellRenderer* cell, gboolean expand)
  (define-function void gtk_tree_view_column_pack_start (void* void* int))

  ;; void gtk_tree_view_column_queue_resize (GtkTreeViewColumn* tree_column)
  (define-function void gtk_tree_view_column_queue_resize (void*))

  ;; void gtk_tree_view_column_set_alignment (GtkTreeViewColumn* tree_column, gfloat xalign)
  (define-function void gtk_tree_view_column_set_alignment (void* float))

  ;; void gtk_tree_view_column_set_attributes (GtkTreeViewColumn* tree_column, GtkCellRenderer* cell_renderer, ...)
  (define-function void gtk_tree_view_column_set_attributes (void* void* ...))

  ;; void gtk_tree_view_column_set_cell_data_func (GtkTreeViewColumn* tree_column, GtkCellRenderer* cell_renderer, GtkTreeCellDataFunc func, gpointer func_data, GDestroyNotify destroy)
  (define-function void gtk_tree_view_column_set_cell_data_func (void* void* (c-callback void (void* void* void* void* void*)) void* (c-callback void (void*))))

  ;; void gtk_tree_view_column_set_clickable (GtkTreeViewColumn* tree_column, gboolean clickable)
  (define-function void gtk_tree_view_column_set_clickable (void* int))

  ;; void gtk_tree_view_column_set_expand (GtkTreeViewColumn* tree_column, gboolean expand)
  (define-function void gtk_tree_view_column_set_expand (void* int))

  ;; void gtk_tree_view_column_set_fixed_width (GtkTreeViewColumn* tree_column, gint fixed_width)
  (define-function void gtk_tree_view_column_set_fixed_width (void* int))

  ;; void gtk_tree_view_column_set_max_width (GtkTreeViewColumn* tree_column, gint max_width)
  (define-function void gtk_tree_view_column_set_max_width (void* int))

  ;; void gtk_tree_view_column_set_min_width (GtkTreeViewColumn* tree_column, gint min_width)
  (define-function void gtk_tree_view_column_set_min_width (void* int))

  ;; void gtk_tree_view_column_set_reorderable (GtkTreeViewColumn* tree_column, gboolean reorderable)
  (define-function void gtk_tree_view_column_set_reorderable (void* int))

  ;; void gtk_tree_view_column_set_resizable (GtkTreeViewColumn* tree_column, gboolean resizable)
  (define-function void gtk_tree_view_column_set_resizable (void* int))

  ;; void gtk_tree_view_column_set_sizing (GtkTreeViewColumn* tree_column, GtkTreeViewColumnSizing type)
  (define-function void gtk_tree_view_column_set_sizing (void* int))

  ;; void gtk_tree_view_column_set_sort_column_id (GtkTreeViewColumn* tree_column, gint sort_column_id)
  (define-function void gtk_tree_view_column_set_sort_column_id (void* int))

  ;; void gtk_tree_view_column_set_sort_indicator (GtkTreeViewColumn* tree_column, gboolean setting)
  (define-function void gtk_tree_view_column_set_sort_indicator (void* int))

  ;; void gtk_tree_view_column_set_sort_order (GtkTreeViewColumn* tree_column, GtkSortType order)
  (define-function void gtk_tree_view_column_set_sort_order (void* int))

  ;; void gtk_tree_view_column_set_spacing (GtkTreeViewColumn* tree_column, gint spacing)
  (define-function void gtk_tree_view_column_set_spacing (void* int))

  ;; void gtk_tree_view_column_set_title (GtkTreeViewColumn* tree_column, const gchar* title)
  (define-function void gtk_tree_view_column_set_title (void* char*))

  ;; void gtk_tree_view_column_set_visible (GtkTreeViewColumn* tree_column, gboolean visible)
  (define-function void gtk_tree_view_column_set_visible (void* int))

  ;; void gtk_tree_view_column_set_widget (GtkTreeViewColumn* tree_column, GtkWidget* widget)
  (define-function void gtk_tree_view_column_set_widget (void* void*))

  ;; GType gtk_tree_view_column_sizing_get_type (void)
  (define-function unsigned-long gtk_tree_view_column_sizing_get_type ())

  ;; void gtk_tree_view_columns_autosize (GtkTreeView* tree_view)
  (define-function void gtk_tree_view_columns_autosize (void*))

  ;; void gtk_tree_view_convert_bin_window_to_tree_coords (GtkTreeView* tree_view, gint bx, gint by, gint* tx, gint* ty)
  (define-function void gtk_tree_view_convert_bin_window_to_tree_coords (void* int int void* void*))

  ;; void gtk_tree_view_convert_bin_window_to_widget_coords (GtkTreeView* tree_view, gint bx, gint by, gint* wx, gint* wy)
  (define-function void gtk_tree_view_convert_bin_window_to_widget_coords (void* int int void* void*))

  ;; void gtk_tree_view_convert_tree_to_bin_window_coords (GtkTreeView* tree_view, gint tx, gint ty, gint* bx, gint* by)
  (define-function void gtk_tree_view_convert_tree_to_bin_window_coords (void* int int void* void*))

  ;; void gtk_tree_view_convert_tree_to_widget_coords (GtkTreeView* tree_view, gint tx, gint ty, gint* wx, gint* wy)
  (define-function void gtk_tree_view_convert_tree_to_widget_coords (void* int int void* void*))

  ;; void gtk_tree_view_convert_widget_to_bin_window_coords (GtkTreeView* tree_view, gint wx, gint wy, gint* bx, gint* by)
  (define-function void gtk_tree_view_convert_widget_to_bin_window_coords (void* int int void* void*))

  ;; void gtk_tree_view_convert_widget_to_tree_coords (GtkTreeView* tree_view, gint wx, gint wy, gint* tx, gint* ty)
  (define-function void gtk_tree_view_convert_widget_to_tree_coords (void* int int void* void*))

  ;; GdkPixmap* gtk_tree_view_create_row_drag_icon (GtkTreeView* tree_view, GtkTreePath* path)
  (define-function void* gtk_tree_view_create_row_drag_icon (void* void*))

  ;; GType gtk_tree_view_drop_position_get_type (void)
  (define-function unsigned-long gtk_tree_view_drop_position_get_type ())

  ;; void gtk_tree_view_enable_model_drag_dest (GtkTreeView* tree_view, const GtkTargetEntry* targets, gint n_targets, GdkDragAction actions)
  (define-function void gtk_tree_view_enable_model_drag_dest (void* void* int int))

  ;; void gtk_tree_view_enable_model_drag_source (GtkTreeView* tree_view, GdkModifierType start_button_mask, const GtkTargetEntry* targets, gint n_targets, GdkDragAction actions)
  (define-function void gtk_tree_view_enable_model_drag_source (void* int void* int int))

  ;; void gtk_tree_view_expand_all (GtkTreeView* tree_view)
  (define-function void gtk_tree_view_expand_all (void*))

  ;; gboolean gtk_tree_view_expand_row (GtkTreeView* tree_view, GtkTreePath* path, gboolean open_all)
  (define-function int gtk_tree_view_expand_row (void* void* int))

  ;; void gtk_tree_view_expand_to_path (GtkTreeView* tree_view, GtkTreePath* path)
  (define-function void gtk_tree_view_expand_to_path (void* void*))

  ;; void gtk_tree_view_get_background_area (GtkTreeView* tree_view, GtkTreePath* path, GtkTreeViewColumn* column, GdkRectangle* rect)
  (define-function void gtk_tree_view_get_background_area (void* void* void* void*))

  ;; GdkWindow* gtk_tree_view_get_bin_window (GtkTreeView* tree_view)
  (define-function void* gtk_tree_view_get_bin_window (void*))

  ;; void gtk_tree_view_get_cell_area (GtkTreeView* tree_view, GtkTreePath* path, GtkTreeViewColumn* column, GdkRectangle* rect)
  (define-function void gtk_tree_view_get_cell_area (void* void* void* void*))

  ;; GtkTreeViewColumn* gtk_tree_view_get_column (GtkTreeView* tree_view, gint n)
  (define-function void* gtk_tree_view_get_column (void* int))

  ;; GList* gtk_tree_view_get_columns (GtkTreeView* tree_view)
  (define-function void* gtk_tree_view_get_columns (void*))

  ;; void gtk_tree_view_get_cursor (GtkTreeView* tree_view, GtkTreePath** path, GtkTreeViewColumn** focus_column)
  (define-function void gtk_tree_view_get_cursor (void* void* void*))

  ;; gboolean gtk_tree_view_get_dest_row_at_pos (GtkTreeView* tree_view, gint drag_x, gint drag_y, GtkTreePath** path, GtkTreeViewDropPosition* pos)
  (define-function int gtk_tree_view_get_dest_row_at_pos (void* int int void* void*))

  ;; void gtk_tree_view_get_drag_dest_row (GtkTreeView* tree_view, GtkTreePath** path, GtkTreeViewDropPosition* pos)
  (define-function void gtk_tree_view_get_drag_dest_row (void* void* void*))

  ;; gboolean gtk_tree_view_get_enable_search (GtkTreeView* tree_view)
  (define-function int gtk_tree_view_get_enable_search (void*))

  ;; gboolean gtk_tree_view_get_enable_tree_lines (GtkTreeView* tree_view)
  (define-function int gtk_tree_view_get_enable_tree_lines (void*))

  ;; GtkTreeViewColumn* gtk_tree_view_get_expander_column (GtkTreeView* tree_view)
  (define-function void* gtk_tree_view_get_expander_column (void*))

  ;; gboolean gtk_tree_view_get_fixed_height_mode (GtkTreeView* tree_view)
  (define-function int gtk_tree_view_get_fixed_height_mode (void*))

  ;; GtkTreeViewGridLines gtk_tree_view_get_grid_lines (GtkTreeView* tree_view)
  (define-function int gtk_tree_view_get_grid_lines (void*))

  ;; GtkAdjustment* gtk_tree_view_get_hadjustment (GtkTreeView* tree_view)
  (define-function void* gtk_tree_view_get_hadjustment (void*))

  ;; gboolean gtk_tree_view_get_headers_clickable (GtkTreeView* tree_view)
  (define-function int gtk_tree_view_get_headers_clickable (void*))

  ;; gboolean gtk_tree_view_get_headers_visible (GtkTreeView* tree_view)
  (define-function int gtk_tree_view_get_headers_visible (void*))

  ;; gboolean gtk_tree_view_get_hover_expand (GtkTreeView* tree_view)
  (define-function int gtk_tree_view_get_hover_expand (void*))

  ;; gboolean gtk_tree_view_get_hover_selection (GtkTreeView* tree_view)
  (define-function int gtk_tree_view_get_hover_selection (void*))

  ;; gint gtk_tree_view_get_level_indentation (GtkTreeView* tree_view)
  (define-function int gtk_tree_view_get_level_indentation (void*))

  ;; GtkTreeModel* gtk_tree_view_get_model (GtkTreeView* tree_view)
  (define-function void* gtk_tree_view_get_model (void*))

  ;; gboolean gtk_tree_view_get_path_at_pos (GtkTreeView* tree_view, gint x, gint y, GtkTreePath** path, GtkTreeViewColumn** column, gint* cell_x, gint* cell_y)
  (define-function int gtk_tree_view_get_path_at_pos (void* int int void* void* void* void*))

  ;; gboolean gtk_tree_view_get_reorderable (GtkTreeView* tree_view)
  (define-function int gtk_tree_view_get_reorderable (void*))

  ;; GtkTreeViewRowSeparatorFunc gtk_tree_view_get_row_separator_func (GtkTreeView* tree_view)
  (define-function void* gtk_tree_view_get_row_separator_func (void*))

  ;; gboolean gtk_tree_view_get_rubber_banding (GtkTreeView* tree_view)
  (define-function int gtk_tree_view_get_rubber_banding (void*))

  ;; gboolean gtk_tree_view_get_rules_hint (GtkTreeView* tree_view)
  (define-function int gtk_tree_view_get_rules_hint (void*))

  ;; gint gtk_tree_view_get_search_column (GtkTreeView* tree_view)
  (define-function int gtk_tree_view_get_search_column (void*))

  ;; GtkEntry* gtk_tree_view_get_search_entry (GtkTreeView* tree_view)
  (define-function void* gtk_tree_view_get_search_entry (void*))

  ;; GtkTreeViewSearchEqualFunc gtk_tree_view_get_search_equal_func (GtkTreeView* tree_view)
  (define-function void* gtk_tree_view_get_search_equal_func (void*))

  ;; GtkTreeViewSearchPositionFunc gtk_tree_view_get_search_position_func (GtkTreeView* tree_view)
  (define-function void* gtk_tree_view_get_search_position_func (void*))

  ;; GtkTreeSelection* gtk_tree_view_get_selection (GtkTreeView* tree_view)
  (define-function void* gtk_tree_view_get_selection (void*))

  ;; gboolean gtk_tree_view_get_show_expanders (GtkTreeView* tree_view)
  (define-function int gtk_tree_view_get_show_expanders (void*))

  ;; gint gtk_tree_view_get_tooltip_column (GtkTreeView* tree_view)
  (define-function int gtk_tree_view_get_tooltip_column (void*))

  ;; gboolean gtk_tree_view_get_tooltip_context(GtkTreeView* tree_view, gint* x, gint* y, gboolean keyboard_tip, GtkTreeModel** model, GtkTreePath** path, GtkTreeIter* iter)
  (define-function int gtk_tree_view_get_tooltip_context (void* void* void* int void* void* void*))

  ;; GType gtk_tree_view_get_type (void)
  (define-function unsigned-long gtk_tree_view_get_type ())

  ;; GtkAdjustment* gtk_tree_view_get_vadjustment (GtkTreeView* tree_view)
  (define-function void* gtk_tree_view_get_vadjustment (void*))

  ;; gboolean gtk_tree_view_get_visible_range (GtkTreeView* tree_view, GtkTreePath** start_path, GtkTreePath** end_path)
  (define-function int gtk_tree_view_get_visible_range (void* void* void*))

  ;; void gtk_tree_view_get_visible_rect (GtkTreeView* tree_view, GdkRectangle* visible_rect)
  (define-function void gtk_tree_view_get_visible_rect (void* void*))

  ;; GType gtk_tree_view_grid_lines_get_type (void)
  (define-function unsigned-long gtk_tree_view_grid_lines_get_type ())

  ;; gint gtk_tree_view_insert_column (GtkTreeView* tree_view, GtkTreeViewColumn* column, gint position)
  (define-function int gtk_tree_view_insert_column (void* void* int))

  ;; gint gtk_tree_view_insert_column_with_attributes (GtkTreeView* tree_view, gint position, const gchar* title, GtkCellRenderer* cell, ...)
  (define-function int gtk_tree_view_insert_column_with_attributes (void* int char* void* ...))

  ;; gint gtk_tree_view_insert_column_with_data_func (GtkTreeView* tree_view, gint position, const gchar* title, GtkCellRenderer* cell, GtkTreeCellDataFunc func, gpointer data, GDestroyNotify dnotify)
  (define-function int gtk_tree_view_insert_column_with_data_func (void* int char* void* (c-callback void (void* void* void* void* void*)) void* (c-callback void (void*))))

  ;; gboolean gtk_tree_view_is_rubber_banding_active (GtkTreeView* tree_view)
  (define-function int gtk_tree_view_is_rubber_banding_active (void*))

  ;; void gtk_tree_view_map_expanded_rows (GtkTreeView* tree_view, GtkTreeViewMappingFunc func, gpointer data)
  (define-function void gtk_tree_view_map_expanded_rows (void* (c-callback void (void* void* void*)) void*))

  ;; GType gtk_tree_view_mode_get_type (void)
  (define-function unsigned-long gtk_tree_view_mode_get_type ())

  ;; void gtk_tree_view_move_column_after (GtkTreeView* tree_view, GtkTreeViewColumn* column, GtkTreeViewColumn* base_column)
  (define-function void gtk_tree_view_move_column_after (void* void* void*))

  ;; GtkWidget* gtk_tree_view_new (void)
  (define-function void* gtk_tree_view_new ())

  ;; GtkWidget* gtk_tree_view_new_with_model (GtkTreeModel* model)
  (define-function void* gtk_tree_view_new_with_model (void*))

  ;; gint gtk_tree_view_remove_column (GtkTreeView* tree_view, GtkTreeViewColumn* column)
  (define-function int gtk_tree_view_remove_column (void* void*))

  ;; void gtk_tree_view_row_activated (GtkTreeView* tree_view, GtkTreePath* path, GtkTreeViewColumn* column)
  (define-function void gtk_tree_view_row_activated (void* void* void*))

  ;; gboolean gtk_tree_view_row_expanded (GtkTreeView* tree_view, GtkTreePath* path)
  (define-function int gtk_tree_view_row_expanded (void* void*))

  ;; void gtk_tree_view_scroll_to_cell (GtkTreeView* tree_view, GtkTreePath* path, GtkTreeViewColumn* column, gboolean use_align, gfloat row_align, gfloat col_align)
  (define-function void gtk_tree_view_scroll_to_cell (void* void* void* int float float))

  ;; void gtk_tree_view_scroll_to_point (GtkTreeView* tree_view, gint tree_x, gint tree_y)
  (define-function void gtk_tree_view_scroll_to_point (void* int int))

  ;; void gtk_tree_view_set_column_drag_function (GtkTreeView* tree_view, GtkTreeViewColumnDropFunc func, gpointer user_data, GDestroyNotify destroy)
  (define-function void gtk_tree_view_set_column_drag_function (void* (c-callback int (void* void* void* void* void*)) void* (c-callback void (void*))))

  ;; void gtk_tree_view_set_cursor (GtkTreeView* tree_view, GtkTreePath* path, GtkTreeViewColumn* focus_column, gboolean start_editing)
  (define-function void gtk_tree_view_set_cursor (void* void* void* int))

  ;; void gtk_tree_view_set_cursor_on_cell (GtkTreeView* tree_view, GtkTreePath* path, GtkTreeViewColumn* focus_column, GtkCellRenderer* focus_cell, gboolean start_editing)
  (define-function void gtk_tree_view_set_cursor_on_cell (void* void* void* void* int))

  ;; void gtk_tree_view_set_destroy_count_func (GtkTreeView* tree_view, GtkTreeDestroyCountFunc func, gpointer data, GDestroyNotify destroy)
  (define-function void gtk_tree_view_set_destroy_count_func (void* (c-callback void (void* void* int void*)) void* (c-callback void (void*))))

  ;; void gtk_tree_view_set_drag_dest_row (GtkTreeView* tree_view, GtkTreePath* path, GtkTreeViewDropPosition pos)
  (define-function void gtk_tree_view_set_drag_dest_row (void* void* int))

  ;; void gtk_tree_view_set_enable_search (GtkTreeView* tree_view, gboolean enable_search)
  (define-function void gtk_tree_view_set_enable_search (void* int))

  ;; void gtk_tree_view_set_enable_tree_lines (GtkTreeView* tree_view, gboolean enabled)
  (define-function void gtk_tree_view_set_enable_tree_lines (void* int))

  ;; void gtk_tree_view_set_expander_column (GtkTreeView* tree_view, GtkTreeViewColumn* column)
  (define-function void gtk_tree_view_set_expander_column (void* void*))

  ;; void gtk_tree_view_set_fixed_height_mode (GtkTreeView* tree_view, gboolean enable)
  (define-function void gtk_tree_view_set_fixed_height_mode (void* int))

  ;; void gtk_tree_view_set_grid_lines (GtkTreeView* tree_view, GtkTreeViewGridLines grid_lines)
  (define-function void gtk_tree_view_set_grid_lines (void* int))

  ;; void gtk_tree_view_set_hadjustment (GtkTreeView* tree_view, GtkAdjustment* adjustment)
  (define-function void gtk_tree_view_set_hadjustment (void* void*))

  ;; void gtk_tree_view_set_headers_clickable (GtkTreeView* tree_view, gboolean setting)
  (define-function void gtk_tree_view_set_headers_clickable (void* int))

  ;; void gtk_tree_view_set_headers_visible (GtkTreeView* tree_view, gboolean headers_visible)
  (define-function void gtk_tree_view_set_headers_visible (void* int))

  ;; void gtk_tree_view_set_hover_expand (GtkTreeView* tree_view, gboolean expand)
  (define-function void gtk_tree_view_set_hover_expand (void* int))

  ;; void gtk_tree_view_set_hover_selection (GtkTreeView* tree_view, gboolean hover)
  (define-function void gtk_tree_view_set_hover_selection (void* int))

  ;; void gtk_tree_view_set_level_indentation (GtkTreeView* tree_view, gint indentation)
  (define-function void gtk_tree_view_set_level_indentation (void* int))

  ;; void gtk_tree_view_set_model (GtkTreeView* tree_view, GtkTreeModel* model)
  (define-function void gtk_tree_view_set_model (void* void*))

  ;; void gtk_tree_view_set_reorderable (GtkTreeView* tree_view, gboolean reorderable)
  (define-function void gtk_tree_view_set_reorderable (void* int))

  ;; void gtk_tree_view_set_row_separator_func (GtkTreeView* tree_view, GtkTreeViewRowSeparatorFunc func, gpointer data, GDestroyNotify destroy)
  (define-function void gtk_tree_view_set_row_separator_func (void* void* void* (c-callback void (void*))))

  ;; void gtk_tree_view_set_rubber_banding (GtkTreeView* tree_view, gboolean enable)
  (define-function void gtk_tree_view_set_rubber_banding (void* int))

  ;; void gtk_tree_view_set_rules_hint (GtkTreeView* tree_view, gboolean setting)
  (define-function void gtk_tree_view_set_rules_hint (void* int))

  ;; void gtk_tree_view_set_search_column (GtkTreeView* tree_view, gint column)
  (define-function void gtk_tree_view_set_search_column (void* int))

  ;; void gtk_tree_view_set_search_entry (GtkTreeView* tree_view, GtkEntry* entry)
  (define-function void gtk_tree_view_set_search_entry (void* void*))

  ;; void gtk_tree_view_set_search_equal_func (GtkTreeView* tree_view, GtkTreeViewSearchEqualFunc search_equal_func, gpointer search_user_data, GDestroyNotify search_destroy)
  (define-function void gtk_tree_view_set_search_equal_func (void* void* void* (c-callback void (void*))))

  ;; void gtk_tree_view_set_search_position_func (GtkTreeView* tree_view, GtkTreeViewSearchPositionFunc func, gpointer data, GDestroyNotify destroy)
  (define-function void gtk_tree_view_set_search_position_func (void* void* void* (c-callback void (void*))))

  ;; void gtk_tree_view_set_show_expanders (GtkTreeView* tree_view, gboolean enabled)
  (define-function void gtk_tree_view_set_show_expanders (void* int))

  ;; void gtk_tree_view_set_tooltip_cell (GtkTreeView* tree_view, GtkTooltip* tooltip, GtkTreePath* path, GtkTreeViewColumn* column, GtkCellRenderer* cell)
  (define-function void gtk_tree_view_set_tooltip_cell (void* void* void* void* void*))

  ;; void gtk_tree_view_set_tooltip_column (GtkTreeView* tree_view, gint column)
  (define-function void gtk_tree_view_set_tooltip_column (void* int))

  ;; void gtk_tree_view_set_tooltip_row (GtkTreeView* tree_view, GtkTooltip* tooltip, GtkTreePath* path)
  (define-function void gtk_tree_view_set_tooltip_row (void* void* void*))

  ;; void gtk_tree_view_set_vadjustment (GtkTreeView* tree_view, GtkAdjustment* adjustment)
  (define-function void gtk_tree_view_set_vadjustment (void* void*))

  ;; void gtk_tree_view_unset_rows_drag_dest (GtkTreeView* tree_view)
  (define-function void gtk_tree_view_unset_rows_drag_dest (void*))

  ;; void gtk_tree_view_unset_rows_drag_source (GtkTreeView* tree_view)
  (define-function void gtk_tree_view_unset_rows_drag_source (void*))

  ) ;[end]
