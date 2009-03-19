#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk text)

  (export gtk_text_attributes_copy
          gtk_text_attributes_copy_values
          gtk_text_attributes_get_type
          gtk_text_attributes_new
          gtk_text_attributes_ref
          gtk_text_attributes_unref
          gtk_text_buffer_add_mark
          gtk_text_buffer_add_selection_clipboard
          gtk_text_buffer_apply_tag
          gtk_text_buffer_apply_tag_by_name
          gtk_text_buffer_backspace
          gtk_text_buffer_begin_user_action
          gtk_text_buffer_copy_clipboard
          gtk_text_buffer_create_child_anchor
          gtk_text_buffer_create_mark
          gtk_text_buffer_create_tag
          gtk_text_buffer_cut_clipboard
          gtk_text_buffer_delete
          gtk_text_buffer_delete_interactive
          gtk_text_buffer_delete_mark
          gtk_text_buffer_delete_mark_by_name
          gtk_text_buffer_delete_selection
          gtk_text_buffer_deserialize
          gtk_text_buffer_deserialize_get_can_create_tags
          gtk_text_buffer_deserialize_set_can_create_tags
          gtk_text_buffer_end_user_action
          gtk_text_buffer_get_bounds
          gtk_text_buffer_get_char_count
          gtk_text_buffer_get_copy_target_list
          gtk_text_buffer_get_deserialize_formats
          gtk_text_buffer_get_end_iter
          gtk_text_buffer_get_has_selection
          gtk_text_buffer_get_insert
          gtk_text_buffer_get_iter_at_child_anchor
          gtk_text_buffer_get_iter_at_line
          gtk_text_buffer_get_iter_at_line_index
          gtk_text_buffer_get_iter_at_line_offset
          gtk_text_buffer_get_iter_at_mark
          gtk_text_buffer_get_iter_at_offset
          gtk_text_buffer_get_line_count
          gtk_text_buffer_get_mark
          gtk_text_buffer_get_modified
          gtk_text_buffer_get_paste_target_list
          gtk_text_buffer_get_selection_bound
          gtk_text_buffer_get_selection_bounds
          gtk_text_buffer_get_serialize_formats
          gtk_text_buffer_get_slice
          gtk_text_buffer_get_start_iter
          gtk_text_buffer_get_tag_table
          gtk_text_buffer_get_text
          gtk_text_buffer_get_type
          gtk_text_buffer_insert
          gtk_text_buffer_insert_at_cursor
          gtk_text_buffer_insert_child_anchor
          gtk_text_buffer_insert_interactive
          gtk_text_buffer_insert_interactive_at_cursor
          gtk_text_buffer_insert_pixbuf
          gtk_text_buffer_insert_range
          gtk_text_buffer_insert_range_interactive
          gtk_text_buffer_insert_with_tags
          gtk_text_buffer_insert_with_tags_by_name
          gtk_text_buffer_move_mark
          gtk_text_buffer_move_mark_by_name
          gtk_text_buffer_new
          gtk_text_buffer_paste_clipboard
          gtk_text_buffer_place_cursor
          gtk_text_buffer_register_deserialize_format
          gtk_text_buffer_register_deserialize_tagset
          gtk_text_buffer_register_serialize_format
          gtk_text_buffer_register_serialize_tagset
          gtk_text_buffer_remove_all_tags
          gtk_text_buffer_remove_selection_clipboard
          gtk_text_buffer_remove_tag
          gtk_text_buffer_remove_tag_by_name
          gtk_text_buffer_select_range
          gtk_text_buffer_serialize
          gtk_text_buffer_set_modified
          gtk_text_buffer_set_text
          gtk_text_buffer_target_info_get_type
          gtk_text_buffer_unregister_deserialize_format
          gtk_text_buffer_unregister_serialize_format
          gtk_text_child_anchor_get_deleted
          gtk_text_child_anchor_get_type
          gtk_text_child_anchor_get_widgets
          gtk_text_child_anchor_new
          gtk_text_direction_get_type
          gtk_text_iter_backward_char
          gtk_text_iter_backward_chars
          gtk_text_iter_backward_cursor_position
          gtk_text_iter_backward_cursor_positions
          gtk_text_iter_backward_find_char
          gtk_text_iter_backward_line
          gtk_text_iter_backward_lines
          gtk_text_iter_backward_search
          gtk_text_iter_backward_sentence_start
          gtk_text_iter_backward_sentence_starts
          gtk_text_iter_backward_to_tag_toggle
          gtk_text_iter_backward_visible_cursor_position
          gtk_text_iter_backward_visible_cursor_positions
          gtk_text_iter_backward_visible_line
          gtk_text_iter_backward_visible_lines
          gtk_text_iter_backward_visible_word_start
          gtk_text_iter_backward_visible_word_starts
          gtk_text_iter_backward_word_start
          gtk_text_iter_backward_word_starts
          gtk_text_iter_begins_tag
          gtk_text_iter_can_insert
          gtk_text_iter_compare
          gtk_text_iter_copy
          gtk_text_iter_editable
          gtk_text_iter_ends_line
          gtk_text_iter_ends_sentence
          gtk_text_iter_ends_tag
          gtk_text_iter_ends_word
          gtk_text_iter_equal
          gtk_text_iter_forward_char
          gtk_text_iter_forward_chars
          gtk_text_iter_forward_cursor_position
          gtk_text_iter_forward_cursor_positions
          gtk_text_iter_forward_find_char
          gtk_text_iter_forward_line
          gtk_text_iter_forward_lines
          gtk_text_iter_forward_search
          gtk_text_iter_forward_sentence_end
          gtk_text_iter_forward_sentence_ends
          gtk_text_iter_forward_to_end
          gtk_text_iter_forward_to_line_end
          gtk_text_iter_forward_to_tag_toggle
          gtk_text_iter_forward_visible_cursor_position
          gtk_text_iter_forward_visible_cursor_positions
          gtk_text_iter_forward_visible_line
          gtk_text_iter_forward_visible_lines
          gtk_text_iter_forward_visible_word_end
          gtk_text_iter_forward_visible_word_ends
          gtk_text_iter_forward_word_end
          gtk_text_iter_forward_word_ends
          gtk_text_iter_free
          gtk_text_iter_get_attributes
          gtk_text_iter_get_buffer
          gtk_text_iter_get_bytes_in_line
          gtk_text_iter_get_char
          gtk_text_iter_get_chars_in_line
          gtk_text_iter_get_child_anchor
          gtk_text_iter_get_language
          gtk_text_iter_get_line
          gtk_text_iter_get_line_index
          gtk_text_iter_get_line_offset
          gtk_text_iter_get_marks
          gtk_text_iter_get_offset
          gtk_text_iter_get_pixbuf
          gtk_text_iter_get_slice
          gtk_text_iter_get_tags
          gtk_text_iter_get_text
          gtk_text_iter_get_toggled_tags
          gtk_text_iter_get_type
          gtk_text_iter_get_visible_line_index
          gtk_text_iter_get_visible_line_offset
          gtk_text_iter_get_visible_slice
          gtk_text_iter_get_visible_text
          gtk_text_iter_has_tag
          gtk_text_iter_in_range
          gtk_text_iter_inside_sentence
          gtk_text_iter_inside_word
          gtk_text_iter_is_cursor_position
          gtk_text_iter_is_end
          gtk_text_iter_is_start
          gtk_text_iter_order
          gtk_text_iter_set_line
          gtk_text_iter_set_line_index
          gtk_text_iter_set_line_offset
          gtk_text_iter_set_offset
          gtk_text_iter_set_visible_line_index
          gtk_text_iter_set_visible_line_offset
          gtk_text_iter_starts_line
          gtk_text_iter_starts_sentence
          gtk_text_iter_starts_word
          gtk_text_iter_toggles_tag
          gtk_text_mark_get_buffer
          gtk_text_mark_get_deleted
          gtk_text_mark_get_left_gravity
          gtk_text_mark_get_name
          gtk_text_mark_get_type
          gtk_text_mark_get_visible
          gtk_text_mark_new
          gtk_text_mark_set_visible
          gtk_text_search_flags_get_type
          gtk_text_tag_event
          gtk_text_tag_get_priority
          gtk_text_tag_get_type
          gtk_text_tag_new
          gtk_text_tag_set_priority
          gtk_text_tag_table_add
          gtk_text_tag_table_foreach
          gtk_text_tag_table_get_size
          gtk_text_tag_table_get_type
          gtk_text_tag_table_lookup
          gtk_text_tag_table_new
          gtk_text_tag_table_remove
          gtk_text_view_add_child_at_anchor
          gtk_text_view_add_child_in_window
          gtk_text_view_backward_display_line
          gtk_text_view_backward_display_line_start
          gtk_text_view_buffer_to_window_coords
          gtk_text_view_forward_display_line
          gtk_text_view_forward_display_line_end
          gtk_text_view_get_accepts_tab
          gtk_text_view_get_border_window_size
          gtk_text_view_get_buffer
          gtk_text_view_get_cursor_visible
          gtk_text_view_get_default_attributes
          gtk_text_view_get_editable
          gtk_text_view_get_indent
          gtk_text_view_get_iter_at_location
          gtk_text_view_get_iter_at_position
          gtk_text_view_get_iter_location
          gtk_text_view_get_justification
          gtk_text_view_get_left_margin
          gtk_text_view_get_line_at_y
          gtk_text_view_get_line_yrange
          gtk_text_view_get_overwrite
          gtk_text_view_get_pixels_above_lines
          gtk_text_view_get_pixels_below_lines
          gtk_text_view_get_pixels_inside_wrap
          gtk_text_view_get_right_margin
          gtk_text_view_get_tabs
          gtk_text_view_get_type
          gtk_text_view_get_visible_rect
          gtk_text_view_get_window
          gtk_text_view_get_window_type
          gtk_text_view_get_wrap_mode
          gtk_text_view_move_child
          gtk_text_view_move_mark_onscreen
          gtk_text_view_move_visually
          gtk_text_view_new
          gtk_text_view_new_with_buffer
          gtk_text_view_place_cursor_onscreen
          gtk_text_view_scroll_mark_onscreen
          gtk_text_view_scroll_to_iter
          gtk_text_view_scroll_to_mark
          gtk_text_view_set_accepts_tab
          gtk_text_view_set_border_window_size
          gtk_text_view_set_buffer
          gtk_text_view_set_cursor_visible
          gtk_text_view_set_editable
          gtk_text_view_set_indent
          gtk_text_view_set_justification
          gtk_text_view_set_left_margin
          gtk_text_view_set_overwrite
          gtk_text_view_set_pixels_above_lines
          gtk_text_view_set_pixels_below_lines
          gtk_text_view_set_pixels_inside_wrap
          gtk_text_view_set_right_margin
          gtk_text_view_set_tabs
          gtk_text_view_set_wrap_mode
          gtk_text_view_starts_display_line
          gtk_text_view_window_to_buffer_coords
          gtk_text_window_type_get_type)

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

  ;; GtkTextAttributes* gtk_text_attributes_copy (GtkTextAttributes* src)
  (define-function void* gtk_text_attributes_copy (void*))

  ;; void gtk_text_attributes_copy_values (GtkTextAttributes* src, GtkTextAttributes* dest)
  (define-function void gtk_text_attributes_copy_values (void* void*))

  ;; GType gtk_text_attributes_get_type (void)
  (define-function unsigned-long gtk_text_attributes_get_type ())

  ;; GtkTextAttributes* gtk_text_attributes_new (void)
  (define-function void* gtk_text_attributes_new ())

  ;; GtkTextAttributes* gtk_text_attributes_ref (GtkTextAttributes* values)
  (define-function void* gtk_text_attributes_ref (void*))

  ;; void gtk_text_attributes_unref (GtkTextAttributes* values)
  (define-function void gtk_text_attributes_unref (void*))

  ;; void gtk_text_buffer_add_mark (GtkTextBuffer* buffer, GtkTextMark* mark, const GtkTextIter* where)
  (define-function void gtk_text_buffer_add_mark (void* void* void*))

  ;; void gtk_text_buffer_add_selection_clipboard (GtkTextBuffer* buffer, GtkClipboard* clipboard)
  (define-function void gtk_text_buffer_add_selection_clipboard (void* void*))

  ;; void gtk_text_buffer_apply_tag (GtkTextBuffer* buffer, GtkTextTag* tag, const GtkTextIter* start, const GtkTextIter* end)
  (define-function void gtk_text_buffer_apply_tag (void* void* void* void*))

  ;; void gtk_text_buffer_apply_tag_by_name (GtkTextBuffer* buffer, const gchar* name, const GtkTextIter* start, const GtkTextIter* end)
  (define-function void gtk_text_buffer_apply_tag_by_name (void* char* void* void*))

  ;; gboolean gtk_text_buffer_backspace (GtkTextBuffer* buffer, GtkTextIter* iter, gboolean interactive, gboolean default_editable)
  (define-function int gtk_text_buffer_backspace (void* void* int int))

  ;; void gtk_text_buffer_begin_user_action (GtkTextBuffer* buffer)
  (define-function void gtk_text_buffer_begin_user_action (void*))

  ;; void gtk_text_buffer_copy_clipboard (GtkTextBuffer* buffer, GtkClipboard* clipboard)
  (define-function void gtk_text_buffer_copy_clipboard (void* void*))

  ;; GtkTextChildAnchor* gtk_text_buffer_create_child_anchor (GtkTextBuffer* buffer, GtkTextIter* iter)
  (define-function void* gtk_text_buffer_create_child_anchor (void* void*))

  ;; GtkTextMark* gtk_text_buffer_create_mark (GtkTextBuffer* buffer, const gchar* mark_name, const GtkTextIter* where, gboolean left_gravity)
  (define-function void* gtk_text_buffer_create_mark (void* char* void* int))

  ;; GtkTextTag* gtk_text_buffer_create_tag (GtkTextBuffer* buffer, const gchar* tag_name, const gchar* first_property_name, ...)
  (define-function void* gtk_text_buffer_create_tag (void* char* char* ...))

  ;; void gtk_text_buffer_cut_clipboard (GtkTextBuffer* buffer, GtkClipboard* clipboard, gboolean default_editable)
  (define-function void gtk_text_buffer_cut_clipboard (void* void* int))

  ;; void gtk_text_buffer_delete (GtkTextBuffer* buffer, GtkTextIter* start, GtkTextIter* end)
  (define-function void gtk_text_buffer_delete (void* void* void*))

  ;; gboolean gtk_text_buffer_delete_interactive (GtkTextBuffer* buffer, GtkTextIter* start_iter, GtkTextIter* end_iter, gboolean default_editable)
  (define-function int gtk_text_buffer_delete_interactive (void* void* void* int))

  ;; void gtk_text_buffer_delete_mark (GtkTextBuffer* buffer, GtkTextMark* mark)
  (define-function void gtk_text_buffer_delete_mark (void* void*))

  ;; void gtk_text_buffer_delete_mark_by_name (GtkTextBuffer* buffer, const gchar* name)
  (define-function void gtk_text_buffer_delete_mark_by_name (void* char*))

  ;; gboolean gtk_text_buffer_delete_selection (GtkTextBuffer* buffer, gboolean interactive, gboolean default_editable)
  (define-function int gtk_text_buffer_delete_selection (void* int int))

  ;; gboolean gtk_text_buffer_deserialize (GtkTextBuffer* register_buffer, GtkTextBuffer* content_buffer, GdkAtom format, GtkTextIter* iter, const guint8* data, gsize length, GError** error)
  (define-function int gtk_text_buffer_deserialize (void* void* void* void* void* unsigned-long void*))

  ;; gboolean gtk_text_buffer_deserialize_get_can_create_tags (GtkTextBuffer* buffer, GdkAtom format)
  (define-function int gtk_text_buffer_deserialize_get_can_create_tags (void* void*))

  ;; void gtk_text_buffer_deserialize_set_can_create_tags (GtkTextBuffer* buffer, GdkAtom format, gboolean can_create_tags)
  (define-function void gtk_text_buffer_deserialize_set_can_create_tags (void* void* int))

  ;; void gtk_text_buffer_end_user_action (GtkTextBuffer* buffer)
  (define-function void gtk_text_buffer_end_user_action (void*))

  ;; void gtk_text_buffer_get_bounds (GtkTextBuffer* buffer, GtkTextIter* start, GtkTextIter* end)
  (define-function void gtk_text_buffer_get_bounds (void* void* void*))

  ;; gint gtk_text_buffer_get_char_count (GtkTextBuffer* buffer)
  (define-function int gtk_text_buffer_get_char_count (void*))

  ;; GtkTargetList* gtk_text_buffer_get_copy_target_list (GtkTextBuffer* buffer)
  (define-function void* gtk_text_buffer_get_copy_target_list (void*))

  ;; GdkAtom* gtk_text_buffer_get_deserialize_formats (GtkTextBuffer* buffer, gint* n_formats)
  (define-function void* gtk_text_buffer_get_deserialize_formats (void* void*))

  ;; void gtk_text_buffer_get_end_iter (GtkTextBuffer* buffer, GtkTextIter* iter)
  (define-function void gtk_text_buffer_get_end_iter (void* void*))

  ;; gboolean gtk_text_buffer_get_has_selection (GtkTextBuffer* buffer)
  (define-function int gtk_text_buffer_get_has_selection (void*))

  ;; GtkTextMark* gtk_text_buffer_get_insert (GtkTextBuffer* buffer)
  (define-function void* gtk_text_buffer_get_insert (void*))

  ;; void gtk_text_buffer_get_iter_at_child_anchor (GtkTextBuffer* buffer, GtkTextIter* iter, GtkTextChildAnchor* anchor)
  (define-function void gtk_text_buffer_get_iter_at_child_anchor (void* void* void*))

  ;; void gtk_text_buffer_get_iter_at_line (GtkTextBuffer* buffer, GtkTextIter* iter, gint line_number)
  (define-function void gtk_text_buffer_get_iter_at_line (void* void* int))

  ;; void gtk_text_buffer_get_iter_at_line_index (GtkTextBuffer* buffer, GtkTextIter* iter, gint line_number, gint byte_index)
  (define-function void gtk_text_buffer_get_iter_at_line_index (void* void* int int))

  ;; void gtk_text_buffer_get_iter_at_line_offset (GtkTextBuffer* buffer, GtkTextIter* iter, gint line_number, gint char_offset)
  (define-function void gtk_text_buffer_get_iter_at_line_offset (void* void* int int))

  ;; void gtk_text_buffer_get_iter_at_mark (GtkTextBuffer* buffer, GtkTextIter* iter, GtkTextMark* mark)
  (define-function void gtk_text_buffer_get_iter_at_mark (void* void* void*))

  ;; void gtk_text_buffer_get_iter_at_offset (GtkTextBuffer* buffer, GtkTextIter* iter, gint char_offset)
  (define-function void gtk_text_buffer_get_iter_at_offset (void* void* int))

  ;; gint gtk_text_buffer_get_line_count (GtkTextBuffer* buffer)
  (define-function int gtk_text_buffer_get_line_count (void*))

  ;; GtkTextMark* gtk_text_buffer_get_mark (GtkTextBuffer* buffer, const gchar* name)
  (define-function void* gtk_text_buffer_get_mark (void* char*))

  ;; gboolean gtk_text_buffer_get_modified (GtkTextBuffer* buffer)
  (define-function int gtk_text_buffer_get_modified (void*))

  ;; GtkTargetList* gtk_text_buffer_get_paste_target_list (GtkTextBuffer* buffer)
  (define-function void* gtk_text_buffer_get_paste_target_list (void*))

  ;; GtkTextMark* gtk_text_buffer_get_selection_bound (GtkTextBuffer* buffer)
  (define-function void* gtk_text_buffer_get_selection_bound (void*))

  ;; gboolean gtk_text_buffer_get_selection_bounds (GtkTextBuffer* buffer, GtkTextIter* start, GtkTextIter* end)
  (define-function int gtk_text_buffer_get_selection_bounds (void* void* void*))

  ;; GdkAtom* gtk_text_buffer_get_serialize_formats (GtkTextBuffer* buffer, gint* n_formats)
  (define-function void* gtk_text_buffer_get_serialize_formats (void* void*))

  ;; gchar* gtk_text_buffer_get_slice (GtkTextBuffer* buffer, const GtkTextIter* start, const GtkTextIter* end, gboolean include_hidden_chars)
  (define-function char* gtk_text_buffer_get_slice (void* void* void* int))

  ;; void gtk_text_buffer_get_start_iter (GtkTextBuffer* buffer, GtkTextIter* iter)
  (define-function void gtk_text_buffer_get_start_iter (void* void*))

  ;; GtkTextTagTable* gtk_text_buffer_get_tag_table (GtkTextBuffer* buffer)
  (define-function void* gtk_text_buffer_get_tag_table (void*))

  ;; gchar* gtk_text_buffer_get_text (GtkTextBuffer* buffer, const GtkTextIter* start, const GtkTextIter* end, gboolean include_hidden_chars)
  (define-function char* gtk_text_buffer_get_text (void* void* void* int))

  ;; GType gtk_text_buffer_get_type (void)
  (define-function unsigned-long gtk_text_buffer_get_type ())

  ;; void gtk_text_buffer_insert (GtkTextBuffer* buffer, GtkTextIter* iter, const gchar* text, gint len)
  (define-function void gtk_text_buffer_insert (void* void* char* int))

  ;; void gtk_text_buffer_insert_at_cursor (GtkTextBuffer* buffer, const gchar* text, gint len)
  (define-function void gtk_text_buffer_insert_at_cursor (void* char* int))

  ;; void gtk_text_buffer_insert_child_anchor (GtkTextBuffer* buffer, GtkTextIter* iter, GtkTextChildAnchor* anchor)
  (define-function void gtk_text_buffer_insert_child_anchor (void* void* void*))

  ;; gboolean gtk_text_buffer_insert_interactive (GtkTextBuffer* buffer, GtkTextIter* iter, const gchar* text, gint len, gboolean default_editable)
  (define-function int gtk_text_buffer_insert_interactive (void* void* char* int int))

  ;; gboolean gtk_text_buffer_insert_interactive_at_cursor (GtkTextBuffer* buffer, const gchar* text, gint len, gboolean default_editable)
  (define-function int gtk_text_buffer_insert_interactive_at_cursor (void* char* int int))

  ;; void gtk_text_buffer_insert_pixbuf (GtkTextBuffer* buffer, GtkTextIter* iter, GdkPixbuf* pixbuf)
  (define-function void gtk_text_buffer_insert_pixbuf (void* void* void*))

  ;; void gtk_text_buffer_insert_range (GtkTextBuffer* buffer, GtkTextIter* iter, const GtkTextIter* start, const GtkTextIter* end)
  (define-function void gtk_text_buffer_insert_range (void* void* void* void*))

  ;; gboolean gtk_text_buffer_insert_range_interactive (GtkTextBuffer* buffer, GtkTextIter* iter, const GtkTextIter* start, const GtkTextIter* end, gboolean default_editable)
  (define-function int gtk_text_buffer_insert_range_interactive (void* void* void* void* int))

  ;; void gtk_text_buffer_insert_with_tags (GtkTextBuffer* buffer, GtkTextIter* iter, const gchar* text, gint len, GtkTextTag* first_tag, ...)
  (define-function void gtk_text_buffer_insert_with_tags (void* void* char* int void* ...))

  ;; void gtk_text_buffer_insert_with_tags_by_name (GtkTextBuffer* buffer, GtkTextIter* iter, const gchar* text, gint len, const gchar* first_tag_name, ...)
  (define-function void gtk_text_buffer_insert_with_tags_by_name (void* void* char* int char* ...))

  ;; void gtk_text_buffer_move_mark (GtkTextBuffer* buffer, GtkTextMark* mark, const GtkTextIter* where)
  (define-function void gtk_text_buffer_move_mark (void* void* void*))

  ;; void gtk_text_buffer_move_mark_by_name (GtkTextBuffer* buffer, const gchar* name, const GtkTextIter* where)
  (define-function void gtk_text_buffer_move_mark_by_name (void* char* void*))

  ;; GtkTextBuffer* gtk_text_buffer_new (GtkTextTagTable* table)
  (define-function void* gtk_text_buffer_new (void*))

  ;; void gtk_text_buffer_paste_clipboard (GtkTextBuffer* buffer, GtkClipboard* clipboard, GtkTextIter* override_location, gboolean default_editable)
  (define-function void gtk_text_buffer_paste_clipboard (void* void* void* int))

  ;; void gtk_text_buffer_place_cursor (GtkTextBuffer* buffer, const GtkTextIter* where)
  (define-function void gtk_text_buffer_place_cursor (void* void*))

  ;; GdkAtom gtk_text_buffer_register_deserialize_format (GtkTextBuffer* buffer, const gchar* mime_type, GtkTextBufferDeserializeFunc function, gpointer user_data, GDestroyNotify user_data_destroy)
  (define-function void* gtk_text_buffer_register_deserialize_format (void* char* (c-callback int (void* void* void* void* unsigned-long int void* void*)) void* (c-callback void (void*))))

  ;; GdkAtom gtk_text_buffer_register_deserialize_tagset (GtkTextBuffer* buffer, const gchar* tagset_name)
  (define-function void* gtk_text_buffer_register_deserialize_tagset (void* char*))

  ;; GdkAtom gtk_text_buffer_register_serialize_format (GtkTextBuffer* buffer, const gchar* mime_type, GtkTextBufferSerializeFunc function, gpointer user_data, GDestroyNotify user_data_destroy)
  (define-function void* gtk_text_buffer_register_serialize_format (void* char* (c-callback void* (void* void* void* void* void* void*)) void* (c-callback void (void*))))

  ;; GdkAtom gtk_text_buffer_register_serialize_tagset (GtkTextBuffer* buffer, const gchar* tagset_name)
  (define-function void* gtk_text_buffer_register_serialize_tagset (void* char*))

  ;; void gtk_text_buffer_remove_all_tags (GtkTextBuffer* buffer, const GtkTextIter* start, const GtkTextIter* end)
  (define-function void gtk_text_buffer_remove_all_tags (void* void* void*))

  ;; void gtk_text_buffer_remove_selection_clipboard (GtkTextBuffer* buffer, GtkClipboard* clipboard)
  (define-function void gtk_text_buffer_remove_selection_clipboard (void* void*))

  ;; void gtk_text_buffer_remove_tag (GtkTextBuffer* buffer, GtkTextTag* tag, const GtkTextIter* start, const GtkTextIter* end)
  (define-function void gtk_text_buffer_remove_tag (void* void* void* void*))

  ;; void gtk_text_buffer_remove_tag_by_name (GtkTextBuffer* buffer, const gchar* name, const GtkTextIter* start, const GtkTextIter* end)
  (define-function void gtk_text_buffer_remove_tag_by_name (void* char* void* void*))

  ;; void gtk_text_buffer_select_range (GtkTextBuffer* buffer, const GtkTextIter* ins, const GtkTextIter* bound)
  (define-function void gtk_text_buffer_select_range (void* void* void*))

  ;; guint8* gtk_text_buffer_serialize (GtkTextBuffer* register_buffer, GtkTextBuffer* content_buffer, GdkAtom format, const GtkTextIter* start, const GtkTextIter* end, gsize* length)
  (define-function void* gtk_text_buffer_serialize (void* void* void* void* void* void*))

  ;; void gtk_text_buffer_set_modified (GtkTextBuffer* buffer, gboolean setting)
  (define-function void gtk_text_buffer_set_modified (void* int))

  ;; void gtk_text_buffer_set_text (GtkTextBuffer* buffer, const gchar* text, gint len)
  (define-function void gtk_text_buffer_set_text (void* char* int))

  ;; GType gtk_text_buffer_target_info_get_type (void)
  (define-function unsigned-long gtk_text_buffer_target_info_get_type ())

  ;; void gtk_text_buffer_unregister_deserialize_format (GtkTextBuffer* buffer, GdkAtom format)
  (define-function void gtk_text_buffer_unregister_deserialize_format (void* void*))

  ;; void gtk_text_buffer_unregister_serialize_format (GtkTextBuffer* buffer, GdkAtom format)
  (define-function void gtk_text_buffer_unregister_serialize_format (void* void*))

  ;; gboolean gtk_text_child_anchor_get_deleted (GtkTextChildAnchor* anchor)
  (define-function int gtk_text_child_anchor_get_deleted (void*))

  ;; GType gtk_text_child_anchor_get_type (void)
  (define-function unsigned-long gtk_text_child_anchor_get_type ())

  ;; GList* gtk_text_child_anchor_get_widgets (GtkTextChildAnchor* anchor)
  (define-function void* gtk_text_child_anchor_get_widgets (void*))

  ;; GtkTextChildAnchor* gtk_text_child_anchor_new (void)
  (define-function void* gtk_text_child_anchor_new ())

  ;; GType gtk_text_direction_get_type (void)
  (define-function unsigned-long gtk_text_direction_get_type ())

  ;; gboolean gtk_text_iter_backward_char (GtkTextIter* iter)
  (define-function int gtk_text_iter_backward_char (void*))

  ;; gboolean gtk_text_iter_backward_chars (GtkTextIter* iter, gint count)
  (define-function int gtk_text_iter_backward_chars (void* int))

  ;; gboolean gtk_text_iter_backward_cursor_position (GtkTextIter* iter)
  (define-function int gtk_text_iter_backward_cursor_position (void*))

  ;; gboolean gtk_text_iter_backward_cursor_positions (GtkTextIter* iter, gint count)
  (define-function int gtk_text_iter_backward_cursor_positions (void* int))

  ;; gboolean gtk_text_iter_backward_find_char (GtkTextIter* iter, GtkTextCharPredicate pred, gpointer user_data, const GtkTextIter* limit)
  (define-function int gtk_text_iter_backward_find_char (void* (c-callback int (uint32_t void*)) void* void*))

  ;; gboolean gtk_text_iter_backward_line (GtkTextIter* iter)
  (define-function int gtk_text_iter_backward_line (void*))

  ;; gboolean gtk_text_iter_backward_lines (GtkTextIter* iter, gint count)
  (define-function int gtk_text_iter_backward_lines (void* int))

  ;; gboolean gtk_text_iter_backward_search (const GtkTextIter* iter, const gchar* str, GtkTextSearchFlags flags, GtkTextIter* match_start, GtkTextIter* match_end, const GtkTextIter* limit)
  (define-function int gtk_text_iter_backward_search (void* char* int void* void* void*))

  ;; gboolean gtk_text_iter_backward_sentence_start (GtkTextIter* iter)
  (define-function int gtk_text_iter_backward_sentence_start (void*))

  ;; gboolean gtk_text_iter_backward_sentence_starts (GtkTextIter* iter, gint count)
  (define-function int gtk_text_iter_backward_sentence_starts (void* int))

  ;; gboolean gtk_text_iter_backward_to_tag_toggle (GtkTextIter* iter, GtkTextTag* tag)
  (define-function int gtk_text_iter_backward_to_tag_toggle (void* void*))

  ;; gboolean gtk_text_iter_backward_visible_cursor_position (GtkTextIter* iter)
  (define-function int gtk_text_iter_backward_visible_cursor_position (void*))

  ;; gboolean gtk_text_iter_backward_visible_cursor_positions (GtkTextIter* iter, gint count)
  (define-function int gtk_text_iter_backward_visible_cursor_positions (void* int))

  ;; gboolean gtk_text_iter_backward_visible_line (GtkTextIter* iter)
  (define-function int gtk_text_iter_backward_visible_line (void*))

  ;; gboolean gtk_text_iter_backward_visible_lines (GtkTextIter* iter, gint count)
  (define-function int gtk_text_iter_backward_visible_lines (void* int))

  ;; gboolean gtk_text_iter_backward_visible_word_start (GtkTextIter* iter)
  (define-function int gtk_text_iter_backward_visible_word_start (void*))

  ;; gboolean gtk_text_iter_backward_visible_word_starts (GtkTextIter* iter, gint count)
  (define-function int gtk_text_iter_backward_visible_word_starts (void* int))

  ;; gboolean gtk_text_iter_backward_word_start (GtkTextIter* iter)
  (define-function int gtk_text_iter_backward_word_start (void*))

  ;; gboolean gtk_text_iter_backward_word_starts (GtkTextIter* iter, gint count)
  (define-function int gtk_text_iter_backward_word_starts (void* int))

  ;; gboolean gtk_text_iter_begins_tag (const GtkTextIter* iter, GtkTextTag* tag)
  (define-function int gtk_text_iter_begins_tag (void* void*))

  ;; gboolean gtk_text_iter_can_insert (const GtkTextIter* iter, gboolean default_editability)
  (define-function int gtk_text_iter_can_insert (void* int))

  ;; gint gtk_text_iter_compare (const GtkTextIter* lhs, const GtkTextIter* rhs)
  (define-function int gtk_text_iter_compare (void* void*))

  ;; GtkTextIter* gtk_text_iter_copy (const GtkTextIter* iter)
  (define-function void* gtk_text_iter_copy (void*))

  ;; gboolean gtk_text_iter_editable (const GtkTextIter* iter, gboolean default_setting)
  (define-function int gtk_text_iter_editable (void* int))

  ;; gboolean gtk_text_iter_ends_line (const GtkTextIter* iter)
  (define-function int gtk_text_iter_ends_line (void*))

  ;; gboolean gtk_text_iter_ends_sentence (const GtkTextIter* iter)
  (define-function int gtk_text_iter_ends_sentence (void*))

  ;; gboolean gtk_text_iter_ends_tag (const GtkTextIter* iter, GtkTextTag* tag)
  (define-function int gtk_text_iter_ends_tag (void* void*))

  ;; gboolean gtk_text_iter_ends_word (const GtkTextIter* iter)
  (define-function int gtk_text_iter_ends_word (void*))

  ;; gboolean gtk_text_iter_equal (const GtkTextIter* lhs, const GtkTextIter* rhs)
  (define-function int gtk_text_iter_equal (void* void*))

  ;; gboolean gtk_text_iter_forward_char (GtkTextIter* iter)
  (define-function int gtk_text_iter_forward_char (void*))

  ;; gboolean gtk_text_iter_forward_chars (GtkTextIter* iter, gint count)
  (define-function int gtk_text_iter_forward_chars (void* int))

  ;; gboolean gtk_text_iter_forward_cursor_position (GtkTextIter* iter)
  (define-function int gtk_text_iter_forward_cursor_position (void*))

  ;; gboolean gtk_text_iter_forward_cursor_positions (GtkTextIter* iter, gint count)
  (define-function int gtk_text_iter_forward_cursor_positions (void* int))

  ;; gboolean gtk_text_iter_forward_find_char (GtkTextIter* iter, GtkTextCharPredicate pred, gpointer user_data, const GtkTextIter* limit)
  (define-function int gtk_text_iter_forward_find_char (void* (c-callback int (uint32_t void*)) void* void*))

  ;; gboolean gtk_text_iter_forward_line (GtkTextIter* iter)
  (define-function int gtk_text_iter_forward_line (void*))

  ;; gboolean gtk_text_iter_forward_lines (GtkTextIter* iter, gint count)
  (define-function int gtk_text_iter_forward_lines (void* int))

  ;; gboolean gtk_text_iter_forward_search (const GtkTextIter* iter, const gchar* str, GtkTextSearchFlags flags, GtkTextIter* match_start, GtkTextIter* match_end, const GtkTextIter* limit)
  (define-function int gtk_text_iter_forward_search (void* char* int void* void* void*))

  ;; gboolean gtk_text_iter_forward_sentence_end (GtkTextIter* iter)
  (define-function int gtk_text_iter_forward_sentence_end (void*))

  ;; gboolean gtk_text_iter_forward_sentence_ends (GtkTextIter* iter, gint count)
  (define-function int gtk_text_iter_forward_sentence_ends (void* int))

  ;; void gtk_text_iter_forward_to_end (GtkTextIter* iter)
  (define-function void gtk_text_iter_forward_to_end (void*))

  ;; gboolean gtk_text_iter_forward_to_line_end (GtkTextIter* iter)
  (define-function int gtk_text_iter_forward_to_line_end (void*))

  ;; gboolean gtk_text_iter_forward_to_tag_toggle (GtkTextIter* iter, GtkTextTag* tag)
  (define-function int gtk_text_iter_forward_to_tag_toggle (void* void*))

  ;; gboolean gtk_text_iter_forward_visible_cursor_position (GtkTextIter* iter)
  (define-function int gtk_text_iter_forward_visible_cursor_position (void*))

  ;; gboolean gtk_text_iter_forward_visible_cursor_positions (GtkTextIter* iter, gint count)
  (define-function int gtk_text_iter_forward_visible_cursor_positions (void* int))

  ;; gboolean gtk_text_iter_forward_visible_line (GtkTextIter* iter)
  (define-function int gtk_text_iter_forward_visible_line (void*))

  ;; gboolean gtk_text_iter_forward_visible_lines (GtkTextIter* iter, gint count)
  (define-function int gtk_text_iter_forward_visible_lines (void* int))

  ;; gboolean gtk_text_iter_forward_visible_word_end (GtkTextIter* iter)
  (define-function int gtk_text_iter_forward_visible_word_end (void*))

  ;; gboolean gtk_text_iter_forward_visible_word_ends (GtkTextIter* iter, gint count)
  (define-function int gtk_text_iter_forward_visible_word_ends (void* int))

  ;; gboolean gtk_text_iter_forward_word_end (GtkTextIter* iter)
  (define-function int gtk_text_iter_forward_word_end (void*))

  ;; gboolean gtk_text_iter_forward_word_ends (GtkTextIter* iter, gint count)
  (define-function int gtk_text_iter_forward_word_ends (void* int))

  ;; void gtk_text_iter_free (GtkTextIter* iter)
  (define-function void gtk_text_iter_free (void*))

  ;; gboolean gtk_text_iter_get_attributes (const GtkTextIter* iter, GtkTextAttributes* values)
  (define-function int gtk_text_iter_get_attributes (void* void*))

  ;; GtkTextBuffer* gtk_text_iter_get_buffer (const GtkTextIter* iter)
  (define-function void* gtk_text_iter_get_buffer (void*))

  ;; gint gtk_text_iter_get_bytes_in_line (const GtkTextIter* iter)
  (define-function int gtk_text_iter_get_bytes_in_line (void*))

  ;; gunichar gtk_text_iter_get_char (const GtkTextIter* iter)
  (define-function uint32_t gtk_text_iter_get_char (void*))

  ;; gint gtk_text_iter_get_chars_in_line (const GtkTextIter* iter)
  (define-function int gtk_text_iter_get_chars_in_line (void*))

  ;; GtkTextChildAnchor* gtk_text_iter_get_child_anchor (const GtkTextIter* iter)
  (define-function void* gtk_text_iter_get_child_anchor (void*))

  ;; PangoLanguage* gtk_text_iter_get_language (const GtkTextIter* iter)
  (define-function void* gtk_text_iter_get_language (void*))

  ;; gint gtk_text_iter_get_line (const GtkTextIter* iter)
  (define-function int gtk_text_iter_get_line (void*))

  ;; gint gtk_text_iter_get_line_index (const GtkTextIter* iter)
  (define-function int gtk_text_iter_get_line_index (void*))

  ;; gint gtk_text_iter_get_line_offset (const GtkTextIter* iter)
  (define-function int gtk_text_iter_get_line_offset (void*))

  ;; GSList* gtk_text_iter_get_marks (const GtkTextIter* iter)
  (define-function void* gtk_text_iter_get_marks (void*))

  ;; gint gtk_text_iter_get_offset (const GtkTextIter* iter)
  (define-function int gtk_text_iter_get_offset (void*))

  ;; GdkPixbuf* gtk_text_iter_get_pixbuf (const GtkTextIter* iter)
  (define-function void* gtk_text_iter_get_pixbuf (void*))

  ;; gchar* gtk_text_iter_get_slice (const GtkTextIter* start, const GtkTextIter* end)
  (define-function char* gtk_text_iter_get_slice (void* void*))

  ;; GSList* gtk_text_iter_get_tags (const GtkTextIter* iter)
  (define-function void* gtk_text_iter_get_tags (void*))

  ;; gchar* gtk_text_iter_get_text (const GtkTextIter* start, const GtkTextIter* end)
  (define-function char* gtk_text_iter_get_text (void* void*))

  ;; GSList* gtk_text_iter_get_toggled_tags (const GtkTextIter* iter, gboolean toggled_on)
  (define-function void* gtk_text_iter_get_toggled_tags (void* int))

  ;; GType gtk_text_iter_get_type (void)
  (define-function unsigned-long gtk_text_iter_get_type ())

  ;; gint gtk_text_iter_get_visible_line_index (const GtkTextIter* iter)
  (define-function int gtk_text_iter_get_visible_line_index (void*))

  ;; gint gtk_text_iter_get_visible_line_offset (const GtkTextIter* iter)
  (define-function int gtk_text_iter_get_visible_line_offset (void*))

  ;; gchar* gtk_text_iter_get_visible_slice (const GtkTextIter* start, const GtkTextIter* end)
  (define-function char* gtk_text_iter_get_visible_slice (void* void*))

  ;; gchar* gtk_text_iter_get_visible_text (const GtkTextIter* start, const GtkTextIter* end)
  (define-function char* gtk_text_iter_get_visible_text (void* void*))

  ;; gboolean gtk_text_iter_has_tag (const GtkTextIter* iter, GtkTextTag* tag)
  (define-function int gtk_text_iter_has_tag (void* void*))

  ;; gboolean gtk_text_iter_in_range (const GtkTextIter* iter, const GtkTextIter* start, const GtkTextIter* end)
  (define-function int gtk_text_iter_in_range (void* void* void*))

  ;; gboolean gtk_text_iter_inside_sentence (const GtkTextIter* iter)
  (define-function int gtk_text_iter_inside_sentence (void*))

  ;; gboolean gtk_text_iter_inside_word (const GtkTextIter* iter)
  (define-function int gtk_text_iter_inside_word (void*))

  ;; gboolean gtk_text_iter_is_cursor_position (const GtkTextIter* iter)
  (define-function int gtk_text_iter_is_cursor_position (void*))

  ;; gboolean gtk_text_iter_is_end (const GtkTextIter* iter)
  (define-function int gtk_text_iter_is_end (void*))

  ;; gboolean gtk_text_iter_is_start (const GtkTextIter* iter)
  (define-function int gtk_text_iter_is_start (void*))

  ;; void gtk_text_iter_order (GtkTextIter* first, GtkTextIter* second)
  (define-function void gtk_text_iter_order (void* void*))

  ;; void gtk_text_iter_set_line (GtkTextIter* iter, gint line_number)
  (define-function void gtk_text_iter_set_line (void* int))

  ;; void gtk_text_iter_set_line_index (GtkTextIter* iter, gint byte_on_line)
  (define-function void gtk_text_iter_set_line_index (void* int))

  ;; void gtk_text_iter_set_line_offset (GtkTextIter* iter, gint char_on_line)
  (define-function void gtk_text_iter_set_line_offset (void* int))

  ;; void gtk_text_iter_set_offset (GtkTextIter* iter, gint char_offset)
  (define-function void gtk_text_iter_set_offset (void* int))

  ;; void gtk_text_iter_set_visible_line_index (GtkTextIter* iter, gint byte_on_line)
  (define-function void gtk_text_iter_set_visible_line_index (void* int))

  ;; void gtk_text_iter_set_visible_line_offset (GtkTextIter* iter, gint char_on_line)
  (define-function void gtk_text_iter_set_visible_line_offset (void* int))

  ;; gboolean gtk_text_iter_starts_line (const GtkTextIter* iter)
  (define-function int gtk_text_iter_starts_line (void*))

  ;; gboolean gtk_text_iter_starts_sentence (const GtkTextIter* iter)
  (define-function int gtk_text_iter_starts_sentence (void*))

  ;; gboolean gtk_text_iter_starts_word (const GtkTextIter* iter)
  (define-function int gtk_text_iter_starts_word (void*))

  ;; gboolean gtk_text_iter_toggles_tag (const GtkTextIter* iter, GtkTextTag* tag)
  (define-function int gtk_text_iter_toggles_tag (void* void*))

  ;; GtkTextBuffer* gtk_text_mark_get_buffer (GtkTextMark* mark)
  (define-function void* gtk_text_mark_get_buffer (void*))

  ;; gboolean gtk_text_mark_get_deleted (GtkTextMark* mark)
  (define-function int gtk_text_mark_get_deleted (void*))

  ;; gboolean gtk_text_mark_get_left_gravity (GtkTextMark* mark)
  (define-function int gtk_text_mark_get_left_gravity (void*))

  ;; const gchar* gtk_text_mark_get_name (GtkTextMark* mark)
  (define-function char* gtk_text_mark_get_name (void*))

  ;; GType gtk_text_mark_get_type (void)
  (define-function unsigned-long gtk_text_mark_get_type ())

  ;; gboolean gtk_text_mark_get_visible (GtkTextMark* mark)
  (define-function int gtk_text_mark_get_visible (void*))

  ;; GtkTextMark* gtk_text_mark_new (const gchar* name, gboolean left_gravity)
  (define-function void* gtk_text_mark_new (char* int))

  ;; void gtk_text_mark_set_visible (GtkTextMark* mark, gboolean setting)
  (define-function void gtk_text_mark_set_visible (void* int))

  ;; GType gtk_text_search_flags_get_type (void)
  (define-function unsigned-long gtk_text_search_flags_get_type ())

  ;; gboolean gtk_text_tag_event (GtkTextTag* tag, GObject* event_object, GdkEvent* event, const GtkTextIter* iter)
  (define-function int gtk_text_tag_event (void* void* void* void*))

  ;; gint gtk_text_tag_get_priority (GtkTextTag* tag)
  (define-function int gtk_text_tag_get_priority (void*))

  ;; GType gtk_text_tag_get_type (void)
  (define-function unsigned-long gtk_text_tag_get_type ())

  ;; GtkTextTag* gtk_text_tag_new (const gchar* name)
  (define-function void* gtk_text_tag_new (char*))

  ;; void gtk_text_tag_set_priority (GtkTextTag* tag, gint priority)
  (define-function void gtk_text_tag_set_priority (void* int))

  ;; void gtk_text_tag_table_add (GtkTextTagTable* table, GtkTextTag* tag)
  (define-function void gtk_text_tag_table_add (void* void*))

  ;; void gtk_text_tag_table_foreach (GtkTextTagTable* table, GtkTextTagTableForeach func, gpointer data)
  (define-function void gtk_text_tag_table_foreach (void* (c-callback void (void* void*)) void*))

  ;; gint gtk_text_tag_table_get_size (GtkTextTagTable* table)
  (define-function int gtk_text_tag_table_get_size (void*))

  ;; GType gtk_text_tag_table_get_type (void)
  (define-function unsigned-long gtk_text_tag_table_get_type ())

  ;; GtkTextTag* gtk_text_tag_table_lookup (GtkTextTagTable* table, const gchar* name)
  (define-function void* gtk_text_tag_table_lookup (void* char*))

  ;; GtkTextTagTable* gtk_text_tag_table_new (void)
  (define-function void* gtk_text_tag_table_new ())

  ;; void gtk_text_tag_table_remove (GtkTextTagTable* table, GtkTextTag* tag)
  (define-function void gtk_text_tag_table_remove (void* void*))

  ;; void gtk_text_view_add_child_at_anchor (GtkTextView* text_view, GtkWidget* child, GtkTextChildAnchor* anchor)
  (define-function void gtk_text_view_add_child_at_anchor (void* void* void*))

  ;; void gtk_text_view_add_child_in_window (GtkTextView* text_view, GtkWidget* child, GtkTextWindowType which_window, gint xpos, gint ypos)
  (define-function void gtk_text_view_add_child_in_window (void* void* int int int))

  ;; gboolean gtk_text_view_backward_display_line (GtkTextView* text_view, GtkTextIter* iter)
  (define-function int gtk_text_view_backward_display_line (void* void*))

  ;; gboolean gtk_text_view_backward_display_line_start (GtkTextView* text_view, GtkTextIter* iter)
  (define-function int gtk_text_view_backward_display_line_start (void* void*))

  ;; void gtk_text_view_buffer_to_window_coords (GtkTextView* text_view, GtkTextWindowType win, gint buffer_x, gint buffer_y, gint* window_x, gint* window_y)
  (define-function void gtk_text_view_buffer_to_window_coords (void* int int int void* void*))

  ;; gboolean gtk_text_view_forward_display_line (GtkTextView* text_view, GtkTextIter* iter)
  (define-function int gtk_text_view_forward_display_line (void* void*))

  ;; gboolean gtk_text_view_forward_display_line_end (GtkTextView* text_view, GtkTextIter* iter)
  (define-function int gtk_text_view_forward_display_line_end (void* void*))

  ;; gboolean gtk_text_view_get_accepts_tab (GtkTextView* text_view)
  (define-function int gtk_text_view_get_accepts_tab (void*))

  ;; gint gtk_text_view_get_border_window_size (GtkTextView* text_view, GtkTextWindowType type)
  (define-function int gtk_text_view_get_border_window_size (void* int))

  ;; GtkTextBuffer* gtk_text_view_get_buffer (GtkTextView* text_view)
  (define-function void* gtk_text_view_get_buffer (void*))

  ;; gboolean gtk_text_view_get_cursor_visible (GtkTextView* text_view)
  (define-function int gtk_text_view_get_cursor_visible (void*))

  ;; GtkTextAttributes* gtk_text_view_get_default_attributes (GtkTextView* text_view)
  (define-function void* gtk_text_view_get_default_attributes (void*))

  ;; gboolean gtk_text_view_get_editable (GtkTextView* text_view)
  (define-function int gtk_text_view_get_editable (void*))

  ;; gint gtk_text_view_get_indent (GtkTextView* text_view)
  (define-function int gtk_text_view_get_indent (void*))

  ;; void gtk_text_view_get_iter_at_location (GtkTextView* text_view, GtkTextIter* iter, gint x, gint y)
  (define-function void gtk_text_view_get_iter_at_location (void* void* int int))

  ;; void gtk_text_view_get_iter_at_position (GtkTextView* text_view, GtkTextIter* iter, gint* trailing, gint x, gint y)
  (define-function void gtk_text_view_get_iter_at_position (void* void* void* int int))

  ;; void gtk_text_view_get_iter_location (GtkTextView* text_view, const GtkTextIter* iter, GdkRectangle* location)
  (define-function void gtk_text_view_get_iter_location (void* void* void*))

  ;; GtkJustification gtk_text_view_get_justification (GtkTextView* text_view)
  (define-function int gtk_text_view_get_justification (void*))

  ;; gint gtk_text_view_get_left_margin (GtkTextView* text_view)
  (define-function int gtk_text_view_get_left_margin (void*))

  ;; void gtk_text_view_get_line_at_y (GtkTextView* text_view, GtkTextIter* target_iter, gint y, gint* line_top)
  (define-function void gtk_text_view_get_line_at_y (void* void* int void*))

  ;; void gtk_text_view_get_line_yrange (GtkTextView* text_view, const GtkTextIter* iter, gint* y, gint* height)
  (define-function void gtk_text_view_get_line_yrange (void* void* void* void*))

  ;; gboolean gtk_text_view_get_overwrite (GtkTextView* text_view)
  (define-function int gtk_text_view_get_overwrite (void*))

  ;; gint gtk_text_view_get_pixels_above_lines (GtkTextView* text_view)
  (define-function int gtk_text_view_get_pixels_above_lines (void*))

  ;; gint gtk_text_view_get_pixels_below_lines (GtkTextView* text_view)
  (define-function int gtk_text_view_get_pixels_below_lines (void*))

  ;; gint gtk_text_view_get_pixels_inside_wrap (GtkTextView* text_view)
  (define-function int gtk_text_view_get_pixels_inside_wrap (void*))

  ;; gint gtk_text_view_get_right_margin (GtkTextView* text_view)
  (define-function int gtk_text_view_get_right_margin (void*))

  ;; PangoTabArray* gtk_text_view_get_tabs (GtkTextView* text_view)
  (define-function void* gtk_text_view_get_tabs (void*))

  ;; GType gtk_text_view_get_type (void)
  (define-function unsigned-long gtk_text_view_get_type ())

  ;; void gtk_text_view_get_visible_rect (GtkTextView* text_view, GdkRectangle* visible_rect)
  (define-function void gtk_text_view_get_visible_rect (void* void*))

  ;; GdkWindow* gtk_text_view_get_window (GtkTextView* text_view, GtkTextWindowType win)
  (define-function void* gtk_text_view_get_window (void* int))

  ;; GtkTextWindowType gtk_text_view_get_window_type (GtkTextView* text_view, GdkWindow* window)
  (define-function int gtk_text_view_get_window_type (void* void*))

  ;; GtkWrapMode gtk_text_view_get_wrap_mode (GtkTextView* text_view)
  (define-function int gtk_text_view_get_wrap_mode (void*))

  ;; void gtk_text_view_move_child (GtkTextView* text_view, GtkWidget* child, gint xpos, gint ypos)
  (define-function void gtk_text_view_move_child (void* void* int int))

  ;; gboolean gtk_text_view_move_mark_onscreen (GtkTextView* text_view, GtkTextMark* mark)
  (define-function int gtk_text_view_move_mark_onscreen (void* void*))

  ;; gboolean gtk_text_view_move_visually (GtkTextView* text_view, GtkTextIter* iter, gint count)
  (define-function int gtk_text_view_move_visually (void* void* int))

  ;; GtkWidget* gtk_text_view_new (void)
  (define-function void* gtk_text_view_new ())

  ;; GtkWidget* gtk_text_view_new_with_buffer (GtkTextBuffer* buffer)
  (define-function void* gtk_text_view_new_with_buffer (void*))

  ;; gboolean gtk_text_view_place_cursor_onscreen (GtkTextView* text_view)
  (define-function int gtk_text_view_place_cursor_onscreen (void*))

  ;; void gtk_text_view_scroll_mark_onscreen (GtkTextView* text_view, GtkTextMark* mark)
  (define-function void gtk_text_view_scroll_mark_onscreen (void* void*))

  ;; gboolean gtk_text_view_scroll_to_iter (GtkTextView* text_view, GtkTextIter* iter, gdouble within_margin, gboolean use_align, gdouble xalign, gdouble yalign)
  (define-function int gtk_text_view_scroll_to_iter (void* void* double int double double))

  ;; void gtk_text_view_scroll_to_mark (GtkTextView* text_view, GtkTextMark* mark, gdouble within_margin, gboolean use_align, gdouble xalign, gdouble yalign)
  (define-function void gtk_text_view_scroll_to_mark (void* void* double int double double))

  ;; void gtk_text_view_set_accepts_tab (GtkTextView* text_view, gboolean accepts_tab)
  (define-function void gtk_text_view_set_accepts_tab (void* int))

  ;; void gtk_text_view_set_border_window_size (GtkTextView* text_view, GtkTextWindowType type, gint size)
  (define-function void gtk_text_view_set_border_window_size (void* int int))

  ;; void gtk_text_view_set_buffer (GtkTextView* text_view, GtkTextBuffer* buffer)
  (define-function void gtk_text_view_set_buffer (void* void*))

  ;; void gtk_text_view_set_cursor_visible (GtkTextView* text_view, gboolean setting)
  (define-function void gtk_text_view_set_cursor_visible (void* int))

  ;; void gtk_text_view_set_editable (GtkTextView* text_view, gboolean setting)
  (define-function void gtk_text_view_set_editable (void* int))

  ;; void gtk_text_view_set_indent (GtkTextView* text_view, gint indent)
  (define-function void gtk_text_view_set_indent (void* int))

  ;; void gtk_text_view_set_justification (GtkTextView* text_view, GtkJustification justification)
  (define-function void gtk_text_view_set_justification (void* int))

  ;; void gtk_text_view_set_left_margin (GtkTextView* text_view, gint left_margin)
  (define-function void gtk_text_view_set_left_margin (void* int))

  ;; void gtk_text_view_set_overwrite (GtkTextView* text_view, gboolean overwrite)
  (define-function void gtk_text_view_set_overwrite (void* int))

  ;; void gtk_text_view_set_pixels_above_lines (GtkTextView* text_view, gint pixels_above_lines)
  (define-function void gtk_text_view_set_pixels_above_lines (void* int))

  ;; void gtk_text_view_set_pixels_below_lines (GtkTextView* text_view, gint pixels_below_lines)
  (define-function void gtk_text_view_set_pixels_below_lines (void* int))

  ;; void gtk_text_view_set_pixels_inside_wrap (GtkTextView* text_view, gint pixels_inside_wrap)
  (define-function void gtk_text_view_set_pixels_inside_wrap (void* int))

  ;; void gtk_text_view_set_right_margin (GtkTextView* text_view, gint right_margin)
  (define-function void gtk_text_view_set_right_margin (void* int))

  ;; void gtk_text_view_set_tabs (GtkTextView* text_view, PangoTabArray* tabs)
  (define-function void gtk_text_view_set_tabs (void* void*))

  ;; void gtk_text_view_set_wrap_mode (GtkTextView* text_view, GtkWrapMode wrap_mode)
  (define-function void gtk_text_view_set_wrap_mode (void* int))

  ;; gboolean gtk_text_view_starts_display_line (GtkTextView* text_view, const GtkTextIter* iter)
  (define-function int gtk_text_view_starts_display_line (void* void*))

  ;; void gtk_text_view_window_to_buffer_coords (GtkTextView* text_view, GtkTextWindowType win, gint window_x, gint window_y, gint* buffer_x, gint* buffer_y)
  (define-function void gtk_text_view_window_to_buffer_coords (void* int int int void* void*))

  ;; GType gtk_text_window_type_get_type (void)
  (define-function unsigned-long gtk_text_window_type_get_type ())

  ) ;[end]
