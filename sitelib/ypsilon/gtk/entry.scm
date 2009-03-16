#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk entry)

  (export gtk_entry_completion_complete
          gtk_entry_completion_delete_action
          gtk_entry_completion_get_completion_prefix
          gtk_entry_completion_get_entry
          gtk_entry_completion_get_inline_completion
          gtk_entry_completion_get_inline_selection
          gtk_entry_completion_get_minimum_key_length
          gtk_entry_completion_get_model
          gtk_entry_completion_get_popup_completion
          gtk_entry_completion_get_popup_set_width
          gtk_entry_completion_get_popup_single_match
          gtk_entry_completion_get_text_column
          gtk_entry_completion_get_type
          gtk_entry_completion_insert_action_markup
          gtk_entry_completion_insert_action_text
          gtk_entry_completion_insert_prefix
          gtk_entry_completion_new
          gtk_entry_completion_set_inline_completion
          gtk_entry_completion_set_inline_selection
          gtk_entry_completion_set_match_func
          gtk_entry_completion_set_minimum_key_length
          gtk_entry_completion_set_model
          gtk_entry_completion_set_popup_completion
          gtk_entry_completion_set_popup_set_width
          gtk_entry_completion_set_popup_single_match
          gtk_entry_completion_set_text_column
          gtk_entry_get_activates_default
          gtk_entry_get_alignment
          gtk_entry_get_completion
          gtk_entry_get_cursor_hadjustment
          gtk_entry_get_has_frame
          gtk_entry_get_inner_border
          gtk_entry_get_invisible_char
          gtk_entry_get_layout
          gtk_entry_get_layout_offsets
          gtk_entry_get_max_length
          gtk_entry_get_overwrite_mode
          gtk_entry_get_text
          gtk_entry_get_text_length
          gtk_entry_get_type
          gtk_entry_get_visibility
          gtk_entry_get_width_chars
          gtk_entry_layout_index_to_text_index
          gtk_entry_new
          gtk_entry_set_activates_default
          gtk_entry_set_alignment
          gtk_entry_set_completion
          gtk_entry_set_cursor_hadjustment
          gtk_entry_set_has_frame
          gtk_entry_set_inner_border
          gtk_entry_set_invisible_char
          gtk_entry_set_max_length
          gtk_entry_set_overwrite_mode
          gtk_entry_set_text
          gtk_entry_set_visibility
          gtk_entry_set_width_chars
          gtk_entry_text_index_to_layout_index)

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

  ;; void gtk_entry_completion_complete (GtkEntryCompletion* completion)
  (define-function void gtk_entry_completion_complete (void*))

  ;; void gtk_entry_completion_delete_action (GtkEntryCompletion* completion, gint index_)
  (define-function void gtk_entry_completion_delete_action (void* int))

  ;; const gchar* gtk_entry_completion_get_completion_prefix (GtkEntryCompletion* completion)
  (define-function char* gtk_entry_completion_get_completion_prefix (void*))

  ;; GtkWidget* gtk_entry_completion_get_entry (GtkEntryCompletion* completion)
  (define-function void* gtk_entry_completion_get_entry (void*))

  ;; gboolean gtk_entry_completion_get_inline_completion (GtkEntryCompletion* completion)
  (define-function int gtk_entry_completion_get_inline_completion (void*))

  ;; gboolean gtk_entry_completion_get_inline_selection (GtkEntryCompletion* completion)
  (define-function int gtk_entry_completion_get_inline_selection (void*))

  ;; gint gtk_entry_completion_get_minimum_key_length (GtkEntryCompletion* completion)
  (define-function int gtk_entry_completion_get_minimum_key_length (void*))

  ;; GtkTreeModel* gtk_entry_completion_get_model (GtkEntryCompletion* completion)
  (define-function void* gtk_entry_completion_get_model (void*))

  ;; gboolean gtk_entry_completion_get_popup_completion (GtkEntryCompletion* completion)
  (define-function int gtk_entry_completion_get_popup_completion (void*))

  ;; gboolean gtk_entry_completion_get_popup_set_width (GtkEntryCompletion* completion)
  (define-function int gtk_entry_completion_get_popup_set_width (void*))

  ;; gboolean gtk_entry_completion_get_popup_single_match (GtkEntryCompletion* completion)
  (define-function int gtk_entry_completion_get_popup_single_match (void*))

  ;; gint gtk_entry_completion_get_text_column (GtkEntryCompletion* completion)
  (define-function int gtk_entry_completion_get_text_column (void*))

  ;; GType gtk_entry_completion_get_type (void)
  (define-function unsigned-long gtk_entry_completion_get_type ())

  ;; void gtk_entry_completion_insert_action_markup (GtkEntryCompletion* completion, gint index_, const gchar* markup)
  (define-function void gtk_entry_completion_insert_action_markup (void* int char*))

  ;; void gtk_entry_completion_insert_action_text (GtkEntryCompletion* completion, gint index_, const gchar* text)
  (define-function void gtk_entry_completion_insert_action_text (void* int char*))

  ;; void gtk_entry_completion_insert_prefix (GtkEntryCompletion* completion)
  (define-function void gtk_entry_completion_insert_prefix (void*))

  ;; GtkEntryCompletion* gtk_entry_completion_new (void)
  (define-function void* gtk_entry_completion_new ())

  ;; void gtk_entry_completion_set_inline_completion (GtkEntryCompletion* completion, gboolean inline_completion)
  (define-function void gtk_entry_completion_set_inline_completion (void* int))

  ;; void gtk_entry_completion_set_inline_selection (GtkEntryCompletion* completion, gboolean inline_selection)
  (define-function void gtk_entry_completion_set_inline_selection (void* int))

  ;; void gtk_entry_completion_set_match_func (GtkEntryCompletion* completion, GtkEntryCompletionMatchFunc func, gpointer func_data, GDestroyNotify func_notify)
  (define-function void gtk_entry_completion_set_match_func (void* (c-callback int (void* void* void* void*)) void* (c-callback void (void*))))

  ;; void gtk_entry_completion_set_minimum_key_length (GtkEntryCompletion* completion, gint length)
  (define-function void gtk_entry_completion_set_minimum_key_length (void* int))

  ;; void gtk_entry_completion_set_model (GtkEntryCompletion* completion, GtkTreeModel* model)
  (define-function void gtk_entry_completion_set_model (void* void*))

  ;; void gtk_entry_completion_set_popup_completion (GtkEntryCompletion* completion, gboolean popup_completion)
  (define-function void gtk_entry_completion_set_popup_completion (void* int))

  ;; void gtk_entry_completion_set_popup_set_width (GtkEntryCompletion* completion, gboolean popup_set_width)
  (define-function void gtk_entry_completion_set_popup_set_width (void* int))

  ;; void gtk_entry_completion_set_popup_single_match (GtkEntryCompletion* completion, gboolean popup_single_match)
  (define-function void gtk_entry_completion_set_popup_single_match (void* int))

  ;; void gtk_entry_completion_set_text_column (GtkEntryCompletion* completion, gint column)
  (define-function void gtk_entry_completion_set_text_column (void* int))

  ;; gboolean gtk_entry_get_activates_default (GtkEntry* entry)
  (define-function int gtk_entry_get_activates_default (void*))

  ;; gfloat gtk_entry_get_alignment (GtkEntry* entry)
  (define-function float gtk_entry_get_alignment (void*))

  ;; GtkEntryCompletion* gtk_entry_get_completion (GtkEntry* entry)
  (define-function void* gtk_entry_get_completion (void*))

  ;; GtkAdjustment* gtk_entry_get_cursor_hadjustment (GtkEntry* entry)
  (define-function void* gtk_entry_get_cursor_hadjustment (void*))

  ;; gboolean gtk_entry_get_has_frame (GtkEntry* entry)
  (define-function int gtk_entry_get_has_frame (void*))

  ;; const GtkBorder* gtk_entry_get_inner_border (GtkEntry* entry)
  (define-function void* gtk_entry_get_inner_border (void*))

  ;; gunichar gtk_entry_get_invisible_char (GtkEntry* entry)
  (define-function uint32_t gtk_entry_get_invisible_char (void*))

  ;; PangoLayout* gtk_entry_get_layout (GtkEntry* entry)
  (define-function void* gtk_entry_get_layout (void*))

  ;; void gtk_entry_get_layout_offsets (GtkEntry* entry, gint* x, gint* y)
  (define-function void gtk_entry_get_layout_offsets (void* void* void*))

  ;; gint gtk_entry_get_max_length (GtkEntry* entry)
  (define-function int gtk_entry_get_max_length (void*))

  ;; gboolean gtk_entry_get_overwrite_mode (GtkEntry* entry)
  (define-function int gtk_entry_get_overwrite_mode (void*))

  ;; const gchar* gtk_entry_get_text (GtkEntry* entry)
  (define-function char* gtk_entry_get_text (void*))

  ;; guint16 gtk_entry_get_text_length (GtkEntry* entry)
  (define-function uint16_t gtk_entry_get_text_length (void*))

  ;; GType gtk_entry_get_type (void)
  (define-function unsigned-long gtk_entry_get_type ())

  ;; gboolean gtk_entry_get_visibility (GtkEntry* entry)
  (define-function int gtk_entry_get_visibility (void*))

  ;; gint gtk_entry_get_width_chars (GtkEntry* entry)
  (define-function int gtk_entry_get_width_chars (void*))

  ;; gint gtk_entry_layout_index_to_text_index (GtkEntry* entry, gint layout_index)
  (define-function int gtk_entry_layout_index_to_text_index (void* int))

  ;; GtkWidget* gtk_entry_new (void)
  (define-function void* gtk_entry_new ())

  ;; void gtk_entry_set_activates_default (GtkEntry* entry, gboolean setting)
  (define-function void gtk_entry_set_activates_default (void* int))

  ;; void gtk_entry_set_alignment (GtkEntry* entry, gfloat xalign)
  (define-function void gtk_entry_set_alignment (void* float))

  ;; void gtk_entry_set_completion (GtkEntry* entry, GtkEntryCompletion* completion)
  (define-function void gtk_entry_set_completion (void* void*))

  ;; void gtk_entry_set_cursor_hadjustment (GtkEntry* entry, GtkAdjustment* adjustment)
  (define-function void gtk_entry_set_cursor_hadjustment (void* void*))

  ;; void gtk_entry_set_has_frame (GtkEntry* entry, gboolean setting)
  (define-function void gtk_entry_set_has_frame (void* int))

  ;; void gtk_entry_set_inner_border (GtkEntry* entry, const GtkBorder* border)
  (define-function void gtk_entry_set_inner_border (void* void*))

  ;; void gtk_entry_set_invisible_char (GtkEntry* entry, gunichar ch)
  (define-function void gtk_entry_set_invisible_char (void* uint32_t))

  ;; void gtk_entry_set_max_length (GtkEntry* entry, gint max)
  (define-function void gtk_entry_set_max_length (void* int))

  ;; void gtk_entry_set_overwrite_mode (GtkEntry* entry, gboolean overwrite)
  (define-function void gtk_entry_set_overwrite_mode (void* int))

  ;; void gtk_entry_set_text (GtkEntry* entry, const gchar* text)
  (define-function void gtk_entry_set_text (void* char*))

  ;; void gtk_entry_set_visibility (GtkEntry* entry, gboolean visible)
  (define-function void gtk_entry_set_visibility (void* int))

  ;; void gtk_entry_set_width_chars (GtkEntry* entry, gint n_chars)
  (define-function void gtk_entry_set_width_chars (void* int))

  ;; gint gtk_entry_text_index_to_layout_index (GtkEntry* entry, gint text_index)
  (define-function int gtk_entry_text_index_to_layout_index (void* int))

  ) ;[end]
