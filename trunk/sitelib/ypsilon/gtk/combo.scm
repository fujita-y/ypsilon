#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk combo)

  (export gtk_combo_box_append_text
          gtk_combo_box_entry_get_text_column
          gtk_combo_box_entry_get_type
          gtk_combo_box_entry_new
          gtk_combo_box_entry_new_text
          gtk_combo_box_entry_new_with_model
          gtk_combo_box_entry_set_text_column
          gtk_combo_box_get_active
          gtk_combo_box_get_active_iter
          gtk_combo_box_get_active_text
          gtk_combo_box_get_add_tearoffs
          gtk_combo_box_get_button_sensitivity
          gtk_combo_box_get_column_span_column
          gtk_combo_box_get_focus_on_click
          gtk_combo_box_get_model
          gtk_combo_box_get_popup_accessible
          gtk_combo_box_get_row_separator_func
          gtk_combo_box_get_row_span_column
          gtk_combo_box_get_title
          gtk_combo_box_get_type
          gtk_combo_box_get_wrap_width
          gtk_combo_box_insert_text
          gtk_combo_box_new
          gtk_combo_box_new_text
          gtk_combo_box_new_with_model
          gtk_combo_box_popdown
          gtk_combo_box_popup
          gtk_combo_box_prepend_text
          gtk_combo_box_remove_text
          gtk_combo_box_set_active
          gtk_combo_box_set_active_iter
          gtk_combo_box_set_add_tearoffs
          gtk_combo_box_set_button_sensitivity
          gtk_combo_box_set_column_span_column
          gtk_combo_box_set_focus_on_click
          gtk_combo_box_set_model
          gtk_combo_box_set_row_separator_func
          gtk_combo_box_set_row_span_column
          gtk_combo_box_set_title
          gtk_combo_box_set_wrap_width)

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

  ;; void gtk_combo_box_append_text (GtkComboBox* combo_box, const gchar* text)
  (define-function void gtk_combo_box_append_text (void* char*))

  ;; gint gtk_combo_box_entry_get_text_column (GtkComboBoxEntry* entry_box)
  (define-function int gtk_combo_box_entry_get_text_column (void*))

  ;; GType gtk_combo_box_entry_get_type (void)
  (define-function unsigned-long gtk_combo_box_entry_get_type ())

  ;; GtkWidget* gtk_combo_box_entry_new (void)
  (define-function void* gtk_combo_box_entry_new ())

  ;; GtkWidget* gtk_combo_box_entry_new_text (void)
  (define-function void* gtk_combo_box_entry_new_text ())

  ;; GtkWidget* gtk_combo_box_entry_new_with_model (GtkTreeModel* model, gint text_column)
  (define-function void* gtk_combo_box_entry_new_with_model (void* int))

  ;; void gtk_combo_box_entry_set_text_column (GtkComboBoxEntry* entry_box, gint text_column)
  (define-function void gtk_combo_box_entry_set_text_column (void* int))

  ;; gint gtk_combo_box_get_active (GtkComboBox* combo_box)
  (define-function int gtk_combo_box_get_active (void*))

  ;; gboolean gtk_combo_box_get_active_iter (GtkComboBox* combo_box, GtkTreeIter* iter)
  (define-function int gtk_combo_box_get_active_iter (void* void*))

  ;; gchar* gtk_combo_box_get_active_text (GtkComboBox* combo_box)
  (define-function char* gtk_combo_box_get_active_text (void*))

  ;; gboolean gtk_combo_box_get_add_tearoffs (GtkComboBox* combo_box)
  (define-function int gtk_combo_box_get_add_tearoffs (void*))

  ;; GtkSensitivityType gtk_combo_box_get_button_sensitivity (GtkComboBox* combo_box)
  (define-function int gtk_combo_box_get_button_sensitivity (void*))

  ;; gint gtk_combo_box_get_column_span_column (GtkComboBox* combo_box)
  (define-function int gtk_combo_box_get_column_span_column (void*))

  ;; gboolean gtk_combo_box_get_focus_on_click (GtkComboBox* combo)
  (define-function int gtk_combo_box_get_focus_on_click (void*))

  ;; GtkTreeModel* gtk_combo_box_get_model (GtkComboBox* combo_box)
  (define-function void* gtk_combo_box_get_model (void*))

  ;; AtkObject* gtk_combo_box_get_popup_accessible (GtkComboBox* combo_box)
  (define-function void* gtk_combo_box_get_popup_accessible (void*))

  ;; GtkTreeViewRowSeparatorFunc gtk_combo_box_get_row_separator_func (GtkComboBox* combo_box)
  (define-function void* gtk_combo_box_get_row_separator_func (void*))

  ;; gint gtk_combo_box_get_row_span_column (GtkComboBox* combo_box)
  (define-function int gtk_combo_box_get_row_span_column (void*))

  ;; const gchar* gtk_combo_box_get_title (GtkComboBox* combo_box)
  (define-function char* gtk_combo_box_get_title (void*))

  ;; GType gtk_combo_box_get_type (void)
  (define-function unsigned-long gtk_combo_box_get_type ())

  ;; gint gtk_combo_box_get_wrap_width (GtkComboBox* combo_box)
  (define-function int gtk_combo_box_get_wrap_width (void*))

  ;; void gtk_combo_box_insert_text (GtkComboBox* combo_box, gint position, const gchar* text)
  (define-function void gtk_combo_box_insert_text (void* int char*))

  ;; GtkWidget* gtk_combo_box_new (void)
  (define-function void* gtk_combo_box_new ())

  ;; GtkWidget* gtk_combo_box_new_text (void)
  (define-function void* gtk_combo_box_new_text ())

  ;; GtkWidget* gtk_combo_box_new_with_model (GtkTreeModel* model)
  (define-function void* gtk_combo_box_new_with_model (void*))

  ;; void gtk_combo_box_popdown (GtkComboBox* combo_box)
  (define-function void gtk_combo_box_popdown (void*))

  ;; void gtk_combo_box_popup (GtkComboBox* combo_box)
  (define-function void gtk_combo_box_popup (void*))

  ;; void gtk_combo_box_prepend_text (GtkComboBox* combo_box, const gchar* text)
  (define-function void gtk_combo_box_prepend_text (void* char*))

  ;; void gtk_combo_box_remove_text (GtkComboBox* combo_box, gint position)
  (define-function void gtk_combo_box_remove_text (void* int))

  ;; void gtk_combo_box_set_active (GtkComboBox* combo_box, gint index_)
  (define-function void gtk_combo_box_set_active (void* int))

  ;; void gtk_combo_box_set_active_iter (GtkComboBox* combo_box, GtkTreeIter* iter)
  (define-function void gtk_combo_box_set_active_iter (void* void*))

  ;; void gtk_combo_box_set_add_tearoffs (GtkComboBox* combo_box, gboolean add_tearoffs)
  (define-function void gtk_combo_box_set_add_tearoffs (void* int))

  ;; void gtk_combo_box_set_button_sensitivity (GtkComboBox* combo_box, GtkSensitivityType sensitivity)
  (define-function void gtk_combo_box_set_button_sensitivity (void* int))

  ;; void gtk_combo_box_set_column_span_column (GtkComboBox* combo_box, gint column_span)
  (define-function void gtk_combo_box_set_column_span_column (void* int))

  ;; void gtk_combo_box_set_focus_on_click (GtkComboBox* combo, gboolean focus_on_click)
  (define-function void gtk_combo_box_set_focus_on_click (void* int))

  ;; void gtk_combo_box_set_model (GtkComboBox* combo_box, GtkTreeModel* model)
  (define-function void gtk_combo_box_set_model (void* void*))

  ;; void gtk_combo_box_set_row_separator_func (GtkComboBox* combo_box, GtkTreeViewRowSeparatorFunc func, gpointer data, GDestroyNotify destroy)
  (define-function void gtk_combo_box_set_row_separator_func (void* void* void* (c-callback void (void*))))

  ;; void gtk_combo_box_set_row_span_column (GtkComboBox* combo_box, gint row_span)
  (define-function void gtk_combo_box_set_row_span_column (void* int))

  ;; void gtk_combo_box_set_title (GtkComboBox* combo_box, const gchar* title)
  (define-function void gtk_combo_box_set_title (void* char*))

  ;; void gtk_combo_box_set_wrap_width (GtkComboBox* combo_box, gint width)
  (define-function void gtk_combo_box_set_wrap_width (void* int))

  ) ;[end]
