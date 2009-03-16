#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk cell)

  (export gtk_cell_editable_editing_done
          gtk_cell_editable_get_type
          gtk_cell_editable_remove_widget
          gtk_cell_editable_start_editing
          gtk_cell_layout_add_attribute
          gtk_cell_layout_clear
          gtk_cell_layout_clear_attributes
          gtk_cell_layout_get_cells
          gtk_cell_layout_get_type
          gtk_cell_layout_pack_end
          gtk_cell_layout_pack_start
          gtk_cell_layout_reorder
          gtk_cell_layout_set_attributes
          gtk_cell_layout_set_cell_data_func
          gtk_cell_renderer_accel_get_type
          gtk_cell_renderer_accel_mode_get_type
          gtk_cell_renderer_accel_new
          gtk_cell_renderer_activate
          gtk_cell_renderer_combo_get_type
          gtk_cell_renderer_combo_new
          gtk_cell_renderer_get_fixed_size
          gtk_cell_renderer_get_size
          gtk_cell_renderer_get_type
          gtk_cell_renderer_mode_get_type
          gtk_cell_renderer_pixbuf_get_type
          gtk_cell_renderer_pixbuf_new
          gtk_cell_renderer_progress_get_type
          gtk_cell_renderer_progress_new
          gtk_cell_renderer_render
          gtk_cell_renderer_set_fixed_size
          gtk_cell_renderer_spin_get_type
          gtk_cell_renderer_spin_new
          gtk_cell_renderer_start_editing
          gtk_cell_renderer_state_get_type
          gtk_cell_renderer_stop_editing
          gtk_cell_renderer_text_get_type
          gtk_cell_renderer_text_new
          gtk_cell_renderer_text_set_fixed_height_from_font
          gtk_cell_renderer_toggle_get_active
          gtk_cell_renderer_toggle_get_radio
          gtk_cell_renderer_toggle_get_type
          gtk_cell_renderer_toggle_new
          gtk_cell_renderer_toggle_set_active
          gtk_cell_renderer_toggle_set_radio
          gtk_cell_type_get_type
          gtk_cell_view_get_cell_renderers
          gtk_cell_view_get_displayed_row
          gtk_cell_view_get_size_of_row
          gtk_cell_view_get_type
          gtk_cell_view_new
          gtk_cell_view_new_with_markup
          gtk_cell_view_new_with_pixbuf
          gtk_cell_view_new_with_text
          gtk_cell_view_set_background_color
          gtk_cell_view_set_displayed_row
          gtk_cell_view_set_model)

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

  ;; void gtk_cell_editable_editing_done (GtkCellEditable* cell_editable)
  (define-function void gtk_cell_editable_editing_done (void*))

  ;; GType gtk_cell_editable_get_type (void)
  (define-function unsigned-long gtk_cell_editable_get_type ())

  ;; void gtk_cell_editable_remove_widget (GtkCellEditable* cell_editable)
  (define-function void gtk_cell_editable_remove_widget (void*))

  ;; void gtk_cell_editable_start_editing (GtkCellEditable* cell_editable, GdkEvent* event)
  (define-function void gtk_cell_editable_start_editing (void* void*))

  ;; void gtk_cell_layout_add_attribute (GtkCellLayout* cell_layout, GtkCellRenderer* cell, const gchar* attribute, gint column)
  (define-function void gtk_cell_layout_add_attribute (void* void* char* int))

  ;; void gtk_cell_layout_clear (GtkCellLayout* cell_layout)
  (define-function void gtk_cell_layout_clear (void*))

  ;; void gtk_cell_layout_clear_attributes (GtkCellLayout* cell_layout, GtkCellRenderer* cell)
  (define-function void gtk_cell_layout_clear_attributes (void* void*))

  ;; GList* gtk_cell_layout_get_cells (GtkCellLayout* cell_layout)
  (define-function void* gtk_cell_layout_get_cells (void*))

  ;; GType gtk_cell_layout_get_type (void)
  (define-function unsigned-long gtk_cell_layout_get_type ())

  ;; void gtk_cell_layout_pack_end (GtkCellLayout* cell_layout, GtkCellRenderer* cell, gboolean expand)
  (define-function void gtk_cell_layout_pack_end (void* void* int))

  ;; void gtk_cell_layout_pack_start (GtkCellLayout* cell_layout, GtkCellRenderer* cell, gboolean expand)
  (define-function void gtk_cell_layout_pack_start (void* void* int))

  ;; void gtk_cell_layout_reorder (GtkCellLayout* cell_layout, GtkCellRenderer* cell, gint position)
  (define-function void gtk_cell_layout_reorder (void* void* int))

  ;; void gtk_cell_layout_set_attributes (GtkCellLayout* cell_layout, GtkCellRenderer* cell, ...)
  (define-variadic-function void gtk_cell_layout_set_attributes (void* void* ...))

  ;; void gtk_cell_layout_set_cell_data_func (GtkCellLayout* cell_layout, GtkCellRenderer* cell, GtkCellLayoutDataFunc func, gpointer func_data, GDestroyNotify destroy)
  (define-function void gtk_cell_layout_set_cell_data_func (void* void* (c-callback void (void* void* void* void* void*)) void* (c-callback void (void*))))

  ;; GType gtk_cell_renderer_accel_get_type (void)
  (define-function unsigned-long gtk_cell_renderer_accel_get_type ())

  ;; GType gtk_cell_renderer_accel_mode_get_type (void)
  (define-function unsigned-long gtk_cell_renderer_accel_mode_get_type ())

  ;; GtkCellRenderer* gtk_cell_renderer_accel_new (void)
  (define-function void* gtk_cell_renderer_accel_new ())

  ;; gboolean gtk_cell_renderer_activate (GtkCellRenderer* cell, GdkEvent* event, GtkWidget* widget, const gchar* path, const GdkRectangle* background_area, const GdkRectangle* cell_area, GtkCellRendererState flags)
  (define-function int gtk_cell_renderer_activate (void* void* void* char* void* void* int))

  ;; GType gtk_cell_renderer_combo_get_type (void)
  (define-function unsigned-long gtk_cell_renderer_combo_get_type ())

  ;; GtkCellRenderer* gtk_cell_renderer_combo_new (void)
  (define-function void* gtk_cell_renderer_combo_new ())

  ;; void gtk_cell_renderer_get_fixed_size (GtkCellRenderer* cell, gint* width, gint* height)
  (define-function void gtk_cell_renderer_get_fixed_size (void* void* void*))

  ;; void gtk_cell_renderer_get_size (GtkCellRenderer* cell, GtkWidget* widget, const GdkRectangle* cell_area, gint* x_offset, gint* y_offset, gint* width, gint* height)
  (define-function void gtk_cell_renderer_get_size (void* void* void* void* void* void* void*))

  ;; GType gtk_cell_renderer_get_type (void)
  (define-function unsigned-long gtk_cell_renderer_get_type ())

  ;; GType gtk_cell_renderer_mode_get_type (void)
  (define-function unsigned-long gtk_cell_renderer_mode_get_type ())

  ;; GType gtk_cell_renderer_pixbuf_get_type (void)
  (define-function unsigned-long gtk_cell_renderer_pixbuf_get_type ())

  ;; GtkCellRenderer* gtk_cell_renderer_pixbuf_new (void)
  (define-function void* gtk_cell_renderer_pixbuf_new ())

  ;; GType gtk_cell_renderer_progress_get_type (void)
  (define-function unsigned-long gtk_cell_renderer_progress_get_type ())

  ;; GtkCellRenderer* gtk_cell_renderer_progress_new (void)
  (define-function void* gtk_cell_renderer_progress_new ())

  ;; void gtk_cell_renderer_render (GtkCellRenderer* cell, GdkWindow* window, GtkWidget* widget, const GdkRectangle* background_area, const GdkRectangle* cell_area, const GdkRectangle* expose_area, GtkCellRendererState flags)
  (define-function void gtk_cell_renderer_render (void* void* void* void* void* void* int))

  ;; void gtk_cell_renderer_set_fixed_size (GtkCellRenderer* cell, gint width, gint height)
  (define-function void gtk_cell_renderer_set_fixed_size (void* int int))

  ;; GType gtk_cell_renderer_spin_get_type (void)
  (define-function unsigned-long gtk_cell_renderer_spin_get_type ())

  ;; GtkCellRenderer* gtk_cell_renderer_spin_new (void)
  (define-function void* gtk_cell_renderer_spin_new ())

  ;; GtkCellEditable* gtk_cell_renderer_start_editing (GtkCellRenderer* cell, GdkEvent* event, GtkWidget* widget, const gchar* path, const GdkRectangle* background_area, const GdkRectangle* cell_area, GtkCellRendererState flags)
  (define-function void* gtk_cell_renderer_start_editing (void* void* void* char* void* void* int))

  ;; GType gtk_cell_renderer_state_get_type (void)
  (define-function unsigned-long gtk_cell_renderer_state_get_type ())

  ;; void gtk_cell_renderer_stop_editing (GtkCellRenderer* cell, gboolean canceled)
  (define-function void gtk_cell_renderer_stop_editing (void* int))

  ;; GType gtk_cell_renderer_text_get_type (void)
  (define-function unsigned-long gtk_cell_renderer_text_get_type ())

  ;; GtkCellRenderer* gtk_cell_renderer_text_new (void)
  (define-function void* gtk_cell_renderer_text_new ())

  ;; void gtk_cell_renderer_text_set_fixed_height_from_font (GtkCellRendererText* renderer, gint number_of_rows)
  (define-function void gtk_cell_renderer_text_set_fixed_height_from_font (void* int))

  ;; gboolean gtk_cell_renderer_toggle_get_active (GtkCellRendererToggle* toggle)
  (define-function int gtk_cell_renderer_toggle_get_active (void*))

  ;; gboolean gtk_cell_renderer_toggle_get_radio (GtkCellRendererToggle* toggle)
  (define-function int gtk_cell_renderer_toggle_get_radio (void*))

  ;; GType gtk_cell_renderer_toggle_get_type (void)
  (define-function unsigned-long gtk_cell_renderer_toggle_get_type ())

  ;; GtkCellRenderer* gtk_cell_renderer_toggle_new (void)
  (define-function void* gtk_cell_renderer_toggle_new ())

  ;; void gtk_cell_renderer_toggle_set_active (GtkCellRendererToggle* toggle, gboolean setting)
  (define-function void gtk_cell_renderer_toggle_set_active (void* int))

  ;; void gtk_cell_renderer_toggle_set_radio (GtkCellRendererToggle* toggle, gboolean radio)
  (define-function void gtk_cell_renderer_toggle_set_radio (void* int))

  ;; GType gtk_cell_type_get_type (void)
  (define-function unsigned-long gtk_cell_type_get_type ())

  ;; GList* gtk_cell_view_get_cell_renderers (GtkCellView* cell_view)
  (define-function void* gtk_cell_view_get_cell_renderers (void*))

  ;; GtkTreePath* gtk_cell_view_get_displayed_row (GtkCellView* cell_view)
  (define-function void* gtk_cell_view_get_displayed_row (void*))

  ;; gboolean gtk_cell_view_get_size_of_row (GtkCellView* cell_view, GtkTreePath* path, GtkRequisition* requisition)
  (define-function int gtk_cell_view_get_size_of_row (void* void* void*))

  ;; GType gtk_cell_view_get_type (void)
  (define-function unsigned-long gtk_cell_view_get_type ())

  ;; GtkWidget* gtk_cell_view_new (void)
  (define-function void* gtk_cell_view_new ())

  ;; GtkWidget* gtk_cell_view_new_with_markup (const gchar* markup)
  (define-function void* gtk_cell_view_new_with_markup (char*))

  ;; GtkWidget* gtk_cell_view_new_with_pixbuf (GdkPixbuf* pixbuf)
  (define-function void* gtk_cell_view_new_with_pixbuf (void*))

  ;; GtkWidget* gtk_cell_view_new_with_text (const gchar* text)
  (define-function void* gtk_cell_view_new_with_text (char*))

  ;; void gtk_cell_view_set_background_color (GtkCellView* cell_view, const GdkColor* color)
  (define-function void gtk_cell_view_set_background_color (void* void*))

  ;; void gtk_cell_view_set_displayed_row (GtkCellView* cell_view, GtkTreePath* path)
  (define-function void gtk_cell_view_set_displayed_row (void* void*))

  ;; void gtk_cell_view_set_model (GtkCellView* cell_view, GtkTreeModel* model)
  (define-function void gtk_cell_view_set_model (void* void*))

  ) ;[end]
