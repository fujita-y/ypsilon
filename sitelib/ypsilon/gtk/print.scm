#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk print)

  (export gtk_print_context_create_pango_context
          gtk_print_context_create_pango_layout
          gtk_print_context_get_cairo_context
          gtk_print_context_get_dpi_x
          gtk_print_context_get_dpi_y
          gtk_print_context_get_height
          gtk_print_context_get_page_setup
          gtk_print_context_get_pango_fontmap
          gtk_print_context_get_type
          gtk_print_context_get_width
          gtk_print_context_set_cairo_context
          gtk_print_duplex_get_type
          gtk_print_error_get_type
          gtk_print_error_quark
          gtk_print_operation_action_get_type
          gtk_print_operation_cancel
          gtk_print_operation_draw_page_finish
          gtk_print_operation_get_default_page_setup
          gtk_print_operation_get_error
          gtk_print_operation_get_print_settings
          gtk_print_operation_get_status
          gtk_print_operation_get_status_string
          gtk_print_operation_get_type
          gtk_print_operation_is_finished
          gtk_print_operation_new
          gtk_print_operation_preview_end_preview
          gtk_print_operation_preview_get_type
          gtk_print_operation_preview_is_selected
          gtk_print_operation_preview_render_page
          gtk_print_operation_result_get_type
          gtk_print_operation_run
          gtk_print_operation_set_allow_async
          gtk_print_operation_set_current_page
          gtk_print_operation_set_custom_tab_label
          gtk_print_operation_set_default_page_setup
          gtk_print_operation_set_defer_drawing
          gtk_print_operation_set_export_filename
          gtk_print_operation_set_job_name
          gtk_print_operation_set_n_pages
          gtk_print_operation_set_print_settings
          gtk_print_operation_set_show_progress
          gtk_print_operation_set_track_print_status
          gtk_print_operation_set_unit
          gtk_print_operation_set_use_full_page
          gtk_print_pages_get_type
          gtk_print_quality_get_type
          gtk_print_run_page_setup_dialog
          gtk_print_run_page_setup_dialog_async
          gtk_print_settings_copy
          gtk_print_settings_foreach
          gtk_print_settings_get
          gtk_print_settings_get_bool
          gtk_print_settings_get_collate
          gtk_print_settings_get_default_source
          gtk_print_settings_get_dither
          gtk_print_settings_get_double
          gtk_print_settings_get_double_with_default
          gtk_print_settings_get_duplex
          gtk_print_settings_get_finishings
          gtk_print_settings_get_int
          gtk_print_settings_get_int_with_default
          gtk_print_settings_get_length
          gtk_print_settings_get_media_type
          gtk_print_settings_get_n_copies
          gtk_print_settings_get_number_up
          gtk_print_settings_get_number_up_layout
          gtk_print_settings_get_orientation
          gtk_print_settings_get_output_bin
          gtk_print_settings_get_page_ranges
          gtk_print_settings_get_page_set
          gtk_print_settings_get_paper_height
          gtk_print_settings_get_paper_size
          gtk_print_settings_get_paper_width
          gtk_print_settings_get_print_pages
          gtk_print_settings_get_printer
          gtk_print_settings_get_printer_lpi
          gtk_print_settings_get_quality
          gtk_print_settings_get_resolution
          gtk_print_settings_get_resolution_x
          gtk_print_settings_get_resolution_y
          gtk_print_settings_get_reverse
          gtk_print_settings_get_scale
          gtk_print_settings_get_type
          gtk_print_settings_get_use_color
          gtk_print_settings_has_key
          gtk_print_settings_load_file
          gtk_print_settings_load_key_file
          gtk_print_settings_new
          gtk_print_settings_new_from_file
          gtk_print_settings_new_from_key_file
          gtk_print_settings_set
          gtk_print_settings_set_bool
          gtk_print_settings_set_collate
          gtk_print_settings_set_default_source
          gtk_print_settings_set_dither
          gtk_print_settings_set_double
          gtk_print_settings_set_duplex
          gtk_print_settings_set_finishings
          gtk_print_settings_set_int
          gtk_print_settings_set_length
          gtk_print_settings_set_media_type
          gtk_print_settings_set_n_copies
          gtk_print_settings_set_number_up
          gtk_print_settings_set_number_up_layout
          gtk_print_settings_set_orientation
          gtk_print_settings_set_output_bin
          gtk_print_settings_set_page_ranges
          gtk_print_settings_set_page_set
          gtk_print_settings_set_paper_height
          gtk_print_settings_set_paper_size
          gtk_print_settings_set_paper_width
          gtk_print_settings_set_print_pages
          gtk_print_settings_set_printer
          gtk_print_settings_set_printer_lpi
          gtk_print_settings_set_quality
          gtk_print_settings_set_resolution
          gtk_print_settings_set_resolution_xy
          gtk_print_settings_set_reverse
          gtk_print_settings_set_scale
          gtk_print_settings_set_use_color
          gtk_print_settings_to_file
          gtk_print_settings_to_key_file
          gtk_print_settings_unset
          gtk_print_status_get_type)

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

  ;; PangoContext* gtk_print_context_create_pango_context (GtkPrintContext* context)
  (define-function void* gtk_print_context_create_pango_context (void*))

  ;; PangoLayout* gtk_print_context_create_pango_layout (GtkPrintContext* context)
  (define-function void* gtk_print_context_create_pango_layout (void*))

  ;; cairo_t* gtk_print_context_get_cairo_context (GtkPrintContext* context)
  (define-function void* gtk_print_context_get_cairo_context (void*))

  ;; gdouble gtk_print_context_get_dpi_x (GtkPrintContext* context)
  (define-function double gtk_print_context_get_dpi_x (void*))

  ;; gdouble gtk_print_context_get_dpi_y (GtkPrintContext* context)
  (define-function double gtk_print_context_get_dpi_y (void*))

  ;; gdouble gtk_print_context_get_height (GtkPrintContext* context)
  (define-function double gtk_print_context_get_height (void*))

  ;; GtkPageSetup* gtk_print_context_get_page_setup (GtkPrintContext* context)
  (define-function void* gtk_print_context_get_page_setup (void*))

  ;; PangoFontMap* gtk_print_context_get_pango_fontmap (GtkPrintContext* context)
  (define-function void* gtk_print_context_get_pango_fontmap (void*))

  ;; GType gtk_print_context_get_type (void)
  (define-function unsigned-long gtk_print_context_get_type ())

  ;; gdouble gtk_print_context_get_width (GtkPrintContext* context)
  (define-function double gtk_print_context_get_width (void*))

  ;; void gtk_print_context_set_cairo_context (GtkPrintContext* context, cairo_t* cr, double dpi_x, double dpi_y)
  (define-function void gtk_print_context_set_cairo_context (void* void* double double))

  ;; GType gtk_print_duplex_get_type (void)
  (define-function unsigned-long gtk_print_duplex_get_type ())

  ;; GType gtk_print_error_get_type (void)
  (define-function unsigned-long gtk_print_error_get_type ())

  ;; GQuark gtk_print_error_quark (void)
  (define-function uint32_t gtk_print_error_quark ())

  ;; GType gtk_print_operation_action_get_type (void)
  (define-function unsigned-long gtk_print_operation_action_get_type ())

  ;; void gtk_print_operation_cancel (GtkPrintOperation* op)
  (define-function void gtk_print_operation_cancel (void*))

  ;; void gtk_print_operation_draw_page_finish (GtkPrintOperation* op)
  (define-function void gtk_print_operation_draw_page_finish (void*))

  ;; GtkPageSetup* gtk_print_operation_get_default_page_setup (GtkPrintOperation* op)
  (define-function void* gtk_print_operation_get_default_page_setup (void*))

  ;; void gtk_print_operation_get_error (GtkPrintOperation* op, GError** error)
  (define-function void gtk_print_operation_get_error (void* void*))

  ;; GtkPrintSettings* gtk_print_operation_get_print_settings (GtkPrintOperation* op)
  (define-function void* gtk_print_operation_get_print_settings (void*))

  ;; GtkPrintStatus gtk_print_operation_get_status (GtkPrintOperation* op)
  (define-function int gtk_print_operation_get_status (void*))

  ;; const gchar* gtk_print_operation_get_status_string (GtkPrintOperation* op)
  (define-function char* gtk_print_operation_get_status_string (void*))

  ;; GType gtk_print_operation_get_type (void)
  (define-function unsigned-long gtk_print_operation_get_type ())

  ;; gboolean gtk_print_operation_is_finished (GtkPrintOperation* op)
  (define-function int gtk_print_operation_is_finished (void*))

  ;; GtkPrintOperation* gtk_print_operation_new (void)
  (define-function void* gtk_print_operation_new ())

  ;; void gtk_print_operation_preview_end_preview (GtkPrintOperationPreview* preview)
  (define-function void gtk_print_operation_preview_end_preview (void*))

  ;; GType gtk_print_operation_preview_get_type (void)
  (define-function unsigned-long gtk_print_operation_preview_get_type ())

  ;; gboolean gtk_print_operation_preview_is_selected (GtkPrintOperationPreview* preview, gint page_nr)
  (define-function int gtk_print_operation_preview_is_selected (void* int))

  ;; void gtk_print_operation_preview_render_page (GtkPrintOperationPreview* preview, gint page_nr)
  (define-function void gtk_print_operation_preview_render_page (void* int))

  ;; GType gtk_print_operation_result_get_type (void)
  (define-function unsigned-long gtk_print_operation_result_get_type ())

  ;; GtkPrintOperationResult gtk_print_operation_run (GtkPrintOperation* op, GtkPrintOperationAction action, GtkWindow* parent, GError** error)
  (define-function int gtk_print_operation_run (void* int void* void*))

  ;; void gtk_print_operation_set_allow_async (GtkPrintOperation* op, gboolean allow_async)
  (define-function void gtk_print_operation_set_allow_async (void* int))

  ;; void gtk_print_operation_set_current_page (GtkPrintOperation* op, gint current_page)
  (define-function void gtk_print_operation_set_current_page (void* int))

  ;; void gtk_print_operation_set_custom_tab_label (GtkPrintOperation* op, const gchar* label)
  (define-function void gtk_print_operation_set_custom_tab_label (void* char*))

  ;; void gtk_print_operation_set_default_page_setup (GtkPrintOperation* op, GtkPageSetup* default_page_setup)
  (define-function void gtk_print_operation_set_default_page_setup (void* void*))

  ;; void gtk_print_operation_set_defer_drawing (GtkPrintOperation* op)
  (define-function void gtk_print_operation_set_defer_drawing (void*))

  ;; void gtk_print_operation_set_export_filename (GtkPrintOperation* op, const gchar* filename)
  (define-function void gtk_print_operation_set_export_filename (void* char*))

  ;; void gtk_print_operation_set_job_name (GtkPrintOperation* op, const gchar* job_name)
  (define-function void gtk_print_operation_set_job_name (void* char*))

  ;; void gtk_print_operation_set_n_pages (GtkPrintOperation* op, gint n_pages)
  (define-function void gtk_print_operation_set_n_pages (void* int))

  ;; void gtk_print_operation_set_print_settings (GtkPrintOperation* op, GtkPrintSettings* print_settings)
  (define-function void gtk_print_operation_set_print_settings (void* void*))

  ;; void gtk_print_operation_set_show_progress (GtkPrintOperation* op, gboolean show_progress)
  (define-function void gtk_print_operation_set_show_progress (void* int))

  ;; void gtk_print_operation_set_track_print_status (GtkPrintOperation* op, gboolean track_status)
  (define-function void gtk_print_operation_set_track_print_status (void* int))

  ;; void gtk_print_operation_set_unit (GtkPrintOperation* op, GtkUnit unit)
  (define-function void gtk_print_operation_set_unit (void* int))

  ;; void gtk_print_operation_set_use_full_page (GtkPrintOperation* op, gboolean full_page)
  (define-function void gtk_print_operation_set_use_full_page (void* int))

  ;; GType gtk_print_pages_get_type (void)
  (define-function unsigned-long gtk_print_pages_get_type ())

  ;; GType gtk_print_quality_get_type (void)
  (define-function unsigned-long gtk_print_quality_get_type ())

  ;; GtkPageSetup* gtk_print_run_page_setup_dialog (GtkWindow* parent, GtkPageSetup* page_setup, GtkPrintSettings* settings)
  (define-function void* gtk_print_run_page_setup_dialog (void* void* void*))

  ;; void gtk_print_run_page_setup_dialog_async (GtkWindow* parent, GtkPageSetup* page_setup, GtkPrintSettings* settings, GtkPageSetupDoneFunc done_cb, gpointer data)
  (define-function void gtk_print_run_page_setup_dialog_async (void* void* void* (c-callback void (void* void*)) void*))

  ;; GtkPrintSettings* gtk_print_settings_copy (GtkPrintSettings* other)
  (define-function void* gtk_print_settings_copy (void*))

  ;; void gtk_print_settings_foreach (GtkPrintSettings* settings, GtkPrintSettingsFunc func, gpointer user_data)
  (define-function void gtk_print_settings_foreach (void* (c-callback void (void* void* void*)) void*))

  ;; const gchar* gtk_print_settings_get (GtkPrintSettings* settings, const gchar* key)
  (define-function char* gtk_print_settings_get (void* char*))

  ;; gboolean gtk_print_settings_get_bool (GtkPrintSettings* settings, const gchar* key)
  (define-function int gtk_print_settings_get_bool (void* char*))

  ;; gboolean gtk_print_settings_get_collate (GtkPrintSettings* settings)
  (define-function int gtk_print_settings_get_collate (void*))

  ;; const gchar* gtk_print_settings_get_default_source (GtkPrintSettings* settings)
  (define-function char* gtk_print_settings_get_default_source (void*))

  ;; const gchar* gtk_print_settings_get_dither (GtkPrintSettings* settings)
  (define-function char* gtk_print_settings_get_dither (void*))

  ;; gdouble gtk_print_settings_get_double (GtkPrintSettings* settings, const gchar* key)
  (define-function double gtk_print_settings_get_double (void* char*))

  ;; gdouble gtk_print_settings_get_double_with_default (GtkPrintSettings* settings, const gchar* key, gdouble def)
  (define-function double gtk_print_settings_get_double_with_default (void* char* double))

  ;; GtkPrintDuplex gtk_print_settings_get_duplex (GtkPrintSettings* settings)
  (define-function int gtk_print_settings_get_duplex (void*))

  ;; const gchar* gtk_print_settings_get_finishings (GtkPrintSettings* settings)
  (define-function char* gtk_print_settings_get_finishings (void*))

  ;; gint gtk_print_settings_get_int (GtkPrintSettings* settings, const gchar* key)
  (define-function int gtk_print_settings_get_int (void* char*))

  ;; gint gtk_print_settings_get_int_with_default (GtkPrintSettings* settings, const gchar* key, gint def)
  (define-function int gtk_print_settings_get_int_with_default (void* char* int))

  ;; gdouble gtk_print_settings_get_length (GtkPrintSettings* settings, const gchar* key, GtkUnit unit)
  (define-function double gtk_print_settings_get_length (void* char* int))

  ;; const gchar* gtk_print_settings_get_media_type (GtkPrintSettings* settings)
  (define-function char* gtk_print_settings_get_media_type (void*))

  ;; gint gtk_print_settings_get_n_copies (GtkPrintSettings* settings)
  (define-function int gtk_print_settings_get_n_copies (void*))

  ;; gint gtk_print_settings_get_number_up (GtkPrintSettings* settings)
  (define-function int gtk_print_settings_get_number_up (void*))

  ;; GtkNumberUpLayout gtk_print_settings_get_number_up_layout (GtkPrintSettings* settings)
  (define-function int gtk_print_settings_get_number_up_layout (void*))

  ;; GtkPageOrientation gtk_print_settings_get_orientation (GtkPrintSettings* settings)
  (define-function int gtk_print_settings_get_orientation (void*))

  ;; const gchar* gtk_print_settings_get_output_bin (GtkPrintSettings* settings)
  (define-function char* gtk_print_settings_get_output_bin (void*))

  ;; GtkPageRange* gtk_print_settings_get_page_ranges (GtkPrintSettings* settings, gint* num_ranges)
  (define-function void* gtk_print_settings_get_page_ranges (void* void*))

  ;; GtkPageSet gtk_print_settings_get_page_set (GtkPrintSettings* settings)
  (define-function int gtk_print_settings_get_page_set (void*))

  ;; gdouble gtk_print_settings_get_paper_height (GtkPrintSettings* settings, GtkUnit unit)
  (define-function double gtk_print_settings_get_paper_height (void* int))

  ;; GtkPaperSize* gtk_print_settings_get_paper_size (GtkPrintSettings* settings)
  (define-function void* gtk_print_settings_get_paper_size (void*))

  ;; gdouble gtk_print_settings_get_paper_width (GtkPrintSettings* settings, GtkUnit unit)
  (define-function double gtk_print_settings_get_paper_width (void* int))

  ;; GtkPrintPages gtk_print_settings_get_print_pages (GtkPrintSettings* settings)
  (define-function int gtk_print_settings_get_print_pages (void*))

  ;; const gchar* gtk_print_settings_get_printer (GtkPrintSettings* settings)
  (define-function char* gtk_print_settings_get_printer (void*))

  ;; gdouble gtk_print_settings_get_printer_lpi (GtkPrintSettings* settings)
  (define-function double gtk_print_settings_get_printer_lpi (void*))

  ;; GtkPrintQuality gtk_print_settings_get_quality (GtkPrintSettings* settings)
  (define-function int gtk_print_settings_get_quality (void*))

  ;; gint gtk_print_settings_get_resolution (GtkPrintSettings* settings)
  (define-function int gtk_print_settings_get_resolution (void*))

  ;; gint gtk_print_settings_get_resolution_x (GtkPrintSettings* settings)
  (define-function int gtk_print_settings_get_resolution_x (void*))

  ;; gint gtk_print_settings_get_resolution_y (GtkPrintSettings* settings)
  (define-function int gtk_print_settings_get_resolution_y (void*))

  ;; gboolean gtk_print_settings_get_reverse (GtkPrintSettings* settings)
  (define-function int gtk_print_settings_get_reverse (void*))

  ;; gdouble gtk_print_settings_get_scale (GtkPrintSettings* settings)
  (define-function double gtk_print_settings_get_scale (void*))

  ;; GType gtk_print_settings_get_type (void)
  (define-function unsigned-long gtk_print_settings_get_type ())

  ;; gboolean gtk_print_settings_get_use_color (GtkPrintSettings* settings)
  (define-function int gtk_print_settings_get_use_color (void*))

  ;; gboolean gtk_print_settings_has_key (GtkPrintSettings* settings, const gchar* key)
  (define-function int gtk_print_settings_has_key (void* char*))

  ;; gboolean gtk_print_settings_load_file (GtkPrintSettings* settings, const gchar* file_name, GError** error)
  (define-function int gtk_print_settings_load_file (void* char* void*))

  ;; gboolean gtk_print_settings_load_key_file (GtkPrintSettings* settings, GKeyFile* key_file, const gchar* group_name, GError** error)
  (define-function int gtk_print_settings_load_key_file (void* void* char* void*))

  ;; GtkPrintSettings* gtk_print_settings_new (void)
  (define-function void* gtk_print_settings_new ())

  ;; GtkPrintSettings* gtk_print_settings_new_from_file (const gchar* file_name, GError** error)
  (define-function void* gtk_print_settings_new_from_file (char* void*))

  ;; GtkPrintSettings* gtk_print_settings_new_from_key_file (GKeyFile* key_file, const gchar* group_name, GError** error)
  (define-function void* gtk_print_settings_new_from_key_file (void* char* void*))

  ;; void gtk_print_settings_set (GtkPrintSettings* settings, const gchar* key, const gchar* value)
  (define-function void gtk_print_settings_set (void* char* char*))

  ;; void gtk_print_settings_set_bool (GtkPrintSettings* settings, const gchar* key, gboolean value)
  (define-function void gtk_print_settings_set_bool (void* char* int))

  ;; void gtk_print_settings_set_collate (GtkPrintSettings* settings, gboolean collate)
  (define-function void gtk_print_settings_set_collate (void* int))

  ;; void gtk_print_settings_set_default_source (GtkPrintSettings* settings, const gchar* default_source)
  (define-function void gtk_print_settings_set_default_source (void* char*))

  ;; void gtk_print_settings_set_dither (GtkPrintSettings* settings, const gchar* dither)
  (define-function void gtk_print_settings_set_dither (void* char*))

  ;; void gtk_print_settings_set_double (GtkPrintSettings* settings, const gchar* key, gdouble value)
  (define-function void gtk_print_settings_set_double (void* char* double))

  ;; void gtk_print_settings_set_duplex (GtkPrintSettings* settings, GtkPrintDuplex duplex)
  (define-function void gtk_print_settings_set_duplex (void* int))

  ;; void gtk_print_settings_set_finishings (GtkPrintSettings* settings, const gchar* finishings)
  (define-function void gtk_print_settings_set_finishings (void* char*))

  ;; void gtk_print_settings_set_int (GtkPrintSettings* settings, const gchar* key, gint value)
  (define-function void gtk_print_settings_set_int (void* char* int))

  ;; void gtk_print_settings_set_length (GtkPrintSettings* settings, const gchar* key, gdouble value, GtkUnit unit)
  (define-function void gtk_print_settings_set_length (void* char* double int))

  ;; void gtk_print_settings_set_media_type (GtkPrintSettings* settings, const gchar* media_type)
  (define-function void gtk_print_settings_set_media_type (void* char*))

  ;; void gtk_print_settings_set_n_copies (GtkPrintSettings* settings, gint num_copies)
  (define-function void gtk_print_settings_set_n_copies (void* int))

  ;; void gtk_print_settings_set_number_up (GtkPrintSettings* settings, gint number_up)
  (define-function void gtk_print_settings_set_number_up (void* int))

  ;; void gtk_print_settings_set_number_up_layout (GtkPrintSettings* settings, GtkNumberUpLayout number_up_layout)
  (define-function void gtk_print_settings_set_number_up_layout (void* int))

  ;; void gtk_print_settings_set_orientation (GtkPrintSettings* settings, GtkPageOrientation orientation)
  (define-function void gtk_print_settings_set_orientation (void* int))

  ;; void gtk_print_settings_set_output_bin (GtkPrintSettings* settings, const gchar* output_bin)
  (define-function void gtk_print_settings_set_output_bin (void* char*))

  ;; void gtk_print_settings_set_page_ranges (GtkPrintSettings* settings, GtkPageRange* page_ranges, gint num_ranges)
  (define-function void gtk_print_settings_set_page_ranges (void* void* int))

  ;; void gtk_print_settings_set_page_set (GtkPrintSettings* settings, GtkPageSet page_set)
  (define-function void gtk_print_settings_set_page_set (void* int))

  ;; void gtk_print_settings_set_paper_height (GtkPrintSettings* settings, gdouble height, GtkUnit unit)
  (define-function void gtk_print_settings_set_paper_height (void* double int))

  ;; void gtk_print_settings_set_paper_size (GtkPrintSettings* settings, GtkPaperSize* paper_size)
  (define-function void gtk_print_settings_set_paper_size (void* void*))

  ;; void gtk_print_settings_set_paper_width (GtkPrintSettings* settings, gdouble width, GtkUnit unit)
  (define-function void gtk_print_settings_set_paper_width (void* double int))

  ;; void gtk_print_settings_set_print_pages (GtkPrintSettings* settings, GtkPrintPages pages)
  (define-function void gtk_print_settings_set_print_pages (void* int))

  ;; void gtk_print_settings_set_printer (GtkPrintSettings* settings, const gchar* printer)
  (define-function void gtk_print_settings_set_printer (void* char*))

  ;; void gtk_print_settings_set_printer_lpi (GtkPrintSettings* settings, gdouble lpi)
  (define-function void gtk_print_settings_set_printer_lpi (void* double))

  ;; void gtk_print_settings_set_quality (GtkPrintSettings* settings, GtkPrintQuality quality)
  (define-function void gtk_print_settings_set_quality (void* int))

  ;; void gtk_print_settings_set_resolution (GtkPrintSettings* settings, gint resolution)
  (define-function void gtk_print_settings_set_resolution (void* int))

  ;; void gtk_print_settings_set_resolution_xy (GtkPrintSettings* settings, gint resolution_x, gint resolution_y)
  (define-function void gtk_print_settings_set_resolution_xy (void* int int))

  ;; void gtk_print_settings_set_reverse (GtkPrintSettings* settings, gboolean reverse)
  (define-function void gtk_print_settings_set_reverse (void* int))

  ;; void gtk_print_settings_set_scale (GtkPrintSettings* settings, gdouble scale)
  (define-function void gtk_print_settings_set_scale (void* double))

  ;; void gtk_print_settings_set_use_color (GtkPrintSettings* settings, gboolean use_color)
  (define-function void gtk_print_settings_set_use_color (void* int))

  ;; gboolean gtk_print_settings_to_file (GtkPrintSettings* settings, const gchar* file_name, GError** error)
  (define-function int gtk_print_settings_to_file (void* char* void*))

  ;; void gtk_print_settings_to_key_file (GtkPrintSettings* settings, GKeyFile* key_file, const gchar* group_name)
  (define-function void gtk_print_settings_to_key_file (void* void* char*))

  ;; void gtk_print_settings_unset (GtkPrintSettings* settings, const gchar* key)
  (define-function void gtk_print_settings_unset (void* char*))

  ;; GType gtk_print_status_get_type (void)
  (define-function unsigned-long gtk_print_status_get_type ())

  ) ;[end]
