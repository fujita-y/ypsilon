#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon pango layout)

  (export pango_layout_context_changed
          pango_layout_copy
          pango_layout_get_alignment
          pango_layout_get_attributes
          pango_layout_get_auto_dir
          pango_layout_get_baseline
          pango_layout_get_context
          pango_layout_get_cursor_pos
          pango_layout_get_ellipsize
          pango_layout_get_extents
          pango_layout_get_font_description
          pango_layout_get_height
          pango_layout_get_indent
          pango_layout_get_iter
          pango_layout_get_justify
          pango_layout_get_line
          pango_layout_get_line_count
          pango_layout_get_line_readonly
          pango_layout_get_lines
          pango_layout_get_lines_readonly
          pango_layout_get_log_attrs
          pango_layout_get_pixel_extents
          pango_layout_get_pixel_size
          pango_layout_get_single_paragraph_mode
          pango_layout_get_size
          pango_layout_get_spacing
          pango_layout_get_tabs
          pango_layout_get_text
          pango_layout_get_type
          pango_layout_get_unknown_glyphs_count
          pango_layout_get_width
          pango_layout_get_wrap
          pango_layout_index_to_line_x
          pango_layout_index_to_pos
          pango_layout_is_ellipsized
          pango_layout_is_wrapped
          pango_layout_iter_at_last_line
          pango_layout_iter_copy
          pango_layout_iter_free
          pango_layout_iter_get_baseline
          pango_layout_iter_get_char_extents
          pango_layout_iter_get_cluster_extents
          pango_layout_iter_get_index
          pango_layout_iter_get_layout
          pango_layout_iter_get_layout_extents
          pango_layout_iter_get_line
          pango_layout_iter_get_line_extents
          pango_layout_iter_get_line_readonly
          pango_layout_iter_get_line_yrange
          pango_layout_iter_get_run
          pango_layout_iter_get_run_extents
          pango_layout_iter_get_run_readonly
          pango_layout_iter_get_type
          pango_layout_iter_next_char
          pango_layout_iter_next_cluster
          pango_layout_iter_next_line
          pango_layout_iter_next_run
          pango_layout_line_get_extents
          pango_layout_line_get_pixel_extents
          pango_layout_line_get_type
          pango_layout_line_get_x_ranges
          pango_layout_line_index_to_x
          pango_layout_line_ref
          pango_layout_line_unref
          pango_layout_line_x_to_index
          pango_layout_move_cursor_visually
          pango_layout_new
          pango_layout_set_alignment
          pango_layout_set_attributes
          pango_layout_set_auto_dir
          pango_layout_set_ellipsize
          pango_layout_set_font_description
          pango_layout_set_height
          pango_layout_set_indent
          pango_layout_set_justify
          pango_layout_set_markup
          pango_layout_set_markup_with_accel
          pango_layout_set_single_paragraph_mode
          pango_layout_set_spacing
          pango_layout_set_tabs
          pango_layout_set_text
          pango_layout_set_width
          pango_layout_set_wrap
          pango_layout_xy_to_index)

  (import (rnrs) (ypsilon ffi))

  (define lib-name
    (cond (on-darwin  "Gtk.framework/Gtk")
          (on-linux   "libpango-1.0.so.0")
          (on-freebsd "libpango-1.0.so.0")
          (on-openbsd "libpango-1.0.so.0")
          (on-windows "libpango-1.0-0.dll")
          (else
           (assertion-violation #f "can not locate GDK library, unknown operating system"))))

  (define lib (load-shared-object lib-name))

  (define-syntax define-function
    (syntax-rules ()
      ((_ ret name args)
       (define name (c-function lib lib-name ret name args)))))

  (define-syntax define-variadic-function
    (syntax-rules ()
      ((_ ret name args)
      (define name (lambda x (assertion-violation 'name "variadic function not supported"))))))

  ;; void pango_layout_context_changed (PangoLayout* layout)
  (define-function void pango_layout_context_changed (void*))

  ;; PangoLayout* pango_layout_copy (PangoLayout* src)
  (define-function void* pango_layout_copy (void*))

  ;; PangoAlignment pango_layout_get_alignment (PangoLayout* layout)
  (define-function int pango_layout_get_alignment (void*))

  ;; PangoAttrList* pango_layout_get_attributes (PangoLayout* layout)
  (define-function void* pango_layout_get_attributes (void*))

  ;; gboolean pango_layout_get_auto_dir (PangoLayout* layout)
  (define-function int pango_layout_get_auto_dir (void*))

  ;; int pango_layout_get_baseline (PangoLayout* layout)
  (define-function int pango_layout_get_baseline (void*))

  ;; PangoContext* pango_layout_get_context (PangoLayout* layout)
  (define-function void* pango_layout_get_context (void*))

  ;; void pango_layout_get_cursor_pos (PangoLayout* layout, int index_, PangoRectangle* strong_pos, PangoRectangle* weak_pos)
  (define-function void pango_layout_get_cursor_pos (void* int void* void*))

  ;; PangoEllipsizeMode pango_layout_get_ellipsize (PangoLayout* layout)
  (define-function int pango_layout_get_ellipsize (void*))

  ;; void pango_layout_get_extents (PangoLayout* layout, PangoRectangle* ink_rect, PangoRectangle* logical_rect)
  (define-function void pango_layout_get_extents (void* void* void*))

  ;; const PangoFontDescription* pango_layout_get_font_description (PangoLayout* layout)
  (define-function void* pango_layout_get_font_description (void*))

  ;; int pango_layout_get_height (PangoLayout* layout)
  (define-function int pango_layout_get_height (void*))

  ;; int pango_layout_get_indent (PangoLayout* layout)
  (define-function int pango_layout_get_indent (void*))

  ;; PangoLayoutIter* pango_layout_get_iter (PangoLayout* layout)
  (define-function void* pango_layout_get_iter (void*))

  ;; gboolean pango_layout_get_justify (PangoLayout* layout)
  (define-function int pango_layout_get_justify (void*))

  ;; PangoLayoutLine* pango_layout_get_line (PangoLayout* layout, int line)
  (define-function void* pango_layout_get_line (void* int))

  ;; int pango_layout_get_line_count (PangoLayout* layout)
  (define-function int pango_layout_get_line_count (void*))

  ;; PangoLayoutLine* pango_layout_get_line_readonly (PangoLayout* layout, int line)
  (define-function void* pango_layout_get_line_readonly (void* int))

  ;; GSList* pango_layout_get_lines (PangoLayout* layout)
  (define-function void* pango_layout_get_lines (void*))

  ;; GSList* pango_layout_get_lines_readonly (PangoLayout* layout)
  (define-function void* pango_layout_get_lines_readonly (void*))

  ;; void pango_layout_get_log_attrs (PangoLayout* layout, PangoLogAttr** attrs, gint* n_attrs)
  (define-function void pango_layout_get_log_attrs (void* void* void*))

  ;; void pango_layout_get_pixel_extents (PangoLayout* layout, PangoRectangle* ink_rect, PangoRectangle* logical_rect)
  (define-function void pango_layout_get_pixel_extents (void* void* void*))

  ;; void pango_layout_get_pixel_size (PangoLayout* layout, int* width, int* height)
  (define-function void pango_layout_get_pixel_size (void* void* void*))

  ;; gboolean pango_layout_get_single_paragraph_mode (PangoLayout* layout)
  (define-function int pango_layout_get_single_paragraph_mode (void*))

  ;; void pango_layout_get_size (PangoLayout* layout, int* width, int* height)
  (define-function void pango_layout_get_size (void* void* void*))

  ;; int pango_layout_get_spacing (PangoLayout* layout)
  (define-function int pango_layout_get_spacing (void*))

  ;; PangoTabArray* pango_layout_get_tabs (PangoLayout* layout)
  (define-function void* pango_layout_get_tabs (void*))

  ;; const char* pango_layout_get_text (PangoLayout* layout)
  (define-function char* pango_layout_get_text (void*))

  ;; GType pango_layout_get_type (void)
  (define-function unsigned-long pango_layout_get_type ())

  ;; int pango_layout_get_unknown_glyphs_count (PangoLayout* layout)
  (define-function int pango_layout_get_unknown_glyphs_count (void*))

  ;; int pango_layout_get_width (PangoLayout* layout)
  (define-function int pango_layout_get_width (void*))

  ;; PangoWrapMode pango_layout_get_wrap (PangoLayout* layout)
  (define-function int pango_layout_get_wrap (void*))

  ;; void pango_layout_index_to_line_x (PangoLayout* layout, int index_, gboolean trailing, int* line, int* x_pos)
  (define-function void pango_layout_index_to_line_x (void* int int void* void*))

  ;; void pango_layout_index_to_pos (PangoLayout* layout, int index_, PangoRectangle* pos)
  (define-function void pango_layout_index_to_pos (void* int void*))

  ;; gboolean pango_layout_is_ellipsized (PangoLayout* layout)
  (define-function int pango_layout_is_ellipsized (void*))

  ;; gboolean pango_layout_is_wrapped (PangoLayout* layout)
  (define-function int pango_layout_is_wrapped (void*))

  ;; gboolean pango_layout_iter_at_last_line (PangoLayoutIter* iter)
  (define-function int pango_layout_iter_at_last_line (void*))

  ;; PangoLayoutIter* pango_layout_iter_copy (PangoLayoutIter* iter)
  (define-function void* pango_layout_iter_copy (void*))

  ;; void pango_layout_iter_free (PangoLayoutIter* iter)
  (define-function void pango_layout_iter_free (void*))

  ;; int pango_layout_iter_get_baseline (PangoLayoutIter* iter)
  (define-function int pango_layout_iter_get_baseline (void*))

  ;; void pango_layout_iter_get_char_extents (PangoLayoutIter* iter, PangoRectangle* logical_rect)
  (define-function void pango_layout_iter_get_char_extents (void* void*))

  ;; void pango_layout_iter_get_cluster_extents (PangoLayoutIter* iter, PangoRectangle* ink_rect, PangoRectangle* logical_rect)
  (define-function void pango_layout_iter_get_cluster_extents (void* void* void*))

  ;; int pango_layout_iter_get_index (PangoLayoutIter* iter)
  (define-function int pango_layout_iter_get_index (void*))

  ;; PangoLayout* pango_layout_iter_get_layout (PangoLayoutIter* iter)
  (define-function void* pango_layout_iter_get_layout (void*))

  ;; void pango_layout_iter_get_layout_extents (PangoLayoutIter* iter, PangoRectangle* ink_rect, PangoRectangle* logical_rect)
  (define-function void pango_layout_iter_get_layout_extents (void* void* void*))

  ;; PangoLayoutLine* pango_layout_iter_get_line (PangoLayoutIter* iter)
  (define-function void* pango_layout_iter_get_line (void*))

  ;; void pango_layout_iter_get_line_extents (PangoLayoutIter* iter, PangoRectangle* ink_rect, PangoRectangle* logical_rect)
  (define-function void pango_layout_iter_get_line_extents (void* void* void*))

  ;; PangoLayoutLine* pango_layout_iter_get_line_readonly (PangoLayoutIter* iter)
  (define-function void* pango_layout_iter_get_line_readonly (void*))

  ;; void pango_layout_iter_get_line_yrange (PangoLayoutIter* iter, int* y0_, int* y1_)
  (define-function void pango_layout_iter_get_line_yrange (void* void* void*))

  ;; PangoLayoutRun* pango_layout_iter_get_run (PangoLayoutIter* iter)
  (define-function void* pango_layout_iter_get_run (void*))

  ;; void pango_layout_iter_get_run_extents (PangoLayoutIter* iter, PangoRectangle* ink_rect, PangoRectangle* logical_rect)
  (define-function void pango_layout_iter_get_run_extents (void* void* void*))

  ;; PangoLayoutRun* pango_layout_iter_get_run_readonly (PangoLayoutIter* iter)
  (define-function void* pango_layout_iter_get_run_readonly (void*))

  ;; GType pango_layout_iter_get_type (void)
  (define-function unsigned-long pango_layout_iter_get_type ())

  ;; gboolean pango_layout_iter_next_char (PangoLayoutIter* iter)
  (define-function int pango_layout_iter_next_char (void*))

  ;; gboolean pango_layout_iter_next_cluster (PangoLayoutIter* iter)
  (define-function int pango_layout_iter_next_cluster (void*))

  ;; gboolean pango_layout_iter_next_line (PangoLayoutIter* iter)
  (define-function int pango_layout_iter_next_line (void*))

  ;; gboolean pango_layout_iter_next_run (PangoLayoutIter* iter)
  (define-function int pango_layout_iter_next_run (void*))

  ;; void pango_layout_line_get_extents (PangoLayoutLine* line, PangoRectangle* ink_rect, PangoRectangle* logical_rect)
  (define-function void pango_layout_line_get_extents (void* void* void*))

  ;; void pango_layout_line_get_pixel_extents (PangoLayoutLine* layout_line, PangoRectangle* ink_rect, PangoRectangle* logical_rect)
  (define-function void pango_layout_line_get_pixel_extents (void* void* void*))

  ;; GType pango_layout_line_get_type (void)
  (define-function unsigned-long pango_layout_line_get_type ())

  ;; void pango_layout_line_get_x_ranges (PangoLayoutLine* line, int start_index, int end_index, int** ranges, int* n_ranges)
  (define-function void pango_layout_line_get_x_ranges (void* int int void* void*))

  ;; void pango_layout_line_index_to_x (PangoLayoutLine* line, int index_, gboolean trailing, int* x_pos)
  (define-function void pango_layout_line_index_to_x (void* int int void*))

  ;; PangoLayoutLine* pango_layout_line_ref (PangoLayoutLine* line)
  (define-function void* pango_layout_line_ref (void*))

  ;; void pango_layout_line_unref (PangoLayoutLine* line)
  (define-function void pango_layout_line_unref (void*))

  ;; gboolean pango_layout_line_x_to_index (PangoLayoutLine* line, int x_pos, int* index_, int* trailing)
  (define-function int pango_layout_line_x_to_index (void* int void* void*))

  ;; void pango_layout_move_cursor_visually (PangoLayout* layout, gboolean strong, int old_index, int old_trailing, int direction, int* new_index, int* new_trailing)
  (define-function void pango_layout_move_cursor_visually (void* int int int int void* void*))

  ;; PangoLayout* pango_layout_new (PangoContext* context)
  (define-function void* pango_layout_new (void*))

  ;; void pango_layout_set_alignment (PangoLayout* layout, PangoAlignment alignment)
  (define-function void pango_layout_set_alignment (void* int))

  ;; void pango_layout_set_attributes (PangoLayout* layout, PangoAttrList* attrs)
  (define-function void pango_layout_set_attributes (void* void*))

  ;; void pango_layout_set_auto_dir (PangoLayout* layout, gboolean auto_dir)
  (define-function void pango_layout_set_auto_dir (void* int))

  ;; void pango_layout_set_ellipsize (PangoLayout* layout, PangoEllipsizeMode ellipsize)
  (define-function void pango_layout_set_ellipsize (void* int))

  ;; void pango_layout_set_font_description (PangoLayout* layout, const PangoFontDescription* desc)
  (define-function void pango_layout_set_font_description (void* void*))

  ;; void pango_layout_set_height (PangoLayout* layout, int height)
  (define-function void pango_layout_set_height (void* int))

  ;; void pango_layout_set_indent (PangoLayout* layout, int indent)
  (define-function void pango_layout_set_indent (void* int))

  ;; void pango_layout_set_justify (PangoLayout* layout, gboolean justify)
  (define-function void pango_layout_set_justify (void* int))

  ;; void pango_layout_set_markup (PangoLayout* layout, const char* markup, int length)
  (define-function void pango_layout_set_markup (void* char* int))

  ;; void pango_layout_set_markup_with_accel (PangoLayout* layout, const char* markup, int length, gunichar accel_marker, gunichar* accel_char)
  (define-function void pango_layout_set_markup_with_accel (void* char* int uint32_t void*))

  ;; void pango_layout_set_single_paragraph_mode (PangoLayout* layout, gboolean setting)
  (define-function void pango_layout_set_single_paragraph_mode (void* int))

  ;; void pango_layout_set_spacing (PangoLayout* layout, int spacing)
  (define-function void pango_layout_set_spacing (void* int))

  ;; void pango_layout_set_tabs (PangoLayout* layout, PangoTabArray* tabs)
  (define-function void pango_layout_set_tabs (void* void*))

  ;; void pango_layout_set_text (PangoLayout* layout, const char* text, int length)
  (define-function void pango_layout_set_text (void* char* int))

  ;; void pango_layout_set_width (PangoLayout* layout, int width)
  (define-function void pango_layout_set_width (void* int))

  ;; void pango_layout_set_wrap (PangoLayout* layout, PangoWrapMode wrap)
  (define-function void pango_layout_set_wrap (void* int))

  ;; gboolean pango_layout_xy_to_index (PangoLayout* layout, int x, int y, int* index_, int* trailing)
  (define-function int pango_layout_xy_to_index (void* int int void* void*))

  ) ;[end]
