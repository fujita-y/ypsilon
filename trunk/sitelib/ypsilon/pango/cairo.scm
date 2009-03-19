#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon pango cairo)

  (export pango_cairo_context_get_font_options
          pango_cairo_context_get_resolution
          pango_cairo_context_get_shape_renderer
          pango_cairo_context_set_font_options
          pango_cairo_context_set_resolution
          pango_cairo_context_set_shape_renderer
          pango_cairo_create_context
          pango_cairo_create_layout
          pango_cairo_error_underline_path
          pango_cairo_font_get_scaled_font
          pango_cairo_font_get_type
          pango_cairo_font_map_create_context
          pango_cairo_font_map_get_default
          pango_cairo_font_map_get_font_type
          pango_cairo_font_map_get_resolution
          pango_cairo_font_map_get_type
          pango_cairo_font_map_new
          pango_cairo_font_map_new_for_font_type
          pango_cairo_font_map_set_default
          pango_cairo_font_map_set_resolution
          pango_cairo_glyph_string_path
          pango_cairo_layout_line_path
          pango_cairo_layout_path
          pango_cairo_show_error_underline
          pango_cairo_show_glyph_item
          pango_cairo_show_glyph_string
          pango_cairo_show_layout
          pango_cairo_show_layout_line
          pango_cairo_update_context
          pango_cairo_update_layout)

  (import (rnrs) (ypsilon ffi))

  (define lib-name
    (cond (on-darwin  "Gtk.framework/Gtk")
          (on-linux   "libpango-1.0.so.0")
          (on-freebsd "libpango-1.0.so.0")
          (on-openbsd "libpango-1.0.so.0")
          (on-windows "libpango-1.0-0.dll")
          (else
           (assertion-violation #f "can not locate Pango library, unknown operating system"))))

  (define lib (load-shared-object lib-name))

  (define-syntax define-function
    (syntax-rules ()
      ((_ ret name args)
       (define name (c-function lib lib-name ret name args)))))

  ;; const cairo_font_options_t* pango_cairo_context_get_font_options (PangoContext* context)
  (define-function void* pango_cairo_context_get_font_options (void*))

  ;; double pango_cairo_context_get_resolution (PangoContext* context)
  (define-function double pango_cairo_context_get_resolution (void*))

  ;; PangoCairoShapeRendererFunc pango_cairo_context_get_shape_renderer (PangoContext* context, gpointer* data)
  (define-function void* pango_cairo_context_get_shape_renderer (void* void*))

  ;; void pango_cairo_context_set_font_options (PangoContext* context, const cairo_font_options_t* options)
  (define-function void pango_cairo_context_set_font_options (void* void*))

  ;; void pango_cairo_context_set_resolution (PangoContext* context, double dpi)
  (define-function void pango_cairo_context_set_resolution (void* double))

  ;; void pango_cairo_context_set_shape_renderer (PangoContext* context, PangoCairoShapeRendererFunc func, gpointer data, GDestroyNotify dnotify)
  (define-function void pango_cairo_context_set_shape_renderer (void* void* void* (c-callback void (void*))))

  ;; PangoContext* pango_cairo_create_context (cairo_t* cr)
  (define-function void* pango_cairo_create_context (void*))

  ;; PangoLayout* pango_cairo_create_layout (cairo_t* cr)
  (define-function void* pango_cairo_create_layout (void*))

  ;; void pango_cairo_error_underline_path (cairo_t* cr, double x, double y, double width, double height)
  (define-function void pango_cairo_error_underline_path (void* double double double double))

  ;; cairo_scaled_font_t* pango_cairo_font_get_scaled_font (PangoCairoFont* font)
  (define-function void* pango_cairo_font_get_scaled_font (void*))

  ;; GType pango_cairo_font_get_type (void)
  (define-function unsigned-long pango_cairo_font_get_type ())

  ;; PangoContext* pango_cairo_font_map_create_context (PangoCairoFontMap* fontmap)
  (define-function void* pango_cairo_font_map_create_context (void*))

  ;; PangoFontMap* pango_cairo_font_map_get_default (void)
  (define-function void* pango_cairo_font_map_get_default ())

  ;; cairo_font_type_t pango_cairo_font_map_get_font_type (PangoCairoFontMap* fontmap)
  (define-function int pango_cairo_font_map_get_font_type (void*))

  ;; double pango_cairo_font_map_get_resolution (PangoCairoFontMap* fontmap)
  (define-function double pango_cairo_font_map_get_resolution (void*))

  ;; GType pango_cairo_font_map_get_type (void)
  (define-function unsigned-long pango_cairo_font_map_get_type ())

  ;; PangoFontMap* pango_cairo_font_map_new (void)
  (define-function void* pango_cairo_font_map_new ())

  ;; PangoFontMap* pango_cairo_font_map_new_for_font_type (cairo_font_type_t fonttype)
  (define-function void* pango_cairo_font_map_new_for_font_type (int))

  ;; void pango_cairo_font_map_set_default (PangoCairoFontMap* fontmap)
  (define-function void pango_cairo_font_map_set_default (void*))

  ;; void pango_cairo_font_map_set_resolution (PangoCairoFontMap* fontmap, double dpi)
  (define-function void pango_cairo_font_map_set_resolution (void* double))

  ;; void pango_cairo_glyph_string_path (cairo_t* cr, PangoFont* font, PangoGlyphString* glyphs)
  (define-function void pango_cairo_glyph_string_path (void* void* void*))

  ;; void pango_cairo_layout_line_path (cairo_t* cr, PangoLayoutLine* line)
  (define-function void pango_cairo_layout_line_path (void* void*))

  ;; void pango_cairo_layout_path (cairo_t* cr, PangoLayout* layout)
  (define-function void pango_cairo_layout_path (void* void*))

  ;; void pango_cairo_show_error_underline (cairo_t* cr, double x, double y, double width, double height)
  (define-function void pango_cairo_show_error_underline (void* double double double double))

  ;; void pango_cairo_show_glyph_item (cairo_t* cr, const char* text, PangoGlyphItem* glyph_item)
  (define-function void pango_cairo_show_glyph_item (void* char* void*))

  ;; void pango_cairo_show_glyph_string (cairo_t* cr, PangoFont* font, PangoGlyphString* glyphs)
  (define-function void pango_cairo_show_glyph_string (void* void* void*))

  ;; void pango_cairo_show_layout (cairo_t* cr, PangoLayout* layout)
  (define-function void pango_cairo_show_layout (void* void*))

  ;; void pango_cairo_show_layout_line (cairo_t* cr, PangoLayoutLine* line)
  (define-function void pango_cairo_show_layout_line (void* void*))

  ;; void pango_cairo_update_context (cairo_t* cr, PangoContext* context)
  (define-function void pango_cairo_update_context (void* void*))

  ;; void pango_cairo_update_layout (cairo_t* cr, PangoLayout* layout)
  (define-function void pango_cairo_update_layout (void* void*))

  ) ;[end]
