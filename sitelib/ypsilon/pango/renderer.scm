#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon pango renderer)

  (export pango_renderer_activate
          pango_renderer_deactivate
          pango_renderer_draw_error_underline
          pango_renderer_draw_glyph
          pango_renderer_draw_glyph_item
          pango_renderer_draw_glyphs
          pango_renderer_draw_layout
          pango_renderer_draw_layout_line
          pango_renderer_draw_rectangle
          pango_renderer_draw_trapezoid
          pango_renderer_get_color
          pango_renderer_get_layout
          pango_renderer_get_layout_line
          pango_renderer_get_matrix
          pango_renderer_get_type
          pango_renderer_part_changed
          pango_renderer_set_color
          pango_renderer_set_matrix)

  (import (rnrs) (ypsilon ffi))

  (define lib-name
    (cond (on-linux   "libpango-1.0.so.0")
          (on-sunos   "libpango-1.0.so.0")
          (on-freebsd "libpango-1.0.so.0")
          (on-openbsd "libpango-1.0.so.0")
          (on-darwin  "Gtk.framework/Gtk")
          (on-windows "libpango-1.0-0.dll")
          (else
           (assertion-violation #f "can not locate Pango library, unknown operating system"))))

  (define lib (load-shared-object lib-name))

  (define-syntax define-function
    (syntax-rules ()
      ((_ ret name args)
       (define name (c-function lib lib-name ret name args)))))

  ;; void pango_renderer_activate (PangoRenderer* renderer)
  (define-function void pango_renderer_activate (void*))

  ;; void pango_renderer_deactivate (PangoRenderer* renderer)
  (define-function void pango_renderer_deactivate (void*))

  ;; void pango_renderer_draw_error_underline (PangoRenderer* renderer, int x, int y, int width, int height)
  (define-function void pango_renderer_draw_error_underline (void* int int int int))

  ;; void pango_renderer_draw_glyph (PangoRenderer* renderer, PangoFont* font, PangoGlyph glyph, double x, double y)
  (define-function void pango_renderer_draw_glyph (void* void* uint32_t double double))

  ;; void pango_renderer_draw_glyph_item (PangoRenderer* renderer, const char* text, PangoGlyphItem* glyph_item, int x, int y)
  (define-function void pango_renderer_draw_glyph_item (void* char* void* int int))

  ;; void pango_renderer_draw_glyphs (PangoRenderer* renderer, PangoFont* font, PangoGlyphString* glyphs, int x, int y)
  (define-function void pango_renderer_draw_glyphs (void* void* void* int int))

  ;; void pango_renderer_draw_layout (PangoRenderer* renderer, PangoLayout* layout, int x, int y)
  (define-function void pango_renderer_draw_layout (void* void* int int))

  ;; void pango_renderer_draw_layout_line (PangoRenderer* renderer, PangoLayoutLine* line, int x, int y)
  (define-function void pango_renderer_draw_layout_line (void* void* int int))

  ;; void pango_renderer_draw_rectangle (PangoRenderer* renderer, PangoRenderPart part, int x, int y, int width, int height)
  (define-function void pango_renderer_draw_rectangle (void* int int int int int))

  ;; void pango_renderer_draw_trapezoid (PangoRenderer* renderer, PangoRenderPart part, double y1_, double x11, double x21, double y2, double x12, double x22)
  (define-function void pango_renderer_draw_trapezoid (void* int double double double double double double))

  ;; PangoColor* pango_renderer_get_color (PangoRenderer* renderer, PangoRenderPart part)
  (define-function void* pango_renderer_get_color (void* int))

  ;; PangoLayout* pango_renderer_get_layout (PangoRenderer* renderer)
  (define-function void* pango_renderer_get_layout (void*))

  ;; PangoLayoutLine* pango_renderer_get_layout_line (PangoRenderer* renderer)
  (define-function void* pango_renderer_get_layout_line (void*))

  ;; const PangoMatrix* pango_renderer_get_matrix (PangoRenderer* renderer)
  (define-function void* pango_renderer_get_matrix (void*))

  ;; GType pango_renderer_get_type (void)
  (define-function unsigned-long pango_renderer_get_type ())

  ;; void pango_renderer_part_changed (PangoRenderer* renderer, PangoRenderPart part)
  (define-function void pango_renderer_part_changed (void* int))

  ;; void pango_renderer_set_color (PangoRenderer* renderer, PangoRenderPart part, const PangoColor* color)
  (define-function void pango_renderer_set_color (void* int void*))

  ;; void pango_renderer_set_matrix (PangoRenderer* renderer, const PangoMatrix* matrix)
  (define-function void pango_renderer_set_matrix (void* void*))

  ) ;[end]
