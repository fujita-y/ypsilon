#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon pango glyph)

  (export pango_glyph_item_apply_attrs
          pango_glyph_item_copy
          pango_glyph_item_free
          pango_glyph_item_get_type
          pango_glyph_item_iter_copy
          pango_glyph_item_iter_free
          pango_glyph_item_iter_get_type
          pango_glyph_item_iter_init_end
          pango_glyph_item_iter_init_start
          pango_glyph_item_iter_next_cluster
          pango_glyph_item_iter_prev_cluster
          pango_glyph_item_letter_space
          pango_glyph_item_split
          pango_glyph_string_copy
          pango_glyph_string_extents
          pango_glyph_string_extents_range
          pango_glyph_string_free
          pango_glyph_string_get_logical_widths
          pango_glyph_string_get_type
          pango_glyph_string_get_width
          pango_glyph_string_index_to_x
          pango_glyph_string_new
          pango_glyph_string_set_size
          pango_glyph_string_x_to_index)

  (import (rnrs) (ypsilon ffi))

  (define lib-name
    (cond (on-darwin  "Pango.framework/Pango")
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

  ;; GSList* pango_glyph_item_apply_attrs (PangoGlyphItem* glyph_item, const char* text, PangoAttrList* list)
  (define-function void* pango_glyph_item_apply_attrs (void* char* void*))

  ;; PangoGlyphItem* pango_glyph_item_copy (PangoGlyphItem* orig)
  (define-function void* pango_glyph_item_copy (void*))

  ;; void pango_glyph_item_free (PangoGlyphItem* glyph_item)
  (define-function void pango_glyph_item_free (void*))

  ;; GType pango_glyph_item_get_type (void)
  (define-function unsigned-long pango_glyph_item_get_type ())

  ;; PangoGlyphItemIter* pango_glyph_item_iter_copy (PangoGlyphItemIter* orig)
  (define-function void* pango_glyph_item_iter_copy (void*))

  ;; void pango_glyph_item_iter_free (PangoGlyphItemIter* iter)
  (define-function void pango_glyph_item_iter_free (void*))

  ;; GType pango_glyph_item_iter_get_type (void)
  (define-function unsigned-long pango_glyph_item_iter_get_type ())

  ;; gboolean pango_glyph_item_iter_init_end (PangoGlyphItemIter* iter, PangoGlyphItem* glyph_item, const char* text)
  (define-function int pango_glyph_item_iter_init_end (void* void* char*))

  ;; gboolean pango_glyph_item_iter_init_start (PangoGlyphItemIter* iter, PangoGlyphItem* glyph_item, const char* text)
  (define-function int pango_glyph_item_iter_init_start (void* void* char*))

  ;; gboolean pango_glyph_item_iter_next_cluster (PangoGlyphItemIter* iter)
  (define-function int pango_glyph_item_iter_next_cluster (void*))

  ;; gboolean pango_glyph_item_iter_prev_cluster (PangoGlyphItemIter* iter)
  (define-function int pango_glyph_item_iter_prev_cluster (void*))

  ;; void pango_glyph_item_letter_space (PangoGlyphItem* glyph_item, const char* text, PangoLogAttr* log_attrs, int letter_spacing)
  (define-function void pango_glyph_item_letter_space (void* char* void* int))

  ;; PangoGlyphItem* pango_glyph_item_split (PangoGlyphItem* orig, const char* text, int split_index)
  (define-function void* pango_glyph_item_split (void* char* int))

  ;; PangoGlyphString* pango_glyph_string_copy (PangoGlyphString* string)
  (define-function void* pango_glyph_string_copy (void*))

  ;; void pango_glyph_string_extents (PangoGlyphString* glyphs, PangoFont* font, PangoRectangle* ink_rect, PangoRectangle* logical_rect)
  (define-function void pango_glyph_string_extents (void* void* void* void*))

  ;; void pango_glyph_string_extents_range (PangoGlyphString* glyphs, int start, int end, PangoFont* font, PangoRectangle* ink_rect, PangoRectangle* logical_rect)
  (define-function void pango_glyph_string_extents_range (void* int int void* void* void*))

  ;; void pango_glyph_string_free (PangoGlyphString* string)
  (define-function void pango_glyph_string_free (void*))

  ;; void pango_glyph_string_get_logical_widths (PangoGlyphString* glyphs, const char* text, int length, int embedding_level, int* logical_widths)
  (define-function void pango_glyph_string_get_logical_widths (void* char* int int void*))

  ;; GType pango_glyph_string_get_type (void)
  (define-function unsigned-long pango_glyph_string_get_type ())

  ;; int pango_glyph_string_get_width(PangoGlyphString* glyphs)
  (define-function int pango_glyph_string_get_width (void*))

  ;; void pango_glyph_string_index_to_x (PangoGlyphString* glyphs, char* text, int length, PangoAnalysis* analysis, int index_, gboolean trailing, int* x_pos)
  (define-function void pango_glyph_string_index_to_x (void* char* int void* int int void*))

  ;; PangoGlyphString* pango_glyph_string_new (void)
  (define-function void* pango_glyph_string_new ())

  ;; void pango_glyph_string_set_size (PangoGlyphString* string, gint new_len)
  (define-function void pango_glyph_string_set_size (void* int))

  ;; void pango_glyph_string_x_to_index (PangoGlyphString* glyphs, char* text, int length, PangoAnalysis* analysis, int x_pos, int* index_, int* trailing)
  (define-function void pango_glyph_string_x_to_index (void* char* int void* int void* void*))

  ) ;[end]
