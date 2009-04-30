#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon pango font)

  (export pango_font_describe
          pango_font_describe_with_absolute_size
          pango_font_description_better_match
          pango_font_description_copy
          pango_font_description_copy_static
          pango_font_description_equal
          pango_font_description_free
          pango_font_description_from_string
          pango_font_description_get_family
          pango_font_description_get_gravity
          pango_font_description_get_set_fields
          pango_font_description_get_size
          pango_font_description_get_size_is_absolute
          pango_font_description_get_stretch
          pango_font_description_get_style
          pango_font_description_get_type
          pango_font_description_get_variant
          pango_font_description_get_weight
          pango_font_description_hash
          pango_font_description_merge
          pango_font_description_merge_static
          pango_font_description_new
          pango_font_description_set_absolute_size
          pango_font_description_set_family
          pango_font_description_set_family_static
          pango_font_description_set_gravity
          pango_font_description_set_size
          pango_font_description_set_stretch
          pango_font_description_set_style
          pango_font_description_set_variant
          pango_font_description_set_weight
          pango_font_description_to_filename
          pango_font_description_to_string
          pango_font_description_unset_fields
          pango_font_descriptions_free
          pango_font_face_describe
          pango_font_face_get_face_name
          pango_font_face_get_type
          pango_font_face_is_synthesized
          pango_font_face_list_sizes
          pango_font_family_get_name
          pango_font_family_get_type
          pango_font_family_is_monospace
          pango_font_family_list_faces
          pango_font_find_shaper
          pango_font_get_coverage
          pango_font_get_font_map
          pango_font_get_glyph_extents
          pango_font_get_metrics
          pango_font_get_type
          pango_font_map_create_context
          pango_font_map_get_type
          pango_font_map_list_families
          pango_font_map_load_font
          pango_font_map_load_fontset
          pango_font_mask_get_type
          pango_font_metrics_get_approximate_char_width
          pango_font_metrics_get_approximate_digit_width
          pango_font_metrics_get_ascent
          pango_font_metrics_get_descent
          pango_font_metrics_get_strikethrough_position
          pango_font_metrics_get_strikethrough_thickness
          pango_font_metrics_get_type
          pango_font_metrics_get_underline_position
          pango_font_metrics_get_underline_thickness
          pango_font_metrics_ref
          pango_font_metrics_unref)

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

  ;; PangoFontDescription* pango_font_describe (PangoFont* font)
  (define-function void* pango_font_describe (void*))

  ;; PangoFontDescription* pango_font_describe_with_absolute_size (PangoFont* font)
  (define-function void* pango_font_describe_with_absolute_size (void*))

  ;; gboolean pango_font_description_better_match (const PangoFontDescription* desc, const PangoFontDescription* old_match, const PangoFontDescription* new_match)
  (define-function int pango_font_description_better_match (void* void* void*))

  ;; PangoFontDescription* pango_font_description_copy (const PangoFontDescription* desc)
  (define-function void* pango_font_description_copy (void*))

  ;; PangoFontDescription* pango_font_description_copy_static (const PangoFontDescription* desc)
  (define-function void* pango_font_description_copy_static (void*))

  ;; gboolean pango_font_description_equal (const PangoFontDescription* desc1, const PangoFontDescription* desc2)
  (define-function int pango_font_description_equal (void* void*))

  ;; void pango_font_description_free (PangoFontDescription* desc)
  (define-function void pango_font_description_free (void*))

  ;; PangoFontDescription* pango_font_description_from_string (const char* str)
  (define-function void* pango_font_description_from_string (char*))

  ;; const char* pango_font_description_get_family (const PangoFontDescription* desc)
  (define-function char* pango_font_description_get_family (void*))

  ;; PangoGravity pango_font_description_get_gravity (const PangoFontDescription* desc)
  (define-function int pango_font_description_get_gravity (void*))

  ;; PangoFontMask pango_font_description_get_set_fields (const PangoFontDescription* desc)
  (define-function int pango_font_description_get_set_fields (void*))

  ;; gint pango_font_description_get_size (const PangoFontDescription* desc)
  (define-function int pango_font_description_get_size (void*))

  ;; gboolean pango_font_description_get_size_is_absolute (const PangoFontDescription* desc)
  (define-function int pango_font_description_get_size_is_absolute (void*))

  ;; PangoStretch pango_font_description_get_stretch (const PangoFontDescription* desc)
  (define-function int pango_font_description_get_stretch (void*))

  ;; PangoStyle pango_font_description_get_style (const PangoFontDescription* desc)
  (define-function int pango_font_description_get_style (void*))

  ;; GType pango_font_description_get_type (void)
  (define-function unsigned-long pango_font_description_get_type ())

  ;; PangoVariant pango_font_description_get_variant (const PangoFontDescription* desc)
  (define-function int pango_font_description_get_variant (void*))

  ;; PangoWeight pango_font_description_get_weight (const PangoFontDescription* desc)
  (define-function int pango_font_description_get_weight (void*))

  ;; guint pango_font_description_hash (const PangoFontDescription* desc)
  (define-function unsigned-int pango_font_description_hash (void*))

  ;; void pango_font_description_merge (PangoFontDescription* desc, const PangoFontDescription* desc_to_merge, gboolean replace_existing)
  (define-function void pango_font_description_merge (void* void* int))

  ;; void pango_font_description_merge_static (PangoFontDescription* desc, const PangoFontDescription* desc_to_merge, gboolean replace_existing)
  (define-function void pango_font_description_merge_static (void* void* int))

  ;; PangoFontDescription* pango_font_description_new (void)
  (define-function void* pango_font_description_new ())

  ;; void pango_font_description_set_absolute_size (PangoFontDescription* desc, double size)
  (define-function void pango_font_description_set_absolute_size (void* double))

  ;; void pango_font_description_set_family (PangoFontDescription* desc, const char* family)
  (define-function void pango_font_description_set_family (void* char*))

  ;; void pango_font_description_set_family_static (PangoFontDescription* desc, const char* family)
  (define-function void pango_font_description_set_family_static (void* char*))

  ;; void pango_font_description_set_gravity (PangoFontDescription* desc, PangoGravity gravity)
  (define-function void pango_font_description_set_gravity (void* int))

  ;; void pango_font_description_set_size (PangoFontDescription* desc, gint size)
  (define-function void pango_font_description_set_size (void* int))

  ;; void pango_font_description_set_stretch (PangoFontDescription* desc, PangoStretch stretch)
  (define-function void pango_font_description_set_stretch (void* int))

  ;; void pango_font_description_set_style (PangoFontDescription* desc, PangoStyle style)
  (define-function void pango_font_description_set_style (void* int))

  ;; void pango_font_description_set_variant (PangoFontDescription* desc, PangoVariant variant)
  (define-function void pango_font_description_set_variant (void* int))

  ;; void pango_font_description_set_weight (PangoFontDescription* desc, PangoWeight weight)
  (define-function void pango_font_description_set_weight (void* int))

  ;; char* pango_font_description_to_filename (const PangoFontDescription* desc)
  (define-function char* pango_font_description_to_filename (void*))

  ;; char* pango_font_description_to_string (const PangoFontDescription* desc)
  (define-function char* pango_font_description_to_string (void*))

  ;; void pango_font_description_unset_fields (PangoFontDescription* desc, PangoFontMask to_unset)
  (define-function void pango_font_description_unset_fields (void* int))

  ;; void pango_font_descriptions_free (PangoFontDescription** descs, int n_descs)
  (define-function void pango_font_descriptions_free (void* int))

  ;; PangoFontDescription* pango_font_face_describe (PangoFontFace* face)
  (define-function void* pango_font_face_describe (void*))

  ;; const char* pango_font_face_get_face_name (PangoFontFace* face)
  (define-function char* pango_font_face_get_face_name (void*))

  ;; GType pango_font_face_get_type (void)
  (define-function unsigned-long pango_font_face_get_type ())

  ;; gboolean pango_font_face_is_synthesized (PangoFontFace* face)
  (define-function int pango_font_face_is_synthesized (void*))

  ;; void pango_font_face_list_sizes (PangoFontFace* face, int** sizes, int* n_sizes)
  (define-function void pango_font_face_list_sizes (void* void* void*))

  ;; const char* pango_font_family_get_name (PangoFontFamily* family)
  (define-function char* pango_font_family_get_name (void*))

  ;; GType pango_font_family_get_type (void)
  (define-function unsigned-long pango_font_family_get_type ())

  ;; gboolean pango_font_family_is_monospace (PangoFontFamily* family)
  (define-function int pango_font_family_is_monospace (void*))

  ;; void pango_font_family_list_faces (PangoFontFamily* family, PangoFontFace** *faces, int* n_faces)
  (define-function void pango_font_family_list_faces (void* void* void*))

  ;; PangoEngineShape* pango_font_find_shaper (PangoFont* font, PangoLanguage* language, guint32 ch)
  (define-function void* pango_font_find_shaper (void* void* uint32_t))

  ;; PangoCoverage* pango_font_get_coverage (PangoFont* font, PangoLanguage* language)
  (define-function void* pango_font_get_coverage (void* void*))

  ;; PangoFontMap* pango_font_get_font_map (PangoFont* font)
  (define-function void* pango_font_get_font_map (void*))

  ;; void pango_font_get_glyph_extents (PangoFont* font, PangoGlyph glyph, PangoRectangle* ink_rect, PangoRectangle* logical_rect)
  (define-function void pango_font_get_glyph_extents (void* uint32_t void* void*))

  ;; PangoFontMetrics* pango_font_get_metrics (PangoFont* font, PangoLanguage* language)
  (define-function void* pango_font_get_metrics (void* void*))

  ;; GType pango_font_get_type (void)
  (define-function unsigned-long pango_font_get_type ())

  ;; PangoContext* pango_font_map_create_context (PangoFontMap* fontmap)
  (define-function void* pango_font_map_create_context (void*))

  ;; GType pango_font_map_get_type (void)
  (define-function unsigned-long pango_font_map_get_type ())

  ;; void pango_font_map_list_families (PangoFontMap* fontmap, PangoFontFamily** *families, int* n_families)
  (define-function void pango_font_map_list_families (void* void* void*))

  ;; PangoFont* pango_font_map_load_font (PangoFontMap* fontmap, PangoContext* context, const PangoFontDescription* desc)
  (define-function void* pango_font_map_load_font (void* void* void*))

  ;; PangoFontset* pango_font_map_load_fontset (PangoFontMap* fontmap, PangoContext* context, const PangoFontDescription* desc, PangoLanguage* language)
  (define-function void* pango_font_map_load_fontset (void* void* void* void*))

  ;; GType pango_font_mask_get_type (void)
  (define-function unsigned-long pango_font_mask_get_type ())

  ;; int pango_font_metrics_get_approximate_char_width (PangoFontMetrics* metrics)
  (define-function int pango_font_metrics_get_approximate_char_width (void*))

  ;; int pango_font_metrics_get_approximate_digit_width (PangoFontMetrics* metrics)
  (define-function int pango_font_metrics_get_approximate_digit_width (void*))

  ;; int pango_font_metrics_get_ascent (PangoFontMetrics* metrics)
  (define-function int pango_font_metrics_get_ascent (void*))

  ;; int pango_font_metrics_get_descent (PangoFontMetrics* metrics)
  (define-function int pango_font_metrics_get_descent (void*))

  ;; int pango_font_metrics_get_strikethrough_position (PangoFontMetrics* metrics)
  (define-function int pango_font_metrics_get_strikethrough_position (void*))

  ;; int pango_font_metrics_get_strikethrough_thickness (PangoFontMetrics* metrics)
  (define-function int pango_font_metrics_get_strikethrough_thickness (void*))

  ;; GType pango_font_metrics_get_type (void)
  (define-function unsigned-long pango_font_metrics_get_type ())

  ;; int pango_font_metrics_get_underline_position (PangoFontMetrics* metrics)
  (define-function int pango_font_metrics_get_underline_position (void*))

  ;; int pango_font_metrics_get_underline_thickness (PangoFontMetrics* metrics)
  (define-function int pango_font_metrics_get_underline_thickness (void*))

  ;; PangoFontMetrics* pango_font_metrics_ref (PangoFontMetrics* metrics)
  (define-function void* pango_font_metrics_ref (void*))

  ;; void pango_font_metrics_unref (PangoFontMetrics* metrics)
  (define-function void pango_font_metrics_unref (void*))

  ) ;[end]
