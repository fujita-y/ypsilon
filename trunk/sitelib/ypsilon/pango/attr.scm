#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon pango attr)

  (export pango_attr_background_new
          pango_attr_fallback_new
          pango_attr_family_new
          pango_attr_font_desc_new
          pango_attr_foreground_new
          pango_attr_gravity_hint_new
          pango_attr_gravity_new
          pango_attr_iterator_copy
          pango_attr_iterator_destroy
          pango_attr_iterator_get
          pango_attr_iterator_get_attrs
          pango_attr_iterator_get_font
          pango_attr_iterator_next
          pango_attr_iterator_range
          pango_attr_language_new
          pango_attr_letter_spacing_new
          pango_attr_list_change
          pango_attr_list_copy
          pango_attr_list_filter
          pango_attr_list_get_iterator
          pango_attr_list_get_type
          pango_attr_list_insert
          pango_attr_list_insert_before
          pango_attr_list_new
          pango_attr_list_ref
          pango_attr_list_splice
          pango_attr_list_unref
          pango_attr_rise_new
          pango_attr_scale_new
          pango_attr_shape_new
          pango_attr_shape_new_with_data
          pango_attr_size_new
          pango_attr_size_new_absolute
          pango_attr_stretch_new
          pango_attr_strikethrough_color_new
          pango_attr_strikethrough_new
          pango_attr_style_new
          pango_attr_type_get_name
          pango_attr_type_get_type
          pango_attr_type_register
          pango_attr_underline_color_new
          pango_attr_underline_new
          pango_attr_variant_new
          pango_attr_weight_new)

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

  ;; PangoAttribute* pango_attr_background_new (guint16 red, guint16 green, guint16 blue)
  (define-function void* pango_attr_background_new (uint16_t uint16_t uint16_t))

  ;; PangoAttribute* pango_attr_fallback_new (gboolean enable_fallback)
  (define-function void* pango_attr_fallback_new (int))

  ;; PangoAttribute* pango_attr_family_new (const char* family)
  (define-function void* pango_attr_family_new (char*))

  ;; PangoAttribute* pango_attr_font_desc_new (const PangoFontDescription* desc)
  (define-function void* pango_attr_font_desc_new (void*))

  ;; PangoAttribute* pango_attr_foreground_new (guint16 red, guint16 green, guint16 blue)
  (define-function void* pango_attr_foreground_new (uint16_t uint16_t uint16_t))

  ;; PangoAttribute* pango_attr_gravity_hint_new (PangoGravityHint hint)
  (define-function void* pango_attr_gravity_hint_new (int))

  ;; PangoAttribute* pango_attr_gravity_new (PangoGravity gravity)
  (define-function void* pango_attr_gravity_new (int))

  ;; PangoAttrIterator* pango_attr_iterator_copy (PangoAttrIterator* iterator)
  (define-function void* pango_attr_iterator_copy (void*))

  ;; void pango_attr_iterator_destroy (PangoAttrIterator* iterator)
  (define-function void pango_attr_iterator_destroy (void*))

  ;; PangoAttribute* pango_attr_iterator_get (PangoAttrIterator* iterator, PangoAttrType type)
  (define-function void* pango_attr_iterator_get (void* int))

  ;; GSList* pango_attr_iterator_get_attrs (PangoAttrIterator* iterator)
  (define-function void* pango_attr_iterator_get_attrs (void*))

  ;; void pango_attr_iterator_get_font (PangoAttrIterator* iterator, PangoFontDescription* desc, PangoLanguage** language, GSList** extra_attrs)
  (define-function void pango_attr_iterator_get_font (void* void* void* void*))

  ;; gboolean pango_attr_iterator_next (PangoAttrIterator* iterator)
  (define-function int pango_attr_iterator_next (void*))

  ;; void pango_attr_iterator_range (PangoAttrIterator* iterator, gint* start, gint* end)
  (define-function void pango_attr_iterator_range (void* void* void*))

  ;; PangoAttribute* pango_attr_language_new (PangoLanguage* language)
  (define-function void* pango_attr_language_new (void*))

  ;; PangoAttribute* pango_attr_letter_spacing_new (int letter_spacing)
  (define-function void* pango_attr_letter_spacing_new (int))

  ;; void pango_attr_list_change (PangoAttrList* list, PangoAttribute* attr)
  (define-function void pango_attr_list_change (void* void*))

  ;; PangoAttrList* pango_attr_list_copy (PangoAttrList* list)
  (define-function void* pango_attr_list_copy (void*))

  ;; PangoAttrList* pango_attr_list_filter (PangoAttrList* list, PangoAttrFilterFunc func, gpointer data)
  (define-function void* pango_attr_list_filter (void* (c-callback int (void*)) void*))

  ;; PangoAttrIterator* pango_attr_list_get_iterator (PangoAttrList* list)
  (define-function void* pango_attr_list_get_iterator (void*))

  ;; GType pango_attr_list_get_type (void)
  (define-function unsigned-long pango_attr_list_get_type ())

  ;; void pango_attr_list_insert (PangoAttrList* list, PangoAttribute* attr)
  (define-function void pango_attr_list_insert (void* void*))

  ;; void pango_attr_list_insert_before (PangoAttrList* list, PangoAttribute* attr)
  (define-function void pango_attr_list_insert_before (void* void*))

  ;; PangoAttrList* pango_attr_list_new (void)
  (define-function void* pango_attr_list_new ())

  ;; PangoAttrList* pango_attr_list_ref (PangoAttrList* list)
  (define-function void* pango_attr_list_ref (void*))

  ;; void pango_attr_list_splice (PangoAttrList* list, PangoAttrList* other, gint pos, gint len)
  (define-function void pango_attr_list_splice (void* void* int int))

  ;; void pango_attr_list_unref (PangoAttrList* list)
  (define-function void pango_attr_list_unref (void*))

  ;; PangoAttribute* pango_attr_rise_new (int rise)
  (define-function void* pango_attr_rise_new (int))

  ;; PangoAttribute* pango_attr_scale_new (double scale_factor)
  (define-function void* pango_attr_scale_new (double))

  ;; PangoAttribute* pango_attr_shape_new (const PangoRectangle* ink_rect, const PangoRectangle* logical_rect)
  (define-function void* pango_attr_shape_new (void* void*))

  ;; PangoAttribute* pango_attr_shape_new_with_data (const PangoRectangle* ink_rect, const PangoRectangle* logical_rect, gpointer data, PangoAttrDataCopyFunc copy_func, GDestroyNotify destroy_func)
  (define-function void* pango_attr_shape_new_with_data (void* void* void* (c-callback void* (void*)) (c-callback void (void*))))

  ;; PangoAttribute* pango_attr_size_new (int size)
  (define-function void* pango_attr_size_new (int))

  ;; PangoAttribute* pango_attr_size_new_absolute (int size)
  (define-function void* pango_attr_size_new_absolute (int))

  ;; PangoAttribute* pango_attr_stretch_new (PangoStretch stretch)
  (define-function void* pango_attr_stretch_new (int))

  ;; PangoAttribute* pango_attr_strikethrough_color_new (guint16 red, guint16 green, guint16 blue)
  (define-function void* pango_attr_strikethrough_color_new (uint16_t uint16_t uint16_t))

  ;; PangoAttribute* pango_attr_strikethrough_new (gboolean strikethrough)
  (define-function void* pango_attr_strikethrough_new (int))

  ;; PangoAttribute* pango_attr_style_new (PangoStyle style)
  (define-function void* pango_attr_style_new (int))

  ;; const char* pango_attr_type_get_name (PangoAttrType type)
  (define-function char* pango_attr_type_get_name (int))

  ;; GType pango_attr_type_get_type (void)
  (define-function unsigned-long pango_attr_type_get_type ())

  ;; PangoAttrType pango_attr_type_register (const gchar* name)
  (define-function int pango_attr_type_register (char*))

  ;; PangoAttribute* pango_attr_underline_color_new (guint16 red, guint16 green, guint16 blue)
  (define-function void* pango_attr_underline_color_new (uint16_t uint16_t uint16_t))

  ;; PangoAttribute* pango_attr_underline_new (PangoUnderline underline)
  (define-function void* pango_attr_underline_new (int))

  ;; PangoAttribute* pango_attr_variant_new (PangoVariant variant)
  (define-function void* pango_attr_variant_new (int))

  ;; PangoAttribute* pango_attr_weight_new (PangoWeight weight)
  (define-function void* pango_attr_weight_new (int))

  ) ;[end]
