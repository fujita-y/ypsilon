#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon pango context)

  (export pango_context_get_base_dir
          pango_context_get_base_gravity
          pango_context_get_font_description
          pango_context_get_font_map
          pango_context_get_gravity
          pango_context_get_gravity_hint
          pango_context_get_language
          pango_context_get_matrix
          pango_context_get_metrics
          pango_context_get_type
          pango_context_list_families
          pango_context_load_font
          pango_context_load_fontset
          pango_context_new
          pango_context_set_base_dir
          pango_context_set_base_gravity
          pango_context_set_font_description
          pango_context_set_font_map
          pango_context_set_gravity_hint
          pango_context_set_language
          pango_context_set_matrix)

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

  ;; PangoDirection pango_context_get_base_dir (PangoContext* context)
  (define-function int pango_context_get_base_dir (void*))

  ;; PangoGravity pango_context_get_base_gravity (PangoContext* context)
  (define-function int pango_context_get_base_gravity (void*))

  ;; PangoFontDescription* pango_context_get_font_description (PangoContext* context)
  (define-function void* pango_context_get_font_description (void*))

  ;; PangoFontMap* pango_context_get_font_map (PangoContext* context)
  (define-function void* pango_context_get_font_map (void*))

  ;; PangoGravity pango_context_get_gravity (PangoContext* context)
  (define-function int pango_context_get_gravity (void*))

  ;; PangoGravityHint pango_context_get_gravity_hint (PangoContext* context)
  (define-function int pango_context_get_gravity_hint (void*))

  ;; PangoLanguage* pango_context_get_language (PangoContext* context)
  (define-function void* pango_context_get_language (void*))

  ;; const PangoMatrix* pango_context_get_matrix (PangoContext* context)
  (define-function void* pango_context_get_matrix (void*))

  ;; PangoFontMetrics* pango_context_get_metrics (PangoContext* context, const PangoFontDescription* desc, PangoLanguage* language)
  (define-function void* pango_context_get_metrics (void* void* void*))

  ;; GType pango_context_get_type (void)
  (define-function unsigned-long pango_context_get_type ())

  ;; void pango_context_list_families (PangoContext* context, PangoFontFamily** *families, int* n_families)
  (define-function void pango_context_list_families (void* void* void*))

  ;; PangoFont* pango_context_load_font (PangoContext* context, const PangoFontDescription* desc)
  (define-function void* pango_context_load_font (void* void*))

  ;; PangoFontset* pango_context_load_fontset (PangoContext* context, const PangoFontDescription* desc, PangoLanguage* language)
  (define-function void* pango_context_load_fontset (void* void* void*))

  ;; PangoContext* pango_context_new (void)
  (define-function void* pango_context_new ())

  ;; void pango_context_set_base_dir (PangoContext* context, PangoDirection direction)
  (define-function void pango_context_set_base_dir (void* int))

  ;; void pango_context_set_base_gravity (PangoContext* context, PangoGravity gravity)
  (define-function void pango_context_set_base_gravity (void* int))

  ;; void pango_context_set_font_description (PangoContext* context, const PangoFontDescription* desc)
  (define-function void pango_context_set_font_description (void* void*))

  ;; void pango_context_set_font_map (PangoContext* context, PangoFontMap* font_map)
  (define-function void pango_context_set_font_map (void* void*))

  ;; void pango_context_set_gravity_hint (PangoContext* context, PangoGravityHint hint)
  (define-function void pango_context_set_gravity_hint (void* int))

  ;; void pango_context_set_language (PangoContext* context, PangoLanguage* language)
  (define-function void pango_context_set_language (void* void*))

  ;; void pango_context_set_matrix (PangoContext* context, const PangoMatrix* matrix)
  (define-function void pango_context_set_matrix (void* void*))

  ) ;[end]
