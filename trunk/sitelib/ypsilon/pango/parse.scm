#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon pango parse)

  (export pango_parse_enum
          pango_parse_markup
          pango_parse_stretch
          pango_parse_style
          pango_parse_variant
          pango_parse_weight)

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

  ;; gboolean pango_parse_enum (GType type, const char* str, int* value, gboolean warn, char** possible_values)
  (define-function int pango_parse_enum (unsigned-long char* void* int void*))

  ;; gboolean pango_parse_markup (const char* markup_text, int length, gunichar accel_marker, PangoAttrList** attr_list, char** text, gunichar* accel_char, GError** error)
  (define-function int pango_parse_markup (char* int uint32_t void* void* void* void*))

  ;; gboolean pango_parse_stretch (const char* str, PangoStretch* stretch, gboolean warn)
  (define-function int pango_parse_stretch (char* void* int))

  ;; gboolean pango_parse_style (const char* str, PangoStyle* style, gboolean warn)
  (define-function int pango_parse_style (char* void* int))

  ;; gboolean pango_parse_variant (const char* str, PangoVariant* variant, gboolean warn)
  (define-function int pango_parse_variant (char* void* int))

  ;; gboolean pango_parse_weight (const char* str, PangoWeight* weight, gboolean warn)
  (define-function int pango_parse_weight (char* void* int))

  ) ;[end]
