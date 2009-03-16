#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon pango tab)

  (export pango_tab_align_get_type
          pango_tab_array_copy
          pango_tab_array_free
          pango_tab_array_get_positions_in_pixels
          pango_tab_array_get_size
          pango_tab_array_get_tab
          pango_tab_array_get_tabs
          pango_tab_array_get_type
          pango_tab_array_new
          pango_tab_array_new_with_positions
          pango_tab_array_resize
          pango_tab_array_set_tab)

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

  ;; GType pango_tab_align_get_type (void)
  (define-function unsigned-long pango_tab_align_get_type ())

  ;; PangoTabArray* pango_tab_array_copy (PangoTabArray* src)
  (define-function void* pango_tab_array_copy (void*))

  ;; void pango_tab_array_free (PangoTabArray* tab_array)
  (define-function void pango_tab_array_free (void*))

  ;; gboolean pango_tab_array_get_positions_in_pixels (PangoTabArray* tab_array)
  (define-function int pango_tab_array_get_positions_in_pixels (void*))

  ;; gint pango_tab_array_get_size (PangoTabArray* tab_array)
  (define-function int pango_tab_array_get_size (void*))

  ;; void pango_tab_array_get_tab (PangoTabArray* tab_array, gint tab_index, PangoTabAlign* alignment, gint* location)
  (define-function void pango_tab_array_get_tab (void* int void* void*))

  ;; void pango_tab_array_get_tabs (PangoTabArray* tab_array, PangoTabAlign** alignments, gint** locations)
  (define-function void pango_tab_array_get_tabs (void* void* void*))

  ;; GType pango_tab_array_get_type (void)
  (define-function unsigned-long pango_tab_array_get_type ())

  ;; PangoTabArray* pango_tab_array_new (gint initial_size, gboolean positions_in_pixels)
  (define-function void* pango_tab_array_new (int int))

  ;; PangoTabArray* pango_tab_array_new_with_positions (gint size, gboolean positions_in_pixels, PangoTabAlign first_alignment, gint first_position, ...)
  (define-variadic-function void* pango_tab_array_new_with_positions (int int int int ...))

  ;; void pango_tab_array_resize (PangoTabArray* tab_array, gint new_size)
  (define-function void pango_tab_array_resize (void* int))

  ;; void pango_tab_array_set_tab (PangoTabArray* tab_array, gint tab_index, PangoTabAlign alignment, gint location)
  (define-function void pango_tab_array_set_tab (void* int int int))

  ) ;[end]
