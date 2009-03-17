#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon pango item)

  (export pango_item_copy
          pango_item_free
          pango_item_get_type
          pango_item_new
          pango_item_split)

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

  ;; PangoItem* pango_item_copy (PangoItem* item)
  (define-function void* pango_item_copy (void*))

  ;; void pango_item_free (PangoItem* item)
  (define-function void pango_item_free (void*))

  ;; GType pango_item_get_type (void)
  (define-function unsigned-long pango_item_get_type ())

  ;; PangoItem* pango_item_new (void)
  (define-function void* pango_item_new ())

  ;; PangoItem* pango_item_split (PangoItem* orig, int split_index, int split_offset)
  (define-function void* pango_item_split (void* int int))

  ) ;[end]
