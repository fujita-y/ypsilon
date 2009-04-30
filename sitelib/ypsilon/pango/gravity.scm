#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon pango gravity)

  (export pango_gravity_get_for_matrix
          pango_gravity_get_for_script
          pango_gravity_get_type
          pango_gravity_hint_get_type
          pango_gravity_to_rotation)

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

  ;; PangoGravity pango_gravity_get_for_matrix (const PangoMatrix* matrix)
  (define-function int pango_gravity_get_for_matrix (void*))

  ;; PangoGravity pango_gravity_get_for_script (PangoScript script, PangoGravity base_gravity, PangoGravityHint hint)
  (define-function int pango_gravity_get_for_script (int int int))

  ;; GType pango_gravity_get_type (void)
  (define-function unsigned-long pango_gravity_get_type ())

  ;; GType pango_gravity_hint_get_type (void)
  (define-function unsigned-long pango_gravity_hint_get_type ())

  ;; double pango_gravity_to_rotation (PangoGravity gravity)
  (define-function double pango_gravity_to_rotation (int))

  ) ;[end]
