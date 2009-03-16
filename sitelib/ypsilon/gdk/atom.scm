#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gdk atom)

  (export gdk_atom_intern
          gdk_atom_intern_static_string
          gdk_atom_name)

  (import (rnrs) (ypsilon ffi))

  (define lib-name
    (cond (on-darwin  "Gdk.framework/Gdk")
          (on-linux   "libgdk-x11-2.0.so.0")
          (on-freebsd "libgdk-x11-2.0.so.0")
          (on-openbsd "libgdk-x11-2.0.so.0")
          (on-windows "libgdk-win32-2.0-0.dll")
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

  ;; GdkAtom gdk_atom_intern (const gchar* atom_name, gboolean only_if_exists)
  (define-function void* gdk_atom_intern (char* int))

  ;; GdkAtom gdk_atom_intern_static_string (const gchar* atom_name)
  (define-function void* gdk_atom_intern_static_string (char*))

  ;; gchar* gdk_atom_name (GdkAtom atom)
  (define-function char* gdk_atom_name (void*))

  ) ;[end]
