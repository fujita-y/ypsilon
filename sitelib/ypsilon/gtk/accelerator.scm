#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk accelerator)

  (export gtk_accelerator_get_default_mod_mask
          gtk_accelerator_get_label
          gtk_accelerator_name
          gtk_accelerator_parse
          gtk_accelerator_set_default_mod_mask
          gtk_accelerator_valid)

  (import (rnrs) (ypsilon ffi))

  (define lib-name
    (cond (on-linux   "libgtk-x11-2.0.so.0")
          (on-sunos   "libgtk-x11-2.0.so.0")
          (on-freebsd "libgtk-x11-2.0.so.0")
          (on-openbsd "libgtk-x11-2.0.so.0")
          (on-darwin  "Gtk.framework/Gtk")
          (on-windows "libgtk-win32-2.0-0.dll")
          (else
           (assertion-violation #f "can not locate GTK library, unknown operating system"))))

  (define lib (load-shared-object lib-name))

  (define-syntax define-function
    (syntax-rules ()
      ((_ ret name args)
       (define name (c-function lib lib-name ret name args)))))

  (define-syntax define-function/va_list
    (syntax-rules ()
      ((_ ret name args)
      (define name (lambda x (assertion-violation 'name "va_list argument not supported"))))))

  ;; guint gtk_accelerator_get_default_mod_mask (void)
  (define-function unsigned-int gtk_accelerator_get_default_mod_mask ())

  ;; gchar* gtk_accelerator_get_label (guint accelerator_key, GdkModifierType accelerator_mods)
  (define-function char* gtk_accelerator_get_label (unsigned-int int))

  ;; gchar* gtk_accelerator_name (guint accelerator_key, GdkModifierType accelerator_mods)
  (define-function char* gtk_accelerator_name (unsigned-int int))

  ;; void gtk_accelerator_parse (const gchar* accelerator, guint* accelerator_key, GdkModifierType* accelerator_mods)
  (define-function void gtk_accelerator_parse (char* void* void*))

  ;; void gtk_accelerator_set_default_mod_mask (GdkModifierType default_mod_mask)
  (define-function void gtk_accelerator_set_default_mod_mask (int))

  ;; gboolean gtk_accelerator_valid (guint keyval, GdkModifierType modifiers)
  (define-function int gtk_accelerator_valid (unsigned-int int))

  ) ;[end]
