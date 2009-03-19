#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk requisition)

  (export gtk_requisition_copy
          gtk_requisition_free
          gtk_requisition_get_type)

  (import (rnrs) (ypsilon ffi))

  (define lib-name
    (cond (on-darwin  "Gtk.framework/Gtk")
          (on-linux   "libgtk-x11-2.0.so.0")
          (on-freebsd "libgtk-x11-2.0.so.0")
          (on-openbsd "libgtk-x11-2.0.so.0")
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

  ;; GtkRequisition* gtk_requisition_copy (const GtkRequisition* requisition)
  (define-function void* gtk_requisition_copy (void*))

  ;; void gtk_requisition_free (GtkRequisition* requisition)
  (define-function void gtk_requisition_free (void*))

  ;; GType gtk_requisition_get_type (void)
  (define-function unsigned-long gtk_requisition_get_type ())

  ) ;[end]
