#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk mount)

  (export gtk_mount_operation_get_parent
          gtk_mount_operation_get_screen
          gtk_mount_operation_get_type
          gtk_mount_operation_is_showing
          gtk_mount_operation_new
          gtk_mount_operation_set_parent
          gtk_mount_operation_set_screen)

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

  ;; GtkWindow* gtk_mount_operation_get_parent (GtkMountOperation* op)
  (define-function void* gtk_mount_operation_get_parent (void*))

  ;; GdkScreen* gtk_mount_operation_get_screen (GtkMountOperation* op)
  (define-function void* gtk_mount_operation_get_screen (void*))

  ;; GType gtk_mount_operation_get_type (void)
  (define-function unsigned-long gtk_mount_operation_get_type ())

  ;; gboolean gtk_mount_operation_is_showing (GtkMountOperation* op)
  (define-function int gtk_mount_operation_is_showing (void*))

  ;; GMountOperation* gtk_mount_operation_new (GtkWindow* parent)
  (define-function void* gtk_mount_operation_new (void*))

  ;; void gtk_mount_operation_set_parent (GtkMountOperation* op, GtkWindow* parent)
  (define-function void gtk_mount_operation_set_parent (void* void*))

  ;; void gtk_mount_operation_set_screen (GtkMountOperation* op, GdkScreen* screen)
  (define-function void gtk_mount_operation_set_screen (void* void*))

  ) ;[end]
