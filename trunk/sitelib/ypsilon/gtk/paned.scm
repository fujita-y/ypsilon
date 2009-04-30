#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk paned)

  (export gtk_paned_add1
          gtk_paned_add2
          gtk_paned_get_child1
          gtk_paned_get_child2
          gtk_paned_get_position
          gtk_paned_get_type
          gtk_paned_pack1
          gtk_paned_pack2
          gtk_paned_set_position)

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

  ;; void gtk_paned_add1 (GtkPaned* paned, GtkWidget* child)
  (define-function void gtk_paned_add1 (void* void*))

  ;; void gtk_paned_add2 (GtkPaned* paned, GtkWidget* child)
  (define-function void gtk_paned_add2 (void* void*))

  ;; GtkWidget* gtk_paned_get_child1 (GtkPaned* paned)
  (define-function void* gtk_paned_get_child1 (void*))

  ;; GtkWidget* gtk_paned_get_child2 (GtkPaned* paned)
  (define-function void* gtk_paned_get_child2 (void*))

  ;; gint gtk_paned_get_position (GtkPaned* paned)
  (define-function int gtk_paned_get_position (void*))

  ;; GType gtk_paned_get_type (void)
  (define-function unsigned-long gtk_paned_get_type ())

  ;; void gtk_paned_pack1 (GtkPaned* paned, GtkWidget* child, gboolean resize, gboolean shrink)
  (define-function void gtk_paned_pack1 (void* void* int int))

  ;; void gtk_paned_pack2 (GtkPaned* paned, GtkWidget* child, gboolean resize, gboolean shrink)
  (define-function void gtk_paned_pack2 (void* void* int int))

  ;; void gtk_paned_set_position (GtkPaned* paned, gint position)
  (define-function void gtk_paned_set_position (void* int))

  ) ;[end]
