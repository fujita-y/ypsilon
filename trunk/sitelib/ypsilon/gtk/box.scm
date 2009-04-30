#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk box)

  (export gtk_box_get_homogeneous
          gtk_box_get_spacing
          gtk_box_get_type
          gtk_box_pack_end
          gtk_box_pack_start
          gtk_box_query_child_packing
          gtk_box_reorder_child
          gtk_box_set_child_packing
          gtk_box_set_homogeneous
          gtk_box_set_spacing)

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

  ;; gboolean gtk_box_get_homogeneous (GtkBox* box)
  (define-function int gtk_box_get_homogeneous (void*))

  ;; gint gtk_box_get_spacing (GtkBox* box)
  (define-function int gtk_box_get_spacing (void*))

  ;; GType gtk_box_get_type (void)
  (define-function unsigned-long gtk_box_get_type ())

  ;; void gtk_box_pack_end (GtkBox* box, GtkWidget* child, gboolean expand, gboolean fill, guint padding)
  (define-function void gtk_box_pack_end (void* void* int int unsigned-int))

  ;; void gtk_box_pack_start (GtkBox* box, GtkWidget* child, gboolean expand, gboolean fill, guint padding)
  (define-function void gtk_box_pack_start (void* void* int int unsigned-int))

  ;; void gtk_box_query_child_packing (GtkBox* box, GtkWidget* child, gboolean* expand, gboolean* fill, guint* padding, GtkPackType* pack_type)
  (define-function void gtk_box_query_child_packing (void* void* void* void* void* void*))

  ;; void gtk_box_reorder_child (GtkBox* box, GtkWidget* child, gint position)
  (define-function void gtk_box_reorder_child (void* void* int))

  ;; void gtk_box_set_child_packing (GtkBox* box, GtkWidget* child, gboolean expand, gboolean fill, guint padding, GtkPackType pack_type)
  (define-function void gtk_box_set_child_packing (void* void* int int unsigned-int int))

  ;; void gtk_box_set_homogeneous (GtkBox* box, gboolean homogeneous)
  (define-function void gtk_box_set_homogeneous (void* int))

  ;; void gtk_box_set_spacing (GtkBox* box, gint spacing)
  (define-function void gtk_box_set_spacing (void* int))

  ) ;[end]
