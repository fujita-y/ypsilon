#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk table)

  (export gtk_table_attach
          gtk_table_attach_defaults
          gtk_table_get_col_spacing
          gtk_table_get_default_col_spacing
          gtk_table_get_default_row_spacing
          gtk_table_get_homogeneous
          gtk_table_get_row_spacing
          gtk_table_get_type
          gtk_table_new
          gtk_table_resize
          gtk_table_set_col_spacing
          gtk_table_set_col_spacings
          gtk_table_set_homogeneous
          gtk_table_set_row_spacing
          gtk_table_set_row_spacings)

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

  (define-syntax define-variadic-function
    (syntax-rules ()
      ((_ ret name args)
      (define name (lambda x (assertion-violation 'name "variadic function not supported"))))))

  ;; void gtk_table_attach (GtkTable* table, GtkWidget* child, guint left_attach, guint right_attach, guint top_attach, guint bottom_attach, GtkAttachOptions xoptions, GtkAttachOptions yoptions, guint xpadding, guint ypadding)
  (define-function void gtk_table_attach (void* void* unsigned-int unsigned-int unsigned-int unsigned-int int int unsigned-int unsigned-int))

  ;; void gtk_table_attach_defaults (GtkTable* table, GtkWidget* widget, guint left_attach, guint right_attach, guint top_attach, guint bottom_attach)
  (define-function void gtk_table_attach_defaults (void* void* unsigned-int unsigned-int unsigned-int unsigned-int))

  ;; guint gtk_table_get_col_spacing (GtkTable* table, guint column)
  (define-function unsigned-int gtk_table_get_col_spacing (void* unsigned-int))

  ;; guint gtk_table_get_default_col_spacing (GtkTable* table)
  (define-function unsigned-int gtk_table_get_default_col_spacing (void*))

  ;; guint gtk_table_get_default_row_spacing (GtkTable* table)
  (define-function unsigned-int gtk_table_get_default_row_spacing (void*))

  ;; gboolean gtk_table_get_homogeneous (GtkTable* table)
  (define-function int gtk_table_get_homogeneous (void*))

  ;; guint gtk_table_get_row_spacing (GtkTable* table, guint row)
  (define-function unsigned-int gtk_table_get_row_spacing (void* unsigned-int))

  ;; GType gtk_table_get_type (void)
  (define-function unsigned-long gtk_table_get_type ())

  ;; GtkWidget* gtk_table_new (guint rows, guint columns, gboolean homogeneous)
  (define-function void* gtk_table_new (unsigned-int unsigned-int int))

  ;; void gtk_table_resize (GtkTable* table, guint rows, guint columns)
  (define-function void gtk_table_resize (void* unsigned-int unsigned-int))

  ;; void gtk_table_set_col_spacing (GtkTable* table, guint column, guint spacing)
  (define-function void gtk_table_set_col_spacing (void* unsigned-int unsigned-int))

  ;; void gtk_table_set_col_spacings (GtkTable* table, guint spacing)
  (define-function void gtk_table_set_col_spacings (void* unsigned-int))

  ;; void gtk_table_set_homogeneous (GtkTable* table, gboolean homogeneous)
  (define-function void gtk_table_set_homogeneous (void* int))

  ;; void gtk_table_set_row_spacing (GtkTable* table, guint row, guint spacing)
  (define-function void gtk_table_set_row_spacing (void* unsigned-int unsigned-int))

  ;; void gtk_table_set_row_spacings (GtkTable* table, guint spacing)
  (define-function void gtk_table_set_row_spacings (void* unsigned-int))

  ) ;[end]
