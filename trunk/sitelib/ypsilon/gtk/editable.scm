#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk editable)

  (export gtk_editable_copy_clipboard
          gtk_editable_cut_clipboard
          gtk_editable_delete_selection
          gtk_editable_delete_text
          gtk_editable_get_chars
          gtk_editable_get_editable
          gtk_editable_get_position
          gtk_editable_get_selection_bounds
          gtk_editable_get_type
          gtk_editable_insert_text
          gtk_editable_paste_clipboard
          gtk_editable_select_region
          gtk_editable_set_editable
          gtk_editable_set_position)

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

  ;; void gtk_editable_copy_clipboard (GtkEditable* editable)
  (define-function void gtk_editable_copy_clipboard (void*))

  ;; void gtk_editable_cut_clipboard (GtkEditable* editable)
  (define-function void gtk_editable_cut_clipboard (void*))

  ;; void gtk_editable_delete_selection (GtkEditable* editable)
  (define-function void gtk_editable_delete_selection (void*))

  ;; void gtk_editable_delete_text (GtkEditable* editable, gint start_pos, gint end_pos)
  (define-function void gtk_editable_delete_text (void* int int))

  ;; gchar* gtk_editable_get_chars (GtkEditable* editable, gint start_pos, gint end_pos)
  (define-function char* gtk_editable_get_chars (void* int int))

  ;; gboolean gtk_editable_get_editable (GtkEditable* editable)
  (define-function int gtk_editable_get_editable (void*))

  ;; gint gtk_editable_get_position (GtkEditable* editable)
  (define-function int gtk_editable_get_position (void*))

  ;; gboolean gtk_editable_get_selection_bounds (GtkEditable* editable, gint* start_pos, gint* end_pos)
  (define-function int gtk_editable_get_selection_bounds (void* void* void*))

  ;; GType gtk_editable_get_type (void)
  (define-function unsigned-long gtk_editable_get_type ())

  ;; void gtk_editable_insert_text (GtkEditable* editable, const gchar* new_text, gint new_text_length, gint* position)
  (define-function void gtk_editable_insert_text (void* char* int void*))

  ;; void gtk_editable_paste_clipboard (GtkEditable* editable)
  (define-function void gtk_editable_paste_clipboard (void*))

  ;; void gtk_editable_select_region (GtkEditable* editable, gint start_pos, gint end_pos)
  (define-function void gtk_editable_select_region (void* int int))

  ;; void gtk_editable_set_editable (GtkEditable* editable, gboolean is_editable)
  (define-function void gtk_editable_set_editable (void* int))

  ;; void gtk_editable_set_position (GtkEditable* editable, gint position)
  (define-function void gtk_editable_set_position (void* int))

  ) ;[end]
