#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk im)

  (export gtk_im_context_delete_surrounding
          gtk_im_context_filter_keypress
          gtk_im_context_focus_in
          gtk_im_context_focus_out
          gtk_im_context_get_preedit_string
          gtk_im_context_get_surrounding
          gtk_im_context_get_type
          gtk_im_context_reset
          gtk_im_context_set_client_window
          gtk_im_context_set_cursor_location
          gtk_im_context_set_surrounding
          gtk_im_context_set_use_preedit
          gtk_im_context_simple_add_table
          gtk_im_context_simple_get_type
          gtk_im_context_simple_new
          gtk_im_multicontext_append_menuitems
          gtk_im_multicontext_get_type
          gtk_im_multicontext_new
          gtk_im_preedit_style_get_type
          gtk_im_status_style_get_type)

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

  ;; gboolean gtk_im_context_delete_surrounding (GtkIMContext* context, gint offset, gint n_chars)
  (define-function int gtk_im_context_delete_surrounding (void* int int))

  ;; gboolean gtk_im_context_filter_keypress (GtkIMContext* context, GdkEventKey* event)
  (define-function int gtk_im_context_filter_keypress (void* void*))

  ;; void gtk_im_context_focus_in (GtkIMContext* context)
  (define-function void gtk_im_context_focus_in (void*))

  ;; void gtk_im_context_focus_out (GtkIMContext* context)
  (define-function void gtk_im_context_focus_out (void*))

  ;; void gtk_im_context_get_preedit_string (GtkIMContext* context, gchar** str, PangoAttrList** attrs, gint* cursor_pos)
  (define-function void gtk_im_context_get_preedit_string (void* void* void* void*))

  ;; gboolean gtk_im_context_get_surrounding (GtkIMContext* context, gchar** text, gint* cursor_index)
  (define-function int gtk_im_context_get_surrounding (void* void* void*))

  ;; GType gtk_im_context_get_type (void)
  (define-function unsigned-long gtk_im_context_get_type ())

  ;; void gtk_im_context_reset (GtkIMContext* context)
  (define-function void gtk_im_context_reset (void*))

  ;; void gtk_im_context_set_client_window (GtkIMContext* context, GdkWindow* window)
  (define-function void gtk_im_context_set_client_window (void* void*))

  ;; void gtk_im_context_set_cursor_location (GtkIMContext* context, const GdkRectangle* area)
  (define-function void gtk_im_context_set_cursor_location (void* void*))

  ;; void gtk_im_context_set_surrounding (GtkIMContext* context, const gchar* text, gint len, gint cursor_index)
  (define-function void gtk_im_context_set_surrounding (void* char* int int))

  ;; void gtk_im_context_set_use_preedit (GtkIMContext* context, gboolean use_preedit)
  (define-function void gtk_im_context_set_use_preedit (void* int))

  ;; void gtk_im_context_simple_add_table (GtkIMContextSimple* context_simple, guint16* data, gint max_seq_len, gint n_seqs)
  (define-function void gtk_im_context_simple_add_table (void* void* int int))

  ;; GType gtk_im_context_simple_get_type (void)
  (define-function unsigned-long gtk_im_context_simple_get_type ())

  ;; GtkIMContext* gtk_im_context_simple_new (void)
  (define-function void* gtk_im_context_simple_new ())

  ;; void gtk_im_multicontext_append_menuitems (GtkIMMulticontext* context, GtkMenuShell* menushell)
  (define-function void gtk_im_multicontext_append_menuitems (void* void*))

  ;; GType gtk_im_multicontext_get_type (void)
  (define-function unsigned-long gtk_im_multicontext_get_type ())

  ;; GtkIMContext* gtk_im_multicontext_new (void)
  (define-function void* gtk_im_multicontext_new ())

  ;; GType gtk_im_preedit_style_get_type (void)
  (define-function unsigned-long gtk_im_preedit_style_get_type ())

  ;; GType gtk_im_status_style_get_type (void)
  (define-function unsigned-long gtk_im_status_style_get_type ())

  ) ;[end]
