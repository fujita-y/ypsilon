#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk selection)

  (export gtk_selection_add_target
          gtk_selection_add_targets
          gtk_selection_clear_targets
          gtk_selection_convert
          gtk_selection_data_copy
          gtk_selection_data_free
          gtk_selection_data_get_data
          gtk_selection_data_get_data_type
          gtk_selection_data_get_display
          gtk_selection_data_get_format
          gtk_selection_data_get_length
          gtk_selection_data_get_pixbuf
          gtk_selection_data_get_selection
          gtk_selection_data_get_target
          gtk_selection_data_get_targets
          gtk_selection_data_get_text
          gtk_selection_data_get_type
          gtk_selection_data_get_uris
          gtk_selection_data_set
          gtk_selection_data_set_pixbuf
          gtk_selection_data_set_text
          gtk_selection_data_set_uris
          gtk_selection_data_targets_include_image
          gtk_selection_data_targets_include_rich_text
          gtk_selection_data_targets_include_text
          gtk_selection_data_targets_include_uri
          gtk_selection_mode_get_type
          gtk_selection_owner_set
          gtk_selection_owner_set_for_display
          gtk_selection_remove_all)

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

  ;; void gtk_selection_add_target (GtkWidget* widget, GdkAtom selection, GdkAtom target, guint info)
  (define-function void gtk_selection_add_target (void* void* void* unsigned-int))

  ;; void gtk_selection_add_targets (GtkWidget* widget, GdkAtom selection, const GtkTargetEntry* targets, guint ntargets)
  (define-function void gtk_selection_add_targets (void* void* void* unsigned-int))

  ;; void gtk_selection_clear_targets (GtkWidget* widget, GdkAtom selection)
  (define-function void gtk_selection_clear_targets (void* void*))

  ;; gboolean gtk_selection_convert (GtkWidget* widget, GdkAtom selection, GdkAtom target, guint32 time_)
  (define-function int gtk_selection_convert (void* void* void* uint32_t))

  ;; GtkSelectionData* gtk_selection_data_copy (GtkSelectionData* data)
  (define-function void* gtk_selection_data_copy (void*))

  ;; void gtk_selection_data_free (GtkSelectionData* data)
  (define-function void gtk_selection_data_free (void*))

  ;; const guchar* gtk_selection_data_get_data (GtkSelectionData* selection_data)
  (define-function void* gtk_selection_data_get_data (void*))

  ;; GdkAtom gtk_selection_data_get_data_type (GtkSelectionData* selection_data)
  (define-function void* gtk_selection_data_get_data_type (void*))

  ;; GdkDisplay* gtk_selection_data_get_display (GtkSelectionData* selection_data)
  (define-function void* gtk_selection_data_get_display (void*))

  ;; gint gtk_selection_data_get_format (GtkSelectionData* selection_data)
  (define-function int gtk_selection_data_get_format (void*))

  ;; gint gtk_selection_data_get_length (GtkSelectionData* selection_data)
  (define-function int gtk_selection_data_get_length (void*))

  ;; GdkPixbuf* gtk_selection_data_get_pixbuf (GtkSelectionData* selection_data)
  (define-function void* gtk_selection_data_get_pixbuf (void*))

  ;; GdkAtom gtk_selection_data_get_selection (GtkSelectionData* selection_data)
  (define-function void* gtk_selection_data_get_selection (void*))

  ;; GdkAtom gtk_selection_data_get_target (GtkSelectionData* selection_data)
  (define-function void* gtk_selection_data_get_target (void*))

  ;; gboolean gtk_selection_data_get_targets (GtkSelectionData* selection_data, GdkAtom** targets, gint* n_atoms)
  (define-function int gtk_selection_data_get_targets (void* void* void*))

  ;; guchar* gtk_selection_data_get_text (GtkSelectionData* selection_data)
  (define-function void* gtk_selection_data_get_text (void*))

  ;; GType gtk_selection_data_get_type (void)
  (define-function unsigned-long gtk_selection_data_get_type ())

  ;; gchar** gtk_selection_data_get_uris (GtkSelectionData* selection_data)
  (define-function void* gtk_selection_data_get_uris (void*))

  ;; void gtk_selection_data_set (GtkSelectionData* selection_data, GdkAtom type, gint format, const guchar* data, gint length)
  (define-function void gtk_selection_data_set (void* void* int void* int))

  ;; gboolean gtk_selection_data_set_pixbuf (GtkSelectionData* selection_data, GdkPixbuf* pixbuf)
  (define-function int gtk_selection_data_set_pixbuf (void* void*))

  ;; gboolean gtk_selection_data_set_text (GtkSelectionData* selection_data, const gchar* str, gint len)
  (define-function int gtk_selection_data_set_text (void* char* int))

  ;; gboolean gtk_selection_data_set_uris (GtkSelectionData* selection_data, gchar** uris)
  (define-function int gtk_selection_data_set_uris (void* void*))

  ;; gboolean gtk_selection_data_targets_include_image (GtkSelectionData* selection_data, gboolean writable)
  (define-function int gtk_selection_data_targets_include_image (void* int))

  ;; gboolean gtk_selection_data_targets_include_rich_text (GtkSelectionData* selection_data, GtkTextBuffer* buffer)
  (define-function int gtk_selection_data_targets_include_rich_text (void* void*))

  ;; gboolean gtk_selection_data_targets_include_text (GtkSelectionData* selection_data)
  (define-function int gtk_selection_data_targets_include_text (void*))

  ;; gboolean gtk_selection_data_targets_include_uri (GtkSelectionData* selection_data)
  (define-function int gtk_selection_data_targets_include_uri (void*))

  ;; GType gtk_selection_mode_get_type (void)
  (define-function unsigned-long gtk_selection_mode_get_type ())

  ;; gboolean gtk_selection_owner_set (GtkWidget* widget, GdkAtom selection, guint32 time_)
  (define-function int gtk_selection_owner_set (void* void* uint32_t))

  ;; gboolean gtk_selection_owner_set_for_display (GdkDisplay* display, GtkWidget* widget, GdkAtom selection, guint32 time_)
  (define-function int gtk_selection_owner_set_for_display (void* void* void* uint32_t))

  ;; void gtk_selection_remove_all (GtkWidget* widget)
  (define-function void gtk_selection_remove_all (void*))

  ) ;[end]
