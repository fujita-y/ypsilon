#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk clipboard)

  (export gtk_clipboard_clear
          gtk_clipboard_get
          gtk_clipboard_get_display
          gtk_clipboard_get_for_display
          gtk_clipboard_get_owner
          gtk_clipboard_get_type
          gtk_clipboard_request_contents
          gtk_clipboard_request_image
          gtk_clipboard_request_rich_text
          gtk_clipboard_request_targets
          gtk_clipboard_request_text
          gtk_clipboard_request_uris
          gtk_clipboard_set_can_store
          gtk_clipboard_set_image
          gtk_clipboard_set_text
          gtk_clipboard_set_with_data
          gtk_clipboard_set_with_owner
          gtk_clipboard_store
          gtk_clipboard_wait_for_contents
          gtk_clipboard_wait_for_image
          gtk_clipboard_wait_for_rich_text
          gtk_clipboard_wait_for_targets
          gtk_clipboard_wait_for_text
          gtk_clipboard_wait_for_uris
          gtk_clipboard_wait_is_image_available
          gtk_clipboard_wait_is_rich_text_available
          gtk_clipboard_wait_is_target_available
          gtk_clipboard_wait_is_text_available
          gtk_clipboard_wait_is_uris_available)

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

  ;; void gtk_clipboard_clear (GtkClipboard* clipboard)
  (define-function void gtk_clipboard_clear (void*))

  ;; GtkClipboard* gtk_clipboard_get (GdkAtom selection)
  (define-function void* gtk_clipboard_get (void*))

  ;; GdkDisplay* gtk_clipboard_get_display (GtkClipboard* clipboard)
  (define-function void* gtk_clipboard_get_display (void*))

  ;; GtkClipboard* gtk_clipboard_get_for_display (GdkDisplay* display, GdkAtom selection)
  (define-function void* gtk_clipboard_get_for_display (void* void*))

  ;; GObject* gtk_clipboard_get_owner (GtkClipboard* clipboard)
  (define-function void* gtk_clipboard_get_owner (void*))

  ;; GType gtk_clipboard_get_type (void)
  (define-function unsigned-long gtk_clipboard_get_type ())

  ;; void gtk_clipboard_request_contents (GtkClipboard* clipboard, GdkAtom target, GtkClipboardReceivedFunc callback, gpointer user_data)
  (define-function void gtk_clipboard_request_contents (void* void* (c-callback void (void* void* void*)) void*))

  ;; void gtk_clipboard_request_image (GtkClipboard* clipboard, GtkClipboardImageReceivedFunc callback, gpointer user_data)
  (define-function void gtk_clipboard_request_image (void* (c-callback void (void* void* void*)) void*))

  ;; void gtk_clipboard_request_rich_text (GtkClipboard* clipboard, GtkTextBuffer* buffer, GtkClipboardRichTextReceivedFunc callback, gpointer user_data)
  (define-function void gtk_clipboard_request_rich_text (void* void* (c-callback void (void* void* void* unsigned-long void*)) void*))

  ;; void gtk_clipboard_request_targets (GtkClipboard* clipboard, GtkClipboardTargetsReceivedFunc callback, gpointer user_data)
  (define-function void gtk_clipboard_request_targets (void* (c-callback void (void* void* int void*)) void*))

  ;; void gtk_clipboard_request_text (GtkClipboard* clipboard, GtkClipboardTextReceivedFunc callback, gpointer user_data)
  (define-function void gtk_clipboard_request_text (void* (c-callback void (void* void* void*)) void*))

  ;; void gtk_clipboard_request_uris (GtkClipboard* clipboard, GtkClipboardURIReceivedFunc callback, gpointer user_data)
  (define-function void gtk_clipboard_request_uris (void* (c-callback void (void* void* void*)) void*))

  ;; void gtk_clipboard_set_can_store (GtkClipboard* clipboard, const GtkTargetEntry* targets, gint n_targets)
  (define-function void gtk_clipboard_set_can_store (void* void* int))

  ;; void gtk_clipboard_set_image (GtkClipboard* clipboard, GdkPixbuf* pixbuf)
  (define-function void gtk_clipboard_set_image (void* void*))

  ;; void gtk_clipboard_set_text (GtkClipboard* clipboard, const gchar* text, gint len)
  (define-function void gtk_clipboard_set_text (void* char* int))

  ;; gboolean gtk_clipboard_set_with_data (GtkClipboard* clipboard, const GtkTargetEntry* targets, guint n_targets, GtkClipboardGetFunc get_func, GtkClipboardClearFunc clear_func, gpointer user_data)
  (define-function int gtk_clipboard_set_with_data (void* void* unsigned-int (c-callback void (void* void* unsigned-int void*)) (c-callback void (void* void*)) void*))

  ;; gboolean gtk_clipboard_set_with_owner (GtkClipboard* clipboard, const GtkTargetEntry* targets, guint n_targets, GtkClipboardGetFunc get_func, GtkClipboardClearFunc clear_func, GObject* owner)
  (define-function int gtk_clipboard_set_with_owner (void* void* unsigned-int (c-callback void (void* void* unsigned-int void*)) (c-callback void (void* void*)) void*))

  ;; void gtk_clipboard_store (GtkClipboard* clipboard)
  (define-function void gtk_clipboard_store (void*))

  ;; GtkSelectionData* gtk_clipboard_wait_for_contents (GtkClipboard* clipboard, GdkAtom target)
  (define-function void* gtk_clipboard_wait_for_contents (void* void*))

  ;; GdkPixbuf* gtk_clipboard_wait_for_image (GtkClipboard* clipboard)
  (define-function void* gtk_clipboard_wait_for_image (void*))

  ;; guint8* gtk_clipboard_wait_for_rich_text (GtkClipboard* clipboard, GtkTextBuffer* buffer, GdkAtom* format, gsize* length)
  (define-function void* gtk_clipboard_wait_for_rich_text (void* void* void* void*))

  ;; gboolean gtk_clipboard_wait_for_targets (GtkClipboard* clipboard, GdkAtom** targets, gint* n_targets)
  (define-function int gtk_clipboard_wait_for_targets (void* void* void*))

  ;; gchar* gtk_clipboard_wait_for_text (GtkClipboard* clipboard)
  (define-function char* gtk_clipboard_wait_for_text (void*))

  ;; gchar** gtk_clipboard_wait_for_uris (GtkClipboard* clipboard)
  (define-function void* gtk_clipboard_wait_for_uris (void*))

  ;; gboolean gtk_clipboard_wait_is_image_available (GtkClipboard* clipboard)
  (define-function int gtk_clipboard_wait_is_image_available (void*))

  ;; gboolean gtk_clipboard_wait_is_rich_text_available (GtkClipboard* clipboard, GtkTextBuffer* buffer)
  (define-function int gtk_clipboard_wait_is_rich_text_available (void* void*))

  ;; gboolean gtk_clipboard_wait_is_target_available (GtkClipboard* clipboard, GdkAtom target)
  (define-function int gtk_clipboard_wait_is_target_available (void* void*))

  ;; gboolean gtk_clipboard_wait_is_text_available (GtkClipboard* clipboard)
  (define-function int gtk_clipboard_wait_is_text_available (void*))

  ;; gboolean gtk_clipboard_wait_is_uris_available (GtkClipboard* clipboard)
  (define-function int gtk_clipboard_wait_is_uris_available (void*))

  ) ;[end]
