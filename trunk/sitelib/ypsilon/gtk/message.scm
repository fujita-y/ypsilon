#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk message)

  (export gtk_message_dialog_format_secondary_markup
          gtk_message_dialog_format_secondary_text
          gtk_message_dialog_get_image
          gtk_message_dialog_get_type
          gtk_message_dialog_new
          gtk_message_dialog_new_with_markup
          gtk_message_dialog_set_image
          gtk_message_dialog_set_markup
          gtk_message_type_get_type)

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

  ;; void gtk_message_dialog_format_secondary_markup (GtkMessageDialog* message_dialog, const gchar* message_format, ...)
  (define-function void gtk_message_dialog_format_secondary_markup (void* char* ...))

  ;; void gtk_message_dialog_format_secondary_text (GtkMessageDialog* message_dialog, const gchar* message_format, ...)
  (define-function void gtk_message_dialog_format_secondary_text (void* char* ...))

  ;; GtkWidget* gtk_message_dialog_get_image (GtkMessageDialog* dialog)
  (define-function void* gtk_message_dialog_get_image (void*))

  ;; GType gtk_message_dialog_get_type (void)
  (define-function unsigned-long gtk_message_dialog_get_type ())

  ;; GtkWidget* gtk_message_dialog_new (GtkWindow* parent, GtkDialogFlags flags, GtkMessageType type, GtkButtonsType buttons, const gchar* message_format, ...)
  (define-function void* gtk_message_dialog_new (void* int int int char* ...))

  ;; GtkWidget* gtk_message_dialog_new_with_markup (GtkWindow* parent, GtkDialogFlags flags, GtkMessageType type, GtkButtonsType buttons, const gchar* message_format, ...)
  (define-function void* gtk_message_dialog_new_with_markup (void* int int int char* ...))

  ;; void gtk_message_dialog_set_image (GtkMessageDialog* dialog, GtkWidget* image)
  (define-function void gtk_message_dialog_set_image (void* void*))

  ;; void gtk_message_dialog_set_markup (GtkMessageDialog* message_dialog, const gchar* str)
  (define-function void gtk_message_dialog_set_markup (void* char*))

  ;; GType gtk_message_type_get_type (void)
  (define-function unsigned-long gtk_message_type_get_type ())

  ) ;[end]
