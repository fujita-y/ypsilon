#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk check)

  (export gtk_check_button_get_type
          gtk_check_button_new
          gtk_check_button_new_with_label
          gtk_check_button_new_with_mnemonic
          gtk_check_menu_item_get_active
          gtk_check_menu_item_get_draw_as_radio
          gtk_check_menu_item_get_inconsistent
          gtk_check_menu_item_get_type
          gtk_check_menu_item_new
          gtk_check_menu_item_new_with_label
          gtk_check_menu_item_new_with_mnemonic
          gtk_check_menu_item_set_active
          gtk_check_menu_item_set_draw_as_radio
          gtk_check_menu_item_set_inconsistent
          gtk_check_menu_item_toggled
          gtk_check_version)

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

  ;; GType gtk_check_button_get_type (void)
  (define-function unsigned-long gtk_check_button_get_type ())

  ;; GtkWidget* gtk_check_button_new (void)
  (define-function void* gtk_check_button_new ())

  ;; GtkWidget* gtk_check_button_new_with_label (const gchar* label)
  (define-function void* gtk_check_button_new_with_label (char*))

  ;; GtkWidget* gtk_check_button_new_with_mnemonic (const gchar* label)
  (define-function void* gtk_check_button_new_with_mnemonic (char*))

  ;; gboolean gtk_check_menu_item_get_active (GtkCheckMenuItem* check_menu_item)
  (define-function int gtk_check_menu_item_get_active (void*))

  ;; gboolean gtk_check_menu_item_get_draw_as_radio (GtkCheckMenuItem* check_menu_item)
  (define-function int gtk_check_menu_item_get_draw_as_radio (void*))

  ;; gboolean gtk_check_menu_item_get_inconsistent (GtkCheckMenuItem* check_menu_item)
  (define-function int gtk_check_menu_item_get_inconsistent (void*))

  ;; GType gtk_check_menu_item_get_type (void)
  (define-function unsigned-long gtk_check_menu_item_get_type ())

  ;; GtkWidget* gtk_check_menu_item_new (void)
  (define-function void* gtk_check_menu_item_new ())

  ;; GtkWidget* gtk_check_menu_item_new_with_label (const gchar* label)
  (define-function void* gtk_check_menu_item_new_with_label (char*))

  ;; GtkWidget* gtk_check_menu_item_new_with_mnemonic (const gchar* label)
  (define-function void* gtk_check_menu_item_new_with_mnemonic (char*))

  ;; void gtk_check_menu_item_set_active (GtkCheckMenuItem* check_menu_item, gboolean is_active)
  (define-function void gtk_check_menu_item_set_active (void* int))

  ;; void gtk_check_menu_item_set_draw_as_radio (GtkCheckMenuItem* check_menu_item, gboolean draw_as_radio)
  (define-function void gtk_check_menu_item_set_draw_as_radio (void* int))

  ;; void gtk_check_menu_item_set_inconsistent (GtkCheckMenuItem* check_menu_item, gboolean setting)
  (define-function void gtk_check_menu_item_set_inconsistent (void* int))

  ;; void gtk_check_menu_item_toggled (GtkCheckMenuItem* check_menu_item)
  (define-function void gtk_check_menu_item_toggled (void*))

  ;; const gchar* gtk_check_version (guint required_major, guint required_minor, guint required_micro)
  (define-function char* gtk_check_version (unsigned-int unsigned-int unsigned-int))

  ) ;[end]
