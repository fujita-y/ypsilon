#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk assistant)

  (export gtk_assistant_add_action_widget
          gtk_assistant_append_page
          gtk_assistant_get_current_page
          gtk_assistant_get_n_pages
          gtk_assistant_get_nth_page
          gtk_assistant_get_page_complete
          gtk_assistant_get_page_header_image
          gtk_assistant_get_page_side_image
          gtk_assistant_get_page_title
          gtk_assistant_get_page_type
          gtk_assistant_get_type
          gtk_assistant_insert_page
          gtk_assistant_new
          gtk_assistant_page_type_get_type
          gtk_assistant_prepend_page
          gtk_assistant_remove_action_widget
          gtk_assistant_set_current_page
          gtk_assistant_set_forward_page_func
          gtk_assistant_set_page_complete
          gtk_assistant_set_page_header_image
          gtk_assistant_set_page_side_image
          gtk_assistant_set_page_title
          gtk_assistant_set_page_type
          gtk_assistant_update_buttons_state)

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

  ;; void gtk_assistant_add_action_widget (GtkAssistant* assistant, GtkWidget* child)
  (define-function void gtk_assistant_add_action_widget (void* void*))

  ;; gint gtk_assistant_append_page (GtkAssistant* assistant, GtkWidget* page)
  (define-function int gtk_assistant_append_page (void* void*))

  ;; gint gtk_assistant_get_current_page (GtkAssistant* assistant)
  (define-function int gtk_assistant_get_current_page (void*))

  ;; gint gtk_assistant_get_n_pages (GtkAssistant* assistant)
  (define-function int gtk_assistant_get_n_pages (void*))

  ;; GtkWidget* gtk_assistant_get_nth_page (GtkAssistant* assistant, gint page_num)
  (define-function void* gtk_assistant_get_nth_page (void* int))

  ;; gboolean gtk_assistant_get_page_complete (GtkAssistant* assistant, GtkWidget* page)
  (define-function int gtk_assistant_get_page_complete (void* void*))

  ;; GdkPixbuf* gtk_assistant_get_page_header_image (GtkAssistant* assistant, GtkWidget* page)
  (define-function void* gtk_assistant_get_page_header_image (void* void*))

  ;; GdkPixbuf* gtk_assistant_get_page_side_image (GtkAssistant* assistant, GtkWidget* page)
  (define-function void* gtk_assistant_get_page_side_image (void* void*))

  ;; const gchar* gtk_assistant_get_page_title (GtkAssistant* assistant, GtkWidget* page)
  (define-function char* gtk_assistant_get_page_title (void* void*))

  ;; GtkAssistantPageType gtk_assistant_get_page_type (GtkAssistant* assistant, GtkWidget* page)
  (define-function int gtk_assistant_get_page_type (void* void*))

  ;; GType gtk_assistant_get_type (void)
  (define-function unsigned-long gtk_assistant_get_type ())

  ;; gint gtk_assistant_insert_page (GtkAssistant* assistant, GtkWidget* page, gint position)
  (define-function int gtk_assistant_insert_page (void* void* int))

  ;; GtkWidget* gtk_assistant_new (void)
  (define-function void* gtk_assistant_new ())

  ;; GType gtk_assistant_page_type_get_type (void)
  (define-function unsigned-long gtk_assistant_page_type_get_type ())

  ;; gint gtk_assistant_prepend_page (GtkAssistant* assistant, GtkWidget* page)
  (define-function int gtk_assistant_prepend_page (void* void*))

  ;; void gtk_assistant_remove_action_widget (GtkAssistant* assistant, GtkWidget* child)
  (define-function void gtk_assistant_remove_action_widget (void* void*))

  ;; void gtk_assistant_set_current_page (GtkAssistant* assistant, gint page_num)
  (define-function void gtk_assistant_set_current_page (void* int))

  ;; void gtk_assistant_set_forward_page_func (GtkAssistant* assistant, GtkAssistantPageFunc page_func, gpointer data, GDestroyNotify destroy)
  (define-function void gtk_assistant_set_forward_page_func (void* (c-callback int (int void*)) void* (c-callback void (void*))))

  ;; void gtk_assistant_set_page_complete (GtkAssistant* assistant, GtkWidget* page, gboolean complete)
  (define-function void gtk_assistant_set_page_complete (void* void* int))

  ;; void gtk_assistant_set_page_header_image (GtkAssistant* assistant, GtkWidget* page, GdkPixbuf* pixbuf)
  (define-function void gtk_assistant_set_page_header_image (void* void* void*))

  ;; void gtk_assistant_set_page_side_image (GtkAssistant* assistant, GtkWidget* page, GdkPixbuf* pixbuf)
  (define-function void gtk_assistant_set_page_side_image (void* void* void*))

  ;; void gtk_assistant_set_page_title (GtkAssistant* assistant, GtkWidget* page, const gchar* title)
  (define-function void gtk_assistant_set_page_title (void* void* char*))

  ;; void gtk_assistant_set_page_type (GtkAssistant* assistant, GtkWidget* page, GtkAssistantPageType type)
  (define-function void gtk_assistant_set_page_type (void* void* int))

  ;; void gtk_assistant_update_buttons_state (GtkAssistant* assistant)
  (define-function void gtk_assistant_update_buttons_state (void*))

  ) ;[end]
