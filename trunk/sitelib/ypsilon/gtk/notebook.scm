#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk notebook)

  (export gtk_notebook_append_page
          gtk_notebook_append_page_menu
          gtk_notebook_get_current_page
          gtk_notebook_get_group
          gtk_notebook_get_menu_label
          gtk_notebook_get_menu_label_text
          gtk_notebook_get_n_pages
          gtk_notebook_get_nth_page
          gtk_notebook_get_scrollable
          gtk_notebook_get_show_border
          gtk_notebook_get_show_tabs
          gtk_notebook_get_tab_detachable
          gtk_notebook_get_tab_label
          gtk_notebook_get_tab_label_text
          gtk_notebook_get_tab_pos
          gtk_notebook_get_tab_reorderable
          gtk_notebook_get_type
          gtk_notebook_insert_page
          gtk_notebook_insert_page_menu
          gtk_notebook_new
          gtk_notebook_next_page
          gtk_notebook_page_num
          gtk_notebook_popup_disable
          gtk_notebook_popup_enable
          gtk_notebook_prepend_page
          gtk_notebook_prepend_page_menu
          gtk_notebook_prev_page
          gtk_notebook_query_tab_label_packing
          gtk_notebook_remove_page
          gtk_notebook_reorder_child
          gtk_notebook_set_current_page
          gtk_notebook_set_group
          gtk_notebook_set_menu_label
          gtk_notebook_set_menu_label_text
          gtk_notebook_set_scrollable
          gtk_notebook_set_show_border
          gtk_notebook_set_show_tabs
          gtk_notebook_set_tab_detachable
          gtk_notebook_set_tab_label
          gtk_notebook_set_tab_label_packing
          gtk_notebook_set_tab_label_text
          gtk_notebook_set_tab_pos
          gtk_notebook_set_tab_reorderable
          gtk_notebook_set_window_creation_hook
          gtk_notebook_tab_get_type)

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

  ;; gint gtk_notebook_append_page (GtkNotebook* notebook, GtkWidget* child, GtkWidget* tab_label)
  (define-function int gtk_notebook_append_page (void* void* void*))

  ;; gint gtk_notebook_append_page_menu (GtkNotebook* notebook, GtkWidget* child, GtkWidget* tab_label, GtkWidget* menu_label)
  (define-function int gtk_notebook_append_page_menu (void* void* void* void*))

  ;; gint gtk_notebook_get_current_page (GtkNotebook* notebook)
  (define-function int gtk_notebook_get_current_page (void*))

  ;; gpointer gtk_notebook_get_group (GtkNotebook* notebook)
  (define-function void* gtk_notebook_get_group (void*))

  ;; GtkWidget* gtk_notebook_get_menu_label (GtkNotebook* notebook, GtkWidget* child)
  (define-function void* gtk_notebook_get_menu_label (void* void*))

  ;; const gchar* gtk_notebook_get_menu_label_text (GtkNotebook* notebook, GtkWidget* child)
  (define-function char* gtk_notebook_get_menu_label_text (void* void*))

  ;; gint gtk_notebook_get_n_pages (GtkNotebook* notebook)
  (define-function int gtk_notebook_get_n_pages (void*))

  ;; GtkWidget* gtk_notebook_get_nth_page (GtkNotebook* notebook, gint page_num)
  (define-function void* gtk_notebook_get_nth_page (void* int))

  ;; gboolean gtk_notebook_get_scrollable (GtkNotebook* notebook)
  (define-function int gtk_notebook_get_scrollable (void*))

  ;; gboolean gtk_notebook_get_show_border (GtkNotebook* notebook)
  (define-function int gtk_notebook_get_show_border (void*))

  ;; gboolean gtk_notebook_get_show_tabs (GtkNotebook* notebook)
  (define-function int gtk_notebook_get_show_tabs (void*))

  ;; gboolean gtk_notebook_get_tab_detachable (GtkNotebook* notebook, GtkWidget* child)
  (define-function int gtk_notebook_get_tab_detachable (void* void*))

  ;; GtkWidget* gtk_notebook_get_tab_label (GtkNotebook* notebook, GtkWidget* child)
  (define-function void* gtk_notebook_get_tab_label (void* void*))

  ;; const gchar* gtk_notebook_get_tab_label_text (GtkNotebook* notebook, GtkWidget* child)
  (define-function char* gtk_notebook_get_tab_label_text (void* void*))

  ;; GtkPositionType gtk_notebook_get_tab_pos (GtkNotebook* notebook)
  (define-function int gtk_notebook_get_tab_pos (void*))

  ;; gboolean gtk_notebook_get_tab_reorderable (GtkNotebook* notebook, GtkWidget* child)
  (define-function int gtk_notebook_get_tab_reorderable (void* void*))

  ;; GType gtk_notebook_get_type (void)
  (define-function unsigned-long gtk_notebook_get_type ())

  ;; gint gtk_notebook_insert_page (GtkNotebook* notebook, GtkWidget* child, GtkWidget* tab_label, gint position)
  (define-function int gtk_notebook_insert_page (void* void* void* int))

  ;; gint gtk_notebook_insert_page_menu (GtkNotebook* notebook, GtkWidget* child, GtkWidget* tab_label, GtkWidget* menu_label, gint position)
  (define-function int gtk_notebook_insert_page_menu (void* void* void* void* int))

  ;; GtkWidget* gtk_notebook_new (void)
  (define-function void* gtk_notebook_new ())

  ;; void gtk_notebook_next_page (GtkNotebook* notebook)
  (define-function void gtk_notebook_next_page (void*))

  ;; gint gtk_notebook_page_num (GtkNotebook* notebook, GtkWidget* child)
  (define-function int gtk_notebook_page_num (void* void*))

  ;; void gtk_notebook_popup_disable (GtkNotebook* notebook)
  (define-function void gtk_notebook_popup_disable (void*))

  ;; void gtk_notebook_popup_enable (GtkNotebook* notebook)
  (define-function void gtk_notebook_popup_enable (void*))

  ;; gint gtk_notebook_prepend_page (GtkNotebook* notebook, GtkWidget* child, GtkWidget* tab_label)
  (define-function int gtk_notebook_prepend_page (void* void* void*))

  ;; gint gtk_notebook_prepend_page_menu (GtkNotebook* notebook, GtkWidget* child, GtkWidget* tab_label, GtkWidget* menu_label)
  (define-function int gtk_notebook_prepend_page_menu (void* void* void* void*))

  ;; void gtk_notebook_prev_page (GtkNotebook* notebook)
  (define-function void gtk_notebook_prev_page (void*))

  ;; void gtk_notebook_query_tab_label_packing (GtkNotebook* notebook, GtkWidget* child, gboolean* expand, gboolean* fill, GtkPackType* pack_type)
  (define-function void gtk_notebook_query_tab_label_packing (void* void* void* void* void*))

  ;; void gtk_notebook_remove_page (GtkNotebook* notebook, gint page_num)
  (define-function void gtk_notebook_remove_page (void* int))

  ;; void gtk_notebook_reorder_child (GtkNotebook* notebook, GtkWidget* child, gint position)
  (define-function void gtk_notebook_reorder_child (void* void* int))

  ;; void gtk_notebook_set_current_page (GtkNotebook* notebook, gint page_num)
  (define-function void gtk_notebook_set_current_page (void* int))

  ;; void gtk_notebook_set_group (GtkNotebook* notebook, gpointer group)
  (define-function void gtk_notebook_set_group (void* void*))

  ;; void gtk_notebook_set_menu_label (GtkNotebook* notebook, GtkWidget* child, GtkWidget* menu_label)
  (define-function void gtk_notebook_set_menu_label (void* void* void*))

  ;; void gtk_notebook_set_menu_label_text (GtkNotebook* notebook, GtkWidget* child, const gchar* menu_text)
  (define-function void gtk_notebook_set_menu_label_text (void* void* char*))

  ;; void gtk_notebook_set_scrollable (GtkNotebook* notebook, gboolean scrollable)
  (define-function void gtk_notebook_set_scrollable (void* int))

  ;; void gtk_notebook_set_show_border (GtkNotebook* notebook, gboolean show_border)
  (define-function void gtk_notebook_set_show_border (void* int))

  ;; void gtk_notebook_set_show_tabs (GtkNotebook* notebook, gboolean show_tabs)
  (define-function void gtk_notebook_set_show_tabs (void* int))

  ;; void gtk_notebook_set_tab_detachable (GtkNotebook* notebook, GtkWidget* child, gboolean detachable)
  (define-function void gtk_notebook_set_tab_detachable (void* void* int))

  ;; void gtk_notebook_set_tab_label (GtkNotebook* notebook, GtkWidget* child, GtkWidget* tab_label)
  (define-function void gtk_notebook_set_tab_label (void* void* void*))

  ;; void gtk_notebook_set_tab_label_packing (GtkNotebook* notebook, GtkWidget* child, gboolean expand, gboolean fill, GtkPackType pack_type)
  (define-function void gtk_notebook_set_tab_label_packing (void* void* int int int))

  ;; void gtk_notebook_set_tab_label_text (GtkNotebook* notebook, GtkWidget* child, const gchar* tab_text)
  (define-function void gtk_notebook_set_tab_label_text (void* void* char*))

  ;; void gtk_notebook_set_tab_pos (GtkNotebook* notebook, GtkPositionType pos)
  (define-function void gtk_notebook_set_tab_pos (void* int))

  ;; void gtk_notebook_set_tab_reorderable (GtkNotebook* notebook, GtkWidget* child, gboolean reorderable)
  (define-function void gtk_notebook_set_tab_reorderable (void* void* int))

  ;; void gtk_notebook_set_window_creation_hook (GtkNotebookWindowCreationFunc func, gpointer data, GDestroyNotify destroy)
  (define-function void gtk_notebook_set_window_creation_hook ((c-callback void* (void* void* int int void*)) void* (c-callback void (void*))))

  ;; GType gtk_notebook_tab_get_type (void)
  (define-function unsigned-long gtk_notebook_tab_get_type ())

  ) ;[end]
