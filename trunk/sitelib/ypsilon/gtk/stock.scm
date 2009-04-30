#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk stock)

  (export gtk_stock_add
          gtk_stock_add_static
          gtk_stock_item_copy
          gtk_stock_item_free
          gtk_stock_list_ids
          gtk_stock_lookup
          gtk_stock_set_translate_func)

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

  ;; void gtk_stock_add (const GtkStockItem* items, guint n_items)
  (define-function void gtk_stock_add (void* unsigned-int))

  ;; void gtk_stock_add_static (const GtkStockItem* items, guint n_items)
  (define-function void gtk_stock_add_static (void* unsigned-int))

  ;; GtkStockItem* gtk_stock_item_copy (const GtkStockItem* item)
  (define-function void* gtk_stock_item_copy (void*))

  ;; void gtk_stock_item_free (GtkStockItem* item)
  (define-function void gtk_stock_item_free (void*))

  ;; GSList* gtk_stock_list_ids (void)
  (define-function void* gtk_stock_list_ids ())

  ;; gboolean gtk_stock_lookup (const gchar* stock_id, GtkStockItem* item)
  (define-function int gtk_stock_lookup (char* void*))

  ;; void gtk_stock_set_translate_func (const gchar* domain, GtkTranslateFunc func, gpointer data, GDestroyNotify notify)
  (define-function void gtk_stock_set_translate_func (char* (c-callback void* (void* void*)) void* (c-callback void (void*))))

  ) ;[end]
