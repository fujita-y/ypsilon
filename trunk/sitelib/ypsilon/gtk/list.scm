#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk list)

  (export gtk_list_store_append
          gtk_list_store_clear
          gtk_list_store_get_type
          gtk_list_store_insert
          gtk_list_store_insert_after
          gtk_list_store_insert_before
          gtk_list_store_insert_with_values
          gtk_list_store_insert_with_valuesv
          gtk_list_store_iter_is_valid
          gtk_list_store_move_after
          gtk_list_store_move_before
          gtk_list_store_new
          gtk_list_store_newv
          gtk_list_store_prepend
          gtk_list_store_remove
          gtk_list_store_reorder
          gtk_list_store_set
          gtk_list_store_set_column_types
          gtk_list_store_set_valist
          gtk_list_store_set_value
          gtk_list_store_set_valuesv
          gtk_list_store_swap)

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

  ;; void gtk_list_store_append (GtkListStore* list_store, GtkTreeIter* iter)
  (define-function void gtk_list_store_append (void* void*))

  ;; void gtk_list_store_clear (GtkListStore* list_store)
  (define-function void gtk_list_store_clear (void*))

  ;; GType gtk_list_store_get_type (void)
  (define-function unsigned-long gtk_list_store_get_type ())

  ;; void gtk_list_store_insert (GtkListStore* list_store, GtkTreeIter* iter, gint position)
  (define-function void gtk_list_store_insert (void* void* int))

  ;; void gtk_list_store_insert_after (GtkListStore* list_store, GtkTreeIter* iter, GtkTreeIter* sibling)
  (define-function void gtk_list_store_insert_after (void* void* void*))

  ;; void gtk_list_store_insert_before (GtkListStore* list_store, GtkTreeIter* iter, GtkTreeIter* sibling)
  (define-function void gtk_list_store_insert_before (void* void* void*))

  ;; void gtk_list_store_insert_with_values (GtkListStore* list_store, GtkTreeIter* iter, gint position, ...)
  (define-variadic-function void gtk_list_store_insert_with_values (void* void* int ...))

  ;; void gtk_list_store_insert_with_valuesv (GtkListStore* list_store, GtkTreeIter* iter, gint position, gint* columns, GValue* values, gint n_values)
  (define-function void gtk_list_store_insert_with_valuesv (void* void* int void* void* int))

  ;; gboolean gtk_list_store_iter_is_valid (GtkListStore* list_store, GtkTreeIter* iter)
  (define-function int gtk_list_store_iter_is_valid (void* void*))

  ;; void gtk_list_store_move_after (GtkListStore* store, GtkTreeIter* iter, GtkTreeIter* position)
  (define-function void gtk_list_store_move_after (void* void* void*))

  ;; void gtk_list_store_move_before (GtkListStore* store, GtkTreeIter* iter, GtkTreeIter* position)
  (define-function void gtk_list_store_move_before (void* void* void*))

  ;; GtkListStore* gtk_list_store_new (gint n_columns, ...)
  (define-variadic-function void* gtk_list_store_new (int ...))

  ;; GtkListStore* gtk_list_store_newv (gint n_columns, GType* types)
  (define-function void* gtk_list_store_newv (int void*))

  ;; void gtk_list_store_prepend (GtkListStore* list_store, GtkTreeIter* iter)
  (define-function void gtk_list_store_prepend (void* void*))

  ;; gboolean gtk_list_store_remove (GtkListStore* list_store, GtkTreeIter* iter)
  (define-function int gtk_list_store_remove (void* void*))

  ;; void gtk_list_store_reorder (GtkListStore* store, gint* new_order)
  (define-function void gtk_list_store_reorder (void* void*))

  ;; void gtk_list_store_set (GtkListStore* list_store, GtkTreeIter* iter, ...)
  (define-variadic-function void gtk_list_store_set (void* void* ...))

  ;; void gtk_list_store_set_column_types (GtkListStore* list_store, gint n_columns, GType* types)
  (define-function void gtk_list_store_set_column_types (void* int void*))

  ;; void gtk_list_store_set_valist (GtkListStore* list_store, GtkTreeIter* iter, va_list var_args)
  (define-variadic-function void gtk_list_store_set_valist (void* void* va_list))

  ;; void gtk_list_store_set_value (GtkListStore* list_store, GtkTreeIter* iter, gint column, GValue* value)
  (define-function void gtk_list_store_set_value (void* void* int void*))

  ;; void gtk_list_store_set_valuesv (GtkListStore* list_store, GtkTreeIter* iter, gint* columns, GValue* values, gint n_values)
  (define-function void gtk_list_store_set_valuesv (void* void* void* void* int))

  ;; void gtk_list_store_swap (GtkListStore* store, GtkTreeIter* a, GtkTreeIter* b)
  (define-function void gtk_list_store_swap (void* void* void*))

  ) ;[end]
