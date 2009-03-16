#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk accel)

  (export gtk_accel_flags_get_type
          gtk_accel_group_activate
          gtk_accel_group_connect
          gtk_accel_group_connect_by_path
          gtk_accel_group_disconnect
          gtk_accel_group_disconnect_key
          gtk_accel_group_find
          gtk_accel_group_from_accel_closure
          gtk_accel_group_get_is_locked
          gtk_accel_group_get_modifier_mask
          gtk_accel_group_get_type
          gtk_accel_group_lock
          gtk_accel_group_new
          gtk_accel_group_query
          gtk_accel_group_unlock
          gtk_accel_groups_activate
          gtk_accel_groups_from_object
          gtk_accel_label_get_accel_widget
          gtk_accel_label_get_accel_width
          gtk_accel_label_get_type
          gtk_accel_label_new
          gtk_accel_label_refetch
          gtk_accel_label_set_accel_closure
          gtk_accel_label_set_accel_widget
          gtk_accel_map_add_entry
          gtk_accel_map_add_filter
          gtk_accel_map_change_entry
          gtk_accel_map_foreach
          gtk_accel_map_foreach_unfiltered
          gtk_accel_map_get
          gtk_accel_map_get_type
          gtk_accel_map_load
          gtk_accel_map_load_fd
          gtk_accel_map_load_scanner
          gtk_accel_map_lock_path
          gtk_accel_map_lookup_entry
          gtk_accel_map_save
          gtk_accel_map_save_fd
          gtk_accel_map_unlock_path)

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

  ;; GType gtk_accel_flags_get_type (void)
  (define-function unsigned-long gtk_accel_flags_get_type ())

  ;; gboolean gtk_accel_group_activate (GtkAccelGroup* accel_group, GQuark accel_quark, GObject* acceleratable, guint accel_key, GdkModifierType accel_mods)
  (define-function int gtk_accel_group_activate (void* uint32_t void* unsigned-int int))

  ;; void gtk_accel_group_connect (GtkAccelGroup* accel_group, guint accel_key, GdkModifierType accel_mods, GtkAccelFlags accel_flags, GClosure* closure)
  (define-function void gtk_accel_group_connect (void* unsigned-int int int void*))

  ;; void gtk_accel_group_connect_by_path (GtkAccelGroup* accel_group, const gchar* accel_path, GClosure* closure)
  (define-function void gtk_accel_group_connect_by_path (void* char* void*))

  ;; gboolean gtk_accel_group_disconnect (GtkAccelGroup* accel_group, GClosure* closure)
  (define-function int gtk_accel_group_disconnect (void* void*))

  ;; gboolean gtk_accel_group_disconnect_key (GtkAccelGroup* accel_group, guint accel_key, GdkModifierType accel_mods)
  (define-function int gtk_accel_group_disconnect_key (void* unsigned-int int))

  ;; GtkAccelKey* gtk_accel_group_find (GtkAccelGroup* accel_group, GtkAccelGroupFindFunc find_func, gpointer data)
  (define-function void* gtk_accel_group_find (void* (c-callback int (void* void* void*)) void*))

  ;; GtkAccelGroup* gtk_accel_group_from_accel_closure (GClosure* closure)
  (define-function void* gtk_accel_group_from_accel_closure (void*))

  ;; gboolean gtk_accel_group_get_is_locked (GtkAccelGroup* accel_group)
  (define-function int gtk_accel_group_get_is_locked (void*))

  ;; GdkModifierType gtk_accel_group_get_modifier_mask (GtkAccelGroup* accel_group)
  (define-function int gtk_accel_group_get_modifier_mask (void*))

  ;; GType gtk_accel_group_get_type (void)
  (define-function unsigned-long gtk_accel_group_get_type ())

  ;; void gtk_accel_group_lock (GtkAccelGroup* accel_group)
  (define-function void gtk_accel_group_lock (void*))

  ;; GtkAccelGroup* gtk_accel_group_new (void)
  (define-function void* gtk_accel_group_new ())

  ;; GtkAccelGroupEntry* gtk_accel_group_query (GtkAccelGroup* accel_group, guint accel_key, GdkModifierType accel_mods, guint* n_entries)
  (define-function void* gtk_accel_group_query (void* unsigned-int int void*))

  ;; void gtk_accel_group_unlock (GtkAccelGroup* accel_group)
  (define-function void gtk_accel_group_unlock (void*))

  ;; gboolean gtk_accel_groups_activate (GObject* object, guint accel_key, GdkModifierType accel_mods)
  (define-function int gtk_accel_groups_activate (void* unsigned-int int))

  ;; GSList* gtk_accel_groups_from_object (GObject* object)
  (define-function void* gtk_accel_groups_from_object (void*))

  ;; GtkWidget* gtk_accel_label_get_accel_widget (GtkAccelLabel* accel_label)
  (define-function void* gtk_accel_label_get_accel_widget (void*))

  ;; guint gtk_accel_label_get_accel_width (GtkAccelLabel* accel_label)
  (define-function unsigned-int gtk_accel_label_get_accel_width (void*))

  ;; GType gtk_accel_label_get_type (void)
  (define-function unsigned-long gtk_accel_label_get_type ())

  ;; GtkWidget* gtk_accel_label_new (const gchar* string)
  (define-function void* gtk_accel_label_new (char*))

  ;; gboolean gtk_accel_label_refetch (GtkAccelLabel* accel_label)
  (define-function int gtk_accel_label_refetch (void*))

  ;; void gtk_accel_label_set_accel_closure (GtkAccelLabel* accel_label, GClosure* accel_closure)
  (define-function void gtk_accel_label_set_accel_closure (void* void*))

  ;; void gtk_accel_label_set_accel_widget (GtkAccelLabel* accel_label, GtkWidget* accel_widget)
  (define-function void gtk_accel_label_set_accel_widget (void* void*))

  ;; void gtk_accel_map_add_entry (const gchar* accel_path, guint accel_key, GdkModifierType accel_mods)
  (define-function void gtk_accel_map_add_entry (char* unsigned-int int))

  ;; void gtk_accel_map_add_filter (const gchar* filter_pattern)
  (define-function void gtk_accel_map_add_filter (char*))

  ;; gboolean gtk_accel_map_change_entry (const gchar* accel_path, guint accel_key, GdkModifierType accel_mods, gboolean replace)
  (define-function int gtk_accel_map_change_entry (char* unsigned-int int int))

  ;; void gtk_accel_map_foreach (gpointer data, GtkAccelMapForeach foreach_func)
  (define-function void gtk_accel_map_foreach (void* (c-callback void (void* void* unsigned-int int int))))

  ;; void gtk_accel_map_foreach_unfiltered (gpointer data, GtkAccelMapForeach foreach_func)
  (define-function void gtk_accel_map_foreach_unfiltered (void* (c-callback void (void* void* unsigned-int int int))))

  ;; GtkAccelMap* gtk_accel_map_get (void)
  (define-function void* gtk_accel_map_get ())

  ;; GType gtk_accel_map_get_type (void)
  (define-function unsigned-long gtk_accel_map_get_type ())

  ;; void gtk_accel_map_load (const gchar* file_name)
  (define-function void gtk_accel_map_load (char*))

  ;; void gtk_accel_map_load_fd (gint fd)
  (define-function void gtk_accel_map_load_fd (int))

  ;; void gtk_accel_map_load_scanner (GScanner* scanner)
  (define-function void gtk_accel_map_load_scanner (void*))

  ;; void gtk_accel_map_lock_path (const gchar* accel_path)
  (define-function void gtk_accel_map_lock_path (char*))

  ;; gboolean gtk_accel_map_lookup_entry (const gchar* accel_path, GtkAccelKey* key)
  (define-function int gtk_accel_map_lookup_entry (char* void*))

  ;; void gtk_accel_map_save (const gchar* file_name)
  (define-function void gtk_accel_map_save (char*))

  ;; void gtk_accel_map_save_fd (gint fd)
  (define-function void gtk_accel_map_save_fd (int))

  ;; void gtk_accel_map_unlock_path (const gchar* accel_path)
  (define-function void gtk_accel_map_unlock_path (char*))

  ) ;[end]
