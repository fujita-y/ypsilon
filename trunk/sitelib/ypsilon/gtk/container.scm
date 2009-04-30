#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk container)

  (export gtk_container_add
          gtk_container_add_with_properties
          gtk_container_check_resize
          gtk_container_child_get
          gtk_container_child_get_property
          gtk_container_child_get_valist
          gtk_container_child_set
          gtk_container_child_set_property
          gtk_container_child_set_valist
          gtk_container_child_type
          gtk_container_class_find_child_property
          gtk_container_class_install_child_property
          gtk_container_class_list_child_properties
          gtk_container_forall
          gtk_container_foreach
          gtk_container_get_border_width
          gtk_container_get_children
          gtk_container_get_focus_chain
          gtk_container_get_focus_child
          gtk_container_get_focus_hadjustment
          gtk_container_get_focus_vadjustment
          gtk_container_get_resize_mode
          gtk_container_get_type
          gtk_container_propagate_expose
          gtk_container_remove
          gtk_container_resize_children
          gtk_container_set_border_width
          gtk_container_set_focus_chain
          gtk_container_set_focus_child
          gtk_container_set_focus_hadjustment
          gtk_container_set_focus_vadjustment
          gtk_container_set_reallocate_redraws
          gtk_container_set_resize_mode
          gtk_container_unset_focus_chain)

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

  ;; void gtk_container_add (GtkContainer* container, GtkWidget* widget)
  (define-function void gtk_container_add (void* void*))

  ;; void gtk_container_add_with_properties (GtkContainer* container, GtkWidget* widget, const gchar* first_prop_name, ...)
  (define-function void gtk_container_add_with_properties (void* void* char* ...))

  ;; void gtk_container_check_resize (GtkContainer* container)
  (define-function void gtk_container_check_resize (void*))

  ;; void gtk_container_child_get (GtkContainer* container, GtkWidget* child, const gchar* first_prop_name, ...)
  (define-function void gtk_container_child_get (void* void* char* ...))

  ;; void gtk_container_child_get_property (GtkContainer* container, GtkWidget* child, const gchar* property_name, GValue* value)
  (define-function void gtk_container_child_get_property (void* void* char* void*))

  ;; void gtk_container_child_get_valist (GtkContainer* container, GtkWidget* child, const gchar* first_property_name, va_list var_args)
  (define-function/va_list void gtk_container_child_get_valist (void* void* char* va_list))

  ;; void gtk_container_child_set (GtkContainer* container, GtkWidget* child, const gchar* first_prop_name, ...)
  (define-function void gtk_container_child_set (void* void* char* ...))

  ;; void gtk_container_child_set_property (GtkContainer* container, GtkWidget* child, const gchar* property_name, const GValue* value)
  (define-function void gtk_container_child_set_property (void* void* char* void*))

  ;; void gtk_container_child_set_valist (GtkContainer* container, GtkWidget* child, const gchar* first_property_name, va_list var_args)
  (define-function/va_list void gtk_container_child_set_valist (void* void* char* va_list))

  ;; GType gtk_container_child_type (GtkContainer* container)
  (define-function unsigned-long gtk_container_child_type (void*))

  ;; GParamSpec* gtk_container_class_find_child_property (GObjectClass* cclass, const gchar* property_name)
  (define-function void* gtk_container_class_find_child_property (void* char*))

  ;; void gtk_container_class_install_child_property (GtkContainerClass* cclass, guint property_id, GParamSpec* pspec)
  (define-function void gtk_container_class_install_child_property (void* unsigned-int void*))

  ;; GParamSpec** gtk_container_class_list_child_properties (GObjectClass* cclass, guint* n_properties)
  (define-function void* gtk_container_class_list_child_properties (void* void*))

  ;; void gtk_container_forall (GtkContainer* container, GtkCallback callback, gpointer callback_data)
  (define-function void gtk_container_forall (void* (c-callback void (void* void*)) void*))

  ;; void gtk_container_foreach (GtkContainer* container, GtkCallback callback, gpointer callback_data)
  (define-function void gtk_container_foreach (void* (c-callback void (void* void*)) void*))

  ;; guint gtk_container_get_border_width (GtkContainer* container)
  (define-function unsigned-int gtk_container_get_border_width (void*))

  ;; GList* gtk_container_get_children (GtkContainer* container)
  (define-function void* gtk_container_get_children (void*))

  ;; gboolean gtk_container_get_focus_chain (GtkContainer* container, GList** focusable_widgets)
  (define-function int gtk_container_get_focus_chain (void* void*))

  ;; GtkWidget* gtk_container_get_focus_child (GtkContainer* container)
  (define-function void* gtk_container_get_focus_child (void*))

  ;; GtkAdjustment* gtk_container_get_focus_hadjustment (GtkContainer* container)
  (define-function void* gtk_container_get_focus_hadjustment (void*))

  ;; GtkAdjustment* gtk_container_get_focus_vadjustment (GtkContainer* container)
  (define-function void* gtk_container_get_focus_vadjustment (void*))

  ;; GtkResizeMode gtk_container_get_resize_mode (GtkContainer* container)
  (define-function int gtk_container_get_resize_mode (void*))

  ;; GType gtk_container_get_type (void)
  (define-function unsigned-long gtk_container_get_type ())

  ;; void gtk_container_propagate_expose (GtkContainer* container, GtkWidget* child, GdkEventExpose* event)
  (define-function void gtk_container_propagate_expose (void* void* void*))

  ;; void gtk_container_remove (GtkContainer* container, GtkWidget* widget)
  (define-function void gtk_container_remove (void* void*))

  ;; void gtk_container_resize_children (GtkContainer* container)
  (define-function void gtk_container_resize_children (void*))

  ;; void gtk_container_set_border_width (GtkContainer* container, guint border_width)
  (define-function void gtk_container_set_border_width (void* unsigned-int))

  ;; void gtk_container_set_focus_chain (GtkContainer* container, GList* focusable_widgets)
  (define-function void gtk_container_set_focus_chain (void* void*))

  ;; void gtk_container_set_focus_child (GtkContainer* container, GtkWidget* child)
  (define-function void gtk_container_set_focus_child (void* void*))

  ;; void gtk_container_set_focus_hadjustment (GtkContainer* container, GtkAdjustment* adjustment)
  (define-function void gtk_container_set_focus_hadjustment (void* void*))

  ;; void gtk_container_set_focus_vadjustment (GtkContainer* container, GtkAdjustment* adjustment)
  (define-function void gtk_container_set_focus_vadjustment (void* void*))

  ;; void gtk_container_set_reallocate_redraws (GtkContainer* container, gboolean needs_redraws)
  (define-function void gtk_container_set_reallocate_redraws (void* int))

  ;; void gtk_container_set_resize_mode (GtkContainer* container, GtkResizeMode resize_mode)
  (define-function void gtk_container_set_resize_mode (void* int))

  ;; void gtk_container_unset_focus_chain (GtkContainer* container)
  (define-function void gtk_container_unset_focus_chain (void*))

  ) ;[end]
