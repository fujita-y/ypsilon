#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk activatable)

  (export gtk_activatable_do_set_related_action
          gtk_activatable_get_related_action
          gtk_activatable_get_type
          gtk_activatable_get_use_action_appearance
          gtk_activatable_set_related_action
          gtk_activatable_set_use_action_appearance
          gtk_activatable_sync_action_properties)

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

  ;; void gtk_activatable_do_set_related_action (GtkActivatable* activatable, GtkAction* action)
  (define-function void gtk_activatable_do_set_related_action (void* void*))

  ;; GtkAction* gtk_activatable_get_related_action (GtkActivatable* activatable)
  (define-function void* gtk_activatable_get_related_action (void*))

  ;; GType gtk_activatable_get_type (void)
  (define-function unsigned-long gtk_activatable_get_type ())

  ;; gboolean gtk_activatable_get_use_action_appearance (GtkActivatable* activatable)
  (define-function int gtk_activatable_get_use_action_appearance (void*))

  ;; void gtk_activatable_set_related_action (GtkActivatable* activatable, GtkAction* action)
  (define-function void gtk_activatable_set_related_action (void* void*))

  ;; void gtk_activatable_set_use_action_appearance (GtkActivatable* activatable, gboolean use_appearance)
  (define-function void gtk_activatable_set_use_action_appearance (void* int))

  ;; void gtk_activatable_sync_action_properties (GtkActivatable* activatable, GtkAction* action)
  (define-function void gtk_activatable_sync_action_properties (void* void*))

  ) ;[end]
