#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk get)

  (export gtk_get_current_event
          gtk_get_current_event_state
          gtk_get_current_event_time
          gtk_get_default_language
          gtk_get_event_widget
          gtk_get_option_group)

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

  ;; GdkEvent* gtk_get_current_event (void)
  (define-function void* gtk_get_current_event ())

  ;; gboolean gtk_get_current_event_state (GdkModifierType* state)
  (define-function int gtk_get_current_event_state (void*))

  ;; guint32 gtk_get_current_event_time (void)
  (define-function uint32_t gtk_get_current_event_time ())

  ;; PangoLanguage* gtk_get_default_language (void)
  (define-function void* gtk_get_default_language ())

  ;; GtkWidget* gtk_get_event_widget (GdkEvent* event)
  (define-function void* gtk_get_event_widget (void*))

  ;; GOptionGroup* gtk_get_option_group (gboolean open_default_display)
  (define-function void* gtk_get_option_group (int))

  ) ;[end]
