#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gdk spawn)

  (export gdk_spawn_command_line_on_screen
          gdk_spawn_on_screen
          gdk_spawn_on_screen_with_pipes)

  (import (rnrs) (ypsilon ffi))

  (define lib-name
    (cond (on-darwin  "Gtk.framework/Gtk")
          (on-linux   "libgdk-x11-2.0.so.0")
          (on-freebsd "libgdk-x11-2.0.so.0")
          (on-openbsd "libgdk-x11-2.0.so.0")
          (on-windows "libgdk-win32-2.0-0.dll")
          (else
           (assertion-violation #f "can not locate GDK library, unknown operating system"))))

  (define lib (load-shared-object lib-name))

  (define-syntax define-function
    (syntax-rules ()
      ((_ ret name args)
       (define name (c-function lib lib-name ret name args)))))

  ;; gboolean gdk_spawn_command_line_on_screen (GdkScreen* screen, const gchar* command_line, GError** error)
  (define-function int gdk_spawn_command_line_on_screen (void* char* void*))

  ;; gboolean gdk_spawn_on_screen (GdkScreen* screen, const gchar* working_directory, gchar** argv, gchar** envp, GSpawnFlags flags, GSpawnChildSetupFunc child_setup, gpointer user_data, gint* child_pid, GError** error)
  (define-function int gdk_spawn_on_screen (void* char* void* void* int (c-callback void (void*)) void* void* void*))

  ;; gboolean gdk_spawn_on_screen_with_pipes (GdkScreen* screen, const gchar* working_directory, gchar** argv, gchar** envp, GSpawnFlags flags, GSpawnChildSetupFunc child_setup, gpointer user_data, gint* child_pid, gint* standard_input, gint* standard_output, gint* standard_error, GError** error)
  (define-function int gdk_spawn_on_screen_with_pipes (void* char* void* void* int (c-callback void (void*)) void* void* void* void* void* void*))

  ) ;[end]
