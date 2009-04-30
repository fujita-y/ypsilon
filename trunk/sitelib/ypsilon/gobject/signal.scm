#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gobject signal)

  (export signal-callback
          g_signal_connect
          g_signal_connect_after
          g_signal_connect_swapped
          g_signal_handler_block
          g_signal_handler_disconnect
          g_signal_handler_is_connected
          g_signal_handler_unblock)

  (import (rnrs) (ypsilon ffi) (only (core) string-contains))

  (define lib-name
    (cond (on-linux   "libgobject-2.0.so.0")
          (on-linux   "libgobject-2.0.so.0")
          (on-freebsd "libgobject-2.0.so")
          (on-openbsd "libgobject-2.0.so")
          (on-darwin  "GLib.framework/GLib")
          (on-windows "libgobject-2.0-0.dll")
          (else
           (assertion-violation #f "can not locate GObject library, unknown operating system"))))

  (define lib (load-shared-object lib-name))

  (define-syntax define-function
    (syntax-rules ()
      ((_ ret name args)
       (define name (c-function lib lib-name ret name args)))))

  ;; gulong g_signal_connect_data (gpointer instance, const gchar* detailed_signal, GCallback c_handler, gpointer data, GClosureNotify destroy_data, GConnectFlags connect_flags)
  (define-function unsigned-long g_signal_connect_data (void* char* void* void* void* int))

  ;; gulong g_signal_connect_object (gpointer instance, const gchar* detailed_signal, GCallback c_handler, gpointer gobject, GConnectFlags connect_flags)
  (define-function unsigned-long g_signal_connect_object (void* char* (c-callback int (void*)) void* int))

  ;; void g_signal_handler_block (gpointer instance, gulong handler_id)
  (define-function void g_signal_handler_block (void* unsigned-long))

  ;; void g_signal_handler_disconnect (gpointer instance, gulong handler_id)
  (define-function void g_signal_handler_disconnect (void* unsigned-long))

  ;; gboolean g_signal_handler_is_connected (gpointer instance, gulong handler_id)
  (define-function int g_signal_handler_is_connected (void* unsigned-long))

  ;; void g_signal_handler_unblock (gpointer instance, gulong handler_id)
  (define-function void g_signal_handler_unblock (void* unsigned-long))

  (define g_signal_connect
    (lambda (instance detailed_signal c_handler data)
      (g_signal_connect_data instance detailed_signal c_handler data 0 0)))

  (define g_signal_connect_after
    (lambda (instance detailed_signal c_handler data)
      (g_signal_connect_data instance detailed_signal c_handler data 0 1)))

  (define g_signal_connect_swapped
    (lambda (instance detailed_signal c_handler data)
      (g_signal_connect_object instance detailed_signal c_handler data 2)))

  (define-syntax signal-callback
    (lambda (x)

      (define marshal
        (lambda (type)
          (let ((name (symbol->string type)))
            (cond ((or (eq? (string-contains name "Gtk") 0)
                       (eq? (string-contains name "Gdk") 0))
                   (if (string-contains name "*") 'void* 'int))
                  ((assq type
                         '((gboolean . int)
                           (gpointer . void*)
                           (gconstpointer . void*)
                           (gchar . int8_t)
                           (guchar . uint8_t)
                           (gint . int)
                           (guint . unsigned-int)
                           (gshort . short)
                           (gushort . unsigned-short)
                           (glong . long)
                           (gulong . unsigned-long)
                           (gint8 . int8_t)
                           (guint8 . uint8_t)
                           (gint16 . int16_t)
                           (guint16 . uint16_t)
                           (gint32 . int32_t)
                           (guint32 . uint32_t)
                           (gint64 . int64_t)
                           (guint64 . uint64_t)
                           (gfloat . float)
                           (gdouble . double)
                           (gsize . unsigned-long)
                           (gssize . long))) => cdr)
                  (else type)))))

      (syntax-case x ()
        ((_ ret args body)
         (with-syntax
             ((ret (datum->syntax #'k (marshal (syntax->datum #'ret))))
              (args (datum->syntax #'k (map marshal (syntax->datum #'args)))))
           #'(make-cdecl-callback 'ret 'args body))))))

  ) ;[end]
