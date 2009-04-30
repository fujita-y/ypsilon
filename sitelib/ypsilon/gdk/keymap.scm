#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gdk keymap)

  (export gdk_keymap_get_caps_lock_state
          gdk_keymap_get_default
          gdk_keymap_get_direction
          gdk_keymap_get_entries_for_keycode
          gdk_keymap_get_entries_for_keyval
          gdk_keymap_get_for_display
          gdk_keymap_get_type
          gdk_keymap_have_bidi_layouts
          gdk_keymap_lookup_key
          gdk_keymap_translate_keyboard_state)

  (import (rnrs) (ypsilon ffi))

  (define lib-name
    (cond (on-linux   "libgdk-x11-2.0.so.0")
          (on-sunos   "libgdk-x11-2.0.so.0")
          (on-freebsd "libgdk-x11-2.0.so.0")
          (on-openbsd "libgdk-x11-2.0.so.0")
          (on-darwin  "Gtk.framework/Gtk")
          (on-windows "libgdk-win32-2.0-0.dll")
          (else
           (assertion-violation #f "can not locate GDK library, unknown operating system"))))

  (define lib (load-shared-object lib-name))

  (define-syntax define-function
    (syntax-rules ()
      ((_ ret name args)
       (define name (c-function lib lib-name ret name args)))))

  ;; gboolean gdk_keymap_get_caps_lock_state (GdkKeymap* keymap)
  (define-function int gdk_keymap_get_caps_lock_state (void*))

  ;; GdkKeymap* gdk_keymap_get_default (void)
  (define-function void* gdk_keymap_get_default ())

  ;; PangoDirection gdk_keymap_get_direction (GdkKeymap* keymap)
  (define-function int gdk_keymap_get_direction (void*))

  ;; gboolean gdk_keymap_get_entries_for_keycode (GdkKeymap* keymap, guint hardware_keycode, GdkKeymapKey** keys, guint** keyvals, gint* n_entries)
  (define-function int gdk_keymap_get_entries_for_keycode (void* unsigned-int void* void* void*))

  ;; gboolean gdk_keymap_get_entries_for_keyval (GdkKeymap* keymap, guint keyval, GdkKeymapKey** keys, gint* n_keys)
  (define-function int gdk_keymap_get_entries_for_keyval (void* unsigned-int void* void*))

  ;; GdkKeymap* gdk_keymap_get_for_display (GdkDisplay* display)
  (define-function void* gdk_keymap_get_for_display (void*))

  ;; GType gdk_keymap_get_type (void)
  (define-function unsigned-long gdk_keymap_get_type ())

  ;; gboolean gdk_keymap_have_bidi_layouts (GdkKeymap* keymap)
  (define-function int gdk_keymap_have_bidi_layouts (void*))

  ;; guint gdk_keymap_lookup_key (GdkKeymap* keymap, const GdkKeymapKey* key)
  (define-function unsigned-int gdk_keymap_lookup_key (void* void*))

  ;; gboolean gdk_keymap_translate_keyboard_state (GdkKeymap* keymap, guint hardware_keycode, GdkModifierType state, gint group, guint* keyval, gint* effective_group, gint* level, GdkModifierType* consumed_modifiers)
  (define-function int gdk_keymap_translate_keyboard_state (void* unsigned-int int int void* void* void* void*))

  ) ;[end]
