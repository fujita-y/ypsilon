#!nobacktrace
;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2009 Y.FUJITA / LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

(library (ypsilon gtk button)

  (export gtk_button_action_get_type
          gtk_button_box_get_child_secondary
          gtk_button_box_get_layout
          gtk_button_box_get_type
          gtk_button_box_set_child_secondary
          gtk_button_box_set_layout
          gtk_button_box_style_get_type
          gtk_button_clicked
          gtk_button_enter
          gtk_button_get_alignment
          gtk_button_get_focus_on_click
          gtk_button_get_image
          gtk_button_get_image_position
          gtk_button_get_label
          gtk_button_get_relief
          gtk_button_get_type
          gtk_button_get_use_stock
          gtk_button_get_use_underline
          gtk_button_leave
          gtk_button_new
          gtk_button_new_from_stock
          gtk_button_new_with_label
          gtk_button_new_with_mnemonic
          gtk_button_pressed
          gtk_button_released
          gtk_button_set_alignment
          gtk_button_set_focus_on_click
          gtk_button_set_image
          gtk_button_set_image_position
          gtk_button_set_label
          gtk_button_set_relief
          gtk_button_set_use_stock
          gtk_button_set_use_underline)

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

  (define-syntax define-function/va_list
    (syntax-rules ()
      ((_ ret name args)
      (define name (lambda x (assertion-violation 'name "va_list argument not supported"))))))

  ;; GType gtk_button_action_get_type (void)
  (define-function unsigned-long gtk_button_action_get_type ())

  ;; gboolean gtk_button_box_get_child_secondary (GtkButtonBox* widget, GtkWidget* child)
  (define-function int gtk_button_box_get_child_secondary (void* void*))

  ;; GtkButtonBoxStyle gtk_button_box_get_layout (GtkButtonBox* widget)
  (define-function int gtk_button_box_get_layout (void*))

  ;; GType gtk_button_box_get_type (void)
  (define-function unsigned-long gtk_button_box_get_type ())

  ;; void gtk_button_box_set_child_secondary (GtkButtonBox* widget, GtkWidget* child, gboolean is_secondary)
  (define-function void gtk_button_box_set_child_secondary (void* void* int))

  ;; void gtk_button_box_set_layout (GtkButtonBox* widget, GtkButtonBoxStyle layout_style)
  (define-function void gtk_button_box_set_layout (void* int))

  ;; GType gtk_button_box_style_get_type (void)
  (define-function unsigned-long gtk_button_box_style_get_type ())

  ;; void gtk_button_clicked (GtkButton* button)
  (define-function void gtk_button_clicked (void*))

  ;; void gtk_button_enter (GtkButton* button)
  (define-function void gtk_button_enter (void*))

  ;; void gtk_button_get_alignment (GtkButton* button, gfloat* xalign, gfloat* yalign)
  (define-function void gtk_button_get_alignment (void* void* void*))

  ;; gboolean gtk_button_get_focus_on_click (GtkButton* button)
  (define-function int gtk_button_get_focus_on_click (void*))

  ;; GtkWidget* gtk_button_get_image (GtkButton* button)
  (define-function void* gtk_button_get_image (void*))

  ;; GtkPositionType gtk_button_get_image_position (GtkButton* button)
  (define-function int gtk_button_get_image_position (void*))

  ;; const gchar* gtk_button_get_label (GtkButton* button)
  (define-function char* gtk_button_get_label (void*))

  ;; GtkReliefStyle gtk_button_get_relief (GtkButton* button)
  (define-function int gtk_button_get_relief (void*))

  ;; GType gtk_button_get_type (void)
  (define-function unsigned-long gtk_button_get_type ())

  ;; gboolean gtk_button_get_use_stock (GtkButton* button)
  (define-function int gtk_button_get_use_stock (void*))

  ;; gboolean gtk_button_get_use_underline (GtkButton* button)
  (define-function int gtk_button_get_use_underline (void*))

  ;; void gtk_button_leave (GtkButton* button)
  (define-function void gtk_button_leave (void*))

  ;; GtkWidget* gtk_button_new (void)
  (define-function void* gtk_button_new ())

  ;; GtkWidget* gtk_button_new_from_stock (const gchar* stock_id)
  (define-function void* gtk_button_new_from_stock (char*))

  ;; GtkWidget* gtk_button_new_with_label (const gchar* label)
  (define-function void* gtk_button_new_with_label (char*))

  ;; GtkWidget* gtk_button_new_with_mnemonic (const gchar* label)
  (define-function void* gtk_button_new_with_mnemonic (char*))

  ;; void gtk_button_pressed (GtkButton* button)
  (define-function void gtk_button_pressed (void*))

  ;; void gtk_button_released (GtkButton* button)
  (define-function void gtk_button_released (void*))

  ;; void gtk_button_set_alignment (GtkButton* button, gfloat xalign, gfloat yalign)
  (define-function void gtk_button_set_alignment (void* float float))

  ;; void gtk_button_set_focus_on_click (GtkButton* button, gboolean focus_on_click)
  (define-function void gtk_button_set_focus_on_click (void* int))

  ;; void gtk_button_set_image (GtkButton* button, GtkWidget* image)
  (define-function void gtk_button_set_image (void* void*))

  ;; void gtk_button_set_image_position (GtkButton* button, GtkPositionType position)
  (define-function void gtk_button_set_image_position (void* int))

  ;; void gtk_button_set_label (GtkButton* button, const gchar* label)
  (define-function void gtk_button_set_label (void* char*))

  ;; void gtk_button_set_relief (GtkButton* button, GtkReliefStyle newstyle)
  (define-function void gtk_button_set_relief (void* int))

  ;; void gtk_button_set_use_stock (GtkButton* button, gboolean use_stock)
  (define-function void gtk_button_set_use_stock (void* int))

  ;; void gtk_button_set_use_underline (GtkButton* button, gboolean use_underline)
  (define-function void gtk_button_set_use_underline (void* int))

  ) ;[end]
