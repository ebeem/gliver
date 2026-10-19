;;; gliver/deps/libxkbcommon.scm --- libxkbcommon FFI bindings and helpers
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver deps libxkbcommon)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 format)
  #:use-module (gliver core ffi)
  #:use-module (gliver core logs)
  #:use-module (gliver deps libc)
  #:export (
			*libxkbcommon*
			xkb-context-new
			xkb-context-unref
			xkb-keymap-new-from-names
			xkb-keymap-new-from-string
			xkb-keymap-new-from-buffer
			xkb-keymap-unref
			xkb-keymap-layout-get-name
			xkb-keymap-get-as-string
			xkb-keymap-num-layouts
			xkb-keymap-layout-get-index
			xkb-state-new
			xkb-state-unref
			xkb-state-update-mask
			xkb-state-key-get-one-sym
			xkb-state-key-get-utf8
			xkb-keysym-to-utf8
			xkb-state-mod-name-is-active
			*ctrl-name*
			*alt-name*
			*shift-name*
			ctrl-name
			alt-name
			shift-name
			xkb-keysym-from-name
			xkb-keysym-get-name
			xkb-value->keysym-name
			query-xkbcommon-default-layout
			xkb-keymap-compile-from-names
			xkb-keymap->string
			xkb-keymap-layouts
))

(define *libxkbcommon*
  (catch #t
    (lambda ()
      (dynamic-link %libxkbcommon))
    (lambda (key . args)
      (log-error "Failed to load libxkbcommon: ~a ~a" key args)
      (log-error "Make sure libxkbcommon is installed.")
      #f)))

(define %xkb-func
  (lambda (name ret-type arg-types)
    (if *libxkbcommon*
        (catch #t
          (lambda ()
            (pointer->procedure ret-type (dynamic-func name *libxkbcommon*) arg-types))
          (lambda _ #f))
        #f)))

(define xkb-context-new
  (%xkb-func "xkb_context_new" '* (list int)))

(define xkb-context-unref
  (%xkb-func "xkb_context_unref" void (list '*)))

(define xkb-keymap-new-from-names
  (%xkb-func "xkb_keymap_new_from_names" '* (list '* '* int)))

(define xkb-keymap-new-from-string
  (%xkb-func "xkb_keymap_new_from_string" '* (list '* '* int int)))

(define xkb-keymap-new-from-buffer
  (%xkb-func "xkb_keymap_new_from_buffer" '* (list '* '* size_t int int)))

(define xkb-keymap-unref
  (%xkb-func "xkb_keymap_unref" void (list '*)))

(define xkb-keymap-layout-get-name
  (%xkb-func "xkb_keymap_layout_get_name" '* (list '* uint32)))

(define xkb-keymap-get-as-string
  (%xkb-func "xkb_keymap_get_as_string" '* (list '* int)))

(define xkb-keymap-num-layouts
  (%xkb-func "xkb_keymap_num_layouts" uint32 (list '*)))

(define xkb-keymap-layout-get-index
  (%xkb-func "xkb_keymap_layout_get_index" uint32 (list '* '*)))

(define xkb-state-new
  (%xkb-func "xkb_state_new" '* (list '*)))

(define xkb-state-unref
  (%xkb-func "xkb_state_unref" void (list '*)))

(define xkb-state-update-mask
  (%xkb-func "xkb_state_update_mask" uint32 (list '* uint32 uint32 uint32 uint32 uint32 uint32)))

(define xkb-state-key-get-one-sym
  (%xkb-func "xkb_state_key_get_one_sym" uint32 (list '* uint32)))

(define xkb-state-key-get-utf8
  (%xkb-func "xkb_state_key_get_utf8" int (list '* uint32 '* size_t)))

(define xkb-keysym-to-utf8
  (%xkb-func "xkb_keysym_to_utf8" int (list uint32 '* size_t)))

(define xkb-state-mod-name-is-active
  (%xkb-func "xkb_state_mod_name_is_active" int (list '* '* int)))

(define *ctrl-name* (string->pointer "Control"))
(define *alt-name* (string->pointer "Mod1"))
(define *shift-name* (string->pointer "Shift"))

(define ctrl-name *ctrl-name*)
(define alt-name *alt-name*)
(define shift-name *shift-name*)

(define xkb-keysym-from-name
  (%xkb-func "xkb_keysym_from_name" uint32 (list '* uint32)))

(define xkb-keysym-get-name
  (%xkb-func "xkb_keysym_get_name" int (list uint32 '* size_t)))

(define (xkb-value->keysym-name val)
  "Convert a uint value to its keysym string name via libxkbcommon."
  (if (not xkb-keysym-get-name)
      #f
      (let ((ptr (bytevector->pointer (make-bytevector 64))))
        (if (> (xkb-keysym-get-name val ptr 64) 0)
            (pointer->string ptr)
            #f))))

(define (query-xkbcommon-default-layout)
  "Query default layout name directly from libxkbcommon context without subprocesses."
  (if (and xkb-context-new xkb-keymap-new-from-names xkb-keymap-layout-get-name)
      (catch #t
        (lambda ()
          (let ((ctx (xkb-context-new 0)))
            (if (and ctx (pointer? ctx) (not (null-pointer? ctx)))
                (let ((km (xkb-keymap-new-from-names ctx %null-pointer 0)))
                  (if (and km (pointer? km) (not (null-pointer? km)))
                      (let* ((name-ptr (xkb-keymap-layout-get-name km 0))
                             (name (and name-ptr
                                        (pointer? name-ptr)
                                        (not (null-pointer? name-ptr))
                                        (pointer->string name-ptr))))
                        (when xkb-keymap-unref (xkb-keymap-unref km))
                        (when xkb-context-unref (xkb-context-unref ctx))
                        name)
                      (begin
                        (when xkb-context-unref (xkb-context-unref ctx))
                        #f)))
                #f)))
        (lambda _ #f))
      #f))

(define* (xkb-keymap-compile-from-names #:key (rules #f) (model #f) (layout #f) (variant #f) (options #f) (ctx #f))
  "Compile an XKB keymap from names. Returns (values keymap-ptr owned-ctx-or-#f).
Caller is responsible for calling xkb-keymap-unref (and xkb-context-unref if owned-ctx)."
  (if (or (not *libxkbcommon*) (not xkb-context-new) (not xkb-keymap-new-from-names))
      (values %null-pointer #f)
      (catch #t
        (lambda ()
          (let* ((own-ctx? (not ctx))
                 (context (or ctx (xkb-context-new 0))))
            (if (or (not context) (null-pointer? context))
                (values %null-pointer #f)
                (let* ((names-bv (make-bytevector (* 5 (sizeof '*)) 0))
                       (set-field! (lambda (idx val)
                                     (when (and val (string? val) (> (string-length val) 0))
                                       (bytevector-u64-native-set!
                                        names-bv (* idx (sizeof '*))
                                        (pointer-address (string->pointer val)))))))
                  (set-field! 0 rules)
                  (set-field! 1 model)
                  (set-field! 2 layout)
                  (set-field! 3 variant)
                  (set-field! 4 options)
                  (let ((km (xkb-keymap-new-from-names
                             context (bytevector->pointer names-bv) 0)))
                    (if (and km (pointer? km) (not (null-pointer? km)))
                        (values km (if own-ctx? context #f))
                        (begin
                          (when own-ctx? (xkb-context-unref context))
                          (values %null-pointer #f))))))))
        (lambda (key . args)
          (log-error "xkb-keymap-compile-from-names failed: ~a ~a" key args)
          (values %null-pointer #f)))))

(define* (xkb-keymap->string km #:optional (format 1))
  "Convert an XKB keymap pointer to an allocated Scheme string.
FORMAT defaults to 1 (XKB_KEYMAP_FORMAT_TEXT_V1)."
  (if (and km (pointer? km) (not (null-pointer? km)) xkb-keymap-get-as-string)
      (let ((str-ptr (xkb-keymap-get-as-string km format)))
        (if (and str-ptr (not (null-pointer? str-ptr)))
            (let ((str (pointer->string str-ptr)))
              (c-free str-ptr)
              str)
            #f))
      #f))

(define (xkb-keymap-layouts km)
  "Return a list of layout names for keymap KM."
  (if (and km (pointer? km) (not (null-pointer? km)) xkb-keymap-num-layouts xkb-keymap-layout-get-name)
      (let ((num (xkb-keymap-num-layouts km)))
        (let loop ((i 0) (res '()))
          (if (< i num)
              (let ((name-ptr (xkb-keymap-layout-get-name km i)))
                (loop (1+ i)
                      (cons (if (and name-ptr (not (null-pointer? name-ptr)))
                                (pointer->string name-ptr)
                                "")
                            res)))
              (reverse res))))
      '()))
