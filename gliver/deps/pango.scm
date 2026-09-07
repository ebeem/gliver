;;; gliver/deps/pango.scm --- Minimal Pango and PangoCairo FFI bindings
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver deps pango)
  #:use-module (cairo)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  #:use-module (gliver core ffi)
  #:export (
            pango-escape-markup
            pango-measure-text
            pango-draw-text
            ))

;; core pango engine
(define *libpango*
  (catch #t
    (lambda ()
      (dynamic-link %libpango))
    (lambda (key . args)
      (log-error "Failed to load libpango-1.0: ~a ~a" key args)
      (log-error "Make sure pango is installed.")
      #f)))

;; bridge between pango engine and cairo surfaces
(define *libpangocairo*
  (catch #t
    (lambda ()
      (dynamic-link %libpangocairo))
    (lambda (key . args)
      (log-error "Failed to load libpangocairo-1.0: ~a ~a" key args)
      (log-error "Make sure pango is installed.")
      #f)))

;; pango layout is GLib's object, used to release (unref them)
(define *libgobject*
  (catch #t
    (lambda ()
      (dynamic-link %libgobject))
    (lambda (key . args)
      (log-error "Failed to load libgobject-2.0 (pango dependency): ~a ~a" key args)
      (log-error "Make sure pango is installed.")
      #f)))

(define (pango-func lib name ret-type arg-types)
  (if lib
      (catch #t
        (lambda ()
          (pointer->procedure ret-type (dynamic-func name lib) arg-types))
        (lambda _
          (lambda _ (error (format #f "Pango symbol ~a not found in library" name)))))
      (lambda _ (error (format #f "Pango library not loaded, cannot call ~a" name)))))

(define (cairo-context->raw-pointer cr)
  "Extract the raw C (cairo_t *) pointer from a Guile cairo context SMOB or pointer."
  (cond
   ((not cr) #f)
   ((pointer? cr) cr)
   (else
    (catch #t
      (lambda ()
        (let ((smob-ptr (scm->pointer cr)))
          (dereference-pointer (make-pointer (+ (pointer-address smob-ptr) (sizeof '*))))))
      (lambda _ #f)))))

;;; ffi bindings
(define %pango-cairo-create-layout
  (pango-func *libpangocairo* "pango_cairo_create_layout" '* (list '*)))

(define %pango-cairo-show-layout
  (pango-func *libpangocairo* "pango_cairo_show_layout" void (list '* '*)))

(define %pango-font-description-from-string
  (pango-func *libpango* "pango_font_description_from_string" '* (list '*)))

(define %pango-font-description-free
  (pango-func *libpango* "pango_font_description_free" void (list '*)))

(define %pango-layout-set-text
  (pango-func *libpango* "pango_layout_set_text" void (list '* '* int)))

(define %pango-layout-set-markup
  (pango-func *libpango* "pango_layout_set_markup" void (list '* '* int)))

(define %pango-layout-set-font-description
  (pango-func *libpango* "pango_layout_set_font_description" void (list '* '*)))

(define %pango-layout-set-width
  (pango-func *libpango* "pango_layout_set_width" void (list '* int)))

(define %pango-layout-set-ellipsize
  (pango-func *libpango* "pango_layout_set_ellipsize" void (list '* int)))

(define %pango-layout-set-alignment
  (pango-func *libpango* "pango_layout_set_alignment" void (list '* int)))

(define %pango-layout-get-pixel-size
  (pango-func *libpango* "pango_layout_get_pixel_size" void (list '* '* '*)))

(define %g-object-unref
  (pango-func *libgobject* "g_object_unref" void (list '*)))

;; layout lifecycle helper
(define-syntax-rule (with-pango-layout (layout cr) body ...)
  (let* ((raw (cairo-context->raw-pointer cr))
         (layout (if (and raw (not (null-pointer? raw)))
                     (%pango-cairo-create-layout raw)
                     #f)))
    (dynamic-wind
      (lambda () #f)
      (lambda () body ...)
      (lambda ()
        (when (and layout (not (null-pointer? layout)))
          (%g-object-unref layout))))))

(define (pango-escape-markup str)
  "Escape XML/Pango markup special characters in STR."
  (if (string? str)
      (string-replace-substring
       (string-replace-substring
        (string-replace-substring
         (string-replace-substring
          (string-replace-substring str "&" "&amp;")
          "<" "&lt;")
         ">" "&gt;")
        "\"" "&quot;")
       "'" "&apos;")
      (pango-escape-markup (format #f "~a" str))))

(define (%setup-layout! layout text-or-markup font font-size markup? width ellipsize align)
  (when (or font font-size)
    (let* ((font-str (cond
                      ((and font font-size) (format #f "~a ~a" font font-size))
                      (font (format #f "~a" font))
                      (else (format #f "~a" font-size))))
           (desc-ptr (%pango-font-description-from-string (string->pointer font-str))))
      (when (and desc-ptr (not (null-pointer? desc-ptr)))
        (%pango-layout-set-font-description layout desc-ptr)
        (%pango-font-description-free desc-ptr))))

  (if width
      (%pango-layout-set-width layout (* (inexact->exact (round width)) 1024))
      (%pango-layout-set-width layout -1))

  (let ((ellip (cond
                ((eq? ellipsize 'start) 1)
                ((eq? ellipsize 'middle) 2)
                ((eq? ellipsize 'end) 3)
                (else 0))))
    (%pango-layout-set-ellipsize layout ellip))

  (when align
    (let ((al (cond
               ((eq? align 'center) 1)
               ((eq? align 'right) 2)
               (else 0))))
      (%pango-layout-set-alignment layout al)))

  (let ((ptr (string->pointer (if (string? text-or-markup)
                                  text-or-markup
                                  (format #f "~a" (or text-or-markup ""))))))
    (if markup?
        (%pango-layout-set-markup layout ptr -1)
        (%pango-layout-set-text layout ptr -1))))

(define* (pango-measure-text cr text-or-markup
                             #:key (font #f) (font-size #f) (markup? #t)
                             (width #f) (ellipsize 'none))
  "Measure text or markup size in pixels on Cairo context CR.
Returns (values width height)."
  (with-pango-layout (layout cr)
					 (if (or (not layout) (null-pointer? layout))
						 (values 0 0)
						 (begin
						   (%setup-layout! layout text-or-markup font font-size markup? width ellipsize #f)
						   (let ((w-bv (make-bytevector (sizeof int) 0))
								 (h-bv (make-bytevector (sizeof int) 0)))
							 (%pango-layout-get-pixel-size layout
														   (bytevector->pointer w-bv)
														   (bytevector->pointer h-bv))
							 (values (bytevector-s32-native-ref w-bv 0)
									 (bytevector-s32-native-ref h-bv 0)))))))

(define* (pango-draw-text cr text-or-markup
                          #:key (x #f) (y #f) (font #f) (font-size #f)
                          (markup? #t) (width #f) (ellipsize 'none)
                          (align 'left))
  "Render text or markup on Cairo context CR."
  (when (or x y)
    (let ((px (if x (exact->inexact x) 0.0))
          (py (if y (exact->inexact y) 0.0)))
      (when (not (pointer? cr))
        (cairo-move-to cr px py))))
  (let ((raw-cr (cairo-context->raw-pointer cr)))
    (when (and raw-cr (not (null-pointer? raw-cr)))
      (with-pango-layout (layout raw-cr)
						 (when (and layout (not (null-pointer? layout)))
						   (%setup-layout! layout text-or-markup font font-size markup? width ellipsize align)
						   (%pango-cairo-show-layout raw-cr layout))))))
