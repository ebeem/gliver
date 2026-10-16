;;; gliver/deps/librsvg.scm --- Minimal librsvg FFI bindings
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver deps librsvg)
  #:use-module (cairo)
  #:use-module (system foreign)
  #:use-module (ice-9 format)
  #:use-module (gliver core ffi)
  #:use-module (gliver core logs)
  #:export (
			*librsvg*
			rsvg-handle-new-from-file
			rsvg-handle-render-cairo
			rsvg-handle-get-dimensions
))

(define *librsvg*
  (catch #t
    (lambda ()
      (dynamic-link %librsvg))
    (lambda (key . args)
      (log-error "Failed to load librsvg-2 (library): ~a ~a" key args)
      (log-error "Make sure librsvg is installed.")
      #f)))

(define (%rsvg-func name ret-type arg-types)
  (if *librsvg*
      (catch #t
        (lambda ()
          (pointer->procedure ret-type (dynamic-func name *librsvg*) arg-types))
        (lambda _
          (lambda _ (error (format #f "librsvg symbol ~a not found in library" name)))))
      (lambda _ (error (format #f "librsvg library not loaded, cannot call ~a" name)))))

(define rsvg-handle-new-from-file
  (%rsvg-func "rsvg_handle_new_from_file" '* (list '* '*)))

(define rsvg-handle-render-cairo
  (%rsvg-func "rsvg_handle_render_cairo" int (list '* '*)))

(define rsvg-handle-get-dimensions
  (%rsvg-func "rsvg_handle_get_dimensions" void (list '* '*)))
