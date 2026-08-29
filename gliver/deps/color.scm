;;; gliver/deps/color.scm --- Color manipulation and conversion library
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver deps color)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:export (
			hex-digit?
			hex-color?
			parse-hex-color-rgba
			color-hex->rgba-32
			hex->rgba
			color-hex->rgba
			hex->rgba-32
			rgba->hex
			rgb->hex
			rgba->rgba-32
			rgba-32->rgba
))

(define (hex-digit? c)
  "Return #t if character C is a hexadecimal digit (0-9, a-f, A-F), #f otherwise."
  (and (char? c)
       (or (char-numeric? c)
           (and (char>=? c #\a) (char<=? c #\f))
           (and (char>=? c #\A) (char<=? c #\F)))))

(define (hex-color? str)
  "Check if STR is a valid hex color string (#RGB, #RRGGBB, or #AARRGGBB)."
  (and (string? str)
       (string-prefix? "#" str)
       (memv (string-length str) '(4 7 9))
       (string-every hex-digit? (substring str 1))))

(define (parse-hex-color-rgba str)
  "Parse a hex color string into a list of normalized (R G B A) floats [0.0, 1.0]."
  (if (hex-color? str)
      (let ((hex (substring str 1)))
        (cond
         ((= (string-length hex) 3)
          (let ((r (string->number (string (string-ref hex 0) (string-ref hex 0)) 16))
                (g (string->number (string (string-ref hex 1) (string-ref hex 1)) 16))
                (b (string->number (string (string-ref hex 2) (string-ref hex 2)) 16)))
            (and r g b (list (/ r 255.0) (/ g 255.0) (/ b 255.0) 1.0))))
         ((= (string-length hex) 6)
          (let ((r (string->number (substring hex 0 2) 16))
                (g (string->number (substring hex 2 4) 16))
                (b (string->number (substring hex 4 6) 16)))
            (and r g b (list (/ r 255.0) (/ g 255.0) (/ b 255.0) 1.0))))
         ((= (string-length hex) 8)
          (let ((a (string->number (substring hex 0 2) 16))
                (r (string->number (substring hex 2 4) 16))
                (g (string->number (substring hex 4 6) 16))
                (b (string->number (substring hex 6 8) 16)))
            (and a r g b (list (/ r 255.0) (/ g 255.0) (/ b 255.0) (/ a 255.0)))))
         (else #f)))
      #f))

(define (color-hex->rgba-32 hex-str)
  "Convert a hex color string (with or without '#' prefix, 6 or 8 hex digits)
to a list of 4 pre-multiplied 32-bit integer values (R G B A) in the range [0, 4294967295]."
  ;; strip the leading '#' if it exists
  (let* ((clean-str (if (and (string? hex-str)
                             (> (string-length hex-str) 0)
                             (char=? (string-ref hex-str 0) #\#))
                        (substring hex-str 1)
                        hex-str))
         (len (if (string? clean-str) (string-length clean-str) 0))
         (get-val (lambda (start)
                    (string->number (substring clean-str start (+ start 2)) 16)))
         ;; multiplier to stretch 0-255 into 0-4294967295
         (scale 16843009))
    (cond
     ((and (string? clean-str) (or (= len 6) (= len 8)))
      (let* ((r (get-val 0))
             (g (get-val 2))
             (b (get-val 4))
             (a (if (= len 8) (get-val 6) 255))

             ;; calculate pre-multiplied 32-bit values using exact integers.
             ;; river uses 32-bit colors rather than 8-bit
             (a-32 (* a scale))
             (r-32 (quotient (* r a scale) 255))
             (g-32 (quotient (* g a scale) 255))
             (b-32 (quotient (* b a scale) 255)))

        (list r-32 g-32 b-32 a-32)))
     (else
      (format #t "Invalid hex color length. Expected 6 or 8 characters: ~a" hex-str)
      (list 4294967295 4294967295 4294967295 4294967295)))))

(define hex->rgba parse-hex-color-rgba)
(define color-hex->rgba parse-hex-color-rgba)
(define hex->rgba-32 color-hex->rgba-32)

(define* (rgba->hex r g b #:optional (a #f))
  "Convert normalized floats or 0-255 integers R G B and optional A to a hex color string."
  (let* ((to-int (lambda (val)
                   (cond
                    ((and (exact? val) (<= 0 val 255)) val)
                    ((inexact? val) (max 0 (min 255 (round (* val 255.0)))))
                    (else (error "Invalid color channel value" val)))))
         (ri (to-int r))
         (gi (to-int g))
         (bi (to-int b)))
    (if a
        (let ((ai (to-int a)))
          (format #f "#~2,'0x~2,'0x~2,'0x~2,'0x" ri gi bi ai))
        (format #f "#~2,'0x~2,'0x~2,'0x" ri gi bi))))

(define (rgb->hex r g b)
  "Convert normalized floats or 0-255 integers R G B to a #RRGGBB hex string."
  (rgba->hex r g b))

(define (rgba->rgba-32 rgba)
  "Convert a list of normalized floats (R G B A) or (R G B) [0.0, 1.0]
to 32-bit pre-multiplied RGBA integers."
  (let* ((scale 16843009)
         (r (round (* (list-ref rgba 0) 255)))
         (g (round (* (list-ref rgba 1) 255)))
         (b (round (* (list-ref rgba 2) 255)))
         (a (if (> (length rgba) 3)
                (round (* (list-ref rgba 3) 255))
                255))
         (a-32 (* (inexact->exact a) scale))
         (r-32 (quotient (* (inexact->exact r) (inexact->exact a) scale) 255))
         (g-32 (quotient (* (inexact->exact g) (inexact->exact a) scale) 255))
         (b-32 (quotient (* (inexact->exact b) (inexact->exact a) scale) 255)))
    (list r-32 g-32 b-32 a-32)))

(define (rgba-32->rgba rgba-32)
  "Convert a list of 32-bit pre-multiplied integer values (R G B A)
back to normalized floats (R G B A) in [0.0, 1.0]."
  (let* ((scale 16843009)
         (r-32 (list-ref rgba-32 0))
         (g-32 (list-ref rgba-32 1))
         (b-32 (list-ref rgba-32 2))
         (a-32 (list-ref rgba-32 3))
         (a (/ a-32 4294967295.0)))
    (if (= a 0.0)
        (list 0.0 0.0 0.0 0.0)
        (list (/ r-32 (* a 4294967295.0))
              (/ g-32 (* a 4294967295.0))
              (/ b-32 (* a 4294967295.0))
              a))))
