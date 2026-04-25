;;; gliver/render.scm --- Surface rendering utilities
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; Text rendering for the message bar and mode-line surfaces.
;;; Uses wlr-layer-shell + wl_shm for real Wayland surface rendering
;;; with a built-in bitmap font for text.

(define-module (gliver render)
  #:use-module (ice-9 format)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (gliver core)
  #:use-module (gliver wayland client)
  #:use-module (gliver wayland gen wayland)
  #:use-module (gliver wayland gen wlr-layer-shell-unstable-v1)
  #:declarative? #f
  #:export (;; font configuration
            font-set!
            *font-current*
            *font-current-size*

            ;; color parsing
            color-hex-parse
            color-hex->argb32

            ;; text metrics
            text-width
            text-height

            ;; layer surface management
            render-surface-create!
            render-surface-destroy!
            render-surface-resize!

            ;; surface rendering
            surface-text-render
            surface-bar-render))

(define %null-pointer (make-pointer 0))

;;; shared memory helpers

(define libc (dynamic-link))

(define %memfd-create
  (pointer->procedure int
    (dynamic-func "memfd_create" libc)
    (list '* unsigned-int)))

(define MFD_CLOEXEC 1)

(define %c-mmap
  (pointer->procedure '*
    (dynamic-func "mmap" libc)
    (list '* size_t int int int int64)))

(define %c-munmap
  (pointer->procedure int
    (dynamic-func "munmap" libc)
    (list '* size_t)))

(define %c-ftruncate
  (pointer->procedure int
    (dynamic-func "ftruncate" libc)
    (list int int64)))

(define %c-close
  (pointer->procedure int
    (dynamic-func "close" libc)
    (list int)))

(define PROT_READ  1)
(define PROT_WRITE 2)
(define MAP_SHARED 1)

(define (shm-open-anon size)
  "Create an anonymous shared-memory fd of SIZE bytes.
Returns the fd (integer)."
  (let ((fd (%memfd-create (string->pointer "gliver-shm") MFD_CLOEXEC)))
    (when (< fd 0)
      (error "memfd_create failed"))
    (let ((rc (%c-ftruncate fd size)))
      (when (< rc 0)
        (%c-close fd)
        (error "ftruncate failed on shm fd")))
    fd))

(define (shm-mmap fd size)
  "Memory-map SIZE bytes from FD.  Returns a pointer to the mapped region."
  (let ((ptr (%c-mmap %null-pointer size
                       (logior PROT_READ PROT_WRITE)
                       MAP_SHARED fd 0)))
    (when (= (pointer-address ptr) (- (expt 2 64) 1))  ;; MAP_FAILED
      (error "mmap failed"))
    ptr))

(define (shm-munmap ptr size)
  "Unmap a previously mapped shared-memory region."
  (%c-munmap ptr size))

;;; font configuration
(define *font-current* "monospace")
(define *font-current-size* 12)

(define (font-set! font-desc)
  "Set the font from a description like \"Iosevka 12\" or \"monospace 10\"."
  (let ((parts (string-split font-desc #\space)))
    (when (>= (length parts) 1)
      (set! *font-current* (car parts)))
    (when (>= (length parts) 2)
      (let ((size (string->number (cadr parts))))
        (when size
          (set! *font-current-size* size))))))

;;; color parsing
(define (color-hex-parse hex)
  "Parse a hex color string like \"#5588ff\" into (r g b) floats 0.0-1.0."
  (let ((str (if (and (> (string-length hex) 0)
                      (char=? (string-ref hex 0) #\#))
                 (substring hex 1)
                 hex)))
    (if (= (string-length str) 6)
        (let ((r (/ (or (string->number (substring str 0 2) 16) 0) 255.0))
              (g (/ (or (string->number (substring str 2 4) 16) 0) 255.0))
              (b (/ (or (string->number (substring str 4 6) 16) 0) 255.0)))
          (list r g b))
        (list 1.0 1.0 1.0))))

(define (color-hex->argb32 hex)
  "Parse a hex color string into a 32-bit ARGB value."
  (let ((str (if (and (> (string-length hex) 0)
                      (char=? (string-ref hex 0) #\#))
                 (substring hex 1)
                 hex)))
    (if (= (string-length str) 6)
        (let ((r (or (string->number (substring str 0 2) 16) 0))
              (g (or (string->number (substring str 2 4) 16) 0))
              (b (or (string->number (substring str 4 6) 16) 0)))
          (logior (ash #xff 24) (ash r 16) (ash g 8) b))
        #xffffffff)))

;;; text metrics
(define *glyph-width* 6)
(define *glyph-height* 10)

(define (text-width text)
  "Return text width in pixels using the bitmap font."
  (* (string-length text) (quotient (* *font-current-size* 6) 10)))

(define (text-height)
  "Return text height in pixels."
  (+ *font-current-size* 4))

;;; built-in 6x10 bitmap font (ascii 32–126)
;;; each character is a vector of 10 rows; each row is a 6-bit integer
;;; where bit 5 is the leftmost pixel.
(define *font-glyphs*
  (vector
   ;; 32 space
   #(#b000000 #b000000 #b000000 #b000000 #b000000
     #b000000 #b000000 #b000000 #b000000 #b000000)
   ;; 33 !
   #(#b000000 #b001000 #b001000 #b001000 #b001000
     #b001000 #b000000 #b001000 #b000000 #b000000)
   ;; 34 "
   #(#b000000 #b010100 #b010100 #b010100 #b000000
     #b000000 #b000000 #b000000 #b000000 #b000000)
   ;; 35 #
   #(#b000000 #b010100 #b111110 #b010100 #b010100
     #b111110 #b010100 #b000000 #b000000 #b000000)
   ;; 36 $
   #(#b000000 #b001000 #b011110 #b101000 #b011100
     #b001010 #b111100 #b001000 #b000000 #b000000)
   ;; 37 %
   #(#b000000 #b100010 #b100100 #b001000 #b001000
     #b010000 #b010010 #b100010 #b000000 #b000000)
   ;; 38 &
   #(#b000000 #b011000 #b100100 #b011000 #b011010
     #b100100 #b100100 #b011010 #b000000 #b000000)
   ;; 39 '
   #(#b000000 #b001000 #b001000 #b000000 #b000000
     #b000000 #b000000 #b000000 #b000000 #b000000)
   ;; 40 (
   #(#b000000 #b000100 #b001000 #b010000 #b010000
     #b010000 #b001000 #b000100 #b000000 #b000000)
   ;; 41 )
   #(#b000000 #b010000 #b001000 #b000100 #b000100
     #b000100 #b001000 #b010000 #b000000 #b000000)
   ;; 42 *
   #(#b000000 #b000000 #b001000 #b101010 #b011100
     #b101010 #b001000 #b000000 #b000000 #b000000)
   ;; 43 +
   #(#b000000 #b000000 #b001000 #b001000 #b111110
     #b001000 #b001000 #b000000 #b000000 #b000000)
   ;; 44 ,
   #(#b000000 #b000000 #b000000 #b000000 #b000000
     #b000000 #b001000 #b001000 #b010000 #b000000)
   ;; 45 -
   #(#b000000 #b000000 #b000000 #b000000 #b111110
     #b000000 #b000000 #b000000 #b000000 #b000000)
   ;; 46 .
   #(#b000000 #b000000 #b000000 #b000000 #b000000
     #b000000 #b000000 #b001000 #b000000 #b000000)
   ;; 47 /
   #(#b000000 #b000010 #b000100 #b001000 #b001000
     #b010000 #b100000 #b000000 #b000000 #b000000)
   ;; 48 0
   #(#b000000 #b011100 #b100010 #b100110 #b101010
     #b110010 #b100010 #b011100 #b000000 #b000000)
   ;; 49 1
   #(#b000000 #b001000 #b011000 #b001000 #b001000
     #b001000 #b001000 #b011100 #b000000 #b000000)
   ;; 50 2
   #(#b000000 #b011100 #b100010 #b000010 #b001100
     #b010000 #b100000 #b111110 #b000000 #b000000)
   ;; 51 3
   #(#b000000 #b011100 #b100010 #b000010 #b001100
     #b000010 #b100010 #b011100 #b000000 #b000000)
   ;; 52 4
   #(#b000000 #b000100 #b001100 #b010100 #b100100
     #b111110 #b000100 #b000100 #b000000 #b000000)
   ;; 53 5
   #(#b000000 #b111110 #b100000 #b111100 #b000010
     #b000010 #b100010 #b011100 #b000000 #b000000)
   ;; 54 6
   #(#b000000 #b011100 #b100000 #b100000 #b111100
     #b100010 #b100010 #b011100 #b000000 #b000000)
   ;; 55 7
   #(#b000000 #b111110 #b000010 #b000100 #b001000
     #b010000 #b010000 #b010000 #b000000 #b000000)
   ;; 56 8
   #(#b000000 #b011100 #b100010 #b100010 #b011100
     #b100010 #b100010 #b011100 #b000000 #b000000)
   ;; 57 9
   #(#b000000 #b011100 #b100010 #b100010 #b011110
     #b000010 #b000010 #b011100 #b000000 #b000000)
   ;; 58 :
   #(#b000000 #b000000 #b000000 #b001000 #b000000
     #b000000 #b001000 #b000000 #b000000 #b000000)
   ;; 59 ;
   #(#b000000 #b000000 #b000000 #b001000 #b000000
     #b000000 #b001000 #b001000 #b010000 #b000000)
   ;; 60 <
   #(#b000000 #b000010 #b000100 #b001000 #b010000
     #b001000 #b000100 #b000010 #b000000 #b000000)
   ;; 61 =
   #(#b000000 #b000000 #b000000 #b111110 #b000000
     #b111110 #b000000 #b000000 #b000000 #b000000)
   ;; 62 >
   #(#b000000 #b010000 #b001000 #b000100 #b000010
     #b000100 #b001000 #b010000 #b000000 #b000000)
   ;; 63 ?
   #(#b000000 #b011100 #b100010 #b000010 #b000100
     #b001000 #b000000 #b001000 #b000000 #b000000)
   ;; 64 @
   #(#b000000 #b011100 #b100010 #b101110 #b101010
     #b101110 #b100000 #b011100 #b000000 #b000000)
   ;; 65 A
   #(#b000000 #b011100 #b100010 #b100010 #b111110
     #b100010 #b100010 #b100010 #b000000 #b000000)
   ;; 66 B
   #(#b000000 #b111100 #b100010 #b100010 #b111100
     #b100010 #b100010 #b111100 #b000000 #b000000)
   ;; 67 C
   #(#b000000 #b011100 #b100010 #b100000 #b100000
     #b100000 #b100010 #b011100 #b000000 #b000000)
   ;; 68 D
   #(#b000000 #b111100 #b100010 #b100010 #b100010
     #b100010 #b100010 #b111100 #b000000 #b000000)
   ;; 69 E
   #(#b000000 #b111110 #b100000 #b100000 #b111100
     #b100000 #b100000 #b111110 #b000000 #b000000)
   ;; 70 F
   #(#b000000 #b111110 #b100000 #b100000 #b111100
     #b100000 #b100000 #b100000 #b000000 #b000000)
   ;; 71 G
   #(#b000000 #b011100 #b100010 #b100000 #b100110
     #b100010 #b100010 #b011100 #b000000 #b000000)
   ;; 72 H
   #(#b000000 #b100010 #b100010 #b100010 #b111110
     #b100010 #b100010 #b100010 #b000000 #b000000)
   ;; 73 I
   #(#b000000 #b011100 #b001000 #b001000 #b001000
     #b001000 #b001000 #b011100 #b000000 #b000000)
   ;; 74 J
   #(#b000000 #b000010 #b000010 #b000010 #b000010
     #b000010 #b100010 #b011100 #b000000 #b000000)
   ;; 75 K
   #(#b000000 #b100010 #b100100 #b101000 #b110000
     #b101000 #b100100 #b100010 #b000000 #b000000)
   ;; 76 L
   #(#b000000 #b100000 #b100000 #b100000 #b100000
     #b100000 #b100000 #b111110 #b000000 #b000000)
   ;; 77 M
   #(#b000000 #b100010 #b110110 #b101010 #b101010
     #b100010 #b100010 #b100010 #b000000 #b000000)
   ;; 78 N
   #(#b000000 #b100010 #b110010 #b101010 #b100110
     #b100010 #b100010 #b100010 #b000000 #b000000)
   ;; 79 O
   #(#b000000 #b011100 #b100010 #b100010 #b100010
     #b100010 #b100010 #b011100 #b000000 #b000000)
   ;; 80 P
   #(#b000000 #b111100 #b100010 #b100010 #b111100
     #b100000 #b100000 #b100000 #b000000 #b000000)
   ;; 81 Q
   #(#b000000 #b011100 #b100010 #b100010 #b100010
     #b101010 #b100100 #b011010 #b000000 #b000000)
   ;; 82 R
   #(#b000000 #b111100 #b100010 #b100010 #b111100
     #b101000 #b100100 #b100010 #b000000 #b000000)
   ;; 83 S
   #(#b000000 #b011100 #b100010 #b100000 #b011100
     #b000010 #b100010 #b011100 #b000000 #b000000)
   ;; 84 T
   #(#b000000 #b111110 #b001000 #b001000 #b001000
     #b001000 #b001000 #b001000 #b000000 #b000000)
   ;; 85 U
   #(#b000000 #b100010 #b100010 #b100010 #b100010
     #b100010 #b100010 #b011100 #b000000 #b000000)
   ;; 86 V
   #(#b000000 #b100010 #b100010 #b100010 #b100010
     #b010100 #b010100 #b001000 #b000000 #b000000)
   ;; 87 W
   #(#b000000 #b100010 #b100010 #b100010 #b101010
     #b101010 #b110110 #b100010 #b000000 #b000000)
   ;; 88 X
   #(#b000000 #b100010 #b100010 #b010100 #b001000
     #b010100 #b100010 #b100010 #b000000 #b000000)
   ;; 89 Y
   #(#b000000 #b100010 #b100010 #b010100 #b001000
     #b001000 #b001000 #b001000 #b000000 #b000000)
   ;; 90 Z
   #(#b000000 #b111110 #b000010 #b000100 #b001000
     #b010000 #b100000 #b111110 #b000000 #b000000)
   ;; 91 [
   #(#b000000 #b011100 #b010000 #b010000 #b010000
     #b010000 #b010000 #b011100 #b000000 #b000000)
   ;; 92 backslash
   #(#b000000 #b100000 #b010000 #b001000 #b001000
     #b000100 #b000010 #b000000 #b000000 #b000000)
   ;; 93 ]
   #(#b000000 #b011100 #b000100 #b000100 #b000100
     #b000100 #b000100 #b011100 #b000000 #b000000)
   ;; 94 ^
   #(#b000000 #b001000 #b010100 #b100010 #b000000
     #b000000 #b000000 #b000000 #b000000 #b000000)
   ;; 95 _
   #(#b000000 #b000000 #b000000 #b000000 #b000000
     #b000000 #b000000 #b000000 #b111110 #b000000)
   ;; 96 `
   #(#b000000 #b010000 #b001000 #b000100 #b000000
     #b000000 #b000000 #b000000 #b000000 #b000000)
   ;; 97 a
   #(#b000000 #b000000 #b000000 #b011100 #b000010
     #b011110 #b100010 #b011110 #b000000 #b000000)
   ;; 98 b
   #(#b000000 #b100000 #b100000 #b111100 #b100010
     #b100010 #b100010 #b111100 #b000000 #b000000)
   ;; 99 c
   #(#b000000 #b000000 #b000000 #b011100 #b100000
     #b100000 #b100000 #b011100 #b000000 #b000000)
   ;; 100 d
   #(#b000000 #b000010 #b000010 #b011110 #b100010
     #b100010 #b100010 #b011110 #b000000 #b000000)
   ;; 101 e
   #(#b000000 #b000000 #b000000 #b011100 #b100010
     #b111110 #b100000 #b011100 #b000000 #b000000)
   ;; 102 f
   #(#b000000 #b001100 #b010000 #b010000 #b111100
     #b010000 #b010000 #b010000 #b000000 #b000000)
   ;; 103 g
   #(#b000000 #b000000 #b000000 #b011110 #b100010
     #b100010 #b011110 #b000010 #b011100 #b000000)
   ;; 104 h
   #(#b000000 #b100000 #b100000 #b111100 #b100010
     #b100010 #b100010 #b100010 #b000000 #b000000)
   ;; 105 i
   #(#b000000 #b001000 #b000000 #b011000 #b001000
     #b001000 #b001000 #b011100 #b000000 #b000000)
   ;; 106 j
   #(#b000000 #b000100 #b000000 #b000100 #b000100
     #b000100 #b000100 #b100100 #b011000 #b000000)
   ;; 107 k
   #(#b000000 #b100000 #b100000 #b100100 #b101000
     #b110000 #b101000 #b100100 #b000000 #b000000)
   ;; 108 l
   #(#b000000 #b011000 #b001000 #b001000 #b001000
     #b001000 #b001000 #b011100 #b000000 #b000000)
   ;; 109 m
   #(#b000000 #b000000 #b000000 #b110100 #b101010
     #b101010 #b101010 #b100010 #b000000 #b000000)
   ;; 110 n
   #(#b000000 #b000000 #b000000 #b111100 #b100010
     #b100010 #b100010 #b100010 #b000000 #b000000)
   ;; 111 o
   #(#b000000 #b000000 #b000000 #b011100 #b100010
     #b100010 #b100010 #b011100 #b000000 #b000000)
   ;; 112 p
   #(#b000000 #b000000 #b000000 #b111100 #b100010
     #b100010 #b111100 #b100000 #b100000 #b000000)
   ;; 113 q
   #(#b000000 #b000000 #b000000 #b011110 #b100010
     #b100010 #b011110 #b000010 #b000010 #b000000)
   ;; 114 r
   #(#b000000 #b000000 #b000000 #b101100 #b110000
     #b100000 #b100000 #b100000 #b000000 #b000000)
   ;; 115 s
   #(#b000000 #b000000 #b000000 #b011100 #b100000
     #b011100 #b000010 #b111100 #b000000 #b000000)
   ;; 116 t
   #(#b000000 #b010000 #b010000 #b111100 #b010000
     #b010000 #b010000 #b001100 #b000000 #b000000)
   ;; 117 u
   #(#b000000 #b000000 #b000000 #b100010 #b100010
     #b100010 #b100010 #b011110 #b000000 #b000000)
   ;; 118 v
   #(#b000000 #b000000 #b000000 #b100010 #b100010
     #b010100 #b010100 #b001000 #b000000 #b000000)
   ;; 119 w
   #(#b000000 #b000000 #b000000 #b100010 #b100010
     #b101010 #b101010 #b010100 #b000000 #b000000)
   ;; 120 x
   #(#b000000 #b000000 #b000000 #b100010 #b010100
     #b001000 #b010100 #b100010 #b000000 #b000000)
   ;; 121 y
   #(#b000000 #b000000 #b000000 #b100010 #b100010
     #b011110 #b000010 #b000010 #b011100 #b000000)
   ;; 122 z
   #(#b000000 #b000000 #b000000 #b111110 #b000100
     #b001000 #b010000 #b111110 #b000000 #b000000)
   ;; 123 {
   #(#b000000 #b000100 #b001000 #b001000 #b010000
     #b001000 #b001000 #b000100 #b000000 #b000000)
   ;; 124 |
   #(#b000000 #b001000 #b001000 #b001000 #b001000
     #b001000 #b001000 #b001000 #b000000 #b000000)
   ;; 125 }
   #(#b000000 #b010000 #b001000 #b001000 #b000100
     #b001000 #b001000 #b010000 #b000000 #b000000)
   ;; 126 ~
   #(#b000000 #b000000 #b000000 #b010000 #b101010
     #b000100 #b000000 #b000000 #b000000 #b000000)
   ))

;;; pixel buffer helpers
(define (buffer-fill-rect! data stride x y w h color)
  "Fill a rectangle in the ARGB pixel buffer DATA with COLOR (uint32).
STRIDE is in bytes."
  (let ((bpp 4))
    (let row-loop ((row 0))
      (when (< row h)
        (let ((row-offset (* (+ y row) stride)))
          (let col-loop ((col 0))
            (when (< col w)
              (let ((offset (+ row-offset (* (+ x col) bpp))))
                (bytevector-u32-native-set! data offset color))
              (col-loop (1+ col)))))
        (row-loop (1+ row))))))

(define (buffer-draw-glyph! data stride ch x y fg-color bg-color)
  "Draw a single character glyph at pixel position (X, Y).
Returns the x-advance (glyph width)."
  (let* ((code (char->integer ch))
         (idx (- code 32))
         (glyph (if (and (>= idx 0) (< idx (vector-length *font-glyphs*)))
                    (vector-ref *font-glyphs* idx)
                    (vector-ref *font-glyphs* 0)))  ;; space for unknown
         (bpp 4))
    (let row-loop ((row 0))
      (when (< row *glyph-height*)
        (let ((bits (vector-ref glyph row))
              (py (+ y row)))
          (let col-loop ((col 0))
            (when (< col *glyph-width*)
              (let* ((px (+ x col))
                     (offset (+ (* py stride) (* px bpp)))
                     (bit-set? (not (zero? (logand bits
                                                   (ash 1 (- *glyph-width* 1 col)))))))
                (when (>= offset 0)
                  (bytevector-u32-native-set! data offset
                                              (if bit-set? fg-color bg-color))))
              (col-loop (1+ col)))))
        (row-loop (1+ row))))
    *glyph-width*))

(define (buffer-draw-text! data stride text x y max-width fg-color bg-color)
  "Draw TEXT string starting at (X, Y), clipping at MAX-WIDTH.
Returns the x position after the last drawn character."
  (let loop ((i 0) (cx x))
    (if (or (>= i (string-length text))
            (> (+ cx *glyph-width*) (+ x max-width)))
        cx
        (begin
          (buffer-draw-glyph! data stride (string-ref text i) cx y fg-color bg-color)
          (loop (1+ i) (+ cx *glyph-width*))))))

;;; render-surface: managed layer surface with shm buffer
(define-record-type <render-surface>
  (%make-render-surface wl-surface layer-surface
                        shm-fd shm-pool wl-buffer
                        shm-data shm-size
                        width height stride
                        configured? buffer-busy?)
  render-surface?
  (wl-surface     rs-wl-surface)
  (layer-surface  rs-layer-surface)
  (shm-fd         rs-shm-fd        rs-shm-fd-set!)
  (shm-pool       rs-shm-pool      rs-shm-pool-set!)
  (wl-buffer      rs-wl-buffer     rs-wl-buffer-set!)
  (shm-data       rs-shm-data      rs-shm-data-set!)
  (shm-size       rs-shm-size      rs-shm-size-set!)
  (width          rs-width         rs-width-set!)
  (height         rs-height        rs-height-set!)
  (stride         rs-stride        rs-stride-set!)
  (configured?    rs-configured?   rs-configured-set!)
  (buffer-busy?   rs-buffer-busy?  rs-buffer-busy-set!))

(define *render-surfaces* '())  ;; alist of (id . <render-surface>)

(define (rendering-available?)
  "Check if the Wayland globals needed for rendering are bound."
  (let ((compositor (@@ (gliver river connector) *wl-compositor*))
        (shm        (@@ (gliver river connector) *wl-shm*))
        (layer-sh   (@@ (gliver river connector) *zwlr-layer-shell*)))
    (and (not (null-pointer? compositor))
         (not (null-pointer? shm))
         (not (null-pointer? layer-sh)))))

(define (allocate-shm-buffer! rs width height)
  "Allocate (or reallocate) the shared-memory buffer for render surface RS."
  (let* ((stride (* width 4))
         (size (* stride height))
         (shm (@@ (gliver river connector) *wl-shm*)))
    ;; clean up old resources
    (when (rs-wl-buffer rs)
      (wl-buffer-destroy (rs-wl-buffer rs)))
    (when (rs-shm-data rs)
      (shm-munmap (rs-shm-data rs) (rs-shm-size rs)))
    (when (rs-shm-pool rs)
      (wl-shm-pool-destroy (rs-shm-pool rs)))
    (when (and (rs-shm-fd rs) (> (rs-shm-fd rs) 0))
      (close-fdes (rs-shm-fd rs)))
    ;; create new shm buffer
    (let* ((fd (shm-open-anon size))
           (data-ptr (shm-mmap fd size))
           (pool (wl-shm-create-pool shm fd size))
           (buffer (wl-shm-pool-create-buffer pool 0 width height
                                               stride WL_SHM_FORMAT_ARGB8888)))
      ;; set up buffer release listener
      (let ((listener (make-wl-buffer-listener
                       (lambda (data buf)
                         (rs-buffer-busy-set! rs #f)))))
        (wl-proxy-add-listener buffer listener %null-pointer))
      ;; update the record
      (rs-shm-fd-set! rs fd)
      (rs-shm-pool-set! rs pool)
      (rs-wl-buffer-set! rs buffer)
      (rs-shm-data-set! rs data-ptr)
      (rs-shm-size-set! rs size)
      (rs-width-set! rs width)
      (rs-height-set! rs height)
      (rs-stride-set! rs stride))))

(define* (render-surface-create! #:key
                                 (anchor (logior ZWLR_LAYER_SURFACE_V1_ANCHOR_LEFT
                                                 ZWLR_LAYER_SURFACE_V1_ANCHOR_RIGHT
                                                 ZWLR_LAYER_SURFACE_V1_ANCHOR_BOTTOM))
                                 (width 0)
                                 (height 24)
                                 (exclusive-zone -1)
                                 (layer ZWLR_LAYER_SHELL_V1_LAYER_TOP)
                                 (namespace "gliver")
                                 (keyboard-interactivity
                                  ZWLR_LAYER_SURFACE_V1_KEYBOARD_INTERACTIVITY_NONE)
                                 (output #f))
  "Create a new layer surface for rendering.
Returns a <render-surface> record, or #f if rendering is unavailable.

ANCHOR: bitmask of ZWLR_LAYER_SURFACE_V1_ANCHOR_* edges.
WIDTH/HEIGHT: desired surface dimensions (0 = stretch to anchored edges).
EXCLUSIVE-ZONE: pixels reserved for this surface (-1 = surface extent).
LAYER: which layer (BACKGROUND, BOTTOM, TOP, OVERLAY).
NAMESPACE: layer-shell namespace string.
OUTPUT: wl_output proxy, or #f for compositor default."
  (if (not (rendering-available?))
      (begin
        (log-warn "Rendering infrastructure not available")
        #f)
      (let* ((compositor (@@ (gliver river connector) *wl-compositor*))
             (layer-sh   (@@ (gliver river connector) *zwlr-layer-shell*))
             (wl-surf (wl-compositor-create-surface compositor))
             (output-ptr (or output %null-pointer))
             (layer-surf (zwlr-layer-shell-v1-get-layer-surface
                          layer-sh wl-surf output-ptr layer namespace)))
        ;; configure the layer surface
        (zwlr-layer-surface-v1-set-size layer-surf width height)
        (zwlr-layer-surface-v1-set-anchor layer-surf anchor)
        (when (>= exclusive-zone 0)
          (zwlr-layer-surface-v1-set-exclusive-zone layer-surf exclusive-zone))
        (zwlr-layer-surface-v1-set-keyboard-interactivity
         layer-surf keyboard-interactivity)

        (let ((rs (%make-render-surface
                   wl-surf layer-surf
                   #f #f #f     ;; shm-fd, shm-pool, wl-buffer
                   #f 0         ;; shm-data, shm-size
                   width height (* width 4)
                   #f #f)))     ;; configured?, buffer-busy?
          ;; attach the configure/closed listener
          (let ((listener
                 (make-zwlr-layer-surface-v1-listener
                  ;; on-configure: (data proxy serial width height)
                  (lambda (data proxy serial w h)
                    (log-debug "Layer surface configure: ~ax~a serial=~a" w h serial)
                    (zwlr-layer-surface-v1-ack-configure proxy serial)
                    (let ((new-w (if (zero? w) (rs-width rs) w))
                          (new-h (if (zero? h) (rs-height rs) h)))
                      (when (or (not (rs-configured? rs))
                                (not (= new-w (rs-width rs)))
                                (not (= new-h (rs-height rs))))
                        (allocate-shm-buffer! rs new-w new-h))
                      (rs-configured-set! rs #t)))
                  ;; on-closed: (data proxy)
                  (lambda (data proxy)
                    (log-info "Layer surface closed by compositor")
                    (rs-configured-set! rs #f)))))
            (wl-proxy-add-listener layer-surf listener %null-pointer))

          ;; initial commit to trigger configure
          (wl-surface-commit wl-surf)

          ;; synchronous roundtrip to receive the configure event
          (let ((display (@@ (gliver river connector) *wl-display*)))
            (wl-display-roundtrip display))

          (log-info "Render surface created: ~ax~a" (rs-width rs) (rs-height rs))
          rs))))

(define (render-surface-destroy! rs)
  "Destroy a render surface and free all resources."
  (when rs
    ;; free the buffer
    (when (rs-wl-buffer rs)
      (wl-buffer-destroy (rs-wl-buffer rs))
      (rs-wl-buffer-set! rs #f))
    ;; unmap
    (when (rs-shm-data rs)
      (shm-munmap (rs-shm-data rs) (rs-shm-size rs))
      (rs-shm-data-set! rs #f))
    ;; destroy pool
    (when (rs-shm-pool rs)
      (wl-shm-pool-destroy (rs-shm-pool rs))
      (rs-shm-pool-set! rs #f))
    ;; close fd
    (when (and (rs-shm-fd rs) (> (rs-shm-fd rs) 0))
      (close-fdes (rs-shm-fd rs))
      (rs-shm-fd-set! rs #f))
    ;; destroy layer surface
    (zwlr-layer-surface-v1-destroy (rs-layer-surface rs))
    ;; destroy wl_surface
    (wl-surface-destroy (rs-wl-surface rs))))

(define (render-surface-resize! rs width height)
  "Request a resize of the render surface."
  (zwlr-layer-surface-v1-set-size (rs-layer-surface rs) width height)
  (wl-surface-commit (rs-wl-surface rs))
  (let ((display (@@ (gliver river connector) *wl-display*)))
    (wl-display-roundtrip display)))

;;; render-surface commit helper
(define (render-surface-commit! rs)
  "Attach the current buffer and commit the surface."
  (when (and (rs-configured? rs) (rs-wl-buffer rs))
    (wl-surface-attach (rs-wl-surface rs) (rs-wl-buffer rs) 0 0)
    (wl-surface-damage (rs-wl-surface rs) 0 0 (rs-width rs) (rs-height rs))
    (wl-surface-commit (rs-wl-surface rs))
    (rs-buffer-busy-set! rs #t)))

;;; surface-text-render
(define *text-render-surface* #f)

(define (surface-text-render text fg-color bg-color width height)
  "Render TEXT onto a layer surface using the bitmap font.
FG-COLOR and BG-COLOR are hex strings like \"#rrggbb\".
WIDTH and HEIGHT are the desired surface dimensions.
Creates a layer surface on first call; reuses it thereafter."
  (if (not (rendering-available?))
      (begin
        (log-debug "render-text (no wayland): ~s (~ax~a) fg=~a bg=~a"
                   text width height fg-color bg-color)
        #f)
      (begin
        ;; create surface on first use
        (unless *text-render-surface*
          (set! *text-render-surface*
                (render-surface-create!
                 #:anchor (logior ZWLR_LAYER_SURFACE_V1_ANCHOR_LEFT
                                  ZWLR_LAYER_SURFACE_V1_ANCHOR_RIGHT
                                  ZWLR_LAYER_SURFACE_V1_ANCHOR_BOTTOM)
                 #:width width
                 #:height height
                 #:exclusive-zone height
                 #:namespace "gliver-text")))
        (let ((rs *text-render-surface*))
          (when (and rs (rs-configured? rs) (rs-shm-data rs))
            ;; resize if needed
            (when (or (not (= width (rs-width rs)))
                      (not (= height (rs-height rs))))
              (render-surface-resize! rs width height))
            (let* ((data (pointer->bytevector (rs-shm-data rs) (rs-shm-size rs)))
                   (fg (color-hex->argb32 fg-color))
                   (bg (color-hex->argb32 bg-color))
                   (stride (rs-stride rs))
                   (text-y (max 0 (quotient (- (rs-height rs) *glyph-height*) 2)))
                   (text-x 4))  ;; left padding
              ;; fill background
              (buffer-fill-rect! data stride 0 0 (rs-width rs) (rs-height rs) bg)
              ;; draw text
              (buffer-draw-text! data stride text text-x text-y
                                 (- (rs-width rs) (* 2 text-x))
                                 fg bg)
              ;; commit
              (render-surface-commit! rs)))))))

;;; surface-bar-render
(define *bar-render-surface* #f)

(define (surface-bar-render segments width height fg-color bg-color)
  "Render a bar with multiple SEGMENTS onto a layer surface.
Each segment is (text . alignment) where alignment is 'left, 'center, or 'right.
FG-COLOR and BG-COLOR are hex strings."
  (if (not (rendering-available?))
      (begin
        (log-debug "render-bar (no wayland): ~a segments, ~ax~a"
                   (length segments) width height)
        #f)
      (begin
        ;; create surface on first use
        (unless *bar-render-surface*
          (set! *bar-render-surface*
                (render-surface-create!
                 #:anchor (logior ZWLR_LAYER_SURFACE_V1_ANCHOR_LEFT
                                  ZWLR_LAYER_SURFACE_V1_ANCHOR_RIGHT
                                  ZWLR_LAYER_SURFACE_V1_ANCHOR_TOP)
                 #:width width
                 #:height height
                 #:exclusive-zone height
                 #:namespace "gliver-bar")))
        (let ((rs *bar-render-surface*))
          (when (and rs (rs-configured? rs) (rs-shm-data rs))
            ;; resize if needed
            (when (or (not (= width (rs-width rs)))
                      (not (= height (rs-height rs))))
              (render-surface-resize! rs width height))
            (let* ((data (pointer->bytevector (rs-shm-data rs) (rs-shm-size rs)))
                   (fg (color-hex->argb32 fg-color))
                   (bg (color-hex->argb32 bg-color))
                   (stride (rs-stride rs))
                   (text-y (max 0 (quotient (- (rs-height rs) *glyph-height*) 2)))
                   (padding 4))
              ;; fill background
              (buffer-fill-rect! data stride 0 0 (rs-width rs) (rs-height rs) bg)
              ;; draw each segment
              (for-each
               (lambda (segment)
                 (let* ((text (car segment))
                        (align (cdr segment))
                        (tw (text-width text))
                        (x (case align
                             ((left)   padding)
                             ((center) (max 0 (quotient (- (rs-width rs) tw) 2)))
                             ((right)  (max 0 (- (rs-width rs) tw padding)))
                             (else     padding))))
                   (buffer-draw-text! data stride text x text-y
                                      (- (rs-width rs) x) fg bg)))
               segments)
              ;; commit
              (render-surface-commit! rs)))))))
