;;; gliver/contrib/ui/gleui/base/draw.scm --- Cairo Drawing Utilities
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module provides Cairo clearing, color setup, and common
;;; drawing utilities for gleui.
;;; It handles:
;;;  - Surface clearing and hex color RGBA source configuration
;;;  - Rounded rectangle path construction and bordered box rendering

(define-module (gliver contrib ui gleui base draw)
  #:use-module (cairo)
  #:use-module (gliver deps color)
  #:declarative? #f
  #:export (
            gleui-cairo-clear!
            gleui-cairo-set-hex-color!
            gleui-draw-rounded-rect
            gleui-draw-box!
))

(define (gleui-cairo-clear! cr)
  "Clear Cairo context CR to transparent black and restore operator to 'over.
Parameters: (CR)
  - CR: Cairo context."
  (cairo-set-operator cr 'clear)
  (cairo-paint cr)
  (cairo-set-operator cr 'over))

(define (gleui-cairo-set-hex-color! cr color)
  "Parse hex string COLOR and set it as the source RGBA on Cairo context CR.
In a Cairo context, this more like picking a color from the palette in GIMP.
Parameters: (CR COLOR)
  - CR: Cairo context.
  - COLOR: String or #f hex color (#RGB, #RRGGBB, #RRGGBBAA).
Returns #t on success, #f if COLOR is invalid or #f."
  (and (string? color)
       (not (string-null? color))
       (let ((rgba (parse-hex-color-rgba color)))
         (and rgba
              (begin
                (cairo-set-source-rgba cr (car rgba) (cadr rgba) (caddr rgba) (cadddr rgba))
                #t)))))

(define (gleui-draw-rounded-rect cr x y w h r)
  "Draw a closed rounded-rectangle subpath at (X, Y) sized W by H with radius R to CR.
Parameters: (CR X Y W H R)
  - CR: Cairo context.
  - X: Float left coordinate in pixels.
  - Y: Float top coordinate in pixels.
  - W: Float rectangle width in pixels.
  - H: Float rectangle height in pixels.
  - R: Float corner radius in pixels (clamped to half the shorter edge)."
  (let* ((r-clamped (min (exact->inexact r) (/ w 2.0) (/ h 2.0)))
         (degrees (/ (acos -1) 180.0)))
    (cairo-new-sub-path cr)
    (cairo-arc cr (+ x w (- r-clamped)) (+ y r-clamped) r-clamped (* -90.0 degrees) 0.0)
    (cairo-arc cr (+ x w (- r-clamped)) (+ y h (- r-clamped)) r-clamped 0.0 (* 90.0 degrees))
    (cairo-arc cr (+ x r-clamped) (+ y h (- r-clamped)) r-clamped (* 90.0 degrees) (* 180.0 degrees))
    (cairo-arc cr (+ x r-clamped) (+ y r-clamped) r-clamped (* 180.0 degrees) (* 270.0 degrees))
    (cairo-close-path cr)))

(define* (gleui-draw-box! cr x y w h
                          #:key (radius 0)
                          (bg-color #f)
                          (border-color #f)
                          (border-width 0)
                          (border-edges 15) ;; use same flags as *wlr-layer-shell-anchor-*
                          (inset-border? #t))
  "Draw a filled and optionally bordered box at (X, Y) sized W by H on Cairo context CR.
Parameters: (CR X Y W H [#:RADIUS 0] [#:BG-COLOR #f] [#:BORDER-COLOR #f]
[#:BORDER-WIDTH 0] [#:BORDER-EDGES 15] [#:INSET-BORDER? #t])
  - CR: Cairo context.
  - X: Float left origin X coordinate in pixels.
  - Y: Float top origin Y coordinate in pixels.
  - W: Float box width in pixels.
  - H: Float box height in pixels.
  - RADIUS: Float or #f corner radius in pixels when all edges are bordered.
  - BG-COLOR: String or #f background fill hex color.
  - BORDER-COLOR: String or #f border stroke hex color.
  - BORDER-WIDTH: Float or #f border stroke thickness in pixels.
  - BORDER-EDGES: Integer or #f four-bit mask selecting top (1), bottom (2), left (4), and right (8) edges.
  - INSET-BORDER?: Boolean insetting full-edge border stroke inside W by H when #t."
  (let* ((edges      (if (number? border-edges)
                         (logand (inexact->exact border-edges) 15)
                         15))
         (all-edges? (= edges 15))
         (r          (if all-edges? (or radius 0) 0)))
    (when (gleui-cairo-set-hex-color! cr bg-color)
      (gleui-draw-rounded-rect cr x y w h r)
      (cairo-fill cr))
    (when (and (number? border-width) (> border-width 0)
               (not (zero? edges))
               (gleui-cairo-set-hex-color! cr border-color))
      (if all-edges?
          (let ((half-w (if inset-border? (/ border-width 2.0) 0.0)))
            (cairo-set-line-width cr border-width)
            (gleui-draw-rounded-rect cr
                                     (+ x half-w)
                                     (+ y half-w)
                                     (max 0 (- w (* 2.0 half-w)))
                                     (max 0 (- h (* 2.0 half-w)))
                                     r)
            (cairo-stroke cr))
          (begin
            ;; top border
            (when (not (zero? (logand edges 1)))
              (cairo-rectangle cr x y
                               w (min h border-width)))
            ;; bottom border
            (when (not (zero? (logand edges 2)))
              (cairo-rectangle cr x (max y (+ y (- h border-width)))
                               w (min h border-width)))
            ;; left border
            (when (not (zero? (logand edges 4)))
              (cairo-rectangle cr x y
                               (min w border-width) h))
            ;; right border
            (when (not (zero? (logand edges 8)))
              (cairo-rectangle cr (max x (+ x (- w border-width))) y
                               (min w border-width) h))
            (cairo-fill cr))))))
