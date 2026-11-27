;;; gliver/contrib/ui/gleui/widgets/text.scm --- Pango Text Widget
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module defines the Pango text and markup widget for gleui.

(define-module (gliver contrib ui gleui widgets text)
  #:use-module (cairo)
  #:use-module (srfi srfi-11)
  #:use-module (gliver deps pango)
  #:use-module (gliver contrib ui gleui base draw)
  #:use-module (gliver contrib ui gleui widgets engine)
  #:declarative? #f
  #:export (
            gleui-w-text
))

(define* (gleui-w-text text #:key (font #f) (font-size #f) (color #f)
                       (markup? #t) (width #f) (ellipsize 'none)
                       (text-align 'left) (min-height 0))
  "Create a Pango TEXT widget.
Parameters: (TEXT [#:FONT #f] [#:FONT-SIZE #f] [#:COLOR #f] [#:MARKUP? #t] [#:WIDTH #f]
[#:ELLIPSIZE 'none] [#:TEXT-ALIGN 'left] [#:MIN-HEIGHT 0])
  - TEXT: String plain or Pango markup text.
  - FONT: String or #f font description (Pango default when #f).
  - FONT-SIZE: Integer or #f font size in points (Pango default when #f).
  - COLOR: String or #f hex foreground color.
  - MARKUP?: Boolean parsing Pango XML markup in TEXT when #t.
  - WIDTH: Integer or #f maximum width to start ellipsizing in pixels.
  - ELLIPSIZE: Symbol ellipsize mode ('none, 'start, 'middle, 'end).
  - TEXT-ALIGN: Symbol text alignment ('left, 'center, 'right).
  - MIN-HEIGHT: Integer minimum reserved height in pixels.
Returns <gleui-widget> node."
  (gleui-make-widget 'text
                     `((text . ,(or text ""))
                       (font . ,font)
                       (font-size . ,font-size)
                       (color . ,color)
                       (markup? . ,markup?)
                       (width . ,width)
                       (ellipsize . ,ellipsize)
                       (text-align . ,text-align)
                       (min-height . ,min-height))
                     #f))

(define (%layout-text cr widget avail-w avail-h tight-w? tight-h?)
  "Layout processor for Pango text WIDGET.
Parameters: (CR WIDGET AVAIL-W AVAIL-H TIGHT-W? TIGHT-H?)
  - CR: Cairo context used for measurement.
  - WIDGET: <gleui-widget> node.
  - AVAIL-W: Float or #f available width constraint in pixels.
  - AVAIL-H: Float or #f available height constraint in pixels.
  - TIGHT-W?: Boolean expanding to AVAIL-W if #t.
  - TIGHT-H?: Boolean expanding to AVAIL-H if #t.
Returns resolved <gleui-layout-node>."
  (let* ((props      (gleui-widget-props widget))
         (text       (gleui-widget-prop props 'text))
         (font       (gleui-widget-prop props 'font))
         (font-size  (gleui-widget-prop props 'font-size))
         (markup?    (gleui-widget-prop props 'markup?))
         (ellipsize  (gleui-widget-prop props 'ellipsize))
         (min-height (gleui-widget-prop props 'min-height))
         ;; resolve ellipsize width from explicit width or parent avail-w
         (wrap-width (or (gleui-widget-prop props 'width)
                         (and avail-w (not (eq? ellipsize 'none)) avail-w)
                         avail-w))
         (max-width  (and (number? wrap-width) (> wrap-width 0) wrap-width)))
    (let-values (((text-width text-height)
                  (if (and (string-null? text) (zero? min-height))
                      ;; still we have to measure empty string height so it doesn't
                      ;; fallback to min-height
                      (pango-measure-text cr "" #:font font #:font-size font-size #:markup? markup?)
                      (pango-measure-text cr text
                                          #:font font
                                          #:font-size font-size
                                          #:markup? markup?
                                          #:width max-width
                                          #:ellipsize ellipsize))))
      (let* ((text-align (gleui-widget-prop props 'text-align))
             (final-w    (cond
                          ;; empty text occupies zero horizontal space
                          ((string-null? text) 0)
                          ;; non-left alignment needs the full max-width span so pango aligns it
                          ((and max-width (not (eq? text-align 'left)))
                           (max text-width max-width))
                          (else text-width)))
             (final-h    (max min-height text-height)))
        (gleui-make-layout-node widget final-w final-h max-width #f)))))

(define (%paint-text! cr lnode x y push-hit!)
  "Paint Pango text layout node LNODE onto CR at (X, Y).
Parameters: (CR LNODE X Y PUSH-HIT!)
  - CR: Cairo context.
  - LNODE: <gleui-layout-node> node to paint.
  - X: Float left surface coordinate in pixels.
  - Y: Float top surface coordinate in pixels.
  - PUSH-HIT!: Procedure collecting <gleui-hit-box> records during painting."
  (let* ((widget     (gleui-layout-node-widget lnode))
         (props      (gleui-widget-props widget))
         (text       (gleui-widget-prop props 'text))
         (font       (gleui-widget-prop props 'font))
         (font-size  (gleui-widget-prop props 'font-size))
         (color      (gleui-widget-prop props 'color))
         (markup?    (gleui-widget-prop props 'markup?))
         (ellipsize  (gleui-widget-prop props 'ellipsize))
         (text-align (gleui-widget-prop props 'text-align))
         (max-width  (gleui-layout-node-data lnode)))
    (unless (string-null? text)
      ;; set text color if provided
      (when color
        (gleui-cairo-set-hex-color! cr color))
      (cairo-move-to cr x y)
      (pango-draw-text cr text
                       #:x x
                       #:y y
                       #:font font
                       #:font-size font-size
                       #:markup? markup?
                       #:width max-width
                       #:ellipsize ellipsize
                       #:align text-align))))

(gleui-register-widget! 'text %layout-text %paint-text!)
