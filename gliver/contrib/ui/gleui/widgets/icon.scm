;;; gliver/contrib/ui/gleui/widgets/icon.scm --- Cairo Surface Icon Leaf Widget
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module defines the scaled Cairo image surface icon widget for gleui.

(define-module (gliver contrib ui gleui widgets icon)
  #:use-module (cairo)
  #:use-module (gliver contrib ui gleui base draw)
  #:use-module (gliver contrib ui gleui widgets engine)
  #:declarative? #f
  #:export (
            gleui-w-icon
))

(define* (gleui-w-icon surface #:key (size 48) (border-radius 0)
                       (border-width 0) (border-color #f))
  "Create a scaled Cairo SURFACE icon leaf widget.
Parameters: (SURFACE [#:SIZE 48] [#:BORDER-RADIUS 0] [#:BORDER-WIDTH 0] [#:BORDER-COLOR #f])
  - SURFACE: Cairo image surface, or #f.
  - SIZE: Integer target square icon edge length in pixels.
  - BORDER-RADIUS: Integer clip/border corner radius in pixels.
  - BORDER-WIDTH: Integer border thickness in pixels.
  - BORDER-COLOR: String or #f border hex color.
Returns <gleui-widget> node."
  (gleui-make-widget 'icon
                     `((surface . ,surface)
                       (size . ,size)
                       (border-radius . ,border-radius)
                       (border-width . ,border-width)
                       (border-color . ,border-color))
                     #f))

(define (%layout-icon cr widget avail-w avail-h tight-w? tight-h?)
  "Layout processor for icon WIDGET.
Parameters: (CR WIDGET AVAIL-W AVAIL-H TIGHT-W? TIGHT-H?)
  - CR: Cairo context used for measurement.
  - WIDGET: <gleui-widget> node.
  - AVAIL-W: Float or #f available width constraint in pixels.
  - AVAIL-H: Float or #f available height constraint in pixels.
  - TIGHT-W?: Boolean expanding to AVAIL-W if #t.
  - TIGHT-H?: Boolean expanding to AVAIL-H if #t.
Returns resolved <gleui-layout-node>."
  (let* ((props (gleui-widget-props widget))
         ;; icon is a fixed square leaf widget based on property size
         (size  (gleui-widget-prop props 'size)))
    (gleui-make-layout-node widget size size #f #f)))

(define (%paint-icon! cr lnode x y push-hit!)
  "Paint icon layout node LNODE onto CR at (X, Y).
Parameters: (CR LNODE X Y PUSH-HIT!)
  - CR: Cairo context.
  - LNODE: <gleui-layout-node> node to paint.
  - X: Float left surface coordinate in pixels.
  - Y: Float top surface coordinate in pixels.
  - PUSH-HIT!: Procedure collecting <gleui-hit-box> records during painting."
  (let* ((widget        (gleui-layout-node-widget lnode))
         (props         (gleui-widget-props widget))
         (surface       (gleui-widget-prop props 'surface))
         (size          (gleui-widget-prop props 'size))
         (border-radius (gleui-widget-prop props 'border-radius))
         (border-width  (gleui-widget-prop props 'border-width))
         (border-color  (gleui-widget-prop props 'border-color)))
    (when surface
      ;; fit the surface inside size x size
      (let* ((surface-width  (exact->inexact (cairo-image-surface-get-width surface)))
             (surface-height (exact->inexact (cairo-image-surface-get-height surface)))
             (scale          (/ (exact->inexact size)
                                (max 1.0 (max surface-width surface-height))))
             (draw-x         (+ x (/ (- size (* surface-width scale)) 2.0)))
             (draw-y         (+ y (/ (- size (* surface-height scale)) 2.0))))
        (cairo-save cr)
        ;; if border-radius is set, clip the scaled icon to rounded corners
        (when (> border-radius 0)
          (gleui-draw-rounded-rect cr x y size size border-radius)
          (cairo-clip cr))
        (cairo-translate cr draw-x draw-y)
        (cairo-scale cr scale scale)
        (cairo-set-source-surface cr surface 0.0 0.0)
        (cairo-paint cr)
        (cairo-restore cr)
        ;; draw the optional border outside the clip/scale state
        (gleui-draw-box! cr x y size size
                         #:radius border-radius
                         #:border-color border-color
                         #:border-width border-width)))))

(gleui-register-widget! 'icon %layout-icon %paint-icon!)
