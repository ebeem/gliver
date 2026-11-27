;;; gliver/contrib/ui/gleui/widgets/box.scm --- Container Box Widget
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module defines the container box widget for gleui.

(define-module (gliver contrib ui gleui widgets box)
  #:use-module (cairo)
  #:use-module (srfi srfi-11)
  #:use-module (gliver contrib ui gleui base draw)
  #:use-module (gliver contrib ui gleui widgets engine)
  #:declarative? #f
  #:export (
            gleui-w-box
))

(define* (gleui-w-box child
                      #:key (width #f) (height #f)
                      (min-width 0) (min-height 0)
                      (max-width #f) (max-height #f)
                      (expand-x? #f) (expand-y? #f)
                      (padding #f) (padding-x #f) (padding-y #f)
                      (padding-top #f) (padding-right #f)
                      (padding-bottom #f) (padding-left #f)
                      (bg-color #f) (border-color #f) (border-width 0)
                      (border-edges 15) (inset-border? #t) (radius 0) (clip? #f)
                      (align 'top-left))
  "Create a container box widget wrapping CHILD with optional sizing, padding,
background, border, and alignment.
Parameters: (CHILD [#:WIDTH #f] [#:HEIGHT #f] [#:MIN-WIDTH 0] [#:MIN-HEIGHT 0] [#:MAX-WIDTH #f]
[#:MAX-HEIGHT #f] [#:EXPAND-X? #f] [#:EXPAND-Y? #f] [#:PADDING #f] [#:PADDING-X #f]
[#:PADDING-Y #f] [#:PADDING-TOP #f] [#:PADDING-RIGHT #f] [#:PADDING-BOTTOM #f] [#:PADDING-LEFT #f]
[#:BG-COLOR #f] [#:BORDER-COLOR #f] [#:BORDER-WIDTH 0] [#:BORDER-EDGES 15] [#:INSET-BORDER? #t]
[#:RADIUS 0] [#:CLIP? #f] [#:ALIGN 'top-left])
  - CHILD: <gleui-widget> node or #f (empty spacer).
  - WIDTH: Integer or #f fixed width in pixels.
  - HEIGHT: Integer or #f fixed height in pixels.
  - MIN-WIDTH: Integer minimum box width in pixels.
  - MIN-HEIGHT: Integer minimum box height in pixels.
  - MAX-WIDTH: Integer or #f maximum box width in pixels.
  - MAX-HEIGHT: Integer or #f maximum box height in pixels.
  - EXPAND-X?: Boolean expanding horizontally to fill available width when #t.
  - EXPAND-Y?: Boolean expanding vertically to fill available height when #t.
  - PADDING: Integer, list of 2 or 4 integers, or #f edge padding (check engine).
  - PADDING-X: Integer or #f horizontal padding override in pixels.
  - PADDING-Y: Integer or #f vertical padding override in pixels.
  - PADDING-TOP: Integer or #f top edge padding override in pixels.
  - PADDING-RIGHT: Integer or #f right edge padding override in pixels.
  - PADDING-BOTTOM: Integer or #f bottom edge padding override in pixels.
  - PADDING-LEFT: Integer or #f left edge padding override in pixels.
  - BG-COLOR: String or #f background hex color.
  - BORDER-COLOR: String or #f border hex color.
  - BORDER-WIDTH: Integer border thickness in pixels.
  - BORDER-EDGES: Integer four-bit mask (1=top, 2=bottom, 4=left, 8=right).
  - INSET-BORDER?: Boolean insetting border stroke inside box bounds when #t.
  - RADIUS: Integer rounded corner radius in pixels.
  - CLIP?: Boolean clipping child painting to box bounds when #t.
  - ALIGN: Symbol or integer (1-9 numpad) alignment for CHILD within inner bounds.
Returns <gleui-widget> node."
  (gleui-make-widget 'box
                     `((width . ,width)
                       (height . ,height)
                       (min-width . ,min-width)
                       (min-height . ,min-height)
                       (max-width . ,max-width)
                       (max-height . ,max-height)
                       (expand-x? . ,expand-x?)
                       (expand-y? . ,expand-y?)
                       (padding . ,padding)
                       (padding-x . ,padding-x)
                       (padding-y . ,padding-y)
                       (padding-top . ,padding-top)
                       (padding-right . ,padding-right)
                       (padding-bottom . ,padding-bottom)
                       (padding-left . ,padding-left)
                       (bg-color . ,bg-color)
                       (border-color . ,border-color)
                       (border-width . ,border-width)
                       (border-edges . ,border-edges)
                       (inset-border? . ,inset-border?)
                       (radius . ,radius)
                       (clip? . ,clip?)
                       (align . ,align))
                     child))

(define (%layout-box cr widget avail-w avail-h tight-w? tight-h?)
  "Layout processor for box WIDGET.
Parameters: (CR WIDGET AVAIL-W AVAIL-H TIGHT-W? TIGHT-H?)
  - CR: Cairo context used for measurement.
  - WIDGET: <gleui-widget> node.
  - AVAIL-W: Float or #f available width constraint in pixels.
  - AVAIL-H: Float or #f available height constraint in pixels.
  - TIGHT-W?: Boolean expanding to AVAIL-W if #t.
  - TIGHT-H?: Boolean expanding to AVAIL-H if #t.
Returns resolved <gleui-layout-node>."
  (let* ((props      (gleui-widget-props widget))
         (child      (gleui-widget-children widget))
         (explicit-w (gleui-widget-prop props 'width))
         (explicit-h (gleui-widget-prop props 'height))
         (expand-x?  (or (and (number? explicit-w) #t)
                         (and tight-w? avail-w #t)
                         (gleui-widget-prop props 'expand-x?)))
         (expand-y?  (or (and (number? explicit-h) #t)
                         (and tight-h? avail-h #t)
                         (gleui-widget-prop props 'expand-y?)))
         (min-w      (gleui-widget-prop props 'min-width))
         (min-h      (gleui-widget-prop props 'min-height))
         (max-w      (or (gleui-widget-prop props 'max-width)
                         avail-w))
         (max-h      (or (gleui-widget-prop props 'max-height)
                         avail-h)))
    (let-values (((pad-top pad-right pad-bottom pad-left)
                  (gleui-widget-resolve-padding props)))
      (let* ((pad-w         (+ pad-left pad-right))
             (pad-h         (+ pad-top pad-bottom))
             (bound-w       (or explicit-w max-w))
             (bound-h       (or explicit-h max-h))
             (inner-avail-w (and bound-w (max 0 (- bound-w pad-w))))
             (inner-avail-h (and bound-h (max 0 (- bound-h pad-h))))
             (inner-tight-w (and expand-x? #t))
             (inner-tight-h (and expand-y? #t))
             ;; it's important to calculate layout of child as well
             (child-layout  (gleui-widget-layout cr child inner-avail-w inner-avail-h
                                                 inner-tight-w inner-tight-h))
             (raw-w         (cond
                             ;; explicit width
                             ((number? explicit-w) explicit-w)
                             ;; expand to available width
                             ((and expand-x? avail-w) avail-w)
                             ;; shrink to child: we use the layout of child here
                             (else (+ (gleui-layout-node-width child-layout) pad-w))))
             (raw-h         (cond
                             ((number? explicit-h) explicit-h)
                             ((and expand-y? avail-h) avail-h)
                             (else (+ (gleui-layout-node-height child-layout) pad-h))))
             (final-w       (gleui-widget-clamp-dim raw-w min-w max-w))
             (final-h       (gleui-widget-clamp-dim raw-h min-h max-h)))
        (gleui-make-layout-node widget final-w final-h
                                (list pad-top pad-right pad-bottom pad-left)
                                (list child-layout))))))

(define (%paint-box! cr lnode x y push-hit!)
  "Paint box layout node LNODE onto CR at (X, Y).
Parameters: (CR LNODE X Y PUSH-HIT!)
  - CR: Cairo context.
  - LNODE: <gleui-layout-node> node to paint.
  - X: Float left surface coordinate in pixels.
  - Y: Float top surface coordinate in pixels.
  - PUSH-HIT!: Procedure collecting <gleui-hit-box> records during painting."
  (let* ((widget        (gleui-layout-node-widget lnode))
         (props         (gleui-widget-props widget))
         (box-w         (gleui-layout-node-width lnode))
         (box-h         (gleui-layout-node-height lnode))
         (padding       (gleui-layout-node-data lnode))
         (pad-top       (car padding))
         (pad-right     (cadr padding))
         (pad-bottom    (caddr padding))
         (pad-left      (cadddr padding))
         (bg-color      (gleui-widget-prop props 'bg-color))
         (border-color  (gleui-widget-prop props 'border-color))
         (border-width  (gleui-widget-prop props 'border-width))
         (border-edges  (gleui-widget-prop props 'border-edges))
         (inset-border? (gleui-widget-prop props 'inset-border?))
         (radius        (gleui-widget-prop props 'radius))
         (clip?         (gleui-widget-prop props 'clip?))
         (align         (gleui-widget-prop props 'align))
         ;; in a box, we know we will only have one child
         ;; if #f is provided, it will be converted to empty widget
         (child-layout  (car (gleui-layout-node-children lnode)))
         (inner-w       (max 0 (- box-w pad-left pad-right)))
         (inner-h       (max 0 (- box-h pad-top pad-bottom))))
    ;; draw the outer box with its properties
    (gleui-draw-box! cr x y box-w box-h
                     #:radius radius
                     #:bg-color bg-color
                     #:border-color border-color
                     #:border-width border-width
                     #:border-edges border-edges
                     #:inset-border? inset-border?)
    (let-values (((offset-x offset-y)
                  (gleui-widget-aligned-offset align inner-w inner-h
                                               (gleui-layout-node-width child-layout)
                                               (gleui-layout-node-height child-layout))))
      (let ((child-x (+ x pad-left offset-x))
            (child-y (+ y pad-top offset-y)))
        (if clip?
            (begin
              ;; if clip is enabled, clip the child to box bounds (rounded when radius > 0)
              (cairo-save cr)
              (if (> radius 0)
                  (gleui-draw-rounded-rect cr x y box-w box-h radius)
                  (cairo-rectangle cr x y box-w box-h))
              (cairo-clip cr)
              (gleui-widget-paint-node! cr child-layout child-x child-y push-hit!)
              (cairo-restore cr))
            ;; if clip isn't enabled, just paint the child directly
            (gleui-widget-paint-node! cr child-layout child-x child-y push-hit!))))))

(gleui-register-widget! 'box %layout-box %paint-box!)
