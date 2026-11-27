;;; gliver/contrib/ui/gleui/widgets/align.scm --- Alignment Widgets
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module defines single-child alignment widgets for gleui.

(define-module (gliver contrib ui gleui widgets align)
  #:use-module (srfi srfi-11)
  #:use-module (gliver contrib ui gleui widgets engine)
  #:declarative? #f
  #:export (
            gleui-w-align
            gleui-w-center
))

(define* (gleui-w-align child #:key (align 'center) (width #f) (height #f))
  "Align CHILD inside available space or WIDTH and HEIGHT according to ALIGN.
Parameters: (CHILD [#:ALIGN 'center] [#:WIDTH #f] [#:HEIGHT #f])
  - CHILD: <gleui-widget> node or #f (empty spacer).
  - ALIGN: Symbol or integer (1-9 numpad) nine-point alignment (e.g. 'center, 5, 'top-left, 7).
  - WIDTH: Integer or #f fixed outer width in pixels.
  - HEIGHT: Integer or #f fixed outer height in pixels.
Returns <gleui-widget> node."
  (gleui-make-widget 'align
                     `((align . ,align)
                       (width . ,width)
                       (height . ,height))
                     child))

(define (gleui-w-center child)
  "Center CHILD within available parent bounds.
NOTE: center is really nothing but a shortcut to align.
but it's quit convenient to have as it's a common pattern.
Parameters: (CHILD)
  - CHILD: <gleui-widget> node or #f (empty spacer).
Returns <gleui-widget> align node."
  (gleui-w-align child #:align 'center))

(define (%layout-align cr widget avail-w avail-h tight-w? tight-h?)
  "Layout processor for align WIDGET.
Parameters: (CR WIDGET AVAIL-W AVAIL-H TIGHT-W? TIGHT-H?)
  - CR: Cairo context used for measurement.
  - WIDGET: <gleui-widget> node.
  - AVAIL-W: Float or #f available width constraint in pixels.
  - AVAIL-H: Float or #f available height constraint in pixels.
  - TIGHT-W?: Boolean expanding to AVAIL-W if #t.
  - TIGHT-H?: Boolean expanding to AVAIL-H if #t.
Returns resolved <gleui-layout-node>."
  (let* ((props        (gleui-widget-props widget))
         (child        (gleui-widget-children widget))
         (explicit-w   (gleui-widget-prop props 'width))
         (explicit-h   (gleui-widget-prop props 'height))
         ;; fall back to parent bounds when explicit width/height aren't fixed
         (bound-w      (or explicit-w avail-w))
         (bound-h      (or explicit-h avail-h))
         ;; lay out the child loosely so it keeps its natural size
         (child-layout (gleui-widget-layout cr child bound-w bound-h #f #f))
         ;; use explicit size, else tight parent size, else child's size
         (final-w      (or explicit-w
                           (and tight-w? avail-w)
                           (gleui-layout-node-width child-layout)))
         (final-h      (or explicit-h
                           (and tight-h? avail-h)
                           (gleui-layout-node-height child-layout))))
    (gleui-make-layout-node widget final-w final-h #f (list child-layout))))

(define (%paint-align! cr lnode x y push-hit!)
  "Paint align layout node LNODE onto CR at (X, Y)
Parameters: (CR LNODE X Y PUSH-HIT!)
  - CR: Cairo context.
  - LNODE: <gleui-layout-node> node to paint.
  - X: Float left surface coordinate in pixels.
  - Y: Float top surface coordinate in pixels.
  - PUSH-HIT!: Procedure collecting <gleui-hit-box> records during painting."
  (let* ((widget       (gleui-layout-node-widget lnode))
         (props        (gleui-widget-props widget))
         ;; align will always have a single child (#f becomes empty widget)
         (child-layout (car (gleui-layout-node-children lnode)))
         (align        (gleui-widget-prop props 'align)))
    ;; compute the child's offset inside lnode bounds and paint it there
    (let-values (((offset-x offset-y)
                  (gleui-widget-aligned-offset
                   align
                   (gleui-layout-node-width lnode)
                   (gleui-layout-node-height lnode)
                   (gleui-layout-node-width child-layout)
                   (gleui-layout-node-height child-layout))))
      (gleui-widget-paint-node! cr child-layout (+ x offset-x) (+ y offset-y) push-hit!))))

(gleui-register-widget! 'align %layout-align %paint-align!)
