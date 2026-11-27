;;; gliver/contrib/ui/gleui/widgets/stack.scm --- Stack Widget
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module defines the overlay stack widget for gleui.
;;; a stacking widget will show widgets on top of each other
;;; it's not used so far, a good use case will be to stack
;;; two elements on top of each other, align 1st child to left
;;; and second child to right (kind of blend effect).

(define-module (gliver contrib ui gleui widgets stack)
  #:use-module (gliver contrib ui gleui widgets engine)
  #:declarative? #f
  #:export (
            gleui-w-stack
))

(define (gleui-w-stack children)
  "Create overlay of CHILDREN back-to-front within the same box.
Parameters: (CHILDREN)
  - CHILDREN: List of <gleui-widget> nodes stacked in draw order.
Returns <gleui-widget> node."
  (gleui-make-widget 'stack
                     '()
                     (gleui-widget-compact-children children)))

(define (%layout-stack cr widget avail-w avail-h tight-w? tight-h?)
  "Layout processor for overlay stack WIDGET.
Parameters: (CR WIDGET AVAIL-W AVAIL-H TIGHT-W? TIGHT-H?)
  - CR: Cairo context used for measurement.
  - WIDGET: <gleui-widget> node.
  - AVAIL-W: Float or #f available width constraint in pixels.
  - AVAIL-H: Float or #f available height constraint in pixels.
  - TIGHT-W?: Boolean expanding to AVAIL-W if #t.
  - TIGHT-H?: Boolean expanding to AVAIL-H if #t.
Returns resolved <gleui-layout-node>."
  (let* ((children      (gleui-widget-children widget))
         ;; lay out every child with the stack's constraints
         (child-layouts (map (lambda (child)
                               (gleui-widget-layout cr child avail-w avail-h tight-w? tight-h?))
                             children))
         ;; stack wraps the largest child on each axis unless forced tight
         (max-w         (apply max 0 (map gleui-layout-node-width child-layouts)))
         (max-h         (apply max 0 (map gleui-layout-node-height child-layouts)))
         (final-w       (if (and tight-w? avail-w) (max avail-w max-w) max-w))
         (final-h       (if (and tight-h? avail-h) (max avail-h max-h) max-h)))
    (gleui-make-layout-node widget final-w final-h #f child-layouts)))

(define (%paint-stack! cr lnode x y push-hit!)
  "Paint overlay stack layout node LNODE children onto CR at (X, Y)
Parameters: (CR LNODE X Y PUSH-HIT!)
  - CR: Cairo context.
  - LNODE: <gleui-layout-node> node to paint.
  - X: Float left surface coordinate in pixels.
  - Y: Float top surface coordinate in pixels.
  - PUSH-HIT!: Procedure collecting <gleui-hit-box> records during painting."
  ;; paint each child in order at the same origin so later items overlap earlier ones
  (for-each (lambda (child-layout) (gleui-widget-paint-node! cr child-layout x y push-hit!))
            (gleui-layout-node-children lnode)))

(gleui-register-widget! 'stack %layout-stack %paint-stack!)
