;;; gliver/contrib/ui/gleui/widgets/clickable.scm --- Clickable Widget
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module defines the clickable pointer widget for gleui.

(define-module (gliver contrib ui gleui widgets clickable)
  #:use-module (gliver contrib ui gleui widgets engine)
  #:declarative? #f
  #:export (
            gleui-w-clickable
))

(define* (gleui-w-clickable child #:key (on-click #f) (on-scroll #f)
                            (on-enter #f) (on-leave #f) (data #f))
  "Create a clickable region that wraps CHILD.
Parameters: (CHILD [#:ON-CLICK #f] [#:ON-SCROLL #f] [#:ON-ENTER #f] [#:ON-LEAVE #f] [#:DATA #f])
  - CHILD: <gleui-widget> node or #f (empty spacer).
  - ON-CLICK: Procedure or #f callback invoked as (ON-CLICK BUTTON PX PY DATA).
  - ON-SCROLL: Procedure or #f callback invoked as (ON-SCROLL AXIS DELTA DATA).
  - ON-ENTER: Procedure or #f callback invoked as (ON-ENTER PX PY DATA).
  - ON-LEAVE: Procedure or #f callback invoked as (ON-LEAVE DATA).
  - DATA: Value component payload forwarded to pointer callbacks.
Returns <gleui-widget> node."
  (gleui-make-widget 'clickable
                     `((on-click . ,on-click)
                       (on-scroll . ,on-scroll)
                       (on-enter . ,on-enter)
                       (on-leave . ,on-leave)
                       (data . ,data))
                     child))

(define (%layout-clickable cr widget avail-w avail-h tight-w? tight-h?)
  "Layout processor for clickable wrapper WIDGET.
Parameters: (CR WIDGET AVAIL-W AVAIL-H TIGHT-W? TIGHT-H?)
  - CR: Cairo context used for measurement.
  - WIDGET: <gleui-widget> node.
  - AVAIL-W: Float or #f available width constraint in pixels.
  - AVAIL-H: Float or #f available height constraint in pixels.
  - TIGHT-W?: Boolean expanding to AVAIL-W if #t.
  - TIGHT-H?: Boolean expanding to AVAIL-H if #t.
Returns resolved <gleui-layout-node>."
  (let ((child-layout (gleui-widget-layout cr (gleui-widget-children widget)
                                           avail-w avail-h tight-w? tight-h?)))
    (gleui-make-layout-node widget
                            (gleui-layout-node-width child-layout)
                            (gleui-layout-node-height child-layout)
                            #f
                            (list child-layout))))

(define (%paint-clickable! cr lnode x y push-hit!)
  "Paint clickable layout node LNODE child onto CR at (X, Y)
Parameters: (CR LNODE X Y PUSH-HIT!)
  - CR: Cairo context.
  - LNODE: <gleui-layout-node> node to paint.
  - X: Float left surface coordinate in pixels.
  - Y: Float top surface coordinate in pixels.
  - PUSH-HIT!: Procedure collecting <gleui-hit-box> records during painting."
  (let* ((widget       (gleui-layout-node-widget lnode))
         (props        (gleui-widget-props widget))
         ;; clickable always holds a single child layout (#f becomes empty widget)
         (child-layout (car (gleui-layout-node-children lnode)))
         (box-width    (gleui-layout-node-width lnode))
         (box-height   (gleui-layout-node-height lnode)))
    ;; push this hit-box before painting the child so nested hit-boxes have higher priority
    (push-hit! (gleui-make-hit-box x y (+ x box-width) (+ y box-height)
                                   (gleui-widget-prop props 'data)
                                   (gleui-widget-prop props 'on-click)
                                   (gleui-widget-prop props 'on-scroll)
                                   (gleui-widget-prop props 'on-enter)
                                   (gleui-widget-prop props 'on-leave)))
    (gleui-widget-paint-node! cr child-layout x y push-hit!)))

(gleui-register-widget! 'clickable %layout-clickable %paint-clickable!)
