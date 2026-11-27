;;; gliver/contrib/ui/gleui/widgets/flex.scm --- Flex Row, Column & Expanded Widgets
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module defines horizontal and vertical flex layout widgets for gleui.
;;;
;;; This module uses flutter terms to define the layout 'flex', it's important
;;; to have a little understanding of the purpose of these components as this
;;; will help in understanding the code itself.
;;; row: sticks children horizontally left to right
;;; column: stacks children vertically top to bottom
;;; main-axis: vertical in column, horizontal in row
;;; cross-axis: horizontal in column, vertical in row
;;; main-align: how to align children in main axis 'start, 'center, 'end, 'space-between
;;; cross-align: how to align children in cross axis 'start, 'center, 'end, 'space-between
;;; expanded: stretches its height/width to fill the remaining space in row/colun.
;;; flex: weight for expanded, if you have multiple expanded in a row, you can assign
;;;       them #:flex 1 and #:flex 2. You can think of it as in this case allocating
;;;       equal space for 3 components, #:flex 1 receives space 1/3, #:flex 2 receives 2/3
;;; NOTE: this is one of the most flexible and important set of widgets in gleui
;;;       understanding these components will be very rewarding when gui is to be built.

(define-module (gliver contrib ui gleui widgets flex)
  #:use-module (srfi srfi-1)
  #:use-module (gliver contrib ui gleui widgets engine)
  #:declarative? #f
  #:export (
            gleui-w-row
            gleui-w-column
            gleui-w-expanded
))

(define* (gleui-w-row children #:key (gap 0) (main-align 'start) (cross-align 'start))
  "Create a horizontal row layout widget with CHILDREN.
Parameters: (CHILDREN [#:GAP 0] [#:MAIN-ALIGN 'start] [#:CROSS-ALIGN 'start])
  - CHILDREN: List of <gleui-widget> nodes (non-widget placeholders are skipped).
  - GAP: Integer spacing between adjacent children in pixels.
  - MAIN-ALIGN: Symbol main-axis alignment ('start, 'center, 'end, or 'space-between).
  - CROSS-ALIGN: Symbol cross-axis alignment ('start, 'center, 'end, or 'stretch).
Returns <gleui-widget> node."
  (gleui-make-widget 'row
                     `((gap . ,gap)
                       (main-align . ,main-align)
                       (cross-align . ,cross-align))
                     (gleui-widget-compact-children children)))

(define* (gleui-w-column children #:key (gap 0) (main-align 'start) (cross-align 'start))
  "Create a vertical column layout widget with CHILDREN.
Parameters: (CHILDREN [#:GAP 0] [#:MAIN-ALIGN 'start] [#:CROSS-ALIGN 'start])
  - CHILDREN: List of <gleui-widget> nodes (non-widget placeholders are skipped).
  - GAP: Integer vertical spacing between adjacent children in pixels.
  - MAIN-ALIGN: Symbol main-axis alignment ('start, 'center, 'end, or 'space-between).
  - CROSS-ALIGN: Symbol cross-axis alignment ('start, 'center, 'end, or 'stretch).
Returns <gleui-widget> node."
  (gleui-make-widget 'column
                     `((gap . ,gap)
                       (main-align . ,main-align)
                       (cross-align . ,cross-align))
                     (gleui-widget-compact-children children)))

(define* (gleui-w-expanded child #:key (flex 1))
  "Expand CHILD so proportionally occupies space based on weight FLEX.
Parameters: (CHILD [#:FLEX 1])
  - CHILD: <gleui-widget> node or #f (empty spacer).
  - FLEX: Float positive weight controlling share of remaining space.
Returns <gleui-widget> node."
  (gleui-make-widget 'expanded
                     `((flex . ,flex))
                     child))

(define (%flex-total-weight children)
  "Sum the flex weights of all expanded widgets in CHILDREN.
Parameters: (CHILDREN)
  - CHILDREN: List of <gleui-widget> nodes.
Returns float total flex weight."
  (fold (lambda (child total)
          (if (eq? (gleui-widget-kind child) 'expanded)
              (let ((child-props (gleui-widget-props child)))
                (+ total (gleui-widget-prop child-props 'flex)))
              total))
        0 children))

(define (%layout-flex cr widget avail-w avail-h tight-w? tight-h? horizontal?)
  "Layout processor for both row or column WIDGET along.
Parameters: (CR WIDGET AVAIL-W AVAIL-H TIGHT-W? TIGHT-H? HORIZONTAL?)
  - CR: Cairo context used for measurement.
  - WIDGET: <gleui-widget> node.
  - AVAIL-W: Float or #f available width constraint in pixels.
  - AVAIL-H: Float or #f available height constraint in pixels.
  - TIGHT-W?: Boolean expanding to AVAIL-W if #t.
  - TIGHT-H?: Boolean expanding to AVAIL-H if #t.
  - HORIZONTAL?: Boolean using horizontal row when #t, vertical column when #f.
Returns resolved <gleui-layout-node>."
  (let* ((props         (gleui-widget-props widget))
         (children      (gleui-widget-children widget))
         (gap           (gleui-widget-prop props 'gap))
         (main-align    (gleui-widget-prop props 'main-align))
         (cross-align   (gleui-widget-prop props 'cross-align))
         ;; normalize width/height into main and cross axes so
         ;; row and column share one algorithm (opposite w and h)
         (avail-main    (if horizontal? avail-w avail-h))
         (avail-cross   (if horizontal? avail-h avail-w))
         (tight-main?   (if horizontal? tight-w? tight-h?))
         (tight-cross?  (if horizontal? tight-h? tight-w?))
         (stretch?      (eq? cross-align 'stretch))
         (child-count   (length children))
         ;; fixed pixel sum of gaps placed between adjacent children
         ;; of course the number of gaps to be counted is children - 1
         ;; as this gap exists only between children. (0 gap for 0 children)
         (total-gaps    (max (* (1- child-count) gap) 0))
         (has-flex?     (any (lambda (child) (eq? (gleui-widget-kind child) 'expanded)) children))
         ;; accessors reading the main-axis or cross-axis size from a child layout node
         (node-main     (lambda (layout-node)
                          (if horizontal?
                              (gleui-layout-node-width layout-node)
                              (gleui-layout-node-height layout-node))))
         (node-cross    (lambda (layout-node)
                          (if horizontal?
                              (gleui-layout-node-height layout-node)
                              (gleui-layout-node-width layout-node))))
         (expand-cross? (or stretch? tight-cross?))
         ;; store child layout nodes in a vector indexed by position so step 1 and step 2
         ;; can fill fixed and expanded slots out of order while keeping original child order
         (slots         (make-vector child-count #f)))

    ;; layout CHILD with main/cross constraints mapped to width/height
    (define (%layout-child child main-avail child-tight-main?)
      (let ((child-tight-cross? (and stretch? tight-cross?)))
        (if horizontal?
            (gleui-widget-layout cr child main-avail avail-cross child-tight-main? child-tight-cross?)
            (gleui-widget-layout cr child avail-cross main-avail child-tight-cross? child-tight-main?))))

    ;; step 1: measure all non-expanded children first and accumulate their main-axis size
    ;;   - idx: index of current child
    ;;   - remaining: remaining children to visit
    ;;   - used-main: cumulative main-axis pixels consumed so far
    (define (%measure-fixed!)
      (let loop ((idx 0) (remaining children) (used-main 0))
        (if (null? remaining)
            used-main
            (let ((child (car remaining)))
              ;; skip expanded wrappers in step 1, they will be handled in step 2
              ;; this is because we don't know at this point what will be the remaining
              ;; space, that's why we have to compute sizes in 2 steps
              (if (eq? (gleui-widget-kind child) 'expanded)
                  ;; we will just skip for expanded
                  (loop (1+ idx) (cdr remaining) used-main)
                  ;; when flex children exist, allow fixed children up to avail-main so
                  ;; remaining space can be computed afterward, otherwise cap to remaining
                  (let* ((max-main     (and avail-main
                                            (if has-flex?
                                                avail-main
                                                (max 0 (- avail-main used-main total-gaps)))))
                         (child-layout (%layout-child child max-main #f)))
                    ;; store child layout in slots[idx] and add its main size to used-main
                    (vector-set! slots idx child-layout)
                    (loop (1+ idx) (cdr remaining) (+ used-main (node-main child-layout)))))))))

    ;; step 2: split remaining main-axis space proportionally across skipped expanded children
    (define (%distribute-expanded! fixed-main)
      (let ((total-flex     (%flex-total-weight children))
            ;; remaining main-axis space after fixed children and gaps (#f if unbounded)
            (remaining-main (and avail-main (max 0 (- avail-main fixed-main total-gaps)))))
        ;; revisit children with index idx to fill the #f expanded slots left by step 1
        (let loop ((idx 0) (remaining children))
          (when (pair? remaining)
            (let ((child (car remaining)))
              ;; remaining should be always expanded
              (when (eq? (gleui-widget-kind child) 'expanded)
                (let* ((child-props       (gleui-widget-props child))
                       (flex-weight       (gleui-widget-prop child-props 'flex))
                       (inner-child       (gleui-widget-children child))
                       ;; proportional share of remaining main-axis space (#f if unbounded)
                       (slot-main         (and remaining-main (> total-flex 0)
                                               (/ (* remaining-main flex-weight) total-flex)))
                       ;; layout the wrapped child tightly along main axis to slot-main when bounded
                       (child-layout      (%layout-child inner-child slot-main (and slot-main #t)))
                       ;; fall back to the child's own main size when avail-main is unbounded
                       (final-child-main  (or slot-main (node-main child-layout)))
                       (final-child-cross (node-cross child-layout))
                       ;; wrap in an expanded layout node matching the allocated slot size
                       (expanded-layout   (gleui-make-layout-node
                                           child
                                           (if horizontal? final-child-main final-child-cross)
                                           (if horizontal? final-child-cross final-child-main)
                                           #f
                                           (list child-layout))))
                  ;; place into the matching slot so original child order is preserved
                  (vector-set! slots idx expanded-layout))))
            (loop (1+ idx) (cdr remaining))))))

    (let ((fixed-main (%measure-fixed!)))
      (when has-flex?
        (%distribute-expanded! fixed-main))
      ;; compute final container dimensions from all populated slots
      (let* ((child-layouts (vector->list slots))
             (sum-main      (fold + 0 (map node-main child-layouts)))
             (content-main  (+ sum-main total-gaps))
             (max-cross     (apply max 0 (map node-cross child-layouts)))
             ;; expand main axis when tight, holding flex children, or alignment needs extra space
             (expand-main?  (and avail-main
                                 (or tight-main? has-flex?
                                     (memq main-align '(center end space-between)))))
             (final-main    (if expand-main? (max avail-main content-main) content-main))
             (final-cross   (if (and expand-cross? avail-cross)
                                (max avail-cross max-cross)
                                max-cross)))
        ;; convert main/cross back to width/height and cache sums for painting
        (gleui-make-layout-node
         widget
         (if horizontal? final-main final-cross)
         (if horizontal? final-cross final-main)
         (cons sum-main total-gaps)
         child-layouts)))))

(define (%paint-flex! cr lnode x y push-hit! horizontal?)
  "Paint row or column flex layout node LNODE onto CR.
Parameters: (CR LNODE X Y PUSH-HIT! HORIZONTAL?)
  - CR: Cairo context.
  - LNODE: <gleui-layout-node> node to paint.
  - X: Float left surface coordinate in pixels.
  - Y: Float top surface coordinate in pixels.
  - PUSH-HIT!: Procedure collecting <gleui-hit-box> records during painting.
  - HORIZONTAL?: Boolean painting row layout when #t, column layout when #f."
  (let* ((widget        (gleui-layout-node-widget lnode))
         (props         (gleui-widget-props widget))
         (gap           (gleui-widget-prop props 'gap))
         (main-align    (gleui-widget-prop props 'main-align))
         (cross-align   (gleui-widget-prop props 'cross-align))
         (box-w         (gleui-layout-node-width lnode))
         (box-h         (gleui-layout-node-height lnode))
         (box-main      (if horizontal? box-w box-h))
         (box-cross     (if horizontal? box-h box-w))
         (sums          (gleui-layout-node-data lnode))
         (sum-main      (car sums))
         (total-gaps    (cdr sums))
         (child-layouts (gleui-layout-node-children lnode))
         (child-count   (length child-layouts))
         ;; extra main-axis space gets distributed by start or step-gap according to main-align
         (extra-main    (max 0 (- box-main sum-main total-gaps)))
         (start         (case main-align
                          ((center) (/ extra-main 2.0))
                          ((end)    extra-main)
                          (else     0.0)))
         (step-gap      (if (and (eq? main-align 'space-between) (> child-count 1))
                            (+ gap (/ extra-main (1- child-count)))
                            gap)))

    ;; loop children sequentially, increasing current by each child's size + gap
    ;; start is the coordinate to start drawing, current is where we current at in pixels
    (let loop ((remaining child-layouts) (current start))
      (when (pair? remaining)
        (let* ((child        (car remaining))
               (child-main   (if horizontal?
                                 (gleui-layout-node-width child)
                                 (gleui-layout-node-height child)))
               (child-cross  (if horizontal?
                                 (gleui-layout-node-height child)
                                 (gleui-layout-node-width child)))
               (cross-offset (case cross-align
                               ((center) (/ (- box-cross child-cross) 2.0))
                               ((end)    (- box-cross child-cross))
                               (else     0.0)))
               (child-x      (if horizontal? (+ x current) (+ x cross-offset)))
               (child-y      (if horizontal? (+ y cross-offset) (+ y current))))
          (gleui-widget-paint-node! cr child child-x child-y push-hit!)
          (loop (cdr remaining) (+ current child-main step-gap)))))))

(define (%layout-expanded cr widget avail-w avail-h tight-w? tight-h?)
  "Layout processor for expanded wrapper WIDGET.
Parameters: (CR WIDGET AVAIL-W AVAIL-H TIGHT-W? TIGHT-H?)
  - CR: Cairo context used for measurement.
  - WIDGET: <gleui-widget> node.
  - AVAIL-W: Float or #f available width constraint in pixels.
  - AVAIL-H: Float or #f available height constraint in pixels.
  - TIGHT-W?: Boolean expanding to AVAIL-W if #t.
  - TIGHT-H?: Boolean expanding to AVAIL-H if #t.
Returns resolved <gleui-layout-node>."
  ;; when used outside a flex container, layout the wrapped child directly
  (gleui-widget-layout cr (gleui-widget-children widget)
                       avail-w avail-h tight-w? tight-h?))

(define (%paint-expanded! cr lnode x y push-hit!)
  "Paint expanded layout node LNODE onto CR.
Parameters: (CR LNODE X Y PUSH-HIT!)
  - CR: Cairo context.
  - LNODE: <gleui-layout-node> node to paint.
  - X: Float left surface coordinate in pixels.
  - Y: Float top surface coordinate in pixels.
  - PUSH-HIT!: Procedure collecting <gleui-hit-box> records during painting."
  ;; expanded inside a flex container always holds a single wrapped child layout
  (gleui-widget-paint-node! cr (car (gleui-layout-node-children lnode))
                            x y push-hit!))

;; NOTE: both row and column use the same paint and layout handlers
;; it's just a matter of few tweaks applied based on the horizontal? variable
(gleui-register-widget!
 'row
 (lambda (cr widget avail-w avail-h tight-w? tight-h?)
   (%layout-flex cr widget avail-w avail-h tight-w? tight-h? #t))
 (lambda (cr lnode x y push-hit!)
   (%paint-flex! cr lnode x y push-hit! #t)))

(gleui-register-widget!
 'column
 (lambda (cr widget avail-w avail-h tight-w? tight-h?)
   (%layout-flex cr widget avail-w avail-h tight-w? tight-h? #f))
 (lambda (cr lnode x y push-hit!)
   (%paint-flex! cr lnode x y push-hit! #f)))

(gleui-register-widget! 'expanded %layout-expanded %paint-expanded!)
