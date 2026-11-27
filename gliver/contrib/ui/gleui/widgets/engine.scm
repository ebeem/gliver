;;; gliver/contrib/ui/gleui/widgets/engine.scm --- Core gleui Widget Engine
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module defines the core records, widget handler registry, property
;;; helpers, and layout processor and Cairo painter dispatchers for gleui.
;;; It handles:
;;;  - Widget and layout-node record types
;;;  - Widget layout processor and Cairo painter registration
;;;  - Providing empty spacer widget
;;;  - Providing custom widget
;;;
;;; NOTE: this engine and components are heavily inspired by flutter and its
;;; design system (including the widgets and their names).
;;; It's worth noting that the system is designed to have 2 stages
;;; stage-1 (layout processor): is producing layout from a widget
;;; stage-2 (cairo painter): is rendering from produced layout

(define-module (gliver contrib ui gleui widgets engine)
  #:use-module (cairo)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (gliver core helpers)
  #:use-module (gliver contrib ui gleui base draw)
  #:use-module (gliver contrib ui gleui base shm)
  #:use-module (gliver contrib ui gleui base pointer)
  #:declarative? #f
  #:re-export (<gleui-hit-box>
               gleui-make-hit-box
               gleui-hit-box?
               gleui-hit-box-x1
               gleui-hit-box-y1
               gleui-hit-box-x2
               gleui-hit-box-y2
               gleui-hit-box-data
               gleui-hit-box-on-click
               gleui-hit-box-on-scroll
               gleui-hit-box-on-enter
               gleui-hit-box-on-leave
               gleui-find-hit-boxes
               gleui-find-hit-box)
  #:export (
            <gleui-widget>
            gleui-make-widget
            gleui-widget?
            gleui-widget-kind
            gleui-widget-props
            gleui-widget-children
            <gleui-layout-node>
            gleui-make-layout-node
            gleui-layout-node?
            gleui-layout-node-widget
            gleui-layout-node-width
            gleui-layout-node-height
            gleui-layout-node-data
            gleui-layout-node-children
            gleui-register-widget!
            gleui-widget-compact-children
            gleui-widget-prop
            gleui-widget-clamp-dim
            gleui-widget-resolve-padding
            gleui-widget-aligned-offset
            gleui-w-empty
            gleui-w-custom
            gleui-widget-layout
            gleui-widget-measure
            gleui-widget-paint-node!
            gleui-widget-paint!
            gleui-widget-render!
            gleui-widget-render-commit!
))

;; immutable widget node record, all widgets will be created
;; using this record passing all their props and children
;;  - kind: symbol widget type ('empty, 'box, 'row, 'column, 'expanded, etc)
;;  - props: association list of widget properties like: styling, constraints, and callbacks
;;  - children: single child widget, list of child widgets, or #f for leaf widgets
(define-record-type <gleui-widget>
  (gleui-make-widget kind props children)
  gleui-widget?
  (kind     gleui-widget-kind)
  (props    gleui-widget-props)
  (children gleui-widget-children))

;; computed layout node record produced by layout processor
;; this is created by performing calculations on <gleui-widget> records
;;  - widget: source <gleui-widget> record represented
;;  - width: resolved width in pixels
;;  - height: resolved height in pixels
;;  - data: kind-specific layout payload
;;  - children: list of child <gleui-layout-node> records or #f
(define-record-type <gleui-layout-node>
  (gleui-make-layout-node widget width height data children)
  gleui-layout-node?
  (widget   gleui-layout-node-widget)
  (width    gleui-layout-node-width)
  (height   gleui-layout-node-height)
  (data     gleui-layout-node-data)
  (children gleui-layout-node-children))

(define-var %layout-handlers (make-hash-table)
            "Mapping widget kind symbol to layout (measurement calculation) handler.")
(define-var %paint-handlers  (make-hash-table)
            "Mapping widget kind symbol to rendering handler.")

(define (gleui-register-widget! kind layout-proc paint-proc)
  "Register LAYOUT-PROC and PAINT-PROC handlers for widget KIND.
Parameters: (KIND LAYOUT-PROC PAINT-PROC)
  - KIND: Symbol widget kind.
  - LAYOUT-PROC: Procedure or #f accepting (CR WIDGET AVAIL-W AVAIL-H TIGHT-W? TIGHT-H?).
  - PAINT-PROC: Procedure or #f accepting (CR LNODE X Y PUSH-HIT!), or #f for no-op painting."
  (when (procedure? layout-proc)
    (hashq-set! %layout-handlers kind layout-proc))
  (when (procedure? paint-proc)
    (hashq-set! %paint-handlers kind paint-proc)))

(define (gleui-widget-compact-children lst)
  "Filter non-widget placeholders in children LST.
Parameters: (LST)
  - LST: List containing <gleui-widget> nodes.
Returns list containing only <gleui-widget> elements."
  ;; NOTE: this is introduced to make it more convenient to simply
  ;; add a #f or other non-widget elements in children.
  ;; it's not recommended to do so, but it's more elegant
  ;; to simply place an (if cond widget #f) in a children list
  (filter gleui-widget? (if (list? lst) lst '())))

(define (gleui-widget-prop props key)
  "Look up KEY in widget alist PROPS.
Parameters: (PROPS KEY)
  - PROPS: Alist of widget properties.
  - KEY: Symbol property key.
Returns associated value, or #f if KEY is absent."
  ;; NOTE: this actually was doing more than simple fetching
  ;; but after refactoring, it's simply referencing the property
  ;; I am keeping this function in case more logic has to be
  ;; implemented later to fetch property from list
  (assq-ref props key))

;; NOTE: these helper functions are expected to be common
;; so even if some are only used by a single widget, it's still worth
;; it to share them so future widgets can reuse them
(define (gleui-widget-clamp-dim val min-v max-v)
  "Clamp numeric dimension VAL between optional MIN-V and positive MAX-V bounds.
The value must be >= 0, so passing a negative value will return 0.
Parameters: (VAL MIN-V MAX-V)
  - VAL: Float unclamped pixel dimension.
  - MIN-V: Float or #f minimum bound (treated as 0 when #f).
  - MAX-V: Float or #f positive maximum bound (unbounded when #f).
Returns float clamped dimension."
  (let* ((min-bound (if (and (number? min-v) (> min-v 0)) min-v 0))
         (max-bound (and (number? max-v) (> max-v 0) max-v))
         (with-min  (max val min-bound)))
    (if max-bound
        (min with-min max-bound)
        with-min)))

(define (gleui-widget-resolve-padding props)
  "Compute padding properties in PROPS into four per-edge pixel values.
Parameters: (PROPS)
  - PROPS: Alist of box widget properties.
Returns four values: TOP, RIGHT, BOTTOM, and LEFT padding in pixels."
  (let ((p (gleui-widget-prop props 'padding)))
    (let-values (((bt br bb bl)
                  (cond
                   ;; if padding is one number, return it from all sides
                   ((number? p) (values p p p p))
                   ;; if padding is (x y) return (y x y x)
                   ((and (list? p) (= (length p) 2))
                    (values (cadr p) (car p) (cadr p) (car p)))
                   ;; if padding is (top right bottom left), return it as is
                   ((and (list? p) (= (length p) 4))
                    (values (car p) (cadr p) (caddr p) (cadddr p)))
                   ;; anything else is 0 padding
                   (else (values 0 0 0 0)))))
      ;; we will also get padding-x and padding-y which will overwrite padding
      (let* ((px (gleui-widget-prop props 'padding-x))
             (py (gleui-widget-prop props 'padding-y))
             ;; also get padding-{DIR} which will overwrite padding and padding-x/y
             (pt (or (gleui-widget-prop props 'padding-top) py bt))
             (pr (or (gleui-widget-prop props 'padding-right) px br))
             (pb (or (gleui-widget-prop props 'padding-bottom) py bb))
             (pl (or (gleui-widget-prop props 'padding-left) px bl)))
        (values pt pr pb pl)))))

(define (gleui-widget-aligned-offset align avail-w avail-h child-w child-h)
  "Compute ALIGN offset of CHILD-W by CHILD-H inside AVAIL-W by AVAIL-H container bounds.
Parameters: (ALIGN AVAIL-W AVAIL-H CHILD-W CHILD-H)
  - ALIGN: Integer gleui numpad alignment or symbol.
  - AVAIL-W: Float container inner width in pixels.
  - AVAIL-H: Float container inner height in pixels.
  - CHILD-W: Float child layout width in pixels.
  - CHILD-H: Float child layout height in pixels.
Returns two float values: DX and DY offsets in pixels."
  (let* ((dx-mid (/ (- avail-w child-w) 2.0))
         (dx-end (- avail-w child-w))
         (dy-mid (/ (- avail-h child-h) 2.0))
         (dy-end (- avail-h child-h)))
    (case align
      ((top-left      7) (values 0.0 0.0))
      ((top-center    8) (values dx-mid 0.0))
      ((top-right     9) (values dx-end 0.0))
      ((center-left   4) (values 0.0 dy-mid))
      ((center        5) (values dx-mid dy-mid))
      ((center-right  6) (values dx-end dy-mid))
      ((bottom-left   1) (values 0.0 dy-end))
      ((bottom-center 2) (values dx-mid dy-end))
      ((bottom-right  3) (values dx-end dy-end))
      (else              (values 0.0 0.0)))))

;; this is a very simple sample widget to be created using the gleui engine
;; you may of course use the quasiquote/unquote syntax here
;; I personally feel it's much easier to read a typical list
;; however, I use quasiquote/unquote usually because it's more compact
(define* (gleui-w-empty #:key (width 0) (height 0))
  "Create an empty spacer widget of WIDTH by HEIGHT pixels.
Parameters: ([#:WIDTH 0] [#:HEIGHT 0])
  - WIDTH: Integer fixed horizontal spacer in pixels.
  - HEIGHT: Integer fixed vertical spacer in pixels.
Returns <gleui-widget> node."
  (gleui-make-widget 'empty
                     `((width . ,width)
                       (height . ,height))
                     #f))

(define (%layout-empty cr widget avail-w avail-h tight-w? tight-h?)
  "Layout processor for empty spacer WIDGET.
Parameters: (CR WIDGET AVAIL-W AVAIL-H TIGHT-W? TIGHT-H?)
  - CR: Cairo context used for measurement.
  - WIDGET: <gleui-widget> node.
  - AVAIL-W: Float or #f available width constraint in pixels.
  - AVAIL-H: Float or #f available height constraint in pixels.
  - TIGHT-W?: Boolean expanding to AVAIL-W if #t.
  - TIGHT-H?: Boolean expanding to AVAIL-H if #t.
Returns resolved <gleui-layout-node>."
  (let ((props (gleui-widget-props widget)))
    ;; this widget is a leaf and doesn't have custom data
    ;; just width and height taken directly from props
    (gleui-make-layout-node widget
                            (gleui-widget-prop props 'width)
                            (gleui-widget-prop props 'height)
                            #f
                            #f)))

;; register the empty widget with its symbol and layout processor
;; the last parameter is for painting, but there is nothing to paint here
;; example 20px height spacer: (gleui-w-empty #:width 0 #:height 20)
(gleui-register-widget! 'empty %layout-empty #f)

(define (gleui-w-custom layout-proc paint-proc)
  "Create a custom leaf widget with handlers for layout and painting.
NOTE: this is useful to draw custom cairo outputs directly to gleui.
Parameters: (LAYOUT-PROC PAINT-PROC)
  - LAYOUT-PROC: Procedure accepting (CR AVAIL-W AVAIL-H) returning (values W H).
  - PAINT-PROC: Procedure accepting (CR X Y WIDTH HEIGHT PUSH-HIT!).
Returns <gleui-widget> node."
  (gleui-make-widget 'custom
                     `((layout . ,layout-proc)
                       (paint  . ,paint-proc))
                     #f))

(define (%layout-custom cr widget avail-w avail-h tight-w? tight-h?)
  "Layout processor for custom widget.
Parameters: (CR WIDGET AVAIL-W AVAIL-H TIGHT-W? TIGHT-H?)
  - CR: Cairo context used for measurement.
  - WIDGET: <gleui-widget> node.
  - AVAIL-W: Float or #f available width constraint in pixels.
  - AVAIL-H: Float or #f available height constraint in pixels.
  - TIGHT-W?: Boolean expanding to AVAIL-W if #t.
  - TIGHT-H?: Boolean expanding to AVAIL-H if #t.
Returns resolved <gleui-layout-node>."
  (let* ((props (gleui-widget-props widget))
         (layout-proc (gleui-widget-prop props 'layout)))
    ;; guard check, layout-proc should always be passed
    (if (procedure? layout-proc)
        ;; compute width and height and pass it to layout
        (let-values (((width height) (layout-proc cr avail-w avail-h)))
          (gleui-make-layout-node widget width height #f #f))
        (gleui-make-layout-node widget 0 0 #f #f))))

(define (%paint-custom! cr lnode x y push-hit!)
  "Paint custom layout node LNODE onto CR at (X, Y).
Parameters: (CR LNODE X Y PUSH-HIT!)
  - CR: Cairo context.
  - LNODE: <gleui-layout-node> node to paint.
  - X: Float left surface coordinate in pixels.
  - Y: Float top surface coordinate in pixels.
  - PUSH-HIT!: Procedure collecting <gleui-hit-box> records during painting."
  (let* ((w           (gleui-layout-node-widget lnode))
         (props       (gleui-widget-props w))
         (paint-proc  (gleui-widget-prop props 'paint)))
    (when (procedure? paint-proc)
      (paint-proc cr x y
                  (gleui-layout-node-width lnode)
                  (gleui-layout-node-height lnode)
                  push-hit!))))

(gleui-register-widget! 'custom %layout-custom %paint-custom!)

(define (%layout-node cr widget avail-w avail-h tight-w? tight-h?)
  "Dispatch layout processor for WIDGET on CR.
Parameters: (CR WIDGET AVAIL-W AVAIL-H TIGHT-W? TIGHT-H?)
  - CR: Cairo context used for measurement.
  - WIDGET: <gleui-widget> node.
  - AVAIL-W: Float or #f available width constraint in pixels.
  - AVAIL-H: Float or #f available height constraint in pixels.
  - TIGHT-W?: Boolean expanding to AVAIL-W if #t.
  - TIGHT-H?: Boolean expanding to AVAIL-H if #t.
Returns resolved <gleui-layout-node>."
  (let* ((w       (if (gleui-widget? widget) widget (gleui-w-empty)))
         (kind    (gleui-widget-kind w))
         (handler (hashq-ref %layout-handlers kind)))
    (if handler
        (handler cr w avail-w avail-h tight-w? tight-h?)
        (gleui-make-layout-node w 0 0 #f #f))))

(define* (gleui-widget-layout cr widget #:optional (avail-w #f) (avail-h #f)
                              (tight-w? #f) (tight-h? #f))
  "Produce layout node for WIDGET under CR.
Parameters: (CR WIDGET [AVAIL-W #f] [AVAIL-H #f] [TIGHT-W? #f] [TIGHT-H? #f])
  - CR: Cairo context used for measurement.
  - WIDGET: <gleui-widget> node.
  - AVAIL-W: Float or #f available width constraint in pixels.
  - AVAIL-H: Float or #f available height constraint in pixels.
  - TIGHT-W?: Boolean expanding to AVAIL-W if #t.
  - TIGHT-H?: Boolean expanding to AVAIL-H if #t.
Returns resolved <gleui-layout-node>."
  (%layout-node cr widget avail-w avail-h tight-w? tight-h?))

(define* (gleui-widget-measure widget #:optional (avail-w #f) (avail-h #f) (cr #f))
  "Measure WIDGET dimensions using CR or a temporary Cairo image surface.
This is useful when the dimensions are needed when a cairo context isn't available.
Parameters: (WIDGET [AVAIL-W #f] [AVAIL-H #f] [CR #f])
  - WIDGET: <gleui-widget> node.
  - AVAIL-W: Float or #f available width constraint in pixels.
  - AVAIL-H: Float or #f available height constraint in pixels.
  - CR: Cairo context or #f (creates a temporary 1x1 surface when #f).
Returns two values: WIDTH and HEIGHT in pixels."
  (let ((measure (lambda (ctx)
                   (let ((ln (%layout-node ctx widget avail-w avail-h #f #f)))
                     (values (gleui-layout-node-width ln)
                             (gleui-layout-node-height ln))))))
    (if cr
        (measure cr)
        (let* ((surf   (cairo-image-surface-create 'argb32 1 1))
               (tmp-cr (cairo-create surf)))
          (dynamic-wind
            (lambda () #f)
            (lambda () (measure tmp-cr))
            (lambda ()
              ;; we don't need this cairo object at all once we have the dimensions
              (cairo-destroy tmp-cr)
              (cairo-surface-destroy surf)))))))

(define (gleui-widget-paint-node! cr lnode x y push-hit!)
  "Dispatch Cairo painter rendering for layout node LNODE.
Parameters: (CR LNODE X Y PUSH-HIT!)
  - CR: Cairo context.
  - LNODE: <gleui-layout-node> node to paint.
  - X: Float left surface coordinate in pixels.
  - Y: Float top surface coordinate in pixels.
  - PUSH-HIT!: Procedure collecting <gleui-hit-box> records during painting."
  (let* ((w       (gleui-layout-node-widget lnode))
         (kind    (gleui-widget-kind w))
         (handler (hashq-ref %paint-handlers kind)))
    (when handler
      (handler cr lnode x y push-hit!))))

(define (gleui-widget-paint! cr lnode x y)
  "Paint layout node LNODE onto CR.
Parameters: (CR LNODE X Y)
  - CR: Cairo context.
  - LNODE: <gleui-layout-node> root node produced by `gleui-widget-layout'.
  - X: Float root origin X coordinate in pixels.
  - Y: Float root origin Y coordinate in pixels.
Returns list of <gleui-hit-box> records in paint order."
  (let ((boxes '()))
    (gleui-widget-paint-node! cr lnode x y
                              (lambda (box)
                                (set! boxes (cons box boxes))))
    (reverse boxes)))

(define (gleui-widget-render! cr width height widget)
  "generate layout and paint  WIDGET to dimensions, and return hit-boxes.
NOTE: this function renders to cairo only (unlike gleui-widget-render-commit!)
Parameters: (CR WIDTH HEIGHT WIDGET)
  - CR: Cairo context.
  - WIDTH: Integer target surface width in pixels.
  - HEIGHT: Integer target surface height in pixels.
  - WIDGET: <gleui-widget> root node.
Returns list of <gleui-hit-box> records in paint order."
  ;; cairo context may be already rendering something
  ;; better always start clean
  (gleui-cairo-clear! cr)
  (let ((lnode (gleui-widget-layout cr widget width height #t #t)))
    (gleui-widget-paint! cr lnode 0 0)))

(define* (gleui-widget-render-commit! surface slots active-idx width height widget
                                      #:key (layer-surface #f) (update-size? #t)
                                      (pre-commit #f) (persistent? #t)
                                      (shm-name "gliver-gleui-shm"))
  "Process layout and paint WIDGET into SHM SLOTS and commit SURFACE.
Parameters: (SURFACE SLOTS ACTIVE-IDX WIDTH HEIGHT WIDGET
[#:LAYER-SURFACE #f] [#:UPDATE-SIZE? #t] [#:PRE-COMMIT #f]
[#:PERSISTENT? #t] [#:SHM-NAME \"gliver-gleui-shm\"])
  - SURFACE: Pointer to Wayland wl_surface.
  - SLOTS: Pair of <gleui-shm-slot> records, or #f.
  - ACTIVE-IDX: Integer or #f index (0 or 1) of the currently displayed slot.
  - WIDTH: Integer target surface width in pixels.
  - HEIGHT: Integer target surface height in pixels.
  - WIDGET: <gleui-widget> root node to render.
  - LAYER-SURFACE: Pointer to zwlr_layer_surface_v1, or #f, resized when UPDATE-SIZE? is #t.
  - UPDATE-SIZE?: Boolean updating layer-surface size when #t and LAYER-SURFACE is valid.
  - PRE-COMMIT: Procedure or #f zero-argument thunk invoked just before attaching the buffer.
  - PERSISTENT?: Boolean retaining double-buffered mappings when #t, or unmapping immediately after commit when #f.
  - SHM-NAME: String or #f descriptive memfd name used when allocating backing SHM slots.
Returns four values: NEW-SLOTS, NEXT-IDX, WL-BUFFER (or #f), and HIT-BOXES."
  (let ((hit-boxes '()))
    (call-with-values
        (lambda ()
          (gleui-shm-render-commit!
           surface slots active-idx width height
           (lambda (cr lw lh)
             ;; run the two stages of generating layout and painting
             ;; also as a result, we store generated hit-boxes for interaction
             (set! hit-boxes (gleui-widget-render! cr lw lh widget)))
           #:layer-surface layer-surface
           #:update-size? update-size?
           #:pre-commit pre-commit
           #:persistent? persistent?
           #:shm-name shm-name))
      (lambda (new-slots next-idx wl-buf)
        (values new-slots next-idx wl-buf hit-boxes)))))
