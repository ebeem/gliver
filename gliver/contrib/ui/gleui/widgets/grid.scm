;;; gliver/contrib/ui/gleui/widgets/grid.scm --- Two-Dimensional Grid Layout Widget
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module defines the column-aligned 2D grid widget for gleui.

(define-module (gliver contrib ui gleui widgets grid)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (gliver contrib ui gleui widgets engine)
  #:use-module (gliver contrib ui gleui widgets text)
  #:declarative? #f
  #:export (
            gleui-w-grid
))

(define* (gleui-w-grid rows #:key (col-gap 0) (row-gap 0) (font #f)
                       (font-size #f) (color #f) (markup? #t))
  "Create a column-aligned grid widget from ROWS of cells or widgets.
Parameters: (ROWS [#:COL-GAP 0] [#:ROW-GAP 0] [#:FONT #f] [#:FONT-SIZE #f] [#:COLOR #f] [#:MARKUP? #t])
  - ROWS: List of row lists containing <gleui-widget> nodes, strings, or printable values.
  - COL-GAP: Integer horizontal column spacing in pixels.
  - ROW-GAP: Integer vertical row spacing in pixels.
  - FONT: String or #f default Pango font for string cells.
  - FONT-SIZE: Integer or #f default font size in points for string cells.
  - COLOR: String or #f default hex foreground color for string cells.
  - MARKUP?: Boolean parsing Pango markup in string cells when #t.
Returns <gleui-widget> node."
  (gleui-make-widget 'grid
                     `((col-gap . ,col-gap)
                       (row-gap . ,row-gap)
                       (font . ,font)
                       (font-size . ,font-size)
                       (color . ,color)
                       (markup? . ,markup?))
                     (if (list? rows) rows '())))

(define (%grid-cell->widget cell font font-size color markup?)
  "Convert grid CELL into a <gleui-widget>.
Parameters: (CELL FONT FONT-SIZE COLOR MARKUP?)
  - CELL: <gleui-widget> node, string, #f spacer, or printable value.
  - FONT: String or #f default font description.
  - FONT-SIZE: Integer or #f default font size in points.
  - COLOR: String or #f default hex foreground color.
  - MARKUP?: Boolean enabling Pango XML markup parsing for string cells.
Returns <gleui-widget> node."
  (cond
   ;; if it's already a widget, return it
   ((gleui-widget? cell) cell)
   ;; if it's #f, replace it with empty widget
   ((not cell) (gleui-w-empty))
   ;; otherwise, turn it into a string and add it as a text widget
   (else (gleui-w-text (if (string? cell) cell (format #f "~a" cell))
                       #:font font
                       #:font-size font-size
                       #:color color
                       #:markup? markup?))))

(define (%prefix-offsets sizes gap)
  "Compute offsets for each cell based on SIZES.
This will help figure out the offset of x or y based on column or row sizes.
For example: if passed column widths (sizes) are (30 50 20) and gap is 5
The offsets return should be (0 30+5 30+5+50+5) = (0 35 90)
NOTE: this later will be added to offset of the whole grid, so this is local offset
Parameters: (SIZES GAP)
  - SIZES: Vector of column widths or row heights in pixels.
  - GAP: Integer constant pixel spacing between consecutive tracks.
Returns vector of starting offsets in pixels."
  (let* ((count   (vector-length sizes))
         (offsets (make-vector count 0)))
    (let loop ((idx 0) (offset 0))
      (when (< idx count)
        (vector-set! offsets idx offset)
        (loop (1+ idx) (+ offset (vector-ref sizes idx) gap))))
    offsets))

(define (%grid-layout-rows cr rows props)
  "Layout all cells in ROWS using text defaults from PROPS and measure sizes.
Parameters: (CR ROWS PROPS)
  - CR: Cairo context used for measurement.
  - ROWS: List of row lists containing widgets, strings, or printable values.
  - PROPS: Alist of grid widget properties.
Returns three values: GRID-NODES, COL-WIDTHS, and ROW-HEIGHTS."
  (let* ((font        (gleui-widget-prop props 'font))
         (font-size   (gleui-widget-prop props 'font-size))
         (color       (gleui-widget-prop props 'color))
         (markup?     (gleui-widget-prop props 'markup?))
         (num-rows    (length rows))
         (num-cols    (if (zero? num-rows) 0
                          (apply max (map length rows))))
         ;; per-column max width and per-row max height vectors
         ;; to be filled in the loop later
         (col-widths  (make-vector num-cols 0))
         (row-heights (make-vector num-rows 0))
         ;; layout every cell and update col-widths / row-heights to track maximums
         (grid-nodes
          (let loop-row ((row-idx 0) (rem-rows rows) (rows-acc '()))
            (if (null? rem-rows)
                (reverse rows-acc)
                (let loop-column ((col-idx 0) (rem-cells (car rem-rows)) (cells-acc '()))
                  (if (null? rem-cells)
                      (loop-row (1+ row-idx) (cdr rem-rows)
                                (cons (reverse cells-acc) rows-acc))
                      (let* ((cell-widget (%grid-cell->widget (car rem-cells) font
                                                              font-size color markup?))
                             (cell-layout (gleui-widget-layout cr cell-widget #f #f #f #f))
                             (cell-width  (gleui-layout-node-width cell-layout))
                             (cell-height (gleui-layout-node-height cell-layout)))
                        ;; if for this column the new cell-wight is larger than what
                        ;; has been stored so far, update it (column width is the max vertically)
                        (when (> cell-width (vector-ref col-widths col-idx))
                          (vector-set! col-widths col-idx cell-width))
                        ;; if for this row the new cell-height is larger than what
                        ;; has been stored so far, update it (row height is the max horizontally)
                        (when (> cell-height (vector-ref row-heights row-idx))
                          (vector-set! row-heights row-idx cell-height))
                        (loop-column (1+ col-idx) (cdr rem-cells)
                                     (cons cell-layout cells-acc)))))))))
    (values grid-nodes col-widths row-heights)))

(define (%layout-grid cr widget avail-w avail-h tight-w? tight-h?)
  "Layout processor for grid WIDGET.
Parameters: (CR WIDGET AVAIL-W AVAIL-H TIGHT-W? TIGHT-H?)
  - CR: Cairo context used for measurement.
  - WIDGET: <gleui-widget> node.
  - AVAIL-W: Float or #f available width constraint in pixels.
  - AVAIL-H: Float or #f available height constraint in pixels.
  - TIGHT-W?: Boolean expanding to AVAIL-W if #t.
  - TIGHT-H?: Boolean expanding to AVAIL-H if #t.
Returns resolved <gleui-layout-node>."
  (let* ((props   (gleui-widget-props widget))
         (rows    (gleui-widget-children widget))
         (col-gap (gleui-widget-prop props 'col-gap))
         (row-gap (gleui-widget-prop props 'row-gap)))
    (let-values (((grid-nodes col-widths row-heights)
                  (%grid-layout-rows cr rows props)))
      ;; total grid size equals sum of all cell sizes plus gaps between cells
      (let* ((num-cols          (vector-length col-widths))
             (num-rows          (vector-length row-heights))
             (total-cols-width  (fold + 0 (vector->list col-widths)))
             (total-col-gaps    (if (> num-cols 1) (* (1- num-cols) col-gap) 0))
             (total-rows-height (fold + 0 (vector->list row-heights)))
             (total-row-gaps    (if (> num-rows 1) (* (1- num-rows) row-gap) 0))
             (final-w           (+ total-cols-width total-col-gaps))
             (final-h           (+ total-rows-height total-row-gaps)))
        (gleui-make-layout-node widget final-w final-h
                                (list col-widths row-heights grid-nodes) #f)))))

(define (%paint-grid! cr lnode x y push-hit!)
  "Paint grid layout node LNODE onto CR at (X, Y).
Parameters: (CR LNODE X Y PUSH-HIT!)
  - CR: Cairo context.
  - LNODE: <gleui-layout-node> node to paint.
  - X: Float left surface coordinate in pixels.
  - Y: Float top surface coordinate in pixels.
  - PUSH-HIT!: Procedure collecting <gleui-hit-box> records during painting."
  (let* ((widget      (gleui-layout-node-widget lnode))
         (props       (gleui-widget-props widget))
         (col-gap     (gleui-widget-prop props 'col-gap))
         (row-gap     (gleui-widget-prop props 'row-gap))
         (data        (gleui-layout-node-data lnode))
         (col-widths  (car data))
         (row-heights (cadr data))
         (grid-nodes  (caddr data))
         ;; starting positions for each column and row
         (col-offsets (%prefix-offsets col-widths col-gap))
         (row-offsets (%prefix-offsets row-heights row-gap)))
    ;; loop every row and column, paint each cell at its offset
    (let loop-row ((row-idx 0) (rem-rows grid-nodes))
      (when (pair? rem-rows)
        ;; remember that offset needs to be added to the grid position
        (let ((cell-y (+ y (vector-ref row-offsets row-idx))))
          (let loop-column ((col-idx 0) (rem-cells (car rem-rows)))
            (when (pair? rem-cells)
              (let ((cell-x (+ x (vector-ref col-offsets col-idx))))
                (gleui-widget-paint-node! cr (car rem-cells) cell-x cell-y push-hit!)
                (loop-column (1+ col-idx) (cdr rem-cells))))))
        (loop-row (1+ row-idx) (cdr rem-rows))))))

(gleui-register-widget! 'grid %layout-grid %paint-grid!)
