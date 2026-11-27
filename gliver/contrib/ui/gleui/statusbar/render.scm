;;; gliver/contrib/ui/gleui/statusbar/render.scm --- Gleui Statusbar Widget
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module declares and renders widget trees for gleui statusbars.
;;; It handles:
;;;  - Measuring and styling left, center, and right statusbar modules
;;;  - Attaching module pointer click and scroll callbacks
;;;  - Committing rendered SHM buffers and updating recorded hit-boxes

(define-module (gliver contrib ui gleui statusbar render)
  #:use-module (srfi srfi-2)
  #:use-module (gliver core)
  #:use-module (gliver contrib ui gleui widgets)
  #:use-module (gliver contrib ui gleui statusbar state)
  #:declarative? #f
  #:export (
            statusbar-build-widget
            statusbar-render-surface!
            statusbar-render-and-commit!
))

;; the below functions are building blocks that will be
;; used in `statusbar-build-widget'
(define (%module-visible? module output)
  "Evaluate the visibility predicate or boolean flag of MODULE for OUTPUT.
Parameters: (MODULE OUTPUT)
  - MODULE: <statusbar-module> record.
  - OUTPUT: <output> record passed to dynamic visibility predicates.
Returns #t if MODULE is visible on OUTPUT, #f otherwise."
  (let ((visibility (statusbar-module-visible? module)))
    (if (procedure? visibility)
        ;; visible? accepts both boolean and procedure to allow
        ;; components to hide themselves dynamically
        (visibility module output)
        visibility)))

(define (%make-module-click-handler module output)
  "Create a click callback for MODULE.
Parameters: (MODULE OUTPUT)
  - MODULE: <statusbar-module> record.
  - OUTPUT: <output> record where the click occurred.
Returns click callback procedure accepting (BUTTON POINTER-X POINTER-Y CUSTOM-DATA)."
  (lambda (button pointer-x pointer-y custom-data)
    (let ((click-handler (statusbar-module-on-click module)))
      ;; if the module has a procedure in on-click, then pass it the event
      (when (procedure? click-handler)
        (click-handler button pointer-x pointer-y module output custom-data)))))

(define (%make-module-scroll-handler module output)
  "Create a pointer scroll callback forwarding axis events to MODULE on OUTPUT.
Parameters: (MODULE OUTPUT)
  - MODULE: <statusbar-module> record.
  - OUTPUT: <output> record where the scroll occurred.
Returns scroll callback procedure accepting (AXIS DELTA CUSTOM-DATA)."
  (lambda (axis delta _custom-data)
    (let ((scroll-handler (statusbar-module-on-scroll module)))
      ;; if the module has a procedure in on-scroll, then pass it the event
      (when (procedure? scroll-handler)
        (scroll-handler axis delta module output)))))

(define (%module->widget module output bar-height font font-size default-bg-color default-fg-color
                         pill-radius pill-padding-x pill-padding-y)
  "Convert statusbar MODULE into a declarative gleui widget node.
Parameters: (MODULE OUTPUT BAR-HEIGHT FONT FONT-SIZE DEFAULT-BG-COLOR DEFAULT-FG-COLOR
PILL-RADIUS PILL-PADDING-X PILL-PADDING-Y)
  - MODULE: <statusbar-module> record.
  - OUTPUT: <output> record in which the bar will be placed.
  - BAR-HEIGHT: Integer maximum statusbar height in pixels.
  - FONT: String font family name.
  - FONT-SIZE: Integer font size in points.
  - DEFAULT-BG-COLOR: String default pill background hex color.
  - DEFAULT-FG-COLOR: String default pill foreground hex color.
  - PILL-RADIUS: Integer default pill corner radius in pixels.
  - PILL-PADDING-X: Integer default horizontal pill padding in pixels.
  - PILL-PADDING-Y: Integer default vertical pill padding in pixels.
Returns <gleui-widget> node, or #f when standard module text is empty."
  (let ((widget-fn (statusbar-module-widget-fn module)))
    (if (procedure? widget-fn)
        ;; if widget-fn is a procedure, then the module builds its
        ;; own <gleui-widget> tree and we should just use the procedure
        (widget-fn module output
                   #:bar-height bar-height
                   #:font font
                   #:font-size font-size
                   #:bg-color default-bg-color
                   #:fg-color default-fg-color
                   #:pill-radius pill-radius
                   #:pill-padding-x pill-padding-x
                   #:pill-padding-y pill-padding-y)
        ;; otherwise, the module is standard text-based and its text
        ;; should just be rendered as <gleui-w-text> in a gleui-w-box
        (let ((text (or (statusbar-module-text module) "")))
          (and (not (string-null? text))
               (let* ((padding-x (or (statusbar-module-padding-x module) pill-padding-x))
                      (padding-y (or (statusbar-module-padding-y module) pill-padding-y))
                      (radius    (or (statusbar-module-border-radius module) pill-radius))
                      (bg-color  (or (statusbar-module-bg-color module) default-bg-color))
                      (fg-color  (or (statusbar-module-fg-color module) default-fg-color)))
                 (gleui-w-clickable
                  (gleui-w-box
                   (gleui-w-text text
                                 #:font font
                                 #:font-size font-size
                                 #:color fg-color
                                 #:markup? #f
                                 #:multiline? #f)
                   #:max-height bar-height
                   #:padding-x padding-x
                   #:padding-y padding-y
                   #:radius radius
                   #:bg-color bg-color
                   #:border-color (statusbar-module-border-color module)
                   #:border-width (or (statusbar-module-border-width module) 0)
                   #:inset-border? #f
                   #:align 'center)
                  #:on-click (%make-module-click-handler module output)
                  #:on-scroll (%make-module-scroll-handler module output))))))))

(define (%build-section module-specs output bar-height font font-size default-bg-color
                        default-fg-color pill-radius pill-padding-x pill-padding-y spacing)
  "Build a MODULE-SPECS for section (like 'center or 'left) into a horizontal row widget.
Parameters: (MODULE-SPECS OUTPUT BAR-HEIGHT FONT FONT-SIZE DEFAULT-BG-COLOR DEFAULT-FG-COLOR
PILL-RADIUS PILL-PADDING-X PILL-PADDING-Y SPACING)
  - MODULE-SPECS: List of <statusbar-module> records, symbols, or factory procedures.
  - OUTPUT: <output> record in which that statusbar is displayed.
  - BAR-HEIGHT: Integer statusbar height in pixels.
  - FONT: String font family name.
  - FONT-SIZE: Integer font size in px.
  - DEFAULT-BG-COLOR: String default pill background hex color.
  - DEFAULT-FG-COLOR: String default pill foreground hex color.
  - PILL-RADIUS: Integer default pill corner radius in pixels.
  - PILL-PADDING-X: Integer default horizontal pill padding in pixels.
  - PILL-PADDING-Y: Integer default vertical pill padding in pixels.
  - SPACING: Integer pixel gap between modules in the section.
Returns <gleui-widget> row node, or #f when no visible widgets remain."
  (let* ((visible-modules (filter (lambda (module) (%module-visible? module output))
                                  (statusbar-resolve-modules module-specs)))
         (module-widgets  (map (lambda (module)
                                 (%module->widget module output bar-height font font-size
                                                  default-bg-color default-fg-color
                                                  pill-radius pill-padding-x pill-padding-y))
                               visible-modules))
         ;; section is nothing but a row of the modules
         (section-row     (gleui-w-row module-widgets
                                       #:gap spacing
                                       #:cross-align 'center)))
    (and (pair? (gleui-widget-children section-row)) section-row)))

(define (statusbar-build-widget output bar width height)
  "Build a declarative statusbar widget tree for BAR.
Parameters: (OUTPUT BAR WIDTH HEIGHT)
  - OUTPUT: <output> record associated with the bar.
  - BAR: <statusbar> record, or #f to use OUTPUT's bound bar.
  - WIDTH: Integer target bar width in pixels.
  - HEIGHT: Integer target bar height in pixels.
Returns <gleui-widget> node, or #f when no valid statusbar is bound."
  (and-let* ((resolved-bar (or bar (output-statusbar output)))
             ((statusbar? resolved-bar)))
    (let* ((font           (or (statusbar-font resolved-bar) *statusbar-font*))
           (font-size      (or (statusbar-font-size resolved-bar) *statusbar-font-size*))
           (bg-color       (or (statusbar-bg-color resolved-bar) *statusbar-bg-color*))
           (border-color   (or (statusbar-border-color resolved-bar) *statusbar-border-color*))
           (border-width   (or (statusbar-border-width resolved-bar) *statusbar-border-width*))
           (border-radius  (or (statusbar-border-radius resolved-bar) *statusbar-border-radius*))
           (fg-color       (or (statusbar-fg-color resolved-bar) *statusbar-fg-color*))
           (spacing        (or (statusbar-spacing resolved-bar) *statusbar-spacing*))
           (padding-x      (or (statusbar-padding-x resolved-bar) *statusbar-padding-x*))
           (pill-radius    (or (statusbar-pill-radius resolved-bar) *statusbar-pill-radius*))
           (pill-padding-x (or (statusbar-pill-padding-x resolved-bar) *statusbar-pill-padding-x*))
           (pill-padding-y (or (statusbar-pill-padding-y resolved-bar) *statusbar-pill-padding-y*))
           (build-section  (lambda (module-specs)
                             (%build-section module-specs output height font font-size
                                             bg-color fg-color pill-radius
                                             pill-padding-x pill-padding-y spacing)))
           (left-section   (build-section (statusbar-modules-left resolved-bar)))
           (center-section (build-section (statusbar-modules-center resolved-bar)))
           (right-section  (build-section (statusbar-modules-right resolved-bar)))
           (left-container (and left-section
                                (gleui-w-box left-section #:padding-x padding-x #:clip? #t)))
           (right-container (and right-section
                                 (gleui-w-box right-section #:padding-x padding-x #:clip? #t)))
           (body           (if center-section
                               ;; if we have center, shrink it to its space and let left and right take the rest
                               (gleui-w-row
                                (list (gleui-w-expanded (gleui-w-align left-container #:align 'center-left))
                                      (gleui-w-box center-section #:clip? #t)
                                      (gleui-w-expanded (gleui-w-align right-container #:align 'center-right)))
                                #:cross-align 'center)
                               ;; if we don't have center, shrink right and let left take the rest
                               (gleui-w-row
                                (list (gleui-w-expanded (gleui-w-align left-container #:align 'center-left))
                                      right-container)
                                #:cross-align 'center))))

      ;; add the body which is a row of the modules aligned in a box
      (gleui-w-box
       body
       #:width width
       #:height height
       #:radius border-radius
       #:bg-color (and bg-color (not (string-null? bg-color)) bg-color)
       #:border-color (and border-color (not (string-null? border-color)) border-color)
       #:border-width border-width
       #:inset-border? #f))))

(define (statusbar-render-surface! output bar cairo-context width height)
  "Render the statusbar widget tree for OUTPUT and BAR onto CAIRO-CONTEXT.
Parameters: (OUTPUT BAR CAIRO-CONTEXT WIDTH HEIGHT)
  - OUTPUT: <output> record associated with the bar.
  - BAR: <statusbar> record whose hit-boxes are updated.
  - CAIRO-CONTEXT: Cairo context.
  - WIDTH: Integer target surface width in pixels.
  - HEIGHT: Integer target surface height in pixels."
  (and-let* ((widget (statusbar-build-widget output bar width height)))
    (let ((hit-boxes (gleui-widget-render! cairo-context width height widget)))
      (statusbar-hit-boxes-set! bar hit-boxes))))

(define (statusbar-render-and-commit! output bar)
  "Render the statusbar surface for OUTPUT and BAR and commit it to Wayland.
Parameters: (OUTPUT BAR)
  - OUTPUT: <output> record associated with the bar.
  - BAR: <statusbar> record."
  (let* ((width   (statusbar-rendered-width bar))
         (height  (statusbar-rendered-height bar))
         (surface (statusbar-surface bar))
         (widget  (statusbar-build-widget output bar width height)))
    (when widget
      (catch #t
        (lambda ()
          (call-with-values
              (lambda ()
                (gleui-widget-render-commit!
                 surface
                 (statusbar-shm-slots bar)
                 (statusbar-shm-active-idx bar)
                 width height widget
                 #:update-size? #f
                 #:shm-name (format #f "gliver-statusbar-~a-shm"
                                    (or (and (output? output) (output-name output)) "output"))))
            (lambda (updated-shm-slots next-shm-index wl-buffer hit-boxes)
              (statusbar-shm-slots-set! bar updated-shm-slots)
              (statusbar-shm-active-idx-set! bar next-shm-index)
              (statusbar-hit-boxes-set! bar hit-boxes)
              (when (valid-pointer? wl-buffer)
                (statusbar-buffer-set! bar wl-buffer)))))
        (lambda (key . args)
          (log-error "statusbar: render and commit failed: ~a ~a" key args))))))
