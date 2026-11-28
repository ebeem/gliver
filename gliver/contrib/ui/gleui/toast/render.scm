;;; gliver/contrib/ui/gleui/toast/render.scm --- Toast Widget Rendering
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module converts toast content (widgets, strings, line lists, or grid rows)
;;; into gleui widgets and renders them to toast surfaces.
;;; It handles:
;;;  - Resolving toast content into widgets
;;;  - Measuring dynamic toast dimensions
;;;  - Rendering toast surfaces and recording interactive hit-boxes

(define-module (gliver contrib ui gleui toast render)
  #:use-module (ice-9 format)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-1)
  #:use-module (gliver core)
  #:use-module (gliver contrib ui gleui base)
  #:use-module (gliver contrib ui gleui widgets)
  #:use-module (gliver contrib ui gleui toast state)
  #:declarative? #f
  #:export (
            gleui-toast-render-build-widget
            gleui-toast-render-calc-dimensions
            gleui-toast-render!
))

(define (%toast-content->widget content font-desc fg-color column-gap row-gap)
  "Convert toast CONTENT into a <gleui-widget>
Parameters: (CONTENT FONT-DESC FG-COLOR COLUMN-GAP ROW-GAP)
  - CONTENT: <gleui-widget>, string, list of lines or widgets, 2D list of grid rows, #f, or value.
  - FONT-DESC: String Pango font specification.
  - FG-COLOR: String foreground hex color.
  - COLUMN-GAP: Integer horizontal grid gap in pixels.
  - ROW-GAP: Integer vertical gap in pixels.
Returns <gleui-widget> node."
  ;; this helper function will normalize any supported type of content
  ;; and return it as gleui widget
  (cond
   ;; return if it's already a gleui widget
   ((gleui-widget? content)
    content)
   ;; if it's falsy, return empty widget
   ((not content)
    (gleui-w-empty))
   ;; if we have a string, return a text widget
   ((string? content)
    (gleui-w-text (gleui-pango-markup-ensure content)
                  #:font font-desc
                  #:color fg-color
                  #:markup? #t))
   ;; if we have list, there are multiple supported formats
   ((list? content)
    (cond
     ;; empty list is just empty widget
     ((null? content)
      (gleui-w-empty))
     ((list? (car content))
      ;; list of lists, turn it into a grid widget
      (let ((rows (map (lambda (row)
                         (map gleui-pango-markup-ensure row))
                       content)))
        (gleui-w-grid rows
                      #:col-gap column-gap
                      #:row-gap row-gap
                      #:font font-desc
                      #:color fg-color
                      #:markup? #t)))
     ((any gleui-widget? content)
      ;; list containing widgets stacks vertically as a column
      (gleui-w-column
       (map (lambda (item)
              (if (gleui-widget? item)
                  item
                  (gleui-w-text (gleui-pango-markup-ensure item)
                                #:font font-desc
                                #:color fg-color
                                #:markup? #t)))
            content)
       #:gap row-gap))
     (else
      ;; flat list of strings/values joins as lines in a single text widget
      (gleui-w-text (string-join
                     (map gleui-pango-markup-ensure content)
                     "\n")
                    #:font font-desc
                    #:color fg-color
                    #:markup? #t))))
   (else
    (gleui-w-text (gleui-pango-markup-ensure content)
                  #:font font-desc
                  #:color fg-color
                  #:markup? #t))))

(define (%build-actions-row actions font-desc fg-color border-color)
  "Build an interactive button row for clickable toast ACTIONS.
Parameters: (ACTIONS FONT-DESC FG-COLOR BORDER-COLOR)
  - ACTIONS: List of (LABEL . CALLBACK) pairs, or #f.
  - FONT-DESC: String Pango font specification for button labels.
  - FG-COLOR: String label foreground hex color.
  - BORDER-COLOR: String button border hex color.
Returns <gleui-widget> row node, or #f if ACTIONS is empty."
  (and (list? actions)
       (pair? actions)
       ;; a row of action buttons
       (gleui-w-row
        (map (lambda (action)
               (let ((label    (car action))
                     (callback (cdr action)))
                 ;; each action is a button with the label and its callback
                 (gleui-w-button
                  (gleui-pango-markup-ensure label)
                  #:font font-desc
                  #:color fg-color
                  #:border-color border-color
                  #:border-width 1.0
                  #:radius 6
                  #:padding-x 10
                  #:padding-y 4
                  #:on-click (lambda (button _px _py _data)
                               ;; 272 is BTN_LEFT (left mouse button)
                               (when (and (= button 272) (procedure? callback))
                                 (callback))))))
             actions)
        #:gap 8)))

(define (gleui-toast-render-build-widget state overrides)
  "Construct the toast widget tree from the state.
Parameters: (STATE OVERRIDES)
  - STATE: <gleui-toast-state> record.
  - OVERRIDES: Alist of theme override keys and values.
Returns <gleui-widget> node."
  (let* ((theme-get      (lambda (key default-var) (gleui-theme-get overrides key default-var)))
         ;; each variable below with theme-get falls back to configuration
         ;; variable if overrides doesn't provide a new value for it
         (custom-width   (theme-get 'width '*gleui-toast-width*))
         (custom-height  (theme-get 'height '*gleui-toast-height*))
         (target-width   (and (number? custom-width) (> custom-width 0) custom-width))
         (target-height  (and (number? custom-height) (> custom-height 0) custom-height))
         (font-family    (theme-get 'font '*gleui-toast-font*))
         (font-size      (theme-get 'font-size '*gleui-toast-font-size*))
         (font-desc      (format #f "~a ~a" font-family font-size))
         (bg-color       (theme-get 'background '*gleui-toast-bg-color*))
         (fg-color       (theme-get 'foreground '*gleui-toast-fg-color*))
         (border-color   (theme-get 'border-color '*gleui-toast-border-color*))
         (border-width   (theme-get 'border-width '*gleui-toast-border-width*))
         (border-radius  (theme-get 'border-radius '*gleui-toast-border-radius*))
         (padding-x      (theme-get 'padding-x *gleui-toast-padding-x*))
         (padding-y      (theme-get 'padding-y *gleui-toast-padding-y*))
         (column-gap     (theme-get 'column-gap *gleui-toast-column-gap*))
         (row-gap        (theme-get 'row-gap *gleui-toast-row-gap*))
         (icon-show?     (theme-get 'icon-show '*gleui-toast-icon-show*))
         (icon-location  (theme-get 'icon-location '*gleui-toast-icon-location*))
         (icon-size      (theme-get 'icon-size '*gleui-toast-icon-size*))
         (icon-border-radius   (theme-get 'icon-border-radius '*gleui-toast-icon-border-radius*))
         (icon-border-width    (theme-get 'icon-border-width '*gleui-toast-icon-border-width*))
         (icon-border-color    (theme-get 'icon-border-color '*gleui-toast-icon-border-color*))
         (icon-surface         (gleui-toast-state-icon-surface state))
         (has-icon?            (and icon-show? icon-surface))
         ;; create the icon widget which will be used later the in the main-row
         (icon-widget    (and has-icon?
                              (gleui-w-icon icon-surface
                                            #:size icon-size
                                            #:border-radius icon-border-radius
                                            #:border-width icon-border-width
                                            #:border-color icon-border-color)))
         ;; create the content widget by converting the toast raw content into a widget
         (content-widget (%toast-content->widget (gleui-toast-state-content state)
                                                 font-desc fg-color column-gap row-gap))
         ;; create the action buttons row which will show actions like reply, mute, etc.
         (actions-row    (%build-actions-row (gleui-toast-state-actions state)
                                             font-desc fg-color border-color))
         (body-column    (gleui-w-column (list content-widget actions-row) #:gap 10))
         (body-slot      (if target-width (gleui-w-expanded body-column) body-column))
         (main-row       (cond
                          ;; no icon displayed
                          ((not icon-widget)
                           body-column)
                          ;; icon on right
                          ((eq? icon-location 'right)
                           (gleui-w-row (list body-slot icon-widget) #:gap 16))
                          ;; icon on left (else)
                          (else
                           (gleui-w-row (list icon-widget body-slot) #:gap 16))))
         (min-width      160))
    (gleui-w-box main-row
                 #:width target-width
                 #:height target-height
                 #:min-width min-width
                 #:min-height 40
                 #:padding-x padding-x
                 #:padding-y padding-y
                 #:bg-color bg-color
                 #:border-color border-color
                 #:border-width border-width
                 #:radius border-radius)))

(define (gleui-toast-render-calc-dimensions state)
  "Calculate the surface dimensions for toast STATE.
Parameters: (STATE)
  - STATE: <gleui-toast-state> record.
Returns two values: WIDTH and HEIGHT in pixels."
  (let* ((overrides     (gleui-toast-state-theme-overrides state))
         (custom-width  (gleui-theme-get overrides 'width '*gleui-toast-width*))
         (target-output (or (gleui-toast-state-output state) (output-current)))
         (output-width  (if target-output (output-width target-output) 1920))
         (max-width     (if (and (number? custom-width)
                                 (> custom-width 0))
                            custom-width
                            (- output-width 60))))
    (gleui-widget-measure (gleui-toast-render-build-widget state overrides)
                          max-width #f)))

(define (gleui-toast-render! state)
  "Render toast STATE into double-buffered shared memory and commit it.
Parameters: (STATE)
  - STATE: <gleui-toast-state> record."
  (when (and state (gleui-toast-state-visible? state) (gleui-toast-state-configured? state))
    ;; mutex prevents rendering while destroying the toast which will crash
    ;; if it for example some properties required re-rendering while timeout
    ;; is destroying the toast
    (with-mutex (gleui-toast-state-render-mutex state)
      (let* ((surface       (gleui-toast-state-surface state))
             (layer-surface (gleui-toast-state-layer-surface state))
             (width         (gleui-toast-state-width state))
             (height        (gleui-toast-state-height state))
             (overrides     (gleui-toast-state-theme-overrides state))
             (widget        (gleui-toast-render-build-widget state overrides)))
        (call-with-values
            (lambda ()
              (gleui-widget-render-commit! surface
                                           (gleui-toast-state-shm-slots state)
                                           (gleui-toast-state-shm-active-idx state)
                                           width height widget
                                           #:layer-surface layer-surface
                                           #:update-size? #f
                                           #:shm-name (format #f "gliver-toast-~a-shm"
                                                              (or (gleui-toast-state-name state) "default"))))
          (lambda (new-slots next-idx _wl-buffer hit-boxes)
            (gleui-toast-state-shm-slots-set! state new-slots)
            (gleui-toast-state-shm-active-idx-set! state next-idx)
            (gleui-toast-state-hit-boxes-set! state hit-boxes)))))))
