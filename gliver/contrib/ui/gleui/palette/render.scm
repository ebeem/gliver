;;; gliver/contrib/ui/gleui/palette/render.scm --- Palette Widget & Rendering
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module builds and renders declarative gleui widget trees for
;;; the palette
;;; It handles:
;;;  - Building the prompt, cursor, match counter, and candidate row widgets
;;;  - Highlighting matching filter tokens inside Pango markup
;;;  - Committing SHM buffers and updating surface hit-boxes

(define-module (gliver contrib ui gleui palette render)
  #:use-module (ice-9 format)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (gliver core)
  #:use-module (gliver contrib ui gleui base)
  #:use-module (gliver contrib ui gleui widgets)
  #:use-module (gliver contrib ui gleui palette state)
  #:use-module (gliver contrib ui gleui palette matching)
  #:declarative? #f
  #:export (
            *palette-font*
            *palette-prompt*
            *palette-prompt-color*
            *palette-input-bg-color*
            *palette-input-border-color*
            *palette-cursor-width*
            *palette-cursor-color*
            *palette-candidates-count-color*
            *palette-no-candidates-count-color*
            *palette-no-candidates-text-color*
            *palette-bg-color*
            *palette-fg-color*
            *palette-selected-fg-color*
            *palette-selected-bg-color*
            *palette-selected-radius*
            *palette-border-color*
            *palette-border-width*
            *palette-border-radius*
            *palette-match-color*
            *palette-row-click-proc*
            *palette-scroll-step-proc*
            gleui-palette-set-interaction-procs!
            gleui-palette-build-widget
            gleui-palette-draw-surface!
            gleui-palette-render!
))

;;; styling configuration
(define-var *palette-font* #:inherit *theme-font*
            "Font family string used inside the launcher palette.")
(define-var *palette-prompt* "> "
            "Prompt string displayed before the input field in the palette.")
(define-var *palette-prompt-color* #:inherit *theme-fg-alt*
            "Color used for rendering prompt text.")
(define-var *palette-input-bg-color* #:inherit *theme-surface0*
            "Color used for prompt text background.")
(define-var *palette-input-border-color* #:inherit *theme-border-color*
            "Color used for prompt text border.")
(define-var *palette-cursor-width* 2.0
            "Width of palette cursor.")
(define-var *palette-cursor-color* #:inherit *theme-text*
            "Color used for palette cursor.")
(define-var *palette-candidates-count-color* #:inherit *theme-overlay2*
            "Color used for palette candidates counter.")
(define-var *palette-no-candidates-count-color* #:inherit *theme-red*
            "Color used for palette candidates counter when empty.")
(define-var *palette-no-candidates-text-color* #:inherit *theme-overlay2*
            "Color used for palette candidates text when empty.")
(define-var *palette-bg-color* #:inherit *theme-bg-main*
            "Background color of the palette window.")
(define-var *palette-fg-color* #:inherit *theme-fg-main*
            "Foreground text color of the palette window.")
(define-var *palette-selected-fg-color* #:inherit *theme-fg-alt*
            "Highlight color used for the currently selected candidate entry.")
(define-var *palette-selected-bg-color* #:inherit *theme-surface1*
            "Color used for background of the currently selected candidate entry.")
(define-var *palette-selected-radius* 0
            "Radius used for background of the currently selected candidate entry.")
(define-var *palette-border-color* #:inherit *theme-border-color*
            "Border color around the palette window.")
(define-var *palette-border-width* #:inherit *theme-border-width*
            "Border width of the palette window in pixels.")
(define-var *palette-border-radius* #:inherit *theme-border-radius*
            "Corner border radius of the palette window in pixels.")
(define-var *palette-match-color* #:inherit *theme-red*
            "Color used to highlight query keywords in search results (defaults to red).")

;; populated by palette/input after load to avoid circular module dependency
(define-var *palette-row-click-proc* #f
            "Callback invoked when a candidate row hit-box is clicked.")
(define-var *palette-scroll-step-proc* #f
            "Callback invoked when scrolling over palette candidate rows.")

(define (gleui-palette-set-interaction-procs! on-row-click on-scroll-step)
  "Register ON-ROW-CLICK and ON-SCROLL-STEP interaction handlers.
Parameters: (ON-ROW-CLICK ON-SCROLL-STEP)
  - ON-ROW-CLICK: Procedure called as (ON-ROW-CLICK state row-index) on row click.
  - ON-SCROLL-STEP: Procedure called as (ON-SCROLL-STEP state delta) on scroll."
  (set! *palette-row-click-proc* on-row-click)
  (set! *palette-scroll-step-proc* on-scroll-step))

;; building blocks for the whole widget
(define (%build-input-bar-widget state theme-get font-str font-small font-size
                                 border-radius input-height fg-color sel-idx total-matching)
  "Build the query input box for palette STATE.
Parameters: (STATE THEME-GET FONT-STR FONT-SMALL FONT-SIZE BORDER-RADIUS
INPUT-HEIGHT FG-COLOR SEL-IDX TOTAL-MATCHING)
  - STATE: <gleui-palette-state> record.
  - THEME-GET: Procedure looking up themed style overrides.
  - FONT-STR: String main Pango font.
  - FONT-SMALL: String secondary Pango font for the counter badge.
  - FONT-SIZE: Integer base font size in points.
  - BORDER-RADIUS: Integer outer palette corner radius in pixels.
  - INPUT-HEIGHT: Integer target height of the query input box in pixels.
  - FG-COLOR: String foreground hex color for typed text.
  - SEL-IDX: Integer index of the currently selected candidate.
  - TOTAL-MATCHING: Integer total number of matching candidates.
Returns <gleui-widget> node."
  ;; the palette header looks something similar to what's below
  ;; Launch: librewolf                         [1/5]
  ;; {prompt-str}: {fliter-text}               {{sel-idx}/{total-matching}}
  (let* ((prompt-str          (or (gleui-palette-state-prompt state)
                                  (theme-get 'prompt '*palette-prompt*)))
         (prompt-color        (theme-get 'prompt-color '*palette-prompt-color*))
         (cursor-color        (theme-get 'cursor-color '*palette-cursor-color*))
         (cursor-w            (theme-get 'cursor-width '*palette-cursor-width*))
         (counter-color       (theme-get 'candidates-count-color '*palette-candidates-count-color*))
         (empty-counter-color (theme-get 'no-candidates-count-color '*palette-no-candidates-count-color*))
         (filter-text         (gleui-palette-state-filter-text state))
         (cursor-pos          (min (gleui-palette-state-cursor-pos state) (string-length filter-text)))
         (escaped-prompt      (format #f "<span color='~a'><b>~a</b></span>"
                                      prompt-color (gleui-pango-escape prompt-str)))
         ;; split input around cursor-pos so the cursor bar can sit inline between characters
         (escaped-before      (gleui-pango-escape (string-take filter-text cursor-pos)))
         (escaped-after       (gleui-pango-escape (string-drop filter-text cursor-pos)))
         (count-str           (format #f "<span color='~a'>[~a/~a]</span>"
                                      (if (zero? total-matching) empty-counter-color counter-color)
                                      (if (zero? total-matching) 0 (1+ sel-idx))
                                      total-matching)))
    (gleui-w-box
     (gleui-w-row
      (list
       (gleui-w-expanded
        (gleui-w-row
         (list
          (gleui-w-text escaped-prompt #:font font-str #:markup? #t)
          (and (not (string-null? escaped-before))
               (gleui-w-text escaped-before #:font font-str #:color fg-color #:markup? #t))
          (gleui-w-box (gleui-w-empty)
                       #:width cursor-w
                       #:height (max 10 (- input-height 16))
                       #:bg-color cursor-color)
          (and (not (string-null? escaped-after))
               (gleui-w-text escaped-after #:font font-str #:color fg-color #:markup? #t)))
         #:gap 0
         #:cross-align 'center))
       (gleui-w-text count-str
                     #:font font-small
                     #:min-height font-size
                     #:markup? #t))
      #:gap 8
      #:cross-align 'center)
     #:height input-height
     #:radius (max 4 (- border-radius 2))
     #:bg-color (theme-get 'input-bg '*palette-input-bg-color*)
     #:border-color (theme-get 'input-border '*palette-input-border-color*)
     #:border-width 1.0
     #:padding-x 12
     #:align 'center)))

(define (%build-candidate-row-widget state theme-get curr-idx row-height sel-idx filtered-idxs font-str)
  "Build clickable candidate row CURR-IDX for palette STATE.
Parameters: (STATE THEME-GET CURR-IDX ROW-HEIGHT SEL-IDX FILTERED-IDXS FONT-STR)
  - STATE: <gleui-palette-state> record.
  - THEME-GET: Procedure looking up themed style overrides.
  - CURR-IDX: Integer visible row index within FILTERED-IDXS.
  - ROW-HEIGHT: Integer target row height in pixels.
  - SEL-IDX: Integer currently selected candidate index.
  - FILTERED-IDXS: List of original candidate indices matching the current query.
  - FONT-STR: String Pango font for candidate labels.
Returns <gleui-widget> node."
  (let* ((cand-orig-idx    (list-ref filtered-idxs curr-idx))
         (selection-radius (theme-get 'selected-radius '*palette-selected-radius*))
         (selected-color   (theme-get 'selected '*palette-selected-fg-color*))
         (raw-str          (gleui-palette-get-display-string state cand-orig-idx))
         (match-color      (theme-get 'match-color '*palette-match-color*))
         (cand-str         (gleui-markup-string-highlight raw-str (gleui-palette-state-filter-text state) match-color))
         (is-selected?     (= curr-idx sel-idx))
         (text-color       (if is-selected?
                               (theme-get 'selected-fg-color '*palette-selected-fg-color*)
                               (theme-get 'fg-color '*palette-fg-color*)))
         (pill-h           (- row-height 4)))

    ;; return the candidate row, it's really nothing but a clickable box
    ;; that contains the name of the string name of candidate (could be pango)
    (gleui-w-clickable
     (gleui-w-box
      (gleui-w-box
       ;; this is the inner row data that contain the candidate string
       (gleui-w-row
        (list
         (and is-selected?
              (gleui-w-box (gleui-w-empty) #:width 4.0 #:height pill-h #:bg-color selected-color))
         (gleui-w-expanded
          (gleui-w-box
           (gleui-w-text cand-str
                         #:font font-str
                         #:color text-color
                         #:ellipsize 'end
                         #:markup? #t)
           #:padding-left (if is-selected? 14 12)
           #:padding-right 12
           #:align 'center-left)))
        #:gap 0
        #:cross-align 'center)
       #:height pill-h
       #:expand-x? #t
       #:radius (if is-selected? selection-radius 0)
       #:bg-color (and is-selected? (theme-get 'bg-active '*palette-selected-bg-color*))
       #:clip? is-selected?
       #:align 'center-left)
      #:height row-height
      #:padding-y 2
      #:align 'center-left)
     #:on-click (lambda (button _px _py _data)
                  ;; 272 is BTN_LEFT (left mouse button)
                  (when (and (= button 272) (procedure? *palette-row-click-proc*))
                    (*palette-row-click-proc* state curr-idx))))))

(define (%build-candidate-list-widget state theme-get row-height visible-lines total-matching sel-idx font-str)
  "Build the candidate list for palette STATE.
Parameters: (STATE THEME-GET ROW-HEIGHT VISIBLE-LINES TOTAL-MATCHING SEL-IDX FONT-STR)
  - STATE: <gleui-palette-state> record.
  - THEME-GET: Procedure looking up themed style overrides.
  - ROW-HEIGHT: Integer target height of each row in pixels.
  - VISIBLE-LINES: Integer maximum number of rows visible at once.
  - TOTAL-MATCHING: Integer total number of matching candidates.
  - SEL-IDX: Integer currently selected candidate index.
  - FONT-STR: String Pango font.
Returns <gleui-widget> node."
  (if (zero? total-matching)
      ;; if we don't have matches, the user will notice a placeholder with
      ;; the message "No matching candidates"
      (let ((empty-color (theme-get 'no-candidates-text-color '*palette-no-candidates-text-color*)))
        (gleui-w-box
         (gleui-w-text (format #f "<span color='~a'>No matching candidates</span>" empty-color)
                       #:font font-str
                       #:markup? #t)
         #:padding '(12 12 0 12)))

      ;; render only the visible candidates starting at scroll-off
      ;; this is how the scrolling behavior is implemented
      (let* ((filtered-idxs (gleui-palette-state-filtered-indices state))
             (scroll-off    (gleui-palette-state-scroll-offset state))
             (end-idx       (min total-matching (+ scroll-off visible-lines)))
             (indices       (iota (max 0 (- end-idx scroll-off)) scroll-off))
             (rows          (map (lambda (curr-idx)
                                   (%build-candidate-row-widget
                                    state theme-get curr-idx row-height sel-idx filtered-idxs font-str))
                                 indices)))

        ;; clickable on-scroll allows pointer to scroll the candidates
        (gleui-w-clickable
         (gleui-w-column rows #:gap 0)
         #:on-scroll (lambda (_axis delta _data)
                       (when (procedure? *palette-scroll-step-proc*)
                         (*palette-scroll-step-proc* state delta)))))))

(define (gleui-palette-build-widget state width height overrides)
  "Build the declarative widget tree for palette STATE.
Parameters: (STATE WIDTH HEIGHT OVERRIDES)
  - STATE: <gleui-palette-state> record.
  - WIDTH: Integer container width in pixels.
  - HEIGHT: Integer container height in pixels.
  - OVERRIDES: Alist of theme override keys and values.
Returns <gleui-widget> node."
  (let* ((theme-get      (lambda (k def) (gleui-theme-get overrides k def)))
         (font-family    (theme-get 'font '*palette-font*))
         (font-size      (theme-get 'font-size '*palette-font-size*))
         (font-str       (format #f "~a ~a" font-family font-size))
         (font-small     (format #f "~a ~a" font-family (max 9 (- font-size 2))))
         (fg-color       (theme-get 'foreground '*palette-fg-color*))
         (border-color   (theme-get 'border-color '*palette-border-color*))
         (border-width   (theme-get 'border-width '*palette-border-width*))
         (border-radius  (theme-get 'border-radius '*palette-border-radius*))
         (filtered-idxs  (gleui-palette-state-filtered-indices state))
         (total-matching (length filtered-idxs))
         (max-lines      (theme-get 'lines '*palette-candidates-count*))
         (visible-lines  (min max-lines (max 1 total-matching)))
         (sel-idx        (gleui-palette-state-selected-index state))
         (row-height     (max 38 (+ font-size 22)))
         (input-height   (max 42 (+ font-size 26)))
         (pad            16))

    ;; the whole palette widget is a column showing filter box followed
    ;; by the list of candidates widgets we created earlier
    ;; NOTE: it's recommended to follow this approach of dividing complex
    ;;       widgets into multiple building blocks that together create the UI
    (gleui-w-box
     (gleui-w-column
      (list
       (%build-input-bar-widget state theme-get font-str font-small font-size
                                border-radius input-height fg-color sel-idx total-matching)
       (%build-candidate-list-widget state theme-get row-height visible-lines
                                     total-matching sel-idx font-str))
      #:gap 16)
     #:width width
     #:height height
     #:padding pad
     #:radius border-radius
     #:bg-color (theme-get 'background '*palette-bg-color*)
     #:border-color border-color
     #:border-width border-width)))

(define (gleui-palette-draw-surface! cr width height state overrides)
  "Render palette STATE at WIDTH and HEIGHT onto Cairo context CR.
Parameters: (CR WIDTH HEIGHT STATE OVERRIDES)
  - CR: Cairo context.
  - WIDTH: Integer target surface width in pixels.
  - HEIGHT: Integer target surface height in pixels.
  - STATE: <gleui-palette-state> record.
  - OVERRIDES: Alist of theme override keys and values."
  (let ((boxes (gleui-widget-render! cr width height
                                     (gleui-palette-build-widget state width height overrides))))
    (gleui-palette-state-hit-boxes-set! state boxes)))

(define (gleui-palette-render! state)
  "Render palette STATE into double-buffered shared memory and commit it.
Parameters: (STATE)
  - STATE: <gleui-palette-state> record."
  (when (and (gleui-palette-state? state)
             (gleui-palette-state-visible? state)
             (gleui-palette-state-configured? state))
    ;; mutex prevents concurrent SHM renders (such as key repeat)
    (with-mutex (gleui-palette-state-render-mutex state)
      (let* ((surface    (gleui-palette-state-surface state))
             (layer-surf (gleui-palette-state-layer-surface state))
             (overrides  (gleui-palette-state-theme-overrides state))
             (width      (gleui-palette-state-width state))
             (height     (gleui-palette-calc-height state))
             (widget     (gleui-palette-build-widget state width height overrides)))
        (gleui-palette-state-height-set! state height)
        (call-with-values
            (lambda ()
              (gleui-widget-render-commit! surface
                                           (gleui-palette-state-shm-slots state)
                                           (gleui-palette-state-shm-active-idx state)
                                           width height widget
                                           #:layer-surface layer-surf
                                           #:shm-name "gliver-palette-shm"))
          (lambda (new-slots next-idx _wl-buf hit-boxes)
            (gleui-palette-state-shm-slots-set! state new-slots)
            (gleui-palette-state-shm-active-idx-set! state next-idx)
            (gleui-palette-state-hit-boxes-set! state hit-boxes)))))))
