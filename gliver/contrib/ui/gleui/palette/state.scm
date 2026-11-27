;;; gliver/contrib/ui/gleui/palette/state.scm --- Palette State & Layout Geometry
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module defines the palette view state record, singleton tracking, and
;;; layout geometry helpers for gleui's interactive palette.
;;; It handles:
;;;  - Defining <gleui-palette-state> and its field accessors/mutators
;;;  - Tracking the global cached palette instance and previously focused window

(define-module (gliver contrib ui gleui palette state)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 format)
  #:use-module (gliver core)
  #:use-module (gliver contrib ui gleui base)
  #:declarative? #f
  #:export (
            *palette-font-size*
            *palette-width*
            *palette-candidates-count*
            <gleui-palette-state>
            gleui-make-palette-state
            gleui-palette-state?
            gleui-palette-state-surface
            gleui-palette-state-surface-set!
            gleui-palette-state-layer-surface
            gleui-palette-state-layer-surface-set!
            gleui-palette-state-shm-slots
            gleui-palette-state-shm-slots-set!
            gleui-palette-state-shm-active-idx
            gleui-palette-state-shm-active-idx-set!
            gleui-palette-state-configured?
            gleui-palette-state-configured?-set!
            gleui-palette-state-visible?
            gleui-palette-state-visible?-set!
            gleui-palette-state-width
            gleui-palette-state-width-set!
            gleui-palette-state-height
            gleui-palette-state-height-set!
            gleui-palette-state-output
            gleui-palette-state-output-set!
            gleui-palette-state-keyboard
            gleui-palette-state-keyboard-set!
            gleui-palette-state-xkb-ctx
            gleui-palette-state-xkb-ctx-set!
            gleui-palette-state-xkb-keymap
            gleui-palette-state-xkb-keymap-set!
            gleui-palette-state-xkb-state
            gleui-palette-state-xkb-state-set!
            gleui-palette-state-candidates
            gleui-palette-state-candidates-set!
            gleui-palette-state-display-candidates
            gleui-palette-state-display-candidates-set!
            gleui-palette-state-search-strings
            gleui-palette-state-search-strings-set!
            gleui-palette-state-parsed-candidates
            gleui-palette-state-parsed-candidates-set!
            gleui-palette-state-filtered-indices
            gleui-palette-state-filtered-indices-set!
            gleui-palette-state-filter-text
            gleui-palette-state-filter-text-set!
            gleui-palette-state-cursor-pos
            gleui-palette-state-cursor-pos-set!
            gleui-palette-state-selected-index
            gleui-palette-state-selected-index-set!
            gleui-palette-state-scroll-offset
            gleui-palette-state-scroll-offset-set!
            gleui-palette-state-prompt
            gleui-palette-state-prompt-set!
            gleui-palette-state-theme-overrides
            gleui-palette-state-theme-overrides-set!
            gleui-palette-state-done?
            gleui-palette-state-done?-set!
            gleui-palette-state-result
            gleui-palette-state-result-set!
            gleui-palette-state-async-callback
            gleui-palette-state-async-callback-set!
            gleui-palette-state-render-mutex
            gleui-palette-state-repeat-rate
            gleui-palette-state-repeat-rate-set!
            gleui-palette-state-repeat-delay
            gleui-palette-state-repeat-delay-set!
            gleui-palette-state-hit-boxes
            gleui-palette-state-hit-boxes-set!
            *gleui-palette-instance*
            *gleui-palette-previous-window*
            gleui-palette-instance-set!
            gleui-palette-previous-window-set!
            gleui-palette-visible?
            gleui-palette-configured?
            gleui-palette-max-lines
            gleui-palette-adjust-scroll!
            gleui-palette-calc-height
            gleui-palette-default-width
            gleui-palette-default-search-strings
))

(define-var *palette-font-size* #:inherit *theme-font-size*
            "Font size in points for the launcher palette.")
(define-var *palette-width* 90
            "Width of the palette window as a percentage of monitor width.")
(define-var *palette-candidates-count* 10
            "Maximum number of visible candidate rows shown in the palette.")

;; palette state record
;;  - surface: pointer to wl_surface or #f, surface targeted for rendering
;;  - layer-surface: pointer to zwlr_layer_surface_v1 or #f, layer-shell for positioning
;;  - shm-slots: pair (slot-a . slot-b) of <gleui-shm-slot> records, one for compositor and one for painting
;;  - shm-active-idx: integer (0 or 1) active slot index toggled after each commit
;;  - configured?: boolean whether the compositor has configured the layer surface
;;  - visible?: boolean whether the palette is currently visible on screen
;;  - width: integer palette width in pixels
;;  - height: integer palette height in pixels
;;  - output: <output> record or #f target output in which palette will show up
;;  - keyboard: list of seat vectors #(wl-seat wl-keyboard seat-listener kb-listener active?)
;;  - xkb-ctx: pointer to xkb_context or %null-pointer used to compile keymaps
;;  - xkb-keymap: pointer to xkb_keymap or %null-pointer compiled from seat keymap fd
;;  - xkb-state: pointer to xkb_state or %null-pointer for keysym and UTF-8 translation
;;  - candidates: list or vector of raw candidate items
;;  - display-candidates: vector of string or #f lazy cache of formatted display strings
;;  - search-strings: list, vector, or #f searchable strings parallel to `candidates`
;;  - parsed-candidates: vector of <palette-candidate> records, or #f pre-parsed for filtering
;;  - filtered-indices: list of integers matching `candidates` indices sorted by score
;;  - filter-text: string current user prompt query
;;  - cursor-pos: integer character cursor offset of `filter-text`
;;  - selected-index: integer index of the highlighted candidate from `filtered-indices`
;;  - scroll-offset: integer index of the top visible candidate from `filtered-indices`
;;  - prompt: string prompt label rendered in the filter input
;;  - theme-overrides: alist or #f per-invocation style and layout overrides
;;  - done?: boolean completion flag set on confirm or cancel to exit the palette
;;  - result: integer, string, or #f palette selection result
;;  - async-callback: procedure or #f one-argument callback invoked when selection finishes
;;  - render-mutex: mutex to serialize Cairo drawing and Wayland commits
;;  - repeat-rate: integer key repeat rate in keys per second
;;  - repeat-delay: integer initial key-repeat delay in ms
;;  - hit-boxes: list of clickable <gleui-hit-box> records
(define-record-type <gleui-palette-state>
  (gleui-make-palette-state surface layer-surface shm-slots shm-active-idx configured? visible?
                            width height output keyboard xkb-ctx xkb-keymap xkb-state
                            candidates display-candidates search-strings parsed-candidates
                            filtered-indices filter-text cursor-pos selected-index scroll-offset
                            prompt theme-overrides done? result async-callback render-mutex
                            repeat-rate repeat-delay hit-boxes)
  gleui-palette-state?
  (surface             gleui-palette-state-surface             gleui-palette-state-surface-set!)
  (layer-surface       gleui-palette-state-layer-surface       gleui-palette-state-layer-surface-set!)
  (shm-slots           gleui-palette-state-shm-slots           gleui-palette-state-shm-slots-set!)
  (shm-active-idx      gleui-palette-state-shm-active-idx      gleui-palette-state-shm-active-idx-set!)
  (configured?         gleui-palette-state-configured?         gleui-palette-state-configured?-set!)
  (visible?            gleui-palette-state-visible?            gleui-palette-state-visible?-set!)
  (width               gleui-palette-state-width               gleui-palette-state-width-set!)
  (height              gleui-palette-state-height              gleui-palette-state-height-set!)
  (output              gleui-palette-state-output              gleui-palette-state-output-set!)
  (keyboard            gleui-palette-state-keyboard            gleui-palette-state-keyboard-set!)
  (xkb-ctx             gleui-palette-state-xkb-ctx             gleui-palette-state-xkb-ctx-set!)
  (xkb-keymap          gleui-palette-state-xkb-keymap          gleui-palette-state-xkb-keymap-set!)
  (xkb-state           gleui-palette-state-xkb-state           gleui-palette-state-xkb-state-set!)
  (candidates          gleui-palette-state-candidates          gleui-palette-state-candidates-set!)
  (display-candidates  gleui-palette-state-display-candidates  gleui-palette-state-display-candidates-set!)
  (search-strings      gleui-palette-state-search-strings      gleui-palette-state-search-strings-set!)
  (parsed-candidates   gleui-palette-state-parsed-candidates   gleui-palette-state-parsed-candidates-set!)
  (filtered-indices    gleui-palette-state-filtered-indices    gleui-palette-state-filtered-indices-set!)
  (filter-text         gleui-palette-state-filter-text         gleui-palette-state-filter-text-set!)
  (cursor-pos          gleui-palette-state-cursor-pos          gleui-palette-state-cursor-pos-set!)
  (selected-index      gleui-palette-state-selected-index      gleui-palette-state-selected-index-set!)
  (scroll-offset       gleui-palette-state-scroll-offset       gleui-palette-state-scroll-offset-set!)
  (prompt              gleui-palette-state-prompt              gleui-palette-state-prompt-set!)
  (theme-overrides     gleui-palette-state-theme-overrides     gleui-palette-state-theme-overrides-set!)
  (done?               gleui-palette-state-done?               gleui-palette-state-done?-set!)
  (result              gleui-palette-state-result              gleui-palette-state-result-set!)
  (async-callback      gleui-palette-state-async-callback      gleui-palette-state-async-callback-set!)
  (render-mutex        gleui-palette-state-render-mutex)
  (repeat-rate         gleui-palette-state-repeat-rate         gleui-palette-state-repeat-rate-set!)
  (repeat-delay        gleui-palette-state-repeat-delay        gleui-palette-state-repeat-delay-set!)
  (hit-boxes           gleui-palette-state-hit-boxes           gleui-palette-state-hit-boxes-set!))

;; global cached palette instance singleton
;; the palette is built assuming that there will be no 2 instances
;; running at the same time; resources are reused to improve performance.
(define-var *gleui-palette-instance* #f
            "Global cached <gleui-palette-state> singleton instance, or #f.")
(define-var *gleui-palette-previous-window* #f
            "Previously focused <window> record saved before opening the palette, or #f.")

(define (gleui-palette-instance-set! val)
  "Set the global palette singleton `*gleui-palette-instance*' to VAL.
Parameters: (VAL)
  - VAL: <gleui-palette-state> record, or #f."
  (set! *gleui-palette-instance* val))

(define (gleui-palette-previous-window-set! val)
  "Set `*gleui-palette-previous-window*' to VAL.
Parameters: (VAL)
  - VAL: <window> record of the previously focused window, or #f."
  (set! *gleui-palette-previous-window* val))

(define (gleui-palette-visible?)
  "Check whether the global palette instance is currently visible on screen.
Parameters: None.
Returns #t if visible, or #f otherwise."
  (and *gleui-palette-instance*
       (gleui-palette-state-visible? *gleui-palette-instance*)))

(define (gleui-palette-configured?)
  "Check whether the palette layer surface has received its initial configure event.
Parameters: None.
Returns #t if configured, or #f otherwise."
  (and *gleui-palette-instance*
       (gleui-palette-state-configured? *gleui-palette-instance*)))

(define (gleui-palette-max-lines state)
  "Resolve maximum visible candidate lines configured for STATE.
Parameters: (STATE)
  - STATE: <gleui-palette-state> record, or #f.
Returns positive integer line limit."
  (or (and state
           (list? (gleui-palette-state-theme-overrides state))
           (assq-ref (gleui-palette-state-theme-overrides state) 'lines))
      (var-get '*palette-candidates-count*)
      10))

(define (gleui-palette-adjust-scroll! state)
  "Adjust scroll offset in STATE so selected-index stays visible.
Parameters: (STATE)
  - STATE: <gleui-palette-state> record."
  (let* ((sel (gleui-palette-state-selected-index state))
         (scroll (gleui-palette-state-scroll-offset state))
         (max-lines (gleui-palette-max-lines state)))
    ;; keep the visible window clamped around selection
    (cond
     ((< sel scroll)
      (gleui-palette-state-scroll-offset-set! state sel))
     ((>= sel (+ scroll max-lines))
      (gleui-palette-state-scroll-offset-set! state (+ (- sel max-lines) 1))))))

(define (gleui-palette-calc-height state)
  "Calculate palette height in pixels for STATE.
Parameters: (STATE)
  - STATE: <gleui-palette-state> record.
Returns integer pixel height."
  (let* ((overrides (gleui-palette-state-theme-overrides state))
         (theme-get (lambda (k def) (gleui-theme-get overrides k def)))
         (font-size (or (theme-get 'font-size '*palette-font-size*) 13))
         (filtered-idxs (gleui-palette-state-filtered-indices state))
         (total-matching (if (list? filtered-idxs) (length filtered-idxs) 0))
         (max-lines (or (theme-get 'lines '*palette-candidates-count*) 10))
         ;; reserve at least one line height so the empty-state message still fits cleanly
         (visible-lines (min max-lines (max 1 total-matching)))
         (row-height (max 38 (+ font-size 22)))
         (input-height (max 42 (+ font-size 26)))
         (pad 16)
         (spacing 10))
    (+ (* 2 pad) input-height spacing 10 (* visible-lines row-height))))

(define* (gleui-palette-default-width #:optional (theme-overrides '()) (output #f))
  "Calculate palette width in pixels.
Parameters: ([THEME-OVERRIDES '()] [OUTPUT #f])
  - THEME-OVERRIDES: Alist of theme overrides.
  - OUTPUT: <output> record or #f for target output (#f uses current output).
Returns integer pixel width clamped to screen bounds."
  (let* ((target-output       (or output (output-current)))
         (target-output-width (if target-output (output-width target-output) 1920))
         (width-percent       (or (and (list? theme-overrides) (assq-ref theme-overrides 'width))
                                  *palette-width*
                                  60)))
    ;; clamp percentage width between 400px minimum and 40px below output width
    (max 400 (min (- target-output-width 40)
                  (inexact->exact (round (* target-output-width
                                            (/ width-percent 100.0))))))))

(define (gleui-palette-default-search-strings candidates)
  "Derive searchable text strings parallel to CANDIDATES.
Parameters: (CANDIDATES)
  - CANDIDATES: List or vector of candidate items.
Returns list of plain strings suitable for filtering."
  (let ((cands (if (vector? candidates) (vector->list candidates) (or candidates '()))))
    (map (lambda (cand)
           (cond
            ((string? cand) cand)
            ((list? cand) (string-join (map (lambda (x) (format #f "~a" x)) cand) " "))
            (else (format #f "~a" cand))))
         cands)))
