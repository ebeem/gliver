;;; gliver/contrib/ui/gleui/palette.scm --- Gleui Interactive Palette
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module orchestrates the gleui interactive candidate palette overlay,
;;; layer surface lifecycle, synchronous/asynchronous event loops, and backend
;;; installation.
;;; It handles:
;;;  - Initializing and reusing the cached palette singleton and XKB context
;;;  - Managing overlay wlr-layer-shell surface visibility and keyboard bindings
;;;  - Running synchronous selection loops or asynchronous callbacks
;;;
;;; NOTE: for simplicity, the palette is implemented in 5 files (in the below order):
;;;  - palette/state.scm: defines <gleui-palette-state> record and global instance
;;;  - palette/matching.scm: parses, scores, and filters candidate items
;;;  - palette/render.scm: builds declarative gleui widget trees and commits SHM buffers
;;;  - palette/input.scm: manages wl_keyboard seats, XKB decoding, and key/pointer navigation
;;;  - palette.scm: manages surface lifecycle, open/hide/update API, and event loop

(define-module (gliver contrib ui gleui palette)
  #:use-module (system foreign)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-2)
  #:use-module (gliver core)
  #:use-module (gliver deps libxkbcommon)
  #:use-module (gliver wayland client)
  #:use-module (gliver river connector)
  #:use-module (gliver river wlr-layer-shell)
  #:use-module (gliver contrib ui gleui base)
  #:use-module (gliver contrib ui gleui palette state)
  #:use-module (gliver contrib ui gleui palette matching)
  #:use-module (gliver contrib ui gleui palette render)
  #:use-module (gliver contrib ui gleui palette input)
  #:declarative? #f
  #:export (
            *palette-output*
            *palette-anchor-margin*
            *palette-location*
            gleui-palette-ensure-surface!
            gleui-palette-init!
            gleui-palette-open!
            gleui-palette-hide!
            gleui-palette-update!
            gleui-palette-show
            gleui-palette-cleanup!
            gleui-palette-on-surface-scale-changed
))

(re-export-modules (gliver contrib ui gleui palette state)
                   (gliver contrib ui gleui palette matching)
                   (gliver contrib ui gleui palette render)
                   (gliver contrib ui gleui palette input))

;;; palette output and location settings
(define-var *palette-output* #f
            "Name of the specific monitor/output to show the palette on, or #f for active.")
(define-var *palette-anchor-margin* 80
            "Margin used in palette form anchor direction defined in *palette-location*.")
(define-var *palette-location* 8
            "The integer value of the location of the palette
following the same order in a keyboard numpad
7 8 9
4 5 6
1 2 3")

(define (gleui-palette-ensure-surface! state)
  "Create and configure the Wayland surface and layer-surface for palette STATE.
Parameters: (STATE)
  - STATE: <gleui-palette-state> record, or #f."
  (when (and (gleui-palette-state? state)
             (or (not (valid-pointer? (gleui-palette-state-surface state)))
                 (not (valid-pointer? (gleui-palette-state-layer-surface state)))))
    (let* ((width         (gleui-palette-state-width state))
           (height        (gleui-palette-calc-height state))
           (anchor        (gleui-numpad->anchor *palette-location*))
           (target-output (output-find *palette-output*)))
      (gleui-layer-surface-create
       #:output target-output
       #:namespace "gliver-palette"
       #:layer *wlr-layer-shell-layer-overlay*
       #:anchor anchor
       #:margin-top *palette-anchor-margin*
       #:margin-bottom *palette-anchor-margin*
       #:exclusive-zone -1
       ;; exclusive keyboard focus ensures all keystrokes go to the palette while open
       #:keyboard-interactivity *wlr-layer-shell-interactivity-exclusive*
       #:width width
       #:height height
       #:on-created (lambda (surface layer-surface)
                      (gleui-palette-state-surface-set! state surface)
                      (gleui-palette-state-layer-surface-set! state layer-surface)
                      (gleui-palette-state-height-set! state height)
                      (gleui-palette-state-configured?-set! state #f)
                      ;; to register mouse/pointer hitboxes for candidate rows
                      (gleui-pointer-setup!)
                      (gleui-pointer-register-surface!
                       surface
                       ;; hitboxes have to be returned as a procedure
                       (lambda () (gleui-palette-state-hit-boxes state))))
       #:on-configure (lambda (configured-width configured-height)
                        (log-debug "gleui-palette: layer surface configured ~ax~a"
                                   configured-width configured-height)
                        (gleui-palette-state-configured?-set! state #t)
                        (when (gleui-palette-state-visible? state)
                          (gleui-palette-render! state)))
       #:on-close (lambda ()
                    (log-info "gleui-palette: layer surface closed")
                    (gleui-palette-hide! state))))))

(define (gleui-palette-init!)
  "Initialize the global palette state and keyboard listener.
NOTE: it's safe to keep calling this as it will skip initializing
      if an instance is already available
Parameters: None."
  (when (and *river-connected?*
             (valid-pointer? *wl-compositor*)
             (valid-pointer? *wlr-layer-shell*)
             (valid-pointer? *wl-shm*)
             (not *gleui-palette-instance*))
    (log-info "gleui-palette: initializing cached palette state")
    (let* ((current-output (output-current))
           (initial-width  (gleui-palette-default-width '() current-output))
           (initial-height 470)
           (xkb-context    (if xkb-context-new (xkb-context-new 0) %null-pointer))
           (state          (gleui-make-palette-state
                            #f #f #f 0 #f #f
                            initial-width initial-height current-output
                            %null-pointer xkb-context %null-pointer %null-pointer
                            '() #f '() #f '() "" 0 0 0 *palette-prompt* '() #f #f #f
                            (make-mutex)
                            *palette-repeat-rate*
                            *palette-repeat-delay*
                            '())))
      (gleui-palette-instance-set! state)
      (gleui-palette-ensure-keyboard! state)
      (log-info "gleui-palette: cached state initialized"))))

(define* (gleui-palette-open! candidates
                              #:key (search-strings #f)
                              (theme-overrides '())
                              (prompt #f)
                              (initial-filter "")
                              (on-select #f)
                              (on-cancel #f)
                              (on-change #f))
  "Open the cached palette view with CANDIDATES.
Parameters: (CANDIDATES [#:SEARCH-STRINGS #f] [#:THEME-OVERRIDES '()] [#:PROMPT #f] [#:INITIAL-FILTER \"\"] [#:ON-SELECT #f] [#:ON-CANCEL #f] [#:ON-CHANGE #f])
  - CANDIDATES: List or vector of candidate items or display strings.
  - SEARCH-STRINGS: List, vector, or #f of searchable strings parallel to CANDIDATES.
  - THEME-OVERRIDES: Alist of palette style overrides.
  - PROMPT: String or #f prompt label.
  - INITIAL-FILTER: String initial query populating the filter input.
  - ON-SELECT: Procedure or #f callback invoked with the selected candidate result.
  - ON-CANCEL: Procedure or #f zero-argument callback invoked when selection is cancelled.
  - ON-CHANGE: Procedure or #f callback for selection change notifications (planned, unavailable)."
  ;; ensure palette is initialized, it will skip if it is the case
  (gleui-palette-init!)
  (and-let* ((state *gleui-palette-instance*))
    (let* ((candidate-count         (if (list? candidates)
                                        (length candidates)
                                        (vector-length candidates)))
           (resolved-search-strings (or search-strings
                                        (gleui-palette-default-search-strings candidates)))
           ;; pre-parse candidates once on open so typing in the filter remains fast
           (parsed-candidates       (gleui-palette-parse-candidates candidates
                                                                    resolved-search-strings))
           (filtered-indices        (gleui-palette-filter-candidates candidates
                                                                     initial-filter
                                                                     *palette-case-sensitive?*
                                                                     resolved-search-strings
                                                                     #:parsed-candidates parsed-candidates))
           (current-output          (output-current))
           (width                   (gleui-palette-default-width theme-overrides current-output)))

      ;; update state with parameters
      (gleui-palette-state-candidates-set! state candidates)
      ;; display-candidates is populated lazily as rows are rendered
      (gleui-palette-state-display-candidates-set! state (make-vector candidate-count #f))
      (gleui-palette-state-search-strings-set! state resolved-search-strings)
      (gleui-palette-state-parsed-candidates-set! state parsed-candidates)
      (gleui-palette-state-filtered-indices-set! state filtered-indices)
      (gleui-palette-state-filter-text-set! state initial-filter)
      (gleui-palette-state-cursor-pos-set! state (string-length initial-filter))
      (gleui-palette-state-selected-index-set! state 0)
      (gleui-palette-state-scroll-offset-set! state 0)
      (gleui-palette-state-prompt-set! state
                                       (or prompt
                                           (and (list? theme-overrides)
                                                (assq-ref theme-overrides 'prompt))
                                           *palette-prompt*))
      (gleui-palette-state-theme-overrides-set! state theme-overrides)
      (gleui-palette-state-output-set! state (output-find (or *palette-output* current-output)))
      (gleui-palette-state-done?-set! state #f)
      (gleui-palette-state-result-set! state #f)
      (gleui-palette-state-async-callback-set! state
                                               (lambda (result)
                                                 (if result
                                                     (when (procedure? on-select) (on-select result))
                                                     (when (procedure? on-cancel) (on-cancel)))))
      (gleui-palette-state-width-set! state width)
      (gleui-palette-state-visible?-set! state #t)
      (gleui-palette-previous-window-set! (window-current))

      ;; ensure we have a valid keyboard, maybe last keyboard was
      ;; unplugged, in that case palette won't receive input
      (gleui-palette-ensure-keyboard! state)

      ;; render right away if already configured, otherwise let on-configure trigger the first paint
      (if (gleui-palette-state-configured? state)
          (gleui-palette-render! state)
          (begin
            (gleui-palette-ensure-surface! state)
            (when (and (gleui-palette-state-configured? state) (gleui-palette-state-visible? state))
              (gleui-palette-render! state)))))))

(define* (gleui-palette-hide! #:optional (state *gleui-palette-instance*))
  "Hide the active palette STATE, destroy its surface, and restore window focus.
Parameters: ([STATE *GLEUI-PALETTE-INSTANCE*])
  - STATE: <gleui-palette-state> record to hide, or #f."
  (when (and (gleui-palette-state? state)
             (gleui-palette-state-visible? state))
    (gleui-repeat-worker-stop!)
    (gleui-palette-state-visible?-set! state #f)
    (gleui-palette-state-configured?-set! state #f)
    (gleui-palette-state-candidates-set! state '())
    (gleui-palette-state-display-candidates-set! state #f)
    (gleui-palette-state-search-strings-set! state #f)
    (gleui-palette-state-parsed-candidates-set! state #f)
    (gleui-palette-state-filtered-indices-set! state '())
    (gleui-palette-state-filter-text-set! state "")
    (gleui-palette-state-cursor-pos-set! state 0)
    (gleui-palette-state-selected-index-set! state 0)
    (gleui-palette-state-scroll-offset-set! state 0)
    (gleui-palette-state-async-callback-set! state #f)

    (gleui-palette-release-keyboard! state)
    (gleui-pointer-unregister-surface! (gleui-palette-state-surface state))

    (gleui-layer-surface-destroy! (gleui-palette-state-surface state) (gleui-palette-state-layer-surface state))
    (gleui-palette-state-layer-surface-set! state #f)
    (gleui-palette-state-surface-set! state #f)

    (let ((current-window (window-current)))
      (gleui-palette-previous-window-set! #f)
      (when (and current-window (window? current-window))
        (window-focus! current-window #:force #t)))))

(define* (gleui-palette-update! #:key (candidates #f)
                                (filter #f)
                                (prompt #f)
                                (selected-index #f))
  "Update CANDIDATES, FILTER, PROMPT, or SELECTED-INDEX of the active palette.
Parameters: ([#:CANDIDATES #f] [#:FILTER #f] [#:PROMPT #f] [#:SELECTED-INDEX #f])
  - CANDIDATES: List, vector, or #f replacement candidate collection.
  - FILTER: String or #f replacement filter query.
  - PROMPT: String or #f replacement prompt label.
  - SELECTED-INDEX: Integer or #f replacement selected candidate index."
  (and-let* ((state *gleui-palette-instance*)
             ((gleui-palette-state? state)))
    (when candidates
      (let* ((candidate-count   (if (list? candidates)
                                    (length candidates)
                                    (vector-length candidates)))
             (search-strings    (gleui-palette-default-search-strings candidates))
             (parsed-candidates (gleui-palette-parse-candidates candidates search-strings)))
        (gleui-palette-state-candidates-set! state candidates)
        (gleui-palette-state-display-candidates-set! state (make-vector candidate-count #f))
        (gleui-palette-state-search-strings-set! state search-strings)
        (gleui-palette-state-parsed-candidates-set! state parsed-candidates)))
    (when prompt
      (gleui-palette-state-prompt-set! state prompt))
    (when filter
      (gleui-palette-state-filter-text-set! state filter)
      (gleui-palette-state-cursor-pos-set! state (string-length filter)))
    (when selected-index
      (gleui-palette-state-selected-index-set! state selected-index))
    (gleui-palette-refilter! state)))

(define (%run-blocking-event-loop! state)
  "Poll and dispatch Wayland events until palette STATE is closed.
Parameters: (STATE)
  - STATE: <gleui-palette-state> record of the active palette."
  (while (and (gleui-palette-state-visible? state)
              (not (gleui-palette-state-done? state)))
    (catch #t
      (lambda ()
        (when *wl-display*
          (wl-display-flush *wl-display*)
          (when (< (wl-display-dispatch-pending *wl-display*) 0)
            (gleui-palette-state-done?-set! state #t))
          ;; wait up to 50ms for new Wayland events so visibility/connection changes still wake us
          (let* ((display-fd (wl-display-get-fd *wl-display*))
                 (ready-fds  (select (list display-fd) '() '() 0 50000)))
            (when (and (pair? (car ready-fds))
                       (< (wl-display-dispatch *wl-display*) 0))
              (gleui-palette-state-done?-set! state #t)))))
      (lambda (key . args)
        (log-error "gleui-palette: Event loop error ~a ~a" key args)
        (gleui-palette-state-done?-set! state #t)))))

(define (%resolve-palette-selection selected-index keys)
  "Map SELECTED-INDEX through optional KEYS list.
Parameters: (SELECTED-INDEX KEYS)
  - SELECTED-INDEX: Integer selected candidate index.
  - KEYS: List of mapped key values corresponding to candidates.
Returns mapped key from KEYS if SELECTED-INDEX is in bounds, otherwise SELECTED-INDEX."
  (if (and (integer? selected-index)
           (>= selected-index 0)
           (pair? keys)
           (< selected-index (length keys)))
      (list-ref keys selected-index)
      selected-index))

(define* (gleui-palette-show candidates #:key (keys '()) (theme-overrides '())
                             (prompt #f) (initial-filter "")
                             (on-select #f) (on-cancel #f))
  "Prompt the user to choose from CANDIDATES asynchronously or synchronously.
Parameters: (CANDIDATES [#:KEYS '()] [#:THEME-OVERRIDES '()]
[#:PROMPT #f] [#:INITIAL-FILTER \"\"] [#:ON-SELECT #f] [#:ON-CANCEL #f])
  - CANDIDATES: List or vector of candidate items or display strings.
  - KEYS: List of values mapped by candidate index.
  - THEME-OVERRIDES: Alist of palette style overrides.
  - PROMPT: String or #f prompt label (#f inherits)
  - INITIAL-FILTER: String initial query populating the filter input.
  - ON-SELECT: Procedure or #f callback opening the palette asynchronously.
  - ON-CANCEL: Procedure or #f cancellation callback used in asynchronous mode.
Returns selected result or #f in synchronous mode (unspecified in asynchronous mode)."
  ;; run asynchronously when given an on-select callback, otherwise block until selection finishes
  (if (procedure? on-select)
      (gleui-palette-open! candidates
                           #:theme-overrides theme-overrides
                           #:prompt prompt
                           #:initial-filter initial-filter
                           #:on-select (lambda (selected-index)
                                         (on-select (%resolve-palette-selection selected-index keys)))
                           #:on-cancel on-cancel)
      (begin
        (gleui-palette-open! candidates
                             #:theme-overrides theme-overrides
                             #:prompt prompt
                             #:initial-filter initial-filter)
        (and-let* ((state *gleui-palette-instance*))
          (%run-blocking-event-loop! state)
          (let ((selection-result (%resolve-palette-selection (gleui-palette-state-result state) keys)))
            (gleui-palette-hide! state)
            selection-result)))))

(define (gleui-palette-cleanup!)
  "Destroy cached palette surfaces, SHM slots, and XKB state on shutdown.
Parameters: None."
  (when *gleui-palette-instance*
    (let* ((state       *gleui-palette-instance*)
           (shm-slots   (gleui-palette-state-shm-slots state))
           (xkb-state   (gleui-palette-state-xkb-state state))
           (xkb-keymap  (gleui-palette-state-xkb-keymap state))
           (xkb-context (gleui-palette-state-xkb-ctx state)))
      (set! *gleui-palette-instance* #f)
      (gleui-palette-hide! state)

      (gleui-shm-slots-destroy! shm-slots)
      (gleui-palette-state-shm-slots-set! state #f)

      ;; unref XKB objects in child-to-parent order state, keymap, context
      (gleui-palette-state-xkb-state-set! state %null-pointer)
      (gleui-palette-state-xkb-keymap-set! state %null-pointer)
      (gleui-palette-state-xkb-ctx-set! state %null-pointer)
      (when (valid-pointer? xkb-state)
        (xkb-state-unref xkb-state))
      (when (valid-pointer? xkb-keymap)
        (xkb-keymap-unref xkb-keymap))
      (when (valid-pointer? xkb-context)
        (xkb-context-unref xkb-context))

      (gleui-palette-release-keyboard! state))))

(define* (gleui-palette-on-surface-scale-changed surface scale)
  "Re-render the palette if SURFACE matches the active instance when SCALE updates.
Parameters: (SURFACE SCALE)
  - SURFACE: Pointer to Wayland wl_surface whose fractional scale updated.
  - SCALE: Float updated surface scale factor (unused)."
  (when (and *gleui-palette-instance*
             (gleui-palette-state-surface *gleui-palette-instance*)
             (equal? surface (gleui-palette-state-surface *gleui-palette-instance*)))
    (gleui-palette-render! *gleui-palette-instance*)))

;; break circular dependency by giving palette/input the hide handler after load
(gleui-palette-set-hide-proc! gleui-palette-hide!)
(gliver-hook-add! *surface-scale-changed-hook* 'gleui-palette-on-surface-scale-changed)
(gliver-hook-add! *gliver-globals-unbind-hook* 'gleui-palette-cleanup!)
