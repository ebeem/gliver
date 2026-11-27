;;; gliver/contrib/ui/gleui/palette/input.scm --- Palette Navigation & Keybindings
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module manages keyboard input management, and candidate
;;; selection navigation for gleui's palette.
;;; It handles:
;;;  - Binding wl_keyboard and managing xkbcommon keymap/state
;;;  - Translating key events and matching configurable palette keybindings

(define-module (gliver contrib ui gleui palette input)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-13)
  #:use-module (gliver core)
  #:use-module (gliver deps libc)
  #:use-module (gliver deps libxkbcommon)
  #:use-module (gliver wayland client)
  #:use-module (gliver wayland gen wayland)
  #:use-module (gliver river connector)
  #:use-module (gliver contrib ui gleui base)
  #:use-module (gliver contrib ui gleui palette state)
  #:use-module (gliver contrib ui gleui palette matching)
  #:use-module (gliver contrib ui gleui palette render)
  #:declarative? #f
  #:export (
            *palette-repeat-rate*
            *palette-repeat-delay*
            *palette-key-select*
            *palette-key-cancel*
            *palette-key-next*
            *palette-key-prev*
            *palette-key-first*
            *palette-key-last*
            *palette-key-page-down*
            *palette-key-page-up*
            *palette-key-delete-word-backward*
            *palette-key-delete-word-forward*
            *palette-key-delete-char-backward*
            *palette-key-delete-char-forward*
            *palette-key-kill-line*
            *palette-key-discard-line*
            *palette-key-word-backward*
            *palette-key-word-forward*
            *palette-key-bol*
            *palette-key-eol*
            *palette-key-char-backward*
            *palette-key-char-forward*
            gleui-palette-select-next!
            gleui-palette-select-prev!
            gleui-palette-page-down!
            gleui-palette-page-up!
            gleui-palette-select-first!
            gleui-palette-select-last!
            gleui-palette-refilter!
            *palette-kbd-cache*
            *palette-hide-proc*
            gleui-palette-set-hide-proc!
            gleui-palette-parse-key
            gleui-palette-single-key-match?
            gleui-palette-key-match?
            gleui-palette-handle-key!
            gleui-palette-release-keyboard!
            gleui-palette-ensure-keyboard!
))

;;; palette keybindings and repeat settings
(define-var *palette-repeat-rate* 25
            "Key repeat rate in characters per second.")
(define-var *palette-repeat-delay* 600
            "Delay in milliseconds before key repeat starts.")
(define-var *palette-key-select* '("Return" "KP_Enter" "C-m")
            "Keybinding list to select candidate or confirm input.")
(define-var *palette-key-cancel* '("Escape" "C-g" "C-c" "C-[")
            "Keybinding list to dismiss the palette.")
(define-var *palette-key-next* '("Down" "C-n" "C-j" "Tab")
            "Keybinding list to select next candidate.")
(define-var *palette-key-prev* '("Up" "C-p" "S-Tab" "ISO_Left_Tab")
            "Keybinding list to select previous candidate.")
(define-var *palette-key-first* '("M-<")
            "Keybinding list to jump to the first candidate.")
(define-var *palette-key-last* '("M->")
            "Keybinding list to jump to the last candidate.")
(define-var *palette-key-page-down* '("Page_Down" "C-v")
            "Keybinding list to scroll down one page.")
(define-var *palette-key-page-up* '("Page_Up" "M-v")
            "Keybinding list to scroll up one page.")
(define-var *palette-key-delete-word-backward* '("C-BackSpace" "M-BackSpace" "C-w")
            "Keybinding list to delete word backward.")
(define-var *palette-key-delete-word-forward* '("M-d")
            "Keybinding list to delete word forward.")
(define-var *palette-key-delete-char-backward* '("BackSpace" "C-h")
            "Keybinding list to delete character backward.")
(define-var *palette-key-delete-char-forward* '("Delete" "C-d")
            "Keybinding list to delete character forward.")
(define-var *palette-key-kill-line* '("C-k")
            "Keybinding list to kill text to end of line.")
(define-var *palette-key-discard-line* '("C-u")
            "Keybinding list to discard text to beginning of line.")
(define-var *palette-key-word-backward* '("M-b")
            "Keybinding list to move cursor backward by word.")
(define-var *palette-key-word-forward* '("M-f")
            "Keybinding list to move cursor forward by word.")
(define-var *palette-key-bol* '("Home" "C-a")
            "Keybinding list to move cursor to beginning of line.")
(define-var *palette-key-eol* '("End" "C-e")
            "Keybinding list to move cursor to end of line.")
(define-var *palette-key-char-backward* '("Left" "C-b")
            "Keybinding list to move cursor backward by character.")
(define-var *palette-key-char-forward* '("Right" "C-f")
            "Keybinding list to move cursor forward by character.")

(define (%select-index! state target-idx)
  "Select candidate TARGET-IDX in STATE and adjust scrolling.
Parameters: (STATE TARGET-IDX)
  - STATE: <gleui-palette-state> record.
  - TARGET-IDX: Integer index inside the filtered candidate list."
  (gleui-palette-state-selected-index-set! state target-idx)
  ;; if we select an index outside the current visible range, it
  ;; will be selected but not visible, adjust scrolling ensures
  ;; that we jump to it
  (gleui-palette-adjust-scroll! state)
  (gleui-palette-render! state))

(define (%on-pointer-row-click! state idx)
  "Confirm selection of filtered candidate IDX when clicked in palette STATE.
Parameters: (STATE IDX)
  - STATE: <gleui-palette-state> record.
  - IDX: Integer visible row index inside filtered indices."
  (let ((filtered-indices (gleui-palette-state-filtered-indices state)))
    (when (and (pair? filtered-indices)
               (>= idx 0)
               (< idx (length filtered-indices)))
      (gleui-palette-state-selected-index-set! state idx)
      (%finish-selection! state (list-ref filtered-indices idx) "on-select"))))

(define (%on-pointer-scroll-step! state delta)
  "Step candidate selection up or down in STATE according to scroll DELTA.
Parameters: (STATE DELTA)
  - STATE: <gleui-palette-state> record.
  - DELTA: Float signed vertical pointer axis delta."
  (cond
   ((> delta 0) (gleui-palette-select-next! state))
   ((< delta 0) (gleui-palette-select-prev! state))))

(gleui-palette-set-interaction-procs! %on-pointer-row-click!
                                      %on-pointer-scroll-step!)

(define (gleui-palette-select-next! state)
  "Move palette selection in STATE to the next candidate.
Parameters: (STATE)
  - STATE: <gleui-palette-state> record."
  (let ((candidate-count (length (gleui-palette-state-filtered-indices state)))
        (current-index   (gleui-palette-state-selected-index state)))
    (when (> candidate-count 0)
      (%select-index! state (if (< (1+ current-index) candidate-count)
                                (1+ current-index)
                                0)))))

(define (gleui-palette-select-prev! state)
  "Move palette selection in STATE to the previous candidate.
Parameters: (STATE)
  - STATE: <gleui-palette-state> record."
  (let ((candidate-count (length (gleui-palette-state-filtered-indices state)))
        (current-index   (gleui-palette-state-selected-index state)))
    (when (> candidate-count 0)
      (%select-index! state (if (> current-index 0)
                                (1- current-index)
                                (1- candidate-count))))))

(define (gleui-palette-page-down! state)
  "Advance palette selection in STATE downward by one visible page.
Parameters: (STATE)
  - STATE: <gleui-palette-state> record."
  (let ((candidate-count (length (gleui-palette-state-filtered-indices state)))
        (current-index   (gleui-palette-state-selected-index state))
        (max-lines       (gleui-palette-max-lines state)))
    (when (> candidate-count 0)
      (%select-index! state (min (1- candidate-count) (+ current-index max-lines))))))

(define (gleui-palette-page-up! state)
  "Move palette selection in STATE upward by one visible page.
Parameters: (STATE)
  - STATE: <gleui-palette-state> record."
  (let ((candidate-count (length (gleui-palette-state-filtered-indices state)))
        (current-index   (gleui-palette-state-selected-index state))
        (max-lines       (gleui-palette-max-lines state)))
    (when (> candidate-count 0)
      (%select-index! state (max 0 (- current-index max-lines))))))

(define (gleui-palette-select-first! state)
  "Jump palette selection in STATE to the first matching candidate.
Parameters: (STATE)
  - STATE: <gleui-palette-state> record."
  (when (pair? (gleui-palette-state-filtered-indices state))
    (gleui-palette-state-selected-index-set! state 0)
    (gleui-palette-state-scroll-offset-set! state 0)
    (gleui-palette-render! state)))

(define (gleui-palette-select-last! state)
  "Jump palette selection in STATE to the last matching candidate.
Parameters: (STATE)
  - STATE: <gleui-palette-state> record."
  (let ((candidate-count (length (gleui-palette-state-filtered-indices state))))
    (when (> candidate-count 0)
      (%select-index! state (1- candidate-count)))))

(define (gleui-palette-refilter! state)
  "Recompute filtered candidate indices in STATE for the current query.
Parameters: (STATE)
  - STATE: <gleui-palette-state> record."
  (let* ((candidates        (gleui-palette-state-candidates state))
         (query             (gleui-palette-state-filter-text state))
         (search-strings    (gleui-palette-state-search-strings state))
         (parsed-candidates (gleui-palette-state-parsed-candidates state))
         (filtered-indices  (gleui-palette-filter-candidates candidates
                                                             query
                                                             *palette-case-sensitive?*
                                                             search-strings
                                                             #:parsed-candidates parsed-candidates)))
    (gleui-palette-state-filtered-indices-set! state filtered-indices)
    (gleui-palette-state-selected-index-set! state 0)
    (gleui-palette-state-scroll-offset-set! state 0)
    (gleui-palette-render! state)))

;; navigation commands
(define (%update-filter! state new-str new-pos)
  "Set filter query to NEW-STR and cursor to NEW-POS in STATE
Parameters: (STATE NEW-STR NEW-POS)
  - STATE: <gleui-palette-state> record.
  - NEW-STR: String updated filter text.
  - NEW-POS: Integer updated character cursor index."
  (gleui-palette-state-filter-text-set! state new-str)
  (gleui-palette-state-cursor-pos-set! state new-pos)
  ;; we have to refilter now to refresh candidates with new
  ;; query results, refilter will re-render as well
  (gleui-palette-refilter! state))

(define (%move-cursor! state new-pos)
  "Move the filter cursor in STATE to NEW-POS if NEW-POS is within filter bounds.
Parameters: (STATE NEW-POS)
  - STATE: <gleui-palette-state> record.
  - NEW-POS: Integer target character cursor offset."
  (let ((filter-text (gleui-palette-state-filter-text state)))
    (when (and (string? filter-text)
               (exact-integer? new-pos)
               (<= 0 new-pos (string-length filter-text))
               (not (= new-pos (gleui-palette-state-cursor-pos state))))
      (gleui-palette-state-cursor-pos-set! state new-pos)
      (gleui-palette-render! state))))

(define (%finish-selection! state res err-label)
  "Close palette STATE with and return RES.
Parameters: (STATE RES ERR-LABEL)
  - STATE: <gleui-palette-state> record.
  - RES: Integer candidate index, string custom text, or #f when cancelled.
  - ERR-LABEL: String callback name label included in error log messages."
  (let ((async-callback (gleui-palette-state-async-callback state)))
    (gleui-palette-state-result-set! state res)
    (gleui-palette-state-done?-set! state #t)
    ;; invoke hide procedure if available
    (when (procedure? *palette-hide-proc*)
      (*palette-hide-proc* state))
    (when (procedure? async-callback)
      (catch #t
        (lambda () (async-callback res))
        (lambda (key . args)
          (log-error "gleui-palette: ~a callback error: ~a ~a" err-label key args))))))

(define (%confirm-selection! state filter)
  "Confirm the currently selected candidate.
Parameters: (STATE FILTER)
  - STATE: <gleui-palette-state> record.
  - FILTER: String current filter query."
  (let* ((filtered-indices (gleui-palette-state-filtered-indices state))
         (selected-index   (gleui-palette-state-selected-index state))
         (trimmed-filter   (string-trim-both filter))
         (selection-result (cond
                            ((and (pair? filtered-indices) (< selected-index (length filtered-indices)))
                             (list-ref filtered-indices selected-index))
                            ((not (string-null? trimmed-filter)) trimmed-filter)
                            (else #f))))
    (%finish-selection! state selection-result "on-select")))

(define (%delete-word-backward! state filter pos)
  "Delete the word preceding POS from FILTER in STATE.
Parameters: (STATE FILTER POS)
  - STATE: <gleui-palette-state> record.
  - FILTER: String current filter query.
  - POS: Integer current character cursor index."
  (when (> pos 0)
    (let ((word-start (gleui-word-backward-pos filter pos)))
      (%update-filter! state (string-append (string-take filter word-start) (string-drop filter pos)) word-start))))

(define (%delete-word-forward! state filter pos)
  "Delete the word following POS from FILTER in STATE.
Parameters: (STATE FILTER POS)
  - STATE: <gleui-palette-state> record.
  - FILTER: String current filter query.
  - POS: Integer current character cursor index."
  (when (< pos (string-length filter))
    (let ((word-end (gleui-word-forward-pos filter pos)))
      (%update-filter! state (string-append (string-take filter pos) (string-drop filter word-end)) pos))))

(define (%delete-char-backward! state filter pos)
  "Delete the character before POS in FILTER for STATE.
Parameters: (STATE FILTER POS)
  - STATE: <gleui-palette-state> record.
  - FILTER: String current filter query.
  - POS: Integer current character cursor index."
  (when (> pos 0)
    (%update-filter! state
                     (string-append
                      (string-take filter (1- pos))
                      (string-drop filter pos))
                     (1- pos))))

(define (%delete-char-forward! state filter pos)
  "Delete the character at POS in FILTER for STATE.
Parameters: (STATE FILTER POS)
  - STATE: <gleui-palette-state> record.
  - FILTER: String current filter query.
  - POS: Integer current character cursor index."
  (when (< pos (string-length filter))
    (%update-filter! state (string-append
                            (string-take filter pos)
                            (string-drop filter (1+ pos)))
                     pos)))

(define (%palette-key-action-table)
  "Build an alist mapping configured palette key patterns to handler procedures.
Parameters: None.
Returns alist of (PATTERN . HANDLER) entries."
  `((,*palette-key-select*
     . ,(lambda (st f _p)
          (%confirm-selection! st f)))
    (,*palette-key-cancel*
     . ,(lambda (st _f _p)
          (%finish-selection! st #f "on-cancel")))
    (,*palette-key-first*
     . ,(lambda (st _f _p)
          (gleui-palette-select-first! st)))
    (,*palette-key-last*
     . ,(lambda (st _f _p)
          (gleui-palette-select-last! st)))
    (,*palette-key-next*
     . ,(lambda (st _f _p)
          (gleui-palette-select-next! st)))
    (,*palette-key-prev*
     . ,(lambda (st _f _p)
          (gleui-palette-select-prev! st)))
    (,*palette-key-page-up*
     . ,(lambda (st _f _p)
          (gleui-palette-page-up! st)))
    (,*palette-key-page-down*
     . ,(lambda (st _f _p)
          (gleui-palette-page-down! st)))
    (,*palette-key-delete-word-backward* . ,%delete-word-backward!)
    (,*palette-key-delete-word-forward*  . ,%delete-word-forward!)
    (,*palette-key-delete-char-backward* . ,%delete-char-backward!)
    (,*palette-key-delete-char-forward*  . ,%delete-char-forward!)
    (,*palette-key-kill-line*
     . ,(lambda (st f p)
          (when (< p (string-length f))
            (%update-filter! st (string-take f p) p))))
    (,*palette-key-discard-line*
     . ,(lambda (st f p)
          (%update-filter! st (string-drop f p) 0)))
    (,*palette-key-word-backward*
     . ,(lambda (st f p)
          (%move-cursor! st (gleui-word-backward-pos f p))))
    (,*palette-key-word-forward*
     . ,(lambda (st f p)
          (%move-cursor! st (gleui-word-forward-pos f p))))
    (,*palette-key-bol*
     . ,(lambda (st _f _p)
          (%move-cursor! st 0)))
    (,*palette-key-eol*
     . ,(lambda (st f _p)
          (%move-cursor! st (string-length f))))
    (,*palette-key-char-backward*
     . ,(lambda (st _f p)
          (%move-cursor! st (1- p))))
    (,*palette-key-char-forward*
     . ,(lambda (st _f p)
          (%move-cursor! st (1+ p))))))

;;; key matching and xkb decoding
(define-var *palette-kbd-cache* (make-hash-table 64)
            "Cache mapping key specification strings to parsed <key> records.")

;; NOTE: populated by palette.scm after load to avoid circular module dependency
(define-var *palette-hide-proc* #f
            "Callback procedure invoked to hide the active palette surface.")

(define (gleui-palette-set-hide-proc! proc)
  "Register callback PROC used to hide the active palette surface.
Parameters: (PROC)
  - PROC: Procedure accepting one <gleui-palette-state> record."
  (set! *palette-hide-proc* proc))

;; the below functions are to handle wayland input
;; NOTE: this probably has to be its own module, current gliver
;;       core modules handle keybindings for the river protocol
;;       which differs from wayland's. However, this use-case key
;;       handling maybe very common in other modules in the future
(define (gleui-palette-parse-key spec)
  "Parse and cache key specification SPEC as a <key> record.
Parameters: (SPEC)
  - SPEC: String in Gliver kbd syntax or <key> record.
Returns <key> record, or #f if SPEC cannot be parsed."
  (cond
   ((key? spec) spec)
   ((string? spec)
    (or (hash-ref *palette-kbd-cache* spec)
        (let ((parsed-key (kbd spec)))
          (when parsed-key
            (hash-set! *palette-kbd-cache* spec parsed-key))
          parsed-key)))
   (else #f)))

(define (gleui-palette-single-key-match? spec keysym ctrl? alt? shift?)
  "Check whether single key SPEC matches KEYSYM.
Parameters: (SPEC KEYSYM CTRL? ALT? SHIFT?)
  - SPEC: String key specification or <key> record.
  - KEYSYM: Integer XKB keysym code.
  - CTRL?: Boolean indicating whether Control modifier is active.
  - ALT?: Boolean indicating whether Alt modifier is active.
  - SHIFT?: Boolean indicating whether Shift modifier is active.
Returns #t if SPEC matches the key event, #f otherwise."
  (let ((parsed-key (gleui-palette-parse-key spec)))
    (and parsed-key
         (let* ((modifiers       (key-modifiers parsed-key))
                (expected-ctrl?  (and (memq 'Control modifiers) #t))
                (expected-alt?   (and (memq 'Alt modifiers) #t))
                (expected-shift? (and (memq 'Shift modifiers) #t))
                (expected-keysym (key-keysym-name->xkb-value (key-keysym parsed-key))))
           (and (> expected-keysym 0)
                (or (= keysym expected-keysym)
                    ;; case-insensitive ascii letter match (e.g. C-n / C-N)
                    (and (>= expected-keysym 97)
                         (<= expected-keysym 122)
                         (= (+ keysym 32) expected-keysym))
                    ;; tab / shift-tab / iso_left_tab
                    (and (or (and expected-shift?
                                  (= expected-keysym 65289))
                             (= expected-keysym 65056))
                         (= keysym 65056)))
                (eq? (and ctrl? #t) (and expected-ctrl? #t))
                (eq? (and alt? #t)  (and expected-alt? #t))
                (or (eq? (and shift? #t) (and expected-shift? #t))
                    ;; allow shift if not explicitly required and keysym is non-letter (e.g. < or >)
                    (and (not expected-shift?)
                         (not (and (>= expected-keysym 97)
                                   (<= expected-keysym 122))))))))))

(define (gleui-palette-key-match? pattern keysym ctrl? alt? shift?)
  "Check whether PATTERN matches KEYSYM under.
Parameters: (PATTERN KEYSYM CTRL? ALT? SHIFT?)
  - PATTERN: String key specification, <key> record, or list of patterns.
  - KEYSYM: Integer XKB keysym code.
  - CTRL?: Boolean indicating whether Control modifier is active.
  - ALT?: Boolean indicating whether Alt modifier is active.
  - SHIFT?: Boolean indicating whether Shift modifier is active.
Returns #t if any pattern matches the key event, #f otherwise."
  (cond
   ((null? pattern) #f)
   ((pair? pattern)
    (or (gleui-palette-single-key-match? (car pattern) keysym ctrl? alt? shift?)
        (gleui-palette-key-match? (cdr pattern) keysym ctrl? alt? shift?)))
   (else
    (gleui-palette-single-key-match? pattern keysym ctrl? alt? shift?))))

(define (%xkb-modifiers xkb-state)
  "Query active Control, Alt, and Shift modifier flags from XKB state XKB-STATE.
Parameters: (XKB-STATE)
  - XKB-STATE: Pointer to xkb_state, or %null-pointer.
Returns three booleans: CTRL?, ALT?, and SHIFT?."
  (let ((has-xkb? (valid-pointer? xkb-state)))
    (define (active? name)
      (and has-xkb?
           (not (zero? (xkb-state-mod-name-is-active xkb-state name 1)))))
    (values (active? ctrl-name)
            (active? alt-name)
            (active? shift-name))))

(define (%xkb-decode-utf8 xkb-state keycode)
  "Decode a UTF-8 text string from XKB-STATE for KEYCODE.
Parameters: (XKB-STATE KEYCODE)
  - XKB-STATE: Pointer to xkb_state, or %null-pointer.
  - KEYCODE: Integer offset XKB keycode.
Returns decoded UTF-8 string, or #f when no printable character applies."
  (and (valid-pointer? xkb-state)
       (let* ((utf8-buffer  (make-bytevector 16 0))
              (utf8-pointer (bytevector->pointer utf8-buffer))
              (utf8-length  (xkb-state-key-get-utf8 xkb-state keycode utf8-pointer 16)))
         (and (> utf8-length 0)
              (pointer->string utf8-pointer utf8-length)))))

(define (gleui-palette-handle-key! state keycode keysym utf8-str)
  "Dispatch a keyboard event with KEYCODE, KEYSYM, and UTF8-STR.
Parameters: (STATE KEYCODE KEYSYM UTF8-STR)
  - STATE: <gleui-palette-state> record.
  - KEYCODE: Integer raw offset XKB keycode (utf8-str is used).
  - KEYSYM: Integer XKB keysym value.
  - UTF8-STR: String decoded UTF-8 character (from keycode), or #f."
  (let-values (((ctrl? alt? shift?) (%xkb-modifiers (gleui-palette-state-xkb-state state))))
    (let* ((filter-text     (gleui-palette-state-filter-text state))
           (cursor-pos      (gleui-palette-state-cursor-pos state))
           ;; find all keybindings from action table that match the key pressed
           ;; find will stop at first match, having multiple keybindings to different
           ;; command is invalid
           (matched-binding (find (lambda (entry)
                                    (gleui-palette-key-match? (car entry)
                                                              keysym ctrl? alt? shift?))
                                  (%palette-key-action-table))))
      ;; the debug keycode to see if there are problems with mapping it to utf8
      (log-debug "gleui-palette: handle-key keycode=~a keysym=~a utf8=~s ctrl=~a alt=~a shift=~a filter=~s"
                 keycode keysym utf8-str ctrl? alt? shift? filter-text)
      (cond
       ;; if we have a matching binding, execute its function (the cdr)
       (matched-binding
        ((cdr matched-binding) state filter-text cursor-pos))
       ;; otherwise, insert character if we have a printable one and
       ;; none of ctrl and alt are pressed
       ((and (not ctrl?) (not alt?)
             utf8-str
             (> (string-length utf8-str) 0)
             (>= (char->integer (string-ref utf8-str 0)) 32)
             (not (= (char->integer (string-ref utf8-str 0)) 127)))
        (%update-filter! state
                         (string-append (string-take filter-text cursor-pos)
                                        utf8-str
                                        (string-drop filter-text cursor-pos))
                         (+ cursor-pos (string-length utf8-str))))))))

;; below functions should belong to a wayland input module probably in gleui
;; but currently only palette uses keyboard input, they handle wl-keyboard lifecycle
;; like creation, keymap loading, and releases.
(define (%load-xkb-keymap! state keymap-fmt fd size)
  "Load an XKB keymap of KEYMAP-FMT and SIZE bytes from descriptor FD into STATE.
Parameters: (STATE KEYMAP-FMT FD SIZE)
  - STATE: <gleui-palette-state> record.
  - KEYMAP-FMT: Integer Wayland wl_keyboard keymap format (1 = XKB v1).
  - FD: Integer file descriptor containing the compositor keymap.
  - SIZE: Integer byte length of the keymap mapping."
  (log-debug "gleui-palette: on-keymap fmt=~a size=~a" keymap-fmt size)
  (when (and (= keymap-fmt 1) (> size 0))
    (let ((keymap-ptr (mmap %null-pointer size *prot-read* *map-shared* fd 0)))
      (when (valid-pointer? keymap-ptr)
        (let* ((xkb-context   (gleui-palette-state-xkb-ctx state))
               (xkb-keymap    (if xkb-keymap-new-from-buffer
                                  (xkb-keymap-new-from-buffer xkb-context keymap-ptr size 1 0)
                                  (xkb-keymap-new-from-string xkb-context keymap-ptr 1 0)))
               (new-xkb-state (and (valid-pointer? xkb-keymap) (xkb-state-new xkb-keymap))))
          (when (valid-pointer? new-xkb-state)
            (let ((previous-xkb-state  (gleui-palette-state-xkb-state state))
                  (previous-xkb-keymap (gleui-palette-state-xkb-keymap state)))
              ;; install new state before unreffing previous XKB handles
              (gleui-palette-state-xkb-keymap-set! state xkb-keymap)
              (gleui-palette-state-xkb-state-set! state new-xkb-state)
              (when (valid-pointer? previous-xkb-state)
                (xkb-state-unref previous-xkb-state))
              (when (valid-pointer? previous-xkb-keymap)
                (xkb-keymap-unref previous-xkb-keymap)))
            (log-debug "gleui-palette: keymap and xkb_state created")))
        (munmap keymap-ptr size))))
  (close-fd fd))

(define (%make-seat-keyboard-listener state)
  "Create a wl_keyboard listener structure to dispatch key events to STATE.
NOTE: should become another module along with other wayland input handlign.
Parameters: (STATE)
  - STATE: <gleui-palette-state> record.
Returns pointer to wl_keyboard listener FFI struct."
  (make-wl-keyboard-listener
   ;; on-keymap
   (lambda (_data _proxy keymap-fmt fd size)
     (%load-xkb-keymap! state keymap-fmt fd size))
   ;; on-enter
   (lambda (_data proxy _serial surface _keys-bv)
     (log-debug "gleui-palette: wl_keyboard entered proxy=~a surface=~a" proxy surface))
   ;; on-leave
   (lambda (_data proxy _serial surface)
     (gleui-repeat-worker-stop!)
     (log-debug "gleui-palette: wl_keyboard left proxy=~a surface=~a" proxy surface))
   ;; on-key
   (lambda (_data _proxy _serial _time key state-flag)
     (if (zero? state-flag)
         (gleui-repeat-worker-stop!)
         (when (gleui-palette-state-visible? state)
           (gleui-repeat-worker-stop!)
           ;; wayland/evdev keycodes are offset by 8 relative to XKB keycodes
           (let* ((keycode   (+ key 8))
                  (xkb-state (gleui-palette-state-xkb-state state)))
             (let-values (((ctrl? alt? shift?) (%xkb-modifiers xkb-state)))
               (let* ((keysym (if (valid-pointer? xkb-state)
                                  (xkb-state-key-get-one-sym xkb-state keycode)
                                  0))
                      (utf8-str (%xkb-decode-utf8 xkb-state keycode))
                      ;; never auto-repeat modifiers (65505..65518) or confirm/cancel actions
                      (non-repeatable?
                       (or (and (>= keysym 65505)
                                (<= keysym 65518))
                           (gleui-palette-key-match? *palette-key-select* keysym ctrl? alt? shift?)
                           (gleui-palette-key-match? *palette-key-cancel* keysym ctrl? alt? shift?))))
                 (gleui-palette-handle-key! state keycode keysym utf8-str)
                 (when (and (not non-repeatable?) (gleui-palette-state-visible? state) (not (gleui-palette-state-done? state)))
                   (gleui-repeat-worker-signal!
                    state
                    (vector keycode keysym utf8-str)
                    (lambda (st params)
                      (gleui-palette-handle-key! st
                                                 (vector-ref params 0)
                                                 (vector-ref params 1)
                                                 (vector-ref params 2)))
                    #:rate (gleui-palette-state-repeat-rate state)
                    #:delay (gleui-palette-state-repeat-delay state)
                    #:active-pred (lambda () (and (gleui-palette-state-visible? state) (not (gleui-palette-state-done? state))))))))))))
   ;; on-modifiers
   (lambda (_data _proxy _serial mods-dep mods-latch mods-lock group)
     (let ((xkb-state (gleui-palette-state-xkb-state state)))
       (when (valid-pointer? xkb-state)
         (xkb-state-update-mask xkb-state mods-dep mods-latch mods-lock 0 0 group))))
   ;; on-repeat-info
   (lambda (_data _proxy rate delay)
     (when (and (number? rate) (> rate 0))
       (gleui-palette-state-repeat-rate-set! state rate))
     (when (and (number? delay) (> delay 0))
       (gleui-palette-state-repeat-delay-set! state delay))
     (log-debug "gleui-palette: repeat info rate=~a delay=~a" rate delay))))

(define (gleui-palette-release-keyboard! state)
  "Invoked when a keyboard is release, should clean proxies attached to STATE.
Parameters: (STATE)
  - STATE: <gleui-palette-state> record, or #f."
  (when state
    (let ((seat-entries (gleui-palette-state-keyboard state)))
      (when (list? seat-entries)
        (for-each
         (lambda (seat-entry)
           (when (vector? seat-entry)
             (vector-set! seat-entry 4 #f)
             (let ((wl-seat  (vector-ref seat-entry 0))
                   (keyboard (vector-ref seat-entry 1)))
               (vector-set! seat-entry 1 #f)
               (vector-set! seat-entry 3 #f)
               (when (valid-pointer? keyboard)
                 (catch #t (lambda () (wl-keyboard-release keyboard)) (lambda _ #f)))
               (when (valid-pointer? wl-seat)
                 (catch #t (lambda () (wl-seat-release wl-seat)) (lambda _ #f))))))
         seat-entries))
      (gleui-palette-state-keyboard-set! state '()))))

(define (gleui-palette-ensure-keyboard! state)
  "Ensure wl_keyboard listeners are bound on seats.
Parameters: (STATE)
  - STATE: <gleui-palette-state> record, or #f."
  (gleui-repeat-worker-start!)
  (when (and state *wl-registry* *manager*)
    (gleui-palette-release-keyboard! state)
    (let ((seat-entries '()))
      (for-each
       (lambda (seat)
         (let* ((seat-object-id (and (seat? seat) (seat-wl-seat seat)))
                (wl-seat        (and (number? seat-object-id)
                                     (gliver-wl-registry-bind *wl-registry* seat-object-id *wl-seat-interface* 7))))
           (when (valid-pointer? wl-seat)
             ;; seat entry slots: #(wl-seat wl-keyboard seat-listener kb-listener active?)
             (letrec* ((seat-entry (vector wl-seat #f #f #f #t))
                       (on-capabilities
                        (lambda (_data _proxy caps)
                          (when (vector-ref seat-entry 4)
                            ;; bit 2 is WL_SEAT_CAPABILITY_KEYBOARD
                            (let ((has-keyboard?    (not (zero? (logand caps 2))))
                                  (current-keyboard (vector-ref seat-entry 1)))
                              (cond
                               ((and has-keyboard? (not (valid-pointer? current-keyboard)))
                                (let ((keyboard (wl-seat-get-keyboard wl-seat)))
                                  (when (valid-pointer? keyboard)
                                    (let ((keyboard-listener (%make-seat-keyboard-listener state)))
                                      ;; attach listener to execute mapping keybindings
                                      (wl-proxy-add-listener keyboard keyboard-listener %null-pointer)
                                      (vector-set! seat-entry 1 keyboard)
                                      (vector-set! seat-entry 3 keyboard-listener)
                                      (log-debug "gleui-palette: wl_keyboard attached for seat ~a" seat)))))
                               ((and (not has-keyboard?) (valid-pointer? current-keyboard))
                                (vector-set! seat-entry 1 #f)
                                (vector-set! seat-entry 3 #f)
                                (catch #t (lambda () (wl-keyboard-release current-keyboard)) (lambda _ #f))))))))
                       (seat-listener (make-wl-seat-listener on-capabilities (lambda _ #t))))
               (vector-set! seat-entry 2 seat-listener)
               (wl-proxy-add-listener wl-seat seat-listener %null-pointer)
               (set! seat-entries (cons seat-entry seat-entries))))))
       (manager-seats *manager*))
      (gleui-palette-state-keyboard-set! state seat-entries))))
