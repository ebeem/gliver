;;; gliver/contrib/ui/gleui/palette.scm --- Native Cairo & Pango Interactive Palette
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver contrib ui gleui palette)
  #:use-module (cairo)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (gliver core)
  #:use-module (gliver deps libc)
  #:use-module (gliver deps libxkbcommon)
  #:use-module (gliver deps color)
  #:use-module (gliver deps pango)
  #:use-module (gliver wayland client)
  #:use-module (gliver wayland gen wayland)
  #:use-module (gliver wayland gen wlr-layer-shell-unstable-v1)
  #:use-module (gliver river connector)
  #:use-module (gliver contrib ui components palette)
  #:use-module (gliver contrib ui gleui base)
  #:declarative? #f
  #:export (
			gleui-palette-state?
			*gleui-palette-instance*
			gleui-palette-visible?
			gleui-palette-configured?
			gleui-palette-candidate-matches?
			gleui-palette-filter-candidates
			gleui-palette-item-formatter
			gleui-palette-safe-col-string
			gleui-palette-make-dmenu-options
			gleui-palette-get-display-string
			gleui-palette-draw-surface!
			gleui-palette-render!
			gleui-palette-adjust-scroll!
			gleui-palette-select-next!
			gleui-palette-select-prev!
			gleui-palette-page-down!
			gleui-palette-page-up!
			gleui-palette-select-first!
			gleui-palette-select-last!
			gleui-palette-refilter!
			gleui-palette-key-match?
			gleui-palette-handle-key!
			gleui-palette-ensure-keyboard!
			gleui-palette-calc-height
			gleui-palette-ensure-surface!
			gleui-palette-init!
			gleui-palette-open!
			gleui-palette-hide!
			gleui-palette-close!
			gleui-palette-kill
			gleui-palette-update!
			gleui-palette-show
			gleui-palette-cleanup!
			gleui-palette-install!
))

(define-record-type <gleui-palette-state>
  (%make-gleui-palette-state surface layer-surface shm-slots shm-active-idx configured? visible?
                             width height output
                             keyboard keyboard-listener
                             xkb-ctx xkb-keymap xkb-state
                             candidates display-candidates search-strings
                             filtered-indices filter-text cursor-pos
                             selected-index scroll-offset prompt theme-overrides
                             done? result async-callback
                             render-mutex filter-gen repeat-rate repeat-delay)
  gleui-palette-state?
  (surface             %gps-surface             %gps-surface-set!)
  (layer-surface       %gps-layer-surface       %gps-layer-surface-set!)
  (shm-slots           %gps-shm-slots           %gps-shm-slots-set!)
  (shm-active-idx      %gps-shm-active-idx      %gps-shm-active-idx-set!)
  (configured?         %gps-configured?         %gps-configured?-set!)
  (visible?            %gps-visible?            %gps-visible?-set!)
  (width               %gps-width               %gps-width-set!)
  (height              %gps-height              %gps-height-set!)
  (output              %gps-output              %gps-output-set!)
  (keyboard            %gps-keyboard            %gps-keyboard-set!)
  (keyboard-listener   %gps-keyboard-listener   %gps-keyboard-listener-set!)
  (xkb-ctx             %gps-xkb-ctx             %gps-xkb-ctx-set!)
  (xkb-keymap          %gps-xkb-keymap          %gps-xkb-keymap-set!)
  (xkb-state           %gps-xkb-state           %gps-xkb-state-set!)
  (candidates          %gps-candidates          %gps-candidates-set!)
  (display-candidates  %gps-display-candidates  %gps-display-candidates-set!)
  (search-strings      %gps-search-strings      %gps-search-strings-set!)
  (filtered-indices    %gps-filtered-indices    %gps-filtered-indices-set!)
  (filter-text         %gps-filter-text         %gps-filter-text-set!)
  (cursor-pos          %gps-cursor-pos          %gps-cursor-pos-set!)
  (selected-index      %gps-selected-index      %gps-selected-index-set!)
  (scroll-offset       %gps-scroll-offset       %gps-scroll-offset-set!)
  (prompt              %gps-prompt              %gps-prompt-set!)
  (theme-overrides     %gps-theme-overrides     %gps-theme-overrides-set!)
  (done?               %gps-done?               %gps-done?-set!)
  (result              %gps-result              %gps-result-set!)
  (async-callback      %gps-async-callback      %gps-async-callback-set!)
  (render-mutex        %gps-render-mutex)
  (filter-gen          %gps-filter-gen          %gps-filter-gen-set!)
  (repeat-rate         %gps-repeat-rate         %gps-repeat-rate-set!)
  (repeat-delay        %gps-repeat-delay        %gps-repeat-delay-set!))

;; global cached palette instance singleton
;; the palette is built assuming that there will be no 2 instances
;; running at the same time, the resources are all reused to improve performance
(define *gleui-palette-instance* #f)

(define (gleui-palette-visible?)
  "Returns #t if the global palette instance exists and
is currently visible on screen."
  (and *gleui-palette-instance*
       (%gps-visible? *gleui-palette-instance*)))

(define (gleui-palette-configured?)
  "Returns #t if the palette's layer surface has received
its initial Wayland configure event from the compositor."
  (and *gleui-palette-instance*
       (%gps-configured? *gleui-palette-instance*)))

;; TODO: called per candidate, at least query operations like downcase
;; and split can be cached in `gleui-palette-filter-candidates` to improve performance.
;; TODO: think about making a filter backend interface so other file-methods
;; can be supplied like `orderless`
(define (gleui-palette-candidate-matches? query search-text case-sensitive?)
  "Return #t if SEARCH-TEXT matches tokens in QUERY."
  (if (or (string-null? query) (string-null? (string-trim-both query)))
      #t
      (let* ((q (if case-sensitive? query (string-downcase query)))
             (t (if case-sensitive? search-text (string-downcase search-text)))
             (terms (filter (lambda (s) (> (string-length s) 0)) (string-split q #\space))))
        (every (lambda (term) (string-contains t term)) terms))))

(define (gleui-palette-filter-candidates search-strings query case-sensitive?)
  "Filter SEARCH-STRINGS against QUERY, returns a list of matching indices."
  (let loop ((rest search-strings) (idx 0) (acc '()))
    (if (null? rest)
        (reverse acc)
        (let ((matches? (gleui-palette-candidate-matches? query (car rest) case-sensitive?)))
          (loop (cdr rest) (1+ idx) (if matches? (cons idx acc) acc))))))

(define (gleui-palette-item-formatter item)
  "Format an item into a tab-delimited or plain string.
The palette accepts a list such as (app-name app-desc)"
  (cond
   ((string? item) item)
   ((list? item)
    (string-join (map (lambda (x) (format #f "~a" x)) item) "\t"))
   (else (format #f "~a" item))))

(define* (gleui-palette-safe-col-string col #:optional (width 80))
  "Safely convert COL to string without unbounded expansion of large objects."
  (cond
   ((string? col) col)
   ((or (number? col) (boolean? col) (symbol? col) (char? col) (null? col))
    (format #f "~a" col))
   (else
    (or (false-if-exception
		 ;; sounds ugly but needed, passing objects here could lead to
		 ;; infinite loop if circular references are used. using format #f
		 ;; and truncating works most of the time except for the mentioned case
         (call-with-output-string
           (lambda (p) (truncated-print col p #:width width))))
        "#<object>"))))

;; TODO: searchable parameter is supposed to limit the columns that can be
;; searched, it's not implemented yet
(define* (gleui-palette-make-dmenu-options options colors #:key (pango #t)
                                           (widths '()) (searchable '()) (visible '()))
  "Returns a list of Pango-markup formatted rows from a list of column values.
Compatible with rofi/dmenu candidate format."
  ;; convert column to vectors once to avoid list-ref and length scans per entry
  (define visible-v (list->vector visible))
  (define widths-v  (list->vector widths))
  (define colors-v  (list->vector colors))

  (define (vec-ref vec idx default)
    (if (< idx (vector-length vec))
        (vector-ref vec idx)
        default))

  (define (truncate-or-pad str width)
    (cond
     ((not width) str)
     ((> (string-length str) width)
      (string-append (string-take str (max 0 (- width 3))) "..."))
     (else
      (string-pad-right str width #\space))))

  (define (format-column col idx)
    (and (vec-ref visible-v idx #t)
         (let* ((str     (gleui-palette-safe-col-string col))
                (width   (vec-ref widths-v idx #f))
                (color   (and pango (vec-ref colors-v idx #f)))
                (escaped (gleui-pango-escape (truncate-or-pad str width))))
           (if (and color (not (string-null? str)))
               (format #f "<span color='~a'>~a</span>" color escaped)
               escaped))))

  (define (format-row columns)
    (let loop ((cols (if (list? columns) columns (list columns)))
               (idx  0)
               (acc  '()))
      (if (null? cols)
          (string-join (reverse! acc) "   ")
          (let ((cell (format-column (car cols) idx)))
            (loop (cdr cols)
                  (1+ idx)
                  (if cell (cons cell acc) acc))))))

  (map format-row options))

(define (gleui-palette-get-display-string state idx)
  "Format display string for candidate at IDX (original index).
The formatted string is cached in disp-cache to avoid recalculation."
  (let* ((disp-cache (%gps-display-candidates state))
         (cands (%gps-candidates state))
         (num-cands (if (vector? cands) (vector-length cands) (length cands))))
    (if (or (< idx 0) (>= idx num-cands))
        ""
        (or (vector-ref disp-cache idx)
            (let* ((item (if (vector? cands) (vector-ref cands idx) (list-ref cands idx)))
                   (formatted
                    (cond
                     ((pair? item) (format #f "~a" (car item)))
                     ((string? item) item)
                     ((list? item) (gleui-palette-item-formatter item))
                     (else (format #f "~a" item)))))
              (vector-set! disp-cache idx formatted)
              formatted)))))

;; TODO: make all vars config parameters rather than hardcoded
(define (gleui-palette-draw-surface! cr width height state overrides)
  "Render all palette elements to Cairo context CR."
  (cairo-set-operator cr 'clear)
  (cairo-paint cr)
  (cairo-set-operator cr 'over)

  (let* ((theme-get (lambda (k def) (gleui-theme-get overrides k def)))
         (font-family (theme-get 'font '*palette-font*))
         (font-size (theme-get 'font-size '*palette-font-size*))
         (font-str (format #f "~a ~a" font-family font-size))
         (font-small (format #f "~a ~a" font-family (max 9 (- font-size 2))))

         (fg-color (theme-get 'foreground '*palette-fg-color*))
         (selected-color (theme-get 'selected '*palette-selected-fg-color*))
         (border-color (theme-get 'border-color '*palette-border-color*))
         (border-width (theme-get 'border-width '*palette-border-width*))
         (border-radius (theme-get 'border-radius '*palette-border-radius*))
         (prompt-color (theme-get 'prompt-color '*palette-prompt-color*))

         (prompt-str (or (%gps-prompt state)
						 (theme-get 'prompt '*palette-prompt*)))
         (filter-text (%gps-filter-text state))
         (cursor-pos (%gps-cursor-pos state))
         (filtered-idxs (%gps-filtered-indices state))
         (sel-idx (%gps-selected-index state))
         (scroll-off (%gps-scroll-offset state))

         (total-matching (length filtered-idxs))
         (max-lines (theme-get 'lines '*palette-candidates-count*))
         (visible-lines (min max-lines (max 1 total-matching)))

         (row-height (max 38 (+ font-size 22)))
         (input-height (max 42 (+ font-size 26)))
         (pad 16)
         (spacing 10))

    ;; draw background surface
	(let* ((bg-color (theme-get 'background '*palette-bg-color*))
		   (bg-color-rgba (parse-hex-color-rgba bg-color)))
      (apply (lambda (r g b a) (cairo-set-source-rgba cr r g b a)) bg-color-rgba)
      (gleui-draw-rounded-rect cr 0 0 width height border-radius)
      (cairo-fill cr))

    ;; draw borders if border-width is more than 0
	;; borders are disabled if the width is 0
    (when (and (number? border-width) (> border-width 0))
      (let ((border-color-rgba (parse-hex-color-rgba border-color))
            (half-w (/ border-width 2.0)))
        (apply (lambda (r g b a) (cairo-set-source-rgba cr r g b a)) border-color-rgba)
        (cairo-set-line-width cr border-width)
        (gleui-draw-rounded-rect cr half-w half-w (- width border-width) (- height border-width) border-radius)
        (cairo-stroke cr)))

    ;; draw input bar box
    (let* ((input-x pad)
           (input-y pad)
           (input-w (- width (* 2 pad)))
           (input-bg (parse-hex-color-rgba (theme-get 'input-bg '*palette-input-bg-color*)))
           (input-border (parse-hex-color-rgba (theme-get 'input-border '*palette-input-border-color*))))
      (apply (lambda (r g b a) (cairo-set-source-rgba cr r g b a)) input-bg)
      (gleui-draw-rounded-rect cr input-x input-y input-w input-height (max 4 (- border-radius 2)))
      (cairo-fill cr)

      (apply (lambda (r g b a) (cairo-set-source-rgba cr r g b a)) input-border)
      (cairo-set-line-width cr 1.0)
      (gleui-draw-rounded-rect cr (+ input-x 0.5) (+ input-y 0.5) (- input-w 1) (- input-height 1) (max 4 (- border-radius 2)))
      (cairo-stroke cr)

      ;; draw prompt and filter text
      (let* ((escaped-prompt (format #f "<span color='~a'><b>~a</b></span>"
                                     prompt-color (gleui-pango-escape prompt-str)))
             (text-before (string-take filter-text (min cursor-pos (string-length filter-text))))
             (escaped-before (gleui-pango-escape text-before))
             (text-after (string-drop filter-text (min cursor-pos (string-length filter-text))))
             (escaped-after (gleui-pango-escape text-after))
             (input-markup (string-append escaped-prompt "<span color='" fg-color "'>" escaped-before escaped-after "</span>")))

        (call-with-values
			(lambda () (pango-measure-text cr input-markup #:font font-str))
          (lambda (iw ih)
            (let ((input-text-y (+ input-y (/ (- input-height ih) 2.0))))
              (cairo-move-to cr (+ input-x 12) input-text-y)
              (pango-draw-text cr input-markup #:font font-str))))

        ;; draw cursor at cursor position
        (call-with-values
			(lambda () (pango-measure-text cr escaped-prompt #:font font-str))
          (lambda (prompt-w prompt-h)
            (call-with-values
				(lambda () (pango-measure-text cr (string-append "<span color='" fg-color "'>" escaped-before "</span>")
                                               #:font font-str))
              (lambda (before-w before-h)
                (let ((cursor-color (theme-get 'cursor-color '*palette-cursor-color*))
					  (cursor-x (+ input-x 12 prompt-w before-w))
                      (cursor-y (+ input-y 8))
					  (cursor-w (theme-get 'cursor-width '*palette-cursor-width*))
                      (cursor-h (- input-height 16)))
                  (apply (lambda (r g b a) (cairo-set-source-rgba cr r g b a))
                         (parse-hex-color-rgba cursor-color))
                  (cairo-rectangle cr cursor-x cursor-y cursor-w cursor-h)
                  (cairo-fill cr))))))

        ;; draw candidate count badge on right
        (let* ((counter-color (theme-get 'candidates-count-color '*palette-candidates-count-color*))
			   (empty-counter-color (theme-get 'no-candidates-count-color '*palette-no-candidates-count-color*))
			   (count-str (format #f "<span color='~a'>[~a/~a]</span>"
                                  (if (zero? total-matching) empty-counter-color counter-color)
                                  (if (zero? total-matching) 0 (1+ sel-idx))
                                  total-matching))
               (count-w (call-with-values
							(lambda () (pango-measure-text cr count-str #:font font-small))
                          (lambda (cw ch) cw))))
          (cairo-move-to cr (- width pad 12 count-w) (+ input-y (/ (- input-height font-size) 2.0) -2))
          (pango-draw-text cr count-str #:font font-small))))

    ;; draw listview rows
    (let ((list-start-y (+ pad input-height spacing 6))
		  (no-cadidate-text-color (theme-get 'no-candidates-text-color '*palette-no-candidates-text-color*)))
      (if (zero? total-matching)
          ;; empty state
          (begin
            (cairo-move-to cr (+ pad 12) (+ list-start-y 12))
            (pango-draw-text cr
							 (format #f "<span color='~a'>No matching candidates</span>" no-cadidate-text-color)
                             #:font font-str))
          ;; candidate rows
          (let loop ((line-i 0)
                     (curr-idx scroll-off)
                     (curr-y list-start-y))
            (when (and (< line-i visible-lines) (< curr-idx total-matching))
              (let* ((cand-orig-idx (list-ref filtered-idxs curr-idx))
					 (selection-radius (theme-get 'selected-radius '*palette-selected-radius*))
                     (raw-str (gleui-palette-get-display-string state cand-orig-idx))
                     (match-color (theme-get 'match-color '*palette-match-color*))
                     (cand-str (gleui-markup-string-highlight raw-str filter-text match-color))
                     (is-selected? (= curr-idx sel-idx))
                     (row-w (- width (* 2 pad)))
                     (pill-margin 2)
                     (pill-y (+ curr-y pill-margin))
                     (pill-h (- row-height (* 2 pill-margin))))

                ;; selection highlight background
                (when is-selected?
                  (apply (lambda (r g b a) (cairo-set-source-rgba cr r g b a))
                         (parse-hex-color-rgba (theme-get 'bg-active *palette-selected-bg-color*)))
                  (gleui-draw-rounded-rect cr pad pill-y row-w pill-h selection-radius)
                  (cairo-fill cr)

                  ;; left accent bar indicator
                  (let* ((bar-h pill-h)
                         (bar-y (+ pill-y (/ (- pill-h bar-h) 2.0))))
                    (apply (lambda (r g b a) (cairo-set-source-rgba cr r g b a))
                           (parse-hex-color-rgba selected-color))
                    (gleui-draw-rounded-rect cr pad bar-y 4.0 bar-h 0)
                    (cairo-fill cr)))

                ;; candidate row text
                (apply (lambda (r g b a) (cairo-set-source-rgba cr r g b a))
                       (parse-hex-color-rgba (if is-selected?
                                                 (theme-get 'selected-fg-color '*palette-selected-fg-color*)
                                                 (theme-get 'fg-color '*palette-fg-color*))))

                ;; vertically center text layout within row
                (call-with-values
					(lambda () (pango-measure-text cr cand-str #:font font-str))
                  (lambda (tw th)
                    (let ((text-y (+ curr-y (/ (- row-height th) 2.0))))
                      (cairo-move-to cr (+ pad (if is-selected? 18 12)) text-y)
                      (pango-draw-text cr cand-str
                                       #:font font-str
                                       #:width (- row-w 28)
                                       #:ellipsize 'end))))

                (loop (1+ line-i) (1+ curr-idx) (+ curr-y row-height)))))))))

(define (gleui-palette-render! state)
  "Render the palette view into persistent double-buffered shared memory and commit."
  (when (and state
             (%gps-visible? state)
             (%gps-configured? state)
             *wl-display*
             (pointer? *wl-compositor*)
             (not (null-pointer? *wl-compositor*))
             (pointer? *wl-shm*)
             (not (null-pointer? *wl-shm*)))
    (with-mutex (%gps-render-mutex state)
      (let* ((surface (%gps-surface state))
             (overrides (%gps-theme-overrides state))
             (w (%gps-width state))
			 (h (gleui-palette-calc-height state)))
        (when (and surface (pointer? surface) (not (null-pointer? surface)) (> w 0) (> h 0))
          (let ((valid-slots (gleui-shm-slots-ensure! (%gps-shm-slots state) w h)))
            (%gps-shm-slots-set! state valid-slots)
            (when (and (pair? valid-slots) (car valid-slots) (cdr valid-slots))
              (let* ((next-idx (if (zero? (%gps-shm-active-idx state)) 1 0))
                     (curr-slot (if (zero? next-idx) (car valid-slots) (cdr valid-slots)))
                     (bv (gleui-shm-slot-bv curr-slot))
                     (stride (gleui-shm-slot-stride curr-slot))
                     (wl-buf (gleui-shm-slot-buffer curr-slot))
                     (layer-surf (%gps-layer-surface state)))

                (%gps-shm-active-idx-set! state next-idx)
                (%gps-height-set! state h)

                (let* ((dst-surf (cairo-image-surface-create-for-data bv 'argb32 w h stride))
                       (cr (cairo-create dst-surf)))
                  (gleui-palette-draw-surface! cr w h state overrides)
                  (cairo-surface-flush dst-surf)
                  (cairo-destroy cr)
                  (cairo-surface-destroy dst-surf))

                (when (and layer-surf (pointer? layer-surf) (not (null-pointer? layer-surf)))
                  (zwlr-layer-surface-v1-set-size layer-surf w h))

                (gleui-buffer-commit! surface wl-buf w h)))))))))

(define (gleui-palette-adjust-scroll! state)
  "Ensure selected-index is visible within current scroll window."
  (let* ((sel (%gps-selected-index state))
         (scroll (%gps-scroll-offset state))
         (max-lines (or (assq-ref (%gps-theme-overrides state) 'lines)
                        (catch #t (lambda () (var-get '*palette-candidates-count*)) (lambda _ 10))
                        10)))
    (cond
     ((< sel scroll)
      (%gps-scroll-offset-set! state sel))
     ((>= sel (+ scroll max-lines))
      (%gps-scroll-offset-set! state (+ (- sel max-lines) 1))))))

(define (gleui-palette-select-next! state)
  "Move selection to next candidate."
  (let* ((count (length (%gps-filtered-indices state)))
         (curr (%gps-selected-index state)))
    (when (> count 0)
      (let ((next (if (< (1+ curr) count) (1+ curr) 0)))
        (%gps-selected-index-set! state next)
        (gleui-palette-adjust-scroll! state)
        (gleui-palette-render! state)))))

(define (gleui-palette-select-prev! state)
  "Move selection to previous candidate."
  (let* ((count (length (%gps-filtered-indices state)))
         (curr (%gps-selected-index state)))
    (when (> count 0)
      (let ((prev (if (> curr 0) (1- curr) (1- count))))
        (%gps-selected-index-set! state prev)
        (gleui-palette-adjust-scroll! state)
        (gleui-palette-render! state)))))

(define (gleui-palette-page-down! state)
  "Scroll down by page."
  (let* ((count (length (%gps-filtered-indices state)))
         (curr (%gps-selected-index state))
         (max-lines (or (assq-ref (%gps-theme-overrides state) 'lines)
                        (catch #t (lambda () (var-get '*palette-candidates-count*)) (lambda _ 10))
                        10)))
    (when (> count 0)
      (let ((next (min (1- count) (+ curr max-lines))))
        (%gps-selected-index-set! state next)
        (gleui-palette-adjust-scroll! state)
        (gleui-palette-render! state)))))

(define (gleui-palette-page-up! state)
  "Scroll up by page."
  (let* ((count (length (%gps-filtered-indices state)))
         (curr (%gps-selected-index state))
         (max-lines (or (assq-ref (%gps-theme-overrides state) 'lines)
                        (catch #t (lambda () (var-get '*palette-candidates-count*)) (lambda _ 10))
                        10)))
    (when (> count 0)
      (let ((prev (max 0 (- curr max-lines))))
        (%gps-selected-index-set! state prev)
        (gleui-palette-adjust-scroll! state)
        (gleui-palette-render! state)))))

(define (gleui-palette-select-first! state)
  "Jump to first matching candidate."
  (when (pair? (%gps-filtered-indices state))
    (%gps-selected-index-set! state 0)
    (%gps-scroll-offset-set! state 0)
    (gleui-palette-render! state)))

(define (gleui-palette-select-last! state)
  "Jump to last matching candidate."
  (let ((count (length (%gps-filtered-indices state))))
    (when (> count 0)
      (%gps-selected-index-set! state (1- count))
      (gleui-palette-adjust-scroll! state)
      (gleui-palette-render! state))))

(define (gleui-palette-refilter! state)
  "Filter candidates against current filter-text and re-render."
  (let* ((search-strs (%gps-search-strings state))
         (query (%gps-filter-text state))
         (case-sens? *palette-case-sensitive?*)
         (num-strs (if search-strs (length search-strs) 0)))

    ;; fast synchronous way if search entries are few
    (let ((filtered (if (or (string-null? query) (string-null? (string-trim-both query)))
                        (iota num-strs)
                        (gleui-palette-filter-candidates search-strs query case-sens?))))
      (%gps-filtered-indices-set! state filtered)
      (%gps-selected-index-set! state 0)
      (%gps-scroll-offset-set! state 0)
      (gleui-palette-render! state))))

;; NOTE: deleted since I optimized filtering and rendering further
;; I am keeping this commented for now as it was working well and it may be useful
;; NOTE: this is suffer race conditions, the thread must be kept in singleton
;; if the filter rules change, the earlier thread must be killed before spawning a new one

;; background worker thread if entries are more than 1000
;; it's a bit expensive to spawn a thread, but if that is
;; not implemented, the palette will freeze until rendering is done
;; (let ((new-gen (1+ (%gps-filter-gen state))))
;;   (%gps-filter-gen-set! state new-gen)
;;   (gleui-palette-render! state)
;;   (define (collect-matches strs idx acc)
;; 	(cond
;; 	 ((not (= (%gps-filter-gen state) new-gen))
;; 	  #f)
;; 	 ((null? strs)
;; 	  (reverse! acc))
;; 	 (else
;; 	  (let ((matched? (gleui-palette-candidate-matches? query (car strs) case-sens?)))
;; 		(collect-matches (cdr strs)
;; 						 (1+ idx)
;; 						 (if matched? (cons idx acc) acc))))))
;;   (call-with-new-thread
;;    (lambda ()
;; 	 (let ((results (collect-matches search-strs 0 '())))
;; 	   (when (and results
;; 				  (%gps-visible? state)
;; 				  (= (%gps-filter-gen state) new-gen))
;; 		 (%gps-filtered-indices-set! state results)
;; 		 (%gps-selected-index-set! state 0)
;; 		 (%gps-scroll-offset-set! state 0)
;; 		 (gleui-palette-render! state))))))

(define *palette-kbd-cache* (make-hash-table 64))

(define *palette-evdev-fallback-map*
  (map (lambda (entry)
         (cons (car entry) (keysym-name->xkb-value (cdr entry))))
       '((1   . "Escape")
         (28  . "Return")
         (14  . "BackSpace")
         (103 . "Up")
         (108 . "Down")
         (104 . "Page_Up")
         (109 . "Page_Down")
         (15  . "Tab"))))

(define (gleui-palette-parse-key spec)
  "Convert a key specification into a <gliver-key> record."
  (cond
   ((gliver-key? spec) spec)
   ((string? spec)
    (or (hash-ref *palette-kbd-cache* spec)
        (let ((k (catch #t (lambda () (kbd spec)) (lambda _ #f))))
          (when k (hash-set! *palette-kbd-cache* spec k))
          k)))
   (else #f)))

(define (gleui-palette-single-key-match? spec keysym ctrl? alt? shift?)
  "Return #t if a single key spec matches keysym and modifier state."
  (let ((k (gleui-palette-parse-key spec)))
    (and k
         (let* ((mods (gliver-key-modifiers k))
                (exp-ctrl?  (and (memq 'Control mods) #t))
                (exp-alt?   (and (memq 'Alt mods) #t))
                (exp-shift? (and (memq 'Shift mods) #t))
                (exp-val    (keysym-name->xkb-value (gliver-key-keysym k))))
           (and (> exp-val 0)
                (or (= keysym exp-val)
                    ;; case-insensitive ascii letter match (e.g. C-n / C-N)
                    (and (>= exp-val 97) (<= exp-val 122) (= (+ keysym 32) exp-val))
                    ;; tab / shift-tab / iso_left_tab
                    (and (or (and exp-shift? (= exp-val 65289)) (= exp-val 65056))
                         (= keysym 65056)))
                (eq? (and ctrl? #t) (and exp-ctrl? #t))
                (eq? (and alt? #t)  (and exp-alt? #t))
                (or (eq? (and shift? #t) (and exp-shift? #t))
                    ;; allow shift if not explicitly required and keysym is non-letter (e.g. < or >)
                    (and (not exp-shift?) (not (and (>= exp-val 97) (<= exp-val 122))))))))))

(define (gleui-palette-key-match? pattern keysym ctrl? alt? shift?)
  "Check whether KEYSYM and modifier flags match PATTERN.
PATTERN can be a string, a <gliver-key>, or a list of either."
  (cond
   ((null? pattern) #f)
   ((pair? pattern)
    (or (gleui-palette-single-key-match? (car pattern) keysym ctrl? alt? shift?)
        (gleui-palette-key-match? (cdr pattern) keysym ctrl? alt? shift?)))
   (else
    (gleui-palette-single-key-match? pattern keysym ctrl? alt? shift?))))

(define (gleui-palette-handle-key! state keycode keysym utf8-str)
  "Handle interactive keyboard event with configurable Emacs keybindings."
  (let* ((x-state (%gps-xkb-state state))
         (has-xkb? (and x-state (not (null-pointer? x-state))))
         (ctrl? (and has-xkb? xkb-state-mod-name-is-active
                     (not (zero? (xkb-state-mod-name-is-active x-state ctrl-name 1)))))
         (alt? (and has-xkb? xkb-state-mod-name-is-active
                    (not (zero? (xkb-state-mod-name-is-active x-state alt-name 1)))))
         (shift? (and has-xkb? xkb-state-mod-name-is-active
                      (not (zero? (xkb-state-mod-name-is-active x-state shift-name 1)))))
         (filter (%gps-filter-text state))
         (pos (%gps-cursor-pos state))
         (key-match? (lambda (pattern)
                       (gleui-palette-key-match? pattern keysym ctrl? alt? shift?))))
    (cond

     ;; select/confirm candidate
     ((key-match? *palette-key-select*)
      (let* ((filtered (%gps-filtered-indices state))
             (sel (%gps-selected-index state))
             (res (if (and (pair? filtered) (< sel (length filtered)))
                      (list-ref filtered sel)
                      (if (not (string-null? (string-trim-both filter)))
                          (string-trim-both filter)
                          #f)))
             (cb (%gps-async-callback state)))
        (%gps-result-set! state res)
        (%gps-done?-set! state #t)
        (gleui-palette-hide! state)
        (when (procedure? cb)
          (catch #t
            (lambda () (cb res))
            (lambda (key . args)
              (log-error "gleui-palette: on-select callback error: ~a ~a" key args))))))

     ;; cancel
     ((key-match? *palette-key-cancel*)
      (let ((cb (%gps-async-callback state)))
        (%gps-result-set! state #f)
        (%gps-done?-set! state #t)
        (gleui-palette-hide! state)
        (when (procedure? cb)
          (catch #t
            (lambda () (cb #f))
            (lambda (key . args)
              (log-error "gleui-palette: on-cancel callback error: ~a ~a" key args))))))

     ;; navigate to first candidate
     ((key-match? *palette-key-first*)
      (gleui-palette-select-first! state))

	 ;; navigate to last candidate
     ((key-match? *palette-key-last*)
      (gleui-palette-select-last! state))

     ;; navigate to next candidate
     ((key-match? *palette-key-next*)
      (gleui-palette-select-next! state))

     ;; navigate to previous candidate
     ((key-match? *palette-key-prev*)
      (gleui-palette-select-prev! state))

     ;; navigate to next page
     ((key-match? *palette-key-page-up*)
      (gleui-palette-page-up! state))

	 ;; navigate to previous page
     ((key-match? *palette-key-page-down*)
      (gleui-palette-page-down! state))

     ;; delete word backward
     ((key-match? *palette-key-delete-word-backward*)
      (when (> pos 0)
        (let* ((target-pos (gleui-word-backward-pos filter pos))
               (new-str (string-append (string-take filter target-pos)
                                       (string-drop filter pos))))
          (%gps-filter-text-set! state new-str)
          (%gps-cursor-pos-set! state target-pos)
          (gleui-palette-refilter! state))))

	 ;; delete word forward
     ((key-match? *palette-key-delete-word-forward*)
      (when (< pos (string-length filter))
        (let* ((target-pos (gleui-word-forward-pos filter pos))
               (new-str (string-append (string-take filter pos)
                                       (string-drop filter target-pos))))
          (%gps-filter-text-set! state new-str)
          (gleui-palette-refilter! state))))

	 ;; delete character backward
     ((key-match? *palette-key-delete-char-backward*)
      (when (> pos 0)
        (let ((new-str (string-append (string-take filter (1- pos))
                                      (string-drop filter pos))))
          (%gps-filter-text-set! state new-str)
          (%gps-cursor-pos-set! state (1- pos))
          (gleui-palette-refilter! state))))

	 ;; delete character forward
     ((key-match? *palette-key-delete-char-forward*)
      (when (< pos (string-length filter))
        (let ((new-str (string-append (string-take filter pos)
                                      (string-drop filter (1+ pos)))))
          (%gps-filter-text-set! state new-str)
          (gleui-palette-refilter! state))))

     ;; kill line forward
     ((key-match? *palette-key-kill-line*)
      (when (< pos (string-length filter))
        (let ((new-str (string-take filter pos)))
          (%gps-filter-text-set! state new-str)
          (gleui-palette-refilter! state))))

	 ;; kill line backward
     ((key-match? *palette-key-discard-line*)
      (let ((new-str (string-drop filter pos)))
        (%gps-filter-text-set! state new-str)
        (%gps-cursor-pos-set! state 0)
        (gleui-palette-refilter! state)))

     ;; move word backward
     ((key-match? *palette-key-word-backward*)
      (let ((target-pos (gleui-word-backward-pos filter pos)))
        (%gps-cursor-pos-set! state target-pos)
        (gleui-palette-render! state)))

     ;; move word forward
     ((key-match? *palette-key-word-forward*)
      (let ((target-pos (gleui-word-forward-pos filter pos)))
        (%gps-cursor-pos-set! state target-pos)
        (gleui-palette-render! state)))

     ;; move to beginning of line
     ((key-match? *palette-key-bol*)
      (%gps-cursor-pos-set! state 0)
      (gleui-palette-render! state))

     ;; move to end of line
     ((key-match? *palette-key-eol*)
      (%gps-cursor-pos-set! state (string-length filter))
      (gleui-palette-render! state))

     ;; move character backward
     ((key-match? *palette-key-char-backward*)
      (when (> pos 0)
        (%gps-cursor-pos-set! state (1- pos))
        (gleui-palette-render! state)))

     ;; move character forward
     ((key-match? *palette-key-char-forward*)
      (when (< pos (string-length filter))
        (%gps-cursor-pos-set! state (1+ pos))
        (gleui-palette-render! state)))

     ;; normal character input
     ((and (not ctrl?) (not alt?)
           utf8-str (> (string-length utf8-str) 0)
           (>= (char->integer (string-ref utf8-str 0)) 32)
           (not (= (char->integer (string-ref utf8-str 0)) 127)))
      (let ((new-str (string-append (string-take filter pos)
                                    utf8-str
                                    (string-drop filter pos))))
        (%gps-filter-text-set! state new-str)
        (%gps-cursor-pos-set! state (+ pos (string-length utf8-str)))
        (gleui-palette-refilter! state))))))

(define (gleui-palette-ensure-keyboard! state)
  "Ensure wl_keyboard is bound and listening for STATE."
  (gleui-repeat-worker-start!)
  (when (and state
             (or (not (%gps-keyboard state))
                 (null-pointer? (%gps-keyboard state)))
             *wl-registry*)
    (let* ((seat (seat-current))
           (seat-obj-id (and seat (seat-wl-seat seat)))
           (wl-seat (and seat-obj-id
                         (number? seat-obj-id)
                         (gliver-wl-registry-bind *wl-registry* seat-obj-id *wl-seat-interface* 7))))
      (when (and wl-seat (pointer? wl-seat) (not (null-pointer? wl-seat)))
        (let ((keyboard (wl-seat-get-keyboard wl-seat)))
          (unless (null-pointer? keyboard)
            (%gps-keyboard-set! state keyboard)
            (let ((kb-listener
                   (make-wl-keyboard-listener
                    ;; on-keymap
                    (lambda (data proxy keymap-fmt fd size)
                      (when (and (= keymap-fmt 1) (> size 0))
                        (let ((map-ptr (mmap %null-pointer size *prot-read* *map-shared* fd 0)))
                          (unless (null-pointer? map-ptr)
                            (let* ((xkb-ctx (%gps-xkb-ctx state))
                                   (km (if xkb-keymap-new-from-buffer
                                           (xkb-keymap-new-from-buffer xkb-ctx map-ptr size 1 0)
                                           (xkb-keymap-new-from-string xkb-ctx map-ptr 1 0)))
                                   (kb-state (and km (not (null-pointer? km)) (xkb-state-new km))))
                              (when (and kb-state (not (null-pointer? kb-state)))
                                (%gps-xkb-keymap-set! state km)
                                (%gps-xkb-state-set! state kb-state)))
                            (munmap map-ptr size)))
                        (close-fd fd)))
                    ;; on-enter
                    (lambda (data proxy serial surface keys-bv)
                      (log-debug "gleui-palette: wl_keyboard entered"))
                    ;; on-leave
                    (lambda (data proxy serial surface)
                      (gleui-repeat-worker-stop!)
                      (log-debug "gleui-palette: wl_keyboard left"))
                    ;; on-key
                    (lambda (data proxy serial time key state-flag)
                      (if (zero? state-flag) ;; key released
                          (gleui-repeat-worker-stop!)
                          (when (%gps-visible? state) ;; key pressed
                            (gleui-repeat-worker-stop!)
                            (let* ((keycode (+ key 8))
                                   (x-state (%gps-xkb-state state))
                                   (ctrl? (and x-state (not (null-pointer? x-state)) xkb-state-mod-name-is-active
                                               (not (zero? (xkb-state-mod-name-is-active x-state ctrl-name 1)))))
                                   (alt? (and x-state (not (null-pointer? x-state)) xkb-state-mod-name-is-active
                                              (not (zero? (xkb-state-mod-name-is-active x-state alt-name 1)))))
                                   (shift? (and x-state (not (null-pointer? x-state)) xkb-state-mod-name-is-active
                                                (not (zero? (xkb-state-mod-name-is-active x-state shift-name 1)))))
                                   (keysym (if (and x-state (not (null-pointer? x-state)) xkb-state-key-get-one-sym)
                                               (xkb-state-key-get-one-sym x-state keycode)
                                               (or (assoc-ref *palette-evdev-fallback-map* key) 0)))
                                   (raw-utf8
                                    (if (and x-state (not (null-pointer? x-state)) xkb-state-key-get-utf8)
                                        (let ((bv (make-bytevector 16 0)))
                                          (let ((len (xkb-state-key-get-utf8 x-state keycode (bytevector->pointer bv) 16)))
                                            (if (> len 0)
                                                (pointer->string (bytevector->pointer bv) len)
                                                #f)))
                                        (if (and xkb-keysym-to-utf8 (> keysym 0))
                                            (let ((bv (make-bytevector 16 0)))
                                              (let ((len (xkb-keysym-to-utf8 keysym (bytevector->pointer bv) 16)))
                                                (if (> len 0)
                                                    (pointer->string (bytevector->pointer bv) len)
                                                    #f)))
                                            #f)))
                                   (utf8-str
                                    (or raw-utf8
                                        (if (and (not ctrl?) (not alt?) (>= keysym 32) (<= keysym 126))
                                            (string (integer->char keysym))
                                            #f)))
                                   (non-repeatable?
                                    (or (and (>= keysym 65505) (<= keysym 65518))
                                        (gleui-palette-key-match? *palette-key-select* keysym ctrl? alt? shift?)
                                        (gleui-palette-key-match? *palette-key-cancel* keysym ctrl? alt? shift?))))

                              ;; initial key press action
                              (gleui-palette-handle-key! state keycode keysym utf8-str)

                              ;; signal persistent repeat worker if key is held down
                              (when (and (not non-repeatable?)
                                         (%gps-visible? state)
                                         (not (%gps-done? state)))
                                (gleui-repeat-worker-signal!
                                 state
                                 (vector keycode keysym utf8-str)
                                 (lambda (st params)
                                   (gleui-palette-handle-key! st
                                                              (vector-ref params 0)
                                                              (vector-ref params 1)
                                                              (vector-ref params 2)))
                                 #:rate (%gps-repeat-rate state)
                                 #:delay (%gps-repeat-delay state)
                                 #:active-pred (lambda ()
                                                 (and (%gps-visible? state)
                                                      (not (%gps-done? state))))))))))
                    ;; on-modifiers
                    (lambda (data proxy serial mods-dep mods-latch mods-lock group)
                      (let ((x-state (%gps-xkb-state state)))
                        (when (and x-state (not (null-pointer? x-state)) xkb-state-update-mask)
                          (xkb-state-update-mask x-state mods-dep mods-latch mods-lock 0 0 group))))
                    ;; on-repeat-info
                    (lambda (data proxy rate delay)
                      (when (and (number? rate) (> rate 0))
                        (%gps-repeat-rate-set! state rate))
                      (when (and (number? delay) (> delay 0))
                        (%gps-repeat-delay-set! state delay))
                      (log-debug "gleui-palette: repeat info rate=~a delay=~a" rate delay)))))
              (%gps-keyboard-listener-set! state kb-listener)
              (wl-proxy-add-listener keyboard kb-listener %null-pointer)
              (log-debug "gleui-palette: wl_keyboard attached successfully."))))))))

(define (gleui-palette-calc-height state)
  "Calculate total height in pixels for the current candidate list and theme."
  (let* ((overrides (%gps-theme-overrides state))
         (theme-get (lambda (k def) (gleui-theme-get overrides k def)))
         (font-size (or (theme-get 'font-size '*palette-font-size*) 13))
         (filtered-idxs (%gps-filtered-indices state))
         (total-matching (if (list? filtered-idxs) (length filtered-idxs) 0))
         (max-lines (or (theme-get 'lines '*palette-candidates-count*) 10))
         (visible-lines (min max-lines (max 1 total-matching)))
         (row-height (max 38 (+ font-size 22)))
         (input-height (max 42 (+ font-size 26)))
         (pad 16)
         (spacing 10))
    (+ (* 2 pad) input-height spacing 10 (* visible-lines row-height))))

(define (gleui-palette-ensure-surface! state)
  "Ensure Wayland surface and layer-surface are created and configured for STATE."
  (when (and state
             (or (not (%gps-surface state))
                 (null-pointer? (%gps-surface state))
                 (not (%gps-layer-surface state))
                 (null-pointer? (%gps-layer-surface state)))
             *connected*
             (pointer? *wl-compositor*)
             (not (null-pointer? *wl-compositor*))
             (pointer? *zwlr-layer-shell*)
             (not (null-pointer? *zwlr-layer-shell*)))
    (let* ((w (%gps-width state))
           (h (gleui-palette-calc-height state))
           (anchor (gleui-numpad->anchor (or *palette-location* 8))))
      (gleui-layer-surface-create
       #:namespace "gliver-palette"
       #:layer ZWLR_LAYER_SHELL_V1_LAYER_OVERLAY
       #:anchor anchor
       #:margin-top *palette-anchor-margin*
       #:margin-bottom *palette-anchor-margin*
       #:exclusive-zone -1
       #:keyboard-interactivity ZWLR_LAYER_SURFACE_V1_KEYBOARD_INTERACTIVITY_EXCLUSIVE
       #:width w
       #:height h
       #:on-created (lambda (surf layer-surf)
                      (%gps-surface-set! state surf)
                      (%gps-layer-surface-set! state layer-surf)
                      (%gps-height-set! state h)
                      (%gps-configured?-set! state #f))
       #:on-configure (lambda (cw ch)
                        (log-debug "gleui-palette: Layer surface configured ~ax~a" cw ch)
                        (%gps-configured?-set! state #t)
                        (when (%gps-visible? state)
                          (gleui-palette-render! state)))
       #:on-close (lambda ()
                    (log-info "gleui-palette: Layer surface closed")
                    (gleui-palette-hide! state))))))

(define (gleui-palette-init!)
  "Initialize the cached palette view state and keyboard listener."
  (unless (or (not *connected*)
              (not (pointer? *wl-compositor*))
              (null-pointer? *wl-compositor*)
              (not (pointer? *zwlr-layer-shell*))
              (null-pointer? *zwlr-layer-shell*)
              (not (pointer? *wl-shm*))
              (null-pointer? *wl-shm*))
    (unless *gleui-palette-instance*
      (log-info "gleui-palette: Initializing cached palette state")
      (let* ((cur-out (output-current))
             (out-w (if cur-out (output-width cur-out) 1920))
             (pal-pct (or *palette-width* 60))
             (init-w (max 400 (min (- out-w 40) (inexact->exact (round (* out-w (/ pal-pct 100.0)))))))
             (init-h 470)
             (xkb-ctx (if xkb-context-new (xkb-context-new 0) %null-pointer))
             (state (%make-gleui-palette-state
                     #f #f #f 0 #f #f
                     init-w init-h cur-out
                     %null-pointer #f
                     xkb-ctx %null-pointer %null-pointer
                     '() #f '() '() "" 0 0 0 *palette-prompt* '() #f #f #f
                     (make-mutex) 0
                     (or (catch #t (lambda () (var-get '*palette-repeat-rate*)) (lambda _ 25)) 25)
                     (or (catch #t (lambda () (var-get '*palette-repeat-delay*)) (lambda _ 600)) 600))))
        (set! *gleui-palette-instance* state)
        (gleui-palette-ensure-keyboard! state)
        (log-info "gleui-palette: Cached state initialized successfully.")))))

(define* (gleui-palette-open! candidates
                              #:key (search-strings #f)
                              (theme-overrides '())
                              (prompt #f)
                              (initial-filter "")
                              (on-select #f)
                              (on-cancel #f)
                              (on-change #f))
  "Open the cached palette view with CANDIDATES asynchronously."
  (gleui-palette-init!)
  (let ((state *gleui-palette-instance*))
    (when state
      (let* ((num-cands (length candidates))
             (search-strs
              (or search-strings
                  (map (lambda (cand)
                         (cond
                          ((pair? cand) (format #f "~a ~a" (car cand) (cdr cand)))
                          ((string? cand) cand)
                          ((list? cand) (string-join (map (lambda (x) (format #f "~a" x)) cand) " "))
                          (else (format #f "~a" cand))))
                       candidates)))
             (filtered (if (or (string-null? initial-filter) (string-null? (string-trim-both initial-filter)))
                           (iota num-cands)
                           (gleui-palette-filter-candidates search-strs initial-filter *palette-case-sensitive?*)))
             (cur-out (output-current))
             (out-w (if cur-out (output-width cur-out) 1920))
             (pal-pct (or (and theme-overrides (assq-ref theme-overrides 'width)) *palette-width* 60))
             (w (max 400 (min (- out-w 40) (inexact->exact (round (* out-w (/ pal-pct 100.0))))))))

        (%gps-candidates-set! state candidates)
        (%gps-display-candidates-set! state (make-vector num-cands #f))
        (%gps-search-strings-set! state search-strs)
        (%gps-filtered-indices-set! state filtered)
        (%gps-filter-text-set! state initial-filter)
        (%gps-cursor-pos-set! state (string-length initial-filter))
        (%gps-selected-index-set! state 0)
        (%gps-scroll-offset-set! state 0)
        (%gps-prompt-set! state (or prompt (and theme-overrides (assq-ref theme-overrides 'prompt)) *palette-prompt*))
        (%gps-theme-overrides-set! state theme-overrides)
        (%gps-done?-set! state #f)
        (%gps-result-set! state #f)
        (let ((combined-cb
               (lambda (res)
                 (if res
                     (when (procedure? on-select) (on-select res))
                     (when (procedure? on-cancel) (on-cancel))))))
          (%gps-async-callback-set! state combined-cb))
        (%gps-width-set! state w)
        (%gps-visible?-set! state #t)

        (gleui-palette-ensure-keyboard! state)

        (if (%gps-configured? state)
            (gleui-palette-render! state)
            (begin
              (gleui-palette-ensure-surface! state)
              (when (and (%gps-configured? state) (%gps-visible? state))
                (gleui-palette-render! state))))))))

(define* (gleui-palette-hide! #:optional (state *gleui-palette-instance*))
  "Hide the cached palette view cleanly."
  (when (and state (%gps-visible? state))
    (gleui-repeat-worker-stop!)
    (%gps-filter-gen-set! state (1+ (%gps-filter-gen state)))
    (%gps-visible?-set! state #f)
    (%gps-configured?-set! state #f)
    (%gps-candidates-set! state '())
    (%gps-display-candidates-set! state #f)
    (%gps-search-strings-set! state #f)
    (%gps-filtered-indices-set! state '())
    (%gps-filter-text-set! state "")
    (%gps-cursor-pos-set! state 0)
    (%gps-selected-index-set! state 0)
    (%gps-scroll-offset-set! state 0)
    (%gps-async-callback-set! state #f)

    (let ((layer-surf (%gps-layer-surface state))
          (surface (%gps-surface state)))
      (gleui-layer-surface-destroy! surface layer-surf)
      (%gps-layer-surface-set! state #f)
      (%gps-surface-set! state #f))))

(define gleui-palette-close! gleui-palette-hide!)
(define* (gleui-palette-kill #:key (name #f))
  (gleui-palette-hide!))

(define* (gleui-palette-update! #:key (candidates #f)
                                (filter #f)
                                (prompt #f)
                                (selected-index #f))
  "Dynamically update the data of the cached palette view."
  (let ((state *gleui-palette-instance*))
    (when state
      (when candidates
        (let* ((num-cands (length candidates))
               (search-strs
                (map (lambda (cand)
                       (cond
                        ((pair? cand) (format #f "~a ~a" (car cand) (cdr cand)))
                        ((string? cand) cand)
                        ((list? cand) (string-join (map (lambda (x) (format #f "~a" x)) cand) " "))
                        (else (format #f "~a" cand))))
                     candidates)))
          (%gps-candidates-set! state candidates)
          (%gps-display-candidates-set! state (make-vector num-cands #f))
          (%gps-search-strings-set! state search-strs)))
      (when prompt
        (%gps-prompt-set! state prompt))
      (when filter
        (%gps-filter-text-set! state filter)
        (%gps-cursor-pos-set! state (string-length filter)))
      (when selected-index
        (%gps-selected-index-set! state selected-index))
      (gleui-palette-refilter! state))))

(define* (gleui-palette-show candidates #:key (theme-overrides '()) (command #f) (prompt #f) (initial-filter "")
                             (on-select #f) (on-cancel #f))
  "Prompt the user to select from CANDIDATES using the cached Cairo/Pango palette view.
When ON-SELECT is provided, opens asynchronously and calls ON-SELECT on selection.
Otherwise, runs a synchronous event loop until selection or cancellation."
  (if (procedure? on-select)
      (gleui-palette-open! candidates
                           #:theme-overrides theme-overrides
                           #:prompt prompt
                           #:initial-filter initial-filter
                           #:on-select on-select
                           #:on-cancel on-cancel)
      (begin
        (gleui-palette-open! candidates
                             #:theme-overrides theme-overrides
                             #:prompt prompt
                             #:initial-filter initial-filter)
        (let ((state *gleui-palette-instance*))
          (if (not state)
              #f
              (begin
                (while (and (%gps-visible? state)
                            (not (%gps-done? state))
                            *connected*)
                  (catch #t
                    (lambda ()
                      (when *wl-display*
                        (wl-display-flush *wl-display*)
                        (let ((p-ret (wl-display-dispatch-pending *wl-display*)))
                          (when (< p-ret 0)
                            (%gps-done?-set! state #t)))
                        (let ((wl-fd (wl-display-get-fd *wl-display*)))
                          (let ((ready (select (list wl-fd) '() '() 0 50000)))
                            (when (pair? (car ready))
                              (let ((ret (wl-display-dispatch *wl-display*)))
                                (when (< ret 0)
                                  (%gps-done?-set! state #t))))))))
                    (lambda (key . args)
                      (log-error "gleui-palette: Event loop error ~a ~a" key args)
                      (%gps-done?-set! state #t))))

                (let ((res (%gps-result state)))
                  (gleui-palette-hide! state)
                  res)))))))

(define (gleui-palette-cleanup!)
  "Destroy cached Wayland surfaces and resources on shutdown."
  (when *gleui-palette-instance*
    (let* ((state *gleui-palette-instance*)
           (slots (%gps-shm-slots state))
           (keyboard (%gps-keyboard state))
           (xkb-state (%gps-xkb-state state))
           (xkb-keymap (%gps-xkb-keymap state))
           (xkb-ctx (%gps-xkb-ctx state)))

      (gleui-palette-hide! state)

      (when (pair? slots)
        (gleui-shm-slot-destroy (car slots))
        (gleui-shm-slot-destroy (cdr slots))
        (%gps-shm-slots-set! state #f))

      (when (and xkb-state (pointer? xkb-state) (not (null-pointer? xkb-state)) xkb-state-unref)
        (catch #t (lambda () (xkb-state-unref xkb-state)) (lambda _ #f)))
      (when (and xkb-keymap (pointer? xkb-keymap) (not (null-pointer? xkb-keymap)) xkb-keymap-unref)
        (catch #t (lambda () (xkb-keymap-unref xkb-keymap)) (lambda _ #f)))
      (when (and xkb-ctx (pointer? xkb-ctx) (not (null-pointer? xkb-ctx)) xkb-context-unref)
        (catch #t (lambda () (xkb-context-unref xkb-ctx)) (lambda _ #f)))

      (when (and keyboard (pointer? keyboard) (not (null-pointer? keyboard)))
        (catch #t (lambda () (wl-keyboard-release keyboard)) (lambda _ #f))))))

(define-command (gleui-palette-install!)
  "Install gleui-palette as the dmenu/palette backend."
  (set! *palette-backend* gleui-palette-show)
  (set! *palette-dmenu-options* gleui-palette-make-dmenu-options)
  (set! *palette-backend-kill* gleui-palette-kill)
  (when (and *connected*
             (pointer? *wl-compositor*) (not (null-pointer? *wl-compositor*))
             (pointer? *zwlr-layer-shell*) (not (null-pointer? *zwlr-layer-shell*)))
    (catch #t gleui-palette-init! (lambda _ #f)))
  (log-info "gleui-palette installed as dmenu/palette backend."))
