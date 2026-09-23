;;; gliver/contrib/ui/gleui/toast.scm --- Cairo & Pango Toast Notifications
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver contrib ui gleui toast)
  #:use-module (cairo)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (gliver core)
  #:use-module (gliver deps libc)
  #:use-module (gliver deps color)
  #:use-module (gliver deps pango)
  #:use-module (gliver wayland client)
  #:use-module (gliver wayland gen wayland)
  #:use-module (gliver wayland gen wlr-layer-shell-unstable-v1)
  #:use-module (gliver river connector)
  #:use-module (gliver contrib ui components toast)
  #:use-module (gliver contrib ui gleui base)
  #:declarative? #f
  #:export (
			*gleui-toast-font*
			*gleui-toast-font-size*
			*gleui-toast-bg-color*
			*gleui-toast-fg-color*
			*gleui-toast-border-color*
			*gleui-toast-border-width*
			*gleui-toast-border-radius*
			*gleui-toast-location*
			*gleui-toast-margin*
			gleui-toast-state?
			*gleui-toast-instance*
			gleui-toast-get-instance
			gleui-toast-visible?
			gleui-toast-configured?
			gleui-toast-has-markup-tags?
			gleui-toast-markup-ensure
			gleui-toast-span-render
			gleui-toast-column-render
			gleui-toast-message-parse
			gleui-toast-calc-dimensions
			gleui-toast-draw-surface!
			gleui-toast-render!
			gleui-toast-ensure-surface!
			gleui-toast-show
			gleui-toast-kill
			gleui-toast-open!
			gleui-toast-cleanup!
			gleui-toast-install!
))

(define-var *gleui-toast-font* #f
			"Font family used for gleui toast notifications (default: *theme-font*).")
(define-var *gleui-toast-font-size* 12
			"Font size used for gleui toast notifications (default: *theme-font-size*).")
(define-var *gleui-toast-bg-color* #f
			"Background color for gleui toast notifications (default: *theme-bg-main*).")
(define-var *gleui-toast-fg-color* #f
			"Text color for gleui toast notifications (default: *theme-fg-main*).")
(define-var *gleui-toast-border-color* #f
			"Border color for gleui toast notifications (default: *theme-border-color*).")
(define-var *gleui-toast-border-width* 2
			"Border width in pixels for gleui toast notifications (default: *theme-border-width*).")
(define-var *gleui-toast-border-radius* #f
			"Corner border radius in pixels for gleui toast notifications (default: *theme-border-radius*).")
(define-var *gleui-toast-location* 8
			"Gliver position (1-9) for gleui toast notifications.")
(define-var *gleui-toast-margin* 45
			"Margin in pixels from the screen edge for gleui toast notifications.")
(define-var *gleui-toast-padding-x* 22
			"Padding horizontal content in pixels for gleui toast notifications.")
(define-var *gleui-toast-padding-y* 14
			"Padding vertical content in pixels for gleui toast notifications.")
(define-var *gleui-toast-column-gap* 24
			"Horizontal gap pixels for gleui toast notifications.")
(define-var *gleui-toast-row-gap* 6
			"Vertical gap pixels for gleui toast notifications.")

(define-record-type <gleui-toast-state>
  (%make-toast-view-state surface layer-surface shm-slots shm-active-idx configured? visible?
                          width height x y output
                          parsed-content raw-message name timeout-thread-gen
                          theme-overrides render-mutex)
  gleui-toast-state?
  (surface             %tvs-surface             %tvs-surface-set!)
  (layer-surface       %tvs-layer-surface       %tvs-layer-surface-set!)
  (shm-slots           %tvs-shm-slots           %tvs-shm-slots-set!)
  (shm-active-idx      %tvs-shm-active-idx      %tvs-shm-active-idx-set!)
  (configured?         %tvs-configured?         %tvs-configured?-set!)
  (visible?            %tvs-visible?            %tvs-visible?-set!)
  (width               %tvs-width               %tvs-width-set!)
  (height              %tvs-height              %tvs-height-set!)
  (x                   %tvs-x                   %tvs-x-set!)
  (y                   %tvs-y                   %tvs-y-set!)
  (output              %tvs-output              %tvs-output-set!)
  (parsed-content      %tvs-parsed-content      %tvs-parsed-content-set!)
  (raw-message         %tvs-raw-message         %tvs-raw-message-set!)
  (name                %tvs-name                %tvs-name-set!)
  (timeout-thread-gen  %tvs-timeout-thread-gen  %tvs-timeout-thread-gen-set!)
  (theme-overrides     %tvs-theme-overrides     %tvs-theme-overrides-set!)
  (render-mutex        %tvs-render-mutex))

;; global cached toast view singleton
(define *gleui-toast-instance* #f)

(define (gleui-toast-get-instance)
  *gleui-toast-instance*)

(define (gleui-toast-visible?)
  (and *gleui-toast-instance*
       (%tvs-visible? *gleui-toast-instance*)))

(define (gleui-toast-configured?)
  (and *gleui-toast-instance*
       (%tvs-configured? *gleui-toast-instance*)))

(define (gleui-toast-has-markup-tags? str)
  "Check if STR appears to contain Pango XML markup tags."
  (and (string? str)
       (regexp-exec (make-regexp "<[a-zA-Z/][^>]*>") str)))

(define (gleui-toast-markup-ensure str)
  "Ensure STR is valid Pango markup. If it doesn't contain tags, XML-escape it."
  (if (string? str)
      (if (gleui-toast-has-markup-tags? str)
          str
          (gleui-pango-escape str))
      (gleui-pango-escape (format #f "~a" (or str "")))))

(define (gleui-toast-span-render span)
  "Render a single toast span alist into Pango markup."
  (let* ((raw-text (or (assoc-ref span 'text) ""))
         (text     (gleui-pango-escape (if (string? raw-text) raw-text (format #f "~a" raw-text))))
         (color    (assoc-ref span 'color))
         (weight   (assoc-ref span 'weight))
         (tag-color (if (and color (string? color)) (format #f " color='~a'" color) ""))
         (tag-weight (if (and weight (or (string? weight) (symbol? weight)))
                         (format #f " weight='~a'" weight)
                         ""))
         (open-tag (if (or (not (string-null? tag-color)) (not (string-null? tag-weight)))
                       (format #f "<span~a~a>" tag-color tag-weight)
                       #f)))
    (if open-tag
        (string-append open-tag text "</span>")
        text)))

(define (gleui-toast-column-render col)
  "Render a toast column into a Pango markup string."
  (cond
   ;; column alist
   ((and (list? col) (assoc-ref col 'spans))
    (let ((spans (assoc-ref col 'spans)))
      (if (list? spans)
          (string-join (map gleui-toast-span-render spans) "")
          (gleui-toast-span-render spans))))
   ;; list of spans
   ((and (list? col) (pair? col) (pair? (car col)) (assoc-ref (car col) 'text))
    (string-join (map gleui-toast-span-render col) ""))
   ;; single span
   ((and (list? col) (assoc-ref col 'text))
    (gleui-toast-span-render col))
   ;; plain string
   ((string? col)
    (gleui-toast-markup-ensure col))
   (else
    (gleui-toast-markup-ensure (format #f "~a" col)))))

(define (gleui-toast-message-parse message)
  "Parse MESSAGE into ('text . markup-str) or ('grid . rows-of-col-markups)."
  (cond
   ((string? message)
    (cons 'text (gleui-toast-markup-ensure message)))
   ((and (list? message) (not (null? message)))
    (let ((first-item (car message)))
      (cond
       ;; list of rows where each row is a list of columns
       ((list? first-item)
        (let ((rows (map
                     (lambda (row)
                       (if (list? row)
                           (map gleui-toast-column-render row)
                           (list (gleui-toast-column-render row))))
                     message)))
          (cons 'grid rows)))
       ;; list of strings/items: (list "line 1" "line 2")
       (else
        (let ((lines (map (lambda (x)
                            (if (string? x)
                                (gleui-toast-markup-ensure x)
                                (gleui-toast-markup-ensure (format #f "~a" x))))
                          message)))
          (cons 'text (string-join lines "\n")))))))
   (else
    (cons 'text (gleui-toast-markup-ensure (format #f "~a" (or message "")))))))

(define (gleui-toast-calc-dimensions state)
  "Calculate (values width height) for STATE based on parsed content and font metrics."
  (let* ((dummy-surf (cairo-image-surface-create 'argb32 1 1))
         (cr (cairo-create dummy-surf))
         (overrides (%tvs-theme-overrides state))
         (theme-get (lambda (k def) (gleui-theme-get overrides k def)))
         (font-family (theme-get 'font '*gleui-toast-font*))
         (font-size (theme-get 'font-size '*gleui-toast-font-size*))
         (font-str (format #f "~a ~a" font-family font-size))
         (pad-x (theme-get 'padding-x *gleui-toast-padding-x*))
         (pad-y (theme-get 'padding-y *gleui-toast-padding-y*))
         (col-gap (theme-get 'column-gap *gleui-toast-column-gap*))
         (row-gap (theme-get 'row-gap *gleui-toast-row-gap*))
         (parsed (%tvs-parsed-content state))
         (cur-out (output-current))
         (out-w (if cur-out (output-width cur-out) 1920))
         (max-w (max 300 (- out-w 60))))
    (let-values (((w h)
                  (cond
                   ((and (pair? parsed) (eq? (car parsed) 'text))
                    (let ((text (cdr parsed)))
                      (call-with-values
                          (lambda () (pango-measure-text cr text #:font font-str))
                        (lambda (tw th)
                          (let ((w (max 120 (min max-w (+ tw (* 2 pad-x)))))
                                (h (+ th (* 2 pad-y))))
                            (values w h))))))
                   ((and (pair? parsed) (eq? (car parsed) 'grid))
                    (let* ((rows (cdr parsed))
                           (num-cols (if (null? rows) 0 (apply max (map length rows))))
                           (col-widths (make-vector num-cols 0))
                           (max-cell-h 0))
                      (for-each
                       (lambda (row)
                         (let loop-c ((c 0) (cols row))
                           (when (and (< c num-cols) (pair? cols))
                             (let ((cell (car cols)))
                               (call-with-values
                                   (lambda () (pango-measure-text cr cell #:font font-str))
                                 (lambda (cw ch)
                                   (when (> cw (vector-ref col-widths c))
                                     (vector-set! col-widths c cw))
                                   (when (> ch max-cell-h)
                                     (set! max-cell-h ch)))))
                             (loop-c (1+ c) (cdr cols)))))
                       rows)
                      (let* ((total-cols-w (let loop-sum ((i 0) (sum 0))
                                             (if (>= i num-cols)
                                                 sum
                                                 (loop-sum (1+ i) (+ sum (vector-ref col-widths i))))))
                             (total-gaps (if (> num-cols 1) (* (1- num-cols) col-gap) 0))
                             (line-h (+ (max font-size max-cell-h) row-gap))
                             (w (max 160 (min max-w (+ total-cols-w total-gaps (* 2 pad-x)))))
                             (h (+ (* (length rows) line-h) (* 2 pad-y))))
                        (values w h))))
                   (else
                    (values 200 60)))))
      (cairo-destroy cr)
      (cairo-surface-destroy dummy-surf)
      (values w h))))

(define (gleui-toast-draw-surface! cr width height state overrides)
  "Render toast notification content to Cairo context CR."
  (cairo-set-operator cr 'clear)
  (cairo-paint cr)
  (cairo-set-operator cr 'over)

  (let* ((theme-get (lambda (k def) (gleui-theme-get overrides k def)))
         (font-family (theme-get 'font '*gleui-toast-font*))
         (font-size (theme-get 'font-size '*gleui-toast-font-size*))
         (font-str (format #f "~a ~a" font-family font-size))
         (bg-color (theme-get 'background '*gleui-toast-bg-color*))
         (fg-color (theme-get 'foreground '*gleui-toast-fg-color*))
         (border-color (theme-get 'border-color '*gleui-toast-border-color*))
         (border-width (theme-get 'border-width '*gleui-toast-border-width*))
         (border-radius (theme-get 'border-radius '*gleui-toast-border-radius*))
         (pad-x (theme-get 'padding-x *gleui-toast-padding-x*))
         (pad-y (theme-get 'padding-y *gleui-toast-padding-y*))
         (col-gap (theme-get 'column-gap *gleui-toast-column-gap*))
         (row-gap (theme-get 'row-gap *gleui-toast-row-gap*))
         (parsed (%tvs-parsed-content state)))

    ;; background surface
    (let ((bg-rgba (parse-hex-color-rgba bg-color)))
      (apply (lambda (r g b a) (cairo-set-source-rgba cr r g b a)) bg-rgba)
      (gleui-draw-rounded-rect cr 0 0 width height border-radius)
      (cairo-fill cr))

    ;; border
    (when (and (number? border-width) (> border-width 0))
      (let ((b-rgba (parse-hex-color-rgba border-color))
            (half-w (/ border-width 2.0)))
        (apply (lambda (r g b a) (cairo-set-source-rgba cr r g b a)) b-rgba)
        (cairo-set-line-width cr border-width)
        (gleui-draw-rounded-rect cr half-w half-w (- width border-width) (- height border-width) border-radius)
        (cairo-stroke cr)))

    ;; content
    (let ((fg-rgba (parse-hex-color-rgba fg-color)))
      (apply (lambda (r g b a) (cairo-set-source-rgba cr r g b a)) fg-rgba))

    (cond
     ;; text content
     ((and (pair? parsed) (eq? (car parsed) 'text))
      (let ((text (cdr parsed)))
        (cairo-move-to cr pad-x pad-y)
        (pango-draw-text cr text #:font font-str)))

     ;; grid content
     ((and (pair? parsed) (eq? (car parsed) 'grid))
      (let* ((rows (cdr parsed))
             (num-cols (if (null? rows) 0 (apply max (map length rows))))
             (col-widths (make-vector num-cols 0))
             (max-cell-h 0))

		;; measure every cell in a row and update column widths and max height
		(define (measure-row-cells! row)
          (for-each
           (lambda (col-idx cell)
             (call-with-values (lambda () (pango-measure-text cr cell #:font font-str))
               (lambda (cw ch)
                 (when (> cw (vector-ref col-widths col-idx))
                   (vector-set! col-widths col-idx cw))
                 (when (> ch max-cell-h)
                   (set! max-cell-h ch)))))
           (iota (min num-cols (length row)))
           row))

		;; calculate X coordinate offsets for each column as a vector for O(1) lookup
		(define (compute-column-offsets)
          (let ((offsets (make-vector num-cols 0)))
			(let loop ((c 0) (acc-x 0))
              (when (< c num-cols)
				(vector-set! offsets c acc-x)
				(loop (1+ c) (+ acc-x (vector-ref col-widths c) col-gap))))
			offsets))

		;; render a single row of cells at row-y
		(define (draw-grid-row! row row-y col-offsets)
          (for-each
           (lambda (col-idx cell)
             (let ((col-x (+ pad-x (vector-ref col-offsets col-idx))))
               (cairo-move-to cr col-x row-y)
               (pango-draw-text cr cell #:font font-str)))
           (iota (min num-cols (length row)))
           row))

		;; measure dimensions
		(for-each measure-row-cells! rows)

		;; render grid
		(let* ((line-h (+ (max font-size max-cell-h) row-gap))
               (col-offsets (compute-column-offsets)))
          (for-each
           (lambda (row-idx row)
             (draw-grid-row! row (+ pad-y (* row-idx line-h)) col-offsets))
           (iota (length rows))
           rows)))))))

(define (gleui-toast-render! state)
  "Render the toast view into persistent double-buffered shared memory and commit."
  (when (and state
             (%tvs-visible? state)
             (%tvs-configured? state)
             *wl-display*
             (pointer? *wl-compositor*)
             (not (null-pointer? *wl-compositor*))
             (pointer? *wl-shm*)
             (not (null-pointer? *wl-shm*)))
    (with-mutex (%tvs-render-mutex state)
      (let* ((surface (%tvs-surface state))
             (layer-surf (%tvs-layer-surface state))
             (w (%tvs-width state))
             (h (%tvs-height state))
             (overrides (%tvs-theme-overrides state)))
        (when (and surface (pointer? surface) (not (null-pointer? surface)) (> w 0) (> h 0))
          (let ((valid-slots (gleui-shm-slots-ensure! (%tvs-shm-slots state) w h)))
            (%tvs-shm-slots-set! state valid-slots)
            (when (and (pair? valid-slots) (car valid-slots) (cdr valid-slots))
              (let* ((next-idx (if (zero? (%tvs-shm-active-idx state)) 1 0))
                     (curr-slot (if (zero? next-idx) (car valid-slots) (cdr valid-slots)))
                     (bv (gleui-shm-slot-bv curr-slot))
                     (stride (gleui-shm-slot-stride curr-slot))
                     (wl-buf (gleui-shm-slot-buffer curr-slot)))
                (%tvs-shm-active-idx-set! state next-idx)
                (let* ((dst-surf (cairo-image-surface-create-for-data bv 'argb32 w h stride))
                       (cr (cairo-create dst-surf)))
                  (gleui-toast-draw-surface! cr w h state overrides)
                  (cairo-surface-flush dst-surf)
                  (cairo-destroy cr)
                  (cairo-surface-destroy dst-surf))
                (when (and layer-surf (pointer? layer-surf) (not (null-pointer? layer-surf)))
                  (zwlr-layer-surface-v1-set-size layer-surf w h))
                (gleui-buffer-commit! surface wl-buf w h)))))))))

(define (gleui-toast-ensure-surface! state)
  "Ensure Wayland surface and layer-surface are created and configured for toast STATE."
  (when (and state
             (or (not (%tvs-surface state))
                 (null-pointer? (%tvs-surface state))
                 (not (%tvs-layer-surface state))
                 (null-pointer? (%tvs-layer-surface state)))
             *connected*
             (pointer? *wl-compositor*)
             (not (null-pointer? *wl-compositor*))
             (pointer? *zwlr-layer-shell*)
             (not (null-pointer? *zwlr-layer-shell*)))
    (let* ((loc (or (assq-ref (%tvs-theme-overrides state) 'location)
                    (catch #t (lambda () (var-get '*gleui-toast-location*)) (lambda _ 2))
                    2))
           (anchor (gleui-numpad->anchor loc))
           (margin (or (assq-ref (%tvs-theme-overrides state) 'margin)
                       (catch #t (lambda () (var-get '*gleui-toast-margin*)) (lambda _ 35))
                       35))
           (w (%tvs-width state))
           (h (%tvs-height state)))
      (gleui-layer-surface-create
       #:namespace "gliver-toast"
       #:layer ZWLR_LAYER_SHELL_V1_LAYER_OVERLAY
       #:anchor anchor
       #:margin-top margin
       #:margin-right margin
       #:margin-bottom margin
       #:margin-left margin
       #:exclusive-zone -1
       #:keyboard-interactivity ZWLR_LAYER_SURFACE_V1_KEYBOARD_INTERACTIVITY_NONE
       #:width w
       #:height h
       #:on-created (lambda (surf layer-surf)
                      (%tvs-surface-set! state surf)
                      (%tvs-layer-surface-set! state layer-surf)
                      (%tvs-configured?-set! state #f))
       #:on-configure (lambda (cw ch)
                        (log-debug "gleui-toast: Layer surface configured ~ax~a" cw ch)
                        (%tvs-configured?-set! state #t)
                        (when (%tvs-visible? state)
                          (gleui-toast-render! state)))
       #:on-close (lambda ()
                    (log-info "gleui-toast: Layer surface closed")
                    (gleui-toast-kill))))))

(define* (gleui-toast-show message
                           #:key (timeout 0)
                           (name "")
                           (theme-overrides '())
                           #:allow-other-keys)
  "Display toast notification with MESSAGE using native Cairo & Pango.
MESSAGE can be a string, a list of strings, or a list of structured rows/columns.
If TIMEOUT > 0, auto-hides after TIMEOUT (seconds or ms)."
  (unless *gleui-toast-instance*
    (set! *gleui-toast-instance*
          (%make-toast-view-state #f #f #f 0 #f #f 0 0 0 0 #f #f #f "" 0 '() (make-mutex))))

  (let* ((state *gleui-toast-instance*)
         (parsed (gleui-toast-message-parse message)))
    (when (%tvs-visible? state)
      (gleui-toast-kill))

    (let ((cur-gen (1+ (%tvs-timeout-thread-gen state))))
      (%tvs-timeout-thread-gen-set! state cur-gen)
      (%tvs-parsed-content-set! state parsed)
      (%tvs-raw-message-set! state message)
      (%tvs-name-set! state (or name ""))
      (%tvs-theme-overrides-set! state theme-overrides)
      (%tvs-visible?-set! state #t)

      ;; calculate required width and height
      (call-with-values
          (lambda () (gleui-toast-calc-dimensions state))
        (lambda (w h)
          (%tvs-width-set! state w)
          (%tvs-height-set! state h)))

      ;; ensure surface is created and rendered
      (gleui-toast-ensure-surface! state)
      (when (and (%tvs-configured? state) (%tvs-visible? state))
        (gleui-toast-render! state))

      ;; set up auto-dismiss timer if timeout > 0
      (when (and (number? timeout) (> timeout 0))
        (let* ((t-num (exact->inexact timeout))
               (ms (if (> t-num 50.0) t-num (* t-num 1000.0)))
               (delay-usec (inexact->exact (round (* ms 1000.0)))))
          (call-with-new-thread
           (lambda ()
             (usleep delay-usec)
             (when (and (= (%tvs-timeout-thread-gen state) cur-gen)
                        (%tvs-visible? state))
               (gleui-toast-kill)))))))))

(define* (gleui-toast-kill #:key (name #f) #:allow-other-keys)
  "Dismiss the active toast notification.
If NAME is provided and non-empty, only dismiss if it matches the active toast's name."
  (let ((state *gleui-toast-instance*))
    (when (and state (%tvs-visible? state))
      (let ((curr-name (%tvs-name state)))
        (when (or (not name)
                  (not (string? name))
                  (string-null? name)
                  (string=? curr-name name))
          (%tvs-timeout-thread-gen-set! state (1+ (%tvs-timeout-thread-gen state)))
          (%tvs-visible?-set! state #f)
          (%tvs-configured?-set! state #f)
          (%tvs-parsed-content-set! state #f)
          (%tvs-raw-message-set! state #f)
          (%tvs-name-set! state "")
          (let ((layer-surf (%tvs-layer-surface state))
                (surface (%tvs-surface state)))
            (gleui-layer-surface-destroy! surface layer-surf)
            (%tvs-layer-surface-set! state #f)
            (%tvs-surface-set! state #f)))))))

(define gleui-toast-open! gleui-toast-show)

(define (gleui-toast-cleanup!)
  "Clean up all toast view resources on unbind/shutdown."
  (when *gleui-toast-instance*
    (let* ((t-state *gleui-toast-instance*)
           (t-slots (%tvs-shm-slots t-state)))
      (gleui-toast-kill)
      (when (pair? t-slots)
        (when (car t-slots) (gleui-shm-slot-destroy (car t-slots)))
        (when (cdr t-slots) (gleui-shm-slot-destroy (cdr t-slots)))
        (%tvs-shm-slots-set! t-state #f))
      (set! *gleui-toast-instance* #f))))

(define-command (gleui-toast-install!)
  "Install gleui-toast as the notification/message backend."
  (var-set! *toast-backend* gleui-toast-show)
  (var-set! *toast-backend-kill* gleui-toast-kill)
  (log-info "gleui-toast installed as message backend."))
