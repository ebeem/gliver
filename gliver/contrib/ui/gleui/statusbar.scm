;;; gliver/contrib/ui/gleui/statusbar.scm --- Waybar Inspired Statusbar for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; Usage:
;;;   (use-modules (gliver contrib ui gleui statusbar))
;;;   (statusbar-enable!)

(define-module (gliver contrib ui gleui statusbar)
  #:use-module (cairo)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-69)
  #:use-module (gliver core)
  #:use-module (gliver core config)
  #:use-module (gliver core logs)
  #:use-module (gliver core hooks)
  #:use-module (gliver core types)
  #:use-module (gliver core output)
  #:use-module (gliver core workspace)
  #:use-module (gliver core window)
  #:use-module (gliver core seat)
  #:use-module (gliver deps libc)
  #:use-module (gliver deps color)
  #:use-module (gliver deps pango)
  #:use-module (gliver wayland client)
  #:use-module (gliver wayland gen wayland)
  #:use-module (gliver wayland gen wlr-layer-shell-unstable-v1)
  #:use-module (gliver river connector)
  #:use-module (gliver contrib commands)
  #:use-module (gliver contrib ui statusbar base)
  #:use-module (gliver contrib ui statusbar)
  #:declarative? #f
  #:export (
			statusbar-output-state-hit-boxes
			statusbar-output-state-pointer-listener
			statusbar-output-state-pointer
			statusbar-output-state-listener
			statusbar-output-state-configured?
			statusbar-output-state-rendered-height
			statusbar-output-state-rendered-width
			statusbar-output-state-buffer
			statusbar-output-state-layer-surface
			statusbar-output-state-surface
			statusbar-output-state-output
			statusbar-output-state?
			*statusbar-output-table*
			*pointer-surface*
			*pointer-x*
			*pointer-y*
			*timer-thread-running?*
			*statusbar-timer-thread*
			statusbar-shm-buffer-create
			statusbar-resolve-modules
			statusbar-render-surface!
			measure-module
			render-module
			filter-visible
			measure-list
			statusbar-render-and-commit!
			statusbar-init-output!
			statusbar-cleanup-output!
			statusbar-cleanup-all!
			statusbar-find-hit-box
			statusbar-seat-setup!
			statusbar-update-all-modules!
			*statusbar-last-tick-time*
			statusbar-on-tick
			statusbar-render-output!
			statusbar-render-all!
			statusbar-active-modules
			statusbar-attach-active-module-hooks!
			statusbar-update-all!
			statusbar-start-timer-thread!
			statusbar-on-output-created
			statusbar-on-output-dimensions
			statusbar-enable!
			statusbar-disable!
			statusbar-toggle!
			statusbar-reload!
))

(define-record-type <statusbar-output-state>
  (%make-statusbar-output-state output surface layer-surface buffer
                                rendered-width rendered-height configured?
                                listener pointer pointer-listener hit-boxes)
  statusbar-output-state?
  (output           statusbar-output-state-output)
  (surface          statusbar-output-state-surface          %statusbar-output-state-surface-set!)
  (layer-surface    statusbar-output-state-layer-surface    %statusbar-output-state-layer-surface-set!)
  (buffer           statusbar-output-state-buffer           %statusbar-output-state-buffer-set!)
  (rendered-width   statusbar-output-state-rendered-width   %statusbar-output-state-rendered-width-set!)
  (rendered-height  statusbar-output-state-rendered-height  %statusbar-output-state-rendered-height-set!)
  (configured?      statusbar-output-state-configured?      %statusbar-output-state-configured?-set!)
  (listener         statusbar-output-state-listener         %statusbar-output-state-listener-set!)
  (pointer          statusbar-output-state-pointer          %statusbar-output-state-pointer-set!)
  (pointer-listener statusbar-output-state-pointer-listener %statusbar-output-state-pointer-listener-set!)
  (hit-boxes        statusbar-output-state-hit-boxes        %statusbar-output-state-hit-boxes-set!))

;; each output has its own statusbar instance
(define *statusbar-output-table* (make-hash-table))

;; shared state for pointer tracking
(define *pointer-surface* %null-pointer)
(define *pointer-x* 0.0)
(define *pointer-y* 0.0)
(define *timer-thread-running?* #f)
(define *statusbar-timer-thread* #f)

(define (statusbar-shm-buffer-create width height render-fn)
  "Allocate a shared memory wl_buffer of size WxH and populate it using RENDER-FN.
RENDER-FN receives (cr width height). Returns the new wl_buffer foreign pointer."
  (catch #t
    (lambda ()
      (let* ((stride (* width 4))     ;; width * 4 bytes per pixel ARGB32
             (size (* stride height)) ;; stride * height
             (fd (memfd-create "gliver-statusbar-shm" *mfd-cloexec*))
             (ptr (begin
                    (ftruncate fd size)
                    (mmap %null-pointer size *prot-read-write* *map-shared* fd 0)))
             (bv (pointer->bytevector ptr size))
             (surf (cairo-image-surface-create-for-data bv 'argb32 width height stride))
             (cr (cairo-create surf)))
        ;; render into mmap memory
        (render-fn cr width height)
        (munmap ptr size)
        ;; create wayland buffer and clean up
        (let* ((pool   (wl-shm-create-pool *wl-shm* fd size))
               (buffer (wl-shm-pool-create-buffer pool 0 width height stride WL_SHM_FORMAT_ARGB8888)))
          (wl-shm-pool-destroy pool)
          (close-fd fd)
          buffer)))

    (lambda (key . args)
      (log-error "Statusbar: SHM buffer creation failed: ~a ~a" key args)
      %null-pointer)))

(define (statusbar-resolve-modules mod-list)
  "Convert a list of module names/definitions into <statusbar-module> records.
This accepts both symbols (default params) and procedures (custom params).
Example: (list 'window (make-module-mpd))"
  (filter statusbar-module? (map statusbar-ensure-module mod-list)))

(define (statusbar-render-surface! output state cr width height)
  "Render the full statusbar for OUTPUT onto Cairo context CR."
  (let* ((bar-x 0)
         (bar-y 0)
         (bar-w width)
         (bar-h height)
         (font (or *statusbar-font* *theme-font* "Sans"))
         (font-size (or *statusbar-font-size* *theme-font-size* 11))
         (new-hit-boxes '())
         (record-hit-box!
          (lambda (x1 y1 x2 y2 mod custom-data)
            (set! new-hit-boxes
                  (cons (vector x1 y1 x2 y2 mod custom-data)
                        new-hit-boxes)))))

    ;; transparent surface
    (cairo-set-operator cr 'clear)
    (cairo-paint cr)
    (cairo-set-operator cr 'over)

    ;; draw background
    (when (and *statusbar-bg-color* (> (string-length *statusbar-bg-color*) 0))
      (let ((bg-rgba (parse-hex-color-rgba *statusbar-bg-color*)))
        (when bg-rgba
          (apply (lambda (r g b a) (cairo-set-source-rgba cr r g b a)) bg-rgba)
          (cairo-rounded-rectangle cr bar-x bar-y bar-w bar-h *statusbar-border-radius*)
          (cairo-fill cr))))

    ;; draw border
    (when (and (> *statusbar-border-width* 0)
               *statusbar-border-color*
               (> (string-length *statusbar-border-color*) 0))
      (let ((border-rgba (parse-hex-color-rgba *statusbar-border-color*)))
        (when border-rgba
          (apply (lambda (r g b a) (cairo-set-source-rgba cr r g b a)) border-rgba)
          (cairo-set-line-width cr *statusbar-border-width*)
          (cairo-rounded-rectangle cr bar-x bar-y bar-w bar-h *statusbar-border-radius*)
          (cairo-stroke cr))))

    ;; compute dimensions of a single module
    (define (measure-module mod)
      (let ((m-fn (statusbar-module-measure-fn mod)))
        (if (procedure? m-fn)
            (m-fn cr mod output)
            (let* ((txt (or (statusbar-module-text mod) ""))
                   (pad-x (or (statusbar-module-padding-x mod) *statusbar-pill-padding-x*))
                   (pad-y (or (statusbar-module-padding-y mod) *statusbar-pill-padding-y*)))
              (if (string-null? txt)
                  (values 0 0)
                  (let-values (((tw th) (pango-measure-text cr txt #:font font #:font-size font-size)))
                    (values (+ tw (* pad-x 2))
                            (+ th (* pad-y 2)))))))))

    ;; draw a single module
    (define (render-module mod x y mod-w mod-h)
      (let ((r-fn (statusbar-module-render-fn mod)))
        (if (procedure? r-fn)
            (r-fn cr x y mod-w mod-h mod output record-hit-box!)
            (let* ((txt (or (statusbar-module-text mod) ""))
                   (pad-x (or (statusbar-module-padding-x mod) *statusbar-pill-padding-x*))
                   (pad-y (or (statusbar-module-padding-y mod) *statusbar-pill-padding-y*))
                   (radius (or (statusbar-module-border-radius mod) *statusbar-pill-radius*))
                   (bg (or (statusbar-module-bg-color mod) *theme-bg-main*))
                   (fg (or (statusbar-module-fg-color mod) *statusbar-fg-color* *theme-text*))
                   (pill-h (min bar-h mod-h))
                   (pill-y (+ y (/ (- bar-h pill-h) 2.0))))
              (unless (string-null? txt)

                ;; module background
                (when bg
                  (let ((rgba-bg (parse-hex-color-rgba bg)))
                    (when rgba-bg
                      (apply (lambda (r g b a) (cairo-set-source-rgba cr r g b a)) rgba-bg)
                      (cairo-rounded-rectangle cr x pill-y mod-w pill-h radius)
                      (cairo-fill cr))))

                ;; module border
                (let ((b-color (statusbar-module-border-color mod))
                      (b-width (or (statusbar-module-border-width mod) 0)))
                  (when (and b-color (> b-width 0))
                    (let ((rgba-b (parse-hex-color-rgba b-color)))
                      (when rgba-b
                        (apply (lambda (r g b a) (cairo-set-source-rgba cr r g b a)) rgba-b)
                        (cairo-set-line-width cr b-width)
                        (cairo-rounded-rectangle cr x pill-y mod-w pill-h radius)
                        (cairo-stroke cr)))))

                ;; text & icon
                (let-values (((tw th) (pango-measure-text cr txt #:font font #:font-size font-size)))
                  (let ((rgba-fg (parse-hex-color-rgba fg))
                        (tx (+ x pad-x))
                        (ty (+ pill-y (/ (- pill-h th) 2.0))))
                    (when rgba-fg
                      (apply (lambda (r g b a) (cairo-set-source-rgba cr r g b a)) rgba-fg)
                      (cairo-move-to cr tx ty)
                      (pango-draw-text cr txt
                                       #:x tx
                                       #:y ty
                                       #:font font
                                       #:font-size font-size
                                       #:markup? #f))))

                ;; record hit box for clicks
                (record-hit-box! x pill-y (+ x mod-w) (+ pill-y pill-h) mod #f))))))

    (define (filter-visible mods)
	  "Filter active visible modules."
      (filter (lambda (mod)
                (let ((visible? (statusbar-module-visible? mod)))
                  (if (procedure? visible?)
					  ;; visible? maybe a lambda procedure (module output) => bool
					  (visible? mod output)
					  visible?)))
              mods))

    (let* ((left-mods (filter-visible (statusbar-resolve-modules *statusbar-modules-left*)))
           (center-mods (filter-visible (statusbar-resolve-modules *statusbar-modules-center*)))
           (right-mods (filter-visible (statusbar-resolve-modules *statusbar-modules-right*)))
           (spacing *statusbar-spacing*))

      (define (measure-list mods)
		"Measure list of modules, returns list of (mod . (width . height))."
        (map (lambda (mod)
               (let-values (((mw mh) (measure-module mod)))
                 (cons mod (cons mw mh))))
             mods))

      (let* ((left-measured (measure-list left-mods))
             (center-measured (measure-list center-mods))
             (right-measured (measure-list right-mods))
             (sum-widths (lambda (measured)
                           (if (null? measured)
                               0
                               (+ (apply + (map cadr measured))
                                  (* (max 0 (- (length measured) 1)) spacing))))))

		;; render a sequence of measured modules starting from start-x
		(define (render-section-modules! items start-x)
		  (let loop ((rem items) (cur-x start-x))
			(when (pair? rem)
			  (match (car rem)
				((mod mw . mh)
				 (when (> mw 0)
				   (render-module mod cur-x bar-y mw mh))
				 (loop (cdr rem) (if (> mw 0) (+ cur-x mw spacing) cur-x)))))))

		;; render left section
		(render-section-modules! left-measured (+ bar-x *statusbar-padding-x*))

		;; render right section
		(let* ((right-total-w (sum-widths right-measured))
			   (right-start-x (- (+ bar-x bar-w) *statusbar-padding-x* right-total-w)))
		  (render-section-modules! right-measured right-start-x))

		;; render center section
		(let* ((center-total-w (sum-widths center-measured))
			   (center-start-x (+ bar-x (/ (- bar-w center-total-w) 2.0))))
		  (render-section-modules! center-measured center-start-x))))

    ;; store updated hit-boxes in output state
    (%statusbar-output-state-hit-boxes-set! state (reverse new-hit-boxes))))

(define (statusbar-render-and-commit! output state)
  "Render statusbar surface for OUTPUT and commit to Wayland compositor."
  (let* ((width (statusbar-output-state-rendered-width state))
         (height (statusbar-output-state-rendered-height state))
         (surface (statusbar-output-state-surface state)))
    (when (and (> width 0) (> height 0) (pointer? surface) (not (null-pointer? surface)))
      (let ((new-buffer (statusbar-shm-buffer-create
                         width height
                         (lambda (cr w h)
                           (statusbar-render-surface! output state cr w h))))
            (old-buffer (statusbar-output-state-buffer state)))
        (when (and (pointer? new-buffer) (not (null-pointer? new-buffer)))
          (%statusbar-output-state-buffer-set! state new-buffer)
          (wl-surface-attach surface new-buffer 0 0)
          (wl-surface-damage surface 0 0 width height)
          (wl-surface-commit surface)
          (when *wl-display*
            (wl-display-flush *wl-display*))
          (when (and (pointer? old-buffer) (not (null-pointer? old-buffer))
                     (not (equal? old-buffer new-buffer)))
            (wl-buffer-destroy old-buffer)))))))

(define (statusbar-init-output! output)
  "Initialize the statusbar layer surface on OUTPUT."
  (define (valid-pointer? p)
    (and (pointer? p) (not (null-pointer? p))))

  (define (globals-ready?)
    (and *statusbar-enabled*
         (valid-pointer? *wl-compositor*)
         (valid-pointer? *zwlr-layer-shell*)
         (valid-pointer? *wl-shm*)))

  (define (resolve-target-wl-output)
    (let ((obj-id (output-wl-output output)))
      (if (and (number? obj-id)
               (valid-pointer? *wl-registry*))
          (let ((bound (gliver-wl-registry-bind *wl-registry* obj-id *wl-output-interface* 4)))
            (if (valid-pointer? bound) bound %null-pointer))
          %null-pointer)))

  (define (calculate-anchor)
    (if (eq? *statusbar-position* 'bottom)
        (+ ZWLR_LAYER_SURFACE_V1_ANCHOR_BOTTOM
           ZWLR_LAYER_SURFACE_V1_ANCHOR_LEFT
           ZWLR_LAYER_SURFACE_V1_ANCHOR_RIGHT)
        (+ ZWLR_LAYER_SURFACE_V1_ANCHOR_TOP
           ZWLR_LAYER_SURFACE_V1_ANCHOR_LEFT
           ZWLR_LAYER_SURFACE_V1_ANCHOR_RIGHT)))

  (define (calculate-initial-width)
    (max 100 (- (output-width output) *statusbar-margin-left* *statusbar-margin-right*)))

  (define (configure-layer-surface! layer-surf anchor total-height)
    ;; width 0 asks the compositor to allocate full width according to anchor
    (zwlr-layer-surface-v1-set-size layer-surf 0 *statusbar-height*)
    (zwlr-layer-surface-v1-set-anchor layer-surf anchor)
    (zwlr-layer-surface-v1-set-margin
     layer-surf
     *statusbar-margin-top*
     *statusbar-margin-right*
     *statusbar-margin-bottom*
     *statusbar-margin-left*)
    (zwlr-layer-surface-v1-set-exclusive-zone layer-surf total-height)
    (zwlr-layer-surface-v1-set-keyboard-interactivity
     layer-surf
     ZWLR_LAYER_SURFACE_V1_KEYBOARD_INTERACTIVITY_NONE))

  (define (update-usable-area! total-height)
    (let* ((cur-w  (output-width output))
           (cur-h  (output-height output))
           (new-uy (if (eq? *statusbar-position* 'bottom) 0 total-height))
           (new-uh (max 0 (- cur-h total-height))))
      (output-usable-area-set! output 0 new-uy cur-w new-uh)
      (gliver-hook-run! *output-change-hook* output)))

  (define (make-output-listener state)
    (define (handle-configure data proxy serial width height)
      (zwlr-layer-surface-v1-ack-configure proxy serial)
      (let ((w (if (> width 0) width (calculate-initial-width)))
            (h (if (> height 0) height *statusbar-height*)))
        (%statusbar-output-state-rendered-width-set! state w)
        (%statusbar-output-state-rendered-height-set! state h)
        (%statusbar-output-state-configured?-set! state #t)
        (statusbar-update-all-modules! output)
        (statusbar-render-and-commit! output state)))

    (define (handle-close data proxy)
      (log-info "Statusbar layer surface closed for output ~a" (output-name output))
      (statusbar-cleanup-output! output))

    (make-zwlr-layer-surface-v1-listener handle-configure handle-close))

  (when (and (globals-ready?)
             (not (hash-table-ref/default *statusbar-output-table* (output-id output) #f)))
    (log-info "Statusbar: Initializing statusbar on output ~a..." (output-name output))

    (let* ((target-wl-out (resolve-target-wl-output))
           (surface (wl-compositor-create-surface *wl-compositor*))
           (layer-surf (zwlr-layer-shell-v1-get-layer-surface
                        *zwlr-layer-shell*
                        surface
                        target-wl-out
                        ZWLR_LAYER_SHELL_V1_LAYER_TOP
                        "gliver-statusbar"))
           (anchor (calculate-anchor))
           (total-height (+ *statusbar-height* *statusbar-margin-top* *statusbar-margin-bottom*))
           (init-width (calculate-initial-width))
           (state (%make-statusbar-output-state
                   output surface layer-surf %null-pointer
                   init-width *statusbar-height* #f #f #f #f '()))
           (listener (make-output-listener state)))

      (configure-layer-surface! layer-surf anchor total-height)
      (%statusbar-output-state-listener-set! state listener)
      (wl-proxy-add-listener layer-surf listener %null-pointer)
      (update-usable-area! total-height)
      (wl-surface-commit surface)
      (when *wl-display*
        (wl-display-flush *wl-display*))
      (hash-table-set! *statusbar-output-table* (output-id output) state))))

(define (statusbar-cleanup-output! output)
  "Destroy statusbar layer surface and clean up resources for OUTPUT."
  (when (and output (output? output))
    (let ((state (hash-table-ref/default *statusbar-output-table* (output-id output) #f)))
      (when state
        (hash-table-delete! *statusbar-output-table* (output-id output))
        (let ((layer-surf (statusbar-output-state-layer-surface state))
              (surface (statusbar-output-state-surface state))
              (buffer (statusbar-output-state-buffer state)))
          (when (and (pointer? layer-surf) (not (null-pointer? layer-surf)))
            (catch #t (lambda () (zwlr-layer-surface-v1-destroy layer-surf)) (lambda _ #f)))
          (when (and (pointer? surface) (not (null-pointer? surface)))
            (catch #t (lambda () (wl-surface-destroy surface)) (lambda _ #f)))
          (when (and (pointer? buffer) (not (null-pointer? buffer)))
            (catch #t (lambda () (wl-buffer-destroy buffer)) (lambda _ #f))))
        ;; restore usable area to full output dimensions
        (output-usable-area-set! output 0 0 (output-width output) (output-height output))
        (gliver-hook-run! *output-change-hook* output)))))

(define (statusbar-cleanup-all!)
  "Clean up all statusbar resources across all outputs."
  (set! *statusbar-enabled* #f)
  (statusbar-detach-module-hooks!)
  (set! *timer-thread-running?* #f)
  (when (and *statusbar-timer-thread* (thread? *statusbar-timer-thread*))
    (catch #t (lambda () (cancel-thread *statusbar-timer-thread*)) (lambda _ #f))
    (set! *statusbar-timer-thread* #f))
  (let ((states (hash-table-values *statusbar-output-table*)))
    (for-each
     (lambda (state)
       (let ((output (statusbar-output-state-output state)))
         (statusbar-cleanup-output! output)))
     states)))

(define (statusbar-find-hit-box state px py)
  "Find a module hit-box matching point (PX, PY) in STATE."
  (let ((boxes (statusbar-output-state-hit-boxes state)))
    (find (lambda (box)
            (let ((x1 (vector-ref box 0))
                  (y1 (vector-ref box 1))
                  (x2 (vector-ref box 2))
                  (y2 (vector-ref box 3)))
              (and (>= px x1) (<= px x2)
                   (>= py y1) (<= py y2))))
          boxes)))

(define (statusbar-seat-setup!)
  "Set up Wayland pointer listener on the current seat for mouse interaction."
  ;; I prefer creating something public for valid-pointer?
  ;; it's a common pattern used pretty much everywhere where ffi is used
  (define (valid-pointer? p)
    (and (pointer? p) (not (null-pointer? p))))

  (define (find-focused-output-and-hit)
    "Find the output state and hit box matching the current pointer position."
    (and (valid-pointer? *pointer-surface*)
         (let* ((surface-addr (pointer-address *pointer-surface*))
                (state (find (lambda (s)
                               (let ((surf (statusbar-output-state-surface s)))
                                 (and (valid-pointer? surf)
                                      (= (pointer-address surf) surface-addr))))
                             (hash-table-values *statusbar-output-table*))))
           (and state
                (let ((hit (statusbar-find-hit-box state *pointer-x* *pointer-y*)))
                  (and hit (cons state hit)))))))

  (define (handle-enter data proxy serial surface sx sy)
    (set! *pointer-surface* surface)
    (set! *pointer-x* (/ sx 256.0))
    (set! *pointer-y* (/ sy 256.0)))

  (define (handle-leave data proxy serial surface)
    (set! *pointer-surface* %null-pointer))

  (define (handle-motion data proxy time sx sy)
    (set! *pointer-x* (/ sx 256.0))
    (set! *pointer-y* (/ sy 256.0)))

  (define (handle-button data proxy serial time button state-val)
    ;; state-val = 1 means button pressed down
    (when (= state-val 1)
      (let ((match (find-focused-output-and-hit)))
        (when match
          (let* ((state (car match))
                 (hit (cdr match))
                 (mod (vector-ref hit 4))
                 (custom-data (vector-ref hit 5))
                 (click-fn (statusbar-module-on-click mod))
                 (out (statusbar-output-state-output state)))
            (when (procedure? click-fn)
              (click-fn button *pointer-x* *pointer-y* mod out custom-data)
              (statusbar-render-output! out)))))))

  (define (handle-axis data proxy time axis value)
    ;; axis = 0 means vertical scroll
    (when (zero? axis)
      (let ((match (find-focused-output-and-hit)))
        (when match
          (let* ((state (car match))
                 (hit (cdr match))
                 (mod (vector-ref hit 4))
                 (scroll-fn (statusbar-module-on-scroll mod))
                 (out (statusbar-output-state-output state)))
            (when (procedure? scroll-fn)
              (scroll-fn axis (/ value 256.0) mod out)
              (statusbar-render-output! out)))))))

  (define (noop-handler . _args) #t)

  (when (and *statusbar-click-enabled*
             (valid-pointer? *wl-registry*))
    (let* ((seat        (seat-current))
           (seat-obj-id (and seat (seat-wl-seat seat)))
           (wl-seat     (and (number? seat-obj-id)
                             (gliver-wl-registry-bind *wl-registry* seat-obj-id *wl-seat-interface* 7))))
      (when (valid-pointer? wl-seat)
        (let ((ptr (wl-seat-get-pointer wl-seat)))
          (when (valid-pointer? ptr)
            (let ((listener (make-wl-pointer-listener
                             handle-enter
                             handle-leave
                             handle-motion
                             handle-button
                             handle-axis
                             noop-handler   ; on-frame
                             noop-handler   ; axis-source
                             noop-handler   ; axis-stop
                             noop-handler   ; axis-discrete
                             noop-handler   ; axis-v120
                             noop-handler))) ; axis-dir
              (wl-proxy-add-listener ptr listener %null-pointer))))))))

(define (statusbar-update-all-modules! output)
  "Update all modules for OUTPUT."
  (let ((all-mods (delete-duplicates
                   (append (statusbar-resolve-modules *statusbar-modules-left*)
                           (statusbar-resolve-modules *statusbar-modules-center*)
                           (statusbar-resolve-modules *statusbar-modules-right*)))))
    (for-each (lambda (mod)
                (statusbar-module-update! mod output))
              all-mods)))

(define *statusbar-last-tick-time* 0)

(define (statusbar-on-tick)
  "Periodic tick handler. Checks module intervals and updates as needed.
Runs at most once per second and only re-renders when module contents change."
  (when *statusbar-enabled*
    (let ((now (current-time)))
      ;; only check intervals once per second even if called every frame
      (when (> now *statusbar-last-tick-time*)
        (set! *statusbar-last-tick-time* now)
        (let ((dirty? #f)
              (all-mods (delete-duplicates
                         (append (statusbar-resolve-modules *statusbar-modules-left*)
                                 (statusbar-resolve-modules *statusbar-modules-center*)
                                 (statusbar-resolve-modules *statusbar-modules-right*)))))
          (for-each
           (lambda (mod)
             (let ((iv (statusbar-module-interval mod)))
               (when (and (number? iv) (> iv 0))
                 (let ((last (statusbar-module-last-poll mod)))
                   (when (>= (- now last) iv)
                     (let ((old-text (statusbar-module-text mod))
                           (old-fg (statusbar-module-fg-color mod))
                           (old-bg (statusbar-module-bg-color mod))
                           (cur-out (output-current)))
                       (statusbar-module-update! mod cur-out)
                       (when (or (not (equal? old-text (statusbar-module-text mod)))
                                 (not (equal? old-fg (statusbar-module-fg-color mod)))
                                 (not (equal? old-bg (statusbar-module-bg-color mod))))
                         (set! dirty? #t))))))))
           all-mods)

          (when dirty?
            (statusbar-render-all!)))))))

(define (statusbar-render-output! output)
  "Re-render statusbar on a specific OUTPUT."
  (when (and *statusbar-enabled* output)
    (let ((state (hash-table-ref/default *statusbar-output-table* (output-id output) #f)))
      (if (and state (statusbar-output-state-configured? state))
          (statusbar-render-and-commit! output state)
          (statusbar-init-output! output)))))

(define (statusbar-render-all!)
  "Re-render statusbars across all active outputs."
  (when (and *statusbar-enabled* *manager*)
    (for-each (lambda (output)
                (let ((state (hash-table-ref/default *statusbar-output-table* (output-id output) #f)))
                  (when (and state (statusbar-output-state-configured? state))
                    (statusbar-render-and-commit! output state))))
              (manager-outputs *manager*))))

(define (statusbar-active-modules)
  "Return list of all configured module instances across all sections."
  (delete-duplicates
   (append (statusbar-resolve-modules *statusbar-modules-left*)
           (statusbar-resolve-modules *statusbar-modules-center*)
           (statusbar-resolve-modules *statusbar-modules-right*))))

(define (statusbar-attach-active-module-hooks!)
  "Attach event hooks for all active statusbar modules."
  (statusbar-attach-module-hooks! (statusbar-active-modules)))

(define (statusbar-update-all!)
  "Force immediate update of all modules and re-render."
  (when (and *statusbar-enabled* *manager*)
    (statusbar-attach-active-module-hooks!)
    (for-each (lambda (output)
                (statusbar-update-all-modules! output))
              (manager-outputs *manager*))
    (statusbar-render-all!)))

(define (statusbar-start-timer-thread!)
  "Start background timer thread that wakes up every 1s to ensure periodic updates."
  (unless *timer-thread-running?*
    (set! *timer-thread-running?* #t)
    (set! *statusbar-timer-thread*
          (call-with-new-thread
           (lambda ()
             (let loop ()
               (when *statusbar-enabled*
                 (sleep 1)
                 ;; when river is connected, *manager-tick-hook* handles updates on the main thread.
                 ;; only drive tick from thread if not connected to river.
                 (unless (river-connected?)
                   (catch #t
                     (lambda () (statusbar-on-tick))
                     (lambda _ #f)))
                 (loop)))
             (set! *timer-thread-running?* #f))))))

(define (statusbar-on-output-created output)
  (when *statusbar-enabled*
    (statusbar-init-output! output)))

(define (statusbar-on-output-dimensions output prev-w prev-h)
  (when *statusbar-enabled*
    (let ((state (hash-table-ref/default *statusbar-output-table* (output-id output) #f)))
      (when state
        (let ((layer-surf (statusbar-output-state-layer-surface state))
              (surface (statusbar-output-state-surface state))
              (total-h (+ *statusbar-height* *statusbar-margin-top* *statusbar-margin-bottom*)))
          (unless (null-pointer? layer-surf)
            (zwlr-layer-surface-v1-set-size layer-surf 0 *statusbar-height*)
            (zwlr-layer-surface-v1-set-exclusive-zone layer-surf total-h)
            (when (and (pointer? surface) (not (null-pointer? surface)))
              (wl-surface-commit surface))))))))

(gliver-hook-add! *statusbar-render-request-hook* 'statusbar-render-all!)
(gliver-hook-add! *output-created-hook* 'statusbar-on-output-created)
(gliver-hook-add! *output-dimensions-changed-hook* 'statusbar-on-output-dimensions)
(gliver-hook-add! *output-destroy-hook* 'statusbar-cleanup-output!)
(gliver-hook-add! *output-removed-hook* 'statusbar-cleanup-output!)
(gliver-hook-add! *gliver-globals-unbind-hook* 'statusbar-cleanup-all!)
(gliver-hook-add! *manager-tick-hook* 'statusbar-on-tick)

(define-command (statusbar-enable!)
  "Enable and show the statusbar across all outputs."
  (var-set! *statusbar-enabled* #t)
  (log-info "Statusbar: enabling...")
  (when *manager*
    (for-each statusbar-init-output! (manager-outputs *manager*)))
  (statusbar-seat-setup!)
  (statusbar-start-timer-thread!)
  (statusbar-attach-active-module-hooks!)
  (statusbar-update-all!))

(define-command (statusbar-disable!)
  "Disable and hide the statusbar."
  (var-set! *statusbar-enabled* #f)
  (log-info "Statusbar: disabling...")
  (statusbar-cleanup-all!)
  (statusbar-clear-module-cache!)
  (set! *statusbar-last-tick-time* 0))

(define-command (statusbar-toggle!)
  "Toggle statusbar visibility."
  (if *statusbar-enabled*
      (statusbar-disable!)
      (statusbar-enable!)))

(define-command (statusbar-reload!)
  "Reload all statusbar modules and re-render."
  (log-info "Statusbar: reloading...")
  (statusbar-clear-module-cache!)
  (set! *statusbar-last-tick-time* 0)
  (statusbar-update-all!))
