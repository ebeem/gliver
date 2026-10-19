;;; gliver/contrib/ui/container/container-tab-indicator.scm --- Container tab indicator
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; Usage:
;;;   (use-modules (gliver contrib ui container container-tab-indicator))
;;;   (container-tab-indicator-enable!)

(define-module (gliver contrib ui container container-tab-indicator)
  #:use-module (cairo)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (system foreign)
  #:use-module (gliver deps libc)
  #:use-module (gliver deps color)
  #:use-module (gliver deps pango)
  #:use-module (gliver core)
  #:use-module (gliver wayland client)
  #:use-module (gliver wayland gen wayland)
  #:use-module (gliver river connector)
  #:use-module (gliver river window-manager)
  #:use-module (gliver river wm-shell-surface-manager)
  #:use-module (gliver river wm-node-manager)
  #:declarative? #f
  #:export (
			*%container-tab-indicator-enabled*
			*container-tab-indicator-last-pointer-x*
			*container-tab-indicator-last-pointer-y*
			valid-pointer?
			*container-tab-indicator-seat-proxies*
			*container-tab-indicator-pointer-surface*
			container-tab-indicator-state?
			make-container-tab-indicator-state
			*container-tab-indicator-table*
			container-tab-indicator-state-get
			container-tab-indicator-state-get-create!
			container-tab-indicator-wl-surface
			container-tab-indicator-wl-shell-surface
			container-tab-indicator-wl-node
			container-tab-indicator-wl-buffer
			container-tab-indicator-find-by-shell-surface
			container-tab-indicator-find-by-surface
			container-tab-indicator-shm-buffer-create
			container-tab-indicator-buffer-create
			apply-hex-color
			window-title-text
			tab-colors
			render-tab-background
			render-tab-border
			render-tab-title
			render-single-tab
			container-tab-indicator-update-input-region!
			container-indicator-signature
			container-tab-indicator-init!
			container-tab-indicator-should-show?
			container-tab-indicator-update-usable-area!
			container-tab-indicator-cleanup!
			container-tab-indicator-hide!
			container-tab-indicator-render!
			container-tab-indicators-update-all!
			container-tab-indicator-on-container-created
			container-tab-indicator-on-container-destroy
			container-tab-indicator-on-container-focused
			container-tab-indicator-on-container-unfocused
			container-tab-indicator-on-container-resize
			container-tab-indicator-on-workspace-switch
			container-tab-indicator-on-window-focused
			container-tab-indicator-on-window-unfocused
			container-tab-indicator-on-window-title-changed
			container-tab-indicator-on-window-app-id-changed
			container-tab-indicator-on-window-urgent
			container-tab-indicator-on-window-container-added
			container-tab-indicator-on-window-container-removed
			container-tab-indicator-on-window-container-moved
			container-tab-indicator-on-window-destroyed
			container-tab-indicator-on-pointer-position
			container-tab-indicator-on-shell-interaction
			container-tab-indicator-find-at-pointer
			container-tab-indicator-switch-window!
			container-tab-indicator-on-scroll
			container-tab-indicator-handle-pointer-enter
			container-tab-indicator-handle-pointer-leave
			container-tab-indicator-handle-pointer-motion
			container-tab-indicator-handle-pointer-button
			container-tab-indicator-handle-pointer-axis
			container-tab-indicator-handle-pointer-frame
			container-tab-indicator-handle-pointer-axis-source
			container-tab-indicator-handle-pointer-axis-stop
			container-tab-indicator-handle-pointer-axis-discrete
			container-tab-indicator-handle-pointer-axis-value120
			container-tab-indicator-handle-pointer-axis-relative-direction
			container-tab-indicator-seat-setup!
			container-tab-indicator-seat-cleanup!
			container-tab-indicator-on-seat-object-id-changed
			container-tab-indicator-on-globals-unbind
			container-tab-indicator-on-listeners-attach
			container-tab-indicator-on-window-fullscreen-entered
			container-tab-indicator-on-window-fullscreen-exited
			container-tab-indicator-enable!
			container-tab-indicator-disable!
			container-tab-indicator-enabled?
))

(define-var *%container-tab-indicator-enabled* #f
  "Internal state indicating whether container window indicators are enabled.")

(define *container-tab-indicator-last-pointer-x* #f)
(define *container-tab-indicator-last-pointer-y* #f)

(define (valid-pointer? p)
  "Ensure P is a non-null pointer."
  (and (pointer? p) (not (null-pointer? p))))

(define *container-tab-indicator-seat-proxies* '())
(define *container-tab-indicator-pointer-surface* %null-pointer)

;;; indicator state for each container
(define-record-type <container-tab-indicator-state>
  (make-container-tab-indicator-state wl-surface wl-shell-surface wl-node wl-buffer width height signature)
  container-tab-indicator-state?
  (wl-surface       %cwi-wl-surface       %cwi-wl-surface-set!)
  (wl-shell-surface %cwi-wl-shell-surface %cwi-wl-shell-surface-set!)
  (wl-node          %cwi-wl-node          %cwi-wl-node-set!)
  (wl-buffer        %cwi-wl-buffer        %cwi-wl-buffer-set!)
  (width            %cwi-width            %cwi-width-set!)
  (height           %cwi-height           %cwi-height-set!)
  (signature        %cwi-signature        %cwi-signature-set!))

;; hash-table to map each container to its window indicator state
(define *container-tab-indicator-table* (make-hash-table))

(define (container-tab-indicator-state-get container)
  "Get the indicator state associated with CONTAINER."
  (and (container? container)
       (hashq-ref *container-tab-indicator-table* container)))

(define (container-tab-indicator-state-get-create! container)
  "Get the indicator state associated with CONTAINER.
State will be created if it doesn't exist."
  (or (container-tab-indicator-state-get container)
      (let ((state (make-container-tab-indicator-state #f #f #f #f 0 0 #f)))
        (hashq-set! *container-tab-indicator-table* container state)
        state)))

(define (container-tab-indicator-wl-surface container)
  "Get Wayland surface for CONTAINER's indicator."
  (and-let* ((state (container-tab-indicator-state-get container)))
    (%cwi-wl-surface state)))

(define (container-tab-indicator-wl-shell-surface container)
  "Get Wayland shell surface for CONTAINER's indicator."
  (and-let* ((state (container-tab-indicator-state-get container)))
    (%cwi-wl-shell-surface state)))

(define (container-tab-indicator-wl-node container)
  "Get Wayland node proxy for CONTAINER's indicator."
  (and-let* ((state (container-tab-indicator-state-get container)))
    (%cwi-wl-node state)))

(define (container-tab-indicator-wl-buffer container)
  "Get Wayland buffer for CONTAINER's indicator."
  (and-let* ((state (container-tab-indicator-state-get container)))
    (%cwi-wl-buffer state)))

(define (container-tab-indicator-find-by-shell-surface shell-surf)
  "Find the container associated with indicator SHELL-SURF pointer."
  (if (or (not (pointer? shell-surf)) (null-pointer? shell-surf))
      #f
      (let ((addr (pointer-address shell-surf))
            (found #f))
        (hash-for-each
         (lambda (c state)
           (let ((surf (%cwi-wl-shell-surface state)))
             (when (and (pointer? surf)
                        (not (null-pointer? surf))
                        (= (pointer-address surf) addr))
               (set! found c))))
         *container-tab-indicator-table*)
        found)))

(define (container-tab-indicator-find-by-surface surface)
  "Find the container associated with indicator SURFACE pointer."
  (if (not (valid-pointer? surface))
      #f
      (let ((addr (pointer-address surface))
            (found #f))
        (hash-for-each
         (lambda (c state)
           (let ((surf (%cwi-wl-surface state)))
             (when (and (valid-pointer? surf)
                        (= (pointer-address surf) addr))
               (set! found c))))
         *container-tab-indicator-table*)
        found)))

(define (container-tab-indicator-shm-buffer-create width height fill-proc!)
  "Allocate a shared memory buffer of size WIDTHxHEIGHT and fill it with FILL-PROC!.
Returns a Wayland buffer pointer."
  (if (or (<= width 0) (<= height 0) (not (pointer? *wl-shm*)) (null-pointer? *wl-shm*))
      %null-pointer
	  ;; same flow followed in wallpaper-manager
	  ;; maybe this should be common? an abstract library in
	  ;; deps will be great for such use cases
      (let* ((stride (* width 4))
             (size (* stride height))
             (fd (memfd-create "gliver-window-indicator-shm" *mfd-cloexec*)))
        (if (< fd 0)
            (begin
              (log-error "Failed to create memfd for container window indicator")
              %null-pointer)
            (begin
              (ftruncate fd size)
              (let ((ptr (mmap %null-pointer size *prot-read-write* *map-shared* fd 0)))
                (if (null-pointer? ptr)
                    (begin
                      (close-fd fd)
                      (log-error "Failed to mmap container window indicator buffer")
                      %null-pointer)
                    (begin
                      (let ((bv (pointer->bytevector ptr size)))
                        (catch #t
                          (lambda () (fill-proc! ptr bv stride size width height))
                          (lambda (key . args)
                            (log-error "Error drawing container indicator with Cairo: ~a ~a" key args))))
                      (munmap ptr size)
                      (let* ((pool (wl-shm-create-pool *wl-shm* fd size))
                             (buffer (wl-shm-pool-create-buffer pool 0 width height stride WL_SHM_FORMAT_ARGB8888)))
                        (wl-shm-pool-destroy pool)
                        (close-fd fd)
                        buffer)))))))))

(define (%draw-rect-shape cr x y w h r)
  "Draw rectangle path with radius R (for rounded rectangles)."
  (if (and (number? r) (> r 0.0))
      (let* ((rx (min (exact->inexact r) (/ w 2.0)))
             (ry (min (exact->inexact r) (/ h 2.0)))
             (degrees (/ (acos -1) 180.0)))
        (cairo-new-sub-path cr)
        (cairo-arc cr (+ x w (- rx)) (+ y ry) rx (* -90.0 degrees) 0.0)
        (cairo-arc cr (+ x w (- rx)) (+ y h (- ry)) rx 0.0 (* 90.0 degrees))
        (cairo-arc cr (+ x rx) (+ y h (- ry)) rx (* 90.0 degrees) (* 180.0 degrees))
        (cairo-arc cr (+ x rx) (+ y ry) rx (* 180.0 degrees) (* 270.0 degrees))
        (cairo-close-path cr))
      (cairo-rectangle cr x y w h)))

(define (container-tab-indicator-buffer-create container width height windows active-window)
  "Create and return a wl_buffer containing the rendered container tab indicator."
  (container-tab-indicator-shm-buffer-create
   width height

   ;; the fill function
   (lambda (ptr bv stride size w h)
     (let* ((dst-surface (cairo-image-surface-create-for-data bv 'argb32 w h stride))
            (cr (cairo-create dst-surface)))

       (define (apply-hex-color color-str)
         (and (string? color-str)
              (hex-color? color-str)
              (let ((rgba (parse-hex-color-rgba color-str)))
                (and rgba (apply (lambda (r g b a) (cairo-set-source-rgba cr r g b a)) rgba)))))

       (define (window-title-text win)
         (or (and (string? (window-title win))
                  (not (string-null? (window-title win)))
                  (window-title win))
			 (format #f "Window ~a" (window-id win))))

       (define (tab-colors win)
         (let ((con-focused? (container-focused? container))
               (active?      (eq? win active-window))
               (urgent?      (and (window? win) (window-urgent? win))))
           (cond
            (urgent?
             (values *container-tab-indicator-urgent-bg-color*
                     *container-tab-indicator-urgent-text-color*))
            (active?
			 (if con-focused?
				 (values *container-tab-indicator-active-bg-color*
						 *container-tab-indicator-active-text-color*)
				 (values *container-tab-indicator-unfocused-active-bg-color*
						 *container-tab-indicator-unfocused-active-text-color*)))
            (else
             (values *container-tab-indicator-inactive-bg-color*
                     *container-tab-indicator-inactive-text-color*)))))

       (define (render-tab-background x y w h r bg-color)
         (when (apply-hex-color bg-color)
           (%draw-rect-shape cr x y w h r)
           (cairo-fill cr)))

       (define (render-tab-border x y w h r)
         (let ((bw *container-tab-indicator-border-width*)
               (bc *container-tab-indicator-border-color*))
           (when (and (number? bw) (> bw 0) (apply-hex-color bc))
             (cairo-set-line-width cr bw)
             (%draw-rect-shape cr x y w h r)
             (cairo-stroke cr))))

       (define (render-tab-title win x y w h fg-color)
         (let ((pad-x *container-tab-indicator-padding-x*)
               (title (window-title-text win)))
           (when (and *container-tab-indicator-show-title?*
                      (> w (* 2 pad-x))
                      (apply-hex-color fg-color))
             (catch #t
               (lambda ()
                 (call-with-values
                     (lambda ()
                       (pango-measure-text cr title
                                           #:font *container-tab-indicator-font*
                                           #:font-size *container-tab-indicator-font-size*
                                           #:markup? #f))
                   (lambda (_tw th)
                     (pango-draw-text cr (pango-escape-markup title)
                                      #:x (+ x pad-x)
                                      #:y (max 0.0 (/ (- h th) 2.0))
                                      #:font *container-tab-indicator-font*
                                      #:font-size *container-tab-indicator-font-size*
                                      #:width (- w (* 2 pad-x))
                                      #:ellipsize 'end
                                      #:align 'center
                                      #:markup? #t))))
               (lambda (k . a)
                 (log-debug "Indicator: error rendering title: ~a ~a" k a))))))

       (define (render-single-tab win idx x w)
         (let ((r (min (exact->inexact *container-tab-indicator-radius*)
                       (/ w 2.0)
                       (/ h 2.0))))
           (call-with-values
               (lambda () (tab-colors win))
             (lambda (bg-color fg-color)
               (render-tab-background x 0 w h r bg-color)
               (render-tab-border     x 0 w h r)
               (render-tab-title      win x 0 w h fg-color)))))

       ;; clear surface
       (cairo-set-operator cr 'clear)
       (cairo-paint cr)
       (cairo-set-operator cr 'over)

       (let* ((n          (length windows))
              (gap        (max 0 (inexact->exact (round *container-tab-indicator-gap*))))
              (total-gaps (if (> n 1) (* (1- n) gap) 0))
              (avail-w    (max 0 (- w total-gaps)))
              (base-w     (if (> n 0) (quotient avail-w n) 0))
              (rem-px     (if (> n 0) (remainder avail-w n) 0)))

		 ;; loop each window, give it equally distributed width and render it
         (let loop ((wins windows) (idx 0) (curr-x 0))
           (when (pair? wins)
             (let ((tab-w (+ base-w (if (< idx rem-px) 1 0))))
               (render-single-tab (car wins) idx curr-x tab-w)
               (loop (cdr wins) (1+ idx) (+ curr-x tab-w gap))))))

       ;; flush and cleanup
       (cairo-surface-flush dst-surface)
       (cairo-destroy cr)
       (cairo-surface-destroy dst-surface)))))

(define (container-tab-indicator-update-input-region! surface w h)
  "Set the input region of the indicator surface to cover the entire indicator.
Needed to enable input behavior."
  (when (and surface
             (pointer? surface)
             (not (null-pointer? surface))
             (> w 0) (> h 0))
    (let ((region (wl-compositor-create-region *wl-compositor*)))
      (wl-region-add region 0 0 w h)
      (wl-surface-set-input-region surface region)
      (wl-region-destroy region))))

(define (container-indicator-signature container)
  "Generate a visual state signature for CONTAINER's indicator to check reusability.
Used to skip redrawing when not necessary."
  (let* ((windows (container-windows container))
         (active-win (or (container-window-current container)
                         (and (pair? windows) (car windows))))
         (focused? (container-focused? container)))
    (list (container-width container)
          *container-tab-indicator-height*
          focused?
          *container-tab-indicator-urgent-bg-color*
		  *container-tab-indicator-active-text-color*
		  *container-tab-indicator-inactive-text-color*
		  *container-tab-indicator-unfocused-active-text-color*
		  *container-tab-indicator-urgent-text-color*
		  *container-tab-indicator-border-color*
		  *container-tab-indicator-active-bg-color*
		  *container-tab-indicator-inactive-bg-color*
          *container-tab-indicator-show-title?*
          *container-tab-indicator-show-single-window?*
          *container-tab-indicator-gap*
          *container-tab-indicator-radius*
          *container-tab-indicator-border-width*
          *container-tab-indicator-font*
          *container-tab-indicator-font-size*
          (map (lambda (w)
                 (list (window-id w)
                       (eq? w active-win)
                       (window-urgent? w)
                       (window-title w)
                       (window-app-id w)))
               windows))))

(define (container-tab-indicator-init! container)
  "Initialize Wayland surface, shell surface, and node for container tab indicator."
  (when (container? container)
    (let ((state (container-tab-indicator-state-get-create! container)))
	  ;; initialize only of no wayland surface is created an mapped
      (unless (%cwi-wl-surface state)
        (let* ((surface (wl-compositor-create-surface *wl-compositor*))
               (shell-surf (wm-manager-shell-surface-get (manager-wl-proxy *manager*) surface))
               (node (wm-shell-surface-node-get! shell-surf)))
          (%cwi-wl-surface-set! state surface)
          (%cwi-wl-shell-surface-set! state shell-surf)
          (%cwi-wl-node-set! state node)
          (log-debug "Initialized indicator surface for container ~a" (container-id container)))))))

(define (container-tab-indicator-should-show? container)
  "Return #t if the container tab indicator should be visible for CONTAINER.
Impacted by show-single-window config parameter as well as fullscreen mode."
  (and *%container-tab-indicator-enabled*
       (container? container)
       (let* ((windows (container-windows container))
              (n (length windows))
              (has-fullscreen? (any window-fullscreen? windows)))
         (and (not has-fullscreen?)
              (> n 0)
              (or (> n 1)
                  *container-tab-indicator-show-single-window?*)))))

(define (container-tab-indicator-update-usable-area! container)
  "Update CONTAINER's usable area based on indicator configuration and state,
and resize/reposition its windows if the usable area changed.
Needed because the indicator will cover some area of windows in the container."
  (when (container? container)
    (let* ((cx (container-x container))
           (cy (container-y container))
           (cw (container-width container))
           (ch (container-height container))
           (show? (container-tab-indicator-should-show? container))
           (h (if show?
                  (min ch (max 0 *container-tab-indicator-height*))
                  0))
           (pos *container-tab-indicator-position*)
           (target-ux cx)
           (target-uy (if (and show? (eq? pos 'top))
                          (+ cy h)
                          cy))
           (target-uw cw)
           (target-uh (if show?
                          (max 0 (- ch h))
                          ch))
           (cur-ux (container-usable-x container))
           (cur-uy (container-usable-y container))
           (cur-uw (container-usable-width container))
           (cur-uh (container-usable-height container)))
      (unless (and (= cur-ux target-ux)
                   (= cur-uy target-uy)
                   (= cur-uw target-uw)
                   (= cur-uh target-uh))
        (container-usable-area-set! container target-ux target-uy target-uw target-uh)
        (container-usable-area-update-windows! container)))))

(define (container-tab-indicator-cleanup! container)
  "Clean up Wayland surface, shell surface, node, and buffer for container indicator."
  (when (container? container)
    (container-tab-indicator-update-usable-area! container)
    (let ((state (container-tab-indicator-state-get container)))
      (when state
        (let ((surface (%cwi-wl-surface state))
              (shell-surf (%cwi-wl-shell-surface state))
              (node (%cwi-wl-node state))
              (buffer (%cwi-wl-buffer state)))
          (hashq-remove! *container-tab-indicator-table* container)
          (when (and node (pointer? node) (not (null-pointer? node)))
            (catch #t (lambda () (wm-node-destroy! node)) (lambda _ #f)))
          (when (and shell-surf (pointer? shell-surf) (not (null-pointer? shell-surf)))
            (catch #t (lambda () (wm-shell-surface-destroy! shell-surf)) (lambda _ #f)))
          (when (and surface (pointer? surface) (not (null-pointer? surface)))
            (catch #t (lambda () (wl-surface-destroy surface)) (lambda _ #f)))
          (when (and buffer (pointer? buffer) (not (null-pointer? buffer)))
            (catch #t (lambda () (wl-buffer-destroy buffer)) (lambda _ #f))))))))

(define (container-tab-indicator-hide! container)
  "Hide container window indicator by moving its node offscreen and synchronizing usable area."
  (when (container? container)
    (container-tab-indicator-update-usable-area! container)
    (let ((state (container-tab-indicator-state-get container)))
      (when state
        (let ((node (%cwi-wl-node state)))
          (when (and node (pointer? node) (not (null-pointer? node)))
			;; NOTE: maybe better for hiding is to make transparent if easier
			;; I don't know if updating position approach has any drawbacks
			;; it's currently working fine for all other components
            (with-render-sequence
             (wm-node-position-set! node -10000 -10000))

			;; mark dirty if needed as this may be called outside of river sequence
            (when (and (not *in-manage-sequence*)
                       (not *in-render-sequence*))
              (wm-manager-manage-dirty (manager-wl-proxy *manager*)))))))))

(define* (container-tab-indicator-render! container #:key (force #f))
  "Render the container tab indicator for CONTAINER and commit."
  (when (container? container)
    ;; synchronize container usable area
    (container-tab-indicator-update-usable-area! container)
    (let ((should-show? (container-tab-indicator-should-show? container)))
      (if (not should-show?)
          (container-tab-indicator-hide! container)
          (when (and *%container-tab-indicator-enabled*
                     (workspace-visible? (container-workspace container)))
            (with-render-sequence
             (unless (container-tab-indicator-wl-surface container)
               (container-tab-indicator-init! container))
             (let* ((state (container-tab-indicator-state-get-create! container))
                    (surface (%cwi-wl-surface state))
                    (shell-surf (%cwi-wl-shell-surface state))
                    (node (%cwi-wl-node state))
                    (windows (container-windows container))
                    (w (container-width container))
                    (h (min (container-height container) (max 0 (inexact->exact (round *container-tab-indicator-height*)))))
                    (x (container-x container))
                    (y (if (eq? *container-tab-indicator-position* 'bottom)
                           (+ (container-y container) (- (container-height container) h))
                           (container-y container)))
                    (active-win (or (container-window-current container)
                                    (and (pair? windows) (car windows))))
                    (sig (container-indicator-signature container))
                    (prev-sig (%cwi-signature state))
                    (curr-buf (%cwi-wl-buffer state))
                    (reusable? (and (not force)
                                    curr-buf
                                    (pointer? curr-buf)
                                    (not (null-pointer? curr-buf))
                                    (= (%cwi-width state) w)
                                    (= (%cwi-height state) h)
                                    (equal? prev-sig sig))))
               (when (and surface (pointer? surface) (not (null-pointer? surface))
                          shell-surf (pointer? shell-surf) (not (null-pointer? shell-surf))
                          node (pointer? node) (not (null-pointer? node))
                          (> w 0) (> h 0))
                 (wm-node-position-set! node x y)
                 (wm-node-place-top! node)
                 (container-tab-indicator-update-input-region! surface w h)
                 (if reusable?
                     (begin
                       (wm-shell-surface-sync-next-commit! shell-surf)
                       (wl-surface-commit surface))
                     (let ((new-buffer (container-tab-indicator-buffer-create container w h windows active-win))
                           (old-buffer curr-buf))
                       (when (and (pointer? new-buffer) (not (null-pointer? new-buffer)))
                         (%cwi-wl-buffer-set! state new-buffer)
                         (%cwi-width-set! state w)
                         (%cwi-height-set! state h)
                         (%cwi-signature-set! state sig)
                         (wm-shell-surface-sync-next-commit! shell-surf)
                         (wl-surface-attach surface new-buffer 0 0)
                         (wl-surface-damage surface 0 0 w h)
                         (wl-surface-commit surface)
                         (when (and old-buffer (pointer? old-buffer) (not (null-pointer? old-buffer))
                                    (not (equal? old-buffer new-buffer)))
                           (catch #t (lambda () (wl-buffer-destroy old-buffer)) (lambda _ #f))))))))))))

	;; mark dirty if needed as this may be called outside of river sequence
    (when (not *in-manage-sequence*)
      (wm-manager-manage-dirty (manager-wl-proxy *manager*)))))

(define (container-tab-indicators-update-all!)
  "Update indicators and usable area for all containers across all active outputs."
  (for-each
   (lambda (output)
     (for-each
      (lambda (ws)
        (for-each container-tab-indicator-render! (workspace-containers ws)))
      (output-workspaces output)))
   (manager-outputs *manager*)))

(define (container-tab-indicator-on-container-created container)
  "Handle container creation, initialize and render indicator."
  (container-tab-indicator-render! container))

(define (container-tab-indicator-on-container-destroy container workspace)
  "Handle container destruction, cleanup resources."
  (container-tab-indicator-cleanup! container))

(define (container-tab-indicator-on-container-focused container)
  "Handle container focus, re-render focused indicator and update other containers."
  (container-tab-indicator-render! container)
  (for-each
   (lambda (output)
     (let ((ws (output-workspace-current output)))
       (when ws
         (for-each
          (lambda (c)
            (when (and (container? c)
                       (not (eq? c container)))
              (container-tab-indicator-render! c)))
          (workspace-containers ws)))))
   (manager-outputs *manager*)))

(define (container-tab-indicator-on-container-unfocused container)
  "Handle container unfocus, update indicator colors."
  (container-tab-indicator-render! container))

(define (container-tab-indicator-on-container-resize container)
  "Handle container resize, reposition and re-render indicator."
  (container-tab-indicator-render! container))

(define (container-tab-indicator-on-workspace-switch workspace prev-workspace)
  "Handle workspace switch, hide previous workspace indicators and render current ones."
  (when (and prev-workspace (not (eq? prev-workspace workspace)))
    (for-each container-tab-indicator-hide! (workspace-containers prev-workspace)))
  (when workspace
    (for-each container-tab-indicator-render! (workspace-containers workspace))))

(define (container-tab-indicator-on-window-focused window)
  "Handle window focused event, update container indicator and keep indicator on top."
  (when (window? window)
    (let ((container (window-container window)))
      (when (container? container)
        (container-tab-indicator-render! container)
        ;; ensure indicator node stays on top of the newly focused window
        (let ((node (container-tab-indicator-wl-node container)))
          (when (and node (pointer? node) (not (null-pointer? node)))
            (with-render-sequence
             (wm-node-place-top! node))))))))

(define (container-tab-indicator-on-window-unfocused window)
  "Handle window unfocused event."
  (when (window? window)
    (let ((container (window-container window)))
      (when container
        (container-tab-indicator-render! container)))))

(define (container-tab-indicator-on-window-title-changed window title)
  "Handle window title update, re-render container indicator."
  (when (window? window)
    (let ((container (window-container window)))
      (when container
        (container-tab-indicator-render! container)))))

(define (container-tab-indicator-on-window-app-id-changed window app-id)
  "Handle window app-id update, re-render container indicator."
  (when (window? window)
    (let ((container (window-container window)))
      (when container
        (container-tab-indicator-render! container)))))

(define (container-tab-indicator-on-window-urgent window)
  "Handle window urgency change, re-render container indicator."
  (when (window? window)
    (let ((container (window-container window)))
      (when container
        (container-tab-indicator-render! container)))))

(define (container-tab-indicator-on-window-container-added window container)
  "Handle window added to container, re-render container indicator."
  (when (container? container)
    (container-tab-indicator-render! container)))

(define (container-tab-indicator-on-window-container-removed window container)
  "Handle window removed from container, re-render container indicator."
  (when (container? container)
    (container-tab-indicator-render! container)))

(define (container-tab-indicator-on-window-container-moved window old-c new-c)
  "Handle window moved between containers, re-render both containers."
  (when (container? old-c)
    (container-tab-indicator-render! old-c))
  (when (container? new-c)
    (container-tab-indicator-render! new-c)))

(define (container-tab-indicator-on-window-destroyed window container workspace)
  "Handle window destroyed event, re-render container indicator."
  (when (container? container)
    (container-tab-indicator-render! container)))

(define (container-tab-indicator-on-pointer-position seat x y)
  "Record pointer coordinates for click resolution."
  (set! *container-tab-indicator-last-pointer-x* x)
  (set! *container-tab-indicator-last-pointer-y* y))

(define (container-tab-indicator-on-shell-interaction seat proxy-shell)
  "Handle shell surface mouse interaction like clicking a window tab."
  (when (and *container-tab-indicator-clickable?*
             proxy-shell
             (pointer? proxy-shell)
             (not (null-pointer? proxy-shell)))
    (let ((container (container-tab-indicator-find-by-shell-surface proxy-shell)))
      (when (container? container)
        (let* ((windows (container-windows container))
               (n (length windows)))
          (cond
           ((= n 1)
            (window-focus! (car windows))
			;; mark dirty to re-render
            (wm-manager-manage-dirty (manager-wl-proxy *manager*)))
           ((> n 1)
            (let* ((cx (container-x container))
                   (cw (container-width container))
                   (px (or *container-tab-indicator-last-pointer-x* cx))
                   (rel-x (max 0 (min (- cw 1) (- px cx))))
                   (tab-idx (inexact->exact (floor (/ rel-x (/ cw n)))))
                   (clamped-idx (max 0 (min (1- n) tab-idx)))
                   (win (list-ref windows clamped-idx)))
              (log-debug "Clicked indicator tab ~a for window ~a in container ~a"
                         clamped-idx (window-title win) (container-id container))
              (window-focus! win)
			  ;; mark dirty to re-render
              (wm-manager-manage-dirty (manager-wl-proxy *manager*))))
           (else #f)))))))

(define (container-tab-indicator-find-at-pointer)
  "Find the container currently under the pointer, using surface or coordinates."
  (or (and (valid-pointer? *container-tab-indicator-pointer-surface*)
           (let ((c (container-tab-indicator-find-by-surface *container-tab-indicator-pointer-surface*)))
             (and c
                  (container? c)
                  (not (container-destroyed? c))
                  (workspace-visible? (container-workspace c))
                  c)))
      (and *container-tab-indicator-last-pointer-x*
           *container-tab-indicator-last-pointer-y*
           (let ((px *container-tab-indicator-last-pointer-x*)
                 (py *container-tab-indicator-last-pointer-y*)
                 (found #f))
             (hash-for-each
              (lambda (c state)
                (when (and (not found)
                           (container? c)
                           (workspace-visible? (container-workspace c))
                           (container-tab-indicator-should-show? c))
                  (let* ((cx (container-x c))
                         (cw (container-width c))
                         (ch (container-height c))
                         (h (min ch (max 0 (inexact->exact (round *container-tab-indicator-height*)))))
                         (cy (if (eq? *container-tab-indicator-position* 'bottom)
                                 (+ (container-y c) (- ch h))
                                 (container-y c))))
                    (when (and (>= px cx) (< px (+ cx cw))
                               (>= py cy) (< py (+ cy h)))
                      (set! found c)))))
              *container-tab-indicator-table*)
             found))))

(define (container-tab-indicator-switch-window! container direction)
  "Switch the active window in CONTAINER in DIRECTION ('next or 'prev)."
  (when (container? container)
    (let* ((windows (container-windows container))
           (n (length windows)))
      (when (> n 1)
        (and-let* ((active-win (or (container-window-current container)
								   (and (pair? windows) (car windows))))
				   (idx (list-index (lambda (w) (eq? w active-win)) windows))
				   (target-idx (modulo (if (eq? direction 'next) (1+ idx) (1- idx)) n))
				   (target-win (list-ref windows target-idx)))
          (unless (eq? target-win active-win)
            (log-debug "Indicator: switching container ~a window ~a -> ~a (~a)"
                       (container-id container)
                       (if (window? active-win) (window-id active-win) "none")
                       (window-id target-win)
                       direction)
            (window-focus! target-win)
            ;; we have to call manage dirty to update focused window
            ;; otherwise river won't go to manage or render state immediately
            (wm-manager-manage-dirty (manager-wl-proxy *manager*))))))))

(define (container-tab-indicator-on-scroll container delta)
  "Handle scroll event on CONTAINER's tab indicator.
DELTA < 0 is scroll up, DELTA > 0 is scroll down.
Calls switch-window to go to target one."
  (when (and *container-tab-indicator-scrollable?*
             (container? container))
    (let* ((up? (< delta 0))
           (direction (if (memq *container-tab-indicator-scroll-direction* '(up-next inverted))
                          (if up? 'next 'prev)
                          (if up? 'prev 'next))))
      (container-tab-indicator-switch-window! container direction))))

(define (container-tab-indicator-handle-pointer-enter data proxy serial surface sx sy)
  "Handle wl_pointer enter event."
  (set! *container-tab-indicator-pointer-surface* surface))

(define (container-tab-indicator-handle-pointer-leave data proxy serial surface)
  "Handle wl_pointer leave event."
  (set! *container-tab-indicator-pointer-surface* %null-pointer))

(define (container-tab-indicator-handle-pointer-motion data proxy time sx sy)
  #t)

(define (container-tab-indicator-handle-pointer-button data proxy serial time button state)
  #t)

(define (container-tab-indicator-handle-pointer-axis data proxy time axis value)
  "Handle wl_pointer axis (scroll) event."
  (when (and *container-tab-indicator-scrollable?*
             (zero? axis)
             (not (zero? value)))
    (and-let* ((container (container-tab-indicator-find-at-pointer)))
      (container-tab-indicator-on-scroll container value))))

(define (container-tab-indicator-handle-pointer-frame data proxy)
  #t)

(define (container-tab-indicator-handle-pointer-axis-source data proxy axis-source)
  #t)

(define (container-tab-indicator-handle-pointer-axis-stop data proxy time axis)
  #t)

(define (container-tab-indicator-handle-pointer-axis-discrete data proxy axis discrete)
  #t)

(define (container-tab-indicator-handle-pointer-axis-value120 data proxy axis value120)
  #t)

(define (container-tab-indicator-handle-pointer-axis-relative-direction data proxy axis dir)
  #t)

(define (container-tab-indicator-seat-setup!)
  "Set up Wayland pointer listener on seats for scrolling tabs."
  (when *%container-tab-indicator-enabled*
    (let ((seats (if (pair? (manager-seats *manager*))
                     (manager-seats *manager*)
                     (let ((s (seat-current))) (if s (list s) '())))))
      (for-each
       (lambda (seat)
         (let ((obj-id (and (seat? seat) (seat-wl-seat seat))))
           (when (and (number? obj-id)
                      (not (assv obj-id *container-tab-indicator-seat-proxies*)))
             (let ((wl-seat (gliver-wl-registry-bind *wl-registry* obj-id *wl-seat-interface* 7)))
               (when (valid-pointer? wl-seat)
                 (let ((ptr (wl-seat-get-pointer wl-seat)))
                   (when (valid-pointer? ptr)
                     (let ((listener (make-wl-pointer-listener
                                      container-tab-indicator-handle-pointer-enter
                                      container-tab-indicator-handle-pointer-leave
                                      container-tab-indicator-handle-pointer-motion
                                      container-tab-indicator-handle-pointer-button
                                      container-tab-indicator-handle-pointer-axis
                                      container-tab-indicator-handle-pointer-frame
                                      container-tab-indicator-handle-pointer-axis-source
                                      container-tab-indicator-handle-pointer-axis-stop
                                      container-tab-indicator-handle-pointer-axis-discrete
                                      container-tab-indicator-handle-pointer-axis-value120
                                      container-tab-indicator-handle-pointer-axis-relative-direction)))
                       (wl-proxy-add-listener ptr listener %null-pointer)
                       (set! *container-tab-indicator-seat-proxies*
                             (cons (list obj-id wl-seat ptr)
                                   *container-tab-indicator-seat-proxies*))
                       (log-debug "Container tab indicator: pointer listener registered for seat ~a" obj-id)))))))))
       seats))))

(define (container-tab-indicator-seat-cleanup!)
  "Clean up Wayland pointer and seat proxies for tab scrolling."
  (set! *container-tab-indicator-pointer-surface* %null-pointer)
  (for-each
   (match-lambda
     ((obj-id wl-seat ptr)
      (when (valid-pointer? ptr)
        (catch #t (lambda () (wl-pointer-release ptr)) (lambda _ #f)))
      (when (valid-pointer? wl-seat)
        (catch #t (lambda () (wl-seat-release wl-seat)) (lambda _ #f)))))
   *container-tab-indicator-seat-proxies*)
  (set! *container-tab-indicator-seat-proxies* '()))

(define (container-tab-indicator-on-seat-object-id-changed seat prev-id)
  "Handle seat object-id change event to initialize pointer listener."
  (when *%container-tab-indicator-enabled*
    (container-tab-indicator-seat-setup!)))

(define (container-tab-indicator-on-globals-unbind)
  "Clean up all container indicator surfaces on unbind/shutdown."
  (container-tab-indicator-seat-cleanup!)
  (let ((containers (hash-map->list (lambda (k v) k) *container-tab-indicator-table*)))
    (for-each container-tab-indicator-cleanup! containers)))

(define (container-tab-indicator-on-listeners-attach)
  "Initialize indicators for any existing containers when listeners attach."
  (container-tab-indicator-seat-setup!)
  (container-tab-indicators-update-all!))

(define (container-tab-indicator-on-window-fullscreen-entered window prev-status)
  "Hide indicator when a window enters fullscreen."
  (when (window? window)
    (let ((container (window-container window)))
      (when (container? container)
        (container-tab-indicator-hide! container)))))

(define (container-tab-indicator-on-window-fullscreen-exited window prev-status)
  "Restore indicator when a window exits fullscreen."
  (when (window? window)
    (let ((container (window-container window)))
      (when (container? container)
        (container-tab-indicator-render! container)))))

(define (container-tab-indicator-enable!)
  "Enable container window indicator and hook into events and render indicators."
  (unless *%container-tab-indicator-enabled*
    (gliver-hook-add! *container-created-hook* 'container-tab-indicator-on-container-created)
    (gliver-hook-add! *container-destroy-hook* 'container-tab-indicator-on-container-destroy)
    (gliver-hook-add! *container-focused-hook* 'container-tab-indicator-on-container-focused)
    (gliver-hook-add! *container-unfocused-hook* 'container-tab-indicator-on-container-unfocused)
    (gliver-hook-add! *container-resize-hook* 'container-tab-indicator-on-container-resize)
    (gliver-hook-add! *workspace-switch-hook* 'container-tab-indicator-on-workspace-switch)
    (gliver-hook-add! *gliver-globals-unbind-hook* 'container-tab-indicator-on-globals-unbind)
    (gliver-hook-add! *gliver-listeners-attach-hook* 'container-tab-indicator-on-listeners-attach)
    (gliver-hook-add! %window-container-removed-hook* 'container-tab-indicator-on-window-container-removed)
    (gliver-hook-add! %window-container-added-hook* 'container-tab-indicator-on-window-container-added)
    (gliver-hook-add! *window-container-moved-hook* 'container-tab-indicator-on-window-container-moved)
    (gliver-hook-add! *window-destroyed-hook* 'container-tab-indicator-on-window-destroyed)
    (gliver-hook-add! *window-focused-hook* 'container-tab-indicator-on-window-focused)
    (gliver-hook-add! *window-unfocused-hook* 'container-tab-indicator-on-window-unfocused)
    (gliver-hook-add! *window-title-changed-hook* 'container-tab-indicator-on-window-title-changed)
    (gliver-hook-add! *window-app-id-changed-hook* 'container-tab-indicator-on-window-app-id-changed)
    (gliver-hook-add! *window-urgent-hook* 'container-tab-indicator-on-window-urgent)
    (gliver-hook-add! *seat-shell-interacted-hook* 'container-tab-indicator-on-shell-interaction)
    (gliver-hook-add! *seat-seat-pointer-position-changed-hook* 'container-tab-indicator-on-pointer-position)
    (gliver-hook-add! *seat-object-id-changed-hook* 'container-tab-indicator-on-seat-object-id-changed)
    (gliver-hook-add! *window-fullscreen-entered-hook* 'container-tab-indicator-on-window-fullscreen-entered)
    (gliver-hook-add! *window-fullscreen-exited-hook* 'container-tab-indicator-on-window-fullscreen-exited)
    (set! *%container-tab-indicator-enabled* #t)
    (container-tab-indicator-seat-setup!)
    (container-tab-indicators-update-all!)
    (log-info "container-tab-indicator enabled.")))

(define (container-tab-indicator-disable!)
  "Disable container window indicators and unhook and clean up all resources."
  (when *%container-tab-indicator-enabled*
    (gliver-hook-remove! *container-created-hook* 'container-tab-indicator-on-container-created)
    (gliver-hook-remove! *container-destroy-hook* 'container-tab-indicator-on-container-destroy)
    (gliver-hook-remove! *container-focused-hook* 'container-tab-indicator-on-container-focused)
    (gliver-hook-remove! *container-unfocused-hook* 'container-tab-indicator-on-container-unfocused)
    (gliver-hook-remove! *container-resize-hook* 'container-tab-indicator-on-container-resize)
    (gliver-hook-remove! *workspace-switch-hook* 'container-tab-indicator-on-workspace-switch)
    (gliver-hook-remove! *gliver-globals-unbind-hook* 'container-tab-indicator-on-globals-unbind)
    (gliver-hook-remove! *gliver-listeners-attach-hook* 'container-tab-indicator-on-listeners-attach)
    (gliver-hook-remove! %window-container-removed-hook* 'container-tab-indicator-on-window-container-removed)
    (gliver-hook-remove! %window-container-added-hook* 'container-tab-indicator-on-window-container-added)
    (gliver-hook-remove! *window-container-moved-hook* 'container-tab-indicator-on-window-container-moved)
    (gliver-hook-remove! *window-destroyed-hook* 'container-tab-indicator-on-window-destroyed)
    (gliver-hook-remove! *window-focused-hook* 'container-tab-indicator-on-window-focused)
    (gliver-hook-remove! *window-unfocused-hook* 'container-tab-indicator-on-window-unfocused)
    (gliver-hook-remove! *window-title-changed-hook* 'container-tab-indicator-on-window-title-changed)
    (gliver-hook-remove! *window-app-id-changed-hook* 'container-tab-indicator-on-window-app-id-changed)
    (gliver-hook-remove! *window-urgent-hook* 'container-tab-indicator-on-window-urgent)
    (gliver-hook-remove! *seat-shell-interacted-hook* 'container-tab-indicator-on-shell-interaction)
    (gliver-hook-remove! *seat-seat-pointer-position-changed-hook* 'container-tab-indicator-on-pointer-position)
    (gliver-hook-remove! *seat-object-id-changed-hook* 'container-tab-indicator-on-seat-object-id-changed)
    (gliver-hook-remove! *window-fullscreen-entered-hook* 'container-tab-indicator-on-window-fullscreen-entered)
    (gliver-hook-remove! *window-fullscreen-exited-hook* 'container-tab-indicator-on-window-fullscreen-exited)
    (set! *%container-tab-indicator-enabled* #f)
    (container-tab-indicator-seat-cleanup!)
    (container-tab-indicator-on-globals-unbind)
    (log-info "container-tab-indicator disabled.")))

(define (container-tab-indicator-enabled?)
  "Return #t if container window indicator is enabled, #f otherwise."
  *%container-tab-indicator-enabled*)
