;;; gliver/contrib/ui/container/container-border.scm --- Container border rendering
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; Manages Cairo-based border rendering for containers.
;;;
;;; Usage:
;;;   (use-modules (gliver contrib ui container container-border))
;;;   (container-border-enable!)

(define-module (gliver contrib ui container container-border)
  #:use-module (cairo)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-9)
  #:use-module (system foreign)
  #:use-module (gliver deps libc)
  #:use-module (gliver deps color)
  #:use-module (gliver core)
  #:use-module (gliver wayland client)
  #:use-module (gliver wayland gen wayland)
  #:use-module (gliver river connector)
  #:use-module (gliver river window-manager)
  #:use-module (gliver river wm-shell-surface-manager)
  #:use-module (gliver river wm-node-manager)
  #:declarative? #f
  #:export (
			*%container-border-enabled*
			container-border-state?
			make-container-border-state
			*container-border-table*
			get-container-border-state
			ensure-container-border-state!
			container-wl-surface
			container-wl-shell-surface
			container-wl-node
			container-wl-buffer
			container-border-color
			container-shm-buffer-create
			container-border-buffer-create
			container-border-init!
			container-border-cleanup!
			container-border-hide!
			container-border-render!
			container-borders-update-all!
			container-border-on-container-created
			container-border-on-container-destroy
			container-border-on-container-focused
			container-border-on-container-unfocused
			container-border-on-container-resize
			container-border-on-workspace-switch
			container-border-on-globals-unbind
			container-border-on-listeners-attach
			container-border-on-window-container-removed
			container-border-on-window-container-added
			container-border-enable!
			container-border-disable!
			container-border-enabled?
))

(define-var *%container-border-enabled* #f
  "Internal state that indicates whether container borders are enabled.")

;;; per-container border state
(define-record-type <container-border-state>
  (make-container-border-state wl-surface wl-shell-surface wl-node wl-buffer color width height bg-color)
  container-border-state?
  (wl-surface       %cbs-wl-surface       %cbs-wl-surface-set!)
  (wl-shell-surface %cbs-wl-shell-surface %cbs-wl-shell-surface-set!)
  (wl-node          %cbs-wl-node          %cbs-wl-node-set!)
  (wl-buffer        %cbs-wl-buffer        %cbs-wl-buffer-set!)
  (color            %cbs-color            %cbs-color-set!)
  (width            %cbs-width            %cbs-width-set!)
  (height           %cbs-height           %cbs-height-set!)
  (bg-color         %cbs-bg-color         %cbs-bg-color-set!))

(define *container-border-table* (make-weak-key-hash-table))

(define (get-container-border-state container)
  (and (container? container)
       (hashq-ref *container-border-table* container)))

(define (ensure-container-border-state! container)
  (or (get-container-border-state container)
      (let ((state (make-container-border-state #f #f #f #f #f 0 0 #f)))
        (hashq-set! *container-border-table* container state)
        state)))

(define (container-wl-surface container)
  "Get the Wayland wl_surface foreign pointer associated with CONTAINER."
  (and-let* ((state (get-container-border-state container)))
    (%cbs-wl-surface state)))

(define (%container-wl-surface-set! container val)
  "Set the Wayland wl_surface foreign pointer associated with CONTAINER."
  (let ((state (ensure-container-border-state! container)))
    (%cbs-wl-surface-set! state val)))

(define (container-wl-shell-surface container)
  "Get the Wayland shell surface foreign pointer associated with CONTAINER."
  (and-let* ((state (get-container-border-state container)))
    (%cbs-wl-shell-surface state)))

(define (%container-wl-shell-surface-set! container val)
  "Set the Wayland shell surface foreign pointer associated with CONTAINER."
  (let ((state (ensure-container-border-state! container)))
    (%cbs-wl-shell-surface-set! state val)))

(define (container-wl-node container)
  "Get the Wayland node foreign pointer associated with CONTAINER."
  (and-let* ((state (get-container-border-state container)))
    (%cbs-wl-node state)))

(define (%container-wl-node-set! container val)
  "Set the Wayland node foreign pointer associated with CONTAINER."
  (let ((state (ensure-container-border-state! container)))
    (%cbs-wl-node-set! state val)))

(define (container-wl-buffer container)
  "Get the Wayland buffer foreign pointer associated with CONTAINER."
  (and-let* ((state (get-container-border-state container)))
    (%cbs-wl-buffer state)))

(define (%container-wl-buffer-set! container val)
  "Set the Wayland buffer foreign pointer associated with CONTAINER."
  (let ((state (ensure-container-border-state! container)))
    (%cbs-wl-buffer-set! state val)))

(define (%container-border-color-set! container val)
  "Set the cached border color associated with CONTAINER."
  (let ((state (ensure-container-border-state! container)))
    (%cbs-color-set! state val)))

(define (container-border-color container)
  "Resolve the effective border color for CONTAINER."
  (cond
   ((and (container? container) (container-urgent? container))
    (or *container-border-color-urgent* *window-border-color-urgent* *theme-urgent-color*))
   ((and (container? container) (container-focused? container))
    (or *container-border-color-focused* *window-border-color-focused* *theme-border-color*))
   (else
    (or *container-border-color-unfocused* *window-border-color-unfocused* *theme-mantle*))))

(define (container-shm-buffer-create width height fill-proc!)
  "Allocate a shared memory wl_buffer of size WxH and populate it with FILL-PROC!.
Returns a Wayland buffer foreign pointer."
  (if (or (<= width 0) (<= height 0) (not (pointer? *wl-shm*)) (null-pointer? *wl-shm*))
      %null-pointer
      (let* ((stride (* width 4))
             (size (* stride height))
             (fd (memfd-create "gliver-container-shm" *mfd-cloexec*)))
        (if (< fd 0)
            (begin
              (log-error "Failed to create memfd for container border buffer")
              %null-pointer)
            (begin
              (ftruncate fd size)
              (let ((ptr (mmap %null-pointer size *prot-read-write* *map-shared* fd 0)))
                (if (null-pointer? ptr)
                    (begin
                      (close-fd fd)
                      (log-error "Failed to mmap container border buffer")
                      %null-pointer)
                    (begin
                      (let ((bv (pointer->bytevector ptr size)))
                        (catch #t
                          (lambda () (fill-proc! ptr bv stride size width height))
                          (lambda (key . args)
                            (log-error "Error drawing container border with Cairo: ~a ~a" key args))))
                      (munmap ptr size)
                      (let* ((pool (wl-shm-create-pool *wl-shm* fd size))
                             (buffer (wl-shm-pool-create-buffer pool 0 width height stride WL_SHM_FORMAT_ARGB8888)))
                        (wl-shm-pool-destroy pool)
                        (close-fd fd)
                        buffer)))))))))

(define* (container-border-buffer-create width height border-width border-color
                                        #:optional (border-radius 0) (border-edges 15) (bg-color #f))
  "Create and return a wl_buffer containing the rendered container border of size WxH."
  (container-shm-buffer-create
   width height
   (lambda (ptr bv stride size w h)
     (let* ((dst-surface (cairo-image-surface-create-for-data bv 'argb32 w h stride))
            (cr (cairo-create dst-surface)))
       ;; clear everything (transparent)
       (cairo-set-operator cr 'clear)
       (cairo-paint cr)
       (cairo-set-operator cr 'over)

       ;; if background color is specified and not transparent, fill it
       (when (and bg-color (string? bg-color) (hex-color? bg-color))
         (let ((bg-rgba (parse-hex-color-rgba bg-color)))
           (when (and bg-rgba (> (list-ref bg-rgba 3) 0.0))
             (apply (lambda (r g b a) (cairo-set-source-rgba cr r g b a)) bg-rgba)
             (if (and (number? border-radius) (> border-radius 0) (= (logand border-edges 15) 15))
                 (let* ((rx (min (exact->inexact border-radius) (/ w 2.0)))
                        (ry (min (exact->inexact border-radius) (/ h 2.0)))
                        (degrees (/ (acos -1) 180.0)))
                   (cairo-new-sub-path cr)
                   (cairo-arc cr (+ w (- rx)) ry rx (* -90.0 degrees) 0.0)
                   (cairo-arc cr (+ w (- rx)) (+ h (- ry)) rx 0.0 (* 90.0 degrees))
                   (cairo-arc cr rx (+ h (- ry)) rx (* 90.0 degrees) (* 180.0 degrees))
                   (cairo-arc cr rx ry rx (* 180.0 degrees) (* 270.0 degrees))
                   (cairo-close-path cr)
                   (cairo-fill cr))
                 (cairo-paint cr)))))

       ;; draw border edges if border-width > 0
       (when (and (number? border-width) (> border-width 0) (string? border-color))
         (let ((rgba (parse-hex-color-rgba border-color)))
           (when rgba
             (apply (lambda (r g b a) (cairo-set-source-rgba cr r g b a)) rgba)
             (if (and (number? border-radius) (> border-radius 0) (= (logand border-edges 15) 15))
                 ;; rounded rectangle border
                 (let* ((half-w (/ border-width 2.0))
                        (rx (min (exact->inexact border-radius) (/ (- w border-width) 2.0)))
                        (ry (min (exact->inexact border-radius) (/ (- h border-width) 2.0)))
                        (x0 half-w)
                        (y0 half-w)
                        (w0 (- w border-width))
                        (h0 (- h border-width))
                        (degrees (/ (acos -1) 180.0)))
                   (cairo-set-line-width cr border-width)
                   (cairo-new-sub-path cr)
                   (cairo-arc cr (+ x0 w0 (- rx)) (+ y0 ry) rx (* -90.0 degrees) 0.0)
                   (cairo-arc cr (+ x0 w0 (- rx)) (+ y0 h0 (- ry)) rx 0.0 (* 90.0 degrees))
                   (cairo-arc cr (+ x0 rx) (+ y0 h0 (- ry)) rx (* 90.0 degrees) (* 180.0 degrees))
                   (cairo-arc cr (+ x0 rx) (+ y0 ry) rx (* 180.0 degrees) (* 270.0 degrees))
                   (cairo-close-path cr)
                   (cairo-stroke cr))
                 ;; rectangular border edges
                 (begin
                   ;; top (1)
                   (when (not (zero? (logand border-edges 1)))
                     (cairo-rectangle cr 0 0 w (min h border-width)))
                   ;; bottom (2)
                   (when (not (zero? (logand border-edges 2)))
                     (cairo-rectangle cr 0 (max 0 (- h border-width)) w (min h border-width)))
                   ;; left (4)
                   (when (not (zero? (logand border-edges 4)))
                     (cairo-rectangle cr 0 0 (min w border-width) h))
                   ;; right (8)
                   (when (not (zero? (logand border-edges 8)))
                     (cairo-rectangle cr (max 0 (- w border-width)) 0 (min w border-width) h))
                   (cairo-fill cr))))))
       (cairo-surface-flush dst-surface)
       (cairo-destroy cr)
       (cairo-surface-destroy dst-surface)))))

(define (container-border-init! container)
  "Initialize Wayland surface, shell surface, and node for container border."
  (when (and (container? container)
             (not (container-destroyed? container))
             *wl-display*
             (pointer? *wl-compositor*)
             (not (null-pointer? *wl-compositor*))
             (pointer? (manager-wl-proxy *manager*))
             (not (null-pointer? (manager-wl-proxy *manager*))))
    (unless (container-wl-surface container)
      (let* ((surface (wl-compositor-create-surface *wl-compositor*)))
        ;; empty input region so all mouse clicks pass through
        (let ((region (wl-compositor-create-region *wl-compositor*)))
          (wl-surface-set-input-region surface region)
          (wl-region-destroy region))
        (let* ((shell-surf (wm-manager-shell-surface-get (manager-wl-proxy *manager*) surface))
               (node (wm-shell-surface-node-get! shell-surf)))
          (%container-wl-surface-set! container surface)
          (%container-wl-shell-surface-set! container shell-surf)
          (%container-wl-node-set! container node)
          (log-debug "Initialized border surface for container ~a" (container-id container)))))))

(define (container-border-cleanup! container)
  "Clean up Wayland surface, shell surface, node, and buffer for container."
  (when (container? container)
    (let ((surface (container-wl-surface container))
          (shell-surf (container-wl-shell-surface container))
          (node (container-wl-node container))
          (buffer (container-wl-buffer container)))
      (hashq-remove! *container-border-table* container)
      (when (and node (pointer? node) (not (null-pointer? node)))
        (catch #t (lambda () (wm-node-destroy! node)) (lambda _ #f)))
      (when (and shell-surf (pointer? shell-surf) (not (null-pointer? shell-surf)))
        (catch #t (lambda () (wm-shell-surface-destroy! shell-surf)) (lambda _ #f)))
      (when (and surface (pointer? surface) (not (null-pointer? surface)))
        (catch #t (lambda () (wl-surface-destroy surface)) (lambda _ #f)))
      (when (and buffer (pointer? buffer) (not (null-pointer? buffer)))
        (catch #t (lambda () (wl-buffer-destroy buffer)) (lambda _ #f))))))

(define (container-border-hide! container)
  "Hide container border by moving its node offscreen."
  (when (and (container? container) (not (container-destroyed? container)))
    (let ((node (container-wl-node container)))
      (when (and node (pointer? node) (not (null-pointer? node)))
        (with-render-sequence
         (wm-node-position-set! node -10000 -10000))))))

(define* (container-border-render! container #:key (force #f))
  "Render the container border for CONTAINER and commit the surface within a render sequence."
  (when (and (container? container)
			 (workspace-visible? (container-workspace container))
             (not (container-destroyed? container))
             *wl-display*
             (pointer? *wl-compositor*)
             (not (null-pointer? *wl-compositor*))
             (pointer? *wl-shm*)
             (not (null-pointer? *wl-shm*))
             (pointer? (manager-wl-proxy *manager*))
             (not (null-pointer? (manager-wl-proxy *manager*))))
    (with-render-sequence
     (unless (container-wl-surface container)
       (container-border-init! container))
     (let* ((state (ensure-container-border-state! container))
            (surface (container-wl-surface container))
            (shell-surf (container-wl-shell-surface container))
            (node (container-wl-node container))
            (color (container-border-color container))
            (border-w *container-border-width*)
            (border-r *container-border-radius*)
            (border-edges *container-border-edges*)
			;; border is outer, not inner
			;; so width and height will be increased by 2 * border width
			;; while x and y will be padded by border width
            (w (+ (container-width container) (* 2 border-w)))
            (h (+ (container-height container) (* 2 border-w)))
            (x (- (container-x container) border-w))
            (y (- (container-y container) border-w))
			;; background color will be false/transparent if container has windows
            (bg-color (and (null? (container-windows container)) *container-border-bg-color*))
            (curr-buf (container-wl-buffer container))
            (prev-w (%cbs-width state))
            (prev-h (%cbs-height state))
            (prev-color (%cbs-color state))
            (prev-bg (%cbs-bg-color state))
            (reusable? (and (not force)
                            curr-buf
                            (pointer? curr-buf)
                            (not (null-pointer? curr-buf))
                            (= prev-w w)
                            (= prev-h h)
                            (equal? prev-color color)
                            (equal? prev-bg bg-color))))
       (when (and surface (pointer? surface) (not (null-pointer? surface))
                  node (pointer? node) (not (null-pointer? node))
                  shell-surf (pointer? shell-surf) (not (null-pointer? shell-surf))
                  (> w 0) (> h 0))
		 (with-render-sequence
		  (wm-node-position-set! node x y)
          (wm-node-place-top! node))
         (if reusable?
             (begin
               (wm-shell-surface-sync-next-commit! shell-surf)
               (wl-surface-commit surface))
             (let ((new-buffer (container-border-buffer-create w h border-w color border-r border-edges bg-color))
                   (old-buffer curr-buf))
               (when (and (pointer? new-buffer) (not (null-pointer? new-buffer)))
                 (%container-wl-buffer-set! container new-buffer)
                 (%container-border-color-set! container color)
                 (%cbs-width-set! state w)
                 (%cbs-height-set! state h)
                 (%cbs-bg-color-set! state bg-color)
                 (wm-shell-surface-sync-next-commit! shell-surf)
                 (wl-surface-attach surface new-buffer 0 0)
                 (wl-surface-damage surface 0 0 w h)
                 (wl-surface-commit surface)
                 (when (and old-buffer (pointer? old-buffer) (not (null-pointer? old-buffer))
                            (not (equal? old-buffer new-buffer)))
                   (catch #t (lambda () (wl-buffer-destroy old-buffer)) (lambda _ #f)))))))))))

(define (container-borders-update-all!)
  "Update borders for all containers in the display."
  (for-each
   (lambda (output)
     (let ((ws (output-workspace-current output)))
       (when ws
         (for-each container-border-render! (workspace-containers ws)))))
   (manager-outputs *manager*)))

(define (container-border-on-container-created container)
  "Handle container creation, initialize and render its border."
  (container-border-init! container)
  (container-border-render! container))

(define (container-border-on-container-destroy container workspace)
  "Handle container destruction, clean up border resources."
  (container-border-cleanup! container))

(define (container-border-on-container-focused container)
  "Handle container focus, update border color."
  (container-border-render! container))

(define (container-border-on-container-unfocused container)
  "Handle container unfocus, update border color."
  (container-border-render! container))

(define (container-border-on-container-resize container)
  "Handle container resize, re-render border."
  (container-border-render! container))

(define (container-border-on-workspace-switch workspace prev-workspace)
  "Handle workspace switch, hide previous containers and render current containers."
  (when prev-workspace
    (for-each container-border-hide! (workspace-containers prev-workspace)))
  (when workspace
    (for-each container-border-render! (workspace-containers workspace))))

(define (container-border-on-globals-unbind)
  "Clean up all container border surfaces on unbind/shutdown."
  (for-each
   (lambda (output)
     (for-each
      (lambda (ws)
        (for-each container-border-cleanup! (workspace-containers ws)))
      (output-workspaces output)))
   (manager-outputs *manager*)))

(define (container-border-on-listeners-attach)
  "Initialize borders for any containers existing when listeners attach."
  (when (and *wl-display*
             (pointer? *wl-compositor*)
             (not (null-pointer? *wl-compositor*))
             (pointer? *wl-shm*)
             (not (null-pointer? *wl-shm*))
             (pointer? (manager-wl-proxy *manager*))
             (not (null-pointer? (manager-wl-proxy *manager*))))
    (for-each
     (lambda (output)
       (let ((ws (output-workspace-current output)))
         (when ws
           (for-each
            (lambda (c)
              (unless (container-wl-surface c)
                (container-border-init! c))
              (container-border-render! c))
            (workspace-containers ws)))))
     (manager-outputs *manager*))))

(define (container-border-on-window-container-removed window container)
  "Handle window removed from container, re-render border (e.g. background)."
  (when (and (container? container) (not (container-destroyed? container)))
    (container-border-render! container)))

(define (container-border-on-window-container-added window container)
  "Handle window added to container, re-render container border (e.g. background)."
  (when (and (container? container) (not (container-destroyed? container)))
    (container-border-render! container)))

(define (container-border-enable!)
  "Enable container borders, hook into container events and render borders."
  (unless *%container-border-enabled*
    (gliver-hook-add! *container-created-hook* 'container-border-on-container-created)
    (gliver-hook-add! *container-destroy-hook* 'container-border-on-container-destroy)
    (gliver-hook-add! *container-focused-hook* 'container-border-on-container-focused)
    (gliver-hook-add! *container-unfocused-hook* 'container-border-on-container-unfocused)
    (gliver-hook-add! *container-resize-hook* 'container-border-on-container-resize)
    (gliver-hook-add! *workspace-switch-hook* 'container-border-on-workspace-switch)
    (gliver-hook-add! *gliver-globals-unbind-hook* 'container-border-on-globals-unbind)
    (gliver-hook-add! *gliver-listeners-attach-hook* 'container-border-on-listeners-attach)
    (gliver-hook-add! %window-container-removed-hook* 'container-border-on-window-container-removed)
    (gliver-hook-add! %window-container-added-hook* 'container-border-on-window-container-added)
    (set! *%container-border-enabled* #t)
    (container-borders-update-all!)
    (log-info "container-border enabled.")))

(define (container-border-disable!)
  "Disable container borders, unhook and cleanup border surfaces."
  (when *%container-border-enabled*
    (gliver-hook-remove! *container-created-hook* 'container-border-on-container-created)
    (gliver-hook-remove! *container-destroy-hook* 'container-border-on-container-destroy)
    (gliver-hook-remove! *container-focused-hook* 'container-border-on-container-focused)
    (gliver-hook-remove! *container-unfocused-hook* 'container-border-on-container-unfocused)
    (gliver-hook-remove! *container-resize-hook* 'container-border-on-container-resize)
    (gliver-hook-remove! *workspace-switch-hook* 'container-border-on-workspace-switch)
    (gliver-hook-remove! *gliver-globals-unbind-hook* 'container-border-on-globals-unbind)
    (gliver-hook-remove! *gliver-listeners-attach-hook* 'container-border-on-listeners-attach)
    (gliver-hook-remove! %window-container-removed-hook* 'container-border-on-window-container-removed)
    (gliver-hook-remove! %window-container-added-hook* 'container-border-on-window-container-added)
    (container-border-on-globals-unbind)
    (set! *%container-border-enabled* #f)
    (log-info "container-border disabled.")))

(define (container-border-enabled?)
  "Return #t if container-border is enabled, #f otherwise."
  *%container-border-enabled*)
