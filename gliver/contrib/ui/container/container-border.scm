;;; gliver/contrib/ui/container/container-border.scm --- Container border rendering
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module manages gleui border rendering for gliver
;;; NOTE: river does support per-window border (which is limited in features).
;;;       this border was created to be border-level rather than window-level,
;;;       and to highlight empty containers that don't have windows allocated
;;;       which is not possible to do with river.
;;; It handles:
;;;  - Managing Wayland surfaces, shell surfaces, and double-buffered pools
;;;  - Computing border geometry, focus/urgent colors, and empty-container fills
;;;  - Updating input regions and reacting to container, workspace and other hooks
;;;
;;; Usage:
;;;   (use-modules (gliver contrib ui container container-border))
;;;   (container-border-enable!)

(define-module (gliver contrib ui container container-border)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-9)
  #:use-module (system foreign)
  #:use-module (gliver core)
  #:use-module (gliver wayland gen wayland)
  #:use-module (gliver river connector)
  #:use-module (gliver river window-manager)
  #:use-module (gliver river wm-shell-surface-manager)
  #:use-module (gliver river wm-node-manager)
  #:use-module (gliver contrib ui gleui base)
  #:use-module (gliver contrib ui gleui widgets)
  #:declarative? #f
  #:export (
            *%container-border-enabled*
            *container-border-table*
            <container-border-state>
            make-container-border-state
            container-border-state?
            get-container-border-state
            ensure-container-border-state!
            container-wl-surface
            container-wl-shell-surface
            container-find-by-shell-surface
            container-wl-buffer
            container-border-color
            container-border-build-widget
            container-border-update-input-region!
            container-border-init!
            container-border-cleanup!
            container-border-hide!
            container-border-render!
            container-borders-update-all!
            container-border-on-globals-unbind
            container-border-on-listeners-attach
            container-border-on-container-created
            container-border-on-container-destroy
            container-border-on-container-focused
            container-border-on-container-unfocused
            container-border-on-container-resize
            container-border-on-workspace-switch
            container-border-on-workspace-destroy
            container-border-on-output-removed
            container-border-on-window-container-removed
            container-border-on-window-container-added
            container-border-on-shell-interaction
            container-border-on-window-fullscreen-entered
            container-border-on-window-fullscreen-exited
            container-border-on-surface-scale-changed
            *container-border-hooks*
            container-border-enable!
            container-border-disable!
))

(define-var *%container-border-enabled* #f
            "Internal state that indicates whether container borders are enabled.")
(define-var *container-border-table* (make-hash-table)
            "Hash table mapping <container> records to <container-border-state>.")

;; per-container border state record
;;  - wl-surface: pointer to wl_surface or #f, surface for the container border
;;  - wl-shell-surface: pointer to river_shell_surface_v1 or #f, shell surface associated with wl-surface
;;  - wl-node: pointer to river_node_v1 or #f, scene node to control behind/above
;;  - wl-buffer: pointer to wl_buffer or #f, active Wayland buffer currently committed
;;  - shm-slots: pair (slot . #f) of <gleui-shm-slot> record or #f, transient SHM buffer slot
;;  - shm-active-idx: integer (0 or 1) currently displayed SHM slot index
;;  - color: string or #f, cached border hex color for reuse checks
;;  - width: integer cached logical border surface width in pixels
;;  - height: integer cached logical border surface height in pixels
;;  - bg-color: string or #f, cached empty-container background fill hex color
;;  - scale: float cached fractional surface scale factor
(define-record-type <container-border-state>
  (make-container-border-state wl-surface wl-shell-surface wl-node wl-buffer
                               shm-slots shm-active-idx color width height bg-color scale)
  container-border-state?
  (wl-surface       %cbs-wl-surface       %cbs-wl-surface-set!)
  (wl-shell-surface %cbs-wl-shell-surface %cbs-wl-shell-surface-set!)
  (wl-node          %cbs-wl-node          %cbs-wl-node-set!)
  (wl-buffer        %cbs-wl-buffer        %cbs-wl-buffer-set!)
  (shm-slots        %cbs-shm-slots        %cbs-shm-slots-set!)
  (shm-active-idx   %cbs-shm-active-idx   %cbs-shm-active-idx-set!)
  (color            %cbs-color            %cbs-color-set!)
  (width            %cbs-width            %cbs-width-set!)
  (height           %cbs-height           %cbs-height-set!)
  (bg-color         %cbs-bg-color         %cbs-bg-color-set!)
  (scale            %cbs-scale            %cbs-scale-set!))

;; the below functions are safe getters to container border state properties
(define (get-container-border-state container)
  "Look up the `<container-border-state>' record attached to CONTAINER.
Parameters: (CONTAINER)
  - CONTAINER: <container> record to look up.
Returns `<container-border-state>' record, or #f if none exists."
  (and (container? container)
       (hashq-ref *container-border-table* container)))

(define (ensure-container-border-state! container)
  "Return the `<container-border-state>' record for CONTAINER, allocate new if unavailable.
Parameters: (CONTAINER)
  - CONTAINER: <container> record owning the border state.
Returns `<container-border-state>' record, or #f if CONTAINER is not a container."
  (or (get-container-border-state container)
      (let ((state (make-container-border-state #f #f #f #f #f 0 #f 0 0 #f 1.0)))
        (hashq-set! *container-border-table* container state)
        state)))

(define (container-wl-surface container)
  "Return the Wayland wl_surface foreign pointer for CONTAINER's border.
Parameters: (CONTAINER)
  - CONTAINER: <container> record.
Returns wl_surface foreign pointer, or #f if uninitialized."
  (and-let* ((state (get-container-border-state container)))
    (%cbs-wl-surface state)))

(define (container-wl-shell-surface container)
  "Return the shell surface pointer for CONTAINER's border.
Parameters: (CONTAINER)
  - CONTAINER: <container> record.
Returns river_shell_surface_v1 foreign pointer, or #f if uninitialized."
  (and-let* ((state (get-container-border-state container)))
    (%cbs-wl-shell-surface state)))

(define (container-find-by-shell-surface shell-surf)
  "Find the container whose border owns shell surface SHELL-SURF.
Parameters: (SHELL-SURF)
  - SHELL-SURF: Pointer to river_shell_surface_v1 proxy.
Returns matching `<container>' record, or #f if not found."
  (and (valid-pointer? shell-surf)
       (let ((target-address (pointer-address shell-surf))
             (matched        #f))
         (hash-for-each
          (lambda (container state)
            (let ((candidate-surface (%cbs-wl-shell-surface state)))
              (when (and (valid-pointer? candidate-surface)
                         (= (pointer-address candidate-surface) target-address))
                (set! matched container))))
          *container-border-table*)
         matched)))

(define (container-wl-buffer container)
  "Return the active Wayland wl_buffer pointer committed for CONTAINER's border.
Parameters: (CONTAINER)
  - CONTAINER: <container> record.
Returns wl_buffer foreign pointer, or #f if none is committed."
  (and-let* ((state (get-container-border-state container)))
    (%cbs-wl-buffer state)))

(define (container-border-color container)
  "Resolve the effective border hex color string for CONTAINER based on urgency and focus.
Parameters: (CONTAINER)
  - CONTAINER: <container> record.
Returns resolved hex color string."
  (cond
   ((and (container? container)
         (container-urgent? container))
    *container-border-color-urgent*)
   ((and (container? container)
         (container-focused? container))
    *container-border-color-focused*)
   (else
    *container-border-color-unfocused*)))

(define* (container-border-build-widget width height border-width border-color
                                        #:optional (border-radius 0) (border-edges 15) (bg-color #f))
  "Build the declarative `<gleui-widget>' box of the container border.
Parameters: (WIDTH HEIGHT BORDER-WIDTH BORDER-COLOR [BORDER-RADIUS 0] [BORDER-EDGES 15] [BG-COLOR #f])
  - WIDTH: Integer outer border surface width in pixels.
  - HEIGHT: Integer outer border surface height in pixels.
  - BORDER-WIDTH: Integer border stroke thickness in pixels.
  - BORDER-COLOR: String border stroke hex color.
  - BORDER-RADIUS: Integer optional corner radius in pixels.
  - BORDER-EDGES: Integer optional four-bit edge mask (1=top, 2=bottom, 4=left, 8=right).
  - BG-COLOR: String or #f optional empty-container fill hex color.
Returns `<gleui-widget>' box node."
  (gleui-w-box
   (gleui-w-empty)
   #:width width
   #:height height
   #:bg-color bg-color
   #:border-color border-color
   #:border-width border-width
   #:border-edges border-edges
   #:radius border-radius))

;; NOTE: this can't be replaced by clickable widget at this point
;;       it's controlling the pass-through behavior to decide if
;;       clicking the container should be routed to windows inside
(define (container-border-update-input-region! container surface width height border-width border-edges)
  "Update the Wayland input region of CONTAINER's border.
Parameters: (CONTAINER SURFACE WIDTH HEIGHT BORDER-WIDTH BORDER-EDGES)
  - CONTAINER: <container> record.
  - SURFACE: Pointer to Wayland wl_surface.
  - WIDTH: Integer surface width in pixels.
  - HEIGHT: Integer surface height in pixels.
  - BORDER-WIDTH: Integer border stroke thickness in pixels.
  - BORDER-EDGES: Integer four-bit mask selecting active border edges."
  (when (and (valid-pointer? surface) (> width 0) (> height 0))
    (let ((region (wl-compositor-create-region *wl-compositor*)))
      (if (null? (container-windows container))
          ;; no windows: whole container is clickable
          (wl-region-add region 0 0 width height)
          (when (and (number? border-width) (> border-width 0))
            ;; windows available: only border edges are clickable
            (when (not (zero? (logand border-edges 1)))
              (wl-region-add region 0 0 width (min height border-width)))
            (when (not (zero? (logand border-edges 2)))
              (wl-region-add region 0 (max 0 (- height border-width)) width (min height border-width)))
            (when (not (zero? (logand border-edges 4)))
              (wl-region-add region 0 0 (min width border-width) height))
            (when (not (zero? (logand border-edges 8)))
              (wl-region-add region (max 0 (- width border-width)) 0 (min width border-width) height))))
      (wl-surface-set-input-region surface region)
      (wl-region-destroy region))))

(define (%container-border-live? container)
  "Check whether CONTAINER is still attached to an active workspace and output.
Parameters: (CONTAINER)
  - CONTAINER: <container> record candidate.
Returns #t if CONTAINER is live, #f if destroyed or detached."
  (and-let* (((container? container))
             (workspace (container-workspace container))
             ((workspace? workspace))
             ((memq container (workspace-containers workspace))))
    (let ((output (workspace-output workspace)))
      (or (not (output? output))
          (and (memq workspace (output-workspaces output)) #t)))))

(define (container-border-init! container)
  "Allocate the Wayland surface, shell surface, and scene node for CONTAINER's border.
Parameters: (CONTAINER)
  - CONTAINER: <container> record to initialize."
  (when (and *%container-border-enabled*
             (%container-border-live? container)
             (valid-pointer? *wl-compositor*)
             (valid-pointer? (manager-wl-proxy *manager*)))
    ;; skip if shell surface is already configured
    (unless (container-wl-surface container)
      ;; ugly, but we can't do and-let here because we have to cleanup
      ;; all created objects in case of a failure
      (let* ((manager    (manager-wl-proxy *manager*))
             (surface    (wl-compositor-create-surface *wl-compositor*))
             (shell-surf (and (valid-pointer? surface)
                              (wm-manager-shell-surface-get manager surface)))
             (node       (and (valid-pointer? shell-surf)
                              (wm-shell-surface-node-get! shell-surf)))
             (output     (or (and=> (container-workspace container) workspace-output)
                             (output-current))))
        (if (and (valid-pointer? surface)
                 (valid-pointer? shell-surf)
                 (valid-pointer? node))
            ;; we should only create the border if all surfaces are allocated
            (let ((state (ensure-container-border-state! container)))
              (surface-scale-create surface output)
              (%cbs-wl-surface-set! state surface)
              (%cbs-wl-shell-surface-set! state shell-surf)
              (%cbs-wl-node-set! state node)
              (%container-wl-node-proxy-set! container node)
              (log-debug "container-border: initialized border surface for container ~a" (container-id container)))
            ;; if something goes wrong, cleanup all objects created
            (begin
              (when (valid-pointer? node)
                (wm-node-destroy! node))
              (when (valid-pointer? shell-surf)
                (wm-shell-surface-destroy! shell-surf))
              (when (valid-pointer? surface)
                (wl-surface-destroy surface))))))))

(define (container-border-cleanup! container)
  "Destroy the Wayland surface, shell surface, node, and SHM slots for CONTAINER's border.
Parameters: (CONTAINER)
  - CONTAINER: <container> record whose border resources are freed."
  (log-info "container-border: cleaning up border container ~a" container)
  (when (container? container)
    (let ((state      (get-container-border-state container))
          (node       (container-wl-node-proxy container)))
      (hashq-remove! *container-border-table* container)
      (%container-wl-node-proxy-set! container #f)
      (when state
        (let ((surface    (%cbs-wl-surface state))
              (shell-surf (%cbs-wl-shell-surface state))
              (snode      (or (%cbs-wl-node state) node)))
          (%cbs-wl-node-set! state #f)
          (%cbs-wl-shell-surface-set! state #f)
          (%cbs-wl-surface-set! state #f)
          (%cbs-wl-buffer-set! state #f)
          (gleui-shm-slots-destroy! (%cbs-shm-slots state))
          (%cbs-shm-slots-set! state #f)
          (when (valid-pointer? snode)
            (wm-node-destroy! snode))
          (when (valid-pointer? shell-surf)
            (wm-shell-surface-destroy! shell-surf))
          (when (valid-pointer? surface)
            (surface-scale-destroy surface)
            (wl-surface-destroy surface)))))))

(define (container-border-hide! container)
  "Hide CONTAINER's border node and release any active SHM buffers.
Parameters: (CONTAINER)
  - CONTAINER: <container> record to hide."
  (when (and *%container-border-enabled*
             (%container-border-live? container))
    (with-render-sequence
     (when (and *%container-border-enabled*
                (%container-border-live? container))
       (and-let* ((state (get-container-border-state container)))
         (let ((node       (or (container-wl-node-proxy container) (%cbs-wl-node state)))
               (surface    (%cbs-wl-surface state))
               (shell-surf (%cbs-wl-shell-surface state)))
           (when (valid-pointer? node)
             ;; TODO: there must be a better approach, currently
             ;;       all hidden borders go to far away coordinates
             (wm-node-position-set! node -10000 -10000))
           (when (and (valid-pointer? surface)
                      (valid-pointer? shell-surf))
             (wm-shell-surface-sync-next-commit! shell-surf)
             (wl-surface-attach surface %null-pointer 0 0)
             (wl-surface-commit surface))
           (%cbs-wl-buffer-set! state #f)
           (gleui-shm-slots-destroy! (%cbs-shm-slots state))
           (%cbs-shm-slots-set! state #f)))))))

(define (%container-border-visible-rect container)
  "Compute the clipped (x y w h edges clipped?) of CONTAINER's border
within output usable bounds, or #f if completely offscreen.
NOTE: the purpose is to clip border so it doesn't expand into another output
      since they are logically close to each other and that could happen.
Parameters: (CONTAINER)
  - CONTAINER: <container> record whose border bounds are checked.
Returns list (x y w h edges clipped?) or #f when completely outside the output."
  (let* ((border-width *container-border-width*)
         (border-edges *container-border-edges*)
         ;; outer border extends container width by 2x border width
         ;; same thing happens for height (we add border width/height both sides)
         (raw-width    (+ (container-width container) (* 2 border-width)))
         (raw-height   (+ (container-height container) (* 2 border-width)))
         (raw-left     (- (container-x container) border-width))
         (raw-top      (- (container-y container) border-width))
         (raw-right    (+ raw-left raw-width))
         (raw-bottom   (+ raw-top raw-height))
         (output       (and=> (container-workspace container) workspace-output)))
    (if (not (output? output))
        ;; fallback, this typically shouldn't happen, for a container be on an output
        (and (> raw-width 0) (> raw-height 0)
             (list raw-left raw-top raw-width raw-height border-edges #f))
        ;; getting usable area edges to avoid write on statusbar
        ;; NOTE: if a statusbar isn't not filling its area, maybe this
        ;;       won't work well, as border will be clipped when it shouldn't?
        (let* ((usable-left   (output-usable-x output))
               (usable-top    (output-usable-y output))
               (usable-right  (+ usable-left (output-usable-width output)))
               (usable-bottom (+ usable-top (output-usable-height output)))
               ;; intersect the unclipped border rect with the output's usable area
               (vis-left      (max raw-left usable-left))
               (vis-top       (max raw-top usable-top))
               (vis-right     (min raw-right usable-right))
               (vis-bottom    (min raw-bottom usable-bottom))
               (vis-width     (- vis-right vis-left))
               (vis-height    (- vis-bottom vis-top)))
          (and (> vis-width 0)
               (> vis-height 0)
               (let* ((clipped?  (or (> vis-left raw-left)
                                     (> vis-top raw-top)
                                     (< vis-right raw-right)
                                     (< vis-bottom raw-bottom)))
                      ;; drop border strokes on edges clipped by usable bounds so
                      ;; offscreen edges aren't redrawn inside the visible viewport
                      ;; bitmask: 1=top, 2=bottom, 4=left, 8=right
                      (vis-edges (if clipped?
                                     (logand border-edges
                                             (+ (if (>= raw-top usable-top) 1 0)
                                                (if (<= raw-bottom usable-bottom) 2 0)
                                                (if (>= raw-left usable-left) 4 0)
                                                (if (<= raw-right usable-right) 8 0)))
                                     border-edges)))
                 (list vis-left vis-top vis-width vis-height vis-edges clipped?)))))))

(define* (container-border-render! container #:key (force #f))
  "Render and commit the border surface for CONTAINER.
Parameters: (CONTAINER [#:FORCE #f])
  - CONTAINER: <container> record to render or hide.
  - FORCE: Boolean flag to force redraw SHM buffers even if cached."
  (when (and *%container-border-enabled*
             (%container-border-live? container)
             (workspace-visible? (container-workspace container)))
    (let ((rect (%container-border-visible-rect container)))
      ;; we will render to both hide and show the border
      ;; if we have a fullscreen window or container isn't in screen
      ;; boundary, then we should hide the border
      (if (or (not rect)
              (any window-fullscreen? (container-windows container)))
          (container-border-hide! container)
          ;; otherwise, show it
          (with-render-sequence
           #:dirty? #t
           ;; with-render-sequence may defer until river's render callback, so
           ;; re-validate visibility and recompute clipped geometry
           (and-let* ((*%container-border-enabled*)
                      ((%container-border-live? container))
                      (workspace (container-workspace container))
                      ((workspace-visible? workspace))
                      (cur-rect (%container-border-visible-rect container)))

             ;; initialize border if it's not already
             (unless (container-wl-surface container)
               (container-border-init! container))
             (and-let* ((state        (get-container-border-state container))
                        (surface      (%cbs-wl-surface state))
                        (shell-surf   (%cbs-wl-shell-surface state))
                        (node         (or (container-wl-node-proxy container)
                                          (%cbs-wl-node state)))
                        ((valid-pointer? surface))
                        ((valid-pointer? node))
                        ((valid-pointer? shell-surf)))

               (let* ((color          (container-border-color container))
                      (border-width   *container-border-width*)
                      (border-radius  *container-border-radius*)
                      (x              (list-ref cur-rect 0))
                      (y              (list-ref cur-rect 1))
                      (width          (list-ref cur-rect 2))
                      (height         (list-ref cur-rect 3))
                      (border-edges   (list-ref cur-rect 4))
                      (clipped?       (list-ref cur-rect 5))
                      (scale          (surface-scale surface 1.0))
                      ;; optional bg-color if container is empty, otherwise #f (transparent)
                      (bg-color       (and (null? (container-windows container))
                                           *container-border-bg-color*))
                      (current-buffer (%cbs-wl-buffer state))
                      ;; skip redraw when dimensions, scale, and colors match
                      ;; clipped surfaces bypass reuse because vis-edges may differ
                      (reusable?      (and (not force)
                                           (not clipped?)
                                           (valid-pointer? current-buffer)
                                           (= (%cbs-width state) width)
                                           (= (%cbs-height state) height)
                                           (= (%cbs-scale state) scale)
                                           (equal? (%cbs-color state) color)
                                           (equal? (%cbs-bg-color state) bg-color))))

                 (when (and (> width 0) (> height 0))
                   ;; keep border node above container windows, windows still
                   ;; have borders configured via river to have proper dimensions
                   (wm-node-position-set! node x y)
                   (wm-node-place-top! node)
                   ;; to configure pass-through if container isn't empty
                   ;; we don't want it to capture pointer events while windows
                   ;; are expecting it, remember that this surface is on top
                   (container-border-update-input-region! container surface
                                                          width height
                                                          border-width border-edges)
                   (if reusable?
                       ;; skip building new widget if reusable, just commit
                       (begin
                         (wm-shell-surface-sync-next-commit! shell-surf)
                         (gleui-layer-surface-buffer-commit! surface current-buffer width height))
                       ;; otherwise, build a new border widget
                       (let ((widget (container-border-build-widget
                                      width height border-width color border-radius border-edges bg-color)))
                         (call-with-values
                             (lambda ()
                               ;; transient SHM unmaps CPU memory right after commit since borders rarely change
                               (gleui-widget-render-commit!
                                surface (%cbs-shm-slots state)
                                (%cbs-shm-active-idx state)
                                width height widget
                                #:persistent? #f
                                #:shm-name (format #f "gliver-container-border-~a-shm"
                                                   (container-id container))
                                #:pre-commit (lambda ()
                                               (wm-shell-surface-sync-next-commit! shell-surf))))
                           (lambda (updated-slots next-slot-index wl-buffer _hit-boxes)
                             (%cbs-shm-slots-set! state updated-slots)
                             (%cbs-shm-active-idx-set! state next-slot-index)
                             (when (valid-pointer? wl-buffer)
                               (%cbs-wl-buffer-set! state wl-buffer)
                               (%cbs-color-set! state color)
                               (%cbs-width-set! state width)
                               (%cbs-height-set! state height)
                               (%cbs-scale-set! state scale)
                               (%cbs-bg-color-set! state bg-color)))))))))))))))

(define (container-borders-update-all!)
  "Re-render container borders on the active workspace of every output.
Parameters: None."
  (for-each
   (lambda (output)
     (and-let* ((workspace (output-workspace-current output)))
       (for-each container-border-render!
                 (workspace-containers workspace))))
   (manager-outputs *manager*)))

;; event hooks
(define (container-border-on-globals-unbind)
  "Clean up all container border surfaces when Wayland globals unbind.
Parameters: None."
  (let ((containers (hash-map->list (lambda (container _state) container)
                                    *container-border-table*)))
    (for-each container-border-cleanup! containers)))

(define (container-border-on-listeners-attach)
  "Initialize and render borders for active containers when listeners attach.
Parameters: None."
  ;; wait until all globals are bound
  (when (and *wl-display*
             (valid-pointer? *wl-compositor*)
             (valid-pointer? *wl-shm*)
             (valid-pointer? (manager-wl-proxy *manager*)))
    ;; render all containers by walking through
    ;; outputs -> current workspace -> containers
    (for-each
     (lambda (output)
       (and-let* ((workspace (output-workspace-current output)))
         (for-each
          (lambda (container)
            (unless (container-wl-surface container)
              (container-border-init! container))
            (container-border-render! container))
          (workspace-containers workspace))))
     (manager-outputs *manager*))))

(define (container-border-on-container-created container)
  "Invoked when CONTAINER is created.
Initialize and render a border for CONTAINER.
Parameters: (CONTAINER)
  - CONTAINER: <container> record newly created."
  (when (container? container)
    (container-border-init! container)
    (container-border-render! container)))

(define (container-border-on-container-destroy container workspace)
  "Invoked when CONTAINER on WORKSPACE is destroyed.
Clean up border resources allocated for CONTAINER.
Parameters: (CONTAINER WORKSPACE)
  - CONTAINER: <container> record being destroyed.
  - WORKSPACE: <workspace> record that owned CONTAINER."
  (when (container? container)
    (container-border-cleanup! container)))

(define (container-border-on-container-focused container)
  "Invoked when CONTAINER is focused.
Render CONTAINER's border and refresh other border colors in the output
to ensure other containers that were focused are re-rendered as unfocused.
Parameters: (CONTAINER)
  - CONTAINER: <container> record newly focused."
  (when (container? container)
    (container-border-render! container)
    ;; also refresh any visible containers whose cached color became stale
    ;; (e.g. when focus moved across outputs without an explicit unfocus hook)
    (for-each
     (lambda (output)
       (and-let* ((workspace (output-workspace-current output)))
         (for-each
          (lambda (other-container)
            (and-let* (((container? other-container))
                       ((not (eq? other-container container)))
                       (state (get-container-border-state other-container))
                       ((not (equal? (%cbs-color state)
                                     (container-border-color other-container)))))
              (container-border-render! other-container)))
          (workspace-containers workspace))))
     (manager-outputs *manager*))))

(define (container-border-on-container-unfocused container)
  "Invoked when CONTAINER loses focus.
Re-render CONTAINER's border so it reflects the unfocused color.
Parameters: (CONTAINER)
  - CONTAINER: <container> record that lost focus."
  (when (container? container)
    (container-border-render! container)))

(define (container-border-on-container-resize container)
  "Invoked when CONTAINER geometry or position changes.
Re-render CONTAINER's border to match its updated position and size.
Parameters: (CONTAINER)
  - CONTAINER: <container> record resized or moved."
  (when (container? container)
    (container-border-render! container)))

(define (container-border-on-workspace-switch workspace prev-workspace)
  "Invoked when active workspace changes from PREV-WORKSPACE to WORKSPACE.
Hide borders on PREV-WORKSPACE and render borders on active WORKSPACE.
Parameters: (WORKSPACE PREV-WORKSPACE)
  - WORKSPACE: <workspace> record newly active, or #f.
  - PREV-WORKSPACE: <workspace> record previously active, or #f."
  (when (and (workspace? prev-workspace)
             (not (eq? prev-workspace workspace)))
    (for-each container-border-hide!
              (workspace-containers prev-workspace)))
  (when (workspace? workspace)
    (for-each container-border-render!
              (workspace-containers workspace))))

(define* (container-border-on-workspace-destroy workspace #:optional target #:rest rest)
  "Invoked when WORKSPACE is destroyed.
Clean up border resources for every container belonging to WORKSPACE.
Parameters: (WORKSPACE [TARGET] [REST ...])
  - WORKSPACE: <workspace> record being destroyed.
  - TARGET: Value optional hook argument (unused).
  - REST: List of additional hook arguments (unused)."
  (when (workspace? workspace)
    (for-each container-border-cleanup!
              (workspace-containers workspace))
    (let* ((containers     (hash-map->list (lambda (container _state) container)
                                           *container-border-table*))
           (in-workspace?  (lambda (container)
                             (and (container? container)
                                  (eq? (container-workspace container) workspace))))
           (orphaned       (filter in-workspace? containers)))
      (for-each container-border-cleanup! orphaned))))

(define (container-border-on-output-removed output)
  "Invoked when an OUTPUT is removed.
Hide container border nodes on every workspace belonging to
removed OUTPUT. We shouldn't destroy as the workspace still available
and the OUTPUT may be added again or another OUTPUT take over.
Parameters: (OUTPUT)
  - OUTPUT: <output> record being removed."
  (when (output? output)
    (for-each
     (lambda (workspace)
       (for-each container-border-hide! (workspace-containers workspace)))
     (output-workspaces output))))

(define (container-border-on-window-container-removed window container)
  "Invoked when a WINDOW is removed from CONTAINER.
Re-render CONTAINER's border, maybe it will be empty and bg filled now.
Parameters: (WINDOW CONTAINER)
  - WINDOW: <window> record removed from CONTAINER.
  - CONTAINER: <container> record that lost WINDOW."
  (when (container? container)
    (container-border-render! container)))

(define (container-border-on-window-container-added window container)
  "Invoked when a WINDOW is added to CONTAINER.
Re-render CONTAINER's border, maybe it was empty and had bg filled.
Parameters: (WINDOW CONTAINER)
  - WINDOW: <window> record added to CONTAINER.
  - CONTAINER: <container> record that received WINDOW."
  (when (container? container)
    (container-border-render! container)))

(define (container-border-on-shell-interaction seat proxy-shell)
  "Invoked when a container is clicked.
Focus the container matching clicked border PROXY-SHELL on SEAT.
Parameters: (SEAT PROXY-SHELL)
  - SEAT: <seat> record where interaction occurred.
  - PROXY-SHELL: Pointer to interacted river_shell_surface_v1 proxy."
  (when (and *seat-focus-on-mouse-click*
             (valid-pointer? proxy-shell))
    (and-let* ((container (container-find-by-shell-surface proxy-shell))
               ((container? container)))
      (log-debug "container-border: container ~a clicked via mouse shell surface" (container-id container))
      (container-focus! container))))

(define (container-border-on-window-fullscreen-entered window prev-status)
  "Invoked when a WINDOW enters fullscreen mode.
Hide the parent container's border, no need to hide other containers' borders
as current window and container will be placed on top hiding the others.
Parameters: (WINDOW PREV-STATUS)
  - WINDOW: <window> record entering fullscreen.
  - PREV-STATUS: Boolean previous fullscreen state (unused)."
  (and-let* ((container (window-container window))
             ((container? container)))
    (container-border-hide! container)))

(define (container-border-on-window-fullscreen-exited window prev-status)
  "Invoked when a WINDOW exists fullscreen mode.
Restore the parent container's border.
Parameters: (WINDOW PREV-STATUS)
  - WINDOW: <window> record exiting fullscreen.
  - PREV-STATUS: Boolean previous fullscreen state (unused)."
  (and-let* ((container (window-container window))
             ((container? container)))
    (container-border-render! container)))

(define (container-border-on-surface-scale-changed surface scale)
  "Invoked when the SURFACE SCALE changes.
Re-render the border so it respects the new SCALE.
Parameters: (SURFACE SCALE)
  - SURFACE: Pointer to Wayland wl_surface whose scale changed.
  - SCALE: Float new fractional scale factor."
  (hash-for-each
   (lambda (container _state)
     ;; find the container that has the surface that got updated
     (when (and (container? container)
                (equal? surface (container-wl-surface container)))
       (container-border-render! container #:force #t)))
   *container-border-table*))

(define *container-border-hooks*
  (list (cons *container-created-hook*          'container-border-on-container-created)
        (cons *container-destroy-hook*          'container-border-on-container-destroy)
        (cons *container-focused-hook*          'container-border-on-container-focused)
        (cons *container-unfocused-hook*        'container-border-on-container-unfocused)
        (cons *container-resize-hook*           'container-border-on-container-resize)
        (cons *container-position-changed-hook* 'container-border-on-container-resize)
        (cons *workspace-switch-hook*           'container-border-on-workspace-switch)
        (cons *workspace-destroy-hook*          'container-border-on-workspace-destroy)
        (cons *gliver-globals-unbind-hook*      'container-border-on-globals-unbind)
        (cons *gliver-listeners-attach-hook*    'container-border-on-listeners-attach)
        (cons *window-container-removed-hook*   'container-border-on-window-container-removed)
        (cons *window-container-added-hook*     'container-border-on-window-container-added)
        (cons *seat-shell-interacted-hook*      'container-border-on-shell-interaction)
        (cons *window-fullscreen-entered-hook*  'container-border-on-window-fullscreen-entered)
        (cons *window-fullscreen-exited-hook*   'container-border-on-window-fullscreen-exited)
        (cons *output-removed-hook*             'container-border-on-output-removed)
        (cons *surface-scale-changed-hook*      'container-border-on-surface-scale-changed)))

(define (container-border-enable!)
  "Install container border hooks and render borders across all active outputs.
Parameters: None."
  (unless *%container-border-enabled*
    (for-each (lambda (hook-pair)
                (gliver-hook-add! (car hook-pair) (cdr hook-pair)))
              *container-border-hooks*)
    (set! *%container-border-enabled* #t)
    (container-borders-update-all!)
    (log-info "container-border: enabled")))

(define (container-border-disable!)
  "Uninstall container border hooks and clean up all border surfaces.
Parameters: None."
  (when *%container-border-enabled*
    (for-each (lambda (hook-pair)
                (gliver-hook-remove! (car hook-pair) (cdr hook-pair)))
              *container-border-hooks*)
    (container-border-on-globals-unbind)
    (set! *%container-border-enabled* #f)
    (log-info "container-border: disabled")))
