;;; gliver/contrib/layout/manual.scm --- Manual layout for gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; StumpWM-style manual layout where the window manager doesn't
;;; automatically create/destroy containers or rearrange windows.

(define-module (gliver contrib layout manual)
  #:use-module (gliver core)
  #:use-module (gliver contrib commands)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:declarative? #f
  #:export (
			layout-manual?
			layout-manual-make-config
			layout-manual-get-param
			layout-manual-reload
			layout-manual-reload-all!
			layout-manual-update
			layout-manual-container-created
			layout-manual-window-created
			layout-manual-window-fullscreen-exited
			layout-manual-window-destroyed
			layout-manual-workspace-created
			layout-manual-workspace-switched
			layout-manual-output-dimensions-changed
			layout-manual-container-destroyed
			layout-manual-config-loaded
			layout-manual-startup
))

(define (layout-manual? workspace)
  "Return #t if the workspace layout is manual."
  (and-let* ((layout-cfg (workspace-layout workspace))
			 (layout-name (if (list? layout-cfg) (assq-ref layout-cfg 'layout) layout-cfg)))
	(equal? layout-name 'manual)))

(define* (layout-manual-make-config #:key
									(split-ratio 0.5)
									(inner-gap #f)
									(outer-gap #f))
  "Create an associated list for the manual layout configuration."
  `((layout . manual)
	(layout-type . manual)
    (split-ratio . ,split-ratio)
    (inner-gap . ,inner-gap)
    (outer-gap . ,outer-gap)))

(define (layout-manual-get-param layout-cfg key default-val)
  "Helper to safely extract a parameter from the layout configuration."
  (if (list? layout-cfg)
      (let ((pair (assq key layout-cfg)))
        (if pair (cdr pair) default-val))
      default-val))

(define* (layout-manual-reload workspace #:key (complete #f))
  "Reload function that ensures all containers in WORKSPACE respect
the output geometry and gap configuration.
Rescales existing container positions/sizes so they proportionally
fit the current output dimensions (e.g. after container is destroyed)."
  (log-debug "layout-manual-reload on workspace ~a" workspace)
  (let* ((output (workspace-output workspace))
		 (ow (output-width output))
		 (oh (output-height output))
		 (layout-cfg (workspace-layout workspace))
		 (containers (workspace-containers workspace))
		 (containers-count (length containers))

		 ;; gap configuration
		 (inner-gap (or (layout-manual-get-param layout-cfg 'inner-gap #f)
						*container-inner-gap*))
		 (outer-gap (or (layout-manual-get-param layout-cfg 'outer-gap #f)
						*container-outer-gap*))
		 (border-width (or *container-border-width* *window-border-width* *theme-border-width* 3))
		 (total-outer-gap (+ outer-gap border-width))
		 (avail-width  (- ow (* 2 total-outer-gap)))
		 (avail-height (- oh (* 2 total-outer-gap))))

	(cond
	 ;; no containers, TODO: make sure at least one container is available
	 ((zero? containers-count)
	  (log-debug "layout-manual-reload: no containers in workspace, creating one"))

	 ;; exactly one container, it should fill the whole output (minus gaps).
	 ((= containers-count 1)
	  (let ((c (car containers)))
		(container-size-set! c avail-width avail-height)
		(container-position-set! c total-outer-gap total-outer-gap)))

	 ;; multiple containers, rescale proportionally.
	 ;; compute the available area (excluding outer-gap)
	 ;; rescale each containers size based on available area and
	 ;; consider inner-gap. set position as expected.
	 (else
	  (let* ((xs (map container-x containers))
			 (ys (map container-y containers))
			 (min-x (apply min xs))
			 (min-y (apply min ys))
			 (max-x+w (apply max (map (lambda (c)
										(+ (container-x c) (container-width c)))
									  containers)))
			 (max-y+h (apply max (map (lambda (c)
										(+ (container-y c) (container-height c)))
									  containers)))
			 (bbox-w (max 1 (- max-x+w min-x)))
			 (bbox-h (max 1 (- max-y+h min-y))))
		(for-each
		 (lambda (c)
		   (let* ((rel-x (/ (- (container-x c) min-x) bbox-w))
				  (rel-y (/ (- (container-y c) min-y) bbox-h))
				  (rel-w (/ (container-width c) bbox-w))
				  (rel-h (/ (container-height c) bbox-h))
				  (new-x (+ total-outer-gap (* rel-x avail-width)))
				  (new-y (+ total-outer-gap (* rel-y avail-height)))
				  (new-w (* rel-w avail-width))
				  (new-h (* rel-h avail-height)))
			 (container-size-set! c new-w new-h)
			 (container-position-set! c new-x new-y)))
		 containers))))))

(define (layout-manual-update hook workspace container window)
  "Main orchestrator for the manual layout.
In a manual layout the only automatic action is placing new windows
into the currently focused container."
  (when (and hook workspace (layout-manual? workspace))
    (layout-manual-reload workspace #:complete #t)))

(define (layout-manual-window-created window)
  "Handle a window being created in the manual layout."
  (let* ((container (window-container window))
		 (workspace (and container (container-workspace container))))
	(layout-manual-update 'window-created workspace container window)))

(define (layout-manual-window-fullscreen-exited window prev-state)
  (let* ((container (window-container window))
		 (workspace (and container (container-workspace container))))
	(when (layout-manual? workspace)
	  (layout-manual-reload workspace #:complete #t))))

(define (layout-manual-window-destroyed window container workspace)
  "Handle a window being destroyed in the manual layout.
Ensure the container maintains at most 1 placeholder."
  (layout-manual-update 'window-destroyed workspace container window))

(define (layout-manual-workspace-created workspace)
  "Handle workspace creation for the manual layout.
Ensure placeholders are spawned for empty containers and at most 1 per container."
  (let* ((container (workspace-container-current workspace))
		 (window (and container (container-window-current container))))
	(layout-manual-update 'workspace-created workspace container window)))

(define (layout-manual-workspace-switched workspace prev-workspace)
  "Handle workspace switch for the manual layout."
  (when (layout-manual? workspace)
    (layout-manual-reload workspace #:complete #t)))

(define (layout-manual-output-dimensions-changed output prev-width prev-height)
  "Handle output dimensions changes in manual layout."
  (let* ((workspace (output-workspace-current output))
		 (container (and workspace (workspace-container-current workspace)))
		 (window (and container (container-window-current container))))
	(layout-manual-update 'output-dimensions-changed workspace container window)))

(define (layout-manual-container-destroyed container workspace)
  "Handle container destruction in manual layout.
Resize the next container after the destroyed container to fill both
the destroyed container's space and its own space, and clean up placeholders."
  (when (and workspace (layout-manual? workspace))
    (let ((remaining (workspace-containers workspace)))
      (cond
       ((null? remaining)
        #f)
       ((= (length remaining) 1)
        (layout-manual-reload workspace #:complete #t))
       (else
        (let* ((layout-cfg (workspace-layout workspace))
               (inner-gap (or (layout-manual-get-param layout-cfg 'inner-gap #f)
                              *container-inner-gap*))
               (border-width (or *container-border-width* *window-border-width* *theme-border-width* 3))
               (total-inner-gap (+ inner-gap border-width))
               (tolerance (+ (* 2 total-inner-gap) 10))

               (x1 (container-x container))
               (y1 (container-y container))
               (w1 (container-width container))
               (h1 (container-height container))
               (x1-max (+ x1 w1))
               (y1-max (+ y1 h1))

               (target (or (and (memq (workspace-container-current workspace) remaining)
                                (workspace-container-current workspace))
                           (car remaining)))

               ;; find candidates in each of the 4 adjacent directions
               (candidates-below
                (filter
                 (lambda (c)
                   (let ((cx (container-x c))
                         (cy (container-y c))
                         (cw (container-width c)))
                     (and (>= cx (- x1 tolerance))
                          (<= (+ cx cw) (+ x1-max tolerance))
                          (>= cy (- y1-max tolerance)))))
                 remaining))

               (candidates-above
                (filter
                 (lambda (c)
                   (let ((cx (container-x c))
                         (cy (container-y c))
                         (cw (container-width c))
                         (ch (container-height c)))
                     (and (>= cx (- x1 tolerance))
                          (<= (+ cx cw) (+ x1-max tolerance))
                          (<= (+ cy ch) (+ y1 tolerance)))))
                 remaining))

               (candidates-right
                (filter
                 (lambda (c)
                   (let ((cx (container-x c))
                         (cy (container-y c))
                         (ch (container-height c)))
                     (and (>= cy (- y1 tolerance))
                          (<= (+ cy ch) (+ y1-max tolerance))
                          (>= cx (- x1-max tolerance)))))
                 remaining))

               (candidates-left
                (filter
                 (lambda (c)
                   (let ((cx (container-x c))
                         (cy (container-y c))
                         (cw (container-width c))
                         (ch (container-height c)))
                     (and (>= cy (- y1 tolerance))
                          (<= (+ cy ch) (+ y1-max tolerance))
                          (<= (+ cx cw) (+ x1 tolerance)))))
                 remaining))

               ;; choose the best candidate group
               ;; prioritize group containing target container then any non-empty group
               (selected-group
                (cond
                 ((and (memq target candidates-below) (pair? candidates-below))
                  candidates-below)
                 ((and (memq target candidates-above) (pair? candidates-above))
                  candidates-above)
                 ((and (memq target candidates-right) (pair? candidates-right))
                  candidates-right)
                 ((and (memq target candidates-left) (pair? candidates-left))
                  candidates-left)
                 ((pair? candidates-below) candidates-below)
                 ((pair? candidates-above) candidates-above)
                 ((pair? candidates-right) candidates-right)
                 ((pair? candidates-left)  candidates-left)
                 (else (list target)))))

          (if (pair? selected-group)
              (let* ((xs (map container-x selected-group))
                     (ys (map container-y selected-group))
                     (min-sx (apply min xs))
                     (min-sy (apply min ys))
                     (max-sx (apply max (map (lambda (c) (+ (container-x c) (container-width c))) selected-group)))
                     (max-sy (apply max (map (lambda (c) (+ (container-y c) (container-height c))) selected-group)))
                     (sw (max 1 (- max-sx min-sx)))
                     (sh (max 1 (- max-sy min-sy)))

                     (new-min-x (min x1 min-sx))
                     (new-min-y (min y1 min-sy))
                     (new-max-x (max x1-max max-sx))
                     (new-max-y (max y1-max max-sy))
                     (new-w (max 1 (- new-max-x new-min-x)))
                     (new-h (max 1 (- new-max-y new-min-y)))

                     ;; determine if we are scaling along x and y
                     (scale-x? (and (> (abs (- new-w sw)) tolerance) (> sw 0)))
                     (scale-y? (and (> (abs (- new-h sh)) tolerance) (> sh 0))))

                (log-debug "layout-manual-container-destroyed: upscaling ~a containers from ~ax~a to ~ax~a"
                           (length selected-group) sw sh new-w new-h)

                (for-each
                 (lambda (c)
                   (let* ((c-x (if scale-x?
                                   (+ new-min-x (* (/ (- (container-x c) min-sx) sw) new-w))
                                   (container-x c)))
                          (c-w (if scale-x?
                                   (* (/ (container-width c) sw) new-w)
                                   (container-width c)))
                          (c-y (if scale-y?
                                   (+ new-min-y (* (/ (- (container-y c) min-sy) sh) new-h))
                                   (container-y c)))
                          (c-h (if scale-y?
                                   (* (/ (container-height c) sh) new-h)
                                   (container-height c))))
                     (container-position-set! c c-x c-y)
                     (container-size-set! c c-w c-h)))
                 selected-group))
              ;; fallback: reload entire layout proportionally
              (layout-manual-reload workspace #:complete #t))))))))

(define (layout-manual-container-created container)
  "Handle container creation in manual layout."
  (let ((workspace (and container (container-workspace container))))
    (when (and workspace (layout-manual? workspace))
      (when (= (length (workspace-containers workspace)) 1)
        (layout-manual-reload workspace #:complete #t)))))

(define (layout-manual-reload-all!)
  "Reload layout for all manual workspaces across all outputs."
  (when (and (defined? '*manager*) *manager*)
    (for-each
     (lambda (output)
       (for-each
        (lambda (ws)
          (when (layout-manual? ws)
            (layout-manual-reload ws #:complete #t)))
        (output-workspaces output)))
     (manager-outputs *manager*))))

(define (layout-manual-startup)
  "Handle startup hook by reloading all manual workspaces."
  (layout-manual-reload-all!))

(define (layout-manual-config-loaded)
  "Handle configuration reload by updating all manual workspaces."
  (layout-manual-reload-all!))

(gliver-hook-add! *container-created-hook* 'layout-manual-container-created)
(gliver-hook-add! *window-created-hook* 'layout-manual-window-created)
(gliver-hook-add! *window-fullscreen-exited-hook* 'layout-manual-window-fullscreen-exited)
(gliver-hook-add! *window-destroyed-hook* 'layout-manual-window-destroyed)
(gliver-hook-add! *workspace-created-hook* 'layout-manual-workspace-created)
(gliver-hook-add! *workspace-switch-hook* 'layout-manual-workspace-switched)
(gliver-hook-add! *output-dimensions-changed-hook* 'layout-manual-output-dimensions-changed)
(gliver-hook-add! *container-destroy-hook* 'layout-manual-container-destroyed)
(gliver-hook-add! *config-loaded-hook* 'layout-manual-config-loaded)
(gliver-hook-add! *startup-hook* 'layout-manual-startup)

;; reload any existing manual workspaces immediately upon loading this module
(layout-manual-reload-all!)
