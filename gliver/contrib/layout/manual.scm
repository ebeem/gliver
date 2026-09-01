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
			layout-manual-update
			layout-manual-window-fullscreen-exited
			layout-manual-window-destroyed
			layout-manual-workspace-created
			layout-manual-output-dimensions-changed
			layout-manual-container-destroyed
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
  (when (and hook workspace)
	(let* ((layout-cfg (workspace-layout workspace))
           (layout-name (if (list? layout-cfg) (assq-ref layout-cfg 'layout) layout-cfg)))
      (when (equal? layout-name 'manual)
		(layout-manual-reload workspace #:complete #t)))))

(define (layout-manual-window-fullscreen-exited window prev-state)
  (let* ((container (window-container window))
		 (workspace (and container (container-workspace container))))
	(when (workspace-manual? workspace)
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
  (when (and workspace (workspace-manual? workspace))
    (let ((remaining (workspace-containers workspace)))
      (when (pair? remaining)
        (let* ((next-container (or (and (memq (workspace-container-current workspace) remaining)
                                        (workspace-container-current workspace))
                                   (car remaining)))
               (x1 (container-x container))
               (y1 (container-y container))
               (w1 (container-width container))
               (h1 (container-height container))
               (x2 (container-x next-container))
               (y2 (container-y next-container))
               (w2 (container-width next-container))
               (h2 (container-height next-container))
               (new-x (min x1 x2))
               (new-y (min y1 y2))
               (new-w (- (max (+ x1 w1) (+ x2 w2)) new-x))
               (new-h (- (max (+ y1 h1) (+ y2 h2)) new-y)))

		  ;; BUG: logic needs more thinking, this will work if the next container size +
		  ;; destroyed container size = the new size, what could happen is if there are
		  ;; 3 containers remaining after destroyed container, the new size will overlap with the remaining
		  ;; reproduce by create 5 alternating splits, then remove the second container (top-right)
          (log-debug "layout-manual-container-destroyed: resizing container ~a to ~ax~a+~a+~a"
                     (container-id next-container) new-w new-h new-x new-y)
          (container-position-set! next-container new-x new-y)
          (container-size-set! next-container new-w new-h))))))

(gliver-hook-add! *window-fullscreen-exited-hook* 'layout-manual-window-fullscreen-exited)
(gliver-hook-add! *window-destroyed-hook* 'layout-manual-window-destroyed)
(gliver-hook-add! *workspace-created-hook* 'layout-manual-workspace-created)
(gliver-hook-add! *output-dimensions-changed-hook* 'layout-manual-output-dimensions-changed)
(gliver-hook-add! *container-destroy-hook* 'layout-manual-container-destroyed)
