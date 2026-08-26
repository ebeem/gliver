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
			layout-manual-make-config
			layout-manual-get-param
			layout-manual-placeholder?
			layout-manual-find-first-empty-container
			layout-manual-placeholder-spawn!
			layout-manual-placeholder-kill!
			layout-manual-container-cleanup-placeholders!
			layout-manual-reload
			layout-manual-update
			layout-manual-window-created
			layout-manual-window-fullscreen-exited
			layout-manual-window-destroyed
			layout-manual-workspace-created
			layout-manual-output-dimensions-changed
			layout-manual-container-created
			layout-manual-container-destroyed
			layout-manual-shutdown
			))

(define (layout-manual? workspace)
  "Return #t if the workspace layout is manual."
  (and-let* ((layout-cfg (workspace-layout workspace))
			 (layout-name (if (list? layout-cfg) (assq-ref layout-cfg 'layout) layout-cfg)))
	(equal? layout-name 'manual)))

(define (layout-manual-placeholder? window)
  "Return #t if WINDOW is a placeholder."
  (and window
	   (equal? (window-app-id window) *placeholder-app-id*)))

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

(define* (layout-manual-find-first-empty-container #:optional created-placeholder)
  "Check each active workspace in each output, and return the first empty container."
  ;; container is empty if it has no windows at all
  ;; the only exception is if the container has only one window
  ;; which the placeholder window that has just been created
  ;; such container should be considered empty
  (define (container-empty? container)
    (if created-placeholder
        (null? (delete created-placeholder (container-windows container)))
        (null? (container-windows container))))

  ;; get first empty container in a manual workspace
  (any (lambda (output)
         (let ((ws (output-workspace-current output)))
           (and ws
                (workspace-manual? ws)
                (find container-empty? (workspace-containers ws)))))
       (manager-outputs *manager*)))

(define (layout-manual-placeholder-spawn!)
  "Spawn a transparent placeholder Wayland client.
The placeholder keeps the container alive in the compositor's view
tree even when it has no application windows, similar to how StumpWM
handles empty frames. The placeholder window will later be placed
on an empty container or deleted if not needed."
  (exec (format #f "exec ~a" *placeholder-script*)))

(define (layout-manual-placeholder-kill! container)
  "Kill the placeholder window for CONTAINER, if one is running."
  (when container
    (let ((placeholders (filter layout-manual-placeholder? (container-windows container))))
      (for-each
       (lambda (win)
         (log-debug "placeholder: killing placeholder window ~a for container ~a"
					(window-id win) (container-id container))
         (window-close! win))
       placeholders))))

(define (layout-manual-container-cleanup-placeholders! container)
  "Ensure CONTAINER has at most 1 placeholder if empty of actual windows,
and 0 placeholders if it contains actual windows."
  (when container
    (let* ((windows (container-windows container))
           (placeholders (filter layout-manual-placeholder? windows))
           (actual-windows (filter (lambda (w) (not (layout-manual-placeholder? w))) windows)))
      (cond
       ;; if there are actual windows, kill all placeholders in this container
       ((pair? actual-windows)
        (for-each
         (lambda (win)
           (log-debug "Killing redundant placeholder ~a in container ~a"
                      (window-id win) (container-id container))
           (window-close! win))
         placeholders))
       ;; if there are no actual windows and more than 1 placeholder, keep the first and kill the rest
       ((> (length placeholders) 1)
        (for-each
         (lambda (win)
           (log-debug "Killing extra placeholder ~a in container ~a (keeping ~a)"
                      (window-id win) (container-id container) (window-id (car placeholders)))
           (window-close! win))
         (cdr placeholders)))
       ;; if there are no windows at all in the container, spawn a placeholder
       ((null? windows)
        (layout-manual-placeholder-spawn!))))))

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
						(manager-config-ref 'container-inner-gap)))
		 (outer-gap (or (layout-manual-get-param layout-cfg 'outer-gap #f)
						(manager-config-ref 'container-outer-gap)))
		 (border-width (manager-config-ref 'border-width))
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

(define (layout-manual-window-created window)
  "Handle a new window in the manual layout.
If the window is a placeholder, check each active workspace in each output
and place the placeholder in the first empty container.
If the window is an actual application, kill the placeholder for this container."
  (if (layout-manual-placeholder? window)
      (let ((target-container (layout-manual-find-first-empty-container window)))
        (if target-container
            (begin
              (log-debug "placeholder: placing placeholder window ~a into empty container ~a"
                         (window-id window) (container-id target-container))
              (unless (eq? target-container (window-container window))
                (window-move-to-container! window target-container #:focus #f))
              (let ((ws (container-workspace target-container)))
                (when (and ws (eq? (workspace-container-current ws) target-container))
                  (window-focus! window)))
              (layout-manual-container-cleanup-placeholders! target-container))
            (begin
              (log-debug "placeholder: placeholder window created, no empty container found; closing redundant placeholder ~a"
                         (window-id window))
              (window-close! window))))
      ;; actual application window: kill the placeholder for this container
      (let* ((container (window-container window))
             (workspace (and container (container-workspace container))))
        (when (and container
                   workspace
                   (workspace-manual? workspace))
          (layout-manual-container-cleanup-placeholders! container))))
  (let* ((container (window-container window))
         (workspace (and container (container-workspace container))))
    (layout-manual-update 'window-created workspace container window)))

(define (layout-manual-window-fullscreen-exited window prev-state)
  (let* ((container (window-container window))
		 (workspace (and container (container-workspace container))))
	(when (workspace-manual? workspace)
	  (layout-manual-reload workspace #:complete #t))))

(define (layout-manual-window-destroyed window container workspace)
  "Handle a window being destroyed in the manual layout.
Ensure the container maintains at most 1 placeholder."
  (when (and container workspace (workspace-manual? workspace))
	(catch #t (lambda () (waitpid WAIT_ANY WNOHANG)) (lambda _ #f))
	(layout-manual-container-cleanup-placeholders! container))
  (layout-manual-update 'window-destroyed workspace container window))

(define (layout-manual-workspace-created workspace)
  "Handle workspace creation for the manual layout.
Ensure placeholders are spawned for empty containers and at most 1 per container."
  (let* ((container (workspace-container-current workspace))
		 (window (and container (container-window-current container))))
	(layout-manual-update 'workspace-created workspace container window)
	(when (workspace-manual? workspace)
	  (for-each layout-manual-container-cleanup-placeholders!
	            (workspace-containers workspace)))))

(define (layout-manual-output-dimensions-changed output prev-width prev-height)
  "Handle output dimensions changes in manual layout."
  (let* ((workspace (output-workspace-current output))
		 (container (and workspace (workspace-container-current workspace)))
		 (window (and container (container-window-current container))))
	(layout-manual-update 'output-dimensions-changed workspace container window)))

(define (layout-manual-container-created container)
  "Handle container creation in manual layout.
Ensure the container has at most 1 placeholder."
  (let ((workspace (container-workspace container)))
    (when (and workspace (workspace-manual? workspace))
      (layout-manual-container-cleanup-placeholders! container))))

(define (layout-manual-container-destroyed container workspace)
  "Handle container destruction in manual layout.
Resize the next container after the destroyed container to fill both
the destroyed container's space and its own space, and clean up placeholders."
  (when (and workspace (workspace-manual? workspace))
    (layout-manual-placeholder-kill! container)
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
          (container-size-set! next-container new-w new-h)

          ;; destroying a container will move its window to another container
		  ;; ensure that no container will have more than 1 placeholder as
		  ;; placeholders are also moved on container destroyed
          (for-each layout-manual-container-cleanup-placeholders! remaining))))))

;; TODO: also call this when the layout changes from manual to auto
(define (layout-manual-shutdown)
  "Kill all placeholder windows on gliver shutdown."
  (log-debug "manual layout shutdown: cleaning up all placeholders")
  (let* ((windows (manager-windows *manager*))
         (placeholders (filter layout-manual-placeholder? windows)))
    (for-each
     (lambda (win)
       (log-debug "placeholder: killing placeholder window ~a on shutdown" (window-id win))
       (window-close! win))
     placeholders)))

(gliver-hook-add! *window-created-hook* 'layout-manual-window-created)
(gliver-hook-add! *window-fullscreen-exited-hook* 'layout-manual-window-fullscreen-exited)
(gliver-hook-add! *window-destroyed-hook* 'layout-manual-window-destroyed)
(gliver-hook-add! *workspace-created-hook* 'layout-manual-workspace-created)
(gliver-hook-add! *output-dimensions-changed-hook* 'layout-manual-output-dimensions-changed)
(gliver-hook-add! *container-created-hook* 'layout-manual-container-created)
(gliver-hook-add! *container-destroy-hook* 'layout-manual-container-destroyed)
(gliver-hook-add! *shutdown-hook* 'layout-manual-shutdown)
