;;; gliver/core/container.scm --- Container layout management and window grouping
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module manages container slots and window grouping in Gliver.
;;; It handles:
;;;  - Adding, removing, and focusing containers within workspaces
;;;  - Directional container navigation (up, down, left, right)
;;;  - Swapping and transferring windows between containers
;;;  - Updating usable areas, positions, and sizing for child windows

(define-module (gliver core container)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (gliver core-base)
  #:autoload (gliver core window) (window-focus!
								   window-position-set!
								   window-dimensions-propose!
								   window-move-to-container!)
  #:autoload (gliver core seat) (seat-wm-window-focus-clear)
  #:autoload (gliver core workspace) (workspace-focus!)
  #:declarative? #f
  #:export (
			*container-inner-gap*
			*container-outer-gap*
			*container-border-width*
			*container-border-color-focused*
			*container-border-color-unfocused*
			*container-border-color-urgent*
			*container-border-radius*
			*container-border-edges*
			*container-border-bg-color*
			*container-navigation-cycle-stacked-windows?*
			*container-focus-new?*
			container-next
			container-prev
			container-active-global
			container-dist-sq
			container-in-dir?
			container-pick-closest
			container-in-direction
			container-add!
			container-move-windows-to-container!
			container-swap-windows!
			container-remove!
			container-focused?
			container-focus!
			container-usable-area-update-windows!
			container-size-set!
			container-position-set!
))

;; gap between containers, managed by layouts
(define-var *container-inner-gap* 4
			"Inner gap size between containers in pixels.")
(define-var *container-outer-gap* 8
			"Outer gap size around containers in pixels.")

;; container border settings, managed by layouts and border drawing modules
(define-var *container-border-width* *theme-border-width*
            "Default container border width in pixels.")
(define-var *container-border-color-focused* *theme-border-color*
            "Border color for focused containers.")
(define-var *container-border-color-unfocused* *theme-mantle*
            "Border color for unfocused containers.")
(define-var *container-border-color-urgent* *theme-urgent-color*
            "Border color for urgent containers.")
(define-var *container-border-radius* *theme-border-radius*
            "Default container corner radius in pixels.")
(define-var *container-border-edges* *wm-behavior-default-border-edges*
            "Bitfield of edges on which container borders are drawn.")
(define-var *container-border-bg-color* "#00000044"
            "Background color for empty containers.")

;; container border navigation behavior, managed by commands
;; TODO: navigation behavior isn't implemented yet
(define-var *container-navigation-cycle-stacked-windows?* #t
            "When #t, navigating left or right between containers first cycles through stacked windows.")
(define-var *container-focus-new?* #t
			"Whether newly created containers are automatically focused.")

(define* (container-next current #:key (recursive #t))
  "Return the next container after CURRENT container.
Parameters: (CURRENT [#:recursive #t])
  - CURRENT: <container> record to move to next from.
  - RECURSIVE: Boolean, whether to wrap around at list boundary (default #t).
Returns next <container> record, or #f if not found."
  (and-let* ((workspace (container-workspace current))
			 (containers (workspace-containers workspace))
			 (idx (list-index (lambda (f) (eq? f current)) containers)))
    (cond
	 ;; index wasn't found: return first container in the workspace
	 ;; return #f if not containers are available (shouldn't happen)
     ((not idx)
      (and (pair? containers) (car containers)))
	 ;; out of bounds and not recursive: return #f
     ((and (not recursive) (= (1+ idx) (length containers)))
      #f)
	 ;; otherwise: return the next container (first if out of bounds)
     (else
      (list-ref containers (modulo (1+ idx) (length containers)))))))

(define* (container-prev current #:key (recursive #t))
  "Return the previous container before CURRENT container.
Parameters: (CURRENT [#:recursive #t])
  - CURRENT: <container> record to move to previous from.
  - RECURSIVE: Boolean, whether to wrap around at list boundary (default #t).
Returns previous <container> record, or #f if not found."
  (and-let* ((workspace (container-workspace current))
			 (containers (workspace-containers workspace))
			 (idx (list-index (lambda (f) (eq? f current)) containers)))
    (cond
	 ;; index wasn't found: return last container in the workspace
	 ;; return #f if not containers are available (shouldn't happen)
     ((not idx)
	  (and (pair? containers) (last containers)))
	 ;; out of bounds and not recursive: return #f
     ((and (not recursive) (zero? idx))
	  #f)
     (else
	  ;; otherwise: return the previous container (last if out of bounds)
	  (list-ref containers (modulo (1- idx) (length containers)))))))

(define (container-active-global)
  "Collect all active containers mapped to global screen coordinates.
This is helpful for navigation and finding closest container in a direction.
Parameters: None.
Returns list of active container entries in global screen coordinates."
  (append-map
   (lambda (output)
	 ;; get active workspace of each output
     (let ((ox (output-x output))
           (oy (output-y output))
           (ws (output-workspace-current output)))
       (if ws
           (map (lambda (con)
				  ;; get container on each workspace
                  (let* ((x (+ ox (container-x con)))
                         (y (+ oy (container-y con)))
                         (w (container-width con))
                         (h (container-height con)))
					;; return the container with its starting position (x, y)
					;; mapped to global screen coordinates, (output_position + container_position)
					;; this also returns ending position by adding container size to position
					;; and global center position of the container
                    (list con x y
						  (+ x w)
						  (+ y h)
						  (+ x (/ w 2.0))
						  (+ y (/ h 2.0)))))
                (workspace-containers ws))
           '())))
   (manager-outputs *manager*)))

(define (container-dist-sq current target)
  "Calculate squared Euclidean distance between the centers of CURRENT container and TARGET container.
Parameters: (CURRENT TARGET)
  - CURRENT: Container geometry tuple.
  - TARGET: Container geometry tuple.
Returns squared Euclidean distance number."
  (match-let (((_ _ _ _ _ ccx ccy) current)
              ((_ _ _ _ _ tcx tcy) target))
    (let ((dx (- tcx ccx))
          (dy (- tcy ccy)))
      (+ (* dx dx) (* dy dy)))))

(define (container-in-dir? dir current target strict?)
  "Check if TARGET is in direction DIR from CURRENT container.
If STRICT? is #t, TARGET's near edge must be past CURRENT's far edge.
Parameters: (DIR CURRENT TARGET STRICT?)
  - DIR: Direction symbol ('up, 'down, 'left, 'right).
  - CURRENT: Container geometry tuple.
  - TARGET: Container geometry tuple.
  - STRICT?: Boolean, whether to require strict far edge separation.
Returns #t if TARGET is in direction DIR from CURRENT, #f otherwise."
  (match-let (((_ cx0 cy0 cx1 cy1 ccx ccy) current)
              ((_ tx0 ty0 tx1 ty1 tcx tcy) target))
    (case dir
      ((right) (and (> tcx ccx) (or (not strict?) (>= tx0 (- cx1 1)))))
      ((left)  (and (< tcx ccx) (or (not strict?) (<= tx1 (+ cx0 1)))))
      ((down)  (and (> tcy ccy) (or (not strict?) (>= ty0 (- cy1 1)))))
      ((up)    (and (< tcy ccy) (or (not strict?) (<= ty1 (+ cy0 1)))))
      (else #f))))

(define (container-pick-closest current candidates)
  "Return the <container> record of the candidate closest to CURRENT, or #f if empty.
Parameters: (CURRENT CANDIDATES)
  - CURRENT: Container geometry tuple.
  - CANDIDATES: List of container geometry tuples.
Returns <container> record of the closest candidate, or #f if empty."
  (and (pair? candidates)
	   ;; caar will return first element of first item
	   ;; in this case it's the <container> record of the first candidate
       (caar (sort candidates
                   (lambda (a b)
                     (< (container-dist-sq current a)
                        (container-dist-sq current b)))))))

(define* (container-in-direction dir #:key (current (container-current)))
  "Find the closest container in direction DIR in unified screen space.
Parameters: (DIR [#:current (container-current)])
  - DIR: Direction symbol or string ('up, 'down, 'left, 'right).
  - CURRENT: <container> record to navigate from (defaults to current container).
Returns closest <container> record in direction DIR, or #f if none."
  (let* ((all (container-active-global))
		 ;; this is in order to get the container current with
		 ;; the same format as the rest where global coordinate is considered
         (cur (find (lambda (e) (eq? (car e) current)) all)))
    (and cur
         (let ((dir-sym (if (string? dir) (string->symbol dir) dir))
               (cands   (remove (lambda (e) (eq? (car e) current)) all)))
           (or (container-pick-closest cur (filter (lambda (e) (container-in-dir? dir-sym cur e #t)) cands))
               (container-pick-closest cur (filter (lambda (e) (container-in-dir? dir-sym cur e #f)) cands)))))))

(define* (container-add! container #:key (focus #t))
  "Register CONTAINER in its parent workspace's container list.
Parameters: (CONTAINER [#:focus #t])
  - CONTAINER: <container> record to register.
  - FOCUS: Boolean, whether to focus CONTAINER (default #t)."
  (and-let* ((workspace (container-workspace container)))
	(log-debug "adding container ~a to " container)
	;; add the container to the back referenced workspace containers
	(%workspace-containers-set! workspace
								(append (workspace-containers workspace) (list container)))
	;; focus the container if no container is currently focused
	;; or if configuration is set to focus new container
	(when (and focus
               (or *container-focus-new?*
			       (not (workspace-container-current workspace))))
	  (container-focus! container))
	(gliver-hook-run! *container-created-hook* container)))

(define (container-move-windows-to-container! source-container target-container)
  "Move all windows from SOURCE-CONTAINER to TARGET-CONTAINER without changing focus.
Parameters: (SOURCE-CONTAINER TARGET-CONTAINER)
  - SOURCE-CONTAINER: <container> record to move windows from.
  - TARGET-CONTAINER: <container> record to receive windows."
  (for-each (lambda (window)
              (window-move-to-container! window target-container #:focus #f))
			(container-windows source-container)))

(define (container-swap-windows! container win1 win2)
  "Swap the positions of WIN1 and WIN2 in CONTAINER's window list.
Parameters: (CONTAINER WIN1 WIN2)
  - CONTAINER: <container> record containing WIN1 and WIN2.
  - WIN1: First <window> record.
  - WIN2: Second <window> record."
  (if (and (container? container)
           (window? win1)
           (window? win2)
           (not (eq? win1 win2)))
      (let* ((windows (container-windows container))
			 ;; get index of both windows in the container list of windows
             (idx1 (list-index (lambda (w) (eq? w win1)) windows))
             (idx2 (list-index (lambda (w) (eq? w win2)) windows)))
        (if (and idx1 idx2 (not (= idx1 idx2)))
			;; form a new-windows list where all the elements remain the
			;; same except replace occurrences of win1 by win2 and vise versa
            (let ((new-windows (map (lambda (w)
                                      (cond
                                       ((eq? w win1) win2)
                                       ((eq? w win2) win1)
                                       (else w)))
                                    windows)))
			  ;; set the new-windows as the windows of the container
              (%container-windows-set! container new-windows)
			  (gliver-hook-run! *window-focused-hook* (container-window-current container)))))))

(define* (container-remove! container #:key (target-container #f) (focus #t))
  "Remove CONTAINER from its workspace.
Parameters: (CONTAINER [#:target-container #f] [#:focus #t])
  - CONTAINER: <container> record to remove.
  - TARGET-CONTAINER: Optional fallback <container> record.
  - FOCUS: Boolean, whether to focus fallback container if CONTAINER was focused (default #t)."
  (and-let* ((workspace (container-workspace container))
			 (container-target (or target-container
								   (container-next container #:recursive #f)
								   (container-prev container #:recursive #f))))
	;; move the windows to target container
	(unless (null? (container-windows container))
	  (container-move-windows-to-container! container container-target))
    ;; remove container from workspace's container list
    (%workspace-containers-set! workspace
                                (delq container (workspace-containers workspace)))
    ;; clear references on workspace if they point to this container
    (when (eq? (workspace-container-previous workspace) container)
      (%workspace-container-previous-set! workspace #f))
    (let ((was-current? (eq? (workspace-container-current workspace) container)))
      (when was-current?
        (%workspace-container-current-set! workspace #f))
      (gliver-hook-run! *container-destroy-hook* container workspace)
      ;; focus a new container if the current focused container was removed
      (when (and focus was-current?)
        (container-focus! container-target)))))

(define (container-focused? container)
  "Return #t if CONTAINER is currently the active focused container.
Parameters: (CONTAINER)
  - CONTAINER: <container> record to check.
Returns boolean indicating whether CONTAINER is focused."
  (eq? container (container-current)))

(define* (container-focus! container #:key (focus-child #t) (focus-parent #t))
  "Focus a container by focusing its last focused window.
Parameters: (CONTAINER [#:focus-child #t] [#:focus-parent #t])
  - CONTAINER: <container> record to focus.
  - FOCUS-CHILD: Boolean, whether to focus the active child window (default #t).
  - FOCUS-PARENT: Boolean, whether to focus the parent workspace (default #t)."
  (when (and container (container? container))
    (log-debug "focusing container ~a, is focused? = ~a" container
			   (container-focused? container))
	(let* ((windows (container-windows container))
		   (window (or (container-window-current container)
					   (and (pair? windows) (car windows)))))

	  ;; proceed with focusing if container isn't focused or
	  ;; container window is focused by seat
	  ;; window may be focused by seat when switching workspaces
      (unless (and (container-focused? container)
				   (eq? window (seat-window-focused (seat-current))))
		(let* ((workspace (container-workspace container))
			   (prev-container (and workspace (workspace-container-current workspace)))
			   (focused-container (container-current))
			   (prev-window (window-current)))
	      (when (and workspace (workspace? workspace))
			(%workspace-container-previous-set! workspace prev-container)
			(%workspace-container-current-set! workspace container)
			(when focus-parent
		      (workspace-focus! workspace #:focus-child #f)))

          ;; unfocus previously focused container
          (when (and focused-container (not (eq? focused-container container)))
			(gliver-hook-run! *container-unfocused-hook* focused-container))
          (when (and prev-container
					 (not (eq? prev-container container))
					 (not (eq? prev-container focused-container)))
			(gliver-hook-run! *container-unfocused-hook* prev-container))
          (gliver-hook-run! *container-focused-hook* container)

	      (if (and focus-child window)
		      (begin
				(when (and prev-window (not (eq? prev-window window)))
		          (gliver-hook-run! *window-unfocused-hook* prev-window))
				(window-focus! window #:focus-parent #f))
		      (when (and focus-child (not window))
				(when (and prev-window (window? prev-window) (not (window-destroyed? prev-window)))
		          (gliver-hook-run! *window-unfocused-hook* prev-window))
				(let ((seat (seat-current)))
		          (when (and seat (seat? seat) (seat-window-focused seat))
					(seat-wm-window-focus-clear seat))))))))))

(define* (container-usable-area-update-windows! container #:key (animate #t))
  "Apply the container's usable area to all non-fullscreen windows inside CONTAINER.
Parameters: (CONTAINER [#:animate #t])
  - CONTAINER: <container> record whose windows to update.
  - ANIMATE: Boolean, whether to animate the transition (default #t)."
  (when (and container (container? container))
    (let ((ux (container-usable-x container))
          (uy (container-usable-y container))
          (uw (container-usable-width container))
          (uh (container-usable-height container)))
      (for-each
       (lambda (window)
		 ;; fullscreen windows shouldn't be altered
		 ;; they should occupy the whole screen regardless of container size
         (unless (window-fullscreen? window)
		   ;; update position and dimensions (size) to usable ones
           (window-position-set! window ux uy #:animate animate)
           (window-dimensions-propose! window uw uh #:animate animate)))
       (container-windows container)))))

(define* (container-size-set! container width height #:key (animate #t))
  "Resize the container to the provided WIDTH and HEIGHT.
Parameters: (CONTAINER WIDTH HEIGHT [#:animate #t])
  - CONTAINER: <container> record to resize.
  - WIDTH: New width in pixels.
  - HEIGHT: New height in pixels.
  - ANIMATE: Boolean, whether to animate the resize (default #t)."
  (let* ((windows (container-windows container))
		 (int-width (inexact->exact (floor width)))
		 (int-height (inexact->exact (floor height)))
		 ;; compare against current ones to skip if no change was made
		 (w-changed? (not (= (container-width container) int-width)))
		 (h-changed? (not (= (container-height container) int-height))))
	(when (or w-changed? h-changed?)
      (let ((dw (- int-width (container-width container)))
            (dh (- int-height (container-height container))))
	    (%container-width-set! container int-width)
	    (%container-height-set! container int-height)
		;; this is a nice to have prediction of usable width and height
		;; as those are expected to change with size as well
		;; the approach now it they will increase/decrease by the difference
		;; in size from last container size, typically this should be followed
		;; by a container-usable-area-set! call to update usable area 
        (%container-usable-width-set! container (max 0 (+ (container-usable-width container) dw)))
        (%container-usable-height-set! container (max 0 (+ (container-usable-height container) dh)))
		;; update size of all windows in the container
		;; otherwise they will have previous container size
		(for-each
	     (lambda (window)
           (unless (window-fullscreen? window)
		     (window-dimensions-propose! window
                                         (container-usable-width container)
                                         (container-usable-height container)
                                         #:animate animate)))
	     windows)
	    (gliver-hook-run! *container-resize-hook* container)))))

(define* (container-position-set! container x y #:key (animate #t))
  "Move the container position to the provided X and Y.
Parameters: (CONTAINER X Y [#:animate #t])
  - CONTAINER: <container> record to move.
  - X: New horizontal position in pixels.
  - Y: New vertical position in pixels.
  - ANIMATE: Boolean, whether to animate the movement (default #t)."
  (let* ((windows (container-windows container))
		 (int-x (inexact->exact (floor x)))
		 (int-y (inexact->exact (floor y)))
		 (x-changed? (not (= (container-x container) int-x)))
		 (y-changed? (not (= (container-y container) int-y))))
	(when (or x-changed? y-changed?)
      (let ((dx (- int-x (container-x container)))
            (dy (- int-y (container-y container))))
	    (%container-x-set! container int-x)
	    (%container-y-set! container int-y)
		;; review container-size-set! for explanation on this snippet
		;; it follows the same exact approach in predicting new usable position
        (%container-usable-x-set! container (+ (container-usable-x container) dx))
        (%container-usable-y-set! container (+ (container-usable-y container) dy))
		;; update position of all windows in the container
		;; otherwise they will have previous container position
	    (for-each
	     (lambda (window)
           (unless (window-fullscreen? window)
		     (window-position-set! window
                                   (container-usable-x container)
                                   (container-usable-y container)
                                   #:animate animate)))
	     windows)
	    (gliver-hook-run! *container-position-changed-hook* container)))))
