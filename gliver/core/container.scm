;;; gliver/core/container.scm --- Core data model for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver core container)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (gliver core types)
  #:use-module (gliver core logs)
  #:use-module (gliver core config)
  #:use-module (gliver core hooks)
  #:autoload (gliver core window) (window-focus!
								   window-position-set!
								   window-dimensions-propose!
								   window-move-to-container!)
  #:autoload (gliver core seat) (seat-wm-window-focus-clear)
  #:autoload (gliver core workspace) (workspace-focus!
									  workspace-focused?)
  #:export (
			container-next
			container-prev
			container-center-x
			container-center-y
			container-in-direction
			container-add!
			container-move-windows-to-container!
			container-remove!
			container-focused?
			container-focus!
			container-size-set!
			container-position-set!
			container-usable-area-update-windows!
))

(define* (container-next current #:key (recursive #t))
  "Return the next container after CURRENT container."
  (and-let* ((workspace (container-workspace current))
			 (containers (workspace-containers workspace))
			 (idx (list-index (lambda (f) (eq? f current)) containers)))
    (cond
     ((not idx)
      (and (pair? containers) (car containers)))
     ((and (not recursive) (= (1+ idx) (length containers)))
      #f)
     (else
      (list-ref containers (modulo (1+ idx) (length containers)))))))

(define* (container-prev current #:key (recursive #t))
  "Return the previous container before CURRENT container."
  (and-let* ((workspace (container-workspace current))
			 (containers (workspace-containers workspace))
			 (idx (list-index (lambda (f) (eq? f current)) containers)))
    (cond
     ((not idx)
      (and (pair? containers) (last containers)))
     ((and (not recursive) (zero? idx))
      #f)
     (else
      (list-ref containers (modulo (1- idx) (length containers)))))))

(define (container-center-x f)
  (+ (container-x f) (quotient (container-width f) 2)))

(define (container-center-y f)
  (+ (container-y f) (quotient (container-height f) 2)))

(define (%container-active-global)
  "Collect all active containers mapped to global screen coordinates."
  (append-map
   (lambda (out)
     (let ((ox (output-x out))
           (oy (output-y out))
           (ws (output-workspace-current out)))
       (if ws
           (map (lambda (con)
                  (let* ((x (+ ox (container-x con)))
                         (y (+ oy (container-y con)))
                         (w (container-width con))
                         (h (container-height con)))
                    (list con x y
						  (+ x w)
						  (+ y h)
						  (+ x (/ w 2.0))
						  (+ y (/ h 2.0)))))
                (workspace-containers ws))
           '())))
   (manager-outputs *manager*)))

(define* (container-in-direction dir #:key (current (container-current)))
  "Find the closest container in direction DIR in unified screen space."
  (let* ((all (%container-active-global))
         (cur (find (lambda (e) (eq? (car e) current)) all)))
    (and cur
         (let* ((dir-sym (if (string? dir) (string->symbol dir) dir))
                (cx0 (list-ref cur 1))  ;; start x coordinate
				(cy0 (list-ref cur 2))  ;; start y coordinate
                (cx1 (list-ref cur 3))  ;; end x coordinate
				(cy1 (list-ref cur 4))  ;; end y coordinate
                (ccx (list-ref cur 5))  ;; center of x
				(ccy (list-ref cur 6))  ;; center of y
                (cands (filter (lambda (e) (not (eq? (car e) current))) all))
                (dist-sq (lambda (e)
                           (let ((dx (- (list-ref e 5) ccx))
                                 (dy (- (list-ref e 6) ccy)))
                             (+ (* dx dx) (* dy dy)))))
                (pick (lambda (lst)
                        (and (pair? lst)
                             (caar (sort lst (lambda (a b) (< (dist-sq a) (dist-sq b))))))))
                (match? (lambda (e primary?)
                          (let ((tx0 (list-ref e 1)) (ty0 (list-ref e 2))
                                (tx1 (list-ref e 3)) (ty1 (list-ref e 4))
                                (tcx (list-ref e 5)) (tcy (list-ref e 6)))
                            (case dir-sym
                              ((right) (and (> tcx ccx) (or (not primary?) (>= tx0 (- cx1 1)))))
                              ((left)  (and (< tcx ccx) (or (not primary?) (<= tx1 (+ cx0 1)))))
                              ((down)  (and (> tcy ccy) (or (not primary?) (>= ty0 (- cy1 1)))))
                              ((up)    (and (< tcy ccy) (or (not primary?) (<= ty1 (+ cy0 1)))))
                              (else #f))))))
           (or (pick (filter (lambda (e) (match? e #t)) cands))
               (pick (filter (lambda (e) (match? e #f)) cands)))))))

(define* (container-add! container #:key (focus #t))
  "Add a new window to the display, placing it in the current container."
  (log-debug "adding container ~a" container)
  (let ((workspace (container-workspace container)))
	;; add the container to the back referenced workspace containers
	(%workspace-containers-set! workspace
	 (append (workspace-containers workspace) (list container)))

	;; focus the container if no container is currently focused
	;; or if configuration is set to focus new container
	(when (and focus
               (or *wm-behavior-focus-new-container*
			       (not (workspace-container-current workspace))))
	  (container-focus! container))

	(gliver-hook-run! *container-created-hook* container)))

(define (container-move-windows-to-container! source-container target-container)
  "Moves all the windows under source-container to target-container"
  (for-each (lambda (window)
            (window-move-to-container! window target-container #:focus #f))
          (container-windows source-container)))

(define* (container-remove! container #:key (target-container #f) (focus #t))
  "Remove CONTAINER from its workspace."
  (let* ((workspace (container-workspace container))
		 (container-target (or target-container
							   (container-next container #:recursive #f)
							   (container-prev container #:recursive #f))))
    (when workspace
	  (unless (null? (container-windows container))
		(container-move-windows-to-container! container container-target))
      ;; mark destroyed
      (%container-destroyed-set! container #t)

      ;; remove container from workspace's container list
      (%workspace-containers-set! workspace
                                 (delq container (workspace-containers workspace)))
      ;; focus a new container if the current focused container will be removed
      (when (and focus (eq? (workspace-container-current workspace) container))
		;; if we have any windows in the container, they should move to focused container
        (container-focus! container-target))
      (log-debug "Running *container-destroy-hook*")
      (gliver-hook-run! *container-destroy-hook* container workspace))))

(define (container-focused? container)
  "Returns true if the container is currently focused"
  (eq? container (container-current)))

(define* (container-focus! container #:key (focus-child #t) (focus-parent #t))
  "Focus a container by focusing its last focused window."
  (when (and (container? container) (not (container-destroyed? container)))
    (log-debug "focusing container ~a, is focused? = ~a" container
			   (container-focused? container))
    (unless (container-focused? container)
	  (let* ((windows (container-windows container))
			 (window (or (container-window-current container)
						 (and (pair? windows) (car windows))))
			 (workspace (container-workspace container))
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
		          (seat-wm-window-focus-clear seat)))))))))

(define* (container-usable-area-update-windows! container #:key (animate #t))
  "Apply the container's usable area to all non-fullscreen windows inside CONTAINER."
  (when (and (container? container) (not (container-destroyed? container)))
    (let ((ux (container-usable-x container))
          (uy (container-usable-y container))
          (uw (container-usable-width container))
          (uh (container-usable-height container)))
      (for-each
       (lambda (window)
         (unless (window-fullscreen? window)
           (window-position-set! window ux uy #:animate animate)
           (window-dimensions-propose! window uw uh #:animate animate)))
       (container-windows container)))))

(define* (container-size-set! container width height #:key (animate #t))
  "Resize the container to the provided width and height."
  (let* ((windows (container-windows container))
		 (int-width (inexact->exact (floor width)))
		 (int-height (inexact->exact (floor height)))
		 (w-changed? (not (= (container-width container) int-width)))
		 (h-changed? (not (= (container-height container) int-height))))
	(when (or w-changed? h-changed?)
      (let ((dw (- int-width (container-width container)))
            (dh (- int-height (container-height container))))
	    (%container-width-set! container int-width)
	    (%container-height-set! container int-height)
        (%container-usable-width-set! container (max 0 (+ (container-usable-width container) dw)))
        (%container-usable-height-set! container (max 0 (+ (container-usable-height container) dh)))
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
  "Move the container position to the provided x and y."
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
        (%container-usable-x-set! container (+ (container-usable-x container) dx))
        (%container-usable-y-set! container (+ (container-usable-y container) dy))
	    (for-each
	     (lambda (window)
           (unless (window-fullscreen? window)
		     (window-position-set! window
                                   (container-usable-x container)
                                   (container-usable-y container)
                                   #:animate animate)))
	     windows)
	    (gliver-hook-run! *container-resize-hook* container)))))
