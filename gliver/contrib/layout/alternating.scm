;;; gliver/contrib/layout/alternating.scm --- Alternating layout for gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This layout provides custom parameters to adjust the behavior so
;;; that it can act like an alternating layout, single tabbed layout
;;; stack layout (horizontal and vertical)
;;; between tiled windows and screen edges.

(define-module (gliver contrib layout alternating)
  #:use-module (gliver core)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:declarative? #f
  #:export (
			layout-alternating?
			layout-alternating-make-config
			layout-alternating-get-param
			layout-alternating-add-container
			layout-alternating-update-container
			layout-alternating-reload
			layout-alternating-update
			layout-alternating-window-created
			layout-alternating-window-fullscreen-exited
			layout-alternating-window-destroyed
			layout-alternating-workspace-created
			layout-alternating-output-dimensions-changed
			))

(define (layout-alternating? workspace)
  "Return #t if the workspace layout is alternating."
  (and-let* ((layout-cfg (workspace-layout workspace))
			 (layout-name (if (list? layout-cfg) (assq-ref layout-cfg 'layout) layout-cfg)))
	(equal? layout-name 'alternating)))

(define* (layout-alternating-make-config #:key
										 (initial-split-direction 'horizontal)
										 (split-ratio 0.5)
										 (max-depth 5)
										 (alternate-direction? #t)
										 (inner-gap #f)
										 (outer-gap #f)
										 (append-method 'tail))
  "Create an associated list for the alternating layout configuration."
  `((layout . alternating)
	(layout-type . auto)
    (initial-split-direction . ,initial-split-direction)
    (split-ratio . ,split-ratio)
    (max-depth . ,max-depth)
    (alternate-direction? . ,alternate-direction?)
    (inner-gap . ,inner-gap)
    (outer-gap . ,outer-gap)
    (append-method . ,append-method)))

(define (layout-alternating-get-param layout-cfg key default-val)
  "Helper to safely extract a parameter from the layout configuration."
  (if (list? layout-cfg)
      (let ((pair (assq key layout-cfg)))
        (if pair (cdr pair) default-val))
      default-val))

(define (layout-alternating-add-container workspace)
  "Creates a new empty container and returns it."
  (let ((container (make-container #:workspace workspace
  								   #:x 0
  								   #:y 0
  								   #:width 0
  								   #:height 0)))
	(container-add! container)
	container))

(define (layout-alternating-update-container workspace index)
  "Modify the target container at the provided index so that it
respects the alternating layout system."
  (when (layout-alternating? workspace)
	(let* ((output (workspace-output workspace))
		   (ow (output-width output))
		   (oh (output-height output))
  		   (layout-cfg (workspace-layout workspace))
		   (containers (workspace-containers workspace))
  		   (containers-count (length containers))
		   (container (and (< index containers-count)
						   (list-ref containers index)))
		   (first-contaier? (= containers-count 1))
  		   (tail? (= (+ 1 index) containers-count))

		   ;; inner and outer gap configuration and values
		   (inner-gap (or (layout-alternating-get-param layout-cfg 'inner-gap #f)
						  (manager-config-ref 'container-inner-gap)))
		   (outer-gap (or (layout-alternating-get-param layout-cfg 'outer-gap #f)
						  (manager-config-ref 'container-outer-gap)))
		   (border-width (manager-config-ref 'border-width))
		   (outer-gap-include-border (layout-alternating-get-param layout-cfg 'outer-gap-include-border #t))
		   (inner-gap-include-border (layout-alternating-get-param layout-cfg 'inner-gap-include-border #t))
		   (total-outer-gap (if outer-gap-include-border (+ outer-gap border-width) outer-gap))
		   (total-inner-gap (if inner-gap-include-border (+ inner-gap border-width) inner-gap))

		   ;; get geometry of previous container or output for 1st container
		   (prev-container (and (> index 0)
								(< (- index 1) containers-count)
								(list-ref containers (- index 1))))
		   (prev-x (if prev-container
					   (container-x prev-container)
					   0))
		   (prev-y (if prev-container
					   (container-y prev-container)
					   0))
		   (prev-width (if prev-container
						   (container-width prev-container)
						   ow))
		   (prev-height (if prev-container
							(container-height prev-container)
							oh))
		   (avail-width (- ow (* 2 total-outer-gap)))
		   (avail-height (- oh (* 2 total-outer-gap)))

		   ;; get index direction split, vertical or horizontal split
		   (initial-dir (layout-alternating-get-param layout-cfg 'initial-split-direction 'vertical))
		   (other-dir (if (eq? initial-dir 'vertical) 'horizontal 'vertical))
		   (split (if (even? index) initial-dir other-dir))
		   (prev-split (if (even? index) other-dir initial-dir))
		   (split-ratio (if tail? 1 (layout-alternating-get-param layout-cfg 'split-ratio 0.5)))
		   (split-ratio-w (if (and (not tail?) (eq? split 'vertical)) 1 split-ratio))
		   (split-ratio-h (if (and (not tail?) (eq? split 'horizontal)) 1 split-ratio)))

	  (cond

	   ((not container)
		(log-debug "case-0: no container is available at the index ~a" index))

	   ((= 0 index)
		(let* ((gap-dt 0)
			   (gap-db (if (and (not tail?) (eq? split 'vertical)) 1 0))
			   (gap-dr (if (and (not tail?) (eq? split 'horizontal)) 1 0))
			   (gap-dl 0)
			   (curr-width  (- (* avail-width split-ratio-w)
							   (* (+ gap-dr gap-dl) total-inner-gap)))
			   (curr-height (- (* avail-height split-ratio-h)
							   (* (+ gap-dt gap-db) total-inner-gap)))
			   (curr-x total-outer-gap)
			   (curr-y total-outer-gap))
		  (container-size-set! container curr-width curr-height)
		  (container-position-set! container curr-x curr-y)))

	   (tail?
		;; tail container just need occupy the remaining space
		(let* ((curr-x (if (eq? prev-split 'vertical)
						   (container-x prev-container)
						   (+ (container-x prev-container)
							  (container-width prev-container)
							  (* 2 total-inner-gap))))
			   (curr-y (if (eq? prev-split 'vertical)
						   (+ (container-y prev-container)
							  (container-height prev-container)
							  (* 2 total-inner-gap))
						   (container-y prev-container)))
			   (curr-width (container-width prev-container))
			   (curr-height (container-height prev-container)))
		  (container-size-set! container curr-width curr-height)
		  (container-position-set! container curr-x curr-y)))

	   ;; case-3: the container has some containers after and before it
  	   (else
		(let* ((curr-x (if (eq? prev-split 'vertical)
						   (container-x prev-container)
						   (+ (container-x prev-container)
							  (container-width prev-container)
							  (* 2 total-inner-gap))))
			   (curr-y (if (eq? prev-split 'vertical)
						   (+ (container-y prev-container)
							  (container-height prev-container)
							  (* 2 total-inner-gap))
						   (container-y prev-container)))
			   (remaining-width (container-width prev-container))
			   (remaining-height (container-height prev-container))
			   (curr-width (if (eq? split 'horizontal)
							   (- (* remaining-width split-ratio) total-inner-gap)
							   (container-width prev-container)))
			   (curr-height (if (eq? split 'vertical)
								(- (* remaining-height split-ratio) total-inner-gap)
								(container-height prev-container))))
		  (container-size-set! container curr-width curr-height)
		  (container-position-set! container curr-x curr-y)))))))

(define* (layout-alternating-reload workspace #:key (complete #f))
  "Reload function that ensures current workspace adheres to the
layout rules."
  (log-debug "layout-alternating-reload on workspace ~a" workspace)
  (let* ((layout-cfg (workspace-layout workspace))
		 (max-depth (layout-alternating-get-param layout-cfg 'max-depth 99))
		 (windows (workspace-windows workspace))
		 (windows-count (length windows))
		 (max-containers (min max-depth windows-count)))

	;; if containers-count is less than the max-containers
	;; start creating new containers until we reach max-containers
	(let* ((containers (workspace-containers workspace))
		   (containers-count (length containers))
		   (pre-fix-container (- containers-count 1)))
	  ;; we will also update geometry if the complete argument is passed
	  (when (or complete (< containers-count max-containers))
		(do ((i containers-count (1+ i)))  ;; start with container i=containers-count
			((>= i max-containers))        ;; stop when i is last container
		  (layout-alternating-add-container workspace))

		;; now that containers are added, we should recalculate the geometry
		;; of all the new containers in addition to the container previous
		;; last container as it will also have its size updated
		(do ((i 0 (1+ i)))
			((>= i max-containers))
		  (layout-alternating-update-container workspace i))))

	;; since containers are altered, we have to get them again
	(let* ((containers (workspace-containers workspace))
		   (containers-count (length containers)))
	  ;; reorganize windows by moving each window to its matching container
	  (do ((i 0 (1+ i)))            ;; start with window i=0
		  ((>= i windows-count))    ;; stop when i is last window
		(let* ((window (list-ref windows i))
			   (current-container (window-container window))
			   (currently-focused? (or (container-focused? current-container)
									   (eq? window (window-current))))
			   (target-container (list-ref containers (min i (- containers-count 1)))))
		  (log-debug "current container to be removed is focused ~a" currently-focused?)
		  (window-move-to-container! window target-container #:focus currently-focused?)))

	  ;; remove any extra empty containers
	  ;; we shouldn't need to apply any focus logic as all deleted containers
	  ;; should be tail ones that have no windows at this point
	  (do ((i 0 (1+ i)))                 ;; start with container i=0
		  ((>= i containers-count))      ;; stop when i is last container
		(let ((container (list-ref containers i)))
		  ;; we will only delete containers that are empty when there
		  ;; are remaining containers in the workspace, at least
		  ;; one empty container must remain available
		  (when (and (null? (container-windows container))
					 (> (length (workspace-containers workspace)) 1))
			(container-remove! container)))))

	;; update last container's geometry
	(layout-alternating-update-container workspace (- (length (workspace-containers workspace)) 1))))

(define (layout-alternating-update hook workspace container window)
  "Main orchestrator for the alternating layout."
  (when (and hook workspace (layout-alternating? workspace))
	(let* ((layout-cfg (workspace-layout workspace))
		   (append-method (layout-alternating-get-param layout-cfg 'append-method 'tail))
		   (append-tail? (eq? append-method 'tail))
		   (containers (workspace-containers workspace)))
	  ;; if the append method is tail, move the window to the last container
	  (when append-tail?
		(window-move-to-container! window (last containers)))
	  (layout-alternating-reload workspace #:complete #t))))

(define (layout-alternating-window-created window)
  (let* ((container (window-container window))
		 (workspace (if container
						(container-workspace container)
						#f)))
	(layout-alternating-update 'window-created workspace container window)))

(define (layout-alternating-window-fullscreen-exited window)
  (let* ((container (window-container window))
		 (workspace (if container
						(container-workspace container)
						#f)))
	(layout-alternating-reload workspace #:complete #t)))

(define (layout-alternating-window-destroyed window container workspace)
  (layout-alternating-update 'window-destroyed workspace container window))

(define (layout-alternating-workspace-created workspace)
  (let* ((container (workspace-container-current workspace))
		 (window (if container
					 (container-window-current container)
					 #f)))
	(layout-alternating-update 'workspace-created workspace container window)))

(define (layout-alternating-output-dimensions-changed output prev-width prev-height)
  (let* ((workspace (output-workspace-current output))
		 (container (workspace-container-current workspace))
		 (window (container-window-current container)))
	(layout-alternating-update 'output-dimensions-changed workspace container window)))

(gliver-hook-add! *window-created-hook* 'layout-alternating-window-created)
(gliver-hook-add! *window-fullscreen-exited-hook* 'layout-alternating-window-fullscreen-exited)
(gliver-hook-add! *window-destroyed-hook* 'layout-alternating-window-destroyed)
(gliver-hook-add! *workspace-created-hook* 'layout-alternating-workspace-created)
(gliver-hook-add! *output-dimensions-changed-hook* 'layout-alternating-output-dimensions-changed)
