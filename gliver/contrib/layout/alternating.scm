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
  #:use-module (gliver core types)
  #:use-module (gliver core container)
  #:use-module (gliver core window)
  #:use-module (gliver core logs)
  #:use-module (gliver core hooks)
  #:use-module (srfi srfi-1)
  #:declarative? #f
  #:export (
			layout-alternating-make-config
			get-param
			layout-alternating-update
))

(define* (layout-alternating-make-config #:key
                                  (initial-split-direction 'vertical)
                                  (split-ratio 0.5)
                                  (max-depth 5)
                                  (alternate-direction? #t)
                                  (inner-gap #f)
                                  (outer-gap #f)
                                  (append-method 'tail))
  "Create an associated list for the alternating layout configuration."
  `((layout . alternating)
    (initial-split-direction . ,initial-split-direction)
    (split-ratio . ,split-ratio)
    (max-depth . ,max-depth)
    (alternate-direction? . ,alternate-direction?)
    (inner-gap . ,inner-gap)
    (outer-gap . ,outer-gap)
    (append-method . ,append-method)))

(define (get-param layout-cfg key default-val)
  "Helper to safely extract a parameter from the layout configuration."
  (if (list? layout-cfg)
      (let ((pair (assq key layout-cfg)))
        (if pair (cdr pair) default-val))
      default-val))

(define (layout-alternating-update-container workspace index)
  "Modify the target container at the provided index so that it
respects the alternating layout system."
  (log-info "layout-alternating-update-container ~a ~a" index (workspace-containers workspace))
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
		 (inner-gap (or (get-param layout-cfg 'inner-gap #f) 
						(manager-config-ref 'container-inner-gap)))
		 (outer-gap (or (get-param layout-cfg 'outer-gap #f) 
						(manager-config-ref 'container-outer-gap)))
		 (border-width (manager-config-ref 'border-width))
		 (outer-gap-include-border (get-param layout-cfg 'outer-gap-include-border #t))
		 (inner-gap-include-border (get-param layout-cfg 'inner-gap-include-border #t))
		 (total-outer-gap (if outer-gap-include-border (+ outer-gap border-width) outer-gap))
		 (total-inner-gap (if inner-gap-include-border (+ inner-gap border-width) inner-gap))

		 ;; get geometry of previous container or output for 1st container
		 (prev-container (and (> index 0)
							  (list-ref containers (- index 1))))
		 (prev-x (if prev-container
					 (container-x prev-container)
					 total-outer-gap))
		 (prev-y (if prev-container
					 (container-y prev-container)
					 total-outer-gap))
		 (prev-width (if prev-container
						 (container-width prev-container)
						 (- ow (* 2 prev-x))))
		 (prev-height (if prev-container
						  (container-height prev-container)
						  (- oh (* 2 prev-y))))

		 ;; get index direction split, vertical or horizontal split
		 (split-ratio (if tail? 1 (get-param layout-cfg 'split-ratio 0.5)))
		 (initial-dir (get-param layout-cfg 'initial-split-direction 'vertical))
		 (other-dir (if (eq? initial-dir 'vertical) 'horizontal 'vertical))
		 (split (if (even? index) initial-dir other-dir)))

	(cond

	 ((not container)
      (log-info "case-0: no container is available at the index ~a" index))

	 ((= 0 index)
	  ;; first container should just occupy the space of the output)
	  (let* ((curr-width (if (eq? split 'vertical)
							 (- (* prev-width split-ratio) total-inner-gap)
							 prev-width))
			 (curr-height (if (eq? split 'vertical)
							  prev-height
							  (- (* prev-height split-ratio) total-inner-gap)))
			 (curr-x prev-x)
			 (curr-y prev-y))
		(log-info "case-1: first container geometry to be returned ~ax~a@(~a+~a)" curr-width curr-height curr-x curr-y)
		(container-size-set! container curr-width curr-height)
		(container-position-set! container curr-x curr-y)))

	 (tail?
	  ;; tail windows just need occupy the remaining space
	  (let* ((curr-width prev-width)
			 (curr-height prev-height)
			 (curr-x (if (eq? split 'vertical)
						 prev-x
						 (+ prev-x prev-width (* 2 total-inner-gap))))
			 (curr-y (if (eq? split 'vertical)
						 (+ prev-y prev-height (* 2 total-inner-gap))
						 prev-y)))
		(log-info "case-2: last container geometry to be returned ~ax~a@(~a+~a)" curr-width curr-height curr-x curr-y)
		(container-size-set! container curr-width curr-height)
		(container-position-set! container curr-x curr-y)))

	 ;; case-3: the container has some containers after it
  	 (else
	  (let* ((curr-width (if (eq? split 'vertical)
							 (- (* prev-width split-ratio) total-inner-gap)
							 prev-width))
			 (curr-height (if (eq? split 'vertical)
							  prev-height
							  (- (* prev-height split-ratio) total-inner-gap)))
			 (curr-x (if (eq? split 'vertical)
						 prev-x
						 (+ prev-x prev-width (* 2 total-inner-gap))))
			 (curr-y (if (eq? split 'vertical)
						 (+ prev-y prev-height (* 2 total-inner-gap))
						 prev-y)))
		(log-info "case-3: middle container geometry to be returned ~ax~a@(~a+~a)" curr-width curr-height curr-x curr-y)
		(container-size-set! container curr-width curr-height)
		(container-position-set! container curr-x curr-y))))))

(define (layout-alternating-add-container workspace)
  "Creates a new empty container and returns it."
  (let ((container (make-container #:workspace workspace
  								   #:x 0
  								   #:y 0
  								   #:width 0
  								   #:height 0)))
	(container-add! container)
	container))

(define* (layout-alternating-reload workspace #:key (complete #f))
  "Reload function that ensures current workspace adheres to the
layout rules."
  (log-info "layout-alternating-reload")
  (let* ((layout-cfg (workspace-layout workspace))
		 (max-depth (get-param layout-cfg 'max-depth 99))
		 (windows (workspace-windows workspace))
		 (windows-count (length windows))
		 (max-containers (min max-depth windows-count)))

	;; if containers-count is less than the max-containers
	;; start creating new containers until we reach max-containers
	(let* ((containers (workspace-containers workspace))
		   (containers-count (length containers))
		   (pre-fix-container (- containers-count 1)))
	  (log-info "check-1: creating missing containers ~a ~a" containers-count max-containers)
	  ;; we will also update geometry if the complete argument is passed
	  (when (or complete (< containers-count max-containers))
		(do ((i containers-count (1+ i)))  ;; start with container i=containers-count
			((>= i max-containers))        ;; stop when i is last container
		  (log-info "creating container ~a" i)
		  (layout-alternating-add-container workspace))

		;; now that containers are added, we should recalculate the geometry
		;; of all the new containers in addition to the container previous
		;; last container as it will also have its size updated
		(do ((i (max (- containers-count 1) 0) (1+ i)))  ;; start with previous last container if available
			((>= i max-containers))        ;; stop when i is last container
		  (log-info "updating geometry of container ~a" i)
		  (layout-alternating-update-container workspace i))))

	;; since containers are altered, we have to get them again
	(let* ((containers (workspace-containers workspace))
		   (containers-count (length containers)))
	  (log-info "check-2: reorganize windows ~a into containers ~a" windows-count containers-count)
	  ;; reorganize windows by moving each window to its matching container
	  (do ((i 0 (1+ i)))            ;; start with window i=0
		  ((>= i windows-count))    ;; stop when i is last window
		(let* ((window (list-ref windows i))
			   (current-container (window-container window))
			   (target-container (list-ref containers (min i (- containers-count 1)))))
		  (log-info "moving window ~a from container ~a to container ~a"
					window current-container current-container)
		  (window-move-to-container! window target-container)))

	  ;; remove any extra empty containers
	  ;; we shouldn't need to apply any focus logic as all deleted containers
	  ;; should be tail ones that have no windows at this point
	  (log-info "check-3: removing empty containers ~a" containers-count)
	  (do ((i 0 (1+ i)))                 ;; start with container i=0
		  ((>= i containers-count))      ;; stop when i is last container
		(let ((container (list-ref containers i)))
		  ;; we will only delete containers that are empty when there
		  ;; are remaining containers in the workspace, at least
		  ;; one empty container must remain available
		  (when (and (null? (container-windows container))
					 (> (length (workspace-containers workspace)) 1))
			(log-info "removing extra containers, current are ~a containers, ~a windows" containers-count windows-count)
			(container-remove! container)))))

	  ;; update last container's geometry
	  (layout-alternating-update-container workspace (- (length (workspace-containers workspace)) 1))

	(log-info "done")))

(define (layout-alternating-update hook-name workspace container window)
  "Main orchestrator for the alternating layout."
  (log-info "layout-alternating-update ~a ~a ~a ~a" hook-name workspace container window)
  (when (and hook-name workspace)
	(let* ((layout-cfg (workspace-layout workspace))
           (layout-name (if (list? layout-cfg) (assq-ref layout-cfg 'layout) layout-cfg)))

      (when (equal? layout-name 'alternating)
		(let* ((append-method (get-param layout-cfg 'append-method 'tail))
		       (max-depth (get-param layout-cfg 'max-depth #f))
			   (containers (workspace-containers workspace))
			   (containers-count (length containers))
			   (windows (workspace-windows workspace))
			   (windows-count (length windows))
			   (max-depth-reached? (and max-depth (>= windows-count max-depth)))
			   (first-window? (and (= 1 windows-count) (= 1 containers-count)))
			   (focus-idx (list-index (lambda (x) (eq? x container)) containers))
			   (append-tail? (eq? append-method 'tail))
			   (target-idx (if (or append-tail?) containers-count focus-idx)))

		  (when (or (equal? hook-name 'workspace-created)
					(equal? hook-name 'output-dimensions-changed))
			(log-info "workspace created, reload layout ~a" workspace)
			(layout-alternating-reload workspace #:complete #t))

		  (when (equal? hook-name 'window-created)
			;; create a new container if needed
			(when (and (not max-depth-reached?)
					   (> windows-count containers-count))
			  (layout-alternating-add-container workspace)

			  ;; update previous container before updating the new one
			  (layout-alternating-update-container workspace (- containers-count 1))
			  (layout-alternating-update-container workspace containers-count))

			;; if append method is tail then place the new window in last container
			(when append-tail?
			  (log-info "appending to tail")
			  (let ((ncontainer (last (workspace-containers workspace))))
				(log-info "window-move-to-container! ~a ~a" window ncontainer)
				(unless (eq? ncontainer (window-container window))
				  (window-move-to-container! window ncontainer))))

			;; TODO: if append method is current, then place the new window in current container
			;; also shift all windows inside each container after current to the next one
			)

		  (when (equal? hook-name 'window-destroyed)
			;; remove the window container
			(log-info "window removed")
			(layout-alternating-reload workspace))
		  )))
	#t))

(gliver-hook-add! *manager-layout-changed-hook* layout-alternating-update)

