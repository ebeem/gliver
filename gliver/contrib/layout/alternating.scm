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
                                  (initial-split-direction 'horizontal)
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

		 ;; get index direction split, vertical or horizontal split
		 (split-ratio (if tail? 1 (get-param layout-cfg 'split-ratio 0.5)))
		 (initial-dir (get-param layout-cfg 'initial-split-direction 'vertical))
		 (other-dir (if (eq? initial-dir 'vertical) 'horizontal 'vertical))
		 (split (if (even? index) initial-dir other-dir))
		 (prev-split (if (even? index) other-dir initial-dir)))

	(cond

	 ((not container)
      (log-debug "case-0: no container is available at the index ~a" index))

	 ((= 0 index)
	  (let* ((avail-width (- ow (* 2 total-outer-gap)))
			 (avail-height (- oh (* 2 total-outer-gap)))
			 (curr-width (if (eq? split 'horizontal)
							 (* avail-width split-ratio)
							 avail-width))
			 (curr-height (if (eq? split 'vertical)
							  (* avail-height split-ratio)
							  avail-height))
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
			 (curr-width (if (eq? prev-split 'vertical)
							 (container-width prev-container)
							 (- ow curr-x total-outer-gap)))
			 (curr-height (if (eq? prev-split 'vertical)
							  (- oh curr-y total-outer-gap)
							  (container-height prev-container))))
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

			 (remaining-width (if (eq? prev-split 'vertical)
								  (container-width prev-container)
								  (- ow curr-x total-outer-gap)))
			 (remaining-height (if (eq? prev-split 'vertical)
								   (- oh curr-y total-outer-gap)
								   (container-height prev-container)))

			 (curr-width (if (eq? split 'horizontal)
							 (- (* remaining-width split-ratio) total-inner-gap)
							 remaining-width))
			 (curr-height (if (eq? split 'vertical)
							  (- (* remaining-height split-ratio) total-inner-gap)
							  remaining-height)))
		(container-size-set! container curr-width curr-height)
		(container-position-set! container curr-x curr-y))))))

(define* (layout-alternating-reload workspace #:key (complete #f))
  "Reload function that ensures current workspace adheres to the
layout rules."
  (log-debug "layout-alternating-reload on workspace ~a" workspace)
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
			   (target-container (list-ref containers (min i (- containers-count 1)))))
		  (window-move-to-container! window target-container)))

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

(define (layout-alternating-add-container workspace)
  "Creates a new empty container and returns it."
  (let ((container (make-container #:workspace workspace
  								   #:x 0
  								   #:y 0
  								   #:width 0
  								   #:height 0)))
	(container-add! container)
	container))

(define layout-alternating-hooks '("window-created"
                                   "window-destroyed"
                                   "workspace-created"
                                   "output-dimensions-changed"))

(define (layout-alternating-update hook workspace container window)
  "Main orchestrator for the alternating layout."
  (when (and hook workspace)
	(let* ((layout-cfg (workspace-layout workspace))
           (layout-name (if (list? layout-cfg) (assq-ref layout-cfg 'layout) layout-cfg)))
      (when (and (equal? layout-name 'alternating)
				 (member (symbol->string hook) layout-alternating-hooks))
		(layout-alternating-reload workspace #:complete #t)))))

(gliver-hook-add! *manager-layout-changed-hook* 'layout-alternating-update)

