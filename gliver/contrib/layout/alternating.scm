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
			layout-laternating-create-container
			layout-laternating-update
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

(define (layout-laternating-create-container workspace index)
  "Create a new container at the provided index. This will also handle
fixing other containers if needed."
  (let* ((containers (workspace-containers workspace))
  		 (containers-count (length containers))
  		 (output (workspace-output workspace))
  		 (tail? (>= index containers-count))
  		 (layout-cfg (workspace-layout workspace))
		 (split-ratio (get-param layout-cfg 'split-ratio 0.5)))
	(log-info "checking case in create container")
	(log-info "containers=~a, tail?=~a, index=~a" containers tail? index)
	(log-info "cond-0, (null? containers)=~a" (null? containers))
	(log-info "cond-1, tail?=~a" tail?)
	(log-info "cond-2, else" tail?)
	(log-info "last container=~a" (last containers))

	(cond
	 ((null? containers)
      (log-info "case 0: no containers are available yet"))

  	 (tail?
   	   ;; case 1: the container to be created at the tail
  	   ;; a container should be created and last container size
  	   ;; should be adjusted
	  (let* ((container (last containers))
			 (width (container-width container))
			 (height (container-height container))
			 (x (container-x container))
			 (y (container-y container))
			 (split (if (>= width height) 'vertical 'horizontal))
			 ;; old container new size
			 (prev-split-ratio (- 1 split-ratio))
			 (prev-n-width (if (eq? split 'vertical) (* width prev-split-ratio) width))
			 (prev-n-height (if (eq? split 'vertical) height (* height prev-split-ratio)))
			 ;; new container position and size
			 (n-width (if (eq? split 'vertical) (* width split-ratio) width))
			 (n-height (if (eq? split 'vertical) height (* height split-ratio)))
			 (n-x (if (eq? split 'vertical) (+ x prev-n-width) y))
			 (n-y (if (eq? split 'vertical) x (+ y prev-n-height)))
			 (new-container (make-container #:workspace workspace
											 #:x n-x
											 #:y n-y
  											 #:width n-width
  											 #:height n-height)))
		 (log-info "case 1: the container to be created at the tail")
		 ;; the old container will also have its size adjusted
		 (container-size-set! container prev-n-width prev-n-height)
  		 (container-add! new-container)
		 )
  	   )

  	  (else
	   ;; case 2: the container to be created is at a given index
	   ;; a container should be created by copying the given index
	   ;; container position and size, then all the containers after
	   ;; should each copy the position and size of the container after
	   ;; the last two containers will have their size and position
	   ;; altered the same way as in case 2
  	   (let* ((tail? (>= index containers-count))
  			  (prev-container (list-ref containers index))
  			  (container
  			   (make-container #:workspace workspace
  							   #:width (container-width prev-container)
  							   #:height (container-height prev-container))))
		 (log-info "case 2: the container to be created is at a given index")
  		 (container-add! container))))))

(define (layout-laternating-update hook-name workspace container window)
  "Main orchestrator for the alternating layout."
  (log-info "layout-laternating-update ~a ~a ~a ~a" hook-name workspace container window)
  (log-info (manager-print-tree))
  (when (and hook-name workspace)
	(let* ((layout-cfg (workspace-layout workspace))
           (layout-name (if (list? layout-cfg) (assq-ref layout-cfg 'layout) layout-cfg)))

      (when (eq? layout-name 'alternating)
		(let* ((append-method (get-param layout-cfg 'append-method 'tail))
		       (max-depth (get-param layout-cfg 'max-depth #f))
			   (containers (workspace-containers workspace))
			   (containers-count (length containers))
			   (windows (workspace-windows workspace))
			   (windows-count (length windows))
			   (max-depth-reached? (and max-depth (< windows-count max-depth)))
			   (first-window? (and (= 1 windows-count) (= 1 containers-count)))
			   (focus-idx (list-index (lambda (x) (eq? x container)) containers))
			   (append-tail? (eq? append-method 'tail))
			   (target-idx (if (or append-tail?) containers-count focus-idx))
			   )

		  (when (eq? hook-name 'window-created)
			;; create a new tailing container if max-depth isn't reached yet
			;; and the target window isn't the first one to be added
			(unless (and max-depth-reached? first-window?)
			  (log-info "creating new container at ~a" target-idx)
			  (layout-laternating-create-container workspace target-idx))

			;; if append method is tail then place the new window in last container
			(when append-tail?
			  (log-info "appending to tail")
			  (let ((ncontainer (last (workspace-containers workspace))))
				(log-info "window-move-to-container! ~a ~a" window ncontainer)				
				(window-move-to-container! window ncontainer)))

			;; if append method is current, then place the new window in current container
			;; also shift all windows inside each container after current to the next one

			
			;; (layout-laternating-on-window-create workspace container window append-method)
			(log-info (manager-print-tree)))

		  )))
	#t))

(gliver-hook-add! *manager-layout-changed-hook* layout-laternating-update)

