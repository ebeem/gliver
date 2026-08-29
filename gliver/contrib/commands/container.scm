;;; gliver/contrib/commands/container.scm --- Container commands for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver contrib commands container)
  #:use-module (gliver core)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:declarative? #f
  #:export (
			container-split-horizontal
			container-split-vertical
			container-destroy
			container-destroy-others
			container-focus-direction
			container-focus-left
			container-focus-right
			container-focus-up
			container-focus-down
))

;;; helpers
(define (layout-type-manual?)
  "Return #t if the current workspace layout-type is manual."
  (and-let* ((workspace (workspace-current))
			 (layout-cfg (workspace-layout workspace))
			 ((list? layout-cfg))
			 (pair (assq 'layout-type layout-cfg)))
    (eq? (cdr pair) 'manual)))

(define (layout-inner-gap)
  "Return the effective inner gap (including border width) for the
current workspace."
  (let* ((workspace (workspace-current))
		 (layout-cfg (workspace-layout workspace))
		 (inner-gap (or (and (list? layout-cfg)
							(assq-ref layout-cfg 'inner-gap))
					    *container-inner-gap*))
		 (border-width *window-border-width*))
	(+ inner-gap border-width)))

;;; container split commands
(define-command (container-split-horizontal)
  "Split the current container horizontally (top/bottom).
Reduces the current container's height by half and creates a new
container below it.  Only works when layout-type is manual."
  (if (not (layout-type-manual?))
	  (log-debug "container-split-horizontal: layout-type is not manual")
	  (let ((container (container-current)))
		(when container
		  (let* ((workspace (container-workspace container))
				 (gap (layout-inner-gap))
				 (old-x (container-x container))
				 (old-y (container-y container))
				 (old-w (container-width container))
				 (old-h (container-height container))
				 ;; split: top half gets half the height minus half the gap,
				 ;; bottom half gets the remainder
				 (top-h (inexact->exact (floor (/ (- old-h (* 2 gap)) 2))))
				 (bot-h (- old-h top-h (* 2 gap)))
				 (bot-y (+ old-y top-h (* 2 gap)))
				 ;; create the new container in the same workspace
				 (new-container (make-container #:workspace workspace
												#:x old-x
												#:y bot-y
												#:width old-w
												#:height bot-h)))
			;; shrink the current container
			(container-size-set! container old-w top-h)
			;; register and focus the new container
			(container-add! new-container)
			;; position must be set after add since add may trigger hooks
			(container-position-set! new-container old-x bot-y)
			(container-size-set! new-container old-w bot-h)
			(container-focus! new-container)
			(gliver-hook-run! *container-split-hook* container new-container)
			(log-debug "Split horizontal."))))))

(define-command (container-split-vertical)
  "Split the current container vertically (left/right).
Reduces the current container's width by half and creates a new
container to the right.  Only works when layout-type is manual."
  (if (not (layout-type-manual?))
	  (log-debug "container-split-vertical: layout-type is not manual")
	  (let ((container (container-current)))
		(when container
		  (let* ((workspace (container-workspace container))
				 (gap (layout-inner-gap))
				 (old-x (container-x container))
				 (old-y (container-y container))
				 (old-w (container-width container))
				 (old-h (container-height container))
				 ;; split: left half gets half the width minus half the gap,
				 ;; right half gets the remainder
				 (left-w (inexact->exact (floor (/ (- old-w (* 2 gap)) 2))))
				 (right-w (- old-w left-w (* 2 gap)))
				 (right-x (+ old-x left-w (* 2 gap)))
				 ;; create the new container in the same workspace
				 (new-container (make-container #:workspace workspace
												#:x right-x
												#:y old-y
												#:width right-w
												#:height old-h)))
			;; shrink the current container
			(container-size-set! container left-w old-h)
			;; register and focus the new container
			(container-add! new-container)
			;; position must be set after add since add may trigger hooks
			(container-position-set! new-container right-x old-y)
			(container-size-set! new-container right-w old-h)
			(container-focus! new-container)
			(gliver-hook-run! *container-split-hook* container new-container)
			(log-debug "Split vertical."))))))

(define-command (container-destroy)
  "Remove the current container, moving its windows to a neighbour.
Only works when layout-type is manual and there is more than one
container."
  (if (not (layout-type-manual?))
	  (log-debug "container-destroy: layout-type is not manual")
	  (let* ((container (container-current))
			 (workspace (and container (container-workspace container)))
			 (containers (and workspace (workspace-containers workspace))))
		(cond
		 ((not container)
		  (log-debug "container-destroy: no current container"))
		 ((<= (length containers) 1)
		  (log-debug "container-destroy: cannot remove the last container"))
		 (else
		  (container-remove! container)
		  (log-debug "Container removed."))))))

(define-command (container-destroy-others)
  "Remove all containers except the current one, absorbing their
windows.  The remaining container is resized to fill the output.
Only works when layout-type is manual."
  (if (not (layout-type-manual?))
	  (log-debug "container-destroy-others: layout-type is not manual")
	  (let* ((container (container-current))
			 (workspace (and container (container-workspace container))))
		(when (and container workspace)
		  (let ((others (filter (lambda (c) (not (eq? c container)))
								(workspace-containers workspace))))
			(for-each (lambda (c) (container-remove! c #:target-container container))
					  others)
			;; resize the remaining container to fill the output
			(let* ((output (workspace-output workspace))
				   (layout-cfg (workspace-layout workspace))
				   (outer-gap (or (and (list? layout-cfg)
									  (assq-ref layout-cfg 'outer-gap))
								 *container-outer-gap*))
				   (border-width *window-border-width*)
				   (total-outer-gap (+ outer-gap border-width)))
			  (container-size-set! container
								   (- (output-width output)  (* 2 total-outer-gap))
								   (- (output-height output) (* 2 total-outer-gap)))
			  (container-position-set! container total-outer-gap total-outer-gap))
			(log-debug "Only one container remaining."))))))

;;; container focus commands

;; (define-command (container-focus-next)
;;   "Focus the next container."
;;   (let* ((workspace (workspace-current))
;;          (container (container-current)))
;;     (when (and workspace container)
;;       (let ((nf (container-next container)))
;;         (when nf
;;           (workspace-container-current-set! workspace nf)
;;           (log-debug "Container ~a" (container-id nf)))))))

;; (define-command (container-focus-prev)
;;   "Focus the previous container."
;;   (let* ((workspace (workspace-current))
;;          (container (container-current)))
;;     (when (and workspace container)
;;       (let ((pf (container-prev container)))
;;         (when pf
;;           (workspace-container-current-set! workspace pf)
;;           (log-debug "Container ~a" (container-id pf)))))))

(define-command (container-focus-direction dir)
  #:interactive (string)
  "Focus the container in direction DIR."
  (and-let* ((target (container-in-direction dir)))
    (if target
		(container-focus! target)
        (log-debug "Couldn't find focus target"))))

(define-command (container-focus-left)
  "Focus the container in the left direction."
  (container-focus-direction 'left))

(define-command (container-focus-right)
  "Focus the container in the right direction."
  (container-focus-direction 'right))

(define-command (container-focus-up)
  "Focus the container in the up direction."
  (container-focus-direction 'up))

(define-command (container-focus-down)
  "Focus the container in the down direction."
  (container-focus-direction 'down))

;; (define (clamp val lo hi)
;;   (max lo (min hi val)))

;; (define-command (container-resize dir amount)
;;   "Resize the current container's parent split."
;;   (let ((container (container-current)))
;;     (when container
;;       (let ((parent (container-parent container)))
;;         (when (and parent (container-split? parent))
;;           (let* ((ratio (container-split-ratio parent))
;;                  (step (or amount 0.05))
;;                  (new-ratio (clamp (case dir
;;                                     ((grow-right grow-down) (+ ratio step))
;;                                     ((shrink-left shrink-up) (- ratio step))
;;                                     (else ratio))
;;                                   0.1 0.9)))
;;             (container-split-ratio-set! parent new-ratio)
;;             (gliver-hook-run! *container-resize-hook* container)))))))

;; (define-command (container-balance)
;;   "Equalize all container split ratios."
;;   (let ((workspace (workspace-current)))
;;     (when workspace
;;       (container-balance! (workspace-containers workspace))
;;       (log-debug "Containers balanced."))))
