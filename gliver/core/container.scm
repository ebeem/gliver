;;; gliver/core/container.scm --- Core data model for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver core container)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (system foreign)
  #:use-module (gliver core types)
  #:use-module (gliver core logs)
  #:use-module (gliver core config)
  #:use-module (gliver core hooks)
  #:autoload (gliver core window) (window-focus!)
  #:export (
			container-next
			container-prev
			container-center-x
			container-center-y
			container-in-direction
			container-add!
			container-remove!
			container-focus!
))

(define* (container-next current #:key (recursive #t))
  "Return the next container after CURRENT container."
  (let* ((workspace (container-workspace current))
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
  (let* ((workspace (container-workspace current))
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

(define (container-in-direction dir current workspace)
  "Find the closest container in direction DIR from CURRENT ('left, 'right, 'up, 'down)."
  (let* ((containers (filter (lambda (f) (not (eq? f current)))
                             (workspace-containers workspace)))
         (cx (container-center-x current))
         (cy (container-center-y current))
         (curr-x (container-x current))
         (curr-y (container-y current))
         (curr-w (container-width current))
         (curr-h (container-height current))
         (curr-x-max (+ curr-x curr-w))
         (curr-y-max (+ curr-y curr-h))
         (candidates
          (filter
           (lambda (f)
             (let* ((tx (container-x f))
                    (ty (container-y f))
                    (tw (container-width f))
                    (th (container-height f))
                    (tx-max (+ tx tw))
                    (ty-max (+ ty th)))
               (case dir
                 ((up)    (< ty-max curr-y))
                 ((down)  (> ty curr-y-max))
                 ((right) (> tx curr-x-max))
                 ((left)  (< tx-max curr-x))
                 (else #f))))
           containers)))
    (if (pair? candidates)
        (car (sort candidates
                   (lambda (a b)
                     (let* ((dx-a (- (container-center-x a) cx))
                            (dy-a (- (container-center-y a) cy))
                            (dist-a (+ (* dx-a dx-a) (* dy-a dy-a)))
                            (dx-b (- (container-center-x b) cx))
                            (dy-b (- (container-center-y b) cy))
                            (dist-b (+ (* dx-b dx-b) (* dy-b dy-b))))
                       (< dist-a dist-b)))))
        #f)))

(define (container-add! container)
  "Add a new window to the display, placing it in the current container."
  (log-debug "adding container ~a" container)
  ;; TODO: each container must at least have one window/node in `container-windows`
  ;; if it doesn't have any windows, one will be created and focused automatically
  ;; this is more of a node/placeholder
  (let ((workspace (container-workspace container)))

	;; add the container to the back referenced workspace containers
	(%workspace-containers-set! workspace
	 (append (workspace-containers workspace) (list container)))

	;; focus the output if no output is currently focused
	;; or if configuration is set to focus new output
	(when (or *wm-behavior-focus-new-container*
			  (not (workspace-container-current workspace)))
	  (container-focus! container))

	(gliver-hook-run! *container-created-hook* container)))

(define (container-remove! container)
  "Remove CONTAINER from its workspace."
  (let* ((workspace (container-workspace container))
		 (container-target (or (container-next container #:recursive #f)
							   (container-prev container #:recursive #f))))
    (when workspace
      ;; remove container from workspace's container list
      (%workspace-containers-set! workspace
                                 (delete container (workspace-containers workspace)))
      ;; focus a new container if the current focused container will be removed
      (when (eq? (workspace-container-current workspace) container)
        (container-focus! container-target))
      (log-debug "Running *container-destroy-hook*")
      (gliver-hook-run! *container-destroy-hook* container workspace))))

(define (container-focus! container)
  "Focus a container by focusing its last focused window."
  ;; target window is current focused or first window
  (let* ((windows (container-windows container))
		 (window (and (not (null? windows))
					  (or (container-window-current container)
						  (car windows))))
		 (workspace (container-workspace container))
		 (prev-container (workspace-container-current workspace)))
	(%workspace-container-previous-set! workspace prev-container)
	(%workspace-container-current-set! workspace container)
	(when window
	  (window-focus! window))))

