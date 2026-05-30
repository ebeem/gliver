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
  #:export (layout-alternating-make-config
            layout-laternating-update))

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

(define (gather-target-windows workspace hook-name window append-method)
  "Returns a flat, ordered list of all windows for this layout iteration."
  (let* ((current-containers (workspace-containers workspace))
         (all-windows-raw (append-map container-windows current-containers)))
    (if (and (eq? hook-name 'window-created)
             (eq? append-method 'tail)
             window
             (memq window all-windows-raw))
        (append (delq window all-windows-raw) (list window))
        all-windows-raw)))

(define (group-windows wins max-depth)
  "Groups windows into sub-lists based on max-depth. Deeper windows are stacked."
  (let ((num-wins (length wins))
        (limit (if max-depth (min (length wins) max-depth) (length wins))))
    (let loop ((i 0) (remaining-wins wins) (groups '()))
      (cond
       ((null? remaining-wins) (reverse groups))
       ((= i (- limit 1))      (reverse (cons remaining-wins groups)))
       (else                   (loop (+ i 1) 
                                     (cdr remaining-wins) 
                                     (cons (list (car remaining-wins)) groups)))))))

(define (sync-containers-to-groups! workspace window-groups)
  "Ensures the workspace has exactly the right number of containers, assigning window groups."
  (let* ((current-containers (workspace-containers workspace))
         (non-empty (filter (lambda (c) (not (null? (container-windows c)))) current-containers))
         (empty (filter (lambda (c) (null? (container-windows c))) current-containers))
         (sorted-containers (append non-empty empty))
         (required-count (length window-groups))
         (current-count (length sorted-containers))
         (discarded (if (> current-count required-count)
                        (drop sorted-containers required-count)
                        '()))
         (active (if (>= current-count required-count)
                     (take sorted-containers required-count)
                     (append sorted-containers
                             (map (lambda (_) (make-container #:workspace workspace))
                                  (iota (- required-count current-count)))))))
    (for-each container-remove! discarded)

    ;; bind windows to their designated containers
    (for-each (lambda (container group)
                (%container-windows-set! container group)
                (%container-window-current-set! container (car group))
                (for-each (lambda (win) (%window-container-set! win container)) group))
              active
              window-groups)

    ;; update the workspace state
    (%workspace-containers-set! workspace active)
    active))

(define (apply-geometry! containers workspace layout-cfg)
  "Calculates boundaries and applies X/Y/W/H to all active containers and their windows."
  (let* ((output (workspace-output workspace))
         (ox (if output (output-x output) 0))
         (oy (if output (output-y output) 0))
         (ow (if output (output-width output) 1920))
         (oh (if output (output-height output) 1080))
         (inner-gap (or (get-param layout-cfg 'inner-gap #f) 
                        (manager-config-ref 'container-inner-gap)))
         (outer-gap (or (get-param layout-cfg 'outer-gap #f) 
                        (manager-config-ref 'container-outer-gap)))
         (initial-dir (get-param layout-cfg 'initial-split-direction 'horizontal))
         (split-ratio (get-param layout-cfg 'split-ratio 0.5))
         (alternate-dir? (get-param layout-cfg 'alternate-direction? #t))
         (x-start (+ ox inner-gap outer-gap))
         (y-start (+ oy inner-gap outer-gap))
         (w-start (max 1 (- ow (* 2 inner-gap) (* 2 outer-gap))))
         (h-start (max 1 (- oh (* 2 inner-gap) (* 2 outer-gap)))))

    (let loop ((conts containers)
               (x x-start) (y y-start) (w w-start) (h h-start)
               (is-horizontal? (eq? initial-dir 'horizontal)))
      (unless (null? conts)
        (let ((c (car conts))
              (is-last? (null? (cdr conts))))

          (if is-last?
			  ;; base case: final container takes all remaining space
              (begin
                (%container-x-set! c x) (%container-y-set! c y)
                (%container-width-set! c w) (%container-height-set! c h)
                (let ((win (container-window-current c)))
                  (when win
					(window-show! win)
                    (window-position-set! win x y)
                    (window-dimensions-propose! win w h))
                  (for-each window-hide! (delq win (container-windows c)))))

              ;; recursive case: split space and continue
              (let* ((next-dir (if alternate-dir? (not is-horizontal?) is-horizontal?))
                     (split-w (if is-horizontal? (max 1 (inexact->exact (round (* (- w inner-gap) split-ratio)))) w))
                     (split-h (if is-horizontal? h (max 1 (inexact->exact (round (* (- h inner-gap) split-ratio)))))))
                
                (%container-x-set! c x) (%container-y-set! c y)
                (%container-width-set! c split-w) (%container-height-set! c split-h)
                
                (let ((win (container-window-current c)))
                  (when win
					(window-show! win)
                    (window-position-set! win x y)
                    (window-dimensions-propose! win split-w split-h))
                  (for-each window-hide! (delq win (container-windows c))))

                (if is-horizontal?
                    (loop (cdr conts) (+ x split-w inner-gap) y (max 1 (- w split-w inner-gap)) h next-dir)
                    (loop (cdr conts) x (+ y split-h inner-gap) w (max 1 (- h split-h inner-gap)) next-dir)))))))))

(define (layout-laternating-update hook-name workspace container window)
  "Main orchestrator for the alternating layout."
  (log-debug "layout-laternating-update ~a ~a ~a ~a" hook-name workspace container window)
  (when (and hook-name workspace)
	(let* ((layout-cfg (workspace-layout workspace))
           (layout-name (if (list? layout-cfg) (assq-ref layout-cfg 'layout) layout-cfg)))

      (when (eq? layout-name 'alternating)
		(log-debug (manager-print-tree))
		
		(let* ((append-method (get-param layout-cfg 'append-method 'tail))
		       (max-depth (get-param layout-cfg 'max-depth #f))
		       (all-windows (gather-target-windows workspace hook-name window append-method)))

		  (if (null? all-windows)
		      ;; fallback: ensure at least one empty container exists
		      (let ((first-container (if (null? (workspace-containers workspace))
		                                 (make-container #:workspace workspace)
		                                 (car (workspace-containers workspace)))))
		        (for-each container-remove! (cdr (workspace-containers workspace)))
		        (%container-windows-set! first-container '())
		        (%workspace-containers-set! workspace (list first-container)))

		      ;; standard flow
		      (let* ((window-groups (group-windows all-windows max-depth))
		             (active-containers (sync-containers-to-groups! workspace window-groups)))

		        ;; calculate and apply geometry
		        (apply-geometry! active-containers workspace layout-cfg))))

		(log-debug "finished layout")
		(log-debug (manager-print-tree))))
	#t))

(gliver-hook-add! *manager-layout-changed-hook* layout-laternating-update)
