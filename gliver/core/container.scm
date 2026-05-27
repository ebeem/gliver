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
  #:export (
			container-geometry-compute!
			container-next
			container-prev
			container-center-x
			container-center-y
			container-in-direction
			container-window-add!
			container-window-remove!
			container-add!
))

;;; container geometry
(define (container-geometry-compute! containers x y w h gap outer-gap)
  "Compute geometry for a flat list of CONTAINERS, distributing them horizontally."
  (let ((count (length containers)))
    (when (> count 0)
      (let ((cw (quotient w count)))
        (let loop ((rest containers) (cx x))
          (when (pair? rest)
            (let ((c (car rest)))
              (container-x-set! c (+ cx gap outer-gap))
              (container-y-set! c (+ y gap outer-gap))
              (container-width-set! c (max 1 (- cw (* 2 gap) (* 2 outer-gap))))
              (container-height-set! c (max 1 (- h (* 2 gap) (* 2 outer-gap))))
              (loop (cdr rest) (+ cx cw)))))))))

(define (container-next workspace current)
  "Return the next container after CURRENT in WORKSPACE's list."
  (let* ((containers (workspace-containers workspace))
         (idx (list-index (lambda (f) (eq? f current)) containers)))
    (if idx
        (list-ref containers (modulo (1+ idx) (length containers)))
        (and (pair? containers) (car containers)))))

(define (container-prev workspace current)
  "Return the previous container before CURRENT in WORKSPACE's list."
  (let* ((containers (workspace-containers workspace))
         (idx (list-index (lambda (f) (eq? f current)) containers)))
    (if idx
        (list-ref containers (modulo (+ idx (length containers) -1) (length containers)))
        (and (pair? containers) (car containers)))))

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

(define (container-window-add! container win)
  "Add WINDOW to the CONTAINER, the window shouldn't be added
to two different containers at the same time."
  (container-windows-set! container
                          (append (container-windows container) (list win))))

(define (container-window-remove! container win)
  "Remove WINDOW from the CONTAINER, the window should be destroyed
separately if that's the desired behavior."
  (container-windows-set! container
                          (delq win (container-windows container))))

(define (container-add! container)
  "Add a new window to the display, placing it in the current container."
  (let ((workspace (container-workspace container)))
	;; focus behavior
	(when *wm-behavior-focus-new-container*
	  (workspace-container-current-set! workspace container))

	;; the global manager will add the created window
	;; to global state automatically with the hook
	(log-debug "Running *container-created-hook*")
	(gliver-hook-run! *container-created-hook* container)))

