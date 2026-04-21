;;; gliver/core.scm --- Core data model for Gliver
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
  #:use-module (gliver core manager)
  #:use-module (gliver core workspace)
  #:use-module (gliver core logs)
  #:use-module (gliver core hooks)
  #:use-module (gliver keybindings)
  #:export (
            ;; container
            <container>
            make-container
            container?
            container-windows container-windows-set!
            container-window-current container-window-current-set!
            container-window-previous container-window-previous-set!
            container-number container-number-set!
            container-x container-x-set!
            container-y container-y-set!
            container-width container-width-set!
            container-height container-height-set!

            container-current
            container-geometry-compute!
            container-find-by-number
            container-next
            container-prev
            container-in-direction
            container-center-x
            container-center-y
            container-number-next!
            container-reset-numbers!

			container-workspace-set!
			container-workspace
			%make-container))

;;; container: similar to an emacs window and stumpwm container
;;; a physical container or "slot" on the screen where a list of windows is displayed
(define-record-type <container>
  (%make-container workspace windows window-current window-previous number
                   x y width height)
  container?
  (workspace       container-workspace       container-workspace-set!)
  (windows         container-windows         container-windows-set!)
  (window-current  container-window-current  container-window-current-set!)
  (window-previous container-window-previous container-window-previous-set!)
  (number          container-number          container-number-set!)
  (x               container-x               container-x-set!)
  (y               container-y               container-y-set!)
  (width           container-width           container-width-set!)
  (height          container-height          container-height-set!))

(set-record-type-printer! <container>
  (lambda (f port)
    (format port "#<container ~a ~ax~a+~a+~a (~a wins)>"
            (container-number f)
            (container-width f) (container-height f)
            (container-x f) (container-y f)
            (length (container-windows f)))))

(define* (make-container #:key (workspace (workspace-current)) (x 0) (y 0) (width 0) (height 0))
  "Create a new flat container."
  (%make-container workspace '() #f #f (container-number-next!)
                   x y width height))

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

(define (container-find-by-number n workspace)
  "Find container with number N in WORKSPACE."
  (find (lambda (f) (= (container-number f) n))
        (workspace-containers workspace)))

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
         (candidates
          (filter
           (lambda (f)
             (case dir
               ((left)  (< (container-center-x f) cx))
               ((right) (> (container-center-x f) cx))
               ((up)    (< (container-center-y f) cy))
               ((down)  (> (container-center-y f) cy))
               (else #f)))
           containers)))
    (and (pair? candidates)
         (car (sort candidates
                    (lambda (a b)
                      (let ((da (+ (abs (- (container-center-x a) cx))
                                   (abs (- (container-center-y a) cy))))
                            (db (+ (abs (- (container-center-x b) cx))
                                   (abs (- (container-center-y b) cy)))))
                        (< da db))))))))

(define (container-number-next!)
  (let ((n (manager-container-number-next *manager*)))
    (manager-container-number-next-set! *manager* (1+ n))
    n))

(define (container-reset-numbers!)
  (manager-container-number-next-set! *manager* 0))

(define (container-current)
  "Return the current focused container based on the active workspace."
  (let ((g (workspace-current)))
    (and g (workspace-container-current g))))

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
