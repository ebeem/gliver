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
			layout-laternating-update
))

(define* (layout-alternating-make-config #:key
                                  (initial-split-direction 'horizontal)
                                  (split-ratio 0.5)
                                  (max-depth 5)
                                  (alternate-direction? #t)
                                  (inner-gap #f)
                                  (outer-gap #f))
  "Create an associated list for the alternating layout configuration."
  `((layout . alternating)
    (initial-split-direction . ,initial-split-direction)
    (split-ratio . ,split-ratio)
    (max-depth . ,max-depth)
    (alternate-direction? . ,alternate-direction?)
    (inner-gap . ,inner-gap)
    (outer-gap . ,outer-gap)))

(define (layout-laternating-update hook-name workspace container window)
  (let* ((layout-cfg (workspace-layout workspace))
         (layout-name (if (list? layout-cfg)
                          (assq-ref layout-cfg 'layout)
                          layout-cfg)))
    (when (eq? layout-name 'alternating)
      (log-debug (manager-print-tree))
      (let* ((output (workspace-output workspace))
             (ox (if output (output-x output) 0))
             (oy (if output (output-y output) 0))
             (ow (if output (output-width output) 1920))
             (oh (if output (output-height output) 1080))

             ;; helper to get param from alist or use default
             (get-param (lambda (key default-val)
                          (if (list? layout-cfg)
                              (let ((pair (assq key layout-cfg)))
                                (if pair (cdr pair) default-val))
                              default-val)))

             (inner-gap-override (get-param 'inner-gap #f))
             (inner-gap (or inner-gap-override (manager-config-ref 'container-inner-gap)))
             (outer-gap-override (get-param 'outer-gap #f))
             (outer-gap (or outer-gap-override (manager-config-ref 'container-outer-gap)))
             (initial-dir (get-param 'initial-split-direction 'horizontal))
             (split-ratio (get-param 'split-ratio 0.5))
             (max-depth (get-param 'max-depth #f))
             (alternate-dir? (get-param 'alternate-direction? #t))

             ;; get all visible windows in the workspace
             (current-containers (workspace-containers workspace))
             (all-windows (filter window-visible?
                                  (append-map container-windows current-containers)))
             (num-wins (length all-windows)))

        (if (= num-wins 0)
            ;; if there are no windows, ensure we have exactly one empty container
            (let ((first-container (if (null? current-containers)
                                       (make-container #:workspace workspace)
                                       (car current-containers))))
              (container-windows-set! first-container '())
              (workspace-containers-set! workspace (list first-container)))

            ;; if there are windows, each should have exactly one parent container
			;; if max depth is reached, all deeper windows will be stacked in one container
            (let* ((M (if max-depth
                          (min num-wins max-depth)
                          num-wins))
                   (window-groups
                    (let loop ((i 0) (wins all-windows) (groups '()))
                      (cond
                       ((null? wins) (reverse groups))
                       ((= i (- M 1))
                        (reverse (cons wins groups)))
                       (else
                        (loop (+ i 1) (cdr wins) (cons (list (car wins)) groups))))))
                   (active-containers
                    (let ((len (length current-containers)))
                      (if (>= len M)
                          (take current-containers M)
                          (append current-containers
                                  (map (lambda (_) (make-container #:workspace workspace))
                                       (iota (- M len))))))))

              ;; assign window groups to containers and update back-references
              (for-each (lambda (c group)
                          (container-windows-set! c group)
                          (container-window-current-set! c (car group))
                          (for-each (lambda (win)
                                      (window-container-set! win c))
                                    group))
                        active-containers
                        window-groups)

              ;; update workspace container list
              (workspace-containers-set! workspace active-containers)

              ;; compute geometries and position windows
              (let* ((x-start (+ ox inner-gap outer-gap))
                     (y-start (+ oy inner-gap outer-gap))
                     (w-start (max 1 (- ow (* 2 inner-gap) (* 2 outer-gap))))
                     (h-start (max 1 (- oh (* 2 inner-gap) (* 2 outer-gap)))))
                (let loop ((conts active-containers)
                           (x x-start) (y y-start) (w w-start) (h h-start)
                           (is-horizontal? (eq? initial-dir 'horizontal)))
                  (cond
                   ((null? conts) #t)
                   ((null? (cdr conts))
                    (let ((c (car conts)))
                      (container-x-set! c x)
                      (container-y-set! c y)
                      (container-width-set! c w)
                      (container-height-set! c h)
                      (for-each (lambda (win)
                                  (window-position-set! win x y)
                                  (window-size-set! win w h))
                                (container-windows c))))
                   (else
                    (let ((c (car conts))
                          (next-dir (if alternate-dir? (not is-horizontal?) is-horizontal?)))
                      (if is-horizontal?
                          (let ((split-w (max 1 (inexact->exact (round (* (- w inner-gap) split-ratio))))))
                            (container-x-set! c x)
                            (container-y-set! c y)
                            (container-width-set! c split-w)
                            (container-height-set! c h)
                            (for-each (lambda (win)
                                        (window-position-set! win x y)
                                        (window-size-set! win split-w h))
                                      (container-windows c))
                            (loop (cdr conts)
                                  (+ x split-w inner-gap)
                                  y
                                  (max 1 (- w split-w inner-gap))
                                  h
                                  next-dir))
                          (let ((split-h (max 1 (inexact->exact (round (* (- h inner-gap) split-ratio))))))
                            (container-x-set! c x)
                            (container-y-set! c y)
                            (container-width-set! c w)
                            (container-height-set! c split-h)
                            (for-each (lambda (win)
                                        (window-position-set! win x y)
                                        (window-size-set! win w split-h))
                                      (container-windows c))
                            (loop (cdr conts)
                                  x
                                  (+ y split-h inner-gap)
                                  w
                                  (max 1 (- h split-h inner-gap))
                                  next-dir)))))))))))))
  #t)

(gliver-hook-add! *manager-layout-changed-hook* layout-laternating-update)
