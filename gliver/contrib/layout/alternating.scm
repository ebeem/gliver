;;; gliver/contrib/layout/alternating.scm --- Alternating layout for gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module provides fine-grained control over gaps (spacing)
;;; between tiled windows and screen edges.

(define-module (gliver contrib layout alternating)
  #:use-module (gliver core)
  #:use-module (gliver core types)
  #:use-module (gliver core container)
  #:use-module (gliver core window)
  #:use-module (gliver core logs)
  #:use-module (gliver core hooks)
  #:declarative? #f
  #:export ())

(define (layout-update hook-name workspace container window)
  (log-debug (manager-print-tree))
  (let* ((output (workspace-output workspace))
         (ox (if output (output-x output) 0))
         (oy (if output (output-y output) 0))
         (ow (if output (output-width output) 1920))
         (oh (if output (output-height output) 1080))
         (gap (manager-config-ref 'container-gap))
         (outer-gap (manager-config-ref 'container-outer-gap))
         (containers (workspace-containers workspace)))
    (container-geometry-compute! containers ox oy ow oh gap outer-gap)
    (for-each
     (lambda (c)
       (let ((windows (filter window-visible? (container-windows c)))
             (cx (container-x c))
             (cy (container-y c))
             (cw (container-width c))
             (ch (container-height c)))
         (let loop ((wins windows)
                    (x cx) (y cy) (w cw) (h ch)
                    (is-horizontal? #t))
           (cond
            ((null? wins) #t)
            ((null? (cdr wins))
             (let ((win (car wins)))
               (window-position-set! win x y)
               (window-size-set! win w h)))
            (else
             (let ((win (car wins)))
               (if is-horizontal?
                   (let ((half-w (quotient (- w gap) 2)))
                     (window-position-set! win x y)
                     (window-size-set! win half-w h)
                     (loop (cdr wins)
                           (+ x half-w gap)
                           y
                           (- w half-w gap)
                           h
                           #f))
                   (let ((half-h (quotient (- h gap) 2)))
                     (window-position-set! win x y)
                     (window-size-set! win w half-h)
                     (loop (cdr wins)
                           x
                           (+ y half-h gap)
                           w
                           (- h half-h gap)
                           #t)))))))))
     containers)))

(gliver-hook-add! *manager-layout-changed-hook* layout-update)

