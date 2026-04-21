;;; gliver/contrib/gaps.scm --- Configurable gaps between containers
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module provides fine-grained control over gaps (spacing)
;;; between tiled windows and screen edges.
;;;
;;; Usage:
;;;   (use-modules (gliver contrib gaps))
;;;   (gap-inner-set! 8)
;;;   (gap-outer-set! 12)
;;;   (gaps-toggle!)

(define-module (gliver contrib gaps)
  #:use-module (gliver core logs)
  #:use-module (gliver core manager)
  #:use-module (gliver commands)
  #:declarative? #f
  #:export (gap-inner-set!
            gap-outer-set!
            gaps-set!
            gaps-toggle!
            gaps-increase!
            gaps-decrease!
            *gap-inner*
            *gap-outer*
            *gaps-enabled*))

(define *gap-inner* 4)
(define *gap-outer* 8)
(define *gaps-enabled* #t)
(define *saved-inner* 0)
(define *saved-outer* 0)

(define (gap-inner-set! gap)
  "Set the gap between containers (in pixels)."
  (set! *gap-inner* gap)
  (apply-gaps!))

(define (gap-outer-set! gap)
  "Set the gap between containers and screen edges (in pixels)."
  (set! *gap-outer* gap)
  (apply-gaps!))

(define (gaps-set! inner outer)
  "Set both inner and outer gaps."
  (set! *gap-inner* inner)
  (set! *gap-outer* outer)
  (apply-gaps!))

(define (gaps-toggle!)
  "Toggle gaps on/off."
  (if *gaps-enabled*
      (begin
        (set! *saved-inner* *gap-inner*)
        (set! *saved-outer* *gap-outer*)
        (set! *gap-inner* 0)
        (set! *gap-outer* 0)
        (set! *gaps-enabled* #f)
        (apply-gaps!)
        (message "Gaps disabled."))
      (begin
        (set! *gap-inner* *saved-inner*)
        (set! *gap-outer* *saved-outer*)
        (set! *gaps-enabled* #t)
        (apply-gaps!)
        (message "Gaps enabled: inner=~a outer=~a" *gap-inner* *gap-outer*))))

(define (gaps-increase! amount)
  "Increase both gaps by AMOUNT pixels."
  (set! *gap-inner* (+ *gap-inner* amount))
  (set! *gap-outer* (+ *gap-outer* amount))
  (apply-gaps!)
  (message "Gaps: inner=~a outer=~a" *gap-inner* *gap-outer*))

(define (gaps-decrease! amount)
  "Decrease both gaps by AMOUNT pixels (minimum 0)."
  (set! *gap-inner* (max 0 (- *gap-inner* amount)))
  (set! *gap-outer* (max 0 (- *gap-outer* amount)))
  (apply-gaps!)
  (message "Gaps: inner=~a outer=~a" *gap-inner* *gap-outer*))

(define (apply-gaps!)
  "Apply the current gap settings to the display."
  (manager-container-gap-set! *manager* *gap-inner*)
  (manager-container-outer-gap-set! *manager* *gap-outer*))

;;; register commands
(command-register! 'toggle-gaps gaps-toggle! "Toggle gaps on/off.")
(command-register! 'increase-gaps
  (lambda () (gaps-increase! 2))
  "Increase gaps by 2px.")
(command-register! 'decrease-gaps
  (lambda () (gaps-decrease! 2))
  "Decrease gaps by 2px.")
