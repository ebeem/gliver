;;; gliver/contrib/layout/alternating.scm --- Alternating layout for gliver
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

(define-module (gliver contrib layout alternating)
  #:use-module (gliver core)
  #:use-module (gliver commands)
  #:declarative? #f
  #:export ())

(define (on-layout-change hook-name workspace container window)
  "Hook handler: handle layout change"
  (log-debug "Alternating Layout changed"))

(gliver-hook-add! *manager-layout-changed-hook* on-layout-change)

