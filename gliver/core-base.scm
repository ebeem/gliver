;;; gliver/core-base.scm --- Base core module re-export umbrella for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module aggregates and re-exports base core subsystems in Gliver.
;;; It handles:
;;;  - re-exporting config, logs, hooks, types, ffi, and helpers
;;;    modules into a unified (gliver core-base) interface

(define-module (gliver core-base)
  #:use-module (gliver core config)
  #:use-module (gliver core ffi)
  #:use-module (gliver core hooks)
  #:use-module (gliver core logs)
  #:use-module (gliver core types)
  #:use-module (gliver core helpers)
  #:declarative? #f)

(define-syntax re-export-modules
  (syntax-rules ()
    ((_ (mod ...) ...)
     (begin
       (module-use! (module-public-interface (current-module))
                    (resolve-interface '(mod ...)))
       ...))))

(re-export-modules (gliver core config)
                   (gliver core hooks)
                   (gliver core ffi)
                   (gliver core logs)
                   (gliver core types)
                   (gliver core helpers))

