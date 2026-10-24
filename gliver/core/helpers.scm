;;; gliver/core/helpers.scm --- Helper utilities for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module provides utilities and helper functions that common in Gliver

(define-module (gliver core helpers)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (system foreign)
  #:declarative? #f
  #:export (
			valid-pointer?
))

(define (valid-pointer? ptr)
  "Return #t if pointer PTR is valid and not null."
  (and ptr (not (null-pointer? ptr))))
