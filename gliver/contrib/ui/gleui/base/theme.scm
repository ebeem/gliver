;;; gliver/contrib/ui/gleui/base/theme.scm --- Theme Lookup Helpers
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module provides theme override lookup utilities for gleui.
;;; It handles:
;;;  - Theme property fallback resolution via `var-get'

(define-module (gliver contrib ui gleui base theme)
  #:use-module (gliver core-base)
  #:declarative? #f
  #:re-export (??)
  #:export (
            gleui-theme-get
))

(define (gleui-theme-get overrides key default-val)
  "Look up KEY in theme OVERRIDES alist, fallback to DEFAULT-VAL.
Parameters: (OVERRIDES KEY DEFAULT-VAL)
  - OVERRIDES: Alist or #f of theme override keys and values.
  - KEY: Symbol style key to resolve.
  - DEFAULT-VAL: Value direct fallback or configuration variable symbol.
Returns resolved style value, or #f if lookup fails."
  (or (and overrides (assq-ref overrides key))
      (if (symbol? default-val)
          (catch #t (lambda () (var-get default-val)) (lambda _ #f))
          default-val)))
