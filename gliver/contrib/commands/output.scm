;;; gliver/contrib/commands/output.scm --- Output commands for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver contrib commands output)
  #:use-module (gliver core)
  #:declarative? #f
  #:export (
			output-focus-next
			output-focus-prev
))

;;; output commands
(define-command (output-focus-next)
  "Focus the next output."
  (let ((ns (output-next)))
    (when ns
      (output-focus! ns))))

(define-command (output-focus-prev)
  "Focus the previous output."
  (let ((ps (output-prev)))
    (when ps
      (output-focus! ps))))
