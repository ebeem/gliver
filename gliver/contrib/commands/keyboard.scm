;;; gliver/contrib/commands/keyboard.scm --- Keyboard commands for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver contrib commands keyboard)
  #:use-module (gliver core)
  #:use-module (gliver river keyboard-manager)
  #:declarative? #f
  #:export (
			keyboard-layout-next
			keyboard-layout-prev
))

;;; keyboard commands
(define-command (keyboard-layout-next)
  "Switch to the next configured keyboard layout."
  (keyboard-manager-layout-next!))

(define-command (keyboard-layout-prev)
  "Switch to the previous configured keyboard layout."
  (keyboard-manager-layout-prev!))

