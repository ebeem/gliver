;;; gliver/contrib/commands/keybindings.scm --- Keybinding commands for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver contrib commands keybindings)
  #:use-module (gliver core)
  #:declarative? #f
  #:export (
			prefix-activated
			prefix-abort
			enter-keymap
			keybindings-clear!
))

;;; prefix mode commands
(define-command (prefix-activated)
  "Handle prefix key activation."
  (keybinding-switch-to-mode! '*root-map*)
  (log-debug "Prefix mode activated."))

(define-command (prefix-abort)
  "Abort prefix mode."
  (keybinding-switch-to-mode! 'normal)
  (log-debug "Aborted."))

(define-command (enter-keymap name)
  #:interactive (symbol)
  "Enter a keymap by symbol."
  (let ((mode (if (keymap? name) (keymap-name name) name)))
    (keybinding-switch-to-mode! mode)
    (log-debug "Entering keymap: ~a" mode)))

(define-command (keybindings-clear!)
  "Clear all keybindings from all standard and registered keymaps."
  (for-each keymap-clear! *keymap-registered*)
  (keybinding-sync!))
