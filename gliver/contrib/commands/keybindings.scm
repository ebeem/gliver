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
			enter-submap
			keybindings-clear!
))

;;; prefix mode commands
(define-command (prefix-activated)
  "Handle prefix key activation."
  (var-set! *mode* '*root-map*)
  (log-debug "Prefix mode activated."))

(define-command (prefix-abort)
  "Abort prefix mode."
  (var-set! *mode* 'normal)
  (log-debug "Aborted."))

(define-command (enter-submap name)
  #:interactive (symbol)
  "Enter a sub-keymap by symbol."
  (log-debug "Entering submap: ~a" name))

(define-command (keybindings-clear!)
  "Clear all keybindings from all standard and registered keymaps."
  (for-each gliver-keymap-clear! (all-keymaps))
  (gliver-hook-run! *keybinding-sync-request-hook*))
