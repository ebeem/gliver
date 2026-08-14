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
  (manager-config-set! 'mode 'prefix)
  (log-debug "Prefix mode activated."))

(define-command (prefix-abort)
  "Abort prefix mode."
  (manager-config-set! 'mode 'normal)
  (log-debug "Aborted."))

(define-command (enter-submap name)
  #:interactive (string)
  "Enter a sub-keymap by name."
  (log-debug "Entering submap: ~a" name))

(define-command (keybindings-clear!)
  "Clear all keybindings from all standard keymaps."
  (gliver-keymap-clear! *top-map*)
  (gliver-keymap-clear! *root-map*)
  (gliver-keymap-clear! *workspace-map*)
  (gliver-keymap-clear! *resize-map*)
  (gliver-hook-run! *keybinding-sync-request-hook*))
