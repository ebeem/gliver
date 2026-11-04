;;; gliver/core/keymap.scm --- Keymap management for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module manages keymaps and keymap layers.
;;; It handles:
;;;  - Defining keymap structures (<keymap>)
;;;  - Registering and looking up keymaps
;;;  - Defining standard keymap layers (*top-map*, *root-map*, etc.)

(define-module (gliver core keymap)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-69)
  #:use-module (gliver core-base)
  #:declarative? #f
  #:export (
			keymap-register!
			keymap-find
			keymap-mode
			keymap-passthrough-mode?
			define-keymap
			keymap->alist
			keymap-clear!
			*top-map*
			*root-map*
			*workspace-map*
			*resize-map*
			*window-map*
			*help-map*
			*hotkeygrab-map*
))

(define (keymap-register! keymap)
  "Register a keymap in the global registry.
Parameters: (KEYMAP)
  - KEYMAP: <keymap> record to register.
Returns registered <keymap> record."
  (let ((keymaps (manager-keymaps *manager*)))
    (unless (memq keymap keymaps)
      (%manager-keymaps-set! *manager* (cons keymap keymaps))))
  keymap)

(define (keymap-find name)
  "Find a registered <keymap> by symbol or string NAME, or return #f.
Parameters: (NAME)
  - NAME: Keymap name symbol or string.
Returns matching <keymap> record or #f."
  (let ((sym (if (symbol? name) name (string->symbol name))))
    (find (lambda (keymap)
            (eq? (keymap-name keymap) sym))
          (manager-keymaps *manager*))))

(define (keymap-mode keymap)
  "Return the active mode symbol for KEYMAP.
Parameters: (KEYMAP)
  - KEYMAP: Target <keymap> record or mode symbol.
Returns symbol."
  (cond
   ((keymap? keymap) (keymap-name keymap))
   ((symbol? keymap) keymap)
   (else (string->symbol (format #f "~a" keymap)))))

(define (keymap-passthrough-mode? mode-name)
  "Return #t if MODE-NAME corresponds to a passthrough keymap.
Parameters: (MODE-NAME)
  - MODE-NAME: Keymap record or mode name symbol.
Returns #t if mode is passthrough, #f otherwise."
  (and-let* ((keymap (if (keymap? mode-name)
                         mode-name
                         (keymap-find mode-name))))
    (keymap-passthrough? keymap)))

(define* (define-keymap name #:key (passthrough? #f))
  "Create, register, and return a new <keymap> named NAME.
Parameters: (NAME [#:passthrough? PASSTHROUGH?])
  - NAME: Keymap name symbol or string.
  - PASSTHROUGH?: Boolean flag; when #t, unbound keys pass through to focused window (default #f).
Returns newly created and registered <keymap> record."
  (let ((keymap (make-keymap name #:passthrough? passthrough?)))
    (keymap-register! keymap)
    keymap))

(define (keymap->alist keymap)
  "Return an alist of (key-string . binding) for all bindings in KEYMAP.
Parameters: (KEYMAP)
  - KEYMAP: Target <keymap> record.
Returns alist of (key-string . <keybinding>) pairs."
  (hash-table->alist (keymap-bindings keymap)))

(define (keymap-clear! keymap)
  "Remove all bindings from KEYMAP.
Parameters: (KEYMAP)
  - KEYMAP: Target <keymap> record."
  (hash-table-walk (keymap-bindings keymap)
                   (lambda (key value)
                     (hash-table-delete! (keymap-bindings keymap) key))))

;;; standard keymaps
(define-var *top-map*
  (define-keymap '*top-map*)
  "Top map layer (default).")

(define-var *root-map*
  (define-keymap '*root-map*)
  "Root map layer (prefix).")

(define-var *workspace-map*
  (define-keymap '*workspace-map*)
  "Workspace map layer.")

(define-var *resize-map*
  (define-keymap '*resize-map*)
  "Resize map layer.")

(define-var *window-map*
  (define-keymap '*window-map*)
  "Window map layer")

(define-var *help-map*
  (define-keymap '*help-map*)
  "Help map layer")

(define-var *hotkeygrab-map*
  (define-keymap '*hotkeygrab-map* #:passthrough? #t)
  "Hotkey grab map layer")
