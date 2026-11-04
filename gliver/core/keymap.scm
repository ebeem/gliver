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
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-69)
  #:use-module (gliver core-base)
  #:declarative? #f
  #:export (
			keymap-passthrough?
			keymap-bindings
			keymap-name
			keymap?
			*keymap-registered*
			keymap-register!
			keymap-find
			keymap-passthrough-mode?
			make-keymap
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

;; keymap representation record:
;;  - name: keymap identifier symbol (e.g. '*top-map*, 'normal, etc.)
;;  - bindings: hash table mapping key string to <keybinding> record
;;  - passthrough?: boolean indicating if unbound keys pass through to focused window
(define-record-type <keymap>
  (%make-keymap name bindings passthrough?)
  keymap?
  (name         keymap-name)
  (bindings     keymap-bindings)
  (passthrough? keymap-passthrough?))

;; list of all registered <keymap> records
;; TODO: I would like to store this in the manager state instead
(define-var *keymap-registered* '()
  "List of all registered <keymap> instances.")

(define (keymap-register! km)
  "Register a keymap in the global registry.
Parameters: (KM)
  - KM: <keymap> record to register.
Returns registered <keymap> record."
  (unless (memq km *keymap-registered*)
    (set! *keymap-registered* (cons km *keymap-registered*)))
  km)

(define (keymap-find name)
  "Find a registered <keymap> by symbol or string NAME, or return #f.
Parameters: (NAME)
  - NAME: Keymap name symbol or string.
Returns matching <keymap> record or #f."
  (let ((sym (if (symbol? name) name (string->symbol name))))
    (find (lambda (km)
			(eq? (keymap-name km) sym))
		  *keymap-registered*)))

(define (keymap-passthrough-mode? mode-name)
  "Return #t if MODE-NAME corresponds to a passthrough keymap.
Parameters: (MODE-NAME)
  - MODE-NAME: Keymap record or mode name symbol.
Returns #t if mode is passthrough, #f otherwise."
  (and-let* ((km (if (keymap? mode-name)
					 mode-name
					 (keymap-find mode-name))))
    (keymap-passthrough? km)))

(define* (make-keymap name #:key (passthrough? #f))
  "Create, register, and return a new <keymap> named NAME.
Parameters: (NAME [#:passthrough? PASSTHROUGH?])
  - NAME: Keymap name symbol or string.
  - PASSTHROUGH?: Boolean flag; when #t, unbound keys pass through to focused window (default #f).
Returns newly created and registered <keymap> record."
  (let* ((sym (if (symbol? name) name (string->symbol name)))
         (km (%make-keymap sym (make-hash-table string=? string-hash) passthrough?)))
    (keymap-register! km)
    km))

(define (keymap->alist km)
  "Return an alist of (key-string . binding) for all bindings in KM.
Parameters: (KM)
  - KM: Target <keymap> record.
Returns alist of (key-string . <keybinding>) pairs."
  (hash-table->alist (keymap-bindings km)))

(define (keymap-clear! km)
  "Remove all bindings from KM.
Parameters: (KM)
  - KM: Target <keymap> record.
Returns unspecified."
  (hash-table-walk (keymap-bindings km)
                   (lambda (key value)
                     (hash-table-delete! (keymap-bindings km) key))))

;;; standard keymaps
(define-var *top-map*
  (make-keymap '*top-map*)
  "Top map layer (default).")

(define-var *root-map*
  (make-keymap '*root-map*)
  "Root map layer (prefix).")

(define-var *workspace-map*
  (make-keymap '*workspace-map*)
  "Workspace map layer.")

(define-var *resize-map*
  (make-keymap '*resize-map*)
  "Resize map layer.")

(define-var *window-map*
  (make-keymap '*window-map*)
  "Window map layer")

(define-var *help-map*
  (make-keymap '*help-map*)
  "Help map layer")

(define-var *hotkeygrab-map*
  (make-keymap '*hotkeygrab-map* #:passthrough? #t)
  "Hotkey grab map layer")
