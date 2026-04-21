;;; gliver/contrib/which-key.scm --- Display available keys in submaps
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; Listens for keymap changes and, when entering a non-top-map mode
;;; (prefix or submap), displays all available keybindings in the
;;; message bar.
;;;
;;; Usage:
;;;   (use-modules (gliver contrib which-key))
;;;   (which-key-enable!)
;;;
;;; Customization:
;;;   (set! *which-key-separator* " | ")
;;;   (set! *which-key-show-docstrings* #f)

(define-module (gliver contrib which-key)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (gliver core logs)
  #:use-module (gliver core hooks)
  #:use-module (gliver keybindings)
  #:use-module (gliver commands)
  #:use-module (gliver message-bar)
  #:declarative? #f
  #:export (which-key-enable!
            which-key-disable!
            *which-key-separator*
            *which-key-show-docstrings*))

;;; configuration
(define *which-key-separator* " | ")
(define *which-key-show-docstrings* #t)

;;; internal state
(define *which-key-active* #f)

(define (mode->keymap mode-name)
  "Resolve a mode name symbol to its corresponding keymap.
Returns the keymap or #f if not found."
  (cond
   ((eq? mode-name 'normal)  *top-map*)
   ((eq? mode-name 'prefix)  *root-map*)
   (else
    ;; Search root-map bindings for a submap whose name matches
    (let ((name-str (symbol->string mode-name)))
      (let loop ((pairs (gliver-keymap->alist *root-map*)))
        (cond
         ((null? pairs) #f)
         (else
          (let ((binding (cdar pairs)))
            (let ((action (gliver-binding-action binding)))
              (if (and (gliver-keymap? action)
                       (string=? (gliver-keymap-name action) name-str))
                  action
                  (loop (cdr pairs))))))))))))

(define (describe-action action)
  "Return a short description string for a binding action."
  (cond
   ((gliver-keymap? action)
    (format #f "+~a" (gliver-keymap-name action)))
   ((symbol? action)
    (symbol->string action))
   ((string? action)
    action)
   ((procedure? action)
    "λ")
   ((and (list? action) (eq? (car action) 'enter-submap))
    (format #f "+~a" (cadr action)))
   (else
    "?")))

(define (format-bindings keymap)
  "Format all bindings in KEYMAP as a single display string."
  (let* ((pairs (gliver-keymap->alist keymap))
         (entries (map (lambda (pair)
                         (let* ((key (car pair))
                                (binding (cdr pair))
                                (action (gliver-binding-action binding)))
                           (format #f "~a: ~a"
                                   (gliver-key->string key)
                                   (describe-action action))))
                       pairs)))
    (string-join (sort entries string<?) *which-key-separator*)))

(define (which-key-on-keymap-change mode-name)
  "Hook handler: show bindings when entering a non-normal mode."
  (cond
   ((eq? mode-name 'normal)
    ;; Returned to normal mode — nothing to show
	(message-bar-hide!)
    #f)
   (else
    (let ((keymap (mode->keymap mode-name)))
      (if keymap
          (let ((text (format #f "[~a] ~a"
                              mode-name (format-bindings keymap))))
            (message-bar-show! text))
          (log-debug "which-key: no keymap found for mode ~a"
                     mode-name))))))

(define (which-key-enable!)
  "Enable which-key: show available keys when entering submaps."
  (unless *which-key-active*
    (gliver-hook-add! *keymap-change-hook* which-key-on-keymap-change)
    (set! *which-key-active* #t)
    (log-info "which-key enabled.")))

(define (which-key-disable!)
  "Disable which-key."
  (when *which-key-active*
    (gliver-hook-remove! *keymap-change-hook* which-key-on-keymap-change)
    (set! *which-key-active* #f)
    (log-info "which-key disabled.")))
