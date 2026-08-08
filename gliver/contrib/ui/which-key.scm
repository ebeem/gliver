;;; gliver/contrib/ui/which-key.scm --- Display available keys in submaps
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; Listens for keymap changes and, when entering a non-top-map mode
;;; (prefix or submap), displays all available keybindings using the
;;; configured toast backend.
;;;
;;; Usage:
;;;   (use-modules (gliver contrib ui which-key))
;;;   (which-key-enable!)

(define-module (gliver contrib ui which-key)
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-1)
  #:use-module (gliver core)
  #:use-module (gliver contrib ui toast)
  #:use-module (gliver contrib commands)
  #:declarative? #f
  #:export (
			*which-key-separator*
			*which-key-columns*
			*which-key-column-width*
			*which-key-delay*
			*which-key-keybinding-color*
			*which-key-separator-color*
			*which-key-command-color*
			*which-key-keymap-color*
			*which-key-keymap-prefix*
			*which-key-keymap-remove-asterisks*
			*%which-key-enabled*
			*which-key-identifier-name*
			describe-action
			format-bindings
			which-key-on-keymap-change
			which-key-enable!
			which-key-disable!
			))

;;; configuration
(define-var *which-key-separator* " -> ")
(define-var *which-key-columns* 4
			"Number of columns displayed")
(define-var *which-key-column-width* 40
			"Number of characters of each column")
(define-var *which-key-delay* 200
			"Delay in ms before showing which-key if no input is provided.")
(define-var *which-key-keybinding-color* *theme-mauve*
			"The color of the keybinding in which key")
(define-var *which-key-separator-color* *theme-overlay1*
			"The color of the separator in which key")
(define-var *which-key-command-color* *theme-text*
			"The color of the command in which key")
(define-var *which-key-keymap-color* *theme-yellow*
			"The color of the keymap in which key")
(define-var *which-key-keymap-prefix* "+"
			"The prefix symbol of the keymap in which key")
(define-var *which-key-keymap-remove-asterisks* #t
			"Whether to remove the asterisks in the keymap name")
(define-var *%which-key-enabled* #f
			"Internal state that indicates which-key is enabled")
(define-var *which-key-identifier-name* "which-key-identifier-name"
            "Identifier name passed to toast and used to detect it later in order to kill it for instance.")

(define (describe-action action)
  "Return a short description toast span for a binding action."
  (let* ((is-keymap? (or (and (list? action) (eq? (car action) 'enter-submap))
                         (gliver-keymap? action)))
         (raw-name (cond
                    ((and (list? action) (eq? (car action) 'enter-submap))
                     (format #f "~a" (cadr action)))
                    ((gliver-keymap? action)
                     (format #f "~a" (gliver-keymap-name action)))
                    ((symbol? action) (symbol->string action))
                    ((string? action) action)
                    ((procedure? action) "λ")
                    (else "?")))

         ;; clean asterisks if it is a keymap and the config is #t
         (cleaned-name (if (and is-keymap? *which-key-keymap-remove-asterisks*)
                           (string-replace-substring raw-name "*" "")
                           raw-name))

         ;; apply prefix if it is a keymap
         (final-name (if is-keymap?
                         (string-append *which-key-keymap-prefix* cleaned-name)
                         cleaned-name))

		 ;; choose keymap or command color
         (color (if is-keymap?
                    *which-key-keymap-color*
                    *which-key-command-color*)))

	;; make and return the span
	(toast-make-span final-name #:color color)))

(define (%chunk-list lst n)
  (let loop ((rest lst) (acc '()))
    (if (null? rest)
        (reverse acc)
        (let* ((len (length rest))
               (take-n (take rest (min n len)))
               (drop-n (drop rest (min n len))))
          (loop drop-n (cons take-n acc))))))

(define (format-bindings keymap)
  "Format all bindings in KEYMAP as a single display string."
  (let* ((pairs (gliver-keymap->alist keymap))
         ;; sort alphabetically by the string representation of the key
         (sorted-pairs (sort pairs (lambda (a b)
                                     (string<? (gliver-key->string (car a))
                                               (gliver-key->string (car b))))))

         ;; convert each binding into an toast column
         (cols (map (lambda (pair)
                      (let* ((key-str     (gliver-key->string (car pair)))
                             (action      (gliver-binding-action (cdr pair)))
                             (key-span    (toast-make-span key-str #:color *which-key-keybinding-color*))
                             (sep-span    (toast-make-span *which-key-separator* #:color *which-key-separator-color*))
                             (action-span (describe-action action)))
                        (toast-make-column (list key-span sep-span action-span)
										   #:width *which-key-column-width*)))
                    sorted-pairs))

         ;; group the columns into rows based on the *which-key-columns* config
         (rows (%chunk-list cols *which-key-columns*)))
	rows))

(define (which-key-on-keymap-change mode-name)
  "Hook handler: show bindings when entering a non-normal mode."
  (cond
   ((eq? mode-name 'normal)
    ;; returned to normal mode, kill the current which-key toast
	(toast-kill #:name *which-key-identifier-name*)
    #f)
   (else
    (let ((keymap (var-get mode-name)))
      (if keymap
          (let ((text (format-bindings keymap)))
			(toast-show text #:name *which-key-identifier-name*))
          (log-debug "which-key: no keymap found for mode ~a"
                     mode-name))))))

(define (which-key-enable!)
  "Enable which-key: show available keys when entering submaps."
  (unless *%which-key-enabled*
    (gliver-hook-add! *keymap-change-hook* 'which-key-on-keymap-change)
    (set! *%which-key-enabled* #t)
    (log-info "which-key enabled.")))

(define (which-key-disable!)
  "Disable which-key."
  (when *%which-key-enabled*
    (gliver-hook-remove! *keymap-change-hook* 'which-key-on-keymap-change)
    (set! *%which-key-enabled* #f)
    (log-info "which-key disabled.")))
