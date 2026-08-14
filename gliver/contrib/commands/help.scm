;;; gliver/contrib/commands/help.scm --- Help and introspection commands for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver contrib commands help)
  #:use-module (gliver core)
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-69)
  #:use-module (gliver contrib ui palette)
  #:declarative? #f
  #:export (
			describe-variable
			describe-key
			describe-command
			where-is
			list-commands
))

;;; help / introspection commands

(define-command (describe-variable)
  "Display the documentation of variables."
  (let ((selected (palette-show
				   (hash-table-fold *variable-registry*
									(lambda (name val-pair acc)
									  (let ((module (car val-pair))
											(docstring (string-replace-substring (cdr val-pair) "\n" " ")))
										(cons (make-dmenu-options
											   (list (list " " name (var-get name) (module-name module) docstring))
											   (list *palette-icon-color* *palette-name-color* *palette-value-color* *palette-help-color* *palette-doc-color*)
											   #:widths *palette-variables-widths*
											   #:searchable *palette-variables-searchable*
											   #:visible *palette-variables-visible*)
											  acc)))
									'())
				   #:keys (hash-table-keys *variable-registry*))))
	;; TODO: show a message with information about the variable
	;; and allow editing its value
	(log-info selected)))

(define-command (describe-key key-str)
  #:interactive (string)
  "Describe what a key binding does."
  (let* ((key (kbd key-str))
         (binding (lookup-key *root-map* key))
         (action (and binding (gliver-binding-action binding))))
    (if action
        (cond
         ((command? (command-find action))
          (log-debug "~a → ~a: ~a" key-str action
                   (command-docstring (command-find action))))
         ((gliver-keymap? action)
          (log-debug "~a → keymap: ~a" key-str (gliver-keymap-name action)))
         (else
          (log-debug "~a → ~a" key-str action)))
        (log-debug "~a is not bound." key-str))))

(define-command (describe-command name)
  #:interactive (string)
  "Describe a command."
  (let ((cmd (command-find name)))
    (if cmd
        (log-debug "~a: ~a" name (command-docstring cmd))
        (log-warn "Unknown command: ~a" name))))

(define-command (where-is name)
  #:interactive (string)
  "Find the keybinding for a command."
  (let ((bindings (filter (lambda (pair)
                            (let ((action (gliver-binding-action (cdr pair))))
                              (or (and (string? action)
                                       (string=? action (if (symbol? name)
                                                            (symbol->string name)
                                                            name)))
                                  (and (symbol? action) (eq? action name)))))
                          (gliver-keymap->alist *root-map*))))
    (if (null? bindings)
        (log-debug "~a is not on any key." name)
        (log-debug "~a is on ~a" name
                 (string-join (map (lambda (b) (gliver-key->string (car b))) bindings)
                              ", ")))))

(define-command (list-commands)
  "List all available commands."
  (log-debug "~a" (string-join (sort (map symbol->string (command-all))
                                   string<?)
                             " ")))
