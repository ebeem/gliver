;;; gliver/contrib/commands/help.scm --- Help and introspection commands for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver contrib commands help)
  #:use-module (gliver core)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 pretty-print)
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
			safe-var-value-string
))

(define (safe-var-value-string val)
  "Return a compact, safe string representation of VAL for display in palette and logs."
  (cond
   ((string? val)
    (if (> (string-length val) 60)
        (string-append (substring val 0 57) "...")
        (format #f "~s" val)))
   ((or (number? val) (boolean? val) (symbol? val) (char? val) (null? val))
    (format #f "~a" val))
   (else
    (catch #t
      (lambda ()
        (call-with-output-string
          (lambda (p) (truncated-print val p #:width 60))))
      (lambda _
        (catch #t
          (lambda ()
            (let ((s (format #f "~a" val)))
              (if (> (string-length s) 60)
                  (string-append (substring s 0 57) "...")
                  s)))
          (lambda _ "#<object>")))))))

;;; help / introspection commands
(define-command (describe-variable)
  "Display the documentation of variables."
  (let* ((keys-and-entries
          (hash-table-fold *variable-registry*
                           (lambda (name val-pair acc)
                             (let* ((module (car val-pair))
                                    (val (catch #t (lambda () (var-get name)) (lambda _ "<unbound>")))
                                    (safe-val (safe-var-value-string val))
                                    (docstring (string-replace-substring (or (cdr val-pair) "") "\n" " "))
                                    (opt (make-dmenu-options
                                          (list (list " " name safe-val (module-name module) docstring))
                                          (list *palette-icon-color* *palette-name-color* *palette-value-color* *palette-help-color* *palette-doc-color*)
                                          #:widths *palette-variables-widths*
                                          #:searchable *palette-variables-searchable*
                                          #:visible *palette-variables-visible*)))
                               (cons (cons name (if (pair? opt) (car opt) opt)) acc)))
                           '()))
         (sorted (sort keys-and-entries
                       (lambda (a b)
                         (string<? (symbol->string (car a))
                                   (symbol->string (car b))))))
         (keys (map car sorted))
         (options (map cdr sorted)))
    (palette-show options #:keys keys
                  #:prompt "Describe variable: "
                  #:on-select
                  (lambda (selected)
                    (when selected
                      (let* ((rec (hash-table-ref/default *variable-registry* selected #f))
                             (val (catch #t (lambda () (var-get selected)) (lambda _ "<unbound>")))
                             (doc (and rec (cdr rec))))
                        (log-info "Variable ~a = ~a~%~a" selected (safe-var-value-string val) (or doc "No documentation."))))))))

(define-command (describe-command)
  "Describe a command, or prompt to select from all commands."
  (let* ((keys-and-entries
          (hash-table-fold *command-registry*
                           (lambda (cname cmd acc)
                             (let* ((doc (string-replace-substring (or (command-docstring cmd) "") "\n" " "))
                                    (opt (make-dmenu-options
                                          (list (list " " cname doc))
                                          (list *palette-icon-color* *palette-name-color* *palette-doc-color*)
                                          #:widths '(2 35 60)
                                          #:searchable '(#f #t #t)
                                          #:visible '(#t #t #t))))
                               (cons (cons cname (if (pair? opt) (car opt) opt)) acc)))
                           '()))
         (sorted (sort keys-and-entries
                       (lambda (a b)
                         (string<? (symbol->string (car a))
                                   (symbol->string (car b))))))
         (keys (map car sorted))
         (options (map cdr sorted)))
    (palette-show options #:keys keys
                  #:prompt "Describe command: "
                  #:on-select
                  (lambda (selected)
                    (when selected
                      (let ((cmd (command-find selected)))
                        (when cmd
                          (log-info "Command ~a: ~a" selected (command-docstring cmd)))))))))

(define-command (describe-key)
  "Describe a keybinding, prompting to select from all active keybindings."
  (let* ((top-pairs (gliver-keymap->alist *top-map*))
         (root-pairs (gliver-keymap->alist *root-map*))
         (all-pairs (append top-pairs root-pairs))
         (entries (map (lambda (pair)
                         (let* ((key-str (gliver-key->string (car pair)))
                                (action (gliver-binding-action (cdr pair)))
                                (act-str (format #f "~a" action))
                                (doc (if (or (symbol? action) (string? action))
                                         (let ((cmd (command-find action)))
                                           (if cmd (or (command-docstring cmd) "") ""))
                                         ""))
                                (opt (make-dmenu-options
                                      (list (list " " key-str act-str doc))
                                      (list *palette-icon-color* *palette-name-color* *palette-value-color* *palette-doc-color*)
                                      #:widths '(2 20 25 50)
                                      #:searchable '(#f #t #t #t)
                                      #:visible '(#t #t #t #t))))
                           (cons key-str (if (pair? opt) (car opt) opt))))
                       all-pairs))
         (sorted (sort entries (lambda (a b) (string<? (car a) (car b))))))
    (palette-show (map cdr sorted) #:keys (map car sorted)
                  #:prompt "Describe key: "
                  #:on-select
                  (lambda (selected)
                    (when selected
                      (log-info "Key ~a is bound in active keymap." selected))))))

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
