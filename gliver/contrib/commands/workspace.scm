;;; gliver/contrib/commands/workspace.scm --- Workspace commands for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver contrib commands workspace)
  #:use-module (gliver core)
  #:use-module (ice-9 format)
  #:declarative? #f
  #:export (
			workspace-create
			workspace-destroy
			workspace-focus-next
			workspace-focus-prev
			workspace-focus-last
			workspace-rename
			workspace-list
))

;;; workspace commands
(define-command (workspace-create name)
  #:interactive (string)
  "Create a new workspace."
  (let ((output (output-current)))
    (when output
      (let ((workspace (make-workspace #:name (or name "New")
                                       #:output output)))
        (workspace-add! workspace)
        (log-debug "Workspace ~a created." (workspace-name workspace))))))

(define-command (workspace-destroy)
  "Kill the current workspace."
  (let ((workspace (workspace-current)))
    (when workspace
      (if (= 1 (length (output-workspaces (output-current))))
          (log-debug "Cannot kill the last workspace.")
          (begin
            (workspace-remove! workspace)
            (log-debug "Workspace killed."))))))

(define-command (workspace-focus-next)
  "Switch to the next workspace."
  (let ((g (workspace-next (output-current))))
    (when g (workspace-focus! g))))

(define-command (workspace-focus-prev)
  "Switch to the previous workspace."
  (let ((g (workspace-prev (output-current))))
    (when g (workspace-focus! g))))

(define-command (workspace-focus-last)
  "Switch to the previously active workspace."
  (let ((prev (output-workspace-previous (output-current))))
    (when prev (workspace-focus! prev))))

(define-command (workspace-rename name)
  #:interactive (string)
  "Rename the current workspace."
  (let ((workspace (workspace-current)))
    (when (and workspace name)
      (workspace-name-set! workspace name)
      (log-debug "Renamed to ~a." name))))

(define-command (workspace-list)
  "List all workspaces."
  (let ((workspaces (output-workspaces (output-current))))
    (log-debug "~a"
             (string-join
              (map (lambda (g)
                     (format #f "~a~a:~a"
                             (if (eq? g (workspace-current)) "*" " ")
                             (workspace-id g)
                             (workspace-name g)))
                   workspaces)
              " "))))
