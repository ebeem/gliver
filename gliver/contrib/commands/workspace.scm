;;; gliver/contrib/commands/workspace.scm --- Workspace commands for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver contrib commands workspace)
  #:use-module (gliver core)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:declarative? #f
  #:export (
			resolve-workspace
			workspace-focus
			workspace-focus-by-name
			workspace-focus-by-index
			workspace-create
			workspace-destroy
			workspace-focus-next
			workspace-focus-prev
			workspace-focus-last
			workspace-focus-right
			workspace-focus-left
			workspace-focus-up
			workspace-focus-down
			workspace-rename
			workspace-list
))

(define (resolve-workspace target)
  "Resolve TARGET into a <workspace> record on the current output."
  (let* ((output (output-current))
         (workspaces (if output (output-workspaces output) '())))
    (cond
     ((workspace? target) target)
     ((number? target)
      (or (find (lambda (w) (= (workspace-id w) target)) workspaces)
          (and (positive? target) (<= target (length workspaces))
               (list-ref workspaces (1- target)))
          (find (lambda (w) (string=? (workspace-name w) (number->string target))) workspaces)))
     ((symbol? target)
      (resolve-workspace (symbol->string target)))
     ((string? target)
      (let ((num (string->number target)))
        (or (workspace-find-by-name target)
            (and num (resolve-workspace num)))))
     (else #f))))

(define-command (workspace-focus target)
  #:interactive (string)
  "Focus/Switch to WORKSPACE (by name, index, ID, or interactive prompt)."
  (let ((ws (resolve-workspace target)))
	(when ws
	  (workspace-focus! ws)
      (log-debug "Switched to workspace ~a." (workspace-name ws)))))

(define-command (workspace-focus-by-name name)
  #:interactive (string)
  "Focus/Switch to workspace by name on current output."
  (workspace-focus name))

(define-command (workspace-focus-by-index index)
  #:interactive (integer)
  "Focus/Switch to workspace by 1-based index on the current output."
  (let* ((output (output-current))
         (workspaces (if output (output-workspaces output) '())))
    (if (and workspaces (number? index) (positive? index) (<= index (length workspaces)))
        (let ((ws (list-ref workspaces (1- index))))
          (workspace-focus! ws)
          (log-debug "Switched to workspace index ~a (~a)." index (workspace-name ws)))
        (log-debug "Workspace index ~a out of range." index))))

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
  (and-let* ((output (output-current))
			 (workspace (workspace-current))
			 (workspaces (output-workspaces output)))
	;; NOTE: should these show up as toast messages?
	(cond
	 ((not *wm-behavior-workspace-destroyable*)
	  (log-info "Destroying workspaces is disabled"))
	 ((<= (length workspaces) 1)
	  (log-info "Each output must at least have 1 workspace."))
	 (else
      (workspace-remove! workspace)
      (log-debug "Workspace destroyed.")))))

(define-command (workspace-focus-next)
  "Switch to the next workspace."
  (let ((g (workspace-next)))
    (when g (workspace-focus! g))))

(define-command (workspace-focus-prev)
  "Switch to the previous workspace."
  (let ((g (workspace-prev)))
    (when g (workspace-focus! g))))

(define-command (workspace-focus-last)
  "Switch to the previously active workspace."
  (let* ((output (output-current))
         (prev (and output (output-workspace-previous output))))
    (when prev (workspace-focus! prev))))

(define-command (workspace-focus-right)
  "Switch to the workspace in the right direction."
  ;; TODO: implement focus right
  (workspace-focus-next))

(define-command (workspace-focus-left)
  "Switch to the workspace in the left direction."
  ;; TODO: implement focus left
  (workspace-focus-prev))

(define-command (workspace-focus-up)
  "Switch to the workspace in the up direction."
    ;; TODO: implement focus up
  (workspace-focus-next))

(define-command (workspace-focus-down)
  "Switch to the workspace in the down direction."
  ;; TODO: implement focus down
  (workspace-focus-prev))

(define-command (workspace-rename name)
  #:interactive (string)
  "Rename the current workspace."
  (let ((workspace (workspace-current)))
    (when (and workspace name)
      (workspace-name-set! workspace name)
      (log-debug "Renamed to ~a." name))))

(define-command (workspace-list)
  "List all workspaces."
  (let* ((output (output-current))
         (workspaces (if output (output-workspaces output) '())))
    (log-debug "~a"
               (string-join
				(map (lambda (g)
                       (format #f "~a~a:~a"
                               (if (eq? g (workspace-current)) "*" " ")
                               (workspace-id g)
                               (workspace-name g)))
                     workspaces)
				" "))))
