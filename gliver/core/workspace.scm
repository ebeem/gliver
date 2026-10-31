;;; gliver/core/workspace.scm --- Workspace management and navigation
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module manages virtual workspaces (desktops) in Gliver.
;;; It handles:
;;;  - Adding, removing, and switching workspaces
;;;  - Moving windows and containers between workspaces
;;;  - Navigating/Switching workspaces in directional and grid-based systems

(define-module (gliver core workspace)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (gliver core-base)
  #:autoload (gliver core container) (container-focus! container-add! container-remove!)
  #:autoload (gliver core window) (window-move-to-workspace!)
  #:autoload (gliver core output) (output-focus!)
  #:declarative? #f
  #:export (
			*workspace-name-default*
			*workspace-focus-new*
			*workspace-remove-to*
			*workspace-destroyable*
			*workspace-grid-rows*
			*workspace-grid-wrap?*
			workspace-add!
			workspace-windows-move
			workspace-containers-move
			workspace-manual?
			workspace-remove!
			workspace-focused?
			workspace-visible?
			workspace-focus!
			workspace-next
			workspace-prev
			workspace-direction
			workspace-down
			workspace-up
			workspace-left
			workspace-right
			workspace-windows
))

(define-var *workspace-name-default* "Default"
			"Default workspace name used for initial workspace.")
(define-var *workspace-focus-new* #t
			"Whether newly created workspaces are automatically focused.")
(define-var *workspace-remove-to* 'focus
			"Strategy for choosing the next active workspace when one is removed ('focus or 'index).")
(define-var *workspace-destroyable* #f
			"Whether empty workspaces can be automatically destroyed.")
(define-var *workspace-grid-rows* 1
			"Number of rows in the workspace grid.up/down workspace focus moves by this number.")
(define-var *workspace-grid-wrap?* #t
			"Whether up/down workspace focus wraps around when moving out of the grid boundaries.")

(define* (workspace-add! workspace #:key (focus #t))
  "Register WORKSPACE in its parent output's workspace list.
Parameters: (WORKSPACE [#:focus #t])
  - WORKSPACE: <workspace> record to register.
  - FOCUS: Boolean, whether to focus WORKSPACE if allowed (default #t)."
  (log-info "Adding workspace ~a" workspace)
  (let* ((output (workspace-output workspace))
		 (output-focused-workspace (output-workspace-current output)))
	;; each workspace must at least have one container in `workspace-containers`
	;; if it doesn't have any workspaces, one will be created and focused automatically
	(when (null? (workspace-containers workspace))
	  (log-debug "Adding empty container to workspace")
	  (let* ((container
			  (make-container #:workspace workspace
							  #:x (output-usable-x output)
							  #:y (output-usable-y output)
							  #:width (output-usable-width output)
							  #:height (output-usable-height output))))
		(container-add! container #:focus focus)))
	;; add the workspace to the back referenced output workspaces
	(%output-workspaces-set! output
							 (append (output-workspaces output) (list workspace)))

	;; focus the workspace if no workspace is currently focused
	;; or if configuration is set to focus new workspace
	(when (and focus
               (or *workspace-focus-new*
			       (not output-focused-workspace)))
	  (workspace-focus! workspace))
	(gliver-hook-run! *workspace-created-hook* workspace)))

(define* (%workspace-remove-target workspace1 workspace2)
  "Determine the target fallback workspace when WORKSPACE1 is removed.
If WORKSPACE2 is provided and valid, returns it; otherwise resolves based on
*workspace-remove-to* ('focus or 'index).
Parameters: (WORKSPACE1 WORKSPACE2)
  - WORKSPACE1: <workspace> record being removed.
  - WORKSPACE2: Target <workspace> record fallback, or #f.
Returns <workspace> record or #f if WORKSPACE1 is the only workspace."
  ;; if workspace2 is different than workspace1 and not #f, return it
  (if (and workspace2 (not (eq? workspace1 workspace2)))
      workspace2
      (let* ((output (workspace-output workspace1))
             (workspaces (output-workspaces output)))
		;; if we have only 1 workspace, we shouldn't remove it, return #f
        (if (<= (length workspaces) 1)
            #f
            (let ((previous-ws (output-workspace-previous output)))
			  ;; if remove behavior set to focus and previous focused workspace
			  ;; is not same as workspace1 and not empty, return it
              (if (and (eq? *workspace-remove-to* 'focus)
                       previous-ws
                       (not (eq? previous-ws workspace1)))
                  previous-ws
				  ;; otherwise, return previous workspace by index
                  (let ((idx (list-index (lambda (w) (eq? w workspace1))
                                         workspaces)))
                    (cond ((and idx (> idx 0))
                           (list-ref workspaces (- idx 1)))
                          ((and idx (= idx 0))
                           (list-ref workspaces 1))
                          (else
						   ;; in case the workspace had no index
                           (find (lambda (ws) (not (eq? ws workspace1)))
                                 workspaces))))))))))

(define (workspace-windows-move s-workspace t-workspace)
  "Move all windows from S-WORKSPACE to T-WORKSPACE without changing focus.
Parameters: (S-WORKSPACE T-WORKSPACE)
  - S-WORKSPACE: Source <workspace> record.
  - T-WORKSPACE: Target <workspace> record."
  (for-each (lambda (window)
              (window-move-to-workspace! window t-workspace #:focus #f))
            (workspace-windows s-workspace)))

(define (workspace-containers-move s-workspace t-workspace)
  "Move all containers from S-WORKSPACE to T-WORKSPACE.
Parameters: (S-WORKSPACE T-WORKSPACE)
  - S-WORKSPACE: Source <workspace> record.
  - T-WORKSPACE: Target <workspace> record."
  (when (positive? (length (workspace-windows s-workspace)))
    (%workspace-containers-set! t-workspace
								(append (workspace-containers t-workspace)
										(workspace-containers s-workspace)))
    (%workspace-containers-set! s-workspace '())))

(define (workspace-manual? workspace)
  "Return #t if WORKSPACE uses manual layout.
Parameters: (WORKSPACE)
  - WORKSPACE: <workspace> record to check.
Returns #t if WORKSPACE uses manual layout, #f otherwise."
  (and workspace
       (let* ((layout-cfg (workspace-layout workspace))
              (layout-type (if (list? layout-cfg)
                               (or (assq-ref layout-cfg 'layout-type)
                                   (assq-ref layout-cfg 'layout))
                               layout-cfg)))
         (eq? layout-type 'manual))))

(define* (workspace-remove! workspace #:key (t-workspace #f))
  "Delete WORKSPACE, moving its windows to T-WORKSPACE.
Parameters: (WORKSPACE [#:t-workspace #f])
  - WORKSPACE: <workspace> record to remove.
  - T-WORKSPACE: Optional target fallback <workspace> record."
  (let ((output (workspace-output workspace))
		(target (%workspace-remove-target workspace t-workspace)))
	(when target
	  ;; move all windows to the target workspace
	  (workspace-windows-move workspace target)
	  ;; destroy all containers of the workspace
	  (let ((containers (list-copy (workspace-containers workspace))))
	    (%workspace-containers-set! workspace '())
	    (%workspace-container-current-set! workspace #f)
	    (%workspace-container-previous-set! workspace #f)
	    (for-each (lambda (c) (container-remove! c #:focus #f))
	              containers))
      ;; remove workspace from output
      (%output-workspaces-set! output
							   (delq workspace (output-workspaces output)))
      ;; if this was current, switch to the target workspace
      (when (eq? (output-workspace-current output) workspace)
        (workspace-focus! target))
      (when (eq? (output-workspace-previous output) workspace)
        (%output-workspace-previous-set! output #f))
      (gliver-hook-run! *workspace-destroy-hook* workspace target))))

(define (workspace-focused? workspace)
  "Return #t if WORKSPACE is currently the active focused workspace.
Parameters: (WORKSPACE)
  - WORKSPACE: <workspace> record to check.
Returns #t if WORKSPACE is currently focused, #f otherwise."
  (eq? workspace (workspace-current)))

(define (workspace-visible? workspace)
  "Return #t if WORKSPACE is currently visible on its output.
Parameters: (WORKSPACE)
  - WORKSPACE: <workspace> record to check.
Returns #t if WORKSPACE is visible on its output, #f otherwise."
  (eq? workspace
	   (output-workspace-current (workspace-output workspace))))

(define* (workspace-focus! workspace #:key (focus-child #t) (focus-parent #t))
  "Focus active container in WORKSPACE.
Parameters: (WORKSPACE [#:focus-child #t] [#:focus-parent #t])
  - WORKSPACE: <workspace> record to focus.
  - FOCUS-CHILD: Boolean, whether to focus active container within WORKSPACE (default #t).
  - FOCUS-PARENT: Boolean, whether to ensure output is focused (default #t)."
  (log-debug "Focusing workspace ~a, is focused? = ~a" workspace
			 (workspace-focused? workspace))
  (unless (workspace-focused? workspace)
	(let* ((containers (workspace-containers workspace))
		   (container (or (workspace-container-current workspace)
						  (and (pair? containers) (car containers))))
		   (output (workspace-output workspace))
		   (prev-workspace (and output (output-workspace-current output))))
	  (unless (eq? prev-workspace workspace)
	    (%output-workspace-previous-set! output prev-workspace)
	    (%output-workspace-current-set! output workspace))
	  (when (and focus-parent output)
		(output-focus! output #:focus-child #f))
	  (when (and focus-child container)
		(container-focus! container #:focus-parent #f))
	  (unless (eq? prev-workspace workspace)
	    (gliver-hook-run! *workspace-switch-hook* workspace prev-workspace)))))

(define* (workspace-next #:optional (workspace (workspace-current)))
  "Return the next <workspace> record in output after WORKSPACE.
Parameters: ([WORKSPACE])
  - WORKSPACE: <workspace> record (defaults to current focused workspace).
Returns next <workspace> record, or #f if not found."
  (let* ((output (workspace-output workspace))
		 (workspaces (output-workspaces output))
         (current (output-workspace-current output))
         (idx (list-index (lambda (g) (eq? g current)) workspaces)))
    (and idx (list-ref workspaces (modulo (1+ idx) (length workspaces))))))

(define* (workspace-prev #:optional (workspace (workspace-current)))
  "Return the previous <workspace> record in output before WORKSPACE.
Parameters: ([WORKSPACE])
  - WORKSPACE: <workspace> record (defaults to current focused workspace).
Returns previous <workspace> record, or #f if not found."
  (let* ((output (workspace-output workspace))
		 (workspaces (output-workspaces output))
         (current (output-workspace-current output))
         (idx (list-index (lambda (g) (eq? g current)) workspaces)))
    (and idx (list-ref workspaces (modulo (+ idx (length workspaces) -1)
										  (length workspaces))))))

(define* (workspace-direction dir #:optional (workspace (workspace-current)))
  "Return the workspace in the direction DIR relative to WORKSPACE in a grid layout, or #f if none.
The number of rows is specified by *workspace-grid-rows* which will change the default behavior
of navigation if its value is greater than 1.
Parameters: (DIR [WORKSPACE])
  - DIR: Direction symbol or string ('up, 'down, 'left, 'right).
  - WORKSPACE: <workspace> record (defaults to current focused workspace).
Returns target <workspace> record in DIR, or #f if none."
  (and-let* ((ws         (or workspace (workspace-current)))
             (output     (workspace-output ws))
             (workspaces (output-workspaces output))
             (len        (length workspaces))
             ((positive? len))
             (idx        (list-index (lambda (w) (eq? w ws)) workspaces)))
    (let* ((dir-sym    (if (string? dir) (string->symbol dir) dir))
           (vertical?  (memq dir-sym '(up down)))
           (previous?  (memq dir-sym '(up left)))
		   ;; number of workspaces per row
           (cols       (if (and (number? *workspace-grid-rows*)
                                (> *workspace-grid-rows* 1))
                           *workspace-grid-rows*
                           len))
           (step       (if vertical? cols 1))
		   ;; starting index of current row and column
           (start      (if vertical?
                           (modulo idx cols)
                           (* (quotient idx cols) cols)))
           (end        (if vertical?
                           (- (1- len) (modulo (- (1- len) start) cols))
                           (min (1- len) (+ start (1- cols)))))
           (target     (if previous? (- idx step) (+ idx step)))
           (target-idx (cond
                        ((<= start target end) target)
                        ((and *workspace-grid-wrap?* (> end start))
                         (if previous? end start))
                        (else #f))))
      (and target-idx (list-ref workspaces target-idx)))))

(define* (workspace-down #:optional (workspace (workspace-current)))
  "Return the workspace below WORKSPACE in the grid layout, or #f if none.
Parameters: ([WORKSPACE])
  - WORKSPACE: <workspace> record (defaults to current focused workspace).
Returns <workspace> below WORKSPACE in grid layout, or #f if none."
  (workspace-direction 'down workspace))

(define* (workspace-up #:optional (workspace (workspace-current)))
  "Return the workspace above WORKSPACE in the grid layout, or #f if none.
Parameters: ([WORKSPACE])
  - WORKSPACE: <workspace> record (defaults to current focused workspace).
Returns <workspace> above WORKSPACE in grid layout, or #f if none."
  (workspace-direction 'up workspace))

(define* (workspace-left #:optional (workspace (workspace-current)))
  "Return the workspace on the left of WORKSPACE in the grid layout, or #f if none.
Parameters: ([WORKSPACE])
  - WORKSPACE: <workspace> record (defaults to current focused workspace).
Returns <workspace> on the left of WORKSPACE in grid layout, or #f if none."
  (workspace-direction 'left workspace))

(define* (workspace-right #:optional (workspace (workspace-current)))
  "Return the workspace on the right of WORKSPACE in the grid layout, or #f if none.
Parameters: ([WORKSPACE])
  - WORKSPACE: <workspace> record (defaults to current focused workspace).
Returns <workspace> on the right of WORKSPACE in grid layout, or #f if none."
  (workspace-direction 'right workspace))

(define (workspace-windows workspace)
  "Return a flat list of all <window> records in WORKSPACE across all its containers.
Parameters: (WORKSPACE)
  - WORKSPACE: <workspace> record.
Returns list of <window> records in WORKSPACE."
  (apply append
         (map container-windows
              (workspace-containers workspace))))
