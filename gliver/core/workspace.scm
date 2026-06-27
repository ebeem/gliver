;;; gliver/core/workspace.scm --- Core workspace model and utility for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Author: ebeem <lord.ebeem@gmail.com>
;;; Maintainer: ebeem <lord.ebeem@gmail.com>
;;; Workspace: similar to an emacs group/isolation concept and stumpwm group
;;; A collection of containers and their associated windows (workspace/virtual desktop)

(define-module (gliver core workspace)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (system foreign)
  #:use-module (gliver core logs)
  #:use-module (gliver core config)
  #:use-module (gliver core hooks)
  #:use-module (gliver core types)
  #:autoload (gliver core container) (container-focus! container-add!)
  #:autoload (gliver core output) (output-focus!)
  #:export (
			workspace-add!
			%workspace-remove-target
			workspace-containers-move
			workspace-remove!
			workspace-focus!
			workspace-next
			workspace-prev
			workspace-windows
			workspace-windows-visible
))

(define* (workspace-add! workspace)
  "Create a new workspace."
  (log-debug "adding workspace ~a" workspace)
  (let* ((output (workspace-output workspace))
		 (output-focused-workspace (output-workspace-current output)))

	;; each workspace must at least have one container in `workspace-containers`
	;; if it doesn't have any workspaces, one will be created and focused automatically
	(when (null? (workspace-containers workspace))
	  (log-debug "adding empty container to workspace")
	  (let* ((container
			  (make-container #:workspace workspace
							  #:x 0
							  #:y 0
							  #:width (output-width output)
							  #:height (output-height output))))
		(container-add! container)))

	;; add the workspace to the back referenced output workspaces
	(%output-workspaces-set! output
							 (append (output-workspaces output) (list workspace)))

	;; focus the workspace if no workspace is currently focused
	;; or if configuration is set to focus new workspace
	(when (or *wm-behavior-focus-new-workspace*
			  (not (output-focused-workspace output)))
	  (workspace-focus! workspace))

	(gliver-hook-run! *workspace-created-hook* workspace)))

(define* (%workspace-remove-target workspace1 workspace2)
  "Return the target workspace based on logic plus configuration."
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
              (if (and (eq? *wm-behavior-workspace-remove-to* 'focus)
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

(define (workspace-containers-move s-workspace t-workspace)
  "Move all containers from @var{s-workspace} to @var{t-workspace}.
This only happens if @var{s-workspace} has any windows. Containers from
@var{s-workspace} are appended to @var{t-workspace}'s container list,
and then @var{s-workspace}'s container list is emptied."
  (when (positive? (length (workspace-windows s-workspace)))
    (%workspace-containers-set! t-workspace
                               (append (workspace-containers t-workspace)
                                       (workspace-containers s-workspace)))
    (%workspace-containers-set! s-workspace '())))

(define* (workspace-remove! workspace #:key (t-workspace #f))
  "Delete @var{workspace}, moving its containers to @var{t-workspace}."
  (let ((output (workspace-output workspace))
		(target (%workspace-remove-target workspace t-workspace)))
	(when target
	  ;; move all containers to the target one
	  (workspace-containers-move workspace t-workspace)
	  ;; then move all current containers to it
	  ;; TODO: this shouldn't be needed anymore, but test
	  ;; (when (> (length (workspace-windows workspace)) 0)
	  ;; 	(for-each (lambda (w) (window-move-to-workspace! w target))
      ;;             (workspace-windows workspace)))
      ;; remove workspace from output
      (%output-workspaces-set! output
        (delete workspace (output-workspaces output)))
      ;; if this was current, switch
      (when (eq? (output-workspace-current output) workspace)
        (%output-workspace-current-set! output (car (output-workspaces output))))
      (gliver-hook-run! *workspace-destroy-hook* workspace t-workspace))))

(define (workspace-focused? workspace)
  "Returns true if the workspace is currently focused"
  (eq? workspace (workspace-current)))

(define* (workspace-focus! workspace #:key (focus-child #t) (focus-parent #t))
  "Focus active container in the workspace"
  ;; focus the current container, it's actually an error
  ;; not to have a current container
  (log-debug "focusing workspace ~a, is focused? = ~a" workspace
			(workspace-focused? workspace))
  (unless (workspace-focused? workspace)
	(let* ((containers (workspace-containers workspace))
		   (container (or (workspace-container-current workspace)
						  (and (pair? containers) (car containers))))
		   (output (workspace-output workspace))
		   (prev-workspace (output-workspace-current output)))
	  (when (and focus-child container)
		(container-focus! container #:focus-parent #f))
	  (%output-workspace-previous-set! output prev-workspace)
	  (%output-workspace-current-set! output workspace)
	  (when (and focus-parent output)
		(output-focus! output #:focus-child #f)))))

(define (workspace-next workspace)
  (let* ((output (workspace-output workspace))
		 (workspaces (output-workspaces output))
         (current (output-workspace-current output))
         (idx (list-index (lambda (g) (eq? g current)) workspaces)))
    (and idx (list-ref workspaces (modulo (1+ idx) (length workspaces))))))

(define (workspace-prev workspace)
  (let* ((output (workspace-output workspace))
		 (workspaces (output-workspaces output))
         (current (output-workspace-current output))
         (idx (list-index (lambda (g) (eq? g current)) workspaces)))
    (and idx (list-ref workspaces (modulo (+ idx (length workspaces) -1)
                                      (length workspaces))))))

(define (workspace-windows workspace)
  "Return all windows in @var{workspace}."
  (apply append 
         (map container-windows
              (workspace-containers workspace))))

(define (workspace-windows-visible workspace)
  "Return the currently visible (not hidden/minimized) windows in @var{workspace}."
  (filter window-visible? (workspace-windows workspace)))

