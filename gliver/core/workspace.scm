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
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (system foreign)
  #:use-module (gliver core logs)
  #:use-module (gliver core config)
  #:use-module (gliver core hooks)
  #:use-module (gliver core types)
  #:export (
			workspace-add!
			%workspace-remove-target
			workspace-containers-move
			workspace-remove!
			workspace-switch-to!
			workspace-switch-to-by-id!
			workspace-switch-to-by-name!
			workspace-next
			workspace-prev
))

(define* (workspace-add! output #:key (layout 'tiling) (name "workspace"))
  "Create a new workspace on @var{output}."
  (let* ((tag (manager-tag-next!))
         (num (manager-workspace-number-next!))
         (workspace (make-workspace #:name name
									#:id num
									#:tag-mask tag
									#:containers '()
									#:output output
									#:layout layout)))
    (output-workspaces-set! output
      (append (output-workspaces output) (list workspace)))
    (gliver-hook-run! *workspace-new-hook* workspace)
    workspace))

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
    (workspace-containers-set! t-workspace
                               (append (workspace-containers t-workspace)
                                       (workspace-containers s-workspace)))
    (workspace-containers-set! s-workspace '())))

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
      (output-workspaces-set! output
        (delete workspace (output-workspaces output)))
      ;; if this was current, switch
      (when (eq? (output-workspace-current output) workspace)
        (output-workspace-current-set! output (car (output-workspaces output))))
      (gliver-hook-run! *workspace-destroy-hook* workspace t-workspace))))

(define (workspace-switch-to! workspace)
  "Switch to @var{workspace} on its output."
  (let ((output (workspace-output workspace))
        (old-workspace (output-workspace-current (workspace-output workspace))))
    (unless (eq? workspace old-workspace)
	  ;; TODO: call river api here
      (output-workspace-previous-set! output old-workspace)
      (output-workspace-current-set! output workspace)
      (gliver-hook-run! *workspace-switch-hook* workspace old-workspace))))

(define (workspace-switch-to-by-id! n)
  "Switch to workspace id N on the current output."
  (let* ((output (output-current))
         (workspace (find (lambda (g) (= (workspace-id g) n))
                      (output-workspaces output))))
    (when workspace
      (workspace-switch-to! workspace))))

(define (workspace-switch-to-by-name! name)
  "Switch to the workspace named NAME on the current output."
  (let ((workspace (workspace-find-by-name name)))
    (when workspace
      (workspace-switch-to! workspace))))

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

