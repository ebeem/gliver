;;; gliver/core/workspace.scm --- Core workspace model and utility for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Author: ebeem <lord.ebeem@gmail.com>
;;; Maintainer: ebeem <lord.ebeem@gmail.com>
;;; Workspace: similar to an emacs group/isolation concept and stumpwm group
;;; A collection of containers and their associated windows (workspace/virtual desktop)

(define-module (gliver core manager)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (gliver core logs)
  #:use-module (gliver core config)
  #:use-module (gliver core hooks)
  #:use-module (gliver core types)
  ;; lazy loaded, core type functions shouldn't be imported here
  #:autoload (gliver core container) (container-add!)
  #:autoload (gliver core workspace) (workspace-add!)
  #:autoload (gliver contrib layout alternating) (layout-alternating-make-config)
  #:export (
))

(define* (on-output-created output)
  "Updates the created output state."
  ;; Each output must at least have one workspace in `output-workspaces`
  ;; If it doesn't have any workspaces, one will be created automatically
  ;; using this hook
  (when (= 0 (length (output-workspaces output)))
	(let ((workspace
		   (make-workspace #:name (format #f "workspace-%d-%d" (output-id output) 1)
						   #:layout (layout-alternating-make-config)
						   #:output output)))
	  (workspace-add! workspace)
	  (output-workspace-current-set! output workspace)))
  (manager-outputs-set! *manager*
						(append (manager-outputs *manager*) (list output)))
  (when (= 1 (length (manager-outputs *manager*)))
	(manager-output-current-set! *manager* output)))

(define* (on-workspace-created workspace)
  "Updates the created workspace state."
  ;; Each workspace must at least have one container in `workspace-containers`
  ;; If it doesn't have any workspaces, one will be created automatically
  ;; using this hook
  (let* ((output (workspace-output workspace)))
	(when (= 0 (length (workspace-containers workspace)))
	  (let* ((container
			  (make-container #:workspace workspace
							  #:width (output-width output)
							  #:height (output-height output))))
		(container-add! container)
		(workspace-container-current-set! workspace container)))
	(output-workspaces-set! output
							(append (output-workspaces output) (list workspace)))))

(define* (on-container-created container)
  "Updates the created container state."
  (let* ((workspace (container-workspace container)))
	(workspace-containers-set! workspace
							   (append (workspace-containers workspace) (list container)))))

(define* (on-window-created window)
  "Updates the created window state."
  ;; To improve performance, windows are stored in the manager directly
  ;; so they can quickly be looked up. Each container references all of its windows
  ;; and windows back reference the container.
  ;; All of this is handled by the hook, developers must only set the window container
  ;; in the window record using `window-container-set`.
  (let* ((container (window-container window)))
	(container-windows-set! container
						  (append (container-windows container) (list window)))
	(manager-windows-set! *manager*
						  (append (manager-windows *manager*) (list window)))))

(gliver-hook-add! *window-created-hook* on-window-created -100)
(gliver-hook-add! *output-created-hook* on-output-created -100)
(gliver-hook-add! *workspace-created-hook* on-workspace-created -100)
(gliver-hook-add! *container-created-hook* on-container-created -100)

