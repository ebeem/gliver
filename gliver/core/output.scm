;;; gliver/core/output.scm --- Core data model for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver core output)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (system foreign)
  #:use-module (gliver core types)
  #:use-module (gliver core hooks)
  #:use-module (gliver core logs)
  #:use-module (gliver core config)
  #:use-module (gliver river wm-output-manager)
  #:autoload (gliver core workspace) (workspace-add! workspace-focus!)
  ;; TODO: contrib modules shouldn't be used in core
  #:autoload (gliver contrib layout alternating) (layout-alternating-make-config)
  #:export (
			output-add!
			output-remove!
			output-next
			output-prev
			output-focus!
			output-presentation-mode-set
			on-output
			on-output-removed
			on-output-wl-output
			on-output-position
			on-output-dimensions
))

;;; output management
(define (output-add! output)
  ;; each output must at least have one workspace in `output-workspaces`
  ;; if it doesn't have any workspaces, one will be created and focused automatically
  (log-debug "adding output ~a" output)
  (when (null? (output-workspaces output))
	(let ((workspace
		   (make-workspace #:name (format #f "workspace-~d-~d" (output-id output) 1)
						   #:layout (layout-alternating-make-config)
						   #:output output)))
	  (workspace-add! workspace)))

  ;; add the output to the back referenced manager outputs
  (%manager-outputs-set! *manager*
						 (append (manager-outputs *manager*) (list output)))

  ;; focus the output if no output is currently focused
  ;; or if configuration is set to focus new output
  (when (or *wm-behavior-focus-new-output*
			(not (manager-output-current *manager*)))
	(output-focus! output))

  (gliver-hook-run! *output-created-hook* output))

(define (output-remove! output)
  (let ((remaining (delete output (manager-outputs *manager*))))
    (%manager-outputs-set! *manager* remaining)
    (when (eq? (manager-output-current *manager*) output)
      (manager-output-current-set! *manager*
        (and (pair? remaining) (car remaining))))
    (gliver-hook-run! *output-removed-hook* output)))

(define (output-focus! output)
  "Focus active workspace in the output"
  ;; focus the current manager, it's actually an error
  ;; not to have a current workspace
  (let* ((workspace (or (output-workspace-current output)
						(car (output-workspaces output))))
		 (prev-output (manager-output-current *manager*)))
	(%manager-output-previous-set! *manager* prev-output)
	(%manager-output-current-set! *manager* output)
	(when workspace
	  (workspace-focus! workspace))))

(define (output-next)
  (let* ((outputs (manager-outputs *manager*))
         (current (output-current))
         (idx (list-index (lambda (s) (eq? s current)) outputs)))
    (and idx (list-ref outputs (modulo (1+ idx) (length outputs))))))

(define (output-prev)
  (let* ((outputs (manager-outputs *manager*))
         (current (output-current))
         (idx (list-index (lambda (s) (eq? s current)) outputs)))
    (and idx (list-ref outputs (modulo (+ idx (length outputs) -1)
                                       (length outputs))))))

(define (output-presentation-mode-set output mode)
  "Set the preferred presentation mode of the output.
mode: enum value `RIVER_OUTPUT_V1_PRESENTATION_MODE_VSYNC`,
`RIVER_OUTPUT_V1_PRESENTATION_MODE_ASYNC`."
  (let ((proxy-output (output-wl-proxy output)))
    (when proxy-output
      (log-debug "Setting output ~a presentation mode to ~a" proxy-output mode)
      (wm-output-presentation-mode-set proxy-output mode))))

;;; output events
(define (on-output data manager output-proxy)
  "Handle a new output event from the compositor."
  (let* ((outputs (manager-outputs *manager*))
         (name (format #f "output-~a" (length outputs)))
         (output (make-output name #:wl-proxy output-proxy)))
	(log-debug "Output created: ~a" output)
	(output-add! output)))

(define (on-output-removed data proxy-output)
  "Output was removed. This will take care of
Removing the output record and clearing up memory.
Hook: *output-destroy-hook*"
  (log-debug "Output removed: ~a" proxy-output)
  (let ((output (output-find-by-proxy proxy-output)))
    (when output (output-remove! output))
	(wm-output-destroy proxy-output)
	(gliver-hook-run! *output-destroy-hook* output)))

(define (on-output-wl-output data proxy-output object-id)
  "The wl_output object corresponding to the river_output_v1."
  (log-debug "Output wl_output global object-id: ~a = ~a" proxy-output object-id)
  (let ((output (output-find-by-proxy proxy-output)))
    (when output
	  (log-debug "setting output ~a object-id to ~a" output object-id)
	  (let ((prev-object-id (output-wl-output output)))
		(%output-wl-output-set! output object-id)
		(gliver-hook-run! *output-object-id-changed-hook* output prev-object-id)))))

(define (on-output-position data proxy-output x y)
  "Position of the output in the compositor's logical coordinate
space changed. The x and y coordinates may be positive or negative."
  (log-debug "Output position: ~a = ~a,~a" proxy-output x y)
  (let ((output (output-find-by-proxy proxy-output)))
    (when output
	  (let ((prev-x (output-x output))
			(prev-y (output-y output)))
		(%output-x-set! output x)
		(%output-y-set! output y)
		(gliver-hook-run! *output-position-changed-hook* output prev-x prev-y)))))

(define (on-output-dimensions data proxy-output width height)
  (log-debug "Output dimensions: ~a = ~ax~a" proxy-output width height)
  (let ((output (output-find-by-proxy proxy-output)))
    (when output
	  (let ((prev-width (output-width output))
			(prev-height (output-height output)))
		(%output-width-set! output width)
		(%output-height-set! output height)
		(gliver-hook-run! *output-dimensions-changed-hook* output prev-width prev-height)))))

(gliver-hook-add! %output-created-hook on-output 0)
(gliver-hook-add! %output-object-id-changed-hook on-output-wl-output 0)
(gliver-hook-add! %output-position-changed-hook on-output-position 0)
(gliver-hook-add! %output-dimensions-changed-hook on-output-dimensions 0)
(gliver-hook-add! %output-removed-hook on-output-removed 0)

