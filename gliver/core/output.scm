;;; gliver/core/output.scm --- Output management for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module manages physical screen outputs in Gliver.
;;; It handles:
;;;  - Adding, removing, and switching active outputs
;;;  - Tracking geometry, position offsets, and usable area changes
;;;  - Configuring presentation modes (vsync / async)

(define-module (gliver core output)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (gliver core-base)
  #:use-module (gliver river wm-output-manager)
  #:autoload (gliver core workspace) (workspace-add! workspace-focus!)
  #:declarative? #f
  #:export (
			*output-focus-new?*
			output-add!
			output-remove!
			output-focused?
			output-prev-focused?
			output-focus!
			output-next
			output-prev
			output-presentation-mode-set
			output-on-output
			output-release-wl-output-proxy!
			output-on-output-removed
			output-on-output-wl-output
			output-on-output-position
			output-on-output-dimensions
))

(define-var *output-focus-new?* #f
			"Whether newly connected monitors/outputs are automatically focused.")

;;; output management
(define* (output-add! output #:key (focus #t))
  "Register OUTPUT in the Gliver's manager hierarchy.
Parameters: (OUTPUT [#:focus #t])
  - OUTPUT: <output> record to register.
  - FOCUS: Boolean that indicates whether to focus OUTPUT if allowed (default #t)."
  (log-info "Adding output ~a" output)
  ;; each output must at least have one workspace in `output-workspaces`
  ;; if it doesn't have any workspaces, one will be created and focused automatically
  (when (null? (output-workspaces output))
	(let ((workspace
		   (make-workspace #:name (format #f "workspace-~d-~d" (output-id output) 1)
						   #:layout `((layout . manual)
									  (layout-type . manual)
									  (split-ratio . 0.5)
									  (inner-gap . #f)
									  (outer-gap . #f))
						   #:output output)))
	  (workspace-add! workspace #:focus focus)))

  ;; add the output to the back referenced manager outputs
  (%manager-outputs-set! *manager*
						 (append (manager-outputs *manager*) (list output)))

  ;; focus the output if no output is currently focused
  ;; or if configuration is set to focus new output
  (when (and focus
             (or *output-focus-new?*
			     (not (manager-output-current *manager*))))
	(output-focus! output))

  (gliver-hook-run! *output-created-hook* output))

(define (output-remove! output)
  "Remove OUTPUT from the manager's active outputs list.
Parameters: (OUTPUT)
  - OUTPUT: <output> record to remove."
  ;;TODO: transfers current output focus to another remaining output if OUTPUT was active.
  (output-release-wl-output-proxy! output)
  (let ((remaining (delq output (manager-outputs *manager*))))
    (%manager-outputs-set! *manager* remaining)
	;; if output is currently focused, focus another one
    (when (output-focused? output)
	  (if (pair? remaining)
		  (output-focus! (and  (car remaining)))
		  ;; if we don't have anymore outputs, dereference deleted one
		  (%manager-output-current-set! *manager* #f)))

	;; if the output deleted is the previously focused, dereference it
	(when (output-prev-focused? output)
	  (%manager-output-previous-set! *manager* #f))
    (gliver-hook-run! *output-removed-hook* output)))

(define (output-focused? output)
  "Return #t if OUTPUT is currently the active focused output in *manager*.
Parameters: (OUTPUT)
  - OUTPUT: <output> record to check.
Returns #t if OUTPUT is focused, #f otherwise."
  (eq? output (manager-output-current *manager*)))

(define (output-prev-focused? output)
  "Return #t if OUTPUT is the previously focused output in *manager*.
Parameters: (OUTPUT)
  - OUTPUT: <output> record to check.
Returns #t if OUTPUT is the previously focused one, #f otherwise."
  (eq? output (manager-output-previous *manager*)))

(define* (output-focus! output #:key (focus-child #t))
  "Focus OUTPUT and its active child workspace when FOCUS-CHILD is #t.
Parameters: (OUTPUT [#:focus-child #t])
  - OUTPUT: <output> record to focus.
  - FOCUS-CHILD: Boolean, whether to focus the active child workspace (default #t)."
  (unless (output-focused? output)
	(let* ((workspaces (output-workspaces output))
		   (workspace (or (output-workspace-current output)
						  (and (pair? workspaces) (car workspaces))))
		   (prev-output (manager-output-current *manager*)))
	  (%manager-output-previous-set! *manager* prev-output)
	  (%manager-output-current-set! *manager* output)
	  (gliver-hook-run! *output-focus-hook* output prev-output)
	  (when (and workspace focus-child)
		(workspace-focus! workspace #:focus-parent #f)))))

(define (output-next)
  "Return the next <output> record after current focused output, or #f.
Parameters: None.
Returns next <output> record, or #f if not found."
  (let* ((outputs (manager-outputs *manager*))
         (current (output-current))
         (idx (list-index (lambda (s) (eq? s current)) outputs)))
    (and idx (list-ref outputs (modulo (1+ idx) (length outputs))))))

(define (output-prev)
  "Return the previous <output> record before current focused output, or #f.
Parameters: None.
Returns previous <output> record, or #f if not found."
  (let* ((outputs (manager-outputs *manager*))
         (current (output-current))
         (idx (list-index (lambda (s) (eq? s current)) outputs)))
    (and idx (list-ref outputs (modulo (+ idx (length outputs) -1)
                                       (length outputs))))))

(define (output-presentation-mode-set output mode)
  "Set the preferred presentation mode of OUTPUT to MODE.
Parameters: (OUTPUT MODE)
  - OUTPUT: <output> record.
  - MODE: Presentation mode (e.g. RIVER_OUTPUT_V1_PRESENTATION_MODE_VSYNC)."
  (let ((proxy-output (output-wl-proxy output)))
    (log-debug "Setting output ~a presentation mode to ~a" proxy-output mode)
    (wm-output-presentation-mode-set proxy-output mode)))

;;; output events
(define (output-on-output data manager proxy-output)
  "Invoked when an output is advertised.
Parameters: (DATA MANAGER PROXY-OUTPUT)
  - DATA: User context pointer (unused, null in Gliver).
  - MANAGER: Pointer to river_window_manager_v1 proxy.
  - PROXY-OUTPUT: Pointer to river_output_v1 proxy."
  (let* ((outputs (manager-outputs *manager*))
         (name (format #f "output-~a" (length outputs)))
         (output (make-output name #:wl-proxy proxy-output)))
	(log-info "Output created: ~a" output)
	(output-add! output)))

(define (output-release-wl-output-proxy! output)
  "Release wl_output proxy associated with OUTPUT.
Parameters: (OUTPUT)
  - OUTPUT: <output> record to release its wl_output proxy."
  (when (and output (output? output))
    (let ((proxy (output-wl-output-proxy output)))
      (when proxy
        (wm-output-wl-output-release proxy)
        (%output-wl-output-proxy-set! output #f)))))

(define (output-on-output-removed data proxy-output)
  "Invoked when OUTPUT is removed.
Parameters: (DATA PROXY-OUTPUT)
  - DATA: User context pointer (unused).
  - PROXY-OUTPUT: Pointer to river_output_v1 proxy."
  (log-info "Output removed: ~a" proxy-output)
  (let ((output (output-find-by-proxy proxy-output)))
    (when output
      (output-remove! output)
	  (gliver-hook-run! *output-destroy-hook* output))))

(define (output-on-output-wl-output data proxy-output object-id)
  "Invoked when wl_output Wayland object is advertisement for river_output_v1.
Parameters: (DATA PROXY-OUTPUT OBJECT-ID)
  - DATA: User context pointer (unused).
  - PROXY-OUTPUT: Pointer to river_output_v1 proxy.
  - OBJECT-ID: Numeric Wayland object ID of the wl_output."
  (log-debug "Output wl_output object-id: ~a = ~a" proxy-output object-id)
  (let ((output (output-find-by-proxy proxy-output)))
    (when output
	  (log-debug "Setting output ~a object-id to ~a" output object-id)
	  (%output-wl-output-set! output object-id)
	  (when (number? object-id)
		;; NOTE: no need to ensure the previous wl-output is cleaned
		;; as this event is supposed to be sent only once
		(let ((proxy (wm-output-wl-output-bind object-id 4)))
		  (%output-wl-output-proxy-set! output proxy)))
	  (gliver-hook-run! *output-object-id-changed-hook* output #f))))

(define (output-on-output-position data proxy-output x y)
  "Invoked when the position of the output in the compositor's logical coordinate space changed.
Dispatches *output-position-changed-hook*.
Parameters: (DATA PROXY-OUTPUT X Y)
  - DATA: User context pointer (unused).
  - PROXY-OUTPUT: Pointer to river_output_v1 proxy.
  - X: Logical horizontal position in compositor space.
  - Y: Logical vertical position in compositor space."
  (log-info "Output position: ~a = ~a,~a" proxy-output x y)
  (let ((output (output-find-by-proxy proxy-output)))
    (when output
	  (let* ((prev-x (output-x output))
			 (prev-y (output-y output))
			 (dx (- x prev-x))
			 (dy (- y prev-y)))
		(%output-x-set! output x)
		(%output-y-set! output y)
		(%output-usable-x-set! output (+ (output-usable-x output) dx))
		(%output-usable-y-set! output (+ (output-usable-y output) dy))
		(gliver-hook-run! *output-position-changed-hook* output prev-x prev-y)))))

(define (output-on-output-dimensions data proxy-output width height)
  "Invoked when the output dimension of PROXY-OUTPUT is changed.
Parameters: (DATA PROXY-OUTPUT WIDTH HEIGHT)
  - DATA: User context pointer (unused).
  - PROXY-OUTPUT: Pointer to river_output_v1 proxy.
  - WIDTH: Logical output width in pixels.
  - HEIGHT: Logical output height in pixels."
  (log-info "Output dimensions: ~a = ~ax~a" proxy-output width height)
  (let ((output (output-find-by-proxy proxy-output)))
    (when output
	  (let ((prev-width (output-width output))
			(prev-height (output-height output)))
		(%output-width-set! output width)
		(%output-height-set! output height)
		(when (= (output-usable-width output) prev-width)
		  (%output-usable-width-set! output width))
		(when (= (output-usable-height output) prev-height)
		  (%output-usable-height-set! output height))
		(gliver-hook-run! *output-dimensions-changed-hook* output prev-width prev-height)))))

(gliver-hook-add! %output-created-hook 'output-on-output 0)
(gliver-hook-add! %output-object-id-changed-hook 'output-on-output-wl-output 0)
(gliver-hook-add! %output-position-changed-hook 'output-on-output-position 0)
(gliver-hook-add! %output-dimensions-changed-hook 'output-on-output-dimensions 0)
(gliver-hook-add! %output-removed-hook 'output-on-output-removed 0)
