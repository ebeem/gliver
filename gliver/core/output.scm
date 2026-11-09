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
  #:use-module (srfi srfi-2)
  #:use-module (gliver core-base)
  #:use-module (gliver river wm-output-manager)
  #:autoload (gliver core workspace) (workspace-add!
									  workspace-focus!
									  workspace-windows
									  workspace-remove!)
  #:autoload (gliver core window)    (window-hide!
								      window-show!)
  #:autoload (gliver core container) (container-remove!)
  #:declarative? #f
  #:export (
			*output-focus-new?*
			*detached-output-workspaces*
			output-add!
			output-remove!
			output-save-state!
			output-restore-state!
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
			output-subpixel-code->symbol
			output-transform-code->symbol
			output-on-wl-output-geometry
			output-on-wl-output-mode
			output-on-wl-output-done
			output-on-wl-output-scale
			output-on-wl-output-name
			output-on-wl-output-description
			output-on-output-position
			output-on-output-dimensions
))

(define-var *output-focus-new?* #f
			"Whether newly connected monitors/outputs are automatically focused.")

(define-var *detached-output-workspaces* (make-hash-table)
			"Hash table storing detached workspaces and output
state for disconnected outputs. Key is output wl-name
and value is a list of workspaces and output state.")

(define (%output-find-free-name)
  "Find the lowest available output name in the form output-N.
Parameters: None.
Returns a string (e.g. \"output-0\")."
  ;; NOTE: this is used over the previous system which kept incrementing
  ;; the output index even if previous allocated names aren't used anymore
  (let ((names (map output-name (manager-outputs *manager*))))
	(do ((idx 0 (1+ idx))
		 (candidate "output-0" (format #f "output-~a" (1+ idx))))
		((not (member candidate names)) candidate))))

(define (%detached-workspaces-pop! wl-name)
  "Retrieve and remove saved detached workspace state for WL-NAME.
Parameters: (WL-NAME)
  - WL-NAME: String Wayland output hardware name (e.g. \"eDP-1\").
Returns saved state list or #f."
  (and-let* ((entry (hash-ref *detached-output-workspaces* wl-name)))
    (hash-remove! *detached-output-workspaces* wl-name)
    entry))

(define (output-save-state! output)
  "Save workspaces and geometry state for OUTPUT before detachment,
and hide its active windows.
Parameters: (OUTPUT)
  - OUTPUT: <output> record to save state for."
  (when (output? output)
    ;; hide all windows on all workspaces of this output
    (for-each
     (lambda (ws)
       (for-each
        (lambda (w)
          (when (and w (window? w) (not (window-destroyed? w)))
            (window-hide! w)))
        (workspace-windows ws)))
     (output-workspaces output))

    ;; preserve workspaces and geometry if this output had workspaces
    (and-let* ((workspaces (output-workspaces output))
			   (name (output-wl-name output)))
      (hash-set! *detached-output-workspaces*
                 name
                 (list workspaces
                       (output-workspace-current output)
                       (output-workspace-previous output)
                       (output-id output)
                       (output-x output)
                       (output-y output)
                       (output-width output)
                       (output-height output)
                       (output-usable-x output)
                       (output-usable-y output)
                       (output-usable-width output)
                       (output-usable-height output)
                       (output-statusbar output)
                       (output-wallpaper output))))))

(define* (output-restore-state! wl-name #:optional output)
  "Restore saved detached workspace state for OUTPUT matching WL-NAME.
Parameters: (WL-NAME [OUTPUT])
  - WL-NAME: String hardware output name (e.g. \"eDP-1\").
  - OUTPUT: Optional <output> record (looked up by WL-NAME if omitted).
Returns #t if state was restored, #f otherwise."
  (and-let* ((saved-state (%detached-workspaces-pop! wl-name))
			 (out (or output (output-find-by-wl-name wl-name))))
	(let ((workspaces (list-ref saved-state 0))
          (current-ws (list-ref saved-state 1))
          (prev-ws (list-ref saved-state 2))
          (saved-id (list-ref saved-state 3))
          (saved-x (list-ref saved-state 4))
          (saved-y (list-ref saved-state 5))
          (saved-w (list-ref saved-state 6))
          (saved-h (list-ref saved-state 7))
          (saved-ux (list-ref saved-state 8))
          (saved-uy (list-ref saved-state 9))
          (saved-uw (list-ref saved-state 10))
          (saved-uh (list-ref saved-state 11))
          (saved-bar (and (> (length saved-state) 12) (list-ref saved-state 12)))
          (saved-wp  (and (> (length saved-state) 13) (list-ref saved-state 13)))
          (initial-ws (output-workspaces out)))

      (log-info "Restoring detached workspaces for output ~a (~a)" out wl-name)
      (when saved-id (%output-id-set! out saved-id))
      (when saved-x (%output-x-set! out saved-x))
      (when saved-y (%output-y-set! out saved-y))
      (when saved-w (%output-width-set! out saved-w))
      (when saved-h (%output-height-set! out saved-h))
      (when saved-ux (%output-usable-x-set! out saved-ux))
      (when saved-uy (%output-usable-y-set! out saved-uy))
      (when saved-uw (%output-usable-width-set! out saved-uw))
      (when saved-uh (%output-usable-height-set! out saved-uh))

	  ;; clean up any empty initial placeholder workspace created during output-add!
      (for-each
       (lambda (ws)
         (when (null? (workspace-windows ws))
           (workspace-remove! ws)))
       initial-ws)

      ;; re-link restored workspaces to this output (in case it was changed)
      (for-each
       (lambda (ws)
         (%workspace-output-set! ws out))
       workspaces)

	  ;; set output workspaces to the new ones
      (%output-workspaces-set! out workspaces)
	  ;; restore previous focused workspace
      (%output-workspace-previous-set! out prev-ws)
	  ;; focus the previously focused workspace (fallback to first)
      (and-let* ((target-ws (or current-ws
								(and (pair? workspaces)
									 (car workspaces)))))
        (workspace-focus! target-ws
                          #:focus-child #t
                          #:focus-parent #t))
      (when saved-wp
        (output-wallpaper-set! out saved-wp))
      (when saved-bar
        (output-statusbar-set! out saved-bar))
      #t)))

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

  ;; save output workspaces and geometry state before removing
  (output-save-state! output)

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

	;; if no remaining outputs, clear window focus on seats
	(when (null? remaining)
	  (for-each
	   (lambda (seat)
		 (when (seat? seat)
		   (%seat-window-focused-set! seat #f)
		   (%seat-window-entered-set! seat #f)))
	   (manager-seats *manager*)))

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
  (let* ((name (%output-find-free-name))
         (output (make-output name #:wl-proxy proxy-output)))
	(log-info "Output created: ~a" output)
	(output-add! output)))

(define (output-release-wl-output-proxy! output)
  "Release wl_output proxy associated with OUTPUT.
Parameters: (OUTPUT)
  - OUTPUT: <output> record to release its wl_output proxy."
  (and-let* (((output? output))
			 (proxy (output-wl-output-proxy output)))
	(wm-output-wl-output-release proxy)
	(%output-wl-output-proxy-set! output #f)))

(define (output-on-output-removed data proxy-output)
  "Invoked when OUTPUT is removed.
Parameters: (DATA PROXY-OUTPUT)
  - DATA: User context pointer (unused).
  - PROXY-OUTPUT: Pointer to river_output_v1 proxy."
  (log-info "Output removed: ~a" proxy-output)
  (and-let* ((output (output-find-by-proxy proxy-output)))
    (output-remove! output)
	(gliver-hook-run! *output-destroy-hook* output)))

(define (output-on-output-wl-output data proxy-output object-id)
  "Invoked when wl_output Wayland object is advertisement for river_output_v1.
Parameters: (DATA PROXY-OUTPUT OBJECT-ID)
  - DATA: User context pointer (unused).
  - PROXY-OUTPUT: Pointer to river_output_v1 proxy.
  - OBJECT-ID: Numeric Wayland object ID of the wl_output."
  (log-debug "Output wl_output object-id: ~a = ~a" proxy-output object-id)
  (and-let* ((output (output-find-by-proxy proxy-output)))
	(log-debug "Setting output ~a object-id to ~a" output object-id)
	(%output-wl-output-set! output object-id)
	(when (number? object-id)
	  ;; NOTE: no need to ensure the previous wl-output is cleaned
	  ;; as this event is supposed to be sent only once
	  (let ((proxy (wm-output-wl-output-bind object-id 4)))
		(%output-wl-output-proxy-set! output proxy)))
	(gliver-hook-run! *output-object-id-changed-hook* output #f)))

(define (output-subpixel-code->symbol code)
  "Convert Wayland subpixel enum integer CODE to symbol."
  (case code
    ((0) 'unknown)
    ((1) 'none)
    ((2) 'horizontal-rgb)
    ((3) 'horizontal-bgr)
    ((4) 'vertical-rgb)
    ((5) 'vertical-bgr)
    (else 'unknown)))

(define (output-transform-code->symbol code)
  "Convert Wayland transform enum integer CODE to symbol."
  (case code
    ((0) 'normal)
    ((1) '90)
    ((2) '180)
    ((3) '270)
    ((4) 'flipped)
    ((5) 'flipped-90)
    ((6) 'flipped-180)
    ((7) 'flipped-270)
    (else 'normal)))

(define (output-on-wl-output-geometry data proxy x y physical-width physical-height subpixel make model transform)
  "Invoked when wl_output geometric properties are advertised."
  (and-let* ((output (output-find-by-wl-output-proxy proxy))
			 (sp-sym (output-subpixel-code->symbol subpixel))
			 (tr-sym (output-transform-code->symbol transform)))
    (%output-physical-width-set! output physical-width)
    (%output-physical-height-set! output physical-height)
    (%output-subpixel-set! output sp-sym)
    (%output-make-set! output make)
    (%output-model-set! output model)
    (%output-transform-set! output tr-sym)
    (gliver-hook-run! *output-geometry-changed-hook*
                      output physical-width physical-height sp-sym make model tr-sym)))

(define (output-on-wl-output-mode data proxy flags width height refresh)
  "Invoked when wl_output mode is advertised."
  (and-let* ((output (output-find-by-wl-output-proxy proxy))
			 (current? (positive? (logand flags 1))))
    (%output-mode-flags-set! output flags)
    (%output-mode-width-set! output width)
    (%output-mode-height-set! output height)
    (%output-mode-refresh-set! output refresh)
	(gliver-hook-run! *output-mode-changed-hook* output flags width height refresh)))

(define (output-on-wl-output-done data proxy)
  "Invoked when all wl_output properties in the current batch have been received."
  (and-let* ((output (output-find-by-wl-output-proxy proxy)))
    (%output-done-set! output #t)
    (gliver-hook-run! *output-done-hook* output)))

(define (output-on-wl-output-scale data proxy factor)
  "Invoked when wl_output scale is advertised."
  (and-let* ((output (output-find-by-wl-output-proxy proxy)))
    (%output-scale-set! output factor)
    (gliver-hook-run! *output-scale-changed-hook* output factor)))

(define (output-on-wl-output-name data proxy name)
  "Invoked when wl_output hardware name is advertised."
  (and-let* ((output (output-find-by-wl-output-proxy proxy)))
    (log-info "wl_output ~a name event: ~a (output: ~a)" proxy name (output-name output))
    (%output-wl-name-set! output name)
    (output-restore-state! name output)
    (gliver-hook-run! *output-wl-name-changed-hook* output name)))

(define (output-on-wl-output-description data proxy desc)
  "Invoked when wl_output description is advertised."
  (and-let* ((output (output-find-by-wl-output-proxy proxy)))
    (log-info "wl_output ~a description event: ~a (output: ~a)" proxy desc (output-name output))
    (%output-description-set! output desc)
    (gliver-hook-run! *output-description-changed-hook* output desc)))

(define (output-on-output-position data proxy-output x y)
  "Invoked when the position of the output in the compositor's logical coordinate space changed.
Dispatches *output-position-changed-hook*.
Parameters: (DATA PROXY-OUTPUT X Y)
  - DATA: User context pointer (unused).
  - PROXY-OUTPUT: Pointer to river_output_v1 proxy.
  - X: Logical horizontal position in compositor space.
  - Y: Logical vertical position in compositor space."
  (log-info "Output position: ~a = ~a,~a" proxy-output x y)
  (and-let* ((output (output-find-by-proxy proxy-output))
			 (prev-x (output-x output))
			 (prev-y (output-y output))
			 (dx (- x prev-x))
			 (dy (- y prev-y)))
	(%output-x-set! output x)
	(%output-y-set! output y)
	(%output-usable-x-set! output (+ (output-usable-x output) dx))
	(%output-usable-y-set! output (+ (output-usable-y output) dy))
	(gliver-hook-run! *output-position-changed-hook* output prev-x prev-y)))

(define (output-on-output-dimensions data proxy-output width height)
  "Invoked when the output dimension of PROXY-OUTPUT is changed.
Parameters: (DATA PROXY-OUTPUT WIDTH HEIGHT)
  - DATA: User context pointer (unused).
  - PROXY-OUTPUT: Pointer to river_output_v1 proxy.
  - WIDTH: Logical output width in pixels.
  - HEIGHT: Logical output height in pixels."
  (log-info "Output dimensions: ~a = ~ax~a" proxy-output width height)
  (and-let* ((output (output-find-by-proxy proxy-output))
			 (prev-width (output-width output))
			 (prev-height (output-height output)))
	(%output-width-set! output width)
	(%output-height-set! output height)
	(when (= (output-usable-width output) prev-width)
	  (%output-usable-width-set! output width))
	(when (= (output-usable-height output) prev-height)
	  (%output-usable-height-set! output height))
	(gliver-hook-run! *output-dimensions-changed-hook* output prev-width prev-height)))

(gliver-hook-add! %output-created-hook 'output-on-output 0)
(gliver-hook-add! %output-object-id-changed-hook 'output-on-output-wl-output 0)
(gliver-hook-add! %output-position-changed-hook 'output-on-output-position 0)
(gliver-hook-add! %output-dimensions-changed-hook 'output-on-output-dimensions 0)
(gliver-hook-add! %output-geometry-changed-hook 'output-on-wl-output-geometry 0)
(gliver-hook-add! %output-mode-changed-hook 'output-on-wl-output-mode 0)
(gliver-hook-add! %output-done-hook 'output-on-wl-output-done 0)
(gliver-hook-add! %output-scale-changed-hook 'output-on-wl-output-scale 0)
(gliver-hook-add! %output-name-changed-hook 'output-on-wl-output-name 0)
(gliver-hook-add! %output-description-changed-hook 'output-on-wl-output-description 0)
(gliver-hook-add! %output-removed-hook 'output-on-output-removed 0)
