;;; gliver/core/window.scm --- Core data model for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver core window)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (system foreign)
  #:use-module (gliver core types)
  #:use-module (gliver core config)
  #:use-module (gliver core logs)
  #:use-module (gliver core hooks)
  #:use-module (gliver river window-manager)
  #:use-module (gliver river wm-window-manager)
  ;; lazy loaded, core type functions shouldn't be imported here
  ;; maybe using hooks is a better idea
  #:autoload (gliver core seat) (seat-wm-window-focus)
  #:export (
			window-next
			window-prev
			window-add!
			window-apply-defaults!
			window-remove!
			%window-container-remove!
			%window-container-add!
			window-focus!
			window-move-to-container!
			color-hex->rgba-32
			window-close!
			window-node-get!
			window-dimensions-propose!
			window-hide!
			window-show!
			window-decoration-client!
			window-decoration-server!
			window-borders-set!
			window-tiled-set!
			window-decoration-above-get!
			window-decoration-below-get!
			window-resize-started-inform!
			window-resize-ended-inform!
			window-capabilities-inform!
			window-maximized-inform!
			window-unmaximized-inform!
			window-fullscreen-inform!
			window-fullscreen-exit-inform!
			window-fullscreen!
			window-fullscreen-exit!
			window-clip-box-set!
			window-content-clip-box-set!
			window-dimension-bounds-set!
			window-position-set!
			on-window
			on-window-closed
			on-window-focused
			on-window-unfocused
			on-window-title-changed
			on-window-parent-changed
			on-window-app-id-changed
			on-window-identifier-changed
			on-window-presentation-hint
			on-window-pid-changed
			on-window-dimensions
			on-window-dimensions-hint
			on-window-decoration-hint
))

(define* (window-next current #:key (recursive #t))
  "Return the next window after CURRENT in WORKSPACE's list."
  (let* ((container (window-container current))
		 (windows (container-windows container))
         (idx (list-index (lambda (f) (eq? f current)) windows)))
    (cond
     ((not idx)
      (and (pair? windows) (car windows)))
     ((and (not recursive) (= (1+ idx) (length windows)))
      #f)
     (else
      (list-ref windows (modulo (1+ idx) (length windows)))))))

(define* (window-prev current #:key (recursive #t))
  "Return the previous window before CURRENT in WORKSPACE's list."
  (let* ((container (window-container current))
		 (windows (container-windows container))
         (idx (list-index (lambda (f) (eq? f current)) windows)))
    (cond
     ((not idx)
      (and (pair? windows) (last windows)))
     ((and (not recursive) (zero? idx))
      #f)
     (else
      (list-ref windows (modulo (1- idx) (length windows)))))))

(define (window-add! window)
  "Add a new window to the display, placing it in the current container."
  (log-debug "adding window ~a" window)
  (let* ((proxy-window (window-wl-proxy window))
		 (container (or (window-container window)
						(container-current)))
		 (all-windows (append (manager-windows *manager*) (list window))))
	;; add window to provided container (current focused if none is provided)
	(%window-container-add! window container
							#:focus *wm-behavior-focus-new-window*)
	;; windows are stored in the manager for quick lookups
	(%manager-windows-set! *manager* all-windows)
	(gliver-hook-run! *window-created-hook* window)))

(define (window-apply-defaults! window)
  "Apply defaults to window, these might be overwritten by layout "
  (let ((output (output-current)))
	(when (eq? *wm-behavior-default-decoration* 'server)
	  (window-decoration-server! window))
	(when (eq? *wm-behavior-default-decoration* 'client)
	  (window-decoration-client! window))
	(window-capabilities-inform! window *wm-behavior-default-capabilties*)	
	(window-unmaximized-inform! window)
    (window-fullscreen-exit-inform! window)
    (window-tiled-set! window *wm-behavior-default-edges*)
	(window-dimensions-propose! window
								(output-width output)
								(output-height output))))

(define (window-remove! window)
  "Remove a window from the display."
  (let ((remaining-windows (delete window (manager-windows *manager*))))
    (%manager-windows-set! *manager* remaining-windows)
	;; make sure the window is removed properly from container so
	;; another window is focused
	(%window-container-remove! window)
    (gliver-hook-run! *window-destroy-hook* window)))

(define* (%window-container-remove! window #:key (focus #t))
  "Remove a window from the container. This function makes the window state
invalid as the window should always have a container."
  (and-let* ((container (window-container window))
			 (container-remaining (delete window (container-windows container)))
			 (window-target (or (window-next window #:recursive #f)
								(window-prev window #:recursive #f))))
      (%window-container-set! window #f)
      (%container-windows-set! container container-remaining)
	  (gliver-hook-run! %window-container-removed-hook* window container)
	  ;; if removed window is currently focused, focus next window in container
      (when (and window-target
				 (eq? (container-window-current container) window))
		;; if focus parameter is true, another window in the container will be focused
		;; otherwise the container will just have it as current window without seat focusing it
		(if focus
			(window-focus! window-target)
			(%container-window-current-set! container window-target)))))

(define* (%window-container-add! window container #:key (focus #t))
  "Add a window to a container. The window should have no container. it's
always better to call ~%window-container-remove!~ before calling this function."
  (let ((container-windows (append (container-windows container) (list window))))
	(%window-container-set! window container)
	(%container-windows-set! container container-windows)
	(gliver-hook-run! %window-container-added-hook* window container)
	;; if focus parameter is true, window will be focused, otherwise the
	;; container will just have it as current window without seat focusing it
	(if focus
		(window-focus! window)
		(%container-window-current-set! container window))))

(define (window-focus! window)
  "Focus a window from the display."
  (let ((seat (seat-current)))
	(when seat
	  (seat-wm-window-focus seat window)
	  (%container-window-current-set! (window-container window) window)
	  (gliver-hook-run! *window-focused-hook* window))))

(define* (window-move-to-container! window container #:key (focus #t))
  "Move a window to a container."
  (let ((container-current (window-container window)))
	;; window should be removed from current container first
	(log-debug "moving window ~a to container ~a" window container)
	(when container-current
	  (%window-container-remove! window #:focus #f))
	;; moves the window to the new container
	(%window-container-add! window container #:focus focus)
	(window-position-set! window
						  (container-x container)
						  (container-y container))
	(window-dimensions-propose! window
								(container-width container)
								(container-height container))
	(gliver-hook-run! *window-container-moved-hook* window container container-current)))

(define (color-hex->rgba-32 hex-str)
  ;; strip the leading '#' if it exists
  (let* ((clean-str (if (char=? (string-ref hex-str 0) #\#)
                        (substring hex-str 1)
                        hex-str))
         (len (string-length clean-str))
         (get-val (lambda (start)
                    (string->number (substring clean-str start (+ start 2)) 16)))
         ;; multiplier to stretch 0-255 into 0-4294967295
         (scale 16843009)) 
    (cond
     ((or (= len 6) (= len 8))
      (let* ((r (get-val 0))
             (g (get-val 2))
             (b (get-val 4))
             (a (if (= len 8) (get-val 6) 255))
             
             ;; calculate pre-multiplied 32-bit values using exact integers.
			 ;; river uses 32-bit colors rather than 8-bit
             (a-32 (* a scale))
             (r-32 (quotient (* r a scale) 255))
             (g-32 (quotient (* g a scale) 255))
             (b-32 (quotient (* b a scale) 255)))
        
        (list r-32 g-32 b-32 a-32)))
     (else
      (log-error "Invalid hex color length. Expected 6 or 8 characters: ~a" hex-str)
	  (list 4294967295 4294967295 4294967295 4294967295)))))

(define (window-close! window)
  "Close a WINDOW, the window may take time to respond or
completely ignore the request. listen for *window-destroy-hook*
in case an action other than clearing state needs to be executed."
  (when window
    (let ((proxy-window (window-wl-proxy window)))
      (wm-window-close proxy-window))))

(define (window-node-get! window)
  "Return node that corresponds to the window, This can only be
called once per window, so it should be cached after first call.
This most likely should be used internally only, and it
will be automatically managed and called when needed and
window record will be updated accordingly to have a node reference."
  (when window
    (let ((proxy-window (window-wl-proxy window))
          (cached-node (window-wl-node-proxy window)))
      (unless cached-node
        (let ((proxy-node (wm-window-node-get proxy-window)))
          (log-debug "Setting node of window: ~a to ~a" proxy-window proxy-node)
          (%window-wl-node-proxy-set! window proxy-node)
          proxy-node))
      cached-node)))

(define* (window-dimensions-propose! window width height #:key (animate #t))
  "Propose dimensions (width and height) for a window.
Must be called in a ~manage_sequence~."
  (when window
    (let ((proxy-window (window-wl-proxy window)))
	  (with-manage-sequence
	   (wm-window-dimensions-propose proxy-window
									 (inexact->exact (floor width))
									 (inexact->exact (floor height)))))))

(define (window-hide! window)
  "Request that the window be hidden.
Must be called in a ~render_sequence~."
  (when window
    (let ((proxy-window (window-wl-proxy window)))
      (with-render-sequence
	   (%window-visbile-set! window #f)
	   (wm-window-hide proxy-window)))))

(define (window-show! window)
  "Request that the window be shown.
Must be called in a ~render_sequence~."
  (when window
    (let ((proxy-window (window-wl-proxy window)))
      (with-render-sequence
	   (%window-visbile-set! window #t)
	   (wm-window-show proxy-window)))))

(define (window-decoration-client! window)
  "Enable client-side decoration for the provided window.
Must be called in a ~manage_sequence~."
  (when window
    (let ((proxy-window (window-wl-proxy window)))
      (with-manage-sequence
	   (wm-window-decoration-client proxy-window)))))

(define (window-decoration-server! window)
  "Enable server-side decoration for the provided window.
Must be called in a ~manage_sequence~."
  (when window
	(let ((proxy-window (window-wl-proxy window)))
	  (with-manage-sequence
	   (wm-window-decoration-server proxy-window)))))

(define (window-borders-set! window edges width color-hex)
  "Set borders for the provided window.
edges: flag enum value, use `RIVER_WINDOW_V1_EDGES_NONE`,
`RIVER_WINDOW_V1_EDGES_TOP`, `RIVER_WINDOW_V1_EDGES_BOTTOM`,
`RIVER_WINDOW_V1_EDGES_RIGHT`, `RIVER_WINDOW_V1_EDGES_LEFT`
Must be called in a ~render_sequence~."
  (log-debug "Setting border color of window: ~a to ~a" window color-hex)
  (when window
    (let ((proxy-window (window-wl-proxy window)))
      (with-render-sequence
	   (apply wm-window-borders-set 
              proxy-window edges width 
              (color-hex->rgba-32 color-hex))))))

(define (window-tiled-set! window edges)
  "Set tiled state for the provided window.
edges: flag enum value, use `RIVER_window_V1_EDGES_NONE`,
`RIVER_window_V1_EDGES_TOP`, `RIVER_window_V1_EDGES_BOTTOM`,
`RIVER_window_V1_EDGES_RIGHT`, `RIVER_window_V1_EDGES_LEFT`
Must be called in a ~manage_sequence~."
  (when window
	(let ((proxy-window (window-wl-proxy window)))
	  (with-manage-sequence
	   (wm-window-tiled-set proxy-window edges)))))

(define (window-decoration-above-get! window proxy-surface)
  "Create a decoration surface above the window and
assign the river_decoration_v1 role to the surface.
Provided ~wl_surface~ shouldn't have a role or a buffer attached."
  (when window
    (let ((proxy-window (window-wl-proxy window))
          (cached-decoration (window-wl-decoration-above-proxy window)))
      (unless cached-decoration
        (let ((proxy-decoration (wm-window-decoration-above-get proxy-window proxy-surface)))
          (log-debug "Setting above decoration of window: ~a to ~a" proxy-window proxy-decoration)
          (%window-wl-decoration-above-proxy-set! window proxy-decoration)
          proxy-decoration))
      cached-decoration)))

(define (window-decoration-below-get! window proxy-surface)
  "Create a decoration surface below the window and
assign the river_decoration_v1 role to the surface.
Provided ~wl_surface~ shouldn't have a role or a buffer attached."
  (when window
    (let ((proxy-window (window-wl-proxy window))
          (cached-decoration (window-wl-decoration-below-proxy window)))
      (unless cached-decoration
        (let ((proxy-decoration (wm-window-decoration-below-get proxy-window proxy-surface)))
          (log-debug "Setting below decoration of window: ~a to ~a" proxy-window proxy-decoration)
          (%window-wl-decoration-below-proxy-set! window proxy-decoration)
          proxy-decoration))
      cached-decoration)))

(define (window-resize-started-inform! window)
  "Inform the window that it is being resized.
Must be called in a ~manage_sequence~."
  (when window
	(let ((proxy-window (window-wl-proxy window)))
      (with-manage-sequence
	   (wm-window-resize-started-inform proxy-window)
       (%window-is-resizing-set! window #t)
       (gliver-hook-run! *window-resize-start-hook* window)))))

(define (window-resize-ended-inform! window)
  "Inform the window that it has ended resizing.
Must be called in a ~manage_sequence~."
  (when window
	(let ((proxy-window (window-wl-proxy window)))
      (with-manage-sequence
	   (wm-window-resize-ended-inform proxy-window)
       (%window-is-resizing-set! window #f)
       (gliver-hook-run! *window-resize-end-hook* window)))))

(define (window-capabilities-inform! window caps)
  "inform the window of the capabilities supported (maximize, minimize).
capabilities: flag enum value, use `RIVER_window_V1_CAPABILITIES_window_MENU`,
`RIVER_window_V1_CAPABILITIES_MAXIMIZE`, `RIVER_window_V1_CAPABILITIES_FULLSCREEN`,
`RIVER_window_V1_CAPABILITIES_MINIMIZE`.
Must be called in a ~manage_sequence~."
  (when window
	(let* ((proxy-window (window-wl-proxy window))
           (prev-caps (window-capabilities window)))
      (with-manage-sequence
	   (wm-window-capabilities-inform proxy-window caps)
       (%window-capabilities-set! window caps)
       (gliver-hook-run! *window-capabilities-changed-hook* window prev-caps)))))

(define (window-maximized-inform! window)
  "inform the window that it has been maximized.
Must be called in a ~manage_sequence~."
  (when window
	(let* ((proxy-window (window-wl-proxy window))
           (prev-status (window-maximized? window)))
	  (with-manage-sequence
	   (wm-window-maximized-inform proxy-window)
	   (%window-maximized-set! window #t)
	   (gliver-hook-run! *window-maximized-hook* window prev-status)))))

(define (window-unmaximized-inform! window)
  "inform the window that it has been unmaximized.
Must be called in a ~manage_sequence~."
  (when window
	(let* ((proxy-window (window-wl-proxy window))
           (prev-status (window-maximized? window)))
	  (with-manage-sequence
	   (wm-window-unmaximized-inform proxy-window)
	   (%window-maximized-set! window #f)
	   (gliver-hook-run! *window-unmaximized-hook* window prev-status)))))

(define (window-fullscreen-inform! window)
  "inform the window that it has entered fullscreen mode.
Must be called in a ~manage_sequence~."
  (when window
	(let* ((proxy-window (window-wl-proxy window))
           (prev-status (window-fullscreen? window)))
	  (with-manage-sequence
	   (wm-window-fullscreen-inform proxy-window)
	   (gliver-hook-run! *window-fullscreen-entered-informed-hook* window prev-status)))))

(define (window-fullscreen-exit-inform! window)
  "inform the window that it has exited fullscreen mode.
Must be called in a ~manage_sequence~."
  (when window
	(let* ((proxy-window (window-wl-proxy window))
           (prev-status (window-fullscreen? window)))
	  (with-manage-sequence
	   (wm-window-fullscreen-exit-inform proxy-window)
	   (gliver-hook-run! *window-fullscreen-exited-informed-hook* window prev-status)))))

(define (window-fullscreen! window output)
  "Make the window fullscreen on the given output. river_shell_surface_v1
objects above the window will still be rendered (move it to top).
This request automatically informs the window that it has entered fullscreen.
output: output object from which a proxy will be retrieved.
Must be called in a ~manage_sequence~."
  (when window
	(let* ((proxy-window (window-wl-proxy window))
		   (proxy-output (output-wl-proxy output))
           (prev-status (window-fullscreen? window)))
	  (with-manage-sequence
	   (wm-window-fullscreen proxy-window proxy-output)
	   (%window-fullscreen-set! window #t)
	   (gliver-hook-run! *window-fullscreen-entered-hook* window prev-status)))))

(define (window-fullscreen-exit! window)
  "Make the window not fullscreen.
This request automatically informs the window that it has exited fullscreen.
Must be called in a ~manage_sequence~."
  (when window
	(let* ((proxy-window (window-wl-proxy window))
           (prev-status (window-fullscreen? window)))
	  (with-manage-sequence
	   (wm-window-fullscreen-exit proxy-window)
	   (%window-fullscreen-set! window #f)
	   (gliver-hook-run! *window-fullscreen-entered-hook* window prev-status)))))

(define (window-clip-box-set! window x y width height)
  "Clip the window, including borders and decoration surfaces.
Setting a clip box with 0 width or height disables clipping.
Clip box is ignored while window is on fullscreen.
Must be called in a ~manage_sequence~."
  (when window
	(let ((proxy-window (window-wl-proxy window)))
      (with-manage-sequence
	   (wm-window-clip-box-set proxy-window x y width height)))))

(define (window-content-clip-box-set! window x y width height)
  "Clip the window, excluding borders and decoration surfaces.
Setting a clip box with 0 width or height disables clipping.
Clip box is ignored while window is on fullscreen.
Must be called in a ~manage_sequence~."
  (when window
    (let ((proxy-window (window-wl-proxy window)))
      (with-manage-sequence
	   (wm-window-content-clip-box-set proxy-window x y width height)))))

(define (window-dimension-bounds-set! window max-width max-height)
  "Recommend that the window keep its dimensions within a given width and height.
Setting bounds of 0 width or height indicates there are no bounds (default).
Must be called in a ~manage_sequence~."
  (when window
    (let ((proxy-window (window-wl-proxy window)))
      (with-manage-sequence
	   (wm-window-dimension-bounds-set proxy-window max-width max-height)))))

(define* (window-position-set! window x y #:key (animate #t))
  "Set the position of the window.
Must be called in a ~render_sequence~."
  (let ((node (window-node-get! window))
		(int-x (inexact->exact (floor x)))
		(int-y (inexact->exact (floor y))))
	(when node
	  (%window-x-set! window int-x)
	  (%window-y-set! window int-y)
      (with-render-sequence
       ((@ (gliver river wm-node-manager) wm-node-position-set!) node int-x int-y)))))

(define (on-window data manager proxy-window)
  "Handle a new window event from the compositor."
  (let ((window (make-window
				 #:wl-proxy proxy-window)))
    (window-add! window)))
(gliver-hook-add! %window-created-hook on-window)

(define (on-window-closed data proxy-window)
  "Handle a new window event from the compositor."
  (let ((window (window-find-by-proxy proxy-window)))
    (window-remove! window)))
(gliver-hook-add! %window-destroy-hook on-window-closed)

(define (on-window-focused window)
  "Handle window focused event."
  ;; colorize the window border with active window border color
  (window-borders-set! window *wm-behavior-default-border-edges*
					   (manager-config-ref 'border-width)
					   (manager-config-ref 'border-color-focused)))
(gliver-hook-add! *window-focused-hook* on-window-focused)

(define (on-window-unfocused window)
  "Handle window unfocused event."
  ;; colorize the window border with inactive window border color
  (window-borders-set! window *wm-behavior-default-border-edges*
					   (manager-config-ref 'border-width)
					   (manager-config-ref 'border-color-unfocused)))
(gliver-hook-add! *window-unfocused-hook* on-window-unfocused)

(define (on-window-title-changed proxy-window title)
  (let ((window (window-find-by-proxy proxy-window)))
    (when window
      (%window-title-set! window title))))
(gliver-hook-add! %window-title-changed-hook on-window-title-changed)

(define (on-window-parent-changed proxy-window parent)
  (let ((window (window-find-by-proxy proxy-window)))
    (when window
      (%window-parent-set! window parent))))
(gliver-hook-add! %window-parent-changed-hook on-window-parent-changed)

(define (on-window-app-id-changed proxy-window app-id)
  (let ((window (window-find-by-proxy proxy-window)))
    (when window
      (%window-app-id-set! window app-id))))
(gliver-hook-add! %window-app-id-changed-hook on-window-app-id-changed)

(define (on-window-identifier-changed proxy-window identifier)
  (let ((window (window-find-by-proxy proxy-window)))
    (when window
      (%window-identifier-set! window identifier))))
(gliver-hook-add! %window-identifier-changed-hook on-window-identifier-changed)

(define (on-window-presentation-hint data proxy-window hint)
  (let ((window (window-find-by-proxy proxy-window)))
    (when window
      (%window-presentation-hint-set! window hint))))
(gliver-hook-add! %window-presentation-hint-changed-hook on-window-presentation-hint)

(define (on-window-pid-changed proxy-window pid)
  (let ((window (window-find-by-proxy proxy-window)))
    (when window
      (%window-pid-set! window pid))))
(gliver-hook-add! %window-pid-changed-hook on-window-pid-changed)

(define (on-window-dimensions proxy-window width height)
  (let ((window (window-find-by-proxy proxy-window)))
    (when window
	  (%window-width-set! window width)
	  (%window-height-set! window height))))
(gliver-hook-add! %window-size-changed-hook on-window-dimensions)

(define (on-window-dimensions-hint proxy-window min-w min-h max-w max-h)
  (let ((window (window-find-by-proxy proxy-window)))
    (when window
	  (%window-width-min-set! window min-w)
	  (%window-height-min-set! window min-h)
	  (%window-width-max-set! window max-w)
	  (%window-height-max-set! window max-h))))
(gliver-hook-add! %window-size-hint-changed-hook on-window-dimensions-hint)

(define (on-window-decoration-hint proxy-window hint)
  (let ((window (window-find-by-proxy proxy-window)))
    (when window
	  (%window-decoration-hint-set! window hint))))
(gliver-hook-add! %window-decoration-hint-changed-hook on-window-decoration-hint)
