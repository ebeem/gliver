;;; gliver/core/window.scm --- Core data model for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver core window)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (system foreign)
  #:use-module (gliver core types)
  #:use-module (gliver core config)
  #:use-module (gliver core logs)
  #:use-module (gliver core hooks)
  #:use-module (gliver river wm-window-manager)
  ;; lazy loaded, core type functions shouldn't be imported here
  ;; maybe using hooks is a better idea
  #:autoload (gliver core seat) (seat-wm-window-focus)
  #:export (
			window-add!
			window-remove!
			window-move-to-container!
			window-move-to-workspace!
			on-window
			color-hex->rgba
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
			window-size-set!
			))

(define (window-add! window)
  "Add a new window to the display, placing it in the current container."
  (let ((output (output-current))
		(seat (seat-current))
		(container (container-current))
		(proxy-window (window-wl-proxy window)))
	;; TODO: add window to container and focus using current-window?
	;; (when container
	;;   (container-windows-set! container
	;; 						  (cons window (container-windows container)))
	;;   (when *wm-behavior-focus-new-window*
	;; 	(container-window-current-set! container window)))

	;; TODO: this should be handled by the layout instead
	;; apply defaults to window
	(when (and *wm-behavior-focus-new-window* seat)
	  (seat-wm-window-focus seat window))
	(window-capabilities-inform! window *wm-behavior-default-capabilties*)
	(window-unmaximized-inform! window)
    (window-fullscreen-exit-inform! window)
	(window-dimensions-propose! window
								(output-width output)
								(output-height output))
    (window-tiled-set! window *wm-behavior-default-edges*))
  (gliver-hook-run! *window-created-hook* window)
  window)

(define (window-remove! window)
  "Remove a window from the display."
  (let ((container (window-container window)))
    (when container
      (container-windows-set! container (delete window (container-windows container)))
	  ;; if removed window is currently focused
      (when (eq? (container-window-current container) window)
        (container-window-current-set! container
									   (and (pair? (container-windows container))
											(car (container-windows container))))))
    (gliver-hook-run! *window-destroy-hook* window)))

(define (window-move-to-container! window target-container)
  "Move window from its current container to TARGET-CONTAINER."
  (let ((old-container (window-container window)))
    (when old-container
      (container-windows-set! old-container (delete window (container-windows old-container)))
      (when (eq? (container-window-current old-container) window)
        (container-window-current-set! old-container
									   (and (pair? (container-windows old-container))
											(car (container-windows old-container))))))
    (window-container-set! window target-container)
    (container-windows-set! target-container
							(cons window (container-windows target-container)))
    (container-window-current-set! target-container window)
    (gliver-hook-run! *window-place-hook* window target-container)))

(define (window-move-to-workspace! window workspace)
  "Move window to WORKSPACE."
  (let ((old-container (window-container window))
		(new-container (or (workspace-container-current workspace)
						   (last (workspace-containers workspace)))))
    ;; remove from old location
    (when old-container
      (container-windows-set! old-container (delete window (container-windows old-container)))
      (when (eq? (container-window-current old-container) window)
        (container-window-current-set! old-container
									   (and (pair? (container-windows old-container))
											(car (container-windows old-container))))))

    ;; add to new workspace
	(container-windows-set! new-container (delete window (container-windows old-container)))
    (window-container-set! window new-container)))

(define (on-window data manager proxy-window)
  "Handle a new window event from the compositor."
  (let ((window (make-window
				 #:wl-proxy proxy-window
				 #:wl-pending #t)))
    (window-add! window)))
(gliver-hook-add! %window-created-hook on-window)

(define (color-hex->rgba hex-str)
  ;; strip the leading '#' if it exists
  (let* ((clean-str (if (char=? (string-ref hex-str 0) #\#)
                        (substring hex-str 1)
                        hex-str))
         (len (string-length clean-str))
         (get-val (lambda (start)
                    (string->number (substring clean-str start (+ start 2)) 16))))
    (cond
     ((= len 6) ;; rrggbb
      (list (get-val 0) (get-val 2) (get-val 4) 255))
     ((= len 8) ;; rrggbbaa
      (list (get-val 0) (get-val 2) (get-val 4) (get-val 6)))
     (else
      (error "Invalid hex color length. Expected 6 or 8 characters:" hex-str)))))

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
          (window-wl-node-proxy-set! window proxy-node)
          proxy-node))
      cached-node)))

(define (window-dimensions-propose! window width height)
  "Propose dimensions (width and height) for a window.
Must be called in a ~manage_sequence~."
  (when window
    (let ((proxy-window (window-wl-proxy window)))
      (wm-window-dimensions-propose proxy-window width height))))

(define (window-hide! window)
  "Request that the window be hidden.
Must be called in a ~render_sequence~."
  (when window
    (let ((proxy-window (window-wl-proxy window)))
      (wm-window-hide proxy-window))))

(define (window-show! window)
  "Request that the window be shown.
Must be called in a ~render_sequence~."
  (when window
    (let ((proxy-window (window-wl-proxy window)))
      (wm-window-show proxy-window))))

(define (window-decoration-client! window)
  "Enable client-side decoration for the provided window.
Must be called in a ~manage_sequence~."
  (when window
    (let ((proxy-window (window-wl-proxy window)))
      (wm-window-decoration-client proxy-window))))

(define (window-decoration-server! window)
  "Enable server-side decoration for the provided window.
Must be called in a ~manage_sequence~."
  (when window
	(let ((proxy-window (window-wl-proxy window)))
	  (wm-window-decoration-server proxy-window))))

(define (window-borders-set! window edges width color-hex)
  "Set borders for the provided window.
edges: flag enum value, use `RIVER_window_V1_EDGES_NONE`,
`RIVER_window_V1_EDGES_TOP`, `RIVER_window_V1_EDGES_BOTTOM`,
`RIVER_window_V1_EDGES_RIGHT`, `RIVER_window_V1_EDGES_LEFT`
Must be called in a ~render_sequence~."
  (when window
    (let ((proxy-window (window-wl-proxy window)))
      (apply wm-window-borders-set 
             proxy-window edges width 
             (color-hex->rgba color-hex)))))

(define (window-tiled-set! window edges)
  "Set tiled state for the provided window.
edges: flag enum value, use `RIVER_window_V1_EDGES_NONE`,
`RIVER_window_V1_EDGES_TOP`, `RIVER_window_V1_EDGES_BOTTOM`,
`RIVER_window_V1_EDGES_RIGHT`, `RIVER_window_V1_EDGES_LEFT`
Must be called in a ~render_sequence~."
  (when window
	(let ((proxy-window (window-wl-proxy window)))
	  (wm-window-tiled-set proxy-window edges))))

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
          (window-wl-decoration-above-proxy-set! window proxy-decoration)
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
          (window-wl-decoration-below-proxy-set! window proxy-decoration)
          proxy-decoration))
      cached-decoration)))

(define (window-resize-started-inform! window)
  "Inform the window that it is being resized.
Must be called in a ~manage_sequence~."
  (when window
	(let ((proxy-window (window-wl-proxy window)))
      (wm-window-resize-started-inform proxy-window)
      (window-is-resizing-set! window #t)
      (gliver-hook-run! *window-resize-start-hook* window))))

(define (window-resize-ended-inform! window)
  "Inform the window that it has ended resizing.
Must be called in a ~manage_sequence~."
  (when window
	(let ((proxy-window (window-wl-proxy window)))
      (wm-window-resize-ended-inform proxy-window)
      (window-is-resizing-set! window #f)
      (gliver-hook-run! *window-resize-end-hook* window))))

(define (window-capabilities-inform! window caps)
  "inform the window of the capabilities supported (maximize, minimize).
capabilities: flag enum value, use `RIVER_window_V1_CAPABILITIES_window_MENU`,
`RIVER_window_V1_CAPABILITIES_MAXIMIZE`, `RIVER_window_V1_CAPABILITIES_FULLSCREEN`,
`RIVER_window_V1_CAPABILITIES_MINIMIZE`.
Must be called in a ~manage_sequence~."
  (when window
	(let* ((proxy-window (window-wl-proxy window))
           (prev-caps (window-capabilities window)))
      (wm-window-capabilities-inform proxy-window caps)
      (window-capabilities-set! window caps)
      (gliver-hook-run! *window-capabilities-changed-hook* window prev-caps))))

(define (window-maximized-inform! window)
  "inform the window that it has been maximized.
Must be called in a ~manage_sequence~."
  (when window
	(let* ((proxy-window (window-wl-proxy window))
           (prev-status (window-maximized? window)))
	  (wm-window-maximized-inform proxy-window)
	  (window-maximized-set! window #t)
	  (gliver-hook-run! *window-maximized-hook* window prev-status))))

(define (window-unmaximized-inform! window)
  "inform the window that it has been unmaximized.
Must be called in a ~manage_sequence~."
  (when window
	(let* ((proxy-window (window-wl-proxy window))
           (prev-status (window-maximized? window)))
	  (wm-window-unmaximized-inform proxy-window)
	  (window-maximized-set! window #f)
	  (gliver-hook-run! *window-unmaximized-hook* window prev-status))))

(define (window-fullscreen-inform! window)
  "inform the window that it has entered fullscreen mode.
Must be called in a ~manage_sequence~."
  (when window
	(let* ((proxy-window (window-wl-proxy window))
           (prev-status (window-fullscreen? window)))
	  (wm-window-fullscreen-inform proxy-window)
	  (gliver-hook-run! *window-fullscreen-entered-informed-hook* window prev-status))))

(define (window-fullscreen-exit-inform! window)
  "inform the window that it has exited fullscreen mode.
Must be called in a ~manage_sequence~."
  (when window
	(let* ((proxy-window (window-wl-proxy window))
           (prev-status (window-fullscreen? window)))
	  (wm-window-fullscreen-exit-inform proxy-window)
	  (gliver-hook-run! *window-fullscreen-exited-informed-hook* window prev-status))))

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
	  (wm-window-fullscreen proxy-window proxy-output)
	  (window-fullscreen-set! window #t)
	  (gliver-hook-run! *window-fullscreen-entered-hook* window prev-status))))

(define (window-fullscreen-exit! window)
  "Make the window not fullscreen.
This request automatically informs the window that it has exited fullscreen.
Must be called in a ~manage_sequence~."
  (when window
	(let* ((proxy-window (window-wl-proxy window))
           (prev-status (window-fullscreen? window)))
	  (wm-window-fullscreen-exit proxy-window)
	  (window-fullscreen-set! window #f)
	  (gliver-hook-run! *window-fullscreen-entered-hook* window prev-status))))

(define (window-clip-box-set! window x y width height)
  "Clip the window, including borders and decoration surfaces.
Setting a clip box with 0 width or height disables clipping.
Clip box is ignored while window is on fullscreen.
Must be called in a ~manage_sequence~."
  (when window
	(let ((proxy-window (window-wl-proxy window)))
      (wm-window-clip-box-set proxy-window x y width height))))

(define (window-content-clip-box-set! window x y width height)
  "Clip the window, excluding borders and decoration surfaces.
Setting a clip box with 0 width or height disables clipping.
Clip box is ignored while window is on fullscreen.
Must be called in a ~manage_sequence~."
  (when window
    (let ((proxy-window (window-wl-proxy window)))
      (wm-window-content-clip-box-set proxy-window x y width height))))

(define (window-dimension-bounds-set! window max-width max-height)
  "Recommend that the window keep its dimensions within a given width and height.
Setting bounds of 0 width or height indicates there are no bounds (default).
Must be called in a ~manage_sequence~."
  (when window
    (let ((proxy-window (window-wl-proxy window)))
      (wm-window-dimension-bounds-set proxy-window max-width max-height))))

(define (window-size-set! window width height)
  "Resize the window, impact only floating windows and stacking layouts."
  ;; proposing dimensions is only possible in manage sequence
  (let* ((wm-mod (resolve-interface '(gliver river window-manager)))
         (queue (module-ref wm-mod '*wm-manage-queue*))
         (prop-func (module-ref wm-mod 'wm-window-dimensions-propose))
         (task (lambda () (prop-func window width height))))
    (module-set! wm-mod '*wm-manage-queue* (append queue (list task)))))

