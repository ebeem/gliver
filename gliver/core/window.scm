;;; gliver/core/window.scm --- Window management for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module manages client application windows
;;; It handles:
;;;  - Adding, removing, focusing, and reparenting windows
;;;  - surface layout properties (position, dimensions, tiling, borders, clipping)
;;;  - Transitioning window state (fullscreen, maximized, minimized, urgent, transient)
;;;  - Deciding window decoration (server-side and client-side)
;;;  - Updating window properties (e.g. title, app-id, dimensions, and parent)

(define-module (gliver core window)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (gliver core-base)
  #:use-module (gliver deps color)
  #:use-module (gliver river window-manager)
  #:use-module (gliver river wm-window-manager)
  #:use-module (gliver river wm-node-manager)
  #:use-module (gliver river wm-decoration-manager)
  ;; lazy loaded, core type functions shouldn't be imported here
  ;; maybe using hooks is a better idea
  #:autoload (gliver core seat) (seat-wm-window-focus)
  #:autoload (gliver core container) (container-focus!
									  container-focused?
									  container-prev
									  container-next)
  #:autoload (gliver core workspace) (workspace-manual?
									  workspace-windows
									  workspace-visible?)
  #:declarative? #f
  #:export (
			*window-focus-new?*
			*window-default-decoration*
			*window-default-capabilties*
			*window-default-edges*
			*window-default-border-edges*
			*window-container-navigation-recursive?*
			window-next
			window-prev
			window-add!
			window-apply-defaults!
			window-remove!
			window-focus!
			window-move-to-container!
			window-move-to-workspace!
			window-close!
			window-node-get!
			window-place-top!
			window-place-above!
			window-place-bottom!
			window-place-below!
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
			window-on-window
			window-on-window-closed
			window-on-window-focused
			window-on-window-unfocused
			window-on-window-title-changed
			window-on-window-parent-changed
			window-on-window-app-id-changed
			window-on-window-identifier-changed
			window-on-window-presentation-hint
			window-on-window-pid-changed
			window-on-window-dimensions
			window-on-window-dimensions-hint
			window-on-window-decoration-hint
			window-on-seat-window-focused
			window-on-workspace-switch
			window-on-fullscreen-requested
			window-on-fullscreen-exit-requested
			window-on-minimize-requested
))

;;; window variables and behavior
(define-var *window-focus-new?* #t
			"Whether newly created windows are automatically focused.")
(define-var *window-default-decoration* 'server
			"Default window decoration mode ('server or 'client).")
(define-var *window-default-capabilties* 4
			"Default capability bitmask advertised to client surfaces (e.g. fullscreen capability).")
(define-var *window-default-edges* 15
			"Default tiled window edge bitmask for tiling layouts.")
(define-var *window-default-border-edges* 15
			"Default window border edge bitmask.")
(define-var *window-container-navigation-recursive?* #t
			"if #t, window-next and window-prev will loop after reaching first/last window.")

;;; window management
(define* (window-next current #:key (recursive *window-container-navigation-recursive?*))
  "Return the next window after CURRENT in its container's window list.
Parameters: (CURRENT [#:recursive #t])
  - CURRENT: <window> record.
  - RECURSIVE: Boolean, whether to wrap around at list boundary (default *window-container-navigation-recursive?*).
Returns next <window> record, or #f if not found."
  (and-let* ((_ (window? current))
			 (container (window-container current))
			 (windows (container-windows container))
			 (idx (list-index (lambda (f) (eq? f current)) windows)))
    (cond
     ((not idx)
      (and (pair? windows) (car windows)))
     ((and (not recursive) (= (1+ idx) (length windows)))
      #f)
     (else
      (list-ref windows (modulo (1+ idx) (length windows)))))))

(define* (window-prev current #:key (recursive *window-container-navigation-recursive?*))
  "Return the previous window before CURRENT in its container's window list.
Parameters: (CURRENT [#:recursive #t])
  - CURRENT: <window> record.
  - RECURSIVE: Boolean, whether to wrap around at list boundary (default *window-container-navigation-recursive?*).
Returns previous <window> record, or #f if not found."
  (and-let* ((_ (window? current))
			 (container (window-container current))
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
  "Register WINDOW in Gliver's manager hierarchy.
Parameters: (WINDOW)
  - WINDOW: <window> record to register."
  (log-info "adding window ~a" window)
  (let* ((proxy-window (window-wl-proxy window))
		 (container (or (window-container window)
						(container-current)))
		 (all-windows (append (manager-windows *manager*) (list window))))
	;; apply default configurations to window (e.g. decoration, capabilities, tiled, border, etc.)
	(window-apply-defaults! window)
	;; windows are stored in the manager for quick lookups
	(%manager-windows-set! *manager* all-windows)
	;; move window to the given container
	(window-move-to-container! window container #:focus *window-focus-new?*)
	(gliver-hook-run! *window-created-hook* window)))

(define (window-apply-defaults! window)
  "Apply defaults to window, these might be overwritten by layout.
Parameters: (WINDOW)
  - WINDOW: <window> record to configure."
  (let ((output (output-current)))
	(when (eq? *window-default-decoration* 'server)
	  (window-decoration-server! window))
	(when (eq? *window-default-decoration* 'client)
	  (window-decoration-client! window))
	(window-capabilities-inform! window *window-default-capabilties*)
	(window-unmaximized-inform! window)
    (window-fullscreen-exit-inform! window)
    (window-tiled-set! window *window-default-edges*)
    (window-borders-set! window *window-default-border-edges*
                         *window-border-width*
                         *window-border-color-unfocused*)))

(define (window-remove! window)
  "Remove a window from Gliver and release its proxies.
Parameters: (WINDOW)
  - WINDOW: <window> record to remove."
  (let* ((remaining-windows (delq window (manager-windows *manager*)))
		 (container (window-container window))
		 (workspace (if container (container-workspace container) #f))
		 (node (window-wl-node-proxy window))
		 (dec-above (window-wl-decoration-above-proxy window))
		 (dec-below (window-wl-decoration-below-proxy window)))
    (%manager-windows-set! *manager* remaining-windows)
	(%window-container-remove! window)
	;; marking window as destroyed to tell other functions not to use it
	(%window-destroyed-set! window #t)
	(when (valid-pointer? node)
	  (wm-node-destroy! node)
	  (%window-wl-node-proxy-set! window #f))
	(when (valid-pointer? dec-above)
	  (wm-decoration-destroy! dec-above)
	  (%window-wl-decoration-above-proxy-set! window #f))
	(when (valid-pointer? dec-below)
	  (wm-decoration-destroy! dec-below)
	  (%window-wl-decoration-below-proxy-set! window #f))
    (gliver-hook-run! *window-destroyed-hook* window container workspace)))

(define* (%window-container-remove! window #:key (focus #t))
  "Remove a window from its container.
Parameters: (WINDOW [#:focus #t])
  - WINDOW: <window> record to remove from its container.
  - FOCUS: Boolean, whether to focus the next window in the container (default #t)."
  (let* ((container (window-container window))
		 (workspace (if container (container-workspace container) #f))
		 (windows (if container (container-windows container) '()))
		 (idx (list-index (lambda (w) (eq? w window)) windows))
		 (windows-remaining (delq window windows))
		 (window-target (and (pair? windows-remaining)
							 (if (and idx (< idx (length windows-remaining)))
								 (list-ref windows-remaining idx)
								 (last windows-remaining)))))
    (%window-container-set! window #f)
    (when container
	  (%container-windows-set! container windows-remaining)
	  (gliver-hook-run! *window-container-removed-hook* window container)
	  ;; if removed window is currently focused, focus another window
	  (when (eq? (container-window-current container) window)
		(%container-window-current-set! container window-target)
		(cond
		 ;; if we have a target window, focus it
		 (window-target
		  (log-debug "current window deleted, focusing next window in same container: ~a" window-target)
		  (when focus
			(window-focus! window-target)))
		 ;; in manual layout, keep container focused as it won't be removed
		 ((workspace-manual? workspace)
		  (log-debug "all windows removed from container ~a in manual layout, keeping container focused"
					 (container-id container)))
		 ;; in automatic layout, focus next container's window if available
		 (else
		  (let* ((container-target (or (container-next container #:recursive #f)
									   (container-prev container #:recursive #f)))
				 (target-win (if container-target (container-window-current container-target) #f)))
			(when (and focus target-win)
			  (window-focus! target-win)))))))))

(define* (%window-container-add! window container #:key (focus #t))
  "Add a window to a container.
Parameters: (WINDOW CONTAINER [#:focus #t])
  - WINDOW: <window> record to add.
  - CONTAINER: <container> record to add WINDOW to.
  - FOCUS: Boolean, whether to focus WINDOW (default #t)."
  ;; it's an error to add a destroyed window to container
  ;; it will be hard to force all callers to ensure that the window
  ;; they pass isn't destroyed, so the validation is done here as well
  (unless (window-destroyed? window)
	;; remove window from old container (if exist)
	(when (window-container window)
	  (%window-container-remove! window #:focus focus))
	;; add window to new container
	(let ((container-windows (append (container-windows container) (list window))))
	  (%window-container-set! window container)
	  (%container-windows-set! container container-windows)
	  (gliver-hook-run! *window-container-added-hook* window container)
	  ;; if focus parameter is true, window will be focused
	  (if focus
		  (window-focus! window)
		  ;; set window as current for container if it's empty
		  (unless (container-window-current container)
			(%container-window-current-set! container window))))))

(define* (window-focus! window #:key (seat (seat-current)) (focus-parent #t) (force #f))
  "Focus a window so it receives seat input.
Parameters: (WINDOW [#:seat (seat-current)] [#:focus-parent #t] [#:force #f])
  - WINDOW: <window> record to focus.
  - SEAT: <seat> record directing input (defaults to current seat).
  - FOCUS-PARENT: Boolean, whether to focus parent container (default #t).
  - FORCE: Boolean, whether to force refocusing even if already focused (default #f)."
  (let ((container (and (window? window)
						(window-container window)))
        (current (window-current)))
    (and-let* (((window? window))
               ((not (window-destroyed? window)))
               ((or force
                    (not (and (seat? seat)
                              (eq? (seat-window-focused seat) window)
                              (eq? current window)
                              (container-focused? container))))))
	  ;; set window as current for its container to focus it if it's
	  ;; focusing another window
      (when (container? container)
        (%container-window-current-set! container window)
		;; also focus container if it's not focused and focus-parent it #t
        (when focus-parent
          (container-focus! container #:focus-child #f)))
      ;; place window at top so it becomes visible above other windows
      (window-place-top! window)
      ;; unfocus previous window if switching to a different one
      (when (and (window? current)
				 (not (eq? current window)))
        (gliver-hook-run! *window-unfocused-hook* current))
	  ;; make seat focus the window so input is forwarded to it
      (when (seat? seat)
        (seat-wm-window-focus seat window))
      (gliver-hook-run! *window-focused-hook* window))))

(define* (window-move-to-container! window container #:key (focus #t))
  "Move WINDOW to CONTAINER.
Parameters: (WINDOW CONTAINER [#:focus #t])
  - WINDOW: <window> record to move.
  - CONTAINER: Target <container> record.
  - FOCUS: Boolean, whether to focus WINDOW in the new container (default #t)."
  (let ((container-current (window-container window)))
	(log-debug "moving window ~a to container ~a with focus=~a" window container focus)
	;; moves the window to the new container
	(unless (eq? container-current container)
	  (%window-container-add! window container #:focus focus))
	(log-debug "set window=~a geometry to ~ax~a+~a+~a"
			   window
			   (container-usable-width container)
			   (container-usable-height container)
			   (container-usable-x container)
			   (container-usable-y container))
	;; update window position and size to match its container usable geometry
	(window-position-set! window
						  (container-usable-x container)
						  (container-usable-y container))
	(window-dimensions-propose! window
								(container-usable-width container)
								(container-usable-height container))
	(gliver-hook-run! *window-container-moved-hook* window container container-current)))

(define* (window-move-to-workspace! window workspace #:key (focus #t))
  "Move WINDOW to WORKSPACE, and place it in WORKSPACE's active container.
Parameters: (WINDOW WORKSPACE [#:focus #t])
  - WINDOW: <window> record to move.
  - WORKSPACE: Target <workspace> record.
  - FOCUS: Boolean, whether to focus WINDOW in the new workspace (default #t)."
  (and-let* (((window? window))
			 ((not (window-destroyed? window)))
			 ((workspace? workspace))
			 (output (workspace-output workspace))
			 (containers (workspace-containers workspace))
			 (container (or (workspace-container-current workspace)
							(and (pair? containers) (car containers)))))
	;; set container as current in workspace if it's not already
	;; this shouldn't happen usually, container should be current in workspace
    (unless (workspace-container-current workspace)
      (%workspace-container-current-set! workspace container))
	;; move the window to the target container (current in workspace)
    (window-move-to-container! window container #:focus focus)
	;; show or hide window base on workspace state
    (if (workspace-visible? workspace)
        (window-show! window)
        (window-hide! window))))

(define (window-close! window)
  "Close a WINDOW. The window may take time to respond or completely ignore the request.
Parameters: (WINDOW)
  - WINDOW: <window> record to close."
  (and-let* (((window? window))
			 (proxy-window (window-wl-proxy window)))
    (wm-window-close proxy-window)))

(define (window-node-get! window)
  "Return river_node_v1 proxy that corresponds to the window, caching it.
Parameters: (WINDOW)
  - WINDOW: <window> record.
Returns river_node_v1 proxy pointer."
  (and-let* (((window? window))
			 (proxy-window (window-wl-proxy window)))
	;; return cached node if it's available
	(or (window-wl-node-proxy window)
		;; otherwise get a new node proxy, store it and return it
		(let ((proxy-node (wm-window-node-get proxy-window)))
		  (log-debug "Setting node of window: ~a to ~a" proxy-window proxy-node)
		  (%window-wl-node-proxy-set! window proxy-node)
		  proxy-node))))

(define (window-place-top! window)
  "Place WINDOW on top of all other windows.
Parameters: (WINDOW)
  - WINDOW: <window> record."
  (and-let* ((node (window-node-get! window))
			 (container (window-container window))
			 (container-node (container-wl-node-proxy container))
			 ((valid-pointer? container-node)))
	(with-render-sequence
	 (wm-node-place-top! node)
	 (wm-node-place-below! node container-node))))

(define (window-place-above! window window-other)
  "Place WINDOW above WINDOW-OTHER.
Parameters: (WINDOW WINDOW-OTHER)
  - WINDOW: <window> record.
  - WINDOW-OTHER: Reference <window> record."
  (and-let* ((node (window-node-get! window))
			 (node-other (window-node-get! window-other)))
    (with-render-sequence
     (wm-node-place-above! node node-other))))

(define (window-place-bottom! window)
  "Place WINDOW on below all other windows.
Parameters: (WINDOW)
  - WINDOW: <window> record."
  (and-let* ((node (window-node-get! window)))
    (with-render-sequence
     (wm-node-place-bottom! node))))

(define (window-place-below! window window-other)
  "Place WINDOW below WINDOW-OTHER.
Parameters: (WINDOW WINDOW-OTHER)
  - WINDOW: <window> record.
  - WINDOW-OTHER: Reference <window> record."
  (and-let* ((node (window-node-get! window))
			 (node-other (window-node-get! window-other)))
    (with-render-sequence
     (wm-node-place-below! node node-other))))

(define* (window-dimensions-propose! window width height #:key (animate #t))
  "Propose dimensions (WIDTH and HEIGHT) for a WINDOW.
Parameters: (WINDOW WIDTH HEIGHT [#:animate #t])
  - WINDOW: <window> record.
  - WIDTH: Proposed width in pixels.
  - HEIGHT: Proposed height in pixels.
  - ANIMATE: Boolean, whether to animate the dimension change (default #t)."
  (and-let* (((window? window))
			 (proxy-window (window-wl-proxy window))
			 ;; round and convert to integers
			 (int-w (inexact->exact (floor width)))
			 (int-h (inexact->exact (floor height))))
	(with-manage-sequence
	 (wm-window-dimensions-propose proxy-window int-w int-h))))

(define (window-hide! window)
  "Hide WINDOW.
Parameters: (WINDOW)
  - WINDOW: <window> record to hide."
  (and-let* (((window? window))
			 (proxy-window (window-wl-proxy window)))
    (with-render-sequence
	 (%window-visbile-set! window #f)
	 (wm-window-hide proxy-window))))

(define (window-show! window)
  "Show WINDOW.
Parameters: (WINDOW)
  - WINDOW: <window> record to show."
  (and-let* (((window? window))
			 (proxy-window (window-wl-proxy window)))
	(with-render-sequence
	 (%window-visbile-set! window #t)
	 (wm-window-show proxy-window))))

(define (window-decoration-client! window)
  "Enable client-side decoration for WINDOW.
Parameters: (WINDOW)
  - WINDOW: <window> record."
  (and-let* (((window? window))
			 (proxy-window (window-wl-proxy window)))
    (with-manage-sequence
	 (wm-window-decoration-client proxy-window))))

(define (window-decoration-server! window)
  "Enable server-side decoration for WINDOW.
Parameters: (WINDOW)
  - WINDOW: <window> record."
  (and-let* (((window? window))
			 (proxy-window (window-wl-proxy window)))
	(with-manage-sequence
	 (wm-window-decoration-server proxy-window))))

(define (window-borders-set! window edges width color-hex)
  "Set border state for WINDOW.
Parameters: (WINDOW EDGES WIDTH COLOR-HEX)
  - WINDOW: <window> record.
  - EDGES: Flag enum bitmask (e.g. RIVER_WINDOW_V1_EDGES_TOP).
  - WIDTH: Border width in pixels.
  - COLOR-HEX: Hex color string (e.g. \"#ff0000\")."
  (and-let* (((window? window))
			 (proxy-window (window-wl-proxy window)))
	(log-debug "Setting border color of window: ~a to ~a" window color-hex)
    (with-render-sequence
	 (apply wm-window-borders-set
            proxy-window edges width
            (color-hex->rgba-32 color-hex)))))

(define (window-tiled-set! window edges)
  "Set tiled state for WINDOW.
Parameters: (WINDOW EDGES)
  - WINDOW: <window> record.
  - EDGES: Flag enum bitmask (e.g. RIVER_WINDOW_V1_EDGES_NONE)."
  (and-let* (((window? window))
			 (proxy-window (window-wl-proxy window)))
	(with-manage-sequence
	 (wm-window-tiled-set proxy-window edges))))

(define (window-decoration-above-get! window proxy-surface)
  "Create a decoration surface above the WINDOW.
Provided PROXY-SURFACE shouldn't have a role or buffer attached.
Parameters: (WINDOW PROXY-SURFACE)
  - WINDOW: <window> record.
  - PROXY-SURFACE: Pointer to wl_surface proxy.
Returns pointer to river_decoration_v1 proxy."
  (when window
    (let ((proxy-window (window-wl-proxy window))
          (cached-decoration (window-wl-decoration-below-proxy window)))
	  ;; return cached if it exists, otherwise get its proxy
      (if cached-decoration
		  cached-decoration
		  (let ((proxy-decoration (wm-window-decoration-above-get proxy-window proxy-surface)))
			(log-debug "Setting above decoration of window: ~a to ~a" proxy-window proxy-decoration)
			(%window-wl-decoration-above-proxy-set! window proxy-decoration)
			proxy-decoration)))))

(define (window-decoration-below-get! window proxy-surface)
  "Create a decoration surface below WINDOW.
Provided WL_SURFACE shouldn't have a role or buffer attached.
Parameters: (WINDOW PROXY-SURFACE)
  - WINDOW: <window> record.
  - PROXY-SURFACE: Pointer to wl_surface proxy.
Returns river_decoration_v1 proxy pointer."
  (when window
    (let ((proxy-window (window-wl-proxy window))
          (cached-decoration (window-wl-decoration-below-proxy window)))
	  ;; return cached if it exists, otherwise get its proxy
      (if cached-decoration
		  cached-decoration
          (let ((proxy-decoration (wm-window-decoration-below-get proxy-window proxy-surface)))
			(log-debug "Setting below decoration of window: ~a to ~a" proxy-window proxy-decoration)
			(%window-wl-decoration-below-proxy-set! window proxy-decoration)
			proxy-decoration)))))

(define (window-resize-started-inform! window)
  "Inform the WINDOW that it is being resized.
Parameters: (WINDOW)
  - WINDOW: <window> record."
  (and-let* (((window? window))
			 (proxy-window (window-wl-proxy window)))
    (with-manage-sequence
	 (wm-window-resize-started-inform proxy-window)
     (%window-is-resizing-set! window #t)
     (gliver-hook-run! *window-resize-start-hook* window))))

(define (window-resize-ended-inform! window)
  "Inform the WINDOW that it has ended resizing.
Parameters: (WINDOW)
  - WINDOW: <window> record."
  (and-let* (((window? window))
			 (proxy-window (window-wl-proxy window)))
    (with-manage-sequence
	 (wm-window-resize-ended-inform proxy-window)
     (%window-is-resizing-set! window #f)
     (gliver-hook-run! *window-resize-end-hook* window))))

(define (window-capabilities-inform! window capabilities)
  "Inform the WINDOW of the CAPABILITIES supported (maximize, minimize).
Parameters: (WINDOW CAPABILITIES)
  - WINDOW: <window> record.
  - CAPABILITIES: Capabilities enum flags bitmask."
  (and-let* (((window? window))
			 (proxy-window (window-wl-proxy window))
			 (prev-capabilities (window-capabilities window)))
    (with-manage-sequence
	 (wm-window-capabilities-inform proxy-window capabilities)
     (%window-capabilities-set! window capabilities)
     (gliver-hook-run! *window-capabilities-changed-hook* window prev-capabilities))))

(define (window-maximized-inform! window)
  "Inform the WINDOW that it has been maximized.
Parameters: (WINDOW)
  - WINDOW: <window> record."
  (and-let* (((window? window))
			 (proxy-window (window-wl-proxy window)))
	(let ((prev-status (window-maximized? window)))
	  (with-manage-sequence
	   (wm-window-maximized-inform proxy-window)
	   (%window-maximized-set! window #t)
	   (gliver-hook-run! *window-maximized-hook* window prev-status)))))

(define (window-unmaximized-inform! window)
  "Inform the WINDOW that it has been unmaximized.
Parameters: (WINDOW)
  - WINDOW: <window> record."
  (and-let* (((window? window))
			 (proxy-window (window-wl-proxy window)))
	(let ((prev-status (window-maximized? window)))
	  (with-manage-sequence
	   (wm-window-unmaximized-inform proxy-window)
	   (%window-maximized-set! window #f)
	   (gliver-hook-run! *window-unmaximized-hook* window prev-status)))))

(define (window-fullscreen-inform! window)
  "Inform the WINDOW that it has entered fullscreen mode.
Parameters: (WINDOW)
  - WINDOW: <window> record."
  (and-let* (((window? window))
			 (proxy-window (window-wl-proxy window)))
	(let ((prev-status (window-fullscreen? window)))
	  (with-manage-sequence
	   (wm-window-fullscreen-inform proxy-window)
	   (gliver-hook-run! *window-fullscreen-entered-informed-hook* window prev-status)))))

(define (window-fullscreen-exit-inform! window)
  "Inform the WINDOW that it has exited fullscreen mode.
Parameters: (WINDOW)
  - WINDOW: <window> record."
  (and-let* (((window? window))
			 (proxy-window (window-wl-proxy window)))
	(let ((prev-status (window-fullscreen? window)))
	  (with-manage-sequence
	   (wm-window-fullscreen-exit-inform proxy-window)
	   (gliver-hook-run! *window-fullscreen-exited-informed-hook* window prev-status)))))

(define (window-fullscreen! window output)
  "Make the WINDOW fullscreen on the OUTPUT.
Parameters: (WINDOW OUTPUT)
  - WINDOW: <window> record.
  - OUTPUT: <output> record."
  (and-let* (((window? window))
			 (proxy-window (window-wl-proxy window))
			 (proxy-output (output-wl-proxy output)))
	(let ((prev-status (window-fullscreen? window)))
	  (with-manage-sequence
	   (wm-window-fullscreen proxy-window proxy-output)
	   (wm-window-fullscreen-inform proxy-window))
	  (gliver-hook-run! *window-fullscreen-entered-hook* window prev-status)
      (gliver-hook-run! *window-fullscreen-entered-informed-hook* window prev-status)
	  (%window-fullscreen-set! window #t)
	  (window-place-top! window))))

(define (window-fullscreen-exit! window)
  "Make the window not fullscreen.
Parameters: (WINDOW)
  - WINDOW: <window> record."
  (and-let* (((window? window))
			 (proxy-window (window-wl-proxy window))
			 (container (window-container window)))
	(let ((prev-status (window-fullscreen? window)))
	  ;; exit fullscreen
	  (%window-fullscreen-set! window #f)
	  (with-manage-sequence
	   (wm-window-fullscreen-exit proxy-window)
       (wm-window-fullscreen-exit-inform proxy-window))
	  ;; resize window to its container size
	  (window-dimensions-propose! window
								  (container-usable-width container)
								  (container-usable-height container))
	  (window-position-set! window
							(container-usable-x container)
							(container-usable-y container))
	  (gliver-hook-run! *window-fullscreen-exited-hook* window prev-status)
	  (gliver-hook-run! *window-fullscreen-exited-informed-hook* window prev-status))))

(define (window-clip-box-set! window x y width height)
  "Clip the WINDOW, including borders and decoration surfaces.
Clip box is ignored while window is on fullscreen.
Parameters: (WINDOW X Y WIDTH HEIGHT)
  - WINDOW: <window> record.
  - X: Horizontal clip coordinate in compositor space.
  - Y: Vertical clip coordinate in compositor space.
  - WIDTH: Clip box width in pixels (disabled if 0).
  - HEIGHT: Clip box height in pixels (disabled if 0)."
  (and-let* (((window? window))
			 (proxy-window (window-wl-proxy window)))
    (with-manage-sequence
	 (wm-window-clip-box-set proxy-window x y width height))))

(define (window-content-clip-box-set! window x y width height)
  "Clip the WINDOW, excluding borders and decoration surfaces.
Clip box is ignored while window is on fullscreen.
Parameters: (WINDOW X Y WIDTH HEIGHT)
  - WINDOW: <window> record.
  - X: Horizontal content clip coordinate in compositor space.
  - Y: Vertical content clip coordinate in compositor space.
  - WIDTH: Content clip box width in pixels (disabled if 0).
  - HEIGHT: Content clip box height in pixels (disabled if 0)."
  (and-let* (((window? window))
			 (proxy-window (window-wl-proxy window)))
    (with-manage-sequence
	 (wm-window-content-clip-box-set proxy-window x y width height))))

(define (window-dimension-bounds-set! window max-width max-height)
  "Recommend that the WINDOW keep its dimensions within a given MAX-WIDTH and MAX-HEIGHT.
Parameters: (WINDOW MAX-WIDTH MAX-HEIGHT)
  - WINDOW: <window> record.
  - MAX-WIDTH: Maximum allowable width in pixels (unbounded if 0).
  - MAX-HEIGHT: Maximum allowable height in pixels (unbounded if 0)."
  (and-let* (((window? window))
			 (proxy-window (window-wl-proxy window)))
    (with-manage-sequence
	 (wm-window-dimension-bounds-set proxy-window max-width max-height))))

(define* (window-position-set! window x y #:key (animate #t))
  "Set the position of the WINDOW.
Parameters: (WINDOW X Y [#:animate #t])
  - WINDOW: <window> record.
  - X: Horizontal position in pixels.
  - Y: Vertical position in pixels.
  - ANIMATE: Boolean, whether to animate position change (default #t)."
  (let ((node (window-node-get! window))
		(int-x (inexact->exact (floor x)))
		(int-y (inexact->exact (floor y))))
	(%window-x-set! window int-x)
	(%window-y-set! window int-y)
	(when node
      (with-render-sequence
       (wm-node-position-set! node int-x int-y)))))

(define (window-on-window data manager proxy-window)
  "Invoked when a window is advertised.
Parameters: (DATA MANAGER WINDOW-PROXY)
  - DATA: User context pointer (unused, null in Gliver).
  - MANAGER: Pointer to river_window_manager_v1 proxy.
  - WINDOW-PROXY: Pointer to river_window_v1 proxy."
  (let ((window (make-window
				 #:wl-proxy proxy-window)))
    (window-add! window)))
(gliver-hook-add! %window-created-hook 'window-on-window)

(define (window-on-window-closed data proxy-window)
  "Invoked when window is closed.
Parameters: (DATA PROXY-WINDOW)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-WINDOW: Pointer to river_window_v1 proxy."
  (let ((window (window-find-by-proxy proxy-window)))
    (window-remove! window)))
(gliver-hook-add! %window-destroyed-hook 'window-on-window-closed)

(define (window-on-window-focused window)
  "Invoked when WINDOW is focused.
Parameters: (WINDOW)
  - WINDOW: <window> record."
  ;; ensure that window's container is focused
  (let ((container (window-container window)))
    (when (and container (container? container)
               (not (container-focused? container)))
      (container-focus! container #:focus-child #f)))
  ;; colorize the window border with active window border color
  (window-borders-set! window *window-default-border-edges*
					   *window-border-width*
					   *window-border-color-focused*))
(gliver-hook-add! *window-focused-hook* 'window-on-window-focused)

(define (window-on-window-unfocused window)
  "Invoked when WINDOW unfocused event.
Parameters: (WINDOW)
  - WINDOW: <window> record."
  ;; colorize the window border with inactive window border color
  (log-debug "unfocusing window ~a" window)
  (window-borders-set! window *window-default-border-edges*
					   *window-border-width*
					   *window-border-color-unfocused*))
(gliver-hook-add! *window-unfocused-hook* 'window-on-window-unfocused)

(define (window-on-window-title-changed proxy-window title)
  "Invoked when a window title is changed.
Parameters: (PROXY-WINDOW TITLE)
  - PROXY-WINDOW: Pointer to river_window_v1 proxy.
  - TITLE: New window title string."
  (and-let* ((window (window-find-by-proxy proxy-window)))
    (%window-title-set! window title)
	(gliver-hook-run! *window-title-changed-hook* window title)))
(gliver-hook-add! %window-title-changed-hook 'window-on-window-title-changed)

;; TODO: check how this should be handled
;; in general, these are popups/dialog windows
;; I think parent should be resolved by window-find-by-proxy
(define (window-on-window-parent-changed proxy-window parent)
  "Invoked when window's transient parent change.
Parameters: (PROXY-WINDOW PARENT-PROXY)
  - PROXY-WINDOW: Pointer to child river_window_v1 proxy.
  - PARENT: Pointer to parent river_window_v1 proxy."
  (and-let* ((window (window-find-by-proxy proxy-window)))
    (%window-parent-set! window parent)
	(gliver-hook-run! *window-parent-changed-hook* window parent)))
(gliver-hook-add! %window-parent-changed-hook 'window-on-window-parent-changed)

(define (window-on-window-app-id-changed proxy-window app-id)
  "Invoked when window's app_id string change.
Parameters: (PROXY-WINDOW APP-ID-PTR)
  - PROXY-WINDOW: Pointer to river_window_v1 proxy.
  - APP-ID: String app_id."
  (and-let* ((window (window-find-by-proxy proxy-window)))
    (%window-app-id-set! window app-id)
	(gliver-hook-run! *window-app-id-changed-hook* window app-id)))
(gliver-hook-add! %window-app-id-changed-hook 'window-on-window-app-id-changed)

(define (window-on-window-identifier-changed proxy-window identifier)
  "Invoked when window's identifier string is advertised.
Parameters: (PROXY-WINDOW ID-PTR)
  - PROXY-WINDOW: Pointer to river_window_v1 proxy.
  - ID-PTR: Pointer to identifier ASCII string."
  (and-let* ((window (window-find-by-proxy proxy-window)))
    (%window-identifier-set! window identifier)
	(gliver-hook-run! *window-identifier-changed-hook* window identifier)))
(gliver-hook-add! %window-identifier-changed-hook 'window-on-window-identifier-changed)

(define (window-on-window-presentation-hint data proxy-window hint)
  "Invoked when window's preferred presentation mode hint change (like vsync).
Parameters: (DATA PROXY-WINDOW HINT)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-WINDOW: Pointer to river_window_v1 proxy.
  - HINT: Presentation mode enum (e.g. RIVER_OUTPUT_V1_PRESENTATION_MODE_VSYNC)."
  (and-let* ((window (window-find-by-proxy proxy-window)))
    (%window-presentation-hint-set! window hint)
	(gliver-hook-run! *window-presentation-hint-changed-hook* window hint)))
(gliver-hook-add! %window-presentation-hint-changed-hook 'window-on-window-presentation-hint)

(define (window-on-window-pid-changed proxy-window pid)
  "Invoked when window's process PID is advertised.
Parameters: (PROXY-WINDOW PID)
  - PROXY-WINDOW: Pointer to river_window_v1 proxy.
  - PID: Integer process ID (unverified)."
  (and-let* ((window (window-find-by-proxy proxy-window)))
    (%window-pid-set! window pid)
	(gliver-hook-run! *window-pid-changed-hook* window pid)))
(gliver-hook-add! %window-pid-changed-hook 'window-on-window-pid-changed)

(define (window-on-window-dimensions proxy-window width height)
  "Invoked when window's dimensions (width and height) change.
Parameters: (PROXY-WINDOW WIDTH HEIGHT)
  - PROXY-WINDOW: Pointer to river_window_v1 proxy.
  - WIDTH: Width in pixels.
  - HEIGHT: Height in pixels."
  (and-let* ((window (window-find-by-proxy proxy-window)))
	(%window-width-set! window width)
	(%window-height-set! window height)
	(gliver-hook-run! *window-size-changed-hook* window width height)))
(gliver-hook-add! %window-size-changed-hook 'window-on-window-dimensions)

(define (window-on-window-dimensions-hint proxy-window min-w min-h max-w max-h)
  "Invoked when window's preferred minimum and maximum dimension hints change.
Parameters: (PROXY-WINDOW MIN-W MIN-H MAX-W MAX-H)
  - PROXY-WINDOW: Pointer to river_window_v1 proxy.
  - MIN-W: Minimum preferred width in pixels.
  - MIN-H: Minimum preferred height in pixels.
  - MAX-W: Maximum preferred width in pixels.
  - MAX-H: Maximum preferred height in pixels."
  (and-let* ((window (window-find-by-proxy proxy-window)))
	(%window-width-min-set! window min-w)
	(%window-height-min-set! window min-h)
	(%window-width-max-set! window max-w)
	(%window-height-max-set! window max-h)
	(gliver-hook-run! *window-size-hint-changed-hook* window min-w min-h max-w max-h)))
(gliver-hook-add! %window-size-hint-changed-hook 'window-on-window-dimensions-hint)

(define (window-on-window-decoration-hint proxy-window hint)
  "Invoked when window's decoration mode preference hint change.
Parameters: (PROXY-WINDOW HINT)
  - PROXY-WINDOW: Pointer to river_window_v1 proxy.
  - HINT: Decoration hint enum value (e.g. RIVER_WINDOW_V1_DECORATION_HINT_NO_PREFERENCE)."
  (and-let* ((window (window-find-by-proxy proxy-window)))
	(%window-decoration-hint-set! window hint)
	(gliver-hook-run! *window-decoration-hint-changed-hook* window hint)))
(gliver-hook-add! %window-decoration-hint-changed-hook 'window-on-window-decoration-hint)

(define (window-on-seat-window-focused seat window)
  "Invoked when SEAT focuses WINDOW.
Parameters: (SEAT WINDOW)
  - SEAT: <seat> record.
  - WINDOW: <window> record being focused."
  (log-debug "window seat has focused ~a" window)
  (if (and window (window? window) (not (window-destroyed? window)))
	  ;; just try to focus window, window-check! already has guard checks
	  (window-focus! window #:seat (or seat (seat-current)))
	  ;; if the window passed isn't a window (maybe surface)
	  ;; then clear the focus from window so surface receives input
	  (let ((current (window-current)))
		(when (and current (window? current) (not (window-destroyed? current)))
		  (gliver-hook-run! *window-unfocused-hook* current)))))
(gliver-hook-add! *seat-window-focused-hook* 'window-on-seat-window-focused)

(define (window-on-workspace-switch workspace prev-workspace)
  "Invoked when active workspace is switched to WORKSPACE.
Parameters: (WORKSPACE PREV-WORKSPACE)
  - WORKSPACE: Currently focused <workspace> record.
  - PREV-WORKSPACE: Previously focused <workspace> record."
  ;; hide all windows of previous workspace
  (when (and prev-workspace (workspace? prev-workspace) (not (eq? prev-workspace workspace)))
    (for-each window-hide! (workspace-windows prev-workspace)))
  ;; show all windows of new workspace
  (when (and workspace (workspace? workspace))
    (for-each window-show! (workspace-windows workspace))))
(gliver-hook-add! *workspace-switch-hook* 'window-on-workspace-switch)

(define (window-on-fullscreen-requested proxy-window proxy-output)
  "Invoked when a client request to enter fullscreen mode.
Parameters: (DATA PROXY-WINDOW OUTPUT-PTR)
  - PROXY-WINDOW: Pointer to river_window_v1 proxy.
  - proxy-output Pointer to target river_output_v1 proxy (or null for current output)."
  (and-let* ((window (window-find-by-proxy proxy-window))
			 ;; NOTE: if not output proxy is passed then current client
			 ;; is requesting fullscreen on current output
			 (output (or (window-find-by-proxy proxy-output)
						 (window-output window))))
    (window-fullscreen! window output)
    (gliver-hook-run! *window-fullscreen-requested-hook* window)))
(gliver-hook-add! %window-fullscreen-requested-hook 'window-on-fullscreen-requested)

(define (window-on-fullscreen-exit-requested proxy-window)
  "Invoked when a client request to exit fullscreen mode.
Parameters: (DATA PROXY-WINDOW)
  - PROXY-WINDOW: Pointer to river_window_v1 proxy."
  (and-let* ((window (window-find-by-proxy proxy-window)))
    (window-fullscreen-exit! window)
    (gliver-hook-run! *window-fullscreen-exit-requested-hook* window)))
(gliver-hook-add! %window-fullscreen-exit-requested-hook 'window-on-fullscreen-exit-requested)

(define (window-on-minimize-requested proxy-window)
  "Invoked when a client requests to be minimized.
Parameters: (PROXY-WINDOW)
  - PROXY-WINDOW: Pointer to river_window_v1 proxy."
  (and-let* ((window (window-find-by-proxy proxy-window)))
    (gliver-hook-run! *window-minimize-requested-hook* window)))
(gliver-hook-add! %window-minimize-requested-hook 'window-on-minimize-requested)
