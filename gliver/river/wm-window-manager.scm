;;; gliver/river/wm-window-manager.scm --- River window management window integration
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module manages the window life-cycle after its creation
;;;
;;; It handles:
;;;   - Binding river_window_v1 and exposing interface requests and events
;;;
;;; This module implements the river interfaces (protocol river_window_manager_v1):
;;;   - river_window_v1

(define-module (gliver river wm-window-manager)
  #:use-module (gliver core-base)
  #:use-module (gliver wayland client)
  #:use-module (gliver wayland gen river-window-management-v1)
  #:use-module (system foreign)
  #:export (
			*wm-window-listener*
			wm-window-on-listeners-attach
			wm-window-on-window
			wm-window-close
			wm-window-destroy
			wm-window-node-get
			wm-window-dimensions-propose
			wm-window-hide
			wm-window-show
			wm-window-decoration-client
			wm-window-decoration-server
			wm-window-borders-set
			wm-window-tiled-set
			wm-window-decoration-above-get
			wm-window-decoration-below-get
			wm-window-resize-started-inform
			wm-window-resize-ended-inform
			wm-window-capabilities-inform
			wm-window-maximized-inform
			wm-window-unmaximized-inform
			wm-window-fullscreen-inform
			wm-window-fullscreen-exit-inform
			wm-window-fullscreen
			wm-window-fullscreen-exit
			wm-window-clip-box-set
			wm-window-content-clip-box-set
			wm-window-dimension-bounds-set
			wm-window-on-window-closed
			wm-window-on-window-dimensions-hint
			wm-window-on-window-dimensions
			wm-window-on-window-app-id
			wm-window-on-window-title
			wm-window-on-window-parent
			wm-window-on-window-decoration-hint
			wm-window-on-window-pointer-move-requested
			wm-window-on-window-pointer-resize-requested
			wm-window-on-window-show-menu-requested
			wm-window-on-window-maximize-requested
			wm-window-on-window-unmaximize-requested
			wm-window-on-window-fullscreen-requested
			wm-window-on-window-exit-fullscreen-requested
			wm-window-on-window-minimize-requested
			wm-window-on-window-unreliable-pid
			wm-window-on-window-presentation-hint
			wm-window-on-window-identifier
))

(define-var *wm-window-listener* #f
  "Wayland listener for river_window_v1 events.")

(define (wm-window-on-listeners-attach)
  "Attach river_window_v1 event listener.
Parameters: None."
  (set! *wm-window-listener*
        (make-river-window-v1-listener
         wm-window-on-window-closed
         wm-window-on-window-dimensions-hint
         wm-window-on-window-dimensions
         wm-window-on-window-app-id
         wm-window-on-window-title
         wm-window-on-window-parent
         wm-window-on-window-decoration-hint
         wm-window-on-window-pointer-move-requested
         wm-window-on-window-pointer-resize-requested
         wm-window-on-window-show-menu-requested
         wm-window-on-window-maximize-requested
         wm-window-on-window-unmaximize-requested
         wm-window-on-window-fullscreen-requested
         wm-window-on-window-exit-fullscreen-requested
         wm-window-on-window-minimize-requested
         wm-window-on-window-unreliable-pid
         wm-window-on-window-presentation-hint
         wm-window-on-window-identifier)))
(gliver-hook-add! *gliver-listeners-attach-hook* 'wm-window-on-listeners-attach 0)

(define (wm-window-on-window data manager window-proxy)
  "Invoked when a window is advertised.
Attach the river_window_v1 listener.
Parameters: (DATA MANAGER WINDOW-PROXY)
  - DATA: User context pointer (unused, null in Gliver).
  - MANAGER: Pointer to river_window_manager_v1 proxy.
  - WINDOW-PROXY: Pointer to river_window_v1 proxy."
  (when *log-river-calls*
	(log-debug "New window proxy: ~a" window-proxy))
  (when *wm-window-listener*
	;; add the new window to the global window listener
    (wl-proxy-add-listener window-proxy *wm-window-listener* %null-pointer)))
(gliver-hook-add! %window-created-hook 'wm-window-on-window 0)

;;; window requests
(define (wm-window-close proxy-window)
  "Close a window, the window may take time to respond or
completely ignore the request. listen for *window-destroyed-hook*
in case an action other than clearing state needs to be executed.
Must be called in a manage sequence.
Parameters: (PROXY-WINDOW)
  - PROXY-WINDOW: Pointer to river_window_v1 proxy."
  (when proxy-window
    (when *log-river-calls*
	  (log-debug "closing window ~a" proxy-window))
    (river-window-v1-close proxy-window)))

(define (wm-window-destroy proxy-window)
  "Destroy river_window_v1 proxy object, this means everything was
cleared from client side and the server should also clear everything.
Parameters: (PROXY-WINDOW)
  - PROXY-WINDOW: Pointer to river_window_v1 proxy."
  (when proxy-window
    (when *log-river-calls*
	  (log-debug "destroying window proxy: ~a" proxy-window))
    (river-window-v1-destroy proxy-window)))

(define (wm-window-node-get proxy-window)
  "Get node corresponding to the window (called once per window).
Parameters: (PROXY-WINDOW)
  - PROXY-WINDOW: Pointer to river_window_v1 proxy."
  (when proxy-window
	(when *log-river-calls*
	  (log-debug "getting node of window: ~a" proxy-window))
	(river-window-v1-get-node proxy-window)))

(define (wm-window-dimensions-propose proxy-window width height)
  "Propose dimensions (width and height) for a window.
Must be called in a manage sequence.
Parameters: (PROXY-WINDOW WIDTH HEIGHT)
  - PROXY-WINDOW: Pointer to river_window_v1 proxy.
  - WIDTH: Proposed window width in pixels.
  - HEIGHT: Proposed window height in pixels."
  (when proxy-window
    (when *log-river-calls*
	  (log-debug "proposing dimensions ~ax~a for window: ~a" width height proxy-window))
    (river-window-v1-propose-dimensions proxy-window width height)))

(define (wm-window-hide proxy-window)
  "Request that the window be hidden.
Must be called in a render sequence.
Parameters: (PROXY-WINDOW)
  - PROXY-WINDOW: Pointer to river_window_v1 proxy."
  (when proxy-window  
    (when *log-river-calls*
	  (log-debug "hiding window: ~a" proxy-window))
    (river-window-v1-hide proxy-window)))

(define (wm-window-show proxy-window)
  "Request that the window be shown.
Must be called in a render sequence.
Parameters: (PROXY-WINDOW)
  - PROXY-WINDOW: Pointer to river_window_v1 proxy."
  (when proxy-window  
    (when *log-river-calls*
	  (log-debug "showing window: ~a" proxy-window))
    (river-window-v1-show proxy-window)))

(define (wm-window-decoration-client proxy-window)
  "Enable client-side decoration for the window.
Must be called in a manage sequence.
Parameters: (PROXY-WINDOW)
  - PROXY-WINDOW: Pointer to river_window_v1 proxy."
  (when proxy-window  
	(when *log-river-calls*
	  (log-debug "using client side decoration for window: ~a" proxy-window))
	(river-window-v1-use-csd proxy-window)))

(define (wm-window-decoration-server proxy-window)
  "Enable server-side decoration for the window.
Must be called in a manage sequence.
Parameters: (PROXY-WINDOW)
  - PROXY-WINDOW: Pointer to river_window_v1 proxy."
  (when proxy-window  
	(when *log-river-calls*
	  (log-debug "using server side decoration for window: ~a" proxy-window))
	(river-window-v1-use-ssd proxy-window)))

(define (wm-window-borders-set proxy-window edges width r g b a)
  "Set border edges, width, and RGBA color for the window.
Must be called in a render sequence.
Parameters: (PROXY-WINDOW EDGES WIDTH R G B A)
  - PROXY-WINDOW: Pointer to river_window_v1 proxy.
  - EDGES: Border edge bitmask flags (e.g. RIVER_WINDOW_V1_EDGES_TOP).
  - WIDTH: Border width in pixels.
  - R: Red color component (0 - 4294967295).
  - G: Green color component (0 - 4294967295).
  - B: Blue color component (0 - 4294967295).
  - A: Alpha opacity component (0 - 4294967295)."
  (when proxy-window  
	(when *log-river-calls*
	  (log-debug "setting borders for window: ~a to width ~a, edges: ~a, and rgba(~a, ~a, ~a, ~a)" proxy-window width edges r g b a))
	(river-window-v1-set-borders proxy-window edges width 
                                 r g b a)))

(define (wm-window-tiled-set proxy-window edges)
  "Set tiled state flags for the window.
Must be called in a manage sequence.
Parameters: (PROXY-WINDOW EDGES)
  - PROXY-WINDOW: Pointer to river_window_v1 proxy.
  - EDGES: Tiled edge bitmask flags (e.g. RIVER_WINDOW_V1_EDGES_TOP)."
  (when proxy-window  
	(when *log-river-calls*
	  (log-debug "setting tiled state for window: ~a" proxy-window))
	(river-window-v1-set-tiled proxy-window edges)))

(define (wm-window-decoration-above-get proxy-window proxy-surface)
  "Create a decoration surface above the window and
assign the river_decoration_v1 role to it.
Parameters: (PROXY-WINDOW PROXY-SURFACE)
  - PROXY-WINDOW: Pointer to river_window_v1 proxy.
  - PROXY-SURFACE: Pointer to unmapped wl_surface proxy."
  (when proxy-window  
	(when *log-river-calls*
	  (log-debug "creating decoration surface ~a above window: ~a" proxy-surface proxy-window))
    (river-window-v1-get-decoration-above proxy-window proxy-surface)))

(define (wm-window-decoration-below-get proxy-window proxy-surface)
  "Create a decoration surface below the window and
assign the river_decoration_v1 role to it.
Parameters: (PROXY-WINDOW PROXY-SURFACE)
  - PROXY-WINDOW: Pointer to river_window_v1 proxy.
  - PROXY-SURFACE: Pointer to unmapped wl_surface proxy."
  (when proxy-window  
	(when *log-river-calls*
	  (log-debug "creating decoration surface ~a below window: ~a" proxy-surface proxy-window))
    (river-window-v1-get-decoration-below proxy-window proxy-surface)))

(define (wm-window-resize-started-inform proxy-window)
  "Inform window that an interactive resize operation has started.
Must be called in a manage sequence.
Parameters: (PROXY-WINDOW)
  - PROXY-WINDOW: Pointer to river_window_v1 proxy."
  (when proxy-window  
	(when *log-river-calls*
	  (log-debug "resizing window ~a started" proxy-window))
	(river-window-v1-inform-resize-start proxy-window)))

(define (wm-window-resize-ended-inform proxy-window)
  "Inform window that an interactive resize operation has ended.
Must be called in a manage sequence.
Parameters: (PROXY-WINDOW)
  - PROXY-WINDOW: Pointer to river_window_v1 proxy."
  (when proxy-window  
	(when *log-river-calls*
	  (log-debug "resizing window ~a ended" proxy-window))
	(river-window-v1-inform-resize-end proxy-window)))

(define (wm-window-capabilities-inform proxy-window caps)
  "Inform window of supported window management capabilities.
Must be called in a manage sequence.
Parameters: (PROXY-WINDOW CAPS)
  - PROXY-WINDOW: Pointer to river_window_v1 proxy.
  - CAPS: Capabilities bitmask flags (e.g. RIVER_WINDOW_V1_CAPABILITIES_MAXIMIZE)."
  (when proxy-window  
    (when *log-river-calls*
	  (log-debug "set capabilities of window ~a to ~a" proxy-window caps))
    (river-window-v1-set-capabilities proxy-window caps)))

(define (wm-window-maximized-inform proxy-window)
  "Inform window that it has been maximized.
Must be called in a manage sequence.
Parameters: (PROXY-WINDOW)
  - PROXY-WINDOW: Pointer to river_window_v1 proxy."
  (when proxy-window  
    (when *log-river-calls*
	  (log-debug "Window ~a has been maximized" proxy-window))
    (river-window-v1-inform-maximized proxy-window)))

(define (wm-window-unmaximized-inform proxy-window)
  "Inform window that it has been unmaximized.
Must be called in a manage sequence.
Parameters: (PROXY-WINDOW)
  - PROXY-WINDOW: Pointer to river_window_v1 proxy."
  (when proxy-window  
    (when *log-river-calls*
	  (log-debug "Window ~a has been unmaximized" proxy-window))
    (river-window-v1-inform-unmaximized proxy-window)))

(define (wm-window-fullscreen-inform proxy-window)
  "Inform window that it has entered fullscreen mode.
Must be called in a manage sequence.
Parameters: (PROXY-WINDOW)
  - PROXY-WINDOW: Pointer to river_window_v1 proxy."
  (when proxy-window
    (when *log-river-calls*
	  (log-debug "Window ~a entered fullscreen mode" proxy-window))
    (river-window-v1-inform-fullscreen proxy-window)))

(define (wm-window-fullscreen-exit-inform proxy-window)
  "Inform window that it has exited fullscreen mode.
Must be called in a manage sequence.
Parameters: (PROXY-WINDOW)
  - PROXY-WINDOW: Pointer to river_window_v1 proxy."
  (when proxy-window
	(when *log-river-calls*
	  (log-debug "Window ~a exited fullscreen mode" proxy-window))
	(river-window-v1-inform-not-fullscreen proxy-window)))

(define (wm-window-fullscreen proxy-window proxy-output)
  "Make window fullscreen on the given output.
Must be called in a manage sequence.
Parameters: (PROXY-WINDOW PROXY-OUTPUT)
  - PROXY-WINDOW: Pointer to river_window_v1 proxy.
  - PROXY-OUTPUT: Pointer to river_output_v1 proxy."
  (when (and proxy-window proxy-output)
    (when *log-river-calls*
	  (log-debug "Window ~a is entering fullscreen mode on output ~a" proxy-window proxy-output))
    (river-window-v1-fullscreen proxy-window proxy-output)))

(define (wm-window-fullscreen-exit proxy-window)
  "Make window exit fullscreen mode.
Must be called in a manage sequence.
Parameters: (PROXY-WINDOW)
  - PROXY-WINDOW: Pointer to river_window_v1 proxy."
  (when proxy-window
    (when *log-river-calls*
	  (log-debug "Window ~a is exiting fullscreen mode" proxy-window))
    (river-window-v1-exit-fullscreen proxy-window)))

(define (wm-window-clip-box-set proxy-window x y width height)
  "Clip the window including borders and decorations.
Clip box is ignored while window is on fullscreen.
Must be called in a render sequence.
Parameters: (PROXY-WINDOW X Y WIDTH HEIGHT)
  - PROXY-WINDOW: Pointer to river_window_v1 proxy.
  - X: Horizontal clip origin in window coordinates.
  - Y: Vertical clip origin in window coordinates.
  - WIDTH: Clip box width (0 disables clipping).
  - HEIGHT: Clip box height (0 disables clipping)."
  (when proxy-window
    (when *log-river-calls*
	  (log-debug "Apply clip box on window ~a at ~ax~a with size ~ax~a" proxy-window x y width height))
    (river-window-v1-set-clip-box proxy-window x y width height)))

(define (wm-window-content-clip-box-set proxy-window x y width height)
  "Clip the window content excluding borders and decorations.
Must be called in a render sequence.
Parameters: (PROXY-WINDOW X Y WIDTH HEIGHT)
  - PROXY-WINDOW: Pointer to river_window_v1 proxy.
  - X: Horizontal clip origin in content coordinates.
  - Y: Vertical clip origin in content coordinates.
  - WIDTH: Content clip width (0 disables clipping).
  - HEIGHT: Content clip height (0 disables clipping)."
  (when proxy-window
    (when *log-river-calls*
	  (log-debug "Apply content clip box on window ~a at ~ax~a with size ~ax~a" proxy-window x y width height))
    (river-window-v1-set-content-clip-box proxy-window x y width height)))

(define (wm-window-dimension-bounds-set proxy-window max-width max-height)
  "Recommend maximum dimension bounds for the window.
Must be called in a manage sequence.
Parameters: (PROXY-WINDOW MAX-WIDTH MAX-HEIGHT)
  - PROXY-WINDOW: Pointer to river_window_v1 proxy.
  - MAX-WIDTH: Maximum recommended width (0 indicates no limit).
  - MAX-HEIGHT: Maximum recommended height (0 indicates no limit)."
  (when proxy-window
    (when *log-river-calls*
	  (log-debug "Setting dimension bounds on window ~a with max size ~ax~a" proxy-window max-width max-height))
    (river-window-v1-set-dimension-bounds proxy-window max-width max-height)))

;;; window events
(define (wm-window-on-window-closed data proxy-window)
  "Invoked when window is closed.
Parameters: (DATA PROXY-WINDOW)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-WINDOW: Pointer to river_window_v1 proxy."
  ;; just let the window manager handle it
  (when *log-river-calls*
	(log-debug "Window closed: ~a" proxy-window))
  (gliver-hook-run! %window-destroyed-hook data proxy-window)
  ;; TODO: cleanup wm-window-decoration-below-get and wm-window-decoration-above-get
  (wm-window-destroy proxy-window))

(define (wm-window-on-window-dimensions-hint data proxy-window min-w min-h max-w max-h)
  "Invoked when window's preferred minimum and maximum dimension hints change.
Parameters: (DATA PROXY-WINDOW MIN-W MIN-H MAX-W MAX-H)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-WINDOW: Pointer to river_window_v1 proxy.
  - MIN-W: Minimum preferred width in pixels.
  - MIN-H: Minimum preferred height in pixels.
  - MAX-W: Maximum preferred width in pixels.
  - MAX-H: Maximum preferred height in pixels."
  (when *log-river-calls*
	(log-debug "Window dimensions hint: ~a min=~ax~a max=~ax~a"
             proxy-window min-w min-h max-w max-h))
  (gliver-hook-run! %window-size-hint-changed-hook
                    proxy-window min-w min-h max-w max-h))

(define (wm-window-on-window-dimensions data proxy-window width height)
  "Invoked when window's dimensions (width and height) change.
Parameters: (DATA PROXY-WINDOW WIDTH HEIGHT)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-WINDOW: Pointer to river_window_v1 proxy.
  - WIDTH: Width in pixels.
  - HEIGHT: Height in pixels."
  (when *log-river-calls*
	(log-debug "Window dimensions: ~a ~ax~a" proxy-window width height))
  (gliver-hook-run! %window-size-changed-hook proxy-window width height))

(define (wm-window-on-window-app-id data proxy-window app-id-ptr)
  "Invoked when window's app_id string change.
Parameters: (DATA PROXY-WINDOW APP-ID-PTR)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-WINDOW: Pointer to river_window_v1 proxy.
  - APP-ID-PTR: Pointer to app_id string."
  (let ((app-id (if (or (not (pointer? app-id-ptr)) (null-pointer? app-id-ptr))
                    ""
                    (pointer->string app-id-ptr))))
    (when *log-river-calls*
	  (log-debug "Window ~a updated its app_id to ~a" proxy-window app-id))
    (gliver-hook-run! %window-app-id-changed-hook proxy-window app-id)))

(define (wm-window-on-window-title data proxy-window title-ptr)
  "Invoked when window's title change.
Parameters: (DATA PROXY-WINDOW TITLE-PTR)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-WINDOW: Pointer to river_window_v1 proxy.
  - TITLE-PTR: Pointer to title string."
  (let ((title (if (or (not (pointer? title-ptr)) (null-pointer? title-ptr))
                   ""
                   (pointer->string title-ptr))))
	(when *log-river-calls*
	  (log-debug "Window ~a updated its title to ~a" proxy-window title))
	(gliver-hook-run! %window-title-changed-hook proxy-window title)))

(define (wm-window-on-window-parent data proxy-window parent-proxy)
  "Invoked when window's transient parent change.
Parameters: (DATA PROXY-WINDOW PARENT-PROXY)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-WINDOW: Pointer to child river_window_v1 proxy.
  - PARENT-PROXY: Pointer to parent river_window_v1 proxy."
  (let ((parent (if (or (not (pointer? parent-proxy)) (null-pointer? parent-proxy))
                    ""
                    parent-proxy)))
    (when *log-river-calls*
	  (log-debug "Window ~a updated its parent to ~a" proxy-window parent))
    (gliver-hook-run! %window-parent-changed-hook proxy-window parent)))

(define (wm-window-on-window-decoration-hint data proxy-window hint)
  "Invoked when window's decoration mode preference hint change.
Parameters: (DATA PROXY-WINDOW HINT)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-WINDOW: Pointer to river_window_v1 proxy.
  - HINT: Decoration hint enum value (e.g. RIVER_WINDOW_V1_DECORATION_HINT_NO_PREFERENCE)."
  (when *log-river-calls*
	(log-debug "Window ~a updated its decoration hint to ~a" proxy-window hint))
  (gliver-hook-run! %window-decoration-hint-changed-hook proxy-window hint))

(define (wm-window-on-window-pointer-move-requested data proxy-window proxy-seat)
  "Invoked when interactive window move request initiated (dragging).
Parameters: (DATA PROXY-WINDOW PROXY-SEAT)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-WINDOW: Pointer to river_window_v1 proxy.
  - PROXY-SEAT: Pointer to river_seat_v1 proxy."
  (when *log-river-calls*
	(log-debug "Window ~a is being dragged by seat ~a" proxy-window proxy-seat))
  (gliver-hook-run! %window-pointer-move-requested-hook proxy-window proxy-seat))

(define (wm-window-on-window-pointer-resize-requested data proxy-window proxy-seat edges)
  "Invoked when interactive window resize request initiated (resizing).
Parameters: (DATA PROXY-WINDOW PROXY-SEAT EDGES)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-WINDOW: Pointer to river_window_v1 proxy.
  - PROXY-SEAT: Pointer to river_seat_v1 proxy.
  - EDGES: Resize edge bitmask flags (e.g. RIVER_WINDOW_V1_EDGES_*)."
  (when *log-river-calls*
	(log-debug "Window ~a is being dragged by seat ~a, edges ~a" proxy-window proxy-seat edges))
  (gliver-hook-run! %window-pointer-resize-requested-hook proxy-window proxy-seat edges))

(define (wm-window-on-window-show-menu-requested data proxy-window x y)
  "Invoked when the client request to display window's menu.
For example, right clicking window decorations (no used in Gliver).
Parameters: (DATA PROXY-WINDOW X Y)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-WINDOW: Pointer to river_window_v1 proxy.
  - X: Horizontal position in window coordinates.
  - Y: Vertical position in window coordinates."
  (when *log-river-calls*
	(log-debug "Window ~a requested to show menu at ~ax~a" proxy-window x y))
  (gliver-hook-run! %window-menu-requested-hook proxy-window x y))

(define (wm-window-on-window-maximize-requested data proxy-window)
  "Invoked when a client request to maximize the window.
Parameters: (DATA PROXY-WINDOW)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-WINDOW: Pointer to river_window_v1 proxy."
  (when *log-river-calls*
	(log-debug "Window ~a requested to be maximized" proxy-window))
  (gliver-hook-run! %window-maximize-requested-hook proxy-window))

(define (wm-window-on-window-unmaximize-requested data proxy-window)
  "Invoked when a client request to unmaximize the window.
Parameters: (DATA PROXY-WINDOW)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-WINDOW: Pointer to river_window_v1 proxy."
  (when *log-river-calls*
	(log-debug "Window ~a requested to be unmaximized" proxy-window))
  (gliver-hook-run! %window-unmaximize-requested-hook proxy-window))

(define (wm-window-on-window-fullscreen-requested data proxy-window output-ptr)
  "Invoked when a client request to enter fullscreen mode.
Parameters: (DATA PROXY-WINDOW OUTPUT-PTR)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-WINDOW: Pointer to river_window_v1 proxy.
  - OUTPUT-PTR: Pointer to target river_output_v1 proxy (or null for current output)."
  (when *log-river-calls*
	(log-debug "Window ~a requested to enter fullscreen on output ~a" proxy-window output-ptr))
  (gliver-hook-run! %window-fullscreen-requested-hook proxy-window output-ptr))

(define (wm-window-on-window-exit-fullscreen-requested data proxy-window)
  "Invoked when a client request to exit fullscreen mode.
Parameters: (DATA PROXY-WINDOW)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-WINDOW: Pointer to river_window_v1 proxy."
  (when *log-river-calls*
	(log-debug "Window ~a requested to exit fullscreen" proxy-window))
  (gliver-hook-run! %window-fullscreen-exit-requested-hook proxy-window))

(define (wm-window-on-window-minimize-requested data proxy-window)
  "Invoked when a client request to minimize the window.
Parameters: (DATA PROXY-WINDOW)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-WINDOW: Pointer to river_window_v1 proxy."
  (when *log-river-calls*
	(log-debug "Window ~a requested to be minimized" proxy-window))
  (gliver-hook-run! *window-minimize-requested-hook* proxy-window))

(define (wm-window-on-window-unreliable-pid data proxy-window pid)
  "Invoke when window's process PID is advertised.
Parameters: (DATA PROXY-WINDOW PID)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-WINDOW: Pointer to river_window_v1 proxy.
  - PID: Integer process ID (unverified)."
  (when *log-river-calls*
	(log-debug "Window ~a received a new pid ~a" proxy-window pid))
  (gliver-hook-run! %window-pid-changed-hook proxy-window pid))

(define (wm-window-on-window-presentation-hint data proxy-window hint)
  "Invoked when window's preferred presentation mode hint change (like vsync).
Parameters: (DATA PROXY-WINDOW HINT)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-WINDOW: Pointer to river_window_v1 proxy.
  - HINT: Presentation mode enum (e.g. RIVER_OUTPUT_V1_PRESENTATION_MODE_VSYNC)."
  (when *log-river-calls*
	(log-debug "Window ~a updated its preferred presentation hint to ~a" proxy-window hint))
  (gliver-hook-run! %window-presentation-hint-changed-hook proxy-window hint))

(define (wm-window-on-window-identifier data proxy-window id-ptr)
  "Invoked when window's identifier string is advertised.
Parameters: (DATA PROXY-WINDOW ID-PTR)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-WINDOW: Pointer to river_window_v1 proxy.
  - ID-PTR: Pointer to identifier ASCII string."
  (let ((identifier (if (or (not (pointer? id-ptr)) (null-pointer? id-ptr))
                        ""
                        (pointer->string id-ptr))))
    (when *log-river-calls*
	  (log-debug "Window identifier: ~a = ~a" proxy-window identifier))
    (gliver-hook-run! %window-identifier-changed-hook proxy-window identifier)))

