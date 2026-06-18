;;; gliver/river/wm-window-manager.scm --- River compositor integration
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; Files under the river directory should only act as a wrapper
;;; to river protocol, they should not take action nor import any of
;;; gliver's files or utilities except for logging and configuration

(define-module (gliver river wm-window-manager)
  #:use-module (gliver core types)
  #:use-module (gliver core logs)
  #:use-module (gliver core hooks)
  #:use-module (gliver wayland client)
  #:use-module (gliver wayland gen river-window-management-v1)
  #:use-module (system foreign)
  #:export (
			*wm-window-listener*
			gliver-on-listeners-attach
			on-window
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
			on-window-closed
			on-window-dimensions-hint
			on-window-dimensions
			on-window-app-id
			on-window-title
			on-window-parent
			on-window-decoration-hint
			on-window-pointer-move-requested
			on-window-pointer-resize-requested
			on-window-show-menu-requested
			on-window-maximize-requested
			on-window-unmaximize-requested
			on-window-fullscreen-requested
			on-window-exit-fullscreen-requested
			on-window-minimize-requested
			on-window-unreliable-pid
			on-window-presentation-hint
			on-window-identifier
))

(define *wm-window-listener* #f)

(define (gliver-on-listeners-attach)
  "Attach wayland listeners, seat-manager expects window-manager
to be properly initialized."
  (set! *wm-window-listener*
        (make-river-window-v1-listener
         on-window-closed
         on-window-dimensions-hint
         on-window-dimensions
         on-window-app-id
         on-window-title
         on-window-parent
         on-window-decoration-hint
         on-window-pointer-move-requested
         on-window-pointer-resize-requested
         on-window-show-menu-requested
         on-window-maximize-requested
         on-window-unmaximize-requested
         on-window-fullscreen-requested
         on-window-exit-fullscreen-requested
         on-window-minimize-requested
         on-window-unreliable-pid
         on-window-presentation-hint
         on-window-identifier)))
(gliver-hook-add! *gliver-listeners-attach-hook* 'gliver-on-listeners-attach 0)

(define (on-window data manager window-proxy)
  "Handle a new window event from the compositor."
  (log-debug "New window proxy: ~a" window-proxy)
  ;; attach the shared event listener
  (when *wm-window-listener*
    (wl-proxy-add-listener window-proxy *wm-window-listener* %null-pointer)))
(gliver-hook-add! %window-created-hook 'on-window 0)

;;; window requests
(define (wm-window-close proxy-window)
  "Close a window, the window may take time to respond or
completely ignore the request. listen for *window-destroyed-hook*
in case an action other than clearing state needs to be executed."
  (when proxy-window
    (log-debug "closing window ~a" proxy-window)
    (river-window-v1-close proxy-window)))

(define (wm-window-destroy proxy-window)
  "Destroy a window, this means everything was cleared from
client side and the server should also clear everything.
This most likely should be used internally only, and it
will be automatically managed and called when needed."
  (when proxy-window
    (log-debug "destroying window proxy: ~a" proxy-window)
    (when proxy (river-window-v1-destroy proxy-window))))

(define (wm-window-node-get proxy-window)
  "Return node that corresponds to the window, This can only be
called once per window, so it should be cached after first call.
This most likely should be used internally only, and it
will be automatically managed and called when needed and
window record will be updated accordingly to have a node reference."
  (when proxy-window
	(log-debug "getting node of window: ~a" proxy-window)
	(river-window-v1-get-node proxy-window)))

(define (wm-window-dimensions-propose proxy-window width height)
  "Propose dimensions (width and height) for a window.
Must be called in a ~manage_sequence~."
  (when proxy-window
    (log-debug "proposing dimensions ~ax~a for window: ~a" width height proxy-window)
    (river-window-v1-propose-dimensions proxy-window width height)))

(define (wm-window-hide proxy-window)
  "Request that the window be hidden.
Must be called in a ~render_sequence~."
  (when proxy-window  
    (log-debug "hiding window: ~a" proxy-window)
    (river-window-v1-hide proxy-window)))

(define (wm-window-show proxy-window)
  "Request that the window be shown.
Must be called in a ~render_sequence~."
  (when proxy-window  
    (log-debug "showing window: ~a" proxy-window)
    (river-window-v1-show proxy-window)))

(define (wm-window-decoration-client proxy-window)
  "Enable client-side decoration for the provided window.
Must be called in a ~manage_sequence~."
  (when proxy-window  
	(log-debug "using client side decoration for window: ~a" proxy-window)
	(river-window-v1-use-csd proxy-window)))

(define (wm-window-decoration-server proxy-window)
  "Enable server-side decoration for the provided window.
Must be called in a ~manage_sequence~."
  (when proxy-window  
	(log-debug "using server side decoration for window: ~a" proxy-window)
	(river-window-v1-use-ssd proxy-window)))

(define (wm-window-borders-set proxy-window edges width r g b a)
  "Set borders for the provided window.
edges: flag enum value, use `RIVER_WINDOW_V1_EDGES_NONE`,
`RIVER_WINDOW_V1_EDGES_TOP`, `RIVER_WINDOW_V1_EDGES_BOTTOM`,
`RIVER_WINDOW_V1_EDGES_RIGHT`, `RIVER_WINDOW_V1_EDGES_LEFT`
Must be called in a ~render_sequence~."
  (when proxy-window  
	(log-debug "setting borders for window: ~a to width ~a, edges: ~a, and rgba(~a, ~a, ~a, ~a)" proxy-window width edges r g b a)
	(river-window-v1-set-borders proxy-window edges width 
                                 r g b a)))

(define (wm-window-tiled-set proxy-window edges)
  "Set tiled state for the provided window.
edges: flag enum value, use `RIVER_WINDOW_V1_EDGES_NONE`,
`RIVER_WINDOW_V1_EDGES_TOP`, `RIVER_WINDOW_V1_EDGES_BOTTOM`,
`RIVER_WINDOW_V1_EDGES_RIGHT`, `RIVER_WINDOW_V1_EDGES_LEFT`
Must be called in a ~manage_sequence~."
  (when proxy-window  
	(log-debug "setting tiled state for window: ~a" proxy-window)
	(river-window-v1-set-tiled proxy-window edges)))

(define (wm-window-decoration-above-get proxy-window proxy-surface)
  "Create a decoration surface above the window and
assign the river_decoration_v1 role to the surface.
Provided ~wl_surface~ shouldn't have a role or a buffer attached."
  (when proxy-window  
	(log-debug "creating decoration surface ~a above window: ~a" proxy-surface proxy-window)
    (river-window-v1-get-decoration-above proxy-window proxy-surface)))

(define (wm-window-decoration-below-get proxy-window proxy-surface)
  "Create a decoration surface below the window and
assign the river_decoration_v1 role to the surface.
Provided ~wl_surface~ shouldn't have a role or a buffer attached."
  (when proxy-window  
	(log-debug "creating decoration surface ~a below window: ~a" proxy-surface proxy-window)
    (river-window-v1-get-decoration-below proxy-window proxy-surface)))

(define (wm-window-resize-started-inform proxy-window)
  "Inform the window that it is being resized.
Must be called in a ~manage_sequence~."
  (when proxy-window  
	(log-debug "resizing window ~a started" proxy-window)
	(river-window-v1-inform-resize-start proxy-window)))

(define (wm-window-resize-ended-inform proxy-window)
  "Inform the window that it has ended resizing.
Must be called in a ~manage_sequence~."
  (when proxy-window  
	(log-debug "resizing window ~a ended" proxy-window)
	(river-window-v1-inform-resize-end proxy-window)))

(define (wm-window-capabilities-inform proxy-window caps)
  "inform the window of the capabilities supported (maximize, minimize).
capabilities: flag enum value, use `RIVER_WINDOW_V1_CAPABILITIES_WINDOW_MENU`,
`RIVER_WINDOW_V1_CAPABILITIES_MAXIMIZE`, `RIVER_WINDOW_V1_CAPABILITIES_FULLSCREEN`,
`RIVER_WINDOW_V1_CAPABILITIES_MINIMIZE`.
Must be called in a ~manage_sequence~."
  (when proxy-window  
    (log-debug "set capabilities of window ~a to ~a" proxy-window caps)
    (river-window-v1-set-capabilities proxy-window caps)))

(define (wm-window-maximized-inform proxy-window)
  "inform the window that it has been maximized.
Must be called in a ~manage_sequence~."
  (when proxy-window  
    (log-debug "Window ~a has been maximized" proxy-window)
    (river-window-v1-inform-maximized proxy-window)))

(define (wm-window-unmaximized-inform proxy-window)
  "inform the window that it has been unmaximized.
Must be called in a ~manage_sequence~."
  (when proxy-window  
    (log-debug "Window ~a has been unmaximized" proxy-window)
    (river-window-v1-inform-unmaximized proxy-window)))

(define (wm-window-fullscreen-inform proxy-window)
  "inform the window that it has entered fullscreen mode.
Must be called in a ~manage_sequence~."
  (when proxy-window
    (log-debug "Window ~a entered fullscreen mode" proxy-window)
    (river-window-v1-inform-fullscreen proxy-window)))

(define (wm-window-fullscreen-exit-inform proxy-window)
  "inform the window that it has exited fullscreen mode.
Must be called in a ~manage_sequence~."
  (when proxy-window
	(log-debug "Window ~a exited fullscreen mode" proxy-window)
	(river-window-v1-inform-not-fullscreen proxy-window)))

(define (wm-window-fullscreen proxy-window proxy-output)
  "Make the window fullscreen on the given output. river_shell_surface_v1
objects above the window will still be rendered (move it to top).
This request automatically informs the window that it has entered fullscreen.
output: output object from which a proxy will be retrieved.
Must be called in a ~manage_sequence~."
  (when (and proxy-window proxy-output)
    (log-debug "Window ~a is entering fullscreen mode on output ~a" proxy-window proxy-output)
    (river-window-v1-fullscreen proxy-window proxy-output)))

(define (wm-window-fullscreen-exit proxy-window)
  "Make the window not fullscreen.
This request automatically informs the window that it has exited fullscreen.
Must be called in a ~manage_sequence~."
  (when proxy-window
    (log-debug "Window ~a is exiting fullscreen mode" proxy-window)
    (river-window-v1-exit-fullscreen proxy-window)))

(define (wm-window-clip-box-set proxy-window x y width height)
  "Clip the window, including borders and decoration surfaces.
Setting a clip box with 0 width or height disables clipping.
Clip box is ignored while window is on fullscreen.
Must be called in a ~manage_sequence~."
  (when proxy-window
    (log-debug "Apply clip box on window ~a at ~ax~a with size ~ax~a" proxy-window x y width height)
    (river-window-v1-set-clip-box proxy-window x y width height)))

(define (wm-window-content-clip-box-set proxy-window x y width height)
  "Clip the window, excluding borders and decoration surfaces.
Setting a clip box with 0 width or height disables clipping.
Clip box is ignored while window is on fullscreen.
Must be called in a ~manage_sequence~."
  (when proxy-window
    (log-debug "Apply content clip box on window ~a at ~ax~a with size ~ax~a" proxy-window x y width height)
    (river-window-v1-set-content-clip-box proxy-window x y width height)))

(define (wm-window-dimension-bounds-set proxy-window max-width max-height)
  "Recommend that the window keep its dimensions within a given width and height.
Setting bounds of 0 width or height indicates there are no bounds (default).
Must be called in a ~manage_sequence~."
  (when proxy-window
    (log-debug "Setting dimension bounds on window ~a with max size ~ax~a" proxy-window max-width max-height)
    (river-window-v1-set-dimension-bounds proxy-window max-width max-height)))

;;; window events
(define (on-window-closed data proxy-window)
  "Window was closed by the client. This will take care of
Removing the window record and clearing up memory.
Hook: *window-destroyed-hook*"
  ;; just let the window manager handle it
  (log-debug "Window closed: ~a" proxy-window)
  (gliver-hook-run! %window-destroyed-hook data proxy-window))

(define (on-window-dimensions-hint data proxy-window min-w min-h max-w max-h)
  "Window shared its preferred min/max dimensions excluding borders and decorations.
Hook: *window-size-hint-changed*"
  (log-debug "Window dimensions hint: ~a min=~ax~a max=~ax~a"
             proxy-window min-w min-h max-w max-h)
  (gliver-hook-run! %window-size-hint-changed-hook
                    proxy-window min-w min-h max-w max-h))

(define (on-window-dimensions data proxy-window width height)
  "Window committed new dimensions (sent before render_start).
Hook: *window-size-changed-hook*"
  (log-debug "Window dimensions: ~a ~ax~a" proxy-window width height)
  (gliver-hook-run! %window-size-changed-hook proxy-window width height))

(define (on-window-app-id data proxy-window app-id-ptr)
  "Window updated its app_id.
Hook: *window-app-id-changed-hook*"
  (let ((app-id (pointer->string app-id-ptr)))
    (log-debug "Window ~a updated its app_id to ~a" proxy-window app-id)
    (gliver-hook-run! %window-app-id-changed-hook proxy-window app-id)))

(define (on-window-title data proxy-window title-ptr)
  "Window updated its title.
Hook: *window-title-changed-hook*"
  (let ((title (pointer->string title-ptr)))
	(log-debug "Window ~a updated its title to ~a" proxy-window title)
	(gliver-hook-run! %window-title-changed-hook proxy-window title)))

(define (on-window-parent data proxy-window parent-proxy)
  "Window updated its parent.
Hook: *window-parent-changed-hook*"
  (log-debug "Window ~a updated its parent to ~a" proxy-window parent-proxy)
  (gliver-hook-run! %window-parent-changed-hook proxy-window parent-proxy))

(define (on-window-decoration-hint data proxy-window hint)
  "Window updated its preferred client side/server side decoration options.
hint: enum value `RIVER_WINDOW_V1_DECORATION_HINT_ONLY_SUPPORTS_CSD`,
`RIVER_WINDOW_V1_DECORATION_HINT_PREFERS_CSD`,
`RIVER_WINDOW_V1_DECORATION_HINT_PREFERS_SSD`,
`RIVER_WINDOW_V1_DECORATION_HINT_NO_PREFERENCE`.
Hook: *window-decoration-hint-changed-hook*"
  (log-debug "Window ~a updated its decoration hint to ~a" proxy-window hint)
  (gliver-hook-run! %window-decoration-hint-changed-hook proxy-window hint))

(define (on-window-pointer-move-requested data proxy-window proxy-seat)
  "Window requested interactive pointer move, e.g. titlebar drag."
  (log-debug "Window ~a is being dragged by seat ~a" proxy-window proxy-seat)
  (gliver-hook-run! %window-pointer-move-requested-hook proxy-window proxy-seat))

(define (on-window-pointer-resize-requested data proxy-window proxy-seat edges)
  "Window requested interactive pointer resize, e.g. edge drag."
  (log-debug "Window ~a is being dragged by seat ~a, edges ~a" proxy-window proxy-seat edges)
  (gliver-hook-run! %window-pointer-resize-requested-hook proxy-window proxy-seat edges))

(define (on-window-show-menu-requested data proxy-window x y)
  "Window requested interactive pointer resize, e.g. edge drag."
  (log-debug "Window ~a requested to show menu at ~ax~a" proxy-window x y)
  (gliver-hook-run! %window-menu-requested-hook proxy-window x y))

(define (on-window-maximize-requested data proxy-window)
  "Window requested to be maximized. The window manager
is free to honor this request with inform_maximize or ignore it.
The hook should be used by the active layout to manage this request."
  (log-debug "Window ~a requested to be maximized" proxy-window)
  (gliver-hook-run! %window-maximize-requested-hook proxy-window))

(define (on-window-unmaximize-requested data proxy-window)
  "Window requested to be unmaximized. The window manager
is free to honor this request with inform_unmaximize or ignore it.
The hook should be used by the active layout to manage this request."
  (log-debug "Window ~a requested to be unmaximized" proxy-window)
  (gliver-hook-run! %window-unmaximize-requested-hook proxy-window))

(define (on-window-fullscreen-requested data proxy-window output-ptr)
  "Window requested to enter fullscreen. The window manager
is free to honor this request with inform_fullscreen or ignore it.
The hook should be used by the active layout to manage this request."
  (log-debug "Window ~a requested to enter fullscreen on output ~a" proxy-window output-ptr)
  (gliver-hook-run! %window-fullscreen-requested-hook proxy-window output-ptr))

(define (on-window-exit-fullscreen-requested data proxy-window)
  "Window requested to exit fullscreen. The window manager
is free to honor this request with inform_not_fullscreen or ignore it.
The hook should be used by the active layout to manage this request."
  (log-debug "Window ~a requested to exit fullscreen" proxy-window)
  (gliver-hook-run! %window-fullscreen-exit-requested-hook proxy-window))

(define (on-window-minimize-requested data proxy-window)
  "Window requested to be minized. The window manager
is free to honor this request with hide or ignore it.
The hook should be used by the active layout to manage this request."
  (log-debug "Window ~a requested to be minimized" proxy-window)
  (gliver-hook-run! *window-minimize-requested-hook* proxy-window))

(define (on-window-unreliable-pid data proxy-window pid)
  "Return an unreliable PID of the process that created the window.
Only called once when the window is created."
  (log-debug "Window ~a received a new pid ~a" proxy-window pid)
  (gliver-hook-run! %window-pid-changed-hook proxy-window pid))

(define (on-window-presentation-hint data proxy-window hint)
  "Window updated its preferred presentation mode.
hint: enum value `RIVER_OUTPUT_V1_PRESENTATION_MODE_VSYNC`,
`RIVER_OUTPUT_V1_PRESENTATION_MODE_ASYNC`."
  (log-debug "Window ~a updated its preferred presentation hint to ~a" proxy-window hint)
  (gliver-hook-run! %window-presentation-hint-changed-hook proxy-window hint))

(define (on-window-identifier data proxy-window id-ptr)
  "The identifier is a string that contains up to 32 printable ASCII bytes.
The identifier will always be unique and will not be reused.
Only called once when the window is created."
  (let ((identifier (pointer->string id-ptr)))
    (log-debug "Window identifier: ~a = ~a" proxy-window identifier)
    (gliver-hook-run! %window-identifier-changed-hook proxy-window identifier)))

