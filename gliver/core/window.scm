;;; gliver/core.scm --- Core data model for Gliver
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
  #:use-module (gliver core manager)
  #:use-module (gliver core container)
  #:use-module (gliver core workspace)
  #:use-module (gliver core output)
  #:use-module (gliver core config)
  #:use-module (gliver core logs)
  #:use-module (gliver core hooks)
  #:use-module (gliver keybindings)
  #:export (;; window
            <window>
			make-window
			window?
			window-id window-id-set!
			window-app-id window-app-id-set!
			window-class window-class-set!
			window-instance window-instance-set!
            window-title window-title-set!
            window-container window-container-set!
            window-floating? window-floating-set!
            window-fullscreen? window-fullscreen-set!
            window-transient? window-transient-set!
            window-marked? window-marked-set!
            window-urgent? window-urgent-set!
            window-user-props window-user-props-set!
			window-height-min window-height-min-set!
			window-height-max window-height-max-set!
			window-height window-height-set!
			window-width-min window-width-min-set!
			window-width-max window-width-max-set!
			window-width window-width-set!
			window-decoration-hint-set! window-decoration-hint
			window-wl-proxy window-wl-proxy-set!
			window-wl-node-proxy window-wl-node-proxy-set!
			window-wl-pending window-wl-pending-set!
			window-find-by-proxy
            window-current
            window-add!
            window-remove!
            window-move-to-container!
            window-move-to-workspace!
            window-toggle-float!
            window-find-by-id
			window-size-set!
			window-hide!
			window-show!
			window-decoration-below-set!
			window-decoration-below
			window-decoration-above-set!
			window-decoration-above
			window-wl-decoration-below-proxy-set!
			window-wl-decoration-below-proxy
			window-wl-decoration-above-proxy-set!
			window-wl-decoration-above-proxy
			window-identifier-set!
			window-identifier
			window-prsentation-hint-set!
			window-prsentation-hint
			window-pid-set!
			window-pid
			window-maximized-set!
			window-maximized?
			window-visible-set!
			window-visible?
			window-capabilities-set!
			window-capabilities
			window-is-resizing-set!
			window-is-resizing
			window-presentation-hint-set!
			window-presentation-hint
			window-parent-set!
			window-parent
			window-visbile-set!
			%make-window
			window-workspace
			window-output))

;;; window: similar to an emacs buffer and stumpwm window
;;; a single application (like a terminal, a browser, or an editor)
(define-record-type <window>
  (%make-window id title app-id class instance container
				floating? transient? marked? urgent? user-props
                height-min height-max height width-min width-max width
                decoration-hint decoration-above decoration-below
                is-resizing capabilities fullscreen? maximized? visible?
                pid parent presentation-hint identifier
                wl-proxy wl-node-proxy wl-decoration-above wl-decoration-below wl-pending)
  window?
  (id               window-id               window-id-set!)
  (title            window-title            window-title-set!)
  (app-id           window-app-id           window-app-id-set!)
  (class            window-class            window-class-set!)
  (instance         window-instance         window-instance-set!)
  (container        window-container        window-container-set!)
  (floating?        window-floating?        window-floating-set!)
  (transient?       window-transient?       window-transient-set!)
  (marked?          window-marked?          window-marked-set!)
  (urgent?          window-urgent?          window-urgent-set!)
  (user-props       window-user-props       window-user-props-set!)
  (height-min       window-height-min       window-height-min-set!)
  (height-max       window-height-max       window-height-max-set!)
  (height           window-height           window-height-set!)
  (width-min        window-width-min        window-width-min-set!)
  (width-max        window-width-max        window-width-max-set!)
  (width            window-width            window-width-set!)
  (decoration-hint  window-decoration-hint  window-decoration-hint-set!)
  (decoration-above window-decoration-above window-decoration-above-set!)
  (decoration-below window-decoration-below window-decoration-below-set!)  
  (is-resizing      window-is-resizing      window-is-resizing-set!)
  (capabilities     window-capabilities     window-capabilities-set!)
  (fullscreen?      window-fullscreen?      window-fullscreen-set!)
  (maximized?       window-maximized?       window-maximized-set!)
  (visible?         window-visible?         window-visbile-set!)
  (pid              window-pid              window-pid-set!)
  (parent           window-parent           window-parent-set!)
  (presentation-hint  window-presentation-hint  window-presentation-hint-set!)
  (identifier       window-identifier       window-identifier-set!)
  (wl-proxy         window-wl-proxy         window-wl-proxy-set!)
  (wl-node-proxy    window-wl-node-proxy    window-wl-node-proxy-set!)
  (wl-decoration-above    window-wl-decoration-above-proxy    window-wl-decoration-above-proxy-set!)
  (wl-decoration-below    window-wl-decoration-below-proxy    window-wl-decoration-below-proxy-set!)
  (wl-pending       window-wl-pending       window-wl-pending-set!))

(set-record-type-printer! <window>
  (lambda (win port)
    (format port "#<window ~a ~s app-id=~s>"
            (window-id win)
            (window-title win)
            (window-app-id win))))

(define* (make-window #:key
                      (id (manager-window-number-next!))
                      (title "") (app-id "") (class "")
                      (instance "") (container #f)
                      (floating? #f) (transient? #f)
                      (marked? #f) (urgent? #f) (user-props '())
                      (height-min #f) (height-max #f) (height #f)
                      (width-min #f) (width-max #f) (width #f)
                      (decoration-hint #f) (decoration-above #f)
                      (decoration-below #f) (is-resizing #f)
                      (capabilities '()) (fullscreen? #f) (maximized? #f)
                      (visible? #t) (pid #f) (parent #f) (presentation-hint #f)
                      (identifier #f) (wl-proxy #f) (wl-node-proxy #f)
                      (wl-decoration-above #f) (wl-decoration-below #f)
                      (wl-pending #f))

  (%make-window id title app-id class instance container
                floating? transient? marked? urgent? user-props
                height-min height-max height width-min width-max width
                decoration-hint decoration-above decoration-below
                is-resizing capabilities fullscreen? maximized?
                visible? pid parent presentation-hint identifier
                wl-proxy wl-node-proxy wl-decoration-above wl-decoration-below wl-pending))

(define (window-find-by-proxy proxy)
  "Look up the <window> record by comparing the raw memory address of the proxy."
  (if (not (pointer? proxy))
      #f ;; early exit
      (let ((addr (pointer-address proxy))
            (windows (manager-windows *manager*)))
        (find (lambda (win)
                (let ((win-proxy (window-wl-proxy win)))
                  (and (pointer? win-proxy)
                       (= (pointer-address win-proxy) addr))))
              windows))))

(define (window-find-by-id id)
  "Find a window by its ID."
  (find (lambda (w) (= (window-id w) id))
        (manager-windows *manager*)))

(define (window-current)
  "Return the currently focused window."
  (let ((f (container-current)))
    (and f (container-window-current f))))

(define (window-workspace win)
  "Return the window workspace."
  (container-workspace (window-container win)))

(define (window-output win)
  "Return the window output."
  (workspace-output (window-workspace win)))

(define (window-close win)
  "Close a window, the window may take time to respond or
completely ignore the request. listen for *window-destroy-hook*
in case an action other than clearing state needs to be executed."
  (log-debug "closing window ~a" win)
  (when win
	(let ((proxy-win (window-wl-proxy win)))
      (when proxy-win
		(log-debug "getting node of window: ~a" proxy-win)
		(river-window-v1-close proxy-win)))))

(define (window-destroy proxy)
  "Destroy a window, this means everything was cleared from
client side and the server should also clear everything.
This most likely should be used internally only, and it
will be automatically managed and called when needed."
  (log-debug "destroying window proxy: ~a" proxy)  
  (when proxy (river-window-v1-destroy proxy)))

(define (window-node-get win)
  "Return node that corresponds to the window, This can only be
called once per window, so it should be cached after first call.
This most likely should be used internally only, and it
will be automatically managed and called when needed and
window record will be updated accordingly to have a node reference."
  (when win
	(let ((proxy-win (window-wl-proxy win))
		  (proxy-node (window-wl-node-proxy win)))
	  (log-debug "getting node of window: ~a" proxy-win)
	  ;; only fetch if win proxy is available but node proxy isn't
	  ;; the retrieved value will be stored in window record
      (when (and proxy-win (not proxy-node))
		(let ((node (river-window-v1-get-node proxy-win)))
		  ;; update window state and return the new node
		  (window-wl-node-proxy-set! win node)
		  node))
	  ;; node already exists
	  proxy-node)))

(define (window-dimensions-propose win width height)
  "Propose dimensions (width and height) for a window.
Must be called in a ~manage_sequence~."
  (when win
	(let ((proxy-win (window-wl-proxy win)))
	  (log-debug "proposing dimensions ~ax~a for window: ~a" width height proxy-win)
	  ;; only fetch if win proxy is available but node proxy isn't
	  ;; the retrieved value will be stored in window record
      (when proxy-win
		(river-window-v1-propose-dimensions proxy-win width height)))))

(define (window-hide win)
  "Request that the window be hidden.
Must be called in a ~render_sequence~."
  (when win
	(let ((proxy-win (window-wl-proxy win)))
	  (log-debug "hiding window: ~a" proxy-win)
	  ;; proceed if we have a proxy value
      (when proxy-win
		(river-window-v1-hide proxy-win)))))

(define (window-show win)
  "Request that the window be shown.
Must be called in a ~render_sequence~."
  (when win
	(let ((proxy-win (window-wl-proxy win)))
	  (log-debug "showing window: ~a" proxy-win)
	  ;; proceed if we have a proxy value
      (when proxy-win
		(river-window-v1-show proxy-win)))))

(define (window-decoration-client win)
  "Enable client-side decoration for the provided window.
Must be called in a ~manage_sequence~."
  (when win
	(let ((proxy-win (window-wl-proxy win)))
	  (log-debug "using client side decoration for window: ~a" proxy-win)
	  ;; proceed if we have a proxy value
      (when proxy-win
		(river-window-v1-use-csd proxy-win)))))

(define (window-decoration-server win)
  "Enable server-side decoration for the provided window.
Must be called in a ~manage_sequence~."
  (when win
	(let ((proxy-win (window-wl-proxy win)))
	  (log-debug "using server side decoration for window: ~a" proxy-win)
	  ;; proceed if we have a proxy value
      (when proxy-win
		(river-window-v1-use-ssd proxy-win)))))

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

(define (window-borders-set win edges width color-hex)
  "Set borders for the provided window.
edges: flag enum value, use `RIVER_WINDOW_V1_EDGES_NONE`,
`RIVER_WINDOW_V1_EDGES_TOP`, `RIVER_WINDOW_V1_EDGES_BOTTOM`,
`RIVER_WINDOW_V1_EDGES_RIGHT`, `RIVER_WINDOW_V1_EDGES_LEFT`
Must be called in a ~render_sequence~."
  (when win
	(let ((proxy-win (window-wl-proxy win)))
	  (log-debug "setting borders for window: ~a" proxy-win)
	  ;; proceed if we have a proxy value
      (when proxy-win
		(apply river-window-v1-set-borders 
               proxy-win edges width 
               (color-hex->rgba color-hex))))))

(define (window-tiled-set win edges)
  "Set tiled state for the provided window.
edges: flag enum value, use `RIVER_WINDOW_V1_EDGES_NONE`,
`RIVER_WINDOW_V1_EDGES_TOP`, `RIVER_WINDOW_V1_EDGES_BOTTOM`,
`RIVER_WINDOW_V1_EDGES_RIGHT`, `RIVER_WINDOW_V1_EDGES_LEFT`
Must be called in a ~render_sequence~."
  (when win
	(let ((proxy-win (window-wl-proxy win)))
	  ;; proceed if we have a proxy value
      (when proxy-win
		(log-debug "setting tiled state for window: ~a" proxy-win)
		(river-window-v1-set-tiled proxy-win edges)))))

(define (window-decoration-above-get win proxy-surface)
  "Create a decoration surface above the window and
assign the river_decoration_v1 role to the surface.
Provided ~wl_surface~ shouldn't have a role or a buffer attached."
  (when win
	(let ((proxy-win (window-wl-proxy win)))
	  ;; proceed if we have a proxy value
      (when (and proxy-win proxy-surface)
		(log-debug "creating decoration surface ~a above window: ~a" proxy-surface proxy-win)
		(let ((decoration (river-window-v1-get-decoration-above
						   proxy-win proxy-surface)))
		  ;; update window state and return the new decoration
		  (window-wl-decoration-above-proxy-set! win decoration)
		  decoration)))))

(define (window-decoration-below-get win proxy-surface)
  "Create a decoration surface below the window and
assign the river_decoration_v1 role to the surface.
Provided ~wl_surface~ shouldn't have a role or a buffer attached."
  (when win
	(let ((proxy-win (window-wl-proxy win)))
	  ;; proceed if we have a proxy value
      (when (and proxy-win proxy-surface)
		(log-debug "creating decoration surface ~a below window: ~a" proxy-surface proxy-win)
		(let ((decoration (river-window-v1-get-decoration-below
						   proxy-win proxy-surface)))
		  ;; update window state and return the new decoration
		  (window-wl-decoration-below-proxy-set! win decoration)
		  decoration)))))

(define (window-resize-started-inform win)
  "Inform the window that it is being resized.
Must be called in a ~manage_sequence~."
  (when win
	(let ((proxy-win (window-wl-proxy win)))
	  ;; proceed if we have a proxy value
      (when proxy-win
		(log-debug "resizing window ~a started" proxy-win)
		(river-window-v1-inform-resize-start proxy-win)
		(window-is-resizing-set! win #t)
		(gliver-hook-run! *window-resize-start-hook* win)))))

(define (window-resize-ended-inform win)
  "Inform the window that it has ended resizing.
Must be called in a ~manage_sequence~."
  (when win
	(let ((proxy-win (window-wl-proxy win)))
	  ;; proceed if we have a proxy value
      (when proxy-win
		(log-debug "resizing window ~a ended" proxy-win)
		(river-window-v1-inform-resize-end proxy-win)
		(window-is-resizing-set! win #f)
		(gliver-hook-run! *window-resize-end-hook* win)))))

(define (window-capabilities-inform win caps)
  "inform the window of the capabilities supported (maximize, minimize).
capabilities: flag enum value, use `RIVER_WINDOW_V1_CAPABILITIES_WINDOW_MENU`,
`RIVER_WINDOW_V1_CAPABILITIES_MAXIMIZE`, `RIVER_WINDOW_V1_CAPABILITIES_FULLSCREEN`,
`RIVER_WINDOW_V1_CAPABILITIES_MINIMIZE`.
Must be called in a ~manage_sequence~."
  (when win
	(let ((proxy-win (window-wl-proxy win)))
	  ;; proceed if we have a proxy value
      (when proxy-win
		(let ((prev-caps (window-capabilities win)))
		  (log-debug "set capabilities of window ~a to ~a" proxy-win caps)
		  (river-window-v1-set-capabilities proxy-win caps)
		  (window-capabilities-set! win caps)
		  (gliver-hook-run! *window-capabilities-changed-hook* win prev-caps))))))

(define (window-maximized-inform win)
  "inform the window that it has been maximized.
Must be called in a ~manage_sequence~."
  (when win
	(let ((proxy-win (window-wl-proxy win)))
	  ;; proceed if we have a proxy value
      (when proxy-win
		(let ((prev-status (window-maximized? win)))
		  (log-debug "Window ~a has been maximized" proxy-win)
		  (river-window-v1-inform-maximized proxy-win)
		  (window-maximized-set! win #t)
		  (gliver-hook-run! *window-maximized-hook* win prev-status))))))

(define (window-unmaximized-inform win)
  "inform the window that it has been unmaximized.
Must be called in a ~manage_sequence~."
  (when win
	(let ((proxy-win (window-wl-proxy win)))
	  ;; proceed if we have a proxy value
      (when proxy-win
		(let ((prev-status (window-maximized? win)))
		  (log-debug "Window ~a has been unmaximized" proxy-win)
		  (river-window-v1-inform-unmaximized proxy-win)
		  (window-maximized-set! win #f)
		  (gliver-hook-run! *window-unmaximized-hook* win prev-status))))))

(define (window-fullscreen-inform win)
  "inform the window that it has entered fullscreen mode.
Must be called in a ~manage_sequence~."
  (when win
	(let ((proxy-win (window-wl-proxy win)))
	  ;; proceed if we have a proxy value
      (when proxy-win
		(let ((prev-status (window-fullscreen? win)))
		  (log-debug "Window ~a entered fullscreen mode" proxy-win)
		  (river-window-v1-inform-fullscreen proxy-win)
		  (window-fullscreen-set! win #t)
		  (gliver-hook-run! *window-fullscreen-entered-hook* win prev-status))))))

(define (window-fullscreen-exit-inform win)
  "inform the window that it has exited fullscreen mode.
Must be called in a ~manage_sequence~."
  (when win
	(let ((proxy-win (window-wl-proxy win)))
	  ;; proceed if we have a proxy value
      (when proxy-win
		(let ((prev-status (window-fullscreen? win)))
		  (log-debug "Window ~a exited fullscreen mode" proxy-win)
		  (river-window-v1-inform-not-fullscreen proxy-win)		  
		  (window-fullscreen-set! win #f)
		  (gliver-hook-run! *window-fullscreen-exited-hook* win prev-status))))))

(define (window-fullscreen win output)
  "Make the window fullscreen on the given output. river_shell_surface_v1
objects above the window will still be rendered (move it to top).
This request automatically informs the window that it has entered fullscreen.
output: output object from which a proxy will be retrieved.
Must be called in a ~manage_sequence~."
  (when win
	(let ((proxy-win (window-wl-proxy win))
		  (proxy-output (output-wl-proxy output)))
	  ;; proceed if we have a proxy value
      (when (and proxy-win proxy-output)
		(let ((prev-status (window-fullscreen? win)))
		  (log-debug "Window ~a is entering fullscreen mode on output ~a" proxy-win proxy-output)
		  (river-window-v1-fullscreen proxy-win proxy-output)
		  (wm-window-fullscreen-inform win))))))

(define (window-fullscreen-exit win)
  "Make the window not fullscreen.
This request automatically informs the window that it has exited fullscreen.
Must be called in a ~manage_sequence~."
  (when win
	(let ((proxy-win (window-wl-proxy win)))
	  ;; proceed if we have a proxy value
      (when proxy-win
		(let ((prev-status (window-fullscreen? win)))
		  (log-debug "Window ~a is exiting fullscreen mode" proxy-win)
		  (river-window-v1-exit-fullscreen proxy-win)
		  (wm-window-fullscreen-exit-inform win))))))

(define (window-clip-box-set win x y width height)
  "Clip the window, including borders and decoration surfaces.
Setting a clip box with 0 width or height disables clipping.
Clip box is ignored while window is on fullscreen.
Must be called in a ~manage_sequence~."
  (when win
	(let ((proxy-win (window-wl-proxy win)))
	  ;; proceed if we have a proxy value
      (when proxy-win
		  (log-debug "Apply clip box on window ~a at ~ax~a with size ~ax~a" proxy-win x y width height)
		  (river-window-v1-set-clip-box proxy-win x y width height)))))

(define (window-content-clip-box-set win x y width height)
  "Clip the window, excluding borders and decoration surfaces.
Setting a clip box with 0 width or height disables clipping.
Clip box is ignored while window is on fullscreen.
Must be called in a ~manage_sequence~."
  (when win
	(let ((proxy-win (window-wl-proxy win)))
	  ;; proceed if we have a proxy value
      (when proxy-win
		  (log-debug "Apply content clip box on window ~a at ~ax~a with size ~ax~a" proxy-win x y width height)
		  (river-window-v1-set-content-clip-box proxy-win x y width height)))))

(define (window-dimension-bounds-set win max-width max-height)
  "Recommend that the window keep its dimensions within a given width and height.
Setting bounds of 0 width or height indicates there are no bounds (default).
Must be called in a ~manage_sequence~."
  (when win
	(let ((proxy-win (window-wl-proxy win)))
	  ;; proceed if we have a proxy value
      (when proxy-win
		  (log-debug "Setting dimension bounds on window ~a with max size ~ax~a" proxy-win max-width max-height)
		  (river-window-v1-set-dimension-bounds proxy-win max-width max-height)))))

(define (window-size-set! win width height)
  "Resize the window, impact only floating windows and stacking layouts."
  ;; proposing dimensions is only possible in manage sequence
  (let* ((wm-mod (resolve-interface '(gliver river window-manager)))
         (queue (module-ref wm-mod '*wm-manage-queue*))
         (prop-func (module-ref wm-mod 'wm-window-dimensions-propose))
         (task (lambda () (prop-func win width height))))
    (module-set! wm-mod '*wm-manage-queue* (append queue (list task)))))

(define (window-hide! win)
  "Resize the window, impact only floating windows and stacking layouts."
  ;; proposing dimensions is only possible in manage sequence
  (let* ((wm-mod (resolve-interface '(gliver river window-manager)))
         (queue (module-ref wm-mod '*wm-manage-queue*))
         (hide-func (module-ref wm-mod 'wm-window-hide))
         (task (lambda () (hide-func win))))
    (module-set! wm-mod '*wm-manage-queue* (append queue (list task)))))

(define (window-show! win)
  "Resize the window, impact only floating windows and stacking layouts."
  ;; proposing dimensions is only possible in manage sequence
  (let* ((wm-mod (resolve-interface '(gliver river window-manager)))
         (queue (module-ref wm-mod '*wm-manage-queue*))
         (show-func (module-ref wm-mod 'wm-window-show))
         (task (lambda () (show-func win))))
    (module-set! wm-mod '*wm-manage-queue* (append queue (list task)))))

(define (window-add! win)
  "Add a new window to the display, placing it in the current container."
  (let ((container (container-current)))
    (when container
	  (container-windows-set! container
							  (cons win (container-windows container)))
	  (when *wm-behavior-focus-new-window*
		(container-window-current-set! container win))
      (gliver-hook-run! *window-new-hook* win)))
  win)

(define (window-remove! win)
  "Remove a window from the display."
  (let ((container (window-container win)))
    (when container
      (container-windows-set! container (delete win (container-windows container)))
	  ;; if removed window is currently focused
      (when (eq? (container-window-current container) win)
        (container-window-current-set! container
          (and (pair? (container-windows container))
               (car (container-windows container))))))
    (gliver-hook-run! *window-destroy-hook* win)))

(define (window-move-to-container! win target-container)
  "Move WIN from its current container to TARGET-CONTAINER."
  (let ((old-container (window-container win)))
    (when old-container
      (container-windows-set! old-container (delete win (container-windows old-container)))
      (when (eq? (container-window-current old-container) win)
        (container-window-current-set! old-container
          (and (pair? (container-windows old-container))
               (car (container-windows old-container))))))
    (window-container-set! win target-container)
    (container-windows-set! target-container
      (cons win (container-windows target-container)))
    (container-window-current-set! target-container win)
    (gliver-hook-run! *window-place-hook* win target-container)))

(define (window-move-to-workspace! win workspace)
  "Move WIN to WORKSPACE."
  (let ((old-container (window-container win))
		(new-container (or (workspace-container-current workspace)
						   (last (workspace-containers workspace)))))
    ;; remove from old location
    (when old-container
      (container-windows-set! old-container (delete win (container-windows old-container)))
      (when (eq? (container-window-current old-container) win)
        (container-window-current-set! old-container
          (and (pair? (container-windows old-container))
               (car (container-windows old-container))))))

    ;; add to new workspace
	(container-windows-set! new-container (delete win (container-windows old-container)))
    (window-container-set! win new-container)))


(define (on-window data manager window-proxy)
  "Handle a new window event from the compositor."
  (let ((win (make-window
			  #:wl-proxy proxy-win
			  #:wl-pending #t)))
    (window-add! win))

(gliver-hook-add! %seat-created-hook on-seat)
