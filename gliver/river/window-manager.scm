;;; gliver/river/window-manager.scm --- River compositor integration
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module integrates Gliver with the River Wayland compositor.
;;; It handles: Window management via river-window-management-v1 protocol
;;; Only one window management client may be active at a time.
;;; Maybe later add variables to disable the default window manager and
;;; other components to allow users to customize them
;;; TODO: create hooks later for all of river native events for more freedom
;;; TODO: divide this file into more files, probably one for each interface
;;; this will introduce dependency problems but will make the code most likely
;;; much more readable and more maintainable p

(define-module (gliver river window-manager)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (system foreign)
  #:use-module (gliver core logs)
  #:use-module (gliver core config)
  #:use-module (gliver core manager)
  #:use-module (gliver core hooks)
  #:use-module (gliver river connector)
  #:use-module (gliver wayland client)
  #:use-module (gliver wayland gen river-window-management-v1)
  #:export (*wm-manager*
			*wm-seats*
			*wm-outputs*
			proxy->node
			proxy-output
			hex-color->rgba
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
			*wm-manage-queue*
			*wm-render-queue*))

;; river_window_manager_v1 interface implementation
;; make sure to initialize and destroy these variables when needed
(define *wm-manager* %null-pointer)
(define *wm-seats* '())                 ;; list of river_seat_v1 proxies
(define *wm-output-listener* #f)           ;; reused for all river_output_v1 proxies

(define *wm-manage-queue* '())
(define *wm-render-queue* '())

(define *in-manage-sequence* #f)

;; extra helper variables and functions
(define RIVER_WINDOW_V1_EDGES_ALL 15)

(define (gliver-on-globals-bind registry protocol-name object-id version)
  "Bind and register the window manager"
  (when (string=? protocol-name RIVER_WINDOW_MANAGER_V1_NAME)
	(log-info "Binding ~a..." protocol-name)
	(set! *wm-manager*
		  (gliver-wl-registry-bind registry object-id
								   *river-window-manager-v1-interface*
								   (min version 4)))))

(define (gliver-on-globals-unbind)
  "Unbind/destroy the window manager"
  (unless (null-pointer? *wm-manager*)
    (river-window-manager-v1-destroy *wm-manager*)
    (set! *wm-manager* %null-pointer))

  ;; cleanup other state variables
  (set! *wm-seats* '())

  ;; clean up window nodes
  (for-each
   (lambda (win)
	 (let ((proxy (window-wl-node-proxy win)))
	   (when proxy
		 (river-node-v1-destroy (window-wl-node-proxy win)))))
   (manager-windows *manager*))

  ;; clean up all of windows
  (manager-windows-set! *manager* '()))

(define (gliver-on-globals-verify)
  "Verify critical globals were bound "
  (when (null-pointer? *wm-manager*)
    (log-error "river_window_manager_v1 not available, is River running?")
    (error "river_window_manager_v1 not available")))

(define (gliver-on-listeners-attach)
  "Attach wayland listeners"
  ;; set up the window manager event listener.
  ;; the window manager must be attached before
  ;; the roundtrip that delivers initial state
  ;; roundtrip is executed after this hook finish running by connector
  (let ((wm-listener
         (make-river-window-manager-v1-listener
          on-unavailable
          on-finished
          on-manage-start
          on-render-start
          on-session-locked
          on-session-unlocked
          on-window
          on-output
          on-seat)))
    (wl-proxy-add-listener *wm-manager*
						   wm-listener %null-pointer))

  (set! *wm-output-listener*
        (make-river-output-v1-listener
         on-output-removed
         on-output-wl-output
         on-output-position
         on-output-dimensions)))

;;; TODO: implement
;;; river_window_manager_v1: requests
(define (wm-manager-stop manager)
  "Close the manager, the manager may take time to closed. listen
for *manager-destroy-hook* in case an action other than clearing
state needs to be executed."
  (log-debug "Stopping manager ~a" manager)
  (when manager
	(let ((proxy-manager (manager-wl-proxy manager)))
      (when proxy-manager
		(river-window-manager-v1-stop proxy-manager)))))

(define (wm-manager-destroy manager)
  "Destroy the manager, this means everything was cleared from
client side and the server should also clear everything.
This most likely should be used internally only, and it
will be automatically managed and called when needed."
  (log-debug "Destroying manager ~a" manager)
  (when manager
	(let ((proxy-manager (manager-wl-proxy manager)))
      (when proxy-manager
		(river-window-manager-v1-destroy proxy-manager)))))

(define (wm-manager-manage-finish manager)
  "Client has made all changes to window management
state it wishes to include in the current manage sequence."
  (log-debug "Finishing manage sequence ~a" manager)
  (when manager
	(let ((proxy-manager (manager-wl-proxy manager)))
      (when proxy-manager
		(river-window-manager-v1-manage-finish proxy-manager)))))

(define (wm-manager-manage-dirty manager)
  "Ensures a manage sequence is started and that a
manage_start event is sent by the server."
  (log-debug "Dirty manage sequence ~a" manager)
  (when manager
	(let ((proxy-manager (manager-wl-proxy manager)))
      (when proxy-manager
		(river-window-manager-v1-manage-dirty proxy-manager)))))

(define (wm-manager-render-finish manager)
  "Client has made all changes to render state
server should atomically apply and display them."
  (log-debug "Dirty manage sequence ~a" manager)
  (when manager
	(let ((proxy-manager (manager-wl-proxy manager)))
      (when proxy-manager
		(river-window-manager-v1-render-finish proxy-manager)))))

(define (wm-manager-shell-surface-get manager proxy-surface)
  "Create a new shell surface for window manager UI
and assign the river_shell_surface_v1 role to the surface."
  (log-debug "Creating a new shell surface ~a" proxy-surface)
  (when manager
	(let ((proxy-manager (manager-wl-proxy manager)))
      (when proxy-manager
		(river-window-manager-v1-get-shell-surface proxy-manager proxy-surface)))))

(define (wm-manager-exit manager)
  "End the current Wayland session and exit the compositor."
  (log-debug "Exiting manager ~a" manager)
  (when manager
	(let ((proxy-manager (manager-wl-proxy manager)))
      (when proxy-manager
		(river-window-manager-v1-exit-session proxy-manager)))))

;;; window manager event handlers
;;; these are called by the window manager's wayland listener during
;;; wl_display_dispatch, events arrive in order:
;;; seat/output/window events -> manage_start -> (client does work) -> manage_finish
(define (on-unavailable data proxy-manager)
  "Indicates that window management is not available to the
client, perhaps due to another window management client already running."
  (log-error "Window management is unavailable, another window manager may be running")
  (river-disconnect!))

(define (on-finished data proxy-manager)
  "This event indicates that the server will send no further events on this
object. The client should destroy the object."
  (log-info "Window manager finished event received.")
  ;; assuming only one manager will be available at a time
  (wm-manager-destroy *manager*)
  (river-disconnect!))

(define (on-window data manager proxy-win)
  "Handle a new window event from the compositor.
Creates a core <window> record, attaches the event listener, and queues
the window for initial setup in the upcoming manage sequence."
  (log-info "New window proxy: ~a" proxy-win)
  (%window-created-hook data manager proxy-win))

(define (on-output data manager output-proxy)
  "Handle a new output event from the compositor.
Creates an output and attaches the output event listener."
  (log-info "New output proxy: ~a" output-proxy)
  (log-info "New output manager: ~a" manager)
  (log-info "New output manager: ~a" *manager*)
  (let ((outputs (manager-outputs *manager*)))
	(log-info "New output manager outputs: ~a" outputs)
	
	;; create an output for this proxy (name is proxy id for now)
	(let* ((out (make-output
				 (format #f "output-~a" (length outputs))
				 #:wl-proxy output-proxy)))
	  (output-add! out)
	  (log-info "Created output with proxy ~a" output-proxy)))

  ;; attach the shared event listener
  (when *wm-output-listener*
    (wl-proxy-add-listener output-proxy *wm-output-listener* %null-pointer)))

(define (on-seat data manager seat-proxy)
  "Handle a new seat event from the compositor."
  (log-debug "New seat proxy: ~a" seat-proxy)
  (%seat-created-hook data manager seat-proxy))

(define (on-session-locked data manager)
  (log-info "Session locked."))

(define (on-session-unlocked data manager)
  (log-info "Session unlocked."))

(define (on-manage-start data manager)
  "Handle manage start: execute pending actions and finish the sequence.
All window management state changes (keybinding enable/disable, focus
changes, etc.) must happen between manage_start and manage_finish."
  (log-debug "manage start")
  (set! *in-manage-sequence* #t)

  (catch #t
    (lambda ()

      ;; allow other modules (like xkb bindings) to perform manage sequence syncs
      (gliver-hook-run! *manager-manage-start-hook*)

      ;; process the tasks in *wm-manage-queue*
	  (define (process-queue!)
		(unless (null? *wm-manage-queue*)
		  (let ((task (car *wm-manage-queue*)))
			(set! *wm-manage-queue* (cdr *wm-manage-queue*))
			(task)
			(process-queue!))))

      (let* ((seat (and (not (null? *wm-seats*)) (car *wm-seats*)))
             (output (output-current))
			 (windows (manager-windows *manager*))
			 (pending (filter window-wl-pending windows)))
		(unless (null? pending)
          (for-each
           (lambda (win)
             (let ((proxy-win (window-wl-proxy win)))
               (when win
                 (let ((container (window-container win)))
                   (if container

                       ;; tiled window: use container geometry
                       (let ((w (max 1 (container-width container)))
                             (h (max 1 (container-height container))))
                         (log-info "Proposing dimensions ~ax~a for tiled window ~a"
                                   w h (window-id win))
                         (river-window-v1-propose-dimensions proxy-win w h)
                         (river-window-v1-set-tiled proxy-win RIVER_WINDOW_V1_EDGES_ALL))

                       ;; floating window: use a portion of output
                       (let ((w (if output
                                    (max 1 (quotient (* (output-width output) 2) 3))
                                    640))
                             (h (if output
                                    (max 1 (quotient (* (output-height output) 2) 3))
                                    480)))
                         (log-info "Proposing dimensions ~ax~a for floating window ~a"
                                   w h (window-id win))
                         (river-window-v1-propose-dimensions proxy-win w h)
                         (river-window-v1-set-tiled proxy-win RIVER_WINDOW_V1_EDGES_NONE)))

                   ;; set capabilities enum (menu, maximize, fullscreen, minimize)
				   ;; TODO: allow customization from variables
                   (river-window-v1-set-capabilities proxy-win 15)

                   ;; inform not fullscreen / unmaximized
                   (river-window-v1-inform-not-fullscreen proxy-win)
                   (river-window-v1-inform-unmaximized proxy-win)

                   ;; focus the newest window
                   (when seat
                     (river-seat-v1-focus-window seat proxy-win))))))
           pending))))
    (lambda (key . args)
      (log-error "Error in manage sequence: ~a ~a" key args)))

  ;; always finish the manage sequence
  (wm-manager-manage-finish *wm-manager*)
  (set! *in-manage-sequence* #f))

(define (on-render-start data manager)
  "Handle render start: position, show, and style all windows, then finish.
The server sends window dimension events before this, so nodes can be
positioned accurately."
  (log-debug "render start")
  (catch #t
    (lambda ()
      (let ((output (output-current)))
        ;; for each tracked window, get/cache node, set position, show, set borders
        (for-each
         (lambda (win)
           (let* ((win-proxy (window-wl-proxy win))
                  (win-addr (pointer-address win-proxy))
				  (node-proxy (window-wl-node-proxy win))
                  (node-addr (pointer-address win-proxy)))
             (when win
               ;; get or cache the scene node (get_node can only be called once)
			   ;; this shouldn't be needed cuz on-window should handle it
			   (unless node-proxy
				 (let ((n (river-window-v1-get-node win-proxy)))
                   (unless (null-pointer? n)
					 (log-warn "node proxy was set on rendering!")
					 (window-wl-node-proxy-set! win n))))

               ;; position the node
			   (let ((node (window-wl-node-proxy win))
                     (container (window-container win)))
               (when (and node (not (null-pointer? node)))
                 (if container
                     ;; tiled: use container geometry
                     (river-node-v1-set-position node
                                                 (container-x container)
                                                 (container-y container))
                     ;; floating: center on output
                     (when output
                       (river-node-v1-set-position
                        node
                        (quotient (output-width output) 6)
                        (quotient (output-height output) 6))))
                 (river-node-v1-place-top node)))

               ;; show the window
               (river-window-v1-show win-proxy)

               ;; apply border configuration
               (let ((focused? (eq? win (window-current))))
                 (log-info "do border here")))))
         (manager-windows *manager*))))
    (lambda (key . args)
      (log-error "Error in render sequence: ~a ~a" key args)))

  ;; always finish the render sequence
  (wm-manager-render-finish *wm-manager*))

;;; =====================================
;;; river_output_v1 interface
;;; =====================================

;;; output requests
(define (wm-output-destroy proxy-output)
  "Destroy an output, this means everything was cleared from
client side and the server should also clear everything.
This most likely should be used internally only, and it
will be automatically managed and called when needed."
  (log-debug "destroying output proxy: ~a" proxy-output)
  (when proxy-output (river-output-v1-destroy proxy-output)))

(define (wm-output-presentation-mode-set output mode)
  "Set the preferred presentation mode of the output.
mode: enum value `RIVER_OUTPUT_V1_PRESENTATION_MODE_VSYNC`,
`RIVER_OUTPUT_V1_PRESENTATION_MODE_ASYNC`."
  (let ((proxy-output (output-wl-proxy output)))
    (when proxy-output
      (log-debug "Setting output ~a presentation mode to ~a" proxy-output mode)
      (river-output-v1-set-presentation-mode proxy-output mode))))

;;; output events
(define (on-output-removed data proxy-output)
  "Output was removed. This will take care of
Removing the output record and clearing up memory.
Hook: *output-destroy-hook*"
  (log-debug "Output removed: ~a" proxy-output)
  (let ((output (output-find-by-proxy proxy-output)))
    (when output (output-remove! output))
	(wm-output-destroy proxy-output)
	(gliver-hook-run! *output-destroy-hook* output)))

(define (on-output-wl-output data proxy-output name)
  "The wl_output object corresponding to the river_output_v1."
  (log-debug "Output wl_output global name: ~a = ~a" proxy-output name)
  (let ((output (output-find-by-proxy proxy-output)))
    (when output
	  (log-debug "got output ~a" output)
	  (let ((prev-name (output-name output)))
		(output-name-set! output name)
		(gliver-hook-run! *output-name-changed-hook* output prev-name)))))

(define (on-output-position data proxy-output x y)
  "Position of the output in the compositor's logical coordinate
space changed. The x and y coordinates may be positive or negative."
  (log-debug "Output position: ~a = ~a,~a" proxy-output x y)
  (let ((output (output-find-by-proxy proxy-output)))
    (when output
	  (let ((prev-x (output-x output))
			(prev-y (output-y output)))
		(output-x-set! output x)
		(output-y-set! output y)
		(gliver-hook-run! *output-position-changed-hook* output prev-x prev-y)))))

(define (on-output-dimensions data proxy-output width height)
  (log-info "Output dimensions: ~a = ~ax~a" proxy-output width height)
  (let ((output (output-find-by-proxy proxy-output)))
    (when output
	  (let ((prev-width (output-width output))
			(prev-height (output-height output)))
		(output-width-set! output width)
		(output-height-set! output height)
		(gliver-hook-run! *output-dimensions-changed-hook* output prev-width prev-height)))))

;; handle river initialization steps
(gliver-hook-add! *gliver-globals-bind-hook* gliver-on-globals-bind)
(gliver-hook-add! *gliver-globals-unbind-hook* gliver-on-globals-unbind)
(gliver-hook-add! *gliver-globals-verify-hook* gliver-on-globals-verify)
(gliver-hook-add! *gliver-listeners-attach-hook* gliver-on-listeners-attach)
