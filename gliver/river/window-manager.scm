;;; gliver/river/window-manager.scm --- River window management protocol integration
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module manages window management in river:
;;; It handles:
;;;   - Binding river_window_manager_v1 and attaching the primary Wayland event listener
;;;   - Handling manage sequences (manage_start / manage_finish)
;;;   - Handling render sequences (render_start / render_finish)
;;;   - Managing river basic events for windows, outputs, seats, and session
;;;
;;; This module implements the river interfaces (protocol river_window_manager_v1):
;;;   - river_window_manager_v1
;;;
;;; NOTE: the rest of the interfaces that belong to river_window_manager_v1
;;; are delegated to other managers that are prefixed with wm- under the river directory

(define-module (gliver river window-manager)
  #:use-module (system foreign)
  #:use-module (gliver core-base)
  #:use-module (gliver river connector)
  #:use-module (gliver wayland client)
  #:use-module (gliver wayland gen river-window-management-v1)
  #:export (
			*wm-manage-queue*
			*wm-render-queue*
			*in-manage-sequence*
			*in-render-sequence*
			window-manager-on-globals-bind
			window-manager-on-globals-unbind
			window-manager-on-globals-verify
			window-manager-on-listeners-attach
			wm-manager-stop
			wm-manager-destroy
			wm-manager-manage-finish
			wm-manager-manage-dirty
			wm-manager-render-finish
			wm-manager-shell-surface-get
			wm-manager-exit
			wm-on-unavailable
			wm-on-finished
			wm-on-window
			wm-on-output
			wm-on-seat
			wm-on-session-locked
			wm-on-session-unlocked
			wm-on-manage-start
			wm-on-render-start
			with-manage-sequence
			with-render-sequence
))

;; river_window_manager_v1 interface implementation
;; make sure to initialize and destroy these variables when needed
(define *wm-manage-queue* '())
(define *wm-render-queue* '())

(define *in-manage-sequence* #f)
(define *in-render-sequence* #f)

(define (window-manager-on-globals-bind registry protocol-name object-id version)
  "Bind river_window_manager_v1 global interface from REGISTRY.
Parameters: (REGISTRY PROTOCOL-NAME OBJECT-ID VERSION)
  - REGISTRY: Pointer to wl_registry proxy.
  - PROTOCOL-NAME: Interface name string.
  - OBJECT-ID: Numeric Wayland object ID.
  - VERSION: Maximum interface version supported by compositor."
  (when (string=? protocol-name RIVER_WINDOW_MANAGER_V1_NAME)
    (log-info "Binding ~a..." protocol-name)
	(let ((proxy (gliver-wl-registry-bind registry object-id
                                          *river-window-manager-v1-interface*
                                          (min version 4))))
	  (%manager-wl-proxy-set! *manager* proxy)
	  (log-info "Manager ~a proxy is set to ~a" *manager* proxy))))

(define (window-manager-on-globals-unbind)
  "Unbind and destroy the river_window_manager_v1 proxy.
Parameters: None."
  (let ((proxy (manager-wl-proxy *manager*)))
	(when (valid-pointer? proxy)
	  (wm-manager-destroy proxy))))

(define (window-manager-on-globals-verify)
  "Verify that river_window_manager_v1 was bound successfully, raise error otherwise.
Parameters: None."
  (unless (valid-pointer? (manager-wl-proxy *manager*))
    (log-error "river_window_manager_v1 not available, is River running?")
    (error "river_window_manager_v1 not available")))

(define (window-manager-on-listeners-attach)
  "Attach the primary Wayland event listener to the river_window_manager_v1 proxy.
Parameters: None."
  ;; set up the window manager event listener.
  ;; the window manager must be attached before
  ;; the roundtrip that delivers initial state
  ;; roundtrip is executed after this hook finish running by connector
  (let ((wm-listener
         (make-river-window-manager-v1-listener
          wm-on-unavailable
          wm-on-finished
          wm-on-manage-start
          wm-on-render-start
          wm-on-session-locked
          wm-on-session-unlocked
          wm-on-window
          wm-on-output
          wm-on-seat)))
    (wl-proxy-add-listener (manager-wl-proxy *manager*)
						   wm-listener %null-pointer)))

;;; river_window_manager_v1: requests
(define (wm-manager-stop proxy-manager)
  "Close the manager, the manager may take time to be closed. listen
for *manager-destroy-hook* in case an action other than clearing
state needs to be executed.
Parameters: (PROXY-MANAGER)
  - PROXY-MANAGER: Pointer to river_window_manager_v1 proxy."
  (when *log-river-calls*
	(log-debug "Stopping manager ~a" proxy-manager))
  (when proxy-manager
	(river-window-manager-v1-stop proxy-manager)))

(define (wm-manager-destroy proxy-manager)
  "Destroy the manager, this means everything was cleared from
client side and the server should also clear everything.
This most likely should be used internally only, and it
will be automatically managed and called when needed.
Parameters: (PROXY-MANAGER)
  - PROXY-MANAGER: Pointer to river_window_manager_v1 proxy."
  (when *log-river-calls*
	(log-debug "Destroying manager ~a" proxy-manager))
  (when proxy-manager
	(river-window-manager-v1-destroy proxy-manager)))

(define (wm-manager-manage-finish proxy-manager)
  "Client has made all changes to window management
state it wishes to include in the current manage sequence.
Parameters: (PROXY-MANAGER)
  - PROXY-MANAGER: Pointer to river_window_manager_v1 proxy."
  (when (and *log-sequences* *log-river-calls*)
	(log-debug "Finishing manage sequence ~a" proxy-manager))
  (when proxy-manager
	(river-window-manager-v1-manage-finish proxy-manager)))

(define (wm-manager-manage-dirty proxy-manager)
  "Mark manage sequence dirty to request a new manage_start
event from server. Should be used if changes need to be made
in manage sequence outside the river flow (like statusbar modules).
Parameters: (PROXY-MANAGER)
  - PROXY-MANAGER: Pointer to river_window_manager_v1 proxy."
  (when (valid-pointer? proxy-manager)
    (when *log-river-calls*
	  (log-debug "Dirty manage sequence ~a" proxy-manager))
	(river-window-manager-v1-manage-dirty proxy-manager)))

(define (wm-manager-render-finish proxy-manager)
  "Inform compositor that client has finished current render sequence.
Parameters: (PROXY-MANAGER)
  - PROXY-MANAGER: Pointer to river_window_manager_v1 proxy."
  (when (and *log-sequences* *log-river-calls*)
	(log-debug "Finishing render sequence ~a" proxy-manager))
  (when proxy-manager
	(river-window-manager-v1-render-finish proxy-manager)))

(define (wm-manager-shell-surface-get proxy-manager proxy-surface)
  "Create a new shell surface for window manager UI
and assign river_shell_surface_v1 role to it.
Parameters: (PROXY-MANAGER PROXY-SURFACE)
  - PROXY-MANAGER: Pointer to river_window_manager_v1 proxy.
  - PROXY-SURFACE: Pointer to wl_surface proxy."
  (when *log-river-calls*
	(log-debug "Creating a new shell surface ~a" proxy-surface))
  (when proxy-manager
	(river-window-manager-v1-get-shell-surface proxy-manager proxy-surface)))

(define (wm-manager-exit proxy-manager)
  "End the current Wayland session and exit the compositor.
Parameters: (PROXY-MANAGER)
  - PROXY-MANAGER: Pointer to river_window_manager_v1 proxy."
  (when *log-river-calls*
	(log-debug "Exiting manager ~a" proxy-manager))
  (when proxy-manager
	(river-window-manager-v1-exit-session proxy-manager)))

;;; window manager event handlers
;;; these are called by the window manager's wayland listener during
;;; wl_display_dispatch, events arrive in order:
;;; seat/output/window events -> manage_start -> (client does work) -> manage_finish
(define (wm-on-unavailable data proxy-manager)
  "Invoked when window management is not available to the
client, perhaps due to another window management client already running.
Parameters: (DATA PROXY-MANAGER)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-MANAGER: Pointer to river_window_manager_v1 proxy."
  (log-error "Window management is unavailable, another window manager may be running")
  (river-disconnect!))

(define (wm-on-finished data proxy-manager)
  "Invoked when the compositor decides not to send any further
events on this object. Destroy the river_window_manager_v1 object.
Parameters: (DATA PROXY-MANAGER)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-MANAGER: Pointer to river_window_manager_v1 proxy."
  (log-info "Window manager finished event received.")
  ;; assuming only one manager will be available at a time
  (wm-manager-destroy proxy-manager)
  (river-disconnect!))

;; NOTE: I use the word advertised here as it's more descriptive
;; than created, a window maybe advertised to Gliver if it happened
;; that the window was created before Gliver (like when Gliver is restarted)
(define (wm-on-window data proxy-manager proxy-win)
  "Invoked when new window is created/advertised.
Creates a core <window> record, attaches the event listener, and queues
the window for initial setup in the upcoming manage sequence.
Parameters: (DATA PROXY-MANAGER PROXY-WIN)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-MANAGER: Pointer to river_window_manager_v1 proxy.
  - PROXY-WIN: Pointer to advertised river_window_v1 proxy."
  (when *log-river-calls*
	(log-debug "New window proxy: ~a" proxy-win))
  (gliver-hook-run! %window-created-hook data proxy-manager proxy-win))

(define (wm-on-output data proxy-manager proxy-output)
  "Invoked when a new output is created/advertised.
Parameters: (DATA PROXY-MANAGER PROXY-OUTPUT)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-MANAGER: Pointer to river_window_manager_v1 proxy.
  - PROXY-OUTPUT: Pointer to advertised river_output_v1 proxy."
  (when *log-river-calls*
	(log-debug "New output pointer created: ~a" proxy-output))
  (gliver-hook-run! %output-created-hook data proxy-manager proxy-output))

(define (wm-on-seat data proxy-manager seat-proxy)
  "Invoked when a new seat is created/advertised.
Parameters: (DATA PROXY-MANAGER SEAT-PROXY)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-MANAGER: Pointer to river_window_manager_v1 proxy.
  - SEAT-PROXY: Pointer to advertised river_seat_v1 proxy."
  (when *log-river-calls*
	(log-debug "New seat proxy: ~a" seat-proxy))
  (gliver-hook-run! %seat-created-hook data proxy-manager seat-proxy))

(define (wm-on-session-locked data proxy-manager)
  "Invoked when session is locked.
Parameters: (DATA PROXY-MANAGER)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-MANAGER: Pointer to river_window_manager_v1 proxy."
  (log-info "Session locked.")
  (gliver-hook-run! %manager-session-locked-hook data proxy-manager))

(define (wm-on-session-unlocked data proxy-manager)
  "Invoked when session is unlocked.
Parameters: (DATA PROXY-MANAGER)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-MANAGER: Pointer to river_window_manager_v1 proxy."
  (log-info "Session unlocked.")
  (gliver-hook-run! %manager-session-unlocked-hook data proxy-manager))

(define (wm-on-manage-start data proxy-manager)
  "Invoked when manage sequence is started.
run queue and send finish manage sequence request.
Parameters: (DATA PROXY-MANAGER)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-MANAGER: Pointer to river_window_manager_v1 proxy."
  (when (and *log-sequences* *log-river-calls*)
	(log-debug "Start manage sequence"))
  (set! *in-manage-sequence* #t)

  (catch #t
	(lambda ()
	  ;; dequeue and invoke tasks from manage queue until empty.
	  (define (process-queue!)
		(unless (null? *wm-manage-queue*)
		  (let ((task (car *wm-manage-queue*)))
			(set! *wm-manage-queue* (cdr *wm-manage-queue*))
			(task)
			(process-queue!))))
	  (gliver-hook-run! *manager-manage-start-hook*)
	  (process-queue!))
    (lambda (key . args)
      (log-error "Error in manage sequence: ~a ~a" key args)))

  ;; always finish the manage sequence
  (when (and *log-sequences* *log-river-calls*)
	(log-debug "Finish manage sequence"))
  (wm-manager-manage-finish proxy-manager)
  (set! *in-manage-sequence* #f))

(define (wm-on-render-start data proxy-manager)
  "Invoked when render sequence is started.
run queue and send finish render sequence request.
Parameters: (DATA PROXY-MANAGER)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-MANAGER: Pointer to river_window_manager_v1 proxy."
  (when (and *log-sequences* *log-river-calls*)
	(log-debug "Start render sequence"))
  (set! *in-render-sequence* #t)

  (catch #t
	(lambda ()
	  ;; dequeue and invoke tasks from render queue until empty.	  
	  (define (process-queue!)
		(unless (null? *wm-render-queue*)
		  (let ((task (car *wm-render-queue*)))
			(set! *wm-render-queue* (cdr *wm-render-queue*))
			(task)
			(process-queue!))))
	  (gliver-hook-run! *manager-render-start-hook*)
	  (process-queue!))
    (lambda (key . args)
      (log-error "Error in render sequence: ~a ~a" key args)))

  ;; always finish the render sequence
  (wm-manager-render-finish proxy-manager)
  (when (and *log-sequences* *log-river-calls*)
	(log-debug "Finish render sequence"))
  (set! *in-render-sequence* #f))

(define-syntax with-manage-sequence
  (syntax-rules ()
    ((_ expr1 expr2 ...)
     (if *in-manage-sequence*
         ;; if #t: execute immediately
         (begin expr1 expr2 ...)
         ;; if #f: wrap in a lambda and append to the queue
         (let ((task (lambda () expr1 expr2 ...)))
           (set! *wm-manage-queue* (append *wm-manage-queue* (list task))))))))

(define-syntax with-render-sequence
  (syntax-rules ()
    ((_ expr1 expr2 ...)
     (if *in-render-sequence*
         ;; if #t: execute immediately
         (begin expr1 expr2 ...)
         ;; if #f: wrap in a lambda and append to the queue
         (let ((task (lambda () expr1 expr2 ...)))
           (set! *wm-render-queue* (append *wm-render-queue* (list task))))))))

;; handle river initialization steps
(gliver-hook-add! *gliver-globals-bind-hook* 'window-manager-on-globals-bind)
(gliver-hook-add! *gliver-globals-unbind-hook* 'window-manager-on-globals-unbind)
(gliver-hook-add! *gliver-globals-verify-hook* 'window-manager-on-globals-verify)
(gliver-hook-add! *gliver-listeners-attach-hook* 'window-manager-on-listeners-attach)
