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
  #:use-module (gliver core types)
  #:use-module (gliver core logs)
  #:use-module (gliver core hooks)
  #:use-module (gliver river connector)
  #:use-module (gliver wayland client)
  #:use-module (gliver wayland gen river-window-management-v1)
  #:export (
			*wm-manage-queue*
			*wm-render-queue*
			*in-manage-sequence*
			RIVER_WINDOW_V1_EDGES_ALL
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
			process-queue!
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

;; extra helper variables and functions
(define RIVER_WINDOW_V1_EDGES_ALL 15)

(define (window-manager-on-globals-bind registry protocol-name object-id version)
  "Bind and register the window manager"
  (when (string=? protocol-name RIVER_WINDOW_MANAGER_V1_NAME)
	(log-info "Binding ~a..." protocol-name)
	(%manager-wl-proxy-set! *manager*
						   (gliver-wl-registry-bind registry object-id
								   *river-window-manager-v1-interface*
								   (min version 4)))
	(log-info "Manager ~a proxy is set to ~a" *manager* (manager-wl-proxy *manager*))))

(define (window-manager-on-globals-unbind)
  "Unbind/destroy the window manager"
  (when (and *manager* (manager-state? *manager*))
    (let ((proxy (manager-wl-proxy *manager*)))
      (when (and (pointer? proxy) (not (null-pointer? proxy)))
        (catch #t (lambda () (river-window-manager-v1-destroy proxy)) (lambda _ #f))))))

(define (window-manager-on-globals-verify)
  "Verify critical globals were bound "
  (when (null-pointer? (manager-wl-proxy *manager*))
    (log-error "river_window_manager_v1 not available, is River running?")
    (error "river_window_manager_v1 not available")))

(define (window-manager-on-listeners-attach)
  "Attach wayland listeners"
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

;;; TODO: implement
;;; river_window_manager_v1: requests
(define (wm-manager-stop proxy-manager)
  "Close the manager, the manager may take time to closed. listen
for *manager-destroy-hook* in case an action other than clearing
state needs to be executed."
  (log-debug "Stopping manager ~a" proxy-manager)
  (when proxy-manager
	(river-window-manager-v1-stop proxy-manager)))

(define (wm-manager-destroy proxy-manager)
  "Destroy the manager, this means everything was cleared from
client side and the server should also clear everything.
This most likely should be used internally only, and it
will be automatically managed and called when needed."
  (log-debug "Destroying manager ~a" proxy-manager)
  (when proxy-manager
	(river-window-manager-v1-destroy proxy-manager)))

(define (wm-manager-manage-finish proxy-manager)
  "Client has made all changes to window management
state it wishes to include in the current manage sequence."
  (when *log-sequences*
	(log-debug "Finishing manage sequence ~a" proxy-manager))
  (when proxy-manager
	(river-window-manager-v1-manage-finish proxy-manager)))

(define (wm-manager-manage-dirty proxy-manager)
  "Ensures a manage sequence is started and that a
manage_start event is sent by the server."
  (log-debug "Dirty manage sequence ~a" proxy-manager)
  (when proxy-manager
	(river-window-manager-v1-manage-dirty proxy-manager)))

(define (wm-manager-render-finish proxy-manager)
  "Client has made all changes to render state
server should atomically apply and display them."
  (when *log-sequences*
	(log-debug "Finishing render sequence ~a" proxy-manager))
  (when proxy-manager
	(river-window-manager-v1-render-finish proxy-manager)))

(define (wm-manager-shell-surface-get proxy-manager proxy-surface)
  "Create a new shell surface for window manager UI
and assign the river_shell_surface_v1 role to the surface."
  (log-debug "Creating a new shell surface ~a" proxy-surface)
  (when proxy-manager
	(river-window-manager-v1-get-shell-surface proxy-manager proxy-surface)))

(define (wm-manager-exit proxy-manager)
  "End the current Wayland session and exit the compositor."
  (log-debug "Exiting manager ~a" proxy-manager)
  (when proxy-manager
	(river-window-manager-v1-exit-session proxy-manager)))

;;; window manager event handlers
;;; these are called by the window manager's wayland listener during
;;; wl_display_dispatch, events arrive in order:
;;; seat/output/window events -> manage_start -> (client does work) -> manage_finish
(define (wm-on-unavailable data proxy-manager)
  "Indicates that window management is not available to the
client, perhaps due to another window management client already running."
  (log-error "Window management is unavailable, another window manager may be running")
  (river-disconnect!))

(define (wm-on-finished data proxy-manager)
  "This event indicates that the server will send no further events on this
object. The client should destroy the object."
  (log-info "Window manager finished event received.")
  ;; assuming only one manager will be available at a time
  (wm-manager-destroy proxy-manager)
  (river-disconnect!))

(define (wm-on-window data proxy-manager proxy-win)
  "Handle a new window event from the compositor.
Creates a core <window> record, attaches the event listener, and queues
the window for initial setup in the upcoming manage sequence."
  (log-debug "New window proxy: ~a" proxy-win)
  (gliver-hook-run! %window-created-hook data proxy-manager proxy-win))

(define (wm-on-output data proxy-manager output-proxy)
  "Handle a new output event from the compositor.
Creates an output and attaches the output event listener."
  (log-debug "New output pointer created: ~a" output-proxy)
  (gliver-hook-run! %output-created-hook data proxy-manager output-proxy))

(define (wm-on-seat data proxy-manager seat-proxy)
  "Handle a new seat event from the compositor."
  (log-debug "New seat proxy: ~a" seat-proxy)
  (gliver-hook-run! %seat-created-hook data proxy-manager seat-proxy))

(define (wm-on-session-locked data proxy-manager)
  (log-info "Session locked.")
  (gliver-hook-run! %manager-session-locked-hook data proxy-manager))

(define (wm-on-session-unlocked data proxy-manager)
  (log-info "Session unlocked.")
  (gliver-hook-run! %manager-session-unlocked-hook data proxy-manager))

(define (wm-on-manage-start data proxy-manager)
  "Handle manage start: execute pending actions and finish the sequence.
All window management state changes (keybinding enable/disable, focus
changes, etc.) must happen between manage_start and manage_finish."
  (when *log-sequences*
	(log-debug "Start manage sequence"))
  (set! *in-manage-sequence* #t)

  (catch #t
	(lambda ()
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
  (when *log-sequences*
	(log-debug "Finish manage sequence"))
  (wm-manager-manage-finish proxy-manager)
  (set! *in-manage-sequence* #f))

(define (wm-on-render-start data proxy-manager)
  "Handle render start: position, show, and style all windows, then finish.
The server sends window dimension events before this, so nodes can be
positioned accurately."
  (when *log-sequences*
	(log-debug "Start render sequence"))
  (set! *in-render-sequence* #t)

  (catch #t
	(lambda ()
	  (define (process-queue!)
		(unless (null? *wm-render-queue*)
		  (let ((task (car *wm-render-queue*)))
			(set! *wm-render-queue* (cdr *wm-render-queue*))
			(task)
			(process-queue!))))
	  (gliver-hook-run! *manager-render-start-hook*)
	  (process-queue!))
    (lambda (key . args)
      (log-error "Error in manage sequence: ~a ~a" key args)))

  ;; always finish the render sequence
  (wm-manager-render-finish proxy-manager)
  (when *log-sequences*
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
