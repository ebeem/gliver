;;; gliver/river/connector.scm --- River compositor integration
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module integrates Gliver with Wayland display & River compositor.
;;; It handles:
;;;  - Connecting to River's Wayland display
;;;  - Advertising Wayland protocols availability
;;;  - Executing main Gliver/River event loop
;;;
;;; This module implements the Wayland protocols
;;;  - zwlr_layer_shell_v1
;;;  - wl_compositor
;;;  - wl_shm
;;;
;;; TODO: move zwlr_layer_shell_v1 away from connector
;;; it should have its own module or be part of another one
;;; that's more related to it

(define-module (gliver river connector)
  #:use-module (system foreign)
  #:use-module (gliver core-base)
  #:use-module (gliver wayland client)
  #:use-module (gliver wayland gen wayland)
  #:use-module (gliver wayland gen wlr-layer-shell-unstable-v1)
  #:declarative? #f
  #:export (
			*wl-display*
			*wl-registry*
			*wl-compositor*
			*wl-shm*
			*zwlr-layer-shell*
			*river-connected?*
			river-on-registry-global
			river-on-registry-global-remove
			river-connect!
			river-disconnect!
			river-event-loop!
			river-on-globals-bind
			river-on-globals-unbind
			river-on-globals-verify
))

;;; protocol names
(define %WL_COMPOSITOR_NAME "wl_compositor")
(define %WL_SHM_NAME "wl_shm")

(define (%valid-pointer? ptr)
  "Return #t if pointer PTR is valid and not null."
  (and ptr (not (null-pointer? ptr))))

;;; state
(define-var *wl-display* %null-pointer
			"Global Wayland display pointer, used for event loop pulling and flushing requests.")
(define-var *wl-registry* %null-pointer
			"Global Wayland registry pointer, used to discover and bind Wayland protocols.")
(define-var *wl-compositor* %null-pointer
			"Proxy for wl_compositor, used to create Wayland surfaces and regions.")
(define-var *wl-shm* %null-pointer
			"Proxy for wl_shm, used to allocate pixel buffers.")
(define-var *zwlr-layer-shell* %null-pointer
			"Proxy for zwlr_layer_shell_v1, used for UI roles and layers.")
(define-var *river-connected?* #f
			"Flag indicating whether Gliver is currently connected to the Wayland display.")


(define (river-on-registry-global data registry object-id protocol-ptr version)
  "Callback for any advertised Wayland protocol. All available protocols
will be passed to this callback upon connecting to Wayland.
  - DATA: pointer to user context, null in gliver
  - REGISTRY: proxy of the registry, a protocol must be bound on this registry
  - OBJECT-ID: the identifier used in the registry to bind to this protocol
  - PROTOCOL-PTR: string pointer to protocol name like river_window_manager_v1
  - VERSION: the maximum interface version supported by the protocol."
  (let ((protocol-name (pointer->string protocol-ptr)))
    ;; any interface should start binding with this hook
    (log-debug "binding ~a..." protocol-name)
    (gliver-hook-run! *gliver-globals-bind-hook*
                      registry protocol-name object-id version)))

(define (river-on-registry-global-remove data registry object-id)
  "Callback when any advertised Wayland protocol becomes unavailable.
  - DATA: pointer to user context, null in gliver
  - REGISTRY: proxy of the registry, a protocol must be bound on this registry
  - OBJECT-ID: the identifier used in the registry to bind to this protocol"
  ;; currently isn't used at all, it should be useful
  ;; for certain situations were advertised protocols become
  ;; unavailable, but using river will probably make this useless
  (log-debug "Registry global_remove: object-id=~a" object-id))

(define (river-connect!)
  "Connect to Wayland display and bind required protocols."
  ;; ensure $DISPLAY is set for Xwayland applications
  ;; only x11 application use this environment variable
  (unless (getenv "DISPLAY")
	(setenv "DISPLAY" ":0"))

  (log-info "Connecting to Wayland display...")

  ;; any error that occur during the initialization process
  ;; is failure in connecting to river and should terminate gliver
  (catch #t
    (lambda ()
	  ;; connect to wayland display using $WAYLAND_DISPLAY environment variable
	  ;; the wayland display is used to flush buffers and read/dispatch compositor events
      (set! *wl-display* (wl-display-connect))
      (log-info "Connected to Wayland display.")
	  (gliver-hook-run! *manager-connected-hook*)

	  ;; get the registry of the wayland display
	  ;; the registry is used to discover what protocols are available
	  ;; under the wayland session (wayland & river protocols)
      (set! *wl-registry* (gliver-wl-display-get-registry *wl-display*))

	  ;; create a listener for on-global and on-global-remove wayland protocol
	  ;; used to advertise for available protocols and when protocols become unavailable
	  (let ((listener (make-wl-listener
					   (list (procedure->pointer void river-on-registry-global (list '* '* uint32 '* uint32))
							 (procedure->pointer void river-on-registry-global-remove (list '* '* uint32))))))
		(wl-proxy-add-listener *wl-registry* listener %null-pointer))

	  ;; first roundtrip: process globals and bind protocols
	  (wl-display-roundtrip *wl-display*)

	  ;; verify all important protocols were bound
	  ;; if a hook throws an error, gliver will crash as they run with strict
	  ;; optional protocols shouldn't throw errors
	  (gliver-hook-run-strict! *gliver-globals-verify-hook*)
	  (log-debug "All critical globals were bound successfully")

	  ;; verify all important protocol listeners were attached
	  ;; if a hook throws an error, gliver will crash as they run with strict
      (gliver-hook-run-strict! *gliver-listeners-attach-hook*)
      (log-info "All critical global listeners were attached successfully")

      ;; second roundtrip: flushes bind requests and receives initial state
      ;; (outputs, seats, manage_start events delivered via wm listener)
      (wl-display-roundtrip *wl-display*)
      (log-info "Initial state synced")
      (set! *river-connected?* #t)
      (log-info "Gliver connected to Wayland display successfully."))
    (lambda (key . args)
      (log-error "Failed to connect to Wayland: ~a ~a" key args)
      (set! *river-connected?* #f))))

(define (river-disconnect!)
  "Disconnect from the Wayland display."
  (when *river-connected?*
    (log-info "Disconnecting from Wayland display...")
    (gliver-hook-run! *gliver-globals-unbind-hook*)
    ;; destroy zwlr layer shell
    (unless (null-pointer? *zwlr-layer-shell*)
      (zwlr-layer-shell-v1-destroy *zwlr-layer-shell*)
      (set! *zwlr-layer-shell* %null-pointer))
    ;; note: wl_compositor and wl_shm have no destroy request,
    ;; they are cleaned up when the display disconnects

    ;; disconnect display
    (when *wl-display*
      (wl-display-disconnect *wl-display*)
      (set! *wl-display* #f))
    (set! *river-connected?* #f)
    (log-info "Disconnected.")))

(define (river-event-loop!)
  "Run the main event loop, flush requests and read events."
  (log-info "Entering Wayland main event loop.")

  (let ((wl-fd (wl-display-get-fd *wl-display*)))
    (while *river-connected?*
      (catch #t
        (lambda ()
          ;; flush outgoing requests
          (let ((flush-ret (wl-display-flush *wl-display*)))
            (when (< flush-ret 0)
              (log-error "Wayland flush error")
              (set! *river-connected?* #f)))

          ;; prepare to read events
		  (let ((prep (wl-display-prepare-read *wl-display*)))
            (if (not (zero? prep))
                ;; events already pending, dispatch them
                (let ((ret (wl-display-dispatch-pending *wl-display*)))
                  (when (< ret 0)
                    (log-error "Wayland dispatch error")
                    (set! *river-connected?* #f)))

                ;; lock acquired, poll the fd before reading
                ;; use select with a 16ms timeout to avoid busy-waiting
                (let ((ready (select (list wl-fd) '() '() 0 16000)))
                  (if (pair? (car ready))
                      ;; data available, read and dispatch
                      (begin
                        (wl-display-read-events *wl-display*)
                        (let ((ret (wl-display-dispatch-pending *wl-display*)))
                          (when (< ret 0)
                            (log-error "Wayland dispatch error")
                            (set! *river-connected?* #f))))

                      ;; timeout, no events, cancel the read lock
                      (wl-display-cancel-read *wl-display*)))))

          ;; periodic tick hook for event loop
		  ;; could be useful for statusbar and monitoring performance
          (when *river-connected?*
            (gliver-hook-run! *manager-tick-hook*)))

        (lambda (key . args)
          (log-error "Main loop error: ~a ~a" key args)
          ;; cancel read lock if we acquired one
          (catch #t
            (lambda () (wl-display-cancel-read *wl-display*))
            (lambda _ #f))
          (set! *river-connected?* #f)))))

  (log-info "Main event loop exited."))

(define (river-on-globals-bind registry protocol-name object-id version)
  "Bind Wayland compositor, shm, and layer-shell globals from REGISTRY."
  (cond
   ;; binding *wl-compositor*
   ((string=? protocol-name %WL_COMPOSITOR_NAME)
    (log-info "Binding ~a..." protocol-name)
    (set! *wl-compositor*
          (gliver-wl-registry-bind registry object-id
                                   *wl-compositor-interface*
                                   (min version 4))))

   ;; binding *wl-shm*
   ((string=? protocol-name %WL_SHM_NAME)
    (log-info "Binding ~a..." protocol-name)
    (set! *wl-shm*
          (gliver-wl-registry-bind registry object-id
                                   *wl-shm-interface*
                                   (min version 1))))

   ;; binding *zwlr-layer-shell*
   ((string=? protocol-name ZWLR_LAYER_SHELL_V1_NAME)
    (log-info "Binding ~a..." protocol-name)
    (set! *zwlr-layer-shell*
          (gliver-wl-registry-bind registry object-id
                                   *zwlr-layer-shell-v1-interface*
                                   (min version 4))))))

(define (river-on-globals-unbind)
  "Reset connector global Wayland proxies on disconnect."
  (set! *wl-compositor* %null-pointer)
  (set! *wl-shm* %null-pointer)
  (set! *zwlr-layer-shell* %null-pointer))

(define (river-on-globals-verify)
  "Verify availability of connector globals and raise error mandatory interfaces are missing."
  (unless (and (%valid-pointer? *wl-display*)
			   (%valid-pointer? *wl-registry*)
			   (%valid-pointer? *wl-compositor*))
    (log-error "Wayland display, registry, or compositor wasn't found")
	(error "Wayland display, registry, or compositor wasn't found"))

  (unless (%valid-pointer? *zwlr-layer-shell*)
    (log-warn "zwlr_layer_shell_v1 not available, layer shell features may be disabled")))

(gliver-hook-add! *gliver-globals-bind-hook* 'river-on-globals-bind)
(gliver-hook-add! *gliver-globals-unbind-hook* 'river-on-globals-unbind)
(gliver-hook-add! *gliver-globals-verify-hook* 'river-on-globals-verify)
