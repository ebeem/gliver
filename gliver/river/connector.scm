;;; gliver/river/connector.scm --- River compositor integration
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module integrates Gliver with the River Wayland compositor.
;;; It handles:
;;;  - Connecting to River's Wayland display
;;;  - Window management via river-window-management-v1 protocol
;;;  - Key bindings via river-xkb-bindings-v1 protocol
;;;  - Input configuration via river-input-management-v1 protocol
;;;  - The main event loop

(define-module (gliver river connector)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-69)
  #:use-module (system foreign)
  #:use-module (gliver core types)
  #:use-module (gliver core config)
  #:use-module (gliver core logs)
  #:use-module (gliver core hooks)
  #:use-module (gliver wayland client)
  #:use-module (gliver wayland gen wayland)
  #:use-module (gliver wayland gen river-input-management-v1)
  #:use-module (gliver wayland gen river-layer-shell-v1)
  #:use-module (gliver wayland gen wlr-layer-shell-unstable-v1)
  #:export (
			WL_COMPOSITOR_NAME
			WL_SHM_NAME
			*wl-display*
			*wl-registry*
			*connected*
			*input-manager*
			*layer-shell*
			*wl-compositor*
			*wl-shm*
			*zwlr-layer-shell*
			*pending-key-action*
			river-connected?
			river-connect!
			river-disconnect!
			river-dispatch-once!
			read-proc-mem-kb
			river-event-loop!
))

;;; protocol names
(define WL_COMPOSITOR_NAME "wl_compositor")
(define WL_SHM_NAME "wl_shm")

;;; state
(define-var *wl-display* #f
			"global wayland display.")
(define-var *wl-registry* #f
			"global wayland registry.")
(define-var *connected* #f
			"Whether gliver is connected to river.")

;;; window management protocol state
(define-var *input-manager* %null-pointer
			"river_input_manager_v1 wayland proxy")
(define-var *layer-shell* %null-pointer
			"river_layer_shell_v1 wayland proxy")
(define-var *wl-compositor* %null-pointer
			"wl_compositor wayland proxy")
(define-var *wl-shm* %null-pointer
			"wl_shm wayland wayland proxy")
(define-var *zwlr-layer-shell* %null-pointer
			"zwlr_layer_shell_v1 wayland proxy")

(define (river-connected?)
  *connected*)

;;; connection
(define (river-connect!)
  "Connect to River's Wayland display and bind required protocols."
  (log-info "Connecting to Wayland display...")
  (catch #t
    (lambda ()
      (set! *wl-display* (wl-display-connect))
      (log-info "Connected to Wayland display.")
	  (gliver-hook-run! *manager-connected-hook*)

	  ;; get the registry and set up the global listener
      (set! *wl-registry* (gliver-wl-display-get-registry *wl-display*))

	  (let ((listener
             (make-wl-listener
              (list
               (procedure->pointer
                void
                (lambda (data registry object-id protocol-ptr version)
                  (let ((protocol-name (pointer->string protocol-ptr)))
					;; any interface should start binding with this hook
                    ;; (log-info "binding ~a..." protocol-name)
					(gliver-hook-run! *gliver-globals-bind-hook*
									  registry protocol-name object-id version)))
                (list '* '* uint32 '* uint32))
               ;; global_remove(data, registry, object-id)
               (procedure->pointer
                void
                (lambda (data registry object-id)
                  (log-debug "Registry global_remove: object-id=~a" object-id))
                (list '* '* uint32))))))
        (wl-proxy-add-listener *wl-registry* listener %null-pointer))

	  ;; first roundtrip: process globals, bind protocols
	  (wl-display-roundtrip *wl-display*)

	  ;; verify all important interfaces were bound
	  (gliver-hook-run-strict! *gliver-globals-verify-hook*)
	  (log-debug "All critical globals were bound")

      (log-info "Core protocols bound.")
      (gliver-hook-run-strict! *gliver-listeners-attach-hook*)

      ;; second roundtrip: flushes bind requests and receives initial state
      ;; (outputs, seats, manage_start events delivered via wm listener)
      (wl-display-roundtrip *wl-display*)
      (log-info "Initial state synced")
      (set! *connected* #t)
      (log-info "River integration initialized."))
    (lambda (key . args)
      (log-error "Failed to connect to Wayland: ~a ~a" key args)
      (set! *connected* #f))))

(define (river-disconnect!)
  "Disconnect from the Wayland display."
  (when *connected*
    (log-info "Disconnecting from Wayland display...")
    (gliver-hook-run! *gliver-globals-unbind-hook*)
    ;; destroy input manager
    (unless (null-pointer? *input-manager*)
      (river-input-manager-v1-destroy *input-manager*)
      (set! *input-manager* %null-pointer))
    ;; destroy layer shell
    (unless (null-pointer? *layer-shell*)
      (river-layer-shell-v1-destroy *layer-shell*)
      (set! *layer-shell* %null-pointer))
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
    (set! *connected* #f)
    (log-info "Disconnected.")))

;;; event loop
(define (river-dispatch-once!)
  "Dispatch pending Wayland events once."
  (when *connected*
    (catch #t
      (lambda ()
        (wl-display-flush *wl-display*)
        ;; Non-blocking dispatch
        (let ((ret (wl-display-dispatch-pending *wl-display*)))
          (when (< ret 0)
            (log-error "Wayland dispatch error")
            (set! *connected* #f))))
      (lambda (key . args)
        (log-error "Event dispatch error: ~a ~a" key args)))))

(define (read-proc-mem-kb)
  "Read current RSS from /proc/self/status (Linux only)."
  (catch #t
    (lambda ()
      (call-with-input-file "/proc/self/status"
        (lambda (port)
          (let loop ()
            (let ((line (read-line port)))
              (if (eof-object? line)
                  0
                  (if (string-prefix? "VmRSS:" line)
                      (string->number
                       (string-trim-both
                        (string-drop-right
                         (string-trim (substring line 6))
                         2)))
                      (loop))))))))
    (lambda _ 0)))

(define (river-event-loop!)
  "Run the main event loop.
This integrates Wayland event dispatching with IPC and REPL polling."
  (log-info "Entering main event loop.")
  (var-set! *running?* #t)

  (let ((wl-fd (wl-display-get-fd *wl-display*)))
    (while (and *running?* *connected*)
      (catch #t
        (lambda ()
          ;; flush outgoing requests
          (let ((flush-ret (wl-display-flush *wl-display*)))
            (when (< flush-ret 0)
              (log-error "Wayland flush error")
              (set! *connected* #f)))

          (when *connected*
            ;; prepare to read events
            (let ((prep (wl-display-prepare-read *wl-display*)))
              (if (not (zero? prep))
                  ;; events already pending, dispatch them
                  (let ((ret (wl-display-dispatch-pending *wl-display*)))
                    (when (< ret 0)
                      (log-error "Wayland dispatch error")
                      (set! *connected* #f)))
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
                              (set! *connected* #f))))
                        ;; timeout, no events, cancel the read lock
                        (wl-display-cancel-read *wl-display*)))))))

        (lambda (key . args)
          (log-error "Main loop error: ~a ~a" key args)
          ;; cancel read lock if we acquired one
          (catch #t
            (lambda () (wl-display-cancel-read *wl-display*))
            (lambda _ #f))
          (set! *connected* #f)))))

  (log-info "Main event loop exited."))

(define (river-on-globals-bind registry protocol-name object-id version)
  "Bind Wayland compositor, shm, layer-shell, and output globals."
  (cond
   ((string=? protocol-name WL_COMPOSITOR_NAME)
    (log-info "Binding ~a..." protocol-name)
    (set! *wl-compositor*
          (gliver-wl-registry-bind registry object-id
                                   *wl-compositor-interface*
                                   (min version 4))))
   ((string=? protocol-name WL_SHM_NAME)
    (log-info "Binding ~a..." protocol-name)
    (set! *wl-shm*
          (gliver-wl-registry-bind registry object-id
                                   *wl-shm-interface*
                                   1)))
   ((string=? protocol-name ZWLR_LAYER_SHELL_V1_NAME)
    (log-info "Binding ~a..." protocol-name)
    (set! *zwlr-layer-shell*
          (gliver-wl-registry-bind registry object-id
                                   *zwlr-layer-shell-v1-interface*
                                   (min version 4))))))

(define (river-on-globals-unbind)
  "Unbind globals and clean up wallpaper resources."
  (set! *wl-compositor* %null-pointer)
  (set! *wl-shm* %null-pointer)
  (set! *zwlr-layer-shell* %null-pointer))

(define (river-on-globals-verify)
  "Verify wallpaper globals."
  (when (null-pointer? *zwlr-layer-shell*)
    (log-warn "zwlr_layer_shell_v1 not available, background wallpaper features may be disabled")))

(gliver-hook-add! *gliver-globals-bind-hook* 'river-on-globals-bind)
(gliver-hook-add! *gliver-globals-unbind-hook* 'river-on-globals-unbind)
(gliver-hook-add! *gliver-globals-verify-hook* 'river-on-globals-verify)
