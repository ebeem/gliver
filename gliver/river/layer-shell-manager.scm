;;; gliver/river/layer-shell-manager.scm --- River layer shell integration
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module manages layer shell in river.
;;; layer shell in Wayland is any desktop UI component such as
;;; Gliver's statusbar, palette, application launcher, toast.
;;; It handles:
;;;   - Binding/unbinding protocol river_layer_shell_v1 global
;;;   - Defining exclusive and non-exclusive areas of the output
;;;   - Configuring default output for layer surfaces
;;;   - Managing focus between layer shell and regular application windows
;;;
;;; This module implements the river interfaces (protocol river_layer_shell_v1):
;;;   - river_layer_shell_v1
;;;   - river_layer_shell_output_v1
;;;   - river_layer_shell_seat_v1

(define-module (gliver river layer-shell-manager)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (system foreign)
  #:use-module (gliver river connector)
  #:use-module (gliver wayland client)
  #:use-module (gliver wayland gen river-layer-shell-v1)
  #:use-module (gliver core-base)
  #:declarative? #f
  #:export (
			*layer-shell*
			*layer-shell-outputs*
			*layer-shell-seats*
			*layer-shell-output-listener*
			*layer-shell-seat-listener*
			layer-shell-on-globals-bind
			layer-shell-on-globals-unbind
			layer-shell-on-globals-verify
			layer-shell-on-listeners-attach
			layer-shell-destroy
			layer-shell-get-output
			layer-shell-get-seat
			layer-shell-output-destroy
			layer-shell-output-set-default
			layer-shell-output-on-non-exclusive-area
			layer-shell-seat-destroy
			layer-shell-seat-on-focus-exclusive
			layer-shell-seat-on-focus-non-exclusive
			layer-shell-seat-on-focus-none
))

;;; global state
(define-var *layer-shell* %null-pointer
			"Proxy for river_layer_shell_v1.")
(define-var *layer-shell-outputs* '()
			"list of layer-shell-output-proxy for tracking.")
(define-var *layer-shell-seats* '()
			"list of layer-shell-seat-proxy for tracking.")

;; NOTE: there is no listener or events for river_layer_shell_v1.
(define-var *layer-shell-output-listener* #f
			"Shared Wayland listener for river_layer_shell_output_v1 events.")
(define-var *layer-shell-seat-listener* #f
			"Shared Wayland listener for river_layer_shell_seat_v1 events.")

(define (layer-shell-on-globals-bind registry protocol-name object-id version)
  "Bind river_layer_shell_v1 global interface from REGISTRY.
Matches PROTOCOL-NAME against `RIVER_LAYER_SHELL_V1_NAME` and binds OBJECT-ID
with VERSION.
Parameters: (REGISTRY PROTOCOL-NAME OBJECT-ID VERSION)
  - REGISTRY: Pointer to wl_registry proxy.
  - PROTOCOL-NAME: Interface name string.
  - OBJECT-ID: Numeric Wayland object ID.
  - VERSION: Maximum interface version supported by compositor."
  (when (string=? protocol-name RIVER_LAYER_SHELL_V1_NAME)
    (log-info "Binding ~a..." protocol-name)
    (set! *layer-shell*
          (gliver-wl-registry-bind registry object-id
                                   *river-layer-shell-v1-interface*
                                   (min version 1)))))

(define (layer-shell-on-globals-unbind)
  "Unbind/destroy the layer shell and all associated objects."
  ;; destroy active layer shell outputs
  (for-each (lambda (proxy)
              (layer-shell-output-destroy proxy))
            *layer-shell-outputs*)
  (set! *layer-shell-outputs* '())

  ;; destroy active layer shell seats
  (for-each (lambda (proxy)
              (layer-shell-seat-destroy proxy))
            *layer-shell-seats*)
  (set! *layer-shell-seats* '())

  (when (valid-pointer? *layer-shell*)
	(layer-shell-destroy *layer-shell*)
	(set! *layer-shell* %null-pointer)))

(define (layer-shell-on-globals-verify)
  "Verify the layer shell global was bound.
Note: layer shell is optional per the protocol spec, so we only warn."
  (when (null-pointer? *layer-shell*)
    (log-warn "river_layer_shell_v1 not available, layer shell features disabled")))

(define (layer-shell-on-listeners-attach)
  "Create the shared listeners for layer shell output and seat events."
  (set! *layer-shell-output-listener*
        (make-river-layer-shell-output-v1-listener
         layer-shell-output-on-non-exclusive-area))
  (set! *layer-shell-seat-listener*
        (make-river-layer-shell-seat-v1-listener
         layer-shell-seat-on-focus-exclusive
         layer-shell-seat-on-focus-non-exclusive
         layer-shell-seat-on-focus-none)))

;;; layer shell requests
(define (layer-shell-destroy proxy-ls)
  "Destroy river_layer_shell_v1 proxy object, this means everything was
cleared from client side and the server should also clear everything.
Parameters: (PROXY-LS)
  - PROXY-LS Pointer to river_layer_shell_v1 proxy."
  (when proxy-ls
    (when *log-river-calls*
	  (log-debug "Destroying layer shell proxy: ~a" proxy-ls))
    (river-layer-shell-v1-destroy proxy-ls)
	(set! *layer-shell* %null-pointer)))

(define (layer-shell-get-output proxy-ls proxy-output)
  "Get layer shell output state (called once per river_output_v1 object).
Parameters: (PROXY-LS PROXY-OUTPUT)
  - PROXY-LS Pointer to river_layer_shell_v1 proxy.
  - PROXY-OUTPUT: Pointer to river_output_v1 proxy."
  (when (and proxy-ls proxy-output)
    (when *log-river-calls*
	  (log-debug "destroying layer shell proxy: ~a" proxy-ls))
	(let ((ls-output (river-layer-shell-v1-get-output
					  proxy-ls proxy-output)))
	  ;; add listeners to layer shell output
	  (unless (null-pointer? ls-output)
		(wl-proxy-add-listener ls-output *layer-shell-output-listener*
							   %null-pointer))
	  ;; add the new layer shell output to proxy and track it
	  (set! *layer-shell-outputs* (cons *layer-shell-outputs* ls-output))
	  ;; return the created layer shell output
	  ls-output)))

(define (layer-shell-get-seat proxy-ls proxy-seat)
  "Get layer shell seat state (called once per river_seat_v1 object).
Parameters: (PROXY-LS PROXY-SEAT)
  - PROXY-LS Pointer to river_layer_shell_v1 proxy.
  - PROXY-SEAT: Pointer to river_seat_v1 proxy."
  (when (and proxy-ls proxy-seat)
    (when *log-river-calls*
	  (log-debug "destroying layer shell proxy: ~a" proxy-ls))
	(let ((ls-seat (river-layer-shell-v1-get-seat
					proxy-ls proxy-seat)))
	  ;; add listeners to layer shell seat
	  (unless (null-pointer? ls-seat)
		(wl-proxy-add-listener ls-seat *layer-shell-seat-listener*
							   %null-pointer))
	  ;; add the new layer shell seat to proxy and track it
	  (set! *layer-shell-seats* (cons *layer-shell-seats* ls-seat))
	  ;; return the created layer shell seat
	  ls-seat)))

;;; layer shell output requests
(define (layer-shell-output-destroy proxy-ls-output)
  "Destroy river_layer_shell_output_v1 proxy object, this means everything was
cleared from client side and the server should also clear everything.
Parameters: (PROXY-LS-OUTPUT)
  - PROXY-LS-OUTPUT Pointer to river_layer_shell_output_v1 proxy."
  (when proxy-ls-output
    (when *log-river-calls*
	  (log-debug "Destroying output layer shell" proxy-ls-output))
    (river-layer-shell-output-v1-destroy proxy-ls-output)
	;; untrack the proxy
	(set! *layer-shell-outputs*
		  (delete proxy-ls-output *layer-shell-outputs*))))

(define (layer-shell-output-set-default proxy-ls-output)
  "Mark output with PROXY-LS-OUTPUT as the default for new
Must be called in a manage sequence.
Parameters: (PROXY-LS-OUTPUT)
  - PROXY-LS-OUTPUT Pointer to river_layer_shell_output_v1 proxy."
  (when proxy-ls-output
    (when *log-river-calls*
	  (log-debug "Setting default layer shell output: ~a" proxy-ls-output))
    (river-layer-shell-output-v1-set-default proxy-ls-output)))

;;; layer shell output events
(define (layer-shell-output-on-non-exclusive-area data proxy-ls-output x y width height)
  "Invoked when non exclusive area of a layer shell output changes.
Window manager should respect this area and update output available space accordingly.
The area maybe occupied by layer shell components like statusbar.
Parameters: (DATA PROXY-LS-OUTPUT X Y WIDTH HEIGHT)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-LS-OUTPUT: Pointer to river_layer_shell_output_v1 proxy.
  - X: Remaining usable area top-left corner global x coordinate.
  - Y: Remaining usable area top-left corner global y coordinate.
  - WIDTH: Remaining usable area width of the output.
  - HEIGHT: Remaining usable area height of the output."
  (when *log-river-calls*
	(log-debug "Non-exclusive area for output ~a is updated to ~ax~a+(~a+~a)"
			   proxy-ls-output width height x y))
  (gliver-hook-run! %layer-shell-output-non-exclusive-area-changed-hook
					data proxy-ls-output x y width height))

;;; layer shell seat requests
(define (layer-shell-seat-destroy proxy-ls-seat)
  "Destroy river_layer_shell_seat_v1 proxy object, this means everything was
cleared from client side and the server should also clear everything.
Parameters: (PROXY-LS-SEAT)
  - PROXY-LS-SEAT Pointer to river_layer_shell_seat_v1 proxy."
  (when proxy-ls-seat
    (when *log-river-calls*
	  (log-debug "Destroying seat layer shell" proxy-ls-seat))
    (river-layer-shell-seat-v1-destroy proxy-ls-seat)
	;; untrack the proxy
	(set! *layer-shell-seats*
		  (delete proxy-ls-seat *layer-shell-seats*))))

;;; layer shell output events
(define (layer-shell-seat-on-focus-exclusive data proxy-ls-seat)
  "Invoked when seat focus changes to exclusive areas.
Parameters: (DATA PROXY-LS-SEAT)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-LS-SEAT Pointer to river_layer_shell_seat_v1 proxy."
  (when *log-river-calls*
	(log-debug "Focusing exclusive area for seat ~a" proxy-ls-seat))
  (gliver-hook-run! %layer-shell-seat-focus-changed-hook
					data proxy-ls-seat 'exclusive))

(define (layer-shell-seat-on-focus-non-exclusive data proxy-ls-seat)
  "Invoked when seat focus changes to non-exclusive areas.
Parameters: (DATA PROXY-LS-SEAT)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-LS-SEAT Pointer to river_layer_shell_seat_v1 proxy."
  (when *log-river-calls*
	(log-debug "Focusing exclusive area for seat ~a" proxy-ls-seat))
  (gliver-hook-run! %layer-shell-seat-focus-changed-hook
					data proxy-ls-seat 'non-exclusive))

(define (layer-shell-seat-on-focus-none data proxy-ls-seat)
  "Invoked when seat focus changes to none.
Parameters: (DATA PROXY-LS-SEAT)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-LS-SEAT Pointer to river_layer_shell_seat_v1 proxy."
  (when *log-river-calls*
	(log-debug "Focusing exclusive area for seat ~a" proxy-ls-seat))
  (gliver-hook-run! %layer-shell-seat-focus-changed-hook
					data proxy-ls-seat 'none))

;; handle river initialization steps
(gliver-hook-add! *gliver-globals-bind-hook* 'layer-shell-on-globals-bind)
(gliver-hook-add! *gliver-globals-unbind-hook* 'layer-shell-on-globals-unbind)
(gliver-hook-add! *gliver-globals-verify-hook* 'layer-shell-on-globals-verify)
(gliver-hook-add! *gliver-listeners-attach-hook* 'layer-shell-on-listeners-attach)
