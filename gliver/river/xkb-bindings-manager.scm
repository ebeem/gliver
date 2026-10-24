;;; gliver/river/xkb-bindings-manager.scm --- River keybindings integration
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module manages keybindings in river.
;;; It handles:
;;;  - Binding river_xkb_bindings_v1 global interface and seat bindings
;;;  - Synchronizing and activating keybindings and submaps
;;;  - Managing and switching between submaps
;;;  - Executing keybindings actions
;;;
;;; This module implements the river interfaces (protocol river_xkb_bindings_v1):
;;;  - river_xkb_bindings_v1
;;;  - river_xkb_bindings_seat_v1
;;;  - river_xkb_binding_v1
;;;
;;; NOTE: river_xkb_bindings_v1 is the keybindings manager, where
;;; river_xkb_binding_v1 is the keybinding instance (like C-g)
;;; it's important to understand the difference otherwise this file
;;; will seem very confusing

(define-module (gliver river xkb-bindings-manager)
  #:use-module (ice-9 format)
  #:use-module (system foreign)
  #:use-module (srfi srfi-2)
  #:use-module (gliver wayland client)
  #:use-module (gliver wayland gen river-window-management-v1)
  #:use-module (gliver wayland gen river-xkb-bindings-v1)
  #:use-module (gliver core-base)
  #:declarative? #f
  #:export (
			*xkb-bindings-manager-bindings*
			*xkb-bindings-manager-binding-list*
			*xkb-bindings-manager-bindings-seat*
			*xkb-bindings-manager-binding-listener*
			*xkb-bindings-manager-bindings-seat-listener*
			xkb-bindings-manager-on-globals-bind
			xkb-bindings-manager-on-globals-unbind
			xkb-bindings-manager-on-globals-verify
			xkb-bindings-manager-on-listeners-attach
			xkb-bindings-manager-bindings-destroy
			xkb-bindings-manager-bindings-create-binding
			xkb-bindings-manager-bindings-create-seat
			xkb-bindings-manager-binding-destroy
			xkb-bindings-manager-binding-set-layout-override
			xkb-bindings-manager-binding-enable
			xkb-bindings-manager-binding-disable
			xkb-bindings-manager-on-binding-pressed
			xkb-bindings-manager-on-binding-released
			xkb-bindings-manager-on-binding-stop-repeat
			xkb-bindings-manager-bindings-seat-destroy
			xkb-bindings-manager-bindings-seat-ensure-next-key-eaten
			xkb-bindings-manager-bindings-seat-cancel-ensure-next-key-eaten
			xkb-bindings-manager-on-bindings-seat-ate
			xkb-bindings-manager-bindings-destroy-all!
))

;;; global state
(define-var *xkb-bindings-manager-bindings* %null-pointer
			"Proxy for river_xkb_bindings_v1 bound from Wayland registry.")
(define-var *xkb-bindings-manager-binding-list* '()
			"List of all created active river_xkb_binding_v1 proxies.")
;; NOTE: gliver supports only 1 seat at the moment
;; river itself allows maintain multiple seats
(define-var *xkb-bindings-manager-bindings-seat* %null-pointer
			"Proxy for river_xkb_bindings_seat_v1 assigned to the current seat.")

;; NOTE: there is no listener or events for river_xkb_bindings_v1
(define-var *xkb-bindings-manager-binding-listener* #f
			"Wayland listener for river_xkb_binding_v1 press/release events.")
(define-var *xkb-bindings-manager-bindings-seat-listener* #f
			"Wayland listener for river_xkb_bindings_seat_v1 ate and modifier key events.")

(define (xkb-bindings-manager-on-globals-bind registry protocol-name object-id version)
  "Bind river_xkb_bindings_v1 global interface from REGISTRY."
  (when (string=? protocol-name RIVER_XKB_BINDINGS_V1_NAME)
	(log-info "Binding ~a..." protocol-name)
    (set! *xkb-bindings-manager-bindings*
          (gliver-wl-registry-bind registry object-id
                                   *river-xkb-bindings-v1-interface*
                                   (min version 2)))))

(define (xkb-bindings-manager-on-globals-unbind)
  "Destroy all active keybinding proxies and unbind the XKB bindings manager."
  ;; destroy active keybindings, will destroy
  ;; each created binding via river-xkb-binding-v1-destroy
  (xkb-bindings-manager-bindings-destroy-all!)

  ;; destroy xkb bindings seat
  (when (valid-pointer? *xkb-bindings-manager-bindings-seat*)
    (river-xkb-bindings-seat-v1-destroy *xkb-bindings-manager-bindings-seat*)
    (set! *xkb-bindings-manager-bindings-seat* %null-pointer))

  ;; destroy xkb bindings
  (when (valid-pointer? *xkb-bindings-manager-bindings*)
    (river-xkb-bindings-v1-destroy *xkb-bindings-manager-bindings*)
    (set! *xkb-bindings-manager-bindings* %null-pointer)))

(define (xkb-bindings-manager-on-globals-verify)
  "Verify that *xkb-bindings-manager-bindings* was bound successfully, raise an error if not."
  (unless (valid-pointer? *xkb-bindings-manager-bindings*)
    (log-error "river_xkb_bindings_v1 not available")
    (error "river_xkb_bindings_v1 not available")))

(define (xkb-bindings-manager-on-listeners-attach)
  "Attach Wayland listeners for river_xkb_binding_v1 and river_xkb_bindings_seat_v1."
  (set! *xkb-bindings-manager-binding-listener*
        (make-river-xkb-binding-v1-listener
		 xkb-bindings-manager-on-binding-pressed
		 xkb-bindings-manager-on-binding-released
		 xkb-bindings-manager-on-binding-stop-repeat))

  (set! *xkb-bindings-manager-bindings-seat-listener*
        (make-river-xkb-bindings-seat-v1-listener
		 xkb-bindings-manager-on-bindings-seat-ate)))

;;; river_xkb_bindings_v1 requests
(define (xkb-bindings-manager-bindings-destroy)
  "Destroy the river_xkb_bindings_v1 object PROXY.
PROXY: river_xkb_bindings_v1 object PROXY."
  (when (valid-pointer? *xkb-bindings-manager-bindings*)
	(river-xkb-bindings-v1-destroy *xkb-bindings-manager-bindings*)))

(define (xkb-bindings-manager-bindings-create-binding proxy-seat keysym modifiers)
  "Create a new binding on PROXY-SEAT.
NOTE: river-xkb-binding created must be enabled
PROXY: river_xkb_bindings_v1 object PROXY.
PROXY-SEAT: the proxy of the targeted seat.
KEYSYM: uint xkb keysym value.
MODIFIERS: uint modifier bitmask.
Returns proxy of created river_xkb_binding_v1."
  (when (valid-pointer? *xkb-bindings-manager-bindings*)
	(when *log-river-calls*
	  (log-debug "Creating binding with keysym ~a and modifier mask ~a in seat ~a" keysym modifiers proxy-seat))
	(let ((proxy (river-xkb-bindings-v1-get-xkb-binding *xkb-bindings-manager-bindings*
														proxy-seat
														keysym
														modifiers)))
	  ;; track the new proxy to the binding list so we can cleanup later
	  (set! *xkb-bindings-manager-binding-list*
			(cons proxy *xkb-bindings-manager-binding-list*))
	  ;; attach the shared event listener for pressed/released events
	  (wl-proxy-add-listener proxy *xkb-bindings-manager-binding-listener*
							 %null-pointer)
	  ;; return the proxy so it can be used by callers
	  proxy)))

(define (xkb-bindings-manager-bindings-create-seat seat-proxy)
  "Create a new xkb bindings seat on PROXY-SEAT.
PROXY: river_xkb_bindings_v1 object PROXY.
PROXY-SEAT: the proxy of the targeted seat.
KEYSYM: uint xkb keysym value.
MODIFIERS: uint modifier bitmask.
Returns proxy of created river_xkb_bindings_seat_v1."
  ;; create if not available and return the *xkb-bindings-manager-bindings-seat*
  (unless (valid-pointer? *xkb-bindings-manager-bindings-seat*)
	(when *log-river-calls*
	  (log-debug "Creating binding seat for seat ~a" seat-proxy))
	(set! *xkb-bindings-manager-bindings-seat*
		  (river-xkb-bindings-v1-get-seat *xkb-bindings-manager-bindings* seat-proxy))
	;; attach shared event listener for eaten bindings
	(wl-proxy-add-listener
	 *xkb-bindings-manager-bindings-seat*
	 *xkb-bindings-manager-bindings-seat-listener*
	 %null-pointer))
  *xkb-bindings-manager-bindings-seat*)

;;; river_xkb_bindings_v1 events
;;; NONE

;;; river_xkb_binding_v1 requests
(define (xkb-bindings-manager-binding-destroy proxy)
  "Destroy the river_xkb_binding_v1 object PROXY.
PROXY: river_xkb_binding_v1 object PROXY."
  (when (valid-pointer? proxy)
	(when *log-river-calls*
	  (log-debug "Destroying binding ~a" proxy))
	;; untrack the proxy
	(set! *xkb-bindings-manager-binding-list*
		  (delete proxy *xkb-bindings-manager-binding-list*))
	(river-xkb-binding-v1-destroy proxy)))

(define (xkb-bindings-manager-binding-set-layout-override proxy layout)
  "Specify the xkb layout that should be used for this binding.
This allows keybindings to work even after switching layout/language.
Must be called in a ~manage_sequence~.
PROXY: river_xkb_binding_v1 object PROXY.
LAYOUT: index of xkb layout."
  (when (valid-pointer? proxy)
	(when *log-river-calls*
	  (log-debug "Setting layout override of binding ~a to index ~a" proxy layout))
	(river-xkb-binding-v1-set-layout-override proxy layout)))

(define (xkb-bindings-manager-binding-enable proxy)
  "Enable the binding (it's disabled by default after creation).
Must be called in a ~manage_sequence~.
PROXY: river_xkb_binding_v1 object PROXY."
  (when (valid-pointer? proxy)
	(when *log-river-calls*
	  (log-debug "Enable xkb binding ~a" proxy))
	(river-xkb-binding-v1-enable proxy)))

(define (xkb-bindings-manager-binding-disable proxy)
  "Disable the binding with the provided PROXY.
Must be called in a ~manage_sequence~.
PROXY: river_xkb_binding_v1 object PROXY."
  (when (valid-pointer? proxy)
	(when *log-river-calls*
	  (log-debug "Disable xkb binding ~a" proxy))
	(river-xkb-binding-v1-disable proxy)))

;;; river_xkb_binding_v1 events
(define (xkb-bindings-manager-on-binding-pressed data proxy-binding)
  "Handle key press event for PROXY-BINDING with user DATA."
  (when *log-river-calls*
	(log-debug "Key pressed on xkb binding ~a" proxy-binding))
  (gliver-hook-run! %keybinding-pressed-hook proxy-binding))

(define (xkb-bindings-manager-on-binding-released data proxy-binding)
  "Handle key binding release event for PROXY-BINDING."
  (when *log-river-calls*
	(log-debug "Key released on xkb binding ~a" proxy-binding))
  (gliver-hook-run! %keybinding-released-hook proxy-binding))

(define (xkb-bindings-manager-on-binding-stop-repeat data proxy-binding)
  "Handle key binding stop repeat event for PROXY-BINDING."
  (when *log-river-calls*
	(log-debug "Repeat stopped on xkb binding ~a" proxy-binding))
  (gliver-hook-run! %keybinding-repeat-stopped-hook proxy-binding))

;;; river_xkb_bindings_seat_v1 requests
(define (xkb-bindings-manager-bindings-seat-destroy proxy)
  "Destroy the river_xkb_bindings_seat_v1 object PROXY.
PROXY: river_xkb_bindings_seat_v1 object PROXY."
  (when (valid-pointer? proxy)
	(when *log-river-calls*
	  (log-debug "Destroy bindings seat ~a" proxy))
	(river-xkb-bindings-seat-v1-destroy proxy)))

(define (xkb-bindings-manager-bindings-seat-ensure-next-key-eaten proxy)
  "Ensure next non-modifier key press will be eaten if no binding was found for seat PROXY.
This will trigger the event xkb-bindings-manager-on-bindings-seat-ate if key is eaten.
Useful for submaps and awaiting for valid binding rather than sending it to applications.
This will be automatically cancelled after a key press (eaten or not).
Must be called in a ~manage_sequence~.
PROXY: river_xkb_bindings_seat_v1 object PROXY."
  (when (valid-pointer? proxy)
	(when *log-river-calls*
	  (log-debug "Ensure next key eaten on seat ~a" proxy))
	(river-xkb-bindings-seat-v1-ensure-next-key-eaten proxy)))

(define (xkb-bindings-manager-bindings-seat-cancel-ensure-next-key-eaten proxy)
  "Cancels the ensure next non-modifier key eaten request for seat PROXY.
Must be called in a ~manage_sequence~.
PROXY: river_xkb_bindings_seat_v1 object PROXY."
  (when (valid-pointer? proxy)
	(when *log-river-calls*
	  (log-debug "Ate binding on seat ~a" proxy))
	(river-xkb-bindings-seat-v1-cancel-ensure-next-key-eaten proxy)))

;;; river_xkb_bindings_seat_v1 events
(define (xkb-bindings-manager-on-bindings-seat-ate data proxy-seat)
  "Handle an eaten key press event on PROXY-SEAT with user DATA."
  (when (valid-pointer? proxy-seat)
	(when *log-river-calls*
	  (log-debug "Ate binding on seat ~a" proxy-seat))
	(gliver-hook-run! %keybinding-eaten-hook proxy-seat)))

(define (xkb-bindings-manager-bindings-destroy-all!)
  "Destroy all existing bindings."
  (for-each (lambda (proxy)
			  (river-xkb-binding-v1-destroy proxy))
            *xkb-bindings-manager-binding-list*)
  (set! *xkb-bindings-manager-binding-list* '()))

;; handle river initialization steps
(gliver-hook-add! *gliver-globals-bind-hook* 'xkb-bindings-manager-on-globals-bind)
(gliver-hook-add! *gliver-globals-unbind-hook* 'xkb-bindings-manager-on-globals-unbind)
(gliver-hook-add! *gliver-globals-verify-hook* 'xkb-bindings-manager-on-globals-verify)
(gliver-hook-add! *gliver-listeners-attach-hook* 'xkb-bindings-manager-on-listeners-attach)
