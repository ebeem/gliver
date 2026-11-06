;;; gliver/river/wlr-layer-shell.scm --- wlr-layer-shell protocol integration
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module manages desktop UI components using wlr-layer-shell.
;;; Components include statusbars, wallpapers, application launchers, toasts, and palettes.
;;; It handles:
;;;   - Binding/unbinding protocol zwlr_layer_shell_v1 global
;;;   - Creating and destroying zwlr_layer_surface_v1 surfaces
;;;   - Configuring layer surface size, anchor, margin, and exclusive zone
;;;   - Configuring keyboard interactivity modes
;;;
;;; This module implements the river interfaces (protocol wlr_layer_shell_unstable_v1):
;;;   - zwlr_layer_shell_v1
;;;   - zwlr_layer_surface_v1

(define-module (gliver river wlr-layer-shell)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (system foreign)
  #:use-module (gliver river connector)
  #:use-module (gliver wayland client)
  #:use-module (gliver wayland gen wlr-layer-shell-unstable-v1)
  #:use-module (gliver core-base)
  #:declarative? #f
  #:export (
			*wlr-layer-shell*
			*wlr-layer-surfaces*
			*wlr-layer-surface-listener*
			*wlr-layer-shell-anchor-top*
			*wlr-layer-shell-anchor-bottom*
			*wlr-layer-shell-anchor-left*
			*wlr-layer-shell-anchor-right*
			*wlr-layer-shell-anchor-all*
			*wlr-layer-shell-layer-background*
			*wlr-layer-shell-layer-bottom*
			*wlr-layer-shell-layer-top*
			*wlr-layer-shell-layer-overlay*
			*wlr-layer-shell-interactivity-none*
			*wlr-layer-shell-interactivity-exclusive*
			*wlr-layer-shell-interactivity-on-demand*
			wlr-layer-shell-on-globals-bind
			wlr-layer-shell-on-globals-unbind
			wlr-layer-shell-on-globals-verify
			wlr-layer-shell-on-listeners-attach
			wlr-layer-shell-destroy
			wlr-layer-shell-get-layer-surface
			wlr-layer-surface-destroy
			wlr-layer-surface-set-size
			wlr-layer-surface-set-anchor
			wlr-layer-surface-set-exclusive-zone
			wlr-layer-surface-set-margin
			wlr-layer-surface-set-keyboard-interactivity
			wlr-layer-surface-get-popup
			wlr-layer-surface-ack-configure
			wlr-layer-surface-set-layer
			wlr-layer-surface-set-exclusive-edge
			wlr-layer-surface-on-configure
			wlr-layer-surface-on-closed
))

;;; global state
(define-var *wlr-layer-shell* %null-pointer
			"Proxy for zwlr_layer_shell_v1.")
(define-var *wlr-layer-surfaces* '()
			"list of zwlr_layer_surface_v1 proxies for tracking.")

(define-var *wlr-layer-surface-listener* #f
			"Shared Wayland listener for zwlr_layer_surface_v1 configure and closed events.")

;; exposing friendly anchors and layer variables
(define *wlr-layer-shell-anchor-top* ZWLR_LAYER_SURFACE_V1_ANCHOR_TOP)
(define *wlr-layer-shell-anchor-bottom* ZWLR_LAYER_SURFACE_V1_ANCHOR_BOTTOM)
(define *wlr-layer-shell-anchor-left* ZWLR_LAYER_SURFACE_V1_ANCHOR_LEFT)
(define *wlr-layer-shell-anchor-right* ZWLR_LAYER_SURFACE_V1_ANCHOR_RIGHT)
(define *wlr-layer-shell-anchor-all* (+ *wlr-layer-shell-anchor-top*
										*wlr-layer-shell-anchor-bottom*
										*wlr-layer-shell-anchor-left*
										*wlr-layer-shell-anchor-right*))

(define *wlr-layer-shell-layer-background* ZWLR_LAYER_SHELL_V1_LAYER_BACKGROUND)
(define *wlr-layer-shell-layer-bottom* ZWLR_LAYER_SHELL_V1_LAYER_BOTTOM)
(define *wlr-layer-shell-layer-top* ZWLR_LAYER_SHELL_V1_LAYER_TOP)
(define *wlr-layer-shell-layer-overlay* ZWLR_LAYER_SHELL_V1_LAYER_OVERLAY)

(define *wlr-layer-shell-interactivity-none* ZWLR_LAYER_SURFACE_V1_KEYBOARD_INTERACTIVITY_NONE)
(define *wlr-layer-shell-interactivity-exclusive* ZWLR_LAYER_SURFACE_V1_KEYBOARD_INTERACTIVITY_EXCLUSIVE)
(define *wlr-layer-shell-interactivity-on-demand* ZWLR_LAYER_SURFACE_V1_KEYBOARD_INTERACTIVITY_ON_DEMAND)

(define (wlr-layer-shell-on-globals-bind registry protocol-name object-id version)
  "Bind zwlr_layer_shell_v1 global interface from REGISTRY.
Matches PROTOCOL-NAME against `ZWLR_LAYER_SHELL_V1_NAME` and binds OBJECT-ID
with VERSION.
Parameters: (REGISTRY PROTOCOL-NAME OBJECT-ID VERSION)
  - REGISTRY: Pointer to wl_registry proxy.
  - PROTOCOL-NAME: Interface name string.
  - OBJECT-ID: Numeric Wayland object ID.
  - VERSION: Maximum interface version supported by compositor."
  (when (string=? protocol-name ZWLR_LAYER_SHELL_V1_NAME)
    (log-info "Binding ~a..." protocol-name)
    (set! *wlr-layer-shell*
          (gliver-wl-registry-bind registry object-id
                                   *zwlr-layer-shell-v1-interface*
                                   (min version 4)))))

(define (wlr-layer-shell-on-globals-unbind)
  "Unbind/destroy the wlr layer shell and all associated layer surfaces.
Parameters: None."
  ;; destroy active layer surfaces
  (for-each (lambda (proxy)
              (wlr-layer-surface-destroy proxy))
            *wlr-layer-surfaces*)

  (when (valid-pointer? *wlr-layer-shell*)
	(wlr-layer-shell-destroy *wlr-layer-shell*)))

(define (wlr-layer-shell-on-globals-verify)
  "Verify the wlr layer shell global was bound.
Note: wlr layer shell is optional per the protocol spec, so we only warn.
Parameters: None."
  (unless (valid-pointer? *wlr-layer-shell*)
    (log-warn "zwlr_layer_shell_v1 not available, layer shell features disabled")))

(define (wlr-layer-shell-on-listeners-attach)
  "Create the shared listener for zwlr_layer_surface_v1 configure and closed events.
Parameters: None."
  (set! *wlr-layer-surface-listener*
        (make-zwlr-layer-surface-v1-listener
         wlr-layer-surface-on-configure
         wlr-layer-surface-on-closed)))

;;; zwlr_layer_shell_v1 requests
(define (wlr-layer-shell-destroy proxy-shell)
  "Destroy zwlr_layer_shell_v1 proxy object, this means everything was
cleared from client side and the server should also clear everything.
Parameters: (PROXY-SHELL)
  - PROXY-SHELL: Pointer to zwlr_layer_shell_v1 proxy."
  (when (valid-pointer? proxy-shell)
    (when *log-river-calls*
	  (log-debug "Destroying wlr layer shell proxy: ~a" proxy-shell))
    (zwlr-layer-shell-v1-destroy proxy-shell)
	(set! *wlr-layer-shell* %null-pointer)))

(define (wlr-layer-shell-get-layer-surface proxy-shell proxy-surface proxy-output layer namespace)
  "Create a layer_surface from a surface.
Parameters: (PROXY-SHELL PROXY-SURFACE PROXY-OUTPUT LAYER NAMESPACE)
  - PROXY-SHELL: Pointer to zwlr_layer_shell_v1 proxy.
  - PROXY-SURFACE: Pointer to wl_surface proxy.
  - PROXY-OUTPUT: Pointer to wl_output proxy, or %null-pointer / #f for compositor default.
  - LAYER: Layer enum (e.g. *wlr-layer-shell-layer-background*).
  - NAMESPACE: String namespace identifying surface purpose.
Returns: zwlr_layer_surface_v1 proxy pointer."
  (when (and (valid-pointer? proxy-shell) (valid-pointer? proxy-surface))
    (let* ((out (if (valid-pointer? proxy-output) proxy-output %null-pointer))
		   (layer-surface (zwlr-layer-shell-v1-get-layer-surface
                           proxy-shell proxy-surface out layer namespace)))
      (when *log-river-calls*
        (log-debug "Creating layer surface on ~a: surface=~a layer=~a namespace=~a"
                   proxy-shell proxy-surface layer namespace))
      (when (valid-pointer? layer-surface)
        (wl-proxy-add-listener layer-surface *wlr-layer-surface-listener* %null-pointer)
        (set! *wlr-layer-surfaces* (cons layer-surface *wlr-layer-surfaces*)))
      layer-surface)))

;;; zwlr_layer_surface_v1 requests
(define (wlr-layer-surface-destroy proxy-layer-surface)
  "Destroy zwlr_layer_surface_v1 proxy object, this means everything was
cleared from client side and the server should also clear everything.
Parameters: (PROXY-LAYER-SURFACE)
  - PROXY-LAYER-SURFACE: Pointer to zwlr_layer_surface_v1 proxy."
  (when (and (valid-pointer? proxy-layer-surface)
             (memq proxy-layer-surface *wlr-layer-surfaces*))
    (when *log-river-calls*
	  (log-debug "Destroying layer surface proxy: ~a" proxy-layer-surface))
    (zwlr-layer-surface-v1-destroy proxy-layer-surface)
	;; untrack the proxy
	(set! *wlr-layer-surfaces*
		  (delq proxy-layer-surface *wlr-layer-surfaces*))))

(define (wlr-layer-surface-set-size proxy-layer-surface width height)
  "Set size of the layer surface in pixels. Setting 0 for either dimension
indicates that the compositor should allocate size based on anchors/exclusive zone.
Parameters: (PROXY-LAYER-SURFACE WIDTH HEIGHT)
  - PROXY-LAYER-SURFACE: Pointer to zwlr_layer_surface_v1 proxy.
  - WIDTH: Width in pixels.
  - HEIGHT: Height in pixels."
  (when (valid-pointer? proxy-layer-surface)
    (when *log-river-calls*
	  (log-debug "Setting layer surface ~a size to ~ax~a"
				 proxy-layer-surface width height))
    (zwlr-layer-surface-v1-set-size proxy-layer-surface width height)))

(define (wlr-layer-surface-set-anchor proxy-layer-surface anchor)
  "Configure anchor point of layer surface.
Parameters: (PROXY-LAYER-SURFACE ANCHOR)
  - PROXY-LAYER-SURFACE: Pointer to zwlr_layer_surface_v1 proxy.
  - ANCHOR: Flag using enums *wlr-layer-shell-anchor-*."
  (when (valid-pointer? proxy-layer-surface)
    (when *log-river-calls*
	  (log-debug "Setting layer surface ~a anchor to ~a"
				 proxy-layer-surface anchor))
    (zwlr-layer-surface-v1-set-anchor proxy-layer-surface anchor)))

(define (wlr-layer-surface-set-exclusive-zone proxy-layer-surface zone)
  "Configure exclusive zone of layer surface in pixels.
> 0 value requests compositor to reserve space from the edge.
0 no exclusive space, but prevents occlusion.
-1 no exclusive space and allows occlusion.
Parameters: (PROXY-LAYER-SURFACE ZONE)
  - PROXY-LAYER-SURFACE: Pointer to zwlr_layer_surface_v1 proxy.
  - ZONE: Exclusive zone in pixels (integer)."
  (when (valid-pointer? proxy-layer-surface)
    (when *log-river-calls*
	  (log-debug "Setting layer surface ~a exclusive zone to ~a"
				 proxy-layer-surface zone))
    (zwlr-layer-surface-v1-set-exclusive-zone proxy-layer-surface zone)))

(define (wlr-layer-surface-set-margin proxy-layer-surface top right bottom left)
  "Set margins from anchor points.
Parameters: (PROXY-LAYER-SURFACE TOP RIGHT BOTTOM LEFT)
  - PROXY-LAYER-SURFACE: Pointer to zwlr_layer_surface_v1 proxy.
  - TOP: Top margin in pixels.
  - RIGHT: Right margin in pixels.
  - BOTTOM: Bottom margin in pixels.
  - LEFT: Left margin in pixels."
  (when (valid-pointer? proxy-layer-surface)
    (when *log-river-calls*
	  (log-debug "Setting layer surface ~a margin: top=~a right=~a bottom=~a left=~a"
				 proxy-layer-surface top right bottom left))
    (zwlr-layer-surface-v1-set-margin proxy-layer-surface top right bottom left)))

(define (wlr-layer-surface-set-keyboard-interactivity proxy-layer-surface interactivity)
  "Request keyboard interactivity mode for layer surface.
Parameters: (PROXY-LAYER-SURFACE INTERACTIVITY)
  - PROXY-LAYER-SURFACE: Pointer to zwlr_layer_surface_v1 proxy.
  - INTERACTIVITY: Interactivity mode enum (e.g. *wlr-layer-shell-interactivity-none*)."
  (when (valid-pointer? proxy-layer-surface)
    (when *log-river-calls*
	  (log-debug "Setting layer surface ~a keyboard interactivity to ~a"
				 proxy-layer-surface interactivity))
    (zwlr-layer-surface-v1-set-keyboard-interactivity proxy-layer-surface interactivity)))

(define (wlr-layer-surface-get-popup proxy-layer-surface popup)
  "Assign layer surface as an xdg_popup parent.
Parameters: (PROXY-LAYER-SURFACE POPUP)
  - PROXY-LAYER-SURFACE: Pointer to zwlr_layer_surface_v1 proxy.
  - POPUP: Pointer to xdg_popup proxy."
  (when (and (valid-pointer? proxy-layer-surface) (valid-pointer? popup))
    (when *log-river-calls*
	  (log-debug "Assigning popup ~a to layer surface ~a"
				 popup proxy-layer-surface))
    (zwlr-layer-surface-v1-get-popup proxy-layer-surface popup)))

(define (wlr-layer-surface-ack-configure proxy-layer-surface serial)
  "Acknowledge configure event from compositor.
Must be called after processing a configure event.
Parameters: (PROXY-LAYER-SURFACE SERIAL)
  - PROXY-LAYER-SURFACE: Pointer to zwlr_layer_surface_v1 proxy.
  - SERIAL: Serial number from configure event."
  (when (valid-pointer? proxy-layer-surface)
    (when *log-river-calls*
	  (log-debug "Acking configure on layer surface ~a serial=~a"
				 proxy-layer-surface serial))
    (zwlr-layer-surface-v1-ack-configure proxy-layer-surface serial)))

(define (wlr-layer-surface-set-layer proxy-layer-surface layer)
  "Change layer of surface.
Parameters: (PROXY-LAYER-SURFACE LAYER)
  - PROXY-LAYER-SURFACE: Pointer to zwlr_layer_surface_v1 proxy.
  - LAYER: Layer enum (e.g. *wlr-layer-shell-layer-background*)."
  (when (valid-pointer? proxy-layer-surface)
    (when *log-river-calls*
	  (log-debug "Setting layer surface ~a layer to ~a"
				 proxy-layer-surface layer))
    (zwlr-layer-surface-v1-set-layer proxy-layer-surface layer)))

(define (wlr-layer-surface-set-exclusive-edge proxy-layer-surface edge)
  "Set edge the exclusive zone will be applied to.
Parameters: (PROXY-LAYER-SURFACE EDGE)
  - PROXY-LAYER-SURFACE: Pointer to zwlr_layer_surface_v1 proxy.
  - EDGE: Anchor edge flag (e.g. *wlr-layer-shell-anchor-top*)."
  (when (valid-pointer? proxy-layer-surface)
    (when *log-river-calls*
	  (log-debug "Setting layer surface ~a exclusive edge to ~a"
				 proxy-layer-surface edge))
    (zwlr-layer-surface-v1-set-exclusive-edge proxy-layer-surface edge)))

;;; zwlr_layer_surface_v1 events
(define (wlr-layer-surface-on-configure data proxy-layer-surface serial width height)
  "Invoked when compositor configures layer surface.
Parameters: (DATA PROXY-LAYER-SURFACE SERIAL WIDTH HEIGHT)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-LAYER-SURFACE: Pointer to zwlr_layer_surface_v1 proxy.
  - SERIAL: Serial number of this configure event.
  - WIDTH: Allocated width of the layer surface in pixels.
  - HEIGHT: Allocated height of the layer surface in pixels."
  (when *log-river-calls*
	(log-debug "Layer surface ~a configure: serial=~a ~ax~a"
			   proxy-layer-surface serial width height))
  (gliver-hook-run! %wlr-layer-surface-configure-hook
					data proxy-layer-surface serial width height))

(define (wlr-layer-surface-on-closed data proxy-layer-surface)
  "Invoked when layer surface has been closed by compositor.
Parameters: (DATA PROXY-LAYER-SURFACE)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-LAYER-SURFACE: Pointer to zwlr_layer_surface_v1 proxy."
  (when *log-river-calls*
	(log-debug "Layer surface closed: ~a" proxy-layer-surface))
  (wlr-layer-surface-destroy proxy-layer-surface)
  (gliver-hook-run! %wlr-layer-surface-closed-hook data proxy-layer-surface))

;; handle river initialization steps
(gliver-hook-add! *gliver-globals-bind-hook* 'wlr-layer-shell-on-globals-bind)
(gliver-hook-add! *gliver-globals-unbind-hook* 'wlr-layer-shell-on-globals-unbind)
(gliver-hook-add! *gliver-globals-verify-hook* 'wlr-layer-shell-on-globals-verify)
(gliver-hook-add! *gliver-listeners-attach-hook* 'wlr-layer-shell-on-listeners-attach)
