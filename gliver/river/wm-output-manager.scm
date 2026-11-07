;;; gliver/river/wm-output-manager.scm --- River window management output integration
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module manages the output life-cycle after its creation
;;;
;;; It handles:
;;;   - Binding river_output_v1 and exposing interface requests and events
;;;   - Updating output presentation mode (enable/disable VSync)
;;;   - Binding and releasing wl_output proxies
;;;
;;; This module implements the river interfaces (protocol river_window_manager_v1):
;;;   - river_output_v1

(define-module (gliver river wm-output-manager)
  #:use-module (gliver core-base)
  #:use-module (gliver river connector)
  #:use-module (gliver wayland client)
  #:use-module (gliver wayland gen river-window-management-v1)
  #:use-module (gliver wayland gen wayland)
  #:use-module (system foreign)
  #:export (
			*wm-output-listener*
			*wl-output-listener*
			wm-output-on-listeners-attach
			wm-output-on-output
			wm-output-destroy
			wm-output-presentation-mode-set
			wm-output-wl-output-bind
			wm-output-wl-output-release
			wm-output-on-output-removed
			wm-output-on-output-wl-output
			wm-output-on-output-position
			wm-output-on-output-dimensions
			wm-output-on-wl-output-geometry
			wm-output-on-wl-output-mode
			wm-output-on-wl-output-done
			wm-output-on-wl-output-scale
			wm-output-on-wl-output-name
			wm-output-on-wl-output-description
))

(define-var *wm-output-listener* #f
			"Wayland listener for river_output_v1 events.")

(define-var *wl-output-listener* #f
			"Wayland listener for wl_output events.")

(define (wm-output-on-listeners-attach)
  "Attach river_output_v1 and wl_output event listeners.
Parameters: None."
  (set! *wm-output-listener*
        (make-river-output-v1-listener
         wm-output-on-output-removed
         wm-output-on-output-wl-output
         wm-output-on-output-position
         wm-output-on-output-dimensions))
  ;; wl-output events are actually part of the wayland protocol
  ;; not river, they advertise useful information like name and scale
  (set! *wl-output-listener*
        (make-wl-output-listener
         wm-output-on-wl-output-geometry
         wm-output-on-wl-output-mode
         wm-output-on-wl-output-done
         wm-output-on-wl-output-scale
         wm-output-on-wl-output-name
         wm-output-on-wl-output-description)))
(gliver-hook-add! *gliver-listeners-attach-hook* 'wm-output-on-listeners-attach 0)

(define (wm-output-on-output data manager proxy-output)
  "Invoked when an output is advertised.
Attach the river_output_v1 listener.
Parameters: (DATA MANAGER PROXY-OUTPUT)
  - DATA: User context pointer (unused, null in Gliver).
  - MANAGER: Pointer to river_window_manager_v1 proxy.
  - PROXY-OUTPUT: Pointer to river_output_v1 proxy."
  (when *log-river-calls*
	(log-debug "New output pointer received: ~a" proxy-output))
  (when *wm-output-listener*
	;; add the new output to the global output listener
    (wl-proxy-add-listener proxy-output *wm-output-listener* %null-pointer)))
(gliver-hook-add! %output-created-hook 'wm-output-on-output 0)

;;; output requests
(define (wm-output-destroy proxy-output)
  "Destroy river_output_v1 proxy object, this means everything was
cleared from client side and the server should also clear everything.
Parameters: (PROXY-OUTPUT)
  - PROXY-OUTPUT: Pointer to river_output_v1 proxy."
  (when (valid-pointer? proxy-output)
    (when *log-river-calls*
	  (log-debug "Destroying output proxy: ~a" proxy-output))
    (river-output-v1-destroy proxy-output)))

(define (wm-output-presentation-mode-set proxy-output mode)
  "Set the preferred presentation mode of the output.
Must be called in a render sequence.
Parameters: (PROXY-OUTPUT MODE)
  - PROXY-OUTPUT: Pointer to river_output_v1 proxy.
  - MODE: Presentation mode enum RIVER_OUTPUT_V1_PRESENTATION_MODE_VSYNC,
or RIVER_OUTPUT_V1_PRESENTATION_MODE_ASYNC."
  (when (valid-pointer? proxy-output)
    (when *log-river-calls*
	  (log-debug "Setting output ~a presentation mode to ~a" proxy-output mode))
    (river-output-v1-set-presentation-mode proxy-output mode)))

(define* (wm-output-wl-output-bind object-id #:optional (version 4))
  "Bind wl_output global OBJECT-ID from Wayland registry.
Parameters: (OBJECT-ID [VERSION])
  - OBJECT-ID: Numeric Wayland object ID.
  - VERSION: Interface version (default 4).
Returns: wl_output proxy pointer, or #f if registry is unavailable or invalid."
  (when (number? object-id)
    (when *log-river-calls*
      (log-debug "Binding wl_output object ~a (version ~a)" object-id version))
    (let ((proxy (gliver-wl-registry-bind *wl-registry* object-id *wl-output-interface* version)))
	  ;; after receiving a valid proxy of wl-output, attach the listeners
      (when (and (valid-pointer? proxy) *wl-output-listener*)
        (wl-proxy-add-listener proxy *wl-output-listener* %null-pointer))
      proxy)))

(define (wm-output-wl-output-release proxy-wl-output)
  "Release wl_output proxy object if valid.
Parameters: (PROXY-WL-OUTPUT)
  - PROXY-WL-OUTPUT: Pointer to wl_output proxy."
  (when (valid-pointer? proxy-wl-output)
    (when *log-river-calls*
      (log-debug "Releasing wl_output proxy: ~a" proxy-wl-output))
    (wl-output-release proxy-wl-output)))

;;; output events
(define (wm-output-on-output-removed data proxy-output)
  "Invoked when output is removed, destroy proxy, and clean up state.
Parameters: (DATA PROXY-OUTPUT)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-OUTPUT: Pointer to river_output_v1 proxy."
  (when *log-river-calls*
	(log-debug "Output removed: ~a" proxy-output))
  (gliver-hook-run! %output-removed-hook data proxy-output)
  (wm-output-destroy proxy-output))

(define (wm-output-on-output-wl-output data proxy-output object-id)
  "Invoked when a wl_output is advertised to a river_output_v1.
Parameters: (DATA PROXY-OUTPUT OBJECT-ID)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-OUTPUT: Pointer to river_output_v1 proxy.
  - OBJECT-ID: Numeric Wayland object ID of the wl_output global."
  (when *log-river-calls*
	(log-debug "Output wl_output object-id: ~a = ~a" proxy-output object-id))
  (gliver-hook-run! %output-object-id-changed-hook data proxy-output object-id))

(define (wm-output-on-output-position data proxy-output x y)
  "Invoked when position of output in the compositor's logical space
has changed. The X and Y coordinates may be positive or negative.
Parameters: (DATA PROXY-OUTPUT X Y)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-OUTPUT: Pointer to river_output_v1 proxy.
  - X: Logical horizontal position in compositor space.
  - Y: Logical vertical position in compositor space."
  (when *log-river-calls*
	(log-debug "Output position: ~a = ~a,~a" proxy-output x y))
  (gliver-hook-run! %output-position-changed-hook data proxy-output x y))

(define (wm-output-on-output-dimensions data proxy-output width height)
  "Invoked when dimensions of the output in the compositor's logical coordinate
space has changed. The WIDTH and HEIGHT will always be greater than zero.
Parameters: (DATA PROXY-OUTPUT WIDTH HEIGHT)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-OUTPUT: Pointer to river_output_v1 proxy.
  - WIDTH: Logical output width in pixels.
  - HEIGHT: Logical output height in pixels."
  (when *log-river-calls*
	(log-debug "Output dimensions: ~a = ~ax~a" proxy-output width height))
  (gliver-hook-run! %output-dimensions-changed-hook data proxy-output width height))

;;; wl-output events
(define (wm-output-on-wl-output-geometry data proxy x y physical-width physical-height subpixel make-ptr model-ptr transform)
  "Invoked when wl_output advertises geometric properties."
  (let ((make (and (valid-pointer? make-ptr)
				   (pointer->string make-ptr)))
        (model (and (valid-pointer? model-ptr)
					(pointer->string model-ptr))))
    (when *log-river-calls*
      (log-debug "wl_output ~a geometry: x=~a y=~a size=~ax~amm subpixel=~a make=~a model=~a transform=~a"
                 proxy x y physical-width physical-height subpixel make model transform))
    (gliver-hook-run! %output-geometry-changed-hook data proxy x y physical-width physical-height subpixel make model transform)))

(define (wm-output-on-wl-output-mode data proxy flags width height refresh)
  "Invoked when wl_output advertises an available mode."
  (when *log-river-calls*
    (log-debug "wl_output ~a mode: ~ax~a @ ~amHz flags=~a"
               proxy width height refresh flags))
  (gliver-hook-run! %output-mode-changed-hook data proxy flags width height refresh))

(define (wm-output-on-wl-output-done data proxy)
  "Invoked when all wl_output properties in the current batch have been sent."
  (when *log-river-calls*
    (log-debug "wl_output ~a done" proxy))
  (gliver-hook-run! %output-done-hook data proxy))

(define (wm-output-on-wl-output-scale data proxy factor)
  "Invoked when wl_output advertises output scaling factor."
  (when *log-river-calls*
    (log-debug "wl_output ~a scale: ~a" proxy factor))
  (gliver-hook-run! %output-scale-changed-hook data proxy factor))

(define (wm-output-on-wl-output-name data proxy name-ptr)
  "Invoked when wl_output advertises hardware output name."
  (let ((name (and (valid-pointer? name-ptr) (pointer->string name-ptr))))
    (when name
      (when *log-river-calls*
        (log-debug "wl_output ~a hardware name: ~a" proxy name))
      (gliver-hook-run! %output-name-changed-hook data proxy name))))

(define (wm-output-on-wl-output-description data proxy desc-ptr)
  "Invoked when wl_output advertises human-readable description."
  (let ((desc (and (valid-pointer? desc-ptr) (pointer->string desc-ptr))))
    (when desc
      (when *log-river-calls*
        (log-debug "wl_output ~a description: ~a" proxy desc))
      (gliver-hook-run! %output-description-changed-hook data proxy desc))))
