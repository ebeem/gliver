;;; gliver/river/input-manager.scm --- River input management integration
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module manages seats and input devices in river.
;;; It handles:
;;;   - Binding/unbinding protocol river_input_manager_v1 global
;;;   - Tracking physical input devices (keyboards, pointers, touch, tablet)
;;;   - Assigning input devices to seats
;;;   - Configuring keyboard repeat rate and delay
;;;   - Setting pointer scroll factor
;;;   - Mapping input devices to outputs or rectangles
;;;
;;; This module implements the river interfaces (protocol river_input_management_v1):
;;;   - river_input_manager_v1
;;;   - river_input_device_v1

(define-module (gliver river input-manager)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (system foreign)
  #:use-module (gliver river connector)
  #:use-module (gliver wayland client)
  #:use-module (gliver wayland gen river-input-management-v1)
  #:use-module (gliver core-base)
  #:declarative? #f
  #:export (
			*input-manager*
			*input-devices*
			*input-manager-listener*
			*input-device-listener*
			*input-device-type-keyboard*
			*input-device-type-pointer*
			*input-device-type-touch*
			*input-device-type-tablet*
			input-manager-on-globals-bind
			input-manager-on-globals-unbind
			input-manager-on-globals-verify
			input-manager-on-listeners-attach
			input-manager-stop
			input-manager-destroy
			input-manager-create-seat
			input-manager-destroy-seat
			input-manager-on-finished
			input-manager-on-input-device
			input-device-destroy
			input-device-assign-to-seat
			input-device-set-repeat-info
			input-device-set-scroll-factor
			input-device-map-to-output
			input-device-map-to-rectangle
			input-device-on-removed
			input-device-on-type
			input-device-on-name
))

;;; global state
(define-var *input-manager* %null-pointer
			"Proxy for river_input_manager_v1.")
(define-var *input-devices* '()
			"list of river_input_device_v1 proxies for tracking.")

(define-var *input-manager-listener* #f
			"Shared Wayland listener for river_input_manager_v1 events.")
(define-var *input-device-listener* #f
			"Shared Wayland listener for river_input_device_v1 events.")

(define *input-device-type-keyboard* RIVER_INPUT_DEVICE_V1_TYPE_KEYBOARD)
(define *input-device-type-pointer* RIVER_INPUT_DEVICE_V1_TYPE_POINTER)
(define *input-device-type-touch* RIVER_INPUT_DEVICE_V1_TYPE_TOUCH)
(define *input-device-type-tablet* RIVER_INPUT_DEVICE_V1_TYPE_TABLET)

(define (input-manager-on-globals-bind registry protocol-name object-id version)
  "Bind river_input_manager_v1 global interface from REGISTRY.
Matches PROTOCOL-NAME against `RIVER_INPUT_MANAGER_V1_NAME` and binds OBJECT-ID
with VERSION.
Parameters: (REGISTRY PROTOCOL-NAME OBJECT-ID VERSION)
  - REGISTRY: Pointer to wl_registry proxy.
  - PROTOCOL-NAME: Interface name string.
  - OBJECT-ID: Numeric Wayland object ID.
  - VERSION: Maximum interface version supported by compositor."
  (when (string=? protocol-name RIVER_INPUT_MANAGER_V1_NAME)
    (log-info "Binding ~a..." protocol-name)
    (set! *input-manager*
          (gliver-wl-registry-bind registry object-id
                                   *river-input-manager-v1-interface*
                                   (min version 1)))))

(define (input-manager-on-globals-unbind)
  "Unbind/destroy the input manager and all associated objects."
  ;; destroy active input devices
  (for-each (lambda (proxy)
              (input-device-destroy proxy))
            *input-devices*)

  (when (valid-pointer? *input-manager*)
	(input-manager-destroy *input-manager*)))

(define (input-manager-on-globals-verify)
  "Verify the input manager global was bound.
Note: input manager is optional per the protocol spec, so we only warn."
  (when (null-pointer? *input-manager*)
    (log-warn "river_input_manager_v1 not available, input device management disabled")))

(define (input-manager-on-listeners-attach)
  "Create the shared listeners for input manager and input device events."
  ;; create listener for river_input_device_v1 events
  (set! *input-device-listener*
        (make-river-input-device-v1-listener
         input-device-on-removed
         input-device-on-type
         input-device-on-name))

  ;; create listener for river_input_manager_v1 events
  (set! *input-manager-listener*
        (make-river-input-manager-v1-listener
         input-manager-on-finished
         input-manager-on-input-device))

  ;; attach listener to river_input_manager_v1 global proxy
  (when (valid-pointer? *input-manager*)
    (wl-proxy-add-listener *input-manager* *input-manager-listener* %null-pointer)))

;;; input manager requests
(define (input-manager-stop proxy-manager)
  "Stop receiving events river_input_manager_v1 proxy object.
The client must wait for a river_input_manager_v1.finished event before
destroying this object. It's a protocol error to destroy before stopping.
Parameters: (PROXY-MANAGER)
  - PROXY-MANAGER: Pointer to river_input_manager_v1 proxy."
  (when proxy-manager
    (when *log-river-calls*
	  (log-debug "Stopping input manager proxy: ~a" proxy-manager))
    (river-input-manager-v1-stop proxy-manager)))

(define (input-manager-destroy proxy-manager)
  "Destroy river_input_manager_v1 proxy object, this means everything was
cleared from client side and the server should also clear everything.
NOTE: This should only be called after the finished event has been received.
Parameters: (PROXY-MANAGER)
  - PROXY-MANAGER: Pointer to river_input_manager_v1 proxy."
  (when proxy-manager
    (when *log-river-calls*
	  (log-debug "Destroying input manager proxy: ~a" proxy-manager))
    (river-input-manager-v1-destroy proxy-manager)
	(set! *input-manager* %null-pointer)))

(define (input-manager-create-seat proxy-manager name)
  "Create a new seat with the given NAME.
Has no effect if a seat with the given name already exists.
The default seat with name \"default\" always exists.
Parameters: (PROXY-MANAGER NAME)
  - PROXY-MANAGER: Pointer to river_input_manager_v1 proxy.
  - NAME: String seat name."
  (when (and proxy-manager name)
    (when *log-river-calls*
	  (log-debug "Creating seat ~a on input manager ~a" name proxy-manager))
    (river-input-manager-v1-create-seat proxy-manager name)))

(define (input-manager-destroy-seat proxy-manager name)
  "Destroy the seat with the given NAME.
Has no effect if a seat with the given name does not exist.
The default seat with name \"default\" cannot be destroyed.
Parameters: (PROXY-MANAGER NAME)
  - PROXY-MANAGER: Pointer to river_input_manager_v1 proxy.
  - NAME: String seat name."
  (when (and proxy-manager name)
    (when *log-river-calls*
	  (log-debug "Destroying seat ~a on input manager ~a" name proxy-manager))
    (river-input-manager-v1-destroy-seat proxy-manager name)))

;;; input manager events
(define (input-manager-on-finished data proxy-manager)
  "Invoked when river compositor indicates that it will send no further events
on river_input_manager_v1. The client should destroy the object.
Parameters: (DATA PROXY-MANAGER)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-MANAGER: Pointer to river_input_manager_v1 proxy."
  (when *log-river-calls*
	(log-debug "Input manager finished: ~a" proxy-manager))
  (gliver-hook-run! %input-manager-finished-hook data proxy-manager)
  (input-manager-destroy proxy-manager))

(define (input-manager-on-input-device data proxy-manager proxy-device)
  "Invoked when a new input device is advertised by compositor.
Parameters: (DATA PROXY-MANAGER PROXY-DEVICE)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-MANAGER: Pointer to river_input_manager_v1 proxy.
  - PROXY-DEVICE: Pointer to river_input_device_v1 proxy."
  (when *log-river-calls*
	(log-debug "New input device received: ~a" proxy-device))
  (when (and proxy-device (valid-pointer? proxy-device))
	;; attach shared device listener to receive input device events
	(when *input-device-listener*
	  (wl-proxy-add-listener proxy-device *input-device-listener* %null-pointer))
	;; track the newly created input device proxy
	(set! *input-devices* (cons proxy-device *input-devices*))
	(gliver-hook-run! %input-manager-device-created-hook
					  data proxy-manager proxy-device)))

;;; input device requests
(define (input-device-destroy proxy-device)
  "Destroy river_input_device_v1 proxy object, this means everything was
cleared from client side and the server should also clear everything.
Parameters: (PROXY-DEVICE)
  - PROXY-DEVICE: Pointer to river_input_device_v1 proxy."
  (when proxy-device
    (when *log-river-calls*
	  (log-debug "Destroying input device proxy: ~a" proxy-device))
    (river-input-device-v1-destroy proxy-device)
	;; untrack the proxy
	(set! *input-devices*
		  (delete proxy-device *input-devices*))))

(define (input-device-assign-to-seat proxy-device name)
  "Assign the input device to a seat with NAME. All input devices not explicitly
assigned to a seat are considered assigned to the default seat.
Has no effect if a seat with the given name does not exist.
Parameters: (PROXY-DEVICE NAME)
  - PROXY-DEVICE: Pointer to river_input_device_v1 proxy.
  - NAME: String seat name."
  (when (and proxy-device name)
    (when *log-river-calls*
	  (log-debug "Assigning input device ~a to seat ~a" proxy-device name))
    (river-input-device-v1-assign-to-seat proxy-device name)))

(define (input-device-set-repeat-info proxy-device rate delay)
  "Set keyboard repeat rate and delay for a keyboard input device.
Has no effect if the device is not a keyboard. Negative values for either
rate or delay are illegal. A rate of zero will disable any repeating.
Parameters: (PROXY-DEVICE RATE DELAY)
  - PROXY-DEVICE: Pointer to river_input_device_v1 proxy.
  - RATE: Rate in key repeats per second.
  - DELAY: Delay in milliseconds."
  (when (and proxy-device rate delay)
    (when *log-river-calls*
	  (log-debug "Setting input device ~a repeat info rate=~a delay=~a"
				 proxy-device rate delay))
    (river-input-device-v1-set-repeat-info proxy-device rate delay)))

(define (input-device-set-scroll-factor proxy-device factor)
  "Set scroll factor for a pointer input device.
Has no effect if the device is not a pointer. Setting a scroll factor
less than 0 is a protocol error.
Parameters: (PROXY-DEVICE FACTOR)
  - PROXY-DEVICE: Pointer to river_input_device_v1 proxy.
  - FACTOR: Scroll factor multiplier (fixed point number or inexact float)."
  (when (and proxy-device factor)
    (when *log-river-calls*
	  (log-debug "Setting input device ~a scroll factor to ~a"
				 proxy-device factor))
	(let ((fixed-factor (if (inexact? factor)
							(inexact->exact (round (* factor 256.0)))
							factor)))
	  (river-input-device-v1-set-scroll-factor proxy-device fixed-factor))))

(define (input-device-map-to-output proxy-device output)
  "Map the input device to the given wl_output proxy.
Has no effect if the device is not a pointer, touch, or tablet device.
If mapped to both an output and a rectangle, the rectangle has priority.
Passing null clears an existing mapping.
Parameters: (PROXY-DEVICE OUTPUT)
  - PROXY-DEVICE: Pointer to river_input_device_v1 proxy.
  - OUTPUT: Pointer to wl_output proxy, or %null-pointer / #f to clear."
  (when (valid-pointer? proxy-device)
	(let ((out (if (valid-pointer? output)
				   output
				   %null-pointer)))
	  (when *log-river-calls*
		(log-debug "Mapping input device ~a to output ~a" proxy-device out))
	  (river-input-device-v1-map-to-output proxy-device out))))

(define (input-device-map-to-rectangle proxy-device x y width height)
  "Map input device to the given rectangle in global compositor coordinate space.
Has no effect if the device is not a pointer, touch, or tablet device.
If mapped to both an output and a rectangle, the rectangle has priority.
Width and height must be greater than or equal to 0. Passing 0 for width
or height clears an existing mapping.
Parameters: (PROXY-DEVICE X Y WIDTH HEIGHT)
  - PROXY-DEVICE: Pointer to river_input_device_v1 proxy.
  - X: Horizontal coordinate in compositor space.
  - Y: Vertical coordinate in compositor space.
  - WIDTH: Rectangle width (>= 0).
  - HEIGHT: Rectangle height (>= 0)."
  (when proxy-device
    (when *log-river-calls*
	  (log-debug "Mapping input device ~a to rectangle ~ax~a+(~a,~a)"
				 proxy-device width height x y))
    (river-input-device-v1-map-to-rectangle proxy-device x y width height)))

;;; input device events
(define (input-device-on-removed data proxy-device)
  "Invoked when an input device is removed from compositor.
Parameters: (DATA PROXY-DEVICE)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-DEVICE: Pointer to river_input_device_v1 proxy."
  (when *log-river-calls*
	(log-debug "Input device removed: ~a" proxy-device))
  (gliver-hook-run! %input-device-removed-hook data proxy-device)
  (input-device-destroy proxy-device))

(define (input-device-on-type data proxy-device type)
  "Invoked when the type of the input device is received.
Sent only once when the river_input_device_v1 object is created.
Parameters: (DATA PROXY-DEVICE TYPE)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-DEVICE: Pointer to river_input_device_v1 proxy.
  - TYPE: Numeric enum (keyboard=0, pointer=1, touch=2, tablet=3)."
  (when *log-river-calls*
	(log-debug "Input device ~a type: ~a" proxy-device type))
  (gliver-hook-run! %input-device-type-hook data proxy-device type))

(define (input-device-on-name data proxy-device name-ptr)
  "Invoked when the name of the input device is received.
Sent only once when the river_input_device_v1 object is created.
Parameters: (DATA PROXY-DEVICE NAME-PTR)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-DEVICE: Pointer to river_input_device_v1 proxy.
  - NAME-PTR: Pointer to device name string."
  (let ((name (if (valid-pointer? name-ptr)
				  (pointer->string name-ptr)
				  "")))
	(when *log-river-calls*
	  (log-debug "Input device ~a name: ~a" proxy-device name))
	(gliver-hook-run! %input-device-name-hook data proxy-device name)))

;; handle river initialization steps
(gliver-hook-add! *gliver-globals-bind-hook* 'input-manager-on-globals-bind)
(gliver-hook-add! *gliver-globals-unbind-hook* 'input-manager-on-globals-unbind)
(gliver-hook-add! *gliver-globals-verify-hook* 'input-manager-on-globals-verify)
(gliver-hook-add! *gliver-listeners-attach-hook* 'input-manager-on-listeners-attach)
