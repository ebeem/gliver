;;; gliver/river/xkb-config-manager.scm --- River keyboard and xkb configuration integration
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module manages keyboard devices and xkb layouts in river.
;;; It handles:
;;;   - Binding/unbinding protocol river_xkb_config_v1 global
;;;   - Uploading xkb keymaps to the compositor
;;;   - Switching active layouts by index or name
;;;   - Toggling capslock and numlock states
;;;
;;; This module implements the river interfaces (protocol river_xkb_config_v1):
;;;   - river_xkb_config_v1
;;;   - river_xkb_keymap_v1
;;;   - river_xkb_keyboard_v1

(define-module (gliver river xkb-config-manager)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (gliver river connector)
  #:use-module (gliver wayland client)
  #:use-module (gliver wayland gen river-xkb-config-v1)
  #:use-module (gliver deps libc)
  #:use-module (gliver deps libxkbcommon)
  #:use-module (gliver core-base)
  #:declarative? #f
  #:export (
			*xkb-config*
			*xkb-config-keymaps*
			*xkb-config-keyboards*
			*xkb-config-listener*
			*xkb-config-keymap-listener*
			*xkb-config-keyboard-listener*
			xkb-config-on-globals-bind
			xkb-config-on-globals-unbind
			xkb-config-on-globals-verify
			xkb-config-on-listeners-attach
			xkb-config-stop
			xkb-config-destroy
			xkb-config-compile-keymap
			xkb-config-create-keymap
			xkb-config-create-keymap-from-string
			xkb-config-create-keymap-from-layout
			xkb-config-on-finished
			xkb-config-on-keyboard
			xkb-keymap-destroy
			xkb-keymap-on-success
			xkb-keymap-on-failure
			xkb-keyboard-destroy
			xkb-keyboard-set-keymap
			xkb-keyboard-set-layout-by-index
			xkb-keyboard-set-layout-by-name
			xkb-keyboard-capslock-enable
			xkb-keyboard-capslock-disable
			xkb-keyboard-numlock-enable
			xkb-keyboard-numlock-disable
			xkb-keyboard-on-removed
			xkb-keyboard-on-input-device
			xkb-keyboard-on-layout
			xkb-keyboard-on-capslock-enabled
			xkb-keyboard-on-capslock-disabled
			xkb-keyboard-on-numlock-enabled
			xkb-keyboard-on-numlock-disabled
))

;;; global state
(define-var *xkb-config* %null-pointer
			"Proxy of river_xkb_config_v1 bound from the wayland registry.")
(define-var *xkb-config-keymaps* '()
			"list of river_xkb_keymap_v1 proxies for tracking.")
(define-var *xkb-config-keyboards* '()
			"list of river_xkb_keyboard_v1 proxies for tracking.")

(define-var *xkb-config-listener* #f
			"Wayland listener for river_xkb_config_v1 global events.")
(define-var *xkb-config-keymap-listener* #f
			"Wayland listener for river_xkb_keymap_v1 success/failure events.")
(define-var *xkb-config-keyboard-listener* #f
			"Wayland listener for river_xkb_keyboard_v1 device events.")

(define (xkb-config-on-globals-bind registry protocol-name object-id version)
  "Bind river_xkb_config_v1 global interface from REGISTRY.
Matches PROTOCOL-NAME against `RIVER_XKB_CONFIG_V1_NAME` and binds OBJECT-ID
with VERSION.
Parameters: (REGISTRY PROTOCOL-NAME OBJECT-ID VERSION)
  - REGISTRY: Pointer to wl_registry proxy.
  - PROTOCOL-NAME: Interface name string.
  - OBJECT-ID: Numeric Wayland object ID.
  - VERSION: Maximum interface version supported by compositor."
  (when (string=? protocol-name RIVER_XKB_CONFIG_V1_NAME)
    (log-info "Binding ~a..." protocol-name)
    (set! *xkb-config*
          (gliver-wl-registry-bind registry object-id
                                   *river-xkb-config-v1-interface*
                                   (min version 1)))))

(define (xkb-config-on-globals-unbind)
  "Unbind/destroy the xkb config manager and associated objects."
  ;; destroy active keyboards
  (for-each (lambda (proxy)
              (xkb-keyboard-destroy proxy))
            *xkb-config-keyboards*)

  ;; destroy active keymaps
  (for-each (lambda (proxy)
              (xkb-keymap-destroy proxy))
            *xkb-config-keymaps*)

  (when (valid-pointer? *xkb-config*)
	;; NOTE: shouldn't call destroy directly
	;; (xkb-config-destroy *xkb-config*)
	(xkb-config-stop *xkb-config*)))

(define (xkb-config-on-globals-verify)
  "Verify that *xkb-config-xkb-config* was bound successfully, raise a warning if not."
  ;; warn if the protocol is not advertised by the compositor
  ;; usually this should be a mandatory protocol and not finding it
  ;; should actually raise an error
  (when (null-pointer? *xkb-config*)
    (log-warn "river_xkb_config_v1 not available, keyboard xkb configuration disabled")))

(define (xkb-config-on-listeners-attach)
  "Create the shared listeners for xkb config, keymap, and keyboard events."
  ;; create listener for river_xkb_keymap_v1 events
  (set! *xkb-config-keymap-listener*
        (make-river-xkb-keymap-v1-listener
         xkb-keymap-on-success
         xkb-keymap-on-failure))

  ;; create listener for river_xkb_keyboard_v1 events
  (set! *xkb-config-keyboard-listener*
        (make-river-xkb-keyboard-v1-listener
         xkb-keyboard-on-removed
         xkb-keyboard-on-input-device
         xkb-keyboard-on-layout
         xkb-keyboard-on-capslock-enabled
         xkb-keyboard-on-capslock-disabled
         xkb-keyboard-on-numlock-enabled
         xkb-keyboard-on-numlock-disabled))

  ;; create listener for river_xkb_config_v1 events
  (set! *xkb-config-listener*
        (make-river-xkb-config-v1-listener
         xkb-config-on-finished
         xkb-config-on-keyboard))

  ;; attach listener to river_xkb_config_v1 global proxy
  (when (valid-pointer? *xkb-config*)
    (wl-proxy-add-listener *xkb-config* *xkb-config-listener* %null-pointer)))

;;; river_xkb_config_v1 requests
(define (xkb-config-stop proxy-config)
  "Stop river_xkb_config_v1 proxy object, this means that Gliver
is no longer interested in this object nor it will send further requests for it.
NOTE: this should be called before destroying the river_xkb_config_v1.
Parameters: (PROXY-CONFIG)
  - PROXY-CONFIG: Pointer to river_xkb_config_v1 proxy."
  (when proxy-config
    (when *log-river-calls*
	  (log-debug "Stopping xkb config proxy: ~a" proxy-config))
    (river-xkb-config-v1-stop proxy-config)))

(define (xkb-config-destroy proxy-config)
  "Destroy river_xkb_config_v1 proxy object, this means everything was
cleared from client side and the server should also clear everything.
NOTE: This should only be called after the finished event has been received.
Parameters: (PROXY-CONFIG)
  - PROXY-CONFIG: Pointer to river_xkb_config_v1 proxy."
  (when proxy-config
    (when *log-river-calls*
	  (log-debug "Destroying xkb config proxy: ~a" proxy-config))
    (river-xkb-config-v1-destroy proxy-config)
	(set! *xkb-config* %null-pointer)))

(define (xkb-config-compile-keymap layout-str variant-str)
  "Compile keymap definition from LAYOUT-STR and VARIANT-STR.
Returns a string representing the kepmap definition or #f on failure."
  (when *log-river-calls*
	(log-debug "keyboard-manager: compiling keymap (layout=~s, variant=~s)"
               layout-str variant-str))
  (call-with-values
      (lambda ()
		;; NOTE: rules and model can be optionally passed
		;; I am not sure if this will be of any use
		;; I also don't very well understand the meaning of these values
        (xkb-keymap-compile-from-names #:layout layout-str
                                       #:variant variant-str))
    (lambda (km ctx)
      (let ((keymap-def (and (valid-pointer? km) (xkb-keymap->string km))))
		;; cleanup keymap and context as they are not needed anymore
        (when (valid-pointer? km) (xkb-keymap-unref km))
        (when (valid-pointer? ctx) (xkb-context-unref ctx))
        (cond
         ((not (valid-pointer? km))
          (log-error "keyboard-manager: failed to compile keymap for layout ~s" layout-str)
          #f)
         ((not keymap-def)
          (log-error "keyboard-manager: failed to convert keymap to string")
          #f)
         (else keymap-def))))))

(define* (xkb-config-create-keymap proxy-config fd #:optional (format RIVER_XKB_CONFIG_V1_KEYMAP_FORMAT_TEXT_V1))
  "Create a keymap object from file descriptor FD and FORMAT.
Parameters: (PROXY-CONFIG FD [FORMAT])
  - PROXY-CONFIG: Pointer to river_xkb_config_v1 proxy.
  - FD: File descriptor containing keymap definition.
  - FORMAT: Keymap format enum (default RIVER_XKB_CONFIG_V1_KEYMAP_FORMAT_TEXT_V1).
Returns river_xkb_keymap_v1 proxy pointer."
  (when (and proxy-config fd)
    (when *log-river-calls*
	  (log-debug "Creating keymap on xkb config ~a with fd=~a format=~a"
				 proxy-config fd format))
	;; create the keymap and get its proxy
	(let ((keymap (river-xkb-config-v1-create-keymap proxy-config fd format)))
	  (when (valid-pointer? keymap)
		;; attach the keymap listener
		(wl-proxy-add-listener keymap *xkb-config-keymap-listener* %null-pointer)
		;; add keymap to *xkb-config-keymaps* for tracking
		(set! *xkb-config-keymaps* (cons keymap *xkb-config-keymaps*)))
	  keymap)))

(define* (xkb-config-create-keymap-from-string proxy-config keymap-str #:optional (format RIVER_XKB_CONFIG_V1_KEYMAP_FORMAT_TEXT_V1))
  "Create a keymap object on PROXY-CONFIG from string KEYMAP-STR.
Allocates a sealed anonymous memory file descriptor (memfd) and sends create_keymap request.
Parameters: (PROXY-CONFIG KEYMAP-STR [FORMAT])
  - PROXY-CONFIG: Pointer to river_xkb_config_v1 proxy.
  - KEYMAP-STR: String containing compiled XKB keymap definition.
  - FORMAT: Keymap format enum (default text_v1).
Returns river_xkb_keymap_v1 proxy pointer or #f on failure."
  (when (and proxy-config keymap-str)
    (let* ((bv (string->utf8 keymap-str))
           (bv-len (bytevector-length bv))
           (size (+ bv-len 1))
           (fd (memfd-create "river-xkb-keymap" (logior *mfd-cloexec* *mfd-allow-sealing*))))
      (if (< fd 0)
          (begin
            (log-error "xkb-config: failed to create memfd for keymap")
            #f)
          (catch #t
            (lambda ()
              (ftruncate-fd fd size)
              (let ((ptr (mmap-memory size #:prot *prot-read-write* #:flags *map-shared* #:fd fd)))
                (when (valid-pointer? ptr)
                  (let ((dest-bv (pointer->bytevector ptr size)))
                    (bytevector-copy! bv 0 dest-bv 0 bv-len)
                    (bytevector-u8-set! dest-bv bv-len 0))
                  (munmap-memory ptr size)))
              (memfd-seal-all! fd)
              (let ((keymap (xkb-config-create-keymap proxy-config fd format)))
                (close-fd fd)
                keymap))
            (lambda (k . args)
              (log-error "xkb-config: error creating keymap memfd: ~a ~a" k args)
              (close-fd fd)
              #f))))))

(define* (xkb-config-create-keymap-from-layout proxy-config layouts variants
											   #:optional (format RIVER_XKB_CONFIG_V1_KEYMAP_FORMAT_TEXT_V1))
  "Create a keymap object on PROXY-CONFIG from layer LAYOUTS.
Allocates a sealed anonymous memory file descriptor (memfd) and sends create_keymap request.
Parameters: (PROXY-CONFIG KEYMAP-LAYOUT KEYMAP-VARIANT [FORMAT])
  - PROXY-CONFIG: Pointer to river_xkb_config_v1 proxy.
  - LAYOUTS: List of layouts (e.g. '(\"us\" \"ara\")).
  - VARIANTS: List of variants (e.g. '(\"\" \"digits\")) - can be left empty.
  - FORMAT: Keymap format enum (default text_v1).
Returns river_xkb_keymap_v1 proxy pointer or #f on failure."
  ;; layout and variant need to be in comma separated format (e.g. "en, ara")
  (and-let* ((layout-str   (string-join layouts ","))
             (variant-str  (string-join variants ","))
			 ;; create keymap-def from layout and variant
			 (keymap-def   (xkb-config-compile-keymap layout-str variant-str)))
	;; pass the keymap-def string to xkb-config-create-keymap-from-string
	(xkb-config-create-keymap-from-string proxy-config keymap-def format)))

;;; river_xkb_config_v1 events
(define (xkb-config-on-finished data proxy-config)
  "Invoked when river compositor indicates that it will send no further events
on river_xkb_config_v1. The client should destroy the object.
Parameters: (DATA PROXY-CONFIG)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-CONFIG: Pointer to river_xkb_config_v1 proxy."
  (when *log-river-calls*
	(log-debug "river_xkb_config_v1 ~a finished" proxy-config))
  (gliver-hook-run! %xkb-config-destroyed-hook data proxy-config)
  (xkb-config-destroy proxy-config))

(define (xkb-config-on-keyboard data proxy-config proxy-keyboard)
  "Invoked when a new river_xkb_keyboard_v1 is advertised.
Parameters: (DATA PROXY-CONFIG PROXY-KEYBOARD)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-CONFIG: Pointer to river_xkb_config_v1 proxy.
  - PROXY-KEYBOARD: Pointer to river_xkb_keyboard_v1 proxy."
  (when *log-river-calls*
	(log-debug "New xkb keyboard received: ~a" proxy-keyboard))
  ;; attach shared keyboard listener to receive the keyboard events
  (wl-proxy-add-listener proxy-keyboard *xkb-config-keyboard-listener* %null-pointer)
  ;; track the newly created keyboard proxy
  (set! *xkb-config-keyboards* (cons proxy-keyboard *xkb-config-keyboards*))
  (gliver-hook-run! %xkb-config-keyboard-created-hook
					data proxy-config proxy-keyboard))

;;; river_xkb_keymap_v1 requests
(define (xkb-keymap-destroy proxy-keymap)
  "Destroy river_xkb_keymap_v1 proxy object, this means everything was
cleared from client side and the server should also clear everything.
Parameters: (PROXY-KEYMAP)
  - PROXY-KEYMAP: Pointer to river_xkb_keymap_v1 proxy."
  (when proxy-keymap
    (when *log-river-calls*
	  (log-debug "Destroying xkb keymap proxy: ~a" proxy-keymap))
    (river-xkb-keymap-v1-destroy proxy-keymap)
	(set! *xkb-config-keymaps*
		  (delete proxy-keymap *xkb-config-keymaps*))))

;;; river_xkb_keymap_v1 events
(define (xkb-keymap-on-success data proxy-keymap)
  "Invoked when keymap creation succeeded. The keymap may now be used
with river_xkb_keyboard_v1.set_keymap.
Parameters: (DATA PROXY-KEYMAP)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-KEYMAP: Pointer to river_xkb_keymap_v1 proxy."
  (when *log-river-calls*
	(log-debug "Keymap creation succeeded for ~a" proxy-keymap))
  (gliver-hook-run! %xkb-config-keymap-succeeded-hook data proxy-keymap))

(define (xkb-keymap-on-failure data proxy-keymap error-msg-ptr)
  "Invoked when compositor failed to create keymap from parameters.
Parameters: (DATA PROXY-KEYMAP ERROR-MSG-PTR)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-KEYMAP: Pointer to river_xkb_keymap_v1 proxy.
  - ERROR-MSG-PTR: Pointer to error message string."
  (let ((msg (if (valid-pointer? error-msg-ptr)
				 (pointer->string error-msg-ptr)
				 "unknown error")))
	(log-error "Keymap creation failed for ~a: ~a" proxy-keymap msg)
	(gliver-hook-run! %xkb-config-keymap-failed-hook data proxy-keymap msg)
	(xkb-keymap-destroy proxy-keymap)))

;;; river_xkb_keyboard_v1 requests
(define (xkb-keyboard-destroy proxy-keyboard)
  "Destroy river_xkb_keyboard_v1 proxy object, this means everything was
cleared from client side and the server should also clear everything.
Parameters: (PROXY-KEYBOARD)
  - PROXY-KEYBOARD: Pointer to river_xkb_keyboard_v1 proxy."
  (when proxy-keyboard
    (when *log-river-calls*
	  (log-debug "Destroying xkb keyboard proxy: ~a" proxy-keyboard))
    (river-xkb-keyboard-v1-destroy proxy-keyboard)
	(set! *xkb-config-keyboards*
		  (delete proxy-keyboard *xkb-config-keyboards*))))

(define (xkb-keyboard-set-keymap proxy-keyboard proxy-keymap)
  "Set keymap for the keyboard. It is a protocol error to pass a keymap
for which success event was not received.
Parameters: (PROXY-KEYBOARD PROXY-KEYMAP)
  - PROXY-KEYBOARD: Pointer to river_xkb_keyboard_v1 proxy.
  - PROXY-KEYMAP: Pointer to river_xkb_keymap_v1 proxy."
  (when (and proxy-keyboard proxy-keymap)
    (when *log-river-calls*
	  (log-debug "Setting keymap on keyboard ~a to ~a"
				 proxy-keyboard proxy-keymap))
    (river-xkb-keyboard-v1-set-keymap proxy-keyboard proxy-keymap)))

(define (xkb-keyboard-set-layout-by-index proxy-keyboard index)
  "Set active layout for keyboard by index.
Has no effect if index is out of bounds for the current keymap.
Parameters: (PROXY-KEYBOARD INDEX)
  - PROXY-KEYBOARD: Pointer to river_xkb_keyboard_v1 proxy.
  - INDEX: Numeric 0-based layout index (integer)."
  (when (and proxy-keyboard index)
    (when *log-river-calls*
	  (log-debug "Setting keyboard ~a layout by index: ~a"
				 proxy-keyboard index))
    (river-xkb-keyboard-v1-set-layout-by-index proxy-keyboard index)))

(define (xkb-keyboard-set-layout-by-name proxy-keyboard name)
  "Set active layout for keyboard by name.
Has no effect if there is no layout with given name in the keymap.
Parameters: (PROXY-KEYBOARD NAME)
  - PROXY-KEYBOARD: Pointer to river_xkb_keyboard_v1 proxy.
  - NAME: String layout name."
  (when (and proxy-keyboard name)
    (when *log-river-calls*
	  (log-debug "Setting keyboard ~a layout by name: ~a"
				 proxy-keyboard name))
    (river-xkb-keyboard-v1-set-layout-by-name proxy-keyboard name)))

(define (xkb-keyboard-capslock-enable proxy-keyboard)
  "Enable capslock for the keyboard.
Parameters: (PROXY-KEYBOARD)
  - PROXY-KEYBOARD: Pointer to river_xkb_keyboard_v1 proxy."
  (when proxy-keyboard
    (when *log-river-calls*
	  (log-debug "Enabling capslock on keyboard ~a" proxy-keyboard))
    (river-xkb-keyboard-v1-capslock-enable proxy-keyboard)))

(define (xkb-keyboard-capslock-disable proxy-keyboard)
  "Disable capslock for the keyboard.
Parameters: (PROXY-KEYBOARD)
  - PROXY-KEYBOARD: Pointer to river_xkb_keyboard_v1 proxy."
  (when proxy-keyboard
    (when *log-river-calls*
	  (log-debug "Disabling capslock on keyboard ~a" proxy-keyboard))
    (river-xkb-keyboard-v1-capslock-disable proxy-keyboard)))

(define (xkb-keyboard-numlock-enable proxy-keyboard)
  "Enable numlock for the keyboard.
Parameters: (PROXY-KEYBOARD)
  - PROXY-KEYBOARD: Pointer to river_xkb_keyboard_v1 proxy."
  (when proxy-keyboard
    (when *log-river-calls*
	  (log-debug "Enabling numlock on keyboard ~a" proxy-keyboard))
    (river-xkb-keyboard-v1-numlock-enable proxy-keyboard)))

(define (xkb-keyboard-numlock-disable proxy-keyboard)
  "Disable numlock for the keyboard.
Parameters: (PROXY-KEYBOARD)
  - PROXY-KEYBOARD: Pointer to river_xkb_keyboard_v1 proxy."
  (when proxy-keyboard
    (when *log-river-calls*
	  (log-debug "Disabling numlock on keyboard ~a" proxy-keyboard))
    (river-xkb-keyboard-v1-numlock-disable proxy-keyboard)))

;;; river_xkb_keyboard_v1 events
(define (xkb-keyboard-on-removed data proxy-keyboard)
  "Invoked when river_xkb_keyboard_v1 is removed from compositor.
Parameters: (DATA PROXY-KEYBOARD)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-KEYBOARD: Pointer to river_xkb_keyboard_v1 proxy."
  (when *log-river-calls*
	(log-debug "xkb keyboard removed: ~a" proxy-keyboard))
  (gliver-hook-run! %xkb-config-keyboard-removed data proxy-keyboard)
  (xkb-keyboard-destroy proxy-keyboard))

(define (xkb-keyboard-on-input-device data proxy-keyboard proxy-device)
  "Invoked when river_xkb_keyboard_v1 is associated with river_input_device_v1.
Sent once immediately after creation.
Parameters: (DATA PROXY-KEYBOARD PROXY-DEVICE)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-KEYBOARD: Pointer to river_xkb_keyboard_v1 proxy.
  - PROXY-DEVICE: Pointer to river_input_device_v1 proxy."
  (when *log-river-calls*
	(log-debug "xkb keyboard ~a associated with input device ~a"
			   proxy-keyboard proxy-device))
  (gliver-hook-run! %xkb-keyboard-input-device-hook
					data proxy-keyboard proxy-device))

(define (xkb-keyboard-on-layout data proxy-keyboard index name-ptr)
  "Invoked when active layout on keyboard changes.
Parameters: (DATA PROXY-KEYBOARD INDEX NAME-PTR)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-KEYBOARD: Pointer to river_xkb_keyboard_v1 proxy.
  - INDEX: Currently active layout index.
  - NAME-PTR: Pointer to layout name string (or null pointer)."
  (let ((name (if (valid-pointer? name-ptr)
				  (pointer->string name-ptr)
				  #f)))
	(when *log-river-calls*
	  (log-debug "xkb keyboard ~a layout changed: index=~a name=~a"
				 proxy-keyboard index name))
	(gliver-hook-run! %xkb-keyboard-layout-changed-hook
					  data proxy-keyboard index name)))

(define (xkb-keyboard-on-capslock-enabled data proxy-keyboard)
  "Invoked when capslock is enabled on keyboard.
Parameters: (DATA PROXY-KEYBOARD)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-KEYBOARD: Pointer to river_xkb_keyboard_v1 proxy."
  (when *log-river-calls*
	(log-debug "xkb keyboard ~a capslock enabled" proxy-keyboard))
  (gliver-hook-run! %xkb-keyboard-capslock-enabled-hook
					data proxy-keyboard))

(define (xkb-keyboard-on-capslock-disabled data proxy-keyboard)
  "Invoked when capslock is disabled on keyboard.
Parameters: (DATA PROXY-KEYBOARD)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-KEYBOARD: Pointer to river_xkb_keyboard_v1 proxy."
  (when *log-river-calls*
	(log-debug "xkb keyboard ~a capslock disabled" proxy-keyboard))
  (gliver-hook-run! %xkb-keyboard-capslock-disabled-hook
					data proxy-keyboard))

(define (xkb-keyboard-on-numlock-enabled data proxy-keyboard)
  "Invoked when numlock is enabled on keyboard.
Parameters: (DATA PROXY-KEYBOARD)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-KEYBOARD: Pointer to river_xkb_keyboard_v1 proxy."
  (when *log-river-calls*
	(log-debug "xkb keyboard ~a numlock enabled" proxy-keyboard))
  (gliver-hook-run! %xkb-keyboard-numlock-enabled-hook
					data proxy-keyboard))

(define (xkb-keyboard-on-numlock-disabled data proxy-keyboard)
  "Invoked when numlock is disabled on keyboard.
Parameters: (DATA PROXY-KEYBOARD)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-KEYBOARD: Pointer to river_xkb_keyboard_v1 proxy."
  (when *log-river-calls*
	(log-debug "xkb keyboard ~a numlock disabled" proxy-keyboard))
  (gliver-hook-run! %xkb-keyboard-numlock-disabled-hook
					data proxy-keyboard))

;; handle river initialization steps
(gliver-hook-add! *gliver-globals-bind-hook* 'xkb-config-on-globals-bind)
(gliver-hook-add! *gliver-globals-unbind-hook* 'xkb-config-on-globals-unbind)
(gliver-hook-add! *gliver-globals-verify-hook* 'xkb-config-on-globals-verify)
(gliver-hook-add! *gliver-listeners-attach-hook* 'xkb-config-on-listeners-attach)
