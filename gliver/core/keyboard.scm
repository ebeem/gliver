;;; gliver/core/keyboard.scm --- Keyboard keymap & layout management
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module manages keyboard devices, xkb keymaps, and active layouts in Gliver.
;;; It handles:
;;;   - Compiling and uploading xkb keymaps to the river compositor
;;;   - Tracking connected physical keyboard devices
;;;   - Switching active layouts (next, previous, by index, or by code)
;;;   - Configuring keyboard repeat rate and delay on input devices

(define-module (gliver core keyboard)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (system foreign)
  #:use-module (gliver core-base)
  #:use-module (gliver river connector)
  #:use-module (gliver river xkb-config-manager)
  #:use-module (gliver river input-manager)
  #:use-module (gliver wayland client)
  #:declarative? #f
  #:export (
			*keyboard-layouts*
			*keyboard-layout-fallback*
			*keyboard-variants*
			*keyboard-repeat-rate*
			*keyboard-repeat-delay*
			*keyboard-xkb-keyboards*
			*keyboard-current-layout-index*
			*keyboard-keymap-proxy*
			*keyboard-keymap-valid?*
			keyboard-device-layout-index-set!
			keyboard-device-layout-index
			keyboard-device-input-device-set!
			keyboard-device-input-device
			keyboard-device-listener
			keyboard-device-proxy
			keyboard-device-state?
			make-keyboard-device-state
			keyboard-current-layout-code
			keyboard-apply-current-keymap-to-keyboards!
			keyboard-layout-set!
			keyboard-apply-config!
			keyboard-layout-next!
			keyboard-layout-prev!
			keyboard-on-created
			keyboard-on-removed
			keyboard-on-input-device
			keyboard-on-layout
			keyboard-on-keymap-success
			keyboard-on-keymap-failure
			keyboard-on-capslock-enabled
			keyboard-on-capslock-disabled
			keyboard-on-numlock-enabled
			keyboard-on-numlock-disabled
			keyboard-on-startup
			keyboard-on-config-loaded
))

;;; user configuration
(define-var *keyboard-layouts* '("us")
			"Keyboard layouts list, e.g. '(\"us\" \"ara\").")
(define-var *keyboard-layout-fallback* "us"
			"Fallback layout code to default to if no active layout is resolved.")
(define-var *keyboard-variants* '()
			"Keyboard variants list corresponding to *keyboard-layouts*, e.g. '(\"\" \"digits\").")
(define-var *keyboard-repeat-rate* 25
			"Keyboard repeat rate in characters per second.")
(define-var *keyboard-repeat-delay* 600
			"Delay in milliseconds before key repeat begins.")

;;; global state
(define-var *keyboard-xkb-keyboards* '()
			"List of active <keyboard-device-state> instances.")
(define-var *keyboard-current-layout-index* 0
			"Active 0-based layout index into *keyboard-layouts*.")
(define-var *keyboard-keymap-proxy* %null-pointer
			"Active river_xkb_keymap_v1 proxy uploaded to river.")
(define-var *keyboard-keymap-valid?* #f
			"Flag indicating whether the current keymap was validated by compositor.")

;; Per-keyboard state tracking record:
;;  - proxy: river_xkb_keyboard_v1 Wayland proxy
;;  - listener: Wayland listener assigned to this keyboard
;;  - input-device: associated river_input_device_v1 proxy (or #f)
;;  - current-layout-index: active layout index for this physical device
(define-record-type <keyboard-device-state>
  (make-keyboard-device-state proxy listener input-device current-layout-index)
  keyboard-device-state?
  (proxy                 keyboard-device-proxy)
  (listener              keyboard-device-listener)
  (input-device          keyboard-device-input-device        keyboard-device-input-device-set!)
  (current-layout-index  keyboard-device-layout-index        keyboard-device-layout-index-set!))

;;; internal helpers
(define (%resolve-layout-index target)
  "Resolve TARGET to a valid 0-based layout index.
Parameters: (TARGET)
  - TARGET: Integer 0-based index or layout code string.
Returns: Valid integer layout index, or #f if not found."
  (cond
   ;; target is valid integer within range
   ((and (integer? target)
		 (>= target 0)
		 (< target (length *keyboard-layouts*)))
    target)
   ;; target is valid layout code that exists in layouts
   ((string? target)
    (list-index (lambda (layout)
				  (string-ci=? target layout))
                *keyboard-layouts*))
   (else #f)))

(define (%keyboard-layout-cycle! step)
  "Cycle the active layout index by STEP with wraparound.
Parameters: (STEP)
  - STEP: Integer offset (+1 for next, -1 for previous).
Returns: #t on successful switch, or #f if only single layout is configured."
  (let ((count (length *keyboard-layouts*))
		(target-index (+ *keyboard-current-layout-index* step)))
    (unless (<= count 1)
      (keyboard-layout-set! (modulo target-index count)))))

(define (keyboard-current-layout-code)
  "Return the active layout code string."
  (if (and (>= *keyboard-current-layout-index* 0)
           (< *keyboard-current-layout-index* (length *keyboard-layouts*)))
      (list-ref *keyboard-layouts* *keyboard-current-layout-index*)
      *keyboard-layout-fallback*))

(define (keyboard-apply-current-keymap-to-keyboards!)
  "Apply the active keymap proxy to all known connected keyboards."
  (when (and *keyboard-keymap-valid?*
             (valid-pointer? *keyboard-keymap-proxy*))
    ;; upload approved keymap to each tracked physical keyboard
	;; NOTE: currently gliver doesn't support individual keyboard keymap
    (for-each
     (lambda (dev)
       (let ((proxy (keyboard-device-proxy dev)))
         (when (valid-pointer? proxy)
           (log-debug "Uploading keymap to keyboard ~a" proxy)
           (xkb-keyboard-set-keymap proxy *keyboard-keymap-proxy*))))
     *keyboard-xkb-keyboards*)
    ;; re-apply active layout index to align keyboards
    (keyboard-layout-set! *keyboard-current-layout-index*)))

(define (keyboard-layout-set! target)
  "Switch all keyboards to TARGET layout.
Parameters: (TARGET)
  - TARGET: Integer 0-based layout index or layout code string.
Returns: #t on success, #f if TARGET cannot be resolved."
  (let ((idx (%resolve-layout-index target)))
    (if (not idx)
		;; couldn't resolve target
        #f
        ;; otherwise, set layout index on each available keyboard
        (begin
          (for-each
           (lambda (dev)
             (let ((proxy (keyboard-device-proxy dev)))
               (when (valid-pointer? proxy)
                 (xkb-keyboard-set-layout-by-index proxy idx))))
           *keyboard-xkb-keyboards*)
          #t))))

(define-command (keyboard-apply-config!)
  "Compile xkb keymap from *KEYBOARD-LAYOUTS*
and *KEYBOARD-VARIANTS* and upload to river.
Returns: river_xkb_keymap_v1 proxy pointer on success, or #f on failure."
  ;; this will automatically invoke keyboard-on-keymap-success if successful
  ;; which will take care of applying uploaded keymap to all keyboards
  (xkb-config-create-keymap-from-layout *xkb-config*
                                        *keyboard-layouts*
                                        *keyboard-variants*))

(define-command (keyboard-layout-next!)
  "Switch all keyboards to the next layout in *KEYBOARD-LAYOUTS*.
Returns: #t on successful switch, #f if only single layout configured."
  (%keyboard-layout-cycle! 1))

(define-command (keyboard-layout-prev!)
  "Switch all keyboards to the previous layout in *KEYBOARD-LAYOUTS*.
Returns: #t on successful switch, #f if only single layout configured."
  (%keyboard-layout-cycle! -1))

;;; event handlers for river xkb events
(define (keyboard-on-created data proxy-config proxy-keyboard)
  "Invoked when a new river_xkb_keyboard_v1 is advertised by compositor.
Parameters: (DATA PROXY-CONFIG PROXY-KEYBOARD)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-CONFIG: Pointer to river_xkb_config_v1 proxy.
  - PROXY-KEYBOARD: Pointer to river_xkb_keyboard_v1 proxy."
  (when (valid-pointer? proxy-keyboard)
    (log-debug "New xkb keyboard detected: ~a" proxy-keyboard)
    (let ((state (make-keyboard-device-state proxy-keyboard #f #f *keyboard-current-layout-index*)))
      (set! *keyboard-xkb-keyboards* (cons state *keyboard-xkb-keyboards*))
      ;; If a valid keymap was already compiled and approved, set it immediately
      (when (and *keyboard-keymap-valid?*
                 (valid-pointer? *keyboard-keymap-proxy*))
        (xkb-keyboard-set-keymap proxy-keyboard *keyboard-keymap-proxy*)
        (when (%resolve-layout-index *keyboard-current-layout-index*)
          (xkb-keyboard-set-layout-by-index proxy-keyboard *keyboard-current-layout-index*))))))

(define (keyboard-on-removed data proxy-keyboard)
  "Invoked when river_xkb_keyboard_v1 is removed from compositor.
Parameters: (DATA PROXY)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY: Pointer to river_xkb_keyboard_v1 proxy."
  (log-debug "Keyboard removed: ~a" proxy-keyboard)
  (set! *keyboard-xkb-keyboards*
        (filter (lambda (dev)
                  (not (equal? (keyboard-device-proxy dev) proxy-keyboard)))
                *keyboard-xkb-keyboards*)))

(define (keyboard-on-input-device data proxy-keyboard proxy-device)
  "Invoked when river_xkb_keyboard_v1 is associated with river_input_device_v1.
Parameters: (DATA PROXY-KEYBOARD PROXY-DEVICE)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-KEYBOARD: Pointer to river_xkb_keyboard_v1 proxy.
  - PROXY-DEVICE: Pointer to river_input_device_v1 proxy."
  (log-debug "Keyboard ~a associated with input device ~a"
             proxy-keyboard proxy-device)
  (when (valid-pointer? proxy-device)
    ;; update input device for the keyboard
    (let ((dev (find (lambda (k)
                       (equal? (keyboard-device-proxy k)
							   proxy-keyboard))
                     *keyboard-xkb-keyboards*)))
      (when dev
        (keyboard-device-input-device-set! dev proxy-device)))
	;; set repeat rate and delay
    (input-device-set-repeat-info proxy-device
                                  *keyboard-repeat-rate*
                                  *keyboard-repeat-delay*)))

(define (keyboard-on-layout data proxy-keyboard index name)
  "Invoked when active layout on river_xkb_keyboard_v1 changes.
Parameters: (DATA PROXY-KEYBOARD INDEX NAME)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-KEYBOARD: Pointer to river_xkb_keyboard_v1 proxy.
  - INDEX: Numeric 0-based layout index.
  - NAME: String layout name or #f."
  (log-debug "On-layout event: index=~a name=~a" index name)
  (let ((dev (find (lambda (k)
                     (equal? (keyboard-device-proxy k) proxy-keyboard))
                   *keyboard-xkb-keyboards*)))
    (when dev
	  ;; update each device current layout index
      (keyboard-device-layout-index-set! dev index)))
  ;; sync global layout index and notify listeners if layout changed
  (unless (= index *keyboard-current-layout-index*)
    (set! *keyboard-current-layout-index* index)
    (gliver-hook-run! *keyboard-layout-changed-hook*
                      index (keyboard-current-layout-code))))

(define (keyboard-on-keymap-success data proxy-keymap)
  "Invoked when created river_xkb_keymap_v1 via river_xkb_config_v1 succeeded.
It's possible after the event to set this keymap to a keyboard.
Parameters: (DATA PROXY)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY: Pointer to river_xkb_keymap_v1 proxy."
  (log-info "Keymap uploaded successfully (layouts: ~a)" *keyboard-layouts*)
  (set! *keyboard-keymap-valid?* #t)
  (let ((old-proxy *keyboard-keymap-proxy*))
    (set! *keyboard-keymap-proxy* proxy-keymap)
    ;; destroy the previous keymap proxy if replaced
    (when (and (valid-pointer? old-proxy)
               (not (equal? old-proxy proxy-keymap)))
      (xkb-keymap-destroy old-proxy)))
  (keyboard-apply-current-keymap-to-keyboards!))

(define (keyboard-on-keymap-failure data proxy-keymap error-msg)
  "Invoked when created river_xkb_keymap_v1 via river_xkb_config_v1 failed.
This may be error in configuration like incorrect language.
Parameters: (DATA PROXY ERROR-MSG)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY: Pointer to river_xkb_keymap_v1 proxy.
  - ERROR-MSG: String of the error message."
  (set! *keyboard-keymap-valid?* #f)
  (log-error "keyboard: compositor failed to create keymap: ~a" error-msg))

(define (keyboard-on-capslock-enabled data proxy-keyboard)
  "Invoked when capslock is enabled on river_xkb_keyboard_v1.
Parameters: (DATA PROXY-KEYBOARD)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-KEYBOARD: Pointer to river_xkb_keyboard_v1 proxy."
  (log-debug "Capslock enabled on ~a" proxy-keyboard))

(define (keyboard-on-capslock-disabled data proxy-keyboard)
  "Invoked when capslock is disabled on river_xkb_keyboard_v1.
Parameters: (DATA PROXY-KEYBOARD)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-KEYBOARD: Pointer to river_xkb_keyboard_v1 proxy."
  (log-debug "Capslock disabled on ~a" proxy-keyboard))

(define (keyboard-on-numlock-enabled data proxy-keyboard)
  "Invoked when numlock is enabled on river_xkb_keyboard_v1.
Parameters: (DATA PROXY-KEYBOARD)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-KEYBOARD: Pointer to river_xkb_keyboard_v1 proxy."
  (log-debug "Numlock enabled on ~a" proxy-keyboard))

(define (keyboard-on-numlock-disabled data proxy-keyboard)
  "Invoked when numlock is disabled on river_xkb_keyboard_v1.
Parameters: (DATA PROXY-KEYBOARD)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-KEYBOARD: Pointer to river_xkb_keyboard_v1 proxy."
  (log-debug "Numlock disabled on ~a" proxy-keyboard))

(define (keyboard-on-startup)
  "Apply keyboard configuration during startup. "
  (keyboard-apply-config!))

(define (keyboard-on-config-loaded)
  "Re-apply keyboard configuration when Gliver config is reloaded."
  (keyboard-apply-config!))

;;; hook registration
(gliver-hook-add! %xkb-config-keyboard-created-hook 'keyboard-on-created 0)
(gliver-hook-add! %xkb-config-keyboard-removed 'keyboard-on-removed 0)
(gliver-hook-add! %xkb-keyboard-input-device-hook 'keyboard-on-input-device 0)
(gliver-hook-add! %xkb-keyboard-layout-changed-hook 'keyboard-on-layout 0)
(gliver-hook-add! %xkb-keyboard-capslock-enabled-hook 'keyboard-on-capslock-enabled 0)
(gliver-hook-add! %xkb-keyboard-capslock-disabled-hook 'keyboard-on-capslock-disabled 0)
(gliver-hook-add! %xkb-keyboard-numlock-enabled-hook 'keyboard-on-numlock-enabled 0)
(gliver-hook-add! %xkb-keyboard-numlock-disabled-hook 'keyboard-on-numlock-disabled 0)
(gliver-hook-add! %xkb-config-keymap-succeeded-hook 'keyboard-on-keymap-success 0)
(gliver-hook-add! %xkb-config-keymap-failed-hook 'keyboard-on-keymap-failure 0)
(gliver-hook-add! *startup-hook* 'keyboard-on-startup 0)
(gliver-hook-add! *config-loaded-hook* 'keyboard-on-config-loaded 0)
