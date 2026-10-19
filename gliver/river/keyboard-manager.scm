;;; gliver/river/keyboard-manager.scm --- River keyboard and xkb layout integration
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module manages keyboard devices and xkb layouts in river.
;;; It handles:
;;;  - binding/unbinding river_xkb_config_v1 protocol global
;;;  - compiling xkb keymaps via libxkbcommon
;;;  - uploading keymaps to river compositor
;;;  - tracking connected physical keyboard devices
;;;  - switching active layouts
;;;  - configuring keyboard repeat rate and delay
;;;
;;; This module implements the river interfaces (protocol river_xkb_config_v1):
;;;  - river_xkb_config_v1
;;;  - river_xkb_keymap_v1
;;;  - river_xkb_keyboard_v1

(define-module (gliver river keyboard-manager)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-9)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (gliver core)
  #:use-module (gliver deps libc)
  #:use-module (gliver deps libxkbcommon)
  #:use-module (gliver wayland client)
  #:use-module (gliver wayland gen river-xkb-config-v1)
  #:use-module (gliver wayland gen river-input-management-v1)
  #:use-module (gliver river connector)
  #:declarative? #f
  #:export (
			*keyboard-layouts*
			*keyboard-variants*
			*keyboard-repeat-rate*
			*keyboard-repeat-delay*
			*keyboard-manager-layout-fallback*
			*keyboard-manager-xkb-config*
			*keyboard-manager-xkb-config-listener*
			*keyboard-manager-xkb-keyboard-listener*
			*keyboard-manager-xkb-keyboards*
			*keyboard-manager-keymap-proxy*
			*keyboard-manager-keymap-listener*
			*keyboard-manager-keymap-valid?*
			*keyboard-manager-current-layout-index*
			keyboard-manager-device-layout-index
			keyboard-manager-device-input-device
			keyboard-manager-device-listener
			keyboard-manager-device-proxy
			keyboard-manager-device-state?
			keyboard-manager-current-layout-code
			keyboard-manager-apply-config!
			keyboard-manager-layout-set!
			keyboard-manager-apply-current-keymap-to-keyboards!
			keyboard-manager-layout-next!
			keyboard-manager-layout-prev!
			keyboard-manager-on-keyboard-removed
			keyboard-manager-on-keyboard-input-device
			keyboard-manager-on-keyboard-layout
			keyboard-manager-on-keyboard-capslock-enabled
			keyboard-manager-on-keyboard-capslock-disabled
			keyboard-manager-on-keyboard-numlock-enabled
			keyboard-manager-on-keyboard-numlock-disabled
			keyboard-manager-on-xkb-config-finished
			keyboard-manager-on-xkb-config-keyboard
			keyboard-manager-on-keymap-success
			keyboard-manager-on-keymap-failure
			keyboard-manager-on-globals-bind
			keyboard-manager-on-globals-unbind
			keyboard-manager-on-globals-verify
			keyboard-manager-on-listeners-attach
			keyboard-manager-on-startup
			keyboard-manager-on-config-loaded
))

;;; user configuration
(define-var *keyboard-layouts* '("us")
			"Keyboard layouts, e.g. '(\"us\" \"ara\").")
(define-var *keyboard-variants* '()
			"Keyboard variants, e.g. '(\"\" \"digits\").")
(define-var *keyboard-repeat-rate* 25
			"Keyboard repeat rate in characters per second.")
(define-var *keyboard-repeat-delay* 600
			"Delay in milliseconds before key repeat starts.")
(define-var *keyboard-manager-layout-fallback* "us"
			"The fallback language to default to if no language is found.")

;;; global state
(define-var *keyboard-manager-xkb-config* %null-pointer
			"Proxy of river_xkb_config_v1 bound from the wayland registry.")
(define-var *keyboard-manager-xkb-config-listener* #f
			"Wayland listener for river_xkb_config_v1 global events.")
(define-var *keyboard-manager-xkb-keyboard-listener* #f
			"Wayland listener for river_xkb_config_v1 device events.")
(define-var *keyboard-manager-xkb-keyboards* '()
			"List of active <keyboard-manager-device-state> instances")
(define-var *keyboard-manager-keymap-proxy* %null-pointer
			"Active river_xkb_keymap_v1 proxy uploaded to river")
(define-var *keyboard-manager-keymap-listener* #f
			"Wayland listener for active keymap success/failure events")
(define-var *keyboard-manager-keymap-valid?* #f
			"Flag that indicates whether the current keymap is valid")
(define-var *keyboard-manager-current-layout-index* 0
			"Active layout index (from *keyboard-layouts*)")

;; per-keyboard state tracking record:
;;  - proxy: river_xkb_keyboard_v1 wayland proxy
;;  - listener: wayland listener assigned to this keyboard
;;  - input-device: associated river_input_device_v1 proxy (or #f)
;;  - current-layout-index: active layout index for this device
;; NOTE: it's possible to switch layout per device, this is currently
;; not implemented and switching layout will update all devices
(define-record-type <keyboard-manager-device-state>
  (%make-keyboard-manager-device-state proxy listener input-device current-layout-index)
  keyboard-manager-device-state?
  (proxy                 keyboard-manager-device-proxy)
  (listener              keyboard-manager-device-listener)
  (input-device          keyboard-manager-device-input-device        %keyboard-manager-device-input-device-set!)
  (current-layout-index  keyboard-manager-device-layout-index        %keyboard-manager-device-layout-index-set!))

(define (keyboard-manager-current-layout-code)
  "Return active layout code string based on *keyboard-manager-current-layout-index*.
Fallback to *keyboard-manager-layout-fallback* if layout not found."
  (if (and (>= *keyboard-manager-current-layout-index* 0)
           (< *keyboard-manager-current-layout-index* (length *keyboard-layouts*)))
      (list-ref *keyboard-layouts* *keyboard-manager-current-layout-index*)
	  *keyboard-manager-layout-fallback*))

(define (%list->comma-string items)
  "Convert list of strings to a comma-separated string."
  (string-join items ","))

(define (%valid-pointer? ptr)
  "Return #t if pointer PTR is valid and not null."
  (and ptr (not (null-pointer? ptr))))

;; NOTE: low-level function makes use of fd and bytevectors.
(define (%create-keymap-memfd keymap-def)
  "Create an anonymous sealed memfd containing the KEYMAP-DEF and a trailing null byte.
River protocol river_xkb_config_v1_create_keymap expects an fd representing the keymap.
The input keymap-def (keymap definition) is produced by xkb-keymap->string via libxkbcommon
Returns fd handle (integer)."
  (let* ((bv (string->utf8 keymap-def))
         (bv-len (bytevector-length bv))
         ;; extra byte for trailing null byte required by river
         (size (+ bv-len 1))
         ;; allocate anonymous file descriptor with cloexec and sealing enabled
         (fd (memfd-create "river-xkb-keymap" (logior *mfd-cloexec* *mfd-allow-sealing*))))
    (if (< fd 0)
        (begin
          (log-error "keyboard-manager: failed to create keymap-memfd for xkb")
          #f)
        (catch #t
          (lambda ()
            ;; truncate memfd to required size
            (ftruncate-fd fd size)
            ;; map shared memory to copy bytevector
            (let ((ptr (mmap-memory size #:prot *prot-read-write* #:flags *map-shared* #:fd fd)))
              (when (%valid-pointer? ptr)
                (let ((dest-bv (pointer->bytevector ptr size)))
                  ;; copy keymap bytes
                  (bytevector-copy! bv 0 dest-bv 0 bv-len)
                  ;; write trailing null byte
                  (bytevector-u8-set! dest-bv bv-len 0))
                ;; unmap memory after copying
                (munmap-memory ptr size)))
            ;; seal the memfd against any further modifications
			;; this fd shouldn't be altered as per river docs
            (memfd-seal-all! fd)
            fd)
          (lambda (key . args)
            (log-error "keyboard-manager: error writing keymap to memfd: ~a ~a" key args)
            ;; ensure file descriptor is closed on error
            (close-fd fd)
            #f)))))

(define (%safe-destroy-keymap proxy)
  "Destroy and cleanup keymap PROXY if it's valid."
  (when (%valid-pointer? proxy)
    (catch #t
      (lambda () (river-xkb-keymap-v1-destroy proxy))
      (lambda _ #f))))

;; NOTE: low-level function makes use of fd and bytevectors.
(define-command (keyboard-manager-apply-config!)
  "Compile xkb keymap in *keyboard-layouts* and upload it to river."

  (define (%compile-keymap-string layout-str variant-str)
	"Compile keymap definition from LAYOUT-STR and VARIANT-STR.
Returns a string representing the kepmap definition or #f on failure."
    (log-debug "keyboard-manager: compiling keymap (layout=~s, variant=~s)"
               layout-str variant-str)
    (call-with-values
        (lambda ()
		  ;; NOTE: rules and model can be optionally passed
		  ;; I am not sure if this will be of any use
		  ;; I also don't very well understand the meaning of these values
          (xkb-keymap-compile-from-names #:layout layout-str
                                         #:variant variant-str))
      (lambda (km ctx)
        (let ((keymap-def (and (%valid-pointer? km) (xkb-keymap->string km))))
		  ;; cleanup keymap and context as they are not needed anymore
          (when (%valid-pointer? km) (xkb-keymap-unref km))
          (when (%valid-pointer? ctx) (xkb-context-unref ctx))
          (cond
           ((not (%valid-pointer? km))
            (log-error "keyboard-manager: failed to compile keymap for layout ~s" layout-str)
            #f)
           ((not keymap-def)
            (log-error "keyboard-manager: failed to convert keymap to string")
            #f)
           (else keymap-def))))))

  (define (%upload-keymap-fd fd)
	"Upload file descriptor keymap to river"
    (let ((proxy (river-xkb-config-v1-create-keymap
                  *keyboard-manager-xkb-config* fd RIVER_XKB_CONFIG_V1_KEYMAP_FORMAT_TEXT_V1)))
      (when *wl-display* (wl-display-flush *wl-display*))
	  ;; close file descriptor as it should no longer be needed
      (close-fd fd)
      (if (%valid-pointer? proxy)
          proxy
          (begin
            (log-error "keyboard-manager: river_xkb_config_v1_create_keymap returned null proxy")
            #f))))

  ;; main function entry point
  (and-let* (((or (%valid-pointer? *keyboard-manager-xkb-config*)
                  (begin
                    (log-warn "keyboard-manager: river_xkb_config_v1 not available, skipping keymap update")
                    #f)))
             (layout-str   (%list->comma-string *keyboard-layouts*))
             (variant-str  (%list->comma-string *keyboard-variants*))
             (keymap-def   (%compile-keymap-string layout-str variant-str))
             (fd           (%create-keymap-memfd keymap-def))
			 ;; just to ensure we have a valid file descriptor
             ((or (and fd (>= fd 0))
                  (begin
                    (log-error "keyboard-manager: failed to create memfd for keymap")
                    #f)))
			 ;; upload keymap to river and get its proxy
             (keymap-proxy (%upload-keymap-fd fd)))

    (wl-proxy-add-listener keymap-proxy *keyboard-manager-keymap-listener* %null-pointer)
    (when *wl-display* (wl-display-flush *wl-display*))))

(define (%resolve-layout-index target)
  "Resolve TARGET (integer index or layout code string) to a 0-based layout index."
  (cond
   ;; integer: verify it's within range [0, length)
   ((and (integer? target) (>= target 0) (< target (length *keyboard-layouts*)))
    target)
   ;; string: find exact layout match (case-insensitive)
   ((string? target)
    (list-index (lambda (layout) (string-ci=? target layout))
                *keyboard-layouts*))
   (else #f)))

(define (keyboard-manager-layout-set! target)
  "Switch all keyboards to target (layout index or layout code)."
  (let ((idx (%resolve-layout-index target)))
    (if (not idx)
        (begin
          (log-warn "keyboard-manager: could not resolve layout ~s" target)
          #f)
        (begin
          ;; update current layout index
          (set! *keyboard-manager-current-layout-index* idx)
          (log-debug "keyboard-manager: switching to layout index ~a (~s)"
                    idx (keyboard-manager-current-layout-code))
          ;; change layout on each keyboard
          (for-each
           (lambda (dev)
             (let ((proxy (keyboard-manager-device-proxy dev)))
               (when (%valid-pointer? proxy)
                 (river-xkb-keyboard-v1-set-layout-by-index proxy idx))))
           *keyboard-manager-xkb-keyboards*)

          ;; notify listeners via hook
		  ;; NOTE: I was expecting keyboard-manager-on-keyboard-layout to be
		  ;; triggered by river, for some reason it doesn't, maybe a bug
          (gliver-hook-run! *keyboard-layout-changed-hook*
							idx (keyboard-manager-current-layout-code))
          #t))))

(define (keyboard-manager-apply-current-keymap-to-keyboards!)
  "Apply the active keymap proxy to all known keyboards."
  ;; verify that active keymap proxy is valid and non-null
  (when (and *keyboard-manager-keymap-valid?*
			 (%valid-pointer? *keyboard-manager-keymap-proxy*))
    ;; upload keymap to each connected keyboard device
    (for-each
     (lambda (dev)
       (let ((proxy (keyboard-manager-device-proxy dev)))
         (when (%valid-pointer? proxy)
           (log-debug "keyboard-manager: uploading keymap to keyboard ~a" proxy)
           ;; assign keymap proxy to keyboard
           (river-xkb-keyboard-v1-set-keymap proxy *keyboard-manager-keymap-proxy*))))
     *keyboard-manager-xkb-keyboards*)
	(keyboard-manager-layout-set! *keyboard-manager-current-layout-index*)))

(define (keyboard-manager-layout-next!)
  "Switch all keyboards to the next layout."
  (let ((count (length *keyboard-layouts*)))
    ;; skip if only 1 layout is configured
    (if (<= count 1)
        (begin
          (log-debug "keyboard-manager: only 1 layout configured, skipping next")
          #f)
        ;; calculate next layout index with wraparound for cycling behavior
        (let* ((target-idx (modulo (1+ *keyboard-manager-current-layout-index*)
								 count)))
          (keyboard-manager-layout-set! target-idx)))))

(define (keyboard-manager-layout-prev!)
  "Switch all keyboards to the previous layout."
  (let ((count (length *keyboard-layouts*)))
    ;; skip if only 1 layout is configured
    (if (<= count 1)
        (begin
          (log-debug "keyboard-manager: only 1 layout configured, skipping next")
          #f)
        ;; calculate previous layout index with wraparound for cycling behavior
        (let* ((target-idx (modulo (1- *keyboard-manager-current-layout-index*)
								 count)))
          (keyboard-manager-layout-set! target-idx)))))

;;; event handlers for river_xkb_keyboard_v1
(define (keyboard-manager-on-keyboard-removed data proxy)
  "Handle keyboard removal event from river."
  (log-debug "keyboard-manager: keyboard removed: ~a" proxy)
  ;; remove device from tracked keyboards list
  (set! *keyboard-manager-xkb-keyboards*
        (filter (lambda (k) (not (equal? (keyboard-manager-device-proxy k) proxy)))
                *keyboard-manager-xkb-keyboards*))
  ;; destroy river_xkb_keyboard_v1 proxy
  (river-xkb-keyboard-v1-destroy proxy))

(define (keyboard-manager-on-keyboard-input-device data proxy device-proxy)
  "Handle association event between an xkb keyboard and an input device."
  ;; NOTE: this is not to be mixed with keyboard-manager-on-xkb-config-keyboard
  ;; which is called when a new keyboard is discovered
  ;; input-device is the first event sent after discovery to configure a keyboard
  (log-debug "keyboard-manager: keyboard ~a associated with input device ~a" proxy device-proxy)
  (when (%valid-pointer? device-proxy)
	(let ((dev (find (lambda (k) (equal? (keyboard-manager-device-proxy k) proxy)) *keyboard-manager-xkb-keyboards*)))
      (when dev
		;; update input device proxy on matching tracked keyboard state
		(%keyboard-manager-device-input-device-set! dev device-proxy)))

    (catch #t
      (lambda ()
		;; configure keyboard repeat rate and delay on the input device if configured
        (river-input-device-v1-set-repeat-info
         device-proxy *keyboard-repeat-rate* *keyboard-repeat-delay*))
      (lambda (k . a)
        (log-debug "keyboard-manager: could not set repeat info on ~a: ~a ~a" device-proxy k a)))))

(define (keyboard-manager-on-keyboard-layout data proxy index name-ptr)
  "Handle layout change event reported by compositor for a keyboard."
  (log-debug "keyboard-manager: on-layout event: index=~a" index)
  ;; update layout index on the specific keyboard device state
  (let ((dev (find (lambda (k)
					 (equal? (keyboard-manager-device-proxy k) proxy))
				   *keyboard-manager-xkb-keyboards*)))

    (when dev
	  ;; update state layout index for the device
	  ;; currently not used actually
      (%keyboard-manager-device-layout-index-set! dev index)))
  ;; update global layout index and trigger hook if layout has changed
  (let ((changed? (not (= index *keyboard-manager-current-layout-index*))))
    (set! *keyboard-manager-current-layout-index* index)
    (when changed?
      (gliver-hook-run! *keyboard-layout-changed-hook* index (keyboard-manager-current-layout-code)))))

;; NOTE: nothing currently is needed for the below events
(define (keyboard-manager-on-keyboard-capslock-enabled data proxy)
  "Handle capslock enabled event for a keyboard."
  (log-debug "keyboard-manager: capslock enabled on ~a" proxy))

(define (keyboard-manager-on-keyboard-capslock-disabled data proxy)
  "Handle capslock disabled event for a keyboard."
  (log-debug "keyboard-manager: capslock disabled on ~a" proxy))

(define (keyboard-manager-on-keyboard-numlock-enabled data proxy)
  "Handle numlock enabled event for a keyboard."
  (log-debug "keyboard-manager: numlock enabled on ~a" proxy))

(define (keyboard-manager-on-keyboard-numlock-disabled data proxy)
  "Handle numlock disabled event for a keyboard."
  (log-debug "keyboard-manager: numlock disabled on ~a" proxy))

;;; event handlers for river_xkb_config_v1
(define (keyboard-manager-on-xkb-config-finished data proxy)
  "Handle finished event from river_xkb_config_v1 compositor object."
  ;; this is triggered by river after calling xkb-config-v1-stop
  (log-debug "keyboard-manager: river_xkb_config_v1 finished")
  ;; client should destroy xkb config at this point
  (river-xkb-config-v1-destroy proxy))

(define (keyboard-manager-on-xkb-config-keyboard data config-proxy kb-proxy)
  "Handle new keyboard event emitted by river_xkb_config_v1."
  ;; this event is triggered when a new keyboard is discovered/plugged
  (log-debug "keyboard-manager: new xkb keyboard detected: ~a" kb-proxy)
  ;; create state record for newly discovered keyboard device
  (let ((state (%make-keyboard-manager-device-state
				kb-proxy *keyboard-manager-xkb-keyboard-listener* #f 0)))

    ;; add discovered keyboard state to tracked keyboards list
    (set! *keyboard-manager-xkb-keyboards* (cons state *keyboard-manager-xkb-keyboards*))
    ;; attach shared keyboard listener to receive the keyboard events
    (when *keyboard-manager-xkb-keyboard-listener*
      (wl-proxy-add-listener kb-proxy *keyboard-manager-xkb-keyboard-listener* %null-pointer))

    ;; if a valid keymap was already compiled and approved, set it immediately
    (when (and *keyboard-manager-keymap-valid?*
			   (%valid-pointer? *keyboard-manager-keymap-proxy*))
      (river-xkb-keyboard-v1-set-keymap kb-proxy *keyboard-manager-keymap-proxy*)
      ;; also apply current layout index if valid
      (when (%resolve-layout-index *keyboard-manager-current-layout-index*)
        (river-xkb-keyboard-v1-set-layout-by-index kb-proxy *keyboard-manager-current-layout-index*)))))

;;; event handlers for river_xkb_keymap_v1
(define (keyboard-manager-on-keymap-success data proxy)
  "Handle successful keymap upload event."
  (log-debug "keyboard-manager: keymap uploaded successfully (layouts: ~a)"
            *keyboard-layouts*)
  ;; set the keymap status to valid
  (set! *keyboard-manager-keymap-valid?* #t)

  ;; update the current keymap proxy and delete old one
  (let ((old-proxy *keyboard-manager-keymap-proxy*))
    (set! *keyboard-manager-keymap-proxy* proxy)
    (unless (equal? old-proxy proxy)
      (%safe-destroy-keymap old-proxy)))

  ;; apply the new keymap to all known keyboards
  (keyboard-manager-apply-current-keymap-to-keyboards!))

(define (keyboard-manager-on-keymap-failure data proxy error-msg-ptr)
  "Handle keymap upload failure event."
  ;; set the keymap status to invalid
  (set! *keyboard-manager-keymap-valid?* #f)

  ;; display the error message
  (let ((msg (if (%valid-pointer? error-msg-ptr)
                 (pointer->string error-msg-ptr)
                 "unknown error")))
    (log-error "keyboard-manager: compositor failed to create keymap: ~a" msg)
    (%safe-destroy-keymap proxy)))

;;; hook handlers
(define (keyboard-manager-on-globals-bind registry protocol-name object-id version)
  "Bind river_xkb_config_v1 global interface."
  (when (string=? protocol-name RIVER_XKB_CONFIG_V1_NAME)
    (log-debug "keyboard-manager: binding ~a..." protocol-name)
    ;; bind protocol global interface to receive keyboard configuration events
    (set! *keyboard-manager-xkb-config*
          (gliver-wl-registry-bind registry object-id
                                    *river-xkb-config-v1-interface*
                                    (min version 1)))))

(define (keyboard-manager-on-globals-unbind)
  "Clean up all keyboard proxies, keymap, and config global on disconnect."
  ;; destroy individual keyboard device proxies
  (for-each
   (lambda (k)
     (let ((proxy (keyboard-manager-device-proxy k)))
       (when (%valid-pointer? proxy)
         (catch #t
           (lambda () (river-xkb-keyboard-v1-destroy proxy))
           (lambda _ #f)))))
   *keyboard-manager-xkb-keyboards*)
  (set! *keyboard-manager-xkb-keyboards* '())

  ;; destroy current keymap proxy
  (%safe-destroy-keymap *keyboard-manager-keymap-proxy*)
  (set! *keyboard-manager-keymap-proxy* %null-pointer)
  (set! *keyboard-manager-keymap-valid?* #f)

  ;; destroy river_xkb_config_v1 global proxy
  (when (%valid-pointer? *keyboard-manager-xkb-config*)
    (catch #t
      (lambda () (river-xkb-config-v1-destroy *keyboard-manager-xkb-config*))
      (lambda _ #f))
    (set! *keyboard-manager-xkb-config* %null-pointer)))

(define (keyboard-manager-on-globals-verify)
  "Verify river_xkb_config_v1 global binding."
  ;; warn if the protocol is not advertised by the compositor
  ;; usually this should be a mandatory protocol and not finding it
  ;; should actually raise an error
  (when (not (%valid-pointer? *keyboard-manager-xkb-config*))
    (log-warn "river_xkb_config_v1 not available; keyboard layout management disabled")))

(define (keyboard-manager-on-listeners-attach)
  "Create and attach listeners for river_xkb_config_v1, river_xkb_keyboard_v1, and river_xkb_keymap_v1."
  ;; create listener for active keymap success/failure events
  ;; this will be attached every time a keymap is created later
  (set! *keyboard-manager-keymap-listener*
        (make-river-xkb-keymap-v1-listener
         keyboard-manager-on-keymap-success
         keyboard-manager-on-keymap-failure))

  ;; create listener for individual keyboard device events
  ;; this will be attached every time a keyboard is discovered
  (set! *keyboard-manager-xkb-keyboard-listener*
        (make-river-xkb-keyboard-v1-listener
         keyboard-manager-on-keyboard-removed
         keyboard-manager-on-keyboard-input-device
         keyboard-manager-on-keyboard-layout
         keyboard-manager-on-keyboard-capslock-enabled
         keyboard-manager-on-keyboard-capslock-disabled
         keyboard-manager-on-keyboard-numlock-enabled
         keyboard-manager-on-keyboard-numlock-disabled))

  ;; create listener for xkb config events (discovery of keyboards)
  (set! *keyboard-manager-xkb-config-listener*
        (make-river-xkb-config-v1-listener
         keyboard-manager-on-xkb-config-finished
         keyboard-manager-on-xkb-config-keyboard))
  ;; attach listener to river_xkb_config_v1 global proxy
  (when (%valid-pointer? *keyboard-manager-xkb-config*)
    (wl-proxy-add-listener *keyboard-manager-xkb-config* *keyboard-manager-xkb-config-listener* %null-pointer)))

(define (keyboard-manager-on-startup)
  "Apply keyboard configuration after initial startup."
  (keyboard-manager-apply-config!))

(define (keyboard-manager-on-config-loaded)
  "Re-apply keyboard configuration when config is reloaded."
  (keyboard-manager-apply-config!))

;; attach handlers to gliver lifecycle hooks
(gliver-hook-add! *gliver-globals-bind-hook* 'keyboard-manager-on-globals-bind)
(gliver-hook-add! *gliver-globals-unbind-hook* 'keyboard-manager-on-globals-unbind)
(gliver-hook-add! *gliver-globals-verify-hook* 'keyboard-manager-on-globals-verify)
(gliver-hook-add! *gliver-listeners-attach-hook* 'keyboard-manager-on-listeners-attach)
(gliver-hook-add! *startup-hook* 'keyboard-manager-on-startup)
(gliver-hook-add! *config-loaded-hook* 'keyboard-manager-on-config-loaded)
