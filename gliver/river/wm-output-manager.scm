;;; gliver/river/wm-output-manager.scm --- River compositor integration
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; Files under the river directory should only act as a wrapper
;;; to river protocol, they should not take action nor import any of
;;; gliver's files or utilities except for logging and configuration

(define-module (gliver river wm-output-manager)
  #:use-module (gliver core types)
  #:use-module (gliver core hooks)
  #:use-module (gliver core logs)
  #:use-module (gliver wayland client)
  #:use-module (gliver wayland gen river-window-management-v1)
  #:use-module (system foreign)
  #:export (
			*wm-output-listener*
			wm-output-on-listeners-attach
			wm-output-destroy
			wm-output-presentation-mode-set
			wm-output-on-output
			wm-output-on-output-removed
			wm-output-on-output-wl-output
			wm-output-on-output-position
			wm-output-on-output-dimensions
))

(define *wm-output-listener* #f)

(define (wm-output-on-listeners-attach)
  "Attach wayland listeners, output-manager expects window-manager
to be properly initialized."
  (set! *wm-output-listener*
        (make-river-output-v1-listener
         wm-output-on-output-removed
         wm-output-on-output-wl-output
         wm-output-on-output-position
         wm-output-on-output-dimensions)))
(gliver-hook-add! *gliver-listeners-attach-hook* 'wm-output-on-listeners-attach 0)

(define (wm-output-on-output data manager output-proxy)
  "Handle a new output event from the compositor."
  (log-debug "New output pointer received: ~a" output-proxy)
  (when *wm-output-listener*
    (wl-proxy-add-listener output-proxy *wm-output-listener* %null-pointer)))
(gliver-hook-add! %output-created-hook 'wm-output-on-output 0)

;;; output requests
(define (wm-output-destroy proxy-output)
  "Destroy an output, this means everything was cleared from
client side and the server should also clear everything.
This most likely should be used internally only, and it
will be automatically managed and called when needed."
  (log-debug "destroying output proxy: ~a" proxy-output)
  (when proxy-output 
    (river-output-v1-destroy proxy-output)))

(define (wm-output-presentation-mode-set proxy-output mode)
  "Set the preferred presentation mode of the output.
mode: enum value `RIVER_OUTPUT_V1_PRESENTATION_MODE_VSYNC`,
`RIVER_OUTPUT_V1_PRESENTATION_MODE_ASYNC`."
  (when proxy-output
    (log-debug "Setting output ~a presentation mode to ~a" proxy-output mode)
    (river-output-v1-set-presentation-mode proxy-output mode)))

;;; output events
(define (wm-output-on-output-removed data proxy-output)
  "Output was removed. This will take care of
Removing the output record and clearing up memory.
Hook: *output-destroy-hook*"
  (log-debug "Output removed: ~a" proxy-output)
  (gliver-hook-run! %output-removed-hook proxy-output))

(define (wm-output-on-output-wl-output data proxy-output object-id)
  "The wl_output object corresponding to the river_output_v1."
  (log-debug "Output wl_output object-id: ~a = ~a" proxy-output object-id)
  (gliver-hook-run! %output-object-id-changed-hook data proxy-output object-id))

(define (wm-output-on-output-position data proxy-output x y)
  "Position of the output in the compositor's logical coordinate
space changed. The x and y coordinates may be positive or negative."
  (log-debug "Output position: ~a = ~a,~a" proxy-output x y)
  (gliver-hook-run! %output-position-changed-hook data proxy-output x y))

(define (wm-output-on-output-dimensions data proxy-output width height)
  (log-debug "Output dimensions: ~a = ~ax~a" proxy-output width height)
  (gliver-hook-run! %output-dimensions-changed-hook data proxy-output width height))

