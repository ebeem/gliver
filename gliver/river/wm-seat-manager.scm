;;; gliver/river/wm-seat-manager.scm --- River window management seat integration
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module manages the seat life-cycle after its creation
;;; Wayland seats are input devices, the most common examples are
;;; keyboards and pointer/mouse.
;;;
;;; It handles:
;;;   - Binding river_seat_v1 and exposing interface requests and events
;;;   - Managing seat window/shell focus to redirect input
;;;   - Setting pointer cursor theme
;;;   - Updating pointer position
;;;   - Handling pointer enter/leave/interaction window events
;;;   - Handling pointer resize/drag window events
;;;
;;; This module implements the river interfaces (protocol river_window_manager_v1):
;;;   - river_seat_v1
;;;
;;; NOTE: river_pointer_binding_v1 introduces more pointer requests/events
;;; NOTE: river_xkb_config_v1 & river_xkb_bindings_v1 introduce more keyboard requests/events

(define-module (gliver river wm-seat-manager)
  #:use-module (gliver core-base)
  #:use-module (gliver wayland client)
  #:use-module (gliver wayland gen river-window-management-v1)
  #:use-module (system foreign)
  #:export (
			*wm-seat-listener*
			wm-seat-on-listeners-attach
			wm-seat-on-seat
			wm-seat-destroy
			wm-seat-window-focus
			wm-seat-shell-focus
			wm-seat-window-focus-clear
			wm-seat-pointer-op-start
			wm-seat-pointer-op-end
			wm-seat-pointer-binding-get
			wm-seat-pointer-theme
			wm-seat-pointer-warp
			wm-seat-on-seat-removed
			wm-seat-on-seat-wl-seat
			wm-seat-on-seat-pointer-enter
			wm-seat-on-seat-pointer-leave
			wm-seat-on-seat-window-interaction
			wm-seat-on-seat-shell-interaction
			wm-seat-on-seat-op-delta
			wm-seat-on-seat-op-release
			wm-seat-on-seat-pointer-position
))

(define-var *wm-seat-listener* #f
			"Wayland listener for river_seat_v1 events.")

(define (wm-seat-on-listeners-attach)
  "Attach wayland listeners, seat-manager expects window-manager
to be properly initialized."
  (set! *wm-seat-listener*
        (make-river-seat-v1-listener
         wm-seat-on-seat-removed
         wm-seat-on-seat-wl-seat
         wm-seat-on-seat-pointer-enter
         wm-seat-on-seat-pointer-leave
         wm-seat-on-seat-window-interaction
         wm-seat-on-seat-shell-interaction
         wm-seat-on-seat-op-delta
         wm-seat-on-seat-op-release
         wm-seat-on-seat-pointer-position))
  (gliver-hook-add! %seat-created-hook 'wm-seat-on-seat -1))

(define (wm-seat-on-seat data manager seat-proxy)
  "Handle a new seat event from the compositor."
  (when *log-river-calls*
	(log-debug "New seat proxy: ~a" seat-proxy))
  (when *wm-seat-listener*
	;; add the new seat to the global seat listener
    (wl-proxy-add-listener seat-proxy *wm-seat-listener* %null-pointer)))

;;; seat requests
(define (wm-seat-destroy proxy-seat)
  "Destroy an seat, this means everything was cleared from
client side and the server should also clear everything.
This most likely should be used internally only, and it
will be automatically managed and called when needed."
  (when proxy-seat
    (when *log-river-calls*
	  (log-debug "destroying seat proxy: ~a" proxy-seat))
    (river-seat-v1-destroy proxy-seat)))

(define (wm-seat-window-focus proxy-seat proxy-window)
  "Request that the compositor send keyboard input to the given window.
Must be called in a ~manage_sequence~."
  (when (and proxy-seat proxy-window)
    (when *log-river-calls*
	  (log-debug "Seat ~a focusing window ~a" proxy-seat proxy-window))
    (river-seat-v1-focus-window proxy-seat proxy-window)))

(define (wm-seat-shell-focus proxy-seat proxy-surface)
  "Request that the compositor send keyboard input to the given shell surface.
Must be called in a ~manage_sequence~."
  (when (and proxy-seat proxy-surface)
    (when *log-river-calls*
	  (log-debug "Seat ~a focusing surface ~a" proxy-seat proxy-surface))
    (river-seat-v1-focus-shell-surface proxy-seat proxy-surface)))

(define (wm-seat-window-focus-clear proxy-seat)
  "Request that the compositor send keyboard input to the given shell surface.
Must be called in a ~manage_sequence~."
  (when proxy-seat
    (when *log-river-calls*
	  (log-debug "Seat ~a clearing focus" proxy-seat))
    (river-seat-v1-clear-focus proxy-seat)))

(define (wm-seat-pointer-op-start proxy-seat)
  "Start an interactive pointer operation.
Must be called in a ~manage_sequence~."
  (when proxy-seat
    (when *log-river-calls*
	  (log-debug "Seat ~a pointer operation started" proxy-seat))
    (river-seat-v1-op-start-pointer proxy-seat)))

(define (wm-seat-pointer-op-end proxy-seat)
  "End an interactive pointer operation.
Must be called in a ~manage_sequence~."
  (when proxy-seat
    (when *log-river-calls*
	  (log-debug "Seat ~a pointer operation ended" proxy-seat))
    (river-seat-v1-op-end proxy-seat)))

;; TODO: needs testing
(define (wm-seat-pointer-binding-get proxy-seat button modifiers)
  "Define a pointer binding in terms of a pointer button.
  - BUTTON: input event code like ~BTN_RIGHT~.
  - MODIFIERS: flag enums.
Returns river_pointer_binding_v1"
  (when proxy-seat
    (when *log-river-calls*
	  (log-debug "Getting binding for seat ~a with button ~a and modifiers ~a" proxy-seat button modifiers))
    (river-seat-v1-get-pointer-binding proxy-seat button modifiers)))

(define (wm-seat-pointer-theme proxy-seat name size)
  "Set the XCursor theme for the seat."
  (when proxy-seat
    (when *log-river-calls*
	  (log-debug "Seat ~a cursor theme changed to ~a size ~a" proxy-seat name size))
    (river-seat-v1-set-xcursor-theme proxy-seat name size)))

(define (wm-seat-pointer-warp proxy-seat x y)
  "Warp the pointer to the given position.
Must be called in a ~manage_sequence~."
  (when proxy-seat
    (when *log-river-calls*
	  (log-debug "Seat ~a pointer position moved to ~ax~a" proxy-seat x y))
    (river-seat-v1-pointer-warp proxy-seat x y)))

;;; seat events
(define (wm-seat-on-seat-removed data proxy-seat)
  "Seat was removed. This will take care of
The seat record should be cleared."
  (when *log-river-calls*
	(log-debug "Seat removed: ~a" proxy-seat))
  (gliver-hook-run! %seat-removed-hook data proxy-seat))

(define (wm-seat-on-seat-wl-seat data proxy-seat object-id)
  "The wl_seat object corresponding to the river_seat_v1."
  (when *log-river-calls*
	(log-debug "Seat wl_seat global object-id: ~a = ~a" proxy-seat object-id))
  (gliver-hook-run! %seat-object-id-changed-hook data proxy-seat object-id))

(define (wm-seat-on-seat-pointer-enter data proxy-seat proxy-window)
  "The seat's pointer entered the given window's area."
  (when *log-river-calls*
	(log-debug "Pointer entered window ~a" proxy-window))
  (gliver-hook-run! %seat-window-pointer-entered-hook data proxy-seat proxy-window))

(define (wm-seat-on-seat-pointer-leave data proxy-seat)
  "The seat's pointer left the recent window entered."
  (when *log-river-calls*
	(log-debug "Pointer left window"))
  (gliver-hook-run! %seat-window-pointer-left-hook data proxy-seat))

(define (wm-seat-on-seat-window-interaction data proxy-seat proxy-win)
  "Window is clicked or input is sent to it, focus it"
  (when *log-river-calls*
	(log-debug "seat ~a interacting with window ~a" proxy-seat proxy-win))
  (gliver-hook-run! %seat-window-interacted-hook data proxy-seat proxy-win))

(define (wm-seat-on-seat-shell-interaction data proxy-seat shell-proxy)
  "Surface is clicked or input is sent to it"
  (when *log-river-calls*
	(log-debug "Shell surface interaction: ~a" shell-proxy))
  (gliver-hook-run! %seat-shell-interacted-hook data proxy-seat shell-proxy))

(define (wm-seat-on-seat-op-delta data proxy-seat dx dy)
  "This event indicates the total change in position since the
start of the operation of the pointer/touch point/etc."
  (when *log-river-calls*
	(log-debug "Op delta: ~a,~a" dx dy))
  (gliver-hook-run! %seat-op-delta-changed-hook data proxy-seat dx dy))

(define (wm-seat-on-seat-op-release data proxy-seat)
  "The input driving the current interactive operation has been released."
  (when *log-river-calls*
	(log-debug "Op release"))
  (gliver-hook-run! %seat-op-released-hook data proxy-seat))

(define (wm-seat-on-seat-pointer-position data proxy-seat x y)
  "The current position of the pointer in the compositor's logical."
  (when *log-river-calls*
	(log-debug "Pointer position of ~a changed to: ~a,~a" proxy-seat x y))
  (gliver-hook-run! %seat-pointer-position-changed-hook data proxy-seat x y))

(gliver-hook-add! *gliver-listeners-attach-hook* 'wm-seat-on-listeners-attach)
