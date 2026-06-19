;;; gliver/river/wm-seat-manager.scm --- River compositor integration
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; Files under the river directory should only act as a wrapper
;;; to river protocol, they should not take action nor import any of
;;; gliver's files or utilities except for logging and configuration

(define-module (gliver river wm-seat-manager)
  #:use-module (gliver core types)
  #:use-module (gliver core logs)
  #:use-module (gliver core hooks)
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

(define *wm-seat-listener* #f)

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
  (log-debug "New seat proxy: ~a" seat-proxy)
  ;; attach the shared event listener
  (when *wm-seat-listener*
    (wl-proxy-add-listener seat-proxy *wm-seat-listener* %null-pointer)))

;;; seat requests
(define (wm-seat-destroy proxy-seat)
  "Destroy an seat, this means everything was cleared from
client side and the server should also clear everything.
This most likely should be used internally only, and it
will be automatically managed and called when needed."
  (when proxy-seat
    (log-debug "destroying seat proxy: ~a" proxy-seat)
    (river-seat-v1-destroy proxy-seat)))

(define (wm-seat-window-focus proxy-seat proxy-window)
  "Request that the compositor send keyboard input to the given window.
Must be called in a ~manage_sequence~."
  (when (and proxy-seat proxy-window)
    (log-debug "Seat ~a focusing window ~a" proxy-seat proxy-window)
    (river-seat-v1-focus-window proxy-seat proxy-window)))

(define (wm-seat-shell-focus proxy-seat proxy-surface)
  "Request that the compositor send keyboard input to the given shell surface.
Must be called in a ~manage_sequence~."
  (when (and proxy-seat proxy-surface)
    (log-debug "Seat ~a focusing surface ~a" proxy-seat proxy-surface)
    (river-seat-v1-focus-shell-surface proxy-seat proxy-surface)))

(define (wm-seat-window-focus-clear proxy-seat)
  "Request that the compositor send keyboard input to the given shell surface.
Must be called in a ~manage_sequence~."
  (when proxy-seat
    (log-debug "Seat ~a clearing focus" proxy-seat)
    (river-seat-v1-clear-focus proxy-seat)))

(define (wm-seat-pointer-op-start proxy-seat)
  "Start an interactive pointer operation.
Must be called in a ~manage_sequence~."
  (when proxy-seat
    (log-debug "Seat ~a pointer operation started" proxy-seat)
    (river-seat-v1-op-start-pointer proxy-seat)))

(define (wm-seat-pointer-op-end proxy-seat)
  "End an interactive pointer operation.
Must be called in a ~manage_sequence~."
  (when proxy-seat
    (log-debug "Seat ~a pointer operation ended" proxy-seat)
    (river-seat-v1-op-end proxy-seat)))

;; TODO: needs testing
(define (wm-seat-pointer-binding-get proxy-seat button modifiers)
  "Define a pointer binding in terms of a pointer button.
button: input event code like ~BTN_RIGHT~.
modifiers: flag enums.
Returns river_pointer_binding_v1"
  (when proxy-seat
    (log-debug "Getting binding for seat ~a with button ~a and modifiers ~a" proxy-seat button modifiers)
    (river-seat-v1-get-pointer-binding proxy-seat button modifiers)))

(define (wm-seat-pointer-theme proxy-seat name size)
  "Set the XCursor theme for the seat."
  (when proxy-seat
    (log-debug "Seat ~a cursor theme changed to ~a size ~a" proxy-seat name size)
    (river-seat-v1-set-xcursor-theme proxy-seat name size)))

(define (wm-seat-pointer-warp proxy-seat x y)
  "Warp the pointer to the given position.
Must be called in a ~manage_sequence~."
  (when proxy-seat
    (log-debug "Seat ~a pointer position moved to ~ax~a" proxy-seat x y)
    (river-seat-v1-pointer-warp proxy-seat x y)))

;;; seat events
(define (wm-seat-on-seat-removed data proxy-seat)
  "Seat was removed. This will take care of
The seat record should be cleared.
Hook: %seat-removed-hook"
  (log-debug "Seat removed: ~a" proxy-seat)
  (gliver-hook-run! %seat-removed-hook data proxy-seat))

(define (wm-seat-on-seat-wl-seat data proxy-seat object-id)
  "The wl_seat object corresponding to the river_seat_v1."
  (log-debug "Seat wl_seat global object-id: ~a = ~a" proxy-seat object-id)
  (gliver-hook-run! %seat-object-id-changed-hook data proxy-seat object-id))

(define (wm-seat-on-seat-pointer-enter data proxy-seat proxy-window)
  "The seat's pointer entered the given window's area."
  (log-debug "Pointer entered window ~a" proxy-window)
  (gliver-hook-run! %seat-window-pointer-entered-hook data proxy-seat proxy-window))

(define (wm-seat-on-seat-pointer-leave data proxy-seat)
  "The seat's pointer left the recent window entered."
  (log-debug "Pointer left window")
  (gliver-hook-run! %seat-window-pointer-left-hook data proxy-seat))

(define (wm-seat-on-seat-window-interaction data proxy-seat proxy-win)
  "Window is clicked or input is sent to it, focus it"
  (log-debug "seat ~a interacting with window ~a" proxy-seat proxy-win)
  (gliver-hook-run! %seat-window-interacted-hook data proxy-seat proxy-win))

(define (wm-seat-on-seat-shell-interaction data proxy-seat shell-proxy)
  "Surface is clicked or input is sent to it"
  (log-debug "Shell surface interaction: ~a" shell-proxy)
  (gliver-hook-run! %seat-shell-interacted-hook data proxy-seat shell-proxy))

(define (wm-seat-on-seat-op-delta data proxy-seat dx dy)
  "This event indicates the total change in position since the
start of the operation of the pointer/touch point/etc."
  (log-debug "Op delta: ~a,~a" dx dy)
  (gliver-hook-run! %seat-op-delta-changed-hook data proxy-seat dx dy))

(define (wm-seat-on-seat-op-release data proxy-seat)
  "The input driving the current interactive operation has been released."
  (log-debug "Op release")
  (gliver-hook-run! %seat-op-released-hook data proxy-seat))

(define (wm-seat-on-seat-pointer-position data proxy-seat x y)
  "The current position of the pointer in the compositor's logical."
  (log-debug "Pointer position of ~a changed to: ~a,~a" proxy-seat x y)
  (gliver-hook-run! %seat-pointer-position-changed-hook data proxy-seat x y))

(gliver-hook-add! *gliver-listeners-attach-hook* 'wm-seat-on-listeners-attach)

