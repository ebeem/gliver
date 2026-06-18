;;; gliver/core/seat.scm --- Core data model for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Author: ebeem <lord.ebeem@gmail.com>
;;; Maintainer: ebeem <lord.ebeem@gmail.com>
;;; Seat: A single seat bundles together the different ways
;;; a user can provide input e.g. (mouse, keyboard, touch input)

(define-module (gliver core seat)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (system foreign)
  #:use-module (gliver core config)
  #:use-module (gliver core logs)
  #:use-module (gliver core hooks)
  #:use-module (gliver core types)
  #:use-module (gliver river window-manager)
  #:use-module (gliver river wm-seat-manager)
  #:export (
			seat-add!
			seat-remove!
			seat-wm-window-focus
			seat-wm-window-focus-clear
			seat-wm-pointer-op-start
			seat-wm-pointer-op-end
			seat-wm-pointer-binding-get
			seat-wm-pointer-theme
			seat-wm-pointer-warp
			on-seat
			seat-on-removed
			seat-on-wl-seat
			seat-on-pointer-enter
			seat-on-pointer-leave
			seat-on-window-interaction
			seat-on-shell-interaction
			seat-on-op-delta
			seat-on-op-release
			seat-on-pointer-position
))

(define (seat-add! seat)
  (%manager-seats-set! *manager*
					  (cons seat (manager-seats *manager*)))
  (gliver-hook-run! *seat-created-hook* seat))

(define (seat-remove! seat)
  (let ((remaining (delete seat (manager-seats *manager*))))
    (%manager-seats-set! *manager* remaining))
  (gliver-hook-run! *seat-removed-hook* seat))

;;; window manager api calls
(define (seat-wm-window-focus seat window)
  "Request that the compositor send keyboard input to the given window.
Must be called in a ~manage_sequence~."
  (let ((proxy-seat (seat-wl-proxy seat))
		(proxy-window (window-wl-proxy window)))
    (when (and proxy-seat proxy-window)
	  (log-debug "Seat ~a focusing surface ~a" seat window)
	  (unless (eq? window (seat-window-focused seat))
		(%seat-window-focused-set! seat window)
		;; unfocus old window and focus the new one via hooks as well
		(with-manage-sequence	   
		 (wm-seat-window-focus proxy-seat proxy-window)	   
		 (gliver-hook-run! *seat-window-focused-hook* seat window))))))

;; NOTE: wm-seat-shell-focus should be implemented here
;; I am not sure if it's actually needed, so I skipped it

(define (seat-wm-window-focus-clear seat)
  "Request that the compositor send keyboard input to the given window.
Must be called in a ~manage_sequence~."
  (let ((proxy-seat (seat-wl-proxy seat)))
    (when proxy-seat
	  (with-manage-sequence
	   (wm-seat-window-focus-clear proxy-seat)
	   (%seat-window-focused-set! seat #f)
	   (gliver-hook-run! *seat-window-focused-hook* seat #f)))))

(define (seat-wm-pointer-op-start seat)
  "Start an interactive pointer operation.
Must be called in a ~manage_sequence~."
  (%seat-pointer-op-set! seat #t)
  (let ((proxy-seat (seat-wl-proxy seat)))
    (when proxy-seat
	  (with-manage-sequence
	   (wm-seat-pointer-op-start proxy-seat)))))

(define (seat-wm-pointer-op-end seat)
  "End an interactive pointer operation.
Must be called in a ~manage_sequence~."
  (%seat-pointer-op-set! seat #f)
  (let ((proxy-seat (seat-wl-proxy seat)))
    (when proxy-seat
	  (with-manage-sequence
	   (wm-seat-pointer-op-end proxy-seat)))))

(define (seat-wm-pointer-binding-get seat button modifiers)
  "Define a pointer binding in terms of a pointer button.
button: input event code like ~BTN_RIGHT~.
modifiers: flag enums.
Returns river_pointer_binding_v1"
  (let ((proxy-seat (seat-wl-proxy seat)))
    (when proxy-seat
	  (wm-seat-pointer-binding-get proxy-seat button modifiers))))

(define (seat-wm-pointer-theme seat name size)
  "Set the XCursor theme for the seat."
  (let ((proxy-seat (seat-wl-proxy seat)))
    (when proxy-seat
	  (wm-seat-pointer-theme proxy-seat name size))))

(define (seat-wm-pointer-warp seat x y)
  "Warp the pointer to the given position.
Must be called in a ~manage_sequence~."
  (let ((proxy-seat (seat-wl-proxy seat)))
    (when proxy-seat
	  (with-manage-sequence
	   (wm-seat-pointer-warp proxy-seat x y)))))

;;; events
(define (on-seat data manager proxy-seat)
  "Handle a new seat event from the compositor."
  (log-debug "New seat proxy: ~a" proxy-seat)
  (let ((seat (make-seat #:wl-proxy proxy-seat)))
    (seat-add! seat)))

(define (seat-on-removed data proxy-seat)
  "Seat was removed. This will take care of
Removing the seat record and clearing up memory.
It calls wm-seat-destroy to destroy wayland references 
Hook: *seat-destroy-hook*"
  (let ((seat (seat-find-by-proxy proxy-seat)))
	(log-debug "Seat removed: ~a" seat)
    (when seat (seat-remove! seat))
	(wm-seat-destroy proxy-seat)
	(gliver-hook-run! *seat-destroy-hook* seat)))

(define (seat-on-wl-seat data proxy-seat object-id)
  "The wl_seat object corresponding to the river_seat_v1."
  (let ((seat (seat-find-by-proxy proxy-seat)))
    (when seat
	  (let ((prev-object-id (seat-wl-seat seat)))
		(%seat-wl-seat-set! seat object-id)
		(log-debug "Seat ~a object-id updated to ~a" seat object-id)
		(gliver-hook-run! *seat-object-id-changed-hook* seat prev-object-id)))))

(define (seat-on-pointer-enter data proxy-seat proxy-window)
  "The seat's pointer entered the given window's area."
  (let ((seat (seat-find-by-proxy proxy-seat))
		(window (window-find-by-proxy proxy-window)))
    (when (and seat window)
	  (log-debug "Seat ~a pointer entered ~a" seat window)
	  (when *wm-behavior-focus-mouse-enter*
		(seat-wm-window-focus seat window))
	  (%seat-window-entered-set! seat window)
	  (gliver-hook-run! *seat-window-entered-changed-hook* seat window))))

(define (seat-on-pointer-leave data proxy-seat)
  "The seat's pointer left the recent window entered."
  (let ((seat (seat-find-by-proxy proxy-seat)))
    (when seat
	  (log-debug "Seat ~a pointer left window" seat)
	  (when *wm-behavior-focus-clear-mouse-leave*
		(wm-seat-window-focus-clear proxy-seat))
	  (%seat-window-entered-set! seat #f)
	  (gliver-hook-run! *seat-window-entered-changed-hook* seat #f))))

(define (seat-on-window-interaction data proxy-seat proxy-window)
  "Window is clicked or input is sent to it, focus it"
  (let ((seat (seat-find-by-proxy proxy-seat))
		(window (window-find-by-proxy proxy-window)))
	(log-debug "Seat ~a is interacting with window ~a" seat window)
    (when (and seat window)
	  (when *wm-behavior-focus-mouse-click*
		(seat-wm-window-focus seat window))
	  (gliver-hook-run! *seat-window-interacted-hook* seat window))))

(define (seat-on-shell-interaction data proxy-seat shell-proxy)
  "Surface is clicked or input is sent to it"
  ;; TODO: need to figure this hook out and test it
  ;; I don't thing it's of any use at the moment honestly
  (log-debug "Seat ~a is interacting with surface: ~a" seat shell-proxy)
  (gliver-hook-run! *seat-shell-interacted-hook* seat window))

(define (seat-on-op-delta data proxy-seat dx dy)
  "This event indicates the total change in position since the
start of the operation of the pointer/touch point/etc."
  (let ((seat (seat-find-by-proxy proxy-seat)))
    (when seat
	  (log-debug "Seat ~a position delta: ~ax~a" seat dx dy)
	  (gliver-hook-run! *seat-seat-op-delta-changed-hook* seat dx dy))))

(define (seat-on-op-release data proxy-seat)
  "The input driving the current interactive operation has been released."
  (let ((seat (seat-find-by-proxy proxy-seat)))
    (when seat
	  (log-debug "Seat ~a interaction released" seat dx dy)
	  (gliver-hook-run! *seat-seat-op-released-hook* seat))))

(define (seat-on-pointer-position data proxy-seat x y)
  "The current position of the pointer in the compositor's logical."
  (let ((seat (seat-find-by-proxy proxy-seat)))
    (when seat
	  (log-debug "Seat ~a pointer position changed to ~ax~a" seat x y)
	  (gliver-hook-run! *seat-seat-pointer-position-changed-hook* seat x y))))

(gliver-hook-add! %seat-created-hook 'on-seat 0)
(gliver-hook-add! %seat-removed-hook 'seat-on-removed 0)
(gliver-hook-add! %seat-object-id-changed-hook 'seat-on-wl-seat 0)
(gliver-hook-add! %seat-window-pointer-entered-hook 'seat-on-pointer-enter 0)
(gliver-hook-add! %seat-window-pointer-left-hook 'seat-on-pointer-leave 0)
(gliver-hook-add! %seat-window-interacted-hook 'seat-on-window-interaction 0)
(gliver-hook-add! %seat-shell-interacted-hook 'seat-on-shell-interaction 0)
(gliver-hook-add! %seat-op-delta-changed-hook 'seat-on-op-delta 0)
(gliver-hook-add! %seat-op-released-hook 'seat-on-op-release 0)
(gliver-hook-add! %seat-pointer-position-changed-hook 'seat-on-pointer-position 0)

