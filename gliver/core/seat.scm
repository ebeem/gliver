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
  #:use-module (gliver core window)
  #:use-module (gliver core manager)
  #:use-module (gliver river wm-seat-manager)
  #:export (seat-wl-proxy-set!
			seat-wl-proxy
			seat-window-focused-set!
			seat-window-focused
			seat-window-entered-set!
			seat-window-entered
			seat-name-set!
			seat-name
			seat?
			%make-seat
			seat-find-by-proxy
			seat-remove!
			seat-add!
			on-seat))

(define-record-type <seat>
  (%make-seat name window-entered window-focused wl-proxy)
  seat?
  (name               seat-name               seat-name-set!)
  (window-entered     seat-window-entered     seat-window-entered-set!)
  (window-focused     seat-window-focused     seat-window-focused-set!)
  (wl-proxy           seat-wl-proxy           seat-wl-proxy-set!))

(set-record-type-printer! <seat>
  (lambda (s port)
    (format port "#<seat ~a (~a window)>"
            (seat-name s)
            (seat-window-focused s))))

(define* (make-seat #:key (name #f) (wl-proxy #f) (window-entered #f) (window-focused #f))
  (%make-seat name window-entered window-focused wl-proxy))

(define (seat-find-by-proxy proxy)
  "Look up the <seat> record by comparing the raw memory address of the proxy."
  (if (not (pointer? proxy))
      #f ;; early exit
      (let ((addr (pointer-address proxy))
            (seats (manager-seats *manager*)))
        (find (lambda (seat)
                (let ((proxy-seat (seat-wl-proxy seat)))
                  (and (pointer? proxy-seat)
                       (= (pointer-address proxy-seat) addr))))
              seats))))

(define (seat-add! seat)
  (manager-seats-set! *manager*
						(cons seat (manager-seats *manager*))))

(define (seat-remove! seat)
  (let ((remaining (delete seat (manager-seats *manager*))))
    (manager-seats-set! *manager* remaining)))

(define (on-seat data manager proxy-seat)
  "Handle a new seat event from the compositor."
  (log-debug "New seat proxy: ~a" proxy-seat)
  (let ((seat (make-seat #:wl-proxy proxy-seat)))
    (seat-add! seat)))

(gliver-hook-add! %seat-created-hook on-seat)

;;; window manager api calls
(define (seat-wm-window-focus seat window)
  "Request that the compositor send keyboard input to the given window.
Must be called in a ~manage_sequence~."
  (let ((proxy-seat (seat-wl-proxy seat))
		(proxy-win (window-wl-proxy window)))
    (when (and proxy-seat proxy-win)
	  (wm-seat-window-focus proxy-seat proxy-win)
	  (seat-window-focused-set! seat window)
	  (gliver-hook-run! *seat-window-focused-hook* seat window))))

;; NOTE: wm-seat-shell-focus should be implemented here
;; I am not sure if it's actually needed, so I skipped it

(define (seat-wm-window-focus-clear seat)
  "Request that the compositor send keyboard input to the given window.
Must be called in a ~manage_sequence~."
  (let ((proxy-seat (seat-wl-proxy seat)))
    (when proxy-seat
	  (wm-seat-window-focus-clear proxy-seat)
	  (seat-window-focused-set! seat #f)
	  (gliver-hook-run! *seat-window-focused-hook* seat #f))))

(define (seat-wm-pointer-op-start seat)
  "Start an interactive pointer operation.
Must be called in a ~manage_sequence~."
  (let ((proxy-seat (seat-wl-proxy seat)))
    (when proxy-seat
	  (wm-seat-pointer-op-start proxy-seat))))

(define (seat-wm-pointer-op-end seat)
  "End an interactive pointer operation.
Must be called in a ~manage_sequence~."
  (let ((proxy-seat (seat-wl-proxy seat)))
    (when proxy-seat
	  (wm-seat-pointer-op-end proxy-seat))))

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
	  (wm-seat-pointer-warp proxy-seat x y))))

;;; events
(define (seat-on-removed data proxy-seat)
  "Seat was removed. This will take care of
Removing the seat record and clearing up memory.
It calls wm-seat-destroy to destroy wayland references 
Hook: *seat-destroy-hook*"
  (let ((seat (seat-find-by-proxy proxy-seat)))
    (when seat (seat-remove! seat))
	(wm-seat-destroy proxy-seat)
	(gliver-hook-run! *seat-destroy-hook* seat)))

(define (seat-on-wl-seat data proxy-seat name)
  "The wl_seat object corresponding to the river_seat_v1."
  (let ((seat (seat-find-by-proxy proxy-seat)))
    (when seat
	  (let ((prev-name (seat-name seat)))
		(seat-name-set! seat name)
		(gliver-hook-run! *seat-name-changed-hook* seat prev-name)))))

(define (seat-on-pointer-enter data proxy-seat proxy-win)
  "The seat's pointer entered the given window's area."
  (let ((seat (seat-find-by-proxy proxy-seat))
		(window (window-find-by-proxy proxy-win)))
    (when (and seat window)
	  (when *wm-behavior-focus-mouse-enter*
		(wm-seat-window-focus proxy-seat proxy-win))
	  (seat-window-entered-set! seat window)
	  (gliver-hook-run! *seat-window-entered-changed-hook* seat window))))

(define (seat-on-pointer-leave data proxy-seat)
  "The seat's pointer left the recent window entered."
  (let ((seat (seat-find-by-proxy proxy-seat)))
    (when seat
	  (when *wm-behavior-focus-clear-mouse-leave*
		(wm-seat-window-focus-clear proxy-seat))
	  (seat-window-entered-set! seat #f)
	  (gliver-hook-run! *seat-window-entered-changed-hook* seat #f))))

(define (seat-on-window-interaction data proxy-seat proxy-win)
  "Window is clicked or input is sent to it, focus it"
  (let ((seat (seat-find-by-proxy proxy-seat))
		(window (window-find-by-proxy proxy-win)))
    (when (and seat window)
	  (when *wm-behavior-focus-mouse-click*
		(wm-seat-window-focus proxy-seat proxy-win))
	  (gliver-hook-run! *seat-window-interacted-hook* seat window))))

(define (seat-on-shell-interaction data proxy-seat shell-proxy)
  "Surface is clicked or input is sent to it"
  ;; TODO: need to figure this hook out and test it
  ;; I don't thing it's of any use at the moment honestly
  (log-debug "Shell surface interaction: ~a" shell-proxy))

(define (seat-on-op-delta data proxy-seat dx dy)
  "This event indicates the total change in position since the
start of the operation of the pointer/touch point/etc."
  (let ((seat (seat-find-by-proxy proxy-seat)))
    (when seat
	  (gliver-hook-run! *seat-seat-op-delta-changed-hook* seat dx dy))))

(define (seat-on-op-release data proxy-seat)
  "The input driving the current interactive operation has been released."
  (let ((seat (seat-find-by-proxy proxy-seat)))
    (when seat
	  (gliver-hook-run! *seat-seat-op-released-hook* seat))))

(define (seat-on-pointer-position data proxy-seat x y)
  "The current position of the pointer in the compositor's logical."
  (let ((seat (seat-find-by-proxy proxy-seat)))
    (when seat
	  (gliver-hook-run! *seat-seat-pointer-position-changed-hook* seat x y))))

