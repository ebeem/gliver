;;; gliver/core/seat.scm --- Seat input and pointer operations
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module manages seat input devices, keyboard focus, and pointer interactions.
;;; It handles:
;;;  - Managing seat creation, removal, and focus tracking
;;;  - Handling pointer gestures, interactive drag/resize operations, and warping
;;;  - Handling pointer bindings and cursor theme configuration
;;;  - Providing callbacks for pointer motion, clicks, and interaction

(define-module (gliver core seat)
  #:use-module (srfi srfi-2)
  #:use-module (gliver core-base)
  #:use-module (gliver river window-manager)
  #:use-module (gliver river wm-seat-manager)
  #:declarative? #f
  #:export (
			*seat-focus-on-mouse-enter*
			*seat-unfocus-on-mouse-leave*
			*seat-focus-on-mouse-click*
			seat-add!
			seat-remove!
			seat-wm-window-focus
			seat-wm-window-focus-clear
			seat-wm-pointer-op-start
			seat-wm-pointer-op-end
			seat-wm-pointer-binding-get
			seat-wm-pointer-theme
			seat-wm-pointer-warp
			seat-on-seat
			seat-on-removed
			seat-on-wl-seat
			seat-on-pointer-enter
			seat-on-pointer-leave
			seat-on-window-interaction
			seat-on-shell-interaction
			seat-on-op-delta
			seat-on-op-release
			seat-on-pointer-position
			seat-on-window-destroyed
))

(define-var *seat-focus-on-mouse-enter* #f
			"Whether moving the mouse pointer into a window automatically focuses it.")
(define-var *seat-unfocus-on-mouse-leave* #f
			"Whether moving the mouse pointer out of a window automatically unfocus it.")
(define-var *seat-focus-on-mouse-click* #t
			"Whether clicking within a window focuses it.")

(define (seat-add! seat)
  "Register SEAT in the Gliver's manager hierarchy.
Parameters: (SEAT)
  - SEAT: <seat> record to register."
  (%manager-seats-set! *manager*
					   (cons seat (manager-seats *manager*)))
  (gliver-hook-run! *seat-created-hook* seat))

(define (seat-remove! seat)
  "Remove SEAT from the Gliver's manager hierarchy.
Parameters: (SEAT)
  - SEAT: <seat> record to remove."
  (let ((remaining (delq seat (manager-seats *manager*))))
    (%manager-seats-set! *manager* remaining))
  (gliver-hook-run! *seat-destroy-hook* seat))

;;; window manager api calls
(define* (seat-wm-window-focus seat window #:key (force #f))
  "Focus WINDOW so all SEAT interaction happen with it.
Parameters: (SEAT WINDOW [#:force #f])
  - SEAT: <seat> record.
  - WINDOW: <window> record to focus.
  - FORCE: Force focusing even if already recorded as the focused window on seat."
  (and-let* (((seat? seat))
			 ((window? window))
			 ((not (window-destroyed? window)))
			 (proxy-seat (seat-wl-proxy seat))
			 (proxy-window (window-wl-proxy window)))
	(log-debug "Seat ~a focusing surface ~a (force=~a)" seat window force)
	(when (or force (not (eq? window (seat-window-focused seat))))
	  (%seat-window-focused-set! seat window)
	  (with-manage-sequence
	   (wm-seat-window-focus proxy-seat proxy-window)
	   (gliver-hook-run! *seat-window-focused-hook* seat window)))))

;; (define (seat-wm-shell-focus seat shell-surface)
;;   "Focus SHELL so all SEAT interaction happen with it.
;; Parameters: (SEAT SHELL-SURFACE)
;;   - SEAT: <seat> record.
;;   - SHELL-SURFACE: <window> record to focus."
;;   (let ((proxy-seat (seat-wl-proxy seat))
;; 		(proxy-shell-surface (shell-surface-wl-proxy shell-surface)))
;;     (when (and proxy-seat proxy-shell-surface)
;; 	  (log-debug "Seat ~a focusing surface ~a" seat shell-surface)
;; 	  (unless (eq? shell-surface (seat-shell-surface-focused seat))
;; 		(%seat-shell-surface-focused-set! seat shell-surface)
;; 		;; unfocus old shell-surface and focus the new one via hooks as well
;; 		(with-manage-sequence
;; 		 (wm-seat-shell-surface-focus proxy-seat proxy-shell-surface)
;; 		 (gliver-hook-run! *seat-shell-surface-focused-hook* seat shell-surface))))))

(define (seat-wm-window-focus-clear seat)
  "Clear SEAT window/surface focus.
Parameters: (SEAT)
  - SEAT: <seat> record to clear focus."
  (and-let* (((seat? seat))
			 (proxy-seat (seat-wl-proxy seat)))
	(with-manage-sequence
	 (wm-seat-window-focus-clear proxy-seat)
	 (%seat-window-focused-set! seat #f)
	 (gliver-hook-run! *seat-window-focused-hook* seat #f))))

(define (seat-wm-pointer-op-start seat)
  "Start an interactive pointer operation (resize or drag window/surface).
Parameters: (SEAT)
  - SEAT: <seat> record."
  (and-let* ((proxy-seat (seat-wl-proxy seat)))
	(%seat-pointer-op-set! seat #t)
	(with-manage-sequence
	 (wm-seat-pointer-op-start proxy-seat))))

(define (seat-wm-pointer-op-end seat)
  "End an interactive pointer operation (resize or drag window/surface).
Parameters: (SEAT)
  - SEAT: <seat> record."
  (and-let* ((proxy-seat (seat-wl-proxy seat)))
	(%seat-pointer-op-set! seat #f)
	(with-manage-sequence
	 (wm-seat-pointer-op-end proxy-seat))))

(define (seat-wm-pointer-binding-get seat button modifiers)
  "Define a pointer binding in terms of a pointer button.
Parameters: (SEAT BUTTON MODIFIERS)
  - SEAT: <seat> record.
  - BUTTON: Input event button code (e.g. BTN_LEFT from input-event-codes.h).
  - MODIFIERS: Modifier bitmask flags (same as keyboard modifiers).
Returns river_pointer_binding_v1 proxy pointer."
  ;; TODO: this should store the river_pointer_binding_v1 somewhere
  ;; I am not implementing this for now as it's more of floating/stacking behavior
  (and-let* ((proxy-seat (seat-wl-proxy seat)))
	(wm-seat-pointer-binding-get proxy-seat button modifiers)))

(define (seat-wm-pointer-theme seat name size)
  "Set the XCursor theme for the seat.
Parameters: (SEAT NAME SIZE)
  - SEAT: <seat> record.
  - NAME: Cursor theme name string.
  - SIZE: Cursor size integer."
  (and-let* ((proxy-seat (seat-wl-proxy seat)))
	(wm-seat-pointer-theme proxy-seat name size)))

(define (seat-wm-pointer-warp seat x y)
  "Warp the pointer to the given position.
Parameters: (SEAT X Y)
  - SEAT: <seat> record.
  - X: Integer X coordinate.
  - Y: Integer Y coordinate."
  (and-let* ((proxy-seat (seat-wl-proxy seat)))
	(with-manage-sequence
	 (wm-seat-pointer-warp proxy-seat x y))))

;;; events
(define (seat-on-seat data manager proxy-seat)
  "Invoked when a seat is advertised.
Parameters: (DATA MANAGER SEAT-PROXY)
  - DATA: User context pointer (unused, null in Gliver).
  - MANAGER: Pointer to river_window_manager_v1 proxy.
  - SEAT-PROXY: Pointer to river_seat_v1 proxy."
  (log-info "New seat proxy: ~a" proxy-seat)
  (let ((seat (make-seat #:wl-proxy proxy-seat)))
    (seat-add! seat)))

(define (seat-on-removed data proxy-seat)
  "Invoked when a seat is removed from compositor.
Parameters: (DATA PROXY-SEAT)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-SEAT: Pointer to river_seat_v1 proxy."
  (and-let* ((seat (seat-find-by-proxy proxy-seat)))
	(log-info "Seat removed: ~a" seat)
    (seat-remove! seat)
	;; NOTE: after this hook, the seat will be destroyed from river
	(gliver-hook-run! *seat-destroy-hook* seat)))

(define (seat-on-wl-seat data proxy-seat object-id)
  "Invoked when a wl_seat is advertised river_seat_v1.
Parameters: (DATA PROXY-SEAT OBJECT-ID)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-SEAT: Pointer to river_seat_v1 proxy.
  - OBJECT-ID: Numeric Wayland object ID of the wl_seat global."
  (and-let* ((seat (seat-find-by-proxy proxy-seat)))
    (let ((prev-object-id (seat-wl-seat seat)))
      (%seat-wl-seat-set! seat object-id)
      (log-debug "Seat ~a object-id updated to ~a" seat object-id)
      (gliver-hook-run! *seat-object-id-changed-hook* seat prev-object-id))))

(define (seat-on-pointer-enter data proxy-seat proxy-window)
  "Invoked when a seat pointer enters a window area.
Parameters: (DATA PROXY-SEAT PROXY-WINDOW)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-SEAT: Pointer to river_seat_v1 proxy.
  - PROXY-WINDOW: Pointer to river_window_v1 proxy."
  (and-let* ((seat (seat-find-by-proxy proxy-seat))
			 (window (window-find-by-proxy proxy-window)))
	(log-debug "Seat ~a pointer entered ~a" seat window)
	(when *seat-focus-on-mouse-enter*
	  (seat-wm-window-focus seat window))
	(%seat-window-entered-set! seat window)
	(gliver-hook-run! *seat-window-entered-changed-hook* seat window)))

(define (seat-on-pointer-leave data proxy-seat)
  "Invoked when a seat pointer leaves a window area.
Parameters: (DATA PROXY-SEAT)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-SEAT: Pointer to river_seat_v1 proxy."
  (and-let* ((seat (seat-find-by-proxy proxy-seat)))
	(log-debug "Seat ~a pointer left window" seat)
	(when *seat-unfocus-on-mouse-leave*
	  (wm-seat-window-focus-clear proxy-seat))
	(%seat-window-entered-set! seat #f)
	(gliver-hook-run! *seat-window-entered-changed-hook* seat #f)))

(define (seat-on-window-interaction data proxy-seat proxy-win)
  "Invoked when a seat interacts (click or input) with a window.
Parameters: (DATA PROXY-SEAT PROXY-WIN)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-SEAT: Pointer to river_seat_v1 proxy.
  - PROXY-WIN: Pointer to river_window_v1 proxy."
  (and-let* ((seat (seat-find-by-proxy proxy-seat))
			 (window (window-find-by-proxy proxy-win)))
	(log-debug "Seat ~a is interacting with window ~a" seat window)
	(when *seat-focus-on-mouse-click*
	  (seat-wm-window-focus seat window))
	(gliver-hook-run! *seat-window-interacted-hook* seat window)))

(define (seat-on-shell-interaction data proxy-seat proxy-shell)
  "Invoked when a seat interacts (click or input) with a shell surface.
Parameters: (DATA PROXY-SEAT SHELL-PROXY)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-SEAT: Pointer to river_seat_v1 proxy.
  - PROXY-SHELL: Pointer to river_shell_surface_v1 proxy."
  (log-debug "Seat ~a is interacting with surface: ~a" proxy-seat proxy-shell)
  (and-let* ((seat (seat-find-by-proxy proxy-seat)))
	(gliver-hook-run! *seat-shell-interacted-hook* seat proxy-shell)))

(define (seat-on-op-delta data proxy-seat dx dy)
  "Invoked when a pointer/touch position delta changes during an interactive operation.
Parameters: (DATA PROXY-SEAT DX DY)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-SEAT: Pointer to river_seat_v1 proxy.
  - DX: Horizontal delta since operation start.
  - DY: Vertical delta since operation start."
  (and-let* ((seat (seat-find-by-proxy proxy-seat)))
	(log-debug "Seat ~a position delta: ~ax~a" seat dx dy)
	(gliver-hook-run! *seat-seat-op-delta-changed-hook* seat dx dy)))

(define (seat-on-op-release data proxy-seat)
  "Invoked when a seat interaction is released (pointer/touch release).
Parameters: (DATA PROXY-SEAT)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-SEAT: Pointer to river_seat_v1 proxy."
  (and-let* ((seat (seat-find-by-proxy proxy-seat)))
	(log-debug "Seat ~a interaction released" seat)
	(gliver-hook-run! *seat-seat-op-released-hook* seat)))

(define (seat-on-pointer-position data proxy-seat x y)
  "Invoked when a pointer position update in compositor logical coordinates.
Parameters: (DATA PROXY-SEAT X Y)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-SEAT: Pointer to river_seat_v1 proxy.
  - X: Logical horizontal pointer position.
  - Y: Logical vertical pointer position."
  (and-let* ((seat (seat-find-by-proxy proxy-seat)))
	(log-debug "Seat ~a pointer position changed to ~ax~a" seat x y)
	(gliver-hook-run! *seat-seat-pointer-position-changed-hook* seat x y)))

(define (seat-on-window-destroyed window container workspace)
  "Invoked when window is destroyed.
Parameters: (WINDOW CONTAINER WORKSPACE)
  - WINDOW: <window> record being destroyed.
  - CONTAINER: <container> record previously containing the window.
  - WORKSPACE: <workspace> record previously containing the container."
  (for-each
   (lambda (seat)
     (when (eq? (seat-window-focused seat) window)
       (%seat-window-focused-set! seat #f))
     (when (eq? (seat-window-entered seat) window)
       (%seat-window-entered-set! seat #f)))
   (manager-seats *manager*)))

(gliver-hook-add! %seat-created-hook 'seat-on-seat 0)
(gliver-hook-add! %seat-removed-hook 'seat-on-removed 0)
(gliver-hook-add! %seat-object-id-changed-hook 'seat-on-wl-seat 0)
(gliver-hook-add! %seat-window-pointer-entered-hook 'seat-on-pointer-enter 0)
(gliver-hook-add! %seat-window-pointer-left-hook 'seat-on-pointer-leave 0)
(gliver-hook-add! %seat-window-interacted-hook 'seat-on-window-interaction 0)
(gliver-hook-add! %seat-shell-interacted-hook 'seat-on-shell-interaction 0)
(gliver-hook-add! %seat-op-delta-changed-hook 'seat-on-op-delta 0)
(gliver-hook-add! %seat-op-released-hook 'seat-on-op-release 0)
(gliver-hook-add! %seat-pointer-position-changed-hook 'seat-on-pointer-position 0)
(gliver-hook-add! *window-destroyed-hook* 'seat-on-window-destroyed 0)
