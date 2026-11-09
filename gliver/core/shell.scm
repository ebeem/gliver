;;; gliver/core/shell.scm --- Layer shell management
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module manages layer shell interactions.
;;; It handles:
;;;   - Defining exclusive and non-exclusive areas of the output
;;;   - Configuring default output for layer surfaces
;;;   - Managing focus between layer shell and regular application windows

(define-module (gliver core shell)
  #:use-module (srfi srfi-2)
  #:use-module (gliver core-base)
  #:use-module (gliver river layer-shell-manager)
  #:use-module (gliver river window-manager)
  #:declarative? #f
  #:export (
			*shell-default-output-name*
			*shell-current-default-output*
			shell-output-set-default!
			shell-on-output-non-exclusive-area
			shell-on-seat-focus-change
			shell-on-output-created
			shell-on-output-destroy
			shell-on-seat-created
			shell-on-seat-destroy
))

(define-var *shell-default-output-name* #f
			"Default output name in which layer shell surfaces will show
if set to #f, first advertised output will be the default.")
(define-var *shell-current-default-output* #f
			"Current default layer shell <output> record.")

(define (shell-output-set-default! output)
  "Mark OUTPUT as the default for new layer surfaces that don't
request a specific output.
Must be called in a manage sequence."
  (log-info "Setting default layer shell output to ~a" output)
  (and-let* ((proxy-ls-output (output-wl-ls-proxy output)))
    (with-manage-sequence
     (layer-shell-output-set-default proxy-ls-output)
	 (set! *shell-current-default-output* output))))

(define (shell-on-output-non-exclusive-area data ls-output-proxy x y width height)
  "Invoked when non exclusive area of a layer shell output changes.
Window manager should respect this area and update output available space accordingly.
The area maybe occupied by layer shell components like statusbar.
Parameters: (DATA PROXY-LS-OUTPUT X Y WIDTH HEIGHT)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-LS-OUTPUT: Pointer to river_layer_shell_output_v1 proxy.
  - X: Remaining usable area top-left corner global x coordinate.
  - Y: Remaining usable area top-left corner global y coordinate.
  - WIDTH: Remaining usable area width of the output.
  - HEIGHT: Remaining usable area height of the output."
  (and-let* ((output (output-find-by-ls-proxy ls-output-proxy))
			 (pv-width (output-usable-width output))
			 (pv-height (output-usable-height output)))
	(log-info "Updating usable output area of output ~a to ~ax~a+(~a+~a)"
			  output width height x y)
	(output-usable-area-set! output x y width height)
    ;; fire the output change hook so layout and ui react
    (gliver-hook-run! *output-dimensions-changed-hook* output pv-width pv-height)))

(define (shell-on-seat-focus-change data proxy-ls-seat type)
  "Invoked when seat focus changes between exclusive and non-exclusive areas.
Parameters: (DATA PROXY-LS-SEAT TYPE)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-LS-SEAT: Pointer to river_layer_shell_seat_v1 proxy.
  - TYPE: Symbol focus type ('exclusive 'non-exclusive 'none)."
  (and-let* ((seat (seat-find-by-ls-proxy proxy-ls-seat))
			 (prev-type (seat-focus-type seat)))
	(%seat-focus-type-set! seat type)
	(cond
	 ((eq? type 'exclusive)
	  ;; unfocus any focused window
	  (gliver-hook-run! *seat-window-focused-hook* seat #f))
	 ((eq? type 'non-exclusive)
	  ;; unfocus any focused window?
	  ;; I am not sure what's the use case for such behavior
	  ;; maybe based on name, window shouldn't be unfocused
	  (gliver-hook-run! *seat-window-focused-hook* seat #f))
	 ((eq? type 'none)
	  ;; re-focus the last focused window before layer shell takes over
	  (and-let* ((window (seat-window-focused seat))
				 ((window? window))
				 ((not (window-destroyed? window))))
        (gliver-hook-run! *seat-window-focused-hook* seat window))))
    (gliver-hook-run! *seat-focus-type-changed-hook* seat prev-type)))

(define (shell-on-output-created output)
  "Invoked when an <output> record is created.
Parameters: (OUTPUT)
  - OUTPUT: <output> record."
  ;; get the output layer shell
  (unless (output-wl-ls-proxy output)
	(let ((ls-proxy (layer-shell-get-output *layer-shell*
											(output-wl-proxy output))))
	  (%output-wl-ls-proxy-set! output ls-proxy)))

  ;; if the output name is *shell-default-output-name*, set as default
  (when (or (eq? (output-name output) *shell-default-output-name*)
			(not *shell-current-default-output*))
    (shell-output-set-default! output)))

(define (shell-on-output-destroy output)
  "Invoked when an <output> record is destroyed.
Parameters: (OUTPUT)
  - OUTPUT: <output> record."
  ;; destroy the output's wl-ls-proxy
  (and-let* ((proxy (output-wl-ls-proxy output))
			 ((valid-pointer? proxy)))
	(layer-shell-output-destroy proxy)
	(%output-wl-ls-proxy-set! output #f))

  ;; if deleted output is set as default, pick another one
  (when (eq? *shell-current-default-output* output)
	(let ((outputs (manager-outputs *manager*)))
	  (if (pair? outputs)
		  (shell-output-set-default! (car outputs))
		  ;; if not output is available, set default to #f
		  (set! *shell-current-default-output* #f)))))

(define (shell-on-seat-created seat)
  "Invoked when an <seat> record is created.
Parameters: (SEAT)
  - SEAT: <seat> record."
  ;; get the seat layer shell
  (unless (seat-wl-ls-proxy seat)
	(let ((ls-proxy (layer-shell-get-seat *layer-shell*
										  (seat-wl-proxy seat))))
	  (%seat-wl-ls-proxy-set! seat ls-proxy))))

(define (shell-on-seat-destroy seat)
  "Invoked when an <seat> record is destroyed.
Parameters: (SEAT)
  - SEAT: <seat> record."
  ;; destroy the seat's wl-ls-proxy
  (and-let* ((proxy (seat-wl-ls-proxy seat))
			 ((valid-pointer? proxy)))
	(layer-shell-seat-destroy proxy)))

(gliver-hook-add! %layer-shell-output-non-exclusive-area-changed-hook
				  'shell-on-output-non-exclusive-area 0)
(gliver-hook-add! %layer-shell-seat-focus-changed-hook 'shell-on-seat-focus-change 0)
(gliver-hook-add! *output-created-hook* 'shell-on-output-created 0)
(gliver-hook-add! *output-destroy-hook* 'shell-on-output-destroy 0)
(gliver-hook-add! *seat-created-hook* 'shell-on-seat-created 0)
(gliver-hook-add! *seat-destroy-hook* 'shell-on-seat-destroy 0)
