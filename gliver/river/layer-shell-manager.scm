;;; gliver/river/layer-shell-manager.scm --- River layer shell integration
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module integrates Gliver with the River Wayland compositor.
;;; It handles: Layer shell via river_layer_shell_v1 protocol

(define-module (gliver river layer-shell-manager)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (system foreign)
  #:use-module (gliver river connector)
  #:use-module (gliver wayland client)
  #:use-module (gliver wayland gen river-layer-shell-v1)
  #:use-module (gliver core)
  #:export (
			*layer-shell-output-listener*
			*layer-shell-seat-listener*
			*active-layer-shell-outputs*
			*active-layer-shell-seats*
			*layer-shell-focus-state*
			*needs-layer-shell-sync*
			layer-shell-on-globals-bind
			layer-shell-on-globals-unbind
			layer-shell-on-globals-verify
			layer-shell-on-listeners-attach
			layer-shell-on-manage-start
			on-layer-shell-output-non-exclusive-area
			on-layer-shell-seat-focus-exclusive
			on-layer-shell-seat-focus-non-exclusive
			on-layer-shell-seat-focus-none
			find-output-for-ls-proxy
			find-seat-for-ls-proxy
			layer-shell-output-set-default!
			sync-layer-shell!
))

;; river_layer_shell_v1 state
(define *layer-shell-output-listener* #f)   ;; shared listener for all layer shell outputs
(define *layer-shell-seat-listener* #f)     ;; shared listener for all layer shell seats
(define *active-layer-shell-outputs* '())   ;; alist: (output-wl-proxy . layer-shell-output-proxy)
(define *active-layer-shell-seats* '())     ;; alist: (seat-wl-proxy . layer-shell-seat-proxy)
(define *layer-shell-focus-state* 'none)    ;; current layer shell focus: 'none | 'exclusive | 'non-exclusive
(define *needs-layer-shell-sync* #f)

(define (layer-shell-on-globals-bind registry protocol-name object-id version)
  "Bind and register the layer shell global."
  (when (string=? protocol-name RIVER_LAYER_SHELL_V1_NAME)
	(log-info "Binding ~a..." protocol-name)
    (set! *layer-shell*
          (gliver-wl-registry-bind registry object-id
                                   *river-layer-shell-v1-interface*
                                   (min version 1)))))

(define (layer-shell-on-globals-unbind)
  "Unbind/destroy the layer shell and all associated objects."
  ;; destroy active layer shell outputs
  (for-each (lambda (pair)
              (river-layer-shell-output-v1-destroy (cdr pair)))
            *active-layer-shell-outputs*)
  (set! *active-layer-shell-outputs* '())

  ;; destroy active layer shell seats
  (for-each (lambda (pair)
              (river-layer-shell-seat-v1-destroy (cdr pair)))
            *active-layer-shell-seats*)
  (set! *active-layer-shell-seats* '()))

(define (layer-shell-on-globals-verify)
  "Verify the layer shell global was bound.
Note: layer shell is optional per the protocol spec, so we only warn."
  (when (null-pointer? *layer-shell*)
    (log-warn "river_layer_shell_v1 not available, layer shell features disabled")))

(define (layer-shell-on-listeners-attach)
  "Create the shared listeners for layer shell output and seat events."
  (set! *layer-shell-output-listener*
        (make-river-layer-shell-output-v1-listener
         on-layer-shell-output-non-exclusive-area))
  (set! *layer-shell-seat-listener*
        (make-river-layer-shell-seat-v1-listener
         on-layer-shell-seat-focus-exclusive
         on-layer-shell-seat-focus-non-exclusive
         on-layer-shell-seat-focus-none))
  ;; request initial sync on next manage sequence
  (set! *needs-layer-shell-sync* #t))

(define (layer-shell-on-manage-start)
  "Handle manage sequence synchronization for layer shell.
Creates layer-shell-output and layer-shell-seat objects on first sync."
  (when *needs-layer-shell-sync*
    (unless (null-pointer? *layer-shell*)
      (log-info "Performing layer shell sync...")
      (sync-layer-shell!))
    (set! *needs-layer-shell-sync* #f)))

;;; layer shell output requests
;;; TODO: maybe better to be called and configured
;;; to set the default output if not specified by a layer surface
(define (layer-shell-output-set-default! output)
  "Mark OUTPUT as the default for new layer surfaces that don't
request a specific output. This request modifies window management
state and must be made as part of a manage sequence.
Overrides any previous set_default request."
  (let ((pair (find (lambda (p)
                      (equal? (pointer-address (car p))
                              (pointer-address (output-wl-proxy output))))
                    *active-layer-shell-outputs*)))
    (when pair
      (log-debug "Setting default layer shell output: ~a" (output-name output))
      (river-layer-shell-output-v1-set-default (cdr pair)))))

;;; layer shell output events
(define (find-output-for-ls-proxy ls-output-proxy)
  "Look up the output wl-proxy for a given layer shell output proxy."
  (let loop ((pairs *active-layer-shell-outputs*))
    (cond
     ((null? pairs) #f)
     ((equal? (pointer-address (cdar pairs))
              (pointer-address ls-output-proxy))
      (caar pairs))
     (else (loop (cdr pairs))))))

(define (on-layer-shell-output-non-exclusive-area data ls-output-proxy x y width height)
  "Handle non_exclusive_area event from a layer shell output.
This indicates the area remaining after subtracting exclusive zones
of layer surfaces."
  (let ((output-proxy (find-output-for-ls-proxy ls-output-proxy)))
    (when output-proxy
      (let ((output (output-find-by-proxy output-proxy)))
        (when output
          (let* ((out-x (output-x output))
                 (out-y (output-y output))
                 ;; river_layer_shell_v1 non_exclusive_area x and y are in global coordinate space
                 (local-x (max 0 (- x out-x)))
                 (local-y (max 0 (- y out-y))))
            (log-debug "Layer shell non-exclusive area: output=~a global=~ax~a+(~a+~a) local=~ax~a+(~a+~a)"
                       (output-name output) x y width height local-x local-y width height)
            (output-usable-area-set! output local-x local-y width height)
            ;; fire the output change hook so layout and ui react
            (gliver-hook-run! *output-change-hook* output)))))))

(define (find-seat-for-ls-proxy ls-seat-proxy)
  "Look up the seat wl-proxy for a given layer shell seat proxy."
  (let loop ((pairs *active-layer-shell-seats*))
    (cond
     ((null? pairs) #f)
     ((equal? (pointer-address (cdar pairs))
              (pointer-address ls-seat-proxy))
      (caar pairs))
     (else (loop (cdr pairs))))))

(define (on-layer-shell-seat-focus-exclusive data ls-seat-proxy)
  "Handle focus_exclusive event.
A layer shell surface will be given exclusive keyboard focus.
The window manager should indicate that no window is focused.
All window manager focus requests are ignored until focus_non_exclusive
or focus_none is sent."
  (log-debug "Layer shell: exclusive focus")
  (set! *layer-shell-focus-state* 'exclusive)
  (let ((seat-proxy (find-seat-for-ls-proxy ls-seat-proxy)))
    (when seat-proxy
      (let ((seat (seat-find-by-proxy seat-proxy)))
        (when seat
          ;; clear window focus indication since layer surface has exclusive focus
          (gliver-hook-run! *seat-window-focused-hook* seat #f))))))

(define (on-layer-shell-seat-focus-non-exclusive data ls-seat-proxy)
  "Handle focus_non_exclusive event.
A layer shell surface will be given non-exclusive keyboard focus.
The window manager continues to control focus and may choose to
focus a different window at any time."
  (log-debug "Layer shell: non-exclusive focus")
  (set! *layer-shell-focus-state* 'non-exclusive)
  (let ((seat-proxy (find-seat-for-ls-proxy ls-seat-proxy)))
    (when seat-proxy
      (let ((seat (seat-find-by-proxy seat-proxy)))
        (when seat
          (gliver-hook-run! *seat-window-focused-hook* seat #f))))))

(define (on-layer-shell-seat-focus-none data ls-seat-proxy)
  "Handle focus_none event.
No layer shell surface will have keyboard focus.
The window manager may want to return focus to whichever window
last had focus."
  (log-debug "Layer shell: focus none")
  (set! *layer-shell-focus-state* 'none)
  (let ((seat-proxy (find-seat-for-ls-proxy ls-seat-proxy)))
    (when seat-proxy
      (let ((seat (seat-find-by-proxy seat-proxy)))
        (when seat
          ;; re-focus the previously focused window if available
          (let ((window (seat-window-focused seat)))
            (cond
             ((and window (window? window) (not (window-destroyed? window)))
              (gliver-hook-run! *seat-window-focused-hook* seat window))
             (else
              (let ((cur (window-current)))
                (when (and cur (window? cur) (not (window-destroyed? cur)))
                  (gliver-hook-run! *seat-window-focused-hook* seat cur)))))))))))


;;; synchronization
(define (sync-layer-shell!)
  "Create layer shell output and seat objects for all known outputs and seats.
Attaches listeners to receive events."
  ;; destroy existing layer shell outputs
  (for-each (lambda (pair)
              (river-layer-shell-output-v1-destroy (cdr pair)))
            *active-layer-shell-outputs*)
  (set! *active-layer-shell-outputs* '())

  ;; destroy existing layer shell seats
  (for-each (lambda (pair)
              (river-layer-shell-seat-v1-destroy (cdr pair)))
            *active-layer-shell-seats*)
  (set! *active-layer-shell-seats* '())

  ;; create layer shell output objects for each known output
  (let ((outputs (manager-outputs *manager*)))
    (log-info "Syncing layer shell for ~a outputs..." (length outputs))
    (set! *active-layer-shell-outputs*
      (filter-map
       (lambda (output)
         (let ((output-proxy (output-wl-proxy output)))
           (when (and output-proxy (pointer? output-proxy)
                      (not (null-pointer? output-proxy)))
             (let ((ls-output (river-layer-shell-v1-get-output
                               *layer-shell* output-proxy)))
               (unless (null-pointer? ls-output)
                 (wl-proxy-add-listener ls-output
                                        *layer-shell-output-listener*
                                        %null-pointer))
               (cons output-proxy ls-output)))))
       outputs)))

  ;; create layer shell seat objects for each known seat
  (let ((seats (manager-seats *manager*)))
    (log-info "Syncing layer shell for ~a seats..." (length seats))
    (set! *active-layer-shell-seats*
      (filter-map
       (lambda (seat)
         (let ((seat-proxy (seat-wl-proxy seat)))
           (when (and seat-proxy (pointer? seat-proxy)
                      (not (null-pointer? seat-proxy)))
             (let ((ls-seat (river-layer-shell-v1-get-seat
                             *layer-shell* seat-proxy)))
               (unless (null-pointer? ls-seat)
                 (wl-proxy-add-listener ls-seat
                                        *layer-shell-seat-listener*
                                        %null-pointer))
               (cons seat-proxy ls-seat)))))
       seats)))

  (log-info "Layer shell synced (~a outputs, ~a seats)."
            (length *active-layer-shell-outputs*)
            (length *active-layer-shell-seats*)))

;; handle river initialization steps
(gliver-hook-add! *gliver-globals-bind-hook* 'layer-shell-on-globals-bind)
(gliver-hook-add! *gliver-globals-unbind-hook* 'layer-shell-on-globals-unbind)
(gliver-hook-add! *gliver-globals-verify-hook* 'layer-shell-on-globals-verify)
(gliver-hook-add! *gliver-listeners-attach-hook* 'layer-shell-on-listeners-attach)
(gliver-hook-add! *manager-manage-start-hook* 'layer-shell-on-manage-start)
