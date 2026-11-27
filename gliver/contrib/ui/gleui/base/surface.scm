;;; gliver/contrib/ui/gleui/base/surface.scm --- Wayland Layer Surface
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; A surface in Wayland is a drawable object that acts like a canvas
;;; it's usually filled with a Wayland buffer which represents the pixels.
;;; A surface layer in Wayland is an assigned role to a surface, like background
;;; overlay, bottom, top.
;;; This module manages Wayland wlr-layer-shell surface creation, lifecycle,
;;; and buffer commits for gleui.
;;; It handles:
;;;  - Layer surface callback registration
;;;  - Layer surface creation, sizing, and buffer commits

(define-module (gliver contrib ui gleui base surface)
  #:use-module (system foreign)
  #:use-module (srfi srfi-2)
  #:use-module (gliver core)
  #:use-module (gliver wayland client)
  #:use-module (gliver wayland gen wayland)
  #:use-module (gliver river connector)
  #:use-module (gliver river wlr-layer-shell)
  #:declarative? #f
  #:export (
            gleui-numpad->anchor
            *gleui-layer-surface-callbacks*
            gleui-layer-surface-create
            gleui-layer-surface-destroy!
            gleui-layer-surface-set-size!
            gleui-layer-surface-set-margin!
            gleui-layer-surface-buffer-commit!
            gleui-layer-surface-on-configure
            gleui-layer-surface-on-closed
            gleui-layer-surface-on-globals-unbind
))

(define (gleui-numpad->anchor n)
  "Convert Gliver numpad location N (1 to 9) into an anchor bitmask.
Parameters: (N)
  - N: Integer numpad digit from 1 (bottom-left) to 9 (top-right).
Returns integer layer-shell anchor bitmask."
  (case n
    ((7) (+ *wlr-layer-shell-anchor-top* *wlr-layer-shell-anchor-left*))
    ((8) *wlr-layer-shell-anchor-top*)
    ((9) (+ *wlr-layer-shell-anchor-top* *wlr-layer-shell-anchor-right*))
    ((4) *wlr-layer-shell-anchor-left*)
    ((5) 0)
    ((6) *wlr-layer-shell-anchor-right*)
    ((1) (+ *wlr-layer-shell-anchor-bottom* *wlr-layer-shell-anchor-left*))
    ((2) *wlr-layer-shell-anchor-bottom*)
    ((3) (+ *wlr-layer-shell-anchor-bottom* *wlr-layer-shell-anchor-right*))
    (else *wlr-layer-shell-anchor-top*)))

(define-var *gleui-layer-surface-callbacks* (make-hash-table)
            "Registry mapping for zwlr_layer_surface_v1 callbacks.
proxy -> (surface on-configure on-close).")

(define* (gleui-layer-surface-create #:key
                                     (output #f)
                                     (namespace "gleui")
                                     (layer *wlr-layer-shell-layer-overlay*)
                                     (anchor 0)
                                     (margin-top 0)
                                     (margin-right 0)
                                     (margin-bottom 0)
                                     (margin-left 0)
                                     (exclusive-zone -1)
                                     (keyboard-interactivity *wlr-layer-shell-interactivity-none*)
                                     (width 0)
                                     (height 0)
                                     (roundtrip? #t)
                                     (on-created #f)
                                     (on-configure #f)
                                     (on-close #f))
  "Create, configure, and commit a Wayland surface.
Parameters: ([#:OUTPUT #f] [#:NAMESPACE \"gleui\"] [#:LAYER *WLR-LAYER-SHELL-LAYER-OVERLAY*]
[#:ANCHOR 0] [#:MARGIN-TOP 0] [#:MARGIN-RIGHT 0] [#:MARGIN-BOTTOM 0] [#:MARGIN-LEFT 0]
[#:EXCLUSIVE-ZONE -1] [#:KEYBOARD-INTERACTIVITY *WLR-LAYER-SHELL-INTERACTIVITY-NONE*]
[#:WIDTH 0] [#:HEIGHT 0] [#:ROUNDTRIP? #t] [#:ON-CREATED #f] [#:ON-CONFIGURE #f] [#:ON-CLOSE #f])
  - OUTPUT: <output> record, string, integer, pointer, or #f for configured default.
  - NAMESPACE: String layer surface namespace (good for debugging).
  - LAYER: Integer Wayland layer constant (check *wlr-layer-shell-layer-*).
  - ANCHOR: Integer Wayland layer-shell edge anchor bitmask (calculated from gleui-numpad->anchor).
  - MARGIN-TOP: Integer top edge margin in pixels.
  - MARGIN-RIGHT: Integer right edge margin in pixels.
  - MARGIN-BOTTOM: Integer bottom edge margin in pixels.
  - MARGIN-LEFT: Integer left edge margin in pixels.
  - EXCLUSIVE-ZONE: Integer exclusive zone reservation in pixels (-1 ignores zones).
  - KEYBOARD-INTERACTIVITY: Integer Wayland keyboard interactivity constant.
  - WIDTH: Integer surface width in pixels (0 if anchored horizontally).
  - HEIGHT: Integer surface height in pixels (0 if anchored vertically).
  - ROUNDTRIP?: Boolean performing wl_display_roundtrip after initial commit when #t.
  - ON-CREATED: Procedure or #f callback invoked as (ON-CREATED SURFACE LAYER-SURFACE) prior to commit.
  - ON-CONFIGURE: Procedure or #f callback invoked as (ON-CONFIGURE WIDTH HEIGHT).
  - ON-CLOSE: Procedure or #f zero-argument callback invoked when closed.
Returns two values: SURFACE and LAYER-SURFACE pointers, or #f and #f on failure."

  ;; the happy path is straight forward, but to avoid crashes and memory leaks
  ;; many guard checks and objects destroy requests are implemented
  (if (or (not *river-connected?*)
          (not (valid-pointer? *wl-display*))
          (not (valid-pointer? *wl-compositor*))
          (not (valid-pointer? *wlr-layer-shell*)))
      (values #f #f)
      (let* ((surface       (wl-compositor-create-surface *wl-compositor*))
             (target-output (output-find output)))
        (if (not (valid-pointer? surface))
            (values #f #f)
            (begin
              ;; take care of creating viewporter and scale
              (surface-scale-create surface target-output)
              ;; variables that need cleaning are stored before catch so
              ;; the catch block can access and clean them
              (let ((layer-surface #f)
                    (proxy-output (if (output? target-output)
                                      (output-wl-output-proxy target-output)
                                      %null-pointer)))
                (catch #t
                  (lambda ()
                    (set! layer-surface (wlr-layer-shell-get-layer-surface
                                         *wlr-layer-shell*
                                         surface
                                         proxy-output
                                         layer
                                         namespace))
                    ;; if layer surface wasn't created successfully then
                    ;; destroy what has been created so far and return #f
                    (if (not (valid-pointer? layer-surface))
                        (begin
                          (surface-scale-destroy surface)
                          (wl-surface-destroy surface)
                          (values #f #f))

                        ;; configure layer surface size, anchor, margin, and zone and return
                        (begin
                          (wlr-layer-surface-set-size layer-surface width height)
                          (wlr-layer-surface-set-anchor layer-surface anchor)
                          (wlr-layer-surface-set-margin layer-surface margin-top margin-right margin-bottom margin-left)
                          (wlr-layer-surface-set-exclusive-zone layer-surface exclusive-zone)
                          (wlr-layer-surface-set-keyboard-interactivity layer-surface keyboard-interactivity)
                          (when (procedure? on-created)
                            (on-created surface layer-surface))
                          (hash-set! *gleui-layer-surface-callbacks*
                                     layer-surface
                                     (list surface on-configure on-close))
                          (wl-surface-commit surface)
                          (when roundtrip?
                            (wl-display-roundtrip *wl-display*))
                          (wl-display-flush *wl-display*)
                          (values surface layer-surface))))

                  (lambda (k . args)
                    ;; on error, clean all pointers that have been created
                    (log-error "gleui: layer-surface-create failed: ~a ~a" k args)
                    (when (valid-pointer? layer-surface)
                      (hash-remove! *gleui-layer-surface-callbacks* layer-surface)
                      (wlr-layer-surface-destroy layer-surface))
                    (when (valid-pointer? surface)
                      (surface-scale-destroy surface)
                      (wl-surface-destroy surface))
                    (values #f #f)))))))))

(define (gleui-layer-surface-destroy! surface layer-surface)
  "Destroy SURFACE and LAYER-SURFACE associated with it.
Parameters: (SURFACE LAYER-SURFACE)
  - SURFACE: Pointer to Wayland wl_surface.
  - LAYER-SURFACE: Pointer to zwlr_layer_surface_v1."
  (when (valid-pointer? layer-surface)
    (hash-remove! *gleui-layer-surface-callbacks* layer-surface)
    (wlr-layer-surface-destroy layer-surface))
  (when (valid-pointer? surface)
    (surface-scale-destroy surface)
    (wl-surface-destroy surface)))

(define (gleui-layer-surface-set-size! layer-surface width height)
  "Set logical WIDTH and HEIGHT on LAYER-SURFACE.
Parameters: (LAYER-SURFACE WIDTH HEIGHT)
  - LAYER-SURFACE: Pointer to zwlr_layer_surface_v1.
  - WIDTH: Integer logical width in pixels.
  - HEIGHT: Integer logical height in pixels."
  (when (valid-pointer? layer-surface)
    (wlr-layer-surface-set-size layer-surface width height)))

(define (gleui-layer-surface-set-margin! layer-surface top right bottom left)
  "Set TOP, RIGHT, BOTTOM, and LEFT edge margins on LAYER-SURFACE.
Parameters: (LAYER-SURFACE TOP RIGHT BOTTOM LEFT)
  - LAYER-SURFACE: Pointer to zwlr_layer_surface_v1.
  - TOP: Integer top edge margin in pixels.
  - RIGHT: Integer right edge margin in pixels.
  - BOTTOM: Integer bottom edge margin in pixels.
  - LEFT: Integer left edge margin in pixels."
  (when (valid-pointer? layer-surface)
    (wlr-layer-surface-set-margin layer-surface top right bottom left)))

(define (gleui-layer-surface-buffer-commit! surface buffer width height)
  "Attach BUFFER to SURFACE, set WIDTH and HEIGHT, and commit.
Parameters: (SURFACE BUFFER WIDTH HEIGHT)
  - SURFACE: Pointer to Wayland wl_surface.
  - BUFFER: Pointer to Wayland wl_buffer.
  - WIDTH: Integer logical destination width in pixels.
  - HEIGHT: Integer logical destination height in pixels."
  (when (and (valid-pointer? surface)
             (valid-pointer? buffer))
    (surface-viewport-destination-set! surface width height)
    (wl-surface-set-buffer-scale surface 1)
    (wl-surface-attach surface buffer 0 0)
    (wl-surface-damage surface 0 0 width height)
    (wl-surface-commit surface)))

(define (gleui-layer-surface-on-configure data proxy serial width height)
  "Invoked when surface is configured, should acknowledge layer surface,
configure SERIAL on PROXY, and invoke registered callback with WIDTH and HEIGHT.
Parameters: (DATA PROXY SERIAL WIDTH HEIGHT)
  - DATA: Pointer to unused listener user data.
  - PROXY: Pointer to zwlr_layer_surface_v1.
  - SERIAL: Integer configure event serial number to acknowledge.
  - WIDTH: Integer suggested surface width in pixels.
  - HEIGHT: Integer suggested surface height in pixels."
  (and-let* ((entry (hash-ref *gleui-layer-surface-callbacks* proxy)))
    (wlr-layer-surface-ack-configure proxy serial)
    (let ((on-configure (list-ref entry 1)))
      (when (procedure? on-configure)
        (on-configure width height)))))

(define (gleui-layer-surface-on-closed data proxy)
  "Invoked when surface is closed, should destroy the layer surface.
Parameters: (DATA PROXY)
  - DATA: Pointer to unused listener user data.
  - PROXY: Pointer to zwlr_layer_surface_v1."
  (and-let* ((entry (hash-ref *gleui-layer-surface-callbacks* proxy)))
    (hash-remove! *gleui-layer-surface-callbacks* proxy)
    (let ((surface  (list-ref entry 0))
          (on-close (list-ref entry 2)))
      (if (procedure? on-close)
          (on-close)
          (gleui-layer-surface-destroy! surface proxy)))))

(define (gleui-layer-surface-on-globals-unbind)
  "Clear the global layer surface callback registry."
  (hash-clear! *gleui-layer-surface-callbacks*))

(gliver-hook-add! %wlr-layer-surface-configure-hook 'gleui-layer-surface-on-configure)
(gliver-hook-add! %wlr-layer-surface-closed-hook 'gleui-layer-surface-on-closed)
(gliver-hook-add! *gliver-globals-unbind-hook* 'gleui-layer-surface-on-globals-unbind)
