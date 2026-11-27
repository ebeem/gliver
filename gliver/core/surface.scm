;;; gliver/core/surface.scm --- Surface management and scaling for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module manages surface scale and fractional scale integration for surfaces.
;;; It handles:
;;;   - Associating fractional scale and viewport objects with wl_surface proxies
;;;   - Querying and converting surface scale factors

(define-module (gliver core surface)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (system foreign)
  #:use-module (gliver core-base)
  #:use-module (gliver river fractional-scale-manager)
  #:use-module (gliver river viewporter-manager)
  #:declarative? #f
  #:export (
            *surface-scale-last*
            *surface-outputs*
            *surface-default-scale*
            *surface-scale-output-map*
            *surface-scale-map*
            surface-scale-predict
            surface-scale
            surface-viewporter
            surface-viewport-destination-set!
            surface-scale-set!
            surface-scale-create
            surface-scale-destroy
            surface-scale-on-preferred-scale
))

(define-var *surface-scale-last* #f
            "Last known surface fractional scale factor (e.g. 1.25).")
(define-var *surface-outputs* (make-hash-table)
            "Hash-map tracking wl_surface proxy -> <output> record.")
(define-var *surface-default-scale* 1.0
            "Default fallback scale if no preferred scale has ever been observed.")
(define-var *surface-scale-output-map* (make-hash-table)
            "Hash-map caching fractional scale factor per output.
<output> record -> scale number (e.g. 1.25).")
(define-var *surface-scale-map* (make-hash-table)
            "Hash-map used to track surface scales.
surface proxy -> scale number (e.g. 1.25).")

(define* (surface-scale-predict #:optional (output #f))
  "Predict the fractional scale factor for OUTPUT,
using cached per-output scale or last known surface scale.
Parameters: ([OUTPUT])
  - OUTPUT: <output> record or #f (optional).
Returns float scale factor."
  (let* ((cached-out-scale (and (output? output)
                                (hashq-ref *surface-scale-output-map* output))))
    (or cached-out-scale
        *surface-scale-last*
        *surface-default-scale*)))

(define* (surface-scale surface #:optional (default-scale #f))
  "Return current scale factor for SURFACE as a float (e.g. 1.25).
Parameters: (SURFACE [DEFAULT-SCALE])
  - SURFACE: Pointer to Wayland wl_surface.
  - DEFAULT-SCALE: Float or #f; fallback scale if not tracked or no preferred scale received.
Returns float scale factor."
  (or (and (valid-pointer? surface)
           (hash-ref *surface-scale-map* surface))
      default-scale
      (surface-scale-predict (and (valid-pointer? surface)
                                  (hash-ref *surface-outputs* surface)))))

(define* (surface-viewporter proxy-surface)
  "Get the wp_viewport proxy associated with SURFACE.
Parameters: (PROXY-SURFACE)
  - PROXY-SURFACE: Pointer to Wayland wl_surface.
Returns: Pointer to wp_viewport or #f."
  (surface->viewport proxy-surface))

(define (surface-viewport-destination-set! surface width height)
  "Set viewport destination rectangle for SURFACE to logical WIDTHxHEIGHT.
Parameters: (SURFACE WIDTH HEIGHT)
  - SURFACE: Pointer to Wayland wl_surface.
  - WIDTH: Integer destination width in logical pixels.
  - HEIGHT: Integer destination height in logical pixels."
  (and-let* ((vp (surface-viewporter surface))
             ((valid-pointer? vp)))
    (viewport-set-destination vp width height)))

(define (surface-scale-set! surface scale)
  "Set stored scale factor for SURFACE and update cache.
Parameters: (SURFACE SCALE)
  - SURFACE: Pointer to Wayland wl_surface.
  - SCALE: Float scale factor."
  (when (and (valid-pointer? surface)
             (number? scale)
             (> scale 0))
    (set! *surface-scale-last* scale)
    (and-let* ((out (hash-ref *surface-outputs* surface))
               ((output? out)))
      (hashq-set! *surface-scale-output-map* out scale)))
  (hash-set! *surface-scale-map* surface scale))

(define* (surface-scale-create surface #:optional (output #f))
  "Create or retrieve the wp_fractional_scale_v1 object for SURFACE.
Also initializes wp_viewport for SURFACE.
Parameters: (SURFACE [OUTPUT])
  - SURFACE: Pointer to Wayland wl_surface.
  - OUTPUT: <output> record or #f (optional).
Returns: wp_fractional_scale_v1 proxy pointer or #f."
  (when (valid-pointer? surface)
    (when (output? output)
      (hash-set! *surface-outputs* surface output))
    (unless (hash-ref *surface-scale-map* surface)
      (let ((predicted-scale (surface-scale-predict output)))
        ;; the reason why this prediction is used is that preferred scale
        ;; events take time until it's received for new surfaces, this makes
        ;; initial default scaling (1.0 by default) shows up for 100ms until
        ;; the new preferred scale is received why is not a good experience
        ;; I am not sure how this is handled in other GUI components
        ;; as in gliver's case this is only impacting gleui
        (hash-set! *surface-scale-map* surface predicted-scale)))

    ;; get viewporter and fractional scale for surface
    (when (valid-pointer? *viewporter*)
      (viewporter-get-for-surface surface))
    (when (valid-pointer? *fractional-scale-manager*)
      (fractional-scale-manager-get-fractional-scale *fractional-scale-manager*
                                                     surface))))

(define (surface-scale-destroy surface)
  "Destroy the wp_fractional_scale_v1 object associated with SURFACE.
Also destroys any associated wp_viewport object.
Parameters: (SURFACE)
  - SURFACE: Pointer to Wayland wl_surface."
  (when (valid-pointer? surface)
    (viewporter-destroy-for-surface surface)
    (and-let* ((proxy-scale (fractional-scale-find-by-surface surface))
               ((valid-pointer? proxy-scale)))
      (fractional-scale-destroy proxy-scale))
    (hash-remove! *surface-scale-map* surface)
    (hash-remove! *surface-outputs* surface)))

(define (surface-scale-on-preferred-scale data proxy-scale scale)
  "Invoked when PREFERRED-SCALE for PROXY-SCALE is changed.
Parameters: (DATA PROXY-SCALE SCALE)
  - DATA: Pointer to user context (unused, null in Gliver).
  - PROXY-SCALE: Pointer to wp_fractional_scale_v1 proxy.
  - SCALE: Integer preferred scale numerator with denominator 120 (uint32)."
  (and-let* ((scale-val (fractional-scale->number scale)) ; converted to float
             (surface (hash-ref *fractional-scale-surfaces* proxy-scale))
             ((valid-pointer? surface)))
    (let ((prev-scale (hash-ref *surface-scale-map* surface)))
      (surface-scale-set! surface scale-val)
      ;; Only fire hook if scale actually changed!
      (unless (and (number? prev-scale)
                   ;; avoid unneeded re-rendering
                   (< (abs (- prev-scale scale-val)) 1e-5))
        (gliver-hook-run! *surface-scale-changed-hook* surface scale-val)))))
(gliver-hook-add! %fractional-scale-preferred-scale-hook 'surface-scale-on-preferred-scale)

