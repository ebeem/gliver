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
            *surface->scale*
            surface-scale
            surface-viewporter
            surface-scale-set!
            surface-scale-create
            surface-scale-destroy
            surface-scale-on-preferred-scale
))

(define-var *surface->scale* (make-hash-table)
			"Hash-map used to track surface scales.
surface proxy -> scale number (e.g. 1.25).")

(define* (surface-scale surface #:optional (default-scale 1.0))
  "Return current scale factor for SURFACE as a number (e.g. 1.0, 1.25, 2.0).
Parameters: (SURFACE [DEFAULT-SCALE])
  - SURFACE: Pointer to wl_surface proxy.
  - DEFAULT-SCALE: Fallback scale if not tracked or no preferred scale received.
Returns: Scale number (float)."
  (if (and (valid-pointer? surface)
		   (hash-ref *surface->scale* surface))
      (hash-ref *surface->scale* surface)
      default-scale))

(define* (surface-viewporter proxy-surface)
  "Get the wp_viewport proxy associated with SURFACE.
Parameters: (PROXY-SURFACE)
  - PROXY-SURFACE: Pointer to wl_surface proxy.
Returns: Pointer to wp_viewport or #f."
  (surface->viewport proxy-surface))

(define (surface-scale-set! surface scale)
  "Set stored scale factor for SURFACE.
Parameters: (SURFACE SCALE)
  - SURFACE: Pointer to wl_surface proxy.
  - SCALE: Scale factor (number)."
  (when (and (valid-pointer? surface)
			 (number? scale)
			 (> scale 0))
    (hash-set! *surface->scale* surface scale)))

(define (surface-scale-create surface)
  "Create or retrieve the wp_fractional_scale_v1 object for SURFACE.
Also initializes wp_viewport for SURFACE.
Parameters: (SURFACE)
  - SURFACE: Pointer to wl_surface proxy.
Returns: wp_fractional_scale_v1 proxy pointer or #f."
  (when (and (valid-pointer? surface)
			 (valid-pointer? *viewporter*)
			 (valid-pointer? *fractional-scale-manager*))
	;; this will create the viewporter and store it in river module
	;; if it doesn't exist, surface-viewport will return it
    (viewporter-get-for-surface surface)
	;; this will also return cached if available
	(fractional-scale-manager-get-fractional-scale *fractional-scale-manager*
												   surface)))

(define (surface-scale-destroy surface)
  "Destroy the wp_fractional_scale_v1 object associated with SURFACE.
Also destroys any associated wp_viewport object.
Parameters: (SURFACE)
  - SURFACE: Pointer to wl_surface proxy."
  (when (valid-pointer? surface)
    (viewporter-destroy-for-surface surface)
    (and-let* ((proxy-scale (fractional-scale-find-by-surface surface))
               ((valid-pointer? proxy-scale)))
      (fractional-scale-destroy proxy-scale))
    (hash-remove! *surface->scale* surface)))

(define (surface-scale-on-preferred-scale data proxy-scale scale)
  "Invoked when PREFERRED-SCALE for PROXY-SCALE is changed.
Parameters: (DATA PROXY-SCALE SCALE)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-SCALE: Pointer to wp_fractional_scale_v1 proxy.
  - SCALE: Preferred scale with denominator 120 (uint32)."
  (and-let* ((scale-val (fractional-scale->number scale)) ; converted to float
             (surface (hash-ref *fractional-scale-surfaces* proxy-scale))
             ((valid-pointer? surface)))
    (surface-scale-set! surface scale-val)
    (gliver-hook-run! *surface-scale-changed-hook* surface scale-val)))
(gliver-hook-add! %fractional-scale-preferred-scale-hook 'surface-scale-on-preferred-scale)

