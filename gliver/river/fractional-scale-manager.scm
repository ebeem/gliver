;;; gliver/river/fractional-scale-manager.scm --- fractional-scale protocol integration
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module manages fractional scale information for surfaces.
;;; It handles:
;;;   - Binding/unbinding protocol wp_fractional_scale_manager_v1 global
;;;   - Listening for preferred_scale events and dispatching to hooks
;;;
;;; This module implements the river interfaces (protocol fractional_scale_v1):
;;;   - wp_fractional_scale_manager_v1
;;;   - wp_fractional_scale_v1

(define-module (gliver river fractional-scale-manager)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (system foreign)
  #:use-module (gliver river connector)
  #:use-module (gliver wayland client)
  #:use-module (gliver wayland gen fractional-scale-v1)
  #:use-module (gliver core-base)
  #:declarative? #f
  #:export (
            *fractional-scale-manager*
            *fractional-scale-listener*
            *fractional-scale-surfaces*
            fractional-scale-find-by-surface
            *fractional-scale-denominator*
            *fractional-scale-manager-error-fractional-scale-exists*
            fractional-scale-on-globals-bind
            fractional-scale-on-globals-unbind
            fractional-scale-on-globals-verify
            fractional-scale-on-listeners-attach
            fractional-scale-manager-destroy
            fractional-scale-manager-get-fractional-scale
            fractional-scale-destroy
            fractional-scale-on-preferred-scale
            fractional-scale->number
            number->fractional-scale
))

;;; global state
(define-var *fractional-scale-manager* %null-pointer
            "Proxy for wp_fractional_scale_manager_v1.")

(define-var *fractional-scale-listener* #f
            "Shared Wayland listener for wp_fractional_scale_v1 preferred_scale events.")

;; surface tracking tables
;; maps fractional proxy-scale -> proxy-surface
(define *fractional-scale-surfaces* (make-hash-table))
(define (fractional-scale-find-by-surface proxy-surface)
  "Get the wp_fractional_scale_v1 proxy associated with SURFACE.
This is a reverse lookup for *fractional-scale-surfaces*
Parameters: (PROXY-SURFACE)
Returns: wp_fractional_scale_v1 pointer or #f."
  (and (valid-pointer? proxy-surface)
       (hash-fold (lambda (scale-obj s acc)
                    (if (equal? s proxy-surface) scale-obj acc))
                  #f
                  *fractional-scale-surfaces*)))

;; protocol constants
(define *fractional-scale-denominator* 120)
(define *fractional-scale-manager-error-fractional-scale-exists*
  WP_FRACTIONAL_SCALE_MANAGER_V1_ERROR_FRACTIONAL_SCALE_EXISTS)

(define (fractional-scale-on-globals-bind registry protocol-name object-id version)
  "Bind wp_fractional_scale_manager_v1 global interface from REGISTRY.
Matches PROTOCOL-NAME against `WP_FRACTIONAL_SCALE_MANAGER_V1_NAME` and binds OBJECT-ID
with VERSION.
Parameters: (REGISTRY PROTOCOL-NAME OBJECT-ID VERSION)
  - REGISTRY: Pointer to wl_registry proxy.
  - PROTOCOL-NAME: Interface name string.
  - OBJECT-ID: Numeric Wayland object ID.
  - VERSION: Maximum interface version supported by compositor."
  (when (string=? protocol-name WP_FRACTIONAL_SCALE_MANAGER_V1_NAME)
    (log-info "Binding ~a..." protocol-name)
    (set! *fractional-scale-manager*
          (gliver-wl-registry-bind registry object-id
                                   *wp-fractional-scale-manager-v1-interface*
                                   (min version 1)))))

(define (fractional-scale-on-globals-unbind)
  "Unbind/destroy the fractional scale manager and all associated fractional scale objects.
Parameters: None."
  (hash-for-each
   (lambda (proxy _val)
     (fractional-scale-destroy proxy))
   *fractional-scale-surfaces*)
  (hash-clear! *fractional-scale-surfaces*)

  (when (valid-pointer? *fractional-scale-manager*)
    (fractional-scale-manager-destroy *fractional-scale-manager*)))

(define (fractional-scale-on-globals-verify)
  "Verify the fractional scale manager global was bound.
Note: fractional scale manager is optional, so we only warn.
Parameters: None."
  (unless (valid-pointer? *fractional-scale-manager*)
    (log-warn "wp_fractional_scale_manager_v1 not available")))

(define (fractional-scale-on-listeners-attach)
  "Create the shared listener for wp_fractional_scale_v1 preferred_scale events.
Parameters: None."
  (set! *fractional-scale-listener*
        (make-wp-fractional-scale-v1-listener
         fractional-scale-on-preferred-scale)))

;;; wp_fractional_scale_manager_v1 requests
(define (fractional-scale-manager-destroy proxy-manager)
  "Destroy wp_fractional_scale_manager_v1 proxy object, this means everything was
cleared from client side and the server should also clear everything.
Parameters: (PROXY-MANAGER)
  - PROXY-MANAGER: Pointer to wp_fractional_scale_manager_v1 proxy."
  (when (and (valid-pointer? proxy-manager)
             (equal? *fractional-scale-manager* proxy-manager))
    (when *log-river-calls*
      (log-debug "Destroying fractional scale manager proxy: ~a" proxy-manager))
    (wp-fractional-scale-manager-v1-destroy proxy-manager)
    (set! *fractional-scale-manager* %null-pointer)))

(define (fractional-scale-manager-get-fractional-scale proxy-manager proxy-surface)
  "Create a fractional_scale object for a surface.
Parameters: (PROXY-MANAGER PROXY-SURFACE)
  - PROXY-MANAGER: Pointer to wp_fractional_scale_manager_v1 proxy.
  - PROXY-SURFACE: Pointer to wl_surface proxy.
Returns: wp_fractional_scale_v1 proxy pointer."
  (when (and (valid-pointer? proxy-manager)
             (valid-pointer? proxy-surface)
             ;; TODO: proxy-surface should be validated against a cached list
             (equal? *fractional-scale-manager* proxy-manager))
    ;; if surface has fractional scale, return it
    (or (fractional-scale-find-by-surface proxy-surface)
        ;; otherwise, get new fractional scale of the surface
        (let ((proxy-scale (wp-fractional-scale-manager-v1-get-fractional-scale
                            proxy-manager proxy-surface)))
          (when *log-river-calls*
            (log-debug "Creating fractional scale on ~a: surface=~a"
                       proxy-manager proxy-surface))
          (when (valid-pointer? proxy-scale)
            (wl-proxy-add-listener proxy-scale *fractional-scale-listener* %null-pointer)
            (hash-set! *fractional-scale-surfaces* proxy-scale proxy-surface))
          proxy-scale))))

;;; wp_fractional_scale_v1 requests
(define (fractional-scale-destroy proxy-scale)
  "Destroy wp_fractional_scale_v1 proxy object, this means everything was
cleared from client side and the server should also clear everything.
Parameters: (PROXY-SCALE)
  - PROXY-SCALE: Pointer to wp_fractional_scale_v1 proxy."
  (when (and (valid-pointer? proxy-scale)
             (hash-ref *fractional-scale-surfaces* proxy-scale))
    (when *log-river-calls*
      (log-debug "Destroying fractional scale proxy: ~a" proxy-scale))
    (wp-fractional-scale-v1-destroy proxy-scale)
    (hash-remove! *fractional-scale-surfaces* proxy-scale)))

;;; wp_fractional_scale_v1 events
(define (fractional-scale-on-preferred-scale data proxy-scale scale)
  "Invoked when compositor announces preferred scale for surface.
Parameters: (DATA PROXY-SCALE SCALE)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-SCALE: Pointer to wp_fractional_scale_v1 proxy.
  - SCALE: Preferred scale numerator with denominator 120 (uint32)."
  (when *log-river-calls*
    (let* ((scale-val (fractional-scale->number scale)))
      (log-debug "Fractional scale ~a preferred_scale: ~a (~a)"
                 proxy-scale scale scale-val)))
  (gliver-hook-run! %fractional-scale-preferred-scale-hook
                    data proxy-scale scale))

;; helper functions to convert from fractional scale to float and vise-versa
(define (fractional-scale->number scale)
  "Convert fractional scale numerator SCALE (with denominator 120) to a real number.
Parameters: (SCALE)
  - SCALE: Integer scale numerator (e.g. 120 for 1.0, 150 for 1.25).
Returns: Inexact real scale number."
  (if (and (number? scale) (> scale 0))
      (/ (exact->inexact scale) *fractional-scale-denominator*)
      1.0))

(define (number->fractional-scale n)
  "Convert a real number N to a fractional scale numerator (with denominator 120).
Parameters: (N)
  - N: Real number (e.g. 1.0, 1.25).
Returns: Integer scale numerator."
  (inexact->exact (round (* n *fractional-scale-denominator*))))

;; handle river initialization steps
(gliver-hook-add! *gliver-globals-bind-hook* 'fractional-scale-on-globals-bind)
(gliver-hook-add! *gliver-globals-unbind-hook* 'fractional-scale-on-globals-unbind)
(gliver-hook-add! *gliver-globals-verify-hook* 'fractional-scale-on-globals-verify)
(gliver-hook-add! *gliver-listeners-attach-hook* 'fractional-scale-on-listeners-attach)
