;;; gliver/river/viewporter-manager.scm --- viewporter protocol integration
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module manages viewports (cropping and scaling) for surfaces.
;;; It handles:
;;;   - Binding/unbinding protocol wp_viewporter global
;;;   - Creating and destroying wp_viewport objects for surfaces
;;;   - Setting viewport destination and source rectangles
;;;
;;; This module implements the river interfaces (protocol viewporter):
;;;   - wp_viewporter
;;;   - wp_viewport

(define-module (gliver river viewporter-manager)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (system foreign)
  #:use-module (gliver river connector)
  #:use-module (gliver wayland client)
  #:use-module (gliver wayland gen viewporter)
  #:use-module (gliver core-base)
  #:declarative? #f
  #:export (
			*viewporter*
			*viewport-surfaces*
			*viewporter-error-viewport-exists*
			viewporter-on-globals-bind
			viewporter-on-globals-unbind
			viewporter-on-globals-verify
			viewporter-destroy
			viewporter-get-viewport
			viewport-destroy
			viewport-set-source
			viewport-set-destination
			viewport->surface
			surface->viewport
			viewporter-get-for-surface
			viewporter-destroy-for-surface
))

;;; global state
(define-var *viewporter* %null-pointer
			"Proxy for wp_viewporter.")

;; surface tracking tables
;; maps viewport proxy -> surface proxy
(define *viewport-surfaces* (make-hash-table))

;; protocol constants
(define *viewporter-error-viewport-exists*
  WP_VIEWPORTER_ERROR_VIEWPORT_EXISTS)

(define (viewporter-on-globals-bind registry protocol-name object-id version)
  "Bind wp_viewporter global interface from REGISTRY.
Matches PROTOCOL-NAME against `WP_VIEWPORTER_NAME` and binds OBJECT-ID
with VERSION.
Parameters: (REGISTRY PROTOCOL-NAME OBJECT-ID VERSION)
  - REGISTRY: Pointer to wl_registry proxy.
  - PROTOCOL-NAME: Interface name string.
  - OBJECT-ID: Numeric Wayland object ID.
  - VERSION: Maximum interface version supported by compositor."
  (when (string=? protocol-name WP_VIEWPORTER_NAME)
    (log-info "Binding ~a..." protocol-name)
    (set! *viewporter*
          (gliver-wl-registry-bind registry object-id
                                   *wp-viewporter-interface*
                                   (min version 1)))))

(define (viewporter-on-globals-unbind)
  "Unbind/destroy the viewporter and all associated viewport objects.
Parameters: None."
  (for-each (lambda (proxy)
              (viewport-destroy proxy))
            (hash-map->list (lambda (vp _) vp) *viewport-surfaces*))
  (hash-clear! *viewport-surfaces*)

  (when (valid-pointer? *viewporter*)
	(viewporter-destroy *viewporter*)))

(define (viewporter-on-globals-verify)
  "Verify that wp_viewporter global interface is bound.
Parameters: None."
  (if (valid-pointer? *viewporter*)
	  (log-info "Verified wp_viewporter interface is bound successfully.")
	  (log-warn "wp_viewporter interface wasn't bound.")))

;;; wp_viewporter requests
(define (viewporter-destroy proxy-viewporter)
  "Destroy wp_viewporter proxy object.
Parameters: (PROXY-VIEWPORTER)
  - PROXY-VIEWPORTER: Pointer to wp_viewporter proxy."
  (when (and (valid-pointer? proxy-viewporter)
			 (equal? *viewporter* proxy-viewporter))
    (when *log-river-calls*
	  (log-debug "Destroying viewporter proxy: ~a" proxy-viewporter))
	(wp-viewporter-destroy proxy-viewporter)
	(set! *viewporter* %null-pointer)))

(define (viewporter-get-viewport proxy-viewporter proxy-surface)
  "Create a viewport object for a surface.
Parameters: (PROXY-VIEWPORTER PROXY-SURFACE)
  - PROXY-VIEWPORTER: Pointer to wp_viewporter proxy.
  - PROXY-SURFACE: Pointer to wl_surface proxy.
Returns: wp_viewport proxy pointer."
  (when (and (valid-pointer? proxy-viewporter)
			 (valid-pointer? proxy-surface)
			 (equal? *viewporter* proxy-viewporter))
	;; get the cached one if it exists
    (or (surface->viewport proxy-surface)
		;; if not, create a new viewport, cache it, and return it
        (let ((vp (wp-viewporter-get-viewport proxy-viewporter proxy-surface)))
          (when *log-river-calls*
            (log-debug "Creating viewport on ~a: surface=~a" proxy-viewporter proxy-surface))
          (hash-set! *viewport-surfaces* vp proxy-surface)
          vp))))

;;; wp_viewport requests
(define (viewport-destroy proxy-viewport)
  "Destroy wp_viewport proxy object.
Parameters: (PROXY-VIEWPORT)
  - PROXY-VIEWPORT: Pointer to wp_viewport proxy."
  (when (and (valid-pointer? proxy-viewport)
             (hash-ref *viewport-surfaces* proxy-viewport))
    (when *log-river-calls*
	  (log-debug "Destroying viewport proxy: ~a" proxy-viewport))
	(wp-viewport-destroy proxy-viewport)
	(hash-remove! *viewport-surfaces* proxy-viewport)))

(define (viewport-set-source proxy-viewport x y width height)
  "Set the source rectangle for cropping.
Parameters: (PROXY-VIEWPORT X Y WIDTH HEIGHT):
  - PROXY-VIEWPORT: Pointer to wp_viewport proxy.
  - X: Integer source rectangle x.
  - Y: Integer source rectangle y.
  - WIDTH: Integer source rectangle width.
  - HEIGHT: Integer source rectangle height."
  (when (and (valid-pointer? proxy-viewport)
			 (hash-ref *viewport-surfaces* proxy-viewport)
			 (number? x) (number? y)
			 (number? width) (number? height)
			 (or (and (>= x 0)  (>= y 0)  (> width 0)   (> height 0))
				 ;; can be disabled only if all of x, y,
				 ;; width, and height are set to -1
				 (and (= x -1) (= y -1) (= width -1) (= height -1))))
    (wp-viewport-set-source proxy-viewport x y width height)))

(define (viewport-set-destination proxy-viewport width height)
  "Set the surface destination size for scaling.
Parameters: (PROXY-VIEWPORT WIDTH HEIGHT):
  - PROXY-VIEWPORT: Pointer to wp_viewport proxy.
  - WIDTH: Integer surface width.
  - HEIGHT: Integer surface height."
  (when (and (valid-pointer? proxy-viewport)
			 (hash-ref *viewport-surfaces* proxy-viewport)
			 (number? width) (number? height)
			 (or (and (> width 0)   (> height 0))
				 ;; can be disabled only width and height = -1
				 (and (= width -1) (= height -1))))
    (wp-viewport-set-destination proxy-viewport
                                 (inexact->exact (round width))
                                 (inexact->exact (round height)))))

;;; surface tracking helpers
(define (viewport->surface proxy-viewport)
  "Get the wl_surface associated with PROXY-VIEWPORT.
Parameters: (PROXY-VIEWPORT):
  - PROXY-VIEWPORT: Pointer to wp_viewport proxy.
Returns: Pointer to wl_surface or #f."
  (and (valid-pointer? proxy-viewport)
       (hash-ref *viewport-surfaces* proxy-viewport)))

(define (surface->viewport proxy-surface)
  "Get the wp_viewport proxy associated with SURFACE.
Parameters: (PROXY-SURFACE:)
  - PROXY-SURFACE: Pointer to wl_surface proxy.
Returns: Pointer to wp_viewport or #f."
  (and (valid-pointer? proxy-surface)
       (hash-fold (lambda (vp s acc)
                    (if (equal? s proxy-surface) vp acc))
                  #f
                  *viewport-surfaces*)))

(define (viewporter-get-for-surface surface)
  "Create or retrieve the wp_viewport object for SURFACE.
Parameters: (SURFACE)
  - SURFACE: Pointer to wl_surface proxy.
Returns: wp_viewport proxy pointer or #f."
  (when (and (valid-pointer? surface)
             (valid-pointer? *viewporter*))
    (or (surface->viewport surface)
        (viewporter-get-viewport *viewporter* surface))))

(define (viewporter-destroy-for-surface surface)
  "Destroy the wp_viewport object associated with SURFACE.
Parameters: (SURFACE)
  - SURFACE: Pointer to wl_surface proxy."
  (and-let* ((   (valid-pointer? surface))
			 (vp (surface->viewport surface))
			 (   (valid-pointer? vp)))
    (viewport-destroy vp)))

;; handle river initialization steps
(gliver-hook-add! *gliver-globals-bind-hook* 'viewporter-on-globals-bind)
(gliver-hook-add! *gliver-globals-unbind-hook* 'viewporter-on-globals-unbind)
(gliver-hook-add! *gliver-globals-verify-hook* 'viewporter-on-globals-verify)
