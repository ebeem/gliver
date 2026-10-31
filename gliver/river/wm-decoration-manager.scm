;;; gliver/river/wm-decoration-manager.scm --- River window management decoration protocol integration
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module manages the rendering order of window decorations, which follows:
;;; 1. Decorations created with get_decoration_below at the bottom
;;; 2. Window content
;;; 3. Borders configured with river_window_v1.set_borders
;;; 4. Decorations created with get_decoration_above at the top
;;;
;;; It handles:
;;;   - Binding river_decoration_v1 and exposing interface requests
;;;
;;; This module implements the river interfaces (protocol river_window_manager_v1):
;;;   - river_decoration_v1

(define-module (gliver river wm-decoration-manager)
  #:use-module (gliver core-base)
  #:use-module (gliver wayland client)
  #:use-module (gliver wayland gen river-window-management-v1)
  #:use-module (system foreign)
  #:export (
			wm-decoration-destroy!
			wm-decoration-offset-set!
			wm-decoration-sync-next-commit!
))

;;; decoration requests
(define (wm-decoration-destroy! proxy-decoration)
  "Destroy a decoration, this means everything was cleared from
client side and the server should also clear everything.
Parameters: (PROXY-DECORATION)
  - PROXY-DECORATION: Pointer to river_decoration_v1 proxy."
  (when proxy-decoration
    (when *log-river-calls*
	  (log-debug "Destroying decoration proxy: ~a" proxy-decoration))
	(river-decoration-v1-destroy proxy-decoration)))

(define (wm-decoration-offset-set! proxy-decoration x y)
  "Set the offset of the decoration surface from the top-left corner.
Must be called in a render sequence.
Parameters: (PROXY-DECORATION X Y)
  - PROXY-DECORATION: Pointer to river_decoration_v1 proxy.
  - X: Horizontal offset in compositor coordinates.
  - Y: Vertical offset in compositor coordinates."
  (when proxy-decoration
    (when *log-river-calls*
	  (log-debug "Setting offset of decoration ~a to ~ax~a" proxy-decoration x y))
    (river-decoration-v1-set-offset proxy-decoration x y)))

(define (wm-decoration-sync-next-commit! proxy-decoration)
  "Synchronize application of the next wl_surface commit
with the next render sequence finish.
Must be called in a render sequence.
Parameters: (PROXY-DECORATION)
  - PROXY-DECORATION: Pointer to river_decoration_v1 proxy."
  (when proxy-decoration
    (when *log-river-calls*
	  (log-debug "Syncing decoration ~a" proxy-decoration))
    (river-decoration-v1-sync-next-commit proxy-decoration)))

;;; NOTE: this interface has no events/listeners
