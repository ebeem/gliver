;;; gliver/river/wm-decoration-manager.scm --- River compositor integration
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; Files under the river directory should only act as a wrapper
;;; to river protocol, they should not take action nor import any of
;;; gliver's files or utilities except for logging and configuration
;;;
;;; The rendering order of windows with decorations is follows:
;;;
;;; 1. Decorations created with get_decoration_below at the bottom
;;; 2. Window content
;;; 3. Borders configured with river_window_v1.set_borders
;;; 4. Decorations created with get_decoration_above at the top

(define-module (gliver river wm-decoration-manager)
  #:use-module (gliver core types)
  #:use-module (gliver core logs)
  #:use-module (gliver core hooks)
  #:use-module (gliver wayland client)
  #:use-module (gliver wayland gen river-window-management-v1)
  #:use-module (system foreign)
  #:export (
			wm-decoration-destroy!
			wm-docoration-offset-set!
			wm-decoration-sync-next-commit!
))

;;; decoration requests
(define (wm-decoration-destroy! proxy-decoration)
  "Destroy a decoration, this means everything was cleared from
client side and the server should also clear everything.
This most likely should be used internally only, and it
will be automatically managed and called when needed."
  (when proxy-decoration
    (log-debug "destroying decoration proxy: ~a" proxy-decoration)
	  (river-decoration-v1-destroy proxy-decoration)))

(define (wm-docoration-offset-set! proxy-docoration x y)
  "Set the offset of the decoration surface from the top left corner
Must be called in a ~render_sequence~."
  (when proxy-docoration
    (log-debug "setting offset of docoration ~a to ~ax~a" proxy-docoration x y)
    (river-docoration-v1-set-offset proxy-docoration x y)))

(define (wm-decoration-sync-next-commit! proxy-decoration)
  "Request that synchronize application of the next wl_surface.commit request on the
shell surface with rest of the rendering state atomically applied with
the next river_window_manager_v1.render_finish request.
Must be called in a ~render_sequence~."
  (when proxy-decoration
    (log-debug "syncing decoration ~a" proxy-decoration)
    (river-decoration-v1-sync-next-commit proxy-decoration)))

