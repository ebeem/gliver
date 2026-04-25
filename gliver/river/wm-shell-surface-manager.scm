;;; gliver/river/wm-shell-surface-manager.scm --- River compositor integration
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; Files under the river directory should only act as a wrapper
;;; to river protocol, they should not take action nor import any of
;;; gliver's files or utilities except for logging and configuration
;;;
;;; The window manager might use a shell surface to display a status bar,
;;; background image, desktop notifications, launcher, desktop menu, or
;;; whatever else it wants.

(define-module (gliver river wm-shell-surface-manager)
  #:use-module (gliver core types)
  #:use-module (gliver core logs)
  #:use-module (gliver core hooks)
  #:use-module (gliver wayland client)
  #:use-module (gliver wayland gen river-window-management-v1)
  #:use-module (system foreign)
  #:export (
			wm-shell-surface-destroy!
			wm-shell-surface-node-get!
			wm-shell-surface-sync-next-commit!
))

;;; shell-surface requests
(define (wm-shell-surface-destroy! proxy-shell-surface)
  "Destroy a shell-surface, this means everything was cleared from
client side and the server should also clear everything.
This most likely should be used internally only, and it
will be automatically managed and called when needed."
  (when proxy-shell-surface
    (log-debug "destroying shell-surface proxy: ~a" proxy-shell-surface)
	  (river-shell-surface-v1-destroy proxy-shell-surface)))

(define (wm-shell-surface-node-get! proxy-shell-surface)
  "Get the node in the render list corresponding to the shell surface.
Should only be sent once per shell-surface"
  (when proxy-shell-surface
    (log-debug "getting node of shell-surface ~a" proxy-shell-surface)
    (river-shell-surface-v1-get-node proxy-shell-surface)))

(define (wm-shell-surface-sync-next-commit! proxy-shell-surface)
  "Request that synchronize application of the next wl_surface.commit request on the
shell surface with rest of the rendering state atomically applied with
the next river_window_manager_v1.render_finish request.
Must be called in a ~render_sequence~."
  (when proxy-shell-surface
    (log-debug "syncing shell-surface ~a" proxy-shell-surface)
    (river-shell-surface-v1-sync-next-commit proxy-shell-surface)))

