;;; gliver/river/wm-shell-surface-manager.scm --- River window management shell surface protocol integration
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module manages the shell surfaces like statusbars or palettes
;;; It's useful because it exposes the surface node which can be used
;;; to manage the ordering of windows/shell surface
;;;
;;; It handles:
;;;   - Binding river_shell_surface_v1 and exposing interface requests
;;;
;;; This module implements the river interfaces (protocol river_window_manager_v1):
;;;   - river_shell_surface_v1

(define-module (gliver river wm-shell-surface-manager)
  #:use-module (gliver core-base)
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
  "Destroy river_shell_surface_v1 proxy object, this means everything was
cleared from client side and the server should also clear everything.
Parameters: (PROXY-SHELL-SURFACE)
  - PROXY-SHELL-SURFACE: Pointer to river_shell_surface_v1 proxy."
  (when proxy-shell-surface
    (when *log-river-calls*
	  (log-debug "destroying shell-surface proxy: ~a" proxy-shell-surface))
	(river-shell-surface-v1-destroy proxy-shell-surface)))

(define (wm-shell-surface-node-get! proxy-shell-surface)
  "Get node corresponding to the shell surface (called once per surface).
Parameters: (PROXY-SHELL-SURFACE)
  - PROXY-SHELL-SURFACE: Pointer to river_shell_surface_v1 proxy."
  (when proxy-shell-surface
    (when *log-river-calls*
	  (log-debug "getting node of shell-surface ~a" proxy-shell-surface))
    (river-shell-surface-v1-get-node proxy-shell-surface)))

(define (wm-shell-surface-sync-next-commit! proxy-shell-surface)
  "Synchronize application of the next wl_surface commit
with the next render sequence.
Must be called in a render sequence.
Parameters: (PROXY-SHELL-SURFACE)
  - PROXY-SHELL-SURFACE: Pointer to river_shell_surface_v1 proxy."
  (when proxy-shell-surface
    (when *log-river-calls*
	  (log-debug "syncing shell-surface ~a" proxy-shell-surface))
    (river-shell-surface-v1-sync-next-commit proxy-shell-surface)))
