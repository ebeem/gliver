;;; gliver/contrib/commands/window.scm --- Window commands for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver contrib commands window)
  #:use-module (gliver core)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-2)
  #:declarative? #f
  #:export (
			window-focus-next
			window-focus-prev
			window-focus-last
			window-all-list
			window-workspace-list
			window-container-list
			window-kill
			window-float-toggle
			window-fullscreen-toggle
			window-swap
			window-mark
			window-workspace-move
			window-container-move-direction
			window-container-move-left
			window-container-move-right
			window-container-move-up
			window-container-move-down
			window-pull-by-number
			window-properties-show
))

;;; window commands
(define-command (window-focus-next)
  "Focus the next window in the current container."
  (and-let* ((win (window-current))
			 (next (window-next win)))
    (when next
      (window-focus! next))))

(define-command (window-focus-prev)
  "Focus the previous window in the current container."
  (and-let* ((win (window-current))
			 (prev (window-prev win)))
    (when prev
      (window-focus! prev))))

(define-command (window-focus-last)
  "Switch to the previously focused window within the current container."
  (and-let* ((win (window-current))
			 (container (window-container win))
			 (prev (container-window-previous container)))
	(when prev
      (window-focus! prev))))

(define-command (window-all-list)
  "Show a list of all available windows."
  (let* ((windows (manager-windows *manager*)))
    (if (null? windows)
        (log-debug "No windows.")
		;; NOTE: there should be a call for dmenu prompt
        (log-debug "Show windows."))))

(define-command (window-workspace-list)
  "Show a list of current workspace windows."
  (and-let* ((workspace (workspace-current))
			 (windows (workspace-windows workspace)))
    (if (null? windows)
        (log-debug "No windows.")
		;; NOTE: there should be a call for dmenu prompt
        (log-debug "Show windows."))))

(define-command (window-container-list)
  "Show a list of current container windows."
  (and-let* ((container (container-current))
			 (windows (container-windows container)))
    (if (null? windows)
        (log-debug "No windows.")
		;; NOTE: there should be a call for dmenu prompt
        (log-debug "Show windows."))))

(define-command (window-kill)
  "Close the current window."
  (let ((win (window-current)))
    (if win
        (begin
          (log-info "Killing window: ~a" (window-title win))
          (window-close! win)
          (log-debug "Closed: ~a" (window-title win)))
        (log-debug "No current window."))))

;; TODO
(define-command (window-float-toggle)
  "Toggle the current window between tiled and floating."
  (log-warn "Not implemented yet!"))

(define-command (window-fullscreen-toggle)
  "Toggle fullscreen for the current window."
  (let ((win (window-current)))
    (when win
      (if (window-fullscreen? win)
          (window-fullscreen-exit! win)
          (window-fullscreen! win (window-output win))))))

;; TODO
(define-command (window-swap)
  "Swap windows between current container and another."
  (log-warn "Not implemented yet!"))

(define-command (window-mark)
  "Toggle mark on the current window."
  (log-warn "Not implemented yet!"))
  ;; (let ((win (window-current)))
  ;;   (when win
  ;;     (window-marked-set! win (not (window-marked? win)))
  ;;     (log-debug "~a ~a"
  ;;              (if (window-marked? win) "Marked" "Unmarked")
  ;;              (window-title win)))))

(define-command (window-workspace-move name)
  #:interactive (string)
  "Move the current window to workspace NAME."
  (let ((win (window-current))
        (target (and name (workspace-find-by-name name))))
    (cond
     ((not win) (log-debug "No current window."))
     ((not target) (log-debug "Workspace not found: ~a" name))
     (else
      (window-move-to-workspace! win target)
      (log-debug "Moved to ~a." name)))))

(define-command (window-container-move-direction dir)
  #:interactive (string)
  "Move the current window to the container in direction DIR."
  (let* ((workspace (workspace-current))
         (container (container-current))
         (win (window-current))
         (target (and workspace container (container-in-direction dir container workspace))))
    (if (and win target)
        (begin
          (window-move-to-container! win target)
          ;(workspace-container-current-set! workspace target)
          (log-debug "Moved to container ~a" (container-id target)))
        (log-debug "Cannot move."))))

(define-command (window-container-move-left)
  "Focus the container in the left direction."
  (window-container-move-direction 'left))

(define-command (window-container-move-right)
  "Focus the container in the left direction."
  (window-container-move-direction 'right))

(define-command (window-container-move-up)
  "Focus the container in the left direction."
  (window-container-move-direction 'up))

(define-command (window-container-move-down)
  "Focus the container in the left direction."
  (window-container-move-direction 'down))

(define-command (window-pull-by-number n)
  #:interactive (integer)
  "Pull window N into the current container."
  (let ((win (window-find-by-id n))
        (container (container-current)))
    (when (and win container)
      (window-move-to-container! win container))))

(define-command (window-properties-show)
  "Show properties of the current window."
  (let ((win (window-current)))
    (if win
        (log-debug "id=~a title=~s app-id=~s class=~s float=~a"
                 (window-id win)
                 (window-title win)
                 (window-app-id win)
                 (window-class win)
                 (window-floating? win))
        (log-debug "No current window."))))
