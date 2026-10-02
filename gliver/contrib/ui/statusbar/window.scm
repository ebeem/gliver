;;; gliver/contrib/ui/statusbar/window.scm --- Active window title statusbar module
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver contrib ui statusbar window)
  #:use-module (ice-9 format)
  #:use-module (gliver core)
  #:use-module (gliver contrib ui statusbar base)
  #:declarative? #f
  #:export (
			make-module-window
))

(define (%truncate-string str max-len)
  (if (and max-len (> (string-length str) max-len))
      (string-append (substring str 0 (max 0 (- max-len 1))) "…")
      str))

(define* (make-module-window #:key
                             (id 'window)
                             (section 'left)
                             (icon "")
                             (max-length 45)
                             (empty-text "")
                             (hooks (list *window-focused-hook*
                                          *window-unfocused-hook*
                                          *window-title-changed-hook*))
                             (bg-color *statusbar-bg-color*)
                             (fg-color *theme-fg-main*)
                             (border-radius 6)
                             (padding-x 10)
                             (padding-y 4))
  "Create an Active Window Title module."

  (define (%update module output)
    (let* ((win (window-current))
           (raw-title (and win (or (window-title win) (window-app-id win))))
           (title (or raw-title empty-text)))
      (if (and title (> (string-length title) 0))
          (let ((truncated (%truncate-string title max-length)))
            (statusbar-module-text-set!
             module
             (if icon (format #f "~a ~a" icon truncated) truncated))
            (statusbar-module-tooltip-set! module title)
            (statusbar-module-visible?-set! module #t))
          (begin
            (statusbar-module-text-set! module "")
            (statusbar-module-visible?-set! module (> (string-length empty-text) 0))))))

  (make-statusbar-module
   #:id id
   #:name "Window"
   #:section section
   #:interval #f  ;; event-driven
   #:hooks hooks
   #:bg-color bg-color
   #:fg-color fg-color
   #:border-radius border-radius
   #:padding-x padding-x
   #:padding-y padding-y
   #:update-fn %update))

(statusbar-register-module! 'window make-module-window)
