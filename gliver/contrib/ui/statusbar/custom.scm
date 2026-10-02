;;; gliver/contrib/ui/statusbar/custom.scm --- Custom script/procedure statusbar module
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver contrib ui statusbar custom)
  #:use-module (ice-9 format)
  #:use-module (gliver core config)
  #:use-module (gliver contrib ui statusbar base)
  #:declarative? #f
  #:export (
			make-module-custom
))

(define* (make-module-custom #:key
                             (id (gensym "custom-"))
                             (name "Custom")
                             (section 'right)
                             (interval 5)
                             (hooks '())
                             (icon #f)
                             (exec #f)         ;; shell command string or (lambda () ...)
                             (format-template "~a")
                             (bg-color *statusbar-bg-color*)
                             (fg-color *theme-fg-main*)
                             (border-radius 6)
                             (padding-x 10)
                             (padding-y 4)
                             (on-click #f))
  "Create a customizable module that runs a shell command or Guile procedure."

  (define (%update module output)
    (let ((val (cond
                ((string? exec) (statusbar-execute-shell-command exec))
                ((procedure? exec) (catch #t (lambda () (exec)) (lambda _ #f)))
                (else ""))))
      (when val
        (let ((str (cond
                    ((string? format-template) (format #f format-template val))
                    ((procedure? format-template) (format-template val))
                    (else (format #f "~a" val)))))
          (statusbar-module-text-set!
           module
           (if icon (format #f "~a ~a" icon str) str))
          (statusbar-module-tooltip-set! module (format #f "~a: ~a" name val))))))

  (define (%handle-click button x y module output custom-data)
    (when (and on-click (= button 272))
      (cond
       ((string? on-click) (system (format #f "~a &" on-click)))
       ((procedure? on-click) (on-click button x y module output custom-data)))))

  (make-statusbar-module
   #:id id
   #:name name
   #:section section
   #:interval interval
   #:hooks hooks
   #:bg-color bg-color
   #:fg-color fg-color
   #:border-radius border-radius
   #:padding-x padding-x
   #:padding-y padding-y
   #:update-fn %update
   #:on-click %handle-click))
