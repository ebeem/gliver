;;; gliver/contrib/ui/statusbar/date.scm --- Date statusbar module
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver contrib ui statusbar date)
  #:use-module (gliver core config)
  #:use-module (gliver contrib ui statusbar base)
  #:declarative? #f
  #:export (
			make-module-date
))

(define* (make-module-date #:key
                           (id 'date)
                           (section 'center)
                           (interval 1)
                           (formats '(" %a %b %d   %H:%M"
                                      " %H:%M:%S"
                                      " %Y-%m-%d   %H:%M:%S"))
                           (bg-color *statusbar-bg-color*)
                           (fg-color *theme-fg-main*)
                           (border-radius 6)
                           (padding-x 12)
                           (padding-y 4))
  "Create a Date & Time module."

  (define (%update module output)
    (let* ((state (statusbar-module-state module))
           (idx (or (assoc-ref state 'fmt-index) 0))
           (fmt (list-ref formats (modulo idx (length formats))))
           (now (localtime (current-time)))
           (formatted (strftime fmt now)))
      (statusbar-module-text-set! module formatted)
      (statusbar-module-tooltip-set! module (strftime "%A, %B %d, %Y (%Z)" now))))

  (define (%handle-click button x y module output custom-data)
    (when (= button 272)
      (let* ((state (statusbar-module-state module))
             (idx (or (assoc-ref state 'fmt-index) 0))
             (next-idx (modulo (+ idx 1) (length formats))))
        (statusbar-module-state-set! module (assq-set! state 'fmt-index next-idx))
        (%update module output))))

  (make-statusbar-module
   #:id id
   #:name "Date"
   #:section section
   #:interval interval
   #:bg-color bg-color
   #:fg-color fg-color
   #:border-radius border-radius
   #:padding-x padding-x
   #:padding-y padding-y
   #:update-fn %update
   #:on-click %handle-click))

(statusbar-register-module! 'date make-module-date)
