;;; gliver/contrib/ui/statusbar/disk.scm --- Disk usage statusbar module
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver contrib ui statusbar disk)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (gliver core config)
  #:use-module (gliver contrib ui statusbar base)
  #:declarative? #f
  #:export (
			make-module-disk
))

(define (%parse-df-output res)
  "Parse df output string into (cons avail use-pct) or #f."
  (and res
       (let ((parts (filter (lambda (s) (> (string-length s) 0))
                            (string-split res #\space))))
         (and (>= (length parts) 5)
              (cons (list-ref parts 3) (list-ref parts 4))))))

(define* (make-module-disk #:key
                           (id 'disk)
                           (section 'right)
                           (interval 30)
                           (mountpoint "/")
                           (icon "󰋊")
                           (bg-color *statusbar-bg-color*)
                           (fg-color *theme-fg-main*)
                           (border-radius 6)
                           (padding-x 10)
                           (padding-y 4))
  "Create a Disk space usage module for MOUNTPOINT."

  (define (%update module output)
    (let* ((res (statusbar-execute-shell-command (format #f "df -h ~s | tail -n 1" mountpoint)))
           (parsed (%parse-df-output res)))
      (if parsed
          (let ((avail (car parsed))
                (use-pct (cdr parsed)))
            (statusbar-module-text-set!
             module
             (format #f "~a ~a free (~a)" icon avail use-pct))
            (statusbar-module-tooltip-set!
             module
             (format #f "Disk: ~a mounted on ~a" use-pct mountpoint)))
          (if res
              (statusbar-module-text-set! module (format #f "~a ~a" icon mountpoint))
              (statusbar-module-text-set! module "")))))

  (make-statusbar-module
   #:id id
   #:name "Disk"
   #:section section
   #:interval interval
   #:bg-color bg-color
   #:fg-color fg-color
   #:border-radius border-radius
   #:padding-x padding-x
   #:padding-y padding-y
   #:update-fn %update))

(statusbar-register-module! 'disk make-module-disk)
