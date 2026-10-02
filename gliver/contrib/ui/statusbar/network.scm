;;; gliver/contrib/ui/statusbar/network.scm --- Network statusbar module
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver contrib ui statusbar network)
  #:use-module (ice-9 format)
  #:use-module (gliver core config)
  #:use-module (gliver contrib ui statusbar base)
  #:declarative? #f
  #:export (
			make-module-network
))

(define (%query-default-route)
  (statusbar-execute-shell-command "ip -o -4 route show to default 2>/dev/null | awk '{print $5}'"))

(define* (make-module-network #:key
                              (id 'network)
                              (section 'right)
                              (interval 5)
                              (bg-color *statusbar-bg-color*)
                              (fg-color *theme-fg-main*)
                              (border-radius 6)
                              (padding-x 10)
                              (padding-y 4))
  "Create a Network interface status module."

  (define (%update module output)
    (let ((route (%query-default-route)))
      (if (and route (> (string-length route) 0))
          (let* ((iface route)
                 (is-wifi? (or (string-prefix? "wl" iface) (string-prefix? "wifi" iface)))
                 (icon (if is-wifi? "󰖩" "󰈀")))
            (statusbar-module-text-set! module (format #f "~a ~a" icon iface))
            (statusbar-module-tooltip-set! module (format #f "Interface: ~a" iface)))
          (begin
            (statusbar-module-text-set! module "󰖪 Offline")
            (statusbar-module-tooltip-set! module "Network: No default route")))))

  (make-statusbar-module
   #:id id
   #:name "Network"
   #:section section
   #:interval interval
   #:bg-color bg-color
   #:fg-color fg-color
   #:border-radius border-radius
   #:padding-x padding-x
   #:padding-y padding-y
   #:update-fn %update))

(statusbar-register-module! 'network make-module-network)
