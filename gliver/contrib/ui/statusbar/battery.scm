;;; gliver/contrib/ui/statusbar/battery.scm --- Battery statusbar module
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver contrib ui statusbar battery)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (gliver core config)
  #:use-module (gliver contrib ui statusbar base)
  #:declarative? #f
  #:export (
			make-module-battery
))

(define (%find-battery-dir)
  (find file-exists?
        '("/sys/class/power_supply/BAT0"
          "/sys/class/power_supply/BAT1"
          "/sys/class/power_supply/BAT2")))

(define (%battery-icon cap charging?)
  "Returns battery icon based on capacity CAP."
  (if charging?
      "󰂄"
      (cond
       ((>= cap 90) "󰁹")
       ((>= cap 70) "󰂀")
       ((>= cap 50) "󰁾")
       ((>= cap 30) "󰁼")
       ((>= cap 15) "󰁻")
       (else "󰂎"))))

(define (%read-file-trimmed path)
  "Read the first line of PATH and return it with whitespace stripped, or #f on error."
  (catch #t
    (lambda ()
      (call-with-input-file path
        (lambda (port)
          (let ((line (read-line port)))
            (if (eof-object? line) "" (string-trim-both line))))))
    (lambda _ #f)))

(define* (make-module-battery #:key
                              (id 'battery)
                              (section 'right)
                              (interval 10)
                              (hide-if-missing? #t)
                              (bg-color *statusbar-bg-color*)
                              (fg-color *statusbar-fg-color*)
                              (fg-warning *theme-yellow*)
                              (fg-critical *theme-red*)
                              (fg-charging *theme-green*)
                              (border-radius 6)
                              (padding-x 10)
                              (padding-y 4))
  "Create a Battery status module."
  (define (%resolve-fg-color cap charging?)
    "Returns foreground color based on capacity CAP"
    (cond
     (charging? fg-charging)
     ((<= cap 15) fg-critical)
     ((<= cap 30) fg-warning)
     (else fg-color)))

  (define (%hide! module)
    (statusbar-module-visible?-set! module #f)
    (statusbar-module-text-set! module ""))

  (define (%show-ac! module)
    (statusbar-module-visible?-set! module #t)
    (statusbar-module-text-set! module "󰚥 AC")
    (statusbar-module-fg-color-set! module fg-color))

  (define (%show-battery! module cap status charging?)
    (let ((icon (%battery-icon cap charging?))
          (fg   (%resolve-fg-color cap charging?)))
      (statusbar-module-visible?-set! module #t)
      (statusbar-module-text-set! module (format #f "~a ~a%" icon cap))
      (statusbar-module-fg-color-set! module fg)
      (statusbar-module-tooltip-set! module (format #f "Battery: ~a% (~a)" cap status))))

  (define (%update module output)
    (let ((bdir (%find-battery-dir)))
      (cond
       ;; no battery found
       ((not bdir)
        (if hide-if-missing? (%hide! module) (%show-ac! module)))

       ;; battery directory found, read metrics
       (else
        (let* ((cap-str (%read-file-trimmed (string-append bdir "/capacity")))
               (stat-str (%read-file-trimmed (string-append bdir "/status")))
               (cap (and cap-str (string->number cap-str)))
               (status (or stat-str "Discharging"))
               (charging? (string-ci=? status "Charging")))
          (if cap
              (%show-battery! module cap status charging?)
              (%hide! module)))))))

  (make-statusbar-module
   #:id id
   #:name "Battery"
   #:section section
   #:interval interval
   #:bg-color bg-color
   #:fg-color fg-color
   #:border-radius border-radius
   #:padding-x padding-x
   #:padding-y padding-y
   #:update-fn %update))

(statusbar-register-module! 'battery make-module-battery)
