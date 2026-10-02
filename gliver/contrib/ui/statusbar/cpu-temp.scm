;;; gliver/contrib/ui/statusbar/cpu-temp.scm --- CPU temperature statusbar module
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver contrib ui statusbar cpu-temp)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (gliver core config)
  #:use-module (gliver contrib ui statusbar base)
  #:declarative? #f
  #:export (
			make-module-cpu-temp
))

(define (%assoc-ref-pred alist pred)
  (let ((entry (find (lambda (pair) (pred (car pair))) alist)))
    (and entry (cdr entry))))

(define (%read-file-trimmed path)
  "Read the first line of PATH and return it with whitespace stripped, or #f on error."
  (catch #t
    (lambda ()
      (call-with-input-file path
        (lambda (port)
          (let ((line (read-line port)))
            (if (eof-object? line) "" (string-trim-both line))))))
    (lambda _ #f)))

(define (%safe-read-file path)
  (and (file-exists? path) (%read-file-trimmed path)))

(define (%list-sysfs-entries dir prefix)
  (if (file-exists? dir)
      (or (scandir dir (lambda (f) (string-prefix? prefix f))) '())
      '()))

(define (%get-hwmon-inputs dir)
  "Return sorted list of (label . full-path) for temperature inputs in DIR."
  (let* ((files (or (scandir dir (lambda (f) (and (string-prefix? "temp" f)
                                                  (string-suffix? "_input" f))))
                    '()))
         (sorted (sort files (lambda (a b)
                               (< (or (string->number (string-filter char-numeric? a)) 9999)
                                  (or (string->number (string-filter char-numeric? b)) 9999))))))
    (map (lambda (f)
           (let* ((prefix (substring f 0 (- (string-length f) 6)))
                  (lbl    (%safe-read-file (string-append dir "/" prefix "_label"))))
             (cons (or lbl "") (string-append dir "/" f))))
         sorted)))

(define %known-cpu-drivers
  '("coretemp" "k10temp" "zenpower" "cpu_thermal" "cpu-thermal" "soc_thermal"))

(define (%find-dedicated-hwmon-sensor hwmon-base entries)
  "Check known dedicated CPU drivers (Intel coretemp, AMD k10temp/zenpower, ARM SoC)."
  (any
   (lambda (d)
     (let* ((dir  (string-append hwmon-base "/" d))
            (name (%safe-read-file (string-append dir "/name"))))
       (and (member name %known-cpu-drivers)
            (let ((pairs (%get-hwmon-inputs dir)))
              (or (%assoc-ref-pred pairs (lambda (l) (or (string-prefix-ci? "Package id" l)
                                                         (string-ci=? l "Tdie"))))
                  (%assoc-ref-pred pairs (lambda (l) (string-ci=? l "Tctl")))
                  (%assoc-ref-pred pairs (lambda (l) (and (string-contains-ci l "cpu")
                                                          (not (string-contains-ci l "core")))))
                  (and (pair? pairs) (cdar pairs)))))))
   entries))

(define (%find-generic-hwmon-sensor hwmon-base entries)
  "Inspect any generic hwmon driver for package, tctl, tdie, or CPU labels."
  (any
   (lambda (d)
     (let* ((dir   (string-append hwmon-base "/" d))
            (pairs (%get-hwmon-inputs dir)))
       (%assoc-ref-pred
        pairs
        (lambda (l)
          (or (string-contains-ci l "package")
              (string-ci=? l "tctl")
              (string-ci=? l "tdie")
              (and (string-contains-ci l "cpu")
                   (not (string-contains-ci l "core"))))))))
   entries))

(define (%find-thermal-zone-sensor)
  "Fallback to ACPI and SoC thermal zone sysfs entries."
  (let* ((tz-base    "/sys/class/thermal")
         (tz-entries (%list-sysfs-entries tz-base "thermal_zone"))
         (tz-pairs   (map (lambda (tz)
                            (cons (or (%safe-read-file (string-append tz-base "/" tz "/type")) "")
                                  (string-append tz-base "/" tz "/temp")))
                          tz-entries)))
    (or (%assoc-ref-pred tz-pairs (lambda (t) (or (string-contains-ci t "pkg")
                                                  (string-contains-ci t "x86_pkg"))))
        (%assoc-ref-pred tz-pairs (lambda (t) (or (string-contains-ci t "cpu-thermal")
                                                  (string-contains-ci t "cpu_thermal"))))
        (%assoc-ref-pred tz-pairs (lambda (t) (string-contains-ci t "acpitz")))
        (let ((zone0 "/sys/class/thermal/thermal_zone0/temp"))
          (and (file-exists? zone0) zone0)))))

(define (%find-cpu-temperature-sensor)
  "Auto-detect CPU package temperature sysfs input path like btop."
  (false-if-exception
   (let* ((hwmon-base "/sys/class/hwmon")
          (hw-entries (%list-sysfs-entries hwmon-base "hwmon")))
     (or (%find-dedicated-hwmon-sensor hwmon-base hw-entries)
         (%find-generic-hwmon-sensor hwmon-base hw-entries)
         (%find-thermal-zone-sensor)))))

(define* (make-module-cpu-temp #:key
                               (id 'cpu-temp)
                               (section 'right)
                               (interval 2)
                               (sensor-path #f)
                               (unit 'celsius)
                               (icon "")
                               (dynamic-icons? #t)
                               (format-template #f)
                               (warning-threshold 70)
                               (critical-threshold 85)
                               (bg-color *statusbar-bg-color*)
                               (fg-color *theme-fg-main*)
                               (fg-warning *theme-yellow*)
                               (fg-critical *theme-red*)
                               (border-radius 6)
                               (padding-x 10)
                               (padding-y 4)
                               (on-click-cmd #f))
  "Create a CPU Temperature statusbar module.
Monitors CPU package/die temperatures via Linux sysfs hwmon or thermal zones."

  (define (%pick-temp-icon temp-c)
    (if (not dynamic-icons?)
        icon
        (cond
         ((>= temp-c critical-threshold) "")
         ((>= temp-c warning-threshold)  "")
         ((>= temp-c 60)                 "")
         ((>= temp-c 45)                 "")
         (else                           ""))))

  (define (%resolve-fg-color temp-c)
    (cond
     ((>= temp-c critical-threshold) fg-critical)
     ((>= temp-c warning-threshold)  fg-warning)
     (else fg-color)))

  (define (%render-module! module temp-c path)
    (let* ((fahrenheit? (eq? unit 'fahrenheit))
           (disp-temp   (if fahrenheit? (+ (* temp-c 1.8) 32.0) temp-c))
           (unit-str    (if fahrenheit? "°F" "°C"))
           (temp-int    (inexact->exact (round disp-temp)))
           (text        (if format-template
                            (format #f format-template temp-int)
                            (format #f "~a ~a~a" (%pick-temp-icon temp-c) temp-int unit-str))))
      (statusbar-module-text-set! module text)
      (statusbar-module-fg-color-set! module (%resolve-fg-color temp-c))
      (statusbar-module-tooltip-set!
       module
       (format #f "CPU Temperature: ~,1f~a\nSensor: ~a" disp-temp unit-str path))
      (statusbar-module-state-set!
       module
       `((sensor-path . ,path) (temp-c . ,temp-c)))))

  (define (%update module output)
    (let* ((state (or (statusbar-module-state module) '()))
           (path  (or (assoc-ref state 'sensor-path)
                      sensor-path
                      (%find-cpu-temperature-sensor)))
           (raw   (and path (%safe-read-file path)))
           (milli (and raw (string->number raw))))
      (when (and path milli)
        (%render-module! module (/ milli 1000.0) path))))

  (define (%handle-click button x y module output custom-data)
    ;; 272 means left click
    (when (= button 272)
      (let ((cmd (or on-click-cmd
                     (format #f "~a -e btop &" *terminal*))))
        (system cmd))))

  (make-statusbar-module
   #:id id
   #:name "CPU Temperature"
   #:section section
   #:interval interval
   #:format-template (or format-template "~a ~a°C")
   #:icon icon
   #:bg-color bg-color
   #:fg-color fg-color
   #:border-radius border-radius
   #:padding-x padding-x
   #:padding-y padding-y
   #:update-fn %update
   #:on-click %handle-click))

(statusbar-register-module! 'cpu-temp make-module-cpu-temp)
