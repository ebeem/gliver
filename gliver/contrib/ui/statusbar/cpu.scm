;;; gliver/contrib/ui/statusbar/cpu-temp.scm --- CPU utilization statusbar module
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver contrib ui statusbar cpu)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (gliver core config)
  #:use-module (gliver contrib ui statusbar base)
  #:declarative? #f
  #:export (
			make-module-cpu
))

(define (%read-cpu-counters)
  "Parse the aggregated 'cpu' line from /proc/stat. Returns (total . idle-all) or #f."
  (catch #t
    (lambda ()
      (call-with-input-file "/proc/stat"
        (lambda (port)
          (let scan ()
            (let ((line (read-line port)))
              (cond
               ((eof-object? line) #f)
               ((string-prefix? "cpu " line)
                (let ((nums (filter-map string->number (string-split (substring line 4) #\space))))
                  (match nums
                    ((user nice sys idle . rest)
                     (let* ((iowait   (if (pair? rest) (car rest) 0))
                            (idle-all (+ idle iowait))
                            (total    (apply + nums)))
                       (cons total idle-all)))
                    (_ #f))))
               (else (scan))))))))
    (lambda _ #f)))

(define* (make-module-cpu #:key
                          (id 'cpu)
                          (section 'right)
                          (interval 2)
                          (icon "")
                          (format-template " ~a%")
                          (warning-threshold 70)
                          (critical-threshold 90)
                          (bg-color *statusbar-bg-color*)
                          (fg-color *theme-fg-main*)
                          (fg-warning *theme-yellow*)
                          (fg-critical *theme-red*)
                          (border-radius 6)
                          (padding-x 10)
                          (padding-y 4)
                          (on-click-cmd #f))
  "Create a CPU utilization module."

  (define (%resolve-fg-color pct)
    (cond
     ((>= pct critical-threshold) fg-critical)
     ((>= pct warning-threshold)  fg-warning)
     (else fg-color)))

  (define (%render-cpu! module pct total idle-all)
    (statusbar-module-text-set! module (format #f format-template pct))
    (statusbar-module-fg-color-set! module (%resolve-fg-color pct))
    (statusbar-module-tooltip-set! module (format #f "CPU Usage: ~a%" pct))
    (statusbar-module-state-set! module `((prev-total . ,total) (prev-idle . ,idle-all))))

  (define (%update module output)
    (let ((counters (%read-cpu-counters)))
      (when counters
        (let* ((total       (car counters))
               (idle-all    (cdr counters))
               (state       (or (statusbar-module-state module) '()))
               (prev-total  (assoc-ref state 'prev-total))
               (prev-idle   (assoc-ref state 'prev-idle)))
          (if (and prev-total prev-idle)
              (let* ((diff-total (max 1 (- total prev-total)))
                     (diff-idle  (- idle-all prev-idle))
                     (usage      (max 0.0 (min 100.0 (* 100.0 (/ (- diff-total diff-idle) diff-total)))))
                     (pct        (inexact->exact (round usage))))
                (%render-cpu! module pct total idle-all))
              (%render-cpu! module 0 total idle-all))))))

  (define (%handle-click button x y module output custom-data)
    ;; 272 means left mouse button
    (when (= button 272)
      (let ((cmd (or on-click-cmd
                     (format #f "~a -e htop &" *terminal*))))
        (system cmd))))

  (make-statusbar-module
   #:id id
   #:name "CPU"
   #:section section
   #:interval interval
   #:format-template format-template
   #:icon icon
   #:bg-color bg-color
   #:fg-color fg-color
   #:border-radius border-radius
   #:padding-x padding-x
   #:padding-y padding-y
   #:update-fn %update
   #:on-click %handle-click))

(statusbar-register-module! 'cpu make-module-cpu)
