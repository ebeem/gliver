;;; gliver/contrib/ui/statusbar/ram.scm --- RAM statusbar module
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver contrib ui statusbar ram)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (gliver core config)
  #:use-module (gliver contrib ui statusbar base)
  #:declarative? #f
  #:export (
			make-module-ram
))

(define (%parse-meminfo-line line)
  "Parse a single '/proc/meminfo' line into (KEY . VALUE-IN-KB)"
  (let ((colon (string-index line #\:)))
    (and colon
         (let* ((key (substring line 0 colon))
                (val-str (string-trim-both (substring line (1+ colon))))
                ;; strip trailing " kB" if present
                (num-str (car (string-split val-str #\space))))
           (cons key (string->number num-str))))))

(define (%collect-meminfo-fields port acc)
  "Read all relevant fields into an alist"
  (let ((line (read-line port)))
    (if (eof-object? line)
        acc
        (let ((pair (%parse-meminfo-line line)))
          (%collect-meminfo-fields port (if pair (cons pair acc) acc))))))

(define (%read-proc-meminfo)
  (catch #t
    (lambda ()
      (call-with-input-file "/proc/meminfo"
        (lambda (port)
          (let* ((data    (%collect-meminfo-fields port '()))
                 (ref     (lambda (key) (or (assoc-ref data key) 0)))
                 (total   (assoc-ref data "MemTotal"))
                 (avail   (or (assoc-ref data "MemAvailable")
                              (+ (ref "MemFree") (ref "Buffers") (ref "Cached")))))
            (list total avail)))))
    (lambda _ '(#f #f))))

(define* (make-module-ram #:key
                          (id 'ram)
                          (section 'right)
                          (interval 2)
                          (icon "")
                          (format-percent " ~a%")
                          (format-detailed " ~,1fG/~,1fG (~a%)")
                          (warning-threshold 80)
                          (critical-threshold 90)
                          (bg-color *statusbar-bg-color*)
                          (fg-color *theme-fg-main*)
                          (fg-warning *theme-yellow*)
                          (fg-critical *theme-red*)
                          (border-radius 6)
                          (padding-x 10)
                          (padding-y 4))
  "Create a RAM / Memory usage module."

  (define (%update module output)
    (match (%read-proc-meminfo)
      ((total avail)
       (when (and total avail (> total 0))
         (let* ((used-kb (- total avail))
                (used-gb (/ used-kb 1048576.0))
                (total-gb (/ total 1048576.0))
                (pct (inexact->exact (round (* 100.0 (/ used-kb total)))))
                (state (statusbar-module-state module))
                (mode (or (assoc-ref state 'mode) 'percent))
                (fg (cond
                     ((>= pct critical-threshold) fg-critical)
                     ((>= pct warning-threshold)  fg-warning)
                     (else fg-color)))
                (text (if (eq? mode 'detailed)
                          (format #f format-detailed used-gb total-gb pct)
                          (format #f format-percent pct))))
           (statusbar-module-text-set! module text)
           (statusbar-module-fg-color-set! module fg)
           (statusbar-module-tooltip-set!
            module
            (format #f "Memory: ~,1f GB / ~,1f GB (~a%)" used-gb total-gb pct))
           (statusbar-module-state-set!
            module
            `((mode . ,mode) (used-gb . ,used-gb) (total-gb . ,total-gb) (pct . ,pct))))))
      (_ #f)))

  (define (%handle-click button x y module output custom-data)
    (when (= button 272)
      (let* ((state (statusbar-module-state module))
             (mode (or (assoc-ref state 'mode) 'percent))
             (new-mode (if (eq? mode 'percent) 'detailed 'percent)))
        (statusbar-module-state-set! module (assq-set! state 'mode new-mode))
        (%update module output))))

  (make-statusbar-module
   #:id id
   #:name "RAM"
   #:section section
   #:interval interval
   #:bg-color bg-color
   #:fg-color fg-color
   #:border-radius border-radius
   #:padding-x padding-x
   #:padding-y padding-y
   #:update-fn %update
   #:on-click %handle-click))

(statusbar-register-module! 'ram make-module-ram)
