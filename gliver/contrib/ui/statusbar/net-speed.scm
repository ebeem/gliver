;;; gliver/contrib/ui/statusbar/net-speed.scm --- Network speed statusbar module
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver contrib ui statusbar net-speed)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (gliver core config)
  #:use-module (gliver contrib ui statusbar base)
  #:declarative? #f
  #:export (
			make-module-net-speed
))

(define (%parse-dev-line line)
  "Parse a single /proc/net/dev line into (iface . (rx-bytes . tx-bytes)), or #f."
  (let ((colon (string-index line #\:)))
    (and colon
         (let* ((iface  (string-trim-both (substring line 0 colon)))
                (rest   (substring line (1+ colon)))
                (tokens (string-tokenize rest)))
           (and (>= (length tokens) 9)
                (let ((rx (string->number (first tokens)))
                      (tx (string->number (list-ref tokens 8))))
                  (and rx tx (cons iface (cons rx tx)))))))))

(define (%parse-proc-net-dev)
  "Read /proc/net/dev and return an alist of (iface . (rx-bytes . tx-bytes))."
  (catch #t
    (lambda ()
      (call-with-input-file "/proc/net/dev"
        (lambda (port)
          ;; skip two header lines
          (read-line port)
          (read-line port)
          (let loop ((stats '()))
            (let ((line (read-line port)))
              (if (eof-object? line)
                  (reverse stats)
                  (let ((entry (%parse-dev-line line)))
                    (loop (if entry (cons entry stats) stats)))))))))
    (lambda _ '())))

(define (%detect-primary-interface all-stats)
  "Detect the primary network interface (from default route or non-loopback with traffic)."
  (or
   ;; default route interface via ip command
   (let ((route-iface (statusbar-execute-shell-command "ip -o -4 route show to default 2>/dev/null | awk '{print $5; exit}'")))
     (and route-iface (> (string-length route-iface) 0) (string-trim-both route-iface)))

   ;; find first non-loopback interface in stats
   (any (lambda (entry)
          (let ((iface (car entry)))
            (if (not (string=? iface "lo")) iface #f)))
        all-stats)

   "lo"))

(define (%format-network-speed bytes-per-sec)
  "Format BYTES-PER-SEC into human-readable rate (e.g. '1.2 MB/s', '450 KB/s', '0 B/s')."
  (let ((b (max 0.0 (exact->inexact bytes-per-sec))))
    (define (%format-num val unit)
      (let* ((str (format #f "~,1f" val))
             (clean-str (if (string-suffix? ".0" str)
                            (substring str 0 (- (string-length str) 2))
                            str)))
        (format #f "~a ~a" clean-str unit)))
    (cond
     ((>= b (* 1024 1024 1024))
      (%format-num (/ b (* 1024.0 1024.0 1024.0)) "GB/s"))
     ((>= b (* 1024 1024))
      (%format-num (/ b (* 1024.0 1024.0)) "MB/s"))
     ((>= b 1024)
      (%format-num (/ b 1024.0) "KB/s"))
     (else
      (format #f "~a B/s" (inexact->exact (round b)))))))

(define* (make-module-net-speed #:key
                                (id 'net-speed)
                                (section 'right)
                                (interval 2)
                                (interface 'auto)  ;; 'auto, 'all, or "eth0"
                                (format-template " ~a  ~a")
                                (show-interface? #f)
                                (bg-color *statusbar-bg-color*)
                                (fg-color *statusbar-fg-color*)
                                (border-radius 6)
                                (padding-x 10)
                                (padding-y 4)
                                (on-click-cmd #f))
  "Create a Network Download/Upload Speed monitor module.
Calculates instantaneous throughput from /proc/net/dev deltas."

  (define (%update module output)
    (let* ((stats (%parse-proc-net-dev))
           (target-iface (if (eq? interface 'auto)
                             (%detect-primary-interface stats)
                             interface))
           (now (current-time))
           (cur-bytes
            (cond
             ((eq? target-iface 'all)
              ;; Aggregate all non-loopback
              (let loop ((rest stats) (rx 0) (tx 0))
                (if (null? rest)
                    (cons rx tx)
                    (let* ((entry (car rest))
                           (iface (car entry))
                           (rx-val (cadr entry))
                           (tx-val (cddr entry)))
                      (if (not (string=? iface "lo"))
                          (loop (cdr rest) (+ rx rx-val) (+ tx tx-val))
                          (loop (cdr rest) rx tx))))))
             ((string? target-iface)
              (let ((entry (assoc-ref stats target-iface)))
                (if entry entry (cons 0 0))))
             (else (cons 0 0))))
           (cur-rx (car cur-bytes))
           (cur-tx (cdr cur-bytes))
           (mod-state (or (statusbar-module-state module) '()))
           (prev-rx (assoc-ref mod-state 'prev-rx))
           (prev-tx (assoc-ref mod-state 'prev-tx))
           (prev-time (assoc-ref mod-state 'prev-time)))

      (if (and prev-rx prev-tx prev-time (> now prev-time))
          (let* ((dt (max 1 (- now prev-time)))
                 (rx-speed (/ (max 0 (- cur-rx prev-rx)) dt))
                 (tx-speed (/ (max 0 (- cur-tx prev-tx)) dt))
                 (rx-str (%format-network-speed rx-speed))
                 (tx-str (%format-network-speed tx-speed))
                 (speed-text (format #f format-template rx-str tx-str))
                 (text (if show-interface?
                           (format #f "~a: ~a" (if (eq? target-iface 'all) "all" target-iface) speed-text)
                           speed-text)))
            (statusbar-module-text-set! module text)
            (statusbar-module-state-set!
             module
             `((prev-rx . ,cur-rx)
               (prev-tx . ,cur-tx)
               (prev-time . ,now)
               (interface . ,target-iface))))

          (begin
            (statusbar-module-text-set! module (format #f format-template "0 B/s" "0 B/s"))
            (statusbar-module-state-set!
             module
             `((prev-rx . ,cur-rx)
               (prev-tx . ,cur-tx)
               (prev-time . ,now)
               (interface . ,target-iface)))))))

  (define (%handle-click button x y module output custom-data)
    (when (= button 272)
      (let ((cmd (or on-click-cmd
                     (and *terminal* (format #f "~a -e nmtui &" *terminal*))
                     "foot -e nmtui &")))
        (system cmd))))

  (make-statusbar-module
   #:id id
   #:name "Network Speed"
   #:section section
   #:interval interval
   #:format-template format-template
   #:icon ""
   #:bg-color bg-color
   #:fg-color fg-color
   #:border-radius border-radius
   #:padding-x padding-x
   #:padding-y padding-y
   #:update-fn %update
   #:on-click %handle-click))

(statusbar-register-module! 'net-speed make-module-net-speed)
