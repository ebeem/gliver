(define-module (gliver contrib systemd)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (gliver core)
  #:export (systemd-status
            systemd-start
            systemd-stop
            systemd-restart
            systemd-enable
            systemd-disable
            systemd-active?
            systemd-ensure-started))

;; helper to build the command list and execute
(define (systemctl action unit system?)
  (let* ((args (if system?
                   (list "systemctl" action unit)
                   (list "systemctl" "--user" action unit)))
         (status (apply system* args)))
    (zero? (status:exit-val status))))

;; returns the full status output as a string
(define* (systemd-status unit #:key (system? #f))
  (let* ((cmd  (if system? 
                   (string-append "systemctl status " unit)
                   (string-append "systemctl --user status " unit)))
         (port (open-input-pipe cmd))
         (str  (read-delimited "" port))) ; read-string alternative
    (close-pipe port)
    str))

;; basic controls using keyword arguments
(define* (systemd-start unit #:key (system? #f))   (systemctl "start" unit system?))
(define* (systemd-stop unit #:key (system? #f))    (systemctl "stop" unit system?))
(define* (systemd-restart unit #:key (system? #f)) (systemctl "restart" unit system?))
(define* (systemd-enable unit #:key (system? #f))  (systemctl "enable" unit system?))
(define* (systemd-disable unit #:key (system? #f)) (systemctl "disable" unit system?))

;; check if a service is currently active
(define* (systemd-active? unit #:key (system? #f))
  (let* ((args (if system?
                   (list "systemctl" "is-active" "--quiet" unit)
                   (list "systemctl" "--user" "is-active" "--quiet" unit)))
         (status (apply system* args)))
    (zero? (status:exit-val status))))

;; start if stopped (conditional start)
(define* (systemd-ensure-started unit #:key (system? #f))
  (if (not (systemd-active? unit #:system? system?))
      (begin
        (log-info (format #f "Unit ~a is stopped. Starting...\n" unit))
        (systemd-start unit #:system? system?))
      (begin
        (log-info (format #f "Unit ~a is already running.\n" unit))
        #t)))
