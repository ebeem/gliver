;;; gliver/contrib/ui/statusbar/weather.scm --- Weather statusbar module
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver contrib ui statusbar weather)
  #:use-module (ice-9 format)
  #:use-module (ice-9 threads)
  #:use-module (gliver core config)
  #:use-module (gliver core logs)
  #:use-module (gliver contrib ui statusbar base)
  #:declarative? #f
  #:export (
			make-module-weather
))

(define (%build-weather-url city weather-format)
  (if (> (string-length city) 0)
      (format #f "wttr.in/~a?format=~a" city weather-format)
      (format #f "wttr.in/?format=~a" weather-format)))

(define* (make-module-weather #:key
                              (id 'weather)
                              (section 'center)
                              (interval 900)  ;; 15 minutes
                              (city "")       ;; "" for auto-detect or "Paris", "Tokyo", etc.
                              (weather-format "%c+%t")
                              (bg-color *statusbar-bg-color*)
                              (fg-color *theme-fg-main*)
                              (border-radius 6)
                              (padding-x 10)
                              (padding-y 4))
  "Create an asynchronous Weather module using wttr.in.
Fetches data in a background thread so the Wayland main loop never freezes."

  (define %weather-cache (cons "" #f))  ;; (cached-text . fetching?)

  (define (%fetch-weather-async! module)
    (unless (cdr %weather-cache)
      ;; set status to fetching
      (set-cdr! %weather-cache #t)
      (call-with-new-thread
       (lambda ()
         (catch #t
           (lambda ()
             (let* ((url (%build-weather-url city weather-format))
                    (cmd (format #f "curl -s --max-time 4 ~s" url))
                    (res (statusbar-execute-shell-command cmd)))
               (if (and res (> (string-length res) 0) (not (string-contains res "Unknown location")))
                   (set-car! %weather-cache res)
                   (when (string-null? (car %weather-cache))
                     (set-car! %weather-cache "󰖐 Weather N/A")))))
           (lambda _ #f))
         (set-cdr! %weather-cache #f)))))

  (define (%update module output)
    (let ((cached (car %weather-cache)))
      (if (string-null? cached)
          (begin
            (statusbar-module-text-set! module "󰖐 Loading…")
            (%fetch-weather-async! module))
          (statusbar-module-text-set! module cached))))

  (define (%handle-click button x y module output custom-data)
    (when (= button 272)
      (log-info "Statusbar: refreshing weather...")
      (%fetch-weather-async! module)))

  (make-statusbar-module
   #:id id
   #:name "Weather"
   #:section section
   #:interval interval
   #:bg-color bg-color
   #:fg-color fg-color
   #:border-radius border-radius
   #:padding-x padding-x
   #:padding-y padding-y
   #:update-fn %update
   #:on-click %handle-click))

(statusbar-register-module! 'weather make-module-weather)
