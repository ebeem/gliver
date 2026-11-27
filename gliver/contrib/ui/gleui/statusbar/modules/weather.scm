;;; gliver/contrib/ui/gleui/statusbar/modules/weather.scm --- Weather statusbar module
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver contrib ui gleui statusbar modules weather)
  #:use-module (ice-9 format)
  #:use-module (ice-9 threads)
  #:use-module (gliver core config)
  #:use-module (gliver core logs)
  #:use-module (gliver contrib ui gleui statusbar state)
  #:use-module (gliver contrib ui gleui statusbar worker)
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
                              (interval 900)  ; 15 minutes
                              (city "")       ; "" for auto-detect or "Paris"
                              (weather-format "%c+%t")
                              (bg-color *statusbar-bg-color*)
                              (fg-color *theme-fg-main*)
                              (border-radius 6)
                              (padding-x 10)
                              (padding-y 4))
  "Create an asynchronous Weather module using wttr.in.
Fetches data in a background thread so the Wayland main loop never freezes."

  (define %weather-mutex (make-mutex))
  (define %weather-cache "")
  (define %fetching? #f)

  (define (%apply-module-text! module text)
    (let ((dirty? #f))
      (with-mutex %weather-mutex
        (unless (equal? text (statusbar-module-text module))
          (statusbar-module-text-set! module text)
          (set! dirty? #t)))
      (when dirty?
        (statusbar-mark-render-dirty!))))

  (define (%spawn-weather-fetch! module)
    (call-with-new-thread
     (lambda ()
       (let ((next-text #f))
         (catch #t
           (lambda ()
             (let* ((url (%build-weather-url city weather-format))
                    (cmd (format #f "curl -s --max-time 4 ~s" url))
                    (res (statusbar-execute-shell-command cmd)))
               (with-mutex %weather-mutex
                 (cond
                  ((and res
                        (> (string-length res) 0)
                        (not (string-contains res "Unknown location")))
                   (set! %weather-cache res)
                   (set! next-text res))
                  ((string-null? %weather-cache)
                   (set! %weather-cache "󰖐 Weather N/A")
                   (set! next-text %weather-cache))))))
           (lambda _ #f))
         (with-mutex %weather-mutex
           (set! %fetching? #f))
         (when next-text
           (%apply-module-text! module next-text))))))

  (define* (%fetch-weather-async! module #:optional (sync-text? #f))
    ;; skip immediately if a weather fetch is already in progress.
    (let ((start-fetch?
           (with-mutex %weather-mutex
             (if %fetching?
                 #f
                 (begin
                   (when sync-text?
                     (let ((target (if (string-null? %weather-cache)
                                       "󰖐 Loading…"
                                       %weather-cache)))
                       (unless (equal? target (statusbar-module-text module))
                         (statusbar-module-text-set! module target))))
                   (set! %fetching? #t)
                   #t)))))
      (when start-fetch?
        (%spawn-weather-fetch! module))))

  (define (%update module output)
    (%fetch-weather-async! module #t))

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
