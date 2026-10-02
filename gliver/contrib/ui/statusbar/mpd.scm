;;; gliver/contrib/ui/statusbar/mpd.scm --- MPD music player statusbar module
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver contrib ui statusbar mpd)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (gliver core)
  #:use-module (gliver contrib ui statusbar base)
  #:declarative? #f
  #:export (
			make-module-mpd
))

(define (%format-mpd-seconds secs)
  "Format a number or numeric string SECS into [[H:]M:]SS representation."
  (let* ((num (cond
               ((number? secs) (inexact->exact (round secs)))
               ((string? secs)
                (let ((n (string->number secs)))
                  (if n (inexact->exact (round n)) #f)))
               (else #f))))
    (if (and num (>= num 0))
        (let* ((hrs (quotient num 3600))
               (rem (remainder num 3600))
               (mins (quotient rem 60))
               (s (remainder rem 60)))
          (if (> hrs 0)
              (format #f "~d:~2,'0d:~2,'0d" hrs mins s)
              (format #f "~d:~2,'0d" mins s)))
        "0:00")))

(define (%truncate-string str max-len)
  "Truncate STR to MAX-LEN characters, appending ellipsis if truncated."
  (if (and max-len (> (string-length str) max-len))
      (string-append (substring str 0 (max 0 (- max-len 1))) "…")
      str))

(define (%parse-mpd-response lines)
  "Parse MPD response LINES from status and current song commands, returns an alist."
  (let ((raw (let loop ((rem lines) (acc '()))
               (if (null? rem)
                   acc
                   (let* ((line (car rem))
                          (colon-idx (string-index line #\:)))
                     (if colon-idx
                         (let ((k (string-downcase (string-trim-both (substring line 0 colon-idx))))
                               (v (string-trim-both (substring line (1+ colon-idx)))))
                           (loop (cdr rem) (cons (cons (string->symbol k) v) acc)))
                         (loop (cdr rem) acc)))))))
    (let* ((raw-state (or (assoc-ref raw 'state) "stop"))
           (state (cond
                   ((string=? raw-state "play")  'play)
                   ((string=? raw-state "pause") 'pause)
                   (else                         'stop)))
           (title (or (assoc-ref raw 'title) (assoc-ref raw 'file) ""))
           (artist (or (assoc-ref raw 'artist) ""))
           (album (or (assoc-ref raw 'album) ""))
           (track (or (assoc-ref raw 'track) ""))
           (file (or (assoc-ref raw 'file) ""))
           (date (or (assoc-ref raw 'date) ""))
           (genre (or (assoc-ref raw 'genre) ""))
           (volume (or (assoc-ref raw 'volume) "100"))
           (elapsed-val (assoc-ref raw 'elapsed))
           (duration-val (assoc-ref raw 'duration))
           (time-val (assoc-ref raw 'time))
           (colon-time (let loop ((entries raw))
                         (cond
                          ((null? entries) #f)
                          ((and (eq? (caar entries) 'time) (string-contains (cdar entries) ":"))
                           (cdar entries))
                          (else (loop (cdr entries))))))
           (time-split (and colon-time (string-split colon-time #\:)))
           (elapsed (cond
                     (elapsed-val (or (string->number elapsed-val) 0))
                     (time-split (or (string->number (car time-split)) 0))
                     (else 0)))
           (duration (cond
                      (duration-val (or (string->number duration-val) 0))
                      ((and time-split (pair? (cdr time-split)))
                       (or (string->number (cadr time-split)) 0))
                      (time-val (or (string->number time-val) 0))
                      (else 0)))
           (elapsed-str (%format-mpd-seconds elapsed))
           (duration-str (%format-mpd-seconds duration))
           (time-str (if (> duration 0)
                         (format #f "(~a/~a)" elapsed-str duration-str)
                         (if (> elapsed 0) elapsed-str ""))))
      `((state . ,state)
        (raw-state . ,raw-state)
        (title . ,title)
        (artist . ,artist)
        (album . ,album)
        (track . ,track)
        (file . ,file)
        (date . ,date)
        (genre . ,genre)
        (volume . ,volume)
        (elapsed . ,elapsed)
        (duration . ,duration)
        (elapsed-str . ,elapsed-str)
        (duration-str . ,duration-str)
        (time-str . ,time-str)
        (time . ,time-str)))))

(define %time-regex
  (make-regexp "([0-9]+:[0-9]+(:[0-9]+)?)/([0-9]+:[0-9]+(:[0-9]+)?)" regexp/extended))

(define (%open-mpd-socket host port)
  "Open a stream socket to MPD via TCP."
  (let ((sock (socket AF_INET SOCK_STREAM 0)))
    (connect sock AF_INET (inet-pton AF_INET (if (string=? host "localhost") "127.0.0.1" host)) port)
    sock))

(define (%send-mpd-command host port command)
  "Send a command line to MPD daemon and close connection."
  (catch #t
    (lambda ()
      (let ((s (%open-mpd-socket host port)))
        (let ((banner (read-line s)))
          (display (string-append command "\nclose\n") s)
          (force-output s)
          (close-port s)
          #t)))
    (lambda _
      (log-warn "Failed to send command to mpd daemon")
      #f)))

(define (%query-mpd-status host port)
  "Query MPD daemon status and current song info.
Returns an alist or #f if unreachable."
  (catch #t
    (lambda ()
      (let ((s (%open-mpd-socket host port)))
        (let ((banner (read-line s)))
          ;; Use command_list batch to receive status and current song together
          (display "command_list_begin\nstatus\ncurrentsong\ncommand_list_end\nclose\n" s)
          (force-output s)
          (let loop ((lines '()))
            (let ((line (read-line s)))
              (cond
               ((or (eof-object? line) (string-prefix? "OK" line) (string-prefix? "ACK" line))
                (close-port s)
                (if (null? lines)
                    #f
                    (%parse-mpd-response (reverse lines))))
               (else
                (loop (cons line lines)))))))))
    (lambda _
      #f)))

(define (%mpd-info-track-name info)
  "Get track title/name from INFO."
  (let ((title (assoc-ref info 'title))
        (file (assoc-ref info 'file)))
    (cond
     ((and (string? title) (> (string-length (string-trim-both title)) 0))
      (string-trim-both title))
     ((and (string? file) (> (string-length (string-trim-both file)) 0))
      (basename (string-trim-both file)))
     (else "MPD"))))

(define (%default-mpd-formatter info)
  "Default MPD formatter: track name truncated to max 20 characters, then current timestamp/total duration."
  (let* ((track-name (%mpd-info-track-name info))
         (trunc-track (%truncate-string track-name 20))
         (time-str (or (assoc-ref info 'time-str)
                       (assoc-ref info 'time)
                       "")))
    (if (and (string? time-str) (> (string-length (string-trim-both time-str)) 0))
        (format #f "~a ~a" trunc-track (string-trim-both time-str))
        trunc-track)))

(define (%mpd-formatter-track info)
  "Format MPD status showing track name only."
  (%mpd-info-track-name info))

(define (%mpd-formatter-album info)
  "Format MPD status showing album name only."
  (let ((album (assoc-ref info 'album)))
    (if (and (string? album) (> (string-length (string-trim-both album)) 0))
        (string-trim-both album)
        "No Album")))

(define (%mpd-formatter-track-album info)
  "Format MPD status showing track name and album."
  (let ((track (%mpd-info-track-name info))
        (album (assoc-ref info 'album)))
    (if (and (string? album) (> (string-length (string-trim-both album)) 0))
        (format #f "~a (~a)" track (string-trim-both album))
        track)))

(define (%mpd-formatter-artist-track info)
  "Format MPD status showing artist and track name."
  (let ((artist (assoc-ref info 'artist))
        (track (%mpd-info-track-name info)))
    (if (and (string? artist) (> (string-length (string-trim-both artist)) 0))
        (format #f "~a - ~a" (string-trim-both artist) track)
        track)))

(define (%mpd-formatter-full info)
  "Format MPD status showing artist, track name, album, and timestamp/duration."
  (let* ((artist (assoc-ref info 'artist))
         (track (%mpd-info-track-name info))
         (album (assoc-ref info 'album))
         (time-str (or (assoc-ref info 'time-str) (assoc-ref info 'time) ""))
         (base (cond
                ((and (string? artist) (> (string-length (string-trim-both artist)) 0)
                      (string? album) (> (string-length (string-trim-both album)) 0))
                 (format #f "~a - ~a [~a]" (string-trim-both artist) track (string-trim-both album)))
                ((and (string? artist) (> (string-length (string-trim-both artist)) 0))
                 (format #f "~a - ~a" (string-trim-both artist) track))
                ((and (string? album) (> (string-length (string-trim-both album)) 0))
                 (format #f "~a [~a]" track (string-trim-both album)))
                (else track))))
    (if (and (string? time-str) (> (string-length (string-trim-both time-str)) 0))
        (format #f "~a ~a" base (string-trim-both time-str))
        base)))

(define (%resolve-mpd-formatter fmt)
  "Resolve FMT into a formatter procedure.
FMT can be a procedure, a preset symbol ('default, 'track, 'album, 'track-album,
'artist-track, 'full), or #f."
  (cond
   ((procedure? fmt) fmt)
   ((memq fmt '(track track-name title)) %mpd-formatter-track)
   ((eq? fmt 'album) %mpd-formatter-album)
   ((memq fmt '(track-album album-track)) %mpd-formatter-track-album)
   ((memq fmt '(artist-track artist-title)) %mpd-formatter-artist-track)
   ((eq? fmt 'full) %mpd-formatter-full)
   (else %default-mpd-formatter)))

(define (%apply-mpd-formatter fmt info)
  "Apply formatter FMT to INFO safely."
  (catch #t
    (lambda ()
      (let ((res (if (procedure? fmt)
                     (let ((arity (car (procedure-minimum-arity fmt))))
                       (if (> arity 1)
                           (fmt (%mpd-info-track-name info)
                                (or (assoc-ref info 'artist) "")
                                (or (assoc-ref info 'album) "")
                                (or (assoc-ref info 'time-str) ""))
                           (fmt info)))
                     (%default-mpd-formatter info))))
        (if (string? res) res (format #f "~a" res))))
    (lambda (key . args)
      (%default-mpd-formatter info))))

(define* (make-module-mpd #:key
                          (id 'mpd)
                          (section 'center)
                          (interval 1)
                          (host (or (getenv "MPD_HOST") "127.0.0.1"))
                          (port (or (and=> (getenv "MPD_PORT") string->number) 6600))
                          (format-template "~a ~a")
                          (formatter #f)
                          (playing-icon "󰐊")
                          (paused-icon "󰏤")
                          (stopped-icon "󰓛")
                          (offline-icon "󰝛")
                          (max-length #f)
                          (hide-when-stopped? #f)
                          (hide-when-offline? #t)
                          (show-offline? #f)
                          (bg-color *statusbar-bg-color*)
                          (fg-color *theme-fg-main*)
                          (border-radius 6)
                          (padding-x 10)
                          (padding-y 4)
                          (on-click-cmd #f))
  "Create an MPD music player statusbar module."
  (define %active-formatter (%resolve-mpd-formatter formatter))

  (define (%update module output)
    (let ((info (%query-mpd-status host port)))
      (if info
          (let* ((raw-state (or (assoc-ref info 'raw-state) (assoc-ref info 'state) "stop"))
                 (state (cond
                         ((or (eq? raw-state 'play) (equal? raw-state "play"))   'play)
                         ((or (eq? raw-state 'pause) (equal? raw-state "pause")) 'pause)
                         (else                                                   'stop)))
                 (artist (or (assoc-ref info 'artist) ""))
                 (title (or (assoc-ref info 'title) ""))
                 (album (or (assoc-ref info 'album) ""))
                 (time-str (or (assoc-ref info 'time-str) (assoc-ref info 'time) ""))
                 (formatted-text (%apply-mpd-formatter %active-formatter info))
                 (disp-title (if max-length (%truncate-string formatted-text max-length) formatted-text))
                 (icon (case state
                         ((play)  playing-icon)
                         ((pause) paused-icon)
                         (else    stopped-icon))))

            (if (and hide-when-stopped? (eq? state 'stop))
                (begin
                  (statusbar-module-text-set! module "")
                  (statusbar-module-visible?-set! module #f))
                (begin
                  (statusbar-module-visible?-set! module #t)
                  (statusbar-module-text-set!
                   module
                   (format #f format-template icon disp-title))
                  (statusbar-module-tooltip-set!
                   module
                   (format #f "MPD: ~a\nTrack: ~a\nAlbum: ~a\nTime:  ~a"
                           (symbol->string state)
                           (if (and (> (string-length artist) 0) (> (string-length title) 0))
                               (format #f "~a - ~a" artist title)
                               (if (> (string-length title) 0) title "N/A"))
                           (if (> (string-length album) 0) album "N/A")
                           (if (> (string-length time-str) 0) time-str "N/A")))))
            (statusbar-module-state-set! module info))
          ;; unreachable
          (if (and hide-when-offline? (not show-offline?))
              (begin
                (statusbar-module-text-set! module "")
                (statusbar-module-visible?-set! module #f))
              (begin
                (statusbar-module-visible?-set! module #t)
                (statusbar-module-text-set!
                 module
                 (format #f format-template offline-icon "Offline"))
                (statusbar-module-tooltip-set! module "MPD: Daemon not responding"))))))

  (define (%handle-click button x y module output custom-data)
    (cond
     ((= button 272)  ;; left click: toggle play/pause
      (if on-click-cmd
          (system (format #f "~a &" on-click-cmd))
          (%send-mpd-command host port "pause")))
     ((= button 273)  ;; right click: next track
      (%send-mpd-command host port "next"))
     ((= button 274)  ;; middle click: previous track
      (%send-mpd-command host port "previous"))))

  (define (%handle-scroll axis value module output)
    (when (zero? axis)  ;; vertical scroll
      (%send-mpd-command host port (if (< value 0) "volume +5" "volume -5"))))

  (make-statusbar-module
   #:id id
   #:name "MPD"
   #:section section
   #:interval interval
   #:format-template format-template
   #:icon playing-icon
   #:bg-color bg-color
   #:fg-color fg-color
   #:border-radius border-radius
   #:padding-x padding-x
   #:padding-y padding-y
   #:update-fn %update
   #:on-click %handle-click
   #:on-scroll %handle-scroll))

(statusbar-register-module! 'mpd make-module-mpd)
