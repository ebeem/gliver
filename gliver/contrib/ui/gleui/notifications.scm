;;; gliver/contrib/ui/gleui/notifications.scm --- Native D-Bus Notification Daemon for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; NOTE: this uses unix sockets rather than implementing dbus bindings via ffi
;;; this has some disadvantages, but it seems like it's not recommended by freedesktop
;;; to bindings to libdbus anymore, maybe needs more investigation and study
;;; the code is ugly because it deals with low-level dbus interaction via POSIX docket
;;; would love to have a library under deps to manage the low-level calls so this
;;; file can become more compact and easier to read and maintain

(define-module (gliver contrib ui gleui notifications)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (gliver core)
  #:use-module (gliver deps pango)
  #:use-module (gliver contrib ui gleui toast)
  #:declarative? #f
  #:export (
			*notification-daemon-enabled*
			*notification-daemon-timeout*
			*notification-daemon-show-app-name*
			*notification-daemon-show-actions*
			*notification-daemon-max-history*
			*notification-daemon-max-visible*
			*notification-daemon-gap*
			*notification-daemon-font*
			*notification-daemon-font-size*
			*notification-daemon-bg-color*
			*notification-daemon-fg-color*
			*notification-daemon-border-color*
			*notification-daemon-border-width*
			*notification-daemon-border-radius*
			*notification-daemon-location*
			*notification-daemon-margin*
			*notification-daemon-padding-x*
			*notification-daemon-padding-y*
			*notification-daemon-width*
			*notification-daemon-icon-show*
			*notification-daemon-icon-location*
			*notification-daemon-icon-border-radius*
			*notification-daemon-icon-border-width*
			*notification-daemon-icon-border-color*
			*notification-daemon-icon-size*
			*notification-daemon-socket*
			*notification-daemon-serial*
			*notification-daemon-id-counter*
			*notification-daemon-active-id*
			*notification-daemon-active-ids*
			*notification-daemon-history*
			*notification-daemon-dbus-in-buffer*
			notification-daemon-clear-history!
			notification-daemon-dismiss!
			notification-daemon-dismiss-all!
			notification-daemon-redisplay-last!
			*notification-daemon-introspection-xml*
			notification-daemon-poll!
			notification-daemon-start!
			notification-daemon-stop!
			notification-daemon-restart!
))

(define-var *notification-daemon-enabled* #t
			"Whether the Gliver notification daemon should be started with gleui components.")
(define-var *notification-daemon-timeout* *message-timeout*
			"Expiration timeout for notifications in milliseconds.")
(define-var *notification-daemon-show-app-name* #t
			"Whether to display the calling application name.")
(define-var *notification-daemon-show-actions* #t
			"Whether to display notification action button labels.")
(define-var *notification-daemon-max-history* 50
			"Maximum number of notifications to keep in history.")
(define-var *notification-daemon-max-visible* 6
			"Maximum number of notification toasts visible on screen.")
(define-var *notification-daemon-gap* 12
			"Vertical gap in pixels between stacked notifications.")

(define-var *notification-daemon-font* *gleui-toast-font*
			"Font used for gleui toast notifications.")
(define-var *notification-daemon-font-size* *gleui-toast-font-size*
			"Font size used for gleui toast notifications.")
(define-var *notification-daemon-bg-color* *gleui-toast-bg-color*
			"Background color for gleui toast notifications.")
(define-var *notification-daemon-fg-color* *gleui-toast-fg-color*
			"Text color for gleui toast notifications.")
(define-var *notification-daemon-border-color* *gleui-toast-border-color*
			"Border color for gleui toast notifications.")
(define-var *notification-daemon-border-width* *gleui-toast-border-width*
			"Border width in pixels for gleui toast notifications.")
(define-var *notification-daemon-border-radius* *gleui-toast-border-radius*
			"Corner border radius in pixels for gleui toast notifications.")
(define-var *notification-daemon-location* *gleui-toast-location*
			"Gliver position (1-9) for gleui toast notifications.")
(define-var *notification-daemon-margin* *gleui-toast-margin*
			"Margin in pixels from the screen edge for gleui toast notifications.")
(define-var *notification-daemon-padding-x* *gleui-toast-padding-x*
			"Padding horizontal content in pixels for gleui toast notifications.")
(define-var *notification-daemon-padding-y* *gleui-toast-padding-y*
			"Padding vertical content in pixels for gleui toast notifications.")
(define-var *notification-daemon-width* 500
			"Fixed width in pixels for notification daemon toasts.")
(define-var *notification-daemon-icon-show* *gleui-toast-icon-show*
			"Whether to display icons in notification daemon toasts.")
(define-var *notification-daemon-icon-location* *gleui-toast-icon-location*
			"Icon placement in notification daemon toasts: 'left or 'right.")
(define-var *notification-daemon-icon-border-radius* *gleui-toast-icon-border-radius*
			"Corner border radius in pixels for icons in notification daemon toasts.")
(define-var *notification-daemon-icon-border-width* *gleui-toast-icon-border-width*
			"Border width in pixels around icons in notification daemon toasts.")
(define-var *notification-daemon-icon-border-color* *gleui-toast-icon-border-color*
			"Border color around icons in notification daemon toasts.")
(define-var *notification-daemon-icon-size* *gleui-toast-icon-size*
			"Icon dimensions (width and height) in pixels in notification daemon toasts.")

;;; global state
(define *notification-daemon-socket* #f)
(define *notification-daemon-serial* 1)
(define *notification-daemon-id-counter* 1)
(define *notification-daemon-active-id* #f)
(define *notification-daemon-active-ids* '())
(define *notification-daemon-history* '())
(define *notification-daemon-dbus-in-buffer* (make-bytevector 0 0))

(define (%next-serial!)
  (let ((s *notification-daemon-serial*))
    (set! *notification-daemon-serial* (1+ *notification-daemon-serial*))
    s))

(define (%next-id!)
  (let ((id *notification-daemon-id-counter*))
    (set! *notification-daemon-id-counter* (1+ *notification-daemon-id-counter*))
    id))

(define-command (notification-daemon-clear-history!)
  "Clear the notification history."
  (set! *notification-daemon-history* '())
  (log-info "notification-daemon: History cleared."))

(define (%dbus-session-bus-path)
  "Locate the dbus UNIX socket path."
  (let ((addr (getenv "DBUS_SESSION_BUS_ADDRESS")))
    (cond
     ((and addr (string-prefix? "unix:path=" addr))
      (let* ((rest (substring addr (string-length "unix:path=")))
             (comma (string-index rest #\,)))
        (if comma (substring rest 0 comma) rest)))
     ((and addr (file-exists? addr))
      addr)
     (else
      (let ((fallback (string-append "/run/user/" (number->string (getuid)) "/bus")))
        (if (file-exists? fallback) fallback #f))))))

(define (%dbus-connect path)
  "Connect to dbus session bus at PATH."
  (let ((sock (socket AF_UNIX SOCK_STREAM 0)))
    ;; prevent socket leakage to child processes
    (fcntl sock F_SETFD FD_CLOEXEC)
    (connect sock AF_UNIX path)
    ;; sasl external auth requires hex-encoding the ASCII string of decimal UID
    (let* ((uid-str (number->string (getuid)))
           (uid-hex (apply string-append
                           (map (lambda (c) (number->string (char->integer c) 16))
                                (string->list uid-str)))))

      ;; initial null byte + auth external <uid-hex>\r\n
      (display (string-append "\x00AUTH EXTERNAL " uid-hex "\r\n") sock)
      (force-output sock)
      (let ((line (read-line sock)))
        (unless (and (string? line) (string-prefix? "OK" line))
          (close-port sock)
          (error "D-Bus SASL auth failed" line)))

      ;; begin\r\n
      (display "BEGIN\r\n" sock)
      (force-output sock)
      sock)))

;;; encoders and decoders
;;; these are all helpers to read and write to dbus socket
(define (%bytevector-slice bv start len)
  (let ((out (make-bytevector len 0)))
    (bytevector-copy! bv start out 0 len)
    out))

(define (%bytevector-append bv1 bv2)
  (let* ((l1 (bytevector-length bv1))
         (l2 (bytevector-length bv2))
         (out (make-bytevector (+ l1 l2) 0)))
    (bytevector-copy! bv1 0 out 0 l1)
    (bytevector-copy! bv2 0 out l1 l2)
    out))

(define (%align-offset offset n)
  (let ((rem (modulo offset n)))
    (if (zero? rem) offset (+ offset (- n rem)))))

(define (%put-align port alignment)
  (let ((rem (modulo (seek port 0 SEEK_CUR) alignment)))
    (unless (zero? rem)
      (put-bytevector port (make-bytevector (- alignment rem) 0)))))

(define (%get-align port alignment)
  (let* ((pos (seek port 0 SEEK_CUR))
         (rem (modulo pos alignment)))
    (unless (zero? rem)
      (catch 'out-of-range
        (lambda ()
          (seek port (- alignment rem) SEEK_CUR))
        (lambda _ #f)))))

(define (%socket-send-all! sock bv)
  (put-bytevector sock bv)
  (force-output sock))

;;; encoders
(define (%put-u8 port n)
  (put-u8 port n))

(define (%put-u32 port n)
  (%put-align port 4)
  (let ((bv (make-bytevector 4 0)))
    (bytevector-u32-set! bv 0 n (endianness little))
    (put-bytevector port bv)))

(define (%put-s32 port n)
  (%put-align port 4)
  (let ((bv (make-bytevector 4 0)))
    (bytevector-s32-set! bv 0 n (endianness little))
    (put-bytevector port bv)))

(define (%put-dbus-string port str)
  (let ((utf (string->utf8 (or str ""))))
    (%put-u32 port (bytevector-length utf))
    (put-bytevector port utf)
    (%put-u8 port 0)))

(define (%put-dbus-signature port sig)
  (let ((utf (string->utf8 (or sig ""))))
    (%put-u8 port (bytevector-length utf))
    (put-bytevector port utf)
    (%put-u8 port 0)))

(define (%call-with-bv-output proc)
  (call-with-values open-bytevector-output-port
    (lambda (port get-bv)
      (proc port)
      (get-bv))))

(define (%encode-u32 n)
  (%call-with-bv-output (lambda (p) (%put-u32 p n))))
(define (%encode-string str)
  (%call-with-bv-output (lambda (p) (%put-dbus-string p str))))

(define (%encode-string-array strs)
  (let ((data (%call-with-bv-output
               (lambda (p) (for-each (lambda (s) (%put-dbus-string p s)) strs)))))
    (%call-with-bv-output
     (lambda (p)
       (%put-u32 p (bytevector-length data))
       (put-bytevector p data)))))

(define (%encode-4-strings . strs)
  (%call-with-bv-output
   (lambda (p) (for-each (lambda (s) (%put-dbus-string p s)) strs))))

;; decoders
(define (%get-u8 port)
  (let ((b (get-u8 port)))
    (if (number? b) b 0)))

(define (%get-u32 port)
  (%get-align port 4)
  (let ((bv (get-bytevector-n port 4)))
    (if (and (bytevector? bv) (= (bytevector-length bv) 4))
        (bytevector-u32-ref bv 0 (endianness little))
        0)))

(define (%get-s32 port)
  (%get-align port 4)
  (let ((bv (get-bytevector-n port 4)))
    (if (and (bytevector? bv) (= (bytevector-length bv) 4))
        (bytevector-s32-ref bv 0 (endianness little))
        0)))

(define (%get-dbus-string port)
  (let* ((len (%get-u32 port))
         (bv  (if (> len 0)
                  (let ((b (get-bytevector-n port len)))
                    (if (bytevector? b) b #vu8()))
                  #vu8())))
    (get-u8 port)
    (catch #t
      (lambda () (utf8->string bv))
      (lambda _ ""))))

(define (%get-dbus-signature port)
  (let* ((len (%get-u8 port))
         (bv  (if (> len 0)
                  (let ((b (get-bytevector-n port len)))
                    (if (bytevector? b) b #vu8()))
                  #vu8())))
    (get-u8 port)
    (catch #t
      (lambda () (utf8->string bv))
      (lambda _ ""))))

(define (%get-dbus-string-array port)
  (let* ((len (%get-u32 port))
         (cur (seek port 0 SEEK_CUR))
         (end (+ cur len)))
    (let loop ((acc '()))
      (if (or (port-eof? port) (>= (seek port 0 SEEK_CUR) end))
          (reverse acc)
          (loop (cons (%get-dbus-string port) acc))))))

(define (%get-dbus-hints-dict port)
  (catch #t
    (lambda ()
  (define (%cur-pos)
    (seek port 0 SEEK_CUR))

  (define (%safe-skip n)
    (when (positive? n)
      (catch 'out-of-range
        (lambda () (seek port n SEEK_CUR))
        (lambda _ #f))))

  (define (%read-8byte ref-proc default)
    (%get-align port 8)
    (let ((b (get-bytevector-n port 8)))
      (if (and (bytevector? b) (= (bytevector-length b) 8))
          (ref-proc b 0 (endianness little))
          default)))

  (define (%read-image-data)
    (%get-align port 8)
    (let* ((w         (%get-s32 port))
           (h         (%get-s32 port))
           (rowstride (%get-s32 port))
           (has-alpha (not (zero? (%get-u32 port))))
           (bps       (%get-s32 port))
           (channels  (%get-s32 port))
           (data-len  (%get-u32 port))
           (data      (get-bytevector-n port data-len)))
          (if (bytevector? data)
              (list 'image-data w h rowstride has-alpha bps channels data)
              #f)))

  (define (%read-variant sig)
    (cond
     ((string=? sig "y")  (%get-u8 port))
     ((string=? sig "u")  (%get-u32 port))
     ((string=? sig "i")  (%get-s32 port))
         ((or (string=? sig "q") (string=? sig "n"))
          (%get-align port 2)
          (let ((bv (get-bytevector-n port 2)))
            (if (and (bytevector? bv) (= (bytevector-length bv) 2))
                (bytevector-u16-ref bv 0 (endianness little))
                0)))
     ((string=? sig "x")  (%read-8byte bytevector-s64-ref 0))
     ((string=? sig "t")  (%read-8byte bytevector-u64-ref 0))
     ((string=? sig "d")  (%read-8byte bytevector-ieee-double-ref 0.0))
     ((string=? sig "b")  (not (zero? (%get-u32 port))))
     ((or (string=? sig "s") (string=? sig "o")) (%get-dbus-string port))
     ((string=? sig "as") (%get-dbus-string-array port))
     ((string=? sig "(iiibiiay)") (%read-image-data))
     ((string-prefix? "a" sig)
          (let ((arr-len (%get-u32 port)))
            (%safe-skip arr-len)
            #f))
     (else #f)))

  (let ((len (%get-u32 port)))
    (%get-align port 8)
    (let ((end (+ (%cur-pos) len)))
      (let loop ((acc '()))
        (let* ((cur (%cur-pos))
               (rem (modulo cur 8))
               (pad (if (zero? rem) 0 (- 8 rem))))
          (if (or (port-eof? port) (>= (+ cur pad) end))
                  (begin
                    (seek port end SEEK_SET)
                    (reverse acc))
              (begin
                (%safe-skip pad)
                (let* ((key (%get-dbus-string port))
                       (sig (%get-dbus-signature port))
                       (val (%read-variant sig)))
                  (if val
                      (loop (cons (cons key val) acc))
                      (if (< (%cur-pos) end)
                          (loop acc)
                              (begin
                                (seek port end SEEK_SET)
                                (reverse acc))))))))))))
    (lambda (k . args)
      (log-error "notification-daemon: Error parsing hints dict: ~a ~a" k args)
      '())))

;;; dbus message and body handlers
(define (%build-header-fields fields)
  (%call-with-bv-output
   (lambda (p)
     (for-each
      (lambda (f)
        (let ((code (car f))
              (sig  (cadr f))
              (val  (caddr f)))
          (%put-align p 8)
          (%put-u8 p code)
          (%put-dbus-signature p sig)
          (cond
           ((or (string=? sig "s") (string=? sig "o")) (%put-dbus-string p val))
           ((string=? sig "u")                         (%put-u32 p val))
           ((string=? sig "g")                         (%put-dbus-signature p val)))))
      fields))))

(define (%build-dbus-message type serial fields body)
  (let* ((hfields     (%build-header-fields fields))
         (hfields-len (bytevector-length hfields))
         (body-len    (if body (bytevector-length body) 0))
         (hdr         (make-bytevector 16 0)))
    ;; dbus header: [endianness, type, flags, version, body length, serial, fields length]
    (bytevector-u8-set!  hdr 0 108) ;; 'l' little endian
    (bytevector-u8-set!  hdr 1 type)
    (bytevector-u8-set!  hdr 2 0)   ;; flags
    (bytevector-u8-set!  hdr 3 1)   ;; protocol version 1
    (bytevector-u32-set! hdr 4  body-len    (endianness little))
    (bytevector-u32-set! hdr 8  serial      (endianness little))
    (bytevector-u32-set! hdr 12 hfields-len (endianness little))
    (%call-with-bv-output
     (lambda (p)
       (put-bytevector p hdr)
       (put-bytevector p hfields)
       (%put-align p 8)
       (when body (put-bytevector p body))))))

(define (%parse-header-fields bv)
  (let* ((p   (open-bytevector-input-port bv))
         (len (bytevector-length bv)))
    (let loop ((acc '()))
      (let* ((pos (seek p 0 SEEK_CUR))
             (rem (modulo pos 8))
             (pad (if (zero? rem) 0 (- 8 rem)))
             (next-pos (+ pos pad)))
        ;; header field struct (yv) needs at least 4 bytes (code, siglen, sig, \0)
        (if (>= next-pos (- len 3))
            (reverse acc)
            (begin
              (when (> pad 0)
                (catch 'out-of-range
                  (lambda () (seek p pad SEEK_CUR))
                  (lambda _ #f)))
              (let* ((code   (%get-u8 p))
                     (siglen (%get-u8 p)))
                (if (or (eof-object? siglen) (>= (+ (seek p 0 SEEK_CUR) siglen 1) len))
                    (reverse acc)
                    (let* ((sig (utf8->string (get-bytevector-n p siglen))))
                      (get-u8 p) ;; sig \0
                      (let ((val (cond
                                  ((or (string=? sig "s") (string=? sig "o")) (%get-dbus-string p))
                                  ((string=? sig "u")                         (%get-u32 p))
                                  ((string=? sig "g")                         (%get-dbus-signature p))
                                  (else #f))))
                        (if val
                            (loop (cons (cons code val) acc))
                            (reverse acc))))))))))))

(define (%parse-notify-body bv)
  (catch #t
    (lambda ()
      (let ((p (open-bytevector-input-port bv)))
        (let* ((app-name       (%get-dbus-string p))
               (replaces-id    (%get-u32 p))
               (app-icon       (%get-dbus-string p))
               (summary        (%get-dbus-string p))
               (body           (%get-dbus-string p))
               (actions        (%get-dbus-string-array p))
               (hints          (%get-dbus-hints-dict p))
               (expire-timeout (%get-s32 p)))
          (list app-name replaces-id app-icon summary body actions hints expire-timeout))))
    (lambda (k . args)
      (log-error "notification-daemon: Error parsing Notify body: ~a ~a" k args)
      #f)))

(define (%dbus-reply! sock sender reply-serial sig body-bv)
  (when sock
    (catch #t
      (lambda ()
        (let* ((serial (%next-serial!))
               (fields `((5 "u" ,reply-serial)
                         (6 "s" ,sender)
                         ,@(if (and sig (not (string-null? sig)))
                               `((8 "g" ,sig))
                               '())))
               (msg (%build-dbus-message 2 serial fields body-bv)))
          (%socket-send-all! sock msg)))
      (lambda (key . args)
        (log-error "notification-daemon: Error sending reply: ~a ~a" key args)))))

(define (%dbus-error-reply! sock sender reply-serial error-name error-msg)
  "Send a D-Bus ERROR reply message (type 3) for the method call with REPLY-SERIAL."
  (when (and sock sender reply-serial)
    (catch #t
      (lambda ()
        (let* ((serial (%next-serial!))
               (err (or error-name "org.freedesktop.DBus.Error.Failed"))
               (fields `((4 "s" ,err)
                         (5 "u" ,reply-serial)
                         (6 "s" ,sender)
                         ,@(if (and error-msg (not (string-null? error-msg)))
                               `((8 "g" "s"))
                               '())))
               (body-bv (if (and error-msg (not (string-null? error-msg)))
                            (%encode-string error-msg)
                            #f))
               (msg (%build-dbus-message 3 serial fields body-bv)))
          (%socket-send-all! sock msg)))
      (lambda (key . args)
        (log-error "notification-daemon: Error sending error reply: ~a ~a" key args)))))

;; integration with gleui toast
(define (%format-notification-message app-name summary body actions urgency)
  "Format notification parts into a Pango markup string suitable for gleui-toast-show."
  (let* ((lines '())
         (badge (if (and *notification-daemon-show-app-name*
                         (string? app-name)
                         (not (string-null? app-name)))
                    (format #f "<span alpha=\"70%\" size=\"small\">[~a]</span> " (pango-escape-markup app-name))
                    ""))
         (safe-summary (if (string? summary) summary ""))
         (title-line (string-append badge "<b>" (pango-escape-markup safe-summary) "</b>")))
    (set! lines (cons title-line lines))
    (when (and (string? body) (not (string-null? body)))
      (set! lines (cons (gleui-toast-markup-ensure body) lines)))
    (when (and *notification-daemon-show-actions*
               (pair? actions))
      ;; actions is list: (id1 label1 id2 label2 ...)
      (let loop ((acts actions) (tags '()))
        (if (or (null? acts) (null? (cdr acts)))
            (when (pair? tags)
              (set! lines (cons (string-append "<span alpha=\"65%\" size=\"smaller\">Actions: "
                                               (string-join (reverse tags) " | ")
                                               "</span>")
                                lines)))
            (let ((key (car acts))
                  (label (cadr acts)))
              (loop (cddr acts)
                    (cons (format #f "<b>[~a]</b>" (pango-escape-markup (if (string? label) label ""))) tags))))))
    (string-join (reverse lines) "\n")))

(define (%extract-notification-icon app-icon hints)
  (let ((hint-img (and (list? hints)
                       (or (assoc-ref hints "image-path")
                           (assoc-ref hints "image_path"))))
        (hint-data (and (list? hints)
                        (or (assoc-ref hints "image-data")
                            (assoc-ref hints "image_data")
                            (assoc-ref hints "icon_data")))))
    (cond
     ((and (string? hint-img) (not (string-null? hint-img))) hint-img)
     ((and (pair? hint-data) (eq? (car hint-data) 'image-data)) hint-data)
     ((and (string? app-icon) (not (string-null? app-icon))) app-icon)
     (else #f))))

(define (%add-to-history! id app-name summary body urgency icon)
  (let ((entry `((id . ,id)
                 (app-name . ,app-name)
                 (summary . ,summary)
                 (body . ,body)
                 (urgency . ,urgency)
                 (icon . ,icon)
                 (timestamp . ,(current-time)))))
    (set! *notification-daemon-history* (cons entry *notification-daemon-history*))
    (when (> (length *notification-daemon-history*) *notification-daemon-max-history*)
      (set! *notification-daemon-history* (take *notification-daemon-history* *notification-daemon-max-history*)))))

(define* (%notification-daemon-theme-overrides urgency #:optional (icon #f))
  `((font . ,*notification-daemon-font*)
    (font-size . ,*notification-daemon-font-size*)
    (background . ,*notification-daemon-bg-color*)
    (bg-color . ,*notification-daemon-bg-color*)
    (foreground . ,*notification-daemon-fg-color*)
    (fg-color . ,*notification-daemon-fg-color*)
    (border-color . ,(case urgency
                       ((2) *theme-urgent-color*)
                       ((0) *theme-overlay0*)
                       (else *notification-daemon-border-color*)))
    (border-width . ,*notification-daemon-border-width*)
    (border-radius . ,*notification-daemon-border-radius*)
    (location . ,*notification-daemon-location*)
    (margin . ,*notification-daemon-margin*)
    (padding-x . ,*notification-daemon-padding-x*)
    (padding-y . ,*notification-daemon-padding-y*)
    (width . ,*notification-daemon-width*)
    (icon-show . ,*notification-daemon-icon-show*)
    (icon-location . ,*notification-daemon-icon-location*)
    (icon-border-radius . ,*notification-daemon-icon-border-radius*)
    (icon-border-width . ,*notification-daemon-icon-border-width*)
    (icon-border-color . ,*notification-daemon-icon-border-color*)
    (icon-size . ,*notification-daemon-icon-size*)
    ,@(if icon `((icon . ,icon)) '())))

(define (%dbus-emit-notification-closed! sock id reason)
  "Emit NotificationClosed DBus signal."
  (when sock
    (catch #t
      (lambda ()
        (let* ((serial (%next-serial!))
               (fields `((1 "o" "/org/freedesktop/Notifications")
                         (2 "s" "org.freedesktop.Notifications")
                         (3 "s" "NotificationClosed")
                         (8 "g" "uu")))
               (body (%call-with-bv-output
                      (lambda (p)
                        (%put-u32 p id)
                        (%put-u32 p reason))))
               (msg (%build-dbus-message 4 serial fields body)))
          (%socket-send-all! sock msg)))
      (lambda (k . args)
        (log-error "notification-daemon: Error sending NotificationClosed signal: ~a ~a" k args)))))

(define (%notification-daemon-restack!)
  "Rerender all active notifications so they stack vertically."
  (let* ((offset  *notification-daemon-margin*)
         (gap     *notification-daemon-gap*)
         (loc     *notification-daemon-location*)
         (bottom? (memv loc '(1 2 3))))
    (for-each
     (lambda (id)
       (let ((name (format #f "notification-~a" id)))
         (when (gleui-toast-visible? name)
           (let* ((h     (gleui-toast-get-height name))
                  (eff-h (if (and (number? h) (positive? h)) h 80)))
             (if bottom?
                 (gleui-toast-set-margin! name #:margin-bottom offset)
                 (gleui-toast-set-margin! name #:margin-top offset))
             (set! offset (+ offset eff-h gap))))))
     *notification-daemon-active-ids*)))

(define (%notification-daemon-on-toast-closed! id)
  "Called when a notification toast is dismissed or timed out."
  (when (memv id *notification-daemon-active-ids*)
    (set! *notification-daemon-active-ids* (delete id *notification-daemon-active-ids*))
    (when (and *notification-daemon-active-id* (= *notification-daemon-active-id* id))
      (set! *notification-daemon-active-id* (if (pair? *notification-daemon-active-ids*)
                                                (car *notification-daemon-active-ids*)
                                                #f)))
    (%dbus-emit-notification-closed! *notification-daemon-socket* id 1)
    (%notification-daemon-restack!)))

(define (%notification-daemon-show! formatted timeout notif-id icon theme-overrides)
  "Display notification toast and pad all existing notifications down."
  ;; remove replaces-id from active list if already present
  (set! *notification-daemon-active-ids* (delete notif-id *notification-daemon-active-ids*))

  ;; kill active notifications exceeding max-visible
  (let ((max-vis (or *notification-daemon-max-visible* 6)))
    (while (>= (length *notification-daemon-active-ids*) max-vis)
      (let ((oldest (last *notification-daemon-active-ids*)))
        (set! *notification-daemon-active-ids* (drop-right *notification-daemon-active-ids* 1))
        (gleui-toast-kill #:name (format #f "notification-~a" oldest)))))

  ;; add new notification to head/top of list
  (set! *notification-daemon-active-ids* (cons notif-id *notification-daemon-active-ids*))
  (set! *notification-daemon-active-id* notif-id)

  ;; display toast
  (gleui-toast-show formatted
                    #:timeout timeout
                    #:name (format #f "notification-~a" notif-id)
                    #:icon icon
                    #:theme-overrides theme-overrides
                    #:on-close (lambda () (%notification-daemon-on-toast-closed! notif-id)))

  ;; call restack to pad down rest of notifications
  (%notification-daemon-restack!))

(define-command (notification-daemon-dismiss-all!)
  "Dismiss all currently visible notification toasts."
  (let ((ids *notification-daemon-active-ids*))
    (for-each
     (lambda (id)
       (gleui-toast-kill #:name (format #f "notification-~a" id)))
     ids)))

(define-command (notification-daemon-redisplay-last!)
  "Redisplay the most recent notification from history."
  (if (null? *notification-daemon-history*)
      (log-info "notification-daemon: No notifications in history.")
      (let* ((last (car *notification-daemon-history*))
             (app-name (assoc-ref last 'app-name))
             (summary (assoc-ref last 'summary))
             (body (assoc-ref last 'body))
             (urgency (or (assoc-ref last 'urgency) 1))
             (icon (assoc-ref last 'icon))
             (formatted (%format-notification-message app-name summary body '() urgency))
             (notif-id (%next-id!))
             (theme-overrides (%notification-daemon-theme-overrides urgency icon)))
        (%notification-daemon-show! formatted
                                    *notification-daemon-timeout*
                                    notif-id
                                    icon
                                    theme-overrides))))

;; i prefer having this a separate xml file under the protocols directory
;; this will cause complexity installing tho as I currently don't install
;; protocols as part of gliver, so I am just embedding it for now
(define *notification-daemon-introspection-xml*
  (string-append
   "<!DOCTYPE node PUBLIC \"-//freedesktop//DTD D-BUS Object Introspection 1.0//EN\"\n"
   " \"http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd\">\n"
   "<node>\n"
   "  <interface name=\"org.freedesktop.Notifications\">\n"
   "    <method name=\"GetCapabilities\">\n"
   "      <arg direction=\"out\" name=\"capabilities\" type=\"as\"/>\n"
   "    </method>\n"
   "    <method name=\"Notify\">\n"
   "      <arg direction=\"in\" name=\"app_name\" type=\"s\"/>\n"
   "      <arg direction=\"in\" name=\"replaces_id\" type=\"u\"/>\n"
   "      <arg direction=\"in\" name=\"app_icon\" type=\"s\"/>\n"
   "      <arg direction=\"in\" name=\"summary\" type=\"s\"/>\n"
   "      <arg direction=\"in\" name=\"body\" type=\"s\"/>\n"
   "      <arg direction=\"in\" name=\"actions\" type=\"as\"/>\n"
   "      <arg direction=\"in\" name=\"hints\" type=\"a{sv}\"/>\n"
   "      <arg direction=\"in\" name=\"expire_timeout\" type=\"i\"/>\n"
   "      <arg direction=\"out\" name=\"id\" type=\"u\"/>\n"
   "    </method>\n"
   "    <method name=\"CloseNotification\">\n"
   "      <arg direction=\"in\" name=\"id\" type=\"u\"/>\n"
   "    </method>\n"
   "    <method name=\"GetServerInformation\">\n"
   "      <arg direction=\"out\" name=\"name\" type=\"s\"/>\n"
   "      <arg direction=\"out\" name=\"vendor\" type=\"s\"/>\n"
   "      <arg direction=\"out\" name=\"version\" type=\"s\"/>\n"
   "      <arg direction=\"out\" name=\"spec_version\" type=\"s\"/>\n"
   "    </method>\n"
   "    <signal name=\"NotificationClosed\">\n"
   "      <arg name=\"id\" type=\"u\"/>\n"
   "      <arg name=\"reason\" type=\"u\"/>\n"
   "    </signal>\n"
   "    <signal name=\"ActionInvoked\">\n"
   "      <arg name=\"id\" type=\"u\"/>\n"
   "      <arg name=\"action_key\" type=\"s\"/>\n"
   "    </signal>\n"
   "  </interface>\n"
   "  <interface name=\"org.freedesktop.DBus.Introspectable\">\n"
   "    <method name=\"Introspect\">\n"
   "      <arg direction=\"out\" name=\"data\" type=\"s\"/>\n"
   "    </method>\n"
   "  </interface>\n"
   "</node>\n"))

;;; method handling
(define (%handle-method-call! member sender serial path iface body-bv)
  (cond
   ((string=? member "Notify")
    (let ((args (%parse-notify-body body-bv)))
      (if args
          (match-let (((app-name replaces-id app-icon summary body actions hints expire-timeout) args))
            (let* ((urgency (or (assoc-ref hints "urgency") 1))
                   (icon (%extract-notification-icon app-icon hints))
                   (notif-id (if (and (number? replaces-id) (> replaces-id 0))
                                 replaces-id
                                 (%next-id!)))
                   (formatted (%format-notification-message app-name summary body actions urgency))
                   (timeout (cond
                             ((> expire-timeout 0) expire-timeout)
                             ((zero? expire-timeout) 0)
                             (else *notification-daemon-timeout*)))
                   (theme-overrides (%notification-daemon-theme-overrides urgency icon)))
              ;; send reply immediately so caller never waits
              (%dbus-reply! *notification-daemon-socket* sender serial "u" (%encode-u32 notif-id))
              (%add-to-history! notif-id app-name summary body urgency icon)
              (catch #t
                (lambda ()
              (%notification-daemon-show! formatted
                                          timeout
                                          notif-id
                                          icon
                                              theme-overrides))
                (lambda (k . args)
                  (log-error "notification-daemon: Error displaying toast: ~a ~a" k args)))))
          (%dbus-reply! *notification-daemon-socket* sender serial "u" (%encode-u32 0)))))

   ((string=? member "GetCapabilities")
    (%dbus-reply! *notification-daemon-socket* sender serial "as"
                  (%encode-string-array '("actions" "body" "body-markup" "persistence" "icon-static"))))

   ((string=? member "GetServerInformation")
    (%dbus-reply! *notification-daemon-socket* sender serial "ssss"
                  (%encode-4-strings "Gliver Notification Daemon" "Gliver" "0.1" "1.2")))

   ((string=? member "CloseNotification")
    (catch #t
      (lambda ()
        (let* ((p (open-bytevector-input-port body-bv))
               (close-id (%get-u32 p)))
          (gleui-toast-kill #:name (format #f "notification-~a" close-id))))
      (lambda _ #f))
    (%dbus-reply! *notification-daemon-socket* sender serial "" #f))

   ((string=? member "Introspect")
    (%dbus-reply! *notification-daemon-socket* sender serial "s"
                  (%encode-string *notification-daemon-introspection-xml*)))

   (else
    (%dbus-error-reply! *notification-daemon-socket*
                        sender
                        serial
                        "org.freedesktop.DBus.Error.UnknownMethod"
                        (format #f "Method ~a is not supported on interface ~a" member (or iface ""))))))

(define (%unpack-dbus-message bv)
  "Unpack a raw dbus message bytevector into a list."
  (let* ((mtype (bytevector-u8-ref bv 1))
         (serial (bytevector-u32-ref bv 8 (endianness little)))
         (blen (bytevector-u32-ref bv 4 (endianness little)))
         (hlen (bytevector-u32-ref bv 12 (endianness little)))
         (rem (modulo (+ 16 hlen) 8))
         (pad (if (zero? rem) 0 (- 8 rem)))
         (hfields-bv (%bytevector-slice bv 16 hlen))
         (body-bv (if (> blen 0)
                      (%bytevector-slice bv (+ 16 hlen pad) blen)
                      (make-bytevector 0 0))))
    (list mtype serial hfields-bv body-bv)))

(define (%handle-dbus-message! bv)
  (let ((unpacked (catch #t
                    (lambda () (%unpack-dbus-message bv))
                    (lambda (k . args)
                      (log-error "notification-daemon: Error unpacking D-Bus message: ~a ~a" k args)
                      #f))))
    (when unpacked
      (match-let (((mtype serial hfields-bv body-bv) unpacked))
        (let* ((fields (catch #t
                         (lambda () (%parse-header-fields hfields-bv))
                         (lambda (k . args)
                           (log-error "notification-daemon: Error parsing header fields: ~a ~a" k args)
                           '())))
               (member (assoc-ref fields 3))
               (sender (assoc-ref fields 7))
               (path (assoc-ref fields 1))
               (iface (assoc-ref fields 2)))
          (when (and (= mtype 1) member sender) ;; METHOD_CALL
            (catch #t
              (lambda ()
                (%handle-method-call! member sender serial path iface body-bv))
    (lambda (k . args)
                (log-error "notification-daemon: Error executing method ~a: ~a ~a" member k args)
                (%dbus-error-reply! *notification-daemon-socket*
                                    sender
                                    serial
                                    "org.freedesktop.DBus.Error.Failed"
                                    (format #f "Internal error handling method ~a" member))))))))))

(define (%pop-message!)
  "Extract the next complete dbus message bytevector from the input buffer."
  (let ((buf-len (bytevector-length *notification-daemon-dbus-in-buffer*)))
    (if (< buf-len 16)
        #f
        (let* ((blen (bytevector-u32-ref *notification-daemon-dbus-in-buffer* 4 (endianness little)))
               (hlen (bytevector-u32-ref *notification-daemon-dbus-in-buffer* 12 (endianness little)))
               (rem (modulo (+ 16 hlen) 8))
               (pad (if (zero? rem) 0 (- 8 rem)))
               (total (+ 16 hlen pad blen)))
          (if (< buf-len total)
              #f
              (let ((msg-bv (%bytevector-slice *notification-daemon-dbus-in-buffer* 0 total)))
                (set! *notification-daemon-dbus-in-buffer*
                      (%bytevector-slice *notification-daemon-dbus-in-buffer* total (- buf-len total)))
                msg-bv))))))

(define (%process-in-buffer!)
  (let loop ()
    (let ((msg-bv (%pop-message!)))
      (when msg-bv
        (%handle-dbus-message! msg-bv)
        (loop)))))

(define (notification-daemon-poll!)
  "Poll the dbus socket for incoming messages.
This should be called manually via tick hook for instance"
  (when *notification-daemon-socket*
    (catch #t
      (lambda ()
        (let loop ()
          (when (and *notification-daemon-socket*
                     (pair? (car (select (list *notification-daemon-socket*) '() '() 0 0))))
            (let* ((scratch (make-bytevector 4096 0))
                   (n (catch 'system-error
                        (lambda () (recv! *notification-daemon-socket* scratch MSG_DONTWAIT))
                        (lambda (k . args) #f))))
              (cond
               ((and (number? n) (> n 0))
                (set! *notification-daemon-dbus-in-buffer*
                      (%bytevector-append *notification-daemon-dbus-in-buffer* (%bytevector-slice scratch 0 n)))
                (%process-in-buffer!)
                (loop))
               ((and (number? n) (= n 0))
                (log-info "notification-daemon: D-Bus socket closed by peer.")
                (notification-daemon-stop!))
               (else #f))))))
      (lambda (key . args)
        (log-error "notification-daemon: Polling error: ~a ~a" key args)))))

(define (%recv-with-timeout! s scratch timeout-sec)
  "Read with timeout TIMEOUT-SEC."
  (let ((ready (select (list s) '() '() timeout-sec 0)))
    (if (pair? (car ready))
        (recv! s scratch 0)
        #f)))

(define (%read-next-dbus-msg! sock timeout-sec)
  "Wait for and extract the next dbus message during handshake."
  (let loop ()
    (let ((msg-bv (%pop-message!)))
      (cond
       (msg-bv
        (%unpack-dbus-message msg-bv))
       (else
        (let* ((scratch (make-bytevector 4096 0))
               (n (%recv-with-timeout! sock scratch timeout-sec)))
          (if (and (number? n) (> n 0))
              (begin
                (set! *notification-daemon-dbus-in-buffer*
                      (%bytevector-append *notification-daemon-dbus-in-buffer*
                                          (%bytevector-slice scratch 0 n)))
                (loop))
              #f)))))))

(define-command (notification-daemon-start!)
  "Start the dbus notification daemon.
Returns bool that indicates whether daemon has been started successfully."

  (if *notification-daemon-socket*
	  ;; socket is already running, return true
      (begin
        (log-info "notification-daemon: Already running.")
        #t)
      (let ((path (%dbus-session-bus-path)))
        (if (not path)
			;; socket path wasn't properly detected, return false
            (begin
              (log-warn "notification-daemon: DBUS_SESSION_BUS_ADDRESS not found; daemon not started.")
              #f)

			(catch #t
              (lambda ()
                ;; connect via posix socket & sasl auth
                (let ((sock (%dbus-connect path)))
                  (set! *notification-daemon-socket* sock)
                  (set! *notification-daemon-dbus-in-buffer* (make-bytevector 0 0))

                  ;; send hello and wait for reply (handshake)
                  (let ((hello-msg (%build-dbus-message 1 (%next-serial!)
														'((1 "o" "/org/freedesktop/DBus")
                                                          (6 "s" "org.freedesktop.DBus")
                                                          (2 "s" "org.freedesktop.DBus")
                                                          (3 "s" "Hello"))
														#f)))
                    (%socket-send-all! sock hello-msg))

				  ;; wait for a response from hello message
                  (let loop-hello ()
                    (let ((msg (%read-next-dbus-msg! sock 3)))
                      (cond
                       ((not msg)
                        (error "Timeout waiting for Hello reply"))
                       ((= (car msg) 2)
                        #t)
                       (else
                        (loop-hello)))))

				  ;; own the RequestName org.freedesktop.Notifications
                  ;; flags: 7 = REPLACE_EXISTING (1) | ALLOW_REPLACEMENT (2) | DO_NOT_QUEUE (4)
                  (let-values (((p get-bv) (open-bytevector-output-port)))
                    (%put-dbus-string p "org.freedesktop.Notifications")
                    (%put-u32 p 7)
                    (let ((req-msg (%build-dbus-message 1 (%next-serial!)
														'((1 "o" "/org/freedesktop/DBus")
                                                          (6 "s" "org.freedesktop.DBus")
                                                          (2 "s" "org.freedesktop.DBus")
                                                          (3 "s" "RequestName")
                                                          (8 "g" "su"))
														(get-bv))))
                      (%socket-send-all! sock req-msg)))

                  ;; wait for RequestName reply
                  (let loop-req ()
                    (let ((msg (%read-next-dbus-msg! sock 3)))
                      (cond
                       ((not msg)
                        (error "Timeout waiting for RequestName reply"))
                       ((= (car msg) 2)
                        (let ((code (bytevector-u32-ref (cadddr msg) 0 (endianness little))))
                          (if (or (= code 1) (= code 4))
                              (begin
                                ;; set socket to non-blocking mode
                                (fcntl sock F_SETFL (logior (fcntl sock F_GETFL) O_NONBLOCK))
								;; poll socket every tick
                                (gliver-hook-add! *manager-tick-hook* 'notification-daemon-poll!)
                                (log-info "notification-daemon: Acquired org.freedesktop.Notifications.")
                                #t)
                              (begin
                                (log-warn "notification-daemon: Could not acquire org.freedesktop.Notifications (code ~a)." code)
                                (close-port sock)
                                (set! *notification-daemon-socket* #f)
                                #f))))
                       (else
                        (loop-req)))))))

              (lambda (key . args)
                (log-error "notification-daemon: Failed to start: ~a ~a" key args)
                (when *notification-daemon-socket*
                  (close-port *notification-daemon-socket*)
                  (set! *notification-daemon-socket* #f))
                #f))))))

(define-command (notification-daemon-stop!)
  "Stop the dbus notification daemon.
Returns bool that indicates whether daemon has been stopped successfully."
  (catch #t
    (lambda () (notification-daemon-dismiss-all!))
    (lambda _ #f))
  (set! *notification-daemon-active-ids* '())
  (set! *notification-daemon-active-id* #f)
  (when *notification-daemon-socket*
    (log-info "notification-daemon: Stopping...")
    (gliver-hook-remove! *manager-tick-hook* 'notification-daemon-poll!)
    (catch #t
      (lambda ()
        (close-port *notification-daemon-socket*))
      (lambda _ #f))
    (set! *notification-daemon-socket* #f)
    (set! *notification-daemon-dbus-in-buffer* (make-bytevector 0 0))
    (log-info "notification-daemon: Stopped.")
    #t))

(define-command (notification-daemon-restart!)
  "Restart the dbus notification daemon."
  (notification-daemon-stop!)
  (notification-daemon-start!))
