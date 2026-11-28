;;; gliver/contrib/ui/gleui/notifications.scm --- Native D-Bus Notification Daemon
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module implements the org.freedesktop.Notifications D-Bus daemon
;;; using (gliver deps dbus) and gleui toasts.
;;; It handles:
;;;  - Configuring and activating notification daemon
;;;  - Decoding and displaying notification payloads
;;;  - Dispatching D-Bus method calls, emitting signals, and daemon lifecycle
;;;
;;; NOTE: this uses unix sockets via (gliver deps dbus) rather than libdbus ffi.
;;; as far as I am concerned, this is the recommended approach as per freedesktop

(define-module (gliver contrib ui gleui notifications)
  #:use-module (cairo)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 threads)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (gliver core)
  #:use-module (gliver deps pango)
  #:use-module (gliver deps dbus)
  #:use-module (gliver contrib ui gleui base)
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
            *notification-daemon-border-urgent-color*
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
            *notification-daemon-dbus*
            *notification-daemon-id-counter*
            *notification-daemon-active-id*
            *notification-daemon-active-ids*
            *notification-daemon-history*
            notification-daemon-clear-history!
            notification-daemon-dismiss-all!
            notification-daemon-redisplay-last!
            notification-daemon-poll!
            notification-daemon-start!
            notification-daemon-stop!
            notification-daemon-restart!
))

(define-var *notification-daemon-enabled* #t
            "Whether the Gliver notification daemon should be started with gleui components.")
(define-var *notification-daemon-timeout* #:inherit *message-timeout*
            "Expiration timeout for notifications in seconds.")
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

(define-var *notification-daemon-font* #:inherit *gleui-toast-font*
            "Font used for gleui toast notifications.")
(define-var *notification-daemon-font-size* #:inherit *gleui-toast-font-size*
            "Font size used for gleui toast notifications.")
(define-var *notification-daemon-bg-color* #:inherit *gleui-toast-bg-color*
            "Background color for gleui toast notifications.")
(define-var *notification-daemon-fg-color* #:inherit *gleui-toast-fg-color*
            "Text color for gleui toast notifications.")
(define-var *notification-daemon-border-color* #:inherit *gleui-toast-border-color*
            "Border color for gleui toast notifications.")
(define-var *notification-daemon-border-urgent-color* #:inherit *theme-urgent-color*
            "Border color for urgent gleui toast notifications.")
(define-var *notification-daemon-border-width* #:inherit *gleui-toast-border-width*
            "Border width in pixels for gleui toast notifications.")
(define-var *notification-daemon-border-radius* #:inherit *gleui-toast-border-radius*
            "Corner border radius in pixels for gleui toast notifications.")
(define-var *notification-daemon-location* #:inherit *gleui-toast-location*
            "Gliver position (1-9) for gleui toast notifications.")
(define-var *notification-daemon-margin* #:inherit *gleui-toast-margin*
            "Margin in pixels from the screen edge for gleui toast notifications.")
(define-var *notification-daemon-padding-x* #:inherit *gleui-toast-padding-x*
            "Padding horizontal content in pixels for gleui toast notifications.")
(define-var *notification-daemon-padding-y* #:inherit *gleui-toast-padding-y*
            "Padding vertical content in pixels for gleui toast notifications.")
(define-var *notification-daemon-width* 500
            "Fixed width in pixels for notification daemon toasts.")
(define-var *notification-daemon-icon-show* #:inherit *gleui-toast-icon-show*
            "Whether to display icons in notification daemon toasts.")
(define-var *notification-daemon-icon-location* #:inherit *gleui-toast-icon-location*
            "Icon placement in notification daemon toasts: 'left or 'right.")
(define-var *notification-daemon-icon-border-radius* #:inherit *gleui-toast-icon-border-radius*
            "Corner border radius in pixels for icons in notification daemon toasts.")
(define-var *notification-daemon-icon-border-width* #:inherit *gleui-toast-icon-border-width*
            "Border width in pixels around icons in notification daemon toasts.")
(define-var *notification-daemon-icon-border-color* #:inherit *gleui-toast-icon-border-color*
            "Border color around icons in notification daemon toasts.")
(define-var *notification-daemon-icon-size* #:inherit *gleui-toast-icon-size*
            "Icon dimensions (width and height) in pixels in notification daemon toasts.")

(define-var *notification-daemon-dbus* #f
            "Connected <dbus-connection> record, or #f when stopped.")
(define-var *notification-daemon-id-counter* 1
            "Monotonic freedesktop notification ID counter.")
(define-var *notification-daemon-active-id* #f
            "Most recently shown active notification ID, or #f.")
(define-var *notification-daemon-active-ids* '()
            "Ordered list of currently visible notification IDs (newest first).")
(define-var *notification-daemon-history* '()
            "Bounded list of recent notification history alists (newest first).")

(define %notification-service-name "org.freedesktop.Notifications")
(define %notification-object-path  "/org/freedesktop/Notifications")
;; dbus may spam notifications in another thread, safe threading
;; must be used when needed to prevent race conditions
(define %notification-state-mutex (make-mutex))
(define %notification-pending-queue '())
;; burst cooldown to prevent spamming notifications from freezing gliver (50ms)
(define %notification-burst-cooldown
  (quotient (* 50 internal-time-units-per-second) 1000))
;; last shown notification time to ensure cooldown passed before showing next
(define %notification-last-show-time (- %notification-burst-cooldown))

(define (%next-id!)
  "Return and increment the next notification ID.
Parameters: None.
Returns positive integer notification ID."
  (with-mutex %notification-state-mutex
    (let ((id *notification-daemon-id-counter*))
      (set! *notification-daemon-id-counter* (1+ *notification-daemon-id-counter*))
      id)))

(define (%notification-toast-name id)
  "Build the toast instance name corresponding to notification ID.
ID is concatenated with 'notification' to create a unique toast name.
Parameters: (ID)
  - ID: Integer notification identifier.
Returns formatted string toast instance name."
  (format #f "notification-~a" id))

(define (%add-to-history! id app-name summary body urgency icon)
  "Prepend a notification entry to `*notification-daemon-history*'
Parameters: (ID APP-NAME SUMMARY BODY URGENCY ICON)
  - ID: Integer assigned notification ID.
  - APP-NAME: String or #f application name.
  - SUMMARY: String or #f notification summary text.
  - BODY: String or #f notification body text.
  - URGENCY: Integer urgency level (0=low, 1=normal, 2=critical).
  - ICON: String, list, or #f icon path/name or decoded '(image-data ...) payload."
  (let ((entry `((id . ,id)
                 (app-name . ,app-name)
                 (summary . ,summary)
                 (body . ,body)
                 (urgency . ,urgency)
                 (icon . ,icon)
                 (timestamp . ,(current-time)))))
    (with-mutex %notification-state-mutex
      (set! *notification-daemon-history*
            (cons entry *notification-daemon-history*))
      ;; pop last notification in history if it exceed max length
      (when (> (length *notification-daemon-history*) *notification-daemon-max-history*)
        (set! *notification-daemon-history*
              (take *notification-daemon-history* *notification-daemon-max-history*))))))

(define-command (notification-daemon-clear-history!)
  "Clear the stored notification history.
Parameters: None."
  (with-mutex %notification-state-mutex
    (set! *notification-daemon-history* '()))
  (log-info "notification-daemon: history cleared"))

;;; notification dbus wire codec signals
(define (%read-image-data port)
  "Decode a freedesktop D-Bus (iiibiiay) image-data struct from PORT.
https://specifications.freedesktop.org/notification/latest-single/#icons-and-images-formats
Parameters: (PORT)
  - PORT: Input port supporting seek (binary) inside a variant payload.
Returns decoded '(image-data WIDTH HEIGHT ROWSTRIDE HAS-ALPHA BPS CHANNELS DATA) list, or #f."
  (dbus-get-align port 8)
  (let* ((width            (dbus-get-s32 port))
         (height           (dbus-get-s32 port))
         (rowstride        (dbus-get-s32 port))
         (has-alpha        (not (zero? (dbus-get-u32 port))))
         (bits-per-sample  (dbus-get-s32 port))
         (channels         (dbus-get-s32 port))
         (data-len         (dbus-get-u32 port))
         (data             (get-bytevector-n port data-len)))
    (and (bytevector? data)
         (list 'image-data width height rowstride has-alpha
               bits-per-sample channels data))))

(define %notification-hint-readers
  `(("(iiibiiay)" . ,%read-image-data)))

(define (%parse-notify-body bv)
  "Parse org.freedesktop.Notifications.Notify body bytevector BV.
Parameters: (BV)
  - BV: Bytevector holding the D-Bus Notify method call body.
Returns list (APP-NAME REPLACES-ID APP-ICON SUMMARY BODY ACTIONS HINTS EXPIRE-TIMEOUT), or #f on error."
  (catch #t
    (lambda ()
      (let* ((p              (open-bytevector-input-port bv))
             (app-name       (dbus-get-string p))
             (replaces-id    (dbus-get-u32 p))
             (app-icon       (dbus-get-string p))
             (summary        (dbus-get-string p))
             (body           (dbus-get-string p))
             (actions        (dbus-get-string-array p))
             (hints          (dbus-get-dict p #:custom-readers %notification-hint-readers))
             (expire-timeout (dbus-get-s32 p)))
        (list app-name replaces-id app-icon summary body actions hints expire-timeout)))
    (lambda (k . args)
      (log-error "notification-daemon: error parsing Notify body: ~a ~a" k args)
      #f)))

(define (%dbus-emit-notification-closed! conn id reason)
  "Emit NotificationClosed for ID and REASON over D-Bus CONN.
Parameters: (CONN ID REASON)
  - CONN: <dbus-connection> record or #f.
  - ID: Integer notification identifier.
  - REASON: Integer close reason code per freedesktop specification."
  (dbus-emit-signal! conn
                     %notification-object-path
                     %notification-service-name
                     "NotificationClosed"
                     "uu"
                     (lambda (p)
                       (dbus-put-u32 p id)
                       (dbus-put-u32 p reason))))

(define (%dbus-emit-action-invoked! conn id action-key)
  "Emit ActionInvoked for ID and ACTION-KEY over D-Bus CONN.
Parameters: (CONN ID ACTION-KEY)
  - CONN: <dbus-connection> record or #f.
  - ID: Integer notification identifier.
  - ACTION-KEY: String invoked action identifier."
  (dbus-emit-signal! conn
                     %notification-object-path
                     %notification-service-name
                     "ActionInvoked"
                     "us"
                     (lambda (p)
                       (dbus-put-u32 p id)
                       (dbus-put-string p action-key))))

;; these functions are all just to parse components from dbus notification
;; response and return the needed data/widgets to be used in %notification-daemon-show!
(define (%build-toast-actions notif-id actions)
  "Convert paired D-Bus ACTIONS for NOTIF-ID into clickable gleui button callbacks.
Parameters: (NOTIF-ID ACTIONS)
  - NOTIF-ID: Integer notification identifier passed to ActionInvoked.
  - ACTIONS: List of alternating action keys and display labels.
Returns list of (LABEL . CALLBACK) pairs."
  ;; only return list of actions if show actions is enabled
  (if (and *notification-daemon-show-actions*
           (pair? actions))
      (let loop ((remaining actions) (buttons '()))
        (if (or (null? remaining) (null? (cdr remaining)))
            (reverse buttons)
            ;; actions has key followed by label in a flat list
            ;; so car for key and cadr for label
            (let* ((action-key   (car remaining))
                   (raw-label    (cadr remaining))
                   (button-label (if (string? raw-label) raw-label ""))
                   ;; emit action and kill toast on click
                   (on-click     (lambda ()
                                   (%dbus-emit-action-invoked! *notification-daemon-dbus*
                                                               notif-id
                                                               action-key)
                                   (gleui-toast-kill! #:name (%notification-toast-name notif-id)))))
              ;; for loop we will skip next item which is actually the
              ;; label of the current key, so use cddr
              (loop (cddr remaining)
                    (cons (cons button-label on-click) buttons)))))
      '()))

(define (%format-notification-message app-name summary body actions urgency)
  "Format APP-NAME, SUMMARY, and BODY into Pango markup for toast display.
Parameters: (APP-NAME SUMMARY BODY ACTIONS URGENCY)
  - APP-NAME: String or #f application name.
  - SUMMARY: String or #f notification title.
  - BODY: String or #f notification body.
  - ACTIONS: List notification actions (unused).
  - URGENCY: Integer notification urgency level (unused).
Returns formatted Pango markup string."
  (let* ((badge (if (and *notification-daemon-show-app-name*
                         (string? app-name)
                         (not (string-null? app-name)))
                    (format #f "<span alpha=\"70%\" size=\"small\">[~a]</span> "
                            (pango-escape-markup app-name))
                    ""))
         (safe-summary (if (string? summary) summary ""))
         (title-line (string-append badge "<b>" (pango-escape-markup safe-summary) "</b>"))
         (lines (list title-line)))
    (when (and (string? body) (not (string-null? body)))
      (set! lines (cons (gleui-pango-markup-ensure body) lines)))
    (string-join (reverse lines) "\n")))

(define (%extract-notification-icon app-icon hints)
  "Resolve the preferred notification icon from HINTS or APP-ICON.
Parameters: (APP-ICON HINTS)
  - APP-ICON: String or #f application icon path or freedesktop icon name.
  - HINTS: Alist or #f of decoded D-Bus notification hints.
Returns icon string, decoded '(image-data ...) payload, or #f."
  (let ((hint-img (and (list? hints)
                       (or (assoc-ref hints "image-path")
                           (assoc-ref hints "image_path"))))
        (hint-data (and (list? hints)
                        (or (assoc-ref hints "image-data")
                            (assoc-ref hints "image_data")
                            (assoc-ref hints "icon_data")))))
    (cond
     ;; hint data is image from app, like avatar from messaging
     ;; platforms notifications coming from browsers
     ((and (pair? hint-data)
           (eq? (car hint-data) 'image-data))
      hint-data)
     ;; hint image has image name or path, this may be used
     ;; for battery icon for example
     ((and (string? hint-img)
           (not (string-null? hint-img)))
      hint-img)
     ;; otherwise icon of app sending the notification
     ((and (string? app-icon)
           (not (string-null? app-icon)))
      app-icon)
     (else #f))))

(define* (%notification-daemon-theme-overrides urgency #:optional (icon #f))
  "Build toast theme overrides for notification.
Parameters: (URGENCY [ICON])
  - URGENCY: Integer notification urgency level (0 = low, 1 = normal, 2 = critical).
  - ICON: String, list, or #f icon specification bound under the 'icon key.
Returns alist of toast theme override keys and values."
  `((font . ,*notification-daemon-font*)
    (font-size . ,*notification-daemon-font-size*)
    (background . ,*notification-daemon-bg-color*)
    (foreground . ,*notification-daemon-fg-color*)
    (border-color . ,(case urgency
                       ((2) *notification-daemon-border-urgent-color*)
                       ((0) *notification-daemon-border-color*)
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

(define (%image-data->cairo-surface data-list)
  "Convert decoded D-Bus DATA-LIST into an ARGB32 Cairo image surface.
Parameters: (DATA-LIST)
  - DATA-LIST: List decoded as ('image-data width height rowstride has-alpha bps channels data).
Returns Cairo image surface, or #f on invalid input or error."
  (match data-list
    (('image-data width height rowstride has-alpha bps channels data)
     (catch #t
       (lambda ()
         ;; cairo argb32 uses 4 bytes per pixel
         ;; some aliases are introduced to make code easier below
         (let* ((src-stride rowstride)
                (src-channels channels)
                (dst-channels 4)
                (dst-stride (* width dst-channels))
                (dst-bv (make-bytevector (* dst-stride height) 0)))
           ;; loop each y and x pixel
           (do ((y 0 (1+ y)))
               ((>= y height))
             (do ((x 0 (1+ x)))
                 ((>= x width))
               ;; dst-channels maybe (typically 3 for rgb or 4 for rgba).
               (let* ((src-off (+ (* y src-stride) (* x src-channels)))
                      (dst-off (+ (* y dst-stride) (* x dst-channels)))
                      (r (bytevector-u8-ref data src-off))
                      (g (bytevector-u8-ref data (+ src-off 1)))
                      (b (bytevector-u8-ref data (+ src-off 2)))
                      ;; default to fully opaque when the source omits an alpha channel.
                      (a (if (and has-alpha (>= src-channels 4))
                             (bytevector-u8-ref data (+ src-off 3))
                             255)))
                 ;; cairo 'argb32 stores 32-bit pixels in host endianness
                 ;; little-endian hosts that is byte order b, g, r, a.
                 (bytevector-u8-set! dst-bv dst-off b)
                 (bytevector-u8-set! dst-bv (+ dst-off 1) g)
                 (bytevector-u8-set! dst-bv (+ dst-off 2) r)
                 (bytevector-u8-set! dst-bv (+ dst-off 3) a))))
           (cairo-image-surface-create-for-data dst-bv 'argb32 width height dst-stride)))
       (lambda (k . args)
         (log-error "notification-daemon: Failed to convert image-data to surface: ~a ~a" k args)
         #f)))
    (_ #f)))

(define (%notification-daemon-restack!)
  "Reposition all active notification toasts so they stack vertically.
When a new notification shows up while other are still visible, the expected
behavior is that all notifications are pushed away (below if location is top,
and above if location bottom).
Parameters: None."
  (let* ((offset  *notification-daemon-margin*)
         (gap     *notification-daemon-gap*)
         (loc     *notification-daemon-location*)
         ;; this won't work well for 4 5 6, but hopefully
         ;; no one will use these weird locations :)
         (bottom? (memv loc '(1 2 3))))
    (for-each
     (lambda (id)
       (and-let* ((state (gleui-toast-get-instance
                          (%notification-toast-name id)))
                  ((gleui-toast-state-visible? state)))
         (let* ((height           (gleui-toast-state-height state))
                ;; there should be a height always, and it's dynamic
                (effective-height (if (and (number? height)
                                           (positive? height))
                                      height
                                      80)))
           ;; margin bottom or top based on notification location
           (if bottom?
               (gleui-toast-set-margin! state #:margin-bottom offset)
               (gleui-toast-set-margin! state #:margin-top offset))
           ;; increase offset so next notification shows up above/below
           (set! offset
                 (+ offset effective-height gap)))))
     *notification-daemon-active-ids*)))

(define (%notification-daemon-on-toast-closed! id)
  "Invoked when notification toast is dismissed or expired.
Parameters: (ID)
  - ID: Integer notification identifier of the closed toast."
  (when (memv id *notification-daemon-active-ids*)
    (let ((remaining (delete id *notification-daemon-active-ids*)))
      (set! *notification-daemon-active-ids* remaining)
      (when (and *notification-daemon-active-id*
                 (= *notification-daemon-active-id* id))
        ;; update the current active, #f if no notifications are currently
        ;; active or first if some exist, this could happen if notification is dismissed
        (set! *notification-daemon-active-id*
              (if (pair? remaining)
                  (car remaining)
                  #f)))
      (%dbus-emit-notification-closed! *notification-daemon-dbus* id 1)
      ;; we will have to restack again, because newer notifications maybe dismissed early
      (%notification-daemon-restack!))))

(define* (%notification-daemon-show! formatted timeout notif-id icon theme-overrides #:optional (actions '()))
  "Display notification NOTIF-ID with FORMATTED markup.
Parameters: (FORMATTED TIMEOUT NOTIF-ID ICON THEME-OVERRIDES [ACTIONS])
  - FORMATTED: String formatted Pango markup.
  - TIMEOUT: Float auto-dismiss delay in seconds, or 0 to disable.
  - NOTIF-ID: Integer notification identifier.
  - ICON: String, list, or #f icon path/name or '(image-data ...) payload.
  - THEME-OVERRIDES: Alist of toast theme override keys and values.
  - ACTIONS: List of flat D-Bus action keys followed by labels (even)."

  ;; kill all toasts that exceed the max visible and add new one as current active
  (let ((ids     (delete notif-id *notification-daemon-active-ids*))
        (max-vis (or *notification-daemon-max-visible* 6)))
    (let loop ((cur ids))
      ;; while length is more than max-vis
      (if (>= (length cur) max-vis)
          (let ((oldest (last cur)))
            (gleui-toast-kill! #:name (%notification-toast-name oldest))
            (loop (drop-right cur 1)))
          (set! ids cur)))
    (set! *notification-daemon-active-ids* (cons notif-id ids))
    (set! *notification-daemon-active-id* notif-id))

  ;; convert raw dbus image-data payloads just before display so history remains reusable
  (let* ((image-data-pair? (lambda (val)
                             (and (pair? val) (eq? (car val) 'image-data))))
         (eff-icon (if (image-data-pair? icon)
                       (%image-data->cairo-surface icon)
                       icon))
         (eff-overrides (if (and (list? theme-overrides)
                                 (image-data-pair? (assq-ref theme-overrides 'icon)))
                            (map (lambda (entry)
                                   (if (eq? (car entry) 'icon)
                                       (cons 'icon eff-icon)
                                       entry))
                                 theme-overrides)
                            theme-overrides)))
    (gleui-toast-show formatted
                      #:timeout timeout
                      #:name (%notification-toast-name notif-id)
                      #:icon eff-icon
                      #:theme-overrides eff-overrides
                      #:actions (%build-toast-actions notif-id actions)
                      #:on-close (lambda () (%notification-daemon-on-toast-closed! notif-id))))
  ;; restack as a new notification must push the older ones
  (%notification-daemon-restack!))

(define-command (notification-daemon-dismiss-all!)
  "Dismiss all currently visible notification toasts.
Parameters: None."
  (for-each
   (lambda (id)
     (gleui-toast-kill! #:name (%notification-toast-name id)))
   *notification-daemon-active-ids*))

(define-command (notification-daemon-redisplay-last!)
  "Redisplay the most recent notification stored in daemon history.
Parameters: None."
  (if (null? *notification-daemon-history*)
      (log-info "notification-daemon: no notifications in history")
      (let* ((last-entry (car *notification-daemon-history*))
             (app-name (assoc-ref last-entry 'app-name))
             (summary (assoc-ref last-entry 'summary))
             (body (assoc-ref last-entry 'body))
             (urgency (or (assoc-ref last-entry 'urgency) 1))
             (icon (assoc-ref last-entry 'icon))
             (formatted (%format-notification-message app-name summary body '() urgency))
             (notif-id (%next-id!))
             (theme-overrides (%notification-daemon-theme-overrides urgency icon)))
        (%notification-daemon-show! formatted
                                    *notification-daemon-timeout*
                                    notif-id
                                    icon
                                    theme-overrides))))

(define (%pop-pending-notification!)
  "Pop the oldest queued notification and update the last-show timestamp.
Parameters: None.
Returns list (FORMATTED TIMEOUT NOTIF-ID ICON THEME-OVERRIDES ACTIONS), or #f if empty."
  ;; mutex to prevent modifying queue from other threads
  (with-mutex %notification-state-mutex
    (if (null? %notification-pending-queue)
        #f
        ;; queue is stored newest-first, so pop from the tail
        (let ((next (last %notification-pending-queue)))
          ;; remove the tail and update last show time
          (set! %notification-pending-queue
                (drop-right %notification-pending-queue 1))
          (set! %notification-last-show-time
                (get-internal-real-time))
          next))))

(define (%display-next-notification!)
  "Pop and display one queued notification on the main thread.
Parameters: None."
  (let ((job (%pop-pending-notification!)))
    ;; create the layer surface outside the mutex so d-bus notify calls can be received
    (when job
      (match-let (((formatted timeout notif-id icon theme-overrides actions) job))
        (catch #t
          (lambda ()
            (%notification-daemon-show! formatted timeout notif-id
                                        icon theme-overrides actions))
          (lambda (k . args)
            (log-error "notification-daemon: Error displaying toast: ~a ~a" k args)))))))

(define (%notification-on-tick)
  "Invoked on manage tick, pop and display a notification from queue if available.
Parameters: None."
  ;; throttling here keeps main-loop wayland dispatch responsive during notification floods
  (let ((due? (with-mutex %notification-state-mutex
                (and (pair? %notification-pending-queue)
                     (>= (- (get-internal-real-time) %notification-last-show-time)
                         %notification-burst-cooldown)))))
    ;; only display after cooldown is passed, otherwise wait for next tick
    (when due?
      (%display-next-notification!))))

(define (%enqueue-notification-show! formatted timeout notif-id icon theme-overrides actions)
  "Queue a notification for main-thread display.
Parameters: (FORMATTED TIMEOUT NOTIF-ID ICON THEME-OVERRIDES ACTIONS)
  - FORMATTED: String formatted Pango markup.
  - TIMEOUT: Float auto-dismiss delay in seconds, or 0 to disable.
  - NOTIF-ID: Integer notification identifier.
  - ICON: String, list, or #f icon path/name or '(image-data ...) payload.
  - THEME-OVERRIDES: Alist of toast theme override keys and values.
  - ACTIONS: List of flat D-Bus action keys followed by labels."
  (with-mutex %notification-state-mutex
    (let* ((max-vis  (max 1 (or *notification-daemon-max-visible* 6)))
           ;; replace any older unshown entry sharing notif-id (replaces_id updates)
           (filtered (filter (lambda (item)
                               (not (equal? (list-ref item 2) notif-id)))
                             %notification-pending-queue))
           (updated  (cons (list formatted timeout notif-id icon theme-overrides actions)
                           filtered)))
      ;; only keep most max-vis unshown items
      (set! %notification-pending-queue
            (if (> (length updated) max-vis)
                (take updated max-vis)
                updated)))))

(define (%handle-notify! sender serial body-bv)
  "Decode a Notify call from SENDER and display it.
This will be invoked when an application sends a notification message.
Parameters: (SENDER SERIAL BODY-BV)
  - SENDER: String unique D-Bus sender bus name.
  - SERIAL: Integer request serial number for the reply.
  - BODY-BV: Bytevector containing raw D-Bus Notify method arguments."
  (let ((args (%parse-notify-body body-bv)))
    (if (not args)
        ;; no body found, we must reply with id 0 which means failure
        (dbus-reply! *notification-daemon-dbus* sender serial "u" (dbus-encode-u32 0))
        ;; otherwise, we will get our arguments from body
        (match-let (((app-name replaces-id app-icon summary body actions hints expire-timeout) args))
          (let* ((urgency (or (assoc-ref hints "urgency") 1))
                 (icon (%extract-notification-icon app-icon hints))
                 (notif-id (if (and (number? replaces-id) (> replaces-id 0))
                               replaces-id
                               (%next-id!)))
                 (formatted (%format-notification-message app-name summary body actions urgency))
                 ;; freedesktop Notify passes expire_timeout in milliseconds; toast expects seconds
                 (timeout (cond
                           ((> expire-timeout 0)
                            (/ expire-timeout 1000.0))
                           ((zero? expire-timeout)
                            0)
                           (else *notification-daemon-timeout*)))
                 (theme-overrides (%notification-daemon-theme-overrides urgency icon)))
            ;; send reply with new notification id, this means a success
            (dbus-reply! *notification-daemon-dbus* sender serial "u" (dbus-encode-u32 notif-id))
            (%add-to-history! notif-id app-name summary body urgency icon)
            (%enqueue-notification-show! formatted timeout notif-id icon theme-overrides actions))))))

(define (%handle-method-call! member sender serial path iface body-bv)
  "Dispatch D-Bus method call MEMBER from SENDER with SERIAL on PATH and IFACE.
https://specifications.freedesktop.org/notification/latest-single/#commands
Parameters: (MEMBER SENDER SERIAL PATH IFACE BODY-BV)
  - MEMBER: String D-Bus method name.
  - SENDER: String unique D-Bus sender bus name.
  - SERIAL: Integer request serial number.
  - PATH: String or #f target D-Bus object path.
  - IFACE: String or #f target D-Bus interface name.
  - BODY-BV: Bytevector containing raw method argument payload."
  (cond
   ((string=? member "Notify")
    (%handle-notify! sender serial body-bv))

   ((string=? member "GetCapabilities")
    ;; if we are asked for available capabilities
    ;; we return a list of the supported ones (refer to docs server capabilities)
    (dbus-reply! *notification-daemon-dbus* sender serial "as"
                 (dbus-encode-string-array '("actions" "body" "body-markup" "persistence" "icon-static"))))

   ((string=? member "GetServerInformation")
    ;; server information should return list (name vendor version spec-version)
    (dbus-reply! *notification-daemon-dbus* sender serial "ssss"
                 (dbus-encode-strings "Gliver Notification Daemon" "Gliver" "0.1" "1.2")))

   ((string=? member "CloseNotification")
    ;; we are asked to close the notification
    (catch #t
      (lambda ()
        (let* ((p        (open-bytevector-input-port body-bv))
               (close-id (dbus-get-u32 p)))
          (with-mutex %notification-state-mutex
            (set! %notification-pending-queue
                  (filter (lambda (item)
                            (not (equal? (list-ref item 2) close-id)))
                          %notification-pending-queue)))
          (gleui-toast-kill! #:name (%notification-toast-name close-id))))
      (lambda _ #f))
    (dbus-reply! *notification-daemon-dbus* sender serial "" #f))

   (else
    (dbus-error-reply! *notification-daemon-dbus*
                       sender
                       serial
                       "org.freedesktop.DBus.Error.UnknownMethod"
                       (format #f "Method ~a is not supported on interface ~a"
                               member (or iface ""))))))

(define (%handle-dbus-message! mtype serial fields body-bv)
  "Dispatch an unpacked D-Bus frame with MTYPE, SERIAL, FIELDS, and BODY-BV.
Parameters: (MTYPE SERIAL FIELDS BODY-BV)
  - MTYPE: Integer D-Bus message type.
  - SERIAL: Integer D-Bus message serial.
  - FIELDS: Alist of parsed D-Bus header fields.
  - BODY-BV: Bytevector containing message body."
  (let ((member (assoc-ref fields 3))
        (sender (assoc-ref fields 7))
        (path   (assoc-ref fields 1))
        (iface  (assoc-ref fields 2)))
    (when (and (= mtype 1) member sender) ;; this type is METHOD_CALL
      (catch #t
        (lambda ()
          ;; now that mtype is (method call), member will become the typed-array?
          ;; of method to call like Notify which requests the notification daemon
          ;; to display a notification message. The best way to learn all of
          ;; those things is the free desktop documentation
          ;; https://specifications.freedesktop.org/notification/latest-single/#commands
          (%handle-method-call! member sender serial path iface body-bv))
        (lambda (k . args)
          (log-error "notification-daemon: Error executing method ~a: ~a ~a" member k args)
          (dbus-error-reply! *notification-daemon-dbus*
                             sender
                             serial
                             "org.freedesktop.DBus.Error.Failed"
                             (format #f "Internal error handling method ~a" member)))))))

(define (notification-daemon-poll!)
  "Poll the shared D-Bus session connection for pending notification method calls.
Parameters: None."
  (dbus-session-poll!))

;;; lifecycle & handshake
(define-command (notification-daemon-start!)
  "Start the D-Bus notification daemon and acquire the session bus name.
Parameters: None.
Returns #t if the daemon is running, #f on failure."
  (if *notification-daemon-dbus*
      (begin
        (log-info "notification-daemon: already running")
        #t)
      (let ((conn (dbus-session-connect!)))
        (if (not conn)
            (begin
              (log-warn "notification-daemon: D-Bus session bus unavailable; daemon not started")
              #f)
            (catch #t
              (lambda ()
                (if (dbus-register-service! conn
                                            %notification-service-name
                                            %handle-dbus-message!
                                            #:path %notification-object-path)
                    (begin
                      (set! *notification-daemon-dbus* conn)
                      (log-info "notification-daemon: acquired ~a" %notification-service-name)
                      #t)
                    (begin
                      (log-warn "notification-daemon: could not acquire ~a" %notification-service-name)
                      #f)))
              (lambda (key . args)
                (log-error "notification-daemon: failed to start: ~a ~a" key args)
                #f))))))

(define-command (notification-daemon-stop!)
  "Stop the D-Bus notification daemon and dismiss active notification toasts.
Parameters: None.
Returns #t if the daemon was active, unspecified otherwise."
  (with-mutex %notification-state-mutex
    (set! %notification-pending-queue '())
    (set! %notification-last-show-time (- %notification-burst-cooldown)))
  (catch #t
    (lambda () (notification-daemon-dismiss-all!))
    (lambda _ #f))
  (set! *notification-daemon-active-ids* '())
  (set! *notification-daemon-active-id* #f)
  (when *notification-daemon-dbus*
    (log-info "notification-daemon: stopping")
    (dbus-unregister-service! *notification-daemon-dbus*
                              %notification-service-name
                              #:path %notification-object-path)
    (set! *notification-daemon-dbus* #f)
    (log-info "notification-daemon: stopped")
    #t))

(define-command (notification-daemon-restart!)
  "Restart the D-Bus notification daemon.
Parameters: None.
Returns #t if restarted successfully, #f on failure."
  (notification-daemon-stop!)
  (notification-daemon-start!))

(gliver-hook-add! *manager-tick-hook* '%notification-on-tick)
