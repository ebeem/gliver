;;; gliver/contrib/ui/statusbar/volume.scm --- Volume statusbar module
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module displays volume and bluetooth indicator
;;; of the currently used output sound devices.
;;;
;;; Dependencies: bluetoothctl and an audio backend (wpctl, pactl)
;;;
;;; Usage:
;;;      (make-module-volume)
;;;
;;; TODO: I don't generally like using dependencies via cli
;;; this rapidly increase the pid generated for processes
;;; I would like to replace this approach with something
;;; else like using libraries instead.

(define-module (gliver contrib ui statusbar volume)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-11)
  #:use-module (gliver core)
  #:use-module (gliver core)
  #:use-module (gliver contrib commands media)
  #:use-module (gliver contrib ui statusbar base)
  #:declarative? #f
  #:export (
			*volume-backends*
			make-module-volume
))

(define (%query-bluetooth-battery mac)
  "Query the battery percentage for MAC address.
Returns battery percentage as an integer (0-100), or #f if unavailable."
  ;; matches "Battery Percentage: 0x50 (80)"
  ;; or "Battery Percentage: 80"
  (and-let* (((string? mac))
             ((not (string-null? mac)))
             (cmd (format #f "bluetoothctl info ~a 2>/dev/null" mac))
             (out (statusbar-execute-shell-command cmd))
             ((string? out))
             (m (or (string-match "Battery Percentage:[^(]*\\(([0-9]+)\\)" out)
                    (string-match "Battery Percentage:[^0-9]*([0-9]+)" out))))
    (string->number (match:substring m 1))))

;; wpctl backend definition
(define (%wpctl-volume-info)
  "Query current volume info via wpctl.
Returns an alist with keys: volume, muted?, bluetooth?, mac, device, battery, backend."

  (define (%wpctl-query-volume)
	"Query volume percentage via wpctl.
Returns volume (0-100)."
	(let* ((cmd    "wpctl get-volume @DEFAULT_AUDIO_SINK@ 2>/dev/null")
           (out    (statusbar-execute-shell-command cmd))
           (regex  (make-regexp "^Volume:[[:space:]]+([0-9.]+)([[:space:]]+\\[MUTED\\])?"))
           (m      (and (string? out) (regexp-exec regex out)))
           (vol-pc (and m (string->number (match:substring m 1))))
           (volume (and vol-pc (inexact->exact (round (* vol-pc 100))))))
      (or volume 0)))

  (define (%wpctl-query-muted?)
	"Query volume mute status via wpctl.
Returns boolean."
	(let* ((cmd    "wpctl get-volume @DEFAULT_AUDIO_SINK@ 2>/dev/null")
           (out    (statusbar-execute-shell-command cmd))
           (regex  (make-regexp "^Volume:[[:space:]]+([0-9.]+)([[:space:]]+\\[MUTED\\])?"))
           (m      (and (string? out) (regexp-exec regex out)))
           (muted? (and m (->bool (match:substring m 2)))))
      muted?))

  (define (%wpctl-query-sink-info)
	"Query default sink via wpctl.
Returns (values bluetooth? mac-address device-description)."
	(define (%extract rx str)
      (let ((m (and (string? str) (string-match rx str))))
		(and m (match:substring m 1))))
	(let* ((cmd  "wpctl inspect @DEFAULT_AUDIO_SINK@ 2>/dev/null")
           (out  (statusbar-execute-shell-command cmd))
           (bt?  (and (string? out)
                      (->bool (string-match "bluez5|bluez_output" out))))
           (mac  (%extract "api\\.bluez5\\.address = \"([^\"]+)\"" out))
           (desc (or (%extract "node\\.description = \"([^\"]+)\"" out)
					 (%extract "media\\.name = \"([^\"]+)\"" out))))
      (values bt? mac desc)))

  (let*-values (((vol) (%wpctl-query-volume))
				((muted?) (%wpctl-query-muted?))
                ((bt? mac desc) (%wpctl-query-sink-info))
                ((battery) (if bt? (%query-bluetooth-battery mac) #f)))
    `((volume . ,vol)
      (muted? . ,muted?)
      (bluetooth? . ,bt?)
      (mac . ,mac)
      (device . ,desc)
      (battery . ,battery)
      (backend . wpctl))))

(define (%wpctl-detect)
  "Check if wpctl is available."
  (and (statusbar-execute-shell-command "which wpctl 2>/dev/null") #t))

(define %wpctl-backend
  `((detect             . ,%wpctl-detect)
    (volume-info        . ,%wpctl-volume-info)))

;; pactl backend definition
(define (%pactl-volume-info)
  "Query current volume info via pactl.
Returns an alist with keys: volume, muted?, bluetooth?, mac, device, battery, backend."

  (define (%pactl-query-volume)
    "Query volume percentage via pactl.
Returns volume (0-100)."
    (let* ((cmd    "pactl get-sink-volume @DEFAULT_SINK@ 2>/dev/null")
           (out    (statusbar-execute-shell-command cmd))
           (m      (and (string? out) (string-match "([0-9]+)%" out)))
           (volume (and m (string->number (match:substring m 1)))))
      (or volume 0)))

  (define (%pactl-query-muted?)
    "Query volume mute status via pactl.
Returns boolean."
    (let* ((cmd    "pactl get-sink-mute @DEFAULT_SINK@ 2>/dev/null")
           (out    (statusbar-execute-shell-command cmd))
           (muted? (and (string? out)
                        (->bool (string-match "Mute:[[:space:]]+yes" out)))))
      muted?))

  (define (%pactl-query-sink-info)
    "Query default sink via pactl.
Returns (values bluetooth? mac-address device-description)."
    (define (%extract rx str)
      (let ((m (and (string? str) (string-match rx str))))
        (and m (match:substring m 1))))
    (let* ((cmd     "pactl get-default-sink 2>/dev/null")
           (out     (statusbar-execute-shell-command cmd))
           (sink    (and (string? out) (string-trim-both out)))
           (bt?     (and (string? sink)
                         (->bool (string-match "bluez" sink))))
           (raw-mac (%extract "(([0-9A-Fa-f]{2}_){5}[0-9A-Fa-f]{2})" sink))
           (mac     (and raw-mac (string-replace-substring raw-mac "_" ":")))
           (desc    (and (string? sink) (not (string-null? sink)) sink)))
      (values bt? mac desc)))

  (let*-values (((vol) (%pactl-query-volume))
                ((muted?) (%pactl-query-muted?))
                ((bt? mac desc) (%pactl-query-sink-info))
                ((battery) (if bt? (%query-bluetooth-battery mac) #f)))
    `((volume . ,vol)
      (muted? . ,muted?)
      (bluetooth? . ,bt?)
      (mac . ,mac)
      (device . ,desc)
      (battery . ,battery)
      (backend . pactl))))

(define (%pactl-detect)
  "Check if pactl is available."
  (and (statusbar-execute-shell-command "which pactl 2>/dev/null") #t))

(define %pactl-backend
  `((detect      . ,%pactl-detect)
    (volume-info . ,%pactl-volume-info)))

(define *volume-backends*
  (list (cons 'wpctl %wpctl-backend)
        (cons 'pactl %pactl-backend)))

(define (%detect-backend)
  "Return the first available volume backend alist, or #f if none."
  (any (lambda (entry)
         (and-let* ((backend (cdr entry))
                    (detect  (assq-ref backend 'detect))
                    ((detect)))
           backend))
       *volume-backends*))

(define (%battery-icon-formatter cap)
  "Return battery icon based on capacity CAP."
  (cond
   ((>= cap 90) "󰁹")
   ((>= cap 70) "󰂀")
   ((>= cap 50) "󰁾")
   ((>= cap 30) "󰁼")
   ((>= cap 15) "󰁻")
   (else "󰂎")))

(define* (make-module-volume #:key
                             (id 'volume)
                             (section 'right)
                             (interval 1)
							 (battery-formatter %battery-icon-formatter)
                             (backend (%detect-backend))
                             (step 5)
                             (max-volume 100)
                             (icon #f)
                             (icon-muted "󰝟")
                             (icon-low "󰕿")
                             (icon-medium "󰖀")
                             (icon-high "󰕾")
                             (icon-bluetooth "󰂯")
                             (show-bluetooth? #t)
                             (show-battery? #t)
                             (show-percent-when-muted? #t)
                             (format-template #f)
                             (format-battery "(~a ~a%)")
                             (bg-color *statusbar-bg-color*)
                             (fg-color *statusbar-fg-color*)
                             (fg-muted *theme-fg-dim*)
                             (border-radius 6)
                             (padding-x 10)
                             (padding-y 4)
                             (on-click-cmd #f)
                             (on-right-click-cmd #f)
                             (on-middle-click-cmd #f))
  "Create a Volume statusbar module."

  (define (%resolve-volume-icon vol muted?)
    (cond
     (muted? icon-muted)
     (icon icon)
     ((<= vol 0) icon-muted)
     ((<= vol 33) icon-low)
     ((<= vol 66) icon-medium)
     (else icon-high)))

  (define (%format-display-text vol muted? bt? battery)
    (cond
     ((procedure? format-template)
      (format-template vol muted? bt? battery))
     ((string? format-template)
      (format #f format-template vol))
     (else
      (let* ((v-icon (%resolve-volume-icon vol muted?))
             (bt-prefix (if (and bt? show-bluetooth?)
                            (format #f "~a " icon-bluetooth)
                            ""))
             (bat-str (if (and bt? show-battery? (number? battery))
                          (format #f " ~a" (format #f format-battery (battery-formatter battery) battery))
                          ""))
             (vol-str (if muted?
                          (if show-percent-when-muted?
                              (format #f "~a%" vol)
                              "Muted")
                          (format #f "~a%" vol))))
        (format #f "~a~a ~a~a" bt-prefix v-icon vol-str bat-str)))))

  (define (%update module output)
    (let* ((info-fn (assq-ref backend 'volume-info))
		   (info (and (procedure? info-fn) (info-fn)))
		   (vol (assoc-ref info 'volume))
		   (muted? (assoc-ref info 'muted?))
		   (bt? (assoc-ref info 'bluetooth?))
		   (mac (assoc-ref info 'mac))
		   (desc (assoc-ref info 'device))
		   (battery (assoc-ref info 'battery)))
      (if (not vol)
          (begin
            (statusbar-module-text-set! module (format #f "~a --%" icon-muted))
            (statusbar-module-fg-color-set! module fg-muted))
		  (let ((text (%format-display-text vol muted? bt? battery))
                (fg (if muted? fg-muted fg-color)))
            (statusbar-module-text-set! module text)
            (statusbar-module-fg-color-set! module fg)
            (statusbar-module-state-set! module info)))))

  (define (%handle-click button x y module output custom-data)
    (cond
     ;; left click: mute or custom
     ((= button 272)
	  (if on-click-cmd
		  (if (string? on-click-cmd)
			  (statusbar-execute-shell-command on-click-cmd)
			  (command-run-by-name on-click-cmd))
		  (media-audio-mute))
	  (%update module output))

     ;; right click: custom
     ((= button 273)
	  (when on-right-click-cmd
        (if (string? on-right-click-cmd)
            (statusbar-execute-shell-command on-right-click-cmd)
            (command-run-by-name on-right-click-cmd))
        (%update module output)))

     ;; middle click: mute or custom
     ((= button 274)
	  (if on-middle-click-cmd
		  (if (string? on-middle-click-cmd)
			  (statusbar-execute-shell-command on-middle-click-cmd)
			  (command-run-by-name on-middle-click-cmd))
		  (media-audio-mute))
	  (%update module output))))

  (define (%handle-scroll axis value module output)
    (when (zero? axis)  ;; vertical scroll
	  (if (< value 0)
		  (media-audio-volume-increase)
		  (media-audio-volume-decrease))
	  (%update module output)))

  (make-statusbar-module
   #:id id
   #:name "Volume"
   #:section section
   #:interval interval
   #:bg-color bg-color
   #:fg-color fg-color
   #:border-radius border-radius
   #:padding-x padding-x
   #:padding-y padding-y
   #:update-fn %update
   #:on-click %handle-click
   #:on-scroll %handle-scroll))

(statusbar-register-module! 'volume make-module-volume)
