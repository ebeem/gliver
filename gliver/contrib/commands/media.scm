;;; gliver/contrib/commands/media.scm --- Command system for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver contrib commands media)
  #:use-module (gliver contrib commands shell)
  #:use-module (gliver core)
  #:declarative? #f
  #:export (
			media-volume-delta
			media-brightness-delta
			media-audio-mute
			media-audio-volume-decrease
			media-audio-volume-increase
			media-mic-mute
			media-brightness-increase
			media-brightness-decrease
			media-screenshot
))

(define-var media-volume-delta 5
			"Volume increase/decrease delta in percentage")
(define-var media-brightness-delta 5
			"Volume increase/decrease delta in percentage")

;;; audio commands
(define-command (media-audio-mute)
  "Mute audio."
  (exec "pactl set-sink-mute @DEFAULT_SINK@ toggle"))

(define-command (media-audio-volume-decrease)
  "Decrease the volume."
  (exec (string-append "pactl set-sink-volume @DEFAULT_SINK@ -"
					   (number->string media-volume-delta) "%")))

(define-command (media-audio-volume-increase)
  "Increase the volume."
  (exec (string-append "pactl set-sink-volume @DEFAULT_SINK@ +"
					   (number->string media-volume-delta) "%")))

(define-command (media-mic-mute)
  "Mute audio."
  (exec "pactl set-source-mute @DEFAULT_SOURCE@ toggle"))

;;; brightness commands
(define-command (media-brightness-increase)
  "Brightness increase."
  (exec (string-append "brightnessctl set +"
					   (number->string media-brightness-delta) "%")))

(define-command (media-brightness-decrease)
  "Brightness decrease."
  (exec (string-append "brightnessctl set "
					 (number->string media-brightness-delta) "%-")))

;;; screenshot commands
(define-command (media-screenshot)
  "Brightness decrease."
  (exec "flameshot gui"))

