;;; gliver/message-bar.scm --- Message bar for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; The message bar is a transient overlay for displaying messages,
;;; interactive prompts, and key sequence echoes.

(define-module (gliver message-bar)
  #:use-module (ice-9 format)
  #:use-module (gliver core logs)
  #:use-module (gliver core manager)
  #:use-module (gliver core output)
  #:use-module (gliver render)
  #:use-module (gliver commands)
  #:declarative? #f
  #:export (message-bar-show!
            message-bar-hide!
            message-bar-update!
            message-bar-font-set!
            message-bar-bg-set!
            message-bar-fg-set!
            message-bar-border-color-set!
            message-bar-gravity-set!
            message-callback-install!
            *message-bar-bg*
            *message-bar-fg*
            *message-bar-border-color*
            *message-bar-gravity*))

;;; configuration
(define *message-bar-bg* "#000000")
(define *message-bar-fg* "#e0e0e0")
(define *message-bar-border-color* "#555555")
(define *message-bar-font* "monospace 12")
(define *message-bar-gravity* 'bottom)   ;; 'top or 'bottom
(define *message-bar-padding* 4)
(define *message-bar-visible* #f)
(define *message-bar-content* "")
(define *message-bar-hide-timer* #f)

(define (message-bar-font-set! font)
  (set! *message-bar-font* font)
  (font-set! font))

(define (message-bar-bg-set! color)
  (set! *message-bar-bg* color))

(define (message-bar-fg-set! color)
  (set! *message-bar-fg* color))

(define (message-bar-border-color-set! color)
  (set! *message-bar-border-color* color))

(define (message-bar-gravity-set! gravity)
  (set! *message-bar-gravity* gravity))

;;; display
(define (current-time-ms)
  "Return the current time in milliseconds since epoch."
  (let ((t (gettimeofday)))
    (+ (* (car t) 1000)
       (quotient (cdr t) 1000))))

(define (message-bar-show! text)
  "Show the message bar with TEXT."
  (set! *message-bar-content* text)
  (set! *message-bar-visible* #t)
  (message-bar-render!)
  ;; set up auto-hide timer
  (let ((timeout (manager-message-timeout *manager*)))
    (when (and timeout (> timeout 0))
      ;; in a full implementation, this would use a timer fd
      ;; or scheduled callback in the event loop
      (set! *message-bar-hide-timer* (+ (current-time-ms) (* timeout 1000))))))

(define (message-bar-hide!)
  "Hide the message bar."
  (set! *message-bar-visible* #f)
  (set! *message-bar-content* "")
  (set! *message-bar-hide-timer* #f)
  (message-bar-render!)
  ;; in production: destroy or hide the layer-shell surface
  (log-debug "Message bar hidden."))

(define (message-bar-update!)
  "Update the message bar display."
  (when *message-bar-visible*
    (message-bar-render!))
  ;; check hide timer
  (when (and *message-bar-hide-timer*
             (>= (current-time-ms) *message-bar-hide-timer*))
    (message-bar-hide!)))

(define (message-bar-render!)
  "Render the message bar content to its surface."
  (let ((output (output-current)))
    (when output
      (let* ((width (output-width output))
             (height (+ (text-height) (* 2 *message-bar-padding*))))
        ;; in production: render to layer-shell surface
        (surface-text-render *message-bar-content*
                             *message-bar-fg* *message-bar-bg*
                             width height)
        (log-debug "Message bar: ~a" *message-bar-content*)))))

;;; integration with command system
(define (message-callback-install!)
  "Hook the message bar into the command system's `message` function."
  ;; the commands module has a *message-callback* that we set
  ;; to route messages through the message bar.
  (set! (@@ (gliver commands) *message-callback*) message-bar-show!)
  (log-debug "Message callback installed."))
