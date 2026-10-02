;;; gliver/contrib/ui/statusbar/keyboard.scm --- Keyboard layout statusbar module
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver contrib ui statusbar keyboard)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (gliver core)
  #:use-module (gliver contrib ui statusbar base)
  #:declarative? #f
  #:export (
			make-module-keyboard
))

(define (%query-keyboard-layout)
  "Detect the currently active keyboard layout from available sources."
  ;; TODO: not sure how to do that currently
  ;; switching languages needs research
  "US")

(define* (make-module-keyboard #:key
                               (id 'keyboard)
                               (section 'right)
                               (interval 2)
                               (hooks (list *keyboard-layout-changed-hook*))
                               (icon "")
                               (format-template " ~a")
                               (short? #t)
                               (uppercase? #t)
                               (mappings '())
                               (layout-cmd #f)
                               (bg-color *statusbar-bg-color*)
                               (fg-color *theme-fg-main*)
                               (border-radius 6)
                               (padding-x 10)
                               (padding-y 4)
                               (on-click-cmd #f))
  "Create a Keyboard Layout / Language statusbar module.
Detects and dynamically listens to Wayland xkb_layout changes.
Supports short aliases, custom icon, layout cycling, and click actions."

  (define (%update module output)
    (let* ((raw (if layout-cmd
                    (or (statusbar-execute-shell-command layout-cmd) "US")
                    (%query-keyboard-layout)))
           ;; check custom user mappings first
           (mapped (assoc-ref mappings raw))
           (display-str (cond
                         (mapped mapped)
                         (short? raw)
                         (uppercase? (string-upcase raw))
                         (else raw)))
           (text (format #f format-template display-str)))
      (statusbar-module-text-set! module text)
      (statusbar-module-tooltip-set! module (format #f "Keyboard Layout: ~a" raw))
      (statusbar-module-state-set! module `((layout . ,raw)))))

  (define (%handle-click button x y module output custom-data)
    (when (= button 272)  ;; left click
      (if on-click-cmd
          ;; TODO: switch language
          (log-info "switch language"))))

  (let ((mod (make-statusbar-module
              #:id id
              #:name "Keyboard Layout"
              #:section section
              #:interval interval
              #:hooks hooks
              #:format-template format-template
              #:icon icon
              #:bg-color bg-color
              #:fg-color fg-color
              #:border-radius border-radius
              #:padding-x padding-x
              #:padding-y padding-y
              #:update-fn %update
              #:on-click %handle-click)))
    mod))

(statusbar-register-module! 'keyboard make-module-keyboard)
