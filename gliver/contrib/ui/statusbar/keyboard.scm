;;; gliver/contrib/ui/statusbar/keyboard.scm --- Keyboard layout statusbar module
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver contrib ui statusbar keyboard)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (gliver core)
  #:use-module (gliver river keyboard-manager)
  #:use-module (gliver deps libxkbcommon)
  #:use-module (gliver contrib ui statusbar base)
  #:declarative? #f
  #:export (
			make-module-keyboard
))

(define (%query-keyboard-layout)
  "Detect the currently active keyboard layout from available sources."
  (keyboard-manager-current-layout-code))

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
    (let* ((raw (%query-keyboard-layout))
		   ;; TODO: this should get layout name rather than code
           (curr-name (keyboard-manager-current-layout-code))
           (curr-code (keyboard-manager-current-layout-code))
           ;; check custom user mappings first for raw, code, or name
           (mapped (or (assoc-ref mappings raw)
                       (and curr-code (assoc-ref mappings curr-code))
                       (and curr-name (assoc-ref mappings curr-name))))
           (display-str (cond
                         (mapped mapped)
                         (short? (if uppercase? (string-upcase raw) raw))
                         (uppercase? (string-upcase (or curr-name raw)))
                         (else (or curr-name raw))))
           (text (format #f format-template display-str))
           (tooltip (format #f "Keyboard Layout: ~a" (or curr-name raw))))
      (statusbar-module-text-set! module text)
      (statusbar-module-tooltip-set! module tooltip)
      (statusbar-module-state-set! module `((layout . ,raw)
                                           (name . ,curr-name)
                                           (code . ,curr-code)
                                           (index . ,*keyboard-manager-current-layout-index*)))))

  (define (%handle-click button x y module output custom-data)
    (cond
     ((= button 272)  ;; left click
      (if on-click-cmd
          (if (string? on-click-cmd)
              (statusbar-execute-shell-command on-click-cmd)
              (command-run-by-name on-click-cmd))
          (command-run-by-name 'keyboard-layout-next)))
     ((= button 273)  ;; right click
      (if on-click-cmd
          (if (string? on-click-cmd)
              (statusbar-execute-shell-command on-click-cmd)
              (command-run-by-name on-click-cmd))
          (command-run-by-name 'keyboard-layout-prev)))))

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
