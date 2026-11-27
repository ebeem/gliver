;;; gliver/contrib/ui/gleui.scm --- Native Cairo & Pango Wayland UI Framework (Gliver UI - Gleui)
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module aggregates and re-exports the gleui UI framework and its
;;; desktop components.
;;; It handles:
;;;  - Re-exporting core, widget, pointer, and component modules
;;;  - Providing unified install and cleanup interactive commands
;;;  - Registering cleanup on Wayland globals unbind

(define-module (gliver contrib ui gleui)
  #:use-module (gliver wayland client)
  #:use-module (gliver river connector)
  #:use-module (gliver river wlr-layer-shell)
  #:use-module (gliver core)
  #:use-module (gliver deps dbus)
  #:use-module (gliver contrib ui gleui base)
  #:use-module (gliver contrib ui gleui widgets)
  #:use-module (gliver contrib ui gleui palette)
  #:use-module (gliver contrib ui gleui launcher)
  #:use-module (gliver contrib ui gleui toast)
  #:use-module (gliver contrib ui gleui statusbar)
  #:use-module (gliver contrib ui gleui notifications)
  #:declarative? #f
  #:export (
            gleui-cleanup!
            gleui-install-all!
))

(re-export-modules (gliver contrib ui gleui base)
                   (gliver contrib ui gleui widgets)
                   (gliver contrib ui gleui palette)
                   (gliver contrib ui gleui launcher)
                   (gliver contrib ui gleui toast)
                   (gliver contrib ui gleui statusbar)
                   (gliver contrib ui gleui notifications))

(define-command (gleui-cleanup!)
  "Clean up all active gleui resources in all components.
Parameters: None."
  (gleui-palette-cleanup!)
  (gleui-toast-cleanup!)
  (notification-daemon-stop!)
  (dbus-session-disconnect!)
  (statusbar-cleanup-all!))

(define-command (gleui-install-all!)
  "Initialize gleui palette cache when connected and start the notification daemon when enabled.
Parameters: None."
  (gleui-palette-init!)
  (when *notification-daemon-enabled*
    (notification-daemon-start!)))

(gliver-hook-add! *gliver-globals-unbind-hook* 'gleui-cleanup!)
