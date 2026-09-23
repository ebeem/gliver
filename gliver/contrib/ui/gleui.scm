;;; gliver/contrib/ui/gleui.scm --- Native Cairo & Pango Wayland UI Framework (Gliver UI - Gleui)
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver contrib ui gleui)
  #:use-module (gliver core)
  #:use-module (gliver contrib ui gleui base)
  #:use-module (gliver contrib ui gleui palette)
  #:use-module (gliver contrib ui gleui launcher)
  #:use-module (gliver contrib ui gleui toast)
  #:use-module (gliver contrib ui gleui statusbar)
  #:declarative? #f
  #:export (
            gleui-cleanup!
            gleui-install-all!
))

(define-syntax re-export-modules
  (syntax-rules ()
    ((_ (mod ...) ...)
     (begin
       (module-use! (module-public-interface (current-module))
                    (resolve-interface '(mod ...)))
       ...))))

(re-export-modules (gliver contrib ui gleui base)
                   (gliver contrib ui gleui palette)
                   (gliver contrib ui gleui launcher)
                   (gliver contrib ui gleui toast)
                   (gliver contrib ui gleui statusbar))

(define-command (gleui-cleanup!)
  "Clean up all active Gleui resources (palette, toast, and statusbar surfaces/buffers)."
  (gleui-palette-cleanup!)
  (gleui-toast-cleanup!)
  (statusbar-cleanup-all!))

(define-command (gleui-install-all!)
  "Install Gleui as the backend for palette, launcher, and toast."
  (gleui-palette-install!)
  (gleui-launcher-install!)
  (gleui-toast-install!))

(gliver-hook-add! *gliver-globals-unbind-hook* 'gleui-cleanup!)
