;;; gliver/river.scm --- Core modules re-exporter
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver core)
  #:use-module (gliver core config)
  #:use-module (gliver core container)
  #:use-module (gliver core ffi)
  #:use-module (gliver core hooks)
  #:use-module (gliver core logs)
  #:use-module (gliver core helpers)
  #:use-module (gliver core output)
  #:use-module (gliver core seat)
  #:use-module (gliver core shell)
  #:use-module (gliver core types)
  #:use-module (gliver core window)
  #:use-module (gliver core key)
  #:use-module (gliver core keymap)
  #:use-module (gliver core keybinding)
  #:use-module (gliver core keyboard)
  #:use-module (gliver core workspace)
  #:declarative? #f)

(define-syntax re-export-modules
  (syntax-rules ()
    ((_ (mod ...) ...)
     (begin
       (module-use! (module-public-interface (current-module))
                    (resolve-interface '(mod ...)))
       ...))))

(re-export-modules (gliver core config)
                   (gliver core hooks)
                   (gliver core container)
                   (gliver core ffi)
                   (gliver core logs)
                   (gliver core helpers)
                   (gliver core types)
                   (gliver core output)
                   (gliver core seat)
                   (gliver core shell)
                   (gliver core window)
                   (gliver core key)
                   (gliver core keymap)
                   (gliver core keybinding)
                   (gliver core keyboard)
                   (gliver core workspace))

