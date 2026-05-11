;;; gliver/core/workspace.scm --- Core workspace model and utility for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Author: ebeem <lord.ebeem@gmail.com>
;;; Maintainer: ebeem <lord.ebeem@gmail.com>
;;; Workspace: similar to an emacs group/isolation concept and stumpwm group
;;; A collection of containers and their associated windows (workspace/virtual desktop)

(define-module (gliver core manager)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (gliver core logs)
  #:use-module (gliver core config)
  #:use-module (gliver core hooks)
  #:use-module (gliver core types)
  #:export (
))

(define* (on-window-created window)
  "Adds the created window to global state."
  (container-windows-set! (window-container window)
						  (cons window (container-windows container)))
  (manager-windows-set! *manager*
						  (append (manager-windows *manager*) (list window))))

(gliver-hook-add! *window-created-hook* on-window-created -100)
