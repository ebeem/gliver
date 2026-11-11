;;; gliver/river.scm --- River modules re-exporter
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver river)
  #:use-module (gliver river connector)
  #:use-module (gliver river xkb-bindings-manager)
  #:use-module (gliver river xkb-config-manager)
  #:use-module (gliver river window-manager)
  #:use-module (gliver river wm-decoration-manager)
  #:use-module (gliver river wm-node-manager)
  #:use-module (gliver river wm-output-manager)
  #:use-module (gliver river wm-seat-manager)
  #:use-module (gliver river wm-shell-surface-manager)
  #:use-module (gliver river wm-window-manager)
  #:use-module (gliver river layer-shell-manager)
  #:use-module (gliver river input-manager)
  #:use-module (gliver river wlr-layer-shell)
  #:use-module (gliver river viewporter-manager)
  #:declarative? #f)

(define-syntax re-export-modules
  (syntax-rules ()
    ((_ (mod ...) ...)
     (begin
       (module-use! (module-public-interface (current-module))
                    (resolve-interface '(mod ...)))
       ...))))

(re-export-modules (gliver river connector)
                   (gliver river xkb-bindings-manager)
				   (gliver river xkb-config-manager)
                   (gliver river window-manager)
                   (gliver river wm-decoration-manager)
                   (gliver river wm-node-manager)
                   (gliver river wm-output-manager)
                   (gliver river wm-seat-manager)
                   (gliver river wm-shell-surface-manager)
                   (gliver river wm-window-manager)
                   (gliver river layer-shell-manager)
                   (gliver river input-manager)
                   (gliver river wlr-layer-shell)
                   (gliver river viewporter-manager))
