;;; gliver/river/core.scm --- River compositor integration

(define-module (gliver river)
  #:use-module (gliver river connector)
  #:use-module (gliver river keybindings-manager)
  #:use-module (gliver river window-manager)
  #:use-module (gliver river wm-decoration-manager)
  #:use-module (gliver river wm-node-manager)
  #:use-module (gliver river wm-output-manager)
  #:use-module (gliver river wm-seat-manager)
  #:use-module (gliver river wm-shell-surface-manager)
  #:use-module (gliver river wm-window-manager))

(define-syntax re-export-modules
  (syntax-rules ()
    ((_ (mod ...) ...)
     (begin
       (module-use! (module-public-interface (current-module))
                    (resolve-interface '(mod ...)))
       ...))))

(re-export-modules (gliver river connector)
				   (gliver river keybindings-manager)
				   (gliver river window-manager)
				   (gliver river wm-decoration-manager)
				   (gliver river wm-node-manager)
				   (gliver river wm-output-manager)
				   (gliver river wm-seat-manager)
				   (gliver river wm-shell-surface-manager)
				   (gliver river wm-window-manager))

