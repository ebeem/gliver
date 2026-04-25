;;; gliver/river/core.scm --- River compositor integration

(define-module (gliver core)
  #:use-module (gliver core config)
  #:use-module (gliver core container)
  #:use-module (gliver core hooks)
  #:use-module (gliver core logs)
  #:use-module (gliver core output)
  #:use-module (gliver core seat)
  #:use-module (gliver core types)
  #:use-module (gliver core window)
  #:use-module (gliver core keybindings)
  #:use-module (gliver core workspace))

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
                   (gliver core logs)
                   (gliver core types)
                   (gliver core output)
                   (gliver core seat)
                   (gliver core window)
                   (gliver core keybindings)
                   (gliver core workspace))

