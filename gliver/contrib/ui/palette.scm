;;; gliver/contrib/ui/palette.scm

(define-module (gliver contrib ui palette)
  #:use-module (gliver contrib ui palette base)
  #:use-module (gliver contrib ui palette rofi))

(define-syntax re-export-modules
  (syntax-rules ()
    ((_ (mod ...) ...)
     (begin
       (module-use! (module-public-interface (current-module))
                    (resolve-interface '(mod ...)))
       ...))))

(re-export-modules (gliver contrib ui palette base)
                   (gliver contrib ui palette rofi))

