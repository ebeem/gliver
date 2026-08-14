;;; gliver/contrib/commands.scm --- Command system for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver contrib commands)
  #:use-module (gliver contrib commands shell)
  #:use-module (gliver contrib commands help)
  #:use-module (gliver contrib commands keybindings)
  #:use-module (gliver contrib commands media)
  #:use-module (gliver contrib commands window)
  #:use-module (gliver contrib commands container)
  #:use-module (gliver contrib commands workspace)
  #:use-module (gliver contrib commands output)
  #:declarative? #f)

(define-syntax re-export-modules
  (syntax-rules ()
    ((_ (mod ...) ...)
     (begin
       (module-use! (module-public-interface (current-module))
                    (resolve-interface '(mod ...)))
       ...))))

(re-export-modules (gliver contrib commands shell)
                   (gliver contrib commands help)
                   (gliver contrib commands keybindings)
                   (gliver contrib commands media)
                   (gliver contrib commands window)
                   (gliver contrib commands container)
                   (gliver contrib commands workspace)
                   (gliver contrib commands output))
