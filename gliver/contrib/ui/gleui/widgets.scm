;;; gliver/contrib/ui/gleui/widgets.scm --- Gleui Widgets Facade
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver contrib ui gleui widgets)
  #:use-module (gliver core helpers)
  #:use-module (gliver contrib ui gleui widgets engine)
  #:use-module (gliver contrib ui gleui widgets box)
  #:use-module (gliver contrib ui gleui widgets flex)
  #:use-module (gliver contrib ui gleui widgets align)
  #:use-module (gliver contrib ui gleui widgets stack)
  #:use-module (gliver contrib ui gleui widgets text)
  #:use-module (gliver contrib ui gleui widgets icon)
  #:use-module (gliver contrib ui gleui widgets grid)
  #:use-module (gliver contrib ui gleui widgets clickable)
  #:use-module (gliver contrib ui gleui widgets button)
  #:declarative? #f)

(re-export-modules (gliver contrib ui gleui widgets engine)
                   (gliver contrib ui gleui widgets box)
                   (gliver contrib ui gleui widgets flex)
                   (gliver contrib ui gleui widgets align)
                   (gliver contrib ui gleui widgets stack)
                   (gliver contrib ui gleui widgets text)
                   (gliver contrib ui gleui widgets icon)
                   (gliver contrib ui gleui widgets grid)
                   (gliver contrib ui gleui widgets clickable)
                   (gliver contrib ui gleui widgets button))
