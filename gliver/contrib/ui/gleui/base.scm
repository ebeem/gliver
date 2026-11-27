;;; gliver/contrib/ui/gleui/base.scm --- Essential GUI Framework Base Facade
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver contrib ui gleui base)
  #:use-module (gliver core helpers)
  #:use-module (gliver contrib ui gleui base surface)
  #:use-module (gliver contrib ui gleui base shm)
  #:use-module (gliver contrib ui gleui base draw)
  #:use-module (gliver contrib ui gleui base theme)
  #:use-module (gliver contrib ui gleui base text)
  #:use-module (gliver contrib ui gleui base icons)
  #:use-module (gliver contrib ui gleui base repeat)
  #:use-module (gliver contrib ui gleui base pointer)
  #:declarative? #f)

(re-export-modules (gliver contrib ui gleui base surface)
                   (gliver contrib ui gleui base shm)
                   (gliver contrib ui gleui base draw)
                   (gliver contrib ui gleui base theme)
                   (gliver contrib ui gleui base text)
                   (gliver contrib ui gleui base icons)
                   (gliver contrib ui gleui base repeat)
                   (gliver contrib ui gleui base pointer))
