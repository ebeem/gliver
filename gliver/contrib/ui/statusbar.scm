;;; gliver/contrib/ui/gleui.scm --- Statusbar modules importer
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver contrib ui statusbar)
  #:use-module (gliver contrib ui statusbar base)
  #:use-module (gliver contrib ui statusbar workspaces)
  #:use-module (gliver contrib ui statusbar window)
  #:use-module (gliver contrib ui statusbar cpu)
  #:use-module (gliver contrib ui statusbar ram)
  #:use-module (gliver contrib ui statusbar date)
  #:use-module (gliver contrib ui statusbar weather)
  #:use-module (gliver contrib ui statusbar battery)
  #:use-module (gliver contrib ui statusbar disk)
  #:use-module (gliver contrib ui statusbar network)
  #:use-module (gliver contrib ui statusbar cpu-temp)
  #:use-module (gliver contrib ui statusbar keyboard)
  #:use-module (gliver contrib ui statusbar net-speed)
  #:use-module (gliver contrib ui statusbar mpd)
  #:use-module (gliver contrib ui statusbar custom))

(define-syntax re-export-modules
  (syntax-rules ()
    ((_ (mod ...) ...)
     (begin
       (module-use! (module-public-interface (current-module))
                    (resolve-interface '(mod ...)))
       ...))))

(re-export-modules (gliver contrib ui statusbar base)
				   (gliver contrib ui statusbar workspaces)
				   (gliver contrib ui statusbar window)
				   (gliver contrib ui statusbar cpu)
				   (gliver contrib ui statusbar ram)
				   (gliver contrib ui statusbar date)
				   (gliver contrib ui statusbar weather)
				   (gliver contrib ui statusbar battery)
				   (gliver contrib ui statusbar disk)
				   (gliver contrib ui statusbar network)
				   (gliver contrib ui statusbar cpu-temp)
				   (gliver contrib ui statusbar keyboard)
				   (gliver contrib ui statusbar net-speed)
				   (gliver contrib ui statusbar mpd)
				   (gliver contrib ui statusbar custom))
