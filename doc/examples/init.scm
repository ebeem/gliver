;;; doc/examples/init.scm --- Example init file for Gliver

(use-modules (gliver core)
             (gliver commands)
			 (gliver contrib ui which-key)
			 (gliver contrib utils systemd)
			 (gliver contrib keybindings sway))

(set! *terminal* "foot")
;; set log level for better debugging
;; (log-level-set! 'debug)
(log-info "Loading init.scm configuration")

;; clear default keybindings and add install sway's keybindings
(keybindings-clear!)
(keybindings-sway-install-default!)

;; define an additional keybinding to spawn fuzzel with super+o
(define-key *top-map* "s-o"
  (lambda () (cmd-exec (format #f "exec fuzzel"))))
