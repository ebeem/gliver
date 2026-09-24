;;; Example Gliver Configuration
;;; ~/.config/gliver/init.scm
;;;
;;; This file is loaded by Gliver at startup.  It is a Guile Scheme
;;; program with access to all Gliver APIs.

(use-modules (gliver core)
             (gliver contrib commands)
			 (gliver contrib layout manual)
			 (gliver contrib utils systemd)
			 (gliver contrib keybindings gliver)
			 (gliver contrib ui overlay which-key)
             (gliver contrib ui gleui)
			 (gliver contrib ui container container-border))

(set! *terminal* "foot")
(set! *theme-font* "Iosevka Nerd Font Bold")
(set! *wallpaper* "~/.wallpapers/fixed/flat-16.png")
(set! *wallpaper-mode* 'fill)

;; set log-level to 'debug (can also be 'info, 'warning, 'error)
(log-level-set! 'debug)
(log-info "Loading init.scm configuration")

;; install gliver default keybindings (you can try out sway, stumpwm, etc)
(keybindings-clear!)
(keybindings-gliver-install-default!)

;; install rofi as dmenu and app launcher (you can replace it with fuzzel, fzf, etc)
(gleui-install-all!)
;; enable which-key module for keybindings discovery
(which-key-enable!)
;; enable container-border to add border to active container/window
(container-border-enable!)

