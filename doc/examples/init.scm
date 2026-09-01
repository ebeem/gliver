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
			 (gliver contrib ui which-key)
			 (gliver contrib ui integrations rofi)
			 (gliver contrib ui container-border))

(set! *terminal* "foot")
(set! *theme-font* "Iosevka Nerd Font Bold")
(set! *wallpaper* "~/.wallpapers/fixed/flat-16.png")
(set! *wallpaper-mode* 'fill)

(log-level-set! 'debug)
(log-info "Loading init.scm configuration")

;; install gliver default keybindings (you can try out sway, stumpwm, etc)
(keybindings-clear!)
(keybindings-gliver-install-default!)

;; install rofi as dmenu and app launcher (you can replace it with fuzzel, fzf, etc)
(rofi-install-all!)
(which-key-enable!)
(container-border-enable!)

