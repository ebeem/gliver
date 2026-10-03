;;; Example Gliver Configuration
;;; ~/.config/gliver/init.scm
;;;
;;; This file is loaded by Gliver at startup. It is a Guile Scheme
;;; program with access to all Gliver APIs.

(use-modules (gliver core)
             (gliver contrib commands)
			 (gliver contrib layout manual)
			 (gliver contrib keybindings gliver)
			 (gliver contrib ui container container-border)

			 ;; optional deps (recommended)
			 (gliver contrib ui overlay which-key)
             (gliver contrib ui gleui)
             (gliver contrib ui statusbar))


;; command to spawn terminals; update to the binary name
;; of your terminal like: kitty, alacritty, ghostty
(set! *terminal* "foot")

;; name of the font to be used, will be inherited by other ui
;; components like toast, statusbar, and palette
;(set! *theme-font* "Iosevka Nerd Font Bold")

;; path of your wallpaper 
;(set! *wallpaper* "~/.wallpapers/fixed/flat-16.png")
;(set! *wallpaper-mode* 'fill)

;; set log-level to 'debug (can also be 'info, 'warning, 'error)
;; you can find the logs in "$XDG_STATE_HOME/gliver/gliver.log"
;; to print path execute: echo "$XDG_STATE_HOME/gliver/gliver.log"
(log-level-set! 'debug)
(log-info "Loading init.scm configuration")

;; install gliver default keybindings (you can try out sway, stumpwm, etc)
(keybindings-clear!)
(keybindings-gliver-install-default!)

;; enable container-border to add border to active container/window
;; highly recommended, considered a default in gliver
(container-border-enable!)

;; install gleui ui components, also highly recommended
;; needed in order to be able to launch applications and view palette
(gleui-install-all!)

;; enable which-key module for keybindings discovery
(which-key-enable!)

;; statusbar, optional and can be replaced by other tools like waybar
(set! *statusbar-modules-left* '())
(set! *statusbar-modules-center*
	  (list
	   (make-module-date
		#:formats '(" %a %b %d   %H:%M:%S"))))
(set! *statusbar-modules-right* '())
(statusbar-enable!)
