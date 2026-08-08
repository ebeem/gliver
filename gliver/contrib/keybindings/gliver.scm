(define-module (gliver contrib keybindings gliver)
  #:use-module (gliver core)
  #:use-module (gliver contrib commands)
  #:export (
			keybindings-gliver-install-default!
			*window-map*
			*help-map*
))

(define-var *window-map*
  (make-gliver-keymap "*window*")
  "window map layer")

(define-var *help-map*
  (make-gliver-keymap "*help*")
  "help map layer")

(define (keybindings-gliver-install-default!)
  "Install the default Gliver keybindings."
  ;; basics
  (define-key *top-map* "s-Return" 'terminal-spawn)
  (define-key *top-map* "s-q" 'window-kill)
  (define-key *top-map* "s-o" 'launcher-run)
  (define-key *top-map* "s-S-r" 'config-reload)
  (define-key *top-map* "s-S-q" 'gliver-quit)

  ;; move focus to direction
  (define-key *top-map* "s-h" container-focus-left)
  (define-key *top-map* "s-j" container-focus-down)
  (define-key *top-map* "s-k" container-focus-up)
  (define-key *top-map* "s-l" container-focus-right)
  (define-key *top-map* "s-Left" container-focus-left)
  (define-key *top-map* "s-Down" container-focus-down)
  (define-key *top-map* "s-Up" container-focus-up)
  (define-key *top-map* "s-Right" container-focus-right)

  ;; move the focused window to direction
  (define-key *top-map* "s-S-Left" window-container-move-left)
  (define-key *top-map* "s-S-Down" window-container-move-down)
  (define-key *top-map* "s-S-Up" window-container-move-up)
  (define-key *top-map* "s-S-Right" window-container-move-right)
  (define-key *top-map* "s-S-h" window-container-move-left)
  (define-key *top-map* "s-S-j" window-container-move-down)
  (define-key *top-map* "s-S-k" window-container-move-up)
  (define-key *top-map* "s-S-l" window-container-move-right)

  (define-key *top-map* "s-f" 'window-fullscreen)

  ;; special keys for volume, brightness, and screenshot
  (define-key *top-map* "XF86AudioMute" "exec pactl set-sink-mute @DEFAULT_SINK@ toggle")
  (define-key *top-map* "XF86AudioLowerVolume" "exec pactl set-sink-volume @DEFAULT_SINK@ -5%")
  (define-key *top-map* "XF86AudioRaiseVolume" "exec pactl set-sink-volume @DEFAULT_SINK@ +5%")
  (define-key *top-map* "XF86AudioMicMute" "exec pactl set-source-mute @DEFAULT_SOURCE@ toggle")
  (define-key *top-map* "XF86MonBrightnessDown" "exec brightnessctl set 5%-")
  (define-key *top-map* "XF86MonBrightnessUp" "exec brightnessctl set 5%+")
  (define-key *top-map* "Print" "exec grim")

  (define-key *top-map* "s-space" '(enter-submap *root-map*))

  (define-key *root-map* "o" 'launcher-run)
  (define-key *root-map* "h" '(enter-submap *help-map*))
  (define-key *root-map* "w" '(enter-submap *window-map*))
  (define-key *root-map* "W" '(enter-submap *workspace-map*))

  (gliver-hook-run! *keybinding-sync-request-hook*))

