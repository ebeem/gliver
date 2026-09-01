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
  ;; top-map keys
  (define-keys *top-map*
	;; basics
	"s-Return"  'terminal-spawn
	"s-q"       'window-kill
	"s-o"       'launcher-run
	"s-S-r"     'config-reload!
	"s-S-q"     'gliver-quit
	"s-f"       'window-fullscreen

	;; move focus to direction
	"s-h"       'container-focus-left
	"s-j"       'container-focus-down
	"s-k"       'container-focus-up
	"s-l"       'container-focus-right
	"s-n"       'window-focus-next
	"s-p"       'window-focus-prev
	"s-Tab"     'window-focus-next
	"s-S-Tab"   'window-focus-prev
	"s-Left"    'container-focus-left
	"s-Down"    'container-focus-down
	"s-Up"      'container-focus-up
	"s-Right"   'container-focus-right

	;; move the focused window to direction
	"s-S-Left"  'window-container-move-left
	"s-S-Down"  'window-container-move-down
	"s-S-Up"    'window-container-move-up
	"s-S-Right" 'window-container-move-right
	"s-H"       'window-container-move-left
	"s-J"       'window-container-move-down
	"s-K"       'window-container-move-up
	"s-L"       'window-container-move-right

	;; split containers (only for manual layouts)
	"s-s"       'container-split-horizontal
	"s-v"       'container-split-vertical
	"s-d"       'container-destroy
	"s-D"       'container-destroy-others

	;; special keys for volume, brightness, and screenshot
	"XF86AudioMute"         'media-audio-mute
	"XF86AudioLowerVolume"  'media-audio-volume-decrease
	"XF86AudioRaiseVolume"  'media-audio-volume-increase
	"XF86AudioMicMute"      'media-mic-mute
	"XF86MonBrightnessDown" 'media-brightness-decrease
	"XF86MonBrightnessUp"   'media-brightness-increase
	"Print"                 'media-screenshot

	"s-space" '(enter-submap *root-map*))

  (define-key *root-map* "o" 'launcher-run)
  (define-key *root-map* "h" '(enter-submap *help-map*))
  (define-key *root-map* "w" '(enter-submap *window-map*))
  (define-key *root-map* "W" '(enter-submap *workspace-map*))

  (gliver-hook-run! *keybinding-sync-request-hook*))

(keybindings-clear!)
(keybindings-gliver-install-default!)
