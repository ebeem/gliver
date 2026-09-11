(define-module (gliver contrib keybindings gliver)
  #:use-module (gliver core)
  #:use-module (gliver contrib commands)
  #:export (
			keybindings-gliver-install-default!
			*window-map*
			*help-map*
))

(define-var *window-map*
  (make-gliver-keymap '*window-map*)
  "window map layer")

(define-var *help-map*
  (make-gliver-keymap '*help-map*)
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
	"s-M-Left"  'window-container-move-left
	"s-M-Down"  'window-container-move-down
	"s-M-Up"    'window-container-move-up
	"s-M-Right" 'window-container-move-right
	"s-M-h"     'window-container-move-left
	"s-M-j"     'window-container-move-down
	"s-M-k"     'window-container-move-up
	"s-M-l"     'window-container-move-right

	;; move workspace focus to direction
	"s-C-Left"  'workspace-focus-right
	"s-C-Down"  'workspace-focus-down
	"s-C-Up"    'workspace-focus-up
	"s-C-Right" 'workspace-focus-right
	"s-C-h"     'workspace-focus-left
	"s-C-j"     'workspace-focus-down
	"s-C-k"     'workspace-focus-up
	"s-C-l"     'workspace-focus-right

	;; move window to workspace in direction
	"s-C-M-Left"  'window-workspace-move-left
	"s-C-M-Down"  'window-workspace-move-down
	"s-C-M-Up"    'window-workspace-move-up
	"s-C-M-Right" 'window-workspace-move-right
	"s-C-M-h"     'window-workspace-move-left
	"s-C-M-j"     'window-workspace-move-down
	"s-C-M-k"     'window-workspace-move-up
	"s-C-M-l"     'window-workspace-move-right

	;; split containers (only for manual layouts)
	"s-s"         'container-split-horizontal
	"s-v"         'container-split-vertical
	"s-d"         'container-destroy
	"s-D"         'container-destroy-others

	;; special keys for volume, brightness, and screenshot
	"XF86AudioMute"         'media-audio-mute
	"XF86AudioLowerVolume"  'media-audio-volume-decrease
	"XF86AudioRaiseVolume"  'media-audio-volume-increase
	"XF86AudioMicMute"      'media-mic-mute
	"XF86MonBrightnessDown" 'media-brightness-decrease
	"XF86MonBrightnessUp"   'media-brightness-increase
	"Print"                 'media-screenshot

	"s-space" '(enter-submap *root-map*))

  (define-keys *help-map*
	"v"         'describe-variable
	"f"         'describe-command
	"k"         'describe-key)

  (define-key *root-map* "o" 'launcher-run)
  (define-key *root-map* "h" '(enter-submap *help-map*))
  (define-key *root-map* "w" '(enter-submap *window-map*))
  (define-key *root-map* "W" '(enter-submap *workspace-map*))

  (gliver-hook-run! *keybinding-sync-request-hook*))

(keybindings-clear!)
(keybindings-gliver-install-default!)
