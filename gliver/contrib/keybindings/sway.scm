(define-module (gliver contrib keybindings sway)
  #:use-module (gliver core)
  #:use-module (gliver commands)
  #:export (
			keybindings-sway-install-default!
))

;; custom helper to ensure a workspace exists when switching/focusing
(define (workspace-ensure-and-focus! name)
  (let ((ws (workspace-find-by-name name)))
    (if ws
        (workspace-switch-to! ws)
        (let* ((output (output-current))
               (new-ws (make-workspace name #:output output)))
          (workspace-add! new-ws)
          (workspace-switch-to! new-ws)))))

;; custom helper to ensure a workspace exists when moving a window
(define (window-move-to-workspace-ensure! name)
  (let ((win (window-current)))
    (if (not win)
        (let ((target (workspace-find-by-name name)))
          (if target
              (begin
                (window-move-to-workspace! win target)
                (let* ((output (output-current))
                       (new-ws (make-workspace name #:output output)))
                  (workspace-add! new-ws)
                  (window-move-to-workspace! win new-ws))))))))

(define (keybindings-sway-install-default!)
  "Install the default Sway-compatible keybindings nested in StumpWM-style keymaps."
  ;; basics
  (define-key *top-map* "s-Return" 'terminal-spawn)
  (define-key *top-map* "s-S-q" 'window-kill-window)
  (define-key *top-map* "s-d" 'cmd-dmenu-run)
  (define-key *top-map* "s-S-c" 'config-reload)
  (define-key *top-map* "s-S-e" 'quit)

  ;; move focus to direction
  (define-key *top-map* "s-h" (lambda () (cmd-container-focus-direction 'left)))
  (define-key *top-map* "s-j" (lambda () (cmd-container-focus-direction 'down)))
  (define-key *top-map* "s-k" (lambda () (cmd-container-focus-direction 'up)))
  (define-key *top-map* "s-l" (lambda () (cmd-container-focus-direction 'right)))
  (define-key *top-map* "s-Left" (lambda () (cmd-container-focus-direction 'left)))
  (define-key *top-map* "s-Down" (lambda () (cmd-container-focus-direction 'down)))
  (define-key *top-map* "s-Up" (lambda () (cmd-container-focus-direction 'up)))
  (define-key *top-map* "s-Right" (lambda () (cmd-container-focus-direction 'right)))

  ;; move the focused window to direction
  (define-key *top-map* "s-S-Left" (lambda () (cmd-window-container-move-direction 'left)))
  (define-key *top-map* "s-S-Down" (lambda () (cmd-window-container-move-direction 'down)))
  (define-key *top-map* "s-S-Up" (lambda () (cmd-window-container-move-direction 'up)))
  (define-key *top-map* "s-S-Right" (lambda () (cmd-window-container-move-direction 'right)))
  (define-key *top-map* "s-S-h" (lambda () (cmd-window-container-move-direction 'left)))
  (define-key *top-map* "s-S-j" (lambda () (cmd-window-container-move-direction 'down)))
  (define-key *top-map* "s-S-k" (lambda () (cmd-window-container-move-direction 'up)))
  (define-key *top-map* "s-S-l" (lambda () (cmd-window-container-move-direction 'right)))

  ;; focus workspace and move focused window to workspace
  (for-each
  (lambda (i)
    (let* ((ws-str (number->string i))
           (key-str (if (= i 10) "0" ws-str)))
      (define-key *top-map* (string-append "s-" key-str)
        (lambda () (workspace-ensure-and-focus! ws-str)))
      (define-key *top-map* (string-append "s-S-" key-str)
        (lambda () (window-move-to-workspace-ensure! ws-str)))))
  '(1 2 3 4 5 6 7 8 9 10))

  ;; TODO: split horizontal / vertical
  ;; (define-key *top-map* "s-b" ...)
  ;; (define-key *top-map* "s-v" ...)

  ;; TODO switch layout styles: stacking / tabbed / toggle split
  ;; (define-key *top-map* "s-s" ...)
  ;; (define-key *top-map* "s-w" ...)
  ;; (define-key *top-map* "s-e" ...)

  (define-key *top-map* "s-f" 'window-fullscreen)

  ;; TODO: toggle current focus and swap focus between tiled/floating
  ;; (define-key *top-map* "s-S-space" ...)
  ;; (define-key *top-map* "s-space" ...)

  ;; move focus to parent container, this is irrelevant with current structure
  ;; (define-key *top-map* "a" ...)

  ;; TODO: scratchpad, requires extension to be complete
  ;; (define-key *top-map* "s-S-minus" ...)
  ;; (define-key *top-map* "s-minus" ...)


  ;; TODO: resize actions
  (define-key *top-map* "s-r" *resize-map*)
  ;; (define-key *resize-map* "Left" ...)
  ;; (define-key *resize-map* "Down" ...)
  ;; (define-key *resize-map* "Up" ...)
  ;; (define-key *resize-map* "Right" ...)
  ;; (define-key *resize-map* "h" ...)
  ;; (define-key *resize-map* "j" ...)
  ;; (define-key *resize-map* "k" ...)
  ;; (define-key *resize-map* "l" ...)

  ;; return to default mode
  (define-key *resize-map* "Return" 'prefix-abort)
  (define-key *resize-map* "Escape" 'prefix-abort)

  ;; special keys for volume, brightness, and screenshot
  (define-key *top-map* "XF86AudioMute" "exec pactl set-sink-mute @DEFAULT_SINK@ toggle")
  (define-key *top-map* "XF86AudioLowerVolume" "exec pactl set-sink-volume @DEFAULT_SINK@ -5%")
  (define-key *top-map* "XF86AudioRaiseVolume" "exec pactl set-sink-volume @DEFAULT_SINK@ +5%")
  (define-key *top-map* "XF86AudioMicMute" "exec pactl set-source-mute @DEFAULT_SOURCE@ toggle")
  (define-key *top-map* "XF86MonBrightnessDown" "exec brightnessctl set 5%-")
  (define-key *top-map* "XF86MonBrightnessUp" "exec brightnessctl set 5%+")
  (define-key *top-map* "Print" "exec grim")

  (gliver-hook-run! *keybinding-sync-request-hook*))
