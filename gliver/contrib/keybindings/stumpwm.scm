(define-module (gliver contrib keybindings stumpwm)
  #:use-module (gliver core)
  #:use-module (gliver contrib commands)
  #:export (
			keybindings-stumpwm-install-default!
))

(define (keybindings-stumpwm-install-default!)
  "Install the default StumpWM keybindings."
  ;; Workspace sub-map
  (define-key *workspace-map* "c" 'workspace-create)
  (define-key *workspace-map* "C" 'workspace-create-float)
  (define-key *workspace-map* "k" 'workspace-destroy)
  (define-key *workspace-map* "g" 'workspace-focus)
  (define-key *workspace-map* "n" 'workspace-focus-next)
  (define-key *workspace-map* "p" 'workspace-focus-prev)
  (define-key *workspace-map* "o" 'workspace-focus-last)
  (define-key *workspace-map* "m" 'window-workspace-move)
  (define-key *workspace-map* "r" 'workspace-rename)
  (define-key *workspace-map* "l" 'workspace-list)

  ;; resize sub-map
  ;; (define-key *resize-map* "Right"
  ;;   (lambda () (cmd-container-resize 'grow-right 0.05)))
  ;; (define-key *resize-map* "Left"
  ;;   (lambda () (cmd-container-resize 'shrink-left 0.05)))
  ;; (define-key *resize-map* "Down"
  ;;   (lambda () (cmd-container-resize 'grow-down 0.05)))
  ;; (define-key *resize-map* "Up"
  ;;   (lambda () (cmd-container-resize 'shrink-up 0.05)))
  (define-key *resize-map* "Return" 'prefix-abort)
  (define-key *resize-map* "Escape" 'prefix-abort)

  ;; root map
  (define-key *root-map* "c" "exec foot")
  (define-key *root-map* "C-c" "exec foot")
  (define-key *root-map* "e" "exec emacsclient -c")
  (define-key *root-map* "!" 'shell-command)
  (define-key *root-map* ";" 'colon)
  (define-key *root-map* ":" 'colon)

  ;; window operations
  (define-key *root-map* "n" 'window-focus-next)
  (define-key *root-map* "C-n" 'window-focus-next)
  (define-key *root-map* "space" 'window-focus-next)
  (define-key *root-map* "p" 'window-focus-prev)
  (define-key *root-map* "C-p" 'window-focus-prev)
  (define-key *root-map* "w" 'window-list)
  (define-key *root-map* "k" 'window-kill)
  (define-key *root-map* "C-k" 'window-kill)
  (define-key *root-map* "i" 'window-properties-show)

    ;; container
  ;; (command-register! 'container-split-horizontal cmd-container-split-horizontal "Split horizontally.")
  ;; (command-register! 'container-split-vertical cmd-container-split-vertical "Split vertically.")
  ;; (command-register! 'container-split-destroy cmd-container-destroy "Remove current split.")
  ;; (command-register! 'container-destory-others cmd-container-destory-others "Remove all splits.")
  ;; (command-register! 'container-focus-next-next-container cmd-container-focus-next "Focus next container.")
  ;; (command-register! 'container-focus-prev-prev-container cmd-container-focus-prev "Focus previous container.")
  ;; (command-register! 'container-balance-containers cmd-container-balance "Balance all containers.")

  ;; container operations
  (define-key *root-map* "S" 'container-split-horizontal)
  (define-key *root-map* "s" 'container-split-vertical)
  (define-key *root-map* "R" 'remove-split)
  (define-key *root-map* "Q" 'only)
  (define-key *root-map* "Tab" 'focus-next-container)
  (define-key *root-map* "r" *resize-map*)
  (define-key *root-map* "plus" 'balance-containers)

  ;; direction-based focus/move
  ;; (define-key *root-map* "Left"
  ;;   (lambda () (cmd-container-focus-direction 'left)))
  ;; (define-key *root-map* "Right"
  ;;   (lambda () (cmd-container-focus-direction 'right)))
  ;; (define-key *root-map* "Up"
  ;;   (lambda () (cmd-container-focus-direction 'up)))
  ;; (define-key *root-map* "Down"
  ;;   (lambda () (cmd-container-focus-direction 'down)))

  ;; workspace map
  (define-key *root-map* "g" *workspace-map*)

  ;; session
  (define-key *root-map* "a" 'time)
  (define-key *root-map* "C-l" 'config-reload)
  (define-key *root-map* "C-q" 'quit)

  ;; prefix
  (define-key *top-map* "C-t" '(enter-submap *root-map*))
  (gliver-hook-run! *keybinding-sync-request-hook*))
