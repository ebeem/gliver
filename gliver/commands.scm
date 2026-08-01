;;; gliver/commands.scm --- Command system for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver commands)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-69)
  #:use-module (gliver river connector)
  #:use-module (gliver core)
  #:use-module (gliver contrib ui palette)
  #:declarative? #f
  #:export (
			command-find
			command-all
			command-run
			command-run-by-name
			shell-command-output
			shell-process-spawn
			shell-process-spawn-detached
			window-focus-next
			window-focus-prev
			window-focus-last
			window-all-list
			window-workspace-list
			window-container-list
			window-kill
			window-float-toggle
			window-fullscreen-toggle
			window-swap
			window-mark
			window-workspace-move
			window-container-move-direction
			window-container-move-left
			window-container-move-right
			window-container-move-up
			window-container-move-down
			window-pull-by-number
			window-properties-show
			container-focus-direction
			container-focus-left
			container-focus-right
			container-focus-up
			container-focus-down
			workspace-create
			workspace-destroy
			workspace-focus-next
			workspace-focus-prev
			workspace-focus-last
			workspace-rename
			workspace-list
			output-focus-next
			output-focus-prev
			exec
			shell-command
			eval-cmd
			terminal-spawn
			dmenu-run
			command-palette-open
			config-reload
			gliver-quit
			restart
			time
			describe-variable
			describe-key
			describe-command
			where-is
			list-commands
			prefix-activated
			prefix-abort
			enter-submap
			send-prefix-key
			keybindings-clear!
))

;;; command registry
(define (command-find name)
  "Find a command by name (symbol or string)."
  (let ((sym (if (symbol? name) name (string->symbol name))))
    (hash-table-ref/default *command-registry* sym #f)))

(define (command-all)
  "Return a list of all registered command names."
  (map car (hash-table->alist *command-registry*)))

(define (command-run cmd . args)
  "Run a command record with ARGS."
  (when (command? cmd)
    (gliver-hook-run! *command-pre-hook* (command-name cmd) args)
    (let ((result (catch #t
                    (lambda () (apply (command-procedure cmd) args))
                    (lambda (key . rest)
                      (log-error "Command ~a error: ~a ~a"
                                 (command-name cmd) key rest)
                      #f))))
      (gliver-hook-run! *command-post-hook* (command-name cmd) args result)
      result)))

(define (command-run-by-name name . args)
  "Look up and run command NAME with ARGS."
  (let ((cmd (command-find name)))
    (if cmd
        (apply command-run cmd args)
        (if (string? name)
            (let ((parts (filter (lambda (s) (> (string-length s) 0))
                                 (string-split name #\space))))
              (if (and (pair? parts) (> (length parts) 1))
                  (let ((cmd-name (car parts))
                        (cmd-args (cdr parts)))
                    (apply command-run-by-name cmd-name (append cmd-args args)))
                  (begin
                    (log-warn "Unknown command: ~a" name)
                    #f)))
            (begin
              (log-warn "Unknown command: ~a" name)
              #f)))))

;;; shell
(define-command (shell-command-output cmd)
  #:interactive (string)
  "Run CMD via /bin/sh and return its stdout as a string."
  (let* ((port (open-input-pipe cmd))
         (output (get-string-all port)))
    (close-pipe port)
    (string-trim-right output #\newline)))

(define-command (shell-process-spawn . args)
  #:interactive (string)
  "Spawn a subprocess. ARGS is the command and arguments.
Returns the PID."
  (let ((pid (primitive-fork)))
    (cond
     ((zero? pid)
      (apply execlp (car args) args)
      (primitive-exit 127))
     (else pid))))

(define-command (shell-process-spawn-detached cmd)
  #:interactive (string)
  "Spawn CMD via /bin/sh in a detached subprocess (double-fork)."
  (system (string-append cmd " &")))

;;; window commands
(define-command (window-focus-next)
  "Focus the next window in the current container."
  (and-let* ((win (window-current))
			 (next (window-next win)))
    (when next
      (window-focus next))))

(define-command (window-focus-prev)
  "Focus the previous window in the current container."
  (and-let* ((win (window-current))
			 (prev (window-prev win)))
    (when prev
      (window-focus prev))))

(define-command (window-focus-last)
  "Switch to the previously focused window within the current container."
  (and-let* ((win (window-current))
			 (container (window-container win))
			 (prev (container-window-previous container))))
    (when prev
      (window-focus prev)))

(define-command (window-all-list)
  "Show a list of all available windows."
  (let* ((windows (windows (manager-windows *manager*))))
    (if (null? windows)
        (log-debug "No windows.")
		;; NOTE: there should be a call for dmenu prompt
        (log-debug "Show windows."))))

(define-command (window-workspace-list)
  "Show a list of current workspace windows."
  (and-let* ((workspace (workspace-current))
			 (windows (workspace-windows workspace)))
    (if (null? windows)
        (log-debug "No windows.")
		;; NOTE: there should be a call for dmenu prompt
        (log-debug "Show windows."))))

(define-command (window-container-list)
  "Show a list of current container windows."
  (and-let* ((container (container-current))
			 (windows (container-windows container)))
    (if (null? windows)
        (log-debug "No windows.")
		;; NOTE: there should be a call for dmenu prompt
        (log-debug "Show windows."))))

(define-command (window-kill)
  "Close the current window."
  (let ((win (window-current)))
    (if win
        (begin
          (log-info "Killing window: ~a" (window-title win))
          (window-close! win)
          (log-debug "Closed: ~a" (window-title win)))
        (log-debug "No current window."))))

;; TODO
(define-command (window-float-toggle)
  "Toggle the current window between tiled and floating."
  (log-warn "Not implemented yet!"))

(define-command (window-fullscreen-toggle)
  "Toggle fullscreen for the current window."
  (let ((win (window-current)))
    (when win
      (if (window-fullscreen? win)
          (window-fullscreen-exit! win)
          (window-fullscreen! win (window-output win))))))

;; TODO
(define-command (window-swap)
  "Swap windows between current container and another."
  (log-warn "Not implemented yet!"))

(define-command (window-mark)
  "Toggle mark on the current window."
  (let ((win (window-current)))
    (when win
      (window-marked-set! win (not (window-marked? win)))
      (log-debug "~a ~a"
               (if (window-marked? win) "Marked" "Unmarked")
               (window-title win)))))

(define-command (window-workspace-move name)
  #:interactive (string)
  "Move the current window to workspace NAME."
  (let ((win (window-current))
        (target (and name (workspace-find-by-name name))))
    (cond
     ((not win) (log-debug "No current window."))
     ((not target) (log-debug "Workspace not found: ~a" name))
     (else
      (window-move-to-workspace! win target)
      (log-debug "Moved to ~a." name)))))

(define-command (window-container-move-direction dir)
  #:interactive (string)
  "Move the current window to the container in direction DIR."
  (let* ((workspace (workspace-current))
         (container (container-current))
         (win (window-current))
         (target (and workspace container (container-in-direction dir container workspace))))
    (if (and win target)
        (begin
          (window-move-to-container! win target)
          ;(workspace-container-current-set! workspace target)
          (log-debug "Moved to container ~a" (container-id target)))
        (log-debug "Cannot move."))))

(define-command (window-container-move-left)
  "Focus the container in the left direction."
  (window-container-move-direction 'left))

(define-command (window-container-move-right)
  "Focus the container in the left direction."
  (window-container-move-direction 'right))

(define-command (window-container-move-up)
  "Focus the container in the left direction."
  (window-container-move-direction 'up))

(define-command (window-container-move-down)
  "Focus the container in the left direction."
  (window-container-move-direction 'down))

(define-command (window-pull-by-number n)
  #:interactive (integer)
  "Pull window N into the current container."
  (let ((win (window-find-by-id n))
        (container (container-current)))
    (when (and win container)
      (window-move-to-container! win container))))

(define-command (window-properties-show)
  "Show properties of the current window."
  (let ((win (window-current)))
    (if win
        (log-debug "id=~a title=~s app-id=~s class=~s float=~a"
                 (window-id win)
                 (window-title win)
                 (window-app-id win)
                 (window-class win)
                 (window-floating? win))
        (log-debug "No current window."))))

;;; container commands
;; (define-command (container-split-horizontal)
;;   "Split the current container horizontally."
;;   (let ((container (container-current)))
;;     (when container
;;       (container-split-horizontal! container 0.5)
;;       (log-debug "Split horizontal."))))

;; (define-command (container-split-vertical)
;;   "Split the current container vertically."
;;   (let ((container (container-current)))
;;     (when container
;;       (container-split-vertical! container 0.5)
;;       (log-debug "Split vertical."))))

;; (define-command (container-destroy)
;;   "Remove the current split."
;;   (let ((container (container-current)))
;;     (when container
;;       (container-split-remove! container)
;;       (log-debug "Split removed."))))

;; (define-command (container-destory-others)
;;   "Remove all splits in the current workspace."
;;   (let ((workspace (workspace-current)))
;;     (when workspace
;;       (container-split-only (workspace-containers workspace))
;;       (log-debug "Only one container."))))

;; (define-command (container-focus-next)
;;   "Focus the next container."
;;   (let* ((workspace (workspace-current))
;;          (container (container-current)))
;;     (when (and workspace container)
;;       (let ((nf (container-next container)))
;;         (when nf
;;           (workspace-container-current-set! workspace nf)
;;           (log-debug "Container ~a" (container-id nf)))))))

;; (define-command (container-focus-prev)
;;   "Focus the previous container."
;;   (let* ((workspace (workspace-current))
;;          (container (container-current)))
;;     (when (and workspace container)
;;       (let ((pf (container-prev container)))
;;         (when pf
;;           (workspace-container-current-set! workspace pf)
;;           (log-debug "Container ~a" (container-id pf)))))))

(define-command (container-focus-direction dir)
  #:interactive (string)
  "Focus the container in direction DIR."
  (and-let* ((target (container-in-direction dir)))
    (if target
		(container-focus! target)
        (log-debug "Couldn't find focus target"))))

(define-command (container-focus-left)
  "Focus the container in the left direction."
  (container-focus-direction 'left))

(define-command (container-focus-right)
  "Focus the container in the left direction."
  (container-focus-direction 'right))

(define-command (container-focus-up)
  "Focus the container in the left direction."
  (container-focus-direction 'up))

(define-command (container-focus-down)
  "Focus the container in the left direction."
  (container-focus-direction 'down))

;; (define-command (clamp val lo hi)
;;   (max lo (min hi val)))

;; (define (container-resize dir amount)
;;   "Resize the current container's parent split."
;;   (let ((container (container-current)))
;;     (when container
;;       (let ((parent (container-parent container)))
;;         (when (and parent (container-split? parent))
;;           (let* ((ratio (container-split-ratio parent))
;;                  (step (or amount 0.05))
;;                  (new-ratio (clamp (case dir
;;                                     ((grow-right grow-down) (+ ratio step))
;;                                     ((shrink-left shrink-up) (- ratio step))
;;                                     (else ratio))
;;                                   0.1 0.9)))
;;             (container-split-ratio-set! parent new-ratio)
;;             (gliver-hook-run! *container-resize-hook* container)))))))

;; (define-command (container-balance)
;;   "Equalize all container split ratios."
;;   (let ((workspace (workspace-current)))
;;     (when workspace
;;       (container-balance! (workspace-containers workspace))
;;       (log-debug "Containers balanced."))))

;;; workspace commands
(define-command (workspace-create name)
  #:interactive (string)
  "Create a new workspace."
  (let ((output (output-current)))
    (when output
      (let ((workspace (workspace-add! (or name "New") output)))
        (workspace-focus! workspace)
        (log-debug "Workspace ~a created." (workspace-name workspace))))))

(define-command (workspace-destroy)
  "Kill the current workspace."
  (let ((workspace (workspace-current)))
    (when workspace
      (if (= 1 (length (output-workspaces (output-current))))
          (log-debug "Cannot kill the last workspace.")
          (begin
            (workspace-remove! workspace)
            (log-debug "Workspace killed."))))))

(define-command (workspace-focus-next)
  "Switch to the next workspace."
  (let ((g (workspace-next (output-current))))
    (when g (workspace-focus! g))))

(define-command (workspace-focus-prev)
  "Switch to the previous workspace."
  (let ((g (workspace-prev (output-current))))
    (when g (workspace-focus! g))))

(define-command (workspace-focus-last)
  "Switch to the previously active workspace."
  (let ((prev (output-workspace-previous (output-current))))
    (when prev (workspace-focus! prev))))

(define-command (workspace-rename name)
  #:interactive (string)
  "Rename the current workspace."
  (let ((workspace (workspace-current)))
    (when (and workspace name)
      (workspace-name-set! workspace name)
      (log-debug "Renamed to ~a." name))))

(define-command (workspace-list)
  "List all workspaces."
  (let ((workspaces (output-workspaces (output-current))))
    (log-debug "~a"
             (string-join
              (map (lambda (g)
                     (format #f "~a~a:~a"
                             (if (eq? g (workspace-current)) "*" " ")
                             (workspace-id g)
                             (workspace-name g)))
                   workspaces)
              " "))))

;;; output commands
(define-command (output-focus-next)
  "Focus the next output."
  (let ((ns (output-next)))
    (when ns
      ;(manager-output-previous-set! *manager* (output-current))
      ;(manager-output-current-set! *manager* ns)
      (gliver-hook-run! *output-focus-hook* ns (manager-output-previous *manager*)))))

(define-command (output-focus-prev)
  "Focus the previous output."
  (let ((ps (output-prev)))
    (when ps
      ;(manager-output-previous-set! *manager* (output-current))
      ;(manager-output-current-set! *manager* ps)
      (gliver-hook-run! *output-focus-hook* ps (manager-output-previous *manager*)))))

;;; session commands
(define (truncate-string str limit)
  (if (> (string-length str) limit)
      (string-append (substring str 0 (- limit 3)) "...")
      str))

(define-command (exec cmd)
  #:interactive (string)
  "Execute a shell command."
  (when cmd
    (log-info "Exec: ~a" (truncate-string cmd 60))
    (shell-process-spawn-detached cmd)))

(define-command (shell-command)
  "Prompt for and execute a shell command."
  (read-one-line "Shell: "))

(define-command (eval-cmd expr-str)
  #:interactive (string)
  "Evaluate a Guile expression."
  (catch #t
    (lambda ()
      (let ((result (eval-string expr-str)))
        (log-debug "~a" result)
        result))
    (lambda (key . args)
      (log-error "Error: ~a ~a" key args)
      #f)))

(define-command (terminal-spawn)
  "Spawn default terminal."
  (exec (format #f "exec ~a" *terminal*)))

(define-command (dmenu-run)
  "Spawn dmenu run process."
  (exec (format #f "exec ~a" *dmenu-command*)))

(define-command (command-palette-open)
  "Open the colon command prompt."
  (read-one-line ":"
                 #:completions (map symbol->string (command-all))))

(define-command (config-reload)
  "Reload the configuration file."
  (config-reload!)
  (gliver-hook-run! *config-loaded-hook*)
  (log-debug "Config reloaded."))

(define-command (gliver-quit)
  "Quit Gliver."
  (gliver-hook-run! *shutdown-hook*)
  (manager-config-set! 'running? #f)
  (log-debug "Goodbye."))

(define-command (restart)
  "Restart Gliver."
  (gliver-hook-run! *restart-hook*)
  (log-debug "Restarting...")
  (config-reload!))

(define-command (time)
  "Show the current time."
  (log-debug "~a" (shell-command-output "date")))

(define-command (describe-variable)
  "Display the documentation of variables."
  (let ((selected (completion-show
				   (hash-table-fold *variable-registry*
									(lambda (name val-pair acc)
									  (let ((module (car val-pair))
											(docstring (string-replace-substring (cdr val-pair) "\n" " ")))
										(cons (make-dmenu-options
											   (list (list " " name (var-get name) (module-name module) docstring))
											   (list *palette-icon-color* *palette-name-color* *palette-value-color* *palette-help-color* *palette-doc-color*)
											   #:widths *palette-variables-widths*
											   #:searchable *palette-variables-searchable*
											   #:visible *palette-variables-visible*)
											  acc)))
									'())
				   #:keys (hash-table-keys *variable-registry*))))
	;; TODO: show a message with information about the variable
	;; and allow editing its value
	(log-info selected)))

(define-command (describe-key key-str)
  #:interactive (string)
  "Describe what a key binding does."
  (let* ((key (kbd key-str))
         (binding (lookup-key *root-map* key))
         (action (and binding (gliver-binding-action binding))))
    (if action
        (cond
         ((command? (command-find action))
          (log-debug "~a → ~a: ~a" key-str action
                   (command-docstring (command-find action))))
         ((gliver-keymap? action)
          (log-debug "~a → keymap: ~a" key-str (gliver-keymap-name action)))
         (else
          (log-debug "~a → ~a" key-str action)))
        (log-debug "~a is not bound." key-str))))

(define-command (describe-command name)
  #:interactive (string)
  "Describe a command."
  (let ((cmd (command-find name)))
    (if cmd
        (log-debug "~a: ~a" name (command-docstring cmd))
        (log-warn "Unknown command: ~a" name))))

(define-command (where-is name)
  #:interactive (string)
  "Find the keybinding for a command."
  (let ((bindings (filter (lambda (pair)
                            (let ((action (gliver-binding-action (cdr pair))))
                              (or (and (string? action)
                                       (string=? action (if (symbol? name)
                                                            (symbol->string name)
                                                            name)))
                                  (and (symbol? action) (eq? action name)))))
                          (gliver-keymap->alist *root-map*))))
    (if (null? bindings)
        (log-debug "~a is not on any key." name)
        (log-debug "~a is on ~a" name
                 (string-join (map (lambda (b) (gliver-key->string (car b))) bindings)
                              ", ")))))

(define-command (list-commands)
  "List all available commands."
  (log-debug "~a" (string-join (sort (map symbol->string (command-all))
                                   string<?)
                             " ")))

;;; prefix mode commands
(define-command (prefix-activated)
  "Handle prefix key activation."
  (manager-config-set! 'mode 'prefix)
  (log-debug "Prefix mode activated."))

(define-command (prefix-abort)
  "Abort prefix mode."
  (manager-config-set! 'mode 'normal)
  (log-debug "Aborted."))

(define-command (enter-submap name)
  #:interactive (string)
  "Enter a sub-keymap by name."
  (log-debug "Entering submap: ~a" name))

(define-command (send-prefix-key)
  "Send the prefix key to the focused application."
  (manager-config-set! 'mode 'normal)
  (let ((prefix (manager-config-ref 'prefix-key)))
    (shell-process-spawn-detached
     (format #f "wtype -M ctrl -k t -m ctrl" ))))

(define-command (keybindings-clear!)
  "Clear all keybindings from all standard keymaps."
  (gliver-keymap-clear! *top-map*)
  (gliver-keymap-clear! *root-map*)
  (gliver-keymap-clear! *workspace-map*)
  (gliver-keymap-clear! *resize-map*)
  (gliver-keymap-clear! *exchange-map*)
  (gliver-hook-run! *keybinding-sync-request-hook*))

