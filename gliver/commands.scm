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
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-69)
  #:use-module (gliver river connector)
  #:use-module (gliver core)
  #:declarative? #f
  #:export (
			command-record-docstring
			command-record-procedure
			command-record-name
			command-record?
			make-command-record
			*command-table*
			command-register!
			command-find
			command-all
			command-run
			command-run-by-name
			shell-command-output
			shell-process-spawn
			shell-process-spawn-detached
			*message-last*
			*message-callback*
			message
			message-no-timeout
			*input-callback*
			*input-prompt*
			*input-buffer*
			*input-completions*
			*input-result-callback*
			read-one-line
			read-yes-or-no
			handle-input-key
			cmd-window-focus-next
			cmd-window-focus-prev
			cmd-window-focus-other
			cmd-window-list
			cmd-window-kill
			cmd-window-fullscreen
			cmd-window-swap
			cmd-window-mark
			cmd-window-workspace-move
			cmd-window-container-move-direction
			cmd-container-focus-direction
			cmd-window-pull-by-number
			cmd-window-properties-show
			cmd-workspace-create
			cmd-workspace-destroy
			cmd-workspace-focus
			cmd-workspace-focus-next
			cmd-workspace-focus-prev
			cmd-workspace-focus-last
			cmd-workspace-rename
			cmd-workspace-list
			cmd-output-focus-next
			cmd-output-focus-prev
			cmd-exec
			cmd-shell-command
			cmd-eval-cmd
			cmd-terminal-spawn
			cmd-dmenu-run
			cmd-colon
			cmd-config-reload
			cmd-quit
			cmd-restart
			cmd-time
			cmd-describe-key
			cmd-describe-command
			cmd-where-is
			cmd-list-commands
			cmd-prefix-activated
			cmd-prefix-abort
			cmd-enter-submap
			cmd-send-prefix-key
			command-register-defaults!
			keybindings-clear!
))

;;; command registry
(define-record-type <command>
  (make-command-record name procedure docstring)
  command-record?
  (name      command-record-name)
  (procedure command-record-procedure)
  (docstring command-record-docstring))

(define *command-table* (make-hash-table))

(define (command-register! name proc docstring)
  "Register a command with NAME, PROC, and DOCSTRING."
  (hash-table-set! *command-table* (if (symbol? name) name (string->symbol name))
                   (make-command-record name proc docstring)))

(define (command-find name)
  "Find a command by name (symbol or string)."
  (let ((sym (if (symbol? name) name (string->symbol name))))
    (hash-table-ref/default *command-table* sym #f)))

(define (command-all)
  "Return a list of all registered command names."
  (map car (hash-table->alist *command-table*)))

(define (command-run cmd . args)
  "Run a command record with ARGS."
  (when (command-record? cmd)
    (gliver-hook-run! *command-pre-hook* (command-record-name cmd) args)
    (let ((result (catch #t
                    (lambda () (apply (command-record-procedure cmd) args))
                    (lambda (key . rest)
                      (log-error "Command ~a error: ~a ~a"
                                 (command-record-name cmd) key rest)
                      #f))))
      (gliver-hook-run! *command-post-hook* (command-record-name cmd) args result)
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
(define (shell-command-output cmd)
  "Run CMD via /bin/sh and return its stdout as a string."
  (let* ((port (open-input-pipe cmd))
         (output (get-string-all port)))
    (close-pipe port)
    (string-trim-right output #\newline)))

(define (shell-process-spawn . args)
  "Spawn a subprocess. ARGS is the command and arguments.
Returns the PID."
  (let ((pid (primitive-fork)))
    (cond
     ((zero? pid)
      ;; Child
      (apply execlp (car args) args)
      (primitive-exit 127))
     (else pid))))

(define (shell-process-spawn-detached cmd)
  "Spawn CMD via /bin/sh in a detached subprocess (double-fork)."
  (system (string-append cmd " &")))

;;; message display
(define *message-last* "")
(define *message-callback* #f)

(define (message fmt . args)
  "Display a message in the message bar."
  (let ((msg (apply format #f fmt args)))
    (set! *message-last* msg)
    (log-info "Message: ~a" msg)
    (when *message-callback*
      (*message-callback* msg))
    msg))

(define (message-no-timeout fmt . args)
  "Display a persistent message."
  (apply message fmt args))

;;; interactive input
(define *input-callback* #f)
(define *input-prompt* "")
(define *input-buffer* "")
(define *input-completions* '())
(define *input-result-callback* #f)

(define* (read-one-line prompt #:key (completions '()))
  "Request one line of input from the user.
In the async architecture, this sets up state and returns #f immediately.
The actual input is handled via handle-input-key callbacks."
  (set! *input-prompt* prompt)
  (set! *input-buffer* "")
  (set! *input-completions* completions)
  ;; display prompt
  (log-debug "~a" prompt)
  ;; return #f, the actual result comes via callback
  #f)

(define (read-yes-or-no prompt)
  "Ask a yes/no question."
  (read-one-line (string-append prompt " (y/n) ")))

(define (handle-input-key key-str)
  "Handle a keypress during input mode. Returns #f or the completed string."
  (cond
   ((string=? key-str "Return")
    (let ((result *input-buffer*))
      (set! *input-buffer* "")
      (set! *input-prompt* "")
      result))
   ((string=? key-str "Escape")
    (set! *input-buffer* "")
    (set! *input-prompt* "")
    (log-debug "Aborted.")
    #f)
   ((string=? key-str "BackSpace")
    (when (> (string-length *input-buffer*) 0)
      (set! *input-buffer*
        (substring *input-buffer* 0 (1- (string-length *input-buffer*)))))
    (log-debug "~a~a" *input-prompt* *input-buffer*)
    #f)
   ((string=? key-str "Tab")
    ;; Tab completion
    (let ((matches (filter (lambda (c)
                             (string-prefix? *input-buffer* c))
                           *input-completions*)))
      (cond
       ((null? matches) #f)
       ((= 1 (length matches))
        (set! *input-buffer* (car matches))
        (log-debug "~a~a" *input-prompt* *input-buffer*)
        #f)
       (else
        (log-debug "~a" (string-join matches " | "))
        #f))))
   ((= (string-length key-str) 1)
    ;; Regular character
    (set! *input-buffer* (string-append *input-buffer* key-str))
    (log-debug "~a~a" *input-prompt* *input-buffer*)
    #f)
   (else #f)))

;;; default commands

;;; window commands
(define (cmd-window-focus-next)
  "Focus the next window in the current container."
  ;; TODO: fix logic, use window-focus!
  )

(define (cmd-window-focus-prev)
  "Focus the previous window in the current container."
  ;; TODO: fix logic, use window-focus!
  )

(define (cmd-window-focus-other)
  "Switch to the previously focused window."
  (cmd-window-focus-prev))

(define (cmd-window-list)
  "Show a list of windows in the current workspace."
  (let* ((workspace (workspace-current))
         (wins (if workspace (workspace-windows workspace) '())))
    (if (null? wins)
        (log-debug "No windows.")
        (log-debug "~a"
                 (string-join
                  (map (lambda (w)
                         (format #f "~a:~a"
                                 (window-id w) (window-title w)))
                       wins)
                  " | ")))))

(define (cmd-window-kill)
  "Close the current window."
  (let ((win (window-current)))
    (if win
        (begin
          (log-info "Killing window: ~a" (window-title win))
          (window-close! win)
          (log-debug "Closed: ~a" (window-title win)))
        (log-debug "No current window."))))

;; (define (cmd-window-float-toggle)
;;   "Toggle the current window between tiled and floating."
;;   (let ((win (window-current)))
;;     (when win
;;       (window-toggle-float! win)
;;       (log-debug "~a: ~a"
;;                (if (window-floating? win) "Floating" "Tiled")
;;                (window-title win)))))

(define (cmd-window-fullscreen)
  "Toggle fullscreen for the current window."
  (let ((win (window-current)))
    (when win
      (if (window-fullscreen? win)
          (window-fullscreen-exit! win)
          (window-fullscreen! win (window-output win))))))

(define (cmd-window-swap)
  "Swap windows between current container and another."
  (log-debug "Select target container..."))

(define (cmd-window-mark)
  "Toggle mark on the current window."
  (let ((win (window-current)))
    (when win
      (window-marked-set! win (not (window-marked? win)))
      (log-debug "~a ~a"
               (if (window-marked? win) "Marked" "Unmarked")
               (window-title win)))))

(define (cmd-window-workspace-move name)
  "Move the current window to workspace NAME."
  (let ((win (window-current))
        (target (and name (workspace-find-by-name name))))
    (cond
     ((not win) (log-debug "No current window."))
     ((not target) (log-debug "Workspace not found: ~a" name))
     (else
      (window-move-to-workspace! win target)
      (log-debug "Moved to ~a." name)))))

(define (cmd-window-container-move-direction dir)
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

(define (cmd-window-pull-by-number n)
  "Pull window N into the current container."
  (let ((win (window-find-by-id n))
        (container (container-current)))
    (when (and win container)
      (window-move-to-container! win container))))

(define (cmd-window-properties-show)
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
;; (define (cmd-container-split-horizontal)
;;   "Split the current container horizontally."
;;   (let ((container (container-current)))
;;     (when container
;;       (container-split-horizontal! container 0.5)
;;       (log-debug "Split horizontal."))))

;; (define (cmd-container-split-vertical)
;;   "Split the current container vertically."
;;   (let ((container (container-current)))
;;     (when container
;;       (container-split-vertical! container 0.5)
;;       (log-debug "Split vertical."))))

;; (define (cmd-container-destroy)
;;   "Remove the current split."
;;   (let ((container (container-current)))
;;     (when container
;;       (container-split-remove! container)
;;       (log-debug "Split removed."))))

;; (define (cmd-container-destory-others)
;;   "Remove all splits in the current workspace."
;;   (let ((workspace (workspace-current)))
;;     (when workspace
;;       (container-split-only (workspace-containers workspace))
;;       (log-debug "Only one container."))))

;; (define (cmd-container-focus-next)
;;   "Focus the next container."
;;   (let* ((workspace (workspace-current))
;;          (container (container-current)))
;;     (when (and workspace container)
;;       (let ((nf (container-next container)))
;;         (when nf
;;           (workspace-container-current-set! workspace nf)
;;           (log-debug "Container ~a" (container-id nf)))))))

;; (define (cmd-container-focus-prev)
;;   "Focus the previous container."
;;   (let* ((workspace (workspace-current))
;;          (container (container-current)))
;;     (when (and workspace container)
;;       (let ((pf (container-prev container)))
;;         (when pf
;;           (workspace-container-current-set! workspace pf)
;;           (log-debug "Container ~a" (container-id pf)))))))

(define (cmd-container-focus-direction dir)
  "Focus the container in direction DIR."
  (let* ((workspace (workspace-current))
         (container (container-current))
         (target (and workspace container (container-in-direction dir container workspace))))
    (if target
        (begin
          ;(workspace-container-current-set! workspace target)
          (let ((win (container-window-current target))
                (seat (seat-current)))
            (when (and win seat)
              (seat-wm-window-focus seat win))))
        (log-debug "Couldn't find focus target"))))

;; (define (clamp val lo hi)
;;   (max lo (min hi val)))

;; (define (cmd-container-resize dir amount)
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

;; (define (cmd-container-balance)
;;   "Equalize all container split ratios."
;;   (let ((workspace (workspace-current)))
;;     (when workspace
;;       (container-balance! (workspace-containers workspace))
;;       (log-debug "Containers balanced."))))

;;; workspace commands
(define (cmd-workspace-create name)
  "Create a new workspace."
  (let ((output (output-current)))
    (when output
      (let ((workspace (workspace-add! (or name "New") output)))
        (workspace-focus! workspace)
        (log-debug "Workspace ~a created." (workspace-name workspace))))))

(define (cmd-workspace-destroy)
  "Kill the current workspace."
  (let ((workspace (workspace-current)))
    (when workspace
      (if (= 1 (length (output-workspaces (output-current))))
          (log-debug "Cannot kill the last workspace.")
          (begin
            (workspace-remove! workspace)
            (log-debug "Workspace killed."))))))

(define (cmd-workspace-focus-next)
  "Switch to the next workspace."
  (let ((g (workspace-next (output-current))))
    (when g (workspace-focus! g))))

(define (cmd-workspace-focus-prev)
  "Switch to the previous workspace."
  (let ((g (workspace-prev (output-current))))
    (when g (workspace-focus! g))))

(define (cmd-workspace-focus-last)
  "Switch to the previously active workspace."
  (let ((prev (output-workspace-previous (output-current))))
    (when prev (workspace-focus! prev))))

(define (cmd-workspace-rename name)
  "Rename the current workspace."
  (let ((workspace (workspace-current)))
    (when (and workspace name)
      (workspace-name-set! workspace name)
      (log-debug "Renamed to ~a." name))))

(define (cmd-workspace-list)
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
(define (cmd-output-focus-next)
  "Focus the next output."
  (let ((ns (output-next)))
    (when ns
      ;(manager-output-previous-set! *manager* (output-current))
      ;(manager-output-current-set! *manager* ns)
      (gliver-hook-run! *output-focus-hook* ns (manager-output-previous *manager*)))))

(define (cmd-output-focus-prev)
  "Focus the previous output."
  (let ((ps (output-prev)))
    (when ps
      ;(manager-output-previous-set! *manager* (output-current))
      ;(manager-output-current-set! *manager* ps)
      (gliver-hook-run! *output-focus-hook* ps (manager-output-previous *manager*)))))

;;; session commands
(define (cmd-exec cmd)
  "Execute a shell command."
  (when cmd
    (log-info "Exec: ~a" cmd)
    (shell-process-spawn-detached cmd)))

(define (cmd-shell-command)
  "Prompt for and execute a shell command."
  (read-one-line "Shell: "))

(define (cmd-eval-cmd expr-str)
  "Evaluate a Guile expression."
  (catch #t
    (lambda ()
      (let ((result (eval-string expr-str)))
        (log-debug "~a" result)
        result))
    (lambda (key . args)
      (log-error "Error: ~a ~a" key args)
      #f)))

(define (cmd-terminal-spawn)
  "Spawn default terminal."
  (cmd-exec (format #f "exec ~a" *terminal*)))

(define (cmd-dmenu-run)
  "Spawn dmenu run process."
  (cmd-exec (format #f "exec ~a" *dmenu*)))

(define (cmd-colon)
  "Open the colon command prompt."
  (read-one-line ":"
                 #:completions (map symbol->string (command-all))))

(define (cmd-config-reload)
  "Reload the configuration file."
  (config-reload!)
  (gliver-hook-run! *config-loaded-hook*)
  (log-debug "Config reloaded."))

(define (cmd-quit)
  "Quit Gliver."
  (gliver-hook-run! *shutdown-hook*)
  (manager-config-set! 'running? #f)
  (log-debug "Goodbye."))

(define (cmd-restart)
  "Restart Gliver."
  (gliver-hook-run! *restart-hook*)
  (log-debug "Restarting...")
  (config-reload!))

(define (cmd-time)
  "Show the current time."
  (log-debug "~a" (shell-command-output "date")))

(define (cmd-describe-key key-str)
  "Describe what a key binding does."
  (let* ((key (kbd key-str))
         (binding (lookup-key *root-map* key))
         (action (and binding (gliver-binding-action binding))))
    (if action
        (cond
         ((command-record? (command-find action))
          (log-debug "~a → ~a: ~a" key-str action
                   (command-record-docstring (command-find action))))
         ((gliver-keymap? action)
          (log-debug "~a → keymap: ~a" key-str (gliver-keymap-name action)))
         (else
          (log-debug "~a → ~a" key-str action)))
        (log-debug "~a is not bound." key-str))))

(define (cmd-describe-command name)
  "Describe a command."
  (let ((cmd (command-find name)))
    (if cmd
        (log-debug "~a: ~a" name (command-record-docstring cmd))
        (log-warn "Unknown command: ~a" name))))

(define (cmd-where-is name)
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

(define (cmd-list-commands)
  "List all available commands."
  (log-debug "~a" (string-join (sort (map symbol->string (command-all))
                                   string<?)
                             " ")))

;;; prefix mode commands
(define (cmd-prefix-activated)
  "Handle prefix key activation."
  (manager-config-set! 'mode 'prefix)
  (log-debug "Prefix mode activated."))

(define (cmd-prefix-abort)
  "Abort prefix mode."
  (manager-config-set! 'mode 'normal)
  (log-debug "Aborted."))

(define (cmd-enter-submap name)
  "Enter a sub-keymap by name."
  (log-debug "Entering submap: ~a" name))

(define (cmd-send-prefix-key)
  "Send the prefix key to the focused application."
  (manager-config-set! 'mode 'normal)
  (let ((prefix (manager-config-ref 'prefix-key)))
    (shell-process-spawn-detached
     (format #f "wtype -M ctrl -k t -m ctrl" ))))

;;; register all default commands
(define (command-register-defaults!)
  ;; window
  (command-register! 'window-focus-next cmd-window-focus-next "Focus the next window in the current container.")
  (command-register! 'window-focus-prev cmd-window-focus-prev "Focus the previous window in the current container.")
  (command-register! 'window-focus-other-window cmd-window-focus-other "Switch to the other window.")
  (command-register! 'window-list cmd-window-list "Show a list of windows.")
  (command-register! 'window-kill-window cmd-window-kill "Close the current window.")
  ;; (command-register! 'window-float-toggle-float cmd-window-float-toggle "Toggle floating state.")
  (command-register! 'window-fullscreen cmd-window-fullscreen "Toggle fullscreen.")
  (command-register! 'window-mark cmd-window-mark "Toggle mark on current window.")
  (command-register! 'window-properties-show cmd-window-properties-show
    "Show window properties.")
  (command-register! 'window-workspace-move cmd-window-workspace-move "Move window to a workspace.")

  ;; container
  ;; (command-register! 'container-split-horizontal cmd-container-split-horizontal "Split horizontally.")
  ;; (command-register! 'container-split-vertical cmd-container-split-vertical "Split vertically.")
  ;; (command-register! 'container-split-destroy cmd-container-destroy "Remove current split.")
  ;; (command-register! 'container-destory-others cmd-container-destory-others "Remove all splits.")
  ;; (command-register! 'container-focus-next cmd-container-focus-next "Focus next container.")
  ;; (command-register! 'container-focus-prev cmd-container-focus-prev "Focus previous container.")
  ;; (command-register! 'container-balance cmd-container-balance "Balance all containers.")
  (command-register! 'container-focus-direction cmd-container-focus-direction "Focus container in direction.")

  ;; workspace
  (command-register! 'workspace-create cmd-workspace-create "Create a new workspace.")
  (command-register! 'workspace-destroy cmd-workspace-destroy "Kill the current workspace.")
  ;(command-register! 'workspace-focus cmd-workspace-focus "Select a workspace by name.")
  (command-register! 'workspace-focus-next cmd-workspace-focus-next "Switch to the next workspace.")
  (command-register! 'workspace-focus-prev cmd-workspace-focus-prev "Switch to the previous workspace.")
  (command-register! 'workspace-focus-last cmd-workspace-focus-last "Switch to the last workspace.")
  (command-register! 'workspace-rename cmd-workspace-rename "Rename the current workspace.")
  (command-register! 'workspace-list cmd-workspace-list "List all workspaces.")

  ;; output
  (command-register! 'output-focus-next-next cmd-output-focus-next "Focus the next output.")
  (command-register! 'output-focus-prev-prev cmd-output-focus-prev "Focus the previous output.")

  ;; session
  (command-register! 'exec cmd-exec "Execute a shell command.")
  (command-register! 'shell-command cmd-shell-command "Prompt for a shell command.")
  (command-register! 'eval cmd-eval-cmd "Evaluate a Guile expression.")
  (command-register! 'terminal-spawn cmd-terminal-spawn "Spawn a terminal.")
  (command-register! 'colon cmd-colon "Open command prompt.")
  (command-register! 'config-reload cmd-config-reload "Reload configuration.")
  (command-register! 'quit cmd-quit "Quit Gliver.")
  (command-register! 'restart cmd-restart "Restart Gliver.")
  (command-register! 'time cmd-time "Show the current time.")
  (command-register! 'describe-key cmd-describe-key "Describe a key binding.")
  (command-register! 'describe-command cmd-describe-command "Describe a command.")
  (command-register! 'where-is cmd-where-is "Find keybinding for a command.")
  (command-register! 'commands cmd-list-commands "List all commands.")
  (command-register! 'send-prefix-key cmd-send-prefix-key "Send prefix to app.")

  ;; prefix mode (internal)
  (command-register! 'prefix-activated cmd-prefix-activated "Prefix activated.")
  (command-register! 'prefix-abort cmd-prefix-abort "Abort prefix mode."))

;; register on module load
(command-register-defaults!)

(define (keybindings-clear!)
  "Clear all keybindings from all standard keymaps."
  (gliver-keymap-clear! *top-map*)
  (gliver-keymap-clear! *root-map*)
  (gliver-keymap-clear! *workspace-map*)
  (gliver-keymap-clear! *resize-map*)
  (gliver-keymap-clear! *exchange-map*)
  (gliver-hook-run! *keybinding-sync-request-hook*))

