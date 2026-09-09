;;; gliver/contrib/commands/shell.scm --- Shell and session commands for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver contrib commands shell)
  #:use-module (gliver core)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (gliver contrib ui components launcher)
  #:declarative? #f
  #:export (
			shell-command-output
			shell-process-spawn
			shell-process-spawn-detached
			truncate-string
			exec
			shell-command
			eval-cmd
			terminal-spawn
			launcher-run
			command-palette-open
			config-reload
			gliver-quit
			restart
			time
))

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
  ;; TODO: implement read-one-line
  (log-warn "not implemented yet"))

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

(define-command (launcher-run)
  "Spawn dmenu run process."
  (launcher-show))

(define-command (command-palette-open)
  "Open the colon command prompt."
  ;; TODO: impelment read palette commands
  (log-warn "not implemented yet"))

(define-command (config-reload)
  "Reload the configuration file."
  (config-reload!)
  (gliver-hook-run! *config-loaded-hook*)
  (log-debug "Config reloaded."))

(define-command (gliver-quit)
  "Quit Gliver."
  (gliver-hook-run! *shutdown-hook*)
  (var-set! *running?* #f)
  (log-debug "Goodbye."))

(define-command (restart)
  "Restart Gliver."
  (gliver-hook-run! *restart-hook*)
  (log-debug "Restarting...")
  (config-reload!))

(define-command (time)
  "Show the current time."
  (log-debug "~a" (shell-command-output "date")))
