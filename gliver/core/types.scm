;;; gliver/core/types.scm --- Core data model for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver core types)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-69)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (gliver core logs)
  #:use-module (gliver core hooks)
  #:use-module (system foreign)
  #:export (
			command-docstring
			command-interactive-spec
			command-procedure
			command-name
			command?
			make-command
			*command-registry*
			command-register!
			define-command
			name
			command-find
			command-all
			command-run
			command-run-by-name
			*variable-registry*
			variable-register!
			define-var
			var-get
			%var-set!
			var-set!
			%manager-wl-proxy-set!
			manager-wl-proxy
			%manager-config-set!
			manager-config
			%manager-windows-set!
			manager-windows
			%manager-seats-set!
			manager-seats
			%manager-output-previous-set!
			manager-output-previous
			%manager-output-current-set!
			manager-output-current
			%manager-outputs-set!
			manager-outputs
			manager-state?
			%make-manager-state
			manager-config-ref
			manager-config-set!
			manager-window-number-next!
			manager-workspace-number-next!
			manager-output-number-next!
			manager-tag-next!
			container-id-next!
			*manager*
			%output-wl-proxy-set!
			output-wl-proxy
			%output-wl-output-set!
			output-wl-output
			%output-workspace-previous-set!
			output-workspace-previous
			%output-workspace-current-set!
			output-workspace-current
			%output-workspaces-set!
			output-workspaces
			%output-height-set!
			output-height
			%output-width-set!
			output-width
			%output-y-set!
			output-y
			%output-x-set!
			output-x
			output-name-set!
			output-name
			%output-id-set!
			output-id
			output?
			%make-output
			make-output
			%seat-wl-proxy-set!
			seat-wl-proxy
			%seat-wl-seat-set!
			seat-wl-seat
			%seat-pointer-op-set!
			seat-pointer-op?
			%seat-window-focused-set!
			seat-window-focused
			%seat-window-entered-set!
			seat-window-entered
			seat-name-set!
			seat-name
			seat?
			%make-seat
			make-seat
			%workspace-container-previous-set!
			workspace-container-previous
			%workspace-container-current-set!
			workspace-container-current
			workspace-layout-set!
			workspace-layout
			%workspace-output-set!
			workspace-output
			%workspace-containers-set!
			workspace-containers
			%workspace-tag-mask-set!
			workspace-tag-mask
			workspace-name-set!
			workspace-name
			%workspace-id-set!
			workspace-id
			workspace?
			%make-workspace
			make-workspace
			%container-height-set!
			container-height
			%container-width-set!
			container-width
			%container-y-set!
			container-y
			%container-x-set!
			container-x
			%container-window-previous-set!
			container-window-previous
			%container-window-current-set!
			container-window-current
			%container-windows-set!
			container-windows
			%container-workspace-set!
			container-workspace
			%container-id-set!
			container-id
			container?
			%make-container
			make-container
			%window-wl-decoration-below-proxy-set!
			window-wl-decoration-below-proxy
			%window-wl-decoration-above-proxy-set!
			window-wl-decoration-above-proxy
			%window-wl-node-proxy-set!
			window-wl-node-proxy
			%window-wl-proxy-set!
			window-wl-proxy
			%window-identifier-set!
			window-identifier
			%window-presentation-hint-set!
			window-presentation-hint
			%window-parent-set!
			window-parent
			%window-pid-set!
			window-pid
			%window-visbile-set!
			window-visible?
			%window-maximized-set!
			window-maximized?
			%window-fullscreen-set!
			window-fullscreen?
			%window-destroyed-set!
			window-destroyed?
			%window-capabilities-set!
			window-capabilities
			%window-is-resizing-set!
			window-is-resizing
			%window-decoration-hint-set!
			window-decoration-hint
			%window-y-set!
			window-y
			%window-x-set!
			window-x
			%window-width-set!
			window-width
			%window-width-max-set!
			window-width-max
			%window-width-min-set!
			window-width-min
			%window-height-set!
			window-height
			%window-height-max-set!
			window-height-max
			%window-height-min-set!
			window-height-min
			%window-user-props-set!
			window-user-props
			%window-urgent-set!
			window-urgent?
			%window-marked-set!
			window-marked?
			%window-transient-set!
			window-transient?
			%window-floating-set!
			window-floating?
			%window-container-set!
			window-container
			%window-instance-set!
			window-instance
			%window-class-set!
			window-class
			%window-app-id-set!
			window-app-id
			%window-title-set!
			window-title
			%window-id-set!
			window-id
			window?
			%make-window
			make-window
			output-find-by-proxy
			output-find-by-name
			output-find-by-id
			output-current
			seat-find-by-proxy
			seat-current
			workspace-find-by-name
			workspace-current
			container-find-by-number
			container-current
			window-find-by-proxy
			window-find-by-id
			window-current
			window-workspace
			window-output
			manager-print-tree
			print-branch
))

(define-record-type <command>
  (make-command name procedure interactive-spec docstring)
  command?
  (name      command-name)
  (procedure command-procedure)
  (interactive-spec command-interactive-spec)
  (docstring command-docstring))

(define *command-registry* (make-hash-table))

(define (command-register! name proc interactive-spec docstring)
  "Register a command with NAME, PROC, INTERACTIVE-SPEC, and DOCSTRING."
  (hash-table-set! *command-registry* (if (symbol? name) name (string->symbol name))
                   (make-command name proc interactive-spec docstring)))

(define-syntax define-command
  (syntax-rules ()
    ;; pattern 1: with the #:interactive keyword
    ((_ (name . args) #:interactive interactive-spec docstring body ...)
     (begin
       (define (name . args)
         body ...)
       (command-register! 'name name 'interactive-spec docstring)))

    ;; pattern 2: without the #:interactive keyword
    ((_ (name . args) docstring body ...)
     (begin
       (define (name . args)
         body ...)
       (command-register! 'name name '() docstring)))))

;;; command registry utilities
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

(define *variable-registry* (make-hash-table))

(define (variable-register! name module docstring)
  "Register a variable's docstring for introspection."
  (hash-table-set! *variable-registry* name (cons module docstring)))

(define-syntax define-var
  (syntax-rules ()
    ;; pattern 1: (define-var name value "docstring")
    ((_ name init-value docstring)
     (begin
       (define name init-value)
       (variable-register! 'name (current-module) docstring)))

    ;; pattern 2: (define-var name value) - no docstring provided
    ((_ name init-value)
     (begin
       (define name init-value)
       (variable-register! 'name (current-module) ""))) ;; no docs

    ;; pattern 3: (define-var name) - initialized to #f by default
    ((_ name)
     (begin
       (define name #f)
       (variable-register! 'name (current-module) ""))))) ;; no docs

(define (var-get name)
  "Dynamically retrieve a variable's value by its symbol, using the registry."
  (let ((record (hash-table-ref/default *variable-registry* name #f)))
    (if record
        (let ((module (car record)))
          (module-ref module name))
        (log-error "var-get: variable not found in registry" name))))

(define (%var-set! name new-value)
  (let ((record (hash-table-ref/default *variable-registry* name #f)))
    (if record
        (let ((module (car record)))
          (module-set! module name new-value))
        (error "var-set!: Variable not found in registry" name))))

;; Expose var-set! as a macro that auto-quotes the name
(define-syntax var-set!
  (syntax-rules ()
    ((_ name new-value)
     (%var-set! 'name new-value))))

;;; display (global state)
;;; all setters are private and should only be generally used by the core
;;; modules to keep the state synced with river
(define-record-type <manager-state>
  (%make-manager-state outputs output-current output-previous
                       seats windows config wl-proxy)
  manager-state?
  (outputs                manager-outputs                %manager-outputs-set!)
  (output-current         manager-output-current         %manager-output-current-set!)
  (output-previous        manager-output-previous        %manager-output-previous-set!)
  (seats                  manager-seats                  %manager-seats-set!)
  (windows                manager-windows                %manager-windows-set!)
  (config                 manager-config                 %manager-config-set!)
  (wl-proxy               manager-wl-proxy               %manager-wl-proxy-set!))

(define (manager-config-ref key)
  "Look up KEY in the manager config hash table."
  (hash-table-ref/default (manager-config *manager*) key #f))

(define (manager-config-set! key value)
  "Set KEY to VALUE in the manager config hash table."
  (hash-table-set! (manager-config *manager*) key value))

(define (manager-window-number-next!)
  (let ((id (manager-config-ref 'window-number-next)))
    (manager-config-set! 'window-number-next (1+ id))
    id))

(define (manager-workspace-number-next!)
  (let ((id (manager-config-ref 'workspace-number-next)))
    (manager-config-set! 'workspace-number-next (1+ id))
    id))

(define (manager-output-number-next!)
  (let ((id (manager-config-ref 'output-number-next)))
    (manager-config-set! 'output-number-next (1+ id))
    id))

(define (manager-tag-next!)
  (let ((bit (manager-config-ref 'next-tag-bit)))
    (manager-config-set! 'next-tag-bit (ash bit 1))
    bit))

(define (container-id-next!)
  (let ((n (manager-config-ref 'container-id-next)))
    (manager-config-set! 'container-id-next (1+ n))
    n))

(define *manager*
  (let ((cfg (make-hash-table)))
    (hash-table-set! cfg 'prefix-timeout         1000)  ; ms
    (hash-table-set! cfg 'message-timeout        5)     ; seconds
    (hash-table-set! cfg 'mode                   'normal)
    (hash-table-set! cfg 'border-width           3)
    (hash-table-set! cfg 'border-color-focused   "#c6a0f6")
    (hash-table-set! cfg 'border-color-unfocused "#1e2030")
    (hash-table-set! cfg 'border-color-urgent    "#ed8796")
    (hash-table-set! cfg 'container-inner-gap    4)
    (hash-table-set! cfg 'container-outer-gap    8)
    (hash-table-set! cfg 'running?               #f)
    (hash-table-set! cfg 'window-number-next     0)
    (hash-table-set! cfg 'container-id-next  0)
    (hash-table-set! cfg 'output-number-next     0)
    (hash-table-set! cfg 'workspace-number-next  0)
    (hash-table-set! cfg 'next-tag-bit           1)
    (%make-manager-state
     '()   ; outputs
     #f    ; output-current
     #f    ; output-previous
     '()   ; seats
     '()   ; windows
     cfg
     #f))) ; wl-proxy

;;; output: similar to an emacs container and stumpwm screen head
;;; a single logical screen/monitor, treated by wayland as output
;;; all setters are private and should only be generally used by the core
;;; modules to keep the state synced with river
(define-record-type <output>
  (%make-output id name x y width height workspaces workspace-current
                workspace-previous wl-proxy wl-output)
  output?
  (id                 output-id                 %output-id-set!)
  (name               output-name               output-name-set!)
  (x                  output-x                  %output-x-set!)
  (y                  output-y                  %output-y-set!)
  (width              output-width              %output-width-set!)
  (height             output-height             %output-height-set!)
  (workspaces         output-workspaces         %output-workspaces-set!)
  (workspace-current  output-workspace-current  %output-workspace-current-set!)
  (workspace-previous output-workspace-previous %output-workspace-previous-set!)
  (wl-output          output-wl-output          %output-wl-output-set!)
  (wl-proxy           output-wl-proxy           %output-wl-proxy-set!))

(define* (make-output name
                      #:key (id (manager-output-number-next!)) (wl-proxy #f) (x 0) (y 0) (width 1920) (height 1080)
					  (workspaces '()) (workspace-current #f) (workspace-previous #f) (wl-output #f))
  "Create a new <output> record with the given NAME.
Other parameters (x, y, width, height, wl-proxy) can be provided as keyword arguments."
  (%make-output id name
                x y
                width height
				workspaces workspace-current workspace-previous
                wl-proxy wl-output))

;;; Seat: A single seat bundles together the different ways
;;; a user can provide input e.g. (mouse, keyboard, touch input)
(define-record-type <seat>
  (%make-seat name window-entered window-focused wl-proxy wl-seat)
  seat?
  (name               seat-name               seat-name-set!)
  (window-entered     seat-window-entered     %seat-window-entered-set!)
  (window-focused     seat-window-focused     %seat-window-focused-set!)
  (pointer-op?        seat-pointer-op?        %seat-pointer-op-set!)
  (wl-seat            seat-wl-seat            %seat-wl-seat-set!)
  (wl-proxy           seat-wl-proxy           %seat-wl-proxy-set!))

(define* (make-seat #:key (name #f) (wl-proxy #f) (wl-seat #f)
					(window-entered #f) (window-focused #f))
  (%make-seat name window-entered window-focused wl-proxy wl-seat))

;;; Workspace: similar to an emacs group/isolation concept and stumpwm group
;;; A collection of containers and their associated windows (workspace/virtual desktop)
(define-record-type <workspace>
  (%make-workspace id name tag-mask containers output
				   layout container-current container-previous)
  workspace?
  (id                   workspace-id                   %workspace-id-set!)
  (name                 workspace-name                 workspace-name-set!)
  (tag-mask             workspace-tag-mask             %workspace-tag-mask-set!)
  (containers           workspace-containers           %workspace-containers-set!)
  (output               workspace-output               %workspace-output-set!)
  (layout               workspace-layout               workspace-layout-set!)
  (container-current    workspace-container-current    %workspace-container-current-set!)
  (container-previous   workspace-container-previous   %workspace-container-previous-set!))

(define* (make-workspace #:key
						 (id (manager-workspace-number-next!))
						 (name "workspace")
                         (tag-mask (manager-tag-next!))
                         (containers '())
                         (output #f)
                         (layout 'tiled))
  (%make-workspace id name tag-mask containers
                   output layout #f #f))

;;; container: similar to an emacs window and stumpwm container
;;; a physical container or "slot" on the screen where a list of windows is displayed
(define-record-type <container>
  (%make-container id workspace windows window-current window-previous
                   x y width height)
  container?
  (id              container-id              %container-id-set!)
  (workspace       container-workspace       %container-workspace-set!)
  (windows         container-windows         %container-windows-set!)
  (window-current  container-window-current  %container-window-current-set!)
  (window-previous container-window-previous %container-window-previous-set!)
  (x               container-x               %container-x-set!)
  (y               container-y               %container-y-set!)
  (width           container-width           %container-width-set!)
  (height          container-height          %container-height-set!))

(define* (make-container #:key (workspace #f) (x 0) (y 0) (width 0) (height 0))
  "Create a new flat container."
  (%make-container (container-id-next!) workspace '() #f #f
                   (inexact->exact (floor x))
				   (inexact->exact (floor y))
				   (inexact->exact (floor width))
				   (inexact->exact (floor height))))

;;; window: similar to an emacs buffer and stumpwm window
;;; a single application (like a terminal, a browser, or an editor)
(define-record-type <window>
  (%make-window id title app-id class instance container
				floating? transient? marked? urgent? user-props
                height-min height-max height width-min width-max width x y
				decoration-hint is-resizing capabilities destroyed? fullscreen?
				maximized? visible? pid parent presentation-hint identifier
                wl-proxy wl-node-proxy wl-decoration-above wl-decoration-below)
  window?
  (id               window-id               %window-id-set!)
  (title            window-title            %window-title-set!)
  (app-id           window-app-id           %window-app-id-set!)
  (class            window-class            %window-class-set!)  ;; NOTE: check if needed
  (instance         window-instance         %window-instance-set!) ;; NOTE: check if needed
  (container        window-container        %window-container-set!)
  (floating?        window-floating?        %window-floating-set!) ;; NOTE: check if needed
  (transient?       window-transient?       %window-transient-set!) ;; NOTE: check if needed
  (marked?          window-marked?          %window-marked-set!)
  (urgent?          window-urgent?          %window-urgent-set!) ;; NOTE: check if needed
  (user-props       window-user-props       %window-user-props-set!) ;; NOTE: check if needed
  (height-min       window-height-min       %window-height-min-set!)
  (height-max       window-height-max       %window-height-max-set!)
  (height           window-height           %window-height-set!)
  (width-min        window-width-min        %window-width-min-set!)
  (width-max        window-width-max        %window-width-max-set!)
  (width            window-width            %window-width-set!)
  (x                window-x                %window-x-set!)
  (y                window-y                %window-y-set!)
  (decoration-hint  window-decoration-hint  %window-decoration-hint-set!)
  (is-resizing      window-is-resizing      %window-is-resizing-set!)
  (capabilities     window-capabilities     %window-capabilities-set!)
  (destroyed?       window-destroyed?       %window-destroyed-set!)
  (fullscreen?      window-fullscreen?      %window-fullscreen-set!)
  (maximized?       window-maximized?       %window-maximized-set!)
  (visible?         window-visible?         %window-visbile-set!)
  (pid              window-pid              %window-pid-set!)
  (parent           window-parent           %window-parent-set!)
  (presentation-hint  window-presentation-hint  %window-presentation-hint-set!)
  (identifier       window-identifier       %window-identifier-set!)
  (wl-proxy         window-wl-proxy         %window-wl-proxy-set!)
  (wl-node-proxy    window-wl-node-proxy    %window-wl-node-proxy-set!)
  (wl-decoration-above    window-wl-decoration-above-proxy    %window-wl-decoration-above-proxy-set!)
  (wl-decoration-below    window-wl-decoration-below-proxy    %window-wl-decoration-below-proxy-set!))

(define* (make-window #:key
                      (id (manager-window-number-next!))
                      (title "") (app-id "") (class "")
                      (instance "") (container #f)
                      (floating? #f) (transient? #f)
                      (marked? #f) (urgent? #f) (user-props '())
                      (height-min #f) (height-max #f) (height #f)
                      (width-min #f) (width-max #f) (width #f) (x 0) (y 0)
                      (decoration-hint #f) (is-resizing #f)
                      (capabilities '()) (destroyed? #f) (fullscreen? #f) (maximized? #f)
                      (visible? #t) (pid #f) (parent #f) (presentation-hint #f)
                      (identifier #f) (wl-proxy #f) (wl-node-proxy #f)
                      (wl-decoration-above #f) (wl-decoration-below #f))

  (%make-window id title app-id class instance container
                floating? transient? marked? urgent? user-props
                height-min height-max height width-min width-max width x y
                decoration-hint is-resizing capabilities destroyed? fullscreen?
				maximized? visible? pid parent presentation-hint identifier
                wl-proxy wl-node-proxy wl-decoration-above wl-decoration-below))

;;; helper printers

(set-record-type-printer! <manager-state>
  (lambda (out port)
    (format port "#<manager proxy=~s outputs=~a>"
            (manager-wl-proxy out)
            (length (manager-outputs out)))))

(set-record-type-printer! <output>
  (lambda (out port)
    (format port "#<output name=~s pos=~ax~a+~a+~a workspaces=~a current-workspace=~a prev-workspace=~a>"
            (output-name out)
            (output-width out) (output-height out)
            (output-x out) (output-y out)
            (length (output-workspaces out))
			(if (output-workspace-current out) (workspace-id (output-workspace-current out)) #f)
			(if (output-workspace-previous out) (workspace-id (output-workspace-previous out)) #f))))

(set-record-type-printer! <seat>
  (lambda (s port)
    (format port "#<seat ~a (~a window)>"
            (seat-name s)
            (seat-window-focused s))))

(set-record-type-printer! <workspace>
  (lambda (g port)
    (format port "#<workspace id=~a name=~s container-current-id=~a, container-current-id=~a tag=~a layout=~a>"
            (workspace-id g)
			(workspace-name g)
			(if (workspace-container-current g) (container-id (workspace-container-current g)) #f)
			(if (workspace-container-previous g) (container-id (workspace-container-previous g)) #f)
            (workspace-tag-mask g)
			(assq-ref (workspace-layout g) 'layout))))

(set-record-type-printer! <container>
  (lambda (f port)
    (format port "#<container id=~a pos=~ax~a+~a+~a wins=~a current-win=~a prev-win=~a>"
            (container-id f)
            (container-width f) (container-height f)
            (container-x f) (container-y f)
            (length (container-windows f))
			(if (container-window-current f) (window-id (container-window-current f)) #f)
			(if (container-window-previous f) (window-id (container-window-previous f)) #f))))

(set-record-type-printer! <window>
  (lambda (window port)
    (format port "#<window id=~a proxy=~a app-id=~s visible=~a pos=~ax~a@~ax~a container=~a>"
            (window-id window)
            (window-wl-proxy window)
            (window-app-id window)
			(window-visible? window)
			(window-width window)
			(window-height window)
			(window-x window)
			(window-y window)
			(if (window-container window) (container-id (window-container window)) #f))))

;;; Common utils

(define (output-find-by-proxy proxy)
  "Look up the <output> record by comparing the raw memory address of the proxy."
  (if (not (pointer? proxy))
      #f ;; early exit
      (let ((addr (pointer-address proxy))
            (outputs (manager-outputs *manager*)))
        (find (lambda (output)
                (let ((output-proxy (output-wl-proxy output)))
                  (and (pointer? output-proxy)
                       (= (pointer-address output-proxy) addr))))
              outputs))))

(define (output-find-by-name output-name)
  (find (lambda (s) (string=? (output-name s) output-name))
        (manager-outputs *manager*)))

(define (output-find-by-id target-id)
  (find (lambda (output) (= (output-id output) target-id))
        (manager-outputs *manager*)))

(define (output-current)
  (manager-output-current *manager*))

(define (seat-find-by-proxy proxy)
  "Look up the <seat> record by comparing the raw memory address of the proxy."
  (if (not (pointer? proxy))
      #f ;; early exit
      (let ((addr (pointer-address proxy))
            (seats (manager-seats *manager*)))
        (find (lambda (seat)
                (let ((proxy-seat (seat-wl-proxy seat)))
                  (and (pointer? proxy-seat)
                       (= (pointer-address proxy-seat) addr))))
              seats))))

(define (seat-current)
  "Return the first seat available or #f."
  (let ((seats (manager-seats *manager*)))
    (if (pair? seats)
        (car seats)
        #f)))

(define (workspace-find-by-name name)
  "Find a workspace by name on the current output."
  (find (lambda (g) (string=? (workspace-name g) name))
        (output-workspaces (output-current))))

(define (workspace-current)
  "Return the current active workspace"
  (and-let* ((output (output-current))
			 (workspace (output-workspace-current output)))
	workspace))

(define (container-find-by-number n workspace)
  "Find container with number N in WORKSPACE."
  (find (lambda (f) (= (container-id f) n))
        (workspace-containers workspace)))

(define (container-current)
  "Return the current focused container based on the active workspace."
  (and-let* ((workspace (workspace-current))
			 (container (workspace-container-current workspace)))
	container))

(define (window-find-by-proxy proxy)
  "Look up the <window> record by comparing the raw memory address of the proxy."
  (if (not (pointer? proxy))
      #f ;; early exit
      (let ((addr (pointer-address proxy))
            (windows (manager-windows *manager*)))
        (find (lambda (window)
                (let ((window-proxy (window-wl-proxy window)))
                  (and (pointer? window-proxy)
                       (= (pointer-address window-proxy) addr))))
              windows))))

(define (window-find-by-id id)
  "Find a window by its ID."
  (find (lambda (w) (= (window-id w) id))
        (manager-windows *manager*)))

(define (window-current)
  "Return the currently focused window."
  (and-let* ((container (container-current))
			 (window (container-window-current container)))
	window))

(define (window-workspace window)
  "Return the window workspace."
  (container-workspace (window-container window)))

(define (window-output window)
  "Return the window output."
  (workspace-output (window-workspace window)))

(define* (manager-print-tree #:optional (manager *manager*))
  "Return the manager's state tree as a string."
  (with-output-to-string
    (lambda ()
      (define (print-branch prefix item)
        (format #t "~a▸ ~a~%" prefix item))
      (print-branch "" manager)
      (for-each
       (lambda (output)
         (print-branch "  └─" output)
         (for-each
          (lambda (workspace)
            (print-branch "    └─" workspace)
            (for-each
             (lambda (container)
               (print-branch "      └─" container)
               (for-each
                (lambda (window)
                  (print-branch "        └─" window))
                (container-windows container)))
             (workspace-containers workspace)))
          (output-workspaces output)))
       (manager-outputs manager)))))
