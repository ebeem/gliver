;;; gliver/core/types.scm --- Core data model for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module defines the core data model, registries, and entity records for Gliver.
;;; It handles:
;;;  - Registering and executing interactive commands (<command>)
;;;  - Storing hierarchical window management records (<manager-state>, <output>, <seat>,
;;;    <workspace>, <container>, <window>)
;;;  - Generating unique IDs and bitmask tags
;;;  - Implementing basic lookup utilities to find records by proxies, names, and IDs

(define-module (gliver core types)
  #:declarative? #f
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-69)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (system foreign)
  #:use-module (gliver core logs)
  #:use-module (gliver core helpers)
  #:use-module (gliver core hooks)
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
			command-find
			command-all
			command-run
			%command-run-by-string
			command-run-by-name
			%manager-wl-proxy-set!
			manager-wl-proxy
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
			*manager*
			*window-number-next*
			*container-id-next*
			*output-number-next*
			*workspace-number-next*
			*next-tag-bit*
			manager-window-number-next!
			manager-workspace-number-next!
			manager-output-number-next!
			manager-tag-next!
			container-id-next!
			%output-metadata-set!
			output-metadata
			%output-statusbar-set!
			output-statusbar
			%output-usable-height-set!
			output-usable-height
			%output-usable-width-set!
			output-usable-width
			%output-usable-y-set!
			output-usable-y
			%output-usable-x-set!
			output-usable-x
			%output-wallpaper-set!
			output-wallpaper
			%output-wl-ls-proxy-set!
			output-wl-ls-proxy
			%output-wl-proxy-set!
			output-wl-proxy
			%output-wl-output-proxy-set!
			output-wl-output-proxy
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
			output-usable-area-set!
			output-usable-area
			output-find-by-proxy
			output-find-by-ls-proxy
			output-find-by-wl-output
			output-find-by-name
			output-find-by-id
			output-wallpaper-set!
			output-statusbar-set!
			output-current
			%seat-metadata-set!
			seat-metadata
			%seat-wl-ls-proxy-set!
			seat-wl-ls-proxy
			%seat-wl-proxy-set!
			seat-wl-proxy
			%seat-wl-seat-set!
			seat-wl-seat
			%seat-pointer-op-set!
			seat-pointer-op?
			%seat-focus-type-set!
			seat-focus-type
			%seat-window-focused-set!
			seat-window-focused
			%seat-window-entered-set!
			seat-window-entered
			seat?
			%make-seat
			make-seat
			seat-find-by-proxy
			seat-find-by-ls-proxy
			seat-current
			%workspace-metadata-set!
			workspace-metadata
			%workspace-wallpaper-set!
			workspace-wallpaper
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
			workspace-find-by-name
			workspace-wallpaper-set!
			workspace-current
			%container-metadata-set!
			container-metadata
			%container-usable-height-set!
			container-usable-height
			%container-usable-width-set!
			container-usable-width
			%container-usable-y-set!
			container-usable-y
			%container-usable-x-set!
			container-usable-x
			%container-wl-node-proxy-set!
			container-wl-node-proxy
			%container-urgent-set!
			container-urgent?
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
			container-usable-area-set!
			container-usable-area
			make-container
			container-find-by-number
			container-current
			%window-metadata-set!
			window-metadata
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
			window-find-by-proxy
			window-find-by-id
			window-current
			window-workspace
			window-output
			manager-print-tree
))

;;; command record & registry
;;  - name: unique command identifier symbol
;;  - procedure: Guile procedure executing the command logic
;;  - interactive-spec: optional interactive specification list
;;  - docstring: human-readable documentation string describing command behavior
(define-record-type <command>
  (make-command name procedure interactive-spec docstring)
  command?
  (name              command-name)
  (procedure         command-procedure)
  (interactive-spec  command-interactive-spec)
  (docstring         command-docstring))

(define-var *command-registry* (make-hash-table)
			"A registry storing all defined command records <command>.")

(define (command-register! name proc interactive-spec docstring)
  "Register a command with NAME, PROC, INTERACTIVE-SPEC, and DOCSTRING.
Parameters: (NAME PROC INTERACTIVE-SPEC DOCSTRING)
  - NAME: Command name symbol or string.
  - PROC: Procedure executed when command is invoked.
  - INTERACTIVE-SPEC: List of interactive specification arguments.
  - DOCSTRING: Documentation string describing the command."
  (hash-table-set! *command-registry* (if (symbol? name) name (string->symbol name))
                   (make-command name proc interactive-spec docstring)))

;;; macro to define a command a and register it
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

;; utilities to find commands and run them
(define (command-find name)
  "Find a command by NAME (symbol or string).
Parameters: (NAME)
  - NAME: Command name symbol or string.
Returns <command> record or #f if not found."
  (let ((sym (if (symbol? name) name (string->symbol name))))
    (hash-table-ref/default *command-registry* sym #f)))

(define (command-all)
  "Return a list of all registered command names.
Parameters: None.
Returns list of command name symbols."
  (map car (hash-table->alist *command-registry*)))

(define (command-run cmd . args)
  "Run a command record CMD with ARGS.
Dispatches *command-pre-hook* and *command-post-hook* around execution.
Parameters: (CMD . ARGS)
  - CMD: <command> record to execute.
  - ARGS: Arguments passed to the command procedure.
Returns procedure result or #f on failure."
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

(define (%command-run-by-string command . extra-args)
  "Run commands in a space-separated command format e.g. \"workspace-switch 2\".
Parameters: (COMMAND)
  - COMMAND: Command in a space-separated string format.
Returns procedure result or #f if command is unknown or fails."
  (and-let* (((string? command))
             (parts    (string-tokenize command))
             ((> (length parts) 1))
             (cmd-name (car parts))
             (cmd-args (cdr parts)))
    (apply command-run-by-name cmd-name (append cmd-args extra-args))))

(define (command-run-by-name name . args)
  "Look up and run command NAME with ARGS.
Supports space-separated command strings (e.g. \"workspace-switch 2\").
Parameters: (NAME . ARGS)
  - NAME: Command name symbol or string.
  - ARGS: Arguments passed to the command procedure.
Returns procedure result or #f if command is unknown or fails."
  (let ((cmd (command-find name))
		(tokens? (and (string? name)
					  (> (length (string-tokenize name)) 1))))
    (cond
	 ;; if command is found by name, run it
     (cmd
      (apply command-run cmd args))
	 ;; if command is a space-separated string with arguments
	 ;; run it by string
     (tokens?
      (apply %command-run-by-string name args))
	 ;; otherwise, log a warning and return #f
     (else
      (log-warn "command: unknown command ~s" name)
      #f))))

;;; window manager global state (<manager-state>)
;;  - outputs: list of all <output> records representing connected screens
;;  - output-current: currently focused <output> record (or #f)
;;  - output-previous: previously focused <output> record (or #f)
;;  - seats: list of active <seat> (input devices)
;;  - windows: flat list of all active <window> records
;;  - wl-proxy: river_window_manager_v1 wayland proxy handle
(define-record-type <manager-state>
  (%make-manager-state outputs output-current output-previous
                       seats windows wl-proxy)
  manager-state?
  (outputs                manager-outputs                %manager-outputs-set!)
  (output-current         manager-output-current         %manager-output-current-set!)
  (output-previous        manager-output-previous        %manager-output-previous-set!)
  (seats                  manager-seats                  %manager-seats-set!)
  (windows                manager-windows                %manager-windows-set!)
  (wl-proxy               manager-wl-proxy               %manager-wl-proxy-set!))


;; singleton global window manager state
(define-var *manager*
  (%make-manager-state '()				; outputs
					   #f				; output-current
					   #f				; output-previous
					   '()				; seats
					   '()				; windows
					   %null-pointer)   ; wl-proxy
  "Global Gliver window manager state.")

;;; ID generation counters
(define-var *window-number-next* 0
			"Counter for assigning unique window IDs.")
(define-var *container-id-next* 0
			"Counter for assigning unique container IDs.")
(define-var *output-number-next* 0
			"Counter for assigning unique output IDs.")
(define-var *workspace-number-next* 0
			"Counter for assigning unique workspace IDs.")
(define-var *next-tag-bit* 1
			"Bitmask tracker for assigning unique workspace tags.")

(define (manager-window-number-next!)
  "Allocate and return the next unique window ID (integer).
Parameters: None.
Returns next unique integer window ID."
  (set! *window-number-next* (1+ *window-number-next*))
  *window-number-next*)

(define (manager-workspace-number-next!)
  "Allocate and return the next unique workspace ID (integer).
Parameters: None.
Returns next unique integer workspace ID."
  (set! *workspace-number-next* (1+ *workspace-number-next*))
  *workspace-number-next*)

(define (manager-output-number-next!)
  "Allocate and return the next unique output ID (integer).
Parameters: None.
Returns next unique integer output ID."
  (set! *output-number-next* (1+ *output-number-next*))
  *output-number-next*)

(define (manager-tag-next!)
  "Allocate and return the next unique workspace tag bitmask (integer).
Parameters: None.
Returns next unique integer workspace tag bitmask."
  (set! *next-tag-bit* (ash *next-tag-bit* 1))
  *next-tag-bit*)

(define (container-id-next!)
  "Allocate and return the next unique container ID (integer).
Parameters: None.
Returns next unique integer container ID."
  (set! *container-id-next* (1+ *container-id-next*))
  *container-id-next*)

;;; output record (<output>) and helper functions
;;  - id: unique integer identifier
;;  - name: string screen name (e.g. \"output-0\", \"eDP-1\")
;;  - x, y: compositor coordinates of top-left corner
;;  - width, height: pixel dimensions of output
;;  - workspaces: list of <workspace> records assigned to this screen
;;  - workspace-current: active visible <workspace>
;;  - workspace-previous: previously active <workspace>
;;  - wl-proxy: river_output_v1 proxy
;;  - wl-output: integer object ID of wl_output
;;  - wl-output-proxy: bound wl_output proxy
;;  - wallpaper: output-specific wallpaper path or hex string (or #f)
;;  - usable-x, usable-y, usable-width, usable-height: dimensions after subtracting statusbar/docks/panels
;;  - statusbar: attached statusbar state record (or #f)
;;  - metadata: alist for any metadata to be stored by extensions (wallpaper and statusbar should be here)
(define-record-type <output>
  (%make-output id name x y width height workspaces workspace-current
                workspace-previous wl-proxy wl-ls-proxy wl-output wl-output-proxy wallpaper
                usable-x usable-y usable-width usable-height
                statusbar metadata)
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
  (wl-output-proxy    output-wl-output-proxy    %output-wl-output-proxy-set!)
  (wl-proxy           output-wl-proxy           %output-wl-proxy-set!)
  (wl-ls-proxy        output-wl-ls-proxy        %output-wl-ls-proxy-set!)
  (wallpaper          output-wallpaper          %output-wallpaper-set!)
  (usable-x           output-usable-x           %output-usable-x-set!)
  (usable-y           output-usable-y           %output-usable-y-set!)
  (usable-width       output-usable-width       %output-usable-width-set!)
  (usable-height      output-usable-height      %output-usable-height-set!)
  (statusbar          output-statusbar          %output-statusbar-set!)
  (metadata           output-metadata           %output-metadata-set!))

(define* (make-output name
                      #:key (id (manager-output-number-next!)) (wl-proxy #f) (wl-ls-proxy #f) (x 0) (y 0) (width 1920) (height 1080)
                      (workspaces '()) (workspace-current #f) (workspace-previous #f) (wl-output #f) (wl-output-proxy #f) (wallpaper #f)
                      (usable-x #f) (usable-y #f) (usable-width #f) (usable-height #f)
                      (statusbar #f) (metadata '()))
  "Create a new <output> record with the given NAME.
Other parameters can be provided as keyword arguments.
Returns newly created <output> record."
  (%make-output id name
                x y
                width height
                workspaces workspace-current workspace-previous
                wl-proxy wl-ls-proxy wl-output wl-output-proxy wallpaper
                (or usable-x x)
                (or usable-y y)
                (or usable-width width)
                (or usable-height height)
                statusbar
				metadata))

(define (output-usable-area-set! output x y width height)
  "Set the usable area coordinates (X, Y) and dimensions (WIDTH, HEIGHT) for OUTPUT.
Parameters: (OUTPUT X Y WIDTH HEIGHT)
  - OUTPUT: Target <output> record.
  - X: Horizontal offset in pixels.
  - Y: Vertical offset in pixels.
  - WIDTH: Usable width in pixels.
  - HEIGHT: Usable height in pixels.
Returns unspecified."
  (log-info "Updating usable output area ~a to ~ax~a+(~a+~a)" output width height x y)
  (%output-usable-x-set! output x)
  (%output-usable-y-set! output y)
  (%output-usable-width-set! output width)
  (%output-usable-height-set! output height))

(define (output-usable-area output)
  "Return (values x y width height) of the usable area for OUTPUT.
Parameters: (OUTPUT)
  - OUTPUT: Target <output> record.
Returns 4 values: (values x y width height)."
  (values (output-usable-x output)
          (output-usable-y output)
          (output-usable-width output)
          (output-usable-height output)))

(define (output-find-by-proxy proxy)
  "Look up the <output> record by PROXY address.
Parameters: (PROXY)
  - PROXY: river_output proxy pointer.
Returns matching <output> record or #f."
  (and-let* (((valid-pointer? proxy)))
    (find (lambda (output)
			;; equals? can compare foreign pointers
            (equal? (output-wl-proxy output) proxy))
          (manager-outputs *manager*))))

(define (output-find-by-ls-proxy ls-proxy)
  "Look up the <output> record by LS-PROXY address.
Parameters: (LS-PROXY)
  - LS-PROXY: river_layer_shell_output proxy pointer.
Returns matching <output> record or #f."
  (and-let* (((valid-pointer? ls-proxy)))
    (find (lambda (output)
			;; equals? can compare foreign pointers
            (equal? (output-wl-ls-proxy output) ls-proxy))
          (manager-outputs *manager*))))

(define (output-find-by-wl-output proxy)
  "Look up the <output> record by wl_output PROXY.
Parameters: (PROXY)
  - PROXY: wl_output proxy pointer.
Returns matching <output> record or #f."
  (and-let* (((valid-pointer? proxy)))
    (find (lambda (out)
            (equal? (output-wl-output out) proxy))
          (manager-outputs *manager*))))

(define (output-find-by-name target-name)
  "Find an <output> record by its TARGET-NAME string.
Parameters: (TARGET-NAME)
  - TARGET-NAME: String output name.
Returns matching <output> record or #f."
  (find (lambda (s) (string=? (output-name s) target-name))
        (manager-outputs *manager*)))

(define (output-find-by-id target-id)
  "Find an <output> record by its integer TARGET-ID.
Parameters: (TARGET-ID)
  - TARGET-ID: Integer output identifier.
Returns matching <output> record or #f."
  (find (lambda (output) (= (output-id output) target-id))
        (manager-outputs *manager*)))

(define (output-wallpaper-set! output wallpaper)
  "Set the WALLPAPER for OUTPUT.
Parameters: (OUTPUT WALLPAPER)
  - OUTPUT: Target <output> record.
  - WALLPAPER: Path string or hex color."
  (%output-wallpaper-set! output wallpaper)
  (gliver-hook-run! *output-wallpaper-changed-hook* output wallpaper))

(define (output-statusbar-set! output statusbar)
  "Set the statusbar for OUTPUT.
Parameters: (OUTPUT STATUSBAR)
  - OUTPUT: Target <output> record.
  - STATUSBAR: Target <statusbar> record."
  (%output-statusbar-set! output statusbar)
  (gliver-hook-run! *output-statusbar-changed-hook* output statusbar))

(define (output-current)
  "Return the currently active <output> record from *manager*, or #f.
Parameters: None.
Returns currently active <output> record from *manager*, or #f."
  (manager-output-current *manager*))

;;; seat record (<seat>) and helper functions
;;  - window-entered: <window> currently under pointer
;;  - window-focused: <window> currently holding keyboard focus
;;  - focus-type: Symbol focus type ('exclusive 'non-exclusive 'none)
;;  - pointer-op?: boolean indicating whether an interactive pointer drag/resize operation is active
;;  - wl-seat: integer object ID of wl_seat
;;  - wl-proxy: river_seat_v1 proxy handle
;;  - wl-ls-proxy: river_layer_shell_seat_v1 proxy handle
;;  - metadata: alist for any metadata to be stored by extensions
(define-record-type <seat>
  (%make-seat window-entered window-focused focus-type wl-proxy wl-seat wl-ls-proxy metadata)
  seat?
  (window-entered     seat-window-entered     %seat-window-entered-set!)
  (window-focused     seat-window-focused     %seat-window-focused-set!)
  (focus-type         seat-focus-type         %seat-focus-type-set!)
  (pointer-op?        seat-pointer-op?        %seat-pointer-op-set!)
  (wl-seat            seat-wl-seat            %seat-wl-seat-set!)
  (wl-proxy           seat-wl-proxy           %seat-wl-proxy-set!)
  (wl-ls-proxy        seat-wl-ls-proxy        %seat-wl-ls-proxy-set!)
  (metadata           seat-metadata           %seat-metadata-set!))

(define* (make-seat #:key (wl-proxy #f) (wl-seat #f)
                    (window-entered #f) (window-focused #f)
					(focus-type 'none) (wl-ls-proxy #f)
					(metadata '()))
  "Create a new <seat> record.
Returns newly created <seat> record."
  (%make-seat window-entered window-focused focus-type wl-proxy wl-seat wl-ls-proxy metadata))

(define (seat-find-by-proxy proxy)
  "Look up the <seat> record by PROXY address.
Parameters: (PROXY)
  - PROXY: river_seat_v1 proxy pointer.
Returns matching <seat> record or #f."
  (and-let* (((valid-pointer? proxy)))
    (find (lambda (seat)
			;; equals? can compare foreign pointers
            (equal? (seat-wl-proxy seat) proxy))
          (manager-seats *manager*))))

(define (seat-find-by-ls-proxy ls-proxy)
  "Look up the <seat> record by LS-PROXY address.
Parameters: (LS-PROXY)
  - LS-PROXY: river_layer_shell_seat_v1 proxy pointer.
Returns matching <seat> record or #f."
  (and-let* (((valid-pointer? ls-proxy)))
    (find (lambda (seat)
			;; equals? can compare foreign pointers
            (equal? (seat-wl-ls-proxy seat) ls-proxy))
          (manager-seats *manager*))))

(define (seat-current)
  "Return the active <seat> record, or #f if no seats exist.
Parameters: None.
Returns active <seat> record, or #f if no seats exist."
  (let ((seats (manager-seats *manager*)))
    (if (pair? seats)
        (car seats)
        #f)))

;;; workspace record (<workspace>) and helpers
;;  - id: unique integer workspace identifier
;;  - name: user-visible workspace label
;;  - tag-mask: bitmask identifier for Wayland tag routing
;;  - containers: ordered list of <container> records in this workspace
;;  - output: parent <output> record
;;  - layout: alist or symbol specifying tiling layout ('manual, 'tiled, etc.)
;;  - container-current: currently focused <container>
;;  - container-previous: previously focused <container>
;;  - wallpaper: workspace-specific wallpaper override
;;  - metadata: alist for any metadata to be stored by extensions (wallpaper should be here)
(define-record-type <workspace>
  (%make-workspace id name tag-mask containers output
                   layout container-current container-previous wallpaper metadata)
  workspace?
  (id                   workspace-id                   %workspace-id-set!)
  (name                 workspace-name                 workspace-name-set!)
  (tag-mask             workspace-tag-mask             %workspace-tag-mask-set!)
  (containers           workspace-containers           %workspace-containers-set!)
  (output               workspace-output               %workspace-output-set!)
  (layout               workspace-layout               workspace-layout-set!)
  (container-current    workspace-container-current    %workspace-container-current-set!)
  (container-previous   workspace-container-previous   %workspace-container-previous-set!)
  (wallpaper            workspace-wallpaper            %workspace-wallpaper-set!)
  (metadata             workspace-metadata             %workspace-metadata-set!))

(define* (make-workspace #:key
                         (id (manager-workspace-number-next!))
                         (name "workspace")
                         (tag-mask (manager-tag-next!))
                         (containers '())
                         (output #f)
                         (layout 'manual)
                         (wallpaper #f)
						 (metadata '()))
  "Create a new <workspace> record.
Returns newly created <workspace> record."
  (%make-workspace id name tag-mask containers
                   output layout #f #f wallpaper metadata))

(define (workspace-find-by-name name)
  "Find a workspace by NAME on the current output.
Parameters: (NAME)
  - NAME: String workspace name.
Returns matching <workspace> record or #f."
  (find (lambda (g) (string=? (workspace-name g) name))
        (output-workspaces (output-current))))

(define (workspace-wallpaper-set! workspace wallpaper)
  "Set the wallpaper for WORKSPACE.
Parameters: (WORKSPACE WALLPAPER)
  - WORKSPACE: Target <workspace> record.
  - WALLPAPER: Path string or hex color."
  (%workspace-wallpaper-set! workspace wallpaper)
  (gliver-hook-run! *workspace-wallpaper-changed-hook* workspace wallpaper))

(define (workspace-current)
  "Return the currently active <workspace> on the focused output, or #f.
Parameters: None.
Returns currently active <workspace> on focused output, or #f."
  (and-let* ((output (output-current))
             (workspace (output-workspace-current output)))
    workspace))

;;; container record (<container>)
;;  - id: unique integer container identifier
;;  - workspace: parent <workspace> record
;;  - windows: ordered list of <window> records inside this container slot
;;  - window-current: active focused <window> in container
;;  - window-previous: previously active <window> in container
;;  - x, y: physical top-left pixel coordinates on screen
;;  - width, height: physical outer dimensions of the container
;;  - urgent?: boolean indicating if any child window is urgent
;;  - wl-node-proxy: river_wm_node_v1 proxy handle
;;  - usable-x, usable-y, usable-width, usable-height: inner area available for window content (after tabs/borders)
;;  - metadata: alist for any metadata to be stored by extensions
(define-record-type <container>
  (%make-container id workspace windows window-current window-previous
                   x y width height
                   urgent?
                   wl-node-proxy
                   usable-x usable-y usable-width usable-height metadata)
  container?
  (id                 container-id                 %container-id-set!)
  (workspace          container-workspace          %container-workspace-set!)
  (windows            container-windows            %container-windows-set!)
  (window-current     container-window-current     %container-window-current-set!)
  (window-previous    container-window-previous    %container-window-previous-set!)
  (x                  container-x                  %container-x-set!)
  (y                  container-y                  %container-y-set!)
  (width              container-width              %container-width-set!)
  (height             container-height             %container-height-set!)
  (urgent?            container-urgent?            %container-urgent-set!)
  (wl-node-proxy      container-wl-node-proxy      %container-wl-node-proxy-set!)
  (usable-x           container-usable-x           %container-usable-x-set!)
  (usable-y           container-usable-y           %container-usable-y-set!)
  (usable-width       container-usable-width       %container-usable-width-set!)
  (usable-height      container-usable-height      %container-usable-height-set!)
  (metadata           container-metadata           %container-metadata-set!))

(define (container-usable-area-set! container x y width height)
  "Set the usable area coordinates (X, Y) and dimensions (WIDTH, HEIGHT) for CONTAINER.
Parameters: (CONTAINER X Y WIDTH HEIGHT)
  - CONTAINER: Target <container> record.
  - X: Horizontal offset in pixels.
  - Y: Vertical offset in pixels.
  - WIDTH: Usable width in pixels.
  - HEIGHT: Usable height in pixels.
Returns unspecified."
  (%container-usable-x-set! container (inexact->exact (floor x)))
  (%container-usable-y-set! container (inexact->exact (floor y)))
  (%container-usable-width-set! container (max 0 (inexact->exact (floor width))))
  (%container-usable-height-set! container (max 0 (inexact->exact (floor height)))))

(define (container-usable-area container)
  "Return (values x y width height) of the usable area for CONTAINER.
Parameters: (CONTAINER)
  - CONTAINER: Target <container> record.
Returns 4 values: (values x y width height)."
  (values (container-usable-x container)
          (container-usable-y container)
          (container-usable-width container)
          (container-usable-height container)))

(define* (make-container #:key (workspace #f) (x 0) (y 0) (width 0) (height 0)
                         (urgent? #f) (wl-node-proxy #f) (usable-x #f)
						 (usable-y #f) (usable-width #f) (usable-height #f) (metadata '()))
  "Create a new <container> record.
Returns newly created <container> record."
  (%make-container (container-id-next!) workspace '() #f #f
                   (inexact->exact (floor x))
                   (inexact->exact (floor y))
                   (inexact->exact (floor width))
                   (inexact->exact (floor height))
                   urgent?
                   wl-node-proxy
                   (or usable-x x)
                   (or usable-y y)
                   (or usable-width width)
                   (or usable-height height)
				   metadata))

(define (container-find-by-number n workspace)
  "Find container with ID integer N in WORKSPACE.
Parameters: (N WORKSPACE)
  - N: Container integer ID.
  - WORKSPACE: <workspace> record to search in.
Returns matching <container> record or #f."
  (find (lambda (f) (= (container-id f) n))
        (workspace-containers workspace)))

(define (container-current)
  "Return the currently focused container in the active workspace, or #f.
Parameters: None.
Returns currently focused <container> in active workspace, or #f."
  (and-let* ((workspace (workspace-current))
             (container (workspace-container-current workspace)))
    container))

;;; window record (<window>) and helpers
;;  - id: unique integer window ID
;;  - title: application window title string
;;  - app-id: application ID string (e.g. \"foot\", \"firefox\")
;;  - class: legacy XWayland window class and instance strings
;;  - container: parent <container> record
;;  - floating?, transient?, marked?, urgent?: window state flags (all unused currently)
;;  - height-min, height-max, height, width-min, width-max, width, x, y: geometry & sizing hints
;;  - decoration-hint: client or server decoration mode hint
;;  - is-resizing: boolean flag set during interactive resizing
;;  - capabilities: supported protocol capabilities bitmask
;;  - destroyed?: #t if window is destroyed, some calls occur after window is removed before it's cleaned
;;  - fullscreen?: #t if window is currently in fullscreen mode
;;  - maximized?: #t if window is currently maximized (planned for floating)
;;  - visible?: #t if window is currently visible
;;  - pid: process ID of client process
;;  - parent: parent <window> record for sub windows (e.g. popups)
;;  - presentation-hint: presentation mode hint (vsync/async)
;;  - identifier: unique compositor surface identifier
;;  - wl-proxy: river_window_v1 proxy
;;  - wl-node-proxy: river_wm_node_v1 proxy
;;  - wl-decoration-above, wl-decoration-below: decoration surface proxies
;;  - metadata: alist for any metadata to be stored by extensions
(define-record-type <window>
  (%make-window id title app-id class instance container
                floating? transient? marked? urgent?
                height-min height-max height width-min width-max width x y
                decoration-hint is-resizing capabilities destroyed? fullscreen?
                maximized? visible? pid parent presentation-hint identifier
                wl-proxy wl-node-proxy wl-decoration-above wl-decoration-below metadata)
  window?
  (id					window-id							%window-id-set!)
  (title				window-title						%window-title-set!)
  (app-id				window-app-id						%window-app-id-set!)
  (class				window-class						%window-class-set!)
  (instance				window-instance						%window-instance-set!)
  (container			window-container					%window-container-set!)
  (floating?			window-floating?					%window-floating-set!)
  (transient?			window-transient?					%window-transient-set!)
  (marked?				window-marked?						%window-marked-set!)
  (urgent?				window-urgent?						%window-urgent-set!)
  (height-min			window-height-min					%window-height-min-set!)
  (height-max			window-height-max					%window-height-max-set!)
  (height				window-height						%window-height-set!)
  (width-min			window-width-min					%window-width-min-set!)
  (width-max			window-width-max					%window-width-max-set!)
  (width				window-width						%window-width-set!)
  (x					window-x							%window-x-set!)
  (y					window-y							%window-y-set!)
  (decoration-hint		window-decoration-hint				%window-decoration-hint-set!)
  (is-resizing			window-is-resizing					%window-is-resizing-set!)
  (capabilities			window-capabilities					%window-capabilities-set!)
  (destroyed?			window-destroyed?					%window-destroyed-set!)
  (fullscreen?			window-fullscreen?					%window-fullscreen-set!)
  (maximized?			window-maximized?					%window-maximized-set!)
  (visible?				window-visible?						%window-visbile-set!)
  (pid					window-pid							%window-pid-set!)
  (parent				window-parent						%window-parent-set!)
  (presentation-hint	window-presentation-hint			%window-presentation-hint-set!)
  (identifier			window-identifier					%window-identifier-set!)
  (wl-proxy				window-wl-proxy						%window-wl-proxy-set!)
  (wl-node-proxy		window-wl-node-proxy				%window-wl-node-proxy-set!)
  (wl-decoration-above  window-wl-decoration-above-proxy    %window-wl-decoration-above-proxy-set!)
  (wl-decoration-below  window-wl-decoration-below-proxy    %window-wl-decoration-below-proxy-set!)
  (metadata				window-metadata						%window-metadata-set!))

(define* (make-window #:key
                      (id (manager-window-number-next!))
                      (title "") (app-id "") (class "")
                      (instance "") (container #f)
                      (floating? #f) (transient? #f)
                      (marked? #f) (urgent? #f)
                      (height-min #f) (height-max #f) (height #f)
                      (width-min #f) (width-max #f) (width #f) (x 0) (y 0)
                      (decoration-hint #f) (is-resizing #f)
                      (capabilities '()) (destroyed? #f) (fullscreen? #f) (maximized? #f)
                      (visible? #t) (pid #f) (parent #f) (presentation-hint #f)
                      (identifier #f) (wl-proxy #f) (wl-node-proxy #f)
                      (wl-decoration-above #f) (wl-decoration-below #f) (metadata '()))
  "Create a new <window> record with default or keyword values.
Returns newly created <window> record."
  (%make-window id title app-id class instance container
                floating? transient? marked? urgent?
                height-min height-max height width-min width-max width x y
                decoration-hint is-resizing capabilities destroyed? fullscreen?
                maximized? visible? pid parent presentation-hint identifier
                wl-proxy wl-node-proxy wl-decoration-above wl-decoration-below metadata))

(define (window-find-by-proxy proxy)
  "Look up the <window> record by PROXY address.
Parameters: (PROXY)
  - PROXY: river_window_v1 proxy pointer.
Returns matching <window> record or #f."
  (and-let* (((valid-pointer? proxy)))
    (find (lambda (window)
			;; equals? can compare foreign pointers
            (equal? (window-wl-proxy window) proxy))
          (manager-windows *manager*))))

(define (window-find-by-id id)
  "Find a window by its integer ID.
Parameters: (ID)
  - ID: Integer window ID.
Returns matching <window> record or #f."
  (find (lambda (w) (= (window-id w) id))
        (manager-windows *manager*)))

(define (window-current)
  "Return the currently focused <window> in the active container, or #f.
Parameters: None.
Returns currently focused <window> in active container, or #f."
  (and-let* ((container (container-current))
             (window (container-window-current container)))
    window))

(define (window-workspace window)
  "Return the parent <workspace> of WINDOW, or #f.
Parameters: (WINDOW)
  - WINDOW: Target <window> record.
Returns parent <workspace> of WINDOW, or #f."
  (container-workspace (window-container window)))

(define (window-output window)
  "Return the parent <output> of WINDOW, or #f.
Parameters: (WINDOW)
  - WINDOW: Target <window> record.
Returns parent <output> of WINDOW, or #f."
  (workspace-output (window-workspace window)))

(set-record-type-printer!
 <manager-state>
 (lambda (out port)
   (format port "#<manager proxy=~s outputs=~a>"
           (manager-wl-proxy out)
           (length (manager-outputs out)))))

(set-record-type-printer!
 <output>
 (lambda (out port)
   (format port "#<output name=~s pos=~ax~a+~a+~a usable=~ax~a+~a+~a workspaces=~a current-workspace=~a prev-workspace=~a>"
		   (output-name out)
		   (output-width out) (output-height out)
		   (output-x out) (output-y out)
		   (output-usable-width out) (output-usable-height out)
		   (output-usable-x out) (output-usable-y out)
		   (length (output-workspaces out))
		   (if (output-workspace-current out) (workspace-id (output-workspace-current out)) #f)
		   (if (output-workspace-previous out) (workspace-id (output-workspace-previous out)) #f))))

(set-record-type-printer!
 <seat>
 (lambda (s port)
   (format port "#<seat ~a (~a window)>"
		   (seat-wl-proxy s)
		   (seat-window-focused s))))

(set-record-type-printer!
 <workspace>
 (lambda (g port)
   (format port "#<workspace id=~a name=~s container-current-id=~a, container-previous-id=~a tag=~a layout=~a>"
           (workspace-id g)
           (workspace-name g)
           (if (workspace-container-current g) (container-id (workspace-container-current g)) #f)
           (if (workspace-container-previous g) (container-id (workspace-container-previous g)) #f)
           (workspace-tag-mask g)
           (assq-ref (workspace-layout g) 'layout))))

(set-record-type-printer!
 <container>
 (lambda (f port)
   (format port "#<container id=~a pos=~ax~a+~a+~a usable=~ax~a+~a+~a wins=~a current-win=~a prev-win=~a>"
           (container-id f)
           (container-width f) (container-height f)
           (container-x f) (container-y f)
           (container-usable-width f) (container-usable-height f)
           (container-usable-x f) (container-usable-y f)
           (length (container-windows f))
           (if (container-window-current f) (window-id (container-window-current f)) #f)
           (if (container-window-previous f) (window-id (container-window-previous f)) #f))))

(set-record-type-printer!
 <window>
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

(define* (manager-print-tree #:optional (manager *manager*))
  "Return the MANAGER state tree in a hierarchical formatted string.
Parameters: ([MANAGER])
  - MANAGER: <manager-state> record (default *manager*).
Returns formatted string hierarchy of the manager state tree."
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
