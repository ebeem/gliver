;;; gliver/core/hooks.scm --- Hook system for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module provides the central event-hook infrastructure for Gliver.
;;; It handles:
;;;  - Defining hooks with name and expected argument
;;;  - Registering callback functions and executing them in priority
;;;
;;; NOTE: perhaps it's cleaner to define hooks in their respective modules
;;; currently hooks are all defined in this module which acts like a centralized hub
;;; it may seem easy to just move them around, but guile not having a flat global
;;; namespace will not play well with this approach. get-var may help but it
;;; will make adding hooks ugly which I don't want to do

(define-module (gliver core hooks)
  #:use-module (srfi srfi-9)
  #:use-module (gliver core logs)
  #:use-module (gliver core helpers)
  #:declarative? #f
  #:export (
			set-gliver-hook-functions!
			gliver-hook-functions
			gliver-hook-arity
			gliver-hook-name
			gliver-hook?
			%make-gliver-hook
			make-gliver-hook
			%gliver-hook-add!
			gliver-hook-add!
			gliver-hook-remove!
			%gliver-hook-run
			gliver-hook-run!
			gliver-hook-run-strict!
			gliver-hook-clear!
			gliver-hook-empty?
			*gliver-globals-bind-hook*
			*gliver-globals-unbind-hook*
			*gliver-globals-verify-hook*
			*gliver-listeners-attach-hook*
			*manager-connected-hook*
			*manager-disconnected-hook*
			*manager-destroy-hook*
			*manager-manage-start-hook*
			*manager-render-start-hook*
			*manager-tick-hook*
			%manager-session-unlocked-hook
			%manager-session-locked-hook
			%window-created-hook
			*window-container-removed-hook*
			*window-container-added-hook*
			%window-destroyed-hook
			%window-size-hint-changed-hook
			%window-size-changed-hook
			%window-app-id-changed-hook
			%window-title-changed-hook
			%window-parent-changed-hook
			%window-decoration-hint-changed-hook
			%window-pointer-move-requested-hook
			%window-pointer-resize-requested-hook
			%window-menu-requested-hook
			%window-maximize-requested-hook
			%window-unmaximize-requested-hook
			%window-fullscreen-requested-hook
			%window-fullscreen-exit-requested-hook
			%window-minimize-requested-hook
			%window-pid-changed-hook
			%window-presentation-hint-changed-hook
			%window-identifier-changed-hook
			*window-created-hook*
			*window-container-moved-hook*
			*window-destroyed-hook*
			*window-focused-hook*
			*window-unfocused-hook*
			*window-place-hook*
			*window-float-hook*
			*window-unfloat-hook*
			*window-size-changed-hook*
			*window-size-hint-changed-hook*
			*window-app-id-changed-hook*
			*window-title-changed-hook*
			*window-decoration-hint-changed-hook*
			*window-pointer-move-requested-hook*
			*window-pointer-resize-requested-hook*
			*window-menu-requested-hook*
			*window-resize-start-hook*
			*window-resize-end-hook*
			*window-capabilities-changed-hook*
			*window-fullscreen-entered-hook*
			*window-fullscreen-exited-hook*
			*window-fullscreen-entered-informed-hook*
			*window-fullscreen-exited-informed-hook*
			*window-maximized-hook*
			*window-unmaximized-hook*
			*window-pid-changed-hook*
			*window-parent-changed-hook*
			*window-presentation-hint-changed-hook*
			*window-identifier-changed-hook*
			*window-fullscreen-requested-hook*
			*window-fullscreen-exit-requested-hook*
			*window-minimize-requested-hook*
			*container-created-hook*
			*container-focused-hook*
			*container-unfocused-hook*
			*container-split-hook*
			*container-destroy-hook*
			*container-resize-hook*
			*container-position-changed-hook*
			*workspace-switch-hook*
			*workspace-created-hook*
			*workspace-destroy-hook*
			*workspace-wallpaper-changed-hook*
			%output-created-hook
			%output-removed-hook
			%output-object-id-changed-hook
			%output-position-changed-hook
			%output-dimensions-changed-hook
			*output-created-hook*
			*output-removed-hook*
			*output-object-id-changed-hook*
			*output-position-changed-hook*
			*output-dimensions-changed-hook*
			*output-destroy-hook*
			*output-focus-hook*
			*output-wallpaper-changed-hook*
			*output-statusbar-changed-hook*
			%seat-created-hook
			%seat-removed-hook
			%seat-object-id-changed-hook
			%seat-window-pointer-entered-hook
			%seat-window-pointer-left-hook
			%seat-window-interacted-hook
			%seat-shell-interacted-hook
			%seat-op-delta-changed-hook
			%seat-op-released-hook
			%seat-pointer-position-changed-hook
			*seat-created-hook*
			*seat-destroy-hook*
			*seat-object-id-changed-hook*
			*seat-window-entered-changed-hook*
			*seat-window-interacted-hook*
			*seat-shell-interacted-hook*
			*seat-seat-op-delta-changed-hook*
			*seat-seat-op-released-hook*
			*seat-seat-pointer-position-changed-hook*
			*seat-window-focused-hook*
			*seat-focus-type-changed-hook*
			%keybinding-pressed-hook
			%keybinding-released-hook
			%keybinding-repeat-stopped-hook
			%keybinding-eaten-hook
			*submap-change-hook**
			*command-pre-hook*
			*command-post-hook*
			*startup-hook*
			*shutdown-hook*
			*restart-hook*
			*config-loaded-hook*
			*wallpaper-changed-hook*
			*keyboard-layout-changed-hook*
			%layer-shell-output-non-exclusive-area-changed-hook
			%layer-shell-seat-focus-changed-hook
			%xkb-config-destroyed-hook
			%xkb-config-keyboard-created-hook
			%xkb-config-keymap-succeeded-hook
			%xkb-config-keymap-failed-hook
			%xkb-config-keyboard-removed
			%xkb-keyboard-input-device-hook
			%xkb-keyboard-layout-changed-hook
			%xkb-keyboard-capslock-enabled-hook
			%xkb-keyboard-capslock-disabled-hook
			%xkb-keyboard-numlock-enabled-hook
			%xkb-keyboard-numlock-disabled-hook
			%input-manager-finished-hook
			%input-manager-device-created-hook
			%input-device-removed-hook
			%input-device-type-hook
			%input-device-name-hook
			%wlr-layer-surface-configure-hook
			%wlr-layer-surface-closed-hook
))

;;; hook record definition
;;  - NAME: unique symbol identifying the hook name
;;  - ARITY: number of arguments to be passed to the hook
;;  - FUNCTIONS: ordered list of (order . (symbol . module)) or (order . procedure)
(define-record-type <gliver-hook>
  (%make-gliver-hook name arity functions)
  gliver-hook?
  (name      gliver-hook-name)
  (arity     gliver-hook-arity)
  (functions gliver-hook-functions set-gliver-hook-functions!))

(define* (make-gliver-hook name #:optional (arity 0))
  "Create a new hook named NAME that expects ARITY arguments.
Parameters: (NAME [ARITY])
  - NAME: Symbol name for the hook.
  - ARITY: Number of arguments the hook expects (default 0).
Returns new <gliver-hook> record."
  (%make-gliver-hook name arity '()))

(define* (%gliver-hook-add! hook fn caller-module #:optional (order 999))
  "Add function or quoted symbol FN to HOOK with priority ORDER (default 999).
Parameters: (HOOK FN CALLER-MODULE [ORDER])
  - HOOK: <gliver-hook> record.
  - FN: Procedure or quoted symbol.
  - CALLER-MODULE: Module of the caller.
  - ORDER: Numeric priority (default 999, lower runs earlier)."
  (let* ((current-funcs (gliver-hook-functions hook))
         (fn-name (if (symbol? fn) fn (and (procedure? fn) (procedure-name fn))))
         (cleaned (filter (lambda (pair)
                            (let ((existing (cdr pair)))
                              (not (or
                                    (eq? existing fn)
                                    (and (symbol? fn) (pair? existing) (eq? (car existing) fn))
                                    (and fn-name
                                         (procedure? existing)
                                         (eq? (procedure-name existing) fn-name))))))
                          current-funcs))
         ;; for symbols, store it as a pair: (symbol . module)
         ;; for lambdas, just store the procedure.
         (item-to-store (if (symbol? fn)
                            (cons fn caller-module)
                            fn))
         (new-list (append cleaned (list (cons order item-to-store)))))
    (set-gliver-hook-functions! hook
                                (stable-sort new-list (lambda (a b) (< (car a) (car b)))))))

(define-syntax gliver-hook-add!
  (syntax-rules ()
    ;; match when the user provides an explicit order
    ((_ hook fn order)
     (%gliver-hook-add! hook fn (current-module) order))
    ;; match when the user omits the order (default is 999)
    ((_ hook fn)
     (%gliver-hook-add! hook fn (current-module)))))

(define (gliver-hook-remove! hook fn)
  "Remove function or quoted symbol FN from HOOK.
Parameters: (HOOK FN)
  - HOOK: <gliver-hook> record.
  - FN: Procedure or quoted symbol to remove."
  (let ((fn-name (if (symbol? fn) fn (and (procedure? fn) (procedure-name fn)))))
    (set-gliver-hook-functions! hook
								(filter (lambda (pair)
										  (let ((existing (cdr pair)))
											(not (or
												  (eq? existing fn)
												  (and (symbol? fn) (pair? existing) (eq? (car existing) fn))
												  (and fn-name
													   (procedure? existing)
													   (eq? (procedure-name existing) fn-name))))))
										(gliver-hook-functions hook)))))

(define (%gliver-hook-run hook strict? args)
  "Invoke all registered callbacks in HOOK with ARGS.
Parameters: (HOOK STRICT? ARGS)
  - HOOK: <gliver-hook> record.
  - STRICT?: Boolean, whether unhandled exceptions terminate the process.
  - ARGS: Argument list passed to hook callbacks."
  (when *log-hooks*
    (log-debug "Running hook ~a ~a" (gliver-hook-name hook) args))
  (for-each
   (lambda (pair)
     (let ((item (cdr pair)))
       (catch #t
         (lambda ()
           (let ((fn (cond
                      ;; if it's a (symbol . module) pair, grab the latest definition
                      ((pair? item)
                       (catch #t
                         (lambda () (module-ref (cdr item) (car item)))
                         (lambda (k . r)
                           (log-error "Hook ~a: could not resolve ~a from ~a: ~a"
                                      (gliver-hook-name hook) (car item) (cdr item) r)
                           #f)))
                      ;; if it is a lambda, execute it directly
                      ((procedure? item)
                       item)
                      (else
                       (log-error "Invalid hook function format ~a" item)
                       #f))))
             (if (procedure? fn)
                 (apply fn args)
                 (log-error "Hook ~a: ~a is not a procedure"
                            (gliver-hook-name hook)
                            (if (pair? item) (car item) item)))))
         (lambda (key . rest)
           (log-error "Hook ~a: function raised ~a: ~a"
                      (gliver-hook-name hook) key rest)
           (when strict?
             (exit 1))))))
   (gliver-hook-functions hook)))

(define (gliver-hook-run! hook . args)
  "Run all functions in HOOK with ARGS in their defined order safely.
If a function fails, the error is logged and the rest of the callbacks continue.
Parameters: (HOOK . ARGS)
  - HOOK: <gliver-hook> record.
  - ARGS: Arguments passed to hook callbacks."
  (%gliver-hook-run hook #f args))

(define (gliver-hook-run-strict! hook . args)
  "Run all functions in HOOK with ARGS in their defined order strictly.
If a function fails, the error is logged and Gliver is terminated.
Parameters: (HOOK . ARGS)
  - HOOK: <gliver-hook> record.
  - ARGS: Arguments passed to hook callbacks."
  (%gliver-hook-run hook #t args))

(define (gliver-hook-clear! hook)
  "Remove all functions from HOOK.
Parameters: (HOOK)
  - HOOK: <gliver-hook> record to clear."
  (set-gliver-hook-functions! hook '()))

(define (gliver-hook-empty? hook)
  "Return #t if HOOK has no registered functions.
Parameters: (HOOK)
  - HOOK: <gliver-hook> record to check.
Returns #t if HOOK has no registered functions, #f otherwise."
  (null? (gliver-hook-functions hook)))

;;; river / wayland lifecycle hooks
(define-var *gliver-globals-bind-hook*
  (make-gliver-hook 'gliver-globals-bind 4)
  "Invoked during Wayland registry when a global interface is advertised.
Parameters: (REGISTRY PROTOCOL-NAME OBJECT-ID VERSION)
  - REGISTRY: Pointer to wl_registry.
  - PROTOCOL-NAME: Name of the advertised global interface.
  - OBJECT-ID: Wayland object ID.
  - VERSION: Interface version.")

(define-var *gliver-globals-unbind-hook*
  (make-gliver-hook 'gliver-globals-unbind 0)
  "Invoked during compositor shutdown or unbinding of Wayland globals.
Parameters: None.")

(define-var *gliver-globals-verify-hook*
  (make-gliver-hook 'gliver-verify-unbind 0)
  "Invoked after initial roundtrip to verify that critical globals were bound successfully.
Parameters: None.")

(define-var *gliver-listeners-attach-hook*
  (make-gliver-hook 'gliver-listeners-attach 0)
  "Invoked after global binding to attach Wayland event listeners.
Parameters: None.")

(define-var *manager-connected-hook*
  (make-gliver-hook 'manager-connected 0)
  "Invoked when Gliver successfully connects to the Wayland display and syncs initial state.
Parameters: None.")

(define-var *manager-disconnected-hook*
  (make-gliver-hook 'manager-disconnected 0)
  "Invoked when the Gliver disconnects from the compositor.
Parameters: None.
NOTE: currently unused.")

(define-var *manager-destroy-hook*
  (make-gliver-hook 'manager-destroy 0)
  "Invoked when the window manager instance is destroyed.
Parameters: None.
NOTE: currently unused.")

(define-var *manager-manage-start-hook*
  (make-gliver-hook 'manager-manage-start 0)
  "Invoked when River compositor sends a manage_start event (starting manage_sequence).
Parameters: None.")

(define-var *manager-render-start-hook*
  (make-gliver-hook 'manager-render-start 0)
  "Invoked when River compositor sends a render_start event (starting render_sequence).
Parameters: None.")

(define-var *manager-tick-hook*
  (make-gliver-hook 'manager-tick 0)
  "Invoked on each iteration of the Wayland event loop dispatch.
Parameters: None.")

(define-var %manager-session-unlocked-hook
  (make-gliver-hook '%manager-session-unlocked 0)
  "Invoked when the River session is unlocked.
Parameters: None.")

(define-var %manager-session-locked-hook
  (make-gliver-hook '%manager-session-locked 0)
  "Invoked when the River session is locked.
Parameters: None.")

(define-var %window-created-hook
  (make-gliver-hook '%window-created 3)
  "Invoked when new window is created/advertised.
Parameters: (DATA PROXY-MANAGER PROXY-WIN)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-MANAGER: Pointer to river_window_manager_v1 proxy.
  - PROXY-WIN: Pointer to advertised river_window_v1 proxy.")

(define-var *window-container-removed-hook*
  (make-gliver-hook 'window-container-removed 2)
  "Invoked when a window is removed from a container.
Parameters: (WINDOW CONTAINER)
  - WINDOW: The <window> record being detached.
  - CONTAINER: The <container> record (parent).")

(define-var *window-container-added-hook*
  (make-gliver-hook 'window-container-added 2)
  "Invoked when a window is placed into a container.
Parameters: (WINDOW CONTAINER)
  - WINDOW: The <window> record being attached.
  - CONTAINER: The target <container> record.")

(define-var %window-destroyed-hook
  (make-gliver-hook '%window-destroy 2)
  "Invoked just before river_window_v1 is destroyed from River's side.
Parameters: (DATA PROXY-WINDOW)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-WINDOW: river_window_v1 proxy handle.")

(define-var %window-size-hint-changed-hook
  (make-gliver-hook '%window-size-hint-changed 5)
  "Invoked when a window updates its minimum and maximum sizing constraint hints.
NOTE: you probably want to use *window-size-hint-changed-hook* instead.
Parameters: (PROXY-WINDOW MIN-W MIN-H MAX-W MAX-H)
  - PROXY-WINDOW: river_window_v1 proxy handle.
  - MIN-W, MIN-H, MAX-W, MAX-H: Sizing dimensions in pixels.")

(define-var %window-size-changed-hook
  (make-gliver-hook '%window-size-changed 3)
  "Invoked when a window width and height dimensions are changed.
Parameters: (PROXY-WINDOW WIDTH HEIGHT)
  - PROXY-WINDOW: river_window_v1 proxy handle.
  - WIDTH, HEIGHT: Surface dimensions in pixels.")

(define-var %window-app-id-changed-hook
  (make-gliver-hook '%window-app-id-changed 2)
  "Invoked when window app_id is changed.
Parameters: (PROXY-WINDOW APP-ID)
  - PROXY-WINDOW: river_window_v1 proxy handle.
  - APP-ID: String identifier of the application.")

(define-var %window-title-changed-hook
  (make-gliver-hook '%window-title-changed 2)
  "Invoked when window title string is changed.
Parameters: (PROXY-WINDOW TITLE)
  - PROXY-WINDOW: river_window_v1 proxy handle.
  - TITLE: Updated title string.")

(define-var %window-parent-changed-hook
  (make-gliver-hook '%window-parent-changed 2)
  "Invoked when window parent is changed.
Parameters: (PROXY-WINDOW PARENT-PROXY)
  - PROXY-WINDOW: river_window_v1 proxy handle.
  - PARENT-PROXY: river_window_v1 proxy handle of parent or %null-pointer.")

(define-var %window-decoration-hint-changed-hook
  (make-gliver-hook '%window-decoration-hint-changed 2)
  "Invoked when client decoration mode hint is changed.
Parameters: (PROXY-WINDOW HINT)
  - PROXY-WINDOW: river_window_v1 proxy handle.
  - HINT: Integer decoration hint enum (CSD, SSD, No Preference).")

(define-var %window-pointer-move-requested-hook
  (make-gliver-hook '%window-pointer-move-requested 2)
  "Invoked when a client requests an interactive window move via seat pointer.
Parameters: (PROXY-WINDOW PROXY-SEAT)
  - PROXY-WINDOW: river_window_v1 proxy handle.
  - PROXY-SEAT: river_seat_v1 proxy handle.")

(define-var %window-pointer-resize-requested-hook
  (make-gliver-hook '%window-pointer-resize-requested 3)
  "Invoked when a client requests an interactive window resize via seat pointer.
Parameters: (PROXY-WINDOW PROXY-SEAT EDGES)
  - PROXY-WINDOW: river_window_v1 proxy handle.
  - PROXY-SEAT: river_seat_v1 proxy handle.
  - EDGES: Integer bitmask of resizing edges.")

(define-var %window-menu-requested-hook
  (make-gliver-hook '%window-menu-requested 3)
  "Invoked when a client requests a window context menu at coordinates.
Parameters: (PROXY-WINDOW PROXY-SEAT X Y)
  - PROXY-WINDOW: river_window_v1 proxy handle.
  - PROXY-SEAT: river_seat_v1 proxy handle.
  - X, Y: Surface coordinates in pixels.")

(define-var %window-maximize-requested-hook
  (make-gliver-hook '%window-maximize-requested 1)
  "Invoked when a client requests to be maximized.
Parameters: (PROXY-WINDOW)
  - PROXY-WINDOW: river_window_v1 proxy handle.")

(define-var %window-unmaximize-requested-hook
  (make-gliver-hook '%window-unmaximize-requested 1)
  "Invoked when a client requests to exit maximized state.
Parameters: (PROXY-WINDOW)
  - PROXY-WINDOW: river_window_v1 proxy handle.")

(define-var %window-fullscreen-requested-hook
  (make-gliver-hook '%window-fullscreen-requested 2)
  "Invoked when a client requests fullscreen on a specified output.
Parameters: (PROXY-WINDOW PROXY-OUTPUT)
  - PROXY-WINDOW: river_window_v1 proxy handle.
  - PROXY-OUTPUT: river_output_v1 proxy handle.")

(define-var %window-fullscreen-exit-requested-hook
  (make-gliver-hook '%window-fullscreen-exit-requested 1)
  "Invoked when a client requests to exit fullscreen state.
Parameters: (PROXY-WINDOW)
  - PROXY-WINDOW: river_window_v1 proxy handle.")

(define-var %window-minimize-requested-hook
  (make-gliver-hook '%window-minimize-requested 1)
  "Invoked when a client requests to be minimized.
Parameters: (PROXY-WINDOW)
  - PROXY-WINDOW: river_window_v1 proxy handle.
Note: Completely unused in the codebase (high-level *window-minimize-requested-hook* is dispatched instead).")

(define-var %window-pid-changed-hook
  (make-gliver-hook '%window-pid-changed 2)
  "Invoked when client process PID is announced.
Parameters: (PROXY-WINDOW PID)
  - PROXY-WINDOW: river_window_v1 proxy handle.
  - PID: Integer process ID.")

(define-var %window-presentation-hint-changed-hook
  (make-gliver-hook '%window-presentation-hint-changed 2)
  "Invoked when a window presentation hint (async/vsync) is changed.
Parameters: (PROXY-WINDOW HINT)
  - PROXY-WINDOW: river_window_v1 proxy handle.
  - HINT: Integer presentation hint enum.")

(define-var %window-identifier-changed-hook
  (make-gliver-hook '%window-identifier-changed 2)
  "Invoked when a unique window identifier string is changed.
Parameters: (PROXY-WINDOW IDENTIFIER)
  - PROXY-WINDOW: river_window_v1 proxy handle.
  - IDENTIFIER: String unique identifier.")

(define-var *window-created-hook*
  (make-gliver-hook 'window-created 1)
  "Invoked when a new gliver <window> record is created.
Parameters: (WINDOW)
  - WINDOW: The initialized <window> record.")

(define-var *window-container-moved-hook*
  (make-gliver-hook 'window-container-removed 3)
  "Invoked when a window moves from one container to another.
Parameters: (WINDOW TARGET-CONTAINER CURRENT-CONTAINER)
  - WINDOW: The <window> being moved.
  - TARGET-CONTAINER: The destination <container>.
  - CURRENT-CONTAINER: The former <container>.")

(define-var *window-destroyed-hook*
  (make-gliver-hook 'window-destroyed 3)
  "Invoked when a window is closed and its resources are destroyed.
Parameters: (WINDOW CONTAINER WORKSPACE)
  - WINDOW: The destroyed <window> record.
  - CONTAINER: The <container> owning the window.
  - WORKSPACE: The <workspace> owning the container.")

(define-var *window-focused-hook*
  (make-gliver-hook 'window-focused 1)
  "Invoked when a window gains keyboard focus.
Parameters: (WINDOW)
  - WINDOW: The focused <window> record.")

(define-var *window-unfocused-hook*
  (make-gliver-hook 'window-unfocused 1)
  "Invoked when a window loses keyboard focus.
Parameters: (WINDOW)
  - WINDOW: The unfocused <window> record.")

;;; NOTE: these are supposed to be helpful if stacking/floating
;;; layout is to be implemented/supported
(define-var *window-place-hook*
  (make-gliver-hook 'window-place 2)
  "Invoked when window stacking/layer ordering is modified.
Parameters: (WINDOW PLACE-MODE)
  - WINDOW: The <window> record.
Note: currently unused.")

(define-var *window-float-hook*
  (make-gliver-hook 'window-float 1)
  "Invoked when a window transitions to floating state.
Parameters: (WINDOW)
  - WINDOW: The <window> record.
Note: currently unused.")

(define-var *window-unfloat-hook*
  (make-gliver-hook 'window-unfloat 1)
  "Invoked when a window transitions from floating to tiled state.
Parameters: (WINDOW)
  - WINDOW: The <window> record.
NOTE: currently unused.")

(define-var *window-size-changed-hook*
  (make-gliver-hook 'window-size-changed 3)
  "Invoked when high-level window dimensions change.
Parameters: (WINDOW WIDTH HEIGHT)
  - WINDOW: The <window> record.
  - WIDTH, HEIGHT: Surface dimensions in pixels.")

(define-var *window-size-hint-changed-hook*
  (make-gliver-hook 'window-size-hint-changed 5)
  "Invoked when high-level window sizing constraints change.
Parameters: (WINDOW MIN-W MIN-H MAX-W MAX-H)
  - WINDOW: The <window> record.
  - MIN-W, MIN-H, MAX-W, MAX-H: Sizing dimensions in pixels.")

(define-var *window-app-id-changed-hook*
  (make-gliver-hook 'window-app-id-changed 2)
  "Invoked when a window application ID updates.
Parameters: (WINDOW APP-ID)
  - WINDOW: The <window> record.
  - APP-ID: String application identifier.")

(define-var *window-title-changed-hook*
  (make-gliver-hook 'window-title-changed 2)
  "Invoked when a window title string updates.
Parameters: (WINDOW TITLE)
  - WINDOW: The <window> record.
  - TITLE: Updated title string.")

(define-var *window-decoration-hint-changed-hook*
  (make-gliver-hook 'window-decoration-hint-changed 2)
  "Invoked when window decoration mode hint changes.
Parameters: (WINDOW HINT)
  - WINDOW: The <window> record.
  - TITLE: Updated title string.")

(define-var *window-pointer-move-requested-hook*
  (make-gliver-hook 'window-pointer-move-requested 2)
  "Invoked when a window interactive move is initiated.
Parameters: (WINDOW SEAT)
NOTE: currently unused.")

(define-var *window-pointer-resize-requested-hook*
  (make-gliver-hook 'window-pointer-resize-requested 3)
  "Invoked when a window interactive resize is initiated.
Parameters: (WINDOW SEAT EDGES)
NOTE: currently unused.")

(define-var *window-menu-requested-hook*
  (make-gliver-hook 'window-menu-requested 3)
  "Invoked when a window menu is requested.
Parameters: (WINDOW SEAT X Y)
NOTE: currently unused.")

(define-var *window-resize-start-hook*
  (make-gliver-hook 'window-resize-start 1)
  "Invoked when an interactive window resize begins.
Parameters: (WINDOW)
  - WINDOW: The <window> being resized.")

(define-var *window-resize-end-hook*
  (make-gliver-hook 'window-resize-end 1)
  "Invoked when an interactive window resize completes.
Parameters: (WINDOW)
  - WINDOW: The resized <window>.")

(define-var *window-capabilities-changed-hook*
  (make-gliver-hook 'window-capabilities-changed 2)
  "Invoked when window capabilities (e.g. can-maximize, can-fullscreen) are changed.
Parameters: (WINDOW CAPABILITIES)
  - WINDOW: The <window> record.
  - CAPABILITIES: Capabilities flag (e.g. RIVER_WINDOW_V1_CAPABILITIES_WINDOW_MENU).")

(define-var *window-fullscreen-entered-hook*
  (make-gliver-hook 'window-fullscreen-entered 2)
  "Invoked when a window enters fullscreen mode.
Parameters: (WINDOW PREV-STATUS)
  - WINDOW: The <window> record.
  - PREV-STATUS: Boolean previous fullscreen status.")

(define-var *window-fullscreen-exited-hook*
  (make-gliver-hook 'window-fullscreen-exited 2)
  "Invoked when a window exits fullscreen mode.
Parameters: (WINDOW PREV-STATUS)
  - WINDOW: The <window> record.
  - PREV-STATUS: Boolean previous fullscreen status.")

(define-var *window-fullscreen-entered-informed-hook*
  (make-gliver-hook 'window-fullscreen-entered-informed 2)
  "Invoked when a window is informed that it entered fullscreen.
Parameters: (WINDOW OUTPUT)
  - WINDOW: The <window> record.
  - OUTPUT: The <output> displaying the fullscreen window.")

(define-var *window-fullscreen-exited-informed-hook*
  (make-gliver-hook 'window-fullscreen-exited-informed 2)
  "Invoked when a window is informed that it exited fullscreen.
Parameters: (WINDOW OUTPUT)
  - WINDOW: The <window> record.
  - OUTPUT: The <output> that had the fullscreen window.")

(define-var *window-maximized-hook*
  (make-gliver-hook 'window-maximized 2)
  "Invoked when a window enters maximized state.
Parameters: (WINDOW PREV-STATUS)
  - WINDOW: The <window> record.
  - PREV-STATUS: Boolean previous maximized status.")

(define-var *window-unmaximized-hook*
  (make-gliver-hook 'window-unmaximized 2)
  "Invoked when a window exits maximized state.
Parameters: (WINDOW PREV-STATUS)
  - WINDOW: The <window> record.
  - PREV-STATUS: Boolean previous maximized status.")

(define-var *window-pid-changed-hook*
  (make-gliver-hook 'window-pid-changed 2)
  "Invoked when window process ID is changed.
Parameters: (WINDOW PID)
  - WINDOW: The <window> record.
  - PID: The integer process identifier.")

(define-var *window-parent-changed-hook*
  (make-gliver-hook 'window-parent-changed 2)
  "Invoked when window parent is changed.
Parameters: (WINDOW PARENT)
  - WINDOW: The <window> record.
  - PARENT: The river_window_v1 proxy handle.")

(define-var *window-presentation-hint-changed-hook*
  (make-gliver-hook 'window-presentation-hint-changed 2)
  "Invoked when window presentation hint is changed.
Parameters: (WINDOW HINT)
  - WINDOW: The <window> record.
  - HINT: The presentation flag (e.g. RIVER_OUTPUT_V1_PRESENTATION_MODE_VSYNC).")

(define-var *window-identifier-changed-hook*
  (make-gliver-hook 'window-identifier-changed 2)
  "Invoked when window presentation hint is changed.
Parameters: (WINDOW HINT)
  - WINDOW: The <window> record.
  - ID-PTR: Pointer to identifier ASCII string.")

(define-var *window-fullscreen-requested-hook*
  (make-gliver-hook 'window-fullscreen-requested 1)
  "Invoked when a client requests to enter fullscreen mode.
Parameters: (WINDOW)
  - WINDOW: The target <window> record.")

(define-var *window-fullscreen-exit-requested-hook*
  (make-gliver-hook 'window-fullscreen-exit-requested 1)
  "Invoked when a client requests to exit fullscreen mode.
Parameters: (WINDOW)
  - WINDOW: The target <window> record.")

(define-var *window-minimize-requested-hook*
  (make-gliver-hook 'window-minimize-requested 1)
  "Invoked when a client requests to be minimized.
Parameters: (WINDOW)
  - WINDOW: The target <window> record.")

(define-var *container-created-hook*
  (make-gliver-hook 'container-created 1)
  "Invoked when a new <container> record is created.
Parameters: (CONTAINER)
  - CONTAINER: The newly created <container> record.")

(define-var *container-focused-hook*
  (make-gliver-hook 'container-focused 1)
  "Invoked when a container is focused.
Parameters: (CONTAINER)
  - CONTAINER: The focused <container> record.")

(define-var *container-unfocused-hook*
  (make-gliver-hook 'container-unfocused 1)
  "Invoked when a container is unfocused.
Parameters: (CONTAINER)
  - CONTAINER: The unfocused <container> record.")

(define-var *container-split-hook*
  (make-gliver-hook 'container-split 2)
  "Invoked when a container is split into two slots.
Parameters: (CONTAINER DIRECTION)
  - CONTAINER: The <container> that was splitted.
  - NEW-CONTAINER: The new created <container>.")

(define-var *container-destroy-hook*
  (make-gliver-hook 'container-destroy 2)
  "Invoked when a container is destroyed and removed from its workspace.
Parameters: (CONTAINER WORKSPACE)
  - CONTAINER: The destroyed <container> record.
  - WORKSPACE: The <workspace> owning the container.")

(define-var *container-resize-hook*
  (make-gliver-hook 'container-resize 1)
  "Invoked when a container dimensions/size change.
Parameters: (CONTAINER)
  - CONTAINER: The resized <container> record.")

(define-var *container-position-changed-hook*
  (make-gliver-hook 'container-resize 1)
  "Invoked when a container position change.
Parameters: (CONTAINER)
  - CONTAINER: The resized <container> record.")

(define-var *workspace-switch-hook*
  (make-gliver-hook 'workspace-switch 2)
  "Invoked when the active workspace on an output is changed.
Parameters: (WORKSPACE PREV-WORKSPACE)
  - WORKSPACE: The newly active <workspace>.
  - PREV-WORKSPACE: The previously active <workspace> or #f.")

(define-var *workspace-created-hook*
  (make-gliver-hook 'workspace-created 1)
  "Invoked when a new <workspace> record is created.
Parameters: (WORKSPACE)
  - WORKSPACE: The newly created <workspace> record.")

(define-var *workspace-destroy-hook*
  (make-gliver-hook 'workspace-destroy 1)
  "Invoked when a workspace is removed.
Parameters: (WORKSPACE)
  - WORKSPACE: The removed <workspace> record.")

(define-var *workspace-wallpaper-changed-hook*
  (make-gliver-hook 'workspace-wallpaper-changed 2)
  "Invoked when a workspace wallpaper is changed.
Parameters: (WORKSPACE WALLPAPER)
  - WORKSPACE: The <workspace> record.
  - WALLPAPER: String path, color string, or symbol mode.")

(define-var %output-created-hook
  (make-gliver-hook '%output-created 3)
  "Invoked when a new output is advertised.
Parameters: (DATA PROXY-MANAGER PROXY-OUTPUT)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-MANAGER: river_window_manager_v1 proxy handle.
  - PROXY-OUTPUT: river_output_v1 proxy handle.")

(define-var %output-removed-hook
  (make-gliver-hook '%output-destroy 2)
  "Invoked when an output has been disconnected.
Parameters: (DATA PROXY-OUTPUT)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-OUTPUT: river_output_v1 proxy handle.")

(define-var %output-object-id-changed-hook
  (make-gliver-hook '%output-object-id-changed 3)
  "Invoked when an output Wayland object ID changes.
Parameters: (DATA PROXY-OUTPUT OBJECT-ID)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-OUTPUT: river_output_v1 proxy handle.
  - OBJECT-ID: Integer object IDs.")

(define-var %output-position-changed-hook
  (make-gliver-hook '%output-position-changed 4)
  "Invoked when output global screen coordinates are changed.
Parameters: (DATA PROXY-OUTPUT X Y)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-OUTPUT: river_output_v1 proxy handle.
  - X, Y: Global coordinates in pixels.")

(define-var %output-dimensions-changed-hook
  (make-gliver-hook '%output-dimensions-changed 4)
  "Invoked when output width and height dimensions are changed.
Parameters: (PROXY-OUTPUT WIDTH HEIGHT DATA)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-OUTPUT: river_output_v1 proxy handle.
  - WIDTH, HEIGHT: Resolution dimensions in pixels.")

(define-var *output-created-hook*
  (make-gliver-hook 'output-created 1)
  "Invoked when an <output> record is created.
Parameters: (OUTPUT)
  - OUTPUT: The initialized <output> record.")

(define-var *output-removed-hook*
  (make-gliver-hook 'output-removed 1)
  "Invoked when an output is removed.
Parameters: (OUTPUT)
  - OUTPUT: The removed <output> record.")

(define-var *output-object-id-changed-hook*
  (make-gliver-hook 'output-object-id-changed 2)
  "Invoked when an output Wayland proxy object ID is changed.
Parameters: (OUTPUT PREV-OBJECT-ID)
  - OUTPUT: The <output> record.
  - PREV-OBJECT-ID: Previous integer Object ID.")

(define-var *output-position-changed-hook*
  (make-gliver-hook 'output-position-changed 3)
  "Invoked when an output position coordinates are changed.
Parameters: (OUTPUT X Y)
  - OUTPUT: The <output> record.
  - X, Y: Output screen coordinates in pixels.")

(define-var *output-dimensions-changed-hook*
  (make-gliver-hook 'output-dimensions-changed 3)
  "Invoked when an output dimensions are changed.
Parameters: (OUTPUT WIDTH HEIGHT)
  - OUTPUT: The <output> record.
  - WIDTH, HEIGHT: Pixel dimensions.")

(define-var *output-destroy-hook*
  (make-gliver-hook 'output-destroy 1)
  "Invoked when an output is destroyed.
Parameters: (OUTPUT)
  - OUTPUT: The destroyed <output> record.")

(define-var *output-focus-hook*
  (make-gliver-hook 'output-focus 2)
  "Invoked when focus transitions between outputs.
Parameters: (OUTPUT PREV-OUTPUT)
  - OUTPUT: The newly focused <output>.
  - PREV-OUTPUT: The previously focused <output> or #f.")

(define-var *output-wallpaper-changed-hook*
  (make-gliver-hook 'output-wallpaper-changed 2)
  "Invoked when output's wallpaper is changed.
Parameters: (OUTPUT WALLPAPER)
  - OUTPUT: The <output> record.
  - WALLPAPER: String path, color string, or symbol.")

(define-var *output-statusbar-changed-hook*
  (make-gliver-hook 'output-statusbar-changed 2)
  "Invoked when a statusbar is attached or modified on an output.
Parameters: (OUTPUT STATUSBAR)
  - OUTPUT: The <output> record.
  - STATUSBAR: The statusbar record instance.")

(define-var %seat-created-hook
  (make-gliver-hook '%seat-created 3)
  "Invoked when a seat is advertised.
Attach the river_seat_v1 listener.
Parameters: (DATA MANAGER SEAT-PROXY)
  - DATA: User context pointer (unused, null in Gliver).
  - MANAGER: Pointer to river_window_manager_v1 proxy.
  - SEAT-PROXY: Pointer to river_seat_v1 proxy.")

(define-var %seat-removed-hook
  (make-gliver-hook '%seat-removed 2)
  "Invoked when a seat is removed from compositor.
Parameters: (DATA PROXY-SEAT)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-SEAT: Pointer to river_seat_v1 proxy.")

(define-var %seat-object-id-changed-hook
  (make-gliver-hook '%seat-object-id-changed 3)
  "Invoked when a wl_seat is advertised river_seat_v1.
Parameters: (DATA PROXY-SEAT OBJECT-ID)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-SEAT: Pointer to river_seat_v1 proxy.
  - OBJECT-ID: Numeric Wayland object ID of the wl_seat global.")

(define-var %seat-window-pointer-entered-hook
  (make-gliver-hook '%seat-window-pointer-entered 3)
  "Invoked when a seat pointer enters a window area.
Parameters: (DATA PROXY-SEAT PROXY-WINDOW)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-SEAT: Pointer to river_seat_v1 proxy.
  - PROXY-WINDOW: Pointer to river_window_v1 proxy.")

(define-var %seat-window-pointer-left-hook
  (make-gliver-hook '%seat-window-pointer-left 2)
  "Invoked when a seat pointer leaves a window area.
Parameters: (DATA PROXY-SEAT)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-SEAT: Pointer to river_seat_v1 proxy.")

(define-var %seat-window-interacted-hook
  (make-gliver-hook '%seat-window-interacted 3)
  "Invoked when a seat interacts (click or input) with a window surface.
Parameters: (DATA PROXY-SEAT PROXY-WIN)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-SEAT: Pointer to river_seat_v1 proxy.
  - PROXY-WIN: Pointer to river_window_v1 proxy.")

(define-var %seat-shell-interacted-hook
  (make-gliver-hook '%seat-shell-interacted 3)
  "Invoked when a seat interacts (click or input) with a shell surface.
Parameters: (DATA PROXY-SEAT SHELL-PROXY)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-SEAT: Pointer to river_seat_v1 proxy.
  - SHELL-PROXY: Pointer to river_shell_surface_v1 proxy.")

(define-var %seat-op-delta-changed-hook
  (make-gliver-hook '%seat-op-delta-changed 4)
  "Invoked when a pointer/touch position delta changes during an interactive operation.
Parameters: (DATA PROXY-SEAT DX DY)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-SEAT: Pointer to river_seat_v1 proxy.
  - DX: Horizontal delta since operation start.
  - DY: Vertical delta since operation start.")

(define-var %seat-op-released-hook
  (make-gliver-hook '%seat-op-released 2)
  "Invoked when a seat interaction is released (pointer/touch release).
Parameters: (DATA PROXY-SEAT)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-SEAT: Pointer to river_seat_v1 proxy.")

(define-var %seat-pointer-position-changed-hook
  (make-gliver-hook '%seat-pointer-position-changed 4)
  "Invoked when a pointer position update in compositor logical coordinates.
Parameters: (DATA PROXY-SEAT X Y)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-SEAT: Pointer to river_seat_v1 proxy.
  - X: Logical horizontal pointer position.
  - Y: Logical vertical pointer position.")

(define-var *seat-created-hook*
  (make-gliver-hook 'seat-created 1)
  "Invoked when a <seat> record is created.
Parameters: (SEAT)
  - SEAT: The <seat> record.")

(define-var *seat-destroy-hook*
  (make-gliver-hook 'seat-destroy 1)
  "Invoked when a <seat> record is destroyed.
Parameters: (SEAT)
  - SEAT: The destroyed <seat> record.")

(define-var *seat-object-id-changed-hook*
  (make-gliver-hook 'seat-object-id-changed 2)
  "Invoked when a <seat> record wl_seat is updated.
Parameters: (SEAT PREV-OBJECT-ID)
  - SEAT: The <seat> record.
  - PREV-OBJECT-ID: Previous integer ID.")

(define-var *seat-window-entered-changed-hook*
  (make-gliver-hook 'seat-window-entered-changed 2)
  "Invoked when a <seat> enters a window.
Parameters: (SEAT WINDOW)
  - SEAT: The <seat> record.
  - WINDOW: The <window> entered or #f.")

(define-var *seat-window-interacted-hook*
  (make-gliver-hook 'seat-window-interacted 2)
  "Invoked when a <seat> interacts with a window surface.
Parameters: (SEAT WINDOW)
  - SEAT: The <seat> record.
  - WINDOW: The <window> interacted with.")

(define-var *seat-shell-interacted-hook*
  (make-gliver-hook 'seat-shell-interacted 2)
  "Invoked when a <seat> interacts with a shell surface.
Parameters: (SEAT PROXY-SHELL)
  - SEAT: The <seat> record.
  - PROXY-SHELL: Pointer to river_shell_surface_v1.")

(define-var *seat-seat-op-delta-changed-hook*
  (make-gliver-hook 'seat-seat-op-delta-changed 3)
  "Invoked when a <seat> position update in compositor logical coordinates.
Parameters: (SEAT DX DY)
  - SEAT: The <seat> record.
  - X: Logical horizontal pointer position.
  - Y: Logical vertical pointer position.")

(define-var *seat-seat-op-released-hook*
  (make-gliver-hook 'seat-seat-op-released 1)
  "Invoked when a <seat> interaction is released (pointer/touch release).
Parameters: (SEAT)
  - SEAT: The <seat> record.")

(define-var *seat-seat-pointer-position-changed-hook*
  (make-gliver-hook 'seat-seat-pointer-position-changed 3)
  "Invoked when a <seat> position update in compositor logical coordinates.
Parameters: (SEAT X Y)
  - SEAT: The <seat> record.
  - X: Logical horizontal pointer position.
  - Y: Logical vertical pointer position.")

(define-var *seat-window-focused-hook*
  (make-gliver-hook 'seat-window-focused 2)
  "Invoked when a <seat> focuses a window.
Parameters: (SEAT WINDOW)
  - SEAT: The <seat> record.
  - WINDOW: The focused <window> record or #f.")

(define-var *seat-focus-type-changed-hook*
  (make-gliver-hook 'seat-window-focused 2)
  "Invoked when a <seat> changes its focus type.
Parameters: (SEAT TYPE)
  - SEAT: The <seat> record.
  - TYPE: Symbol focus type ('exclusive 'non-exclusive 'none).")

(define-var %keybinding-pressed-hook
  (make-gliver-hook '%keybinding-pressed-hook 1)
  "Invoked when key binding is pressed.
Parameters: (PROXY-BINDING)
  - PROXY-BINDING: Pointer to river_xkb_binding_v1 proxy.")

(define-var %keybinding-released-hook
  (make-gliver-hook '%keybinding-released-hook 1)
  "Invoked when key binding is released.
Parameters: (PROXY-BINDING)
  - PROXY-BINDING: Pointer to river_xkb_binding_v1 proxy.")

(define-var %keybinding-repeat-stopped-hook
  (make-gliver-hook '%keybinding-repeat-stopped-hook 1)
  "Invoked when key binding repeat is stopped.
Parameters: (PROXY-BINDING)
  - PROXY-BINDING: Pointer to river_xkb_binding_v1 proxy.")

(define-var %keybinding-eaten-hook
  (make-gliver-hook '%keybinding-eaten-hook 1)
  "Invoked when a binding is eaten on seat.
Parameters: (PROXY-SEAT)
  - PROXY-SEAT: Pointer to river_xkb_bindings_seat_v1 proxy.")

(define-var *submap-change-hook**
  (make-gliver-hook 'keymap-change 1)
  "Invoked when active submap changes.
Parameters: (TARGET-MODE)
  - TARGET-MODE: Symbol representing the target submap mode.")

(define-var *command-pre-hook*
  (make-gliver-hook 'command-pre 2)
  "Invoked before an interactive command is executed via command-run.
Parameters: (COMMAND-NAME ARGS)
  - COMMAND-NAME: Symbol representing the command name.
  - ARGS: List of arguments passed to the command.")

(define-var *command-post-hook*
  (make-gliver-hook 'command-post 3)
  "Invoked after an interactive command is executed via command-run.
Parameters: (COMMAND-NAME ARGS)
  - COMMAND-NAME: Symbol representing the command name.
  - ARGS: List of arguments passed to the command.")

(define-var *startup-hook*
  (make-gliver-hook 'startup 0)
  "Invoked when Gliver startup sequence completes.
Parameters: None.")

(define-var *shutdown-hook*
  (make-gliver-hook 'shutdown 0)
  "Invoked when Gliver begins its shutdown sequence.
Parameters: None.")

(define-var *restart-hook*
  (make-gliver-hook 'restart 0)
  "Invoked when Gliver initiates a process restart.
Parameters: None.")

(define-var *config-loaded-hook*
  (make-gliver-hook 'config-loaded 0)
  "Invoked after user configuration (init.scm) has been loaded or reloaded.
Parameters: None.")

(define-var *wallpaper-changed-hook*
  (make-gliver-hook 'wallpaper-changed 1)
  "Invoked when global wallpaper setting is updated.
Parameters: (PATH-OR-COLOR)
  - PATH-OR-COLOR: String file path or color string.")

(define-var *keyboard-layout-changed-hook*
  (make-gliver-hook 'keyboard-layout-changed 2)
  "Invoked when keyboard layout changes.
Parameters: (LAYOUT-INDEX LAYOUT-CODE)
  - LAYOUT-INDEX: Integer layout index (0-indexed).
  - LAYOUT-CODE: Short layout code string (e.g. \"us\", \"ara\").")

(define-var %layer-shell-output-non-exclusive-area-changed-hook
  (make-gliver-hook '%layer-shell-output-non-exclusive-area-changed-hook 6)
  "Invoked when non exclusive area of a layer shell output changes.
Window manager should respect this area and update output available space accordingly.
The area maybe occupied by layer shell components like statusbar.
Parameters: (DATA PROXY-LS-OUTPUT X Y WIDTH HEIGHT)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-LS-OUTPUT: Pointer to river_layer_shell_output_v1 proxy.
  - X: Remaining usable area top-left corner global x coordinate.
  - Y: Remaining usable area top-left corner global y coordinate.
  - WIDTH: Remaining usable area width of the output.
  - HEIGHT: Remaining usable area height of the output.")

(define-var %layer-shell-seat-focus-changed-hook
  (make-gliver-hook '%layer-shell-seat-focus-changed-hook 3)
  "Invoked when seat focus changes between exclusive and non-exclusive areas.
Parameters: (DATA PROXY-LS-SEAT TYPE)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-LS-SEAT: Pointer to river_layer_shell_output_v1 proxy.
  - TYPE: Symbol focus type ('exclusive 'non-exclusive 'none).")

(define-var %xkb-config-destroyed-hook
  (make-gliver-hook '%xkb-config-destroyed-hook 2)
  "Invoked when river decides to stop sending events for river_xkb_config_v1 object.
It will be invoked after sending the stop request and it means that the object should be destroyed.
Parameters: (DATA PROXY)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY: Pointer to river_xkb_config_v1 proxy.")

(define-var %xkb-config-keyboard-created-hook
  (make-gliver-hook '%xkb-config-keyboard-created-hook 3)
  "Invoked when a new river_xkb_keyboard_v1 is advertised by compositor.
Parameters: (DATA PROXY-CONFIG PROXY-KEYBOARD)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-CONFIG: Pointer to river_xkb_config_v1 proxy.
  - PROXY-KEYBOARD: Pointer to river_xkb_keyboard_v1 proxy.")

(define-var %xkb-config-keymap-succeeded-hook
  (make-gliver-hook '%xkb-config-keymap-succeeded-hook 2)
  "Invoked when created river_xkb_keymap_v1 via river_xkb_config_v1 succeeded.
It's possible after the event to set this keymap to a keyboard.
Parameters: (DATA PROXY)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY: Pointer to river_xkb_keymap_v1 proxy.")

(define-var %xkb-config-keymap-failed-hook
  (make-gliver-hook '%xkb-config-keymap-failed-hook 3)
  "Invoked when created river_xkb_keymap_v1 via river_xkb_config_v1 failed.
This may be error in configuration like incorrect language.
Parameters: (DATA PROXY ERROR-MSG)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY: Pointer to river_xkb_keymap_v1 proxy.
  - ERROR-MSG: String of the error message.")

(define-var %xkb-config-keyboard-removed
  (make-gliver-hook '%xkb-config-keyboard-removed 2)
  "Invoked when river_xkb_keyboard_v1 is removed from compositor.
Parameters: (DATA PROXY)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY: Pointer to river_xkb_keyboard_v1 proxy.")

(define-var %xkb-keyboard-input-device-hook
  (make-gliver-hook '%xkb-keyboard-input-device-hook 3)
  "Invoked when river_xkb_keyboard_v1 is associated with river_input_device_v1.
Parameters: (DATA PROXY-KEYBOARD PROXY-DEVICE)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-KEYBOARD: Pointer to river_xkb_keyboard_v1 proxy.
  - PROXY-DEVICE: Pointer to river_input_device_v1 proxy.")

(define-var %xkb-keyboard-layout-changed-hook
  (make-gliver-hook '%xkb-keyboard-layout-changed-hook 4)
  "Invoked when active layout on river_xkb_keyboard_v1 changes.
Parameters: (DATA PROXY-KEYBOARD INDEX NAME)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-KEYBOARD: Pointer to river_xkb_keyboard_v1 proxy.
  - INDEX: Numeric 0-based layout index.
  - NAME: String layout name or #f.")

(define-var %xkb-keyboard-capslock-enabled-hook
  (make-gliver-hook '%xkb-keyboard-capslock-enabled-hook 2)
  "Invoked when capslock is enabled on river_xkb_keyboard_v1.
Parameters: (DATA PROXY-KEYBOARD)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-KEYBOARD: Pointer to river_xkb_keyboard_v1 proxy.")

(define-var %xkb-keyboard-capslock-disabled-hook
  (make-gliver-hook '%xkb-keyboard-capslock-disabled-hook 2)
  "Invoked when capslock is disabled on river_xkb_keyboard_v1.
Parameters: (DATA PROXY-KEYBOARD)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-KEYBOARD: Pointer to river_xkb_keyboard_v1 proxy.")

(define-var %xkb-keyboard-numlock-enabled-hook
  (make-gliver-hook '%xkb-keyboard-numlock-enabled-hook 2)
  "Invoked when numlock is enabled on river_xkb_keyboard_v1.
Parameters: (DATA PROXY-KEYBOARD)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-KEYBOARD: Pointer to river_xkb_keyboard_v1 proxy.")

(define-var %xkb-keyboard-numlock-disabled-hook
  (make-gliver-hook '%xkb-keyboard-numlock-disabled-hook 2)
  "Invoked when numlock is disabled on river_xkb_keyboard_v1.
Parameters: (DATA PROXY-KEYBOARD)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-KEYBOARD: Pointer to river_xkb_keyboard_v1 proxy.")

(define-var %input-manager-finished-hook
  (make-gliver-hook '%input-manager-finished-hook 2)
  "Invoked when river compositor has finished sending events for river_input_manager_v1.
Parameters: (DATA PROXY-MANAGER)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-MANAGER: Pointer to river_input_manager_v1 proxy.")

(define-var %input-manager-device-created-hook
  (make-gliver-hook '%input-manager-device-created-hook 3)
  "Invoked when a new river_input_device_v1 is advertised.
Parameters: (DATA PROXY-MANAGER PROXY-DEVICE)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-MANAGER: Pointer to river_input_manager_v1 proxy.
  - PROXY-DEVICE: Pointer to river_input_device_v1 proxy.")

(define-var %input-device-removed-hook
  (make-gliver-hook '%input-device-removed-hook 2)
  "Invoked when river_input_device_v1 is removed from compositor.
Parameters: (DATA PROXY-DEVICE)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-DEVICE: Pointer to river_input_device_v1 proxy.")

(define-var %input-device-type-hook
  (make-gliver-hook '%input-device-type-hook 3)
  "Invoked when river_input_device_v1 type is advertised.
Parameters: (DATA PROXY-DEVICE TYPE)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-DEVICE: Pointer to river_input_device_v1 proxy.
  - TYPE: Numeric type enum (0=keyboard, 1=pointer, 2=touch, 3=tablet).")

(define-var %input-device-name-hook
  (make-gliver-hook '%input-device-name-hook 3)
  "Invoked when river_input_device_v1 name is advertised.
Parameters: (DATA PROXY-DEVICE NAME)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-DEVICE: Pointer to river_input_device_v1 proxy.
  - NAME: String device name.")

(define-var %wlr-layer-surface-configure-hook
  (make-gliver-hook '%wlr-layer-surface-configure-hook 5)
  "Invoked when zwlr_layer_surface_v1 is configured by compositor.
Parameters: (DATA PROXY-LAYER-SURFACE SERIAL WIDTH HEIGHT)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-LAYER-SURFACE: Pointer to zwlr_layer_surface_v1 proxy.
  - SERIAL: Serial number from compositor.
  - WIDTH: Allocated width in pixels.
  - HEIGHT: Allocated height in pixels.")

(define-var %wlr-layer-surface-closed-hook
  (make-gliver-hook '%wlr-layer-surface-closed-hook 2)
  "Invoked when zwlr_layer_surface_v1 is closed by compositor.
Parameters: (DATA PROXY-LAYER-SURFACE)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-LAYER-SURFACE: Pointer to zwlr_layer_surface_v1 proxy.")
