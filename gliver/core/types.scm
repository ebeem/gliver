;;; gliver/core/types.scm --- Core data model for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver core types)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (gliver core keybindings)
  #:use-module (system foreign)
  #:export (
			manager-wl-proxy-set!
			manager-wl-proxy
			manager-tag-next-set!
			manager-tag-next
			manager-workspace-number-next-set!
			manager-workspace-number-next
			manager-output-number-next-set!
			manager-output-number-next
			manager-container-number-next-set!
			manager-container-number-next
			manager-window-number-next-set!
			manager-window-number-next
			manager-running-set!
			manager-running?
			manager-container-outer-gap-set!
			manager-container-outer-gap
			manager-container-gap-set!
			manager-container-gap
			manager-border-color-urgent-set!
			manager-border-color-urgent
			manager-border-color-unfocused-set!
			manager-border-color-unfocused
			manager-border-color-focused-set!
			manager-border-color-focused
			manager-border-width-set!
			manager-border-width
			manager-mode-set!
			manager-mode
			manager-message-timeout-set!
			manager-message-timeout
			manager-prefix-timeout-set!
			manager-prefix-timeout
			manager-prefix-key-set!
			manager-prefix-key
			manager-windows-set!
			manager-windows
			manager-seats-set!
			manager-seats
			manager-output-previous-set!
			manager-output-previous
			manager-output-current-set!
			manager-output-current
			manager-outputs-set!
			manager-outputs
			manager-state?
			%make-manager-state
			manager-window-number-next!
			manager-workspace-number-next!
			manager-output-number-next!
			manager-tag-next!
			container-number-next!
			*manager*
			output-wl-proxy-set!
			output-wl-proxy
			output-wl-output-set!
			output-wl-output
			output-workspace-previous-set!
			output-workspace-previous
			output-workspace-current-set!
			output-workspace-current
			output-workspaces-set!
			output-workspaces
			output-height-set!
			output-height
			output-width-set!
			output-width
			output-y-set!
			output-y
			output-x-set!
			output-x
			output-name-set!
			output-name
			output-id-set!
			output-id
			output?
			%make-output
			make-output
			seat-wl-proxy-set!
			seat-wl-proxy
			seat-wl-seat-set!
			seat-wl-seat
			seat-window-focused-set!
			seat-window-focused
			seat-window-entered-set!
			seat-window-entered
			seat-name-set!
			seat-name
			seat?
			%make-seat
			make-seat
			workspace-container-previous-set!
			workspace-container-previous
			workspace-container-current-set!
			workspace-container-current
			workspace-layout-set!
			workspace-layout
			workspace-output-set!
			workspace-output
			workspace-containers-set!
			workspace-containers
			workspace-tag-mask
			workspace-id
			workspace-name-set!
			workspace-name
			workspace?
			%make-workspace
			make-workspace
			container-height-set!
			container-height
			container-width-set!
			container-width
			container-y-set!
			container-y
			container-x-set!
			container-x
			container-number-set!
			container-number
			container-window-previous-set!
			container-window-previous
			container-window-current-set!
			container-window-current
			container-windows-set!
			container-windows
			container-workspace-set!
			container-workspace
			container?
			%make-container
			make-container
			window-wl-pending-set!
			window-wl-pending
			window-wl-decoration-below-proxy-set!
			window-wl-decoration-below-proxy
			window-wl-decoration-above-proxy-set!
			window-wl-decoration-above-proxy
			window-wl-node-proxy-set!
			window-wl-node-proxy
			window-wl-proxy-set!
			window-wl-proxy
			window-identifier-set!
			window-identifier
			window-presentation-hint-set!
			window-presentation-hint
			window-parent-set!
			window-parent
			window-pid-set!
			window-pid
			window-visbile-set!
			window-visible?
			window-maximized-set!
			window-maximized?
			window-fullscreen-set!
			window-fullscreen?
			window-capabilities-set!
			window-capabilities
			window-is-resizing-set!
			window-is-resizing
			window-decoration-below-set!
			window-decoration-below
			window-decoration-above-set!
			window-decoration-above
			window-decoration-hint-set!
			window-decoration-hint
			window-width-set!
			window-width
			window-width-max-set!
			window-width-max
			window-width-min-set!
			window-width-min
			window-height-set!
			window-height
			window-height-max-set!
			window-height-max
			window-height-min-set!
			window-height-min
			window-user-props-set!
			window-user-props
			window-urgent-set!
			window-urgent?
			window-marked-set!
			window-marked?
			window-transient-set!
			window-transient?
			window-floating-set!
			window-floating?
			window-container-set!
			window-container
			window-instance-set!
			window-instance
			window-class-set!
			window-class
			window-app-id-set!
			window-app-id
			window-title-set!
			window-title
			window-id-set!
			window-id
			window?
			%make-window
			make-window
			output-find-by-proxy
			output-find-by-name
			output-find-by-id
			output-current
			seat-find-by-proxy
			workspace-find-by-name
			workspace-windows
			workspace-windows-visible
			workspace-current
			container-find-by-number
			container-current
			window-find-by-proxy
			window-find-by-id
			window-current
			window-workspace
			window-output
))

;;; display (global state)
(define-record-type <manager-state>
  (%make-manager-state outputs output-current output-previous
                       seats windows prefix-key prefix-timeout
                       message-timeout mode
                       border-width border-color-focused
                       border-color-unfocused border-color-urgent
                       container-gap container-outer-gap running?
                       window-number-next container-number-next output-number-next
					   workspace-number-next next-tag-bit wl-proxy)
  manager-state?
  (outputs                manager-outputs                manager-outputs-set!)
  (output-current         manager-output-current         manager-output-current-set!)
  (output-previous        manager-output-previous        manager-output-previous-set!)
  (seats                  manager-seats                  manager-seats-set!)
  (windows                manager-windows                manager-windows-set!)
  (prefix-key             manager-prefix-key             manager-prefix-key-set!)
  (prefix-timeout         manager-prefix-timeout         manager-prefix-timeout-set!)
  (message-timeout        manager-message-timeout        manager-message-timeout-set!)
  (mode                   manager-mode                   manager-mode-set!)
  (border-width           manager-border-width           manager-border-width-set!)
  (border-color-focused   manager-border-color-focused   manager-border-color-focused-set!)
  (border-color-unfocused manager-border-color-unfocused manager-border-color-unfocused-set!)
  (border-color-urgent    manager-border-color-urgent    manager-border-color-urgent-set!)
  (container-gap          manager-container-gap          manager-container-gap-set!)
  (container-outer-gap    manager-container-outer-gap    manager-container-outer-gap-set!)
  (running?               manager-running?               manager-running-set!)
  (window-number-next     manager-window-number-next     manager-window-number-next-set!)
  (container-number-next  manager-container-number-next  manager-container-number-next-set!)
  (output-number-next     manager-output-number-next     manager-output-number-next-set!)
  (workspace-number-next  manager-workspace-number-next  manager-workspace-number-next-set!)
  (next-tag-bit           manager-tag-next               manager-tag-next-set!)
  (wl-proxy               manager-wl-proxy               manager-wl-proxy-set!))

(define (manager-window-number-next!)
  (let ((id (manager-window-number-next *manager*)))
    (manager-window-number-next-set! *manager* (1+ id))
    id))

(define (manager-workspace-number-next!)
  (let ((id (manager-workspace-number-next *manager*)))
    (manager-workspace-number-next-set! *manager* (1+ id))
    id))

(define (manager-output-number-next!)
  (let ((id (manager-output-number-next *manager*)))
    (manager-output-number-next-set! *manager* (1+ id))
    id))

(define (manager-tag-next!)
  (let ((bit (manager-tag-next *manager*)))
    (manager-tag-next-set! *manager* (ash bit 1))
    bit))

(define (container-number-next!)
  (let ((n (manager-container-number-next *manager*)))
    (manager-container-number-next-set! *manager* (1+ n))
    n))

(define *manager*
  (%make-manager-state
   '()   ; outputs
   #f    ; output-current
   #f    ; output-previous
   '()   ; seats
   '()   ; windows
   (make-gliver-key '(Control) 't) ; prefix-key
   1000  ; prefix-timeout ms
   5     ; message-timeout seconds
   'normal ; mode
   2     ; border-width
   "#5588ff"  ; border-color-focused
   "#333333"  ; border-color-unfocused
   "#ff5555"  ; border-color-urgent
   0     ; container-gap
   0     ; container-outer-gap
   #f    ; running?
   0     ; window-number-next
   0     ; container-number-next
   0     ; output-number-next
   0     ; workspace-number-next
   1     ; next-tag-bit
   #f))  ;; wl-proxy

;;; output: similar to an emacs container and stumpwm screen head
;;; a single logical screen/monitor, treated by wayland as output
(define-record-type <output>
  (%make-output id name x y width height workspaces workspace-current
                workspace-previous wl-proxy wl-output)
  output?
  (id                 output-id                 output-id-set!)
  (name               output-name               output-name-set!)
  (x                  output-x                  output-x-set!)
  (y                  output-y                  output-y-set!)
  (width              output-width              output-width-set!)
  (height             output-height             output-height-set!)
  (workspaces         output-workspaces         output-workspaces-set!)
  (workspace-current  output-workspace-current  output-workspace-current-set!)
  (workspace-previous output-workspace-previous output-workspace-previous-set!)
  (wl-output          output-wl-output          output-wl-output-set!)
  (wl-proxy           output-wl-proxy           output-wl-proxy-set!))

(set-record-type-printer! <output>
  (lambda (out port)
    (format port "#<output ~s ~ax~a+~a+~a (~a workspaces)>"
            (output-name out)
            (output-width out) (output-height out)
            (output-x out) (output-y out)
            (length (output-workspaces out)))))

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
  (window-entered     seat-window-entered     seat-window-entered-set!)
  (window-focused     seat-window-focused     seat-window-focused-set!)
  (wl-seat            seat-wl-seat            seat-wl-seat-set!)
  (wl-proxy           seat-wl-proxy           seat-wl-proxy-set!))

(set-record-type-printer! <seat>
  (lambda (s port)
    (format port "#<seat ~a (~a window)>"
            (seat-name s)
            (seat-window-focused s))))

(define* (make-seat #:key (name #f) (wl-proxy #f) (wl-seat #f)
					(window-entered #f) (window-focused #f))
  (%make-seat name window-entered window-focused wl-proxy wl-seat))

;;; Workspace: similar to an emacs group/isolation concept and stumpwm group
;;; A collection of containers and their associated windows (workspace/virtual desktop)
(define-record-type <workspace>
  (%make-workspace name id tag-mask containers output
				   layout container-current container-previous)
  workspace?
  (name                 workspace-name                 workspace-name-set!)
  (id                   workspace-id)
  (tag-mask             workspace-tag-mask)
  (containers           workspace-containers           workspace-containers-set!)
  (output               workspace-output               workspace-output-set!)
  (layout               workspace-layout               workspace-layout-set!)
  (container-current    workspace-container-current    workspace-container-current-set!)
  (container-previous   workspace-container-previous   workspace-container-previous-set!))

(set-record-type-printer! <workspace>
  (lambda (g port)
    (format port "#<workspace ~a ~s tag=~a layout=~a>"
            (workspace-id g) (workspace-name g)
            (workspace-tag-mask g) (workspace-layout g))))

(define* (make-workspace #:key
						 (name "workspace")
						 (id (manager-workspace-number-next!))
                         (tag-mask (manager-tag-next!))
                         (containers '())
                         (output #f)
                         (layout 'tiled))
  (%make-workspace name id tag-mask containers
                   output layout #f #f))

;;; container: similar to an emacs window and stumpwm container
;;; a physical container or "slot" on the screen where a list of windows is displayed
(define-record-type <container>
  (%make-container workspace windows window-current window-previous number
                   x y width height)
  container?
  (workspace       container-workspace       container-workspace-set!)
  (windows         container-windows         container-windows-set!)
  (window-current  container-window-current  container-window-current-set!)
  (window-previous container-window-previous container-window-previous-set!)
  (number          container-number          container-number-set!)
  (x               container-x               container-x-set!)
  (y               container-y               container-y-set!)
  (width           container-width           container-width-set!)
  (height          container-height          container-height-set!))

(set-record-type-printer! <container>
  (lambda (f port)
    (format port "#<container ~a ~ax~a+~a+~a (~a wins)>"
            (container-number f)
            (container-width f) (container-height f)
            (container-x f) (container-y f)
            (length (container-windows f)))))

(define* (make-container workspace #:key (x 0) (y 0) (width 0) (height 0))
  "Create a new flat container."
  (%make-container workspace '() #f #f (container-number-next!)
                   x y width height))

;;; window: similar to an emacs buffer and stumpwm window
;;; a single application (like a terminal, a browser, or an editor)
(define-record-type <window>
  (%make-window id title app-id class instance container
				floating? transient? marked? urgent? user-props
                height-min height-max height width-min width-max width
                decoration-hint decoration-above decoration-below
                is-resizing capabilities fullscreen? maximized? visible?
                pid parent presentation-hint identifier
                wl-proxy wl-node-proxy wl-decoration-above wl-decoration-below wl-pending)
  window?
  (id               window-id               window-id-set!)
  (title            window-title            window-title-set!)
  (app-id           window-app-id           window-app-id-set!)
  (class            window-class            window-class-set!)
  (instance         window-instance         window-instance-set!)
  (container        window-container        window-container-set!)
  (floating?        window-floating?        window-floating-set!)
  (transient?       window-transient?       window-transient-set!)
  (marked?          window-marked?          window-marked-set!)
  (urgent?          window-urgent?          window-urgent-set!)
  (user-props       window-user-props       window-user-props-set!)
  (height-min       window-height-min       window-height-min-set!)
  (height-max       window-height-max       window-height-max-set!)
  (height           window-height           window-height-set!)
  (width-min        window-width-min        window-width-min-set!)
  (width-max        window-width-max        window-width-max-set!)
  (width            window-width            window-width-set!)
  (decoration-hint  window-decoration-hint  window-decoration-hint-set!)
  (decoration-above window-decoration-above window-decoration-above-set!)
  (decoration-below window-decoration-below window-decoration-below-set!)  
  (is-resizing      window-is-resizing      window-is-resizing-set!)
  (capabilities     window-capabilities     window-capabilities-set!)
  (fullscreen?      window-fullscreen?      window-fullscreen-set!)
  (maximized?       window-maximized?       window-maximized-set!)
  (visible?         window-visible?         window-visbile-set!)
  (pid              window-pid              window-pid-set!)
  (parent           window-parent           window-parent-set!)
  (presentation-hint  window-presentation-hint  window-presentation-hint-set!)
  (identifier       window-identifier       window-identifier-set!)
  (wl-proxy         window-wl-proxy         window-wl-proxy-set!)
  (wl-node-proxy    window-wl-node-proxy    window-wl-node-proxy-set!)
  (wl-decoration-above    window-wl-decoration-above-proxy    window-wl-decoration-above-proxy-set!)
  (wl-decoration-below    window-wl-decoration-below-proxy    window-wl-decoration-below-proxy-set!)
  (wl-pending       window-wl-pending       window-wl-pending-set!))

(set-record-type-printer! <window>
  (lambda (window port)
    (format port "#<window ~a ~s app-id=~s>"
            (window-id window)
            (window-title window)
            (window-app-id window))))

(define* (make-window #:key
                      (id (manager-window-number-next!))
                      (title "") (app-id "") (class "")
                      (instance "") (container #f)
                      (floating? #f) (transient? #f)
                      (marked? #f) (urgent? #f) (user-props '())
                      (height-min #f) (height-max #f) (height #f)
                      (width-min #f) (width-max #f) (width #f)
                      (decoration-hint #f) (decoration-above #f)
                      (decoration-below #f) (is-resizing #f)
                      (capabilities '()) (fullscreen? #f) (maximized? #f)
                      (visible? #t) (pid #f) (parent #f) (presentation-hint #f)
                      (identifier #f) (wl-proxy #f) (wl-node-proxy #f)
                      (wl-decoration-above #f) (wl-decoration-below #f)
                      (wl-pending #f))

  (%make-window id title app-id class instance container
                floating? transient? marked? urgent? user-props
                height-min height-max height width-min width-max width
                decoration-hint decoration-above decoration-below
                is-resizing capabilities fullscreen? maximized?
                visible? pid parent presentation-hint identifier
                wl-proxy wl-node-proxy wl-decoration-above wl-decoration-below wl-pending))

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

(define (workspace-find-by-name name)
  "Find a workspace by name on the current output."
  (find (lambda (g) (string=? (workspace-name g) name))
        (output-workspaces (output-current))))

(define (workspace-windows workspace)
  "Return all windows in @var{workspace}."
  (apply append 
         (map container-windows
              (workspace-containers workspace))))

(define (workspace-windows-visible workspace)
  "Return the currently visible (not hidden/minimized) windows in @var{workspace}."
  (filter window-visible? (workspace-windows workspace)))

(define (workspace-current)
  "Return the current active workspace"
  (and (output-current)
       (output-workspace-current (output-current))))

(define (container-find-by-number n workspace)
  "Find container with number N in WORKSPACE."
  (find (lambda (f) (= (container-number f) n))
        (workspace-containers workspace)))

(define (container-current)
  "Return the current focused container based on the active workspace."
  (let ((g (workspace-current)))
    (and g (workspace-container-current g))))

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
  (let ((f (container-current)))
    (and f (container-window-current f))))

(define (window-workspace window)
  "Return the window workspace."
  (container-workspace (window-container window)))

(define (window-output window)
  "Return the window output."
  (workspace-output (window-workspace window)))

