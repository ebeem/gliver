;;; gliver/contrib/ui/container/container-tab-indicator.scm --- Container tab indicator
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module manages declarative gleui-based tab indicator bars rendered on
;;; top or bottom edges of gliver containers.
;;; It handles:
;;;  - Updating container usable area for visible indicator bars
;;;  - Rendering per-window tabs with active, unfocused, inactive styling
;;;  - Handling pointer clicks and wheel scroll events
;;;
;;; Usage:
;;;   (use-modules (gliver contrib ui container container-tab-indicator))
;;;   (container-tab-indicator-enable!)

(define-module (gliver contrib ui container container-tab-indicator)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-9)
  #:use-module (system foreign)
  #:use-module (gliver core)
  #:use-module (gliver wayland gen wayland)
  #:use-module (gliver river connector)
  #:use-module (gliver river window-manager)
  #:use-module (gliver river wm-shell-surface-manager)
  #:use-module (gliver river wm-node-manager)
  #:use-module (gliver contrib ui gleui base)
  #:use-module (gliver contrib ui gleui widgets)
  #:declarative? #f
  #:export (
            *%container-tab-indicator-enabled*
            *container-tab-indicator-height*
            *container-tab-indicator-show-single-window?*
            *container-tab-indicator-show-title?*
            *container-tab-indicator-active-bg-color*
            *container-tab-indicator-active-text-color*
            *container-tab-indicator-inactive-bg-color*
            *container-tab-indicator-inactive-text-color*
            *container-tab-indicator-unfocused-active-bg-color*
            *container-tab-indicator-unfocused-active-text-color*
            *container-tab-indicator-urgent-bg-color*
            *container-tab-indicator-urgent-text-color*
            *container-tab-indicator-border-color*
            *container-tab-indicator-font*
            *container-tab-indicator-font-size*
            *container-tab-indicator-position*
            *container-tab-indicator-gap*
            *container-tab-indicator-radius*
            *container-tab-indicator-border-width*
            *container-tab-indicator-padding-x*
            *container-tab-indicator-clickable?*
            *container-tab-indicator-scrollable?*
            *container-tab-indicator-scroll-direction*
            *container-tab-indicator-last-pointer-x*
            *container-tab-indicator-last-pointer-y*
            <container-tab-indicator-state>
            make-container-tab-indicator-state
            container-tab-indicator-state?
            *container-tab-indicator-table*
            container-tab-indicator-state-get
            container-tab-indicator-state-get-create!
            container-tab-indicator-wl-surface
            container-tab-indicator-wl-shell-surface
            container-tab-indicator-wl-node
            container-tab-indicator-wl-buffer
            container-tab-indicator-find-by-shell-surface
            container-tab-indicator-find-by-surface
            container-tab-indicator-build-widget
            container-tab-indicator-update-input-region!
            container-indicator-signature
            container-tab-indicator-seat-setup!
            container-tab-indicator-init!
            container-tab-indicator-should-show?
            container-tab-indicator-update-usable-area!
            container-tab-indicator-cleanup!
            container-tab-indicator-hide!
            container-tab-indicator-render!
            container-tab-indicators-update-all!
            container-tab-indicator-on-container-created
            container-tab-indicator-on-container-destroy
            container-tab-indicator-on-container-focused
            container-tab-indicator-on-container-unfocused
            container-tab-indicator-on-container-resize
            container-tab-indicator-on-workspace-switch
            container-tab-indicator-on-output-removed
            container-tab-indicator-on-window-focused
            container-tab-indicator-on-window-unfocused
            container-tab-indicator-on-window-title-changed
            container-tab-indicator-on-window-app-id-changed
            container-tab-indicator-on-window-urgent
            container-tab-indicator-on-window-container-added
            container-tab-indicator-on-window-container-removed
            container-tab-indicator-on-window-container-moved
            container-tab-indicator-on-window-destroyed
            container-tab-indicator-on-pointer-position
            container-tab-indicator-on-shell-interaction
            container-tab-indicator-switch-window!
            container-tab-indicator-on-scroll
            container-tab-indicator-on-seat-object-id-changed
            container-tab-indicator-on-globals-unbind
            container-tab-indicator-on-handshake-complete
            container-tab-indicator-on-listeners-attach
            container-tab-indicator-on-window-fullscreen-entered
            container-tab-indicator-on-window-fullscreen-exited
            container-tab-indicator-on-surface-scale-changed
            *container-tab-indicator-hooks*
            container-tab-indicator-enable!
            container-tab-indicator-disable!
))

;;; configuration variables
(define-var *%container-tab-indicator-enabled* #f
            "Internal state indicating whether container window indicator is enabled.")
(define-var *container-tab-indicator-height* 22
            "Height of the container window tab indicator in pixels.")
(define-var *container-tab-indicator-show-single-window?* #t
            "Whether to display the tab indicator when the container has only 1 window.")
(define-var *container-tab-indicator-show-title?* #t
            "Whether to display the title of the window inside each tab.")

(define-var *container-tab-indicator-active-bg-color* #:inherit *theme-border-color*
            "Background color for the active tab in focused containers.")
(define-var *container-tab-indicator-active-text-color* #:inherit *theme-base*
            "Text color for the active window's tab title.")
(define-var *container-tab-indicator-inactive-bg-color* #:inherit *theme-mantle*
            "Background color for inactive window tabs.")
(define-var *container-tab-indicator-inactive-text-color* #:inherit *theme-text*
            "Text color for inactive windows' tab titles.")
(define-var *container-tab-indicator-unfocused-active-bg-color* #:inherit *theme-mantle*
            "Background color for the active tab in an unfocused container.")
(define-var *container-tab-indicator-unfocused-active-text-color* #:inherit *theme-text*
            "Text color for the active tab title in an unfocused container.")
(define-var *container-tab-indicator-urgent-bg-color* #:inherit *theme-urgent-color*
            "Background color for tabs representing urgent windows.")
(define-var *container-tab-indicator-urgent-text-color* #:inherit *theme-crust*
            "Text color for urgent tab titles.")
(define-var *container-tab-indicator-border-color* #:inherit *theme-border-color*
            "Border stroke color for each tab.")

(define-var *container-tab-indicator-font* #:inherit *theme-font*
            "Font family string used for tab title labels.")
(define-var *container-tab-indicator-font-size* 12
            "Font size in points for tab title labels.")
(define-var *container-tab-indicator-position* 'top
            "Position of the indicator bar relative to container ('top or 'bottom).")
(define-var *container-tab-indicator-gap* 0
            "Spacing in pixels between adjacent tabs.")
(define-var *container-tab-indicator-radius* 0
            "Corner radius in pixels for each tab.")
(define-var *container-tab-indicator-border-width* 0
            "Border stroke width in pixels for each tab.")
(define-var *container-tab-indicator-padding-x* 0
            "Horizontal padding inside each tab in pixels.")
(define-var *container-tab-indicator-clickable?* #t
            "Whether clicking on a tab focuses that window.")
(define-var *container-tab-indicator-scrollable?* #t
            "Whether scrolling on the tab indicator switches windows.")
(define-var *container-tab-indicator-scroll-direction* 'up-prev
            "Scroll direction for switching windows: 'up-prev or 'up-next.")

(define-var *container-tab-indicator-last-pointer-x* #f
            "Last tracked compositor pointer X coordinate for WM shell click hit-testing.")
(define-var *container-tab-indicator-last-pointer-y* #f
            "Last tracked compositor pointer Y coordinate for WM shell click hit-testing.")

;; per-container tab indicator state record
;;  - wl-surface: pointer to wl_surface or #f, surface for the indicator bar
;;  - wl-shell-surface: pointer to river_shell_surface_v1 or #f, shell surface of surface
;;  - wl-node: pointer to river_node_v1 or #f, scene node for order placement
;;  - wl-buffer: pointer to wl_buffer or #f, active Wayland buffer currently committed
;;  - shm-slots: pair (slot . #f) of <gleui-shm-slot> record or #f
;;  - shm-active-idx: integer (0 or 1) currently displayed SHM slot index
;;  - hit-boxes: list of <gleui-hit-box> records
;;  - width: integer cached width in pixels
;;  - height: integer cached height in pixels
;;  - signature: list or #f, structural snapshot used to skip redundant SHM redraws
(define-record-type <container-tab-indicator-state>
  (make-container-tab-indicator-state wl-surface wl-shell-surface wl-node wl-buffer
                                      shm-slots shm-active-idx hit-boxes width height signature)
  container-tab-indicator-state?
  (wl-surface       %cwi-wl-surface       %cwi-wl-surface-set!)
  (wl-shell-surface %cwi-wl-shell-surface %cwi-wl-shell-surface-set!)
  (wl-node          %cwi-wl-node          %cwi-wl-node-set!)
  (wl-buffer        %cwi-wl-buffer        %cwi-wl-buffer-set!)
  (shm-slots        %cwi-shm-slots        %cwi-shm-slots-set!)
  (shm-active-idx   %cwi-shm-active-idx   %cwi-shm-active-idx-set!)
  (hit-boxes        %cwi-hit-boxes        %cwi-hit-boxes-set!)
  (width            %cwi-width            %cwi-width-set!)
  (height           %cwi-height           %cwi-height-set!)
  (signature        %cwi-signature        %cwi-signature-set!))

(define-var *container-tab-indicator-table* (make-hash-table)
            "Hash table mapping <container> records to <container-tab-indicator-state>.")

;; the below functions are safe getters to container tab indicator state properties
(define (container-tab-indicator-state-get container)
  "Look up the `<container-tab-indicator-state>' record bound to CONTAINER.
Parameters: (CONTAINER)
  - CONTAINER: <container> record to inspect.
Returns `<container-tab-indicator-state>' record, or #f if none exists."
  (and (container? container)
       (hashq-ref *container-tab-indicator-table* container)))

(define (container-tab-indicator-state-get-create! container)
  "Return the `<container-tab-indicator-state>' for CONTAINER, allocate new if unavailable.
Parameters: (CONTAINER)
  - CONTAINER: <container> record owning the tab indicator state.
Returns `<container-tab-indicator-state>' record, or #f if CONTAINER is not a container."
  (and (container? container)
       (or (container-tab-indicator-state-get container)
           (let ((state (make-container-tab-indicator-state #f #f #f #f #f 0 '() 0 0 #f)))
             (hashq-set! *container-tab-indicator-table* container state)
             state))))

(define (container-tab-indicator-wl-surface container)
  "Return the Wayland wl_surface foreign pointer for CONTAINER's tab indicator.
Parameters: (CONTAINER)
  - CONTAINER: <container> record.
Returns wl_surface foreign pointer, or #f if uninitialized."
  (and-let* ((state (container-tab-indicator-state-get container)))
    (%cwi-wl-surface state)))

(define (container-tab-indicator-wl-shell-surface container)
  "Return the shell surface foreign pointer for CONTAINER's tab indicator.
Parameters: (CONTAINER)
  - CONTAINER: <container> record.
Returns river_shell_surface_v1 foreign pointer, or #f if uninitialized."
  (and-let* ((state (container-tab-indicator-state-get container)))
    (%cwi-wl-shell-surface state)))

(define (container-tab-indicator-wl-node container)
  "Return the river scene node foreign pointer for CONTAINER's tab indicator.
Parameters: (CONTAINER)
  - CONTAINER: <container> record.
Returns river_node_v1 foreign pointer, or #f if uninitialized."
  (and-let* ((state (container-tab-indicator-state-get container)))
    (%cwi-wl-node state)))

(define (container-tab-indicator-wl-buffer container)
  "Return the active Wayland wl_buffer foreign pointer for CONTAINER's tab indicator.
Parameters: (CONTAINER)
  - CONTAINER: <container> record.
Returns wl_buffer foreign pointer, or #f if none is committed."
  (and-let* ((state (container-tab-indicator-state-get container)))
    (%cwi-wl-buffer state)))

(define (%find-by-state-pointer ptr getter)
  "Search `*container-tab-indicator-table*' for the container whose
state GETTER field matches PTR.
Parameters: (PTR GETTER)
  - PTR: Pointer to Wayland foreign object to match by address.
  - GETTER: Procedure field accessor returning a foreign pointer from state.
Returns matching `<container>' record, or #f if not found."
  (and (valid-pointer? ptr)
       (let ((target-address (pointer-address ptr))
             (matched        #f))
         (hash-for-each
          (lambda (container state)
            (let ((candidate-ptr (getter state)))
              (when (and (valid-pointer? candidate-ptr)
                         (= (pointer-address candidate-ptr) target-address))
                (set! matched container))))
          *container-tab-indicator-table*)
         matched)))

(define (container-tab-indicator-find-by-shell-surface shell-surf)
  "Find the container whose tab indicator owns shell surface SHELL-SURF.
Parameters: (SHELL-SURF)
  - SHELL-SURF: Pointer to river_shell_surface_v1 proxy.
Returns matching `<container>' record, or #f if not found."
  (%find-by-state-pointer shell-surf %cwi-wl-shell-surface))

(define (container-tab-indicator-find-by-surface surface)
  "Find the container whose tab indicator owns wl_surface SURFACE.
Parameters: (SURFACE)
  - SURFACE: Pointer to Wayland wl_surface.
Returns matching `<container>' record, or #f if not found."
  (%find-by-state-pointer surface %cwi-wl-surface))

;;; declarative gleui tab indicator widget
(define (%window-title-text window)
  "Return the non-empty title string for WINDOW, falling back to a formatted window ID.
Parameters: (WINDOW)
  - WINDOW: <window> record to format.
Returns display title string."
  (let ((title (window-title window)))
    (if (and (string? title) (not (string-null? title)))
        title
        (format #f "Window ~a" (window-id window)))))

(define (%tab-colors container window active-window)
  "Resolve tab background and foreground hex colors for WINDOW in CONTAINER.
Parameters: (CONTAINER WINDOW ACTIVE-WINDOW)
  - CONTAINER: <container> record parent of WINDOW.
  - WINDOW: <window> record represented by the tab.
  - ACTIVE-WINDOW: <window> record or #f currently active in CONTAINER.
Returns two values: BG-COLOR and FG-COLOR hex strings."
  (let ((container-focused-flag? (container-focused? container))
        (active?                 (eq? window active-window))
        (urgent?                 (and (window? window)
                                      (window-urgent? window))))
    (cond
     (urgent?
      (values *container-tab-indicator-urgent-bg-color*
              *container-tab-indicator-urgent-text-color*))
     (active?
      (if container-focused-flag?
          (values *container-tab-indicator-active-bg-color*
                  *container-tab-indicator-active-text-color*)
          ;; container active on other output, so not really focused
          (values *container-tab-indicator-unfocused-active-bg-color*
                  *container-tab-indicator-unfocused-active-text-color*)))
     (else
      (values *container-tab-indicator-inactive-bg-color*
              *container-tab-indicator-inactive-text-color*)))))

(define (%build-tab-widget container window active-window)
  "Build the declarative widget for a single WINDOW tab in CONTAINER.
Parameters: (CONTAINER WINDOW ACTIVE-WINDOW)
  - CONTAINER: <container> record owning the tab indicator.
  - WINDOW: <window> record represented by the tab.
  - ACTIVE-WINDOW: <window> record or #f currently active in CONTAINER.
Returns `<gleui-widget>' expanded node."
  (call-with-values (lambda () (%tab-colors container window active-window))
    (lambda (bg-color fg-color)
      ;; if show-title is disabled, gleui will receive #f as child
      ;; it will convert it into an empty widget
      (let ((label-widget (and *container-tab-indicator-show-title?*
                               (gleui-w-text (%window-title-text window)
                                             #:font *container-tab-indicator-font*
                                             #:font-size *container-tab-indicator-font-size*
                                             #:color fg-color
                                             #:ellipsize 'end
                                             #:text-align 'center
                                             #:markup? #f))))
        (gleui-w-expanded
         (gleui-w-clickable
          (gleui-w-box label-widget
                       #:expand-x? #t
                       #:expand-y? #t
                       #:padding-x *container-tab-indicator-padding-x*
                       #:radius *container-tab-indicator-radius*
                       #:bg-color bg-color
                       #:border-color *container-tab-indicator-border-color*
                       #:border-width *container-tab-indicator-border-width*
                       #:inset-border? #f
                       #:align 'center)
          #:data window
          #:on-scroll (lambda (_axis delta _data)
                        (container-tab-indicator-on-scroll container delta))))))))

(define (container-tab-indicator-build-widget container width height windows active-window)
  "Build the declarative tab widget for WINDOWS in CONTAINER.
Parameters: (CONTAINER WIDTH HEIGHT WINDOWS ACTIVE-WINDOW)
  - CONTAINER: <container> record owning the tab indicator.
  - WIDTH: Integer strip width in pixels.
  - HEIGHT: Integer strip height in pixels.
  - WINDOWS: List of `<window>' records inside CONTAINER in display order.
  - ACTIVE-WINDOW: <window> record or #f currently active.
Returns `<gleui-widget>' box node."
  (let* ((tab-gap     (max 0 (inexact->exact (round *container-tab-indicator-gap*))))
         (tab-widgets (map (lambda (window)
                             (%build-tab-widget container window active-window))
                           windows)))
    (gleui-w-box
     ;; add all window tab widgets in a single row
     (gleui-w-row tab-widgets #:gap tab-gap #:cross-align 'center)
     #:width width
     #:height height)))

;;; usable area management
(define (container-tab-indicator-update-input-region! surface width height)
  "Set the Wayland input region of indicator SURFACE.
Parameters: (SURFACE WIDTH HEIGHT)
  - SURFACE: Pointer to Wayland wl_surface.
  - WIDTH: Integer indicator strip width in pixels.
  - HEIGHT: Integer indicator strip height in pixels."
  (when (and (valid-pointer? surface)
             (> width 0)
             (> height 0))
    (let ((region (wl-compositor-create-region *wl-compositor*)))
      (wl-region-add region 0 0 width height)
      (wl-surface-set-input-region surface region)
      (wl-region-destroy region))))

(define* (container-indicator-signature container #:optional (scale 1.0))
  "Build a snapshot list for CONTAINER to detect unchanged frames.
We can later compare signature and avoid unnecessary re-renders.
Parameters: (CONTAINER [SCALE 1.0])
  - CONTAINER: <container> record being rendered.
  - SCALE: Float optional fractional surface scale factor.
Returns list capturing visual configuration, geometry, focus, and tab titles."
  (let* ((windows       (container-windows container))
         (active-window (or (container-window-current container)
                            (and (pair? windows) (car windows)))))
    (list scale
          (container-width container)
          *container-tab-indicator-height*
          (container-focused? container)
          *container-tab-indicator-urgent-bg-color*
          *container-tab-indicator-active-text-color*
          *container-tab-indicator-inactive-text-color*
          *container-tab-indicator-unfocused-active-text-color*
          *container-tab-indicator-urgent-text-color*
          *container-tab-indicator-border-color*
          *container-tab-indicator-active-bg-color*
          *container-tab-indicator-inactive-bg-color*
          *container-tab-indicator-unfocused-active-bg-color*
          *container-tab-indicator-show-title?*
          *container-tab-indicator-show-single-window?*
          *container-tab-indicator-gap*
          *container-tab-indicator-radius*
          *container-tab-indicator-border-width*
          *container-tab-indicator-padding-x*
          *container-tab-indicator-font*
          *container-tab-indicator-font-size*
          (map (lambda (window)
                 (list (window-id window)
                       (eq? window active-window)
                       (window-urgent? window)
                       (window-title window)
                       (window-app-id window)))
               windows))))

(define (container-tab-indicator-seat-setup!)
  "Ensure shared Wayland pointer listeners are bound when tab indicators are enabled.
Parameters: None."
  (when *%container-tab-indicator-enabled*
    (gleui-pointer-setup!)))

(define (%container-tab-indicator-live? container)
  "Check whether CONTAINER is still attached to an active workspace.
Parameters: (CONTAINER)
  - CONTAINER: <container> record candidate.
Returns #t if CONTAINER is live, #f if destroyed or detached."
  (and-let* (((container? container))
             (workspace (container-workspace container))
             ((workspace? workspace))
             ((memq container (workspace-containers workspace))))
    (let ((output (workspace-output workspace)))
      (or (not (output? output))
          (and (memq workspace (output-workspaces output)) #t)))))

(define (container-tab-indicator-init! container)
  "Allocate the Wayland surface, shell surface, and scene node for CONTAINER's indicator.
Parameters: (CONTAINER)
  - CONTAINER: <container> record to initialize."
  (when (and *%container-tab-indicator-enabled*
             (%container-tab-indicator-live? container)
             (valid-pointer? *wl-compositor*)
             (valid-pointer? (manager-wl-proxy *manager*)))
    ;; skip if shell surface is already configured
    (unless (valid-pointer? (container-tab-indicator-wl-surface container))
      ;; ugly, but we can't do and-let here because we have to cleanup
      ;; all created objects in case of a failure
      (let* ((manager    (manager-wl-proxy *manager*))
             (surface    (wl-compositor-create-surface *wl-compositor*))
             (shell-surf (and (valid-pointer? surface)
                              (wm-manager-shell-surface-get manager surface)))
             (node       (and (valid-pointer? shell-surf)
                              (wm-shell-surface-node-get! shell-surf)))
             (output     (or (and=> (container-workspace container) workspace-output)
                             (output-current))))
        (if (and (valid-pointer? surface)
                 (valid-pointer? shell-surf)
                 (valid-pointer? node))
            ;; we should only create the indicator if all surfaces are allocated
            (let ((state (container-tab-indicator-state-get-create! container)))
              (surface-scale-create surface output)
              (%cwi-wl-surface-set! state surface)
              (%cwi-wl-shell-surface-set! state shell-surf)
              (%cwi-wl-node-set! state node)
              ;; expose tab hit-boxes to pointer dispatch for scroll/click tracking
              (gleui-pointer-register-surface!
               surface
               (lambda ()
                 (if *%container-tab-indicator-enabled*
                     (%cwi-hit-boxes state)
                     '())))
              (log-debug "container-tab-indicator: initialized indicator surface for container ~a"
                         (container-id container)))
            ;; if something goes wrong, cleanup all objects created
            (begin
              (when (valid-pointer? node)
                (wm-node-destroy! node))
              (when (valid-pointer? shell-surf)
                (wm-shell-surface-destroy! shell-surf))
              (when (valid-pointer? surface)
                (wl-surface-destroy surface))))))))

(define (container-tab-indicator-should-show? container)
  "Check whether the tab indicator bar should be visible on CONTAINER.
This depends on a configuration variable which hides it if only one
window exist in the container, in addition to the fullscreen mode.
Parameters: (CONTAINER)
  - CONTAINER: <container> record to test.
Returns #t if indicator should render, #f if hidden or fullscreened."
  (and *%container-tab-indicator-enabled*
       (container? container)
       (let* ((windows         (container-windows container))
              (window-count    (length windows))
              (has-fullscreen? (any window-fullscreen? windows)))
         (and (not has-fullscreen?)
              (> window-count 0)
              (or (> window-count 1)
                  *container-tab-indicator-show-single-window?*)))))

(define (container-tab-indicator-update-usable-area! container)
  "Reserve CONTAINER's usable area for its indicator strip.
This is important because otherwise windows content will show up
behind the tab indicator.
Parameters: (CONTAINER)
  - CONTAINER: <container> record whose usable area is updated."
  (when (container? container)
    (let* ((outer-x       (container-x container))
           (outer-y       (container-y container))
           (outer-width   (container-width container))
           (outer-height  (container-height container))
           (show?         (container-tab-indicator-should-show? container))
           (strip-height  (if show?
                              (min outer-height (max 0 *container-tab-indicator-height*))
                              0))
           ;; shift usable-y down only when bar is at top, bottom shrink height
           (usable-y      (if (and show? (eq? *container-tab-indicator-position* 'top))
                              (+ outer-y strip-height)
                              outer-y))
           (usable-height (if show?
                              (max 0 (- outer-height strip-height))
                              outer-height)))
      (unless (and (= (container-usable-x container) outer-x)
                   (= (container-usable-y container) usable-y)
                   (= (container-usable-width container) outer-width)
                   (= (container-usable-height container) usable-height))
        (container-usable-area-set! container outer-x usable-y outer-width usable-height)
        (container-usable-area-update-windows! container)))))

(define (container-tab-indicator-cleanup! container)
  "Destroy the Wayland surface, shell surface, and node for CONTAINER's indicator.
Parameters: (CONTAINER)
  - CONTAINER: <container> record whose indicator resources are cleaned up."
  (when (container? container)
    (container-tab-indicator-update-usable-area! container)
    (and-let* ((state (container-tab-indicator-state-get container)))
      (let ((surface    (%cwi-wl-surface state))
            (shell-surf (%cwi-wl-shell-surface state))
            (node       (%cwi-wl-node state)))
        (hashq-remove! *container-tab-indicator-table* container)
        (gleui-pointer-unregister-surface! surface)
        (%cwi-wl-node-set! state #f)
        (%cwi-wl-shell-surface-set! state #f)
        (%cwi-wl-surface-set! state #f)
        (%cwi-wl-buffer-set! state #f)
        (%cwi-hit-boxes-set! state '())
        (gleui-shm-slots-destroy! (%cwi-shm-slots state))
        (%cwi-shm-slots-set! state #f)
        (when (valid-pointer? node)
          (wm-node-destroy! node))
        (when (valid-pointer? shell-surf)
          (wm-shell-surface-destroy! shell-surf))
        (when (valid-pointer? surface)
          (surface-scale-destroy surface)
          (wl-surface-destroy surface))))))

(define (container-tab-indicator-hide! container)
  "Hide CONTAINER's tab indicator node.
Parameters: (CONTAINER)
  - CONTAINER: <container> record to hide."
  (when (and *%container-tab-indicator-enabled*
             (%container-tab-indicator-live? container))
    (container-tab-indicator-update-usable-area! container)
    (when (container-tab-indicator-state-get container)
      (with-render-sequence
       #:dirty? #t
       (when (and *%container-tab-indicator-enabled*
                  (%container-tab-indicator-live? container))
         (and-let* ((cur-state (container-tab-indicator-state-get container)))
           (let ((node       (%cwi-wl-node cur-state))
                 (surface    (%cwi-wl-surface cur-state))
                 (shell-surf (%cwi-wl-shell-surface cur-state)))
             (when (valid-pointer? node)
               ;; TODO: there must be a better approach, currently
               ;;       all hidden indicators go to far away coordinates
               (wm-node-position-set! node -10000 -10000))
             (when (and (valid-pointer? surface)
                        (valid-pointer? shell-surf))
               (wm-shell-surface-sync-next-commit! shell-surf)
               (wl-surface-attach surface %null-pointer 0 0)
               (wl-surface-commit surface))
             (%cwi-wl-buffer-set! cur-state #f)
             (%cwi-hit-boxes-set! cur-state '())
             (gleui-shm-slots-destroy! (%cwi-shm-slots cur-state))
             (%cwi-shm-slots-set! cur-state #f))))))))

(define (%container-tab-indicator-visible-rect container)
  "Compute the clipped (x y w h clipped?) of CONTAINER's tab indicator
within output usable bounds, or #f if completely offscreen.
NOTE: the purpose is to clip indicator so it doesn't expand into another output
      or statusbar since they are logically close to each other and that could happen.
Parameters: (CONTAINER)
  - CONTAINER: <container> record whose indicator bounds are checked.
Returns list (x y w h clipped?) or #f when completely outside the output."
  (let* ((raw-width  (container-width container))
         (raw-height (min (container-height container)
                          (max 0 (inexact->exact (round *container-tab-indicator-height*)))))
         (raw-left   (container-x container))
         (raw-top    (if (eq? *container-tab-indicator-position* 'bottom)
                         (+ (container-y container) (- (container-height container) raw-height))
                         (container-y container)))
         (raw-right  (+ raw-left raw-width))
         (raw-bottom (+ raw-top raw-height))
         (output     (and=> (container-workspace container) workspace-output)))
    (if (not (output? output))
        ;; fallback when container workspace has no output attached (shouldn't happen)
        (and (> raw-width 0) (> raw-height 0)
             (list raw-left raw-top raw-width raw-height #f))
        (let* ((usable-left   (output-usable-x output))
               (usable-top    (output-usable-y output))
               (usable-right  (+ usable-left (output-usable-width output)))
               (usable-bottom (+ usable-top (output-usable-height output)))
               ;; intersect the unclipped indicator with the output's usable area
               (vis-left      (max raw-left usable-left))
               (vis-top       (max raw-top usable-top))
               (vis-right     (min raw-right usable-right))
               (vis-bottom    (min raw-bottom usable-bottom))
               (vis-width     (- vis-right vis-left))
               (vis-height    (- vis-bottom vis-top)))
          (and (> vis-width 0)
               (> vis-height 0)
               (let ((clipped? (or (> vis-left raw-left)
                                   (> vis-top raw-top)
                                   (< vis-right raw-right)
                                   (< vis-bottom raw-bottom))))
                 (list vis-left vis-top vis-width vis-height clipped?)))))))

(define* (container-tab-indicator-render! container #:key (force #f))
  "Render and commit the tab indicator for CONTAINER.
Parameters: (CONTAINER [#:FORCE #f])
  - CONTAINER: <container> record to update.
  - FORCE: Boolean flag to force redraw SHM buffers even if cached."
  (when (and *%container-tab-indicator-enabled*
             (%container-tab-indicator-live? container))
    ;; usable area must be updated regardless of workspace visibility
    (container-tab-indicator-update-usable-area! container)
    (let ((rect (%container-tab-indicator-visible-rect container)))
      ;; we render to both hide and show the indicator, hide if
      ;; fullscreen mode, or outside output bounds
      (if (or (not (container-tab-indicator-should-show? container))
              (not rect))
          (container-tab-indicator-hide! container)
          ;; otherwise, show it when workspace is visible
          (when (workspace-visible? (container-workspace container))
            (with-render-sequence
             #:dirty? #t
             ;; with-render-sequence may defer until river's render callback, so
             ;; re-validate visibility and recompute clipped geometry
             (and-let* ((*%container-tab-indicator-enabled*)
                        ((%container-tab-indicator-live? container))
                        ((container-tab-indicator-should-show? container))
                        (workspace (container-workspace container))
                        ((workspace-visible? workspace))
                        (cur-rect (%container-tab-indicator-visible-rect container)))

               ;; initialize indicator surface if it's not already
               (unless (valid-pointer? (container-tab-indicator-wl-surface container))
                 (container-tab-indicator-init! container))
               (and-let* ((state      (container-tab-indicator-state-get container))
                          (surface    (%cwi-wl-surface state))
                          (shell-surf (%cwi-wl-shell-surface state))
                          (node       (%cwi-wl-node state))
                          ((valid-pointer? surface))
                          ((valid-pointer? shell-surf))
                          ((valid-pointer? node)))

                 (let* ((windows        (container-windows container))
                        (x              (list-ref cur-rect 0))
                        (y              (list-ref cur-rect 1))
                        (width          (list-ref cur-rect 2))
                        (height         (list-ref cur-rect 3))
                        (clipped?       (list-ref cur-rect 4))
                        (active-window  (or (container-window-current container)
                                            (and (pair? windows) (car windows))))
                        (scale          (surface-scale surface 1.0))
                        (signature      (container-indicator-signature container scale))
                        (current-buffer (%cwi-wl-buffer state))
                        ;; skip redraw when dimensions and visual signature match
                        ;; clipped strips bypass reuse since visible tab geometry may differ
                        (reusable?      (and (not force)
                                             (not clipped?)
                                             (valid-pointer? current-buffer)
                                             (= (%cwi-width state) width)
                                             (= (%cwi-height state) height)
                                             (equal? (%cwi-signature state) signature))))

                   (when (and (> width 0) (> height 0))
                     ;; keep indicator node above container windows and mark it clickable
                     (wm-node-position-set! node x y)
                     (wm-node-place-top! node)
                     (container-tab-indicator-update-input-region! surface width height)
                     (if reusable?
                         ;; skip building new widget if reusable, just commit
                         (begin
                           (wm-shell-surface-sync-next-commit! shell-surf)
                           (gleui-layer-surface-buffer-commit! surface current-buffer width height))
                         ;; otherwise, build a new tab bar widget
                         (let ((widget (container-tab-indicator-build-widget
                                        container width height windows active-window)))
                           (call-with-values
                               (lambda ()
                                 ;; transient SHM unmaps CPU memory right after commit since tabs rarely change
                                 (gleui-widget-render-commit!
                                  surface (%cwi-shm-slots state) (%cwi-shm-active-idx state)
                                  width height widget
                                  #:persistent? #f
                                  #:shm-name (format #f "gliver-container-tab-~a-shm"
                                                     (container-id container))
                                  #:pre-commit (lambda ()
                                                 (wm-shell-surface-sync-next-commit! shell-surf))))
                             (lambda (updated-slots next-slot-index wl-buffer hit-boxes)
                               (%cwi-shm-slots-set! state updated-slots)
                               (%cwi-shm-active-idx-set! state next-slot-index)
                               (%cwi-hit-boxes-set! state hit-boxes)
                               (when (valid-pointer? wl-buffer)
                                 (%cwi-wl-buffer-set! state wl-buffer)
                                 (%cwi-width-set! state width)
                                 (%cwi-height-set! state height)
                                 (%cwi-signature-set! state signature))))))))))))))))

(define (container-tab-indicators-update-all!)
  "Initialize and re-render tab indicators for all containers on all outputs.
Parameters: None."
  (when (and *%container-tab-indicator-enabled*
             (valid-pointer? (manager-wl-proxy *manager*)))
    (for-each
     (lambda (output)
       (for-each
        (lambda (workspace)
          (for-each
           (lambda (container)
             (unless (valid-pointer? (container-tab-indicator-wl-surface container))
               (container-tab-indicator-init! container))
             (container-tab-indicator-render! container))
           (workspace-containers workspace)))
        (output-workspaces output)))
     (manager-outputs *manager*))))

;; event hooks
(define (container-tab-indicator-on-container-created container)
  "Invoked when CONTAINER is created.
Initialize and render the tab indicator for CONTAINER.
Parameters: (CONTAINER)
  - CONTAINER: <container> record newly created."
  (when (container? container)
    (container-tab-indicator-init! container)
    (container-tab-indicator-render! container)))

(define (container-tab-indicator-on-container-destroy container workspace)
  "Invoked when CONTAINER on WORKSPACE is destroyed.
Clean up tab indicator resources allocated for CONTAINER.
Parameters: (CONTAINER WORKSPACE)
  - CONTAINER: <container> record being destroyed.
  - WORKSPACE: <workspace> record that owned CONTAINER (unused)."
  (when (container? container)
    (container-tab-indicator-cleanup! container)))

(define (container-tab-indicator-on-container-focused container)
  "Invoked when CONTAINER is focused.
Re-render CONTAINER's indicator and refresh indicators on active workspaces.
Parameters: (CONTAINER)
  - CONTAINER: <container> record newly focused."
  (when (container? container)
    (container-tab-indicator-render! container)
    ;; refresh sibling containers so previously active container drops focused styling
    (for-each
     (lambda (output)
       (and-let* ((workspace (output-workspace-current output)))
         (for-each
          (lambda (other-container)
            (when (and (container? other-container)
                       (not (eq? other-container container)))
              (container-tab-indicator-render! other-container)))
          (workspace-containers workspace))))
     (manager-outputs *manager*))))

(define (container-tab-indicator-on-container-unfocused container)
  "Invoked when CONTAINER loses focus.
Re-render CONTAINER's tab indicator so it reflects unfocused colors.
Parameters: (CONTAINER)
  - CONTAINER: <container> record that lost focus."
  (when (container? container)
    (container-tab-indicator-render! container)))

(define (container-tab-indicator-on-container-resize container)
  "Invoked when CONTAINER geometry or position changes.
Reposition and re-render CONTAINER's tab indicator to match its updated bounds.
Parameters: (CONTAINER)
  - CONTAINER: <container> record resized or moved."
  (when (container? container)
    (container-tab-indicator-render! container)))

(define (container-tab-indicator-on-workspace-switch workspace prev-workspace)
  "Invoked when active workspace changes.
Hide tab indicators on PREV-WORKSPACE and render indicators on active WORKSPACE.
Parameters: (WORKSPACE PREV-WORKSPACE)
  - WORKSPACE: <workspace> record newly active, or #f.
  - PREV-WORKSPACE: <workspace> record previously active, or #f."
  (when (and (workspace? prev-workspace)
             (not (eq? prev-workspace workspace)))
    (for-each container-tab-indicator-hide! (workspace-containers prev-workspace)))
  (when (workspace? workspace)
    (for-each container-tab-indicator-render! (workspace-containers workspace))))

(define (container-tab-indicator-on-output-removed output)
  "Invoked when an OUTPUT is removed.
Hide tab indicator nodes on every workspace belonging to removed OUTPUT.
We shouldn't destroy as the workspace is still available and OUTPUT may be
added again or another output take over.
Parameters: (OUTPUT)
  - OUTPUT: <output> record being removed."
  (when (output? output)
    (for-each
     (lambda (workspace)
       (for-each container-tab-indicator-hide! (workspace-containers workspace)))
     (output-workspaces output))))

(define (container-tab-indicator-on-window-focused window)
  "Invoked when WINDOW gains focus.
Update its parent container indicator and ensure the indicator node stays on top.
Parameters: (WINDOW)
  - WINDOW: <window> record that gained focus."
  (and-let* (((window? window))
             (container (window-container window))
             ((container? container)))
    (container-tab-indicator-render! container)
    (and-let* ((node (container-tab-indicator-wl-node container))
               ((valid-pointer? node)))
      (with-render-sequence
       #:dirty? #t
       (wm-node-place-top! node)))))

(define (%render-window-container window)
  "Re-render the tab indicator of WINDOW's parent container when present.
Parameters: (WINDOW)
  - WINDOW: <window> record whose container indicator should be updated."
  (and-let* (((window? window))
             (container (window-container window))
             ((container? container)))
    (container-tab-indicator-render! container)))

(define (container-tab-indicator-on-window-unfocused window)
  "Invoked when WINDOW loses focus.
Parameters: (WINDOW)
  - WINDOW: <window> record that lost focus."
  (%render-window-container window))

(define (container-tab-indicator-on-window-title-changed window title)
  "Invoked when WINDOW title changes.
Re-render its parent container's tab indicator.
Parameters: (WINDOW TITLE)
  - WINDOW: <window> record whose title changed.
  - TITLE: String new window title (unused)."
  (%render-window-container window))

(define (container-tab-indicator-on-window-app-id-changed window app-id)
  "Invoked when WINDOW application ID changes.
Re-render its parent container's tab indicator.
Parameters: (WINDOW APP-ID)
  - WINDOW: <window> record whose app-id changed.
  - APP-ID: String new application ID (unused)."
  (%render-window-container window))

(define (container-tab-indicator-on-window-urgent window)
  "Invoked when WINDOW urgency state changes.
Re-render its parent container's tab indicator.
Parameters: (WINDOW)
  - WINDOW: <window> record whose urgent state changed."
  (%render-window-container window))

(define (container-tab-indicator-on-window-container-added window container)
  "Invoked when WINDOW is added to CONTAINER.
Re-render CONTAINER's tab indicator to include the new tab.
Parameters: (WINDOW CONTAINER)
  - WINDOW: <window> record added (unused).
  - CONTAINER: <container> record that gained WINDOW."
  (when (container? container)
    (container-tab-indicator-render! container)))

(define (container-tab-indicator-on-window-container-removed window container)
  "Invoked when WINDOW is removed from CONTAINER.
Re-render CONTAINER's tab indicator to drop or hide the removed tab.
Parameters: (WINDOW CONTAINER)
  - WINDOW: <window> record removed (unused).
  - CONTAINER: <container> record that lost WINDOW."
  (when (container? container)
    (container-tab-indicator-render! container)))

(define (container-tab-indicator-on-window-container-moved window old-container new-container)
  "Invoked when WINDOW moves from OLD-CONTAINER to NEW-CONTAINER.
Re-render both containers' tab indicators.
Parameters: (WINDOW OLD-CONTAINER NEW-CONTAINER)
  - WINDOW: <window> record moved (unused).
  - OLD-CONTAINER: <container> record previously holding WINDOW, or #f.
  - NEW-CONTAINER: <container> record receiving WINDOW, or #f."
  (when (container? old-container)
    (container-tab-indicator-render! old-container))
  (when (container? new-container)
    (container-tab-indicator-render! new-container)))

(define (container-tab-indicator-on-window-destroyed window container workspace)
  "Invoked when WINDOW is destroyed.
Re-render CONTAINER's tab indicator when present.
Parameters: (WINDOW CONTAINER WORKSPACE)
  - WINDOW: <window> record destroyed (unused).
  - CONTAINER: <container> record that held WINDOW, or #f.
  - WORKSPACE: <workspace> record (unused)."
  (when (container? container)
    (container-tab-indicator-render! container)))

(define (container-tab-indicator-on-pointer-position seat x y)
  "Invoked when compositor pointer coordinates update.
Record X and Y for WM shell interaction click resolution.
Parameters: (SEAT X Y)
  - SEAT: <seat> record reporting pointer movement (unused).
  - X: Float pointer X coordinate.
  - Y: Float pointer Y coordinate."
  (set! *container-tab-indicator-last-pointer-x* x)
  (set! *container-tab-indicator-last-pointer-y* y))

(define (container-tab-indicator-on-shell-interaction seat proxy-shell)
  "Invoked when a tab indicator shell surface is clicked.
Focus the window tab matching clicked PROXY-SHELL using recorded hit-boxes.
Parameters: (SEAT PROXY-SHELL)
  - SEAT: <seat> record that initiated the interaction (unused).
  - PROXY-SHELL: Pointer to clicked river_shell_surface_v1 proxy."
  (when (and *container-tab-indicator-clickable?*
             (valid-pointer? proxy-shell))
    (and-let* ((container (container-tab-indicator-find-by-shell-surface proxy-shell))
               ((container? container)))
      (let* ((state        (container-tab-indicator-state-get container))
             (strip-height (min (container-height container)
                                (max 0 (inexact->exact (round *container-tab-indicator-height*)))))
             (origin-x     (container-x container))
             (origin-y     (if (eq? *container-tab-indicator-position* 'bottom)
                               (+ (container-y container) (- (container-height container) strip-height))
                               (container-y container)))
             ;; convert global compositor pointer coords to surface-local hit-box coords
             (rel-x        (max 0 (- (or *container-tab-indicator-last-pointer-x*
                                         origin-x)
                                     origin-x)))
             (rel-y        (max 0 (- (or *container-tab-indicator-last-pointer-y*
                                         origin-y)
                                     origin-y)))
             (hit          (and state (gleui-find-hit-box (%cwi-hit-boxes state) rel-x rel-y)))
             (windows      (container-windows container))
             (target       (or (and hit (gleui-hit-box-data hit))
                               (and (pair? windows) (car windows)))))
        (when (window? target)
          (window-focus! target)
          ;; I am not sure why I have to let river know that manager
          ;; became dirty, I thought it should do it with mouse/keyboard interactions
          (wm-manager-manage-dirty (manager-wl-proxy *manager*)))))))

(define (container-tab-indicator-switch-window! container direction)
  "Switch the active window in CONTAINER in DIRECTION.
Parameters: (CONTAINER DIRECTION)
  - CONTAINER: <container> record target.
  - DIRECTION: Symbol 'next or 'prev indicating tab step direction."
  (when (container? container)
    (let* ((windows      (container-windows container))
           (window-count (length windows)))
      (when (> window-count 1)
        (and-let* ((active-window (or (container-window-current container)
                                      (car windows)))
                   (active-index  (list-index (lambda (window)
                                                (eq? window active-window))
                                              windows))
                   (target-index  (modulo (if (eq? direction 'next)
                                              (1+ active-index)
                                              (1- active-index))
                                          window-count))
                   (target-window (list-ref windows target-index)))
          (unless (eq? target-window active-window)
            (log-debug "container-tab-indicator: switching container ~a window ~a -> ~a (~a)"
                       (container-id container)
                       (if (window? active-window)
                           (window-id active-window)
                           "none")
                       (window-id target-window)
                       direction)
            (window-focus! target-window)
            (wm-manager-manage-dirty (manager-wl-proxy *manager*))))))))

(define (container-tab-indicator-on-scroll container delta)
  "Invoked when pointer scroll occurs on CONTAINER's tab indicator.
Step to the next or previous window in CONTAINER according to DELTA.
Parameters: (CONTAINER DELTA)
  - CONTAINER: <container> record target.
  - DELTA: Float scroll axis delta (< 0 scrolls up/previous, > 0 scrolls down/next)."
  (when (and *container-tab-indicator-scrollable?*
             (container? container)
             (not (zero? delta)))
    (let* ((up?       (< delta 0))
           (direction (if (memq *container-tab-indicator-scroll-direction* '(up-next inverted))
                          (if up? 'next 'prev)
                          (if up? 'prev 'next))))
      (container-tab-indicator-switch-window! container direction))))

(define (container-tab-indicator-on-seat-object-id-changed seat prev-id)
  "Invoked when SEAT updates its Wayland object ID.
Ensure pointer listeners are attached to the updated seat.
Parameters: (SEAT PREV-ID)
  - SEAT: <seat> record whose object ID changed (unused).
  - PREV-ID: Integer previous Wayland object ID (unused)."
  (container-tab-indicator-seat-setup!))

(define (container-tab-indicator-on-globals-unbind)
  "Invoked when Wayland globals unbind or shut down.
Clean up all container tab indicator surfaces.
Parameters: None."
  (let ((containers (hash-map->list (lambda (container _state) container)
                                    *container-tab-indicator-table*)))
    (for-each container-tab-indicator-cleanup! containers)))

(define (container-tab-indicator-on-handshake-complete)
  "Invoked once the manager handshake completes.
This caused some bugs because it was running before initialization of
Wayland objects like output. Should probably only load user configuration
file after manage start hook, then this is not needed.
Attach seat pointer listeners and render all container tab indicators.
Parameters: None."
  (gliver-hook-remove! *manager-manage-start-hook* 'container-tab-indicator-on-handshake-complete)
  (when *%container-tab-indicator-enabled*
    (container-tab-indicator-seat-setup!)
    (container-tab-indicators-update-all!)))

(define (container-tab-indicator-on-listeners-attach)
  "Invoked when Wayland listeners attach.
Defer indicator setup until the first River manage_start handshake completes.
Parameters: None."
  (when *%container-tab-indicator-enabled*
    (gliver-hook-add! *manager-manage-start-hook* 'container-tab-indicator-on-handshake-complete)))

(define (container-tab-indicator-on-window-fullscreen-entered window prev-status)
  "Invoked when WINDOW enters fullscreen mode.
Hide the parent container's tab indicator.
Parameters: (WINDOW PREV-STATUS)
  - WINDOW: <window> record entering fullscreen.
  - PREV-STATUS: Boolean previous fullscreen status (unused)."
  (and-let* (((window? window))
             (container (window-container window))
             ((container? container)))
    (container-tab-indicator-hide! container)))

(define (container-tab-indicator-on-window-fullscreen-exited window prev-status)
  "Invoked when WINDOW exits fullscreen mode.
Restore the parent container's tab indicator.
Parameters: (WINDOW PREV-STATUS)
  - WINDOW: <window> record exiting fullscreen.
  - PREV-STATUS: Boolean previous fullscreen status (unused)."
  (%render-window-container window))

(define (container-tab-indicator-on-surface-scale-changed surface scale)
  "Invoked when the SURFACE SCALE changes.
Re-render the matching tab indicator so it respects the new SCALE.
Parameters: (SURFACE SCALE)
  - SURFACE: Pointer to Wayland wl_surface whose scale changed.
  - SCALE: Float new fractional surface scale factor (unused)."
  (hash-for-each
   (lambda (container state)
     ;; find the container that has the surface that got updated
     (when (and (container? container)
                (equal? surface (%cwi-wl-surface state)))
       (container-tab-indicator-render! container #:force #t)))
   *container-tab-indicator-table*))

(define *container-tab-indicator-hooks*
  (list (cons *container-created-hook*                 'container-tab-indicator-on-container-created)
        (cons *container-destroy-hook*                 'container-tab-indicator-on-container-destroy)
        (cons *container-focused-hook*                 'container-tab-indicator-on-container-focused)
        (cons *container-unfocused-hook*               'container-tab-indicator-on-container-unfocused)
        (cons *container-resize-hook*                  'container-tab-indicator-on-container-resize)
        (cons *container-position-changed-hook*        'container-tab-indicator-on-container-resize)
        (cons *workspace-switch-hook*                  'container-tab-indicator-on-workspace-switch)
        (cons *gliver-globals-unbind-hook*             'container-tab-indicator-on-globals-unbind)
        (cons *gliver-listeners-attach-hook*           'container-tab-indicator-on-listeners-attach)
        (cons *window-container-removed-hook*          'container-tab-indicator-on-window-container-removed)
        (cons *window-container-added-hook*            'container-tab-indicator-on-window-container-added)
        (cons *window-container-moved-hook*            'container-tab-indicator-on-window-container-moved)
        (cons *window-destroyed-hook*                  'container-tab-indicator-on-window-destroyed)
        (cons *window-focused-hook*                    'container-tab-indicator-on-window-focused)
        (cons *window-unfocused-hook*                  'container-tab-indicator-on-window-unfocused)
        (cons *window-title-changed-hook*              'container-tab-indicator-on-window-title-changed)
        (cons *window-app-id-changed-hook*             'container-tab-indicator-on-window-app-id-changed)
        (cons *seat-shell-interacted-hook*             'container-tab-indicator-on-shell-interaction)
        (cons *seat-seat-pointer-position-changed-hook* 'container-tab-indicator-on-pointer-position)
        (cons *seat-object-id-changed-hook*            'container-tab-indicator-on-seat-object-id-changed)
        (cons *window-fullscreen-entered-hook*         'container-tab-indicator-on-window-fullscreen-entered)
        (cons *window-fullscreen-exited-hook*          'container-tab-indicator-on-window-fullscreen-exited)
        (cons *output-removed-hook*                    'container-tab-indicator-on-output-removed)
        (cons *surface-scale-changed-hook*             'container-tab-indicator-on-surface-scale-changed)))

(define (container-tab-indicator-enable!)
  "Install container tab indicator hooks and render indicators across all active outputs.
Parameters: None."
  (unless *%container-tab-indicator-enabled*
    (for-each (lambda (hook-pair)
                (gliver-hook-add! (car hook-pair) (cdr hook-pair)))
              *container-tab-indicator-hooks*)
    (set! *%container-tab-indicator-enabled* #t)
    (if (and (valid-pointer? (manager-wl-proxy *manager*))
             (or *in-manage-sequence* *in-render-sequence*))
        (begin
          (container-tab-indicator-seat-setup!)
          (container-tab-indicators-update-all!))
        (gliver-hook-add! *manager-manage-start-hook* 'container-tab-indicator-on-handshake-complete))
    (log-info "container-tab-indicator: enabled")))

(define (container-tab-indicator-disable!)
  "Uninstall container tab indicator hooks and clean up all indicator surfaces.
Parameters: None."
  (when *%container-tab-indicator-enabled*
    (gliver-hook-remove! *manager-manage-start-hook* 'container-tab-indicator-on-handshake-complete)
    (for-each (lambda (hook-pair)
                (gliver-hook-remove! (car hook-pair) (cdr hook-pair)))
              *container-tab-indicator-hooks*)
    (set! *%container-tab-indicator-enabled* #f)
    (container-tab-indicator-on-globals-unbind)
    (log-info "container-tab-indicator: disabled")))
