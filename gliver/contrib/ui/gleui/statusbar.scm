;;; gliver/contrib/ui/gleui/statusbar.scm --- Waybar Inspired Statusbar for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module manages per-output wlr-layer-shell statusbar surfaces, module
;;; polling ticks, pointer setup, and interactive bar commands.
;;; It handles:
;;;  - Creating and resizing statusbar surfaces and usable areas
;;;  - Polling interval modules and dispatching hook updates
;;;
;;; Usage:
;;;   (use-modules (gliver contrib ui gleui statusbar))
;;;   (statusbar-enable!)

(define-module (gliver contrib ui gleui statusbar)
  #:use-module (system foreign)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-69)
  #:use-module (gliver core)
  #:use-module (gliver river connector)
  #:use-module (gliver river wlr-layer-shell)
  #:use-module (gliver contrib ui gleui base)
  #:use-module (gliver contrib ui gleui statusbar state)
  #:use-module (gliver contrib ui gleui statusbar worker)
  #:use-module (gliver contrib ui gleui statusbar render)
  #:use-module (gliver contrib ui gleui statusbar modules workspaces)
  #:use-module (gliver contrib ui gleui statusbar modules window)
  #:use-module (gliver contrib ui gleui statusbar modules cpu)
  #:use-module (gliver contrib ui gleui statusbar modules ram)
  #:use-module (gliver contrib ui gleui statusbar modules date)
  #:use-module (gliver contrib ui gleui statusbar modules weather)
  #:use-module (gliver contrib ui gleui statusbar modules battery)
  #:use-module (gliver contrib ui gleui statusbar modules disk)
  #:use-module (gliver contrib ui gleui statusbar modules network)
  #:use-module (gliver contrib ui gleui statusbar modules cpu-temp)
  #:use-module (gliver contrib ui gleui statusbar modules keyboard)
  #:use-module (gliver contrib ui gleui statusbar modules net-speed)
  #:use-module (gliver contrib ui gleui statusbar modules mpd)
  #:use-module (gliver contrib ui gleui statusbar modules volume)
  #:use-module (gliver contrib ui gleui statusbar modules custom)
  #:declarative? #f
  #:export (statusbar-seat-setup!
            statusbar-seat-cleanup!
            statusbar-init-output!
            statusbar-cleanup-output!
            statusbar-cleanup-all!
            statusbar-update-all-modules!
            statusbar-on-tick
            statusbar-render-output!
            statusbar-render-all!
            statusbar-attach-active-module-hooks!
            statusbar-update-all!
            statusbar-on-output-created
            statusbar-on-output-id-changed
            statusbar-on-output-dimensions
            statusbar-on-surface-scale-changed
            statusbar-on-output-statusbar-changed
            statusbar-enable!
            statusbar-disable!
            statusbar-toggle!
            statusbar-reload!
            statusbar-assign-all!))

(re-export-modules (gliver contrib ui gleui statusbar state)
                   (gliver contrib ui gleui statusbar worker)
                   (gliver contrib ui gleui statusbar render)
                   (gliver contrib ui gleui statusbar modules workspaces)
                   (gliver contrib ui gleui statusbar modules window)
                   (gliver contrib ui gleui statusbar modules cpu)
                   (gliver contrib ui gleui statusbar modules ram)
                   (gliver contrib ui gleui statusbar modules date)
                   (gliver contrib ui gleui statusbar modules weather)
                   (gliver contrib ui gleui statusbar modules battery)
                   (gliver contrib ui gleui statusbar modules disk)
                   (gliver contrib ui gleui statusbar modules network)
                   (gliver contrib ui gleui statusbar modules cpu-temp)
                   (gliver contrib ui gleui statusbar modules keyboard)
                   (gliver contrib ui gleui statusbar modules net-speed)
                   (gliver contrib ui gleui statusbar modules mpd)
                   (gliver contrib ui gleui statusbar modules volume)
                   (gliver contrib ui gleui statusbar modules custom))

;;; pointer helpers delegating to gleui pointer manager
(define (statusbar-seat-setup!)
  "Ensure shared Wayland pointer listeners are active when statusbar clicks are enabled.
Parameters: None."
  (when *statusbar-click-enabled*
    (gleui-pointer-setup!)))

(define (statusbar-seat-cleanup!)
  "Unregister all active statusbar surfaces from shared pointer dispatch.
Parameters: None."
  (for-each
   (lambda (bar)
     (gleui-pointer-unregister-surface! (statusbar-surface bar)))
   (hash-table-values *statusbar-output-table*)))

;;; output usable area layer
(define (%calculate-anchor position)
  "Convert bar POSITION symbol ('top or 'bottom) into wlr-layer-shell anchor flags.
Parameters: (POSITION)
  - POSITION: Symbol bar placement edge ('top or 'bottom).
Returns integer anchor bitmask."
  ;; NOTE: it only supports top and bottom right now
  ;; this is anchor value, so we have to include left and right
  (if (eq? position 'bottom)
      (+ *wlr-layer-shell-anchor-bottom*
         *wlr-layer-shell-anchor-left*
         *wlr-layer-shell-anchor-right*)
      (+ *wlr-layer-shell-anchor-top*
         *wlr-layer-shell-anchor-left*
         *wlr-layer-shell-anchor-right*)))

(define (%update-output-usable-area! output bar-position total-height skip-unchanged?)
  "Update usable screen area on OUTPUT, reserve TOTAL-HEIGHT along edge BAR-POSITION.
Parameters: (OUTPUT BAR-POSITION TOTAL-HEIGHT SKIP-UNCHANGED?)
  - OUTPUT: <output> record whose usable area is updated.
  - BAR-POSITION: Symbol indicating bar position ('top or 'bottom).
  - TOTAL-HEIGHT: Integer reserved height including margins in pixels.
  - SKIP-UNCHANGED?: Boolean; when #t, omit updates and hooks if usable bounds did not change."
  ;; NOTE: usable area is updated to make containers and windows aware
  ;;       that statusbar area shouldn't be used by them
  (let* ((current-x         (output-x output))
         (current-y         (output-y output))
         (current-width     (output-width output))
         (current-height    (output-height output))
         (new-usable-y      (+ current-y (if (eq? bar-position 'bottom)
                                             0
                                             total-height)))
         (new-usable-height (max 0 (- current-height total-height))))
    ;; avoid re-rendering when usable area didn't change
    (unless (and skip-unchanged?
                 (= new-usable-height (output-usable-height output))
                 (= new-usable-y (output-usable-y output)))
      (output-usable-area-set! output
                               current-x
                               new-usable-y
                               current-width
                               new-usable-height)
      (gliver-hook-run! *output-dimensions-changed-hook*
                        output
                        current-width
                        current-height))))

(define* (statusbar-init-output! output #:optional statusbar)
  "Initialize the layer-shell statusbar surface on OUTPUT.
Parameters: (OUTPUT [STATUSBAR])
  - OUTPUT: <output> record to attach the statusbar to.
  - STATUSBAR: <statusbar> record or #f (defaulting to OUTPUT's bound bar)."
  (let ((bar (or statusbar
                 (output-statusbar output))))
    (when (and bar
               (statusbar? bar)
               *statusbar-enabled*
               (valid-pointer? *wl-compositor*)
               (valid-pointer? *wlr-layer-shell*)
               (valid-pointer? *wl-shm*)
               (not (hash-table-ref/default *statusbar-output-table* (output-id output) #f)))
      (let ((wl-output-proxy (output-wl-output-proxy output)))
        (when (valid-pointer? wl-output-proxy)
          (log-info "statusbar: initializing statusbar on output ~a" (output-name output))
          (let* ((bar-position  (statusbar-position bar))
                 (bar-height    (statusbar-height bar))
                 (margin-top    (statusbar-margin-top bar))
                 (margin-bottom (statusbar-margin-bottom bar))
                 (margin-left   (statusbar-margin-left bar))
                 (margin-right  (statusbar-margin-right bar))
                 (total-height  (+ bar-height margin-top margin-bottom))
                 (anchor        (%calculate-anchor bar-position))
                 (initial-width (max 100 (- (output-width output)
                                            margin-left
                                            margin-right)))
                 (bound-bar     (statusbar-bind-output! bar output initial-width bar-height)))
            (%update-output-usable-area! output bar-position total-height #f)

            (gleui-layer-surface-create
             #:output output
             #:namespace "gliver-statusbar"
             #:layer *wlr-layer-shell-layer-top*
             #:anchor anchor
             #:margin-top margin-top
             #:margin-right margin-right
             #:margin-bottom margin-bottom
             #:margin-left margin-left
             #:exclusive-zone total-height
             #:keyboard-interactivity *wlr-layer-shell-interactivity-none*
             #:width 0
             #:height bar-height
             #:roundtrip? #f
             #:on-created (lambda (surface layer-surface)
                            (statusbar-surface-set! bound-bar surface)
                            (statusbar-layer-surface-set! bound-bar layer-surface)
                            (when *statusbar-click-enabled*
                              (gleui-pointer-setup!))
                            (gleui-pointer-register-surface!
                             surface
                             (lambda ()
                               (if (and *statusbar-click-enabled*
                                        (statusbar-click-enabled? bound-bar))
                                   (statusbar-hit-boxes bound-bar)
                                   '()))
                             #:redraw-proc (lambda () (statusbar-render-output! output))))
             #:on-configure (lambda (width height)
                              (let ((resolved-width  (if (> width 0) width initial-width))
                                    (resolved-height (if (> height 0) height bar-height)))
                                (statusbar-rendered-width-set! bound-bar resolved-width)
                                (statusbar-rendered-height-set! bound-bar resolved-height)
                                (statusbar-configured?-set! bound-bar #t)
                                (statusbar-update-all-modules! output)
                                (statusbar-render-and-commit! output bound-bar)))
             #:on-close (lambda ()
                          (log-info "statusbar: layer surface closed for output ~a" (output-name output))
                          (statusbar-cleanup-output! output)))))))))

(define (statusbar-cleanup-output! output)
  "Destroy the statusbar layer surface, SHM slots, and restore usable-area.
Parameters: (OUTPUT)
  - OUTPUT: <output> record or #f to clean up."
  (when (and output (output? output))
    (let ((bar (hash-table-ref/default *statusbar-output-table* (output-id output) #f)))
      (when bar
        (hash-table-delete! *statusbar-output-table* (output-id output))
        (let ((layer-surface (statusbar-layer-surface bar))
              (surface       (statusbar-surface bar))
              (slots         (statusbar-shm-slots bar)))
          (gleui-pointer-unregister-surface! surface)
          (gleui-shm-slots-destroy! slots)
          (statusbar-unbind-output! bar)
          (gleui-layer-surface-destroy! surface layer-surface))
        ;; restore usable area to full output dimensions if output is still available
        (when (memq output (manager-outputs *manager*))
          (output-usable-area-set! output (output-x output) (output-y output)
                                   (output-width output) (output-height output))
          (gliver-hook-run! *output-dimensions-changed-hook* output
                            (output-width output) (output-height output)))))))

(define (statusbar-cleanup-all!)
  "Clean up all statusbar surfaces, hooks, and worker thread.
Parameters: None."
  (set! *statusbar-enabled* #f)
  (statusbar-detach-module-hooks!)
  (statusbar-worker-stop!)
  (for-each
   (lambda (bar)
     (statusbar-cleanup-output! (statusbar-output bar)))
   (hash-table-values *statusbar-output-table*)))

;;; module updates & periodic tick
(define (statusbar-update-all-modules! output)
  "Execute all update function for all modules on the worker thread.
Parameters: (OUTPUT)
  - OUTPUT: <output> record passed to each module update handler."
  (and-let* ((bar (output-statusbar output))
             ((statusbar? bar)))
    (for-each (lambda (module)
                (statusbar-module-update! module output))
              (statusbar-bar-modules bar))))

(define (%tick-update-output! output now)
  "Schedule update for due statusbar modules on OUTPUT.
Parameters: (OUTPUT NOW)
  - OUTPUT: <output> record to check.
  - NOW: Integer current POSIX epoch time in seconds."
  (let ((bar (hash-table-ref/default *statusbar-output-table*
                                     (output-id output) #f)))
    (when (and bar
               (statusbar-configured? bar)
               (statusbar? bar))
      (for-each
       (lambda (module)
         ;; for each module, check its interval and last poll
         ;; if the difference between time now and last poll
         ;; is >= interval, then update the module
         (let ((interval (statusbar-module-interval module)))
           (when (and (number? interval)
                      (> interval 0)
                      (>= (- now (statusbar-module-last-poll module)) interval)
                      (not (statusbar-module-queued-or-running? module)))
             (statusbar-module-update! module output))))
       (statusbar-bar-modules bar)))))

(define (statusbar-on-tick)
  "Invoked on each manager tick.
Flush pending worker-driven renders on main thread and
schedule due interval modules.
Parameters: None."
  (when *statusbar-enabled*
    ;; consume undirty will return the current dirty flag
    ;; it will also mark undirty, so we will re-render only when dirty
    (when (statusbar-consume-mark-undirty!)
      (statusbar-render-all!))
    (let ((now (current-time)))
      (when (> now *statusbar-last-tick-time*)
        (set! *statusbar-last-tick-time* now)
        (for-each (lambda (output) (%tick-update-output! output now))
                  (manager-outputs *manager*))))))

(define (statusbar-render-output! output)
  "Re-render the statusbar surface on OUTPUT.
Parameters: (OUTPUT)
  - OUTPUT: <output> record or #f of the target output."
  (when (and *statusbar-enabled* output)
    (let ((bar (hash-table-ref/default *statusbar-output-table* (output-id output) #f)))
      (if (and bar (statusbar-configured? bar))
          ;; just re-render it
          (statusbar-render-and-commit! output bar)
          ;; initialize statusbar if not initialized already
          (let ((configured-bar (output-statusbar output)))
            (when (statusbar? configured-bar)
              (statusbar-init-output! output)))))))

(define (statusbar-render-all!)
  "Re-render configured statusbar surfaces in all known outputs.
Parameters: None."
  (when (and *statusbar-enabled*)
    (for-each (lambda (output)
                (let ((bar (hash-table-ref/default *statusbar-output-table* (output-id output) #f)))
                  (when (and bar (statusbar-configured? bar))
                    (statusbar-render-and-commit! output bar))))
              (manager-outputs *manager*))))

(define (statusbar-attach-active-module-hooks!)
  "Attach hooks for all active modules in statusbars.
Parameters: None."
  (statusbar-attach-module-hooks! (statusbar-active-modules)))

(define (statusbar-update-all!)
  "Attach active module hooks, force-update all modules, and re-render.
Parameters: None."
  (when (and *statusbar-enabled*)
    (statusbar-attach-active-module-hooks!)
    (for-each statusbar-update-all-modules!
              (manager-outputs *manager*))
    (statusbar-render-all!)))

;;; hooks
(define (statusbar-on-output-created output)
  "Invoked when OUTPUT is created.
Initialize a statusbar surface on OUTPUT when one is configured.
Parameters: (OUTPUT)
  - OUTPUT: <output> record newly created."
  (and-let* ((*statusbar-enabled*)
             ((output? output))
             (bar (output-statusbar output))
             ((statusbar? bar)))
    (statusbar-init-output! output)))

(define (statusbar-on-output-id-changed output prev-id)
  "Invoked when OUTPUT receives a Wayland object ID.
Initialize and render the statusbar surface on OUTPUT if not already registered.
Parameters: (OUTPUT PREV-ID)
  - OUTPUT: <output> record whose global object ID changed.
  - PREV-ID: Integer previous output identifier (unused)."
  (and-let* ((*statusbar-enabled*)
             ((output? output))
             (bar (output-statusbar output))
             ((statusbar? bar))
             ((not (hash-table-ref/default *statusbar-output-table* (output-id output) #f))))
    (statusbar-init-output! output)
    (statusbar-attach-active-module-hooks!)
    (statusbar-render-output! output)))

(define (statusbar-on-output-dimensions output prev-width prev-height)
  "Invoked when OUTPUT dimensions change.
Resize the statusbar layer surface and update OUTPUT's usable area.
Parameters: (OUTPUT PREV-WIDTH PREV-HEIGHT)
  - OUTPUT: <output> record resized.
  - PREV-WIDTH: Integer previous output width in pixels (unused).
  - PREV-HEIGHT: Integer previous output height in pixels (unused)."
  (and-let* ((*statusbar-enabled*)
             ((output? output))
             (bar (hash-table-ref/default *statusbar-output-table* (output-id output) #f))
             ((statusbar? bar))
             (layer-surface (statusbar-layer-surface bar))
             ((valid-pointer? layer-surface)))
    (let* ((bar-height    (statusbar-height bar))
           (margin-top    (statusbar-margin-top bar))
           (margin-bottom (statusbar-margin-bottom bar))
           (total-height  (+ bar-height margin-top margin-bottom))
           (bar-position  (statusbar-position bar)))
      (gleui-layer-surface-set-size! layer-surface 0 bar-height)
      (wlr-layer-surface-set-exclusive-zone layer-surface total-height)
      ;; last parameter #t will skip commit if usable area is unchanged
      (%update-output-usable-area! output bar-position total-height #t))))

(define (statusbar-on-surface-scale-changed surface scale)
  "Invoked when the SURFACE SCALE changes.
Re-render the statusbar matching SURFACE so it respects the new SCALE.
Parameters: (SURFACE SCALE)
  - SURFACE: Pointer to Wayland wl_surface whose scale changed.
  - SCALE: Float new fractional scale factor (unused)."
  (when *statusbar-enabled*
    (hash-table-walk
     *statusbar-output-table*
     (lambda (id bar)
       ;; find the statusbar owning the surface whose scale updated
       (and-let* ((bar)
                  ((equal? surface (statusbar-surface bar)))
                  (output (statusbar-output bar)))
         (statusbar-render-and-commit! output bar))))))

(define (statusbar-on-output-statusbar-changed output bar)
  "Invoked when OUTPUT's bound BAR changes.
Clean up the previous surface on OUTPUT and initialize BAR when provided.
Parameters: (OUTPUT BAR)
  - OUTPUT: <output> record whose statusbar property changed.
  - BAR: <statusbar> record newly assigned, or #f to remove."
  (when (and *statusbar-enabled*
             (output? output))
    (statusbar-cleanup-output! output)
    (when (statusbar? bar)
      (statusbar-init-output! output)
      (statusbar-attach-active-module-hooks!)
      (statusbar-render-output! output))))

(gliver-hook-add! *statusbar-render-request-hook* 'statusbar-render-all!)
(gliver-hook-add! *output-created-hook* 'statusbar-on-output-created)
(gliver-hook-add! *output-object-id-changed-hook* 'statusbar-on-output-id-changed)
(gliver-hook-add! *output-dimensions-changed-hook* 'statusbar-on-output-dimensions)
(gliver-hook-add! *surface-scale-changed-hook* 'statusbar-on-surface-scale-changed)
(gliver-hook-add! *output-removed-hook* 'statusbar-cleanup-output!)
(gliver-hook-add! *output-statusbar-changed-hook* 'statusbar-on-output-statusbar-changed)
(gliver-hook-add! *gliver-globals-unbind-hook* 'statusbar-cleanup-all!)
(gliver-hook-add! *manager-tick-hook* 'statusbar-on-tick)

;;; commands
(define-command (statusbar-enable!)
  "Enable and display the statusbar across all configured outputs.
Parameters: None."
  (var-set! *statusbar-enabled* #t)
  (log-info "statusbar: enabling")
  (when *manager*
    (for-each (lambda (output)
                (let ((bar (output-statusbar output)))
                  (when (and bar (statusbar? bar))
                    (statusbar-init-output! output))))
              (manager-outputs *manager*)))
  (statusbar-seat-setup!)
  (statusbar-attach-active-module-hooks!)
  (statusbar-update-all!))

(define-command (statusbar-disable!)
  "Disable the statusbar and tear down all output surfaces and hooks.
Parameters: None."
  (var-set! *statusbar-enabled* #f)
  (log-info "statusbar: disabling")
  (statusbar-cleanup-all!)
  (set! *statusbar-last-tick-time* 0))

(define-command (statusbar-toggle!)
  "Toggle statusbar visibility between enabled and disabled states.
Parameters: None."
  (if *statusbar-enabled*
      (statusbar-disable!)
      (statusbar-enable!)))

(define-command (statusbar-reload!)
  "Detach module hooks and force an update in all outputs.
Parameters: None."
  (log-info "statusbar: reloading")
  (statusbar-detach-module-hooks!)
  (set! *statusbar-last-tick-time* 0)
  (statusbar-update-all!))

(define-command (statusbar-assign-all! bar)
  "Assign BAR to every currently known output.
Parameters: (BAR)
  - BAR: <statusbar> record to assign."
  (when (and *manager* (statusbar? bar))
    (let ((assigned? (statusbar-output bar)))
      (for-each
       (lambda (output)
         ;; if statusbar is already assigned to an output, clone it
         (output-statusbar-set! output (if assigned?
                                           (statusbar-clone bar)
                                           bar))
         (set! assigned? #t))
       (manager-outputs *manager*)))))
