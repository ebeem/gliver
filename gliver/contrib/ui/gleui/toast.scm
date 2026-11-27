;;; gliver/contrib/ui/gleui/toast.scm --- Gleui Toast Notifications
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module manages gleui toast notification lifecycles.
;;; It handles:
;;;  - Creating, resizing, and destroying wlr-layer-shell toast surfaces
;;;  - Implementing eat-next-key dismissal and background auto-hide threads
;;;
;;; NOTE: for simplicity, the toast is implemented in 3 files (in the below order):
;;;  - toast/state.scm: defines custom toast variables and state record
;;;  - toast/render.scm: paint and renders the toast gleui widget from the state
;;;  - toast.scm: manages surface lifecycle, input dismissal, and timeouts

(define-module (gliver contrib ui gleui toast)
  #:use-module (cairo)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-11)
  #:use-module (gliver core)
  #:use-module (gliver wayland client)
  #:use-module (gliver wayland gen wayland)
  #:use-module (gliver river connector)
  #:use-module (gliver river window-manager)
  #:use-module (gliver river xkb-bindings-manager)
  #:use-module (gliver river wlr-layer-shell)
  #:use-module (gliver contrib ui gleui base)
  #:use-module (gliver contrib ui gleui toast state)
  #:use-module (gliver contrib ui gleui toast render)
  #:declarative? #f
  #:export (
            gleui-toast-ensure-surface!
            gleui-toast-set-margin!
            *gleui-toast-eat-key-instances*
            gleui-toast-show
            gleui-toast-kill!
            gleui-toast-kill-all!
            gleui-toast-cleanup!
            gleui-toast-on-surface-scale-changed
))

(re-export-modules (gliver contrib ui gleui toast state)
                   (gliver contrib ui gleui toast render))

(define* (%resolve-toast-margins overrides #:key (margin-top #f) (margin-right #f)
                                 (margin-bottom #f) (margin-left #f))
  "Resolve per-edge layer-surface margins from explicit args, OVERRIDES, and default theme margin.
Parameters: (OVERRIDES [#:MARGIN-TOP #f] [#:MARGIN-RIGHT #f] [#:MARGIN-BOTTOM #f] [#:MARGIN-LEFT #f])
  - OVERRIDES: Alist of toast theme overrides, or #f.
  - MARGIN-TOP: Integer or #f explicit top margin override in pixels.
  - MARGIN-RIGHT: Integer or #f explicit right margin override in pixels.
  - MARGIN-BOTTOM: Integer or #f explicit bottom margin override in pixels.
  - MARGIN-LEFT: Integer or #f explicit left margin override in pixels.
Returns four values: TOP, RIGHT, BOTTOM, and LEFT margins in pixels."
  (let ((base-margin (gleui-theme-get overrides 'margin '*gleui-toast-margin*))
        (edge        (lambda (explicit key)
                       (or explicit (and (list? overrides) (assq-ref overrides key))))))
    (values (or (edge margin-top 'margin-top) base-margin)
            (or (edge margin-right 'margin-right) base-margin)
            (or (edge margin-bottom 'margin-bottom) base-margin)
            (or (edge margin-left 'margin-left) base-margin))))

(define (gleui-toast-ensure-surface! state)
  "Create and configure Wayland surface and layer-surface for toast STATE.
Parameters: (STATE)
  - STATE: <gleui-toast-state> record, or #f."
  (when (and state
             (or (not (valid-pointer? (gleui-toast-state-surface state)))
                 (not (valid-pointer? (gleui-toast-state-layer-surface state))))
             *river-connected?*
             (valid-pointer? *wl-compositor*)
             (valid-pointer? *wlr-layer-shell*))
    (let* ((overrides (gleui-toast-state-theme-overrides state))
           (location  (gleui-theme-get overrides 'location '*gleui-toast-location*))
           (name      (gleui-toast-state-name state)))
      (let-values (((top right bottom left) (%resolve-toast-margins overrides)))
        (gleui-layer-surface-create
         #:output (gleui-toast-state-output state)
         #:namespace (string-append "gliver-toast-" name)
         #:layer *wlr-layer-shell-layer-overlay*
         #:anchor (gleui-numpad->anchor location)
         #:margin-top top
         #:margin-right right
         #:margin-bottom bottom
         #:margin-left left
         #:exclusive-zone -1
         #:keyboard-interactivity *wlr-layer-shell-interactivity-none*
         #:width (gleui-toast-state-width state)
         #:height (gleui-toast-state-height state)
         #:on-created (lambda (surface layer-surface)
                        (gleui-toast-state-surface-set! state surface)
                        (gleui-toast-state-layer-surface-set! state layer-surface)
                        (gleui-toast-state-configured?-set! state #f)
                        (when (pair? (gleui-toast-state-actions state))
                          ;; registering mouse/pointer hitboxes for action buttons
                          (gleui-pointer-setup!)
                          (gleui-pointer-register-surface!
                           surface
                           ;; hitboxes have to be returned as a procedure
                           (lambda () (gleui-toast-state-hit-boxes state))
                           #:redraw-proc (lambda ()
                                           (when (gleui-toast-state-visible? state)
                                             (gleui-toast-render! state))))))
         #:on-configure (lambda (configured-w configured-h)
                          (log-debug "gleui-toast: layer surface configured ~ax~a (~a)"
                                     configured-w configured-h name)
                          (gleui-toast-state-configured?-set! state #t)
                          (when (gleui-toast-state-visible? state)
                            (gleui-toast-render! state)))
         #:on-close (lambda ()
                      (log-info "gleui-toast: layer surface closed (~a)" name)
                      (gleui-toast-kill! #:name name)))))))

(define* (gleui-toast-set-margin! name
                                  #:key (margin-top #f)
                                  (margin-right #f)
                                  (margin-bottom #f)
                                  (margin-left #f))
  "Update layer-surface margins for active toast instance NAME.
This is useful to update margins after they are visible on screen.
For example: to push notifications down as new are received.
Parameters: (NAME [#:MARGIN-TOP #f] [#:MARGIN-RIGHT #f] [#:MARGIN-BOTTOM #f] [#:MARGIN-LEFT #f])
  - NAME: String toast instance name or <gleui-toast-state> record.
  - MARGIN-TOP: Integer or #f override top margin in pixels (#f keeps default).
  - MARGIN-RIGHT: Integer or #f override right margin in pixels (#f keeps default).
  - MARGIN-BOTTOM: Integer or #f override bottom margin in pixels (#f keeps default).
  - MARGIN-LEFT: Integer or #f override left margin in pixels (#f keeps default)."
  (and-let* ((state (if (string? name)
                        (gleui-toast-get-instance name)
                        name))
             ((gleui-toast-state? state))
             ((gleui-toast-state-visible? state))
             (layer-surface (gleui-toast-state-layer-surface state))
             (surface       (gleui-toast-state-surface state))
             (overrides     (or (gleui-toast-state-theme-overrides state) '()))
             ((valid-pointer? layer-surface))
             ((valid-pointer? surface)))
    (let-values (((top right bottom left)
                  (%resolve-toast-margins overrides
                                          #:margin-top margin-top
                                          #:margin-right margin-right
                                          #:margin-bottom margin-bottom
                                          #:margin-left margin-left)))
      (gleui-layer-surface-set-margin! layer-surface top right bottom left)
      (wl-surface-commit surface))))

;; eating next key is capturing any non-modifier key pressed that's not a keybinding.
;; it's useful to ensure next key is eaten if we want to have blocking toast where users
;; must press a key to have a certain command executed
;; this is clear in which-key where pressing any key should dismiss the toast
;; NOTE: add these instances are managed by %sync-eat-key-state!
(define-var *gleui-toast-eat-key-instances* '()
            "List of toast instance names that should eat the next key and disappear.")

(define (%gleui-toast-ensure-key-eaten!)
  "Request that the compositor eat the next key press in all seats."
  (with-manage-sequence
   #:dirty? #t
   (xkb-bindings-manager-bindings-seat-ensure-next-key-eaten-all!)))

(define (%gleui-toast-cancel-ensure-key-eaten!)
  "Cancel any pending next-key-eaten request in all seats."
  (with-manage-sequence
   #:dirty? #t
   (xkb-bindings-manager-bindings-seat-cancel-ensure-next-key-eaten-all!)))

(define (%gleui-toast-on-key-event . _)
  "Invoked when a key is pressed or eaten.
Dismiss all toasts waiting to eat the next key press."
  (unless (null? *gleui-toast-eat-key-instances*)
    (for-each
     (lambda (name) (gleui-toast-kill! #:name name))
     *gleui-toast-eat-key-instances*)
    (set! *gleui-toast-eat-key-instances* '())))

(gliver-hook-add! %keybinding-eaten-hook '%gleui-toast-on-key-event)
(gliver-hook-add! %keybinding-pressed-hook '%gleui-toast-on-key-event)

(define (%sync-eat-key-state! toast-name eat-next-key?)
  "Update next-key-eaten dismissal tracking for TOAST-NAME based on EAT-NEXT-KEY?.
Parameters: (TOAST-NAME EAT-NEXT-KEY?)
  - TOAST-NAME: String toast instance name.
  - EAT-NEXT-KEY?: Boolean indicating whether the toast should eat the next key and dismiss."
  (if eat-next-key?
      (begin
        ;; add the toast name to instances if not already
        (unless (member toast-name *gleui-toast-eat-key-instances*)
          (set! *gleui-toast-eat-key-instances*
                (cons toast-name *gleui-toast-eat-key-instances*)))
        ;; ensure that we eat next key
        (%gleui-toast-ensure-key-eaten!))
      (when (member toast-name *gleui-toast-eat-key-instances*)
        ;; delete the toast from instances if it exists
        (set! *gleui-toast-eat-key-instances*
              (delete toast-name *gleui-toast-eat-key-instances*))
        ;; only cancel key eating if we don't have anymore instances
        ;; if we have instances pending eating key we shouldn't cancel
        ;; otherwise they will be stuck on screen
        (when (null? *gleui-toast-eat-key-instances*)
          (%gleui-toast-cancel-ensure-key-eaten!)))))

(define (%schedule-toast-timeout! state toast-name timeout)
  "Spawn a timeout thread to dismiss toast STATE named TOAST-NAME.
Parameters: (STATE TOAST-NAME TIMEOUT)
  - STATE: <gleui-toast-state> record.
  - TOAST-NAME: String toast instance name.
  - TIMEOUT: Float delay duration in seconds (non-positive disables timeout).
Returns thread object if scheduled, or unspecified when TIMEOUT is non-positive."
  (when (and (real? timeout)
             (> timeout 0))
    ;; we accept float seconds, but usleep accepts microseconds
    ;; it must be a valid integer, everything else is unaccepted by usleep
    (let ((delay-usec (inexact->exact (round (* timeout 1000000.0)))))
      (call-with-new-thread
       (lambda ()
         (usleep delay-usec)
         ;; skip if this toast was already closed or replaced by
         ;; a newer one with the same name
         (when (gleui-toast-state-visible? state)
           (gleui-toast-kill! #:name toast-name)))))))

(define* (gleui-toast-show content
                           #:key (timeout 0)
                           (name "default")
                           (icon #f)
                           (theme-overrides '())
                           (on-close #f)
                           (output #f)
                           (eat-next-key? #f)
                           (actions '()))
  "Show a toast notification displaying CONTENT.
Parameters: (CONTENT [#:TIMEOUT 0] [#:NAME \"default\"] [#:ICON #f]
[#:THEME-OVERRIDES '()] [#:ON-CLOSE #f] [#:OUTPUT #f] [#:EAT-NEXT-KEY? #f] [#:ACTIONS '()])
  - CONTENT: <gleui-widget>, string, list of lines or widgets, 2D list of grid rows, or value.
  - TIMEOUT: Float auto-dismiss delay in seconds, or 0 for persistent.
  - NAME: String unique toast instance name.
  - ICON: String file path or icon name, Cairo image surface, or #f.
  - THEME-OVERRIDES: Alist of toast style overrides.
  - ON-CLOSE: Procedure or #f zero-argument closure invoked when toast closes.
  - OUTPUT: <output> record, string, integer, pointer, or #f target output.
  - EAT-NEXT-KEY?: Boolean; when #t, eat the next pressed key and dismiss the toast.
  - ACTIONS: List of (LABEL . CALLBACK) action button pairs."
  ;; kill any existing toast with the same name, the 'overwrite' behavior
  (when (hash-ref *gleui-toast-instances* name #f)
    (gleui-toast-kill! #:name name))

  (%sync-eat-key-state! name eat-next-key?)
  (let* ((overrides      (or theme-overrides '())) ; handle #f if passed
         (target-output  (output-find (or output (assq-ref overrides 'output))))
         (effective-icon (or icon (assq-ref overrides 'icon)))
         (icon-surface   (gleui-load-icon effective-icon *gleui-toast-icon-theme*))
         (state          (gleui-make-toast-state #f #f #f 0 #f #t 0 0
                                                 target-output content name
                                                 overrides (make-mutex)
                                                 icon-surface on-close actions '())))
    (hash-set! *gleui-toast-instances* name state)

    (let-values (((width height) (gleui-toast-render-calc-dimensions state)))
      (gleui-toast-state-width-set! state width)
      (gleui-toast-state-height-set! state height))

    ;; ensure the state has proper surface painted and render it
    (gleui-toast-ensure-surface! state)
    (when (and (gleui-toast-state-configured? state)
               (gleui-toast-state-visible? state))
      (gleui-toast-render! state))

    ;; to dismiss the toast after timeout (if > 0)
    (%schedule-toast-timeout! state name timeout)))

(define* (gleui-toast-kill! #:key (name "default"))
  "Dismiss and destroy the toast instance identified by NAME.
Parameters: ([#:NAME \"default\"])
  - NAME: String toast instance name."
  (%sync-eat-key-state! name #f)
  (and-let* ((state (hash-ref *gleui-toast-instances* name #f))
             (on-close-proc
              ;; mutex justification explain `gleui-toast-render!`
              (with-mutex (gleui-toast-state-render-mutex state)
                ;; we set it to invisible so threads can stop trying to kill it
                (gleui-toast-state-visible?-set! state #f)
                (gleui-toast-state-configured?-set! state #f)
                (gleui-toast-state-content-set! state #f)
                (let ((icon-surface (gleui-toast-state-icon-surface state)))
                  (gleui-toast-state-on-close-set! state #f)
                  (when icon-surface
                    (catch #t (lambda ()
                                (cairo-surface-destroy icon-surface))
                      (lambda _ #f))
                    (gleui-toast-state-icon-surface-set! state #f))
                  (gleui-pointer-unregister-surface! (gleui-toast-state-surface state))
                  (gleui-layer-surface-destroy! (gleui-toast-state-surface state)
                                                (gleui-toast-state-layer-surface state))
                  (gleui-toast-state-layer-surface-set! state #f)
                  (gleui-toast-state-surface-set! state #f)
                  (gleui-shm-slots-destroy! (gleui-toast-state-shm-slots state))
                  (gleui-toast-state-shm-slots-set! state #f)
                  (hash-remove! *gleui-toast-instances* name)
                  ;; we return the toast on-close procedure
                  (gleui-toast-state-on-close state))))
             ((procedure? on-close-proc)))
    (catch #t on-close-proc
      (lambda (k . args)
        (log-error "gleui-toast: on-close callback error: ~a ~a" k args)))))

(define (gleui-toast-kill-all!)
  "Dismiss all active toast notification instances.
Parameters: None."
  ;; loop every name (hash key) in *gleui-toast-instances* and kill its toast
  (for-each (lambda (name)
              (gleui-toast-kill! #:name name))
            (hash-map->list (lambda (name _state) name)
                            *gleui-toast-instances*)))

(define (gleui-toast-cleanup!)
  "Clean up all toast view resources during on (unbind or shutdown).
Parameters: None."
  (set! *gleui-toast-eat-key-instances* '())
  (gleui-toast-kill-all!)
  (hash-clear! *gleui-toast-instances*))

(define* (gleui-toast-on-surface-scale-changed surface scale)
  "Re-render any toast matching SURFACE when its fractional SCALE changes.
Parameters: (SURFACE SCALE)
  - SURFACE: Pointer to Wayland wl_surface whose fractional scale updated.
  - SCALE: Float updated surface scale factor."
  (log-info "gleui-toast: scale changed to ~a" scale)
  (hash-for-each
   (lambda (_key state)
     (when (and state (equal? surface (gleui-toast-state-surface state)))
       (gleui-toast-render! state)))
   *gleui-toast-instances*))

(gliver-hook-add! *surface-scale-changed-hook* 'gleui-toast-on-surface-scale-changed)
(gliver-hook-add! *gliver-globals-unbind-hook* 'gleui-toast-cleanup!)
