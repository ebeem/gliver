;;; gliver/contrib/ui/gleui/statusbar/state.scm --- Statusbar records and registry
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module defines the <statusbar> and <statusbar-module> records,
;;; module registry and instance cache
;;; It handles:
;;;  - Configuring per-output <statusbar> and state
;;;  - Creating <statusbar-module> definitions, registry, and instance factory
;;;  - Tracking output table and active modules

(define-module (gliver contrib ui gleui statusbar state)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-69)
  #:use-module (system foreign)
  #:use-module (gliver core)
  #:declarative? #f
  #:export (
            *statusbar-enabled*
            *statusbar-position*
            *statusbar-height*
            *statusbar-margin-top*
            *statusbar-margin-bottom*
            *statusbar-margin-left*
            *statusbar-margin-right*
            *statusbar-padding-x*
            *statusbar-padding-y*
            *statusbar-spacing*
            *statusbar-bg-color*
            *statusbar-fg-color*
            *statusbar-border-color*
            *statusbar-border-width*
            *statusbar-border-radius*
            *statusbar-pill-radius*
            *statusbar-pill-padding-x*
            *statusbar-pill-padding-y*
            *statusbar-font*
            *statusbar-font-size*
            *statusbar-modules-left*
            *statusbar-modules-center*
            *statusbar-modules-right*
            *statusbar-click-enabled*
            *statusbar-module-registry*
            *statusbar-output-table*
            *statusbar-last-tick-time*
            <statusbar>
            statusbar?
            statusbar-id
            statusbar-position
            statusbar-position-set!
            statusbar-height
            statusbar-height-set!
            statusbar-margin-top
            statusbar-margin-top-set!
            statusbar-margin-bottom
            statusbar-margin-bottom-set!
            statusbar-margin-left
            statusbar-margin-left-set!
            statusbar-margin-right
            statusbar-margin-right-set!
            statusbar-padding-x
            statusbar-padding-x-set!
            statusbar-padding-y
            statusbar-padding-y-set!
            statusbar-spacing
            statusbar-spacing-set!
            statusbar-bg-color
            statusbar-bg-color-set!
            statusbar-fg-color
            statusbar-fg-color-set!
            statusbar-border-color
            statusbar-border-color-set!
            statusbar-border-width
            statusbar-border-width-set!
            statusbar-border-radius
            statusbar-border-radius-set!
            statusbar-pill-radius
            statusbar-pill-radius-set!
            statusbar-pill-padding-x
            statusbar-pill-padding-x-set!
            statusbar-pill-padding-y
            statusbar-pill-padding-y-set!
            statusbar-font
            statusbar-font-set!
            statusbar-font-size
            statusbar-font-size-set!
            statusbar-modules-left
            statusbar-modules-left-set!
            statusbar-modules-center
            statusbar-modules-center-set!
            statusbar-modules-right
            statusbar-modules-right-set!
            statusbar-click-enabled?
            statusbar-click-enabled?-set!
            statusbar-output
            statusbar-output-set!
            statusbar-surface
            statusbar-surface-set!
            statusbar-layer-surface
            statusbar-layer-surface-set!
            statusbar-buffer
            statusbar-buffer-set!
            statusbar-rendered-width
            statusbar-rendered-width-set!
            statusbar-rendered-height
            statusbar-rendered-height-set!
            statusbar-configured?
            statusbar-configured?-set!
            statusbar-hit-boxes
            statusbar-hit-boxes-set!
            statusbar-shm-slots
            statusbar-shm-slots-set!
            statusbar-shm-active-idx
            statusbar-shm-active-idx-set!
            statusbar-modules
            make-statusbar
            statusbar-bind-output!
            statusbar-unbind-output!
            <statusbar-module>
            statusbar-module?
            statusbar-module-id
            statusbar-module-name
            statusbar-module-section
            statusbar-module-interval
            statusbar-module-last-poll
            statusbar-module-last-poll-set!
            statusbar-module-format-template
            statusbar-module-icon
            statusbar-module-text
            statusbar-module-text-set!
            statusbar-module-tooltip
            statusbar-module-tooltip-set!
            statusbar-module-state
            statusbar-module-state-set!
            statusbar-module-bg-color
            statusbar-module-bg-color-set!
            statusbar-module-fg-color
            statusbar-module-fg-color-set!
            statusbar-module-border-color
            statusbar-module-border-color-set!
            statusbar-module-border-width
            statusbar-module-border-radius
            statusbar-module-padding-x
            statusbar-module-padding-y
            statusbar-module-update-fn
            statusbar-module-widget-fn
            statusbar-module-on-click
            statusbar-module-on-scroll
            statusbar-module-visible?
            statusbar-module-visible?-set!
            statusbar-module-event-hooks
            statusbar-module-event-hooks-set!
            make-statusbar-module
            statusbar-register-module!
            statusbar-get-module
            statusbar-resolve-modules
            statusbar-bar-modules
            statusbar-active-modules
            statusbar-execute-shell-command
))

(define-var *statusbar-enabled* #f
            "Whether the statusbar is currently enabled.")
(define-var *statusbar-position* 'top
            "Position of the statusbar: 'top or 'bottom.")
(define-var *statusbar-height* 32
            "Height of the statusbar in pixels.")
(define-var *statusbar-margin-top* 4
            "Top margin of the statusbar.")
(define-var *statusbar-margin-bottom* 0
            "Bottom margin of the statusbar.")
(define-var *statusbar-margin-left* 8
            "Left margin of the statusbar.")
(define-var *statusbar-margin-right* 8
            "Right margin of the statusbar.")
(define-var *statusbar-padding-x* 8
            "Horizontal inner padding of the statusbar.")
(define-var *statusbar-padding-y* 2
            "Vertical inner padding of the statusbar.")
(define-var *statusbar-spacing* 6
            "Spacing in pixels between module capsules.")
(define-var *statusbar-bg-color* #:inherit *theme-bg-main*
            "Background color of the statusbar (hex string, supports RGBA).")
(define-var *statusbar-fg-color* #:inherit *theme-fg-main*
            "Default foreground text color of the statusbar.")
(define-var *statusbar-border-color* #:inherit *theme-fg-alt*
            "Border color of the statusbar.")
(define-var *statusbar-border-width* 0
            "Border width of the statusbar in pixels (0 for no border).")
(define-var *statusbar-border-radius* 10
            "Corner border radius of the statusbar (0 for rectangular bar, >0 for floating island).")
(define-var *statusbar-pill-radius* 6
            "Default corner border radius for module capsules.")
(define-var *statusbar-pill-padding-x* 10
            "Default horizontal padding inside module capsules.")
(define-var *statusbar-pill-padding-y* 3
            "Default vertical padding inside module capsules.")
(define-var *statusbar-font* #:inherit *theme-font*
            "Font family string used inside the statusbar.")
(define-var *statusbar-font-size* 12
            "Font size in points for statusbar text.")
(define-var *statusbar-modules-left* '(workspaces window)
            "List of modules to render on the left side of the statusbar.")
(define-var *statusbar-modules-center* '(date weather)
            "List of modules to render at the center of the statusbar.")
(define-var *statusbar-modules-right* '(cpu ram battery)
            "List of modules to render on the right side of the statusbar.")
(define-var *statusbar-click-enabled* #t
            "Whether pointer click and scroll interaction is enabled on the statusbar.")
(define-var *statusbar-module-registry* (make-hash-table)
            "Hash table mapping module symbol IDs to factory procedures.")
(define-var *statusbar-output-table* (make-hash-table)
            "Hash table mapping output IDs to active <statusbar> records.")
(define-var *statusbar-last-tick-time* 0
            "POSIX timestamp in seconds of the most recent statusbar tick.")

;; statusbar record combining configuration and per-output runtime state
;;  - id: symbol unique identifier for the statusbar
;;  - position: symbol bar edge placement ('top or 'bottom)
;;  - height: integer statusbar height in pixels
;;  - margin-top: integer outer top margin in pixels
;;  - margin-bottom: integer outer bottom margin in pixels
;;  - margin-left: integer outer left margin in pixels
;;  - margin-right: integer outer right margin in pixels
;;  - padding-x: integer inner horizontal bar padding in pixels
;;  - padding-y: integer inner vertical bar padding in pixels
;;  - spacing: integer gap between modules in pixels
;;  - bg-color: string background hex color
;;  - fg-color: string foreground hex color
;;  - border-color: string border hex color
;;  - border-width: integer border thickness in pixels
;;  - border-radius: integer bar corner radius in pixels
;;  - pill-radius: integer default module pill corner radius in pixels
;;  - pill-padding-x: integer default horizontal pill padding in pixels
;;  - pill-padding-y: integer default vertical pill padding in pixels
;;  - font: string font family name
;;  - font-size: integer font size in points
;;  - modules-left: list of modules in the left section
;;  - modules-center: list of modules in the center section
;;  - modules-right: list of modules in the right section
;;  - click-enabled?: boolean enabling pointer clicks when #t
;;  - output: record <output> currently bound to this statusbar instance, or #f
;;  - surface: pointer to wl_surface of the statusbar
;;  - layer-surface: pointer to zwlr_layer_surface_v1 managing layer placement
;;  - buffer: pointer to active wl_buffer committed to the surface
;;  - rendered-width: integer last configured surface width in pixels
;;  - rendered-height: integer last configured surface height in pixels
;;  - configured?: boolean indicating whether the initial layer-surface configure event arrived
;;  - hit-boxes: list of collected <gleui-hit-box> records
;;  - shm-slots: pair (slot-a . slot-b) of double-buffered <gleui-shm-slot> records
;;  - shm-active-idx: integer active SHM slot index (0 or 1) alternated per commit
(define-record-type <statusbar>
  (%make-statusbar id position height
                   margin-top margin-bottom margin-left margin-right padding-x padding-y
                   spacing bg-color fg-color border-color border-width border-radius
                   pill-radius pill-padding-x pill-padding-y font font-size
                   modules-left modules-center modules-right click-enabled? output
                   surface layer-surface buffer rendered-width rendered-height
                   configured? hit-boxes shm-slots shm-active-idx)
  statusbar?
  (id               statusbar-id)
  (position         statusbar-position         statusbar-position-set!)
  (height           statusbar-height           statusbar-height-set!)
  (margin-top       statusbar-margin-top       statusbar-margin-top-set!)
  (margin-bottom    statusbar-margin-bottom    statusbar-margin-bottom-set!)
  (margin-left      statusbar-margin-left      statusbar-margin-left-set!)
  (margin-right     statusbar-margin-right     statusbar-margin-right-set!)
  (padding-x        statusbar-padding-x        statusbar-padding-x-set!)
  (padding-y        statusbar-padding-y        statusbar-padding-y-set!)
  (spacing          statusbar-spacing          statusbar-spacing-set!)
  (bg-color         statusbar-bg-color         statusbar-bg-color-set!)
  (fg-color         statusbar-fg-color         statusbar-fg-color-set!)
  (border-color     statusbar-border-color     statusbar-border-color-set!)
  (border-width     statusbar-border-width     statusbar-border-width-set!)
  (border-radius    statusbar-border-radius    statusbar-border-radius-set!)
  (pill-radius      statusbar-pill-radius      statusbar-pill-radius-set!)
  (pill-padding-x   statusbar-pill-padding-x   statusbar-pill-padding-x-set!)
  (pill-padding-y   statusbar-pill-padding-y   statusbar-pill-padding-y-set!)
  (font             statusbar-font             statusbar-font-set!)
  (font-size        statusbar-font-size        statusbar-font-size-set!)
  (modules-left     statusbar-modules-left     statusbar-modules-left-set!)
  (modules-center   statusbar-modules-center   statusbar-modules-center-set!)
  (modules-right    statusbar-modules-right    statusbar-modules-right-set!)
  (click-enabled?   statusbar-click-enabled?   statusbar-click-enabled?-set!)
  (output           statusbar-output           statusbar-output-set!)
  (surface          statusbar-surface          statusbar-surface-set!)
  (layer-surface    statusbar-layer-surface    statusbar-layer-surface-set!)
  (buffer           statusbar-buffer           statusbar-buffer-set!)
  (rendered-width   statusbar-rendered-width   statusbar-rendered-width-set!)
  (rendered-height  statusbar-rendered-height  statusbar-rendered-height-set!)
  (configured?      statusbar-configured?      statusbar-configured?-set!)
  (hit-boxes        statusbar-hit-boxes        statusbar-hit-boxes-set!)
  (shm-slots        statusbar-shm-slots        statusbar-shm-slots-set!)
  (shm-active-idx   statusbar-shm-active-idx   statusbar-shm-active-idx-set!))

(set-record-type-printer! <statusbar>
                          (lambda (bar port)
                            (format port "#<statusbar id=~a pos=~a height=~a left=~a center=~a right=~a>"
                                    (statusbar-id bar)
                                    (statusbar-position bar)
                                    (statusbar-height bar)
                                    (length (statusbar-modules-left bar))
                                    (length (statusbar-modules-center bar))
                                    (length (statusbar-modules-right bar)))))

(define (statusbar-modules bar)
  "Return a flat list of all modules configured on BAR.
Parameters: (BAR)
  - BAR: <statusbar> record.
Returns flat list of module specs."
  (if (statusbar? bar)
      (append (statusbar-modules-left bar)
              (statusbar-modules-center bar)
              (statusbar-modules-right bar))
      '()))

(define* (make-statusbar #:key
                         (id (gensym "statusbar-"))
                         (position *statusbar-position*)
                         (height *statusbar-height*)
                         (margin-top *statusbar-margin-top*)
                         (margin-bottom *statusbar-margin-bottom*)
                         (margin-left *statusbar-margin-left*)
                         (margin-right *statusbar-margin-right*)
                         (padding-x *statusbar-padding-x*)
                         (padding-y *statusbar-padding-y*)
                         (spacing *statusbar-spacing*)
                         (bg-color *statusbar-bg-color*)
                         (fg-color *statusbar-fg-color*)
                         (border-color *statusbar-border-color*)
                         (border-width *statusbar-border-width*)
                         (border-radius *statusbar-border-radius*)
                         (pill-radius *statusbar-pill-radius*)
                         (pill-padding-x *statusbar-pill-padding-x*)
                         (pill-padding-y *statusbar-pill-padding-y*)
                         (font *statusbar-font*)
                         (font-size *statusbar-font-size*)
                         (modules-left *statusbar-modules-left*)
                         (modules-center *statusbar-modules-center*)
                         (modules-right *statusbar-modules-right*)
                         (click-enabled? *statusbar-click-enabled*)
                         (output #f))
  "Construct a new <statusbar> record instance."
  (let ((bar (%make-statusbar id position height
                              margin-top margin-bottom margin-left margin-right
                              padding-x padding-y spacing
                              bg-color fg-color border-color border-width border-radius
                              pill-radius pill-padding-x pill-padding-y
                              font font-size
                              (statusbar-resolve-modules modules-left)
                              (statusbar-resolve-modules modules-center)
                              (statusbar-resolve-modules modules-right)
                              click-enabled?
                              #f %null-pointer %null-pointer %null-pointer
                              0 0 #f '() #f 0)))
    (when (and output (output? output))
      (output-statusbar-set! output bar))
    bar))

(define (statusbar-bind-output! bar output init-width bar-height)
  "Transfer and bind BAR to OUTPUT.
If BAR is currently bound to a different output, detach it from that output first.
Parameters: (BAR OUTPUT INIT-WIDTH BAR-HEIGHT)
  - BAR: <statusbar> record to bind.
  - OUTPUT: <output> record receiving the statusbar.
  - INIT-WIDTH: Integer surface width in pixels.
  - BAR-HEIGHT: Integer surface height in pixels.
Returns BAR stored in `*statusbar-output-table*'."
  (let ((prev (statusbar-output bar)))
    (when (and prev (not (eq? prev output)))
      (output-statusbar-set! prev #f)
      (hash-table-delete! *statusbar-output-table* (output-id prev))))
  (statusbar-output-set! bar output)
  (statusbar-surface-set! bar %null-pointer)
  (statusbar-layer-surface-set! bar %null-pointer)
  (statusbar-buffer-set! bar %null-pointer)
  (statusbar-rendered-width-set! bar init-width)
  (statusbar-rendered-height-set! bar bar-height)
  (statusbar-configured?-set! bar #f)
  (statusbar-hit-boxes-set! bar '())
  (statusbar-shm-slots-set! bar #f)
  (statusbar-shm-active-idx-set! bar 0)
  (%output-statusbar-set! output bar)
  (hash-table-set! *statusbar-output-table* (output-id output) bar)
  bar)

(define (statusbar-unbind-output! bar)
  "Clear surface and output references on BAR after cleanup.
Parameters: (BAR)
  - BAR: <statusbar> record."
  (statusbar-output-set! bar #f)
  (statusbar-layer-surface-set! bar %null-pointer)
  (statusbar-surface-set! bar %null-pointer)
  (statusbar-buffer-set! bar %null-pointer)
  (statusbar-configured?-set! bar #f)
  (statusbar-hit-boxes-set! bar '())
  (statusbar-shm-slots-set! bar #f)
  (statusbar-shm-active-idx-set! bar 0))

;; statusbar module record describing widget state, styling, and callbacks
;;  - id: symbol unique module identifier
;;  - name: string human-readable module name
;;  - section: symbol default bar section ('left, 'center, or 'right)
;;  - interval: number polling interval in seconds, or #f when event-driven
;;  - last-poll: integer POSIX timestamp of last update execution
;;  - format-template: string format template (currently just metadata)
;;  - icon: string or #f default icon
;;  - text: string currently rendered module text
;;  - tooltip: string tooltip text (currently not implemented)
;;  - state: arbitrary module-specific state value
;;  - bg-color: string or #f pill background hex color
;;  - fg-color: string or #f pill foreground hex color
;;  - border-color: string or #f pill border hex color
;;  - border-width: integer or #f pill border thickness in pixels
;;  - border-radius: integer or #f pill corner radius in pixels
;;  - padding-x: integer or #f horizontal pill padding in pixels
;;  - padding-y: integer or #f vertical pill padding in pixels
;;  - update-fn: procedure (MODULE OUTPUT) updating state, or #f
;;  - widget-fn: procedure building declarative gleui widget tree, or #f
;;  - on-click: procedure handling pointer clicks, or #f
;;  - on-scroll: procedure handling pointer scrolls, or #f
;;  - visible?: boolean or predicate (MODULE OUTPUT)
;;  - event-hooks: list of reactive hook triggers attached for this module
(define-record-type <statusbar-module>
  (%make-statusbar-module id name section interval last-poll
                          format-template icon text tooltip state
                          bg-color fg-color border-color border-width border-radius
                          padding-x padding-y update-fn widget-fn
                          on-click on-scroll visible? event-hooks)
  statusbar-module?
  (id               statusbar-module-id)
  (name             statusbar-module-name)
  (section          statusbar-module-section)
  (interval         statusbar-module-interval)
  (last-poll        statusbar-module-last-poll     statusbar-module-last-poll-set!)
  (format-template  statusbar-module-format-template)
  (icon             statusbar-module-icon)
  (text             statusbar-module-text          statusbar-module-text-set!)
  (tooltip          statusbar-module-tooltip       statusbar-module-tooltip-set!)
  (state            statusbar-module-state         statusbar-module-state-set!)
  (bg-color         statusbar-module-bg-color      statusbar-module-bg-color-set!)
  (fg-color         statusbar-module-fg-color      statusbar-module-fg-color-set!)
  (border-color     statusbar-module-border-color  statusbar-module-border-color-set!)
  (border-width     statusbar-module-border-width)
  (border-radius    statusbar-module-border-radius)
  (padding-x        statusbar-module-padding-x)
  (padding-y        statusbar-module-padding-y)
  (update-fn        statusbar-module-update-fn)
  (widget-fn        statusbar-module-widget-fn)
  (on-click         statusbar-module-on-click)
  (on-scroll        statusbar-module-on-scroll)
  (visible?         statusbar-module-visible?      statusbar-module-visible?-set!)
  (event-hooks      statusbar-module-event-hooks   statusbar-module-event-hooks-set!))

(define* (make-statusbar-module #:key
                                (id (gensym "mod-"))
                                (name "Module")
                                (section 'right)
                                (interval #f)
                                (format-template "~a")
                                (icon #f)
                                (text "")
                                (tooltip "")
                                (state '())
                                (bg-color #f)
                                (fg-color #f)
                                (border-color #f)
                                (border-width #f)
                                (border-radius #f)
                                (padding-x #f)
                                (padding-y #f)
                                (update-fn #f)
                                (widget-fn #f)
                                (on-click #f)
                                (on-scroll #f)
                                (visible? #t)
                                (hooks '()))
  "Create a new <statusbar-module> record instance."
  (%make-statusbar-module id name section interval 0
                          format-template icon text tooltip state
                          bg-color fg-color border-color border-width border-radius
                          padding-x padding-y update-fn widget-fn
                          on-click on-scroll visible? hooks))

;; module registry to resolve modules by symbol
;; for example, statusbar-modules-left can be '(workspaces window)
(define (statusbar-register-module! id module-factory)
  "Register a module factory in the global registry.
Parameters: (ID MODULE-FACTORY)
  - ID: Symbol module identifier (e.g. 'workspaces).
  - MODULE-FACTORY: 0-arity procedure returning a <statusbar-module> record.
Returns unspecified."
  (unless (procedure? module-factory)
    (error "statusbar-register-module!: MODULE-FACTORY must be a procedure" id module-factory))
  (hash-table-set! *statusbar-module-registry* id module-factory))

(define (statusbar-get-module id)
  "Look up a registered module factory by its symbol ID.
Parameters: (ID)
  - ID: Symbol module identifier.
Returns <statusbar-module> factory procedure, or #f if not found."
  (hash-table-ref/default *statusbar-module-registry* id #f))

(define (%resolve-module item)
  (cond
   ((statusbar-module? item) item)
   ((symbol? item)
    (let ((factory (statusbar-get-module item)))
      (and (procedure? factory)
           (catch #t (lambda () (factory)) (lambda _ #f)))))
   ((procedure? item)
    (catch #t (lambda () (item)) (lambda _ #f)))
   (else #f)))

(define (statusbar-resolve-modules mod-list)
  "Convert module names or definitions in MOD-LIST into <statusbar-module> records.
Parameters: (MOD-LIST)
  - MOD-LIST: List of <statusbar-module> records, module symbols, or factory procedures.
Returns filtered list of valid <statusbar-module> records."
  (filter statusbar-module? (map %resolve-module mod-list)))

(define (statusbar-bar-modules bar)
  "Return deduplicated <statusbar-module> records configured on BAR.
Parameters: (BAR)
  - BAR: <statusbar> record.
Returns deduplicated list of <statusbar-module> records."
  (delete-duplicates
   (append (statusbar-resolve-modules (statusbar-modules-left bar))
           (statusbar-resolve-modules (statusbar-modules-center bar))
           (statusbar-resolve-modules (statusbar-modules-right bar)))))

(define (statusbar-active-modules)
  "Return all configured module instances in output statusbars.
Parameters: None.
Returns deduplicated list of active <statusbar-module> records."
  (delete-duplicates
   (append-map statusbar-bar-modules
               (filter statusbar?
                       (hash-table-values *statusbar-output-table*)))))

(define (statusbar-execute-shell-command cmd)
  "Execute shell command CMD and return its trimmed stdout.
This is probably the most common method to create modules.
Parameters: (CMD)
  - CMD: String shell command executed via `sh -c'.
Returns trimmed stdout string on zero exit status, or #f on failure."
  (catch #t
    (lambda ()
      (let* ((port (open-pipe* OPEN_READ "sh" "-c" cmd))
             (output (read-delimited "" port))
             (status (close-pipe port)))
        (and (string? output)
             (zero? (status:exit-val status))
             (string-trim-both output))))
    (lambda _ #f)))
