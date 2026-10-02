;;; gliver/contrib/ui/statusbar/base.scm --- Base abstractions for statusbar modules
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver contrib ui statusbar base)
  #:use-module (cairo)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-69)
  #:use-module (gliver core)
  #:declarative? #f
  #:export (
			statusbar-cairo-rounded-rectangle
			statusbar-execute-shell-command
			statusbar-module-event-hooks-set!
			statusbar-module-event-hooks
			statusbar-module-visible?-set!
			statusbar-module-visible?
			statusbar-module-on-scroll
			statusbar-module-on-click
			statusbar-module-measure-fn
			statusbar-module-render-fn
			statusbar-module-update-fn
			statusbar-module-padding-y
			statusbar-module-padding-x
			statusbar-module-border-radius
			statusbar-module-border-width
			statusbar-module-border-color-set!
			statusbar-module-border-color
			statusbar-module-fg-color-set!
			statusbar-module-fg-color
			statusbar-module-bg-color-set!
			statusbar-module-bg-color
			statusbar-module-state-set!
			statusbar-module-state
			statusbar-module-tooltip-set!
			statusbar-module-tooltip
			statusbar-module-text-set!
			statusbar-module-text
			statusbar-module-icon
			statusbar-module-format
			statusbar-module-last-poll-set!
			statusbar-module-last-poll
			statusbar-module-interval
			statusbar-module-section
			statusbar-module-name
			statusbar-module-id
			statusbar-module?
			make-statusbar-module
			statusbar-module-update!
			*statusbar-render-request-hook*
			*statusbar-active-module-hooks*
			statusbar-request-render!
			statusbar-detach-module-hooks!
			statusbar-make-default-module-hook-handler
			statusbar-make-custom-hook-wrapper
			statusbar-attach-module-hooks!
			statusbar-attach-hook!
			statusbar-bind-module-entry!
			*statusbar-module-registry*
			*statusbar-module-instance-cache*
			statusbar-clear-module-cache!
			statusbar-register-module!
			statusbar-get-module
			statusbar-ensure-module
			statusbar-cache-or-create!
))

(define (statusbar-cairo-rounded-rectangle cr x y w h r)
  "Draw a rounded rectangle path at (X, Y) with width W, height H, and corner radius R."
  (let ((radius (max 0.0 (min r (/ w 2.0) (/ h 2.0)))))
    (if (<= radius 0.0)
        (cairo-rectangle cr x y w h)
        (let ((pi (acos -1)))
          (cairo-new-sub-path cr)
          (cairo-arc cr (+ x w (- radius)) (+ y radius) radius (* -0.5 pi) 0.0)
          (cairo-arc cr (+ x w (- radius)) (+ y h (- radius)) radius 0.0 (* 0.5 pi))
          (cairo-arc cr (+ x radius) (+ y h (- radius)) radius (* 0.5 pi) pi)
          (cairo-arc cr (+ x radius) (+ y radius) radius pi (* 1.5 pi))
          (cairo-close-path cr)))))

(define (statusbar-execute-shell-command cmd)
  "Execute shell command CMD and return its stdout as a trimmed string.
This is usually a common pattern used in many statusbar modules."
  (catch #t
    (lambda ()
      (let* ((port (open-pipe* OPEN_READ "sh" "-c" cmd))
             (output (read-delimited "" port))
             (status (close-pipe port)))
        (and (string? output)
             (zero? (status:exit-val status))
             (string-trim-both output))))
    (lambda _ #f)))

(define-record-type <statusbar-module>
  (%make-statusbar-module id name section interval last-poll
                          format icon text tooltip state
                          bg-color fg-color border-color border-width border-radius
                          padding-x padding-y update-fn render-fn measure-fn
                          on-click on-scroll visible? event-hooks)
  statusbar-module?
  (id            statusbar-module-id)
  (name          statusbar-module-name)
  (section       statusbar-module-section)
  (interval      statusbar-module-interval)
  (last-poll     statusbar-module-last-poll     statusbar-module-last-poll-set!)
  (format        statusbar-module-format)
  (icon          statusbar-module-icon)
  (text          statusbar-module-text          statusbar-module-text-set!)
  (tooltip       statusbar-module-tooltip       statusbar-module-tooltip-set!)
  (state         statusbar-module-state         statusbar-module-state-set!)
  (bg-color      statusbar-module-bg-color      statusbar-module-bg-color-set!)
  (fg-color      statusbar-module-fg-color      statusbar-module-fg-color-set!)
  (border-color  statusbar-module-border-color  statusbar-module-border-color-set!)
  (border-width  statusbar-module-border-width)
  (border-radius statusbar-module-border-radius)
  (padding-x     statusbar-module-padding-x)
  (padding-y     statusbar-module-padding-y)
  (update-fn     statusbar-module-update-fn)
  (render-fn     statusbar-module-render-fn)
  (measure-fn    statusbar-module-measure-fn)
  (on-click      statusbar-module-on-click)
  (on-scroll     statusbar-module-on-scroll)
  (visible?      statusbar-module-visible?      statusbar-module-visible?-set!)
  (event-hooks   statusbar-module-event-hooks   statusbar-module-event-hooks-set!))

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
                                (render-fn #f)
                                (measure-fn #f)
                                (on-click #f)
                                (on-scroll #f)
                                (visible? #t)
                                (hooks '()))
  "Construct a new <statusbar-module> record instance."
  (%make-statusbar-module id name section interval 0
                          format-template icon text tooltip state
                          bg-color fg-color border-color border-width border-radius
                          padding-x padding-y update-fn render-fn measure-fn
                          on-click on-scroll visible? hooks))

(define (statusbar-module-update! module output)
  "Execute MODULE's update-fn if defined and capture timestamps and errors."
  (let ((up-fn (statusbar-module-update-fn module)))
    (when (procedure? up-fn)
      (catch #t
        (lambda () (up-fn module output))
        (lambda (key . args)
          (log-error "Error updating statusbar module ~a: ~a ~a"
                     (statusbar-module-id module) key args))))
    (statusbar-module-last-poll-set! module (current-time))))

(define *statusbar-render-request-hook* (make-gliver-hook 'statusbar-render-request 0))
(define *statusbar-active-module-hooks* '())

(define (statusbar-request-render!)
  "Request an asynchronous re-render of the statusbar across all outputs."
  (gliver-hook-run! *statusbar-render-request-hook*))

(define (statusbar-detach-module-hooks!)
  "Detach all active event hooks currently registered by statusbar modules."
  (for-each
   (lambda (binding)
     (let ((hook (car binding))
           (handler (cdr binding)))
       (when (gliver-hook? hook)
         (gliver-hook-remove! hook handler))))
   *statusbar-active-module-hooks*)
  (set! *statusbar-active-module-hooks* '()))

(define (statusbar-make-default-module-hook-handler mod)
  "Create an update handler that triggers re-render only when the module's visual state changes."
  (lambda _args
    (let ((up-fn (statusbar-module-update-fn mod)))
      (if (procedure? up-fn)
          (let ((old-text (statusbar-module-text mod))
                (old-fg (statusbar-module-fg-color mod))
                (old-bg (statusbar-module-bg-color mod))
                (old-vis (statusbar-module-visible? mod))
                (cur-out (and (defined? 'output-current)
                              (catch #t (lambda () (output-current)) (lambda _ #f)))))
            (statusbar-module-update! mod cur-out)
            (when (or (not (equal? old-text (statusbar-module-text mod)))
                      (not (equal? old-fg (statusbar-module-fg-color mod)))
                      (not (equal? old-bg (statusbar-module-bg-color mod)))
                      (not (eq? old-vis (statusbar-module-visible? mod))))
              (statusbar-request-render!)))
          (statusbar-request-render!)))))

(define (statusbar-make-custom-hook-wrapper mod custom-proc)
  "Wrap custom callback to support both (proc mod args...) and (proc args...)."
  (lambda args
    (catch #t
      (lambda () (apply custom-proc mod args))
      (lambda _
        (catch #t
          (lambda () (apply custom-proc args))
          (lambda (k . r)
            (log-error "Error in module ~a hook handler: ~a ~a"
                       (statusbar-module-id mod) k r)))))))

(define (statusbar-attach-hook! hook handler)
  (gliver-hook-add! hook handler)
  (set! *statusbar-active-module-hooks*
        (cons (cons hook handler) *statusbar-active-module-hooks*)))

(define (statusbar-bind-module-entry! mod entry)
  (cond
   ;; format: (hook . custom-handler)
   ((and (pair? entry) (gliver-hook? (car entry)) (procedure? (cdr entry)))
    (statusbar-attach-hook! (car entry) (statusbar-make-custom-hook-wrapper mod (cdr entry))))

   ;; format: hook (uses default change-detecting handler)
   ((gliver-hook? entry)
    (statusbar-attach-hook! entry (statusbar-make-default-module-hook-handler mod)))

   (else
    (log-warn "Invalid statusbar module event-hook entry ~a in ~a"
              entry (statusbar-module-id mod)))))

(define (statusbar-attach-module-hooks! modules)
  "Attach event hooks for the given list of MODULES. Detaches previous hooks first."
  (statusbar-detach-module-hooks!)
  (for-each
   (lambda (mod)
     (let ((hooks (statusbar-module-event-hooks mod)))
       (when (list? hooks)
         (for-each (lambda (entry) (statusbar-bind-module-entry! mod entry)) hooks))))
   (delete-duplicates (filter statusbar-module? modules))))

(define *statusbar-module-registry* (make-hash-table))
(define *statusbar-module-instance-cache* (make-hash-table))

(define (statusbar-clear-module-cache!)
  "Clear cached module instances and detach active hooks."
  (statusbar-detach-module-hooks!)
  (set! *statusbar-module-instance-cache* (make-hash-table)))

(define (statusbar-register-module! id module-or-factory)
  "Register a module instance or factory in the global registry."
  (hash-table-set! *statusbar-module-registry* id module-or-factory))

(define (statusbar-get-module id)
  "Look up a registered module or factory by its symbol ID."
  (hash-table-ref/default *statusbar-module-registry* id #f))

(define (statusbar-cache-or-create! key instantiate-fn)
  (or (hash-table-ref/default *statusbar-module-instance-cache* key #f)
      (let ((mod (instantiate-fn)))
        (when (statusbar-module? mod)
          (hash-table-set! *statusbar-module-instance-cache* key mod))
        mod)))

(define (statusbar-ensure-module item)
  "Resolve ITEM into a <statusbar-module> object.
ITEM can be a module instance, a registered symbol ID, or a 0-arity factory procedure.
Caches resolved instances so modules maintain state and aren't rebuilt each frame."
  (cond
   ((statusbar-module? item) item)

   ((symbol? item)
    (statusbar-cache-or-create! item
                                (lambda ()
                                  (let ((entry (statusbar-get-module item)))
                                    (cond
                                     ((statusbar-module? entry) entry)
                                     ((procedure? entry)        (catch #t (lambda () (entry)) (lambda _ #f)))
                                     (else                      #f))))))

   ((procedure? item)
    (statusbar-cache-or-create! item (lambda () (catch #t (lambda () (item)) (lambda _ #f)))))

   (else #f)))
