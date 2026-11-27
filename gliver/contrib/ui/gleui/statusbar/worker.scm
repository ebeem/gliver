;;; gliver/contrib/ui/gleui/statusbar/worker.scm --- Background update worker
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module manages asynchronous statusbar module polling on a thread.
;;; The main loop of gliver shouldn't be blocked/impacted by operations
;;; that are otherwise not considered critical. statusbar accepts user
;;; modules as well which makes it very easy to make mistakes, that's
;;; why running all ticks in a thread for statusbar modules is crucial

(define-module (gliver contrib ui gleui statusbar worker)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-1)
  #:use-module (gliver core)
  #:use-module (gliver contrib ui gleui statusbar state)
  #:declarative? #f
  #:export (
            statusbar-module-update!
            statusbar-worker-start!
            statusbar-worker-stop!
            statusbar-worker-thread
            statusbar-module-queued-or-running?
            statusbar-mark-render-dirty!
            statusbar-consume-mark-undirty!
            *statusbar-render-request-hook*
            *statusbar-active-module-hooks*
            statusbar-request-render!
            statusbar-detach-module-hooks!
            statusbar-make-default-module-hook-handler
            statusbar-attach-module-hooks!
            statusbar-attach-hook!
            statusbar-bind-module-hook!
))

(define-var %statusbar-worker-mutex (make-mutex)
            "Mutex to synchronize access to the statusbar worker state and queue.")
(define-var %statusbar-worker-cond (make-condition-variable)
            "Condition variable waking the background statusbar worker thread.")
(define-var %statusbar-worker-queue '()
            "FIFO list of pending (MODULE . OUTPUT) worker update jobs.")
(define-var %statusbar-worker-thread #f
            "Background thread, or #f executing statusbar module updates.")
(define-var %statusbar-worker-alive? #f
            "Mutable alive-token pair (ALIVE? . #f), or #f for the active worker thread.")
(define-var %statusbar-running-module #f
            "Currently executing <statusbar-module> on the worker thread, or #f.")
(define-var %statusbar-render-dirty? #f
            "Boolean set when worker module updates produce a visual change needing render.")

(define (statusbar-worker-thread)
  "Return the active background statusbar worker thread, or #f if stopped."
  (with-mutex %statusbar-worker-mutex
    %statusbar-worker-thread))

(define (statusbar-mark-render-dirty!)
  "Mark statusbar surfaces as needing a render on the main thread."
  (with-mutex %statusbar-worker-mutex
    (set! %statusbar-render-dirty? #t)))

(define (statusbar-consume-mark-undirty!)
  "Remove the dirty mark from statusbar surfaces.
Returns #t if a render was requested since the last check, #f otherwise."
  (with-mutex %statusbar-worker-mutex
    (let ((dirty? %statusbar-render-dirty?))
      (set! %statusbar-render-dirty? #f)
      dirty?)))

(define (statusbar-module-queued-or-running? module)
  "Return #t if MODULE currently has an update queued or is running."
  (with-mutex %statusbar-worker-mutex
    (or (eq? %statusbar-running-module module)
        (any (lambda (entry)
               (eq? (car entry)
                    module))
             %statusbar-worker-queue))))

(define (%module-visual-snapshot module)
  "Capture visual state fields of MODULE to detect whether a render is needed."
  ;; this list is considered more of a signature to the module
  ;; if it doesn't change between updates, no need for re-render
  (list (statusbar-module-text module)
        (statusbar-module-fg-color module)
        (statusbar-module-bg-color module)
        (statusbar-module-border-color module)
        (statusbar-module-visible? module)
        (and (procedure? (statusbar-module-widget-fn module))
             (statusbar-module-state module))))

(define (%run-module-update! module output)
  "Execute MODULE's update-fn on the statusbar worker thread
and mark dirty on visual change."
  (let ((update-fn (statusbar-module-update-fn module))
        (before    (%module-visual-snapshot module)))
    (when (procedure? update-fn)
      ;; execute the module update function in a catch to avoid crashes
      (catch #t
        (lambda () (update-fn module output))
        (lambda (key . args)
          (log-error "Error updating statusbar module ~a: ~a ~a"
                     (statusbar-module-id module) key args))))
    (statusbar-module-last-poll-set! module (current-time))
    ;; mark dirty if the signature/snapshot has become different that before
    (unless (equal? before (%module-visual-snapshot module))
      (statusbar-mark-render-dirty!))))

(define (%worker-dequeue-job! alive-token)
  "Wait for and pop the next (MODULE . OUTPUT) job while ALIVE-TOKEN remains active.
Parameters: (ALIVE-TOKEN)
  - ALIVE-TOKEN: Mutable pair (ALIVE? . #f) owned by the calling worker thread.
Returns (MODULE . OUTPUT) pair, or #f when ALIVE-TOKEN is cleared."
  (with-mutex %statusbar-worker-mutex
    ;; sleep until a job arrives or this worker's alive flag is cleared
    (while (and (car alive-token)
                (null? %statusbar-worker-queue))
      (wait-condition-variable %statusbar-worker-cond %statusbar-worker-mutex))
    ;; dequeue the next (module . output) pair and record the active module
    (and (car alive-token)
         (pair? %statusbar-worker-queue)
         (let ((job (car %statusbar-worker-queue)))
           (set! %statusbar-worker-queue (cdr %statusbar-worker-queue))
           (set! %statusbar-running-module (car job))
           job))))

(define (%worker-loop alive-token)
  "Main loop for the background statusbar worker thread.
Parameters: (ALIVE-TOKEN)
  - ALIVE-TOKEN: Mutable pair (ALIVE? . #f) owned by this worker instance."
  (let loop ()
    ;; keep popping jobs and run their module update
    ;; this will set alive token %statusbar-running-module to #t
    (let ((job (%worker-dequeue-job! alive-token)))
      (when job
        ;; run outside the mutex so new updates can enqueue while slow I/O runs
        (catch #t
          (lambda () (%run-module-update! (car job) (cdr job)))
          (lambda (key . args)
            (log-error "statusbar: worker error: ~a ~a" key args)))
        (with-mutex %statusbar-worker-mutex
          ;; only clear running state if this thread is still current worker
          (when (eq? %statusbar-worker-alive? alive-token)
            (set! %statusbar-running-module #f)))
        (loop))))

  ;; worker finished the loop, reset globals only if this
  ;; thread is still current worker
  (with-mutex %statusbar-worker-mutex
    (when (eq? %statusbar-worker-alive? alive-token)
      (set! %statusbar-running-module #f)
      (set! %statusbar-worker-thread #f)
      (set! %statusbar-worker-alive? #f))))

(define (statusbar-worker-start!)
  "Start the background worker thread that executes module #:update-fn callbacks.
Parameters: None."
  ;; mutex and guard check to prevent starting twice
  (with-mutex %statusbar-worker-mutex
    (unless (and %statusbar-worker-alive?
                 (car %statusbar-worker-alive?))
      ;; allocate a fresh mutable alive-token pair per worker instance so a stopped
      ;; thread observes shutdown on its own token
      (let ((alive-token (cons #t #f)))
        (set! %statusbar-worker-alive? alive-token)
        (set! %statusbar-worker-thread
              ;; spawn a thread to run worker loop, it's important to understand
              ;; that at this point the mutex is released
              (call-with-new-thread
               (lambda () (%worker-loop alive-token))))))))

(define (statusbar-worker-stop!)
  "Stop the background statusbar worker thread and discard pending update jobs.
Parameters: None."
  (with-mutex %statusbar-worker-mutex
    ;; set running thread's local token first so its loop terminates
    (when %statusbar-worker-alive?
      (set-car! %statusbar-worker-alive? #f))
    (set! %statusbar-worker-alive? #f)
    (set! %statusbar-worker-thread #f)
    (set! %statusbar-running-module #f)
    (set! %statusbar-worker-queue '())
    (set! %statusbar-render-dirty? #f)
    (broadcast-condition-variable %statusbar-worker-cond)))

(define (statusbar-module-update! module output)
  "Run MODULE's #:update-fn on the worker thread.
Parameters: (MODULE OUTPUT)
  - MODULE: <statusbar-module> record to update.
  - OUTPUT: <output> record or #f forwarded to #:update-fn."
  (when (and (statusbar-module? module)
             (procedure? (statusbar-module-update-fn module)))
    (statusbar-worker-start!)
    (with-mutex %statusbar-worker-mutex
      ;; if MODULE already has a pending job, replace its output
      ;; rather than queuing duplicates
      (let ((existing (find (lambda (entry)
                              (eq? (car entry) module))
                            %statusbar-worker-queue)))
        (if existing
            (set-cdr! existing output)
            (set! %statusbar-worker-queue
                  (append %statusbar-worker-queue (list (cons module output))))))
      ;; notify worker thread that work is available
      (signal-condition-variable %statusbar-worker-cond))))

(define *statusbar-render-request-hook* (make-gliver-hook 'statusbar-render-request 0))
(define *statusbar-active-module-hooks* '())

(define (statusbar-request-render!)
  "Request a re-render of all statusbars.
Parameters: None."
  ;; Wayland/SHM commits must run on the main thread
  ;; worker callers only set the dirty flag
  (if (eq? (current-thread)
           (statusbar-worker-thread))
      (statusbar-mark-render-dirty!)
      (gliver-hook-run! *statusbar-render-request-hook*)))

(define (statusbar-make-default-module-hook-handler module)
  "Create handler to dispatche MODULE's update onto the worker thread.
Parameters: (MODULE)
  - MODULE: <statusbar-module> record.
Returns hook handler procedure."
  ;; handle to any hook signature
  (lambda args
    (let ((update-fn (statusbar-module-update-fn module)))
      (if (procedure? update-fn)
          ;; module has #:update-fn, run it
          (statusbar-module-update! module (output-current))
          ;; module is has no #:update-fn
          ;; trigger redraw directly
          (statusbar-request-render!)))))

(define (statusbar-attach-hook! hook handler)
  "Attach HANDLER to HOOK.
Parameters: (HOOK HANDLER)
  - HOOK: <gliver-hook> instance.
  - HANDLER: Procedure callback to register."
  (gliver-hook-add! hook handler)
  (set! *statusbar-active-module-hooks*
        (cons (cons hook handler) *statusbar-active-module-hooks*)))

(define (statusbar-bind-module-hook! module hook)
  "Bind HOOK declared by MODULE.
Parameters: (MODULE HOOK)
  - MODULE: <statusbar-module> record.
  - HOOK: <gliver-hook> instance."
  (if (gliver-hook? hook)
      (statusbar-attach-hook! hook (statusbar-make-default-module-hook-handler module))
      (log-warn "Invalid statusbar module event-hook ~a in ~a"
                hook (statusbar-module-id module))))

(define (statusbar-detach-module-hooks!)
  "Detach all active event hooks currently registered by statusbar modules.
Parameters: None."
  (for-each
   (lambda (binding)
     (let ((hook (car binding))
           (handler (cdr binding)))
       (when (gliver-hook? hook)
         (gliver-hook-remove! hook handler))))
   *statusbar-active-module-hooks*)
  (set! *statusbar-active-module-hooks* '()))

(define (statusbar-attach-module-hooks! modules)
  "Attach event hooks for the given list of MODULES.
Parameters: (MODULES)
  - MODULES: List of <statusbar-module> records."
  ;; clear prior bindings first so reload/re-attach never has duplicate listeners
  (statusbar-detach-module-hooks!)
  (for-each
   (lambda (module)
     (let ((hooks (statusbar-module-event-hooks module)))
       (when (list? hooks)
         (for-each (lambda (hook)
                     (statusbar-bind-module-hook! module hook))
                   hooks))))
   ;; deduplicate module instances so a shared instance registers each hook only once
   (delete-duplicates (filter statusbar-module? modules))))
