;;; gliver/contrib/ui/gleui/base/repeat.scm --- Key Repeat Worker
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module manages the shared background worker thread the manages key
;;; auto-repeat for interactive gleui inputs (like palette text input).
;;; It handles:
;;;  - Lazy worker thread creation
;;;  - Rate and initial delay configuration per active key hold

(define-module (gliver contrib ui gleui base repeat)
  #:use-module (ice-9 match)
  #:use-module (ice-9 threads)
  #:declarative? #f
  #:export (
            gleui-repeat-worker-start!
            gleui-repeat-worker-stop!
            gleui-repeat-worker-signal!
))

;; NOTE: these variables will be overwritten, so they don't need to inherit
;; mutex for protecting read/write to thread variables
(define %repeat-mutex (make-mutex))
;; wake-up signal that lets repeat worker thread sleep efficiently
(define %repeat-cond (make-condition-variable))
;; indicates whether the repeat thread is active
(define %repeat-active? #f)
;; custom state to be passed to handler - check <gleui-palette-state>
(define %repeat-state #f)
;; vector of 3 elements (keycode keysym urf8-str)
;; NOTE: utf-8-str maybe #f for keys like BackSpace
(define %repeat-params #f)
;; callback invoked on each tick - should expect 2 params (handler state params)
(define %repeat-handler #f)
;; the rate of key presses per second

(define %repeat-rate 25)
;; the initial delay in ms to start repeating
(define %repeat-delay 600)
;; predicate checked on every tick, caller can use it to cancel repeat
(define %repeat-active-pred #f)
;; used as a guard check to ensure a repeat will stop if another one stars
(define %repeat-seq 0)
;; to ensure the repeat worker is only activated once
(define %repeat-worker-started? #f)

(define (gleui-repeat-worker-start!)
  "Start the shared persistent background worker thread for
key auto-repeat if not running.
Parameters: None."
  (define (%start-worker-thread!)
    "Spawn the background thread executing key-repeat loops."
    (call-with-new-thread
     (lambda ()
       (define (%wait-for-activation)
         "Wait for `%repeat-cond' until key repeat becomes active."
         (with-mutex %repeat-mutex
           (while (not %repeat-active?)
             (wait-condition-variable %repeat-cond %repeat-mutex))
           (list %repeat-seq
                 %repeat-state
                 %repeat-params
                 %repeat-handler
                 %repeat-active-pred
                 (max 1 (or %repeat-rate 25))
                 (max 50 (or %repeat-delay 600)))))

       (define (%still-active? seq pred)
         "Verify that repeat job SEQ is still active."
         (with-mutex %repeat-mutex
           (and %repeat-active?
                (= %repeat-seq seq)
                (or (not (procedure? pred))
                    (catch #t pred (lambda _ #f))))))

       (let loop ()
         ;; this will only return upon pressing a key
         (match-let (((seq state params handler active-pred rate delay-ms)
                      (%wait-for-activation)))
           ;; start sleep with initial delay
           (usleep (* delay-ms 1000))
           (let ((interval-us (inexact->exact (round (/ 1000000.0 rate)))))
             (let repeat-loop ()
               ;; still-active is used so no other key is pressed while
               ;; waiting for initial delay (seq will increment in that case)
               (when (%still-active? seq active-pred)
                 (when (procedure? handler)
                   ;; if we have a handler, send it the state and params
                   (catch #t (lambda () (handler state params)) (lambda _ #f)))
                 ;; start sleep for repeat rate
                 (usleep interval-us)
                 (repeat-loop)))))
         (loop)))))

  ;; start the worker thread only if it's not started already
  ;; to prevent multiple workers and repeats
  (with-mutex %repeat-mutex
    (unless %repeat-worker-started?
      (set! %repeat-worker-started? #t)
      (%start-worker-thread!))))

(define (gleui-repeat-worker-stop!)
  "Stop any active key-repeat job and increment the cancellation sequence counter.
Parameters: None."
  (with-mutex %repeat-mutex
    (set! %repeat-seq (1+ %repeat-seq))
    (set! %repeat-active? #f)
    (set! %repeat-state #f)
    (set! %repeat-handler #f)
    (set! %repeat-active-pred #f)))

(define* (gleui-repeat-worker-signal! state params handler #:key (rate 25) (delay 600) (active-pred #f))
  "Signal the key-repeat worker thread to repeat PARAMS on STATE via HANDLER.
Parameters: (STATE PARAMS HANDLER [#:RATE 25] [#:DELAY 600] [#:ACTIVE-PRED #f])
  - STATE: Value component state passed to HANDLER on each repeat tick.
  - PARAMS: Value repeat payload passed as the second argument to HANDLER.
  - HANDLER: Procedure called as (HANDLER STATE PARAMS).
  - RATE: Integer repeat rate per second.
  - DELAY: Integer initial key hold delay before repeating in ms.
  - ACTIVE-PRED: Procedure or #f zero-argument predicate checked before each repeat tick."
  (with-mutex %repeat-mutex
    (set! %repeat-active? #t)
    (set! %repeat-state state)
    (set! %repeat-params params)
    (set! %repeat-handler handler)
    (set! %repeat-rate rate)
    (set! %repeat-delay delay)
    (set! %repeat-active-pred active-pred)
    (signal-condition-variable %repeat-cond)))
