;;; gliver/contrib/ui/gleui/base/pointer.scm --- Wayland Pointer Input & Hit-Box
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module manages Wayland pointer listeners and hit-box event dispatch.
;;; It handles:
;;;  - Tracking pointer surface focus and surface-local coordinates
;;;  - Tracking hovered hit-boxes and dispatching enter/leave callbacks
;;;  - Handling click and scroll events and routing them to widget hit-boxes

(define-module (gliver contrib ui gleui base pointer)
  #:use-module (system foreign)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-9)
  #:use-module (gliver core)
  #:use-module (gliver wayland client)
  #:use-module (gliver wayland gen wayland)
  #:use-module (gliver river connector)
  #:declarative? #f
  #:export (
            *gleui-pointer-surface*
            *gleui-pointer-x*
            *gleui-pointer-y*
            *gleui-pointer-seat-proxies*
            <gleui-hit-box>
            gleui-make-hit-box
            gleui-hit-box?
            gleui-hit-box-x1
            gleui-hit-box-y1
            gleui-hit-box-x2
            gleui-hit-box-y2
            gleui-hit-box-data
            gleui-hit-box-on-click
            gleui-hit-box-on-scroll
            gleui-hit-box-on-enter
            gleui-hit-box-on-leave
            gleui-find-hit-boxes
            gleui-find-hit-box
            gleui-pointer-register-surface!
            gleui-pointer-unregister-surface!
            gleui-pointer-cleanup!
            gleui-pointer-setup!
))

(define-var *gleui-pointer-surface* %null-pointer
            "Currently focused Wayland surface under the pointer, or %null-pointer.")
(define-var *gleui-pointer-x* 0.0
            "Current surface-local pointer X coordinate in logical pixels.")
(define-var *gleui-pointer-y* 0.0
            "Current surface-local pointer Y coordinate in logical pixels.")
(define-var *gleui-pointer-seat-proxies* '()
            "List of seat proxy vectors #(wl-seat wl-pointer seat-listener ptr-listener active?).")
(define %pointer-surfaces (make-hash-table))
(define %hovered-surface %null-pointer)
(define %hovered-hit-boxes '())

;; pointer hit-box record initialized during widget drawing
;;  - x1: left logical coordinate in surface space
;;  - y1: top logical coordinate in surface space
;;  - x2: right logical coordinate in surface space
;;  - y2: bottom logical coordinate in surface space
;;  - data: component payload associated with the interactive region
;;  - on-click: procedure (button px py data) invoked on pointer click, or #f
;;  - on-scroll: procedure (axis delta data) invoked on pointer scroll, or #f
;;  - on-enter: procedure (px py data) invoked when pointer enters the region, or #f
;;  - on-leave: procedure (data) invoked when pointer leaves the region, or #f
(define-record-type <gleui-hit-box>
  (%make-hit-box x1 y1 x2 y2 data on-click on-scroll on-enter on-leave)
  gleui-hit-box?
  (x1        gleui-hit-box-x1)
  (y1        gleui-hit-box-y1)
  (x2        gleui-hit-box-x2)
  (y2        gleui-hit-box-y2)
  (data      gleui-hit-box-data)
  (on-click  gleui-hit-box-on-click)
  (on-scroll gleui-hit-box-on-scroll)
  (on-enter  gleui-hit-box-on-enter)
  (on-leave  gleui-hit-box-on-leave))

(define* (gleui-make-hit-box x1 y1 x2 y2 data on-click on-scroll
                             #:optional (on-enter #f) (on-leave #f))
  "Create a <gleui-hit-box> record for pointer hit testing.
Parameters: (X1 Y1 X2 Y2 DATA ON-CLICK ON-SCROLL [ON-ENTER #f] [ON-LEAVE #f])
  - X1: Float left surface coordinate in pixels.
  - Y1: Float top surface coordinate in pixels.
  - X2: Float right surface coordinate in pixels.
  - Y2: Float bottom surface coordinate in pixels.
  - DATA: Value payload forwarded to pointer callbacks.
  - ON-CLICK: Procedure or #f callback invoked as (ON-CLICK BUTTON PX PY DATA).
  - ON-SCROLL: Procedure or #f callback invoked as (ON-SCROLL AXIS DELTA DATA).
  - ON-ENTER: Procedure or #f callback invoked as (ON-ENTER PX PY DATA).
  - ON-LEAVE: Procedure or #f callback invoked as (ON-LEAVE DATA).
Returns <gleui-hit-box> record."
  (%make-hit-box x1 y1 x2 y2 data on-click on-scroll on-enter on-leave))

(define (%hit-box-contains? box px py)
  "Check whether hit-box BOX contains point (PX, PY).
Used to return all hit-boxes under PX and PY regardless of order.
Parameters: (BOX PX PY)
  - BOX: <gleui-hit-box> record.
  - PX: Float surface-local pointer X coordinate in pixels.
  - PY: Float surface-local pointer Y coordinate in pixels.
Returns #t if (PX, PY) lies within BOX bounds, #f otherwise."
  (and (>= px (gleui-hit-box-x1 box))
       (<= px (gleui-hit-box-x2 box))
       (>= py (gleui-hit-box-y1 box))
       (<= py (gleui-hit-box-y2 box))))

(define (gleui-find-hit-boxes hit-boxes px py)
  "Locate all interactive regions in HIT-BOXES containing point (PX, PY).
Parameters: (HIT-BOXES PX PY)
  - HIT-BOXES: List of <gleui-hit-box> records in paint order, or #f.
  - PX: Float surface local pointer X coordinate in pixels.
  - PY: Float surface local pointer Y coordinate in pixels.
Returns list of matching <gleui-hit-box> records in topmost-first order."
  (filter (lambda (box) (%hit-box-contains? box px py))
          (reverse (or hit-boxes '()))))

(define (gleui-find-hit-box hit-boxes px py)
  "Locate the topmost interactive region in HIT-BOXES containing point (PX, PY).
Parameters: (HIT-BOXES PX PY)
  - HIT-BOXES: List of <gleui-hit-box> records in paint order, or #f.
  - PX: Float surface local pointer X coordinate in pixels.
  - PY: Float surface local pointer Y coordinate in pixels.
Returns matching <gleui-hit-box> record, or #f if none match."
  (let ((matches (gleui-find-hit-boxes hit-boxes px py)))
    (and (pair? matches) (car matches))))

(define (%same-hit-box? a b)
  "Check whether hit-boxes A and B represent the same region.
Parameters: (A B)
  - A: <gleui-hit-box> record or #f.
  - B: <gleui-hit-box> record or #f.
Returns #t if A and B match in bounds and payload, #f otherwise."
  (and a b
       (or (eq? a b)
           (and (= (gleui-hit-box-x1 a) (gleui-hit-box-x1 b))
                (= (gleui-hit-box-y1 a) (gleui-hit-box-y1 b))
                (= (gleui-hit-box-x2 a) (gleui-hit-box-x2 b))
                (= (gleui-hit-box-y2 a) (gleui-hit-box-y2 b))
                (let ((da (gleui-hit-box-data a))
                      (db (gleui-hit-box-data b)))
                  (or (eq? da db)
                      (and (not (struct? da))
                           (not (struct? db))
                           (equal? da db))))))))

(define* (gleui-pointer-register-surface! surface hit-boxes-proc
                                          #:key (redraw-proc #f))
  "Register Wayland SURFACE for hit-box dispatch using HIT-BOXES-PROC.
Parameters: (SURFACE HIT-BOXES-PROC [#:REDRAW-PROC #f])
  - SURFACE: Pointer to Wayland wl_surface.
  - HIT-BOXES-PROC: Procedure of zero arguments that returns active <gleui-hit-box> list.
This is a proc as hit-boxes may change in a surface during its life-cycle.
  - REDRAW-PROC: Procedure or #f, zero-argument callback invoked after a handled event."
  (when (valid-pointer? surface)
    (hashv-set! %pointer-surfaces
                (pointer-address surface)
                (list surface hit-boxes-proc redraw-proc))))

(define (gleui-pointer-unregister-surface! surface)
  "Remove Wayland SURFACE from pointer hit-box dispatch.
Parameters: (SURFACE)
  - SURFACE: Pointer to Wayland wl_surface to unregister."
  (when (valid-pointer? surface)
    (when (and (valid-pointer? %hovered-surface)
               (= (pointer-address surface)
                  (pointer-address %hovered-surface)))
      (%clear-hovered-hits! #:redraw? #f))
    (when (and (valid-pointer? *gleui-pointer-surface*)
               (= (pointer-address surface)
                  (pointer-address *gleui-pointer-surface*)))
      (set! *gleui-pointer-surface* %null-pointer))
    (hashv-remove! %pointer-surfaces (pointer-address surface))))

(define (%find-focused-entry)
  "Locate the registered surface entry for the currently focused pointer surface.
Parameters: None.
Returns registered surface list (SURFACE HIT-BOXES-PROC REDRAW-PROC), or #f."
  (and-let* (((valid-pointer? *gleui-pointer-surface*)))
    (hashv-ref %pointer-surfaces (pointer-address *gleui-pointer-surface*))))

(define* (%dispatch-pointer-hit! #:key (button #f) (axis #f) (delta #f))
  "Dispatch a click or scroll event to matching hit-boxes and trigger redraw if handled.
Parameters: ([#:BUTTON #f] [#:AXIS #f] [#:DELTA #f])
  - BUTTON: Integer or #f Linux evdev button code for click dispatch.
  - AXIS: Integer or #f Wayland scroll axis for scroll dispatch.
  - DELTA: Float or #f scroll delta in pixels for scroll dispatch."
  (and-let* ((entry (%find-focused-entry)))
    (match-let (((_surf boxes-proc redraw-proc) entry))
      (and-let* ((boxes (and (procedure? boxes-proc)
                             (boxes-proc)))
                 (hits  (gleui-find-hit-boxes boxes *gleui-pointer-x* *gleui-pointer-y*))
                 ((pair? hits)))
        (let* ((click-hit  (and button
                                (find (lambda (h) (procedure? (gleui-hit-box-on-click h)))
                                      hits)))
               (scroll-hit (and axis delta
                                (find (lambda (h) (procedure? (gleui-hit-box-on-scroll h)))
                                      hits))))
          (when click-hit
            ((gleui-hit-box-on-click click-hit)
             button *gleui-pointer-x* *gleui-pointer-y* (gleui-hit-box-data click-hit)))
          (when scroll-hit
            ((gleui-hit-box-on-scroll scroll-hit)
             axis delta (gleui-hit-box-data scroll-hit)))
          (when (and (or click-hit scroll-hit)
                     (procedure? redraw-proc))
            (redraw-proc)))))))

(define* (%clear-hovered-hits! #:key (redraw? #t))
  "Invoke on-leave callbacks for all currently hovered hit-boxes and reset hover state.
Parameters: ([#:REDRAW? #t])
  - REDRAW?: Boolean invoking the hovered surface redraw callback when #t."
  (when (pair? %hovered-hit-boxes)
    (let* ((surf        %hovered-surface)
           (entry       (and (valid-pointer? surf)
                             (hashv-ref %pointer-surfaces (pointer-address surf))))
           (redraw-proc (and entry (caddr entry)))
           (old-hits    (reverse %hovered-hit-boxes))
           (handled?    #f))
      (set! %hovered-hit-boxes '())
      (set! %hovered-surface %null-pointer)
      (for-each
       (lambda (hit)
         (let ((on-leave (gleui-hit-box-on-leave hit)))
           (when (procedure? on-leave)
             (set! handled? #t)
             (on-leave (gleui-hit-box-data hit)))))
       old-hits)
      (when (and redraw?
                 handled?
                 (procedure? redraw-proc)
                 (valid-pointer? surf)
                 (hashv-ref %pointer-surfaces (pointer-address surf)))
        (redraw-proc)))))

(define (%sync-hovered-hits!)
  "Synchronize hovered hit-boxes with the current pointer surface and coordinates.
Parameters: None."
  (let ((surface *gleui-pointer-surface*)
        (px      *gleui-pointer-x*)
        (py      *gleui-pointer-y*))
    (when (and (valid-pointer? %hovered-surface)
               (or (not (valid-pointer? surface))
                   (not (= (pointer-address %hovered-surface)
                           (pointer-address surface)))))
      (%clear-hovered-hits!))
    (let ((entry (and (valid-pointer? surface)
                      (hashv-ref %pointer-surfaces (pointer-address surface)))))
      (if (not entry)
          (%clear-hovered-hits! #:redraw? #f)
          (match-let (((_surf boxes-proc redraw-proc) entry))
            (let* ((boxes        (or (and (procedure? boxes-proc) (boxes-proc)) '()))
                   ;; keep paint order (outermost to innermost) in %hovered-hit-boxes
                   (new-hits     (filter (lambda (box) (%hit-box-contains? box px py))
                                         boxes))
                   (old-hits     %hovered-hit-boxes)
                   ;; leave innermost-first, enter outermost-first
                   (left-hits    (filter (lambda (old)
                                           (not (any (lambda (new) (%same-hit-box? old new))
                                                     new-hits)))
                                         (reverse old-hits)))
                   (entered-hits (filter (lambda (new)
                                           (not (any (lambda (old) (%same-hit-box? new old))
                                                     old-hits)))
                                         new-hits))
                   (handled?     #f))
              (set! %hovered-surface surface)
              (set! %hovered-hit-boxes new-hits)
              (for-each
               (lambda (hit)
                 (let ((on-leave (gleui-hit-box-on-leave hit)))
                   (when (procedure? on-leave)
                     (set! handled? #t)
                     (on-leave (gleui-hit-box-data hit)))))
               left-hits)
              (for-each
               (lambda (hit)
                 (let ((on-enter (gleui-hit-box-on-enter hit)))
                   (when (procedure? on-enter)
                     (set! handled? #t)
                     (on-enter px py (gleui-hit-box-data hit)))))
               entered-hits)
              (when (and handled?
                         (procedure? redraw-proc)
                         (hashv-ref %pointer-surfaces (pointer-address surface)))
                (redraw-proc))))))))

(define (%pointer-detach! entry)
  "Release and clear any active wl_pointer on seat ENTRY.
Parameters: (ENTRY)
  - ENTRY: Vector #(wl-seat wl-pointer seat-listener ptr-listener active?)."
  (let ((wl-pointer (vector-ref entry 1)))
    (vector-set! entry 1 #f)
    (vector-set! entry 3 #f)
    (when (valid-pointer? wl-pointer)
      (catch #t (lambda () (wl-pointer-release wl-pointer)) (lambda _ #f)))))

(define (gleui-pointer-cleanup!)
  "Release bound Wayland seat and pointer proxies and clear registered surfaces.
Used for cleanup and destroying allocated Wayland objects.
Parameters: None."
  (for-each
   (lambda (entry)
     (when (vector? entry)
       ;; #(wl-seat wl-pointer seat-listener ptr-listener active?)
       (vector-set! entry 4 #f)     ; deactivate
       (%pointer-detach! entry)
       (let ((wl-seat (vector-ref entry 0)))
         (vector-set! entry 0 #f)
         (when (valid-pointer? wl-seat)
           (catch #t (lambda () (wl-seat-release wl-seat)) (lambda _ #f))))))
   *gleui-pointer-seat-proxies*)

  (set! *gleui-pointer-seat-proxies* '())
  (set! *gleui-pointer-surface* %null-pointer)
  (set! *gleui-pointer-x* 0.0)
  (set! *gleui-pointer-y* 0.0)
  (set! %hovered-surface %null-pointer)
  (set! %hovered-hit-boxes '())
  (hash-clear! %pointer-surfaces))

(define (gleui-pointer-setup!)
  "Bind wl_pointer on active seats when not already attached."
  ;; NOTE: this composed of several helper functions for simplicity

  (define (%handle-enter data proxy serial surface sx sy)
    "Invoked when the pointer enters the SURFACE.
NOTE: this isn't triggered again when SX and SY change (handle-motion).
NOTE: all coordinates are in 24.8 format which is an int32 with 24+8 bits
      which allows representing floats in int format (must divide by 256)
Parameters: (DATA PROXY SERIAL SURFACE SX SY)
  - DATA: Pointer to unused listener user data.
  - PROXY: Pointer to Wayland wl_pointer proxy.
  - SERIAL: Integer Wayland enter event serial (unused).
  - SURFACE: Pointer to focused Wayland wl_surface.
  - SX: Integer fixed-point surface X coordinate (24.8 format).
  - SY: Integer fixed-point surface Y coordinate (24.8 format)."
    ;; update pointer focused surface and its local coordinates.
    (when (and (valid-pointer? %hovered-surface)
               (or (not (valid-pointer? surface))
                   (not (= (pointer-address %hovered-surface)
                           (pointer-address surface)))))
      (%clear-hovered-hits!))
    (set! *gleui-pointer-surface* surface)
    (set! *gleui-pointer-x* (/ sx 256.0))
    (set! *gleui-pointer-y* (/ sy 256.0))
    (%sync-hovered-hits!))

  (define (%handle-leave data proxy serial surface)
    "Invoked when the pointer leaves the SURFACE.
Parameters: (DATA PROXY SERIAL SURFACE)
  - DATA: Pointer to unused listener user data.
  - PROXY: Pointer to Wayland wl_pointer proxy.
  - SERIAL: Integer Wayland leave event serial (unused).
  - SURFACE: Pointer to departed Wayland wl_surface."
    ;; clear the last set pointer surface and any hovered hit-boxes on it
    (when (or (not (valid-pointer? surface))
              (not (valid-pointer? *gleui-pointer-surface*))
              (= (pointer-address surface)
                 (pointer-address *gleui-pointer-surface*)))
      (%clear-hovered-hits!)
      (set! *gleui-pointer-surface* %null-pointer)))

  (define (%handle-motion data proxy time sx sy)
    "Invoked when the current surface position SX and SY change.
Parameters: (DATA PROXY TIME SX SY)
  - DATA: Pointer to unused listener user data.
  - PROXY: Pointer to Wayland wl_pointer proxy.
  - TIME: Integer millisecond event timestamp (unused).
  - SX: Integer fixed-point surface X coordinate (24.8 format).
  - SY: Integer fixed-point surface Y coordinate (24.8 format)."
    ;; update last pointer position x and y and dispatch enter/leave transitions
    (set! *gleui-pointer-x* (/ sx 256.0))
    (set! *gleui-pointer-y* (/ sy 256.0))
    (%sync-hovered-hits!))

  (define (%handle-button data proxy serial time button state-val)
    "Invoked when the current surface receives a button click.
Parameters: (DATA PROXY SERIAL TIME BUTTON STATE-VAL)
  - DATA: Pointer to unused listener user data.
  - PROXY: Pointer to Wayland wl_pointer proxy.
  - SERIAL: Integer Wayland button event serial (unused).
  - TIME: Integer millisecond event timestamp (unused).
  - BUTTON: Integer Linux evdev button code.
  - STATE-VAL: Integer Wayland button state (1 = pressed, 0 = released)."
    (when (= state-val 1) ;; we trigger with press, released is unused.
      (%dispatch-pointer-hit! #:button button)))

  (define (%handle-axis data proxy time axis value)
    "Invoked when the current surface receives a scroll event.
Parameters: (DATA PROXY TIME AXIS VALUE)
  - DATA: Pointer to unused listener user data.
  - ROXY: Pointer to Wayland wl_pointer proxy.
  - TIME: Integer millisecond event timestamp (unused).
  - AXIS: Integer axis identifier (0 = vertical scroll).
  - VALUE: Integer fixed-point scroll delta (24.8 format)."
    (when (zero? axis)
      (%dispatch-pointer-hit! #:axis axis #:delta (/ value 256.0))))

  (define (%noop-handler . _args)
    "Invoked on the rest of the wl-pointer-listener events that are not used."
    #t)

  (define (%attach-pointer! entry)
    "Bind wl_pointer for seat ENTRY and attach pointer event listeners.
Parameters: (ENTRY)
  - ENTRY: Vector #(wl-seat wl-pointer seat-listener ptr-listener active?)."
    (and-let* ((wl-seat (vector-ref entry 0))
               (ptr     (wl-seat-get-pointer wl-seat))
               ((valid-pointer? ptr)))
      (let ((listener (make-wl-pointer-listener %handle-enter
                                                %handle-leave
                                                %handle-motion
                                                %handle-button
                                                %handle-axis
                                                %noop-handler
                                                %noop-handler
                                                %noop-handler
                                                %noop-handler
                                                %noop-handler
                                                %noop-handler)))
        (wl-proxy-add-listener ptr listener %null-pointer)
        (vector-set! entry 1 ptr)
        (vector-set! entry 3 listener))))

  (define (%on-seat-capabilities entry caps)
    "Attach or detach wl_pointer for seat ENTRY when pointer capability CAPS changes.
Parameters: (ENTRY CAPS)
  - ENTRY: Vector #(wl-seat wl-pointer seat-listener ptr-listener active?).
  - CAPS: Integer wl_seat capability bitmask."
    (when (vector-ref entry 4)
      ;; 1 on capabilities bitmask means seat has pointer capability
      (let ((has-ptr? (not (zero? (logand caps 1))))
            ;; #t if wl-pointer is already attached to this seat
            (has-cur? (valid-pointer? (vector-ref entry 1))))
        (cond
         ;; we only attach if we current don't have a wl-pointer and cap shows pointer
         ((and has-ptr? (not has-cur?)) (%attach-pointer! entry))
         ;; detach if caps say seat has no pointer and we already have a wl-pointer
         ((and (not has-ptr?) has-cur?) (%pointer-detach! entry))))))

  (define (%bind-seat! seat)
    "Bind wl_seat for SEAT and listen for pointer capability changes.
Parameters: (SEAT)
  - SEAT: <seat> record from *manager*."
    (and-let* (((seat? seat))
               (seat-id (seat-wl-seat seat))
               ((number? seat-id))
               (wl-seat (gliver-wl-registry-bind *wl-registry* seat-id *wl-seat-interface* 7))
               ((valid-pointer? wl-seat)))
      ;; not the best approach, but it makes code easier
      ;; entry vector is updated when it advertises capabilities
      (let* ((entry (vector wl-seat #f #f #f #t))
             (seat-listener (make-wl-seat-listener
                             (lambda (_data _proxy caps)
                               (%on-seat-capabilities entry caps))
                             %noop-handler)))
        (vector-set! entry 2 seat-listener)
        (wl-proxy-add-listener wl-seat seat-listener %null-pointer)
        (set! *gleui-pointer-seat-proxies*
              (cons entry *gleui-pointer-seat-proxies*)))))

  (when (and (null? *gleui-pointer-seat-proxies*)
             (valid-pointer? *wl-registry*)
             *manager*)
    ;; bind each seat to detect its pointer events
    (for-each %bind-seat!
              (manager-seats *manager*))))

(gliver-hook-add! *gliver-globals-unbind-hook* 'gleui-pointer-cleanup!)
