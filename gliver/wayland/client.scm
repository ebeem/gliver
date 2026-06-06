;;; gliver/wayland/client.scm --- Wayland client bindings via Guile FFI
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module provides low-level Wayland client bindings using Guile's
;;; FFI (system foreign).  It wraps libwayland-client functions for
;;; connecting to a display, managing the registry, and dispatching events.

(define-module (gliver wayland client)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (gliver core logs)
  #:export (;; display
            wl-display-connect
            wl-display-disconnect
            wl-display-get-fd
            wl-display-dispatch
            wl-display-dispatch-pending
            wl-display-roundtrip
            wl-display-flush
            gliver-wl-display-get-registry

            ;; registry
            gliver-wl-registry-bind

            ;; proxy
            wl-proxy-destroy
            wl-proxy-get-id
            wl-proxy-marshal-flags

            ;; event queue
            wl-display-prepare-read
            wl-display-cancel-read
            wl-display-read-events

            ;; listener framework
            make-wl-listener
            wl-proxy-add-listener

            ;; interface construction
            make-wl-interface
            make-wl-interface-full

            ;; gc protection
            wl-gc-protect!

            ;; proxy version
            wl-proxy-get-version

            ;; Array-based marshaling
            wl-marshal-request
            wl-marshal-request-destroy
            wl-marshal-constructor
))

;;; load libwayland-client
(define libwayland-client
  (catch #t
    (lambda ()
      (dynamic-link "libwayland-client"))
    (lambda (key . args)
      (log-error "Failed to load libwayland-client: ~a ~a" key args)
      (log-error "Make sure libwayland-client is installed.")
      #f)))

(define %null-pointer (make-pointer 0))

;;; helper to look up a function, returns a dummy if the lib is unavailable
(define (wl-func name return-type arg-types)
  (if libwayland-client
      (pointer->procedure return-type
                          (dynamic-func name libwayland-client)
                          arg-types)
      (lambda args
        (error (format #f "libwayland-client not loaded, cannot call ~a" name)))))

;;; display
(define %wl-display-connect
  (wl-func "wl_display_connect" '* (list '*)))

(define (wl-display-connect . name)
  "Connect to a Wayland display.  If NAME is omitted or #f, uses WAYLAND_DISPLAY."
  (let ((name-ptr (if (and (pair? name) (car name))
                      (string->pointer (car name))
                      %null-pointer)))
    (let ((display (%wl-display-connect name-ptr)))
      (if (null-pointer? display)
          (error "Failed to connect to Wayland display")
          display))))

(define %wl-display-disconnect
  (wl-func "wl_display_disconnect" void (list '*)))

(define (wl-display-disconnect display)
  (%wl-display-disconnect display))

(define %wl-display-get-fd
  (wl-func "wl_display_get_fd" int (list '*)))

(define (wl-display-get-fd display)
  (%wl-display-get-fd display))

(define %wl-display-dispatch
  (wl-func "wl_display_dispatch" int (list '*)))

(define (wl-display-dispatch display)
  (%wl-display-dispatch display))

(define %wl-display-dispatch-pending
  (wl-func "wl_display_dispatch_pending" int (list '*)))

(define (wl-display-dispatch-pending display)
  (%wl-display-dispatch-pending display))

(define %wl-display-roundtrip
  (wl-func "wl_display_roundtrip" int (list '*)))

(define (wl-display-roundtrip display)
  (%wl-display-roundtrip display))

(define %wl-display-flush
  (wl-func "wl_display_flush" int (list '*)))

(define (wl-display-flush display)
  (%wl-display-flush display))

(define %wl-display-prepare-read
  (wl-func "wl_display_prepare_read" int (list '*)))

(define (wl-display-prepare-read display)
  (%wl-display-prepare-read display))

(define %wl-display-cancel-read
  (wl-func "wl_display_cancel_read" void (list '*)))

(define (wl-display-cancel-read display)
  (%wl-display-cancel-read display))

(define %wl-display-read-events
  (wl-func "wl_display_read_events" int (list '*)))

(define (wl-display-read-events display)
  (%wl-display-read-events display))

;;; registry
;;; wl_display_get_registry and wl_registry_bind are inline functions
;;; in the c headers, not exported symbols.  we reimplement them using
;;; wl_proxy_marshal_flags.
(define %wl-proxy-get-version
  (wl-func "wl_proxy_get_version" uint32 (list '*)))

;; wl_registry_interface is an exported data symbol
(define %wl-registry-interface
  (dynamic-pointer "wl_registry_interface" libwayland-client))

;; TODO: these should be used from wayland.scm, the auto generated
;; definitions are not explicit, I probably should check guile-wayland
;; wl_display_get_registry opcode = 1 (sync=0, get_registry=1)
(define (gliver-wl-display-get-registry display)
  "Get the registry from a Wayland display.
This reimplements the inline C function using wl_proxy_marshal_flags."
  (let ((version (%wl-proxy-get-version display)))
    (%wl-proxy-marshal-flags display 1 %wl-registry-interface version 0)))

(define (gliver-wl-registry-bind registry name interface version)
  "Bind a registry object.  INTERFACE is a pointer to a wl_interface struct.
NAME is the global name (uint32).  VERSION is the desired version."
  ;; the wl_interface struct starts with a const char* name field.
  ;; we dereference it once to get the char* pointer, then convert to string.
  (let ((interface-name-str (pointer->string (dereference-pointer interface))))
    (wl-marshal-constructor registry 0 interface version
                            (list (cons 'uint name)
                                  (cons 'string interface-name-str)
                                  (cons 'uint version)
                                  (cons 'new-id 0)))))

;;; proxy
(define %wl-proxy-destroy
  (wl-func "wl_proxy_destroy" void (list '*)))

(define (wl-proxy-destroy proxy)
  (%wl-proxy-destroy proxy))

(define %wl-proxy-get-id
  (wl-func "wl_proxy_get_id" uint32 (list '*)))

(define (wl-proxy-get-id proxy)
  (%wl-proxy-get-id proxy))

(define %wl-proxy-add-listener
  (wl-func "wl_proxy_add_listener" int (list '* '* '*)))

(define (wl-proxy-add-listener proxy listener data)
  (%wl-proxy-add-listener proxy listener data))

(define %wl-proxy-marshal-flags
  (wl-func "wl_proxy_marshal_flags" '* (list '* uint32 '* uint32 uint32)))

(define (wl-proxy-marshal-flags proxy opcode interface version flags . args)
  ;; real implementation needs variadic ffi
  (%wl-proxy-marshal-flags proxy opcode interface version flags))

;;; interface construction
;;; A wl_interface is:
;;;   struct wl_interface {
;;;       const char *name;
;;;       int version;
;;;       int method_count;
;;;       const struct wl_message *methods;
;;;       int event_count;
;;;       const struct wl_message *events;
;;;   };

(define (make-wl-interface name version)
  "Create a minimal wl_interface struct with NAME and VERSION.
Methods and events are set to NULL/0, sufficient for wl_registry_bind."
  (let* ((name-ptr (wl-gc-protect! (string->pointer name)))
         ;; 6 fields: name*, version (int), method_count (int),
         ;;           methods*, event_count (int), events*
         ;; use a bytevector large enough for the struct
         (ptr-size (sizeof '*))
         (int-size (sizeof int))
         ;; layout: char* name, int version, int method_count,
         ;;         wl_message* methods, int event_count, wl_message* events
         ;; on 64-bit: 8 + 4 + 4 + 8 + 4 + 4(pad) + 8 = use aligned struct
         (bv (wl-gc-protect!
              (make-bytevector (+ ptr-size    ; name
                                  int-size    ; version
                                  int-size    ; method_count
                                  ptr-size    ; methods
                                  int-size    ; event_count
                                  int-size    ; padding
                                  ptr-size)   ; events (may need ptr alignment)
                               0))))
    ;; name
    (bytevector-uint-set! bv 0 (pointer-address name-ptr)
                          (native-endianness) ptr-size)
    ;; version
    (bytevector-sint-set! bv ptr-size version
                          (native-endianness) int-size)
    ;; method_count = 0, methods = NULL, event_count = 0, events = NULL
    ;; already zeroed by make-bytevector
    (bytevector->pointer bv)))

;;; listener framework
;;; in wayland, listeners are structs of function pointers.
;;; we create them by allocating memory for the function pointer array
;;; and populating it with guile procedure->pointer conversions.
(define (make-wl-listener callbacks)
  "Create a Wayland listener from a list of callback procedures.
Each callback should be a (procedure return-type arg-types) triple,
or a Guile procedure that will be wrapped.
Returns a pointer to the listener struct."
  (let* ((n (length callbacks))
         (bv (wl-gc-protect! (make-bytevector (* n (sizeof '*)) 0))))
    (let loop ((i 0) (cbs callbacks))
      (when (pair? cbs)
        (let* ((cb (car cbs))
               (proc-ptr (if (pointer? cb)
                             cb
                             ;; create a c-callable function pointer
                             (procedure->pointer void cb (list '* '*)))))
          (wl-gc-protect! proc-ptr)
          (bytevector-uint-set! bv (* i (sizeof '*))
                                (pointer-address proc-ptr)
                                (native-endianness) (sizeof '*))
          (loop (1+ i) (cdr cbs)))))
    (bytevector->pointer bv)))

;;; gc protection
;;; wayland structs are backed by bytevectors whose pointers are
;;; stored in c-side data structures.  the gc cannot trace those
;;; pointers, so we must prevent collection explicitly.
(define *wl-gc-roots* '())
(define (wl-gc-protect! obj)
  "Prevent OBJ from being garbage collected."
  (set! *wl-gc-roots* (cons obj *wl-gc-roots*))
  obj)

;;; proxy version
(define (wl-proxy-get-version proxy)
  "Return the protocol version of PROXY."
  (%wl-proxy-get-version proxy))

;;; array-based marshaling
;;; wl_proxy_marshal_array_flags is the non-variadic counterpart of
;;; wl_proxy_marshal_flags.  it takes a wl_argument[] array instead of
;;; variadic arguments, which is much friendlier for guile's ffi.
(define %wl-proxy-marshal-array-flags
  (if libwayland-client
      (pointer->procedure '*
        (dynamic-func "wl_proxy_marshal_array_flags" libwayland-client)
        (list '* uint32 '* uint32 uint32 '*))
      (lambda args
        (error "libwayland-client not loaded, cannot call wl_proxy_marshal_array_flags"))))

(define (make-wl-args args)
  "Create a wl_argument bytevector from a list of tagged argument values.
Each element is one of:
  ('uint . value)    uint32
  ('int . value)     int32
  ('object . ptr)    object reference (pointer)
  ('string . str)    C string pointer
  ('new-id . 0)      placeholder for new_id (filled by marshal)"
  (let* ((n (length args))
         (arg-size (sizeof '*))  ;; sizeof(wl_argument) = pointer size
         (bv (wl-gc-protect! (make-bytevector (* n arg-size) 0))))
    (let loop ((i 0) (rest args))
      (when (pair? rest)
        (let ((offset (* i arg-size)))
          (match (car rest)
            (('uint . val)
             (bytevector-uint-set! bv offset val (native-endianness) 4))
            (('int . val)
             (bytevector-sint-set! bv offset val (native-endianness) 4))
            (('object . ptr)
             (bytevector-uint-set! bv offset (pointer-address ptr)
                                   (native-endianness) (sizeof '*)))
            (('string . str)
             (let ((str-ptr (wl-gc-protect! (string->pointer str))))
               (bytevector-uint-set! bv offset (pointer-address str-ptr)
                                     (native-endianness) (sizeof '*))))
            (('fixed . val)
             (bytevector-sint-set! bv offset val (native-endianness) 4))
            (('fd . val)
             (bytevector-sint-set! bv offset val (native-endianness) 4))
            (('array . ptr)
             (bytevector-uint-set! bv offset (pointer-address ptr)
                                   (native-endianness) (sizeof '*)))
            (('new-id . _)
             #t)  ;; leave as zero, filled by marshal
            (_ (error "Unknown wl_argument type" (car rest)))))
        (loop (1+ i) (cdr rest))))
    bv))

(define (wl-marshal-request proxy opcode args-list)
  "Send a Wayland request that returns no new object.
OPCODE: the request opcode.
ARGS-LIST: list of tagged arguments for make-wl-args."
  (let ((args-bv (make-wl-args args-list)))
    (%wl-proxy-marshal-array-flags proxy opcode %null-pointer 0 0
                                    (bytevector->pointer args-bv))
    *unspecified*))

(define WL_MARSHAL_FLAG_DESTROY 1)

(define (wl-marshal-request-destroy proxy opcode)
  "Send a destructor request and destroy the client-side proxy."
  (let ((args-bv (make-bytevector 0 0)))
    (%wl-proxy-marshal-array-flags proxy opcode %null-pointer
                                    (wl-proxy-get-version proxy)
                                    WL_MARSHAL_FLAG_DESTROY
                                    (bytevector->pointer args-bv))
    *unspecified*))

(define (wl-marshal-constructor proxy opcode interface version args-list)
  "Send a Wayland request that creates a new object.
Returns the new proxy.
INTERFACE: wl_interface pointer for the new object.
VERSION: protocol version for the new object.
ARGS-LIST: list of tagged arguments for make-wl-args."
  (let ((args-bv (make-wl-args args-list)))
    (%wl-proxy-marshal-array-flags proxy opcode interface version 0
                                    (bytevector->pointer args-bv))))

;;; full interface construction
;;; a wl_message is { const char *name; const char *signature;
;;;                    const struct wl_interface **types; }
;;; = 3 pointers = 24 bytes on 64-bit.
;;;
;;; a wl_interface has methods and events arrays that are contiguous
;;; arrays of wl_message structs.
(define (make-wl-message-array messages)
  "Create a contiguous array of wl_message structs.
MESSAGES: list of (name signature types-pointers) triples.
TYPES-POINTERS: list of interface pointers (%null-pointer for non-object args).
Returns a pointer to the array."
  (let* ((msg-size (* 3 (sizeof '*)))  ;; sizeof(wl_message)
         (n (length messages))
         (array-bv (wl-gc-protect! (make-bytevector (* n msg-size) 0))))
    (let loop ((i 0) (msgs messages))
      (when (pair? msgs)
        (match (car msgs)
          ((msg-name signature types-list)
           (let* ((name-ptr (wl-gc-protect! (string->pointer msg-name)))
                  (sig-ptr (wl-gc-protect! (string->pointer signature)))
                  (n-types (length types-list))
                  (types-bv (if (zero? n-types)
                                #f
                                (wl-gc-protect!
                                 (make-bytevector (* n-types (sizeof '*)) 0))))
                  (offset (* i msg-size)))
             ;; Fill types array
             (when types-bv
               (let tloop ((j 0) (tl types-list))
                 (when (pair? tl)
                   (bytevector-uint-set! types-bv (* j (sizeof '*))
                                         (pointer-address (car tl))
                                         (native-endianness) (sizeof '*))
                   (tloop (1+ j) (cdr tl)))))
             ;; name
             (bytevector-uint-set! array-bv offset
                                   (pointer-address name-ptr)
                                   (native-endianness) (sizeof '*))
             ;; signature
             (bytevector-uint-set! array-bv (+ offset (sizeof '*))
                                   (pointer-address sig-ptr)
                                   (native-endianness) (sizeof '*))
             ;; types
             (bytevector-uint-set! array-bv (+ offset (* 2 (sizeof '*)))
                                   (pointer-address
                                    (if types-bv
                                        (bytevector->pointer types-bv)
                                        %null-pointer))
                                   (native-endianness) (sizeof '*)))))
        (loop (1+ i) (cdr msgs))))
    (bytevector->pointer array-bv)))

(define (make-wl-interface-full name version methods events)
  "Create a complete wl_interface struct with method and event tables.
NAME: interface name string.
VERSION: integer version.
METHODS: list of (name signature types-pointers) triples, in opcode order.
EVENTS: list of (name signature types-pointers) triples, in opcode order."
  (let* ((name-ptr (wl-gc-protect! (string->pointer name)))
         (ptr-size (sizeof '*))
         (int-size (sizeof int))
         (method-count (length methods))
         (methods-ptr (if (zero? method-count) %null-pointer
                          (make-wl-message-array methods)))
         (event-count (length events))
         (events-ptr (if (zero? event-count) %null-pointer
                         (make-wl-message-array events)))
         (bv (wl-gc-protect!
              (make-bytevector (+ ptr-size    ;; name       (offset 0)
                                  int-size    ;; version    (offset 8)
                                  int-size    ;; method_count (offset 12)
                                  ptr-size    ;; methods    (offset 16)
                                  int-size    ;; event_count (offset 24)
                                  int-size    ;; padding    (offset 28)
                                  ptr-size)   ;; events     (offset 32)
                               0))))
    ;; name (offset 0)
    (bytevector-uint-set! bv 0 (pointer-address name-ptr)
                          (native-endianness) ptr-size)
    ;; version (offset ptr-size)
    (bytevector-sint-set! bv ptr-size version
                          (native-endianness) int-size)
    ;; method_count (offset ptr-size + int-size)
    (bytevector-sint-set! bv (+ ptr-size int-size) method-count
                          (native-endianness) int-size)
    ;; methods (offset ptr-size + 2*int-size)
    (bytevector-uint-set! bv (+ ptr-size int-size int-size)
                          (pointer-address methods-ptr)
                          (native-endianness) ptr-size)
    ;; event_count (offset 2*ptr-size + 2*int-size)
    (bytevector-sint-set! bv (+ ptr-size int-size int-size ptr-size)
                          event-count
                          (native-endianness) int-size)
    ;; events (offset 2*ptr-size + 4*int-size)
    (bytevector-uint-set! bv (+ ptr-size int-size int-size ptr-size
                                int-size int-size)
                          (pointer-address events-ptr)
                          (native-endianness) ptr-size)
    (bytevector->pointer bv)))
