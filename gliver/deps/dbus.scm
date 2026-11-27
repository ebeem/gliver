;;; gliver/deps/dbus.scm --- D-Bus UNIX Socket Transport
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module implements D-Bus UNIX domain socket transport, SASL EXTERNAL
;;; authentication, and callback-based service routing.
;;; It handles:
;;;  - Locating and connecting to the D-Bus session bus socket
;;;  - Managing a <dbus-connection> with per-service callbacks
;;;  - Routing incoming calls, replies, errors, and signals to registered callbacks
;;;  - Encoding/decoding D-Bus primitives, headers, calls, replies, and signals

(define-module (gliver deps dbus)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 threads)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-9)
  #:use-module (gliver core logs)
  #:declarative? #f
  #:export (
            <dbus-connection>
            dbus-connection?
            dbus-connection-socket
            dbus-connection-in-buffer
            dbus-connection-in-buffer-set!
            dbus-connection-serial
            dbus-connection-serial-set!
            dbus-connection-pending-calls
            dbus-connection-path-services
            dbus-connection-service-callbacks
            dbus-connection-signal-routes
            dbus-connection-signal-routes-set!
            dbus-connection-sync-waiters
            dbus-connection-mutex
            dbus-connection-write-mutex
            dbus-connection-poll-thread
            dbus-connection-poll-thread-set!
            dbus-connection-stop-pipe
            dbus-connection-stop-pipe-set!
            dbus-connection-stop-requested?
            dbus-connection-stop-requested?-set!
            *dbus-session-connection*
            dbus-connection-next-serial!
            dbus-session-bus-path
            dbus-connect
            dbus-set-nonblock!
            dbus-connection-reset-buffer!
            dbus-close!
            dbus-send-all!
            dbus-call-with-bv-output
            dbus-put-align
            dbus-get-align
            dbus-safe-skip
            dbus-put-u32
            dbus-put-string
            dbus-put-signature
            dbus-encode-u32
            dbus-encode-string
            dbus-encode-string-array
            dbus-encode-strings
            dbus-get-u8
            dbus-get-u16
            dbus-get-u32
            dbus-get-s32
            dbus-get-s64
            dbus-get-u64
            dbus-get-double
            dbus-get-string
            dbus-get-signature
            dbus-get-string-array
            dbus-read-variant
            dbus-get-dict
            dbus-connection-append-buffer!
            dbus-build-message
            dbus-parse-header-fields
            dbus-unpack-message
            dbus-pop-message!
            dbus-read-next-message!
            dbus-dispatch-message!
            dbus-wait-reply
            dbus-send-call!
            dbus-call!
            dbus-hello!
            dbus-request-name!
            dbus-release-name!
            dbus-register-service!
            dbus-unregister-service!
            dbus-subscribe-signal!
            dbus-unsubscribe-signal!
            dbus-reply!
            dbus-error-reply!
            dbus-emit-signal!
            dbus-poll!
            dbus-start-poll!
            dbus-stop-poll!
            dbus-session-poll!
            dbus-session-disconnect!
            dbus-session-connect!
))

;; D-Bus connection state record
;;  - socket: open AF_UNIX socket port to the D-Bus daemon
;;  - in-buffer: bytevector accumulating partial incoming wire frames
;;  - serial: uint32 outgoing message serial counter
;;  - pending-calls: hash table (outgoing-serial -> callback-proc or service-name)
;;  - path-services: hash table (object-path -> service-name) for method call routing
;;  - service-callbacks: hash table (service-name -> callback-proc)
;;  - signal-routes: list of subscriptions
;;  - sync-waiters: hash table (outgoing-serial -> (condvar . result-box))
;;  - mutex: mutex protecting connection state
;;  - write-mutex: mutex serializing outgoing socket writes
;;  - poll-thread: background reader thread
;;  - stop-pipe: (read-port . write-port) wakeup pipe for poll-thread
;;  - stop-requested?: boolean signaling poll-thread to exit
(define-record-type <dbus-connection>
  (%make-dbus-connection socket in-buffer serial pending-calls path-services
                         service-callbacks signal-routes sync-waiters mutex write-mutex
                         poll-thread stop-pipe stop-requested?)
  dbus-connection?
  (socket            dbus-connection-socket)
  (in-buffer         dbus-connection-in-buffer         dbus-connection-in-buffer-set!)
  (serial            dbus-connection-serial            dbus-connection-serial-set!)
  (pending-calls     dbus-connection-pending-calls)
  (path-services     dbus-connection-path-services)
  (service-callbacks dbus-connection-service-callbacks)
  (signal-routes     dbus-connection-signal-routes     dbus-connection-signal-routes-set!)
  (sync-waiters      dbus-connection-sync-waiters)
  (mutex             dbus-connection-mutex)
  (write-mutex       dbus-connection-write-mutex)
  (poll-thread       dbus-connection-poll-thread       dbus-connection-poll-thread-set!)
  (stop-pipe         dbus-connection-stop-pipe         dbus-connection-stop-pipe-set!)
  (stop-requested?   dbus-connection-stop-requested?   dbus-connection-stop-requested?-set!))

(define (%make-empty-connection sock)
  "Create a new empty <dbus-connection>.
Parameters: (SOCK)
  - SOCK: Open AF_UNIX socket port.
Returns <dbus-connection> record."
  (%make-dbus-connection sock (make-bytevector 0 0) 1
                         (make-hash-table) (make-hash-table) (make-hash-table) '()
                         (make-hash-table) (make-mutex) (make-mutex) #f #f #f))

;; shared session bus connection, typically there should be only one
;; active dbus connection, while it's possible to have more, this module
;; is designed for one connection to improve efficiency
(define *dbus-session-connection* #f)

(define (dbus-connection-next-serial! conn)
  "Return and increment the next D-Bus message serial number on CONN.
Parameters: (CONN)
  - CONN: <dbus-connection> record.
Returns positive integer D-Bus serial."
  (with-mutex (dbus-connection-mutex conn)
    (let ((serial (dbus-connection-serial conn)))
      (dbus-connection-serial-set! conn (1+ serial))
      serial)))

(define (dbus-session-bus-path)
  "Locate the D-Bus UNIX session socket path.
Parameters: None.
Returns UNIX socket file path string, or #f if unavailable."
  ;; dbus unix path should always be available in env variables
  ;; and it's always better to get the path from there
  ;; value will be something like unix:path=/run/user/1000/bus
  (let ((addr (getenv "DBUS_SESSION_BUS_ADDRESS")))
    (cond
     ((and addr (string-prefix? "unix:path=" addr))
      ;; if we see the expected prefix, just
      (let* ((rest  (substring addr (string-length "unix:path=")))
             ;; we also substring by comma because sometimes there are suffix
             ;; params following a comma (no in all environments I checked)
             (comma (string-index rest #\,)))
        (if comma (substring rest 0 comma) rest)))
     ((and addr (file-exists? addr))
      addr)
     (else
      ;; default path under /run/user/{ID}/bus
      (let ((fallback (string-append "/run/user/"
                                     (number->string (getuid))
                                     "/bus")))
        (if (file-exists? fallback) fallback #f))))))

(define* (dbus-connect #:optional (path (dbus-session-bus-path)) #:key (nonblock? #f))
  "Connect to D-Bus bus socket at PATH and authenticate with SASL EXTERNAL.
Parameters: ([PATH (dbus-session-bus-path)] [#:NONBLOCK? #f])
  - PATH: String UNIX domain socket path of the D-Bus bus.
  - NONBLOCK?: Boolean enabling O_NONBLOCK on the socket when #t.
Returns <dbus-connection> record, or raises an error if PATH is missing or auth fails."
  (unless path
    (error "D-Bus socket path not found"))
  (let ((sock (socket AF_UNIX SOCK_STREAM 0)))
    ;; prevent socket leakage to child processes
    (fcntl sock F_SETFD FD_CLOEXEC)
    (catch #t
      (lambda ()
        (connect sock AF_UNIX path)
        ;; sasl external auth requires hex-encoding the ascii string of decimal UID
        (let* ((uid-str (number->string (getuid)))
               (uid-hex (apply string-append
                               (map (lambda (c) (number->string (char->integer c) 16))
                                    (string->list uid-str)))))
          ;; initial null byte + AUTH EXTERNAL <uid-hex>\r\n
          (display (string-append "\x00AUTH EXTERNAL " uid-hex "\r\n") sock)
          (force-output sock)
          (let ((line (read-line sock)))
            (unless (and (string? line) (string-prefix? "OK" line))
              (error "D-Bus SASL auth failed" line)))
          (display "BEGIN\r\n" sock)
          (force-output sock)
          (when nonblock?
            (fcntl sock F_SETFL (logior (fcntl sock F_GETFL) O_NONBLOCK)))
          (%make-empty-connection sock)))
      (lambda (k . args)
        (catch #t (lambda () (close-port sock)) (lambda _ #f))
        (apply throw k args)))))

(define (dbus-set-nonblock! conn)
  "Enable O_NONBLOCK on CONN's socket port.
Parameters: (CONN)
  - CONN: <dbus-connection> record."
  (let ((sock (dbus-connection-socket conn)))
    (fcntl sock F_SETFL (logior (fcntl sock F_GETFL) O_NONBLOCK))))

(define (dbus-connection-reset-buffer! conn)
  "Clear CONN's incoming wire frame buffer.
Parameters: (CONN)
  - CONN: <dbus-connection> record."
  (with-mutex (dbus-connection-mutex conn)
    (dbus-connection-in-buffer-set! conn (make-bytevector 0 0))))

(define (dbus-close! conn)
  "Stop any background poll thread, close CONN's socket port, and clear state.
Parameters: (CONN)
  - CONN: <dbus-connection> record or #f."
  (when (dbus-connection? conn)
    (dbus-stop-poll! conn)
    (catch #t
      (lambda () (close-port (dbus-connection-socket conn)))
      (lambda _ #f))
    (with-mutex (dbus-connection-mutex conn)
      (dbus-connection-in-buffer-set! conn (make-bytevector 0 0))
      (hash-clear! (dbus-connection-pending-calls conn))
      (hash-clear! (dbus-connection-path-services conn))
      (hash-clear! (dbus-connection-service-callbacks conn))
      (hash-clear! (dbus-connection-sync-waiters conn))
      (dbus-connection-signal-routes-set! conn '()))))

(define (dbus-send-all! conn bv)
  "Write bytevector BV to CONN's socket and flush output.
Parameters: (CONN BV)
  - CONN: <dbus-connection> record.
  - BV: Bytevector containing encoded D-Bus message bytes."
  (with-mutex (dbus-connection-write-mutex conn)
    (let ((sock (dbus-connection-socket conn)))
      (put-bytevector sock bv)
      (force-output sock))))

;;; bytevector and alignment helpers
(define (dbus-call-with-bv-output proc)
  "Open a bytevector output port, invoke PROC on it, and return the written bytes.
This is better as a macro, but I don't usually like to abuse macros.
Parameters: (PROC)
  - PROC: Procedure accepting one bytevector output port argument.
Returns bytevector written by PROC."
  (call-with-values open-bytevector-output-port
    (lambda (port get-bv)
      (proc port)
      (get-bv))))

;; align is used to pad dbus bytevectors writes and reads
;; depending on whether you use 1-byte like char, 4-byte values like int32
;; or 8-byte values like int32, double, and struct. dbus expects
;; reads and writes to be aligned with the bytes length. For example,
;; if cursor is at 7 now, and you will write an 8-byte value, you must
;; skip 1 byte so cursor is at 8 then start writing your value from 8 to 15
;; dbus expects same behavior for read as well, should be able to know when
;; to skip as types are known via signature (header)
(define (dbus-put-align port alignment)
  "Pad output PORT with zero bytes up.
Parameters: (PORT ALIGNMENT)
  - PORT: Bytevector input port
  - ALIGNMENT: Integer byte alignment boundary (should be 4 or 8)."
  (let ((rem (modulo (seek port 0 SEEK_CUR) alignment)))
    (unless (zero? rem)
      (put-bytevector port (make-bytevector (- alignment rem) 0)))))

(define (dbus-get-align port alignment)
  "Advance input PORT forward to the next ALIGNMENT byte boundary.
Parameters: (PORT ALIGNMENT)
  - PORT: Bytevector input port
  - ALIGNMENT: Integer byte alignment boundary (should be 4 or 8)."
  (let* ((pos (seek port 0 SEEK_CUR))
         (rem (modulo pos alignment)))
    (unless (zero? rem)
      (catch #t
        (lambda () (seek port (- alignment rem) SEEK_CUR))
        (lambda _ #f)))))

(define (dbus-safe-skip port n)
  "Advance PORT forward by N bytes, ignoring seek out-of-range errors.
Parameters: (PORT N)
  - PORT: Bytevector input port
  - N: Integer non-negative byte count to skip."
  (when (positive? n)
    (catch #t
      (lambda () (seek port n SEEK_CUR))
      (lambda _ #f))))

;;; encoders
(define (dbus-put-u32 port n)
  "Align PORT to 4 bytes and write unsigned 32-bit little-endian integer N.
Parameters: (PORT N)
  - PORT: Bytevector output port.
  - N: Integer unsigned 32-bit value."
  (dbus-put-align port 4)
  (let ((bv (make-bytevector 4 0)))
    (bytevector-u32-set! bv 0 n (endianness little))
    (put-bytevector port bv)))

(define (dbus-put-string port str)
  "Write D-Bus UTF-8 string STR with uint32 length and NUL terminator.
Parameters: (PORT STR)
  - PORT: Bytevector output port.
  - STR: String or #f value (treated as empty string when #f)."
  (let ((utf (string->utf8 (or str ""))))
    (dbus-put-u32 port (bytevector-length utf))
    (put-bytevector port utf)
    (put-u8 port 0)))

(define (dbus-put-signature port sig)
  "Write D-Bus type signature SIG with uint8 length and NUL terminator.
NOTE: sig will be characters like 's', 'u', check `dbus-read-variant`
Parameters: (PORT SIG)
  - PORT: Bytevector output port.
  - SIG: String or #f D-Bus type signature (treated as empty string when #f)."
  (let ((utf (string->utf8 (or sig ""))))
    (put-u8 port (bytevector-length utf))
    (put-bytevector port utf)
    (put-u8 port 0)))

(define (dbus-encode-u32 value)
  "Encode unsigned 32-bit integer VALUE into a D-Bus bytevector.
Parameters: (N)
  - VALUE: Integer unsigned 32-bit value.
Returns encoded 4-byte little-endian bytevector."
  (dbus-call-with-bv-output
   (lambda (p) (dbus-put-u32 p value))))

(define (dbus-encode-string str)
  "Encode string STR into a D-Bus bytevector.
Parameters: (STR)
  - STR: String value to encode.
Returns encoded D-Bus string bytevector."
  (dbus-call-with-bv-output
   (lambda (p) (dbus-put-string p str))))

(define (dbus-encode-string-array strs)
  "Encode list of strings STRS into a D-Bus array bytevector.
Parameters: (STRS)
  - STRS: List of string elements.
Returns encoded D-Bus array bytevector."
  (let ((data (dbus-call-with-bv-output
               (lambda (p)
                 (for-each (lambda (s)
                             (dbus-put-string p s))
                           strs)))))
    (dbus-call-with-bv-output
     (lambda (p)
       (dbus-put-u32 p (bytevector-length data))
       (put-bytevector p data)))))

(define (dbus-encode-strings . strs)
  "Encode consecutive D-Bus strings STRS into a bytevector.
Parameters: (STRS ...)
  - STRS: String values as rest arguments.
Returns encoded D-Bus bytevector."
  (dbus-call-with-bv-output
   (lambda (p)
     (for-each (lambda (s)
                 (dbus-put-string p s))
               strs))))

;;; decoders
(define (%get-aligned-num port alignment ref-proc default)
  "Align PORT to ALIGNMENT, read ALIGNMENT bytes, and decode with REF-PROC.
Parameters: (PORT ALIGNMENT REF-PROC DEFAULT)
  - PORT: Input port.
  - ALIGNMENT: Integer byte width and alignment boundary.
  - REF-PROC: Procedure bytevector numeric accessor accepting (bv index endianness).
  - DEFAULT: Value returned as fallback on short reads.
Returns decoded numeric value, or DEFAULT on EOF."
  (dbus-get-align port alignment)
  (let ((bv (get-bytevector-n port alignment)))
    (if (and (bytevector? bv) (= (bytevector-length bv) alignment))
        (ref-proc bv 0 (endianness little))
        default)))

(define (dbus-get-u8 port)
  "Read an unsigned byte from PORT, returning 0 on EOF.
Parameters: (PORT)
  - PORT: Input port (binary).
Returns integer byte value in [0, 255]."
  (let ((b (get-u8 port)))
    (if (number? b) b 0)))

(define (dbus-get-u16 port)
  "Read an aligned little-endian uint16 from PORT.
Parameters: (PORT)
  - PORT: Bytevector input port
Returns unsigned 16-bit integer, or 0 on EOF."
  (%get-aligned-num port 2 bytevector-u16-ref 0))

(define (dbus-get-u32 port)
  "Read an aligned little-endian uint32 from PORT.
Parameters: (PORT)
  - PORT: Bytevector input port
Returns unsigned 32-bit integer, or 0 on EOF."
  (%get-aligned-num port 4 bytevector-u32-ref 0))

(define (dbus-get-s32 port)
  "Read an aligned little-endian int32 from PORT.
Parameters: (PORT)
  - PORT: Bytevector input port
Returns signed 32-bit integer, or 0 on EOF."
  (%get-aligned-num port 4 bytevector-s32-ref 0))

(define (dbus-get-s64 port)
  "Read an aligned little-endian int64 from PORT.
Parameters: (PORT)
  - PORT: Bytevector input port
Returns signed 64-bit integer, or 0 on EOF."
  (%get-aligned-num port 8 bytevector-s64-ref 0))

(define (dbus-get-u64 port)
  "Read an aligned little-endian uint64 from PORT.
Parameters: (PORT)
  - PORT: Bytevector input port
Returns unsigned 64-bit integer, or 0 on EOF."
  (%get-aligned-num port 8 bytevector-u64-ref 0))

(define (dbus-get-double port)
  "Read an aligned little-endian IEEE 754 double from PORT.
Parameters: (PORT)
  - PORT: Bytevector input port
Returns float double, or 0.0 on EOF."
  (%get-aligned-num port 8 bytevector-ieee-double-ref 0.0))

(define (%get-utf8-with-len port len)
  "Read LEN UTF-8 bytes plus trailing NUL terminator from PORT.
Parameters: (PORT LEN)
  - PORT: Bytevector input port
  - LEN: Integer byte length of the UTF-8 payload excluding NUL.
Returns decoded UTF-8 string, or empty string on invalid encoding."
  (let ((bv (if (> len 0)
                (let ((b (get-bytevector-n port len)))
                  (if (bytevector? b) b #vu8()))
                #vu8())))
    (get-u8 port)
    (catch #t
      (lambda () (utf8->string bv))
      (lambda _ ""))))

(define (dbus-get-string port)
  "Decode a D-Bus uint32-prefixed UTF-8 string from PORT.
Parameters: (PORT)
  - PORT: Bytevector input port
Returns decoded UTF-8 string."
  ;; u32-length, str, null character
  (%get-utf8-with-len port (dbus-get-u32 port)))

(define (dbus-get-signature port)
  "Decode a D-Bus uint8-prefixed type signature string from PORT.
Parameters: (PORT)
  - PORT: Bytevector input port
Returns decoded D-Bus signature string."
  (%get-utf8-with-len port (dbus-get-u8 port)))

(define (dbus-get-string-array port)
  "Decode a D-Bus string array from PORT.
Parameters: (PORT)
  - PORT: Bytevector input port
Returns list of decoded strings."
  (let* ((len (dbus-get-u32 port))
         (end (+ (seek port 0 SEEK_CUR) len)))
    (let loop ((acc '()))
      (if (or (port-eof? port) (>= (seek port 0 SEEK_CUR) end))
          (reverse acc)
          (loop (cons (dbus-get-string port) acc))))))

(define* (dbus-read-variant port sig #:key (custom-readers '()))
  "Decode a D-Bus variant payload with signature SIG from PORT.
Parameters: (PORT SIG [#:CUSTOM-READERS '()])
  - PORT: Bytevector input port
  - SIG: String D-Bus type signature for the variant value.
  - CUSTOM-READERS: Alist of (SIGNATURE . READER-PROC) handlers for custom types.
Returns decoded value, or #f if SIG is unsupported."
  ;; NOTE: maybe should have symbols for these signature characters
  (let ((custom (assoc-ref custom-readers sig)))
    (cond
     ((procedure? custom)                         (custom port))
     ((string=? sig "y")                          (dbus-get-u8 port))
     ((string=? sig "u")                          (dbus-get-u32 port))
     ((string=? sig "i")                          (dbus-get-s32 port))
     ((or (string=? sig "q") (string=? sig "n"))  (dbus-get-u16 port))
     ((string=? sig "x")                          (dbus-get-s64 port))
     ((string=? sig "t")                          (dbus-get-u64 port))
     ((string=? sig "d")                          (dbus-get-double port))
     ((string=? sig "b")                          (not (zero? (dbus-get-u32 port))))
     ((or (string=? sig "s") (string=? sig "o"))  (dbus-get-string port))
     ((string=? sig "as")                         (dbus-get-string-array port))
     ((string-prefix? "a" sig)
      (dbus-safe-skip port (dbus-get-u32 port))
      #f)
     (else #f))))

;; TODO: continue documenting the rest of the functions below
;;       following the standard docstrings
(define* (dbus-get-dict port #:key (custom-readers '()))
  "Decode a D-Bus dictionary from PORT into an alist."
  (catch #t
    (lambda ()
      (let ((len (dbus-get-u32 port)))
        (dbus-get-align port 8)
        (let ((end (+ (seek port 0 SEEK_CUR) len)))
          (let loop ((acc '()))
            (let* ((cur (seek port 0 SEEK_CUR))
                   (rem (modulo cur 8))
                   (pad (if (zero? rem) 0 (- 8 rem))))
              (if (or (port-eof? port) (>= (+ cur pad) end))
                  (begin
                    (seek port end SEEK_SET)
                    (reverse acc))
                  (begin
                    (dbus-safe-skip port pad)
                    (let* ((key (dbus-get-string port))
                           (sig (dbus-get-signature port))
                           (val (dbus-read-variant port sig #:custom-readers custom-readers)))
                      (cond
                       (val (loop (cons (cons key val) acc)))
                       ((< (seek port 0 SEEK_CUR) end) (loop acc))
                       (else
                        (seek port end SEEK_SET)
                        (reverse acc)))))))))))
    (lambda (k . args)
      (log-error "dbus: error parsing dict: ~a ~a" k args)
      '())))

;;; all of these functions are helpers to read bytevectors
;;; dbus is a binary protocol except for few ascii handshake steps
(define (%bytevector-slice bv start len)
  "Copy LEN bytes from BV starting at START into a new bytevector."
  (let ((out (make-bytevector len 0)))
    (bytevector-copy! bv start out 0 len)
    out))

(define (%bytevector-append bv1 bv2)
  "Concatenate two bytevectors BV1 and BV2 into a new bytevector."
  (let* ((l1  (bytevector-length bv1))
         (l2  (bytevector-length bv2))
         (out (make-bytevector (+ l1 l2) 0)))
    (bytevector-copy! bv1 0 out 0 l1)
    (bytevector-copy! bv2 0 out l1 l2)
    out))

(define (dbus-connection-append-buffer! conn bv)
  "Append received bytevector BV to CONN's incoming frame buffer."
  (with-mutex (dbus-connection-mutex conn)
    (dbus-connection-in-buffer-set!
     conn (%bytevector-append (dbus-connection-in-buffer conn) bv))))

;;; message builders & parsers
(define (%build-header-fields fields)
  "Encode D-Bus header FIELDS entries into a wire bytevector."
  (dbus-call-with-bv-output
   (lambda (p)
     (for-each
      (lambda (f)
        (let ((code (car f))
              (sig  (cadr f))
              (val  (caddr f)))
          (dbus-put-align p 8)
          (put-u8 p code)
          (dbus-put-signature p sig)
          (cond
           ((or (string=? sig "s") (string=? sig "o")) (dbus-put-string p val))
           ((string=? sig "u")                         (dbus-put-u32 p val))
           ((string=? sig "g")                         (dbus-put-signature p val)))))
      fields))))

(define (dbus-build-message type serial fields body)
  "Assemble a complete D-Bus message wireframe."
  (let* ((hfields     (%build-header-fields fields))
         (hfields-len (bytevector-length hfields))
         (body-len    (if body (bytevector-length body) 0))
         (hdr         (make-bytevector 16 0)))
    ;; dbus header: [endianness, type, flags, version, body length, serial, fields length]
    (bytevector-u8-set!  hdr 0 108) ;; 'l' little endian
    (bytevector-u8-set!  hdr 1 type)
    (bytevector-u8-set!  hdr 2 0)   ;; flags
    (bytevector-u8-set!  hdr 3 1)   ;; protocol version 1
    (bytevector-u32-set! hdr 4  body-len    (endianness little))
    (bytevector-u32-set! hdr 8  serial      (endianness little))
    (bytevector-u32-set! hdr 12 hfields-len (endianness little))
    (dbus-call-with-bv-output
     (lambda (p)
       (put-bytevector p hdr)
       (put-bytevector p hfields)
       (dbus-put-align p 8)
       (when body (put-bytevector p body))))))

(define (dbus-parse-header-fields bv)
  "Parse raw D-Bus header fields bytevector BV into an alist."
  (let* ((p   (open-bytevector-input-port bv))
         (len (bytevector-length bv)))
    (let loop ((acc '()))
      (let* ((pos      (seek p 0 SEEK_CUR))
             (rem      (modulo pos 8))
             (pad      (if (zero? rem) 0 (- 8 rem)))
             (next-pos (+ pos pad)))
        ;; header field struct (yv) needs at least 4 bytes (code, siglen, sig, \0)
        (if (>= next-pos (- len 3))
            (reverse acc)
            (begin
              (dbus-safe-skip p pad)
              (let* ((code   (dbus-get-u8 p))
                     (siglen (dbus-get-u8 p)))
                (if (or (eof-object? siglen) (>= (+ (seek p 0 SEEK_CUR) siglen 1) len))
                    (reverse acc)
                    (let* ((sig (utf8->string (get-bytevector-n p siglen))))
                      (get-u8 p) ;; sig \0
                      (let ((val (cond
                                  ((or (string=? sig "s") (string=? sig "o")) (dbus-get-string p))
                                  ((string=? sig "u")                         (dbus-get-u32 p))
                                  ((string=? sig "g")                         (dbus-get-signature p))
                                  (else #f))))
                        (if val
                            (loop (cons (cons code val) acc))
                            (reverse acc))))))))))))

(define (dbus-unpack-message bv)
  "Unpack a complete raw D-Bus frame bytevector BV into its message components."
  (let* ((mtype      (bytevector-u8-ref bv 1))
         (serial     (bytevector-u32-ref bv 8 (endianness little)))
         (blen       (bytevector-u32-ref bv 4 (endianness little)))
         (hlen       (bytevector-u32-ref bv 12 (endianness little)))
         (rem        (modulo (+ 16 hlen) 8))
         (pad        (if (zero? rem) 0 (- 8 rem)))
         (hfields-bv (%bytevector-slice bv 16 hlen))
         (body-bv    (if (> blen 0)
                         (%bytevector-slice bv (+ 16 hlen pad) blen)
                         (make-bytevector 0 0))))
    (list mtype serial hfields-bv body-bv)))

(define (dbus-pop-message! conn)
  "Extract the next complete D-Bus message from CONN's incoming buffer."
  (with-mutex (dbus-connection-mutex conn)
    (let* ((buf     (dbus-connection-in-buffer conn))
           (buf-len (bytevector-length buf)))
      (and-let* (((>= buf-len 16))
                 (blen   (bytevector-u32-ref buf 4 (endianness little)))
                 (hlen   (bytevector-u32-ref buf 12 (endianness little)))
                 (rem    (modulo (+ 16 hlen) 8))
                 (pad    (if (zero? rem) 0 (- 8 rem)))
                 (total  (+ 16 hlen pad blen))
                 ((>= buf-len total))
                 (msg-bv (%bytevector-slice buf 0 total)))
        (dbus-connection-in-buffer-set!
         conn (%bytevector-slice buf total (- buf-len total)))
        msg-bv))))

(define (%recv-with-timeout! s scratch timeout-sec)
  "Read bytes from socket S into SCRATCH."
  (let ((ready (select (list s) '() '() timeout-sec 0)))
    (and (pair? (car ready))
         (catch 'system-error
           (lambda () (recv! s scratch 0))
           (lambda _ #f)))))

(define* (dbus-read-next-message! conn #:optional (timeout-sec 3))
  "Wait up to TIMEOUT-SEC on CONN and unpack the next incoming message."
  (let ((sock (dbus-connection-socket conn)))
    (let loop ()
      (or (and-let* ((msg-bv (dbus-pop-message! conn)))
            (dbus-unpack-message msg-bv))
          (let* ((scratch (make-bytevector 4096 0))
                 (n       (%recv-with-timeout! sock scratch timeout-sec)))
            (and (number? n)
                 (> n 0)
                 (begin
                   (dbus-connection-append-buffer! conn (%bytevector-slice scratch 0 n))
                   (loop))))))))

;;; callback-based service routing
(define (%resolve-message-callbacks conn mtype fields)
  (with-mutex (dbus-connection-mutex conn)
    (let ((path         (assoc-ref fields 1))
          (iface        (assoc-ref fields 2))
          (member-name  (assoc-ref fields 3))
          (reply-serial (assoc-ref fields 5))
          (dest         (assoc-ref fields 6))
          (sender       (assoc-ref fields 7))
          (callbacks    (dbus-connection-service-callbacks conn))
          (targets      '()))
      (define (add-cb! cb)
        (when (and (procedure? cb)
                   (not (memq cb targets)))
          (set! targets (cons cb targets))))
      (define (add-service! svc)
        (when (string? svc)
          (add-cb! (hash-ref callbacks svc #f))))
      (case mtype
        ;; METHOD_RETURN or ERROR: route by pending reply_serial -> callback or service
        ((2 3)
         (let ((target (and reply-serial
                            (hash-ref (dbus-connection-pending-calls conn) reply-serial #f))))
           (when reply-serial
             (hash-remove! (dbus-connection-pending-calls conn) reply-serial))
           (cond
            ((procedure? target) (add-cb! target))
            ((string? target)    (add-service! target))))
         (add-service! sender))
        ;; METHOD_CALL: route by registered object path, destination, or interface
        ((1)
         (when path
           (add-service! (hash-ref (dbus-connection-path-services conn) path #f)))
         (add-service! dest)
         (add-service! iface))
        ;; SIGNAL: route by signal subscriptions and registered interface callbacks
        ((4)
         (for-each
          (lambda (route)
            (let ((svc (list-ref route 0))
                  (rp  (list-ref route 1))
                  (ri  (list-ref route 2))
                  (rm  (list-ref route 3))
                  (cb  (list-ref route 4)))
              (when (and (or (not rp) (and path (string=? rp path)))
                         (or (not ri) (and iface (string=? ri iface)))
                         (or (not rm) (and member-name (string=? rm member-name))))
                (if (procedure? cb)
                    (add-cb! cb)
                    (add-service! svc)))))
          (dbus-connection-signal-routes conn))
         (add-service! iface)))
      (reverse targets))))

(define (dbus-dispatch-message! conn mtype serial fields body-bv)
  "Invoke registered callbacks for an unpacked message on CONN."
  (for-each
   (lambda (cb)
     (catch #t
       (lambda () (cb mtype serial fields body-bv))
       (lambda (k . args)
         (log-error "dbus: callback error: ~a ~a" k args))))
   (%resolve-message-callbacks conn mtype fields)))

(define (%deliver-sync-waiter! conn mtype reply-serial msg)
  (and (memv mtype '(2 3))
       reply-serial
       (let ((waiter (with-mutex (dbus-connection-mutex conn)
                       (let ((w (hash-ref (dbus-connection-sync-waiters conn) reply-serial #f)))
                         (when w
                           (hash-remove! (dbus-connection-sync-waiters conn) reply-serial))
                         w))))
         (and waiter
              (with-mutex (dbus-connection-mutex conn)
                (set-car! (cdr waiter) msg)
                (signal-condition-variable (car waiter)))
              #t))))

(define (%wait-sync-reply conn expected-serial timeout-msg timeout-sec)
  (let* ((now      (gettimeofday))
         (deadline (cons (+ (car now) timeout-sec) (cdr now)))
         (reply    (with-mutex (dbus-connection-mutex conn)
                     (let ((waiter (hash-ref (dbus-connection-sync-waiters conn) expected-serial #f)))
                       (when waiter
                         (let loop ()
                           (when (and (not (cadr waiter))
                                      (not (dbus-connection-stop-requested? conn)))
                             (when (wait-condition-variable (car waiter)
                                                            (dbus-connection-mutex conn)
                                                            deadline)
                               (loop))))
                         (hash-remove! (dbus-connection-sync-waiters conn) expected-serial)
                         (cadr waiter))))))
    (unless reply
      (error timeout-msg))
    reply))

(define* (dbus-wait-reply conn timeout-msg #:optional (timeout-sec 3) (expected-serial #f))
  (if (and expected-serial
           (with-mutex (dbus-connection-mutex conn)
             (and (dbus-connection-poll-thread conn)
                  (not (dbus-connection-stop-requested? conn))
                  (hash-ref (dbus-connection-sync-waiters conn) expected-serial #f))))
      (%wait-sync-reply conn expected-serial timeout-msg timeout-sec)
      (let loop ()
        (let ((msg (dbus-read-next-message! conn timeout-sec)))
          (unless msg
            (error timeout-msg))
          (let* ((mtype      (list-ref msg 0))
                 (serial     (list-ref msg 1))
                 (hfields-bv (list-ref msg 2))
                 (body-bv    (list-ref msg 3))
                 (fields     (dbus-parse-header-fields hfields-bv))
                 (rserial    (assoc-ref fields 5)))
            (if (and (= mtype 2)
                     (or (not expected-serial)
                         (and rserial (= rserial expected-serial))))
                msg
                (begin
                  (dbus-dispatch-message! conn mtype serial fields body-bv)
                  (loop))))))))

;;; high-level calls, service registration, replies & signals
(define* (dbus-send-call! conn path iface member sig body-bv
                          #:key (destination #f) (service #f) (callback #f))
  (let* ((serial  (dbus-connection-next-serial! conn))
         (target  (or callback service destination iface))
         (fields  `((1 "o" ,path)
                    ,@(if iface `((2 "s" ,iface)) '())
                    (3 "s" ,member)
                    ,@(if destination `((6 "s" ,destination)) '())
                    ,@(if (and sig (not (string-null? sig)))
                          `((8 "g" ,sig))
                          '())))
         (msg     (dbus-build-message 1 serial fields body-bv)))
    (when target
      (with-mutex (dbus-connection-mutex conn)
        (hash-set! (dbus-connection-pending-calls conn) serial target)))
    (dbus-send-all! conn msg)
    serial))

(define* (dbus-call! conn path iface member sig body-bv
                     #:key (destination #f) (reply? #t) (timeout-sec 3))
  (let* ((serial (dbus-connection-next-serial! conn))
         (fields `((1 "o" ,path)
                   ,@(if iface `((2 "s" ,iface)) '())
                   (3 "s" ,member)
                   ,@(if destination `((6 "s" ,destination)) '())
                   ,@(if (and sig (not (string-null? sig)))
                         `((8 "g" ,sig))
                         '())))
         (msg    (dbus-build-message 1 serial fields body-bv)))
    (when reply?
      (with-mutex (dbus-connection-mutex conn)
        (when (and (dbus-connection-poll-thread conn)
                   (not (dbus-connection-stop-requested? conn)))
          (hash-set! (dbus-connection-sync-waiters conn)
                     serial
                     (list (make-condition-variable) #f)))))
    (catch #t
      (lambda ()
        (dbus-send-all! conn msg)
        (if reply?
            (dbus-wait-reply conn (format #f "Timeout waiting for ~a reply" member) timeout-sec serial)
            serial))
      (lambda (k . args)
        (with-mutex (dbus-connection-mutex conn)
          (hash-remove! (dbus-connection-sync-waiters conn) serial))
        (apply throw k args)))))

(define (dbus-hello! conn)
  "Perform the D-Bus org.freedesktop.DBus.Hello handshake on CONN."
  (dbus-call! conn
              "/org/freedesktop/DBus"
              "org.freedesktop.DBus"
              "Hello"
              #f #f
              #:destination "org.freedesktop.DBus"))

(define* (dbus-request-name! conn name #:optional (flags 7))
  "Request ownership of bus NAME on CONN with FLAGS."
  (let* ((body  (dbus-call-with-bv-output
                 (lambda (p)
                   (dbus-put-string p name)
                   (dbus-put-u32 p flags))))
         (reply (dbus-call! conn
                            "/org/freedesktop/DBus"
                            "org.freedesktop.DBus"
                            "RequestName"
                            "su" body
                            #:destination "org.freedesktop.DBus")))
    (bytevector-u32-ref (cadddr reply) 0 (endianness little))))

(define (dbus-release-name! conn name)
  "Release ownership of bus NAME on CONN."
  (let* ((body  (dbus-encode-string name))
         (reply (dbus-call! conn
                            "/org/freedesktop/DBus"
                            "org.freedesktop.DBus"
                            "ReleaseName"
                            "s" body
                            #:destination "org.freedesktop.DBus")))
    (bytevector-u32-ref (cadddr reply) 0 (endianness little))))

(define* (dbus-register-service! conn service callback
                                 #:key (path #f) (request-name? #t) (flags 7))
  "Register SERVICE and its CALLBACK on CONN, optionally binding PATH and acquiring its bus name."
  (with-mutex (dbus-connection-mutex conn)
    (when (procedure? callback)
      (hash-set! (dbus-connection-service-callbacks conn) service callback))
    (when path
      (hash-set! (dbus-connection-path-services conn) path service)))
  (if request-name?
      (let ((code (dbus-request-name! conn service flags)))
        (if (or (= code 1) (= code 4))
            #t
            (begin
              (with-mutex (dbus-connection-mutex conn)
                (hash-remove! (dbus-connection-service-callbacks conn) service)
                (when path
                  (hash-remove! (dbus-connection-path-services conn) path)))
              #f)))
      #t))

(define* (dbus-unregister-service! conn service #:key (path #f) (release-name? #t))
  "Unregister SERVICE routes and callback from CONN and optionally release its bus name."
  (when (dbus-connection? conn)
    (with-mutex (dbus-connection-mutex conn)
      (hash-remove! (dbus-connection-service-callbacks conn) service)
      (when path
        (hash-remove! (dbus-connection-path-services conn) path)))
    (when release-name?
      (catch #t
        (lambda () (dbus-release-name! conn service))
        (lambda _ #f)))))

(define* (dbus-subscribe-signal! conn service callback
                                 #:key (path #f) (interface #f) (member #f) (add-match? #t))
  (with-mutex (dbus-connection-mutex conn)
    (dbus-connection-signal-routes-set!
     conn (cons (list service path interface member callback)
                (dbus-connection-signal-routes conn))))
  (when add-match?
    (let* ((parts (append '("type='signal'")
                          (if path `(,(format #f "path='~a'" path)) '())
                          (if interface `(,(format #f "interface='~a'" interface)) '())
                          (if member `(,(format #f "member='~a'" member)) '())))
           (rule  (string-join parts ",")))
      (dbus-call! conn
                  "/org/freedesktop/DBus"
                  "org.freedesktop.DBus"
                  "AddMatch"
                  "s" (dbus-encode-string rule)
                  #:destination "org.freedesktop.DBus"))))

(define* (dbus-unsubscribe-signal! conn service #:key (path #f) (interface #f) (member #f))
  (when (dbus-connection? conn)
    (with-mutex (dbus-connection-mutex conn)
      (dbus-connection-signal-routes-set!
       conn
       (filter (lambda (route)
                 (not (and (string=? (list-ref route 0) service)
                           (equal? (list-ref route 1) path)
                           (equal? (list-ref route 2) interface)
                           (equal? (list-ref route 3) member))))
               (dbus-connection-signal-routes conn))))))

(define (dbus-reply! conn sender reply-serial sig body-bv)
  "Send a D-Bus METHOD_RETURN reply to SENDER for REPLY-SERIAL over CONN."
  (when (dbus-connection? conn)
    (catch #t
      (lambda ()
        (let* ((serial (dbus-connection-next-serial! conn))
               (fields `((5 "u" ,reply-serial)
                         (6 "s" ,sender)
                         ,@(if (and sig (not (string-null? sig)))
                               `((8 "g" ,sig))
                               '())))
               (msg    (dbus-build-message 2 serial fields body-bv)))
          (dbus-send-all! conn msg)))
      (lambda (key . args)
        (log-error "dbus: error sending reply: ~a ~a" key args)))))

(define (dbus-error-reply! conn sender reply-serial error-name error-msg)
  (when (and (dbus-connection? conn) sender reply-serial)
    (catch #t
      (lambda ()
        (let* ((serial   (dbus-connection-next-serial! conn))
               (err      (or error-name "org.freedesktop.DBus.Error.Failed"))
               (has-msg? (and error-msg (not (string-null? error-msg))))
               (fields   `((4 "s" ,err)
                           (5 "u" ,reply-serial)
                           (6 "s" ,sender)
                           ,@(if has-msg? '((8 "g" "s")) '())))
               (body-bv  (and has-msg? (dbus-encode-string error-msg)))
               (msg      (dbus-build-message 3 serial fields body-bv)))
          (dbus-send-all! conn msg)))
      (lambda (key . args)
        (log-error "dbus: error sending error reply: ~a ~a" key args)))))

(define (dbus-emit-signal! conn path iface member sig body-proc)
  "Emit a D-Bus signal MEMBER on PATH and IFACE with signature SIG over CONN."
  (when (dbus-connection? conn)
    (catch #t
      (lambda ()
        (let* ((serial (dbus-connection-next-serial! conn))
               (fields `((1 "o" ,path)
                         (2 "s" ,iface)
                         (3 "s" ,member)
                         ,@(if (and sig (not (string-null? sig)))
                               `((8 "g" ,sig))
                               '())))
               (body   (dbus-call-with-bv-output body-proc))
               (msg    (dbus-build-message 4 serial fields body)))
          (dbus-send-all! conn msg)))
      (lambda (k . args)
        (log-error "dbus: error sending ~a signal: ~a ~a" member k args)))))

(define* (dbus-poll! conn #:optional (on-message #f))
  (if (not (dbus-connection? conn))
      'ok
      (let ((sock (dbus-connection-socket conn)))
        (let loop ()
          (if (not (pair? (car (select (list sock) '() '() 0 0))))
              'ok
              (let* ((scratch (make-bytevector 4096 0))
                     (n       (catch 'system-error
                                (lambda () (recv! sock scratch MSG_DONTWAIT))
                                (lambda _ #f))))
                (cond
                 ((and (number? n) (> n 0))
                  (dbus-connection-append-buffer! conn (%bytevector-slice scratch 0 n))
                  (let drain ()
                    (and-let* ((msg-bv (dbus-pop-message! conn)))
                      (let ((unpacked (catch #t
                                        (lambda () (dbus-unpack-message msg-bv))
                                        (lambda (k . args)
                                          (log-error "dbus: error unpacking message: ~a ~a" k args)
                                          #f))))
                        (when unpacked
                          (let* ((mtype      (list-ref unpacked 0))
                                 (serial     (list-ref unpacked 1))
                                 (hfields-bv (list-ref unpacked 2))
                                 (body-bv    (list-ref unpacked 3))
                                 (fields     (catch #t
                                               (lambda () (dbus-parse-header-fields hfields-bv))
                                               (lambda (k . args)
                                                 (log-error "dbus: error parsing header fields: ~a ~a" k args)
                                                 '()))))
                            (unless (%deliver-sync-waiter! conn mtype (assoc-ref fields 5) unpacked)
                              (dbus-dispatch-message! conn mtype serial fields body-bv)
                              (when (procedure? on-message)
                                (on-message mtype serial fields body-bv))))))
                      (drain)))
                  (loop))
                 ((and (number? n) (= n 0))
                  'eof)
                 (else 'ok))))))))

;;; background poll thread & shared session bus lifecycle
(define (dbus-start-poll! conn)
  "Start a background thread that blocks on CONN's socket and dispatches incoming messages."
  (when (dbus-connection? conn)
    (dbus-set-nonblock! conn)
    (let ((start? #f)
          (stop-read #f))
      (with-mutex (dbus-connection-mutex conn)
        (unless (dbus-connection-poll-thread conn)
          (let ((sp (pipe)))
            (setvbuf (cdr sp) 'none)
            (dbus-connection-stop-pipe-set! conn sp)
            (dbus-connection-stop-requested?-set! conn #f)
            (set! stop-read (car sp))
            (set! start? #t)
            (dbus-connection-poll-thread-set!
             conn
             (call-with-new-thread
              (lambda ()
                (let ((sock (dbus-connection-socket conn)))
                  (let loop ()
                    (unless (with-mutex (dbus-connection-mutex conn)
                              (dbus-connection-stop-requested? conn))
                      (let ((ready (catch #t
                                     (lambda () (select (list sock stop-read) '() '()))
                                     (lambda _ '(() () ())))))
                        (cond
                         ((with-mutex (dbus-connection-mutex conn)
                            (dbus-connection-stop-requested? conn))
                          #f)
                         ((memq stop-read (car ready))
                          #f)
                         ((memq sock (car ready))
                          (let ((res (catch #t
                                       (lambda () (dbus-poll! conn))
                                       (lambda (k . args)
                                         (log-error "dbus: poll thread error: ~a ~a" k args)
                                         'ok))))
                            (if (eq? res 'eof)
                                (begin
                                  (log-info "dbus: bus socket closed by peer")
                                  (when (eq? conn *dbus-session-connection*)
                                    (set! *dbus-session-connection* #f))
                                  (dbus-close! conn))
                                (loop))))
                         (else (loop)))))))))))))
      start?)))

(define (dbus-stop-poll! conn)
  (when (dbus-connection? conn)
    (let ((th #f)
          (sp #f))
      (with-mutex (dbus-connection-mutex conn)
        (set! th (dbus-connection-poll-thread conn))
        (set! sp (dbus-connection-stop-pipe conn))
        (dbus-connection-stop-requested?-set! conn #t)
        (dbus-connection-poll-thread-set! conn #f)
        (dbus-connection-stop-pipe-set! conn #f)
        (hash-for-each (lambda (_serial waiter)
                         (signal-condition-variable (car waiter)))
                       (dbus-connection-sync-waiters conn)))
      (when sp
        (catch #t
          (lambda ()
            (put-u8 (cdr sp) 1)
            (force-output (cdr sp)))
          (lambda _ #f)))
      (when (and th (not (eq? th (current-thread))))
        (catch #t (lambda () (join-thread th)) (lambda _ #f)))
      (when sp
        (catch #t (lambda () (close-port (car sp))) (lambda _ #f))
        (catch #t (lambda () (close-port (cdr sp))) (lambda _ #f))))))

(define (dbus-session-poll!)
  "Poll the shared D-Bus session connection manually for non-threaded usage."
  (when *dbus-session-connection*
    (catch #t
      (lambda ()
        (when (eq? (dbus-poll! *dbus-session-connection*) 'eof)
          (log-info "dbus: session bus socket closed by peer")
          (dbus-session-disconnect!)))
      (lambda (key . args)
        (log-error "dbus: session polling error: ~a ~a" key args)))))

(define (dbus-session-disconnect!)
  "Stop background polling, disconnect, and clean up the shared D-Bus session connection."
  (when *dbus-session-connection*
    (let ((conn *dbus-session-connection*))
      (set! *dbus-session-connection* #f)
      (dbus-close! conn))))

(define (dbus-session-connect!)
  "Return the shared session <dbus-connection>.
Connect and start its poll thread if needed."
  (or *dbus-session-connection*
      (let ((path (dbus-session-bus-path)))
        (if (not path)
            (begin
              (log-warn "dbus: DBUS_SESSION_BUS_ADDRESS not found")
              #f)
            (catch #t
              (lambda ()
                (let ((conn (dbus-connect path)))
                  (dbus-hello! conn)
                  (set! *dbus-session-connection* conn)
                  (dbus-start-poll! conn)
                  conn))
              (lambda (key . args)
                (log-error "dbus: failed to connect session bus: ~a ~a" key args)
                #f))))))
