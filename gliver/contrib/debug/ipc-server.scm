;;; gliver/contrib/debug/ipc-server.scm --- IPC socket server for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; Provides a UNIX domain socket server that accepts commands
;;; from gliver-msg and other clients.

(define-module (gliver contrib debug ipc-server)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (gliver core)
  #:use-module (gliver contrib commands)
  #:export (ipc-start!
            ipc-stop!
            ipc-poll!
            ipc-socket-path
            ipc-handle-message))

;;; state
(define *ipc-server-socket* #f)
(define *ipc-clients* '())

(define (ipc-socket-path)
  "Return the path to the IPC socket."
  (string-append *gliver-runtime-dir* "/ipc.sock"))

;;; server
(define (ipc-start!)
  "Start the IPC server on a UNIX domain socket."
  (let ((path (ipc-socket-path)))
    ;; remove stale socket
    (when (file-exists? path)
      (delete-file path))

    (catch #t
      (lambda ()
        (let ((sock (socket AF_UNIX SOCK_STREAM 0)))
          (bind sock AF_UNIX path)
          (chmod path #o700)  ;; owner-only access
          (listen sock 5)
          ;; non-blocking
          (fcntl sock F_SETFL (logior (fcntl sock F_GETFL) O_NONBLOCK))
          (set! *ipc-server-socket* sock)
          (log-info "IPC server listening on ~a" path)))
      (lambda (key . args)
        (log-error "Failed to start IPC server: ~a ~a" key args)))))

(define (ipc-stop!)
  "Stop the IPC server."
  (when *ipc-server-socket*
    (catch #t
      (lambda ()
        ;; close all client connections
        (for-each (lambda (client)
                    (catch #t
                      (lambda () (close-port client))
                      (lambda _ #f)))
                  *ipc-clients*)
        (set! *ipc-clients* '())
        ;; close server socket
        (close-port *ipc-server-socket*)
        (set! *ipc-server-socket* #f)
        ;; remove socket file
        (let ((path (ipc-socket-path)))
          (when (file-exists? path)
            (delete-file path)))
        (log-info "IPC server stopped."))
      (lambda (key . args)
        (log-error "Error stopping IPC server: ~a ~a" key args)))))

;;; client handling
(define (ipc-accept-new!)
  "Accept any pending connections."
  (when *ipc-server-socket*
    (catch #t
      (lambda ()
        (let ((client (accept *ipc-server-socket*)))
          (when client
            (let ((port (car client)))
              (fcntl port F_SETFL (logior (fcntl port F_GETFL) O_NONBLOCK))
              (set! *ipc-clients* (cons port *ipc-clients*))
              (log-debug "IPC client connected.")))))
      (lambda (key . args)
        ;; EAGAIN / EWOULDBLOCK is normal for non-blocking
        (unless (and (eq? key 'system-error)
                     (pair? args)
                     (string-contains (car args) "Resource temporarily"))
          ;; ignore non-blocking "no pending connections" errors silently
          #f)))))

(define (ipc-read-client! port)
  "Read a message from a client PORT.  Returns the message string or #f."
  (catch #t
    (lambda ()
      (let ((line (read-line port)))
        (if (eof-object? line)
            (begin
              (set! *ipc-clients* (delq port *ipc-clients*))
              (close-port port)
              (log-debug "IPC client disconnected.")
              #f)
            (string-trim-both line))))
    (lambda (key . args)
      ;; EAGAIN means no data yet
      #f)))

(define (ipc-send-response! port response)
  "Send a response to a client."
  (catch #t
    (lambda ()
      (display response port)
      (newline port)
      (force-output port))
    (lambda (key . args)
      (log-error "IPC send error: ~a" key))))

;;; message handling
(define (ipc-handle-message msg)
  "Parse and handle an IPC message.
Messages are s-expressions: (command-name arg1 arg2 ...)
Or bare strings: \"command-name arg1 arg2\""
  (catch #t
    (lambda ()
      (let ((expr (if (and (> (string-length msg) 0)
                           (char=? (string-ref msg 0) #\())
                      ;; s-expression format
                      (call-with-input-string msg read)

                      ;; bare string format: split on space
                      (let ((parts (string-split msg #\space)))
                        (if (or (null? parts) (string-null? (car parts)))
                            '()
                            (cons (string->symbol (car parts))
                                  ;; automatically convert numeric strings to actual numbers
                                  (map (lambda (arg)
                                         (let ((num (string->number arg)))
                                           (if num num arg)))
                                       (cdr parts))))))))

        (match expr
          (('eval expr-to-eval)
           (let ((result (eval expr-to-eval (current-module))))
             (format #f "(ok ~a)" result)))

          (('eval . rest)
           "(error \"eval requires exactly one expression\")")

          ((cmd-name args ...)
           (let ((cmd (command-find cmd-name)))
             (if cmd
                 (let ((result (apply command-run cmd args)))
                   (format #f "(ok ~a)" result))
                 (format #f "(error \"Unknown command: ~a\")" cmd-name))))

          (_
           "(error \"Invalid message format\")"))))

    (lambda (key . rest)
      (if (eq? key 'read-error)
          "(error \"Invalid message format\")"
          (format #f "(error \"~a: ~a\")" key rest)))))

;;; polling
(define (ipc-poll!)
  "Poll for IPC activity.  Call this from the main event loop."
  (when *ipc-server-socket*
    ;; accept new connections
    (ipc-accept-new!)

    ;; read from each client
    (for-each
     (lambda (client)
       (let ((msg (ipc-read-client! client)))
         (when msg
           (log-debug "IPC received: ~a" msg)
           (let ((response (ipc-handle-message msg)))
             (ipc-send-response! client response)))))
     (list-copy *ipc-clients*))))

