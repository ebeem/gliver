;;; gliver/contrib/debug/repl-server.scm --- REPL server for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; Provides a Guile REPL over a UNIX domain socket.
;;; Compatible with Geiser (Emacs) for interactive development.

(define-module (gliver contrib debug repl-server)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 ports)
  #:use-module (system repl server)
  #:use-module (system repl repl)
  #:use-module (gliver core)
  #:export (repl-start!
            repl-stop!
            repl-socket-path))

;;; state
(define *repl-server* #f)

(define (repl-socket-path)
  "Return the path to the REPL socket."
  (string-append (*gliver-runtime-dir*) "/repl.sock"))

;;; server
(define (gliver-serve-client client addr)
  (let ((thread (current-thread)))
    ((@@ (system repl server) add-open-socket!)
     client
     (lambda () (cancel-thread thread))))

  ((@@ (system repl server) guard-against-http-request) client)

  (dynamic-wind
    (lambda () #f)
    (lambda ()
      (with-continuation-barrier
       (lambda ()
         (parameterize ((current-input-port client)
                        (current-output-port client)
                        (current-error-port client)
                        (current-warning-port client))
           (with-fluids (((@@ (system repl server) *repl-stack*) '()))
             (catch 'system-error
               (lambda ()
                 (start-repl))
               (lambda (key . args)
                 (let ((errno (system-error-errno (cons key args))))
                   (cond
                    ((member errno (list EPIPE ECONNRESET))
					 ;; client disconnected
                     ;; (log-debug "REPL client disconnected: ~a" (strerror errno))
					 )
                    (else
                     (apply throw key args)))))))))))
    (lambda ()
      ((@@ (system repl server) close-socket!) client))))

(define (gliver-run-server server-socket)
  ((@@ (system repl server) run-server*) server-socket gliver-serve-client))

(define (gliver-spawn-server server-socket)
  (make-thread gliver-run-server server-socket))

(define (repl-start!)
  "Start the REPL server on a UNIX domain socket.
Uses Guile's built-in (system repl server) for Geiser compatibility."
  (let ((path (repl-socket-path)))
    ;; Remove stale socket
    (when (file-exists? path)
      (delete-file path))

    (catch #t
      (lambda ()
        ;; spawn-server takes a list specifying the server type
        (set! *repl-server* (gliver-spawn-server (make-unix-domain-server-socket #:path path)))
        (chmod path #o700)
        (log-info "REPL server listening on ~a" path)
        (log-info "Connect with: guile -c '(begin (use-modules (system repl server)) (run-client (make-unix-domain-server-socket #:path \"~a\")))'" path))
      (lambda (key . args)
        (log-error "Failed to start REPL server: ~a ~a" key args)
        ;; fall back to a simpler socket server
        (catch #t
          (lambda ()
            (simple-repl-start! path))
          (lambda (key2 . args2)
            (log-error "Fallback REPL also failed: ~a ~a" key2 args2)))))))

(define (simple-repl-start! path)
  "Start a simple REPL server without (system repl server)."
  (let ((sock (socket AF_UNIX SOCK_STREAM 0)))
    (bind sock AF_UNIX path)
    (chmod path #o700)
    (listen sock 1)
    (set! *repl-server* sock)
    (log-info "Simple REPL server listening on ~a" path)))

(define (repl-stop!)
  "Stop the REPL server."
  (when *repl-server*
    (catch #t
      (lambda ()
        ;; if it's a thread from spawn-server, it'll be cleaned up by gc
        ;; if it's our simple socket, close it
        (when (port? *repl-server*)
          (close-port *repl-server*))
        (set! *repl-server* #f)
        (let ((path (repl-socket-path)))
          (when (file-exists? path)
            (delete-file path)))
        (log-info "REPL server stopped."))
      (lambda (key . args)
        (log-error "Error stopping REPL: ~a ~a" key args)))))
