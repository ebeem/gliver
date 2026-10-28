;;; gliver/core/logs.scm --- Logging system for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module provides the central logging system for Gliver.
;;; It handles:
;;;  - filtering log levels (debug, info, warn, error)
;;;  - formatting structured log records with timestamps
;;;  - logging to configurable log files
;;;  - syntax macros for logging debug, info, warning, and error messages

(define-module (gliver core logs)
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:use-module (gliver core helpers)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-19)
  #:declarative? #f
  #:export (
			*log-level*
			*log-hooks*
			*log-sequences*
			*log-river-calls*
			*log-port*
			*log-file-path*
			*log-thread-padding*
			*log-padding*
			*log-levels*
			log-level-value
			log-level-set!
			log-file-set!
			log-write
			log-debug
			log-info
			log-warn
			log-error
))

;;; logging configuration & parameters
(define-var *log-level* (make-parameter 'info))
(define-var *log-hooks* #f)
(define-var *log-sequences* #f)
(define-var *log-river-calls* #f)
(define-var *log-port* (make-parameter (current-error-port)))
(define-var *log-file-path* (make-parameter #f))
(define-var *log-thread-padding* 6)
(define-var *log-padding* 14)

;; mapping of log level symbols to integer priority
(define-var *log-levels*
  '((debug . 0) (info . 1) (warn . 2) (error . 3)))

;;; level management
(define (log-level-value level)
  "Return the integer severity value associated with LEVEL symbol.
Parameters: (LEVEL)
  - LEVEL: Severity symbol ('debug, 'info, 'warn, 'error).
Returns integer priority (0-3), or #f if LEVEL is unknown."
  (assoc-ref *log-levels* level))

(define (log-level-set! level)
  "Set the global logging threshold to LEVEL symbol.
Parameters: (LEVEL)
  - LEVEL: Severity symbol ('debug, 'info, 'warn, 'error)."
  (*log-level* level))

(define (log-file-set! path)
  "Redirect log output to the file at PATH.
Creates parent directory if it does not exist.
Parameters: (PATH)
  - PATH: File path string, or #f to close file port."
  (when (*log-file-path*)
    (when (and (port? (*log-port*))
               (not (eq? (*log-port*) (current-error-port))))
      (close-port (*log-port*))))
  (*log-file-path* path)
  (when path
    (let ((dir (dirname path)))
      (unless (file-exists? dir)
        (mkdir dir)))
    (*log-port* (open-file path "a"))))

;;; log writing
(define (log-write filename level fmt args)
  "Format and write a log entry at LEVEL from FILENAME using FMT and ARGS.
Filters out any messages with a priority lower than (*log-level*).
Parameters: (FILENAME LEVEL FMT ARGS)
  - FILENAME: File of caller generating the log message.
  - LEVEL: Severity symbol ('debug, 'info, 'warn, 'error).
  - FMT: Format string.
  - ARGS: Arguments list for FMT."
  (when (>= (log-level-value level)
            (log-level-value (*log-level*)))
    (let* ((now (current-date))
           (ms (string-pad (number->string (quotient (date-nanosecond now) 1000000)) 3 #\0))
           (timestamp (string-append (date->string now "~Y-~m-~d ~H:~M:~S.") ms))
           (raw-level (string-upcase (symbol->string level)))
           (level-str (string-pad raw-level 5 #\space))
           (thread-name "main")
           (message (apply format #f fmt args))
           (basename (if (and filename (string? filename))
                         (let* ((trimmed (string-trim-right filename #\/))
                                (match (string-match "[^/]+$" trimmed)))
                           (if match (match:substring match 0) trimmed))
                         "Unknown")))
      ;; standard log4j format with logger name
      (format (*log-port*) "~a [~a] ~a ~a - ~a~%"
              timestamp 
              thread-name
              level-str 
              basename 
              message)
      (force-output (*log-port*)))))

;;; convenience logging macros
(define-syntax log-debug
  (syntax-rules ()
    ((_ fmt args ...)
     (log-write (current-filename) 'debug fmt (list args ...)))))

(define-syntax log-info
  (syntax-rules ()
    ((_ fmt args ...)
     (log-write (current-filename) 'info fmt (list args ...)))))

(define-syntax log-warn
  (syntax-rules ()
    ((_ fmt args ...)
     (log-write (current-filename) 'warn fmt (list args ...)))))

(define-syntax log-error
  (syntax-rules ()
    ((_ fmt args ...)
     (log-write (current-filename) 'error fmt (list args ...)))))
