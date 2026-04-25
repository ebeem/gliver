;;; gliver/core/logs.scm --- Logging system for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver core logs)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)
  #:export (
			*log-level*
			*log-port*
			*log-file-path*
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

(define *log-level* (make-parameter 'info))
(define *log-port* (make-parameter (current-error-port)))
(define *log-file-path* (make-parameter #f))

(define *log-levels*
  '((debug . 0) (info . 1) (warn . 2) (error . 3)))

(define (log-level-value level)
  (assoc-ref *log-levels* level))

(define (log-level-set! level)
  (*log-level* level))

(define (log-file-set! path)
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

(define (log-write filename level fmt args)
  (when (>= (log-level-value level)
            (log-level-value (*log-level*)))
    (let* ((now (current-date))
           (timestamp (date->string now "~Y-~m-~d ~H:~M:~S"))
           (raw-level (string-upcase (symbol->string level)))
           (level-pad (- 5 (string-length raw-level)))
           (level-str (if (> level-pad 0)
                          (string-append raw-level (make-string level-pad #\space))
                          raw-level))
           (message (apply format #f fmt args))
           (thread-name "main"))
      (if (and filename (string? filename))
        (let* ((trimmed-filename (string-trim-right filename #\/))
               (match (string-match "[^/]+$" trimmed-filename))
               (basename (if match
                             (match:substring match 0)
                             trimmed-filename)))
          ;; standard log4j format with logger name
          (format (*log-port*) "~a [~a] ~a ~a - ~a~%" 
                  timestamp thread-name level-str basename message))
        ;; standard log4j format without logger name
        (format (*log-port*) "~a [~a] ~a - ~a~%" 
                timestamp thread-name level-str message))
      (force-output (*log-port*)))))

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

