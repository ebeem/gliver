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
  #:export (*log-level*
            log-debug
			log-info
			log-warn
			log-error
            log-level-set!
			log-file-set!))

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
           (level-str (string-upcase (symbol->string level)))
           (message (apply format #f fmt args)))
      (if (and filename (string? filename))
        (let* ((trimmed-filename (string-trim-right filename #\/))
               (match (string-match "[^/]+$" trimmed-filename))
               (basename (if match
                             (match:substring match 0)
                             trimmed-filename)))
          (format (*log-port*) "[~a] ~a [~a]: ~a~%" timestamp level-str basename message))
        (format (*log-port*) "[~a] ~a: ~a~%" timestamp level-str message))
      (force-output (*log-port*)))))

(define (log-debug fmt . args)
  (log-write (current-filename) 'debug fmt args))
(define (log-info fmt . args)
  (log-write (current-filename) 'info fmt args))
(define (log-warn fmt . args)
  (log-write (current-filename) 'warn fmt args))
(define (log-error fmt . args)
  (log-write (current-filename) 'error fmt args))
