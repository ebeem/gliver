;;; gliver/core/helpers.scm --- Helper utilities for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module provides utilities and helper functions that are common in Gliver
;;; It handles:
;;;  - Helpers for FFI
;;;  - Helpers for variable introspection

(define-module (gliver core helpers)
  #:use-module (system foreign)
  #:use-module (srfi srfi-69)
  #:use-module (gliver core logs)
  #:declarative? #f
  #:export (
			valid-pointer?
			*variable-registry*
			variable-register!
			define-var
			name
			var-get
			%var-set!
			var-set!
))

(define (valid-pointer? ptr)
  "Return #t if pointer PTR is valid and not null."
  (and ptr (not (null-pointer? ptr))))

;;; variable introspection registry
(define *variable-registry* (make-hash-table))

(define (variable-register! name module docstring)
  "Register a variable NAME, in MODULE, and DOCSTRING for introspection."
  (hash-table-set! *variable-registry* name (cons module docstring)))

(define-syntax define-var
  (syntax-rules ()
    ;; pattern 1: (define-var name value "docstring")
    ((_ name init-value docstring)
     (begin
       (define name init-value)
       (variable-register! 'name (current-module) docstring)))

    ;; pattern 2: (define-var name value) - no docstring provided
    ((_ name init-value)
     (begin
       (define name init-value)
       (variable-register! 'name (current-module) "")))

    ;; pattern 3: (define-var name) - initialized to #f by default
    ((_ name)
     (begin
       (define name #f)
       (variable-register! 'name (current-module) "")))))

(define (var-get name)
  "Retrieve a variable's value by its symbol NAME, using the registry."
  (let ((record (hash-table-ref/default *variable-registry* name #f)))
    (if record
        (let ((module (car record)))
          (module-ref module name))
        (log-error "var-get: variable not found in registry ~a" name))))

(define (%var-set! name new-value)
  "Update a registered variable NAME with NEW-VALUE."
  (let ((record (hash-table-ref/default *variable-registry* name #f)))
    (if record
        (let ((module (car record)))
          (module-set! module name new-value))
        (begin
		  (log-error "var-set!: Variable not found in registry" name)))))

(define-syntax var-set!
  (syntax-rules ()
    ((_ name new-value)
     (%var-set! 'name new-value))))
