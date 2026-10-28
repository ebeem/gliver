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
  #:declarative? #f
  #:export (
			valid-pointer?
			*variable-registry*
			variable-register!
			define-var
			var-get
			%var-set!
			var-set!
))

(define (valid-pointer? ptr)
  "Validate that pointer PTR is valid and not null.
Parameters: (PTR)
  - PTR: Foreign pointer.
Returns #t if PTR is valid, #f otherwise."
  (and ptr (not (null-pointer? ptr))))

;;; variable introspection registry
(define *variable-registry* (make-hash-table))

(define (variable-register! name module docstring)
  "Register a variable NAME with DOCSTRING in MODULE.
Parameters: (NAME MODULE DOCSTRING)
  - NAME: Symbol name of the variable.
  - MODULE: Module record where the variable resides.
  - DOCSTRING: Documentation string for the variable."
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
  "Get a variable's value by its symbol NAME.
Parameters: (NAME)
  - NAME: Symbol name of the variable.
Returns variable value, or #f if not found."
  (let ((record (hash-table-ref/default *variable-registry* name #f)))
    (if record
        (let ((module (car record)))
          (module-ref module name))
        #f)))

;; TODO: maybe similar to emacs, this should overwrite the defined
;; variable if exists, if it doesn't then it should create it
;; define-var should create the variable only if it doesn't exist
;; so it doesn't overwrite variables
(define (%var-set! name new-value)
  "Update a registered variable NAME with NEW-VALUE.
Parameters: (NAME NEW-VALUE)
  - NAME: Symbol name of the variable.
  - NEW-VALUE: New value to set.
Returns the updated value, or #f if not found."
  (let ((record (hash-table-ref/default *variable-registry* name #f)))
    (if record
        (let ((module (car record)))
          (module-set! module name new-value))
        #f)))

(define-syntax var-set!
  (syntax-rules ()
    ((_ name new-value)
     (%var-set! 'name new-value))))
