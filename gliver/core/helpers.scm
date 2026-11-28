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
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-69)
  #:declarative? #f
  #:export (
            valid-pointer?
            executable-exists?
            *variable-registry*
            *variable-accessors*
            ??
            variable-register!
            define-var
            var-get
            var-set!
            re-export-modules
))

(define (valid-pointer? ptr)
  "Validate that pointer PTR is valid and not null.
Parameters: (PTR)
  - PTR: Pointer to validate.
Returns #t if PTR is valid, #f otherwise."
  (and (pointer? ptr) (not (null-pointer? ptr))))

(define (executable-exists? name)
  "Return #t if executable NAME is found in PATH.
Parameters: (NAME)
  - NAME: String executable name.
Returns #t if found and executable, #f otherwise."
  (and-let* (((string? name))
             (path (getenv "PATH"))
             (bin (search-path (string-split path #\:) name)))
    ;; execute access on the first returned binary
    (access? bin X_OK)))

;;; variable introspection registry
(define *variable-registry* (make-hash-table))
(define *variable-accessors* (make-hash-table))
;; helper for fallback (unsetting variable value)
(define ?? *unspecified*)

(define (variable-register! name module docstring . accessors)
  "Register a variable NAME with DOCSTRING in MODULE.
Parameters: (NAME MODULE DOCSTRING [GETTER SETTER])
  - NAME: Symbol name of the variable.
  - MODULE: Module record where the variable resides.
  - DOCSTRING: String documentation describing the variable.
  - GETTER: Procedure taking zero arguments returning the variable value (optional).
  - SETTER: Procedure taking one argument updating the variable value (optional)."
  (hash-table-set! *variable-registry* name (cons module docstring))
  (if (pair? accessors)
      (hash-table-set! *variable-accessors* name
                       (cons (car accessors) (cadr accessors)))
      (hash-table-delete! *variable-accessors* name)))

(define-syntax define-var
  (lambda (x)
    (define (emit-regular name init-val doc)
      (with-syntax ((name name)
                    (init-val init-val)
                    (doc doc))
        #'(begin
            (define name init-val)
            (variable-register! 'name (current-module) doc))))

    ;; expand an inherited variable into a hidden backing binding, registry accessors,
    ;; and a variable transformer that falls back to `parent' when unspecified.
    (define (emit-inherited name init-val parent doc)
      (with-syntax ((name name)
                    (init-val init-val)
                    (parent parent)
                    (doc doc))
        #'(begin
            ;; register custom getter/setter closures
            ;; we can't have just name like other variables, because
            ;; name is here a define-syntax, not a defined variable
            (let ((backing init-val))
              (variable-register! 'name (current-module) doc
                                  (lambda ()
                                    (if (unspecified? backing)
                                        parent
                                        backing))
                                  (lambda (val)
                                    (set! backing val))))
            ;; variable transformer, just utilize var-get and var-set.
            (define-syntax name
              (make-variable-transformer
               (lambda (y)
                 (syntax-case y (set!)
                   ;; set should update the backing override
                   ;; use `??` to revert to `parent`
                   ((set! id val) #'(var-set! name val))
                   ;; operator position, invoke it
                   ((id . args)
                    #'((var-get 'name) . args))
                   ;; value position, resolve via registry accessor.
                   (id (identifier? #'id)
                       #'(var-get 'name)))))))))

    (syntax-case x ()
      ;; (define-var name #:inherit parent "docstring")
      ((_ name #:inherit parent docstring)
       (emit-inherited #'name #'*unspecified* #'parent #'docstring))

      ;; (define-var name #:inherit parent)
      ((_ name #:inherit parent)
       (emit-inherited #'name #'*unspecified* #'parent #'""))

      ;; (define-var name value #:inherit parent "docstring")
      ((_ name init-value #:inherit parent docstring)
       (emit-inherited #'name #'init-value #'parent #'docstring))

      ;; (define-var name value #:inherit parent)
      ((_ name init-value #:inherit parent)
       (emit-inherited #'name #'init-value #'parent #'""))

      ;; (define-var name value "docstring" #:inherit parent)
      ((_ name init-value docstring #:inherit parent)
       (emit-inherited #'name #'init-value #'parent #'docstring))

      ;; (define-var name value "docstring")
      ((_ name init-value docstring)
       (emit-regular #'name #'init-value #'docstring))

      ;; (define-var name value)
      ((_ name init-value)
       (emit-regular #'name #'init-value #'""))

      ;; (define-var name)
      ((_ name)
       (emit-regular #'name #'#f #'"")))))

(define (var-get name)
  "Get a variable's value by its symbol NAME.
Parameters: (NAME)
  - NAME: Symbol name of the variable.
Returns variable value, or #f if not found."
  (let ((acc (hash-table-ref/default *variable-accessors* name #f)))
    (if acc
        ((car acc))
        (and-let* ((record (hash-table-ref/default *variable-registry* name #f))
                   (module (car record)))
          (module-ref module name)))))

;; TODO: maybe similar to emacs, this should overwrite the defined
;; variable if exists, if it doesn't then it should create it
;; define-var should create the variable only if it doesn't exist
;; so it doesn't overwrite variables
(define (%var-set! name new-value)
  "Update a registered variable NAME with NEW-VALUE.
Parameters: (NAME NEW-VALUE)
  - NAME: Symbol name of the variable.
  - NEW-VALUE: Value to assign to the variable.
Returns the updated value, or #f if not found."
  (let ((acc (hash-table-ref/default *variable-accessors* name #f)))
    (if acc
        ((cdr acc) new-value)
        (and-let* ((record (hash-table-ref/default *variable-registry* name #f))
                   (module (car record)))
          (module-set! module name new-value)))))

(define-syntax var-set!
  (syntax-rules ()
    ((_ name new-value)
     (%var-set! 'name new-value))))

;; re-export all public bindings from each named module into the current module
(define-syntax re-export-modules
  (syntax-rules ()
    ((_ (mod ...) ...)
     (begin
       (module-use! (module-public-interface (current-module))
                    (resolve-interface '(mod ...)))
       ...))))
