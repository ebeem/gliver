;;; gliver/window-rules.scm --- Window rules for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver window-rules)
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (gliver core)
  #:declarative? #f
  #:export (window-rule-add!
            window-rules-clear-all!
            window-rule-apply!
            *window-rules*))

;;; window rule
(define-record-type <window-rule>
  (make-window-rule match-criteria actions continue?)
  window-rule?
  (match-criteria window-rule-match)    ;; alist: ((property . pattern) ...)
  (actions        window-rule-actions)  ;; alist: ((action . value) ...)
  (continue?      window-rule-continue?)) ;; #t to allow cascading

(define *window-rules* '())

(define* (window-rule-add! #:key match actions (continue #f))
  "Define a window rule.  MATCH is an alist of (property . pattern).
ACTIONS is an alist of (action . value)."
  (set! *window-rules*
    (append *window-rules*
            (list (make-window-rule match actions continue)))))

(define (window-rules-clear-all!)
  "Remove all window rules."
  (set! *window-rules* '()))

(define (match-property? window property pattern)
  "Check if WINDOW's PROPERTY matches PATTERN.
PATTERN can be a string (exact match) or a regex-capable string."
  (let ((value (case property
                 ((app-id)   (window-app-id window))
                 ((title)    (window-title window))
                 ((class)    (window-class window))
                 ((instance) (window-instance window))
                 (else ""))))
    (cond
     ((not value) #f)
     ;; If pattern looks like a regex (contains special chars), use regex
     ((or (string-prefix? "^" pattern)
          (string-contains pattern ".*")
          (string-contains pattern "[")
          (string-contains pattern "("))
      (let ((rx (catch #t
                  (lambda () (make-regexp pattern regexp/extended))
                  (lambda _ #f))))
        (and rx (regexp-exec rx value) #t)))
     ;; Otherwise exact match
     (else (string=? value pattern)))))

(define (window-rule-matches? rule window)
  "Check if RULE matches WINDOW."
  (every (lambda (criterion)
           (match-property? window (car criterion) (cdr criterion)))
         (window-rule-match rule)))

(define (window-rule-actions-apply! window actions)
  "Apply ACTIONS to WINDOW."
  (for-each
   (lambda (action)
     (let ((key (car action))
           (val (cdr action)))
       (case key
         ((workspace)
          (let ((target (workspace-find-by-name val)))
            (when target
              (window-move-to-workspace! window target))))
         ((container)
          (let ((workspace (window-workspace window)))
            (when workspace
              (let ((target (container-find-by-number val workspace)))
                (when target
                  (window-move-to-container! window target))))))
         ;; ((float)
         ;;  (when (and val (not (window-floating? window)))
         ;;    (window-toggle-float! window))
         ;;  (when (and (not val) (window-floating? window))
         ;;    (window-toggle-float! window)))
         ((fullscreen)
          (%window-fullscreen-set! window val))
         ((focus)
          (when val
            (let ((container (window-container window)))
              (when container
				(log-debug "focus window")
				;; TODO: focus window/container
                ))))
         (else
          (log-warn "Unknown window rule action: ~a" key)))))
   actions))

(define (window-rule-apply! window)
  "Apply matching window rules to WINDOW.
Rules are checked in order; the first match applies unless continue? is set."
  (let loop ((rules *window-rules*))
    (when (pair? rules)
      (let ((rule (car rules)))
        (if (window-rule-matches? rule window)
            (begin
              (log-info "Window rule matched for ~a (~a)"
                        (window-title window) (window-app-id window))
              (window-rule-actions-apply! window (window-rule-actions rule))
              (when (window-rule-continue? rule)
                (loop (cdr rules))))
            (loop (cdr rules)))))))

;;; hook integration
;;; rules are applied automatically via the new-window hook.
;;; the user can add this to their config or it's done in the main startup.
(define (install-rule-hook!)
  (gliver-hook-add! *window-created-hook*
    (lambda (win) (window-rule-apply! win))))

(install-rule-hook!)
