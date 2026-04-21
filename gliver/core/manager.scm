;;; gliver/core.scm --- Core data model for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver core manager)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (system foreign)
  #:use-module (gliver core logs)
  #:use-module (gliver core hooks)
  #:use-module (gliver keybindings)
  #:export (*manager*
			manager-state?
            manager-outputs manager-outputs-set!
            manager-output-current manager-output-current-set!
            manager-output-previous manager-output-previous-set!
            manager-windows manager-windows-set!
            manager-window-number-next
            manager-prefix-key manager-prefix-key-set!
            manager-prefix-timeout manager-prefix-timeout-set!
            manager-message-timeout manager-message-timeout-set!
            manager-mode manager-mode-set!
            manager-border-width manager-border-width-set!
            manager-border-color-focused manager-border-color-focused-set!
            manager-border-color-unfocused manager-border-color-unfocused-set!
            manager-border-color-urgent manager-border-color-urgent-set!
            manager-container-gap manager-container-gap-set!
            manager-container-outer-gap manager-container-outer-gap-set!
            manager-running? manager-running-set!  
			manager-wl-proxy-set!
			manager-wl-proxy
			manager-tag-next-set!
			manager-tag-next
			manager-container-number-next-set!
			manager-container-number-next
			manager-window-number-next-set!
			manager-seats-set!
			manager-seats
			manager-window-number-next!
			manager-tag-next!
			manager-workspace-number-next!
			manager-output-number-next!
			manager-workspace-number-next-set!
			manager-workspace-number-next
			manager-output-number-next-set!
			manager-output-number-next
			%make-manager-state))

;;; display (global state)
(define-record-type <manager-state>
  (%make-manager-state outputs output-current output-previous
                       seats windows prefix-key prefix-timeout
                       message-timeout mode
                       border-width border-color-focused
                       border-color-unfocused border-color-urgent
                       container-gap container-outer-gap running?
                       window-number-next container-number-next next-tag-bit
					   wl-proxy)
  manager-state?
  (outputs                manager-outputs                manager-outputs-set!)
  (output-current         manager-output-current         manager-output-current-set!)
  (output-previous        manager-output-previous        manager-output-previous-set!)
  (seats                  manager-seats                  manager-seats-set!)
  (windows                manager-windows                manager-windows-set!)
  (prefix-key             manager-prefix-key             manager-prefix-key-set!)
  (prefix-timeout         manager-prefix-timeout         manager-prefix-timeout-set!)
  (message-timeout        manager-message-timeout        manager-message-timeout-set!)
  (mode                   manager-mode                   manager-mode-set!)
  (border-width           manager-border-width           manager-border-width-set!)
  (border-color-focused   manager-border-color-focused   manager-border-color-focused-set!)
  (border-color-unfocused manager-border-color-unfocused manager-border-color-unfocused-set!)
  (border-color-urgent    manager-border-color-urgent    manager-border-color-urgent-set!)
  (container-gap          manager-container-gap          manager-container-gap-set!)
  (container-outer-gap    manager-container-outer-gap    manager-container-outer-gap-set!)
  (running?               manager-running?               manager-running-set!)
  (window-number-next     manager-window-number-next     manager-window-number-next-set!)
  (container-number-next  manager-container-number-next  manager-container-number-next-set!)
  (output-number-next     manager-output-number-next     manager-output-number-next-set!)
  (workspace-number-next  manager-workspace-number-next  manager-workspace-number-next-set!)
  (next-tag-bit           manager-tag-next               manager-tag-next-set!)
  (wl-proxy               manager-wl-proxy               manager-wl-proxy-set!))

(define *manager*
  (%make-manager-state
   '()   ; outputs
   #f    ; output-current
   #f    ; output-previous
   '()   ; seats
   '()   ; windows
   (make-gliver-key '(Control) 't) ; prefix-key
   1000  ; prefix-timeout ms
   5     ; message-timeout seconds
   'normal ; mode
   2     ; border-width
   "#5588ff"  ; border-color-focused
   "#333333"  ; border-color-unfocused
   "#ff5555"  ; border-color-urgent
   0     ; container-gap
   0     ; container-outer-gap
   #f    ; running?
   0     ; window-number-next
   0     ; container-number-next
   1     ; next-tag-bit
   #f))  ;; wl-proxy

(define (manager-window-number-next!)
  (let ((id (manager-window-number-next *manager*)))
    (manager-window-number-next-set! *manager* (1+ id))
    id))

(define (manager-workspace-number-next!)
  (let ((id (manager-workspace-number-next *manager*)))
    (manager-workspace-number-next-set! *manager* (1+ id))
    id))

(define (manager-output-number-next!)
  (let ((id (manager-output-number-next *manager*)))
    (manager-output-number-next-set! *manager* (1+ id))
    id))

(define (manager-tag-next!)
  (let ((bit (manager-tag-next *manager*)))
    (manager-tag-next-set! *manager* (ash bit 1))
    bit))

