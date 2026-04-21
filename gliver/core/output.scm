;;; gliver/core/output.scm --- Core data model for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver core output)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (system foreign)
  #:use-module (gliver core manager)
  #:use-module (gliver core logs)
  #:use-module (gliver core hooks)
  #:use-module (gliver keybindings)
  #:export (<output>
			%make-output
			output-wl-proxy-set!
			output-wl-proxy
			output-workspace-previous-set!
			output-workspace-previous
			output-workspace-current-set!
			output-workspace-current
			output-workspaces-set!
			output-workspaces
			output-height-set!
			output-height
			output-width-set!
			output-width
			output-y-set!
			output-y
			output-x-set!
			output-x
			output-name-set!
			output-name
			output-id-set!
			output-id
			output?
			output-find-by-proxy
			output-find-by-name
			output-find-by-id
			output-current
			output-add!
			output-remove!
			output-next
			output-prev))

;;; output: similar to an emacs container and stumpwm screen head
;;; a single logical screen/monitor, treated by wayland as output
(define-record-type <output>
  (%make-output id name x y width height workspaces workspace-current
                workspace-previous wl-proxy)
  output?
  (id                 output-id                 output-id-set!)
  (name               output-name               output-name-set!)
  (x                  output-x                  output-x-set!)
  (y                  output-y                  output-y-set!)
  (width              output-width              output-width-set!)
  (height             output-height             output-height-set!)
  (workspaces         output-workspaces         output-workspaces-set!)
  (workspace-current  output-workspace-current  output-workspace-current-set!)
  (workspace-previous output-workspace-previous output-workspace-previous-set!)
  (wl-proxy           output-wl-proxy           output-wl-proxy-set!))

(set-record-type-printer! <output>
  (lambda (out port)
    (format port "#<output ~s ~ax~a+~a+~a (~a workspaces)>"
            (output-name out)
            (output-width out) (output-height out)
            (output-x out) (output-y out)
            (length (output-workspaces out)))))

(define* (make-output name
                      #:key (id (manager-output-number-next!)) (wl-proxy #f) (x 0) (y 0) (width 1920) (height 1080)
					  (workspaces '()) (workspace-current #f) (workspace-previous #f))
  "Create a new <output> record with the given NAME.
Other parameters (x, y, width, height, wl-proxy) can be provided as keyword arguments."
  (%make-output id name 
                x y
                width height
				workspaces workspace-current workspace-previous
                wl-proxy))

(define (output-find-by-proxy proxy)
  "Look up the <output> record by comparing the raw memory address of the proxy."
  (if (not (pointer? proxy))
      #f ;; early exit
      (let ((addr (pointer-address proxy))
            (outputs (manager-outputs *manager*)))
        (find (lambda (output)
                (let ((output-proxy (output-wl-proxy output)))
                  (and (pointer? output-proxy)
                       (= (pointer-address output-proxy) addr))))
              outputs))))

(define (output-find-by-name output-name)
  (find (lambda (s) (string=? (output-name s) output-name))
        (manager-outputs *manager*)))

(define (output-find-by-id target-id)
  (find (lambda (output) (= (output-id output) target-id))
        (manager-outputs *manager*)))

(define (output-current)
  (manager-output-current *manager*))

;;; output management
(define (output-add! output)
  (manager-outputs-set! *manager*
    (append (manager-outputs *manager*) (list output)))
  (unless (manager-output-current *manager*)
    (manager-output-current-set! *manager* output))
  (gliver-hook-run! *output-change-hook* output))

(define (output-remove! output)
  (let ((remaining (delete output (manager-outputs *manager*))))
    (manager-outputs-set! *manager* remaining)
    (when (eq? (manager-output-current *manager*) output)
      (manager-output-current-set! *manager*
        (and (pair? remaining) (car remaining))))
    (gliver-hook-run! *output-change-hook* output)))

(define (output-next)
  (let* ((outputs (manager-outputs *manager*))
         (current (output-current))
         (idx (list-index (lambda (s) (eq? s current)) outputs)))
    (and idx (list-ref outputs (modulo (1+ idx) (length outputs))))))

(define (output-prev)
  (let* ((outputs (manager-outputs *manager*))
         (current (output-current))
         (idx (list-index (lambda (s) (eq? s current)) outputs)))
    (and idx (list-ref outputs (modulo (+ idx (length outputs) -1)
                                       (length outputs))))))

