;;; gliver/core/layout.scm --- Core data model for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver core layout)
  #:use-module (gliver core hooks)
  #:use-module (gliver core types)
  #:use-module (gliver core logs)
  #:use-module (gliver contrib layout alternating)
  #:export (setup-layout-manager!))

(define (make-window-handler hook-name)
  (lambda args
    (let* ((window (car args))
           (container (if window (window-container window) #f))
		   (workspace (if container (container-workspace container) #f)))
	  (gliver-hook-run! *manager-layout-changed-hook*
						hook-name workspace container window))))

(define (make-container-handler hook-name)
  (lambda args
    (let* ((container (car args))
		   (workspace (if container (container-workspace container) #f)))
	  (gliver-hook-run! *manager-layout-changed-hook*
						hook-name workspace container #f))))

(define (make-workspace-handler hook-name)
  (lambda args
    (let* ((workspace (car args)))
	  (gliver-hook-run! *manager-layout-changed-hook*
						hook-name workspace container #f))))

(define (make-output-handler hook-name)
  (lambda args
    (let* ((output (car args))
		   (workspace (output-workspaces output)))
	  (gliver-hook-run! *manager-layout-changed-hook*
						hook-name workspace container #f))))

(define (setup-layout-manager!)

  ;; window handlers
  (for-each (lambda (hook)
              (gliver-hook-add! hook (make-window-handler (gliver-hook-name hook))))
            (list *window-created-hook*
                  *window-place-hook*
                  *window-float-hook*
				  *window-focused-hook*
                  *window-unfloat-hook*
                  *window-size-changed-hook*
                  *window-size-hint-changed-hook*
                  *window-decoration-hint-changed-hook*
                  *window-resize-end-hook*
                  *window-fullscreen-entered-hook*
                  *window-fullscreen-exited-hook*
                  *window-maximized-hook*
                  *window-unmaximized-hook*))

  ;; custom window handlers
  ;; *window-destroyed-hook* handler
  (gliver-hook-add! *window-destroyed-hook*
					(lambda args
					  (gliver-hook-run! *manager-layout-changed-hook*
										(gliver-hook-name *window-destroyed-hook*)
										(list-ref args 2)
										(list-ref args 1)
										(list-ref args 0))))

  ;; containers handlers
  (for-each (lambda (hook)
              (gliver-hook-add! hook (make-container-handler (gliver-hook-name hook))))
            (list *container-split-hook*
                  *container-destroy-hook*
                  *container-resize-hook*))

  ;; workspaces handlers
  (for-each (lambda (hook)
              (gliver-hook-add! hook (make-workspace-handler (gliver-hook-name hook))))
            (list *workspace-created-hook*))

  ;; outputs handlers
  (for-each (lambda (hook)
              (gliver-hook-add! hook (make-output-handler (gliver-hook-name hook))))
            (list *output-dimensions-changed-hook*)))

  (setup-layout-manager!)
