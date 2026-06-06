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

(define (setup-layout-manager!)
  (define (make-handler type hook-name)
    (lambda args
      (let* ((window (if (eq? type 'window) (car args) #f))
             (container (if (eq? type 'container)
                            (car args)
                            (and window (window-container window))))
             (workspace (cond ((eq? type 'output)
                               (output-workspace-current (if (pair? args) (car args) (output-current))))
							  ((eq? type 'workspace)
                               (car args))
                              (container
                               (container-workspace container))
                              (else #f))))
        (gliver-hook-run! *manager-layout-changed-hook*
                          hook-name
                          workspace
                          container
                          window))))

  (for-each (lambda (hook)
              (gliver-hook-add! hook (make-handler 'window (gliver-hook-name hook))))
            (list *window-created-hook*
                  *window-destroyed-hook*
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

  (for-each (lambda (hook)
              (gliver-hook-add! hook (make-handler 'container (gliver-hook-name hook))))
            (list *container-split-hook*
                  *container-destroy-hook*
                  *container-resize-hook*))

  (for-each (lambda (hook)
              (gliver-hook-add! hook (make-handler 'workspace (gliver-hook-name hook))))
            (list *workspace-created-hook*))

  (for-each (lambda (hook)
              (gliver-hook-add! hook (make-handler 'output (gliver-hook-name hook))))
            (list *output-dimensions-changed-hook*)))

(setup-layout-manager!)
