;;; gliver/core/hooks.scm --- Hook system for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver core hooks)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (gliver core logs)
  #:export (
			set-gliver-hook-functions!
			gliver-hook-functions
			gliver-hook-arity
			gliver-hook-name
			gliver-hook?
			%make-gliver-hook
			make-gliver-hook
			gliver-hook-add!
			gliver-hook-remove!
			%gliver-hook-run
			gliver-hook-run!
			gliver-hook-run-strict!
			gliver-hook-clear!
			gliver-hook-empty?
			*gliver-globals-bind-hook*
			*gliver-globals-unbind-hook*
			*gliver-globals-verify-hook*
			*gliver-listeners-attach-hook*
			*keybinding-sync-request-hook*
			*manager-connected-hook*
			*manager-disconnected-hook*
			*manager-destroy-hook*
			*manager-manage-start-hook*
			*manager-render-start-hook*
			%manager-session-unlocked-hook
			%manager-session-locked-hook
			%window-created-hook
			%window-destroy-hook
			%window-size-hint-changed-hook
			%window-size-changed-hook
			%window-app-id-changed-hook
			%window-title-changed-hook
			%window-parent-changed-hook
			%window-decoration-hint-changed-hook
			%window-pointer-move-requested-hook
			%window-pointer-resize-requested-hook
			%window-menu-requested-hook
			%window-maximize-requested-hook
			%window-unmaximize-requested-hook
			%window-fullscreen-requested-hook
			%window-fullscreen-exit-requested-hook
			%window-minimize-requested-hook
			%window-pid-changed-hook
			%window-presentation-hint-changed-hook
			%window-identifier-changed-hook
			*window-created-hook*
			*window-destroy-hook*
			*window-focus-hook*
			*window-unfocus-hook*
			*window-place-hook*
			*window-float-hook*
			*window-unfloat-hook*
			*window-urgent-hook*
			*window-size-changed-hook*
			*window-size-hint-changed-hook*
			*window-app-id-changed-hook*
			*window-title-changed-hook*
			*window-decoration-hint-changed-hook*
			*window-pointer-move-requested-hook*
			*window-pointer-resize-requested-hook*
			*window-menu-requested-hook*
			*window-resize-start-hook*
			*window-resize-end-hook*
			*window-capabilities-changed-hook*
			*window-fullscreen-entered-hook*
			*window-fullscreen-exited-hook*
			*window-fullscreen-entered-informed-hook*
			*window-fullscreen-exited-informed-hook*
			*window-maximized-hook*
			*window-unmaximized-hook*
			*window-pid-changed-hook*
			*window-parent-changed-hook*
			*window-presentation-hint-changed-hook*
			*window-maximize-requested-hook*
			*window-unmaximize-requested-hook*
			*window-fullscreen-requested-hook*
			*window-fullscreen-exit-requested-hook*
			*window-minimize-requested-hook*
			*container-split-hook*
			*container-destroy-hook*
			*container-resize-hook*
			*workspace-switch-hook*
			*workspace-new-hook*
			*workspace-destroy-hook*
			%output-created-hook
			%output-removed-hook
			%output-object-id-changed-hook
			%output-position-changed-hook
			%output-dimensions-changed-hook
			*output-created-hook*
			*output-removed-hook*
			*output-object-id-changed-hook*
			*output-name-changed-hook*
			*output-position-changed-hook*
			*output-dimensions-changed-hook*
			*output-destroy-hook*
			*output-change-hook*
			*output-focus-hook*
			%seat-created-hook
			%seat-removed-hook
			%seat-object-id-changed-hook
			%seat-window-pointer-entered-hook
			%seat-window-pointer-left-hook
			%seat-window-interacted-hook
			%seat-shell-interacted-hook
			%seat-op-delta-changed-hook
			%seat-op-released-hook
			%seat-pointer-position-changed-hook
			*seat-created-hook*
			*seat-destroy-hook*
			*seat-object-id-changed-hook*
			*seat-name-changed-hook*
			*seat-window-entered-changed-hook*
			*seat-window-interacted-hook*
			*seat-shell-interacted-hook*
			*seat-seat-op-delta-changed-hook*
			*seat-seat-op-released-hook*
			*seat-seat-pointer-position-changed-hook*
			*seat-window-focused-hook*
			*key-press-hook*
			*keymap-change-hook*
			*command-pre-hook*
			*command-post-hook*
			*startup-hook*
			*shutdown-hook*
			*restart-hook*
			*config-loaded-hook*
))

;;; hooks are named list of functions that are called when an event occurs.
(define-record-type <gliver-hook>
  (%make-gliver-hook name arity functions)
  gliver-hook?
  (name     gliver-hook-name)
  (arity    gliver-hook-arity)
  (functions gliver-hook-functions set-gliver-hook-functions!))

(define* (make-gliver-hook name #:optional (arity 0))
  "Create a new hook named NAME that expects ARITY arguments."
  (%make-gliver-hook name arity '()))

(define* (gliver-hook-add! hook fn #:optional (order 999))
  "Add function FN to HOOK with the given ORDER (default 999).
Functions with lower or negative order values are executed first.
If multiple functions share the same order, they execute in the order they were added."
  (let* ((current-funcs (gliver-hook-functions hook))
         (cleaned (filter (lambda (pair) (not (eq? (cdr pair) fn))) 
                          current-funcs))
         ;; append the new pair to the end of the list instead of the front
         (new-list (append cleaned (list (cons order fn)))))

    (set-gliver-hook-functions! hook
      (stable-sort new-list 
                   (lambda (a b) (< (car a) (car b)))))))

(define (gliver-hook-remove! hook fn)
  "Remove function FN from HOOK."
  (set-gliver-hook-functions! hook
    (filter (lambda (pair) (not (eq? (cdr pair) fn)))
            (gliver-hook-functions hook))))

(define (%gliver-hook-run hook strict? args)
  (log-debug "Running hook ~a ~a" (gliver-hook-name hook) args)
  (for-each
   (lambda (pair)
     (let ((fn (cdr pair)))
       (catch #t
         (lambda () (apply fn args))
         (lambda (key . rest)
           (log-error "Hook ~a: function raised ~a: ~a"
                      (gliver-hook-name hook) key rest)
           (when strict?
             (exit 1))))))
   (gliver-hook-functions hook)))

(define (gliver-hook-run! hook . args)
  "Run all functions in HOOK with ARGS in their defined order safely.
If a function fails, the error is logged and the next function runs."
  (%gliver-hook-run hook #f args))

(define (gliver-hook-run-strict! hook . args)
  "Run all functions in HOOK with ARGS in their defined order strictly.
If a function fails, the error is logged and the script is terminated."
  (%gliver-hook-run hook #t args))

(define (gliver-hook-clear! hook)
  "Remove all functions from HOOK."
  (set-gliver-hook-functions! hook '()))

(define (gliver-hook-empty? hook)
  "Return #t if HOOK has no registered functions."
  (null? (gliver-hook-functions hook)))

;; river/wayland hooks, users should probably use only standard hooks
;; these hooks are intended to be used to extend gliver's core

;; registry protocol-name object-id version
(define *gliver-globals-bind-hook*		(make-gliver-hook 'gliver-globals-bind 4))
(define *gliver-globals-unbind-hook*	(make-gliver-hook 'gliver-globals-unbind 0))
(define *gliver-globals-verify-hook*	(make-gliver-hook 'gliver-verify-unbind 0))
(define *gliver-listeners-attach-hook*	(make-gliver-hook 'gliver-listeners-attach 0))
(define *keybinding-sync-request-hook*	(make-gliver-hook 'keybinding-sync-request 0))

;;; manager hooks
(define *manager-connected-hook*		(make-gliver-hook 'manager-connected 0))
(define *manager-disconnected-hook*		(make-gliver-hook 'manager-disconnected 0))
(define *manager-destroy-hook*			(make-gliver-hook 'manager-destroy 0))
(define *manager-manage-start-hook*		(make-gliver-hook 'manager-manage-start 0))
(define *manager-render-start-hook*		(make-gliver-hook 'manager-render-start 0))
(define %manager-session-unlocked-hook	(make-gliver-hook '%manager-session-unlocked 0))
(define %manager-session-locked-hook	(make-gliver-hook '%manager-session-locked 0))

;;; window hooks
(define %window-created-hook					(make-gliver-hook '%window-created 3))
(define %window-destroy-hook					(make-gliver-hook '%window-destroy 1))
(define %window-size-hint-changed-hook			(make-gliver-hook '%window-size-hint-changed 5))
(define %window-size-changed-hook				(make-gliver-hook '%window-size-changed 3))
(define %window-app-id-changed-hook				(make-gliver-hook '%window-app-id-changed 2))
(define %window-title-changed-hook				(make-gliver-hook '%window-title-changed 2))
(define %window-parent-changed-hook				(make-gliver-hook '%window-parent-changed 2))
(define %window-decoration-hint-changed-hook    (make-gliver-hook '%window-decoration-hint-changed 2))
(define %window-pointer-move-requested-hook     (make-gliver-hook '%window-pointer-move-requested 2))
(define %window-pointer-resize-requested-hook   (make-gliver-hook '%window-pointer-resize-requested 3))
(define %window-menu-requested-hook				(make-gliver-hook '%window-menu-requested 3))
(define %window-maximize-requested-hook			(make-gliver-hook '%window-maximize-requested 1))
(define %window-unmaximize-requested-hook       (make-gliver-hook '%window-unmaximize-requested 1))
(define %window-fullscreen-requested-hook       (make-gliver-hook '%window-fullscreen-requested 2))
(define %window-fullscreen-exit-requested-hook  (make-gliver-hook '%window-fullscreen-exit-requested 1))
(define %window-minimize-requested-hook			(make-gliver-hook '%window-minimize-requested 1))
(define %window-pid-changed-hook				(make-gliver-hook '%window-pid-changed 2))
(define %window-presentation-hint-changed-hook  (make-gliver-hook '%window-presentation-hint-changed 2))
(define %window-identifier-changed-hook			(make-gliver-hook '%window-identifier-changed 2))

(define *window-created-hook*						(make-gliver-hook 'window-created 1))
(define *window-destroy-hook*						(make-gliver-hook 'window-destroy 1))
(define *window-focus-hook*							(make-gliver-hook 'window-focus 2))
(define *window-unfocus-hook*						(make-gliver-hook 'window-unfocus 1))
(define *window-place-hook*							(make-gliver-hook 'window-place 2))
(define *window-float-hook*							(make-gliver-hook 'window-float 1))
(define *window-unfloat-hook*						(make-gliver-hook 'window-unfloat 1))
(define *window-urgent-hook*						(make-gliver-hook 'window-urgent 1))
(define *window-size-changed-hook*					(make-gliver-hook 'window-size-changed 3))
(define *window-size-hint-changed-hook*				(make-gliver-hook 'window-size-hint-changed 5))
(define *window-app-id-changed-hook*				(make-gliver-hook 'window-app-id-changed 2))
(define *window-title-changed-hook*					(make-gliver-hook 'window-title-changed 2))
(define *window-decoration-hint-changed-hook*       (make-gliver-hook 'window-decoration-hint-changed 2))
(define *window-pointer-move-requested-hook*		(make-gliver-hook 'window-pointer-move-requested 2))
(define *window-pointer-resize-requested-hook*      (make-gliver-hook 'window-pointer-resize-requested 3))
(define *window-menu-requested-hook*				(make-gliver-hook 'window-menu-requested 3))
(define *window-resize-start-hook*					(make-gliver-hook 'window-resize-start 1))
(define *window-resize-end-hook*					(make-gliver-hook 'window-resize-end 1))
(define *window-capabilities-changed-hook*			(make-gliver-hook 'window-capabilities-changed 2))
(define *window-fullscreen-entered-hook*			(make-gliver-hook 'window-fullscreen-entered 2))
(define *window-fullscreen-exited-hook*				(make-gliver-hook 'window-fullscreen-exited 2))
(define *window-fullscreen-entered-informed-hook*   (make-gliver-hook 'window-fullscreen-entered-informed 2))
(define *window-fullscreen-exited-informed-hook*    (make-gliver-hook 'window-fullscreen-exited-informed 2))
(define *window-maximized-hook*						(make-gliver-hook 'window-maximized 2))
(define *window-unmaximized-hook*					(make-gliver-hook 'window-unmaximized 2))
(define *window-pid-changed-hook*					(make-gliver-hook 'window-pid-changed 2))
(define *window-parent-changed-hook*				(make-gliver-hook 'window-parent-changed 2))
(define *window-presentation-hint-changed-hook*     (make-gliver-hook 'window-presentation-hint-changed 2))
(define *window-maximize-requested-hook*			(make-gliver-hook 'window-maximize-requested 1))
(define *window-unmaximize-requested-hook*			(make-gliver-hook 'window-unmaximize-requested 1))
(define *window-fullscreen-requested-hook*			(make-gliver-hook 'window-fullscreen-requested 1))
(define *window-fullscreen-exit-requested-hook*     (make-gliver-hook 'window-fullscreen-exit-requested 1))
(define *window-minimize-requested-hook*			(make-gliver-hook 'window-minimize-requested 1))

(define *container-split-hook*       (make-gliver-hook 'container-split 2))
(define *container-destroy-hook*     (make-gliver-hook 'container-destroy 2))
(define *container-resize-hook*      (make-gliver-hook 'container-resize 1))
(define *workspace-switch-hook*      (make-gliver-hook 'workspace-switch 2))
(define *workspace-new-hook*         (make-gliver-hook 'workspace-new 1))
(define *workspace-destroy-hook*     (make-gliver-hook 'workspace-destroy 1))

(define %output-created-hook				(make-gliver-hook '%output-created 3))
(define %output-removed-hook				(make-gliver-hook '%output-destroy 2))
(define %output-object-id-changed-hook		(make-gliver-hook '%output-object-id-changed 3))
(define %output-position-changed-hook		(make-gliver-hook '%output-position-changed 4))
(define %output-dimensions-changed-hook 	(make-gliver-hook '%output-dimensions-changed 4))

(define *output-created-hook*			    (make-gliver-hook 'output-created 1))
(define *output-removed-hook*			    (make-gliver-hook 'output-removed 1))
(define *output-object-id-changed-hook*		(make-gliver-hook 'output-object-id-changed 2))

;; ensure these are still used
(define *output-name-changed-hook*			(make-gliver-hook 'output-name-changed 2))
(define *output-position-changed-hook*		(make-gliver-hook 'output-position-changed 3))
(define *output-dimensions-changed-hook*	(make-gliver-hook 'output-dimensions-changed 3))
(define *output-destroy-hook*				(make-gliver-hook 'output-destroy 1))
(define *output-change-hook*				(make-gliver-hook 'output-change 1))
(define *output-focus-hook*					(make-gliver-hook 'output-focus 2))

(define %seat-created-hook                  (make-gliver-hook '%seat-created 3))
(define %seat-removed-hook                  (make-gliver-hook '%seat-removed 2))
(define %seat-object-id-changed-hook        (make-gliver-hook '%seat-object-id-changed 3))
(define %seat-window-pointer-entered-hook   (make-gliver-hook '%seat-window-pointer-entered 3))
(define %seat-window-pointer-left-hook      (make-gliver-hook '%seat-window-pointer-left 2))
(define %seat-window-interacted-hook        (make-gliver-hook '%seat-window-interacted 3))
(define %seat-shell-interacted-hook         (make-gliver-hook '%seat-shell-interacted 3))
(define %seat-op-delta-changed-hook         (make-gliver-hook '%seat-op-delta-changed 4))
(define %seat-op-released-hook              (make-gliver-hook '%seat-op-released 2))
(define %seat-pointer-position-changed-hook (make-gliver-hook '%seat-pointer-position-changed 4))

(define *seat-created-hook*							(make-gliver-hook 'seat-created 1))
(define *seat-destroy-hook*							(make-gliver-hook 'seat-destroy 1))
(define *seat-object-id-changed-hook*				(make-gliver-hook 'seat-object-id-changed 2))
(define *seat-name-changed-hook*					(make-gliver-hook 'seat-name-changed 2))
(define *seat-window-entered-changed-hook*          (make-gliver-hook 'seat-window-entered-changed 2))
(define *seat-window-interacted-hook*				(make-gliver-hook 'seat-window-interacted 2))
(define *seat-shell-interacted-hook*				(make-gliver-hook 'seat-shell-interacted 2))
(define *seat-seat-op-delta-changed-hook*			(make-gliver-hook 'seat-seat-op-delta-changed 3))
(define *seat-seat-op-released-hook*				(make-gliver-hook 'seat-seat-op-released 1))
(define *seat-seat-pointer-position-changed-hook*   (make-gliver-hook 'seat-seat-pointer-position-changed 3))
(define *seat-window-focused-hook*					(make-gliver-hook 'seat-window-focused 2))

(define *key-press-hook*             (make-gliver-hook 'key-press 1))
(define *keymap-change-hook*         (make-gliver-hook 'keymap-change 1))
(define *command-pre-hook*           (make-gliver-hook 'command-pre 2))
(define *command-post-hook*          (make-gliver-hook 'command-post 3))
(define *startup-hook*               (make-gliver-hook 'startup 0))
(define *shutdown-hook*              (make-gliver-hook 'shutdown 0))
(define *restart-hook*               (make-gliver-hook 'restart 0))
(define *config-loaded-hook*         (make-gliver-hook 'config-loaded 0))

