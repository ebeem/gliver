;;; gliver/contrib/ui/gleui/statusbar/modules/workspaces.scm --- Workspaces statusbar module
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver contrib ui gleui statusbar modules workspaces)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (gliver core)
  #:use-module (gliver contrib ui gleui widgets)
  #:use-module (gliver contrib ui gleui statusbar state)
  #:declarative? #f
  #:export (
			make-module-workspaces
))

(define (%workspace-status workspace current-workspace)
  "Returns workspace status: active, urgent, occupied, or empty."
  (cond
   ((and current-workspace (eq? workspace current-workspace)) 'active)
   ;; urgent if any window in workspace is marked urgent
   ((any (lambda (w) (and (window? w) (window-urgent? w)))
         (workspace-windows workspace))
    'urgent)
   ;; occupied if it has windows
   ((not (null? (workspace-windows workspace))) 'occupied)
   ;; empty otherwise
   (else 'empty)))

(define* (make-module-workspaces #:key
                                 (id 'workspaces)
                                 (section 'left)
                                 (custom-labels '())  ;; alist to overwrite labels
                                 (active-only? #f)
                                 (hooks (list *workspace-switch-hook*
                                              *workspace-created-hook*
                                              *workspace-destroy-hook*))
                                 (format-name #f)
                                 (bg-color *statusbar-bg-color*)
                                 (fg-color *statusbar-fg-color*)
                                 (active-bg *statusbar-bg-color*)
                                 (active-fg *theme-fg-alt*)
                                 (occupied-bg *theme-fg-dim*)
                                 (occupied-fg *statusbar-fg-color*)
                                 (empty-bg *theme-bg-dim*)
                                 (empty-fg *theme-fg-dim*)
                                 (urgent-bg *statusbar-bg-color*)
                                 (urgent-fg *theme-urgent-color*)
                                 (border-color *statusbar-border-color*)
                                 (border-width 0)
                                 (border-radius 6)
                                 (padding-x 10)
                                 (padding-y 4)
                                 (spacing 6))
  "Create a workspaces module."

  (define (%workspace-label workspace)
    "Returns the workspace label, may have icon associated."
    (let* ((id (workspace-id workspace))
           (raw-name (workspace-name workspace))
           (formatted (and (procedure? format-name)
                           (catch #t
                             (lambda () (format-name workspace))
                             (lambda (k . args)
                               (log-warn "Statusbar: workspaces format-name failed for workspace ~a: ~a ~a"
                                         id k args)
                               #f))))
           (name (if (string? formatted)
                     formatted
                     raw-name))
           ;; check if custom label is provided
           (custom-label (assoc-ref custom-labels name)))
      (or custom-label name (format #f "~a" id))))

  (define (%update module output)
    "Update active workspace text and state on module."
    (let* ((ws (if output
                   (or (output-workspace-current output)
                       (workspace-current))
                   (workspace-current))))
      (if (and ws (workspace? ws))
          (let ((label (%workspace-label ws)))
            (statusbar-module-text-set! module (or label ""))
            (statusbar-module-state-set! module ws)
            (statusbar-module-tooltip-set! module (workspace-name ws))
            (statusbar-module-visible?-set! module #t))
          (begin
            (statusbar-module-text-set! module "")
            (statusbar-module-state-set! module #f)
            (statusbar-module-visible?-set! module #f)))))

  (define (%get-workspaces output)
    "Returns the list of available workspaces."
    (if output (output-workspaces output) '()))

  (define (%status->colors status)
    (case status
      ((active)   (values active-bg   active-fg))
      ((urgent)   (values urgent-bg   urgent-fg))
      ((occupied) (values occupied-bg occupied-fg))
      (else       (values empty-bg    empty-fg))))

  (define (%pill-widget module output workspace current-workspace font font-size bar-height)
    "Build a clickable gleui pill widget for WORKSPACE."
    (let* ((lbl    (%workspace-label workspace))
           (status (%workspace-status workspace current-workspace)))
      (call-with-values (lambda () (%status->colors status))
        (lambda (bg fg)
          (gleui-w-clickable
           (gleui-w-box
            (gleui-w-text lbl
                          #:font font
                          #:font-size font-size
                          #:color fg
                          #:markup? #f
                          #:multiline? #f)
            #:max-height bar-height
            #:padding-x padding-x
            #:padding-y padding-y
            #:radius border-radius
            #:bg-color bg
            #:border-color border-color
            #:border-width (or border-width 0)
            #:inset-border? #f
            #:align 'center)
           #:data workspace
           #:on-click (lambda (button x y target)
                        (%on-click button x y module output target))
           #:on-scroll (lambda (axis delta _target)
                         (%on-scroll axis delta module output)))))))

  (define* (%workspace-widget module output
                              #:key
                              (bar-height #f)
                              (font *statusbar-font*)
                              (font-size *statusbar-font-size*)
                              #:allow-other-keys)
    "Build declarative gleui row widget displaying all workspaces on OUTPUT."
    (let ((workspaces (%get-workspaces output)))
      (and (pair? workspaces)
           (let ((current-workspace (or (output-workspace-current output)
                                        (workspace-current))))
             (gleui-w-row
              (map (lambda (ws)
                     (%pill-widget module output ws current-workspace font font-size bar-height))
                   workspaces)
              #:gap spacing
              #:cross-align 'center)))))

  (define (%on-click button x y module output workspace)
    "Switch to workspace on left click."
    (when (and (= button 272) (or (workspace? workspace) active-only?))
      (let ((target (or workspace
                        (and output (output-workspace-current output))
                        (workspace-current))))
        (when (and target (workspace? target))
          (log-info "Statusbar: switching to workspace ~a" (workspace-name target))
          (workspace-focus! target)))))

  (define (%on-scroll axis delta module output)
    "Cycle workspaces on scroll wheel"
    ;; : delta < 0 is scroll up, delta > 0 is scroll down.
    (let ((current (if output
                       (or (output-workspace-current output) (workspace-current))
                       (workspace-current))))
      (when current
        (let ((target (if (< delta 0)
                          (workspace-prev current)
                          (workspace-next current))))
          (when target
            (workspace-focus! target))))))

  (make-statusbar-module
   #:id id
   #:name "Workspaces"
   #:section section
   #:interval #f  ;; event-driven via hooks
   #:hooks hooks
   #:bg-color (if active-only? active-bg bg-color)
   #:fg-color (if active-only? active-fg fg-color)
   #:border-color border-color
   #:border-width border-width
   #:border-radius border-radius
   #:padding-x padding-x
   #:padding-y padding-y
   #:update-fn (if active-only? %update #f)
   #:widget-fn (if active-only? #f %workspace-widget)
   #:on-click %on-click
   #:on-scroll %on-scroll))

(statusbar-register-module! 'workspaces make-module-workspaces)
