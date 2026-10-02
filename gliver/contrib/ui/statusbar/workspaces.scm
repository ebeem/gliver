;;; gliver/contrib/ui/statusbar/workspaces.scm --- Workspaces statusbar module
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver contrib ui statusbar workspaces)
  #:use-module (cairo)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (gliver core)
  #:use-module (gliver deps pango)
  #:use-module (gliver deps color)
  #:use-module (gliver contrib ui statusbar base)
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

  ;; custom measure procedure for all workspaces
  (define (%measure-workspace cr module output)
    "Returns values width and height."
    (let* ((workspaces (%get-workspaces output)))
      (if (null? workspaces)
          (values 0 0)
          (let loop ((workspaces-list workspaces)
                     (width 0)
                     (height 0))
            (if (null? workspaces-list)
                (let ((final-w (+ width (* (max 0 (- (length workspaces) 1)) spacing))))
                  (values final-w height))
                (let* ((workspace (car workspaces-list))
                       (label (%workspace-label workspace)))
                  (let-values (((lwidth lheight) (pango-measure-text cr label #:font *statusbar-font* #:font-size *statusbar-font-size*)))
                    (let ((pwidth (+ lwidth (* padding-x 2)))
                          (pheight (+ lheight (* padding-y 2))))
                      (loop (cdr workspaces-list)
                            (+ width pwidth)
                            (max height pheight))))))))))

  ;; custom render procedure that draws each workspace and returns hit-boxes
  (define (%render-workspace cr x y width height module output record-hit-box!)
    (define (%current-workspace)
      (if output
          (or (output-workspace-current output) (workspace-current))
          (workspace-current)))

    (define (%set-color! hex)
      (let ((rgba (and hex (parse-hex-color-rgba hex))))
        (and rgba
             (begin
               (apply cairo-set-source-rgba cr rgba)
               #t))))

    (define (%status->colors status)
      (case status
        ((active)   (values active-bg   active-fg))
        ((urgent)   (values urgent-bg   urgent-fg))
        ((occupied) (values occupied-bg occupied-fg))
        (else       (values empty-bg    empty-fg))))

    (define (%render-workspace-pill workspace cur-x current-workspace)
      (let* ((lbl    (%workspace-label workspace))
             (status (%workspace-status workspace current-workspace)))
        (let-values (((bg fg) (%status->colors status))
                     ((lw lh) (pango-measure-text cr lbl #:font *statusbar-font* #:font-size *statusbar-font-size*)))
          (let* ((pw (+ lw (* padding-x 2)))
                 (ph (min height (+ lh (* padding-y 2))))
                 (py (+ y (/ (- height ph) 2.0))))

            ;; background
            (when (%set-color! bg)
              (statusbar-cairo-rounded-rectangle cr cur-x py pw ph border-radius)
              (cairo-fill cr))

            ;; border
            (when (and border-color (> (or border-width 0) 0) (%set-color! border-color))
              (cairo-set-line-width cr border-width)
              (statusbar-cairo-rounded-rectangle cr cur-x py pw ph border-radius)
              (cairo-stroke cr))

            ;; label
            (when (%set-color! fg)
              (let ((tx (+ cur-x padding-x))
                    (ty (+ py (/ (- ph lh) 2.0))))
                (pango-draw-text cr lbl
                                 #:x tx #:y ty
                                 #:font *statusbar-font*
                                 #:font-size *statusbar-font-size*
                                 #:markup? #f)))

            ;; hit box
            (when (procedure? record-hit-box!)
              (record-hit-box! cur-x py (+ cur-x pw) (+ py ph) module workspace))
            pw))))

    (let ((current-workspace (%current-workspace)))
      (let loop ((workspaces (%get-workspaces output))
                 (cur-x x))
        (when (pair? workspaces)
          (let ((pw (%render-workspace-pill (car workspaces) cur-x current-workspace)))
            (loop (cdr workspaces) (+ cur-x pw spacing)))))))

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
   #:measure-fn (if active-only? #f %measure-workspace)
   #:render-fn (if active-only? #f %render-workspace)
   #:on-click %on-click
   #:on-scroll %on-scroll))

(statusbar-register-module! 'workspaces make-module-workspaces)
