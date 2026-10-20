;;; gliver/contrib/workspaces/workspace-group.scm --- Group workspaces across outputs
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; Workspace grouping allows workspaces spanning multiple outputs/monitors
;;; to be switched together. When a workspace is focused, other workspaces in
;;; its group are switched to in the other outputs.
;;;
;;; Usage:
;;;   (use-modules (gliver contrib workspaces workspace-group))
;;;
;;;   ;; enable with explicit groups (by name, record, or id):
;;;   (workspace-group-enable! (list (list w1 w2) (list w3 w4)))
;;;   (workspace-group-enable! '(("workspace-01" "workspace-02") ("workspace-11" "workspace-12")))
;;;
;;;   ;; enable with auto-grouping by index across available outputs:
;;;   (workspace-group-enable!)

(define-module (gliver contrib workspaces workspace-group)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (gliver core)
  #:declarative? #f
  #:export (
			*workspace-groups*
			*workspace-group-enabled*
			*workspace-group-syncing?*
			workspace-group-sync!
			workspace-group-on-workspace-switch
			workspace-group-set!
			workspace-group-add!
			workspace-group-remove!
			workspace-group-clear!
			workspace-group-enable!
			workspace-group-disable!
))

(define-var *workspace-groups* '()
			"List of workspace groups that switch together.
Each group is a list of <workspace> records.")

(define-var *workspace-group-enabled* #f
			"Whether workspace group synchronization is enabled.")

(define-var *workspace-group-syncing?* #f
			"Internal re-entrancy guard to prevent recursive workspace switch notifications.")

(define (%workspaces-all)
  "Return all workspaces on all outputs."
  (append-map output-workspaces (manager-outputs *manager*)))

(define (%workspace-resolve entry)
  "Return from all workspaces the workspace matching ENTRY (record, name, or id)."
  (cond
   ((workspace? entry) entry)
   ((number? entry)
    (find (lambda (w) (= (workspace-id w) entry)) (%workspaces-all)))
   ((string? entry)
    (find (lambda (w) (string=? (workspace-name w) entry)) (%workspaces-all)))
   (else #f)))

(define (%normalize-groups args)
  "Normalize ARGS into a list of groups, where each group is a list of <workspace> records.
ARGS can be passed as:
  - a list of groups: '((w1 w2) (w3 w4))
  - a nested list containing groups: '(((w1 w2) (w3 w4)))
  - a single group of entries: '(w1 w2)."
  (let ((raw (cond ((null? args) '())
                   ((and (pair? (car args)) (every list? (car args))) (car args))
                   ((every list? args) args)
                   (else (list args)))))
    (filter-map (lambda (grp)
                  (let ((res (filter-map %workspace-resolve grp)))
                    (and (pair? res) res)))
                raw)))

(define (%workspace-group-find ws)
  "Return the workspace group containing WS as a list of <workspace> records.
If *workspace-groups* is configured, searches for a group containing WS.
Otherwise, it will return workspaces having the same index as WS."
  (and (workspace? ws)
       (if (pair? *workspace-groups*)
           (find (lambda (grp) (memq ws grp)) *workspace-groups*)
		   ;; same index workspaces
           (and-let* ((out (workspace-output ws))
                      (idx (list-index (lambda (w) (eq? w ws))
									   (output-workspaces out))))
             (filter-map (lambda (o)
                           (let ((w (output-workspaces o)))
                             (and (< idx (length w)) (list-ref w idx))))
                         (manager-outputs *manager*))))))

(define (workspace-group-sync! focused-ws)
  "Synchronize all other workspaces in the group of FOCUSED-WS across outputs."
  (and-let* (((workspace? focused-ws))
             (group (%workspace-group-find focused-ws))
             ((pair? group))
             (output (workspace-output focused-ws))
             (switched (list output)))

    (define (%sync-target! ws)
      (and-let* (((workspace? ws))
                 (out (workspace-output ws))
                 ((not (memq out switched))))
		;; switched keeps track of synced outputs
		;; this group sync won't switch workspace on
		;; an output more than once
        (set! switched (cons out switched))
        (unless (workspace-visible? ws)
		  ;; focus workspace without focusing its container or output
          (workspace-focus! ws #:focus-child #f #:focus-parent #f))))

    (for-each %sync-target! group)))

(define (workspace-group-on-workspace-switch ws prev)
  "Hook handler called when a workspace is switched."
  (when (and *workspace-group-enabled*
			 (workspace? ws)
			 (not *workspace-group-syncing?*))
	;; safe locking mechanism to avoid infinite loops in case of
	;; user invalid configuration. all synced workspaces must be
	;; in different outputs, if it happens that they share the same
	;; output an infinite loop will occur
	(dynamic-wind (lambda () (set! *workspace-group-syncing?* #t))
				  (lambda () (workspace-group-sync! ws))
                  (lambda () (set! *workspace-group-syncing?* #f)))))

(define (workspace-group-set! groups)
  "Set *workspace-groups* to the given GROUPS."
  (set! *workspace-groups* (%normalize-groups groups)))

(define (workspace-group-add! groups)
  "Append one or more workspace GROUPS to *workspace-groups*."
  (workspace-group-set!
   (append *workspace-groups* (%normalize-groups groups))))

(define (workspace-group-remove! group)
  "Remove GROUP from *workspace-groups*."
  (let ((target (filter-map %workspace-resolve group)))
    (set! *workspace-groups* (delete target *workspace-groups*))))

(define (workspace-group-clear!)
  "Clear all configured workspace groups."
  (set! *workspace-groups* '()))

(define* (workspace-group-enable! #:optional groups)
  "Enable workspace group synchronization.
GROUPS may be passed as:
  - (workspace-group-enable! (list (list w1 w2) (list w3 w4)))
  - (workspace-group-enable! '((\"ws1\" \"ws2\") (\"ws3\" \"ws4\")))
If GROUPS are supplied, they configure *workspace-groups*.
otherwise, *workspace-groups* will be configured by index."
  (when (pair? groups)
    (workspace-group-set! groups))
  (unless *workspace-group-enabled*
    (gliver-hook-add! *workspace-switch-hook* 'workspace-group-on-workspace-switch)
    (set! *workspace-group-enabled* #t)))

(define (workspace-group-disable!)
  "Disable workspace group synchronization and unregister hooks."
  (when *workspace-group-enabled*
    (gliver-hook-remove! *workspace-switch-hook* 'workspace-group-on-workspace-switch)
    (set! *workspace-group-enabled* #f)
	(workspace-group-clear!)
	(log-info "workspace-group disabled.")))
