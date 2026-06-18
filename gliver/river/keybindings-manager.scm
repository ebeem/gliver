;;; gliver/river/keybindings-manager.scm --- River compositor integration
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module integrates Gliver with the River Wayland compositor.
;;; It handles: Key bindings via river-xkb-bindings-v1 protocol

(define-module (gliver river keybindings-manager)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (system foreign)
  #:use-module (gliver river connector)
  #:use-module (gliver wayland client)
  #:use-module (gliver wayland gen river-window-management-v1)
  #:use-module (gliver wayland gen river-xkb-bindings-v1)
  #:use-module (gliver core)
  #:use-module (gliver commands)
  #:export (
			*xkb-bindings*
			*xkb-bindings-seat*
			*xkb-binding-listener*
			*xkb-bindings-seat-listener*
			*active-bindings*
			*current-mode*
			*pending-key-action*
			*needs-keybinding-sync*
			gliver-on-globals-bind
			gliver-on-globals-unbind
			gliver-on-globals-verify
			gliver-on-listeners-attach
			gliver-on-manage-start
			find-spec-for-proxy
			on-binding-pressed
			on-binding-released
			on-binding-stop-repeat
			on-bindings-seat-ate
			execute-binding-action
			switch-to-mode!
			sync-all-keybindings!
			request-keybinding-sync!
))

;; river_xkb_bindings_v1 state
(define *xkb-bindings* %null-pointer)      ;; river_xkb_bindings_v1 proxy
(define *xkb-bindings-seat* %null-pointer) ;; river_xkb_bindings_seat_v1 proxy
(define *xkb-binding-listener* #f)         ;; shared listener for all xkb bindings
(define *xkb-bindings-seat-listener* #f)   ;; shared listener for xkb bindings seat
(define *active-bindings* '())             ;; alist: (gliver-binding-spec . xkb-binding-proxy)
(define *current-mode* 'normal)            ;; current keymap mode
(define *pending-key-action* #f)
(define *needs-keybinding-sync* #f)

(define (%km-seat)
  "Return the first seat proxy available or #f."
  (let ((seats (manager-seats *manager*)))
    (if (pair? seats)
        (seat-wl-proxy (car seats))
        #f)))

(define (gliver-on-globals-bind registry protocol-name object-id version)
  "Bind and register the xkb bindings manager"
  (when (string=? protocol-name RIVER_XKB_BINDINGS_V1_NAME)
	(log-info "Binding ~a..." protocol-name)
    (set! *xkb-bindings*
          (gliver-wl-registry-bind registry object-id
                                   *river-xkb-bindings-v1-interface*
                                   (min version 2)))))

(define (gliver-on-globals-unbind)
  "Unbind/destroy the bindings manager"
  ;; destroy active keybindings
  (for-each (lambda (pair)
              (river-xkb-binding-v1-destroy (cdr pair)))
            *active-bindings*)
  (set! *active-bindings* '())

  ;; destroy xkb bindings seat
  (unless (null-pointer? *xkb-bindings-seat*)
    (river-xkb-bindings-seat-v1-destroy *xkb-bindings-seat*)
    (set! *xkb-bindings-seat* %null-pointer))

  ;; destroy xkb bindings global
  (unless (null-pointer? *xkb-bindings*)
    (river-xkb-bindings-v1-destroy *xkb-bindings*)
    (set! *xkb-bindings* %null-pointer)))

(define (gliver-on-globals-verify)
  "Verify critical globals were bound "
  (when (null-pointer? *xkb-bindings*)
    (log-error "river_xkb_bindings_v1 not available")
    (error "river_xkb_bindings_v1 not available")))

(define (gliver-on-listeners-attach)
  "Attach wayland listeners"
  (set! *xkb-binding-listener*
        (make-river-xkb-binding-v1-listener
         on-binding-pressed
         on-binding-released
         on-binding-stop-repeat))
  (set! *xkb-bindings-seat-listener*
        (make-river-xkb-bindings-seat-v1-listener
         on-bindings-seat-ate)))

(define (gliver-on-manage-start)
  "Handle manage sequence synchronization for keybindings"
  (when *needs-keybinding-sync*
    (log-info "Performing keybinding sync...")
    (sync-all-keybindings!)
    ;; create xkb bindings seat for ensure_next_key_eaten
	(let ((seat (%km-seat)))
      (when (and seat (not (null-pointer? *xkb-bindings*)))
		(set! *xkb-bindings-seat*
              (river-xkb-bindings-v1-get-seat *xkb-bindings* seat))
		(unless (null-pointer? *xkb-bindings-seat*)
          (wl-proxy-add-listener
           *xkb-bindings-seat*
           *xkb-bindings-seat-listener*
           %null-pointer))))
    (set! *needs-keybinding-sync* #f))

  (when *pending-key-action*
    (let* ((pending *pending-key-action*)
           (action (if (pair? pending) (car pending) pending))
           (persist (if (pair? pending) (cdr pending) #f)))
      (set! *pending-key-action* #f)
      (execute-binding-action action persist))))

(define (find-spec-for-proxy binding-proxy)
  "Look up the gliver-binding-spec for a given XKB binding proxy."
  (let loop ((pairs *active-bindings*))
    (cond
     ((null? pairs) #f)
     ((equal? (pointer-address (cdar pairs))
              (pointer-address binding-proxy))
      (caar pairs))
     (else (loop (cdr pairs))))))

(define (on-binding-pressed data binding-proxy)
  "Handle a key binding pressed event.
Records the action for execution in the upcoming manage_start handler."
  (let ((spec (find-spec-for-proxy binding-proxy)))
    (when spec
      (log-debug "Key pressed: key=~a action=~a mode=~a persist=~a"
                 (gliver-binding-spec->string spec)
                 (gliver-binding-spec-action spec)
				 (gliver-binding-spec-mode spec)
                 (gliver-binding-spec-persist spec))
      (set! *pending-key-action*
            (cons (gliver-binding-spec-action spec)
                  (gliver-binding-spec-persist spec))))))

(define (on-binding-released data binding-proxy)
  (log-debug "Key released"))

(define (on-binding-stop-repeat data binding-proxy)
  (log-debug "Key stop-repeat"))

(define (on-bindings-seat-ate data seat-proxy)
  "Handle an unbound key press in prefix/submap mode."
  (log-debug "Ate unbound key, abort prefix mode")
  ;; queue a prefix-abort action for the next manage sequence
  (set! *pending-key-action* 'prefix-abort))

(define (execute-binding-action action persist)
  "Execute a keybinding action within a manage sequence.
When PERSIST is #f, the mode returns to normal after execution."
  (log-debug "Executing binding action: ~a (persist=~a)" action persist)
  (cond
   ;; special actions
   ((eq? action 'prefix-activated)
    (switch-to-mode! 'prefix))
   ((eq? action 'prefix-abort)
    (switch-to-mode! 'normal))

   ;; enter a submap
   ((and (list? action)
         (eq? (car action) 'enter-submap))
    (let ((submap-name (cadr action)))
      (log-info "Entering submap: ~a" submap-name)
      (switch-to-mode! (string->symbol submap-name))))

   ;; procedure (thunk)
   ((procedure? action)
    (catch #t
      (lambda () (action))
      (lambda (key . args)
        (log-error "Keybinding action error: ~a ~a" key args)))
    (unless persist (switch-to-mode! 'normal)))

   ;; string or symbol command name
   ((or (string? action) (symbol? action))
    (command-run-by-name action)
    (unless persist (switch-to-mode! 'normal)))

   (else
    (log-warn "Unknown binding action type: ~a" action))))

;;; mode / keybinding management
(define* (switch-to-mode! mode-name #:key (force #f))
  "Switch to a keybinding mode by enabling/disabling binding sets.
MODE-NAME is a symbol: 'normal or 'prefix or a submap name.
Must be called during a manage sequence."
  (unless (and (not force) (eq? mode-name *current-mode*))
	(log-debug "Switching to mode: ~a" mode-name)
	(gliver-hook-run! *keymap-change-hook* mode-name)
	(set! *current-mode* mode-name)
	;; enable bindings matching the target mode, disable others
	(for-each
	 (lambda (pair)
       (let ((spec (car pair))
			 (proxy (cdr pair)))
		 (let ((spec-mode (gliver-binding-spec-mode spec)))
           (if (or (eq? spec-mode mode-name)
                   ;; submap bindings are part of the prefix map, so when
                   ;; in a submap, we want prefix bindings to be active.
                   (and (not (eq? mode-name 'normal))
						(eq? spec-mode 'prefix)))
               (river-xkb-binding-v1-enable proxy)
               (river-xkb-binding-v1-disable proxy)))))
	 *active-bindings*)
	;; if entering a prefix or submap mode, eat unbound keys
	(when (and (not (eq? mode-name 'normal))
               (not (null-pointer? *xkb-bindings-seat*)))
      (river-xkb-bindings-seat-v1-ensure-next-key-eaten *xkb-bindings-seat*))))

(define (sync-all-keybindings!)
  "Synchronize all Gliver keybindings with River.
Creates river_xkb_binding_v1 objects for all bindings and enables
those matching the current mode."

  ;; destroy existing bindings
  (for-each (lambda (pair)
              (river-xkb-binding-v1-destroy (cdr pair)))
            *active-bindings*)
  (set! *active-bindings* '())

  ;; generate binding specs
  (let* ((specs (gliver-binding-spec-generate *top-map* *root-map*
                                            (manager-config-ref 'prefix-key)
                                            'prefix))
         (seat (%km-seat)))

    (if (not seat)
        (log-warn "Skipping keybindings sync: no seat available.")
        (begin
          (log-info "Syncing ~a keybindings to River..." (length specs))

          ;; create xkb binding objects for each spec
          (set! *active-bindings*
            (map (lambda (spec)
                   (let ((proxy (river-xkb-bindings-v1-get-xkb-binding
                                 *xkb-bindings*
                                 seat
                                 (gliver-binding-spec-keysym spec)
                                 (gliver-binding-spec-modifiers spec))))
                     ;; attach the shared event listener for pressed/released
                     (unless (null-pointer? proxy)
                       (wl-proxy-add-listener proxy *xkb-binding-listener*
                                              %null-pointer))
                     ;; enable bindings matching the current mode
                     (when (eq? (gliver-binding-spec-mode spec) *current-mode*)
                       (river-xkb-binding-v1-enable proxy))
                     (cons spec proxy)))
                 specs))

          (log-info "Keybindings synced (~a bindings created)."
                    (length *active-bindings*))))))

(define (request-keybinding-sync!)
  "Request a keybinding sync in the next manage sequence.
Use this instead of calling sync-all-keybindings! directly when
outside a manage sequence."
  (let ((manager-proxy (manager-wl-proxy *manager*)))
	(set! *needs-keybinding-sync* #t)
	(unless (null-pointer? manager-proxy)
      (river-window-manager-v1-manage-dirty manager-proxy))))

;; handle river initialization steps
(gliver-hook-add! *gliver-globals-bind-hook* 'gliver-on-globals-bind)
(gliver-hook-add! *gliver-globals-unbind-hook* 'gliver-on-globals-unbind)
(gliver-hook-add! *gliver-globals-verify-hook* 'gliver-on-globals-verify)
(gliver-hook-add! *gliver-listeners-attach-hook* 'gliver-on-listeners-attach)
(gliver-hook-add! *manager-manage-start-hook* 'gliver-on-manage-start)
(gliver-hook-add! *keybinding-sync-request-hook* 'request-keybinding-sync!)
