;;; gliver/core/keybinding.scm --- Keybinding actions and execution for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module manages keybinding definitions and execution.
;;; It handles:
;;;  - Defining keybinding records (<keybinding>)
;;;  - Binding keys in keymaps (define-key, undefine-key, lookup-key)
;;;  - Generating Wayland/river XKB bindings directly from <keymap>, <keybinding>, and <key>

(define-module (gliver core keybinding)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-69)
  #:use-module (system foreign)
  #:use-module (gliver core-base)
  #:use-module (gliver core key)
  #:use-module (gliver core keymap)
  #:use-module (gliver river xkb-bindings-manager)
  #:use-module (gliver river window-manager)
  #:declarative? #f
  #:export (
			*keybinding-current-mode*
			*keybinding-mapping*
			keybinding->string
			keybinding-modifiers-list
			define-key
			define-keys
			undefine-key
			lookup-key
			keybindings-generate
			keybinding-generate-all
			keybinding-on-pressed
			keybinding-on-eaten
			keybinding-execute-action!
			keybinding-switch-to-mode!
			keybinding-sync-all!
			keybinding-sync!
))

;;; state
(define-var *keybinding-current-mode* '*top-map*
  "Current active keymap mode symbol (e.g. '*top-map*, '*root-map*, or keymap name).")

(define-var *keybinding-mapping* '()
  "Alist of (<keybinding> . river-binding-proxy).")

(define (%keybinding-find-by-proxy proxy-binding)
  "Look up the <keybinding> record associated with PROXY-BINDING.
Parameters: (PROXY-BINDING)
  - PROXY-BINDING: river_xkb_binding_v1 proxy pointer.
Returns matching <keybinding> record or #f."
  (let ((addr (pointer-address proxy-binding)))
    (and=> (find (lambda (p) (= (pointer-address (cdr p)) addr))
                 *keybinding-mapping*)
           car)))

(define (%ensure-key key)
  "Ensure KEY is a <key> record."
  (if (key? key) key (kbd key)))

(define (keybinding->string binding)
  "Convert a <keybinding> to its display string, e.g., \"C-t\".
Parameters: (BINDING)
  - BINDING: Target <keybinding> record.
Returns formatted keybinding string representation."
  (key->string (keybinding-key binding)))

(define (keybinding-modifiers-list binding)
  "Return a list of modifier symbols for BINDING.
Parameters: (BINDING)
  - BINDING: Target <keybinding> record.
Returns list of modifier symbols."
  (key-modifiers (keybinding-key binding)))

;; define-key and undefine-key maintain user-friendly names common in emacs
(define* (define-key keymap key action #:key (persist #f))
  "Bind KEY to ACTION in KEYMAP.
Parameters: (KEYMAP KEY ACTION [#:persist PERSIST])
  - KEYMAP: Target <keymap> record.
  - KEY: <key> record or key description string (kbd).
  - ACTION: Procedure, command symbol/string, command list, or <keymap>.
  - PERSIST: Boolean flag indicating whether keymap stays active (default #f)."
  (let* ((k (%ensure-key key))
         (key-str (key->string k))
         (mode (keymap-mode keymap)))
    (hash-table-set! (keymap-bindings keymap) key-str
                     (make-keybinding k action persist mode keymap))))

(define-syntax define-keys
  (syntax-rules ()
    ((_ keymap)
     (begin))
    ((_ keymap key action rest ...)
     (begin
       (define-key keymap key action)
       (define-keys keymap rest ...)))))

(define (undefine-key keymap key)
  "Remove the binding for KEY from KEYMAP.
Parameters: (KEYMAP KEY)
  - KEYMAP: Target <keymap> record.
  - KEY: <key> record or key description string (kbd)."
  (hash-table-delete! (keymap-bindings keymap) (key->string (%ensure-key key))))

(define (lookup-key keymap key)
  "Look up KEY in KEYMAP. Returns the <keybinding> or #f.
Parameters: (KEYMAP KEY)
  - KEYMAP: Target <keymap> record.
  - KEY: <key> record or key description string (kbd).
Returns matching <keybinding> record or #f."
  (hash-table-ref/default (keymap-bindings keymap) (key->string (%ensure-key key)) #f))

;;; XKB binding generation
;;; Keybindings are translated into River XKB bindings directly using
;;; <keymap>, <keybinding>, and <key> records.

(define (keybindings-generate keymap mode-sym)
  "Generate active <keybinding> records for a single KEYMAP under MODE-SYM.
Parameters: (KEYMAP MODE-SYM)
  - KEYMAP: Source <keymap> record.
  - MODE-SYM: Mode symbol for generated bindings.
Returns list of <keybinding> records."
  (map (lambda (pair)
         (let* ((binding (cdr pair))
                (action (keybinding-action binding))
                (submap? (keymap? action)))
           (make-keybinding (keybinding-key binding)
                            (if submap? (list 'enter-keymap (keymap-mode action)) action)
                            (if submap? #t (keybinding-persist binding))
                            mode-sym
                            keymap)))
       (keymap->alist keymap)))

(define %default-abort-keys
  (map kbd '("Escape" "C-g")))

(define (%default-abort-bindings keymap mode-sym)
  "Return synthetic abort bindings (Escape and C-g) for non-passthrough KEYMAP."
  (if (keymap-passthrough? keymap)
      '()
      (filter-map
       (lambda (k)
         (and (not (lookup-key keymap k))
              (make-keybinding k 'prefix-abort #f mode-sym keymap)))
       %default-abort-keys)))

(define* (keybinding-generate-all top-map root-map #:optional (root-mode-sym '*root-map*))
  "Generate a list of <keybinding> records for all keymaps.
Parameters: (TOP-MAP ROOT-MAP [ROOT-MODE-SYM])
  - TOP-MAP: Top-level <keymap> record.
  - ROOT-MAP: Prefix <keymap> record.
  - ROOT-MODE-SYM: Symbol identifier for prefix mode (default '*root-map*).
Returns a list of <keybinding> records for top-map, root-map,
and all registered keymaps."
  (define (%generate-submap keymap mode)
    (append (keybindings-generate keymap mode)
            (%default-abort-bindings keymap mode)))

  (append (keybindings-generate top-map '*top-map*)
          (%generate-submap root-map (keymap-mode root-mode-sym))
          (append-map (lambda (km)
                        (%generate-submap km (keymap-mode km)))
                      (remove (lambda (km)
                                (or (eq? km top-map) (eq? km root-map)))
                              (manager-keymaps *manager*)))))

(define (keybinding-on-pressed proxy-binding)
  "Handle key press event for PROXY-BINDING.
Parameters: (PROXY-BINDING)
  - PROXY-BINDING: river_xkb_binding_v1 proxy pointer."
  ;; find the mapping <keybinding> of the proxy
  (and-let* ((binding (%keybinding-find-by-proxy proxy-binding)))
    (log-debug "Key pressed: key=~a action=~a mode=~a persist=~a"
               (keybinding->string binding)
               (keybinding-action binding)
               (keybinding-mode binding)
               (keybinding-persist binding))

    ;; execute action in next manage_sequence as per river docs
    (with-manage-sequence
     (keybinding-execute-action!
      (keybinding-action binding)
      (keybinding-persist binding)))))
(gliver-hook-add! %keybinding-pressed-hook 'keybinding-on-pressed)

(define (keybinding-on-eaten proxy-seat)
  "Handle an eaten key press event on PROXY-SEAT.
Occurs when an unbound key is pressed after an ensure-next-key-eaten call.
This will only be called in Gliver if a prefix or keymap mode is active.
Parameters: (PROXY-SEAT)
  - PROXY-SEAT: river_xkb_bindings_seat_v1 proxy pointer."
  ;; abort prefix and go back to *top-map* mode
  (with-manage-sequence
   (keybinding-execute-action! 'prefix-abort #f)))
(gliver-hook-add! %keybinding-eaten-hook 'keybinding-on-eaten)

(define (keybinding-execute-action! action persist)
  "Execute keybinding ACTION within a manage sequence.
Parameters: (ACTION PERSIST)
  - ACTION: Procedure, command symbol/string, command list, or <keymap>.
  - PERSIST: Boolean flag indicating if mode should persist."
  (log-debug "Executing binding action: ~a (persist=~a)" action persist)
  (let ((mode-before *keybinding-current-mode*)
        (is-keymap? (or (and (pair? action) (eq? (car action) 'enter-keymap))
                        (keymap? action))))
    (catch #t
      (lambda ()
        (cond
         ((procedure? action) (action))
         ((pair? action)      (apply command-run-by-name (car action) (cdr action)))
         ((keymap? action)    (keybinding-switch-to-mode! (keymap-mode action)))
         (else                (command-run-by-name action))))
      (lambda (key . args)
        (log-error "Keybinding action error: ~a ~a" key args)))

    ;; return to *top-map* if the action did not transition to a submap
    (unless (or is-keymap?
                (eq? *keybinding-current-mode* '*top-map*)
                (not (eq? *keybinding-current-mode* mode-before))
                (keymap-passthrough-mode? *keybinding-current-mode*))
      (if persist
          (xkb-bindings-manager-bindings-seat-ensure-next-key-eaten *xkb-bindings-manager-bindings-seat*)
          (keybinding-switch-to-mode! '*top-map*)))))

(define* (keybinding-switch-to-mode! mode-name #:key (force #f))
  "Switch to keybinding mode MODE-NAME by enabling and disabling corresponding binding sets.
Parameters: (MODE-NAME [#:force FORCE])
  - MODE-NAME: Symbol ('*top-map*, '*root-map*, keymap name) or <keymap>.
  - FORCE: When #t, switches even if mode matches *keybinding-current-mode* (default #f)."
  (let ((target-mode (keymap-mode mode-name)))
    (when (or force (not (eq? target-mode *keybinding-current-mode*)))
      (log-debug "Switching to mode: ~a" target-mode)
      (gliver-hook-run! *keymap-change-hook* target-mode)
      (set! *keybinding-current-mode* target-mode)

      ;; enable bindings matching the target mode, disable others
      (for-each
       (lambda (pair)
         (if (eq? (keybinding-mode (car pair)) target-mode)
             (xkb-bindings-manager-binding-enable (cdr pair))
             (xkb-bindings-manager-binding-disable (cdr pair))))
       *keybinding-mapping*)

      ;; if entering a non-passthrough keymap mode, eat unbound keys so we handle chord aborts;
      ;; if entering passthrough mode or *top-map* mode, cancel any active ensure_next_key_eaten request
      (if (or (eq? target-mode '*top-map*)
              (keymap-passthrough-mode? target-mode))
          (xkb-bindings-manager-bindings-seat-cancel-ensure-next-key-eaten *xkb-bindings-manager-bindings-seat*)
          (xkb-bindings-manager-bindings-seat-ensure-next-key-eaten *xkb-bindings-manager-bindings-seat*)))))

(define (keybinding-sync-all!)
  "Synchronize all configured Gliver keybindings with the River compositor.
Destroys existing bindings, generates binding records from registered keymaps,
and creates `river_xkb_binding_v1` objects for the active seat.
Parameters: None."
  ;; destroy all existing keybindings first
  (xkb-bindings-manager-bindings-destroy-all!)

  ;; generate bindings
  (and-let* ((bindings (keybinding-generate-all *top-map* *root-map*))
             (seat (seat-current))
             (seat-proxy (seat-wl-proxy seat)))

    (log-info "Syncing ~a keybindings to River..." (length bindings))

    (set! *keybinding-mapping*
          ;; map each <keybinding> to xkb binding object
          (map (lambda (binding)
                 (let* ((xkb (key->xkb-binding-args (keybinding-key binding)))
                        (proxy (xkb-bindings-manager-bindings-create-binding
                                seat-proxy
                                (cdr xkb)
                                (car xkb))))

                   ;; enable bindings matching the current mode
                   (when (eq? (keybinding-mode binding) *keybinding-current-mode*)
                     (xkb-bindings-manager-binding-enable proxy))
                   (cons binding proxy)))
               bindings))

    (log-info "Keybindings synced (~a bindings created)."
              (length *keybinding-mapping*))))

(define (keybinding-sync!)
  "Request a keybinding sync during the next compositor manage sequence.
Parameters: None."
  ;; sync keybindings in manage sequence
  (with-manage-sequence
   (log-info "Performing keybinding sync...")
   (keybinding-sync-all!))

  ;; it's safe to call manage dirty because it will be ignored
  ;; if we are currently in a one
  (wm-manager-manage-dirty (manager-wl-proxy *manager*))

  ;; create xkb bindings seat for ensure_next_key_eaten
  ;; this will return the existing seat if it's already created
  (and-let* ((seat (seat-current))
             (seat-proxy (seat-wl-proxy seat)))
    (xkb-bindings-manager-bindings-create-seat seat-proxy)))
