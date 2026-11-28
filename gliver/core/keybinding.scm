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
            *keybinding-dirty?*
            keybinding-mark-dirty!
            keybinding-on-keymap-cleared
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
            keybinding-on-manage-start
))

;;; state
(define-var *keybinding-current-mode* '*top-map*
            "Current active keymap mode symbol (e.g. '*top-map*, '*root-map*, or keymap name).")

(define-var *keybinding-mapping* '()
            "Alist of (<keybinding> . river-binding-proxy).")

(define-var *keybinding-dirty?* #f
            "Boolean indicating whether keybindings have been modified since the last sync.")

(define (keybinding-mark-dirty!)
  "Mark keybindings as dirty and request a manage sequence."
  (unless *keybinding-dirty?*
    (set! *keybinding-dirty?* #t)
    (wm-manager-manage-dirty (manager-wl-proxy *manager*))))

(define (keybinding-on-keymap-cleared keymap)
  "Invoked when KEYMAP is cleared, mark keybindings dirty.
Parameters: (KEYMAP)
  - KEYMAP: <keymap> record that was cleared."
  (keybinding-mark-dirty!))
(gliver-hook-add! %keymap-cleared-hook 'keybinding-on-keymap-cleared)

(define (%keybinding-find-by-proxy proxy-binding)
  "Look up the <keybinding> record associated with PROXY-BINDING.
Parameters: (PROXY-BINDING)
  - PROXY-BINDING: Pointer to river_xkb_binding_v1 proxy.
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
  - BINDING: <keybinding> record to format.
Returns formatted keybinding string representation."
  (key->string (keybinding-key binding)))

(define (keybinding-modifiers-list binding)
  "Return a list of modifier symbols for BINDING.
Parameters: (BINDING)
  - BINDING: <keybinding> record to inspect.
Returns list of modifier symbols."
  (key-modifiers (keybinding-key binding)))

;; define-key and undefine-key maintain user-friendly names common in emacs
(define* (define-key keymap key action #:key (persist #f))
  "Bind KEY to ACTION in KEYMAP.
Parameters: (KEYMAP KEY ACTION [#:persist PERSIST])
  - KEYMAP: <keymap> record to bind in.
  - KEY: <key> record or string key description (for kbd).
  - ACTION: Procedure, symbol, string, list, or <keymap> record representing the action.
  - PERSIST: Boolean flag indicating whether keymap stays active (default #f)."
  (let* ((k (%ensure-key key))
         (key-str (key->string k))
         (mode (keymap-mode keymap)))
    (hash-table-set! (keymap-bindings keymap) key-str
                     (make-keybinding k action persist mode keymap))
    (keybinding-mark-dirty!)))

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
  - KEYMAP: <keymap> record to unbind from.
  - KEY: <key> record or string key description (for kbd)."
  (let ((bindings (keymap-bindings keymap))
        (key-str (key->string (%ensure-key key))))
    (when (hash-table-exists? bindings key-str)
      (hash-table-delete! bindings key-str)
      (keybinding-mark-dirty!))))

(define (lookup-key keymap key)
  "Look up KEY in KEYMAP. Returns the <keybinding> or #f.
Parameters: (KEYMAP KEY)
  - KEYMAP: <keymap> record to search.
  - KEY: <key> record or string key description (for kbd).
Returns matching <keybinding> record or #f."
  (hash-table-ref/default (keymap-bindings keymap) (key->string (%ensure-key key)) #f))

;;; XKB binding generation
;;; Keybindings are translated into River XKB bindings directly using
;;; <keymap>, <keybinding>, and <key> records.

(define (keybindings-generate keymap mode-sym)
  "Generate active <keybinding> records for a single KEYMAP under MODE-SYM.
Parameters: (KEYMAP MODE-SYM)
  - KEYMAP: <keymap> record source.
  - MODE-SYM: Symbol mode identifier for generated bindings.
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
  - TOP-MAP: <keymap> record for top-level bindings.
  - ROOT-MAP: <keymap> record for prefix bindings.
  - ROOT-MODE-SYM: Symbol or <keymap> record for prefix mode (default '*root-map*).
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
  - PROXY-BINDING: Pointer to river_xkb_binding_v1 proxy."
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
  - PROXY-SEAT: Pointer to river_xkb_bindings_seat_v1 proxy."
  ;; abort prefix and go back to *top-map* mode
  (with-manage-sequence
   (keybinding-execute-action! 'prefix-abort #f)))
(gliver-hook-add! %keybinding-eaten-hook 'keybinding-on-eaten)

(define (keybinding-execute-action! action persist)
  "Execute keybinding ACTION within a manage sequence.
Parameters: (ACTION PERSIST)
  - ACTION: Procedure, symbol, string, list, or <keymap> record representing the action.
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
         ((keymap? action)    (keybinding-switch-to-mode! action))
         (else                (command-run-by-name action))))
      (lambda (key . args)
        (log-error "Keybinding action error: ~a ~a" key args)))

    ;; return to *top-map* if the action did not transition to a submap
    (unless (or is-keymap?
                (eq? *keybinding-current-mode* '*top-map*)
                (not (eq? *keybinding-current-mode* mode-before))
                (keymap-passthrough-mode? *keybinding-current-mode*))
      (if persist
          (xkb-bindings-manager-bindings-seat-ensure-next-key-eaten-all!)
          (keybinding-switch-to-mode! *top-map*)))))

(define* (keybinding-switch-to-mode! keymap #:key (force #f))
  "Switch to keybinding KEYMAP by enabling and disabling corresponding binding sets.
Parameters: (KEYMAP [#:force FORCE])
  - KEYMAP: <keymap> record or symbol mode name (e.g. '*top-map*, '*root-map*).
  - FORCE: Boolean flag; when #t, switches even if mode matches *keybinding-current-mode* (default #f)."
  (let* ((resolved-keymap (keymap-find keymap))
         (target-mode (cond
                       ((keymap? keymap) (keymap-name keymap))
                       (resolved-keymap (keymap-name resolved-keymap))
                       ((symbol? keymap) keymap)
                       (else #f)))
         (hook-keymap (or resolved-keymap keymap)))
    (when (and target-mode (or force (not (eq? target-mode *keybinding-current-mode*))))
      (log-debug "Switching to mode: ~a" target-mode)
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
          (xkb-bindings-manager-bindings-seat-cancel-ensure-next-key-eaten-all!)
          (xkb-bindings-manager-bindings-seat-ensure-next-key-eaten-all!))

      (gliver-hook-run! *keymap-change-hook* hook-keymap))))

(define (keybinding-sync-all!)
  "Synchronize all configured Gliver keybindings with the River compositor.
Destroys existing bindings, generates binding records from registered keymaps,
and creates `river_xkb_binding_v1` objects for all seats.
Parameters: None."
  (set! *keybinding-dirty?* #f)

  ;; destroy all existing keybindings first
  (xkb-bindings-manager-bindings-destroy-all!)

  ;; generate bindings
  (let ((bindings (keybinding-generate-all *top-map* *root-map*))
        (seats (manager-seats *manager*)))
    (when (pair? seats)
      (log-info "Syncing ~a keybindings to River across ~a seat(s)..."
                (length bindings) (length seats))

      (set! *keybinding-mapping*
            ;; map each <keybinding> to xkb binding object for each seat
            (append-map
             (lambda (seat)
               (let ((seat-proxy (seat-wl-proxy seat)))
                 (if (valid-pointer? seat-proxy)
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
                          bindings)
                     '())))
             seats))

      (log-info "Keybindings synced (~a bindings created)."
                (length *keybinding-mapping*)))))

(define (keybinding-sync!)
  "Request a keybinding sync during the next compositor manage sequence.
Parameters: None."
  ;; sync keybindings in manage sequence
  (with-manage-sequence #:dirty #t
   (log-info "Performing keybinding sync...")
   (keybinding-sync-all!))

  ;; create xkb bindings seat for ensure_next_key_eaten
  ;; this will return the existing seat if it's already created
  (for-each
   (lambda (seat)
     (let ((seat-proxy (seat-wl-proxy seat)))
       (when (valid-pointer? seat-proxy)
         (xkb-bindings-manager-bindings-create-seat seat-proxy))))
   (manager-seats *manager*)))

(define (keybinding-on-manage-start)
  "Sync keybindings at the start of a manage sequence when marked dirty.
Parameters: None."
  (when *keybinding-dirty?*
    (keybinding-sync!)))
(gliver-hook-add! *manager-manage-start-hook* 'keybinding-on-manage-start)
