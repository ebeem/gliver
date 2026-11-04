;;; gliver/core/keybinding.scm --- Keybinding actions and execution for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module manages keybinding definitions, specs, and execution.
;;; It handles:
;;;  - Defining keybinding records (<keybinding>)
;;;  - Binding keys in keymaps (define-key, undefine-key, lookup-key)
;;;  - Generating Wayland/river XKB binding specs (<keybinding-spec>)

(define-module (gliver core keybinding)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-69)
  #:use-module (system foreign)
  #:use-module (gliver core-base)
  #:use-module (gliver core key)
  #:use-module (gliver core keymap)
  #:use-module (gliver deps libxkbcommon)
  #:use-module (gliver river xkb-bindings-manager)
  #:use-module (gliver river window-manager)
  #:declarative? #f
  #:export (
			<keybinding>
			*keybinding-current-mode*
			*keybinding-mapping*
			keybinding-key
			keybinding-persist
			keybinding-action
			keybinding?
			make-keybinding
			define-key
			define-keys
			undefine-key
			lookup-key
			keybinding-spec-persist
			keybinding-spec-mode
			keybinding-spec-action
			keybinding-spec-modifiers
			keybinding-spec-keysym
			keybinding-spec?
			make-keybinding-spec
			keybinding-spec->modifiers-list
			keybinding-spec->string
			keybinding-specs-generate
			keybinding-spec-generate
			keybinding-on-pressed
			keybinding-on-eaten
			keybinding-execute-action!
			keybinding-switch-to-mode!
			keybinding-sync-all!
			keybinding-sync!
))

;;; state
(define-var *keybinding-current-mode* 'normal
  "Current active keymap mode symbol (e.g. 'normal, '*root-map*, or keymap name).")

(define-var *keybinding-mapping* '()
  "Alist of <keybinding-spec> and the xkb-binding-proxy it corresponds to.")

(define (%keybinding-find-spec-by-proxy proxy-binding)
  "Look up the <keybinding-spec> record associated with PROXY-BINDING.
Parameters: (PROXY-BINDING)
  - PROXY-BINDING: river_xkb_binding_v1 proxy pointer.
Returns matching <keybinding-spec> record or #f."
  (let loop ((pairs *keybinding-mapping*))
    (cond
     ((null? pairs) #f)
     ((equal? (pointer-address (cdar pairs))
              (pointer-address proxy-binding))
      (caar pairs))
     (else (loop (cdr pairs))))))

;;; key binding: wraps an action with a persist flag and the parsed <key> record
;;  - key: <key> record representing modifiers and keysym
;;  - action: procedure, command name (symbol/string), command list, or <keymap>
;;  - persist: boolean flag indicating whether keymap stays active after key press
(define-record-type <keybinding>
  (make-keybinding key action persist)
  keybinding?
  (key     keybinding-key)
  (action  keybinding-action)
  (persist keybinding-persist))

;; define-key and undefine-key maintain user-friendly names common in emacs
(define* (define-key keymap key action #:key (persist #f))
  "Bind KEY to ACTION in KEYMAP.
Parameters: (KEYMAP KEY ACTION [#:persist PERSIST])
  - KEYMAP: Target <keymap> record.
  - KEY: <key> record or key description string (kbd).
  - ACTION: Procedure, command symbol/string, command list, or <keymap>.
  - PERSIST: Boolean flag indicating whether keymap stays active (default #f)."
  (let* ((k (if (key? key) key (kbd key)))
         (key-str (key->string k)))
    (hash-table-set! (keymap-bindings keymap) key-str
                     (make-keybinding k action persist))))

(define-syntax define-keys
  (syntax-rules ()
    ;; case 0: no bindings provided
    ((_ keymap)
     (begin))

    ;; case 1: 1 binding pair provided
    ((_ keymap key action)
     (define-key keymap key action))

    ;; case 2: more than one binding pair
    ((_ keymap key action rest ...)
     (begin
       (define-key keymap key action)
       (define-keys keymap rest ...)))))

(define (undefine-key keymap key)
  "Remove the binding for KEY from KEYMAP.
Parameters: (KEYMAP KEY)
  - KEYMAP: Target <keymap> record.
  - KEY: <key> record or key description string (kbd).
Returns unspecified."
  (let ((key-str (key->string (if (key? key) key (kbd key)))))
    (hash-table-delete! (keymap-bindings keymap) key-str)))

(define (lookup-key keymap key)
  "Look up KEY in KEYMAP. Returns the <keybinding> or #f.
Parameters: (KEYMAP KEY)
  - KEYMAP: Target <keymap> record.
  - KEY: <key> record or key description string (kbd).
Returns matching <keybinding> record or #f."
  (let ((key-str (key->string (if (key? key) key (kbd key)))))
    (hash-table-ref/default (keymap-bindings keymap) key-str #f)))

;;; XKB binding spec generation
;;; a keybinding-spec describes a key binding to be created via the
;;; river-xkb-bindings-v1 Wayland protocol.
;;; TODO: now I think keysym and modifiers should be replaced by <key>
;;; they are only used to generate spec, which can be done via <key> if needed
(define-record-type <keybinding-spec>
  (make-keybinding-spec keysym modifiers action mode persist)
  keybinding-spec?
  (keysym    keybinding-spec-keysym)      ;; uint: xkb keysym value
  (modifiers keybinding-spec-modifiers)   ;; uint: modifier bitmask
  (action    keybinding-spec-action)      ;; procedure, symbol, or string
  (mode      keybinding-spec-mode)        ;; symbol: 'normal, 'prefix, or keymap name
  (persist   keybinding-spec-persist))    ;; bool: stay in current keymap after press

(define (keybinding-spec->modifiers-list spec)
  "Converts a SPEC's modifier bitmask into a list of modifier symbols.
Parameters: (SPEC)
  - SPEC: Target <keybinding-spec> record.
Returns list of modifier symbols."
  (filter-map (lambda (pair)
                (let ((mod-name (car pair))
                      (mod-bit  (cdr pair)))
                  (and (not (zero?
							 (logand (keybinding-spec-modifiers spec) mod-bit)))
                       mod-name)))
              *key-modifier-bitmask-map*))

(define (keybinding-spec->string spec)
  "Convert a SPEC to its display string, e.g., \"C-t\".
Parameters: (SPEC)
  - SPEC: Target <keybinding-spec> record.
Returns formatted keybinding string representation."
  (let* ((mods (map (lambda (m)
					  (let ((pair (find (lambda (p) (eq? (cdr p) m))
                                        *key-modifier-map*)))
                        (if pair
                            (car pair)
                            (symbol->string m))))
                    (keybinding-spec->modifiers-list spec)))
         (keysym (xkb-value->keysym-name (keybinding-spec-keysym spec))))
    (string-join (append mods (list keysym)) "-")))

(define (keybinding-specs-generate keymap mode-sym)
  "Generate binding specs for a single keymap under MODE-SYM.
Parameters: (KEYMAP MODE-SYM)
  - KEYMAP: Source <keymap> record.
  - MODE-SYM: Mode symbol for generated specs.
Returns list of <keybinding-spec> records."
  (let ((specs '()))
    (for-each
     (lambda (pair)
       (let* ((binding (cdr pair))
              (key (keybinding-key binding))
              (action (keybinding-action binding))
              (persist (keybinding-persist binding))
              (xkb (key->xkb-binding-args key)))
         (cond
          ((keymap? action)
           ;; keymap action: enter that keymap mode by symbol
           (let ((keymap-mode (keymap-name action)))
             (set! specs
               (cons (make-keybinding-spec
                      (cdr xkb)
                      (car xkb)
                      (list 'enter-keymap keymap-mode)
                      mode-sym
                      #t)
                     specs))))
          ((and (list? action) (eq? (car action) 'enter-keymap))
           (let* ((target (cadr action))
                  (keymap-mode (if (keymap? target)
                                   (keymap-name target)
                                   (if (symbol? target)
                                       target
                                       (string->symbol (format #f "~a" target))))))
             (set! specs
               (cons (make-keybinding-spec
                      (cdr xkb)
                      (car xkb)
                      (list 'enter-keymap keymap-mode)
                      mode-sym
                      #t)
                     specs))))
          (else
           (set! specs
             (cons (make-keybinding-spec
                    (cdr xkb)
                    (car xkb)
                    action
                    mode-sym
                    persist)
                   specs))))))
     (keymap->alist keymap))
    specs))

(define* (keybinding-spec-generate top-map root-map #:optional (root-mode-sym '*root-map*))
  "Generate a list of <keybinding-spec> records for XKB key bindings.
Parameters: (TOP-MAP ROOT-MAP [ROOT-MODE-SYM])
  - TOP-MAP: Top-level <keymap> record.
  - ROOT-MAP: Prefix <keymap> record.
  - ROOT-MODE-SYM: Symbol identifier for prefix mode (default '*root-map*).
Returns a list of keybinding-spec records for top-map, root-map,
and all registered keymaps."
  (let* ((root-sym (if (symbol? root-mode-sym) root-mode-sym (string->symbol root-mode-sym)))
         (keymaps (filter (lambda (keymap)
                            (and (not (eq? keymap top-map))
                                 (not (eq? keymap root-map))))
                          *keymap-registered*))
         (specs '()))

    ;; top-map bindings (normal mode)
    (set! specs (append (keybinding-specs-generate top-map 'normal) specs))

    ;; root-map bindings
    (set! specs (append (keybinding-specs-generate root-map root-sym) specs))
    ;; escape and C-g to exit prefix mode
    ;; NOTE: should this be configurable?
    (let ((esc-xkb (key->xkb-binding-args (kbd "Escape")))
          (cg-xkb  (key->xkb-binding-args (kbd "C-g"))))
      (unless (lookup-key root-map (kbd "Escape"))
        (set! specs
          (cons (make-keybinding-spec
                 (cdr esc-xkb) (car esc-xkb)
                 'prefix-abort root-sym #f)
                specs)))
      (unless (lookup-key root-map (kbd "C-g"))
        (set! specs
          (cons (make-keybinding-spec
                 (cdr cg-xkb) (car cg-xkb)
                 'prefix-abort root-sym #f)
                specs))))

    ;; keymap bindings
    (for-each
     (lambda (keymap)
       (let* ((keymap-mode (keymap-name keymap))
              (keymap-specs (keybinding-specs-generate keymap keymap-mode))
              (esc-xkb (key->xkb-binding-args (kbd "Escape")))
              (cg-xkb  (key->xkb-binding-args (kbd "C-g"))))
         (set! specs (append keymap-specs specs))
         (unless (or (keymap-passthrough? keymap)
                     (lookup-key keymap (kbd "Escape")))
           (set! specs
             (cons (make-keybinding-spec
                    (cdr esc-xkb) (car esc-xkb)
                    'prefix-abort keymap-mode #f)
                   specs)))
         (unless (or (keymap-passthrough? keymap)
                     (lookup-key keymap (kbd "C-g")))
           (set! specs
             (cons (make-keybinding-spec
                    (cdr cg-xkb) (car cg-xkb)
                    'prefix-abort keymap-mode #f)
                   specs)))))
     keymaps)

    (reverse specs)))

(define (keybinding-on-pressed proxy-binding)
  "Handle key press event for PROXY-BINDING.
Parameters: (PROXY-BINDING)
  - PROXY-BINDING: river_xkb_binding_v1 proxy pointer.
Returns unspecified."
  ;; find the mapping <keybinding-spec> of the proxy
  (let ((spec (%keybinding-find-spec-by-proxy proxy-binding)))
    (when spec
      (log-debug "Key pressed: key=~a action=~a mode=~a persist=~a"
                 (keybinding-spec->string spec)
                 (keybinding-spec-action spec)
				 (keybinding-spec-mode spec)
                 (keybinding-spec-persist spec))

	  ;; execute action in next manage_sequence as per river docs
	  (with-manage-sequence
	   (keybinding-execute-action!
		(keybinding-spec-action spec)
		(keybinding-spec-persist spec))))))
(gliver-hook-add! %keybinding-pressed-hook 'keybinding-on-pressed)

(define (keybinding-on-eaten proxy-seat)
  "Handle an eaten key press event on PROXY-SEAT.
Occurs when an unbound key is pressed after an ensure-next-key-eaten call.
This will only be called in Gliver if a prefix or keymap mode is active.
Parameters: (PROXY-SEAT)
  - PROXY-SEAT: river_xkb_bindings_seat_v1 proxy pointer.
Returns unspecified."
  ;; abort prefix and go back to normal mode
  (with-manage-sequence
   (keybinding-execute-action! 'prefix-abort #f)))
(gliver-hook-add! %keybinding-eaten-hook 'keybinding-on-eaten)

(define (keybinding-execute-action! action persist)
  "Execute keybinding ACTION within a manage sequence.
Parameters: (ACTION PERSIST)
  - ACTION: Procedure, command symbol/string, command list, or <keymap>.
  - PERSIST: Boolean flag indicating if mode should persist.
Returns unspecified."
  (log-debug "Executing binding action: ~a (persist=~a)" action persist)
  (let ((mode-before *keybinding-current-mode*)
        (is-keymap-action? (or (and (pair? action)
                                    (eq? (car action) 'enter-keymap))
                               (keymap? action))))
    (cond
     ;; direct keymap action
     ((keymap? action)
      (keybinding-switch-to-mode! (keymap-name action)))

     ;; procedure, command name (symbol/string), or command with arguments (list/pair)
     ((or (procedure? action)
          (string? action)
          (symbol? action)
          (pair? action))
      (catch #t
        (lambda ()
          (cond
           ((procedure? action)
            (action))
           ((pair? action)
            (apply command-run-by-name (car action) (cdr action)))
           (else
            (command-run-by-name action))))
        (lambda (key . args)
          (log-error "Keybinding action error: ~a ~a" key args))))

     ;; unknown type of action
     (else
      (log-warn "Unknown binding action type: ~a" action)))

    ;; only switch to normal keymap if the action did not transition to a keymap
    (unless (or is-keymap-action?
                (not (eq? *keybinding-current-mode* mode-before)))
      (cond
       ((keymap-passthrough-mode? *keybinding-current-mode*)
        ;; in passthrough mode, do not switch to normal
        #t)
       (persist
        (unless (eq? *keybinding-current-mode* 'normal)
          ;; if a persistent non-normal mode keybinding is pressed, ensure next key is eaten as well
          (xkb-bindings-manager-bindings-seat-ensure-next-key-eaten *xkb-bindings-manager-bindings-seat*)))
       (else
        ;; if action isn't set to persist, immediately return to normal mode
        (keybinding-switch-to-mode! 'normal))))))

(define* (keybinding-switch-to-mode! mode-name #:key (force #f))
  "Switch to keybinding mode MODE-NAME by enabling and disabling corresponding binding sets.
Parameters: (MODE-NAME [#:force FORCE])
  - MODE-NAME: Symbol ('normal, '*root-map*, keymap name) or <keymap>.
  - FORCE: When #t, switches even if mode matches *keybinding-current-mode* (default #f)."
  (let ((target-mode (cond
                      ((keymap? mode-name) (keymap-name mode-name))
                      ((symbol? mode-name) mode-name)
                      (else (string->symbol (format #f "~a" mode-name))))))
    (unless (and (not force)
                 (eq? target-mode *keybinding-current-mode*))
      (log-debug "Switching to mode: ~a" target-mode)
      (gliver-hook-run! *keymap-change-hook* target-mode)
      (set! *keybinding-current-mode* target-mode)

      ;; enable bindings matching the target mode, disable others
      (for-each
       (lambda (pair)
         (let ((spec (car pair))
               (proxy (cdr pair)))
           (let ((spec-mode (keybinding-spec-mode spec)))
             (if (eq? spec-mode target-mode)
                 ;; enable the proxy/binding if spec's mode matches target-mode
                 (xkb-bindings-manager-binding-enable proxy)
                 ;; disable the proxy/binding otherwise
                 (xkb-bindings-manager-binding-disable proxy)))))
       *keybinding-mapping*)

      ;; if entering a non-passthrough keymap mode, eat unbound keys so we handle chord aborts;
      ;; if entering passthrough mode or normal mode, cancel any active ensure_next_key_eaten request
      (if (or (eq? target-mode 'normal)
              (keymap-passthrough-mode? target-mode))
          (xkb-bindings-manager-bindings-seat-cancel-ensure-next-key-eaten *xkb-bindings-manager-bindings-seat*)
          (xkb-bindings-manager-bindings-seat-ensure-next-key-eaten *xkb-bindings-manager-bindings-seat*)))))

(define (keybinding-sync-all!)
  "Synchronize all configured Gliver keybindings with the River compositor.
Destroys existing bindings, generates new binding specs, and creates
`river_xkb_binding_v1` objects for the active seat.
Parameters: None."
  ;; destroy all existing keybindings first
  (xkb-bindings-manager-bindings-destroy-all!)

  ;; generate binding specs
  (and-let* ((specs (keybinding-spec-generate *top-map* *root-map*))
			 (seat (seat-current))
			 (seat-proxy (seat-wl-proxy seat)))

	(log-info "Syncing ~a keybindings specs to River..." (length specs))

	(set! *keybinding-mapping*
		  ;; map each <keybinding-spec> to xkb binding object
		  (map (lambda (spec)
				 (let ((proxy (xkb-bindings-manager-bindings-create-binding
							   seat-proxy
							   (keybinding-spec-keysym spec)
							   (keybinding-spec-modifiers spec))))

				   ;; enable bindings matching the current mode
				   (when (eq? (keybinding-spec-mode spec) *keybinding-current-mode*)
					 (xkb-bindings-manager-binding-enable proxy))
				   (cons spec proxy)))
			   specs))

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
