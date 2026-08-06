;;; gliver/core/keybindings.scm --- Keybinding engine for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver core keybindings)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-69)
  #:use-module (system foreign)
  #:use-module (gliver core logs)
  #:use-module (rnrs bytevectors)
  #:autoload (gliver core types) (define-var)
  #:export (
			*modifier-map*
			*modifier-bitmask-map*
			gliver-key-keysym
			gliver-key-modifiers
			gliver-key?
			%make-gliver-key
			make-gliver-key
			modifier-weight
			modifier<?
			gliver-key=?
			gliver-key->string
			gliver-key->xkb-binding-args
			xkb-lib-common
			xkb-keysym-from-name
			keysym-name->xkb-value
			kbd
			gliver-keymap-bindings
			gliver-keymap-name
			gliver-keymap?
			%make-gliver-keymap
			gliver-key-hash
			make-gliver-keymap
			gliver-keymap-keys
			gliver-keymap->alist
			gliver-binding-persist
			gliver-binding-action
			gliver-binding?
			make-gliver-binding
			define-key
			undefine-key
			lookup-key
			gliver-keymap-clear!
			*top-map*
			*root-map*
			*workspace-map*
			*resize-map*
			gliver-binding-spec-persist
			gliver-binding-spec-mode
			gliver-binding-spec-action
			gliver-binding-spec-modifiers
			gliver-binding-spec-keysym
			gliver-binding-spec?
			make-gliver-binding-spec
			gliver-binding-spec->string
			gliver-binding-spec-generate
))

;;; key representation
(define *modifier-map*
  '(("C" . Control)
    ("M" . Alt)
    ("s" . Super)
    ("S" . Shift)
    ("H" . Hyper)))

(define *modifier-bitmask-map*
  '((Control . 4)     ;; RIVER_MOD_CTRL
    (Alt     . 8)     ;; RIVER_MOD_ALT (mod1)
    (Super   . 64)    ;; RIVER_MOD_SUPER (mod4)
    (Shift   . 1)     ;; RIVER_MOD_SHIFT
    (Hyper   . 32)))  ;; RIVER_MOD_MOD3

;; gliver-key: stores a keybinding definition like C-g, m-x
;; comes with helper functions to convert these keybindings
;; to display strings (C-g), wayland XKB binding
(define-record-type <gliver-key>
  (%make-gliver-key modifiers keysym)
  gliver-key?
  (modifiers gliver-key-modifiers)
  (keysym    gliver-key-keysym))

(define (make-gliver-key modifiers keysym)
  "Create a new key with MODIFIERS and KEYSYM."
  (%make-gliver-key (sort modifiers modifier<?) keysym))

(define (modifier-weight m)
  "Find the index of the modifier in *modifier-map*. Lower index = printed first."
  (or (list-index (lambda (pair) (eq? (cdr pair) m)) *modifier-map*)
      99)) ;; fallback: put unknown at the very end

(define (modifier<? a b)
  "Compare two modifiers based on their definition order in *modifier-map*."
  (< (modifier-weight a) (modifier-weight b)))

(define (gliver-key=? a b)
  "Return #t if a and b represent the same key binding."  
  (and (eq? (gliver-key-keysym a) (gliver-key-keysym b))
       (equal? (gliver-key-modifiers a) (gliver-key-modifiers b))))

(define (gliver-key->string key)
  "Convert a gliver-key to its display string, e.g., \"C-t\"."
  (let* ((mods (map (lambda (m)
					  (let ((pair (find (lambda (p) (eq? (cdr p) m)) 
                                        *modifier-map*)))
                        (if pair 
                            (car pair) 
                            (symbol->string m))))
                    (gliver-key-modifiers key)))
         (keysym (symbol->string (gliver-key-keysym key))))
    (string-join (append mods (list keysym)) "-")))

(define (gliver-key->xkb-binding-args key)
  "Convert a key to XKB binding arguments.
Returns a pair: (modifier-bitmask . xkb-keysym-uint)."
  (let* ((mods (gliver-key-modifiers key))
         (mod-bits (fold (lambda (m acc)
                           (logior acc
                                   (or (assoc-ref *modifier-bitmask-map* m)
                                       0)))
                         0 mods))
         (keysym-val (keysym-name->xkb-value (gliver-key-keysym key))))
    (cons mod-bits keysym-val)))

;; XKB binding argument conversion
;; dynamically link the system's libxkbcommon library
;; bind the C function: uint32_t xkb_keysym_from_name(const char *name, uint32_t flags);
;; this is much better than storing a hash table to map xkb key symbols
(define xkb-lib-common (dynamic-link "libxkbcommon"))
(define xkb-keysym-from-name
  (pointer->procedure
   uint32
   (dynamic-func "xkb_keysym_from_name" xkb-lib-common)
   (list '* uint32)))

(define (keysym-name->xkb-value sym)
  "Ask libxkbcommon to convert a keysym symbol to its uint value."
  (let* ((str (symbol->string sym))
         ;; pass the string pointer, and 0 for XKB_KEYSYM_NO_FLAGS
         (val (xkb-keysym-from-name (string->pointer str) 0)))
    (if (= val 0)
        ;; if xkbcommon returns 0, it means it doesn't recognize the key
        (begin
          (log-warn "Unknown keysym: ~a, using 0" sym)
          0)
        ;; otherwise, return the actual hex value
        val)))

(define xkb-keysym-get-name
  (pointer->procedure int
                      (dynamic-func "xkb_keysym_get_name" xkb-lib-common)
                      (list uint32 '* size_t)))

(define (xkb-value->keysym-name val)
  "Convert a uint value to its keysym string name via libxkbcommon."
  (let ((ptr (bytevector->pointer (make-bytevector 64))))
    (if (> (xkb-keysym-get-name val ptr 64) 0)
        (pointer->string ptr)
        #f)))

;; kbd doesn't have a prefix (gliver-kbd) because I want to maintain
;; this keyword since it's very common for emacs and stumpwm users
;; otherwise, typically this should be called something like `make-gliver-key-kbd`
(define (kbd str)
  "Parse a key description string like \"C-t\", \"M-S-F5\", \"s-Return\", or \"C--\"
Returns gliver-key"
  (let ((parts (string-split str #\-)))
    (when (null? parts)
      (error "Empty key string"))

    (let loop ((remaining parts) (mods '()))
      (cond
       ;; Case 1: Only one part left (e.g., the "t" in "C-t")
       ((null? (cdr remaining))
        (let ((keysym-str (car remaining)))
          (if (string=? keysym-str "")
              (error "Trailing dash in key string" str)
              (make-gliver-key (reverse mods) (string->symbol keysym-str)))))

       ;; Case 2: Special "C--" handling. 
       ;; If the current part is a modifier and the NEXT part is empty, 
       ;; it means the dash was intended as the keysym.
       ((and (assoc-ref *modifier-map* (car remaining))
             (string=? (cadr remaining) "")
             (null? (cddr remaining)))
        (let ((mod (assoc-ref *modifier-map* (car remaining))))
          (make-gliver-key (reverse (cons mod mods)) (string->symbol "-"))))

       ;; Case 3: Current part is a valid modifier
       ((assoc-ref *modifier-map* (car remaining))
        (let ((mod (assoc-ref *modifier-map* (car remaining))))
          (loop (cdr remaining) (cons mod mods))))

       ;; Case 4: Not a modifier, but there are more dashes.
       ;; This handles keysyms that contain dashes themselves (e.g., "XF86-Audio-Raise-Volume")
       (else
        (let ((keysym (string-join remaining "-")))
          (make-gliver-key (reverse mods) (string->symbol keysym))))))))

;; gliver-keymap: stores a name of keymap and list of keybindings
;; defined under it. This is similar to emacs's C-c which has many
;; keybindings defined under it.
;; keymaps are defined by a name and a list of keybindings under it
(define-record-type <gliver-keymap>
  (%make-gliver-keymap name bindings)
  gliver-keymap?
  (name     gliver-keymap-name)
  (bindings gliver-keymap-bindings))  ;; hash-table: gliver-key -> action

(define (gliver-key-hash key . rest)
  "Hash a <gliver-key> by its string representation."
  (apply string-hash (gliver-key->string key) rest))

(define (make-gliver-keymap name)
  (%make-gliver-keymap name (make-hash-table gliver-key=? gliver-key-hash)))

(define (gliver-keymap-keys keymap)
  "Return all keys bound in KEYMAP."
  (hash-table-keys (gliver-keymap-bindings keymap)))

(define (gliver-keymap->alist keymap)
  "Return an alist of (key . action) for all bindings in KEYMAP."
  (hash-table->alist (gliver-keymap-bindings keymap)))

;;; key binding: wraps an action with a persist flag
(define-record-type <gliver-binding>
  (make-gliver-binding action persist)
  gliver-binding?
  (action  gliver-binding-action)
  (persist gliver-binding-persist))

;; below functions don't have a prefix (gliver-keymap-define-key) because I want to maintain
;; this keyword since it's very common for emacs and stumpwm users
(define* (define-key keymap key action #:key (persist #f))
  "Bind KEY to ACTION in KEYMAP.
KEY can be a <gliver-key> or a string (parsed with kbd).
ACTION can be a procedure, a command name string/symbol, or a keymap.
When PERSIST is #t, the keymap stays active after the key is pressed
instead of returning to *top-map*."
  (let ((k (if (gliver-key? key) key (kbd key))))
    (hash-table-set! (gliver-keymap-bindings keymap) k
                     (make-gliver-binding action persist))))

(define (undefine-key keymap key)
  "Remove the binding for KEY from KEYMAP."
  (let ((k (if (gliver-key? key) key (kbd key))))
    (hash-table-delete! (gliver-keymap-bindings keymap) k)))

(define (lookup-key keymap key)
  "Look up KEY in KEYMAP. Returns the <gliver-binding> or #f."
  (let ((k (if (gliver-key? key) key (kbd key))))
    (hash-table-ref/default (gliver-keymap-bindings keymap) k #f)))

(define (gliver-keymap-clear! keymap)
  "Remove all bindings from KEYMAP."
  (hash-table-walk (gliver-keymap-bindings keymap)
                   (lambda (key value)
                     (hash-table-delete! (gliver-keymap-bindings keymap) key))))

;;; standard keymaps
(define-var *top-map*
  (make-gliver-keymap "*top*")
  "Top map layer (default)")

(define-var *root-map*
  (make-gliver-keymap "*root*")
  "Root map layer (prefix)")

(define-var *workspace-map*
  (make-gliver-keymap "*workspace*")
  "workspace map layer")

(define-var *resize-map*
  (make-gliver-keymap "*resize*")
  "Resize map layer")

;;; XKB binding spec generation
;;; a gliver-binding-spec describes a key binding to be created via the
;;; river-xkb-bindings-v1 Wayland protocol.
(define-record-type <gliver-binding-spec>
  (make-gliver-binding-spec keysym modifiers action mode persist)
  gliver-binding-spec?
  (keysym    gliver-binding-spec-keysym)      ;; uint: xkb keysym value
  (modifiers gliver-binding-spec-modifiers)   ;; uint: modifier bitmask
  (action    gliver-binding-spec-action)      ;; procedure, symbol, or string
  (mode      gliver-binding-spec-mode)        ;; symbol: 'normal, 'prefix, or submap name
  (persist   gliver-binding-spec-persist))    ;; bool: stay in current keymap after press

(define (gliver-binding-spec->modifiers-list spec)
  "Converts an integer bitmask into a list of modifier symbols."
  (filter-map (lambda (pair)
                (let ((mod-name (car pair))
                      (mod-bit  (cdr pair)))
                  (and (not (zero?
							 (logand (gliver-binding-spec-modifiers spec) mod-bit)))
                       mod-name)))
              *modifier-bitmask-map*))

(define (gliver-binding-spec->string spec)
  "Convert a gliver-binding-spec to its display string, e.g., \"C-t\"."
  (let* ((mods (map (lambda (m)
					  (let ((pair (find (lambda (p) (eq? (cdr p) m))
                                        *modifier-map*)))
                        (if pair
                            (car pair)
                            (symbol->string m))))
                    (gliver-binding-spec->modifiers-list spec)))
         (keysym (xkb-value->keysym-name (gliver-binding-spec-keysym spec))))
    (string-join (append mods (list keysym)) "-")))

(define (gliver-binding-spec-generate top-map root-map prefix-key mode-name)
  "Generate a list of <gliver-binding-spec> records for XKB key bindings.
Returns a list of gliver-binding-spec records.

For top-map: bindings have mode 'normal.
For root-map: bindings have mode MODE-NAME (a symbol).
Prefix key activation and escape bindings are included."
  (let ((specs '()))
    ;; top-map bindings (always active in normal mode)
    (for-each
     (lambda (pair)
       (let* ((key (car pair))
              (binding (cdr pair))
              (action (gliver-binding-action binding))
              (persist (gliver-binding-persist binding))
              (xkb (gliver-key->xkb-binding-args key)))
         (set! specs
           (cons (make-gliver-binding-spec
                  (cdr xkb)    ;; keysym uint
                  (car xkb)    ;; modifier bitmask
                  action
                  'normal
                  persist)
                 specs))))
     (gliver-keymap->alist top-map))

    ;; prefix key activation (in normal mode)
    (let ((pk (gliver-key->xkb-binding-args prefix-key)))
      (set! specs
        (cons (make-gliver-binding-spec
               (cdr pk)
               (car pk)
               'prefix-activated
               'normal
               #f)
              specs)))

    ;; root-map bindings (in prefix mode)
    (let ((mode-sym (if (symbol? mode-name)
                        mode-name
                        (string->symbol mode-name))))
      (for-each
       (lambda (pair)
         (let* ((key (car pair))
                (binding (cdr pair))
                (action (gliver-binding-action binding))
                (persist (gliver-binding-persist binding))
                (xkb (gliver-key->xkb-binding-args key)))
           (cond
            ((gliver-keymap? action)
             ;; sub-keymap: action is to enter that sub-mode
             (set! specs
               (cons (make-gliver-binding-spec
                      (cdr xkb)
                      (car xkb)
                      (list 'enter-submap (gliver-keymap-name action))
                      mode-sym
                      persist)
                     specs)))
            (else
             (set! specs
               (cons (make-gliver-binding-spec
                      (cdr xkb)
                      (car xkb)
                      action
                      mode-sym
                      persist)
                     specs))))))
       (gliver-keymap->alist root-map))

      ;; escape to exit prefix mode
	  ;; NOTE: should this be configurable?
      (let ((esc-xkb (gliver-key->xkb-binding-args (kbd "Escape")))
            (cg-xkb  (gliver-key->xkb-binding-args (kbd "C-g"))))
        (set! specs
          (cons (make-gliver-binding-spec
                 (cdr esc-xkb) (car esc-xkb)
                 'prefix-abort mode-sym
                 #f)
                specs))
        (set! specs
          (cons (make-gliver-binding-spec
                 (cdr cg-xkb) (car cg-xkb)
                 'prefix-abort mode-sym
                 #f)
                specs))))

    (reverse specs)))
