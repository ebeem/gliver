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
  #:use-module (gliver deps libxkbcommon)
  #:use-module (rnrs bytevectors)
  #:use-module (gliver core types)
  #:export (
			*modifier-map*
			*modifier-bitmask-map*
			*keysym-aliases*
			keysym-name->xkb-value
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
			kbd
			gliver-keymap-bindings
			gliver-keymap-name
			gliver-keymap?
			%make-gliver-keymap
			gliver-key-hash
			make-gliver-keymap
			*registered-keymaps*
			registered-keymaps
			register-keymap!
			gliver-keymap-keys
			gliver-keymap->alist
			gliver-binding-persist
			gliver-binding-action
			gliver-binding?
			make-gliver-binding
			define-key
			define-keys
			keymap
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
			gliver-binding-spec->modifiers-list
			gliver-binding-spec->string
			gliver-binding-spec-generate))

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

;; stolen from mahogany :)
(define *keysym-aliases*
  '(("RET" . "Return")
	("ESC" . "Escape")
	("TAB" . "Tab")
	("DEL" . "BackSpace")
	("SPC" . "space")
	("!" . "exclam")
	("\"" . "quotedbl")
	("$" . "dollar")
	("£" . "sterling")
	("%" . "percent")
	("&" . "ampersand")
	("'" . "apostrophe")
	("`" . "grave")
	("&" . "ampersand")
	("(" . "parenleft")
	(")" . "parenright")
	("*" . "asterisk")
	("+" . "plus")
	("," . "comma")
	("-" . "minus")
	("." . "period")
	("/" . "slash")
	(":" . "colon")
	(";" . "semicolon")
	("<" . "less")
	("=" . "equal")
	(">" . "greater")
	("?" . "question")
	("@" . "at")
	("[" . "bracketleft")
	("\\" . "backslash")
	("]" . "bracketright")
	("^" . "asciicircum")
	("_" . "underscore")
	("#" . "numbersign")
	("{" . "braceleft")
	("|" . "bar")
	("}" . "braceright")
	("~" . "asciitilde")
	("«" . "guillemotleft")
	("»" . "guillemotright")
	("À" . "Agrave")
	("à" . "agrave")
	("Ç" . "Ccedilla")
	("ç" . "ccedilla")
	("É" . "Eacute")
	("é" . "eacute")
	("È" . "Egrave")
	("è" . "egrave")
	("Ê" . "Ecircumflex")
	("ê" . "ecircumflex")

	;; user-friendly common aliases
	("Enter" . "Return")
	("Esc" . "Escape")
	("Backspace" . "BackSpace")
	("Tab" . "Tab")
	("Space" . "space")))

(define (keysym-name->xkb-value sym)
  "Ask libxkbcommon to convert a keysym symbol or string to its uint value."
  (if (not xkb-keysym-from-name)
      0
      (let* ((raw-str (if (symbol? sym) (symbol->string sym) (format #f "~a" sym)))
             (aliased-str (or (assoc-ref *keysym-aliases* raw-str) raw-str))
             ;; pass the string pointer, and 0 for XKB_KEYSYM_NO_FLAGS
             (val (xkb-keysym-from-name (string->pointer aliased-str) 0)))
        (if (= val 0)
            (if (and (= (string-length aliased-str) 1)
                     (<= 32 (char->integer (string-ref aliased-str 0)) 126))
                (char->integer (string-ref aliased-str 0))
                (begin
                  (log-warn "Unknown keysym: ~a, using 0" sym)
                  0))
            ;; otherwise, return the actual hex value
            val))))

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

(define *registered-keymaps* '())

(define (registered-keymaps)
  "Return a list of all registered keymaps."
  *registered-keymaps*)

(define (register-keymap! keymap)
  "Register a keymap in the global registry."
  (unless (memq keymap *registered-keymaps*)
    (set! *registered-keymaps* (cons keymap *registered-keymaps*)))
  keymap)

(define (make-gliver-keymap name)
  (let* ((sym (if (symbol? name) name (string->symbol name)))
         (km (%make-gliver-keymap sym (make-hash-table gliver-key=? gliver-key-hash))))
    (register-keymap! km)
    km))

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

(define-syntax define-keys
  (syntax-rules ()
    ;; case 0: no bindings provided
    ((_ keymap)
     (begin))

    ;; case 1: 1 binding pair left
    ((_ keymap key action)
     (define-key keymap key action))

    ;; case 2: more than one binding pair
    ((_ keymap key action rest ...)
     (begin
       (define-key keymap key action)
       (define-keys keymap rest ...)))))

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
  (make-gliver-keymap '*top-map*)
  "Top map layer (default)")

(define-var *root-map*
  (make-gliver-keymap '*root-map*)
  "Root map layer (prefix)")

(define-var *workspace-map*
  (make-gliver-keymap '*workspace-map*)
  "workspace map layer")

(define-var *resize-map*
  (make-gliver-keymap '*resize-map*)
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

(define (generate-keymap-specs keymap mode-sym)
  "Generate binding specs for a single keymap under MODE-SYM."
  (let ((specs '()))
    (for-each
     (lambda (pair)
       (let* ((key (car pair))
              (binding (cdr pair))
              (action (gliver-binding-action binding))
              (persist (gliver-binding-persist binding))
              (xkb (gliver-key->xkb-binding-args key)))
         (cond
          ((gliver-keymap? action)
           ;; sub-keymap: action is to enter that sub-mode by symbol
           (let ((sub-mode (gliver-keymap-name action)))
             (set! specs
               (cons (make-gliver-binding-spec
                      (cdr xkb)
                      (car xkb)
                      (list 'enter-submap sub-mode)
                      mode-sym
                      persist)
                     specs))))
          ((and (list? action) (eq? (car action) 'enter-submap))
           (let* ((target (cadr action))
                  (sub-mode (if (gliver-keymap? target)
                                (gliver-keymap-name target)
                                (if (symbol? target)
                                    target
                                    (string->symbol (format #f "~a" target))))))
             (set! specs
               (cons (make-gliver-binding-spec
                      (cdr xkb)
                      (car xkb)
                      (list 'enter-submap sub-mode)
                      mode-sym
                      persist)
                     specs))))
          (else
           (set! specs
             (cons (make-gliver-binding-spec
                    (cdr xkb)
                    (car xkb)
                    action
                    mode-sym
                    persist)
                   specs))))))
     (gliver-keymap->alist keymap))
    specs))

(define* (gliver-binding-spec-generate top-map root-map #:optional (root-mode-sym '*root-map*))
  "Generate a list of <gliver-binding-spec> records for XKB key bindings.
Returns a list of gliver-binding-spec records for top-map, root-map,
and all registered submaps."
  (let* ((root-sym (if (symbol? root-mode-sym) root-mode-sym (string->symbol root-mode-sym)))
         (submaps (filter (lambda (km)
                            (and (not (eq? km top-map))
                                 (not (eq? km root-map))))
                          *registered-keymaps*))
         (specs '()))

    ;; top-map bindings (normal mode)
    (set! specs (append (generate-keymap-specs top-map 'normal) specs))

    ;; root-map bindings
    (set! specs (append (generate-keymap-specs root-map root-sym) specs))
    ;; escape and C-g to exit prefix mode
    ;; NOTE: should this be configurable?
    (let ((esc-xkb (gliver-key->xkb-binding-args (kbd "Escape")))
          (cg-xkb  (gliver-key->xkb-binding-args (kbd "C-g"))))
      (unless (lookup-key root-map (kbd "Escape"))
        (set! specs
          (cons (make-gliver-binding-spec
                 (cdr esc-xkb) (car esc-xkb)
                 'prefix-abort root-sym #f)
                specs)))
      (unless (lookup-key root-map (kbd "C-g"))
        (set! specs
          (cons (make-gliver-binding-spec
                 (cdr cg-xkb) (car cg-xkb)
                 'prefix-abort root-sym #f)
                specs))))

    ;; submap bindings
    (for-each
     (lambda (submap)
       (let* ((submap-mode (gliver-keymap-name submap))
              (submap-specs (generate-keymap-specs submap submap-mode))
              (esc-xkb (gliver-key->xkb-binding-args (kbd "Escape")))
              (cg-xkb  (gliver-key->xkb-binding-args (kbd "C-g"))))
         (set! specs (append submap-specs specs))
         (unless (lookup-key submap (kbd "Escape"))
           (set! specs
             (cons (make-gliver-binding-spec
                    (cdr esc-xkb) (car esc-xkb)
                    'prefix-abort submap-mode #f)
                   specs)))
         (unless (lookup-key submap (kbd "C-g"))
           (set! specs
             (cons (make-gliver-binding-spec
                    (cdr cg-xkb) (car cg-xkb)
                    'prefix-abort submap-mode #f)
                   specs)))))
     submaps)

    (reverse specs)))
