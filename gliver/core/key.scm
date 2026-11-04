;;; gliver/core/key.scm --- Key representation and parsing for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module manages key representations and parsing.
;;; It handles:
;;;  - Defining key representation records (<key>)
;;;  - Parsing Emacs-like key strings (kbd)
;;;  - Translating keys to Wayland/XKB binding arguments

(define-module (gliver core key)
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-11)
  #:use-module (system foreign)
  #:use-module (gliver core-base)
  #:use-module (gliver deps libxkbcommon)
  #:declarative? #f
  #:export (
			*key-modifier-bitmask-map*
			key=?
			key->string
			*key-keysym-aliases*
			key-keysym-name->xkb-value
			key->xkb-binding-args
			xkb-args->key
			kbd
))

(define-var *key-modifier-bitmask-map*
  '((Control . 4)     ;; RIVER_MOD_CTRL
    (Alt     . 8)     ;; RIVER_MOD_ALT (mod1)
    (Super   . 64)    ;; RIVER_MOD_SUPER (mod4)
    (Shift   . 1)     ;; RIVER_MOD_SHIFT
    (Hyper   . 32))   ;; RIVER_MOD_MOD3
  "Mapping from modifier symbol to river/wayland modifier bitmask.")

(define (key=? a b)
  "Return #t if keys A and A represent the same key binding."
  (and (eq? (key-keysym a)
			(key-keysym b))
       (equal? (key-modifiers a)
			   (key-modifiers b))))

(define (key->string key)
  "Convert a KEY to its display string, e.g., \"C-t\".
Parameters: (KEY)
  - KEY: Target <key> record or key description string.
Returns formatted key string (e.g. \"C-t\", \"s-Return\")."
  (define (%modifier->string m)
    "Return string representation of modifiers as per *key-modifier-map*"
    (let ((pair (find (lambda (entry) (eq? (cdr entry) m))
                      *key-modifier-map*)))
      (if pair
          (car pair)
          (symbol->string m))))

  (if (string? key)
      key
      (let* ((mods (map %modifier->string (key-modifiers key)))
             (keysym (symbol->string (key-keysym key))))
        ;; join the mods with keysym via "-"
        (string-join (append mods (list keysym)) "-"))))

;; NOTE: these symbols are used to map Emacs-like keys to XKB
;; they are stolen from the project mahogany with some little extras
(define-var *key-keysym-aliases*
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
	("Enter" . "Return")
	("Esc" . "Escape")
	("Backspace" . "BackSpace")
	("Tab" . "Tab")
	("Space" . "space"))
  "Alist mapping common keysym shorthand and aliases to standard XKB keysym names.")

(define (key-keysym-name->xkb-value sym)
  "Convert a keysym symbol or string to its XKB uint value.
Parameters: (SYM)
  - SYM: Keysym symbol, string, or character.
Returns 32-bit unsigned integer XKB keysym code."
  (let* ((syms (cond ((symbol? sym) (symbol->string sym))
                     ((string? sym) sym)
                     (else (format #f "~a" sym))))
         ;; if aliases has the key then proceed with it
         (alias (or (assoc-ref *key-keysym-aliases* syms) syms))
         (code (if (= (string-length alias) 1)
                   (char->integer (string-ref alias 0))
                   0))
         (val (xkb-keysym-from-name (string->pointer alias) 0)))
    (cond
     ;; valid xkb keysym found
     ((not (= val 0)) val)
     ;; single printable ascii character (32 to 126)
     ((<= 32 code 126) code)
     ;; fallback: warn and return 0
     (else
      (log-warn "Unknown keysym: ~a, using 0" sym)
      0))))

(define (key->xkb-binding-args key)
  "Convert a key to XKB binding arguments.
Parameters: (KEY)
  - KEY: Target <key> record.
Returns a pair: (modifier-bitmask . xkb-keysym-uint)."
  (define (%modifiers->bitmask mods)
	"Return bit from list of modifiers."
	;; really logior here is nothing but an addition instruction
	;; to get bitmask simply sum all of the modifier values
    (apply logior
           (filter-map (lambda (m)
                         (assq-ref *key-modifier-bitmask-map* m))
                       mods)))

  (cons (%modifiers->bitmask (key-modifiers key))
        (key-keysym-name->xkb-value (key-keysym key))))

(define (xkb-args->key modifier-bitmask keysym-value)
  "Convert an XKB modifier bitmask and numeric keysym value to a <key> record.
Parameters: (MODIFIER-BITMASK KEYSYM-VALUE)
  - MODIFIER-BITMASK: River/Wayland modifier bitmask integer.
  - KEYSYM-VALUE: Numeric XKB keysym.
Returns <key> record."
  (let* ((mods (filter-map (lambda (pair)
                             (let ((mod-name (car pair))
                                   (mod-bit  (cdr pair)))
                               (and (not (zero? (logand modifier-bitmask mod-bit)))
                                    mod-name)))
                           *key-modifier-bitmask-map*))
         (name-str (xkb-value->keysym-name keysym-value))
         (keysym (if name-str (string->symbol name-str) 'None)))
    (make-key mods keysym)))

;; kbd doesn't have a prefix (key-kbd) because we maintain
;; this keyword since it's very common for emacs and stumpwm users
(define (kbd str)
  "Parse a key description string like \"C-t\" into a <key> record.
Parameters: (STR)
  - STR: String key description (e.g. \"C-t\", \"s-Return\", \"C--\").
Returns parsed <key> record."
  (define (%extract-parts s)
	"Group key parts via regex.
Group 1: all leading prefixes (e.g. \"C-\", \"C-M-\").
Group 3: the final keysym"
    (and-let* (((string? s))
               ((not (string-null? s)))
               (m (string-match "^(([^-]+-)*)(.+)$" s)))
      (values (match:substring m 1)
              (match:substring m 3))))

  (define (%parse-prefix prefix keysym-str)
    "Split \"C-M-\" into '(\"C\" \"M\")."
    (let* ((tokens (filter (negate string-null?) (string-split prefix #\-)))
           (mods   (take-while (lambda (t) (assoc-ref *key-modifier-map* t)) tokens))
           (extra  (drop tokens (length mods))))
      (values (map (lambda (t)
					 (assoc-ref *key-modifier-map* t))
				   mods)
              ;; re-attach any non-modifier dash prefixes (e.g. "XF86-Audio-Mute")
              (string->symbol (string-join (append extra (list keysym-str)) "-")))))

  (let-values (((prefix keysym-str) (%extract-parts str)))
    (unless keysym-str
      (error "Invalid or empty key string" str))
    (let-values (((mods keysym) (%parse-prefix prefix keysym-str)))
      (make-key mods keysym))))
