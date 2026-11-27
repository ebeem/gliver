;;; gliver/contrib/ui/overlay/which-key.scm --- Display available keys in submaps
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; Listens for keymap changes and, when entering a non-top-map mode
;;; (prefix or submap), displays all available keybindings using the
;;; configured toast backend. Also provides which-key-show-keymap to
;;; display *top-map* or any keymap on demand.
;;;
;;; Usage:
;;;   (use-modules (gliver contrib ui overlay which-key))
;;;   (which-key-enable!)

(define-module (gliver contrib ui overlay which-key)
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-1)
  #:use-module (gliver core)
  #:use-module (gliver contrib ui gleui widgets)
  #:use-module (gliver contrib ui gleui toast)
  #:use-module (gliver contrib commands)
  #:declarative? #f
  #:export (
            *which-key-separator*
            *which-key-columns*
            *which-key-column-width*
            *which-key-delay*
            *which-key-keybinding-color*
            *which-key-separator-color*
            *which-key-command-color*
            *which-key-keymap-color*
            *which-key-keymap-prefix*
            *which-key-keymap-remove-asterisks*
            *%which-key-enabled*
            *which-key-identifier-name*
            *which-key-font*
            *which-key-font-size*
            *which-key-bg-color*
            *which-key-fg-color*
            *which-key-border-color*
            *which-key-border-width*
            *which-key-border-radius*
            *which-key-location*
            *which-key-margin*
            *which-key-padding-x*
            *which-key-padding-y*
            describe-action
            format-bindings
            *which-key-current-keymap*
            which-key-on-keymap-change
            which-key-show-keymap
            which-key-enable!
            which-key-disable!
))

;;; configuration
(define-var *which-key-separator* " -> ")
(define-var *which-key-columns* 4
            "Number of columns displayed")
(define-var *which-key-column-width* 40
            "Number of characters of each column")
(define-var *which-key-delay* 200
            "Delay in ms before showing which-key if no input is provided.")
(define-var *which-key-keybinding-color* #:inherit *theme-mauve*
            "The color of the keybinding in which key")
(define-var *which-key-separator-color* #:inherit *theme-overlay1*
            "The color of the separator in which key")
(define-var *which-key-command-color* #:inherit *theme-text*
            "The color of the command in which key")
(define-var *which-key-keymap-color* #:inherit *theme-yellow*
            "The color of the keymap in which key")
(define-var *which-key-keymap-prefix* "+"
            "The prefix symbol of the keymap in which key")
(define-var *which-key-keymap-remove-asterisks* #t
            "Whether to remove the asterisks in the keymap name")
(define-var *%which-key-enabled* #f
            "Internal state that indicates which-key is enabled")
(define-var *which-key-identifier-name* "which-key-identifier-name"
            "Identifier name passed to toast and used to detect it later in order to kill it for instance.")

(define-var *which-key-font* #:inherit *gleui-toast-font*
            "Font family used for which-key toast.")
(define-var *which-key-font-size* #:inherit *gleui-toast-font-size*
            "Font size used for which-key toast.")
(define-var *which-key-bg-color* #:inherit *gleui-toast-bg-color*
            "Background color for which-key toast.")
(define-var *which-key-fg-color* #:inherit *gleui-toast-fg-color*
            "Text color for which-key toast.")
(define-var *which-key-border-color* #:inherit *gleui-toast-border-color*
            "Border color for which-key toast.")
(define-var *which-key-border-width* #:inherit *gleui-toast-border-width*
            "Border width in pixels for which-key toast.")
(define-var *which-key-border-radius* #:inherit *gleui-toast-border-radius*
            "Corner border radius in pixels for which-key toast.")
(define-var *which-key-location* 8
            "Gliver position (1-9) for which-key toast.")
(define-var *which-key-margin* #:inherit *gleui-toast-margin*
            "Margin in pixels from the screen edge for which-key toast.")
(define-var *which-key-padding-x* #:inherit *gleui-toast-padding-x*
            "Padding horizontal content in pixels for which-key toast.")
(define-var *which-key-padding-y* #:inherit *gleui-toast-padding-y*
            "Padding vertical content in pixels for which-key toast.")

(define (%which-key-theme-overrides)
  `((font . ,*which-key-font*)
    (font-size . ,*which-key-font-size*)
    (background . ,*which-key-bg-color*)
    (bg-color . ,*which-key-bg-color*)
    (foreground . ,*which-key-fg-color*)
    (fg-color . ,*which-key-fg-color*)
    (border-color . ,*which-key-border-color*)
    (border-width . ,*which-key-border-width*)
    (border-radius . ,*which-key-border-radius*)
    (location . ,*which-key-location*)
    (margin . ,*which-key-margin*)
    (padding-x . ,*which-key-padding-x*)
    (padding-y . ,*which-key-padding-y*)))

(define (%which-key-font-spec)
  "Return the Pango font description string configured for which-key."
  (format #f "~a ~a" *which-key-font* *which-key-font-size*))

(define (describe-action action)
  "Return a short description toast span for a binding ACTION."
  (let* ((is-keymap? (or (and (list? action) (eq? (car action) 'enter-keymap))
                         (keymap? action)))
         (raw-name (cond
                    ((and (list? action) (eq? (car action) 'enter-keymap))
                     (format #f "~a" (cadr action)))
                    ((keymap? action)
                     (format #f "~a" (keymap-name action)))
                    ((symbol? action) (symbol->string action))
                    ((string? action) action)
                    ((procedure? action) "λ")
                    (else "?")))

         ;; clean asterisks if it is a keymap and the config is #t
         (cleaned-name (if (and is-keymap? *which-key-keymap-remove-asterisks*)
                           (string-replace-substring raw-name "*" "")
                           raw-name))

         ;; apply prefix if it is a keymap
         (final-name (if is-keymap?
                         (string-append *which-key-keymap-prefix* cleaned-name)
                         cleaned-name))

         ;; choose keymap or command color
         (color (if is-keymap?
                    *which-key-keymap-color*
                    *which-key-command-color*)))
    (gleui-w-text final-name
                  #:font (%which-key-font-spec)
                  #:color color
                  #:markup? #f)))

(define (%chunk-list lst n)
  (let loop ((rest lst) (acc '()))
    (if (null? rest)
        (reverse acc)
        (let* ((len (length rest))
               (take-n (take rest (min n len)))
               (drop-n (drop rest (min n len))))
          (loop drop-n (cons take-n acc))))))

(define (format-bindings keymap)
  "Format all bindings in KEYMAP as a <gleui-widget> grid."
  (let* ((font-spec    (%which-key-font-spec))
         (pairs        (keymap->alist keymap))
         ;; sort alphabetically by the string representation of the key
         (sorted-pairs (sort pairs (lambda (a b)
                                     (string<? (key->string (car a))
                                               (key->string (car b))))))
         ;; convert each binding into a widget row cell
         (cols (map (lambda (pair)
                      (let* ((key-str       (key->string (car pair)))
                             (action        (keybinding-action (cdr pair)))
                             (key-widget    (gleui-w-text key-str
                                                          #:font font-spec
                                                          #:color *which-key-keybinding-color*
                                                          #:markup? #f))
                             (sep-widget    (gleui-w-text *which-key-separator*
                                                          #:font font-spec
                                                          #:color *which-key-separator-color*
                                                          #:markup? #f))
                             (action-widget (describe-action action)))
                        (gleui-w-row (list key-widget sep-widget action-widget))))
                    sorted-pairs))
         ;; group the cells into rows based on the *which-key-columns* config
         (rows (%chunk-list cols *which-key-columns*)))
    (gleui-w-grid rows
                  #:col-gap *gleui-toast-column-gap*
                  #:row-gap *gleui-toast-row-gap*)))

(define-var *which-key-current-keymap* #f
            "The <keymap> currently displayed in which-key, or #f.")

(define (which-key-on-keymap-change keymap)
  "Hook handler: show bindings when entering a non-top-map mode."
  (let ((km (keymap-find keymap)))
    (cond
     ((eq? km *top-map*)
      ;; returned to *top-map* mode, kill the current which-key toast unless
      ;; *top-map* is what we are explicitly showing
      (unless (eq? *which-key-current-keymap* *top-map*)
        (set! *which-key-current-keymap* #f)
        (gleui-toast-kill! #:name *which-key-identifier-name*))
      #f)
     (else
      (if (keymap? km)
          (begin
            (set! *which-key-current-keymap* km)
            (gleui-toast-show (format-bindings km)
                              #:name *which-key-identifier-name*
                              #:theme-overrides (%which-key-theme-overrides)
                              #:on-close (lambda ()
                                           (when (eq? *which-key-current-keymap* km)
                                             (set! *which-key-current-keymap* #f)))))
          (begin
            (log-warn "No keymap found for mode ~a" keymap)
            (set! *which-key-current-keymap* #f)
            (gleui-toast-kill! #:name *which-key-identifier-name*)))))))

(define* (which-key-show-keymap #:optional (keymap *top-map*))
  "Display keybindings for KEYMAP (defaults to *top-map*) using toast.
Passes eat-next-key? so that any key press dismisses the toast."
  (let ((km (keymap-find keymap)))
    (if km
        (begin
          ;; return to top-map if in a submap so normal mode is restored
          (unless (eq? *keybinding-current-mode* '*top-map*)
            (keybinding-switch-to-mode! *top-map*))
          (set! *which-key-current-keymap* km)
          (gleui-toast-show (format-bindings km)
                            #:name *which-key-identifier-name*
                            #:theme-overrides (%which-key-theme-overrides)
                            #:eat-next-key? #t
                            #:on-close (lambda ()
                                         (when (eq? *which-key-current-keymap* km)
                                           (set! *which-key-current-keymap* #f)))))
        (log-warn "No keymap found for mode ~a" keymap))))

(define (which-key-enable!)
  "Enable which-key: show available keys when entering keymaps."
  (unless *%which-key-enabled*
    (gliver-hook-add! *keymap-change-hook* 'which-key-on-keymap-change)
    (set! *%which-key-enabled* #t)
    (log-info "Which key enabled.")))

(define (which-key-disable!)
  "Disable which-key."
  (when *%which-key-enabled*
    (gliver-hook-remove! *keymap-change-hook* 'which-key-on-keymap-change)
    (set! *which-key-current-keymap* #f)
    (gleui-toast-kill! #:name *which-key-identifier-name*)
    (set! *%which-key-enabled* #f)
    (log-info "Which key disabled.")))
