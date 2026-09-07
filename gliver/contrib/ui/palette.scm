(define-module (gliver contrib ui palette)
  #:use-module (gliver core)
  #:declarative? #f
  #:export (
			*palette-backend*
			*palette-dmenu-options*
			*palette-backend-kill*
			palette-show
			palette-kill
			make-dmenu-options
))

(define-var *palette-backend* #f
			"The command palette backend that will be used when a
palette is called. This should be any function that will call
make-launcher-backend.")

(define-var *palette-dmenu-options* #f
			"The command palette backend that will be used when a
palette is called. This should be any function that will call
make-launcher-backend.")

(define-var *palette-backend-kill* #f
			"The command to kill palette menu.")

(define* (palette-show candidates #:key (keys '())
					   (theme-overrides '()) (backend #f)
					   (on-select #f) (on-cancel #f)
					   (prompt #f) (initial-filter ""))
  "Prompt the user to select from CANDIDATES.
BACKEND defaults to the globally configured `*palette-backend*`.
When ON-SELECT is provided, runs asynchronously and calls ON-SELECT with the selected key or candidate index.
When ON-SELECT is #f, runs synchronously and returns (index . original-candidate) or #f if cancelled."
  (palette-kill)
  (let ((palette (or backend *palette-backend*)))
    (if (procedure? palette)
        (if (procedure? on-select)
            (palette candidates
                #:theme-overrides theme-overrides
                #:prompt prompt
                #:initial-filter initial-filter
                #:on-select (lambda (idx)
                              (let ((val (if (and idx (pair? keys) (number? idx) (< idx (length keys)))
                                             (list-ref keys idx)
                                             idx)))
                                (on-select val)))
                #:on-cancel on-cancel)
            (let ((idx (palette candidates
                           #:theme-overrides theme-overrides
                           #:prompt prompt
                           #:initial-filter initial-filter)))
              (if (and idx (pair? keys) (number? idx) (< idx (length keys)))
                  (list-ref keys idx)
                  idx)))
        (begin
          (log-error "No palette backend configured")
          (when (procedure? on-cancel) (on-cancel))
          #f))))

(define* (palette-kill #:key (backend-kill #f))
  "Kill the backend process."
  (let ((kill-fn (or backend-kill *palette-backend-kill*)))
    (when (procedure? kill-fn)
      (kill-fn))))

(define* (make-dmenu-options options colors #:key (pango #t)
							 (widths '()) (searchable '()) (visible '()))
  "Returns a list of rofi/dmenu compatible strings from a list of values."
  (if (procedure? *palette-dmenu-options*)
      (*palette-dmenu-options* options colors
							   #:pango pango #:widths widths
							   #:searchable searchable #:visible visible)
      '()))
