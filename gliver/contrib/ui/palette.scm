(define-module (gliver contrib ui palette)
  #:use-module (gliver core)
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
					   (theme-overrides '()) (backend *palette-backend*))
  "Prompt the user to select from CANDIDATES.
BACKEND defaults to the globally configured `*palette-backend*`.
Returns (index . original-candidate) or #f if cancelled."
  (palette-kill)
  (let ((idx (backend candidates
					  #:theme-overrides theme-overrides)))
	(if (and idx (> (length keys) idx))
		(list-ref keys idx)
		idx)))

(define* (palette-kill #:key (backend-kill *palette-backend-kill*))
  "Kill the backend process."
  (backend-kill))

(define* (make-dmenu-options options colors #:key (pango #t)
							 (widths '()) (searchable '()) (visible '()))
  "Returns a list of rofi/dmenu compatible strings from a list of values."
  (*palette-dmenu-options* options colors
						   #:pango pango #:widths widths
						   #:searchable searchable #:visible visible))
