(define-module (gliver contrib ui launcher)
  #:use-module (gliver core)
  #:declarative? #f
  #:export (
			*launcher-backend*
			*launcher-backend-kill*
			launcher-show
			launcher-kill
))

(define-var *launcher-backend* #f
			"The command launcher backend that will be used when a
launcher is called. This should be any function that will call
make-launcher-backend.")

(define-var *launcher-backend-kill* #f
			"The command to kill launcher process.")

(define* (launcher-show #:key (theme-overrides '())
						(backend *launcher-backend*))
  "Prompt the user to select from list of applications.
BACKEND defaults to the globally configured `*launcher-backend*`. "
  (backend #:theme-overrides theme-overrides))

(define* (launcher-kill #:key (backend-kill *launcher-backend-kill*))
  "Kill the backend process."
  (backend-kill))

