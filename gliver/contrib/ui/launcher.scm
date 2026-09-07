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
						(backend #f))
  "Prompt the user to select from list of applications.
BACKEND defaults to the globally configured `*launcher-backend*`. "
  (let ((launcher (or backend *launcher-backend*)))
    (if (procedure? launcher)
        (launcher #:theme-overrides theme-overrides)
        (begin
          (log-error "No launcher backend configured")
          #f))))

(define* (launcher-kill #:key (backend-kill #f))
  "Kill the backend process."
  (let ((kill-fn (or backend-kill *launcher-backend-kill*)))
    (when (procedure? kill-fn)
      (kill-fn))))
