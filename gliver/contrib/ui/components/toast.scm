(define-module (gliver contrib ui components toast)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-1)
  #:use-module (gliver core)
  #:declarative? #f
  #:export (
			*toast-backend*
			*toast-backend-kill*
			toast-show
			toast-make-span
			toast-make-column
			toast-make-row
			toast-kill
))

(define-var *toast-backend* #f
			"The toast backend that will be used when a
toast is called. This should be any function that will call
make-toast-backend.")

(define-var *toast-backend-kill* #f
			"The toast backend that will be used when a
toast is called. This should be any function that will call
make-toast-backend.")

(define* (toast-show message #:key (timeout 0)
					 (name "") (backend #f))
  "Display the user a dialog with MESSAGE.
BACKEND defaults to the globally configured `*toast-backend*`."
  (let ((toast (or backend *toast-backend*)))
    (if (procedure? toast)
        (toast message #:timeout timeout #:name name)
        (log-debug "toast: No toast backend configured for toast-show"))))

(define* (toast-make-span text #:key color weight)
  "Creates a text span with formatting attributes."
  `((text . ,text)
    (color . ,color)
    (weight . ,weight)))

(define* (toast-make-column spans #:key width)
  "Creates a column containing a list of spans, with column metadata."
  ;; ensure spans is always a list, even if the user passes a single span
  (let ((span-list (if (and (list? spans) (not (null? spans)) (pair? (car spans)))
                       spans
                       (list spans))))
    `((spans . ,span-list)
      (width . ,width))))

(define (toast-make-row . columns)
  "A row is a list of columns."
  columns)

(define* (toast-kill #:key (name #f)
					 (backend-kill #f))
  "Kill the user active toast dialog.
BACKEND defaults to the globally configured `*toast-backend-kill*`."
  (let ((kill-fn (or backend-kill *toast-backend-kill*)))
    (when (procedure? kill-fn)
      (kill-fn #:name name))))
