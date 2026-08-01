(define-module (gliver contrib ui palette base)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-1)
  #:use-module (gliver core)
  #:export (
			*palette-backend*
			completion-show
			shell-quote
			item-formatter
			make-launcher-backend
			make-dmenu-options
))

(define-var *palette-backend* #f
			"The command palette backend that will be used when a
palette is called. This should be any function that will call
make-launcher-backend.")

(define* (completion-show candidates #:key (keys '()) (backend *palette-backend*))
  "Prompt the user to select from CANDIDATES.
BACKEND defaults to the globally configured `*palette-backend*`.
Returns (index . original-candidate) or #f if cancelled."
  (let ((idx (backend candidates)))
	(if (and idx (> (length keys) idx))
		(list-ref keys idx)
		idx)))

(define (shell-quote val)
  "Quote a value so it can be safely passed in a shell command."
  (let ((str (if (string? val) val (format #f "~a" val))))
    (string-append "'" (string-replace-substring str "'" "'\\''") "'")))

(define (item-formatter item)
  (string-join
   (map
	(lambda (x)
	  (format #f "~a" x)) item) "\t"))

(define* (make-launcher-backend command #:key (formatter item-formatter))
  (lambda (candidates)
    (let* ((display-strings (map formatter candidates))
           (input-str (string-join display-strings "\n"))
           ;; printf to escape null byte needed for metadata injection
		   (cmd (string-append "while IFS= read -r line; do printf \"%b\\n\" \"$line\"; done <<'EOF_LAUNCHER' | " command "\n"
                               input-str
                               "\nEOF_LAUNCHER"))
           (port (open-input-pipe cmd))
           (selected (read-line port)))
      (close-pipe port)
	  (if (or (eof-object? selected) (string-null? selected))
            #f
            (string->number selected)))))

(define* (make-dmenu-options options colors #:key (pango #t)
							 (widths '()) (searchable '()) (visible '()))
  "Returns a list of rofi/dmenu compatible strings from a list of values."
  (map (lambda (columns)
         (let* ((search-parts
                 (filter-map (lambda (col idx)
                               (let ((is-searchable? 
                                      (if (and (not (null? searchable))
                                               (< idx (length searchable)))
                                          (list-ref searchable idx)
                                          #t)))
                                 (if is-searchable? (format #f "~a" col) #f)))
                             columns
                             (iota (length columns))))

                ;; join the searchable columns with spaces so rofi can filter them
                (search-string (string-join search-parts " "))

                ;; build the formatted display columns (with truncation and pango)
				(formatted-columns
                 (filter-map (lambda (col idx)
                               (let ((is-visible? 
                                      (if (and (not (null? visible))
                                               (< idx (length visible)))
                                          (list-ref visible idx)
                                          #t)))
                                 (if is-visible?
                                     (let* ((str (format #f "~a" col))
                                            ;; column width if available
                                            (width (if (and (not (null? widths))
                                                            (< idx (length widths)))
                                                       (list-ref widths idx)
                                                       #f))
                                            ;; column value truncated (if exceeds width)
                                            (truncated (if width
                                                           (if (> (string-length str) width)
                                                               (string-append (string-take str (max 0 (- width 3))) "...")
                                                               (string-pad-right str width #\space))
                                                           str))
                                            ;; column color if available
                                            (color (if (and pango
                                                            (not (null? colors))
                                                            (< idx (length colors)))
                                                       (list-ref colors idx)
                                                       #f)))
                                       ;; apply pango markup if a color exists, else return raw truncated string
                                       (if color
                                           (format #f "<span color='~a'>~a</span>" color truncated)
                                           truncated))
                                     ;; if not visible, return #f so filter-map drops it
                                     #f)))
                             columns
                             (iota (length columns))))

                ;; concatenate the formatted columns for this row with tabs
                (display-string (string-join formatted-columns "  ")))

           ;; return the rofi-compatible string separating search text from display text
		   (string-append search-string
						  "\\000"
						  "display\\037" display-string "\\037"
						  "meta\\037" search-string "\\037")))
       options))
