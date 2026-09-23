;;; gliver/contrib/ui/gleui/launcher.scm --- Desktop Application Launcher for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver contrib ui gleui launcher)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-69)
  #:use-module (gliver core)
  #:use-module (gliver core config)
  #:use-module (gliver core logs)
  #:use-module (gliver contrib ui components launcher)
  #:use-module (gliver contrib ui gleui base)
  #:use-module (gliver contrib ui gleui palette)
  #:declarative? #f
  #:export (
			*desktop-apps-cache*
			*desktop-apps-cache-timestamp*
			*desktop-apps-cache-ttl*
			desktop-app-search-str
			desktop-app-filepath
			desktop-app-keywords
			desktop-app-icon
			desktop-app-terminal?
			desktop-app-generic-name
			desktop-app-comment
			desktop-app-exec
			desktop-app-name
			desktop-app?
			desktop-app-comment-strip
			desktop-app-exec-clean
			desktop-app-format-display
			desktop-app-format-exec
			desktop-app->candidate-item
			desktop-file-parse
			desktop-applications-scan
			gleui-launcher-get-desktop-applications
			gleui-launcher-get-desktop-search-strings
			gleui-launcher-rescan-applications!
			gleui-launcher-launch-command
			gleui-launcher-show
			gleui-launcher-install!
))


(define-record-type <desktop-app>
  (%make-desktop-app name exec comment generic-name terminal? icon keywords filepath search-str)
  desktop-app?
  (name         desktop-app-name)
  (exec         desktop-app-exec)
  (comment      desktop-app-comment)
  (generic-name desktop-app-generic-name)
  (terminal?    desktop-app-terminal?)
  (icon         desktop-app-icon)
  (keywords     desktop-app-keywords)
  (filepath     desktop-app-filepath)
  (search-str   desktop-app-search-str))

(define (desktop-app-comment-strip str)
  "Strip trailing comments starting with # outside of quotes."
  (if (not (string? str))
      ""
      (let ((len (string-length str)))
        (let loop ((i 0) (in-quote #f))
          (if (>= i len)
              (string-trim-both str)
              (let ((c (string-ref str i)))
                (cond
                 ((and in-quote (char=? c in-quote))
                  (loop (1+ i) #f))
                 ((and (not in-quote) (or (char=? c #\") (char=? c #\x27)))
                  (loop (1+ i) c))
                 ((and (not in-quote) (char=? c #\#))
                  (string-trim-both (substring str 0 i)))
                 (else
                  (loop (1+ i) in-quote)))))))))

(define (desktop-app-exec-clean exec-str)
  "Strip standard XDG field codes (%f, %u, etc.) and sanitize Exec command line."
  (if (string? exec-str)
      (let* ((no-comment (desktop-app-comment-strip exec-str))
             (stripped (regexp-substitute/global #f "%[a-zA-Z]" no-comment 'pre "" 'post))
             (trimmed (string-trim-both stripped)))
        ;; if the entire command is wrapped in outer quotes e.g. "cmd arg1 arg2"
        ;; but is not an existing single file on disk, strip the outer quotes.
        (if (and (>= (string-length trimmed) 2)
                 (string-prefix? "\"" trimmed)
                 (string-suffix? "\"" trimmed)
                 (not (file-exists? (substring trimmed 1 (1- (string-length trimmed))))))
            (let ((unquoted (substring trimmed 1 (1- (string-length trimmed)))))
              (string-trim-both unquoted))
            trimmed))
      ""))

(define (desktop-app-format-display app)
  "Format APP into Pango markup for visible row rendering."
  (let* ((name (desktop-app-name app))
         (generic (desktop-app-generic-name app))
         (comment (desktop-app-comment app))
         (desc (cond
                ((and (string? generic) (not (string-null? generic))) generic)
                ((and (string? comment) (not (string-null? comment))) comment)
                (else ""))))
    (if (string-null? desc)
        (gleui-pango-escape name)
        (format #f "<b>~a</b>   <span alpha='65%'>~a</span>"
                (gleui-pango-escape name)
                (gleui-pango-escape desc)))))

(define (desktop-app-format-exec app)
  "Format executable command for APP, wrapping in a terminal emulator if required."
  (let ((exec (desktop-app-exec app))
        (term? (desktop-app-terminal? app)))
    (if term?
        (let ((terminal-bin (or (catch #t (lambda () *terminal*) (lambda _ #f))
                                (catch #t (lambda () (var-get '*terminal*)) (lambda _ #f))
                                "foot")))
          (format #f "~a -e ~a" terminal-bin exec))
        exec)))

(define (desktop-app->candidate-item app)
  "Convert a <desktop-app> record into a (display . search) candidate pair."
  (cons (desktop-app-format-display app)
        (desktop-app-search-str app)))

(define (desktop-file-parse filepath)
  "Parse a .desktop file and return a <desktop-app> record, or #f if invalid/hidden."
  (define (parse-line line)
    (let ((trimmed (desktop-app-comment-strip (string-trim-both line))))
      (cond
       ((or (string-null? trimmed) (string-prefix? "#" trimmed))
        '(comment))
       ((string-prefix? "[" trimmed)
        (cons 'section (string-trim-both trimmed (char-set #\[ #\] #\space))))
       ((string-index trimmed #\=)
        => (lambda (idx)
             (cons 'kv (cons (string-trim-both (substring trimmed 0 idx))
                             (string-trim-both (substring trimmed (+ idx 1)))))))
       (else '(ignored)))))

  (define (build-app-record fields)
    (let ((ref (lambda (k) (assoc-ref fields k))))
      (let ((name      (ref "Name"))
            (exec      (ref "Exec"))
            (type      (or (ref "Type") "Application"))
            (nodisplay (string-ci=? (or (ref "NoDisplay") "") "true"))
            (hidden    (string-ci=? (or (ref "Hidden") "") "true")))
        (if (and name exec
                 (string=? type "Application")
                 (not nodisplay)
                 (not hidden))
            (let* ((clean-cmd (desktop-app-exec-clean exec))
                   (comment   (or (ref "Comment") ""))
                   (generic   (or (ref "GenericName") ""))
                   (icon      (or (ref "Icon") ""))
                   (kw        (or (ref "Keywords") ""))
                   (term?     (string-ci=? (or (ref "Terminal") "") "true"))
                   (search    (string-join (list name generic comment clean-cmd kw icon) " ")))
              (%make-desktop-app name clean-cmd comment generic term? icon kw filepath search))
            #f))))

  (catch #t
    (lambda ()
      (call-with-input-file filepath
        (lambda (port)
          (let loop ((in-desktop-entry? #f)
                     (fields '()))
            (let ((line (read-line port)))
              (if (eof-object? line)
                  (and in-desktop-entry? (build-app-record fields))
                  (let ((parsed (parse-line line)))
                    (case (car parsed)
                      ((section)
                       (if (string=? (cdr parsed) "Desktop Entry")
                           (loop #t fields)
                           (if in-desktop-entry?
                               (build-app-record fields)
                               (loop #f fields))))
                      ((kv)
                       (if (and in-desktop-entry? (not (assoc-ref fields (cadr parsed))))
                           (loop #t (cons (cdr parsed) fields))
                           (loop in-desktop-entry? fields)))
                      (else
                       (loop in-desktop-entry? fields))))))))))
    (lambda _ #f)))

(define-var *desktop-apps-cache* #f
			"Cached list of desktop applications.")

(define-var *desktop-apps-cache-timestamp* 0
			"POSIX timestamp when *desktop-apps-cache* was last updated.")

(define-var *desktop-apps-cache-ttl* 600
			"Time-to-live in seconds for desktop applications cache (default 600s = 10 minutes).")

(define* (desktop-applications-scan #:key (force-rescan? #f))
  "Scan XDG application directories and return a sorted list of desktop application records.
Results are cached and only refreshed if forced or if older than *desktop-apps-cache-ttl* (10 minutes)."
  (let* ((now (current-time))
         (age (- now *desktop-apps-cache-timestamp*)))
    (if (and (not force-rescan?)
             *desktop-apps-cache*
             (>= age 0)
             (< age *desktop-apps-cache-ttl*))
        *desktop-apps-cache*
        (let* ((home (or (getenv "HOME") ""))
               (xdg-home (or (getenv "XDG_DATA_HOME") (string-append home "/.local/share")))
               (xdg-dirs (string-split (or (getenv "XDG_DATA_DIRS") "/usr/local/share:/usr/share") #\:))
               ;; maybe there should be a better way to get flatpak directories?
               (extra-dirs (list (string-append home "/.local/share/flatpak/exports/share")
                                 "/var/lib/flatpak/exports/share"))
               (all-dirs (delete-duplicates (cons xdg-home (append xdg-dirs extra-dirs))))
               (app-dirs (filter file-exists? (map (lambda (d) (string-append d "/applications")) all-dirs)))
               (seen (make-hash-table)))

		  ;; process a desktop file if not already seen
          (define (process-desktop-file dir filename)
			(if (hash-table-ref/default seen filename #f)
				#f
				(begin
                  (hash-table-set! seen filename #t)
                  (desktop-file-parse (string-append dir "/" filename)))))

          ;; scan a single directory and return all valid apps found
          (define (scan-directory dir)
			(let ((files (or (scandir dir (lambda (f) (string-suffix? ".desktop" f))) '())))
              (filter-map (lambda (f) (process-desktop-file dir f)) files)))

          ;; compare apps alphabetically by name
          (define (desktop-app-ci<? a b)
			(string-ci<? (desktop-app-name a) (desktop-app-name b)))

          ;; collect and sort apps across all directories
          (let ((scanned (sort (append-map scan-directory app-dirs) desktop-app-ci<?)))
			(set! *desktop-apps-cache* scanned)
			(set! *desktop-apps-cache-timestamp* now)
			scanned)))))

(define* (gleui-launcher-get-desktop-applications #:key (force-rescan? #f))
  "Return list of desktop application records scanned on demand or from cache."
  (desktop-applications-scan #:force-rescan? force-rescan?))

(define (gleui-launcher-get-desktop-search-strings)
  "Return search strings for desktop applications scanned on demand or from cache."
  (map desktop-app-search-str (desktop-applications-scan)))

(define-command (gleui-launcher-rescan-applications!)
  "Refresh desktop applications cache."
  (desktop-applications-scan #:force-rescan? #t)
  (log-info "gleui-launcher: Desktop applications refreshed."))

(define (gleui-launcher-launch-command cmd)
  "Spawn CMD as a detached background process."
  (when (and (string? cmd) (not (string-null? (string-trim-both cmd))))
    (let ((trimmed (string-trim-both cmd)))
      (log-info "gleui-launcher: Launching ~a" trimmed)
      (catch #t
        (lambda ()
          (system (string-append trimmed " >/dev/null 2>&1 &")))
        (lambda (key . args)
          (log-error "gleui-launcher: Error executing ~a: ~a ~a" trimmed key args))))))

(define* (gleui-launcher-show #:key (theme-overrides '()) (command #f))
  "Show the application launcher prompt and launch the selected application."
  (let* ((apps (desktop-applications-scan))
         (candidates (map desktop-app->candidate-item apps))
         (search-strs (map cdr candidates)))
    (gleui-palette-open!
     candidates
     #:search-strings search-strs
     #:prompt (or (and theme-overrides (assq-ref theme-overrides 'prompt))
                  "Launch: ")
     #:theme-overrides theme-overrides
     #:on-select
     (lambda (res)
       (cond
        ((integer? res)
         (when (and (>= res 0) (< res (length apps)))
           (let* ((app (list-ref apps res))
                  (cmd (desktop-app-format-exec app)))
             (gleui-launcher-launch-command cmd))))
        ((string? res)
         (gleui-launcher-launch-command res))
        ((desktop-app? res)
         (let ((cmd (desktop-app-format-exec res)))
           (gleui-launcher-launch-command cmd)))
        (else #f))))))

(define-command (gleui-launcher-install!)
  "Install gleui-launcher as the application launcher backend."
  (set! *launcher-backend* gleui-launcher-show)
  (set! *launcher-backend-kill* gleui-palette-kill)
  (log-info "gleui-launcher installed as application launcher."))
