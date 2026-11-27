;;; gliver/contrib/ui/gleui/launcher.scm --- Desktop Application Launcher for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module manages XDG .desktop application discovery, caching, and launching
;;; through gleui palette prompts.
;;; It handles:
;;;  - Parsing FreeDesktop Desktop Entries
;;;  - Caching scanned desktop applications
;;;  - Opening the palette launcher prompt and executing selected commands

(define-module (gliver contrib ui gleui launcher)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (gliver core)
  #:use-module (gliver contrib ui gleui base)
  #:use-module (gliver contrib ui gleui palette)
  #:declarative? #f
  #:export (
            *desktop-apps-cache-ttl*
            *desktop-apps-pango-formatter*
            *gleui-launcher-prompt*
            <desktop-app>
            gleui-launcher-rescan-applications!
            gleui-launcher-show
))

(define-var *desktop-apps-cache-ttl* 600
            "Time-to-live in seconds for desktop applications cache
default 600s = 10 minutes, less or equal 0 values disable caching.")
(define-var *desktop-apps-pango-formatter* "<b>~a</b>   <span alpha='65%'>~a</span>"
            "The Pango format the palette will receive.
First parameter is the name of the application, second is the description .")
(define-var *gleui-launcher-prompt* "Launch: "
            "Prompt string displayed before the filter input in the application launcher palette.")

;; parsed desktop entry record
;; NOTE: caching is very important, it consumes little memory from RAM
;;       while improving the speed making the palette shows up instantly
;;  - name: string application display name from Desktop Entry Name
;;  - exec: string cleaned executable command line
;;  - comment: string comment description
;;  - generic-name: string generic category name
;;  - terminal?: boolean indicating whether launch requires a terminal
;;  - icon: string icon name or file path
;;  - keywords: string raw keywords for search indexing
;;  - filepath: string .desktop file path
;;  - search-str: string pre-joined searchable string used
(define-record-type <desktop-app>
  (%make-desktop-app name exec comment generic-name terminal? icon keywords filepath search-str)
  %desktop-app?
  (name         %desktop-app-name)
  (exec         %desktop-app-exec)
  (comment      %desktop-app-comment)
  (generic-name %desktop-app-generic-name)
  (terminal?    %desktop-app-terminal?)
  (icon         %desktop-app-icon)
  (keywords     %desktop-app-keywords)
  (filepath     %desktop-app-filepath)
  (search-str   %desktop-app-search-str))

;; the below helper functions help parse and create <desktop-app> from entries
(define (%desktop-app-comment-strip str)
  "Strip unquoted trailing comments starting with # from STR.
NOTE: some .desktop files have inline comments, if we have the below
      Exec=river -c gliver -t '#test'    # this is a comment
      this function will skip the later comment and preserve what's quoted
Parameters: (STR)
  - STR: String or value (non-strings treated as empty).
Returns trimmed string without trailing comments."
  (if (not (string? str))
      ""
      (let ((str-length (string-length str)))
        (let loop ((index 0) (quote-char #f))
          (if (>= index str-length)
              (string-trim-both str)
              (let ((current-char (string-ref str index)))
                (cond
                 ((and quote-char (char=? current-char quote-char))
                  (loop (1+ index) #f))
                 ((and (not quote-char) (or (char=? current-char #\") (char=? current-char #\x27)))
                  (loop (1+ index) current-char))
                 ((and (not quote-char) (char=? current-char #\#))
                  (string-trim-both (substring str 0 index)))
                 (else
                  (loop (1+ index) quote-char)))))))))

(define (%desktop-app-exec-clean exec-str)
  "Strip XDG field codes and surrounding command quotes from EXEC-STR.
Parameters: (EXEC-STR)
  - EXEC-STR: String or #f raw Desktop Entry Exec command line.
Returns cleaned executable command string."
  (if (string? exec-str)
      ;; we strip comments because there may be some inline comments
      (let* ((without-comment     (%desktop-app-comment-strip exec-str))
             ;; any xdg placeholders like %u , %U, %f are stripped as well
             ;; https://specifications.freedesktop.org/desktop-entry/latest/exec-variables.html
             (without-field-codes (regexp-substitute/global #f "%[a-zA-Z]" without-comment 'pre "" 'post))
             (trimmed-command     (string-trim-both without-field-codes)))
        ;; if the entire command is in outer quotes like "librewolf --new-window"
        ;; but is not an existing single file on disk, strip the outer quotes.
        (if (and (>= (string-length trimmed-command) 2)
                 (string-prefix? "\"" trimmed-command)
                 (string-suffix? "\"" trimmed-command)
                 (not (file-exists? (substring trimmed-command
                                               1 (1- (string-length trimmed-command))))))
            (let ((unquoted-command (substring trimmed-command
                                               1 (1- (string-length trimmed-command)))))
              (string-trim-both unquoted-command))
            trimmed-command))
      ""))

(define (%desktop-app-format-display app)
  "Format <desktop-app> APP into pango string ready for palette.
Parameters: (APP)
  - APP: <desktop-app> record.
Returns Pango markup string."
  (let* ((name         (%desktop-app-name app))
         (generic-name (%desktop-app-generic-name app))
         (comment      (%desktop-app-comment app))
         (description  (cond
                        ((and (string? generic-name) (not (string-null? generic-name))) generic-name)
                        ((and (string? comment) (not (string-null? comment))) comment)
                        (else ""))))
    (if (string-null? description)
        (gleui-pango-escape name)
        ;; guile compiler complains if we don't apply format
        ;; even though it's correct and much easier to read
        (apply format #f *desktop-apps-pango-formatter*
               (list (gleui-pango-escape name)
                     (gleui-pango-escape description))))))

(define (%desktop-app-format-exec app)
  "Format the launch command for APP, wrapping in a terminal if required.
Parameters: (APP)
  - APP: <desktop-app> record.
Returns shell-executable command string."
  (let ((command         (%desktop-app-exec app))
        (needs-terminal? (%desktop-app-terminal? app)))
    (if needs-terminal?
        ;; I believe the -e argument is universal in all terminals
        (format #f "~a -e ~a" *terminal* command)
        command)))

(define (%desktop-file-parse filepath)
  "Parse the XDG desktop entry file at FILEPATH into a <desktop-app> record.
Parameters: (FILEPATH)
  - FILEPATH: String path to a .desktop file.
Returns <desktop-app> record, or #f when invalid, hidden, or unreadable."

  (define (%parse-line line)
    "Classify a single .desktop LINE into comment, section, key-value, or ignored.
Parameters: (LINE)
  - LINE: String raw line read from the .desktop file.
Returns tagged pair '(comment), '(section . NAME), '(kv . (K . V)), or '(ignored)."
    (let ((trimmed-line (%desktop-app-comment-strip (string-trim-both line))))
      (cond
       ;; empty or comment
       ((or (string-null? trimmed-line) (string-prefix? "#" trimmed-line))
        '(comment))
       ;; to parse sections like [Desktop Entry]
       ((string-prefix? "[" trimmed-line)
        (cons 'section (string-trim-both trimmed-line (char-set #\[ #\] #\space))))
       ;; to parse key value pair, like Exec=librewolf
       ((string-index trimmed-line #\=)
        => (lambda (equals-index)
             (cons 'kv (cons (string-trim-both (substring trimmed-line 0 equals-index))
                             (string-trim-both (substring trimmed-line (+ equals-index 1)))))))
       (else '(ignored)))))

  (define (%build-app-record fields)
    "Create a <desktop-app> record from parsed Desktop Entry FIELDS alist.
Parameters: (FIELDS)
  - FIELDS: Alist mapping Desktop Entry key strings to parsed value strings.
Returns <desktop-app> record, or #f when hidden or missing Name/Exec."
    (let ((field-ref (lambda (key) (assoc-ref fields key))))
      (let ((name        (field-ref "Name"))
            (exec        (field-ref "Exec"))
            (type        (or (field-ref "Type") "Application"))
            (no-display? (string-ci=? (or (field-ref "NoDisplay") "") "true"))
            (hidden?     (string-ci=? (or (field-ref "Hidden") "") "true")))
        (if (and name
                 exec
                 (string=? type "Application")
                 (not no-display?)
                 (not hidden?))
            ;; cleanup fields to remove undesired comments
            (let* ((clean-command (%desktop-app-exec-clean exec))
                   (comment       (or (field-ref "Comment") ""))
                   (generic-name  (or (field-ref "GenericName") ""))
                   (icon          (or (field-ref "Icon") ""))
                   (keywords      (or (field-ref "Keywords") ""))
                   (terminal?     (string-ci=? (or (field-ref "Terminal") "") "true"))
                   (search-string (string-join (list name generic-name comment clean-command keywords icon) " ")))
              (%make-desktop-app name clean-command comment generic-name terminal? icon keywords filepath search-string))
            #f))))

  ;; ignore unreadable or malformed .desktop files
  ;; so one bad entry never aborts scanning
  (catch #t
    (lambda ()
      (call-with-input-file filepath
        (lambda (port)
          (let loop ((in-desktop-entry? #f)
                     (fields '()))
            (let ((line (read-line port)))
              (if (eof-object? line)
                  (and in-desktop-entry? (%build-app-record fields))
                  (let ((parsed-line (%parse-line line)))
                    (case (car parsed-line)
                      ((section)
                       (if (string=? (cdr parsed-line) "Desktop Entry")
                           (loop #t fields)
                           ;; once [Desktop Entry] ends stop and build record
                           (if in-desktop-entry?
                               (%build-app-record fields)
                               (loop #f fields))))
                      ((kv)
                       ;; keep only the first value per key within [Desktop Entry]
                       (if (and in-desktop-entry? (not (assoc-ref fields (cadr parsed-line))))
                           (loop #t (cons (cdr parsed-line) fields))
                           (loop in-desktop-entry? fields)))
                      ;; skip comments and blank/unrecognized lines
                      (else
                       (loop in-desktop-entry? fields))))))))))
    (lambda _ #f)))

(define %desktop-apps-cache #f)
(define %desktop-apps-cache-timestamp 0)

(define (%scan-desktop-directory dir)
  "Scan DIR for .desktop files and return (FILENAME . FILEPATH) pairs.
Parameters: (DIR)
  - DIR: String application directory path to scan.
Returns alist of (FILENAME . FILEPATH) pairs."
  (let* ((desktop-file? (lambda (filename)
                          (string-suffix? ".desktop" filename)))
         (desktop-files (or (scandir dir desktop-file?) '())))
    (map (lambda (filename)
           (cons filename (string-append dir "/" filename)))
         desktop-files)))

(define* (%desktop-applications-scan #:key (force-rescan? #f))
  "Scan XDG application directories for desktop applications
Parameters: ([#:FORCE-RESCAN? #f])
  - FORCE-RESCAN?: Boolean bypassing the cached application list when #t.
Returns sorted list of <desktop-app> records."
  (let* ((current-timestamp (current-time))
         (cache-age         (- current-timestamp %desktop-apps-cache-timestamp)))
    ;; caching will be ignored if force-rescan? is #t, or the cache
    ;; time-to-live is <= 0, or the cache-age exceeded time-to-live
    (if (and (not force-rescan?)
             %desktop-apps-cache
             (number? *desktop-apps-cache-ttl*)
             (> *desktop-apps-cache-ttl* 0)
             (>= cache-age 0)
             (< cache-age *desktop-apps-cache-ttl*))
        %desktop-apps-cache
        (let* ((home-dir         (or (getenv "HOME") ""))
               (xdg-data-home    (or (getenv "XDG_DATA_HOME")
                                     (string-append home-dir "/.local/share")))
               (xdg-data-dirs    (string-split (or (getenv "XDG_DATA_DIRS")
                                                   "/usr/local/share:/usr/share") #\:))
               ;; maybe there should be a better way to get flatpak directories?
               (flatpak-dirs     (list (string-append home-dir "/.local/share/flatpak/exports/share")
                                       "/var/lib/flatpak/exports/share"))
               (data-dirs        (delete-duplicates (cons xdg-data-home
                                                          (append xdg-data-dirs
                                                                  flatpak-dirs))))
               (application-dirs (filter file-exists? (map (lambda (data-dir)
                                                             (string-append data-dir "/applications"))
                                                           data-dirs)))
               ;; keep only the first entry per desktop filename
               ;; so higher-priority XDG dirs shadow lower ones
               (desktop-entries  (delete-duplicates (append-map %scan-desktop-directory application-dirs)
                                                    (lambda (a b)
                                                      (string=? (car a) (car b)))))
               (desktop-app-ci<? (lambda (a b) (string-ci<? (%desktop-app-name a)
                                                            (%desktop-app-name b))))
               (sorted-apps      (sort (filter-map (lambda (entry)
                                                     (%desktop-file-parse (cdr entry)))
                                                   desktop-entries)
                                       desktop-app-ci<?)))
          (if (and (number? *desktop-apps-cache-ttl*)
                   (> *desktop-apps-cache-ttl* 0))
              (begin
                (set! %desktop-apps-cache sorted-apps)
                (set! %desktop-apps-cache-timestamp current-timestamp))
              (set! %desktop-apps-cache #f))
          sorted-apps))))

(define-command (gleui-launcher-rescan-applications!)
  "Force a refresh of the cached desktop applications list.
Parameters: None."
  (%desktop-applications-scan #:force-rescan? #t)
  (log-info "gleui-launcher: desktop applications refreshed"))

(define (%launch-command cmd)
  "Spawn shell command CMD as a detached background process.
NOTE: if not spawned as detached, the process will be killed with gliver
Parameters: (CMD)
  - CMD: String or #f shell command line to execute."
  (when (and (string? cmd)
             (not (string-null? (string-trim-both cmd))))
    (let ((trimmed-command (string-trim-both cmd)))
      (log-info "gleui-launcher: launching ~a" trimmed-command)
      (catch #t
        (lambda ()
          (system (string-append trimmed-command " >/dev/null 2>&1 &")))
        (lambda (key . args)
          (log-error "gleui-launcher: error executing ~a: ~a ~a" trimmed-command key args))))))

(define* (gleui-launcher-show #:key (theme-overrides '()))
  "Display the application launcher palette and launch the chosen entry.
Parameters: ([#:THEME-OVERRIDES '()])
  - THEME-OVERRIDES: Alist or #f of palette style overrides."
  (let* ((desktop-apps   (%desktop-applications-scan))
         (candidates     (map %desktop-app-format-display desktop-apps))
         (search-strings (map %desktop-app-search-str desktop-apps)))
    (gleui-palette-open!
     candidates
     #:search-strings search-strings
     #:prompt (or (and theme-overrides (assq-ref theme-overrides 'prompt))
                  *gleui-launcher-prompt*)
     #:theme-overrides theme-overrides
     #:on-select
     (lambda (selection)
       (cond
        ((integer? selection)
         (when (and (>= selection 0)
                    (< selection (length desktop-apps)))
           (let* ((selected-app   (list-ref desktop-apps selection))
                  (launch-command (%desktop-app-format-exec selected-app)))
             (%launch-command launch-command))))
        (else #f))))))
