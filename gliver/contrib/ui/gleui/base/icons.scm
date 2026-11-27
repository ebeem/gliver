;;; gliver/contrib/ui/gleui/base/icons.scm --- XDG Icon Discovery
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module handles XDG icon theme lookup and PNG/SVG Cairo image surface loading
;;; It handles:
;;;  - Alias/symbolic icon resolution across XDG icon directories
;;;  - PNG and SVG (via librsvg) loading into Cairo ARGB32 image surfaces

(define-module (gliver contrib ui gleui base icons)
  #:use-module (cairo)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-11)
  #:use-module (gliver core)
  #:use-module (gliver deps librsvg)
  #:use-module (gliver deps pango)
  #:use-module (gliver contrib ui gleui base draw)
  #:declarative? #f
  #:export (
            gleui-load-icon
))

(define (%expand-path path)
  "Expand an icon PATH (or name) to a valid absolute path or icon theme name.
Loading an icon require absolute path to be passed, so file:// and ~/ aren't allowed.
Parameters: (PATH)
  - PATH: String filesystem path or value passed through unchanged.
Returns expanded filesystem path string."
  (cond
   ;; not a string is actually an error, guard check so string-prefix? doesn't complain
   ((not (string? path)) path)
   ;; strip the file:// part if it exists
   ((string-prefix? "file://" path)
    (substring path 7))
   ;; replace ~/ with absolute home path
   ((string-prefix? "~/" path)
    (string-append (or (getenv "HOME") "") (substring path 1)))
   ;; some icons are passed as names, like "emacs", this is to return as is
   (else path)))

(define (%load-svg-surface path)
  "Load the SVG icon file at PATH into a new Cairo ARGB32 image surface.
NOTE: this only handles absolute paths (no icon theme name) and svg format.
Parameters: (PATH)
  - PATH: String filesystem path to an SVG image file.
Returns Cairo ARGB32 surface pointer, or #f on error."
  (catch #t
    (lambda ()
      (and-let* ((handle (rsvg-handle-new-from-file path))
                 ((valid-pointer? handle)))
        (let-values (((raw-w raw-h) (rsvg-handle-get-dimensions handle)))
          (let* ((w      (max 1 raw-w))
                 (h      (max 1 raw-h))
                 (surf   (cairo-image-surface-create 'argb32 w h))
                 (cr     (cairo-create surf))
                 (raw-cr (pango-cairo-context->raw-pointer cr)))
            (dynamic-wind
              (lambda () #f)
              (lambda ()
                (gleui-cairo-clear! cr)
                (rsvg-handle-render-cairo handle raw-cr))
              (lambda ()
                (cairo-destroy cr)
                (pango-g-object-unref handle)))
            surf))))
    (lambda (k . args)
      (log-error "gleui: Failed to render SVG icon ~a: ~a ~a" path k args)
      #f)))

(define* (%detect-icon-theme #:optional (preferred-theme #f))
  "Detect active icon theme.
Parameters: ([PREFERRED-THEME #f])
  - PREFERRED-THEME: String or #f (configured icon theme by default).
Returns icon theme name string."
  (or preferred-theme
      ;; will be much easier if there's an env variable for icon theme
      ;; only way to get gtk icon theme without gsettings is to read gtk-3 settings
      (catch #t
        (lambda ()
          (and-let* ((home (getenv "HOME"))
                     (settings-file (string-append home "/.config/gtk-3.0/settings.ini"))
                     ((file-exists? settings-file)))
            (call-with-input-file settings-file
              (lambda (p)
                (let loop ((line (read-line p)))
                  (cond
                   ((eof-object? line) #f)
                   ;; read each line until prefix is gtk-icon-theme-name
                   ((string-prefix? "gtk-icon-theme-name" line)
                    ;; get everything trimmed after the '=' symbol
                    (let ((eq-pos (string-index line #\=)))
                      (if eq-pos
                          (string-trim-both (substring line (1+ eq-pos)))
                          (loop (read-line p)))))
                   (else (loop (read-line p)))))))))
        (lambda _ #f))
      ;; TODO: shall this also fallback to qt configuration?
      "hicolor"))

(define* (%icon-search-dirs #:optional (preferred-theme #f))
  "Return existing XDG icon theme directories ordered by lookup priority for PREFERRED-THEME.
Parameters: ([PREFERRED-THEME #f])
  - PREFERRED-THEME: String or #f optional icon theme override.
Returns list of existing directory path strings."
  (let* ((home (or (getenv "HOME") ""))
         (theme-name (%detect-icon-theme preferred-theme))
         (dirs
          (delete-duplicates
           (filter identity
                   ;; the priority is configured theme first
                   ;; the theme may be installed in many default directories
                   ;; TODO: check how guix handle this, could be in a store
                   (list (and theme-name (string-append home "/.local/share/icons/" theme-name))
                         (and theme-name (string-append home "/.icons/" theme-name))
                         (and theme-name (string-append "/usr/share/icons/" theme-name))
                         ;; apps and users can place icons in these directories
                         ;; based on freedesktop specifications
                         (string-append home "/.local/share/icons")
                         (string-append home "/.icons")
                         "/usr/share/icons/hicolor"
                         "/usr/share/icons/Adwaita"
                         "/usr/share/icons/breeze"
                         "/usr/share/icons/elementary"
                         "/usr/share/icons"
                         "/usr/share/pixmaps")))))
    (filter file-exists? dirs)))

(define (%icon-name-candidates name)
  "Expand icon NAME into ordered symbolic, non-symbolic, and alias candidates.
Parameters: (NAME)
  - NAME: String icon name.
Returns unique list of candidate icon name strings."
  (let* ((aliases (cond
                   ;; these aliases are used by some apps and scripts where their icons
                   ;; are not defined and not standard freedesktop icon names
                   ((string=? name "info") '("dialog-information"))
                   ((string=? name "information") '("dialog-information"))
                   ((string=? name "warning") '("dialog-warning"))
                   ((string=? name "error") '("dialog-error"))
                   ((string=? name "question") '("dialog-question"))
                   ((string=? name "battery") '("battery-level-100-symbolic" "battery-full-symbolic"))
                   ((string=? name "battery-low") '("battery-level-20-symbolic" "battery-low-symbolic"))
                   ((string=? name "battery-caution") '("battery-level-10-symbolic" "battery-caution-symbolic"))
                   ((string=? name "battery-empty") '("battery-level-0-symbolic" "battery-empty-symbolic"))
                   (else '())))
         ;; we try both symbolic and non-symbolic icon name
         ;; some apps or scripts may pass any of those types
         (has-sym? (string-suffix? "-symbolic" name))
         (no-sym   (if has-sym?
                       (substring name 0 (- (string-length name) (string-length "-symbolic")))
                       name))
         (with-sym (if has-sym? name (string-append name "-symbolic"))))
    (delete-duplicates (append (list name with-sym no-sym) aliases))))

;; helper to avoid the pyramid of doom caused by checking icon directories
(define-syntax %file-path-any
  (syntax-rules ()
    ((_ () body ...)
     (begin body ...))
    ((_ ((var lst) rest ...) body ...)
     (any (lambda (var) (%file-path-any (rest ...) body ...)) lst))))

(define* (%find-icon-file name #:optional (preferred-theme #f))
  "Search XDG icon theme directories for an icon file matching NAME.
Parameters: (NAME [PREFERRED-THEME #f])
  - NAME: String icon name or relative path to resolve.
  - PREFERRED-THEME: String or #f optional preferred icon theme name.
Returns existing icon file path string, or #f if not found."
  (let* ((theme-dirs (%icon-search-dirs preferred-theme))
         ;; list of various sub directories that need to be checked for icons
         ;; examples of icons paths listed below
         ;; /usr/share/icons/hicolor/48x48/apps/emacs.png
         ;; /usr/share/icons/breeze/actions/16/Info-amarok-symbolic.svg
         (categories '("status" "apps" "actions" "devices" "places"
                       "categories" "notifications" "mimetypes" "emblems" ""))
         (sizes      '("scalable" "symbolic" "48x48" "64x64" "128x128" "256x256"
                       "32x32" "24x24" "22x22" "16x16" "48" "32" "24" "22" "16" ""))
         (exts       '(".svg" ".png" ""))
         ;; extract just icon name, drop extension and path
         (clean-name (if (or (string-prefix? "/" name)
                             (string-contains name "/"))
                         (let* ((base (basename name))
                                (dot  (string-rindex base #\.)))
                           (if dot
                               (substring base 0 dot)
                               base))
                         name))
         (candidates (%icon-name-candidates clean-name)))

    (define (%try-file f)
      "Check whether candidate path F exists on disk.
Parameters: (F)
  - F: String candidate file path.
Returns file F when existing on disk, #f otherwise."
      (and (string? f)
           (file-exists? f)
           f))

    ;; iterate through theme-dirs -> candidates -> categories -> sizes -> exts
    (%file-path-any ((tdir theme-dirs)
                     (cand candidates)
                     (cat  categories)
                     (sz   sizes)
                     (ext  exts))
                    (let* ((f1 (string-append tdir "/" cat "/" sz "/" cand ext))
                           (f2 (string-append tdir "/" sz "/" cat "/" cand ext)))
                      (or (%try-file f1)
                          ;; avoid checking f2 again if it's same as f1
                          ;; this will happen if both sz and ext are empty
                          (and (not (string=? f1 f2))
                               (%try-file f2)))))))

(define (%load-icon-from-file path)
  "Load the SVG or PNG icon file at PATH into a new Cairo ARGB32 image surface.
Parameters: (PATH)
  - PATH: String filesystem path to an SVG or PNG image file.
Returns Cairo ARGB32 surface pointer, or #f on error."
  (let ((lower (string-downcase path)))
    (cond
     ((string-suffix? ".png" lower)
      ;; for png files, this is supported by cairo
      (catch #t
        (lambda () (cairo-image-surface-create-from-png path))
        (lambda (k . args)
          (log-error "gleui: Failed to load PNG icon ~a: ~a ~a" path k args)
          #f)))
     ;; for svg files, use librsvg
     ((string-suffix? ".svg" lower)
      (%load-svg-surface path))
     (else
      ;; for others, try to load as png then svg if successful
      (or (catch #t (lambda ()
                      (cairo-image-surface-create-from-png path))
            (lambda _ #f))
          (%load-svg-surface path))))))

(define* (gleui-load-icon icon-spec #:optional (preferred-theme #f))
  "Load ICON-SPEC into a Cairo image surface using.
Parameters: (ICON-SPEC [PREFERRED-THEME #f])
  - ICON-SPEC: String (file path, file:// URI, or icon name).
  - PREFERRED-THEME: String or #f optional icon theme name.
Returns Cairo ARGB32 surface pointer, or #f on error."
  (cond
   ((not icon-spec) #f)
   ((and (string? icon-spec) (string-null? icon-spec)) #f)
   ;; this assumes a cairo image surface is passed, return as is
   ((not (string? icon-spec)) icon-spec)
   (else
    ;; if we have an icon string, expand it to get full path if needed
    ;; some icons may already be a file path, expand will keep them as they are
    (let* ((raw-path (%expand-path icon-spec))
           (path     (cond
                      ;; if expanded path exists, return it
                      ((file-exists? raw-path) raw-path)
                      ;; if icon was a path with scalable, try to swap with symbolic
                      ((and (string-contains raw-path "/scalable/")
                            (file-exists? (string-replace-substring raw-path "/scalable/" "/symbolic/")))
                       (string-replace-substring raw-path "/scalable/" "/symbolic/"))
                      ;; if icon was a path with symbolic, try to swap with scalable
                      ((and (string-contains raw-path "/symbolic/")
                            (file-exists? (string-replace-substring raw-path "/symbolic/" "/scalable/")))
                       (string-replace-substring raw-path "/symbolic/" "/scalable/"))
                      ;; otherwise, try to bruteforce with %find-icon-file
                      (else (%find-icon-file raw-path preferred-theme)))))
      (and path (file-exists? path)
           (%load-icon-from-file path))))))
