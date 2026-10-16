;;; gliver/contrib/ui/gleui/toast.scm --- Cairo & Pango Toast Notifications
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver contrib ui gleui toast)
  #:use-module (cairo)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 match)
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-13)
  #:use-module (gliver core)
  #:use-module (gliver deps libc)
  #:use-module (gliver deps color)
  #:use-module (gliver deps librsvg)
  #:use-module (gliver deps pango)
  #:use-module (gliver wayland client)
  #:use-module (gliver wayland gen wayland)
  #:use-module (gliver wayland gen wlr-layer-shell-unstable-v1)
  #:use-module (gliver river connector)
  #:use-module (gliver contrib ui components toast)
  #:use-module (gliver contrib ui gleui base)
  #:declarative? #f
  #:export (
			*gleui-toast-font*
			*gleui-toast-font-size*
			*gleui-toast-bg-color*
			*gleui-toast-fg-color*
			*gleui-toast-border-color*
			*gleui-toast-border-width*
			*gleui-toast-border-radius*
			*gleui-toast-location*
			*gleui-toast-margin*
			*gleui-toast-padding-x*
			*gleui-toast-padding-y*
			*gleui-toast-column-gap*
			*gleui-toast-row-gap*
			*gleui-toast-width*
			*gleui-toast-height*
			*gleui-toast-icon-show*
			*gleui-toast-icon-location*
			*gleui-toast-icon-border-radius*
			*gleui-toast-icon-border-width*
			*gleui-toast-icon-border-color*
			*gleui-toast-icon-size*
			*gleui-toast-icon-theme*
			gleui-toast-load-icon
			gleui-toast-set-margin!
			gleui-toast-get-height
			gleui-toast-state?
			*gleui-toast-instances*
			gleui-toast-get-instance
			gleui-toast-visible?
			gleui-toast-configured?
			gleui-toast-has-markup-tags?
			gleui-toast-markup-ensure
			gleui-toast-span-render
			gleui-toast-column-render
			gleui-toast-message-parse
			gleui-toast-calc-dimensions
			gleui-toast-draw-surface!
			gleui-toast-render!
			gleui-toast-ensure-surface!
			gleui-toast-show
			gleui-toast-kill
			gleui-toast-kill-all!
			gleui-toast-open!
			gleui-toast-cleanup!
			gleui-toast-install!
))

(define-var *gleui-toast-font* *theme-font*
			"Font family used for gleui toast notifications.")
(define-var *gleui-toast-font-size* 12
			"Font size used for gleui toast notifications.")
(define-var *gleui-toast-bg-color* *theme-bg-main*
			"Background color for gleui toast notifications.")
(define-var *gleui-toast-fg-color* *theme-fg-main*
			"Text color for gleui toast notifications.")
(define-var *gleui-toast-border-color* *theme-border-color*
			"Border color for gleui toast notifications.")
(define-var *gleui-toast-border-width* 2
			"Border width in pixels for gleui toast notifications.")
(define-var *gleui-toast-border-radius* *theme-border-radius*
			"Corner border radius in pixels for gleui toast notifications.")
(define-var *gleui-toast-location* 9
			"Gliver position (1-9) for gleui toast notifications.")
(define-var *gleui-toast-margin* 45
			"Margin in pixels from the screen edge for gleui toast notifications.")
(define-var *gleui-toast-padding-x* 22
			"Padding horizontal content in pixels for gleui toast notifications.")
(define-var *gleui-toast-padding-y* 14
			"Padding vertical content in pixels for gleui toast notifications.")
(define-var *gleui-toast-column-gap* 24
			"Horizontal gap pixels for gleui toast notifications.")
(define-var *gleui-toast-row-gap* 6
			"Vertical gap pixels for gleui toast notifications.")
(define-var *gleui-toast-width* #f
			"Fixed width in pixels for gleui toast notifications (#f = dynamic).")
(define-var *gleui-toast-height* #f
			"Fixed height in pixels for gleui toast notifications (#f = dynamic).")
(define-var *gleui-toast-icon-show* #t
			"Whether to display icons in gleui toast notifications.")
(define-var *gleui-toast-icon-location* 'left
			"Icon placement in gleui toast notifications: 'left or 'right.")
(define-var *gleui-toast-icon-border-radius* *theme-border-radius*
			"Border radius in pixels for icons in gleui toast notifications.")
(define-var *gleui-toast-icon-border-width* 0
			"Border width in pixels around icons in gleui toast notifications.")
(define-var *gleui-toast-icon-border-color* *theme-border-color*
			"Border color around icons in gleui toast notifications.")
(define-var *gleui-toast-icon-size* 48
			"Icon dimensions (width and height) in pixels in gleui toast notifications.")
(define-var *gleui-toast-icon-theme* #f
			"Current icon theme name (#f = auto-detect from theme).")

(define-record-type <gleui-toast-state>
  (%make-toast-view-state surface layer-surface shm-slots shm-active-idx configured? visible?
                          width height x y output
                          parsed-content raw-message name timeout-thread-gen
                          theme-overrides render-mutex icon-surface on-close)
  gleui-toast-state?
  (surface             %tvs-surface             %tvs-surface-set!)
  (layer-surface       %tvs-layer-surface       %tvs-layer-surface-set!)
  (shm-slots           %tvs-shm-slots           %tvs-shm-slots-set!)
  (shm-active-idx      %tvs-shm-active-idx      %tvs-shm-active-idx-set!)
  (configured?         %tvs-configured?         %tvs-configured?-set!)
  (visible?            %tvs-visible?            %tvs-visible?-set!)
  (width               %tvs-width               %tvs-width-set!)
  (height              %tvs-height              %tvs-height-set!)
  (x                   %tvs-x                   %tvs-x-set!)
  (y                   %tvs-y                   %tvs-y-set!)
  (output              %tvs-output              %tvs-output-set!)
  (parsed-content      %tvs-parsed-content      %tvs-parsed-content-set!)
  (raw-message         %tvs-raw-message         %tvs-raw-message-set!)
  (name                %tvs-name                %tvs-name-set!)
  (timeout-thread-gen  %tvs-timeout-thread-gen  %tvs-timeout-thread-gen-set!)
  (theme-overrides     %tvs-theme-overrides     %tvs-theme-overrides-set!)
  (render-mutex        %tvs-render-mutex)
  (icon-surface        %tvs-icon-surface        %tvs-icon-surface-set!)
  (on-close            %tvs-on-close            %tvs-on-close-set!))

;; hash table mapping toast name (string) to <gleui-toast-state>
(define *gleui-toast-instances* (make-hash-table))

(define* (gleui-toast-get-instance #:optional (name "default"))
  "Retrieve the toast instance associated with NAME."
  (let ((toast-name (if (or (not name) (not (string? name)) (string-null? name))
                        "default"
                        name)))
    (hash-ref *gleui-toast-instances* toast-name #f)))

(define* (gleui-toast-visible? #:optional (name "default"))
  "Check whether the toast instance with NAME is visible."
  (let ((inst (gleui-toast-get-instance name)))
    (and inst (%tvs-visible? inst))))

(define* (gleui-toast-configured? #:optional (name "default"))
  "Check whether the toast instance with NAME is configured."
  (let ((inst (gleui-toast-get-instance name)))
    (and inst (%tvs-configured? inst))))

(define* (gleui-toast-get-height #:optional (name "default"))
  "Retrieve the rendered height of the toast instance NAME."
  (let ((inst (gleui-toast-get-instance name)))
    (and inst (%tvs-height inst))))

(define (gleui-toast-has-markup-tags? str)
  "Check if STR appears to contain Pango XML markup tags."
  (and (string? str)
       (regexp-exec (make-regexp "<[a-zA-Z/][^>]*>") str)))

(define (gleui-toast-markup-ensure str)
  "Ensure STR is valid Pango markup. If it doesn't contain tags, XML-escape it."
  (if (string? str)
      (if (gleui-toast-has-markup-tags? str)
          str
          (gleui-pango-escape str))
      (gleui-pango-escape (format #f "~a" (or str "")))))

(define (gleui-toast-span-render span)
  "Render a single toast span alist into Pango markup."
  (let* ((raw-text (or (assoc-ref span 'text) ""))
         (text     (gleui-pango-escape (if (string? raw-text) raw-text (format #f "~a" raw-text))))
         (color    (assoc-ref span 'color))
         (weight   (assoc-ref span 'weight))
         (tag-color (if (and color (string? color)) (format #f " color='~a'" color) ""))
         (tag-weight (if (and weight (or (string? weight) (symbol? weight)))
                         (format #f " weight='~a'" weight)
                         ""))
         (open-tag (if (or (not (string-null? tag-color)) (not (string-null? tag-weight)))
                       (format #f "<span~a~a>" tag-color tag-weight)
                       #f)))
    (if open-tag
        (string-append open-tag text "</span>")
        text)))

(define (gleui-toast-column-render col)
  "Render a toast column into a Pango markup string."
  (cond
   ;; column alist
   ((and (list? col) (assoc-ref col 'spans))
    (let ((spans (assoc-ref col 'spans)))
      (if (list? spans)
          (string-join (map gleui-toast-span-render spans) "")
          (gleui-toast-span-render spans))))
   ;; list of spans
   ((and (list? col) (pair? col) (pair? (car col)) (assoc-ref (car col) 'text))
    (string-join (map gleui-toast-span-render col) ""))
   ;; single span
   ((and (list? col) (assoc-ref col 'text))
    (gleui-toast-span-render col))
   ;; plain string
   ((string? col)
    (gleui-toast-markup-ensure col))
   (else
    (gleui-toast-markup-ensure (format #f "~a" col)))))

(define (gleui-toast-message-parse message)
  "Parse MESSAGE into ('text . markup-str) or ('grid . rows-of-col-markups)."
  (cond
   ((string? message)
    (cons 'text (gleui-toast-markup-ensure message)))
   ((and (list? message) (not (null? message)))
    (let ((first-item (car message)))
      (cond
       ;; list of rows where each row is a list of columns
       ((list? first-item)
        (let ((rows (map
                     (lambda (row)
                       (if (list? row)
                           (map gleui-toast-column-render row)
                           (list (gleui-toast-column-render row))))
                     message)))
          (cons 'grid rows)))
       ;; list of strings/items: (list "line 1" "line 2")
       (else
        (let ((lines (map (lambda (x)
                            (if (string? x)
                                (gleui-toast-markup-ensure x)
                                (gleui-toast-markup-ensure (format #f "~a" x))))
                          message)))
          (cons 'text (string-join lines "\n")))))))
   (else
    (cons 'text (gleui-toast-markup-ensure (format #f "~a" (or message "")))))))

(define (%load-svg-surface path)
  (catch #t
    (lambda ()
      (let ((handle (rsvg-handle-new-from-file (string->pointer path) %null-pointer)))
        (if (and handle (not (null-pointer? handle)))
            (let ((dim-bv (make-bytevector 32 0)))
              (rsvg-handle-get-dimensions handle (bytevector->pointer dim-bv))
              (let* ((w (max 1 (bytevector-s32-native-ref dim-bv 0)))
                     (h (max 1 (bytevector-s32-native-ref dim-bv 4)))
                     (surf (cairo-image-surface-create 'argb32 w h))
                     (cr (cairo-create surf))
                     (raw-cr (pango-cairo-context->raw-pointer cr)))
                (cairo-set-operator cr 'clear)
                (cairo-paint cr)
                (cairo-set-operator cr 'over)
                (rsvg-handle-render-cairo handle raw-cr)
                (cairo-destroy cr)
                (pango-g-object-unref handle)
                surf))
            #f)))
    (lambda (k . args)
      (log-error "gleui-toast: Failed to render SVG icon ~a: ~a ~a" path k args)
      #f)))

;; TODO: should probably be placed in notifications.scm
(define (%image-data->cairo-surface data-list)
  "Convert D-Bus raw image-data ('image-data w h rowstride has-alpha bps channels data) to Cairo surface."
  (match data-list
    (('image-data w h rowstride has-alpha bps channels data)
     (catch #t
       (lambda ()
         (let* ((dst-stride (* w 4))
                (dst-bv (make-bytevector (* dst-stride h) 0)))
           (let loop-y ((y 0))
             (when (< y h)
               (let loop-x ((x 0))
                 (when (< x w)
                   (let* ((src-off (+ (* y rowstride) (* x channels)))
                          (dst-off (+ (* y dst-stride) (* x 4)))
                          (r (bytevector-u8-ref data src-off))
                          (g (bytevector-u8-ref data (+ src-off 1)))
                          (b (bytevector-u8-ref data (+ src-off 2)))
                          (a (if (and has-alpha (>= channels 4))
                                 (bytevector-u8-ref data (+ src-off 3))
                                 255)))
                     (let* ((alpha-factor (/ a 255.0))
                            (pm-r (inexact->exact (round (* r alpha-factor))))
                            (pm-g (inexact->exact (round (* g alpha-factor))))
                            (pm-b (inexact->exact (round (* b alpha-factor)))))
                       (bytevector-u8-set! dst-bv dst-off b)
                       (bytevector-u8-set! dst-bv (+ dst-off 1) g)
                       (bytevector-u8-set! dst-bv (+ dst-off 2) r)
                       (bytevector-u8-set! dst-bv (+ dst-off 3) a)))
                   (loop-x (1+ x))))
               (loop-y (1+ y))))
           (cairo-image-surface-create-for-data dst-bv 'argb32 w h dst-stride)))
       (lambda (k . args)
         (log-error "gleui-toast: Failed to convert image-data to surface: ~a ~a" k args)
         #f)))
    (_ #f)))

(define (%detect-icon-theme)
  "Detect current GTK icon theme name from config or settings."
  (or (and (defined? '*gleui-toast-icon-theme*)
           *gleui-toast-icon-theme*)
      (getenv "XDG_ICON_THEME")
      (catch #t
        (lambda ()
          (let* ((home (or (getenv "HOME") ""))
                 (settings-file (string-append home "/.config/gtk-3.0/settings.ini")))
            (if (file-exists? settings-file)
                (call-with-input-file settings-file
                  (lambda (p)
                    (let loop ((line (read-line p)))
                      (cond
                       ((eof-object? line) #f)
                       ((string-prefix? "gtk-icon-theme-name" line)
                        (let ((eq-pos (string-index line #\=)))
                          (if eq-pos
                              (string-trim-both (substring line (1+ eq-pos)))
                              (loop (read-line p)))))
                       (else (loop (read-line p)))))))
                #f)))
        (lambda _ #f))
      "hicolor"))

(define (%icon-search-theme-dirs)
  (let* ((home (or (getenv "HOME") ""))
         (theme-name (%detect-icon-theme))
         (theme-subdirs
          (delete-duplicates
           (filter identity
                   (list (and theme-name (string-append home "/.local/share/icons/" theme-name))
                         (and theme-name (string-append home "/.icons/" theme-name))
                         (and theme-name (string-append "/usr/share/icons/" theme-name))
                         (string-append home "/.local/share/icons")
                         (string-append home "/.icons")
                         "/usr/share/icons/hicolor"
                         "/usr/share/icons/Adwaita"
                         "/usr/share/icons/breeze"
                         "/usr/share/icons/elementary"
                         "/usr/share/icons"
                         "/usr/share/pixmaps")))))
    (filter file-exists? theme-subdirs)))

(define (%icon-name-candidates name)
  (let* ((aliases (cond
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
         (has-sym? (string-suffix? "-symbolic" name))
         (no-sym (if has-sym? (substring name 0 (- (string-length name) (string-length "-symbolic"))) name))
         (with-sym (if has-sym? name (string-append name "-symbolic"))))
    (delete-duplicates
     (append (list name with-sym no-sym)
             aliases))))

;; flattening the pyramid in %find-icon-file
(define-syntax %any
  (syntax-rules ()
    ((_ () body ...)
     (begin body ...))
    ((_ ((var lst) rest ...) body ...)
     (any (lambda (var) (%any (rest ...) body ...)) lst))))

(define (%find-icon-file name)
  "Search standard XDG icon theme directories for an icon matching NAME."
  (let* ((theme-dirs (filter file-exists? (%icon-search-theme-dirs)))
         (categories '("status" "apps" "actions" "devices" "places" "categories" "notifications" "mimetypes" "emblems" ""))
         (sizes '("scalable" "symbolic" "48x48" "64x64" "128x128" "256x256" "32x32" "24x24" "22x22" "16x16" "48" "32" "24" "22" "16" ""))
         (exts '(".svg" ".png" ""))
         ;; a file path was given but not found directly, extract bare icon name
         (clean-name
          (if (or (string-prefix? "/" name) (string-contains name "/"))
              (let* ((base (basename name))
                     (dot (string-rindex base #\.)))
                (if dot (substring base 0 dot) base))
              name))
         (candidates (%icon-name-candidates clean-name)))
    (define (try-file f) (and (string? f) (file-exists? f) f))
    (%any ((tdir theme-dirs)
           (cand candidates)
           (cat  categories)
           (sz   sizes)
           (ext  exts))
      (let* ((both-null? (and (string-null? cat) (string-null? sz)))
             (f1 (if both-null?
                     (string-append tdir "/" cand ext)
                     (string-append tdir "/" cat "/" sz "/" cand ext)))
             (f2 (if both-null?
                     #f
                     (string-append tdir "/" sz "/" cat "/" cand ext))))
        (or (try-file f1) (try-file f2))))))

(define (%load-icon-from-file path)
  (let ((lower (string-downcase path)))
    (cond
     ((string-suffix? ".png" lower)
      (catch #t
        (lambda () (cairo-image-surface-create-from-png path))
        (lambda (k . args)
          (log-error "gleui-toast: Failed to load PNG icon ~a: ~a ~a" path k args)
          #f)))
     ((string-suffix? ".svg" lower)
      (%load-svg-surface path))
     (else
      (or (catch #t (lambda () (cairo-image-surface-create-from-png path)) (lambda _ #f))
          (%load-svg-surface path))))))

(define (gleui-toast-load-icon icon-spec)
  "Load ICON-SPEC (file path, icon name, image-data, or cairo surface) and return a cairo surface, or #f."
  (cond
   ((not icon-spec) #f)
   ((and (string? icon-spec) (string-null? icon-spec)) #f)
   ((and (pair? icon-spec) (eq? (car icon-spec) 'image-data))
	;; TODO: is this going to be used by anything else but notification daemon?
	;; I think I should instead pass cairo surface form notification daemon
	;; instead of handling this specific format that I don't believe other
	;; toast users will use
    (%image-data->cairo-surface icon-spec))
   ((not (string? icon-spec))
    ;; a cairo surface or foreign pointer
    icon-spec)
   ((string? icon-spec)
    (let* ((raw-path (cond
                      ((string-prefix? "file://" icon-spec)
                       (substring icon-spec 7))
                      ((string-prefix? "~/" icon-spec)
                       (string-append (or (getenv "HOME") "") (substring icon-spec 1)))
                      (else icon-spec)))
           (path (cond
                  ((file-exists? raw-path) raw-path)
                  ((and (string-contains raw-path "/scalable/")
                        (file-exists? (string-replace-substring raw-path "/scalable/" "/symbolic/")))
                   (string-replace-substring raw-path "/scalable/" "/symbolic/"))
                  ((and (string-contains raw-path "/symbolic/")
                        (file-exists? (string-replace-substring raw-path "/symbolic/" "/scalable/")))
                   (string-replace-substring raw-path "/symbolic/" "/scalable/"))
                  (else (%find-icon-file raw-path)))))
      (if (and path (file-exists? path))
          (%load-icon-from-file path)
          #f)))
   (else #f)))

(define (gleui-toast-calc-dimensions state)
  "Calculate (values width height) for STATE based on parsed content and font metrics."
  (let* ((dummy-surf (cairo-image-surface-create 'argb32 1 1))
         (cr (cairo-create dummy-surf))
         (overrides (%tvs-theme-overrides state))
         (theme-get (lambda (k def) (gleui-theme-get overrides k def)))
         (custom-w (theme-get 'width '*gleui-toast-width*))
         (custom-h (theme-get 'height '*gleui-toast-height*))
         (target-w (if (and (number? custom-w) (> custom-w 0)) custom-w #f))
         (target-h (if (and (number? custom-h) (> custom-h 0)) custom-h #f))
         (icon-show? (theme-get 'icon-show '*gleui-toast-icon-show*))
         (icon-surf (%tvs-icon-surface state))
         (icon-size (theme-get 'icon-size '*gleui-toast-icon-size*))
         (has-icon? (and icon-show? icon-surf))
         (icon-gap (if has-icon? 16 0))
         (icon-total-w (if has-icon? (+ icon-size icon-gap) 0))
         (font-family (theme-get 'font '*gleui-toast-font*))
         (font-size (theme-get 'font-size '*gleui-toast-font-size*))
         (font-str (format #f "~a ~a" font-family font-size))
         (pad-x (theme-get 'padding-x *gleui-toast-padding-x*))
         (pad-y (theme-get 'padding-y *gleui-toast-padding-y*))
         (col-gap (theme-get 'column-gap *gleui-toast-column-gap*))
         (row-gap (theme-get 'row-gap *gleui-toast-row-gap*))
         (parsed (%tvs-parsed-content state))
         (cur-out (output-current))
         (out-w (if cur-out (output-width cur-out) 1920))
         (max-w (or target-w (max 300 (- out-w 60)))))
    (let-values (((w h)
                  (cond
                   ((and (pair? parsed) (eq? (car parsed) 'text))
                    (let ((text (cdr parsed)))
                      (call-with-values
                          (lambda ()
                            (pango-measure-text cr text
                                                #:font font-str
                                                #:width (if target-w
                                                            (max 10 (- target-w (* 2 pad-x) icon-total-w))
                                                            #f)))
                        (lambda (tw th)
                          (let ((w (or target-w (max 120 (min max-w (+ tw (* 2 pad-x) icon-total-w)))))
                                (h (or target-h (max (+ th (* 2 pad-y))
                                                     (if has-icon? (+ icon-size (* 2 pad-y)) 0)))))
                            (values w h))))))
                   ((and (pair? parsed) (eq? (car parsed) 'grid))
                    (let* ((rows (cdr parsed))
                           (num-cols (if (null? rows) 0 (apply max (map length rows))))
                           (col-widths (make-vector num-cols 0))
                           (max-cell-h 0))
                      (for-each
                       (lambda (row)
                         (let loop-c ((c 0) (cols row))
                           (when (and (< c num-cols) (pair? cols))
                             (let ((cell (car cols)))
                               (call-with-values
                                   (lambda () (pango-measure-text cr cell #:font font-str))
                                 (lambda (cw ch)
                                   (when (> cw (vector-ref col-widths c))
                                     (vector-set! col-widths c cw))
                                   (when (> ch max-cell-h)
                                     (set! max-cell-h ch)))))
                             (loop-c (1+ c) (cdr cols)))))
                       rows)
                      (let* ((total-cols-w (let loop-sum ((i 0) (sum 0))
                                             (if (>= i num-cols)
                                                 sum
                                                 (loop-sum (1+ i) (+ sum (vector-ref col-widths i))))))
                             (total-gaps (if (> num-cols 1) (* (1- num-cols) col-gap) 0))
                             (line-h (+ (max font-size max-cell-h) row-gap))
                             (w (or target-w (max 160 (min max-w (+ total-cols-w total-gaps (* 2 pad-x) icon-total-w)))))
                             (h (or target-h (max (+ (* (length rows) line-h) (* 2 pad-y))
                                                  (if has-icon? (+ icon-size (* 2 pad-y)) 0)))))
                        (values w h))))
                   (else
                    (values (or target-w (+ 200 icon-total-w))
                            (or target-h (max 60 (if has-icon? (+ icon-size (* 2 pad-y)) 0))))))))
      (cairo-destroy cr)
      (cairo-surface-destroy dummy-surf)
      (values w h))))

(define (gleui-toast-draw-surface! cr width height state overrides)
  "Render toast notification content to Cairo context CR."
  (cairo-set-operator cr 'clear)
  (cairo-paint cr)
  (cairo-set-operator cr 'over)

  (let* ((theme-get (lambda (k def) (gleui-theme-get overrides k def)))
         (font-family (theme-get 'font '*gleui-toast-font*))
         (font-size (theme-get 'font-size '*gleui-toast-font-size*))
         (font-str (format #f "~a ~a" font-family font-size))
         (bg-color (theme-get 'background '*gleui-toast-bg-color*))
         (fg-color (theme-get 'foreground '*gleui-toast-fg-color*))
         (border-color (theme-get 'border-color '*gleui-toast-border-color*))
         (border-width (theme-get 'border-width '*gleui-toast-border-width*))
         (border-radius (theme-get 'border-radius '*gleui-toast-border-radius*))
         (pad-x (theme-get 'padding-x *gleui-toast-padding-x*))
         (pad-y (theme-get 'padding-y *gleui-toast-padding-y*))
         (col-gap (theme-get 'column-gap *gleui-toast-column-gap*))
         (row-gap (theme-get 'row-gap *gleui-toast-row-gap*))
         (parsed (%tvs-parsed-content state))
         (icon-show? (theme-get 'icon-show '*gleui-toast-icon-show*))
         (icon-loc (theme-get 'icon-location '*gleui-toast-icon-location*))
         (icon-border-radius (theme-get 'icon-border-radius '*gleui-toast-icon-border-radius*))
         (icon-border-width (theme-get 'icon-border-width '*gleui-toast-icon-border-width*))
         (icon-border-color (theme-get 'icon-border-color '*gleui-toast-icon-border-color*))
         (icon-size (theme-get 'icon-size '*gleui-toast-icon-size*))
         (icon-surf (%tvs-icon-surface state))
         (has-icon? (and icon-show? icon-surf))
         (icon-gap (if has-icon? 16 0))
         (icon-total-w (if has-icon? (+ icon-size icon-gap) 0))
         (is-right? (eq? icon-loc 'right))
         (icon-x (if is-right?
                     (- width pad-x icon-size)
                     pad-x))
         (icon-y pad-y)
         (text-x (if has-icon?
                     (if is-right? pad-x (+ pad-x icon-total-w))
                     pad-x))
         (text-y pad-y)
         (text-w (max 10 (- width (* 2 pad-x) icon-total-w))))

    ;; background surface
    (let ((bg-rgba (parse-hex-color-rgba bg-color)))
      (apply (lambda (r g b a) (cairo-set-source-rgba cr r g b a)) bg-rgba)
      (gleui-draw-rounded-rect cr 0 0 width height border-radius)
      (cairo-fill cr))

    ;; border
    (when (and (number? border-width) (> border-width 0))
      (let ((b-rgba (parse-hex-color-rgba border-color))
            (half-w (/ border-width 2.0)))
        (apply (lambda (r g b a) (cairo-set-source-rgba cr r g b a)) b-rgba)
        (cairo-set-line-width cr border-width)
        (gleui-draw-rounded-rect cr half-w half-w (- width border-width) (- height border-width) border-radius)
        (cairo-stroke cr)))

    ;; icon
    (when has-icon?
      (let* ((sw (exact->inexact (cairo-image-surface-get-width icon-surf)))
             (sh (exact->inexact (cairo-image-surface-get-height icon-surf)))
             (scale (/ (exact->inexact icon-size) (max 1.0 (max sw sh))))
             (scaled-w (* sw scale))
             (scaled-h (* sh scale))
             (draw-x (+ icon-x (/ (- icon-size scaled-w) 2.0)))
             (draw-y (+ icon-y (/ (- icon-size scaled-h) 2.0))))
        (cairo-save cr)
        (when (and (number? icon-border-radius) (> icon-border-radius 0))
          (gleui-draw-rounded-rect cr icon-x icon-y icon-size icon-size icon-border-radius)
          (cairo-clip cr))
        (cairo-translate cr draw-x draw-y)
        (cairo-scale cr scale scale)
        (cairo-set-source-surface cr icon-surf 0.0 0.0)
        (cairo-paint cr)
        (cairo-restore cr)
        (when (and (number? icon-border-width) (> icon-border-width 0))
          (let* ((half-bw (/ (exact->inexact icon-border-width) 2.0))
                 (b-rgba (parse-hex-color-rgba icon-border-color)))
            (apply (lambda (r g b a) (cairo-set-source-rgba cr r g b a)) b-rgba)
            (cairo-set-line-width cr icon-border-width)
            (gleui-draw-rounded-rect cr
                                    (+ icon-x half-bw)
                                    (+ icon-y half-bw)
                                    (- icon-size icon-border-width)
                                    (- icon-size icon-border-width)
                                    icon-border-radius)
            (cairo-stroke cr)))))

    ;; content
    (let ((fg-rgba (parse-hex-color-rgba fg-color)))
      (apply (lambda (r g b a) (cairo-set-source-rgba cr r g b a)) fg-rgba))

    (cond
     ;; text content
     ((and (pair? parsed) (eq? (car parsed) 'text))
      (let ((text (cdr parsed)))
        (cairo-move-to cr text-x text-y)
        (pango-draw-text cr text #:font font-str #:width text-w)))

     ;; grid content
     ((and (pair? parsed) (eq? (car parsed) 'grid))
      (let* ((rows (cdr parsed))
             (num-cols (if (null? rows) 0 (apply max (map length rows))))
             (col-widths (make-vector num-cols 0))
             (max-cell-h 0))

		;; measure every cell in a row and update column widths and max height
		(define (%measure-row-cells! row)
          (for-each
           (lambda (col-idx cell)
             (call-with-values (lambda () (pango-measure-text cr cell #:font font-str))
               (lambda (cw ch)
                 (when (> cw (vector-ref col-widths col-idx))
                   (vector-set! col-widths col-idx cw))
                 (when (> ch max-cell-h)
                   (set! max-cell-h ch)))))
           (iota (min num-cols (length row)))
           row))

		;; calculate X coordinate offsets for each column as a vector for O(1) lookup
		(define (%compute-column-offsets)
          (let ((offsets (make-vector num-cols 0)))
			(let loop ((c 0) (acc-x 0))
              (when (< c num-cols)
				(vector-set! offsets c acc-x)
				(loop (1+ c) (+ acc-x (vector-ref col-widths c) col-gap))))
			offsets))

		;; render a single row of cells at row-y
		(define (%draw-grid-row! row row-y col-offsets)
          (for-each
           (lambda (col-idx cell)
             (let ((col-x (+ text-x (vector-ref col-offsets col-idx))))
               (cairo-move-to cr col-x row-y)
               (pango-draw-text cr cell #:font font-str)))
           (iota (min num-cols (length row)))
           row))

		;; measure dimensions
		(for-each %measure-row-cells! rows)

		;; render grid
		(let* ((line-h (+ (max font-size max-cell-h) row-gap))
               (col-offsets (%compute-column-offsets)))
          (for-each
           (lambda (row-idx row)
             (%draw-grid-row! row (+ pad-y (* row-idx line-h)) col-offsets))
           (iota (length rows))
           rows)))))))

(define (gleui-toast-render! state)
  "Render the toast view into persistent double-buffered shared memory and commit."
  (when (and state
             (%tvs-visible? state)
             (%tvs-configured? state)
             *wl-display*
             (pointer? *wl-compositor*)
             (not (null-pointer? *wl-compositor*))
             (pointer? *wl-shm*)
             (not (null-pointer? *wl-shm*)))
    (with-mutex (%tvs-render-mutex state)
      (let* ((surface (%tvs-surface state))
             (layer-surf (%tvs-layer-surface state))
             (w (%tvs-width state))
             (h (%tvs-height state))
             (overrides (%tvs-theme-overrides state)))
        (when (and surface (pointer? surface) (not (null-pointer? surface)) (> w 0) (> h 0))
          (let ((valid-slots (gleui-shm-slots-ensure! (%tvs-shm-slots state) w h)))
            (%tvs-shm-slots-set! state valid-slots)
            (when (and (pair? valid-slots) (car valid-slots) (cdr valid-slots))
              (let* ((next-idx (if (zero? (%tvs-shm-active-idx state)) 1 0))
                     (curr-slot (if (zero? next-idx) (car valid-slots) (cdr valid-slots)))
                     (bv (gleui-shm-slot-bv curr-slot))
                     (stride (gleui-shm-slot-stride curr-slot))
                     (wl-buf (gleui-shm-slot-buffer curr-slot)))
                (%tvs-shm-active-idx-set! state next-idx)
                (let* ((dst-surf (cairo-image-surface-create-for-data bv 'argb32 w h stride))
                       (cr (cairo-create dst-surf)))
                  (gleui-toast-draw-surface! cr w h state overrides)
                  (cairo-surface-flush dst-surf)
                  (cairo-destroy cr)
                  (cairo-surface-destroy dst-surf))
                (when (and layer-surf (pointer? layer-surf) (not (null-pointer? layer-surf)))
                  (zwlr-layer-surface-v1-set-size layer-surf w h))
                (gleui-buffer-commit! surface wl-buf w h)))))))))

(define (gleui-toast-ensure-surface! state)
  "Ensure Wayland surface and layer-surface are created and configured for toast STATE."
  (when (and state
             (or (not (%tvs-surface state))
                 (null-pointer? (%tvs-surface state))
                 (not (%tvs-layer-surface state))
                 (null-pointer? (%tvs-layer-surface state)))
             *connected*
             (pointer? *wl-compositor*)
             (not (null-pointer? *wl-compositor*))
             (pointer? *zwlr-layer-shell*)
             (not (null-pointer? *zwlr-layer-shell*)))
    (let* ((loc (or (assq-ref (%tvs-theme-overrides state) 'location)
                    (catch #t (lambda () (var-get '*gleui-toast-location*)) (lambda _ 2))
                    2))
           (anchor (gleui-numpad->anchor loc))
           (margin (or (assq-ref (%tvs-theme-overrides state) 'margin)
                       (catch #t (lambda () (var-get '*gleui-toast-margin*)) (lambda _ 35))
                       35))
           (mt (or (assq-ref (%tvs-theme-overrides state) 'margin-top) margin))
           (mr (or (assq-ref (%tvs-theme-overrides state) 'margin-right) margin))
           (mb (or (assq-ref (%tvs-theme-overrides state) 'margin-bottom) margin))
           (ml (or (assq-ref (%tvs-theme-overrides state) 'margin-left) margin))
           (w (%tvs-width state))
           (h (%tvs-height state))
           (name (%tvs-name state)))
      (gleui-layer-surface-create
       #:output (%tvs-output state)
       #:namespace (string-append "gliver-toast-" name)
       #:layer ZWLR_LAYER_SHELL_V1_LAYER_OVERLAY
       #:anchor anchor
       #:margin-top mt
       #:margin-right mr
       #:margin-bottom mb
       #:margin-left ml
       #:exclusive-zone -1
       #:keyboard-interactivity ZWLR_LAYER_SURFACE_V1_KEYBOARD_INTERACTIVITY_NONE
       #:width w
       #:height h
       #:on-created (lambda (surf layer-surf)
                      (%tvs-surface-set! state surf)
                      (%tvs-layer-surface-set! state layer-surf)
                      (%tvs-configured?-set! state #f))
       #:on-configure (lambda (cw ch)
                        (log-debug "gleui-toast: Layer surface configured ~ax~a (~a)" cw ch name)
                        (%tvs-configured?-set! state #t)
                        (when (%tvs-visible? state)
                          (gleui-toast-render! state)))
       #:on-close (lambda ()
                    (log-info "gleui-toast: Layer surface closed (~a)" name)
                    (gleui-toast-kill #:name name))))))

(define* (gleui-toast-set-margin! name
                                  #:key (margin-top #f)
                                        (margin-right #f)
                                        (margin-bottom #f)
                                        (margin-left #f))
  "Update margins for toast instance NAME."
  (let ((state (if (string? name)
                   (hash-ref *gleui-toast-instances* name #f)
                   name)))
    (when (and state (%tvs-visible? state))
      (let* ((layer-surf (%tvs-layer-surface state))
             (surf (%tvs-surface state))
             (overrides (%tvs-theme-overrides state))
             (base-margin (or (assq-ref overrides 'margin)
                              (catch #t (lambda () (var-get '*gleui-toast-margin*)) (lambda _ 35))
                              35))
             (mt (or margin-top (assq-ref overrides 'margin-top) base-margin))
             (mr (or margin-right (assq-ref overrides 'margin-right) base-margin))
             (mb (or margin-bottom (assq-ref overrides 'margin-bottom) base-margin))
             (ml (or margin-left (assq-ref overrides 'margin-left) base-margin)))
        (when (and layer-surf (pointer? layer-surf) (not (null-pointer? layer-surf))
                   surf (pointer? surf) (not (null-pointer? surf)))
          (zwlr-layer-surface-v1-set-margin layer-surf mt mr mb ml)
          (wl-surface-commit surf)
		  (wl-display-flush *wl-display*))))))

(define* (gleui-toast-show message
                           #:key (timeout 0)
                           (name "default")
                           (icon #f)
                           (theme-overrides '())
                           (on-close #f)
                           (output #f))
  "Display toast notification with MESSAGE using native Cairo & Pango.
MESSAGE can be a string, a list of strings, or a list of structured rows/columns.
If TIMEOUT > 0, auto-hides after TIMEOUT (seconds or ms).
NAME specifies the unique toast instance name to support multiple toast instances.
ICON can be a file path, icon name, or existing Cairo image surface.
ON-CLOSE is an optional zero-argument procedure called when this toast is closed.
OUTPUT specifies the target output to display the toast on (defaults to current output)."
  (let* ((toast-name (if (or (not name) (not (string? name)) (string-null? name))
                         "default"
                         name)))

	;; kill previous toast if one exists
    (when (hash-ref *gleui-toast-instances* toast-name #f)
      (gleui-toast-kill #:name toast-name))

    (let* ((state (%make-toast-view-state #f #f #f 0 #f #f 0 0 0 0 #f #f #f toast-name 0 '() (make-mutex) #f #f))
           (parsed (gleui-toast-message-parse message)))
      (hash-set! *gleui-toast-instances* toast-name state)

      (let* ((cur-gen (1+ (%tvs-timeout-thread-gen state)))
             (target-out (or output
                             (and (list? theme-overrides) (assq-ref theme-overrides 'output))
                             (output-current))))
        (%tvs-output-set! state target-out)
        (%tvs-timeout-thread-gen-set! state cur-gen)
        (%tvs-parsed-content-set! state parsed)
        (%tvs-raw-message-set! state message)
        (%tvs-name-set! state toast-name)
        (%tvs-theme-overrides-set! state theme-overrides)
        (%tvs-on-close-set! state on-close)
        (let* ((effective-icon (or icon (and (list? theme-overrides) (assq-ref theme-overrides 'icon))))
               (old-surf (%tvs-icon-surface state))
               (new-surf (gleui-toast-load-icon effective-icon)))
          (when (and old-surf (not (eq? old-surf new-surf)))
            (catch #t (lambda () (cairo-surface-destroy old-surf)) (lambda _ #f)))
          (%tvs-icon-surface-set! state new-surf))
        (%tvs-visible?-set! state #t)

        ;; calculate required width and height
        (call-with-values
            (lambda () (gleui-toast-calc-dimensions state))
          (lambda (w h)
            (%tvs-width-set! state w)
            (%tvs-height-set! state h)))

        ;; ensure surface is created and rendered
        (gleui-toast-ensure-surface! state)
        (when (and (%tvs-configured? state) (%tvs-visible? state))
          (gleui-toast-render! state))

        ;; set up auto-dismiss timer if timeout > 0
        (when (and (number? timeout) (> timeout 0))
          (let* ((t-num (exact->inexact timeout))
                 (ms (if (> t-num 50.0) t-num (* t-num 1000.0)))
                 (delay-usec (inexact->exact (round (* ms 1000.0)))))
            (call-with-new-thread
             (lambda ()
               (usleep delay-usec)
               (when (and (= (%tvs-timeout-thread-gen state) cur-gen)
                          (%tvs-visible? state))
                 (gleui-toast-kill #:name toast-name))))))))))

(define* (gleui-toast-kill #:key (name "default"))
  "Dismiss the toast notification instance matching NAME."
  (let* ((toast-name (if (or (not name) (not (string? name)) (string-null? name))
                         "default"
                         name))
         (state (hash-ref *gleui-toast-instances* toast-name #f)))
    (when state
      (when (%tvs-visible? state)
        (%tvs-timeout-thread-gen-set! state (1+ (%tvs-timeout-thread-gen state)))
        (%tvs-visible?-set! state #f)
        (%tvs-configured?-set! state #f)
        (%tvs-parsed-content-set! state #f)
        (%tvs-raw-message-set! state #f)
        (let ((cb (%tvs-on-close state)))
          (%tvs-on-close-set! state #f)
          (when (procedure? cb)
            (catch #t cb (lambda (k . args) (log-error "gleui-toast: on-close callback error: ~a ~a" k args)))))
        (let ((icon-surf (%tvs-icon-surface state)))
          (when icon-surf
            (catch #t (lambda () (cairo-surface-destroy icon-surf)) (lambda _ #f))
            (%tvs-icon-surface-set! state #f)))
        (let ((layer-surf (%tvs-layer-surface state))
              (surface (%tvs-surface state)))
          (gleui-layer-surface-destroy! surface layer-surf)
          (%tvs-layer-surface-set! state #f)
          (%tvs-surface-set! state #f)))
      (let ((t-slots (%tvs-shm-slots state)))
        (when (pair? t-slots)
          (when (car t-slots) (gleui-shm-slot-destroy (car t-slots)))
          (when (cdr t-slots) (gleui-shm-slot-destroy (cdr t-slots)))
          (%tvs-shm-slots-set! state #f)))
      (hash-remove! *gleui-toast-instances* toast-name))))

(define (gleui-toast-kill-all!)
  "Dismiss all active toast notification instances."
  (let ((names (hash-map->list (lambda (name state) name) *gleui-toast-instances*)))
    (for-each
     (lambda (name)
       (gleui-toast-kill #:name name))
     names)))

(define gleui-toast-open! gleui-toast-show)

(define (gleui-toast-cleanup!)
  "Clean up all toast view resources on unbind/shutdown."
  (gleui-toast-kill-all!)
  (hash-clear! *gleui-toast-instances*))

(gliver-hook-add! *gliver-globals-unbind-hook* 'gleui-toast-cleanup!)

(define-command (gleui-toast-install!)
  "Install gleui-toast as the notification/message backend."
  (var-set! *toast-backend* gleui-toast-show)
  (var-set! *toast-backend-kill* gleui-toast-kill)
  (log-info "gleui-toast installed as message backend."))
