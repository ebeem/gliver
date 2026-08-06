(define-module (gliver contrib ui integrations rofi)
  #:use-module (gliver contrib ui palette)
  #:use-module (gliver contrib ui toast)
  #:use-module (gliver core)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:export (
			%rofi-shell-quote
			%rofi-%rofi-format-rasi-value
			%rofi-%rofi-format-rasi-rule
			%rofi-%rofi-format-rasi-block
			%rofi-format-rasi
			*numpad->rofi-mapping*
			numpad->rofi-location
			%rofi-pango-escape
			%rofi-item-formatter
			%rofi-make-dmenu-options
			make-rofi-backend
			*rofi-palette-command*
			rofi-palette-show
			rofi-install-dmenu-launcher!
			rofi-install-app-launcher!
			rofi-install-toast!
			rofi-install-all!
))

(define (%rofi-shell-quote val)
  "Quote a value so it can be safely passed in a shell command."
  (let ((str (if (string? val) val (format #f "~a" val))))
    (string-append "'" (string-replace-substring str "'" "'\\''") "'")))

(define (%rofi-%rofi-format-rasi-value val)
  "Format a single value into a rasi compatible string fragment."
  (cond
   ((symbol? val) (symbol->string val))
   ((number? val) (number->string val))
   ((string? val) val)
   ((pair? val)
    (cond
     ((and (eq? (car val) 'var) (= (length val) 2))
      (format #f "var(~a)" (cadr val)))
     ((and (memq (car val) '(q quote string)) (= (length val) 2))
      (format #f "~s" (cadr val)))
     (else
      (string-join (map %rofi-%rofi-format-rasi-value val) " "))))
   (else (format #f "~a" val))))

(define (%rofi-%rofi-format-rasi-rule rule)
  "Format a (property value) pair as a rasi style rule."
  (let ((prop (car rule))
        (val (cadr rule)))
    (format #f "    ~a: ~a;" prop (%rofi-%rofi-format-rasi-value val))))

(define (%rofi-%rofi-format-rasi-block block)
  "Format a (selector (prop val) ...) block into a rasi css-like section."
  (let ((selector (car block))
        (rules (filter-map (lambda (r) (if (cadr r) (%rofi-%rofi-format-rasi-rule r) #f)) (cdr block))))
    (let ((sel-str (cond
                    ((symbol? selector) (symbol->string selector))
                    ((string? selector) selector)
                    ((list? selector) (string-join (map %rofi-%rofi-format-rasi-value selector) " "))
                    (else (format #f "~a" selector)))))
      (string-append sel-str " {\n"
                     (string-join rules "\n")
                     "\n}"))))

(define (%rofi-format-rasi theme)
  "Convert an S-expression representation of a rofi rasi theme into a string."
  (string-join (map %rofi-%rofi-format-rasi-block theme) "\n"))

(define-var *numpad->rofi-mapping*
  ;; 0  1  2  3  4  5  6  7  8  9
  #(#f
	"south west" "south" "south east"
	"west" "center" "east"
	"north west" "north" "north east"))

(define (numpad->rofi-location n)
  "Maps a standard numpad number (1-9) to the rofi location layout."
  (vector-ref *numpad->rofi-mapping* n))

(define (%rofi-pango-escape str)
  "Escapes xml characters in pango string"
  (string-replace-substring
   (string-replace-substring
    (string-replace-substring str "&" "&amp;")
    "<" "&lt;")
   ">" "&gt;"))

(define (%rofi-item-formatter item)
  (string-join
   (map
	(lambda (x)
	  (format #f "~a" x)) item) "\t"))

(define* (%rofi-make-dmenu-options options colors #:key (pango #t)
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
									   (if (and color (> (string-length str) 0))
                                           (format #f "<span color='~a'>~a</span>" color (%rofi-pango-escape truncated))
                                           truncated))
                                     ;; if not visible, return #f so filter-map drops it
                                     #f)))
                             columns
                             (iota (length columns))))

                ;; concatenate the formatted columns for this row with tabs
                (display-string (string-join formatted-columns "  ")))
		   
           ;; return the rofi-compatible string separating search text from display text
		   ;; NOTE: the below commented is supposed to be able to send metadata
		   ;; as well as define the searchable strings even if they aren't visible
		   ;; (string-append display-string
		   ;; 				  "\\000"
		   ;; 				  "display\\037" display-string "\\037"
		   ;; 				  "meta\\037" display-string "\\037")
		   display-string))
       options))

(define* (make-rofi-backend
          #:key
		  (action 'dmenu)
          (prompt *palette-prompt*)
          (font (format #f "~a ~a" *palette-font* *palette-font-size*))
          (case-sensitive? *palette-case-sensitive?*)
          (show-icons? *palette-show-icons?*)
          (markup-rows? #t)

          (background *palette-bg-color*)
          (background-alt *palette-bg-color*)
          (foreground *theme-fg-main*)
          (selected *theme-bg-active*)
          (active *palette-selected-color*)
          (urgent *palette-urgent-color*)
          (border-color *palette-border-color*)

          (border-width (format #f "~apx solid" *palette-border-width*))
          (border-radius (format #f "~apx" *palette-border-radius*))

          (location (numpad->rofi-location *palette-location*))
          (anchor (numpad->rofi-location *palette-anchor*))
          (width (format #f "~a%" *palette-width*))
          (x-offset "0px")
          (y-offset "20px")
          (window-margin "0px")
          (window-padding "15px")

          (mainbox-spacing "0px")
          (mainbox-margin "0px")
          (mainbox-padding "0px")

          (inputbar-spacing "0px")
          (inputbar-margin "0px")
          (inputbar-padding "10px")

          (columns 1)
          (lines *palette-candidates-count*)
          (listview-spacing "5px")
          (listview-margin "0px")
          (listview-padding "0px")

          (element-spacing "0px")
          (element-margin "0px")
          (element-padding "3px")
          (element-vertical-align 0.5)
		  (element-icon-right-spacing *palette-icon-right-spacing*)
		  (element-icon-size *palette-icon-size*))
  "Create a customizable Rofi backend."

  (let* ((cli-flags
          `(,@(cond
			   ((eq? action 'dmenu) (list "-dmenu"))
			   ((eq? action 'launcher) '("-show" "drun"))
			   ((eq? action 'message) '("-e" "test")))
            "-format" "i"
			"-p" prompt
            ,@(if case-sensitive? '() '("-i"))
            ,@(if show-icons? '("-show-icons") '())
            ,@(if markup-rows? '("-markup-rows") '())))

         (theme-str
          (%rofi-format-rasi
           `((* (font ,(format #f "~s" font))
                (background ,background)
                (background-alt ,background-alt)
                (foreground ,foreground)
                (selected ,selected)
                (active ,active)
                (urgent ,urgent)
                (border-color ,border-color)
                (border-width ,border-width)
                (border-radius ,border-radius))

             (configuration
              (display-run "\" \"")
              (display-drun "\" \"")

			  ;; TODO: make these based on configured keybindings somehow
              (kb-mode-complete "\"\"")
              (kb-remove-to-eol "\"\"")
              (kb-accept-entry "\"Return,KP_Enter\"")
              (kb-remove-char-back "\"BackSpace,Shift+BackSpace\"")
              (kb-row-up "\"Up,Control+p,Control+k\"")
              (kb-row-down "\"Down,Control+n,Control+j\"")
              (kb-move-char-back "\"Left,Control+b,Control+h\"")
              (kb-move-char-forward "\"Right,Control+f,Control+l\"")
              (kb-move-front "\"Control+a\"")
              (kb-move-end "\"Control+e\"")
              (kb-move-word-back "\"Alt+b\"")
              (kb-move-word-forward "\"Alt+f\"")
              (kb-remove-word-back "\"Alt+BackSpace,Control+BackSpace\"")
              (kb-remove-to-sol "\"Control+u\""))

             (window
              (location ,location)
              (anchor ,anchor)
              (width ,width)
              (x-offset ,x-offset)
              (y-offset ,y-offset)
              (margin ,window-margin)
              (padding ,window-padding)
              (border (var border-width))
              (border-radius (var border-radius))
              (background-color (var background)))

             (mainbox
              (spacing ,mainbox-spacing)
              (margin ,mainbox-margin)
              (padding ,mainbox-padding)
              (background-color transparent))

             (inputbar
              (spacing ,inputbar-spacing)
              (margin ,inputbar-margin)
              (padding ,inputbar-padding)
              (background-color transparent)
              (text-color (var foreground)))

             (listview
              (columns ,columns)
              (lines ,lines)
              (spacing ,listview-spacing)
              (margin ,listview-margin)
              (padding ,listview-padding)
              (background-color transparent)
              (border 0))

             (element
				 (spacing ,element-spacing)
               (margin ,element-margin)
               (padding ,element-padding)
               (background-color transparent)
               (text-color (var foreground)))

             ("element selected.normal"
              (background-color (var selected))
              (text-color (var foreground)))

             ("element alternate.normal"
              (background-color (var background)))

             (element-text
              (background-color transparent)
              (text-color inherit)
              (vertical-align ,element-vertical-align))

             (element-icon
              (background-color transparent)
              (margin ,(format #f "0px ~apx 0px 0px" element-icon-right-spacing))
              (size ,(format #f "~apx" element-icon-size))))))

		 (cmd (string-join
			   (append (list "rofi")
					   (map %rofi-shell-quote cli-flags)
					   (list "-theme-str" (%rofi-shell-quote theme-str)))
			   " ")))

	cmd))

(define-var *rofi-palette-command* (make-rofi-backend #:action 'dmenu))
(define* (rofi-palette-show candidates #:key (theme-overrides '())
							(command *rofi-palette-command*))
  (let* ((display-strings (map %rofi-item-formatter candidates))
         (input-str (string-join display-strings "\n"))
         ;; printf to escape null byte needed for metadata injection
		 ;; (cmd (string-append "while IFS= read -r line; do printf \"%b\\n\" \"$line\"; done <<'EOF_LAUNCHER' | " command "\n"
         ;;                     input-str
         ;;                     "\nEOF_LAUNCHER"))
		 (cmd (string-append "printf '%s\\n' " (%rofi-shell-quote input-str) " | " command))
         (port (open-input-pipe cmd))
         (selected (read-line port)))
    (close-pipe port)
	(if (or (eof-object? selected) (string-null? selected))
        #f
        (string->number selected))))

(define-command (rofi-install-dmenu-launcher!)
  "Install rofi as the dmenu launcher"
  (set! *palette-backend* rofi-palette-show)
  (set! *palette-dmenu-options* %rofi-make-dmenu-options))

(define-command (rofi-install-app-launcher!)
  "Install rofi as the app launcher"
  (set! *dmenu-command* (make-rofi-backend #:action 'launcher)))

(define-command (rofi-install-toast!)
  "Install rofi as the message backend"
  (set! *toast-backend* (make-rofi-backend #:action 'message)))

(define-command (rofi-install-all!)
  "Install rofi as the dmenu launcher, app launcher, and message backend"
  (rofi-install-dmenu-launcher!)
  (rofi-install-toast!)
  (rofi-install-dmenu-launcher!))

