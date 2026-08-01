(define-module (gliver contrib ui palette rofi)
  #:use-module (gliver contrib ui palette base)
  #:use-module (gliver core)
  #:use-module (srfi srfi-1)
  #:export (
			format-rasi-value
			format-rasi-rule
			format-rasi-block
			format-rasi
			*numpad->rofi-mapping*
			numpad->rofi-location
			make-rofi-backend
			install-rofi-dmenu-launcher!
			install-rofi-app-launcher!
))

(define (format-rasi-value val)
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
      (string-join (map format-rasi-value val) " "))))
   (else (format #f "~a" val))))

(define (format-rasi-rule rule)
  "Format a (property value) pair as a rasi style rule."
  (let ((prop (car rule))
        (val (cadr rule)))
    (format #f "    ~a: ~a;" prop (format-rasi-value val))))

(define (format-rasi-block block)
  "Format a (selector (prop val) ...) block into a rasi css-like section."
  (let ((selector (car block))
        (rules (filter-map (lambda (r) (if (cadr r) (format-rasi-rule r) #f)) (cdr block))))
    (let ((sel-str (cond
                    ((symbol? selector) (symbol->string selector))
                    ((string? selector) selector)
                    ((list? selector) (string-join (map format-rasi-value selector) " "))
                    (else (format #f "~a" selector)))))
      (string-append sel-str " {\n"
                     (string-join rules "\n")
                     "\n}"))))

(define (format-rasi theme)
  "Convert an S-expression representation of a rofi rasi theme into a string."
  (string-join (map format-rasi-block theme) "\n"))

(define-var *numpad->rofi-mapping*
  ;; 0  1  2  3  4  5  6  7  8  9
  #(#f
	"south west" "south" "south east"
	"west" "center" "east"
	"north west" "north" "north east"))

(define (numpad->rofi-location n)
  "Maps a standard numpad number (1-9) to the rofi location layout."
  (vector-ref *numpad->rofi-mapping* n))

(define* (make-rofi-backend
          #:key
		  (dmenu? #t)
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
          `(,@(if dmenu?
                  (list "-dmenu")
                  '("-show" "drun"))
            "-format" "i"
			"-p" prompt
            ,@(if case-sensitive? '() '("-i"))
            ,@(if show-icons? '("-show-icons") '())
            ,@(if markup-rows? '("-markup-rows") '())))

         (theme-str
          (format-rasi
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
					   (map shell-quote cli-flags)
					   (list "-theme-str" (shell-quote theme-str)))
			   " ")))

	cmd))

(define-command (install-rofi-dmenu-launcher!)
  "Install rofi as the dmenu launcher"
  (set! *palette-backend* (make-launcher-backend (make-rofi-backend))))

(define-command (install-rofi-app-launcher!)
  "Install rofi as the app launcher"
  (set! *dmenu-command* (make-rofi-backend #:dmenu? #f)))

