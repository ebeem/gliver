;;; gliver/config.scm --- Configuration system for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver core config)
  #:use-module (ice-9 format)
  #:use-module (gliver core logs)
  #:use-module (gliver core types)
  #:declarative? #f
  #:export (
			*default-workspace-name*
			*terminal*
			*startup-message*
			*wm-behavior-focus-mouse-enter*
			*wm-behavior-focus-clear-mouse-leave*
			*wm-behavior-focus-mouse-click*
			*wm-behavior-focus-new-window*
			*wm-behavior-focus-new-workspace*
			*wm-behavior-focus-new-container*
			*wm-behavior-focus-new-output*
			*wm-behavior-workspace-remove-to*
			*wm-behavior-default-decoration*
			*wm-behavior-default-capabilties*
			*wm-behavior-default-edges*
			*wm-behavior-default-border-edges*
			*theme-font*
			*theme-font-size*
			*theme-rosewater*
			*theme-flamingo*
			*theme-pink*
			*theme-mauve*
			*theme-red*
			*theme-maroon*
			*theme-peach*
			*theme-yellow*
			*theme-green*
			*theme-teal*
			*theme-sky*
			*theme-sapphire*
			*theme-blue*
			*theme-lavender*
			*theme-text*
			*theme-subtext1*
			*theme-subtext0*
			*theme-overlay2*
			*theme-overlay1*
			*theme-overlay0*
			*theme-surface2*
			*theme-surface1*
			*theme-surface0*
			*theme-base*
			*theme-mantle*
			*theme-crust*
			*theme-border-width*
			*theme-border-radius*
			*theme-bg-main*
			*theme-bg-dim*
			*theme-fg-main*
			*theme-fg-dim*
			*theme-fg-alt*
			*theme-bg-active*
			*theme-bg-inactive*
			*theme-border-color*
			*theme-urgent-color*
			*palette-font*
			*palette-font-size*
			*palette-icon-color*
			*palette-name-color*
			*palette-value-color*
			*palette-help-color*
			*palette-doc-color*
			*palette-prompt*
			*palette-case-sensitive?*
			*palette-show-icons?*
			*palette-show-sidebar?*
			*palette-icon-size*
			*palette-icon-right-spacing*
			*palette-width*
			*palette-candidates-count*
			*palette-output*
			*palette-icon-theme*
			*palette-bg-color*
			*palette-fg-color*
			*palette-selected-color*
			*palette-urgent-color*
			*palette-border-color*
			*palette-border-width*
			*palette-border-radius*
			*palette-location*
			*palette-anchor*
			*palette-variables-widths*
			*palette-variables-searchable*
			*palette-variables-visible*
			*xdg-config-home*
			*xdg-runtime-dir*
			*xdg-state-home*
			*xdg-data-home*
			*gliver-config-dir*
			*gliver-runtime-dir*
			*gliver-state-dir*
			*config-file-path*
			config-load!
			config-reload!
))

;;; config variables
(define-var *default-workspace-name* "Default"
			"Default workspace name used for initial workspace.")
(define-var *terminal* "foot"
			"Terminal emulator to be used when spawning terminals.")
(define-var *startup-message* #t
			"Whether to show message at startup.")

;;; window manager behavior
(define-var *wm-behavior-focus-mouse-enter* #f)
(define-var *wm-behavior-focus-clear-mouse-leave* #f)
(define-var *wm-behavior-focus-mouse-click* #t)
(define-var *wm-behavior-focus-new-window* #t)
(define-var *wm-behavior-focus-new-workspace* #t)
(define-var *wm-behavior-focus-new-container* #t)
(define-var *wm-behavior-focus-new-output* #f)
(define-var *wm-behavior-workspace-remove-to* 'focus)    ;; 'focus | 'index
(define-var *wm-behavior-default-decoration* 'server)    ;; 'server | 'client
(define-var *wm-behavior-default-capabilties* 0)
(define-var *wm-behavior-default-edges* 15)
(define-var *wm-behavior-default-border-edges* 15)

;;; theme options
(define-var *theme-font* "iosevka Bold"
            "The primary font family used across the application interface.")
(define-var *theme-font-size* 15
            "The default UI font size in points.")

;;; catppuccin macchiato color palette
(define-var *theme-rosewater* "#f4dbd6"
            "Hex color code for theme rosewater accent.")
(define-var *theme-flamingo* "#f0c6c6"
            "Hex color code for theme flamingo accent.")
(define-var *theme-pink* "#f5bde6"
            "Hex color code for theme pink accent.")
(define-var *theme-mauve* "#c6a0f6"
            "Hex color code for theme mauve accent.")
(define-var *theme-red* "#ed8796"
            "Hex color code for theme red accent.")
(define-var *theme-maroon* "#ee99a0"
            "Hex color code for theme maroon accent.")
(define-var *theme-peach* "#f5a97f"
            "Hex color code for theme peach accent.")
(define-var *theme-yellow* "#eed49f"
            "Hex color code for theme yellow accent.")
(define-var *theme-green* "#a6da95"
            "Hex color code for theme green accent.")
(define-var *theme-teal* "#8bd5ca"
            "Hex color code for theme teal accent.")
(define-var *theme-sky* "#91d7e3"
            "Hex color code for theme sky accent.")
(define-var *theme-sapphire* "#7dc4e4"
            "Hex color code for theme sapphire accent.")
(define-var *theme-blue* "#8aadf4"
            "Hex color code for theme blue accent.")
(define-var *theme-lavender* "#b7bdf8"
            "Hex color code for theme lavender accent.")
(define-var *theme-text* "#cad3f5"
            "Hex color code for primary text foreground.")
(define-var *theme-subtext1* "#b8c0e0"
            "Hex color code for secondary subtext.")
(define-var *theme-subtext0* "#a5adcb"
            "Hex color code for tertiary subtext.")
(define-var *theme-overlay2* "#939ab7"
            "Hex color code for high-contrast overlay elements.")
(define-var *theme-overlay1* "#8087a2"
            "Hex color code for medium-contrast overlay elements.")
(define-var *theme-overlay0* "#6e738d"
            "Hex color code for low-contrast overlay elements.")
(define-var *theme-surface2* "#5b6078"
            "Hex color code for highest surface elevation.")
(define-var *theme-surface1* "#494d64"
            "Hex color code for medium surface elevation.")
(define-var *theme-surface0* "#363a4f"
            "Hex color code for lowest surface elevation.")
(define-var *theme-base* "#24273a"
            "Hex color code for primary background base.")
(define-var *theme-mantle* "#1e2030"
            "Hex color code for secondary mantle background.")
(define-var *theme-crust* "#181926"
            "Hex color code for deepest background layer.")
(define-var *theme-border-width* 3
            "Default window and element border width in pixels.")
(define-var *theme-border-radius* 4
            "Default window corner radius in pixels.")

;; general colors
(define-var *theme-bg-main* *theme-base*
            "Main background color used for windows and primary surfaces.")
(define-var *theme-bg-dim* *theme-crust*
            "Dimmed background color for sunken or secondary areas.")
(define-var *theme-fg-main* *theme-text*
            "Main foreground color for primary text.")
(define-var *theme-fg-dim* *theme-subtext0*
            "Dimmed foreground color for subtle or secondary text.")
(define-var *theme-fg-alt* *theme-mauve*
            "Alternative foreground color for highlighted text.")
(define-var *theme-bg-active* *theme-surface1*
            "Background color for active or focused elements.")
(define-var *theme-bg-inactive* *theme-surface0*
            "Background color for unfocused or inactive elements.")
(define-var *theme-border-color* *theme-mauve*
            "Default color used for window borders.")
(define-var *theme-urgent-color* *theme-red*
            "Accent color used to indicate urgent or error states.")

;;; palette/launcher options
(define-var *palette-font* *theme-font*
            "Font family string used inside the launcher palette.")
(define-var *palette-font-size* *theme-font-size*
            "Font size in points for the launcher palette.")
(define-var *palette-icon-color* *theme-blue*
            "Pango color used for rendering candidate icons in the palette.")
(define-var *palette-name-color* *theme-text*
            "Pango color used for displaying candidate names in the palette.")
(define-var *palette-value-color* *theme-mauve*
            "Pango color used for displaying item values in the palette.")
(define-var *palette-help-color* *theme-peach*
            "Pango color used for help text and keybinding hints.")
(define-var *palette-doc-color* *theme-green*
            "Pango color used for rendering variable documentation text.")
(define-var *palette-prompt* "> "
            "Prompt string displayed before the input field in the palette.")
(define-var *palette-case-sensitive?* #f
            "Whether string filtering in the palette is case sensitive.")
(define-var *palette-show-icons?* #t
            "Whether to display candidate icons in the palette list.")
(define-var *palette-show-sidebar?* #f
            "Whether to display sidebar mode indicator in the palette list.")
(define-var *palette-icon-size* 32
            "Size of icons displayed in the launcher (app icons).")
(define-var *palette-icon-right-spacing* 10
            "Spacing between icon and text in the launcher.")
(define-var *palette-width* 90
            "Width of the palette window as a percentage of monitor width.")
(define-var *palette-candidates-count* 10
            "Maximum number of visible candidate rows shown in the palette.")
(define-var *palette-output* #f
            "Name of the specific monitor/output to show the palette on, or #f for active.")
(define-var *palette-icon-theme* #f
            "Icon theme name used for palette candidates, or #f for system default.")
(define-var *palette-bg-color* *theme-bg-main*
            "Background color of the palette window.")
(define-var *palette-fg-color* *theme-fg-main*
            "Foreground text color of the palette window.")
(define-var *palette-selected-color* *theme-fg-alt*
            "Highlight color used for the currently selected candidate entry.")
(define-var *palette-urgent-color* *theme-urgent-color*
            "Color used for urgent or high-priority candidate rows.")
(define-var *palette-border-color* *theme-border-color*
            "Border color around the palette window.")
(define-var *palette-border-width* *theme-border-width*
            "Border width of the palette window in pixels.")
(define-var *palette-border-radius* *theme-border-radius*
            "Corner border radius of the palette window in pixels.")
(define-var *palette-location* 8
            "The integer value of the location of the palette
following the same order in a keyboard numpad
7 8 9
4 5 6
1 2 3")
(define-var *palette-anchor* 8
            "The integer value of the anchor of the palette
following the same order in a keyboard numpad
7 8 9
4 5 6
1 2 3")

;;; palette column configuration
;;; palette variables help
;;; (icon variable-name variable-value module-name documentation)
(define-var *palette-variables-widths* '(2 35 40 40 40)
            "List of character widths for columns in the variables inspector table.")
(define-var *palette-variables-searchable* '(#f #t #t #f #f)
            "Boolean mask determining which columns in the variables inspector are searchable.")
(define-var *palette-variables-visible* '(#t #t #t #f #t)
            "Boolean mask determining which columns are rendered in the variables inspector.")

;;; XDG directories
(define-var *xdg-config-home*
  (or (getenv "XDG_CONFIG_HOME")
      (string-append (getenv "HOME") "/.config")))

(define-var *xdg-runtime-dir*
  (or (getenv "XDG_RUNTIME_DIR")
      (string-append "/run/user/" (number->string (getuid)))))

(define-var *xdg-state-home*
  (or (getenv "XDG_STATE_HOME")
      (string-append (getenv "HOME") "/.local/state")))

(define-var *xdg-data-home*
  (or (getenv "XDG_DATA_HOME")
      (string-append (getenv "HOME") "/.local/share")))

(define-var *gliver-config-dir*
  (string-append *xdg-config-home* "/gliver"))

(define-var *gliver-runtime-dir*
  (let ((dir (string-append *xdg-runtime-dir* "/gliver")))
    (unless (file-exists? dir)
      (mkdir dir))
    dir))

(define-var *gliver-state-dir*
  (let ((dir (string-append *xdg-state-home* "/gliver")))
    (unless (file-exists? dir)
      (mkdir dir #o755))
    dir))

;;; configuration file loading
(define-var *config-file-path*
  (string-append *gliver-config-dir* "/init.scm")
  "Return the path to the config file.")

(define-command (config-load!)
  "Load the user configuration file."
  (let ((path *config-file-path*))
    (if (file-exists? path)
        (catch #t
          (lambda ()
            (log-info "Loading config: ~a" path)
            (load path)
            (log-info "Config loaded successfully."))
          (lambda (key . args)
            (log-error "Error loading config ~a: ~a ~a" path key args)
            (format (current-error-port)
                    "Gliver: Error loading config: ~a ~a~%" key args)))
        (log-info "No config file found at ~a, using defaults." path))))

(define-command (config-reload!)
  "Reload the configuration file."
  (catch #t
    (lambda ()
      (config-load!)
      (log-info "Config reloaded."))
    (lambda (key . args)
      (log-error "Config reload error: ~a ~a" key args))))

