;;; gliver/core/config.scm --- Configuration system for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module manages user configuration options.
;;;
;;; TODO: this file is expected to have the least amount of configuration
;;; needed and shared between several modules or components.
;;; any configuration that's primarily used by one file/module should
;;; be defined in that file instead.

(define-module (gliver core config)
  #:use-module (ice-9 format)
  #:use-module (gliver config)
  #:use-module (gliver core logs)
  #:use-module (gliver core types)
  #:use-module (gliver core hooks)
  #:use-module (gliver core helpers)
  #:declarative? #f
  #:export (
			*quit-exits-river*
			*terminal*
			*startup-message*
			*wallpaper*
			*wallpaper-mode*
			*prefix-timeout*
			*message-timeout*
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
			*window-border-width*
			*window-border-color-focused*
			*window-border-color-unfocused*
			*window-border-color-urgent*
			*palette-font*
			*palette-font-size*
			*palette-icon-color*
			*palette-name-color*
			*palette-value-color*
			*palette-help-color*
			*palette-doc-color*
			*palette-prompt*
			*palette-prompt-color*
			*palette-input-bg-color*
			*palette-input-border-color*
			*palette-cursor-width*
			*palette-cursor-color*
			*palette-candidates-count-color*
			*palette-no-candidates-count-color*
			*palette-no-candidates-text-color*
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
			*palette-selected-fg-color*
			*palette-selected-bg-color*
			*palette-selected-radius*
			*palette-anchor-margin*
			*palette-urgent-color*
			*palette-border-color*
			*palette-border-width*
			*palette-border-radius*
			*palette-match-color*
			*palette-repeat-rate*
			*palette-repeat-delay*
			*palette-location*
			*palette-anchor*
			*palette-variables-widths*
			*palette-variables-searchable*
			*palette-variables-visible*
			*palette-key-select*
			*palette-key-cancel*
			*palette-key-next*
			*palette-key-prev*
			*palette-key-first*
			*palette-key-last*
			*palette-key-page-down*
			*palette-key-page-up*
			*palette-key-delete-word-backward*
			*palette-key-delete-word-forward*
			*palette-key-delete-char-backward*
			*palette-key-delete-char-forward*
			*palette-key-kill-line*
			*palette-key-discard-line*
			*palette-key-word-backward*
			*palette-key-word-forward*
			*palette-key-bol*
			*palette-key-eol*
			*palette-key-char-backward*
			*palette-key-char-forward*
			*statusbar-enabled*
			*statusbar-position*
			*statusbar-height*
			*statusbar-margin-top*
			*statusbar-margin-bottom*
			*statusbar-margin-left*
			*statusbar-margin-right*
			*statusbar-padding-x*
			*statusbar-padding-y*
			*statusbar-spacing*
			*statusbar-bg-color*
			*statusbar-fg-color*
			*statusbar-border-color*
			*statusbar-border-width*
			*statusbar-border-radius*
			*statusbar-pill-radius*
			*statusbar-pill-padding-x*
			*statusbar-pill-padding-y*
			*statusbar-font*
			*statusbar-font-size*
			*statusbar-modules-left*
			*statusbar-modules-center*
			*statusbar-modules-right*
			*statusbar-click-enabled*
			*xdg-config-home*
			*xdg-state-home*
			*xdg-data-home*
			*gliver-config-dir*
			*gliver-runtime-dir*
			*gliver-state-dir*
			*config-file-path*
			*example-config-file*
			config-load!
			config-reload!
))

;;; config variables
(define-var *quit-exits-river* #t
			"Whether the quit command should also exit river.")
(define-var *terminal* "foot"
			"Terminal emulator to be used when spawning terminals.")
(define-var *startup-message* #t
			"Whether to show message at startup.")
(define-var *wallpaper* #f
			"Default wallpaper (image file path or hex color).")
(define-var *wallpaper-mode* 'fill
			"Default wallpaper scaling mode: 'fill, 'fit, 'stretch, 'center, or 'tile.")
(define-var *prefix-timeout* 1000
			"Timeout in milliseconds for keybinding prefix sequences.")
(define-var *message-timeout* 5
			"Timeout in seconds for notification messages.")

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
(define-var *theme-border-color* *theme-fg-alt*
            "Default color used for window borders.")
(define-var *theme-urgent-color* *theme-red*
            "Accent color used to indicate urgent or error states.")

;; container and window borders
(define-var *window-border-width* *theme-border-width*
            "Default window border width in pixels.")
(define-var *window-border-color-focused* "#00000000"
            "Border color for focused windows.")
(define-var *window-border-color-unfocused* "#00000000"
            "Border color for unfocused windows.")
(define-var *window-border-color-urgent* "#00000000"
            "Border color for urgent windows.")

;;; palette/launcher options
(define-var *palette-font* *theme-font*
            "Font family string used inside the launcher palette.")
(define-var *palette-font-size* *theme-font-size*
            "Font size in points for the launcher palette.")
(define-var *palette-icon-color* *theme-blue*
            "Color used for rendering candidate icons in the palette.")
(define-var *palette-name-color* *theme-text*
            "Color used for displaying candidate names in the palette.")
(define-var *palette-value-color* *theme-fg-alt*
            "Color used for displaying item values in the palette.")
(define-var *palette-help-color* *theme-peach*
            "Color used for help text and keybinding hints.")
(define-var *palette-doc-color* *theme-green*
            "Color used for rendering variable documentation text.")
(define-var *palette-prompt* "> "
            "Prompt string displayed before the input field in the palette.")
(define-var *palette-prompt-color* *theme-fg-alt*
            "Color used for rendering prompt text.")
(define-var *palette-input-bg-color* *theme-surface0*
            "Color used for prompt text background.")
(define-var *palette-input-border-color* *theme-border-color*
            "Color used for prompt text border.")
(define-var *palette-cursor-width* 2.0
            "Width of palette cursor.")
(define-var *palette-cursor-color* *theme-text*
            "Color used for palette cursor.")
(define-var *palette-candidates-count-color* *theme-overlay2*
            "Color used for palette candidates counter.")
(define-var *palette-no-candidates-count-color* *theme-red*
            "Color used for palette candidates counter when empty.")
(define-var *palette-no-candidates-text-color* *theme-overlay2*
            "Color used for palette candidates text when empty.")
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
(define-var *palette-selected-fg-color* *theme-fg-alt*
            "Highlight color used for the currently selected candidate entry.")
(define-var *palette-selected-bg-color* *theme-surface1*
            "Color used for background of the currently selected candidate entry.")
(define-var *palette-selected-radius* 0
            "Radius used for background of the currently selected candidate entry.")
(define-var *palette-anchor-margin* 80
            "Margin used in palette form anchor direction defined in *palette-location*.")
(define-var *palette-urgent-color* *theme-urgent-color*
            "Color used for urgent or high-priority candidate rows.")
(define-var *palette-border-color* *theme-border-color*
            "Border color around the palette window.")
(define-var *palette-border-width* *theme-border-width*
            "Border width of the palette window in pixels.")
(define-var *palette-border-radius* *theme-border-radius*
            "Corner border radius of the palette window in pixels.")
(define-var *palette-match-color* *theme-red*
            "Color used to highlight query keywords in search results (defaults to red).")
(define-var *palette-repeat-rate* 25
            "Key repeat rate in characters per second.")
(define-var *palette-repeat-delay* 600
            "Delay in milliseconds before key repeat starts.")
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

;;; palette keybindings
(define-var *palette-key-select* '("Return" "KP_Enter" "C-m")
            "Keybinding list to select candidate or confirm input.")
(define-var *palette-key-cancel* '("Escape" "C-g" "C-c" "C-[")
            "Keybinding list to dismiss the palette.")
(define-var *palette-key-next* '("Down" "C-n" "C-j" "Tab")
            "Keybinding list to select next candidate.")
(define-var *palette-key-prev* '("Up" "C-p" "S-Tab" "ISO_Left_Tab")
            "Keybinding list to select previous candidate.")
(define-var *palette-key-first* '("M-<")
            "Keybinding list to jump to the first candidate.")
(define-var *palette-key-last* '("M->")
            "Keybinding list to jump to the last candidate.")
(define-var *palette-key-page-down* '("Page_Down" "C-v")
            "Keybinding list to scroll down one page.")
(define-var *palette-key-page-up* '("Page_Up" "M-v")
            "Keybinding list to scroll up one page.")
(define-var *palette-key-delete-word-backward* '("C-BackSpace" "M-BackSpace" "C-w")
            "Keybinding list to delete word backward.")
(define-var *palette-key-delete-word-forward* '("M-d")
            "Keybinding list to delete word forward.")
(define-var *palette-key-delete-char-backward* '("BackSpace" "C-h")
            "Keybinding list to delete character backward.")
(define-var *palette-key-delete-char-forward* '("Delete" "C-d")
            "Keybinding list to delete character forward.")
(define-var *palette-key-kill-line* '("C-k")
            "Keybinding list to kill text to end of line.")
(define-var *palette-key-discard-line* '("C-u")
            "Keybinding list to discard text to beginning of line.")
(define-var *palette-key-word-backward* '("M-b")
            "Keybinding list to move cursor backward by word.")
(define-var *palette-key-word-forward* '("M-f")
            "Keybinding list to move cursor forward by word.")
(define-var *palette-key-bol* '("Home" "C-a")
            "Keybinding list to move cursor to beginning of line.")
(define-var *palette-key-eol* '("End" "C-e")
            "Keybinding list to move cursor to end of line.")
(define-var *palette-key-char-backward* '("Left" "C-b")
            "Keybinding list to move cursor backward by character.")
(define-var *palette-key-char-forward* '("Right" "C-f")
            "Keybinding list to move cursor forward by character.")

;;; statusbar options
(define-var *statusbar-enabled* #f
            "Whether the statusbar is currently enabled.")
(define-var *statusbar-position* 'top
            "Position of the statusbar: 'top or 'bottom.")
(define-var *statusbar-height* 32
            "Height of the statusbar in pixels.")
(define-var *statusbar-margin-top* 4
            "Top margin of the statusbar.")
(define-var *statusbar-margin-bottom* 0
            "Bottom margin of the statusbar.")
(define-var *statusbar-margin-left* 8
            "Left margin of the statusbar.")
(define-var *statusbar-margin-right* 8
            "Right margin of the statusbar.")
(define-var *statusbar-padding-x* 8
            "Horizontal inner padding of the statusbar.")
(define-var *statusbar-padding-y* 2
            "Vertical inner padding of the statusbar.")
(define-var *statusbar-spacing* 6
            "Spacing in pixels between module capsules.")
(define-var *statusbar-bg-color* *theme-bg-main*
            "Background color of the statusbar (hex string, supports RGBA).")
(define-var *statusbar-fg-color* *theme-fg-main*
            "Default foreground text color of the statusbar.")
(define-var *statusbar-border-color* *theme-fg-alt*
            "Border color of the statusbar.")
(define-var *statusbar-border-width* 0
            "Border width of the statusbar in pixels (0 for no border).")
(define-var *statusbar-border-radius* 10
            "Corner border radius of the statusbar (0 for rectangular bar, >0 for floating island).")
(define-var *statusbar-pill-radius* 6
            "Default corner border radius for module capsules.")
(define-var *statusbar-pill-padding-x* 10
            "Default horizontal padding inside module capsules.")
(define-var *statusbar-pill-padding-y* 3
            "Default vertical padding inside module capsules.")
(define-var *statusbar-font* *theme-font*
            "Font family string used inside the statusbar.")
(define-var *statusbar-font-size* 12
            "Font size in points for statusbar text.")
(define-var *statusbar-modules-left* '(workspaces window)
            "List of modules to render on the left side of the statusbar.")
(define-var *statusbar-modules-center* '(date weather)
            "List of modules to render at the center of the statusbar.")
(define-var *statusbar-modules-right* '(cpu ram battery)
            "List of modules to render on the right side of the statusbar.")
(define-var *statusbar-click-enabled* #t
            "Whether pointer click and scroll interaction is enabled on the statusbar.")

;;; XDG directories
(define-var *xdg-config-home*
  (or (getenv "XDG_CONFIG_HOME")
      (string-append (getenv "HOME") "/.config"))
  "Directory path where user configuration files reside.")

(define-var *xdg-state-home*
  (or (getenv "XDG_STATE_HOME")
      (string-append (getenv "HOME") "/.local/state"))
  "Directory path where persistent state files reside.")

(define-var *xdg-data-home*
  (or (getenv "XDG_DATA_HOME")
      (string-append (getenv "HOME") "/.local/share"))
  "Directory path where application data files reside.")

(define-var *gliver-config-dir*
  (string-append *xdg-config-home* "/gliver")
  "Directory path for Gliver configuration files.")

(define-var *gliver-runtime-dir*
  (let ((base (string-append *xdg-state-home* "/gliver"))
        (wayland-display (getenv "WAYLAND_DISPLAY")))
    (if (and wayland-display
             (not (string=? wayland-display "wayland-1")))
        (string-append base "/" wayland-display)
        base))
  "Directory path for Gliver runtime state.")

(define-var *gliver-state-dir*
  (let ((base (string-append *xdg-state-home* "/gliver"))
        (wayland-display (getenv "WAYLAND_DISPLAY")))
    (if (and wayland-display
             (not (string=? wayland-display "wayland-1")))
        (string-append base "/" wayland-display)
        base))
  "Directory path for Gliver persistent state.")

;;; configuration file loading
(define-var *config-file-path*
  (string-append *gliver-config-dir* "/init.scm")
  "File path to the user's primary configuration file.")

(define *example-config-file*
  ;; in-vicinity doesn't accept multiple arguments
  ;; in 3.0.11
  (in-vicinity %gliver-installation-prefix
               "share/gliver/examples/init.scm"))

(define-command (config-load!)
  "Load the user configuration file."
  (let ((path *config-file-path*))
    (if (file-exists? path)
        (catch #t
          (lambda ()
            (log-info "Loading config: ~a" path)
            (load path)
			(gliver-hook-run! *config-loaded-hook*)
            (log-info "Config loaded successfully."))
          (lambda (key . args)
            (log-error "Error loading config ~a: ~a ~a" path key args)
            (format (current-error-port)
                    "Gliver: Error loading config: ~a ~a~%" key args)))
        (begin
          (log-info "No config file found at ~a, loading example file."
                    path)
          (load *example-config-file*)
		  (gliver-hook-run! *config-loaded-hook*)
          (log-info "Example config file loaded.")))))

(define-command (config-reload!)
  "Reload the configuration file."
  (catch #t
    (lambda ()
      (config-load!)
      (log-info "Config reloaded."))
    (lambda (key . args)
      (log-error "Config reload error: ~a ~a" key args))))
