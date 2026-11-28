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
  #:use-module (srfi srfi-1)
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
            *palette-icon-color*
            *palette-name-color*
            *palette-value-color*
            *palette-help-color*
            *palette-doc-color*
            *palette-variables-widths*
            *palette-variables-searchable*
            *palette-variables-visible*
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
;; default terminal is the first found terminal from known ones
(define-var *terminal*
  (or (find executable-exists? `(,(or (getenv "TERM") "foot")
                                 "foot" "kitty" "alacritty"
                                 "ghostty" "tilix" "xterm"))
      "foot")
  "Terminal emulator to be used when spawning terminals.")
(define-var *startup-message* #t
            "Whether to show message at startup.")
(define-var *wallpaper* #f
            "Default wallpaper (image file path or hex color).")
(define-var *wallpaper-mode* 'fill
            "Default wallpaper scaling mode: 'fill, 'fit, 'stretch, 'center, or 'tile.")
(define-var *prefix-timeout* 1000
            "Timeout in milliseconds for keybinding prefix sequences.")
(define-var *message-timeout* 8
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
(define-var *theme-bg-main* #:inherit *theme-base*
            "Main background color used for windows and primary surfaces.")
(define-var *theme-bg-dim* #:inherit *theme-crust*
            "Dimmed background color for sunken or secondary areas.")
(define-var *theme-fg-main* #:inherit *theme-text*
            "Main foreground color for primary text.")
(define-var *theme-fg-dim* #:inherit *theme-subtext0*
            "Dimmed foreground color for subtle or secondary text.")
(define-var *theme-fg-alt* #:inherit *theme-mauve*
            "Alternative foreground color for highlighted text.")
(define-var *theme-bg-active* #:inherit *theme-surface1*
            "Background color for active or focused elements.")
(define-var *theme-bg-inactive* #:inherit *theme-surface0*
            "Background color for unfocused or inactive elements.")
(define-var *theme-border-color* #:inherit *theme-fg-alt*
            "Default color used for window borders.")
(define-var *theme-urgent-color* #:inherit *theme-red*
            "Accent color used to indicate urgent or error states.")

;; container and window borders
(define-var *window-border-width* #:inherit *theme-border-width*
            "Default window border width in pixels.")
(define-var *window-border-color-focused* "#00000000"
            "Border color for focused windows.")
(define-var *window-border-color-unfocused* "#00000000"
            "Border color for unfocused windows.")
(define-var *window-border-color-urgent* "#00000000"
            "Border color for urgent windows.")

;;; palette/help options
(define-var *palette-icon-color* #:inherit *theme-blue*
            "Color used for rendering candidate icons in the palette.")
(define-var *palette-name-color* #:inherit *theme-text*
            "Color used for displaying candidate names in the palette.")
(define-var *palette-value-color* #:inherit *theme-fg-alt*
            "Color used for displaying item values in the palette.")
(define-var *palette-help-color* #:inherit *theme-peach*
            "Color used for help text and keybinding hints.")
(define-var *palette-doc-color* #:inherit *theme-green*
            "Color used for rendering variable documentation text.")

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
