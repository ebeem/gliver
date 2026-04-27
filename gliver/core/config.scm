;;; gliver/config.scm --- Configuration system for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver core config)
  #:use-module (ice-9 format)
  #:use-module (gliver core logs)
  #:declarative? #f
  #:export (
			*default-workspace-name*
			*terminal*
			*shell-program*
			*suppress-container-indicator*
			*startup-message*
			*window-name-source*
			*wm-behavior-focus-mouse-enter*
			*wm-behavior-focus-clear-mouse-leave*
			*wm-behavior-focus-mouse-click*
			*wm-behavior-focus-new-window*
			*wm-behavior-workspace-remove-to*
			*wm-behavior-default-capabilties*
			*wm-behavior-default-edges*
			config-file-path
			config-load!
			config-reload!
			*xdg-config-home*
			*xdg-runtime-dir*
			*xdg-state-home*
			*xdg-data-home*
			*gliver-config-dir*
			*gliver-runtime-dir*
			*gliver-state-dir*
))

;;; config variables
(define *default-workspace-name* "Default")
(define *terminal* "foot")
(define *shell-program* "/bin/sh")
(define *suppress-container-indicator* #f)
(define *startup-message* #t)
(define *window-name-source* 'title)  ;; 'title | 'app-id | 'class

(define *wm-behavior-focus-mouse-enter* #f)
(define *wm-behavior-focus-clear-mouse-leave* #f)
(define *wm-behavior-focus-mouse-click* #t)
(define *wm-behavior-focus-new-window* #t)
(define *wm-behavior-workspace-remove-to* 'focus)    ;; 'focus | 'index
(define *wm-behavior-default-capabilties* 15)
(define *wm-behavior-default-edges* 15)

;;; config file loading
(define (config-file-path)
  "Return the path to the config file."
  (let ((config-dir (*gliver-config-dir*)))
    (string-append config-dir "/init.scm")))

(define (config-load!)
  "Load the user configuration file."
  (let ((path (config-file-path)))
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

(define (config-reload!)
  "Reload the configuration file."
  (catch #t
    (lambda ()
      (config-load!)
      (log-info "Config reloaded."))
    (lambda (key . args)
      (log-error "Config reload error: ~a ~a" key args))))
;;; xdg directories
(define (*xdg-config-home*)
  (or (getenv "XDG_CONFIG_HOME")
      (string-append (getenv "HOME") "/.config")))

(define (*xdg-runtime-dir*)
  (or (getenv "XDG_RUNTIME_DIR")
      (string-append "/run/user/" (number->string (getuid)))))

(define (*xdg-state-home*)
  (or (getenv "XDG_STATE_HOME")
      (string-append (getenv "HOME") "/.local/state")))

(define (*xdg-data-home*)
  (or (getenv "XDG_DATA_HOME")
      (string-append (getenv "HOME") "/.local/share")))

(define (*gliver-config-dir*)
  (string-append (*xdg-config-home*) "/gliver"))

(define (*gliver-runtime-dir*)
  (let ((dir (string-append (*xdg-runtime-dir*) "/gliver")))
    (unless (file-exists? dir)
      (mkdir dir))
    dir))

(define (*gliver-state-dir*)
  (let ((dir (string-append (*xdg-state-home*) "/gliver")))
    (unless (file-exists? dir)
      (mkdir dir #o755))
    dir))
