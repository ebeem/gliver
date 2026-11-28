;;; gliver/contrib/utils/xdg-desktop.scm --- XDG desktop session and portal utilities
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module manages XDG desktop session environment setup, D-Bus/systemd
;;; activation environment updates, and desktop portal restarts.

(define-module (gliver contrib utils xdg-desktop)
  #:use-module (srfi srfi-2)
  #:use-module (gliver core)
  #:use-module (gliver contrib utils systemd)
  #:export (
            xdg-desktop-log-environment
            xdg-desktop-ensure-environment!
            xdg-desktop-restart-portals!
            xdg-desktop-update-activation-environment!
))

(define (xdg-desktop-log-environment phase)
  "Log key XDG and Wayland session environment variables (for debugging).
Parameters: (PHASE)
  - PHASE: String label identifying the startup phase."
  (log-info "Session environment (~a): XDG_SESSION_TYPE=~s XDG_CURRENT_DESKTOP=~s WAYLAND_DISPLAY=~s DBUS_SESSION_BUS_ADDRESS=~s"
            phase
            (getenv "XDG_SESSION_TYPE")
            (getenv "XDG_CURRENT_DESKTOP")
            (getenv "WAYLAND_DISPLAY")
            (getenv "DBUS_SESSION_BUS_ADDRESS")))

(define (xdg-desktop-ensure-environment!)
  "Ensure XDG session environment variables are set to proper values."
  (let ((session-type    (getenv "XDG_SESSION_TYPE"))
        (current-desktop (getenv "XDG_CURRENT_DESKTOP")))
    (cond
     ;; if XDG_SESSION_TYPE is empty in environment variables
     ;; set it to "wayland"
     ((or (not session-type)
          (string-null? session-type))
      (setenv "XDG_SESSION_TYPE" "wayland"))
     ;; if XDG_SESSION_TYPE has a value other than "wayland"
     ;; leave it but warn user as this may cause compatibility issues
     ((not (string=? session-type "wayland"))
      (log-warn "XDG_SESSION_TYPE is ~s (expected \"wayland\"); this may cause issues."
                session-type)))

    (cond
     ;; if XDG_CURRENT_DESKTOP is empty in environment variables
     ;; set it to "gliver:river:wlr" (fallback gliver -> river -> wlr)
     ((or (not current-desktop) (string-null? current-desktop))
      (setenv "XDG_CURRENT_DESKTOP" "gliver:river:wlr"))
     ;; if it's set to something else that doesn't contain wlr
     ;; leave it but warn user
     ((not (member "wlr" (string-split current-desktop #\:)))
      (log-warn "XDG_CURRENT_DESKTOP is ~s (missing \"wlr\" fallback); this may cause issues."
                current-desktop)))))

(define* (xdg-desktop-restart-portals! #:optional (systemd? (systemd-session?)))
  "Restart xdg-desktop-portal processes.
Parameters: (SYSTEMD?)
  - SYSTEMD?: Boolean indicating whether the session uses systemd."
  (if systemd?
      (unless (systemd-restart '("xdg-desktop-portal"
                                 "xdg-desktop-portal-wlr")
                               #:no-block? #t)
        (log-warn "systemctl restart portals failed"))
      (when (executable-exists? "pkill")
        (let* ((status (system* "pkill" "-f"
                                "(^|/)xdg-desktop-portal(-wlr)?([[:space:]]|$)"))
               (rc (status:exit-val status)))
          ;; exit status 1 means no matching portal processes were running
          (unless (and rc (memv rc '(0 1)))
            (log-warn "pkill xdg-desktop-portal exited with status ~a" rc))))))

(define (xdg-desktop-update-activation-environment!)
  "Export session environment variables to D-Bus and systemd, then restart portals."
  (and-let* ((dbus-addr (getenv "DBUS_SESSION_BUS_ADDRESS"))
             ((not (string-null? dbus-addr)))
             ;; I believe there should be no distro missing this binary
             ((executable-exists? "dbus-update-activation-environment")))
    (let* ((systemd-dbus? (systemd-session?))
           (args (if systemd-dbus?
                     ;; for systemd, we have to pass --systemd to dbus
                     '("--systemd" "--all")
                     '("--all")))
           (status (apply system* "dbus-update-activation-environment" args))
           (rc (status:exit-val status)))
      (if (and rc (zero? rc))
          ;; restart portal after update variables, actually this isn't needed
          ;; during first session because dbus only reads these variables when
          ;; xdg-desktop-portal is called. However, if you exit the session and
          ;; login again, xdg-desktop-portal must be restart to respect new variables
          (xdg-desktop-restart-portals! systemd-dbus?)
          (log-warn "dbus-update-activation-environment exited with status ~a" rc)))))
