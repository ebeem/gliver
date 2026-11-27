;;; gliver/contrib/ui/gleui/toast/state.scm --- Toast Variables & State Record
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module defines configuration variables, state record, and instances
;;; It handles:
;;;  - Defining toast customization variables
;;;  - Exposing <gleui-toast-state> view record
;;;  - Registering toast Instances

(define-module (gliver contrib ui gleui toast state)
  #:use-module (srfi srfi-9)
  #:use-module (gliver core)
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
            *gleui-toast-instances*
            gleui-toast-get-instance
            <gleui-toast-state>
            gleui-make-toast-state
            gleui-toast-state?
            gleui-toast-state-surface
            gleui-toast-state-surface-set!
            gleui-toast-state-layer-surface
            gleui-toast-state-layer-surface-set!
            gleui-toast-state-shm-slots
            gleui-toast-state-shm-slots-set!
            gleui-toast-state-shm-active-idx
            gleui-toast-state-shm-active-idx-set!
            gleui-toast-state-configured?
            gleui-toast-state-configured?-set!
            gleui-toast-state-visible?
            gleui-toast-state-visible?-set!
            gleui-toast-state-width
            gleui-toast-state-width-set!
            gleui-toast-state-height
            gleui-toast-state-height-set!
            gleui-toast-state-output
            gleui-toast-state-content
            gleui-toast-state-content-set!
            gleui-toast-state-name
            gleui-toast-state-theme-overrides
            gleui-toast-state-render-mutex
            gleui-toast-state-icon-surface
            gleui-toast-state-icon-surface-set!
            gleui-toast-state-on-close
            gleui-toast-state-on-close-set!
            gleui-toast-state-actions
            gleui-toast-state-hit-boxes
            gleui-toast-state-hit-boxes-set!
))

(define-var *gleui-toast-font* #:inherit *theme-font*
            "Font family used for gleui toast notifications.")
(define-var *gleui-toast-font-size* 12
            "Font size used for gleui toast notifications.")
(define-var *gleui-toast-bg-color* #:inherit *theme-bg-main*
            "Background color for gleui toast notifications.")
(define-var *gleui-toast-fg-color* #:inherit *theme-fg-main*
            "Text color for gleui toast notifications.")
(define-var *gleui-toast-border-color* #:inherit *theme-border-color*
            "Border color for gleui toast notifications.")
(define-var *gleui-toast-border-width* 2
            "Border width in pixels for gleui toast notifications.")
(define-var *gleui-toast-border-radius* #:inherit *theme-border-radius*
            "Corner border radius in pixels for gleui toast notifications.")
(define-var *gleui-toast-location* 9
            "Gliver position (1-9) for gleui toast notifications.")
(define-var *gleui-toast-margin* 90
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
(define-var *gleui-toast-icon-border-radius* #:inherit *theme-border-radius*
            "Border radius in pixels for icons in gleui toast notifications.")
(define-var *gleui-toast-icon-border-width* 0
            "Border width in pixels around icons in gleui toast notifications.")
(define-var *gleui-toast-icon-border-color* #:inherit *theme-border-color*
            "Border color around icons in gleui toast notifications.")
(define-var *gleui-toast-icon-size* 48
            "Icon dimensions (width and height) in pixels in gleui toast notifications.")
(define-var *gleui-toast-icon-theme* #f
            "Current icon theme name (#f = auto-detect from theme).")

;; NOTE: instances are used to isolate toasts, for example: which-key
;; isn't supposed to have multiple instances, because there should be
;; only one which-key toast shown at a time, this can be controlled via
;; instance name. Multiple notifications on the other hand can show up
;; on screen at the same time, so each should have its own instance and
;; its own toast name
(define-var *gleui-toast-instances* (make-hash-table)
            "Hash table mapping toast instance names to <gleui-toast-state>.")

(define* (gleui-toast-get-instance #:optional (name "default"))
  "Return the toast instance state associated with NAME.
Parameters: ([NAME \"default\"])
  - NAME: String toast instance name.
Returns <gleui-toast-state> record if found, or #f."
  (hash-ref *gleui-toast-instances* name #f))

;; toast view state record
;;  - surface: pointer to wl_surface for toast rendering
;;  - layer-surface: pointer to zwlr_layer_surface_v1 for positioning
;;  - shm-slots: pair (slot-a . slot-b) of double-buffered <gleui-shm-slot> records
;;  - shm-active-idx: integer active SHM slot index (0 or 1) alternated per commit
;;  - configured?: boolean indicating whether the layer surface has been configured
;;  - visible?: boolean indicating whether the toast instance is currently visible
;;  - width: integer width of the toast surface in pixels
;;  - height: integer height of the toast surface in pixels
;;  - output: record <output> where the toast is displayed
;;  - content: <gleui-widget>, string, list, or value passed to gleui-toast-show
;;  - name: string unique instance name (defaults to "default")
;;  - theme-overrides: alist of theme and layout overrides
;;  - render-mutex: muted for serializing SHM drawing and Wayland commits
;;  - icon-surface: loaded Cairo image surface for the toast icon
;;  - on-close: optional zero-argument callback invoked when the toast closes
;;  - actions: list of (label . callback) pairs for clickable action buttons
;;  - hit-boxes: list of collected <gleui-hit-box> records
(define-record-type <gleui-toast-state>
  (gleui-make-toast-state surface layer-surface shm-slots shm-active-idx configured? visible?
                          width height output content name
                          theme-overrides render-mutex icon-surface on-close actions hit-boxes)
  gleui-toast-state?
  (surface             gleui-toast-state-surface             gleui-toast-state-surface-set!)
  (layer-surface       gleui-toast-state-layer-surface       gleui-toast-state-layer-surface-set!)
  (shm-slots           gleui-toast-state-shm-slots           gleui-toast-state-shm-slots-set!)
  (shm-active-idx      gleui-toast-state-shm-active-idx      gleui-toast-state-shm-active-idx-set!)
  (configured?         gleui-toast-state-configured?         gleui-toast-state-configured?-set!)
  (visible?            gleui-toast-state-visible?            gleui-toast-state-visible?-set!)
  (width               gleui-toast-state-width               gleui-toast-state-width-set!)
  (height              gleui-toast-state-height              gleui-toast-state-height-set!)
  (output              gleui-toast-state-output)
  (content             gleui-toast-state-content             gleui-toast-state-content-set!)
  (name                gleui-toast-state-name)
  (theme-overrides     gleui-toast-state-theme-overrides)
  (render-mutex        gleui-toast-state-render-mutex)
  (icon-surface        gleui-toast-state-icon-surface        gleui-toast-state-icon-surface-set!)
  (on-close            gleui-toast-state-on-close            gleui-toast-state-on-close-set!)
  (actions             gleui-toast-state-actions)
  (hit-boxes           gleui-toast-state-hit-boxes           gleui-toast-state-hit-boxes-set!))
