;;; gliver/contrib/ui/gleui/widgets/button.scm --- Button Widget
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module defines the composite clickable pill button widget for gleui.
;;; NOTE: this is an interesting component that shows how eventually, if gluei
;;; grows and cover the important building blocks, creating new widgets becomes
;;; a matter of reusing the existing widgets to achieve the desired outcomes.
;;; button in this case is nothing but a clickable box with a text inside!

(define-module (gliver contrib ui gleui widgets button)
  #:use-module (gliver contrib ui gleui widgets engine)
  #:use-module (gliver contrib ui gleui widgets box)
  #:use-module (gliver contrib ui gleui widgets text)
  #:use-module (gliver contrib ui gleui widgets clickable)
  #:declarative? #f
  #:export (
            gleui-w-button
))

(define* (gleui-w-button label-or-child
                         #:key (on-click #f) (on-enter #f) (on-leave #f) (data #f)
                         (font #f) (font-size #f)
                         (color #f) (bg-color #f)
                         (border-color #f) (border-width 0)
                         (radius 6) (padding-x 10) (padding-y 4))
  "Create a clickable button.
Parameters: (LABEL-OR-CHILD [#:ON-CLICK #f] [#:ON-ENTER #f] [#:ON-LEAVE #f] [#:DATA #f] [#:FONT #f] [#:FONT-SIZE #f] [#:COLOR #f] [#:BG-COLOR #f] [#:BORDER-COLOR #f] [#:BORDER-WIDTH 0] [#:RADIUS 6] [#:PADDING-X 10] [#:PADDING-Y 4])
  - LABEL-OR-CHILD: String Pango markup text or <gleui-widget> child.
  - ON-CLICK: Procedure or #f callback invoked on pointer click.
  - ON-ENTER: Procedure or #f callback invoked as (ON-ENTER PX PY DATA) when pointer enters button.
  - ON-LEAVE: Procedure or #f callback invoked as (ON-LEAVE DATA) when pointer leaves button.
  - DATA: Payload forwarded to pointer callbacks.
  - FONT: String or #f font description used when LABEL-OR-CHILD is text.
  - FONT-SIZE: Integer or #f font size in points when LABEL-OR-CHILD is text.
  - COLOR: String or #f label hex color when LABEL-OR-CHILD is text.
  - BG-COLOR: String or #f pill background hex color.
  - BORDER-COLOR: String or #f pill border hex color.
  - BORDER-WIDTH: Integer pill border thickness in pixels.
  - RADIUS: Integer pill corner radius in pixels.
  - PADDING-X: Integer horizontal inner button padding in pixels.
  - PADDING-Y: Integer vertical inner button padding in pixels.
Returns <gleui-widget> clickable node."
  (let ((child (if (gleui-widget? label-or-child)
                   label-or-child
                   (gleui-w-text label-or-child
                                 #:font font
                                 #:font-size font-size
                                 #:color color
                                 #:markup? #t))))
    (gleui-w-clickable
     (gleui-w-box child
                  #:padding-x padding-x
                  #:padding-y padding-y
                  #:radius radius
                  #:bg-color bg-color
                  #:border-color border-color
                  #:border-width border-width
                  #:align 'center)
     #:on-click on-click
     #:on-enter on-enter
     #:on-leave on-leave
     #:data data)))
