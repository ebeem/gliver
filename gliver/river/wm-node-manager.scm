;;; gliver/river/wm-node-manager.scm --- River compositor integration
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; Files under the river directory should only act as a wrapper
;;; to river protocol, they should not take action nor import any of
;;; gliver's files or utilities except for logging and configuration
;;;
;;; The render list is a list of nodes that determines the rendering order of
;;; the compositor. Nodes may correspond to windows or shell surfaces. The
;;; relative ordering of nodes may be changed with the place_above and
;;; place_below requests, changing the rendering order.

(define-module (gliver river wm-node-manager)
  #:use-module (gliver core types)
  #:use-module (gliver core logs)
  #:use-module (gliver core hooks)
  #:use-module (gliver wayland client)
  #:use-module (gliver wayland gen river-window-management-v1)
  #:use-module (system foreign)
  #:export (
			wm-node-destroy!
			wm-node-position-set!
			wm-node-place-top!
			wm-node-place-bottom!
			wm-node-place-above!
			wm-node-place-below!
))

;;; node requests
(define (wm-node-destroy! proxy-node)
  "Destroy a node, this means everything was cleared from
client side and the server should also clear everything.
This most likely should be used internally only, and it
will be automatically managed and called when needed."
  (when proxy-node
    (log-debug "destroying node proxy: ~a" proxy-node)
	  (river-node-v1-destroy proxy-node)))

(define (wm-node-position-set! proxy-node x y)
  "Set the absolute position of the node in the compositor's logical
coordinate space. The x and y coordinates may be positive or negative.
Must be called in a ~render_sequence~."
  (when proxy-node
    (log-debug "setting position of node ~a to ~ax~a" proxy-node x y)
    (river-node-v1-set-position proxy-node x y)))

(define (wm-node-place-top! proxy-node)
  "Request that the node is placed above all other nodes in the render list
Must be called in a ~render_sequence~."
  (when proxy-node
    (log-debug "placing node ~a at top" proxy-node)
    (river-node-v1-place-top proxy-node)))

(define (wm-node-place-bottom! proxy-node)
  "Request that the node is placed below all other nodes in the render list
Must be called in a ~render_sequence~."
  (when proxy-node
    (log-debug "placing node ~a at bottom" proxy-node)
    (river-node-v1-place-bottom proxy-node)))

(define (wm-node-place-above! proxy-node proxy-other)
  "Request that the node is placed above the other node in the render list
Must be called in a ~render_sequence~."
  (when (and proxy-node proxy-other)
    (log-debug "placing node ~a above node ~a" proxy-node proxy-other)
    (river-node-v1-place-above proxy-node proxy-other)))

(define (wm-node-place-below! proxy-node proxy-other)
  "Request that the node is placed below the other node in the render list
Must be called in a ~render_sequence~."
  (when (and proxy-node proxy-other)
    (log-debug "placing node ~a below node ~a" proxy-node proxy-other)
    (river-node-v1-place-below proxy-node proxy-other)))

