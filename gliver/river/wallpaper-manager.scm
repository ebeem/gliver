;;; gliver/river/wallpaper-manager.scm --- River wallpaper manager
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver river wallpaper-manager)
  #:use-module (cairo)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-69)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (gliver core)
  #:use-module (gliver deps libc)
  #:use-module (gliver deps color)
  #:use-module (gliver wayland client)
  #:use-module (gliver wayland gen wayland)
  #:use-module (gliver wayland gen wlr-layer-shell-unstable-v1)
  #:use-module (gliver river connector)
  #:declarative? #f
  #:export (
			*wallpaper-manager-enabled*
			wallpaper-output-state-listener
			wallpaper-output-state-configured?
			wallpaper-output-state-rendered-height
			wallpaper-output-state-rendered-width
			wallpaper-output-state-rendered-wallpaper
			wallpaper-output-state-buffer
			wallpaper-output-state-layer-surface
			wallpaper-output-state-surface
			wallpaper-output-state-output
			wallpaper-output-state?
			*wallpaper-output-table*
			wallpaper-render-and-commit!
			wallpaper-init-output!
			wallpaper-on-output-created
			wallpaper-on-workspace-wallpaper-changed
			wallpaper-on-wallpaper-output-changed
			wallpaper-on-workspace-switch
			wallpaper-update-output!
			wallpaper-on-output-dimensions
			wallpaper-cleanup-all!
			wallpaper-cleanup-output!
			wallpaper-update-all!
			wallpaper-set
			wallpaper-output-set
			wallpaper-workspace-set
			wallpaper-reload
			wallpaper-shm-buffer-create
			wallpaper-buffer-render
))

(define-var *wallpaper-manager-enabled* #t
			"Whether the background wallpaper manager is active.")

(define-record-type <wallpaper-output-state>
  (%make-wallpaper-output-state output surface layer-surface buffer
								rendered-wallpaper rendered-width rendered-height
								configured? listener)
  wallpaper-output-state?
  (output              wallpaper-output-state-output)
  (surface             wallpaper-output-state-surface             %wallpaper-output-state-surface-set!)
  (layer-surface       wallpaper-output-state-layer-surface       %wallpaper-output-state-layer-surface-set!)
  (buffer              wallpaper-output-state-buffer              %wallpaper-output-state-buffer-set!)
  (rendered-wallpaper  wallpaper-output-state-rendered-wallpaper  %wallpaper-output-state-rendered-wallpaper-set!)
  (rendered-width      wallpaper-output-state-rendered-width      %wallpaper-output-state-rendered-width-set!)
  (rendered-height     wallpaper-output-state-rendered-height     %wallpaper-output-state-rendered-height-set!)
  (configured?         wallpaper-output-state-configured?         %wallpaper-output-state-configured?-set!)
  (listener            wallpaper-output-state-listener            %wallpaper-output-state-listener-set!))

;; hash table: output-id -> <wallpaper-output-state>
(define *wallpaper-output-table* (make-hash-table))

(define (wallpaper-render-and-commit! output state)
  "Render the effective wallpaper for OUTPUT and commit the surface."
  (let* ((wp (output-effective-wallpaper output))
         (width (wallpaper-output-state-rendered-width state))
         (height (wallpaper-output-state-rendered-height state))
         (surface (wallpaper-output-state-surface state)))
    (when (and (> width 0) (> height 0) (pointer? surface) (not (null-pointer? surface)))
      (log-debug "Rendering wallpaper ~s for output ~a (~ax~a)"
                 wp (output-name output) width height)
      (let ((new-buffer (wallpaper-buffer-render wp width height))
            (old-buffer (wallpaper-output-state-buffer state)))
        (when (and (pointer? new-buffer) (not (null-pointer? new-buffer)))
		  ;; set the new wallpaper buffer and cache rendered wallpaper
          (%wallpaper-output-state-buffer-set! state new-buffer)
          (%wallpaper-output-state-rendered-wallpaper-set! state wp)

		  ;; wayland surface commit cycle, attach, mark dirty, and commit
          (wl-surface-attach surface new-buffer 0 0)
          (wl-surface-damage surface 0 0 width height)
          (wl-surface-commit surface)
          (when *wl-display*
            (wl-display-flush *wl-display*))

		  ;; cleanup old buffer
          (when (and (pointer? old-buffer) (not (null-pointer? old-buffer))
                     (not (equal? old-buffer new-buffer)))
            (wl-buffer-destroy old-buffer)))))))

(define (wallpaper-init-output! output)
  "Initialize the background layer surface for OUTPUT."
  (unless (or (not *wallpaper-manager-enabled*)
              (null-pointer? *wl-compositor*)
              (null-pointer? *zwlr-layer-shell*)
              (null-pointer? *wl-shm*))

	;; only initialize if the output isn't initialized already
    (let ((existing (hash-table-ref/default *wallpaper-output-table* (output-id output) #f)))
      (unless existing
		;; create new wallpaper surface
        (let* ((int-wl-out (output-wl-output output))
			   ;; get pointer from object-id (int-32) using global registry
			   ;; ensure int-wl-out is initialized (not #f nor invalid pointer)
               (wl-out (and int-wl-out
                            (number? int-wl-out)
                            (gliver-wl-registry-bind *wl-registry* int-wl-out *wl-output-interface* 4))))
          (when (and wl-out (pointer? wl-out) (not (null-pointer? wl-out)))
            (log-info "Initializing wallpaper surface for output ~a..." (output-name output))
            (let* ((surface (wl-compositor-create-surface *wl-compositor*))
                   (layer-surf (zwlr-layer-shell-v1-get-layer-surface
                                *zwlr-layer-shell*
                                surface
                                wl-out
                                ZWLR_LAYER_SHELL_V1_LAYER_BACKGROUND
                                "gliver-wallpaper"))
                   (anchor-all (+ ZWLR_LAYER_SURFACE_V1_ANCHOR_TOP
                                  ZWLR_LAYER_SURFACE_V1_ANCHOR_BOTTOM
                                  ZWLR_LAYER_SURFACE_V1_ANCHOR_LEFT
                                  ZWLR_LAYER_SURFACE_V1_ANCHOR_RIGHT))
                   (init-width (output-width output))
                   (init-height (output-height output))
                   (state (%make-wallpaper-output-state
                           output surface layer-surf %null-pointer
                           #f init-width init-height #f #f)))

			  ;; create a new wayland surface with the size of the output
              (zwlr-layer-surface-v1-set-size layer-surf init-width init-height)
			  ;; anchored to all 4 edges
              (zwlr-layer-surface-v1-set-anchor layer-surf anchor-all)
			  ;; doesn't reserve space (unlike top-bar)
              (zwlr-layer-surface-v1-set-exclusive-zone layer-surf -1)
			  ;; doesn't steal input/keyboard focus
              (zwlr-layer-surface-v1-set-keyboard-interactivity
               layer-surf ZWLR_LAYER_SURFACE_V1_KEYBOARD_INTERACTIVITY_NONE)

              (let ((listener
                     (make-zwlr-layer-surface-v1-listener
					  ;; on configured event
                      (lambda (data proxy serial width height)
                        (zwlr-layer-surface-v1-ack-configure proxy serial)
                        (let ((prev-w (wallpaper-output-state-rendered-width state))
                              (prev-h (wallpaper-output-state-rendered-height state))
                              (prev-wp (wallpaper-output-state-rendered-wallpaper state))
                              (eff-wp (output-effective-wallpaper output))
                              (has-buf? (and (wallpaper-output-state-buffer state)
                                             (not (null-pointer? (wallpaper-output-state-buffer state))))))
                          (%wallpaper-output-state-rendered-width-set! state width)
                          (%wallpaper-output-state-rendered-height-set! state height)
                          (%wallpaper-output-state-configured?-set! state #t)
                          (if (and has-buf?
                                   (= prev-w width)
                                   (= prev-h height)
                                   (equal? prev-wp eff-wp))
                              (log-debug "Wallpaper configure unchanged for ~a (~ax~a), skipping re-render"
                                         (output-name output) width height)
                              (begin
                                (log-info "Wallpaper layer surface configured for output ~a. width=~a, height=~a" (output-name output) width height)
                                (wallpaper-render-and-commit! output state)))))
					  ;; on closed event
                      (lambda (data proxy)
                        (log-info "Wallpaper layer surface closed for output ~a" (output-name output))
                        (wallpaper-cleanup-output! output)))))
                (%wallpaper-output-state-listener-set! state listener)
                (wl-proxy-add-listener layer-surf listener %null-pointer))

              ;; initial commit to trigger configure event from compositor
              (wl-surface-commit surface)
              (when *wl-display*
                (wl-display-flush *wl-display*))
              (hash-table-set! *wallpaper-output-table* (output-id output) state))))))))

(define (wallpaper-on-output-created output prev-obj-id)
  "Handle new output creation."
  (wallpaper-init-output! output))

(define (wallpaper-on-workspace-wallpaper-changed workspace wp)
  "Update output wallpaper when workspace wallpaper is modified."
  (when workspace
    (let ((output (workspace-output workspace)))
      (when (and output (eq? (output-workspace-current output) workspace))
        (wallpaper-update-output! output)))))

(define (wallpaper-on-wallpaper-output-changed output wp)
  "Update output wallpaper when output wallpaper is modified."
  (when output
    (let* ((ws (output-workspace-current output))
           (ws-wp (and ws (workspace-wallpaper ws))))
      ;; only update if active workspace does not have an overriding wallpaper
	  ;; the override order workspace > output > global
      (unless ws-wp
        (wallpaper-update-output! output)))))

(define (wallpaper-on-workspace-switch workspace prev-workspace)
  "Update output wallpaper when workspace focus changes."
  (when workspace
    (let ((output (workspace-output workspace)))
      (when output
        (wallpaper-update-output! output)))))

(define (wallpaper-update-output! output)
  "Update the wallpaper rendered on OUTPUT if its effective wallpaper changed."
  (when (and *wallpaper-manager-enabled* output)
    (let ((state (hash-table-ref/default *wallpaper-output-table* (output-id output) #f)))
      (if (and state (wallpaper-output-state-configured? state))
		  ;; if output is configured and has rendered wallpaper
          (let ((current-wp (output-effective-wallpaper output))
                (rendered-wp (wallpaper-output-state-rendered-wallpaper state)))
            (unless (equal? current-wp rendered-wp)
              (wallpaper-render-and-commit! output state)))
		  ;; if output isn't initialized yet
          (wallpaper-init-output! output)))))

(define (wallpaper-on-output-dimensions output prev-width prev-height)
  "Handle output dimensions change."
  (let ((state (hash-table-ref/default *wallpaper-output-table* (output-id output) #f)))
    (when state
      (let ((layer-surf (wallpaper-output-state-layer-surface state))
            (surface (wallpaper-output-state-surface state))
            (width (output-width output))
            (height (output-height output)))
        (unless (null-pointer? layer-surf)
		  ;; update the layer surface size and commit it
          (zwlr-layer-surface-v1-set-size layer-surf width height)
          (when (and (pointer? surface) (not (null-pointer? surface)))
            (wl-surface-commit surface)))))))

(define (wallpaper-cleanup-all!)
  "Clean up all wallpaper layer surfaces and state."
  (for-each wallpaper-cleanup-output! (manager-outputs *manager*))
  (for-each (lambda (k) (hash-table-delete! *wallpaper-output-table* k))
            (hash-table-keys *wallpaper-output-table*)))

(define (wallpaper-cleanup-output! output)
  "Clean up layer shell resources for OUTPUT."
  (when (and output (output? output))
    (let ((state (hash-table-ref/default *wallpaper-output-table* (output-id output) #f)))
      (when state
        (hash-table-delete! *wallpaper-output-table* (output-id output))
        (let ((layer-surf (wallpaper-output-state-layer-surface state))
              (surface (wallpaper-output-state-surface state))
              (buffer (wallpaper-output-state-buffer state)))
          (unless (null-pointer? layer-surf)
            (catch #t (lambda () (zwlr-layer-surface-v1-destroy layer-surf)) (lambda _ #f)))
          (unless (null-pointer? surface)
            (catch #t (lambda () (wl-surface-destroy surface)) (lambda _ #f)))
          (unless (null-pointer? buffer)
            (catch #t (lambda () (wl-buffer-destroy buffer)) (lambda _ #f))))))))

;; TODO: test the created commands
(define (wallpaper-update-all!)
  "Update wallpaper across all known outputs."
  (when (and *wallpaper-manager-enabled* *manager* (manager-state? *manager*))
    (for-each wallpaper-update-output! (manager-outputs *manager*))))

(define-command (wallpaper-set path-or-color)
  #:interactive (string)
  "Set the global wallpaper to PATH-OR-COLOR (image path or hex color)."
  (var-set! *wallpaper* path-or-color)
  (gliver-hook-run! *wallpaper-changed-hook* path-or-color)
  (wallpaper-update-all!)
  (log-info "Global wallpaper set to ~s" path-or-color))

(define-command (wallpaper-output-set path-or-color)
  #:interactive (string)
  "Set the wallpaper for the current output."
  (let ((output (output-current)))
    (when output
      (output-wallpaper-set! output path-or-color)
      (wallpaper-update-output! output)
      (log-info "Wallpaper for output ~a set to ~s" (output-name output) path-or-color))))

(define-command (wallpaper-workspace-set path-or-color)
  #:interactive (string)
  "Set the wallpaper for the current workspace."
  (let ((workspace (workspace-current)))
    (when workspace
      (workspace-wallpaper-set! workspace path-or-color)
      (let ((out (workspace-output workspace)))
        (when out
          (wallpaper-update-output! out)))
      (log-info "Wallpaper for workspace ~a set to ~s" (workspace-name workspace) path-or-color))))

(define-command (wallpaper-reload)
  "Force re-render of wallpapers across all outputs."
  (wallpaper-update-all!)
  (log-info "Wallpapers reloaded."))

(gliver-hook-add! *workspace-wallpaper-changed-hook* 'wallpaper-on-workspace-wallpaper-changed)
(gliver-hook-add! *output-wallpaper-changed-hook* 'wallpaper-on-wallpaper-output-changed)
(gliver-hook-add! *workspace-switch-hook* 'wallpaper-on-workspace-switch)
(gliver-hook-add! *output-object-id-changed-hook* 'wallpaper-on-output-created)
(gliver-hook-add! *output-dimensions-changed-hook* 'wallpaper-on-output-dimensions)
(gliver-hook-add! *gliver-globals-unbind-hook* 'wallpaper-cleanup-all!)
(gliver-hook-add! *output-destroy-hook* 'wallpaper-cleanup-output!)
(gliver-hook-add! *output-removed-hook* 'wallpaper-cleanup-output!)

(define (wallpaper-shm-buffer-create width height fill-proc!)
  "Allocate a shared memory wl_buffer of size WxH and populate it with FILL-PROC!.
Returns a wayland buffer foreign pointer."
  (if (or (<= width 0) (<= height 0) (not (pointer? *wl-shm*)) (null-pointer? *wl-shm*))
      %null-pointer
      (let* (;; pixel size is 4 bytes (ARGB8888)
			 ;; stride is width * pixel size
			 (stride (* width 4))
			 ;; total size is stride * height
             (size (* stride height))
			 ;; file an anonymous shared file descriptor
             (fd (memfd-create "gliver-wallpaper-shm" *mfd-cloexec*)))
        (if (< fd 0)
            (begin
              (log-error "Failed to create memfd for wallpaper buffer")
              %null-pointer)
            (begin
			  ;; allocate size needed for the image in file descriptor
              (ftruncate fd size)
			  ;; maps the file descriptor into process virtual memory
              (let ((ptr (mmap %null-pointer size *prot-read-write* *map-shared* fd 0)))
                (if (null-pointer? ptr)
                    (begin
                      (close-fd fd)
                      (log-error "Failed to mmap wallpaper buffer")
                      %null-pointer)
                    (begin
                      (let ((bv (pointer->bytevector ptr size)))
                        (catch #t
						  ;; fill the bytevector of the pointer using cairo
                          (lambda () (fill-proc! ptr bv stride size width height))
                          (lambda (key . args)
                            (log-error "Error filling wallpaper buffer with Cairo: ~a ~a" key args))))
					  ;; free the virtual memory (won't impact fd)
                      (munmap ptr size)
					  ;; create wayland buffer and close file descriptor
                      (let* ((pool (wl-shm-create-pool *wl-shm* fd size))
                             (buffer (wl-shm-pool-create-buffer pool 0 width height stride WL_SHM_FORMAT_ARGB8888)))
                        (wl-shm-pool-destroy pool)
                        (close-fd fd)
                        buffer)))))))))

(define (wallpaper-buffer-render wp width height)
  "Create and return a wl_buffer containing the rendered wallpaper WP for size WxH using Cairo."
  ;; create the shared memory wl_buffer by providing width, height, and painting callback
  (wallpaper-shm-buffer-create
   width height
   (lambda (ptr bv stride size width height)
     (let* ((dst-surface (cairo-image-surface-create-for-data bv 'argb32 width height stride))
            (cr (cairo-create dst-surface))
            (paint-bg! (lambda ()
                         (apply (lambda (r g b a) (cairo-set-source-rgba cr r g b a))
                                (parse-hex-color-rgba *theme-bg-main*))
                         (cairo-paint cr)))
            (paint-pattern! (lambda (src-surface extend-mode)
                              (let ((pat (cairo-pattern-create-for-surface src-surface)))
                                (when extend-mode (cairo-pattern-set-extend pat extend-mode))
                                (cairo-pattern-set-filter pat 'best)
                                (cairo-set-source cr pat)
                                (cairo-paint cr))))
            (rendered? #f))

       ;; wallpaper is hex color string RGB or RGBA format (e.g. "#ffffff")
       (when (and (string? wp) (hex-color? wp))
         (let ((col (parse-hex-color-rgba wp)))
           (when col
			 ;; paint the entire surface with the color
             (apply (lambda (r g b a)
					  (cairo-set-source-rgba cr r g b a)
					  (cairo-paint cr)) col)
			 ;; setting rendered to #t so we stop checking for image
             (set! rendered? #t))))

       ;; wallpaper is image file path
       (when (and (not rendered?) (string? wp))
		 ;; path will most likely have home prefix, cairo only accepts full-path
		 ;; the workaround is to convert the common unix relative paths to full-paths
		 ;; TODO: currently this only accepts .png, will be great to support other extensions
         (let* ((path (if (string-prefix? "~" wp)
                          (string-append (or (getenv "HOME") "") (substring wp 1))
                          wp)))
           (when (file-exists? path)
			 ;; create image surface using cairo
             (let ((src-surface (catch #t
                                  (lambda () (cairo-image-surface-create-from-png path))
                                  (lambda (k . args)
                                    (log-warn "Failed to load image ~a: ~a ~a" path k args)
                                    #f))))
               (when src-surface
                 (let* ((src-width (exact->inexact (cairo-image-surface-get-width src-surface)))
                        (src-height (exact->inexact (cairo-image-surface-get-height src-surface)))
                        (dst-width (exact->inexact width))
                        (dst-height (exact->inexact height))
                        (mode (or *wallpaper-mode* 'fill)))
                   (cond
                    ((eq? mode 'stretch)
                     (cairo-save cr)
                     (cairo-scale cr (/ dst-width src-width) (/ dst-height src-height))
                     (paint-pattern! src-surface #f)
                     (cairo-restore cr))

                    ((eq? mode 'center)
                     (paint-bg!)
                     (cairo-set-source-surface cr src-surface
                                               (/ (- dst-width src-width) 2.0)
                                               (/ (- dst-height src-height) 2.0))
                     (cairo-paint cr))

                    ((eq? mode 'tile)
                     (paint-pattern! src-surface 'repeat))

                    ;; 'fit or 'fill (default)
                    (else
                     (when (eq? mode 'fit) (paint-bg!))
                     (let* ((scale ((if (eq? mode 'fit) min max) (/ dst-width src-width) (/ dst-height src-height)))
                            (off-x (/ (- dst-width (* src-width scale)) 2.0))
                            (off-y (/ (- dst-height (* src-height scale)) 2.0)))
                       (cairo-save cr)
                       (cairo-translate cr off-x off-y)
                       (cairo-scale cr scale scale)
                       (paint-pattern! src-surface #f)
                       (cairo-restore cr))))
                   (cairo-surface-destroy src-surface)
                   (set! rendered? #t)))))))

       ;; fallback: solid background color if nothing is rendered
       (unless rendered?
         (paint-bg!))

       (cairo-surface-flush dst-surface)
       (cairo-destroy cr)
       (cairo-surface-destroy dst-surface)))))
