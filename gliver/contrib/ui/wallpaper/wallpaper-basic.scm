;;; gliver/contrib/ui/wallpaper/wallpaper-basic.scm --- Basic wallpaper manager
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module manages background desktop wallpapers for outputs using wlr-layer-shell.
;;; It handles:
;;;  - Initializing background layer surfaces on outputs
;;;  - Rendering solid colors and PNG images via Cairo to wl_shm buffers

(define-module (gliver contrib ui wallpaper wallpaper-basic)
  #:use-module (cairo)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-69)
  #:use-module (system foreign)
  #:use-module (gliver core)
  #:use-module (gliver deps libc)
  #:use-module (gliver deps color)
  #:use-module (gliver wayland gen wayland)
  #:use-module (gliver river connector)
  #:use-module (gliver river wlr-layer-shell)
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
			wallpaper-on-layer-surface-configure
			wallpaper-on-layer-surface-closed
			wallpaper-shm-buffer-create
			wallpaper-buffer-render
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
			wallpaper-enable!
			wallpaper-disable!
))

;;; state
(define-var *wallpaper-manager-enabled* #f
			"Whether the basic wallpaper manager is active.")

;;; output wallpaper state record:
;;  - output: associated <output> record
;;  - surface: wl_surface proxy handle for wallpaper layer
;;  - layer-surface: zwlr_layer_surface_v1 proxy handle
;;  - buffer: active wl_buffer proxy handle containing rendered Cairo surface
;;  - rendered-wallpaper: cached wallpaper spec (file path string or hex color)
;;  - rendered-width, rendered-height: pixel dimensions of rendered buffer
;;  - configured?: boolean indicating whether compositor configure event was acked
(define-record-type <wallpaper-output-state>
  (%make-wallpaper-output-state output surface layer-surface buffer
								rendered-wallpaper rendered-width rendered-height
								configured?)
  wallpaper-output-state?
  (output              wallpaper-output-state-output)
  (surface             wallpaper-output-state-surface             %wallpaper-output-state-surface-set!)
  (layer-surface       wallpaper-output-state-layer-surface       %wallpaper-output-state-layer-surface-set!)
  (buffer              wallpaper-output-state-buffer              %wallpaper-output-state-buffer-set!)
  (rendered-wallpaper  wallpaper-output-state-rendered-wallpaper  %wallpaper-output-state-rendered-wallpaper-set!)
  (rendered-width      wallpaper-output-state-rendered-width      %wallpaper-output-state-rendered-width-set!)
  (rendered-height     wallpaper-output-state-rendered-height     %wallpaper-output-state-rendered-height-set!)
  (configured?         wallpaper-output-state-configured?         %wallpaper-output-state-configured?-set!))

;; hash table: output-id -> <wallpaper-output-state>
(define-var *wallpaper-output-table* (make-hash-table)
  "Hash table mapping output-id to `<wallpaper-output-state>` records.")

(define (output-effective-wallpaper output)
  "Resolve the effective wallpaper path or color for OUTPUT.
Priority: workspace -> output -> global *wallpaper* -> #f.
Parameters: (OUTPUT)
  - OUTPUT: Target <output> record.
Returns effective wallpaper path or color for OUTPUT, or #f."
  (let* ((ws (output-workspace-current output))
         (ws-wp (and ws (workspace-wallpaper ws)))
         (out-wp (output-wallpaper output))
         (global-wp (var-get '*wallpaper*)))
    (or ws-wp out-wp global-wp #f)))

;;; internal helpers
(define (%wallpaper-find-state-by-layer-surface proxy)
  "Find `<wallpaper-output-state>` record associated with layer surface PROXY.
Parameters: (PROXY)
  - PROXY: Pointer to zwlr_layer_surface_v1 proxy.
Returns: `<wallpaper-output-state>` record if found, #f otherwise."
      (find (lambda (state)
              (equal? (wallpaper-output-state-layer-surface state)
					  proxy))
            (hash-table-values *wallpaper-output-table*)))

;;; layer surface event handlers
(define (wallpaper-on-layer-surface-configure data proxy serial width height)
  "Invoked when compositor configures layer surface.
Parameters: (DATA PROXY-LAYER-SURFACE SERIAL WIDTH HEIGHT)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-LAYER-SURFACE: Pointer to zwlr_layer_surface_v1 proxy.
  - SERIAL: Serial number of this configure event.
  - WIDTH: Allocated width of the layer surface in pixels.
  - HEIGHT: Allocated height of the layer surface in pixels."
  (and-let* ((state (%wallpaper-find-state-by-layer-surface proxy)))
    (wlr-layer-surface-ack-configure proxy serial)
    (let* ((output (wallpaper-output-state-output state))
           (prev-w (wallpaper-output-state-rendered-width state))
           (prev-h (wallpaper-output-state-rendered-height state))
           (prev-wp (wallpaper-output-state-rendered-wallpaper state))
           (eff-wp (output-effective-wallpaper output))
           (has-buf? (valid-pointer? (wallpaper-output-state-buffer state))))
      (%wallpaper-output-state-rendered-width-set! state width)
      (%wallpaper-output-state-rendered-height-set! state height)
      (%wallpaper-output-state-configured?-set! state #t)
      (if (and has-buf?
               (= prev-w width)
               (= prev-h height)
               (equal? prev-wp eff-wp))
          (log-debug "wallpaper: configure unchanged for ~a (~ax~a), skipping re-render"
                     (output-name output) width height)
          (wallpaper-render-and-commit! output state)))))

(define (wallpaper-on-layer-surface-closed data proxy)
  "Invoked when layer surface has been closed by compositor.
Parameters: (DATA PROXY-LAYER-SURFACE)
  - DATA: User context pointer (unused, null in Gliver).
  - PROXY-LAYER-SURFACE: Pointer to zwlr_layer_surface_v1 proxy."
  (and-let* ((state (%wallpaper-find-state-by-layer-surface proxy)))
    (log-info "wallpaper: layer surface closed for output ~a"
              (output-name (wallpaper-output-state-output state)))
    ;; clear layer surface pointer before cleanup to prevent double destruction
    (%wallpaper-output-state-layer-surface-set! state %null-pointer)
    (wallpaper-cleanup-output! (wallpaper-output-state-output state))))

;;; rendering and buffer management
(define (wallpaper-shm-buffer-create width height fill-proc!)
  "Allocate a shared memory wl_buffer of size WIDTHxHEIGHT and populate it with FILL-PROC!.
Parameters: (WIDTH HEIGHT FILL-PROC!)
  - WIDTH: Buffer width in pixels.
  - HEIGHT: Buffer height in pixels.
  - FILL-PROC!: Procedure `(lambda (ptr bv stride size width height) ...)` to paint pixels.
Returns: Wayland buffer foreign pointer on success, %null-pointer on failure."
  (if (or (<= width 0) (<= height 0) (not (valid-pointer? *wl-shm*)))
      %null-pointer
      (let* (;; pixel size is 4 bytes (ARGB8888)
             ;; stride is width * pixel size
             (stride (* width 4))
             ;; total size is stride * height
             (size (* stride height))
             ;; allocate an anonymous shared memory file descriptor
             (fd (memfd-create "gliver-wallpaper-shm" *mfd-cloexec*)))
        (if (< fd 0)
            (begin
              (log-error "wallpaper: failed to create memfd for buffer")
              %null-pointer)
            (begin
              ;; allocate size needed for the image in file descriptor
              (ftruncate fd size)
              ;; map the file descriptor into process virtual memory
              (let ((ptr (mmap %null-pointer size *prot-read-write* *map-shared* fd 0)))
                (if (null-pointer? ptr)
                    (begin
                      (close-fd fd)
                      (log-error "wallpaper: failed to mmap buffer")
                      %null-pointer)
                    (let ((fill-ok?
                           (catch #t
                             (lambda ()
                               (let ((bv (pointer->bytevector ptr size)))
                                 ;; fill the bytevector using Cairo
                                 (fill-proc! ptr bv stride size width height)
                                 #t))
                             (lambda (key . args)
                               (log-error "wallpaper: error filling buffer with Cairo: ~a ~a" key args)
                               #f))))
                      ;; unmap virtual memory (does not affect fd)
                      (catch #t (lambda () (munmap ptr size)) (lambda _ #f))
                      ;; create wayland buffer and close file descriptor
                      (if (not fill-ok?)
                          (begin
                            (close-fd fd)
                            %null-pointer)
                          (catch #t
                            (lambda ()
                              (let* ((pool (wl-shm-create-pool *wl-shm* fd size))
                                     (buffer (wl-shm-pool-create-buffer pool 0 width height stride WL_SHM_FORMAT_ARGB8888)))
                                (wl-shm-pool-destroy pool)
                                (close-fd fd)
                                buffer))
                            (lambda (k . args)
                              (close-fd fd)
                              (log-error "wallpaper: error creating wl_buffer: ~a ~a" k args)
                              %null-pointer)))))))))))

(define (wallpaper-buffer-render wp width height)
  "Create and return a wl_buffer containing the rendered wallpaper WP for size WIDTHxHEIGHT using Cairo.
Parameters: (WP WIDTH HEIGHT)
  - WP: Wallpaper specification (file path string or hex color string).
  - WIDTH: Target width in pixels.
  - HEIGHT: Target height in pixels.
Returns: Wayland buffer foreign pointer on success, %null-pointer on failure."
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
             ;; mark rendered so image loading is skipped
             (set! rendered? #t))))

       ;; wallpaper is image file path
       (when (and (not rendered?) (string? wp))
         ;; expand tilde prefix if present since Cairo expects an absolute path
         ;; TODO: currently only PNG images are supported; expand to other formats
         (let* ((path (if (string-prefix? "~" wp)
                          (string-append (or (getenv "HOME") "") (substring wp 1))
                          wp)))
           (when (file-exists? path)
             ;; create image surface using Cairo
             (let ((src-surface (catch #t
                                  (lambda () (cairo-image-surface-create-from-png path))
                                  (lambda (k . args)
                                    (log-warn "wallpaper: failed to load image ~a: ~a ~a" path k args)
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

       ;; fallback: solid background color if nothing was rendered
       (unless rendered?
         (paint-bg!))

       (cairo-surface-flush dst-surface)
       (cairo-destroy cr)
       (cairo-surface-destroy dst-surface)))))

(define (wallpaper-render-and-commit! output state)
  "Render the effective wallpaper for OUTPUT and commit the surface.
Parameters: (OUTPUT STATE)
  - OUTPUT: Output record to render wallpaper for.
  - STATE: `<wallpaper-output-state>` record holding current surface handles and cached dimensions."
  (let* ((wp (output-effective-wallpaper output))
         (width (wallpaper-output-state-rendered-width state))
         (height (wallpaper-output-state-rendered-height state))
         (surface (wallpaper-output-state-surface state)))
    (when (and (> width 0) (> height 0) (valid-pointer? surface))
      (log-info "wallpaper: rendering wallpaper ~s for output ~a (~ax~a)"
                 wp (output-name output) width height)
      (let ((new-buffer (wallpaper-buffer-render wp width height))
            (old-buffer (wallpaper-output-state-buffer state)))
        (when (valid-pointer? new-buffer)
          ;; cache new wallpaper buffer and rendered wallpaper spec
          (%wallpaper-output-state-buffer-set! state new-buffer)
          (%wallpaper-output-state-rendered-wallpaper-set! state wp)

          ;; Wayland surface commit cycle: attach, damage, and commit
          (wl-surface-attach surface new-buffer 0 0)
          (wl-surface-damage surface 0 0 width height)
          (wl-surface-commit surface)

          ;; cleanup previous buffer
          (when (and (valid-pointer? old-buffer)
                     (not (equal? old-buffer new-buffer)))
            (wl-buffer-destroy old-buffer)))))))

;;; lifecycle and update procedures
(define (wallpaper-init-output! output)
  "Initialize the background layer surface for OUTPUT.
Parameters: (OUTPUT)
  - OUTPUT: Output record to initialize wallpaper surface for."
  (unless (or (not *wallpaper-manager-enabled*)
              (not (valid-pointer? *wl-compositor*))
              (not (valid-pointer? *wlr-layer-shell*))
              (not (valid-pointer? *wl-shm*)))

    ;; only initialize if the output isn't initialized already
    (let ((existing (hash-table-ref/default *wallpaper-output-table* (output-id output) #f)))
      (unless existing
        ;; retrieve Wayland output proxy
        (let ((wl-out (output-wl-output-proxy output)))
          (when (valid-pointer? wl-out)
            (log-info "wallpaper: initializing wallpaper surface for output ~a..." (output-name output))
            (let ((surface (wl-compositor-create-surface *wl-compositor*)))
              (if (not (valid-pointer? surface))
                  (log-error "wallpaper: failed to create surface for output ~a" (output-name output))
                  (let ((layer-surf
                         (catch #t
                           (lambda ()
                             (wlr-layer-shell-get-layer-surface
                              *wlr-layer-shell*
                              surface
                              wl-out
                              *wlr-layer-shell-layer-background*
                              "gliver-wallpaper"))
                           (lambda (k . args)
                             (log-error "wallpaper: failed to create layer surface: ~a ~a" k args)
                             #f))))
                    (if (not (valid-pointer? layer-surf))
                        (catch #t (lambda () (wl-surface-destroy surface)) (lambda _ #f))
                        (let* ((init-width (output-width output))
                               (init-height (output-height output))
                               (state (%make-wallpaper-output-state
                                       output surface layer-surf %null-pointer
                                       #f init-width init-height #f)))

                          ;; configure layer surface geometry and behavior
                          (wlr-layer-surface-set-size layer-surf init-width init-height)
                          (wlr-layer-surface-set-anchor layer-surf *wlr-layer-shell-anchor-all*)
                          (wlr-layer-surface-set-exclusive-zone layer-surf -1)
                          (wlr-layer-surface-set-keyboard-interactivity
                           layer-surf *wlr-layer-shell-interactivity-none*)

                          ;; store state in output table before initial commit
                          (hash-table-set! *wallpaper-output-table* (output-id output) state)

                          ;; initial commit to trigger configure event from compositor
                          (wl-surface-commit surface))))))))))))

(define (wallpaper-on-output-created output prev-obj-id)
  "Handle new output creation event for OUTPUT.
Parameters: (OUTPUT PREV-OBJ-ID)
  - OUTPUT: Output record newly created or modified.
  - PREV-OBJ-ID: Previous Wayland object ID or #f."
  (when (and *wallpaper-manager-enabled* output)
	(wallpaper-init-output! output)))

(define (wallpaper-on-workspace-wallpaper-changed workspace wp)
  "Update output wallpaper when WORKSPACE wallpaper is modified to WP.
Parameters: (WORKSPACE WP)
  - WORKSPACE: Workspace record whose wallpaper changed.
  - WP: New wallpaper specification (file path string or hex color)."
  (when (and *wallpaper-manager-enabled* workspace)
    (let ((output (workspace-output workspace)))
      (when (and output (eq? (output-workspace-current output) workspace))
        (wallpaper-update-output! output)))))

(define (wallpaper-on-wallpaper-output-changed output wp)
  "Update OUTPUT wallpaper when output wallpaper is modified to WP.
Parameters: (OUTPUT WP)
  - OUTPUT: Output record whose wallpaper changed.
  - WP: New wallpaper specification (file path string or hex color)."
  (when (and *wallpaper-manager-enabled* output ws)
    (let* ((ws (output-workspace-current output))
           (ws-wp (and ws (workspace-wallpaper ws))))
      ;; only update if active workspace does not have an overriding wallpaper;
      ;; override priority: workspace > output > global
      (unless ws-wp
        (wallpaper-update-output! output)))))

(define (wallpaper-on-workspace-switch workspace prev-workspace)
  "Update output wallpaper when switching to WORKSPACE from PREV-WORKSPACE.
Parameters: (WORKSPACE PREV-WORKSPACE)
  - WORKSPACE: Workspace switched to.
  - PREV-WORKSPACE: Workspace switched from."
  (when (and *wallpaper-manager-enabled* workspace)
    (and-let* ((output (workspace-output workspace)))
      (wallpaper-update-output! output))))

(define (wallpaper-update-output! output)
  "Update the wallpaper rendered on OUTPUT if its effective wallpaper changed.
Parameters: (OUTPUT)
  - OUTPUT: Output record to update."
  (when (and *wallpaper-manager-enabled* output)
    (let ((state (hash-table-ref/default *wallpaper-output-table* (output-id output) #f)))
      (if (and state (wallpaper-output-state-configured? state))
          (let ((current-wp (output-effective-wallpaper output))
                (rendered-wp (wallpaper-output-state-rendered-wallpaper state)))
            (unless (equal? current-wp rendered-wp)
              (wallpaper-render-and-commit! output state)))
          (wallpaper-init-output! output)))))

(define (wallpaper-on-output-dimensions output prev-width prev-height)
  "Handle dimensions change event for OUTPUT from PREV-WIDTHxPREV-HEIGHT.
Parameters: (OUTPUT PREV-WIDTH PREV-HEIGHT)
  - OUTPUT: Output record whose dimensions changed.
  - PREV-WIDTH: Previous width in pixels.
  - PREV-HEIGHT: Previous height in pixels."
  (when (and *wallpaper-manager-enabled* output)
	(let ((state (hash-table-ref/default *wallpaper-output-table* (output-id output) #f)))
      (when state
		(let ((layer-surf (wallpaper-output-state-layer-surface state))
              (surface (wallpaper-output-state-surface state))
              (width (output-width output))
              (height (output-height output)))
          (when (valid-pointer? layer-surf)
			;; update layer surface size and commit surface
			(wlr-layer-surface-set-size layer-surf width height)
			(when (valid-pointer? surface)
              (wl-surface-commit surface))))))))

(define (wallpaper-cleanup-all!)
  "Clean up all wallpaper layer surfaces and state across all outputs.
Parameters: None."
  (for-each wallpaper-cleanup-output! (manager-outputs *manager*))
  (for-each (lambda (k) (hash-table-delete! *wallpaper-output-table* k))
            (hash-table-keys *wallpaper-output-table*)))

(define (wallpaper-cleanup-output! output)
  "Clean up layer shell resources and buffer state for OUTPUT.
Parameters: (OUTPUT)
  - OUTPUT: Output record to clean up."
  (when (and output (output? output))
    (let ((state (hash-table-ref/default *wallpaper-output-table* (output-id output) #f)))
      (when state
        (hash-table-delete! *wallpaper-output-table* (output-id output))
        (let ((layer-surf (wallpaper-output-state-layer-surface state))
              (surface (wallpaper-output-state-surface state))
              (buffer (wallpaper-output-state-buffer state)))
          (when (valid-pointer? layer-surf)
            (catch #t (lambda () (wlr-layer-surface-destroy layer-surf)) (lambda _ #f)))
          (when (valid-pointer? surface)
            (catch #t (lambda () (wl-surface-destroy surface)) (lambda _ #f)))
          (when (valid-pointer? buffer)
            (catch #t (lambda () (wl-buffer-destroy buffer)) (lambda _ #f))))))))

(define (wallpaper-update-all!)
  "Update wallpaper across all known outputs.
Parameters: None."
  (when (and *wallpaper-manager-enabled* *manager* (manager-state? *manager*))
    (for-each wallpaper-update-output! (manager-outputs *manager*))))

;;; commands
(define-command (wallpaper-set path-or-color)
  #:interactive (string)
  "Set the global wallpaper to PATH-OR-COLOR (image path or hex color).
Parameters: (PATH-OR-COLOR)
  - PATH-OR-COLOR: Image path string or hex color string."
  (var-set! *wallpaper* path-or-color)
  (gliver-hook-run! *wallpaper-changed-hook* path-or-color)
  (wallpaper-update-all!)
  (log-info "wallpaper: global wallpaper set to ~s" path-or-color))

(define-command (wallpaper-output-set path-or-color)
  #:interactive (string)
  "Set the wallpaper for the current output.
Parameters: (PATH-OR-COLOR)
  - PATH-OR-COLOR: Image path string or hex color string."
  (let ((output (output-current)))
    (when output
      (output-wallpaper-set! output path-or-color)
      (wallpaper-update-output! output)
      (log-info "wallpaper: output ~a set to ~s" (output-name output) path-or-color))))

(define-command (wallpaper-workspace-set path-or-color)
  #:interactive (string)
  "Set the wallpaper for the current workspace.
Parameters: (PATH-OR-COLOR)
  - PATH-OR-COLOR: Image path string or hex color string."
  (let ((workspace (workspace-current)))
    (when workspace
      (workspace-wallpaper-set! workspace path-or-color)
      (let ((out (workspace-output workspace)))
        (when out
          (wallpaper-update-output! out)))
      (log-info "wallpaper: workspace ~a set to ~s" (workspace-name workspace) path-or-color))))

(define-command (wallpaper-reload)
  "Force re-render of wallpapers across all outputs.
Parameters: None."
  (wallpaper-update-all!)
  (log-info "wallpaper: wallpapers reloaded"))

(define-command (wallpaper-enable!)
  "Enable and show the statusbar across configured outputs."
  (var-set! *wallpaper-manager-enabled* #t)
  (wallpaper-reload)
  (log-info "Enabling wallpaper-basic ..."))

(define-command (wallpaper-disable!)
  "Enable and show the statusbar across configured outputs."
  (var-set! *wallpaper-manager-enabled* #f)
  (log-info "Disabling wallpaper-basic ..."))

;;; hooks
(gliver-hook-add! *workspace-wallpaper-changed-hook* 'wallpaper-on-workspace-wallpaper-changed)
(gliver-hook-add! *output-wallpaper-changed-hook* 'wallpaper-on-wallpaper-output-changed)
(gliver-hook-add! *workspace-switch-hook* 'wallpaper-on-workspace-switch)
(gliver-hook-add! *output-object-id-changed-hook* 'wallpaper-on-output-created)
(gliver-hook-add! *output-dimensions-changed-hook* 'wallpaper-on-output-dimensions)
(gliver-hook-add! *gliver-globals-unbind-hook* 'wallpaper-cleanup-all!)
(gliver-hook-add! *output-destroy-hook* 'wallpaper-cleanup-output!)
(gliver-hook-add! *output-removed-hook* 'wallpaper-cleanup-output!)
(gliver-hook-add! %wlr-layer-surface-configure-hook 'wallpaper-on-layer-surface-configure)
(gliver-hook-add! %wlr-layer-surface-closed-hook 'wallpaper-on-layer-surface-closed)
