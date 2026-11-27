;;; gliver/contrib/ui/gleui/base/shm.scm --- Double-Buffered Wayland SHM Pool
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module manages double-buffered memfd and wl_shm pixel pools
;;; In Wayland and CPU graphics (Cairo), wl_shm (Wayland Shared Memory) has to
;;; be used. The client draws pixels to a buffer and hand it to the compositor
;;; which takes care of displaying it on screen. Double-burrefering is used to
;;; avoid visual teardown as client may write to buffer while compositor is still
;;; reading and displaying it. Transient single-shot surfaces are ones that
;;; rarely change such as wallpapers and tooltips.
;;;
;;; It handles:
;;;  - `<gleui-shm-slot>' allocation, CPU mmap cleanup, and wl_buffer teardown
;;;  - Persistent double-buffered and transient single-shot surface commits
;;;  - Fractional scale handling during Cairo surface rendering

(define-module (gliver contrib ui gleui base shm)
  #:use-module (cairo)
  #:use-module (system foreign)
  #:use-module (srfi srfi-9)
  #:use-module (gliver core)
  #:use-module (gliver deps libc)
  #:use-module (gliver wayland client)
  #:use-module (gliver wayland gen wayland)
  #:use-module (gliver river connector)
  #:use-module (gliver river wlr-layer-shell)
  #:use-module (gliver contrib ui gleui base surface)
  #:declarative? #f
  #:export (
            <gleui-shm-slot>
            gleui-shm-slots-destroy!
            gleui-shm-render-commit!
))

;; persistent SHM slot record
;;  - buffer: pointer to Wayland wl_buffer
;;  - ptr: pointer to mmap
;;  - bv: bytevector view over the mapped pixel memory
;;  - size: byte size (stride * height)
;;  - width: physical buffer width in pixels
;;  - height: physical buffer height in pixels
;;  - stride: byte stride (width * 4 for ARGB8888)
(define-record-type <gleui-shm-slot>
  (%make-shm-slot buffer ptr bv size width height stride)
  %shm-slot?
  (buffer %shm-slot-buffer %shm-slot-buffer-set!)
  (ptr    %shm-slot-ptr    %shm-slot-ptr-set!)
  (bv     %shm-slot-bv     %shm-slot-bv-set!)
  (size   %shm-slot-size)
  (width  %shm-slot-width)
  (height %shm-slot-height)
  (stride %shm-slot-stride))

(define (%sanitize-memfd-name name)
  "Normalize NAME into a valid, length-bounded memfd identifier string.
name should be < 200 characters and doesn't contain slash character (/)
Parameters: (NAME)
  - NAME: String or #f candidate SHM region identifier.
Returns sanitized non-empty memfd name string."
  (let* ((raw     (if (and (string? name) (> (string-length name) 0))
                      name
                      "gliver-gleui-shm"))
         (cleaned (string-map (lambda (c) (if (char=? c #\/) #\- c)) raw))
         (max-len 200))
    (if (> (string-length cleaned) max-len)
        (substring cleaned 0 max-len)
        cleaned)))

(define* (%shm-slot-create width height #:key (shm-name "gliver-gleui-shm"))
  "Allocate a persistent SHM buffer slot of WIDTHxHEIGHT pixels.
Parameters: (WIDTH HEIGHT [#:SHM-NAME \"gliver-gleui-shm\"])
  - WIDTH: Integer physical pixel buffer width.
  - HEIGHT: Integer physical pixel buffer height.
  - SHM-NAME: String or #f descriptive identifier passed to memfd_create for debugging.
Returns <gleui-shm-slot> record, or #f on invalid dimensions or allocation failure."
  (if (or (<= width 0)
          (<= height 0)
          (not (valid-pointer? *wl-shm*)))
      #f
      (let* ((stride (* width 4))
             (size (* stride height))
             (fd (memfd-create (%sanitize-memfd-name shm-name) *mfd-cloexec*)))
        (if (< fd 0)
            ;; fd < 0 means it wasn't created successfully
            #f
            (begin
              ;; extend fd size to required one based on dimensions
              (ftruncate fd size)
              ;; map the fd to client address space as writable shared memory
              (let ((ptr (mmap %null-pointer size *prot-read-write* *map-shared* fd 0)))
                (if (null-pointer? ptr)
                    (begin (close-fd fd) #f)      ;; mapping failed, return #f
                    (catch #t
                      (lambda ()
                        (let* ((bv   (pointer->bytevector ptr size))
                               ;; send fd to create a pool in the compositor
                               (pool (wl-shm-create-pool *wl-shm* fd size))
                               ;; use the pool to create a wl_buffer
                               (buf  (wl-shm-pool-create-buffer pool 0 width height
                                                                stride WL_SHM_FORMAT_ARGB8888)))
                          ;; at this point, there's no need for the pool or fd as both client
                          ;; and compositor have access to the page via bv and buf
                          (wl-shm-pool-destroy pool)
                          (close-fd fd)
                          (%make-shm-slot buf ptr bv size width height stride)))
                      (lambda (k . args)
                        ;; cleanup fd and unmap the address pointer
                        (log-error "gleui: shm-slot-create failed: ~a ~a" k args)
                        (catch #t (lambda () (munmap ptr size)) (lambda _ #f))
                        (close-fd fd)
                        #f)))))))))

(define (%shm-slot-unmap! slot)
  "Unmap CPU memory for SHM SLOT (Wayland wl_buffer is preserved).
This is used for surfaces that don't change their buffers quite often.
It clear the client-side pointer and bytevector while keeping wl_buffer.
Parameters: (SLOT)
  - SLOT: <gleui-shm-slot> record or #f."
  (when (and slot (%shm-slot? slot))
    (let ((ptr  (%shm-slot-ptr slot))
          (size (%shm-slot-size slot)))
      (%shm-slot-ptr-set! slot %null-pointer)
      (%shm-slot-bv-set! slot #f)
      (when (and (valid-pointer? ptr) (> size 0))
        (catch #t (lambda () (munmap ptr size)) (lambda _ #f))))))

(define (%shm-slot-destroy slot)
  "Unmap the memory and destroy the Wayland buffer for SHM SLOT.
This will unmap the memory and clears the wl_buffer as well.
Parameters: (SLOT)
  - SLOT: <gleui-shm-slot> record or #f."
  (when (and slot (%shm-slot? slot))
    (%shm-slot-unmap! slot)
    (let ((buf (%shm-slot-buffer slot)))
      (%shm-slot-buffer-set! slot %null-pointer)
      (when (valid-pointer? buf)
        (catch #t (lambda () (wl-buffer-destroy buf)) (lambda _ #f))))))

(define* (%shm-slots-ensure! slots w h #:key (shm-name "gliver-gleui-shm"))
  "Ensure SLOTS holds two active SHM buffers matching pixel size W by H.
Parameters: (SLOTS W H [#:SHM-NAME \"gliver-gleui-shm\"])
  - SLOTS: Pair of <gleui-shm-slot> records, or #f.
  - W: Integer required physical pixel width.
  - H: Integer required physical pixel height.
  - SHM-NAME: String or #f descriptive memfd identifier.
Returns pair of <gleui-shm-slot> records, or #f if allocation failed."
  (let* ((slot0 (and (pair? slots) (car slots)))
         (slot1 (and (pair? slots) (cdr slots)))
         ;; any slot is missing or has no bytevector or doesn't match size
         (needs-realloc?
          (or (not slot0) (not slot1)
              (not (%shm-slot-bv slot0)) (not (%shm-slot-bv slot1))
              (not (= (%shm-slot-width slot0) w))
              (not (= (%shm-slot-height slot0) h)))))
    (if needs-realloc?
        (begin
          ;; in case a reallocation is needed, destroy both slots and
          ;; recreate them with desired width and height
          (when slot0 (%shm-slot-destroy slot0))
          (when slot1 (%shm-slot-destroy slot1))
          (let ((new0 (%shm-slot-create w h #:shm-name shm-name))
                (new1 (%shm-slot-create w h #:shm-name shm-name)))
            (if (and new0 new1)
                (cons new0 new1)
                (begin
                  ;; if allocation failed, destroy whatever was created
                  (when new0 (%shm-slot-destroy new0))
                  (when new1 (%shm-slot-destroy new1))
                  #f))))
        slots)))

(define (gleui-shm-slots-destroy! slots)
  "Destroy both persistent SHM buffer SLOTS pair if allocated.
Parameters: (SLOTS)
  - SLOTS: Pair of <gleui-shm-slot> records, or #f."
  (when (pair? slots)
    (when (car slots) (%shm-slot-destroy (car slots)))
    (when (cdr slots) (%shm-slot-destroy (cdr slots)))))

(define (%shm-slot-draw-and-commit! surface slot width height scale
                                    draw-proc layer-surface update-size? pre-commit)
  "Paint DRAW-PROC into SHM SLOT and commit the resulting buffer to SURFACE.
Parameters: (SURFACE SLOT WIDTH HEIGHT SCALE DRAW-PROC
LAYER-SURFACE UPDATE-SIZE? PRE-COMMIT)
  - SURFACE: Pointer to Wayland wl_surface.
  - SLOT: <gleui-shm-slot> record with active mapped memory.
  - WIDTH: Integer logical surface width in pixels.
  - HEIGHT: Integer logical surface height in pixels.
  - SCALE: Float surface output scale factor.
  - DRAW-PROC: Procedure called as (DRAW-PROC CR WIDTH HEIGHT).
  - LAYER-SURFACE: Pointer to zwlr_layer_surface_v1, or #f.
  - UPDATE-SIZE?: Boolean deciding whether LAYER-SURFACE size is updated.
  - PRE-COMMIT: Procedure or #f zero-argument thunk called prior to commit.
Returns the committed wl_buffer foreign pointer."
  (let* ((bv       (%shm-slot-bv slot))
         (w-scaled (%shm-slot-width slot))
         (h-scaled (%shm-slot-height slot))
         (stride   (%shm-slot-stride slot))
         (wl-buf   (%shm-slot-buffer slot))
         (dst-surf (cairo-image-surface-create-for-data bv 'argb32 w-scaled h-scaled stride))
         (cr       (cairo-create dst-surf)))
    (dynamic-wind
      (lambda () #f)
      (lambda ()
        (when (and (number? scale) (> scale 0))
          (cairo-scale cr (exact->inexact scale) (exact->inexact scale)))
        (draw-proc cr width height)
        (cairo-surface-flush dst-surf))
      (lambda ()
        (cairo-destroy cr)
        (cairo-surface-destroy dst-surf)))
    (when (and update-size? (valid-pointer? layer-surface))
      (wlr-layer-surface-set-size layer-surface width height))
    (when (procedure? pre-commit)
      (pre-commit))
    (gleui-layer-surface-buffer-commit! surface wl-buf width height)
    wl-buf))

(define* (gleui-shm-render-commit! surface slots active-idx width height draw-proc
                                   #:key (layer-surface #f) (update-size? #t)
                                   (pre-commit #f) (persistent? #t)
                                   (shm-name "gliver-gleui-shm"))
  "Render WIDTH by HEIGHT logical pixels via DRAW-PROC into SHM SLOTS and commit SURFACE.
Parameters: (SURFACE SLOTS ACTIVE-IDX WIDTH HEIGHT DRAW-PROC [#:LAYER-SURFACE #f]
[#:UPDATE-SIZE? #t] [#:PRE-COMMIT #f] [#:PERSISTENT? #t] [#:SHM-NAME \"gliver-gleui-shm\"])
  - SURFACE: Pointer to Wayland wl_surface.
  - SLOTS: Pair of <gleui-shm-slot> records, or #f.
  - ACTIVE-IDX: Integer or #f currently presented slot index (0 or 1).
  - WIDTH: Integer logical surface width in pixels.
  - HEIGHT: Integer logical surface height in pixels.
  - DRAW-PROC: Procedure called as (DRAW-PROC CR WIDTH HEIGHT) with fractional scaling applied.
  - LAYER-SURFACE: Pointer to zwlr_layer_surface_v1, or #f, resized when UPDATE-SIZE? is #t.
  - UPDATE-SIZE?: Boolean updating layer-surface size when #t.
  - PRE-COMMIT: Procedure or #f zero-argument thunk called immediately prior to buffer commit.
  - PERSISTENT?: Boolean retaining double-buffered mappings when #t, or unmapping immediately after commit when #f.
  - SHM-NAME: String or #f descriptive memfd name used when allocating backing SHM slots.
Returns three values: NEW-SLOTS, NEXT-IDX, and WL-BUFFER (or #f if skipped)."
  (if (and (valid-pointer? surface)
           (valid-pointer? *wl-shm*)
           (> width 0) (> height 0))
      (let* ((scale    (surface-scale surface 1.0))
             (w-scaled (inexact->exact (round (* width scale))))
             (h-scaled (inexact->exact (round (* height scale))))
             (draw-slot!
              (lambda (slot on-error)
                (catch #t
                  (lambda ()
                    (%shm-slot-draw-and-commit!
                     surface slot width height scale
                     draw-proc layer-surface update-size? pre-commit))
                  (lambda (k . args)
                    (on-error)
                    (apply throw k args))))))
        (if persistent?
            (let ((valid-slots (%shm-slots-ensure! slots w-scaled h-scaled #:shm-name shm-name)))
              (if (and (pair? valid-slots)
                       (car valid-slots)
                       (cdr valid-slots))
                  (let* ((next-idx  (if (zero? (or active-idx 0)) 1 0))
                         (curr-slot (if (zero? next-idx) (car valid-slots) (cdr valid-slots)))
                         (wl-buf    (draw-slot! curr-slot
                                                (lambda ()
                                                  (unless (eq? valid-slots slots)
                                                    (gleui-shm-slots-destroy! valid-slots))))))
                    (values valid-slots next-idx wl-buf))
                  (values valid-slots active-idx #f)))
            (let ((slot (%shm-slot-create w-scaled h-scaled #:shm-name shm-name)))
              (if slot
                  (let ((wl-buf (draw-slot! slot (lambda () (%shm-slot-destroy slot)))))
                    ;; release the CPU mmap right after commit while keeping only wl_buffer
                    (%shm-slot-unmap! slot)
                    (gleui-shm-slots-destroy! slots)
                    (values (cons slot #f) 0 wl-buf))
                  (values slots active-idx #f)))))
      (values slots active-idx #f)))
