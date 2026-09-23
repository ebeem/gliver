;;; gliver/contrib/ui/gleui/base.scm --- Essential GUI Framework Base for Gliver Gleui
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver contrib ui gleui base)
  #:use-module (cairo)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (gliver core)
  #:use-module (gliver deps libc)
  #:use-module (gliver deps color)
  #:use-module (gliver deps pango)
  #:use-module (gliver deps libxkbcommon)
  #:use-module (gliver wayland client)
  #:use-module (gliver wayland gen wayland)
  #:use-module (gliver wayland gen wlr-layer-shell-unstable-v1)
  #:use-module (gliver river connector)
  #:declarative? #f
  #:export (
			gleui-shm-slot-stride
			gleui-shm-slot-height
			gleui-shm-slot-width
			gleui-shm-slot-size
			gleui-shm-slot-bv
			gleui-shm-slot-ptr
			gleui-shm-slot-buffer
			gleui-shm-slot?
			gleui-shm-slot-create
			gleui-shm-slot-destroy
			gleui-shm-slots-ensure!
			gleui-numpad->anchor
			gleui-layer-surface-create
			gleui-layer-surface-destroy!
			gleui-buffer-commit!
			gleui-draw-rounded-rect
			gleui-theme-get
			gleui-pango-escape
			gleui-pango-markup-strip
			gleui-pango-markup-split
			gleui-text-chunk-highlight
			gleui-markup-string-highlight
			gleui-word-char?
			gleui-word-backward-pos
			gleui-word-forward-pos
			*gleui-repeat-mutex*
			*gleui-repeat-cond*
			*gleui-repeat-active?*
			*gleui-repeat-state*
			*gleui-repeat-params*
			*gleui-repeat-handler*
			*gleui-repeat-rate*
			*gleui-repeat-delay*
			*gleui-repeat-active-pred*
			*gleui-repeat-seq*
			*gleui-repeat-worker-started?*
			gleui-repeat-worker-start!
			gleui-repeat-worker-stop!
			gleui-repeat-worker-signal!
))

;;; persistent double-buffered shm pool
(define-record-type <gleui-shm-slot>
  (%make-shm-slot buffer ptr bv size width height stride)
  gleui-shm-slot?
  (buffer gleui-shm-slot-buffer)
  (ptr    gleui-shm-slot-ptr)
  (bv     gleui-shm-slot-bv)
  (size   gleui-shm-slot-size)
  (width  gleui-shm-slot-width)
  (height gleui-shm-slot-height)
  (stride gleui-shm-slot-stride))

(define (gleui-shm-slot-create width height)
  "Allocate a persistent SHM buffer slot of size WIDTH x HEIGHT."
  (if (or (<= width 0) (<= height 0) (not (pointer? *wl-shm*)) (null-pointer? *wl-shm*))
      #f
      (let* ((stride (* width 4))
             (size (* stride height))
             (fd (memfd-create "gliver-gleui-shm" *mfd-cloexec*)))
        (if (< fd 0)
            #f
            (begin
              (ftruncate fd size)
              (let ((ptr (mmap %null-pointer size *prot-read-write* *map-shared* fd 0)))
                (if (null-pointer? ptr)
                    (begin (close-fd fd) #f)
                    (let* ((bv (pointer->bytevector ptr size))
                           (pool (wl-shm-create-pool *wl-shm* fd size))
                           (buf (wl-shm-pool-create-buffer pool 0 width height stride WL_SHM_FORMAT_ARGB8888)))
                      (wl-shm-pool-destroy pool)
                      (close-fd fd)
                      (%make-shm-slot buf ptr bv size width height stride)))))))))

(define (gleui-shm-slot-destroy slot)
  "Release and unmap a persistent SHM buffer slot."
  (when (and slot (gleui-shm-slot? slot))
    (let ((ptr (gleui-shm-slot-ptr slot))
          (size (gleui-shm-slot-size slot))
          (buf (gleui-shm-slot-buffer slot)))
      (when (and ptr (pointer? ptr) (not (null-pointer? ptr)) (> size 0))
        (catch #t (lambda () (munmap ptr size)) (lambda _ #f)))
      (when (and buf (pointer? buf) (not (null-pointer? buf)))
        (catch #t (lambda () (wl-buffer-destroy buf)) (lambda _ #f))))))

(define (gleui-shm-slots-ensure! slots w h)
  "Ensure SLOTS is a pair of valid SHM slots of size W x H."
  (let* ((slot0 (and (pair? slots) (car slots)))
         (slot1 (and (pair? slots) (cdr slots)))
         (needs-realloc?
          (or (not slot0) (not slot1)
              (not (= (gleui-shm-slot-width slot0) w))
              (not (= (gleui-shm-slot-height slot0) h)))))
    (if needs-realloc?
        (begin
          (when slot0 (gleui-shm-slot-destroy slot0))
          (when slot1 (gleui-shm-slot-destroy slot1))
          (let ((s0 (gleui-shm-slot-create w h))
                (s1 (gleui-shm-slot-create w h)))
            (cons s0 s1)))
        slots)))

(define (gleui-numpad->anchor n)
  "Convert numpad position (1-9) used in gliver to zwlr_layer_surface_v1 anchor bitmask."
  (case n
    ((7) (+ ZWLR_LAYER_SURFACE_V1_ANCHOR_TOP ZWLR_LAYER_SURFACE_V1_ANCHOR_LEFT))
    ((8) ZWLR_LAYER_SURFACE_V1_ANCHOR_TOP)
    ((9) (+ ZWLR_LAYER_SURFACE_V1_ANCHOR_TOP ZWLR_LAYER_SURFACE_V1_ANCHOR_RIGHT))
    ((4) ZWLR_LAYER_SURFACE_V1_ANCHOR_LEFT)
    ((5) 0) ;; centered
    ((6) ZWLR_LAYER_SURFACE_V1_ANCHOR_RIGHT)
    ((1) (+ ZWLR_LAYER_SURFACE_V1_ANCHOR_BOTTOM ZWLR_LAYER_SURFACE_V1_ANCHOR_LEFT))
    ((2) ZWLR_LAYER_SURFACE_V1_ANCHOR_BOTTOM)
    ((3) (+ ZWLR_LAYER_SURFACE_V1_ANCHOR_BOTTOM ZWLR_LAYER_SURFACE_V1_ANCHOR_RIGHT))
    (else ZWLR_LAYER_SURFACE_V1_ANCHOR_TOP)))

(define* (gleui-layer-surface-create #:key
                                     (namespace "gleui")
                                     (layer ZWLR_LAYER_SHELL_V1_LAYER_OVERLAY)
                                     (anchor 0)
                                     (margin-top 0)
                                     (margin-right 0)
                                     (margin-bottom 0)
                                     (margin-left 0)
                                     (exclusive-zone -1)
                                     (keyboard-interactivity ZWLR_LAYER_SURFACE_V1_KEYBOARD_INTERACTIVITY_NONE)
                                     (width 0)
                                     (height 0)
                                     (on-created #f)
                                     (on-configure #f)
                                     (on-close #f))
  "Create and configure a Wayland surface and zwlr_layer_surface_v1.
Returns (values surface layer-surface) or (values #f #f) on failure."
  (if (or (not *connected*)
          (not (pointer? *wl-compositor*))
          (null-pointer? *wl-compositor*)
          (not (pointer? *zwlr-layer-shell*))
          (null-pointer? *zwlr-layer-shell*))
      (values #f #f)
      (let* ((output (output-current))
             (output-wl (and output (output-wl-output output)))
             (output-if (and output-wl
							 (number? output-wl)
							 (gliver-wl-registry-bind *wl-registry* output-wl *wl-output-interface* 4)))
             (surface (wl-compositor-create-surface *wl-compositor*))
             (layer-surface (zwlr-layer-shell-v1-get-layer-surface
							 *zwlr-layer-shell*
							 surface
							 (if (and output-if (pointer? output-if)) output-if %null-pointer)
							 layer
							 namespace)))
        (zwlr-layer-surface-v1-set-size layer-surface width height)
        (zwlr-layer-surface-v1-set-anchor layer-surface anchor)
        (zwlr-layer-surface-v1-set-margin layer-surface margin-top margin-right margin-bottom margin-left)
        (zwlr-layer-surface-v1-set-exclusive-zone layer-surface exclusive-zone)
        (zwlr-layer-surface-v1-set-keyboard-interactivity layer-surface keyboard-interactivity)

        (when (procedure? on-created)
          (on-created surface layer-surface))

        (let ((listener
               (make-zwlr-layer-surface-v1-listener
                (lambda (data proxy serial w h)
                  (zwlr-layer-surface-v1-ack-configure proxy serial)
                  (when (procedure? on-configure)
                    (on-configure w h)))
                (lambda (data proxy)
                  (when (procedure? on-close)
                    (on-close))))))
          (wl-proxy-add-listener layer-surface listener %null-pointer))
        (wl-surface-commit surface)
        (when *wl-display*
          (catch #t
            (lambda () (wl-display-roundtrip *wl-display*))
            (lambda _ (wl-display-flush *wl-display*))))
        (values surface layer-surface))))

(define (gleui-layer-surface-destroy! surface layer-surface)
  "Destroy a LAYER-SURFACE and SURFACE."
  (when (and layer-surface (pointer? layer-surface) (not (null-pointer? layer-surface)))
    (catch #t (lambda () (zwlr-layer-surface-v1-destroy layer-surface)) (lambda _ #f)))
  (when (and surface (pointer? surface) (not (null-pointer? surface)))
    (catch #t (lambda () (wl-surface-destroy surface)) (lambda _ #f)))
  (when *wl-display*
    (catch #t (lambda () (wl-display-flush *wl-display*)) (lambda _ #f))))

(define (gleui-buffer-commit! surface buffer width height)
  "Attach buffer to SURFACE, damage, and commit."
  (when (and surface (pointer? surface) (not (null-pointer? surface))
             buffer (pointer? buffer) (not (null-pointer? buffer)))
    (wl-surface-attach surface buffer 0 0)
    (wl-surface-damage surface 0 0 width height)
    (wl-surface-commit surface)
    (when *wl-display*
      (wl-display-flush *wl-display*))))

(define (gleui-draw-rounded-rect cr x y w h r)
  "Draw a rounded rectangle path with radius R."
  (let* ((r-clamped (min (exact->inexact r) (/ w 2.0) (/ h 2.0)))
         (degrees (/ (acos -1) 180.0)))
    (cairo-new-sub-path cr)
    (cairo-arc cr (+ x w (- r-clamped)) (+ y r-clamped) r-clamped (* -90.0 degrees) 0.0)
    (cairo-arc cr (+ x w (- r-clamped)) (+ y h (- r-clamped)) r-clamped 0.0 (* 90.0 degrees))
    (cairo-arc cr (+ x r-clamped) (+ y h (- r-clamped)) r-clamped (* 90.0 degrees) (* 180.0 degrees))
    (cairo-arc cr (+ x r-clamped) (+ y r-clamped) r-clamped (* 180.0 degrees) (* 270.0 degrees))
    (cairo-close-path cr)))

(define (gleui-theme-get overrides key default-val)
  "Lookup KEY in theme OVERRIDES, falling back to DEFAULT-VAL (or resolving if a symbol).
This should always be used to allow users to customize themes of components while falling back
to global theme configuration."
  (or (and overrides (assq-ref overrides key))
      (if (symbol? default-val)
          (catch #t (lambda () (var-get default-val)) (lambda _ #f))
          default-val)))

(define (gleui-pango-escape str)
  "Escapes XML/Pango characters in STR."
  (pango-escape-markup str))

(define (gleui-pango-markup-strip str)
  "Remove Pango markup tags from STR."
  (regexp-substitute/global #f "<[^>]*>" str 'pre 'post))

(define (gleui-pango-markup-split str)
  "Split STR into an alternating list of (text . is-tag?) segments."
  (let ((len (string-length str)))
    (let loop ((pos 0) (acc '()))
      (cond
       ;; end of string
       ((>= pos len)
        (reverse acc))

       ;; start of a tag, should find the closing (>)
       ((char=? (string-ref str pos) #\<)
        (let ((close-idx (string-index str #\> pos)))
          (if close-idx
              (let ((end (+ close-idx 1)))
                (loop end (cons (cons (substring str pos end) #t) acc)))
              ;; Unterminated tag: take the rest of the string as a tag segment
              (reverse (cons (cons (substring str pos len) #t) acc)))))

       ;; plain text, find a start of a tag (<)
       (else
        (let ((next-tag (string-index str #\< pos)))
          (if next-tag
              (loop next-tag (cons (cons (substring str pos next-tag) #f) acc))
              ;; Trailing text to end of string
              (reverse (cons (cons (substring str pos len) #f) acc)))))))))

(define (gleui-text-chunk-highlight txt terms color)
  "Highlight occurrences of TERMS in TXT with COLOR using Pango markup."
  ;; pre-sanitize and lowercase terms
  (let* ((clean-terms (filter-map (lambda (t)
                                    (let ((s (string-downcase t)))
                                      (if (string-null? s) #f s)))
                                  terms))
         (txt-len (string-length txt)))
    (if (or (zero? txt-len) (null? clean-terms))
        (pango-escape-markup txt)
        (let ((txt-lower (string-downcase txt)))
          ;; find the earliest match among all search terms
          (define (find-next-match pos)
            (let loop ((candidates clean-terms)
                       (best #f))       ; (pos . len)
              (if (null? candidates)
                  best
                  (let* ((needle (car candidates))
                         (nlen (string-length needle))
                         (found (string-contains txt-lower needle pos)))
                    (loop (cdr candidates)
                          (cond
                           ((not found) best)
                           ((not best) (cons found nlen))
                           ;; pick earlier match
                           ((< found (car best)) (cons found nlen))
                           ((and (= found (car best)) (> nlen (cdr best))) (cons found nlen))
                           (else best)))))))

          ;; accumulating string chunks in reverse
          (let loop ((pos 0) (acc '()))
            (let ((m (find-next-match pos)))
              (if (not m)
                  (string-concatenate-reverse
                   (cons (pango-escape-markup (substring txt pos txt-len)) acc))
                  (let* ((m-start (car m))
                         (m-end   (+ m-start (cdr m)))
                         (prefix  (pango-escape-markup (substring txt pos m-start)))
                         (matched (pango-escape-markup (substring txt m-start m-end)))
                         (span    (format #f "<span color='~a' weight='bold'>~a</span>"
                                          color matched)))
                    (loop m-end (cons* span prefix acc))))))))))

(define (gleui-markup-string-highlight markup-str query color)
  "Highlight search QUERY terms in MARKUP-STR using COLOR without corrupting XML tags."
  (if (or (string-null? query) (string-null? (string-trim-both query)))
      markup-str
      (let* ((terms (filter (lambda (s) (> (string-length s) 0))
                            (string-split query #\space)))
             (segments (gleui-pango-markup-split markup-str)))
        (string-join
         (map (lambda (seg)
                (let ((chunk (car seg))
                      (is-tag? (cdr seg)))
                  (if is-tag?
                      chunk
                      (gleui-text-chunk-highlight chunk terms color))))
              segments)
         ""))))

(define (gleui-word-char? chr)
  "Return #t if CHR is considered a character."
  (or (char-alphabetic? chr)
      (char-numeric? chr)
      (char=? chr #\_)
      (char=? chr #\-)))

(define (gleui-word-backward-pos str pos)
  "Find start position of the word preceding POS in STR."
  ;; skip backward over any trailing non-word characters
  (define (skip-non-word i)
    (cond
     ((<= i 0) 0)
     ((not (gleui-word-char? (string-ref str (1- i))))
      (skip-non-word (1- i)))
     (else (skip-word i))))

  ;; skip backward over the actual word characters
  (define (skip-word i)
    (cond
     ((<= i 0) 0)
     ((gleui-word-char? (string-ref str (1- i)))
      (skip-word (1- i)))
     (else i)))

  (skip-non-word (min (max 0 pos) (string-length str))))

(define (gleui-word-forward-pos str pos)
  "Find end position of the word following POS in STR."
  (let ((len (string-length str)))
    ;; skip forward over any leading non-word characters
    (define (skip-non-word i)
      (cond
       ((>= i len) len)
       ((not (gleui-word-char? (string-ref str i)))
        (skip-non-word (1+ i)))
       (else (skip-word i))))

    ;; skip forward over the actual word characters
    (define (skip-word i)
      (cond
       ((>= i len) len)
       ((gleui-word-char? (string-ref str i))
        (skip-word (1+ i)))
       (else i)))

    (skip-non-word (min (max 0 pos) len))))

;;; persistent key repeat worker thread
(define-var *gleui-repeat-mutex* (make-mutex))
(define-var *gleui-repeat-cond* (make-condition-variable))
(define-var *gleui-repeat-active?* #f)
(define-var *gleui-repeat-state* #f)
(define-var *gleui-repeat-params* #f)
(define-var *gleui-repeat-handler* #f)
(define-var *gleui-repeat-rate* 25)
(define-var *gleui-repeat-delay* 600)
(define-var *gleui-repeat-active-pred* #f)
(define-var *gleui-repeat-seq* 0)
(define-var *gleui-repeat-worker-started?* #f)

(define (gleui-repeat-worker-start!)
  "Start single persistent worker thread for key repeating."
  (define (start-worker-thread!)
    (call-with-new-thread
     (lambda ()

       ;; wait for activation and capture a snapshot of configuration
       (define (wait-for-activation)
         (with-mutex *gleui-repeat-mutex*
           (while (not *gleui-repeat-active?*)
             (wait-condition-variable *gleui-repeat-cond* *gleui-repeat-mutex*))
           (list *gleui-repeat-seq*
                 *gleui-repeat-state*
                 *gleui-repeat-params*
                 *gleui-repeat-handler*
                 *gleui-repeat-active-pred*
                 (max 1 (or *gleui-repeat-rate* 25))
                 (max 50 (or *gleui-repeat-delay* 600)))))

       ;; verify the current repeat generation is still active
       (define (still-active? seq pred)
         (with-mutex *gleui-repeat-mutex*
           (and *gleui-repeat-active?*
                (= *gleui-repeat-seq* seq)
                (or (not (procedure? pred))
                    (catch #t pred (lambda _ #f))))))

       ;; main worker lifecycle
       (let loop ()
         (match-let (((seq state params handler active-pred rate delay-ms)
                      (wait-for-activation)))

           ;; initial key-repeat delay (milliseconds -> microseconds)
           (usleep (* delay-ms 1000))

           ;; repeat pulse loop while the key is continuously held
           (let ((interval-us (inexact->exact (round (/ 1000000.0 rate)))))
             (let repeat-loop ()
               (when (still-active? seq active-pred)
                 (when (procedure? handler)
                   (catch #t (lambda () (handler state params)) (lambda _ #f)))
                 (usleep interval-us)
                 (repeat-loop)))))

         (loop)))))

  ;; ensure worker starts atomically once
  (with-mutex *gleui-repeat-mutex*
    (unless *gleui-repeat-worker-started?*
      (set! *gleui-repeat-worker-started?* #t)
      (start-worker-thread!))))

(define (gleui-repeat-worker-stop!)
  "Stop active key repeat."
  (with-mutex *gleui-repeat-mutex*
    (set! *gleui-repeat-seq* (1+ *gleui-repeat-seq*))
    (set! *gleui-repeat-active?* #f)
    (set! *gleui-repeat-state* #f)
    (set! *gleui-repeat-handler* #f)
    (set! *gleui-repeat-active-pred* #f)))

(define* (gleui-repeat-worker-signal! state params handler #:key (rate 25) (delay 600) (active-pred #f))
  "Signal the key repeat worker to start repeating PARAMS via HANDLER."
  (with-mutex *gleui-repeat-mutex*
    (set! *gleui-repeat-active?* #t)
    (set! *gleui-repeat-state* state)
    (set! *gleui-repeat-params* params)
    (set! *gleui-repeat-handler* handler)
    (set! *gleui-repeat-rate* rate)
    (set! *gleui-repeat-delay* delay)
    (set! *gleui-repeat-active-pred* active-pred)
    (signal-condition-variable *gleui-repeat-cond*)))
