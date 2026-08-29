;;; gliver/deps/libc.scm --- Standard C library (libc) FFI bindings
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gliver deps libc)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:export (
			*mfd-cloexec*
			*mfd-allow-sealing*
			*prot-none*
			*prot-read*
			*prot-write*
			*prot-read-write*
			*prot-exec*
			*map-shared*
			*map-private*
			*map-anonymous*
			*libc*
			memfd-create
			ftruncate
			ftruncate-fd
			close-fd
			mmap
			mmap-memory
			munmap
			munmap-memory
			memcpy
			memset
))

(define %null-pointer (make-pointer 0))

(define *mfd-cloexec* 1)
(define *mfd-allow-sealing* 2)

(define *prot-none* 0)
(define *prot-read* 1)
(define *prot-write* 2)
(define *prot-read-write* 3)
(define *prot-exec* 4)

(define *map-shared* 1)
(define *map-private* 2)
(define *map-anonymous* #x20)

;; dynamic link to libc
(define *libc* (dynamic-link))

(define memfd-create
  (let ((proc (pointer->procedure int (dynamic-func "memfd_create" *libc*) (list '* uint32))))
    (lambda (name flags)
      (let ((ptr (if (string? name) (string->pointer name) name)))
        (proc ptr flags)))))

(define ftruncate
  (pointer->procedure int (dynamic-func "ftruncate" *libc*) (list int int64)))

(define (ftruncate-fd fd length)
  "Truncate a file to a specified LENGTH."
  (ftruncate fd length))

(define close-fd
  (pointer->procedure int (dynamic-func "close" *libc*) (list int)))

(define mmap
  (pointer->procedure '* (dynamic-func "mmap" *libc*) (list '* size_t int int int int64)))

(define* (mmap-memory length #:key (addr %null-pointer) (prot *prot-read-write*) (flags *map-shared*) (fd -1) (offset 0))
  "Map memory pages and return a foreign pointer."
  (mmap addr length prot flags fd offset))

(define munmap
  (pointer->procedure int (dynamic-func "munmap" *libc*) (list '* size_t)))

(define (munmap-memory addr length)
  "Unmap memory pages at ADDR with LENGTH."
  (munmap addr length))

(define memcpy
  (pointer->procedure '* (dynamic-func "memcpy" *libc*) (list '* '* size_t)))

(define memset
  (pointer->procedure '* (dynamic-func "memset" *libc*) (list '* int size_t)))
