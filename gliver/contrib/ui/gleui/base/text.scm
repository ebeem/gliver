;;; gliver/contrib/ui/gleui/base/text.scm --- Text Markup Helpers
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module provides Pango markup manipulation and cursor word-motion
;;; utilities for gleui.
;;; It handles:
;;;  - Pango XML markup escaping, stripping, detection, and query highlighting
;;;  - Word-boundary backward and forward cursor navigation (similar to Emacs)

(define-module (gliver contrib ui gleui base text)
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (gliver deps pango)
  #:declarative? #f
  #:export (
            gleui-pango-escape
            gleui-pango-markup-strip
            gleui-pango-has-markup-tags?
            gleui-pango-markup-ensure
            gleui-markup-string-highlight
            gleui-word-backward-pos
            gleui-word-forward-pos
))

(define (gleui-pango-escape str)
  "Escape XML/Pango markup special characters in STR.
Parameters: (STR)
  - STR: String input text to escape.
Returns XML-escaped string."
  (pango-escape-markup str))

(define (gleui-pango-markup-strip str)
  "Remove XML and Pango markup tags from STR.
Parameters: (STR)
  - STR: String input that may contain Pango markup tags.
Returns plain text string without markup tags."
  (regexp-substitute/global #f "<[^>]*>" str 'pre 'post))

(define (gleui-pango-has-markup-tags? str)
  "Check whether STR contains recognized Pango XML markup tags.
Parameters: (STR)
  - STR: String input to inspect for Pango tags.
Returns regex match value when markup tags are found, #f otherwise."
  (and (string? str)
       (string-match "<(b|i|u|s|tt|big|small|sub|sup|span)([[:space:]]|>)" str)))

(define (gleui-pango-markup-ensure str)
  "Ensure STR is valid Pango markup, escape non-markup strings.
Parameters: (STR)
  - STR: String to convert to Pango markup (can be object as well).
Returns valid Pango markup string."
  (if (string? str)
      (if (gleui-pango-has-markup-tags? str)
          str
          ;; only escape markup if the string doesn't have tags
          (pango-escape-markup str))
      (pango-escape-markup (format #f "~a" (or str "")))))

(define (%pango-markup-split str)
  "Split STR into an alternating list of text and XML tag segments.
Parameters: (STR)
  - STR: String Pango markup input.
Returns list of (SEGMENT . IS-TAG?) pairs in original order."
  (let ((len (string-length str)))
    (let loop ((pos 0) (acc '()))
      (if (>= pos len)
          (reverse acc)
          (let* ((is-tag? (char=? (string-ref str pos) #\<))
                 (end     (if is-tag?
                              ;; if we have a tag, find index of > and add 1
                              ;; if no index of > was found, match end of text
                              (or (and=> (string-index str #\> pos) 1+) len)
                              ;; if we don't have a tag, find index of <
                              ;; if no index of < was found, match end of text
                              (or (string-index str #\< pos) len))))
            (loop end (acons (substring str pos end) is-tag? acc)))))))

(define (%find-earliest-match haystack needles start)
  "Locate the earliest occurrence among NEEDLES in HAYSTACK at or after START.
Parameters: (HAYSTACK NEEDLES START)
  - HAYSTACK: String downcased text to search.
  - NEEDLES: List of non-empty downcased search strings.
  - START: Integer character index in HAYSTACK.
Returns pair (START-INDEX . MATCH-LENGTH), or #f if no needle matches."
  (define (better-match? a b)
    "Return #t when match pair A precedes B, breaking ties by longer length."
    (or (not b)
        (< (car a) (car b))
        (and (= (car a) (car b))
             (> (cdr a) (cdr b)))))

  (fold (lambda (needle best)
          (let ((found (string-contains haystack needle start)))
            (if found
                (let ((candidate (cons found (string-length needle))))
                  (if (better-match? candidate best) candidate best))
                best)))
        #f
        needles))

(define (%text-chunk-highlight txt terms color)
  "Highlight occurrences of TERMS inside plain text TXT using Pango span COLOR.
Parameters: (TXT TERMS COLOR)
  - TXT: String plain text segment to search and XML-escape.
  - TERMS: List of query token strings.
  - COLOR: String highlight foreground hex color.
Returns Pango-escaped string with matching spans highlighted."
  (define clean-terms (remove string-null? (map string-downcase terms)))
  (define txt-len (string-length txt))

  (define (escape-slice start end)
    "XML-escape the substring of TXT from START to END."
    (pango-escape-markup (substring txt start end)))

  (if (or (zero? txt-len) (null? clean-terms))
      (pango-escape-markup txt)
      (let ((txt-lower (string-downcase txt)))
        (let loop ((pos 0) (acc '()))
          (cond
           ((%find-earliest-match txt-lower clean-terms pos)
            => (lambda (m)
                 (let* ((m-start (car m))
                        (m-end   (+ m-start (cdr m)))
                        (prefix  (escape-slice pos m-start))
                        (span    (format #f "<span color='~a' weight='bold'>~a</span>"
                                         color (escape-slice m-start m-end))))
                   (loop m-end (cons* span prefix acc)))))
           (else
            (string-concatenate-reverse
             (cons (escape-slice pos txt-len) acc))))))))

(define (gleui-markup-string-highlight markup-str query color)
  "Highlight tokens from QUERY inside MARKUP-STR with COLOR while preserving XML tags.
Parameters: (MARKUP-STR QUERY COLOR)
  - MARKUP-STR: String Pango markup whose text segments are highlighted.
  - QUERY: String whitespace-delimited search query.
  - COLOR: String highlight hex color.
Returns updated Pango markup string."
  (if (or (string-null? query) (string-null? (string-trim-both query)))
      markup-str
      (let* ((terms    (filter (lambda (s) (> (string-length s) 0))
                               (string-split query #\space)))
             (segments (%pango-markup-split markup-str)))
        (string-join
         (map (lambda (seg)
                (if (cdr seg)
                    (car seg)
                    (%text-chunk-highlight (car seg) terms color)))
              segments)
         ""))))

(define (%word-char? chr)
  "Check whether CHR is an identifier or word constituent character.
Parameters: (CHR)
  - CHR: Character to inspect.
Returns #t if CHR is alphabetic, numeric, underscore, or hyphen; #f otherwise."
  (or (char-alphabetic? chr)
      (char-numeric? chr)
      (char=? chr #\_)
      (char=? chr #\-)))

(define (gleui-word-backward-pos str pos)
  "Find the starting index of the word preceding cursor POS in STR.
Parameters: (STR POS)
  - STR: String input text.
  - POS: Integer current character cursor index.
Returns new integer cursor index at the start of the previous word."
  (define (skip-non-word i)
    "Skip non-word characters backward starting from index I.
Parameters: (I)
  - I: Integer current character offset in STR.
Returns integer cursor index after skipping word delimiter and word characters."
    (cond
     ((<= i 0) 0)
     ((not (%word-char? (string-ref str (1- i)))) (skip-non-word (1- i)))
     (else (skip-word i))))
  (define (skip-word i)
    "Skip contiguous word characters backward starting from index I.
Parameters: (I)
  - I: Integer current character offset inside a word.
Returns integer cursor index at the start of the word."
    (cond
     ((<= i 0) 0)
     ((%word-char? (string-ref str (1- i))) (skip-word (1- i)))
     (else i)))
  (skip-non-word (min (max 0 pos) (string-length str))))

(define (gleui-word-forward-pos str pos)
  "Find the ending index of the word following cursor POS in STR.
Parameters: (STR POS)
  - STR: String input text.
  - POS: Integer current character cursor index.
Returns new integer cursor index after the next word."
  (let ((len (string-length str)))
    (define (skip-non-word i)
      "Skip non-word characters forward starting from index I.
Parameters: (I)
  - I: Integer current character offset in STR.
Returns integer cursor index after skipping delimiters and the following word."
      (cond
       ((>= i len) len)
       ((not (%word-char? (string-ref str i))) (skip-non-word (1+ i)))
       (else (skip-word i))))
    (define (skip-word i)
      "Skip contiguous word characters forward starting from index I.
Parameters: (I)
  - I: Integer current character offset inside a word.
Returns integer cursor index immediately after the word."
      (cond
       ((>= i len) len)
       ((%word-char? (string-ref str i)) (skip-word (1+ i)))
       (else i)))
    (skip-non-word (min (max 0 pos) len))))
