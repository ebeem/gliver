;;; gliver/contrib/ui/gleui/palette/matching.scm --- Candidate Parsing & Matching
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This module parses, scores, filters, and formats candidate items for
;;; gleui's palette.
;;; It handles:
;;;  - Extracting display columns from candidate items
;;;  - Ranking queries and weighting candidates

(define-module (gliver contrib ui gleui palette matching)
  #:use-module (ice-9 format)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:use-module (gliver core)
  #:use-module (gliver contrib ui gleui base)
  #:use-module (gliver contrib ui gleui palette state)
  #:declarative? #f
  #:export (
            *palette-case-sensitive?*
            <palette-candidate>
            gleui-palette-parse-candidates
            gleui-palette-filter-candidates
            gleui-palette-make-dmenu-options
            gleui-palette-get-display-string
))

(define-var *palette-case-sensitive?* #f
            "Whether string filtering in the palette is case sensitive.")

;; pre-parsed candidate entry for fast searching and scoring
;; NOTE: while consuming more memory, caching values and lowercasing
;; the strings makes searching almost twice as fast. The more consumed
;; memory is low even with large entries (10k), the initial parsing tho
;; takes longer for this approach, and it scales the more candidates we have
;;  - primary: string primary column stripped of markup (first column)
;;  - primary-low: string lowercased primary column
;;  - secondary: string concatenated secondary columns (other columns)
;;  - secondary-low: string lowercased secondary columns
;;  - search: string full searchable metadata
;;  - search-low: string lowercased searchable metadata
;;  - index: integer candidate index
(define-record-type <palette-candidate>
  (%construct-palette-candidate primary primary-low secondary secondary-low search search-low index)
  %palette-candidate?
  (primary       %palette-candidate-primary)
  (primary-low   %palette-candidate-primary-low)
  (secondary     %palette-candidate-secondary)
  (secondary-low %palette-candidate-secondary-low)
  (search        %palette-candidate-search)
  (search-low    %palette-candidate-search-low)
  (index         %palette-candidate-index))

(define (%make-palette-candidate primary secondary search index)
  "Create a <palette-candidate> record with lowercased
fields from PRIMARY, SECONDARY, SEARCH, and INDEX.
Parameters: (PRIMARY SECONDARY SEARCH INDEX)
  - PRIMARY: String primary column stripped of markup.
  - SECONDARY: String concatenated secondary columns.
  - SEARCH: String full searchable text for the candidate.
  - INDEX: Integer candidate index.
Returns <palette-candidate> record."
  (%construct-palette-candidate primary (string-downcase primary)
                                secondary (string-downcase secondary)
                                search (string-downcase search)
                                index))

;; TODO: think about making a filter backend interface so other filter methods
;; can be supplied like `orderless`.

(define* (%safe-col-string col #:optional (width 80))
  "Helper to safely format COL into a string
Parameters: (COL [WIDTH 80])
  - COL: Value for the column.
  - WIDTH: Integer maximum printed width for structured objects
NOTE: this isn't meant to truncate visible text, width only resolves loops
Returns string representation of COL."
  ;; strings are returned as they are, other types are converted
  ;; to strings using format
  (cond
   ((string? col) col)
   ((or (number? col)
        (boolean? col)
        (symbol? col)
        (char? col)
        (null? col))
    (format #f "~a" col))
   (else
    ;; passing objects directly to format could loop on
    ;; circular references truncated-print prevents it handles that
    (or (false-if-exception
         (call-with-output-string
           (lambda (p) (truncated-print col p #:width width))))
        "#<object>"))))

(define (%split-non-empty-columns strs)
  "Split STRS into its first non-empty string paired
with the joined remaining strings.
Parameters: (STRS)
  - STRS: List of trimmed column strings.
Returns pair (FIRST-NON-EMPTY . REMAINING-JOINED)."
  ;; drop blank columns (such as leading spacer/icon cells) before
  ;; picking primary vs secondary
  (let ((non-empty (filter (lambda (s) (not (string-null? s))) strs)))
    (if (null? non-empty)
        (cons "" "")
        ;; first column is primary, all remaining columns joined are secondary
        (cons (car non-empty)
              (string-join (cdr non-empty) " ")))))

(define %col-split-rx (make-regexp "[ \t]{2,}|\t"))
(define (%extract-text-columns item)
  "Helper to extract primary and secondary plain-text columns from candidate ITEM.
Parameters: (ITEM)
  - ITEM: String, list, or value candidate item.
Returns pair (PRIMARY . SECONDARY) with Pango markup stripped."
  (cond
   ((list? item)
    ;; convert each item to string and strip markup for better scoring
    (%split-non-empty-columns
     (map (lambda (x)
            (string-trim-both
             (gleui-pango-markup-strip (%safe-col-string x))))
          item)))
   ((string? item)
    ;; strip Pango markup first, then collapse column separators
    ;; (2+ spaces or tab form regex) to #\t (tab)
    (let* ((no-markup (gleui-pango-markup-strip item))
           (delimited (regexp-substitute/global #f %col-split-rx
                                                no-markup 'pre "\t" 'post)))
      (%split-non-empty-columns
       (map string-trim-both (string-split delimited #\tab)))))
   (else
    ;; format arbitrary objects into a single primary column
    (cons (string-trim-both (%safe-col-string item)) ""))))

(define* (gleui-palette-parse-candidates candidates #:optional (search-strings #f))
  "Pre-parse CANDIDATES and optional SEARCH-STRINGS into <palette-candidate> records.
Parameters: (CANDIDATES [SEARCH-STRINGS #f])
  - CANDIDATES: List or vector of candidate items.
  - SEARCH-STRINGS: List, vector, or #f of custom search strings parallel to CANDIDATES.
Returns vector of <palette-candidate> records."
  ;; normalize candidates and search strings to lists
  (let* ((candidate-list     (if (vector? candidates)
                                 (vector->list candidates)
                                 (or candidates '())))
         (search-string-list (if (vector? search-strings)
                                 (vector->list search-strings)
                                 (or search-strings '())))
         (candidate-count    (length candidate-list))
         ;; parsed results will be a vector <palette-candidate>
         (parsed-candidates  (make-vector candidate-count #f)))
    ;; loop candidates, append a <palette-candidate> of each in `parsed-candidates`
    (let loop ((remaining-candidates     candidate-list)
               (remaining-search-strings search-string-list)
               (index                    0))
      (when (< index candidate-count)
        (let* ((candidate         (and (pair? remaining-candidates)
                                       (car remaining-candidates)))
               (search-string     (and (pair? remaining-search-strings)
                                       (car remaining-search-strings)))
               (extracted-columns (%extract-text-columns candidate))
               (primary           (car extracted-columns))
               (secondary         (cdr extracted-columns))
               ;; use caller's search string if given, otherwise use columns
               (search-text       (or (and (string? search-string)
                                           search-string)
                                      (if (and (string-null? primary)
                                               (string-null? secondary))
                                          ""
                                          (string-append primary " " secondary)))))
          (vector-set! parsed-candidates index
                       (%make-palette-candidate primary secondary search-text index))
          (loop (if (pair? remaining-candidates) (cdr remaining-candidates) '())
                (if (pair? remaining-search-strings) (cdr remaining-search-strings) '())
                (1+ index)))))
    parsed-candidates))

;; actual matching and sorting algorithm is implemented below
;; pretty sure there is a better approach, I had to work around this
;; because with simple filtering, there is no gurantee that "libre w"
;; will return librewolf as first result, it might show libreoffice writer
;; or even libreoffice draw, which is bad if quick search and enter is done
(define (%word-boundary? str pos)
  "Check whether POS in STR sits at a word start.
Parameters: (STR POS)
  - STR: String text being checked.
  - POS: Integer character index inside STR.
Returns #t if POS is a segment beginning in STR, #f otherwise."
  ;; to explain the logic better, I will show an example of each return
  ;; considering the STR is "librewolf browser"
  ;; return #t if match first character, query here will be "librew"
  (or (<= pos 0)
      (let* ((prev (string-ref str (1- pos)))
             (curr (if (< pos (string-length str)) (string-ref str pos) #\space)))
        ;; return #t if the prev character is whitespace, query here will be "browser"
        (or (char-whitespace? prev)
            ;; return #t also if previous character is any of the below
            ;; symbols which act like a whitespace or separator
            ;; for example, if our STR is "librewolf-browser" and query is "browser"
            (memv prev '(#\- #\_ #\. #\/ #\: #\( #\[ #\" #\'))
            ;; if prev is lower and current is upper, such as if
            ;; STR is LibreWolf and query is "wolf"
            (and (char-upper-case? curr)
                 (char-lower-case? prev))))))

(define (%score-term term candidate case-sensitive?)
  "Score CANDIDATE against query TERM.
NOTE: we read pre-lowercased fields as well because they are cached; this
      process repeats on each character typed and would stall on large lists
      if lowercased every time.
Parameters: (TERM CANDIDATE CASE-SENSITIVE?)
  - TERM: String query token.
  - CANDIDATE: <palette-candidate> record.
  - CASE-SENSITIVE?: Boolean controlling case sensitivity.
Returns integer relevance score, or #f when TERM does not match."
  ;; pick original strings when case-sensitive, or pre-lowercased when insensitive
  (let* ((primary             (%palette-candidate-primary candidate))
         (secondary           (%palette-candidate-secondary candidate))
         (search              (%palette-candidate-search candidate))
         (effective-primary   (if case-sensitive? primary (%palette-candidate-primary-low candidate)))
         (effective-secondary (if case-sensitive? secondary (%palette-candidate-secondary-low candidate)))
         (effective-search    (if case-sensitive? search (%palette-candidate-search-low candidate)))
         (term-length         (string-length term))
         (primary-length      (string-length effective-primary))
         (secondary-length    (string-length effective-secondary)))
    (cond

     ;; match in primary column (highest importance)
     ;; if term is exact match of primary, score 11000
     ((and (> primary-length 0)
           (string=? effective-primary term))
      11000)

     ;; if term in a prefix of primary, score 5000 + score based on primary length
     ;; this means if term is "librew", then "librewolf" will have higher score
     ;; than "librewolf (browser)", in which both will be high and +5000
     ((and (> primary-length 0)
           (string-prefix? term effective-primary))
      (+ 5000 (inexact->exact
               (round (* (/ term-length
                            (max term-length primary-length)) 1000)))))

     ;; if primary contains the string
     ((and (> primary-length 0)
           (string-contains effective-primary term))
      => (lambda (pos)
           ;; if it's with in a word boundary, give it higher score
           (if (%word-boundary? primary pos)
               (+ 3000 (max 0 (- 500 (* 10 pos)))
                       (inexact->exact (round (* (/ term-length (max term-length primary-length)) 500))))
               (+ 1000 (max 0 (- 300 (* 10 pos)))))))

     ;; match in secondary columns such as: description, generic name, etc.
     ;; exact same logic as in primary, except score is less
     ((and (> secondary-length 0)
           (string=? effective-secondary term))
      500)
     ((and (> secondary-length 0)
           (string-prefix? term effective-secondary))
      (+ 300 (inexact->exact (round (* (/ term-length (max term-length secondary-length)) 100)))))
     ((and (> secondary-length 0)
           (string-contains effective-secondary term))
      => (lambda (pos)
           (if (%word-boundary? secondary pos)
               (+ 200 (max 0 (- 50 (* 2 pos))))
               (+ 100 (max 0 (- 30 (* 2 pos)))))))

     ;; match in search-text only (exec, keywords, icon, etc.)
     ;; using word boundary and less score than primary and secondary
     ((and effective-search (string-contains effective-search term))
      => (lambda (pos)
           (if (and search (%word-boundary? search pos))
               (+ 50 (max 0 (- 20 pos)))
               (+ 20 (max 0 (- 10 pos))))))

     (else #f))))

(define (%score-candidate candidate query terms case-sensitive?)
  "Score CANDIDATE against QUERY and TERMS.
Parameters: (CANDIDATE QUERY TERMS CASE-SENSITIVE?)
  - CANDIDATE: <palette-candidate> record.
  - QUERY: String full trimmed query.
  - TERMS: List of whitespace-delimited query strings (really QUERY space-separated).
  - CASE-SENSITIVE?: Boolean controlling case sensitivity.
Returns integer relevance score, or #f if any token fails to match."
  (let* ((primary             (%palette-candidate-primary candidate))
         (secondary           (%palette-candidate-secondary candidate))
         (effective-primary   (if case-sensitive? primary (%palette-candidate-primary-low candidate)))
         (effective-secondary (if case-sensitive? secondary (%palette-candidate-secondary-low candidate))))
    ;; score each term sequentially, any unmatched term rejects the candidate (#f)
    (let loop ((remaining-terms terms)
               (total 0)
               (all-in-primary? #t)
               (all-in-display? #t))
      (if (null? remaining-terms)
          (let* ((primary-length (string-length effective-primary))
                 (query-length   (string-length query))

                 ;; reward where the entire query appears in primary
                 ;; same principles as in %score-term (which does per-term scoring)
                 ;; here we do query-level scoring
                 (full-query-bonus
                  (cond
                   ((and (> primary-length 0)
                         (string=? effective-primary query))
                    20000)
                   ((and (> primary-length 0)
                         (string-prefix? query effective-primary))
                    (+ 10000 (inexact->exact (round (* (/ query-length (max query-length primary-length)) 2000)))))
                   ((and (> primary-length 0)
                         (string-contains effective-primary query))
                    => (lambda (pos)
                         (if (%word-boundary? primary pos) 5000 2000)))
                   (else 0)))

                 ;; reward when every token hits visible
                 ;; columns rather than hidden search metadata
                 (cohesion-bonus
                  (+ (if all-in-primary? 2000 0)
                     (if all-in-display? 500 0)))
                 ;; penalty for longer descriptions when scores tie
                 (length-penalty (min 50 (quotient (string-length effective-secondary) 20))))
            (- (+ total full-query-bonus cohesion-bonus) length-penalty))

          ;; use %score-term and to sum score
          (let ((term-score (%score-term (car remaining-terms) candidate case-sensitive?)))
            (and term-score
                 (let ((in-primary?   (and (> (string-length effective-primary) 0)
                                           (string-contains effective-primary (car remaining-terms))))
                       (in-secondary? (and (> (string-length effective-secondary) 0)
                                           (string-contains effective-secondary (car remaining-terms)))))
                   (loop (cdr remaining-terms)
                         (+ total term-score)
                         (and all-in-primary? in-primary?)
                         (and all-in-display? (or in-primary? in-secondary?))))))))))

(define* (gleui-palette-filter-candidates candidates query case-sensitive?
                                          #:optional (search-strings #f)
                                          #:key (parsed-candidates #f))
  "Filter and rank candidates matching QUERY in relevance order.
Parameters: (CANDIDATES QUERY CASE-SENSITIVE? [SEARCH-STRINGS #f] [#:PARSED-CANDIDATES #f])
  - CANDIDATES: List or vector of candidate items.
  - QUERY: String or #f user filter query.
  - CASE-SENSITIVE?: Boolean controlling case sensitivity.
  - SEARCH-STRINGS: List, vector, or #f of custom search strings parallel to CANDIDATES.
  - PARSED-CANDIDATES: Vector or #f pre-parsed <palette-candidate> records.
Returns list of candidate indices sorted by descending relevance."
  ;; determine candidate count from pre-parsed cache or raw candidates
  (let* ((trimmed-query   (if (string? query) (string-trim-both query) ""))
         (candidate-count (cond
                           ((vector? parsed-candidates) (vector-length parsed-candidates))
                           ((list? candidates) (length candidates))
                           ((vector? candidates) (vector-length candidates))
                           (else 0))))
    ;; empty queries short-circuit to keep candidate order
    (if (or (string-null? trimmed-query) (zero? candidate-count))
        (iota candidate-count)
        ;; reuse cached <palette-candidate> vector when available
        (let* ((parsed-entries  (or parsed-candidates
                                    (gleui-palette-parse-candidates candidates search-strings)))
               (effective-query (if case-sensitive?
                                    trimmed-query
                                    (string-downcase trimmed-query)))
               ;; split into tokens by whitespaces
               (terms           (filter (lambda (s) (> (string-length s) 0))
                                        (string-split effective-query #\space)))
               (parsed-count    (vector-length parsed-entries)))
          (if (null? terms)
              ;; no query, return original order
              (iota candidate-count)
              (let loop ((index 0)
                         (scored-matches '()))
                (if (< index parsed-count)
                    (let* ((entry (vector-ref parsed-entries index))
                           (score (and (%palette-candidate? entry)
                                       (%score-candidate entry effective-query terms case-sensitive?))))
                      (loop (1+ index)
                            (if score
                                (cons (cons score
                                            (%palette-candidate-index entry))
                                      scored-matches)
                                scored-matches)))
                    ;; stable-sort keeps original ordering among scoring candidates
                    (let ((sorted-matches (stable-sort (reverse! scored-matches)
                                                       (lambda (a b) (> (car a) (car b))))))
                      (map cdr sorted-matches)))))))))

(define (%item-formatter item)
  "Format candidate ITEM into a tab-delimited or plain display string.
Parameters: (ITEM)
  - ITEM: String, list of column values, or arbitrary object.
Returns formatted candidate string."
  (cond
   ((string? item) item)
   ((list? item)
    ;; join column list items with tabs so rows align with column formatting
    (string-join (map (lambda (x) (format #f "~a" x)) item) "\t"))
   (else (format #f "~a" item))))

;; TODO: searchable parameter is supposed to limit the columns that can be
;; searched, it's not implemented yet.
(define* (gleui-palette-make-dmenu-options options colors #:key (pango #t)
                                           (widths '()) (searchable '()) (visible '()))
  "Format OPTIONS dmenu strings.
NOTE: this was created to make it easy to pass dmenu-like strings to the palette
Parameters: (OPTIONS COLORS [#:PANGO #t] [#:WIDTHS '()] [#:SEARCHABLE '()] [#:VISIBLE '()])
  - OPTIONS: List of row column lists or items.
  - COLORS: List of per-column foreground color strings or #f.
  - PANGO: Boolean when #t, wrap colored cells in Pango span tags.
  - WIDTHS: List of optional fixed character widths per column.
  - SEARCHABLE: List of searchable columns for filtering (unused).
  - VISIBLE: List of booleans controlling visibility per column.
Returns list of formatted row display strings."
  ;; convert column-config lists to vectors for fast column lookup
  (define visible-columns (list->vector visible))
  (define column-widths   (list->vector widths))
  (define column-colors   (list->vector colors))

  (define (vec-ref vec index default)
    "Look up element INDEX in vector VEC with fallback DEFAULT.
Parameters: (VEC INDEX DEFAULT)
  - VEC: Vector to index.
  - INDEX: Integer target index.
  - DEFAULT: Value returned when INDEX is out of bounds.
Returns vector element at INDEX, or DEFAULT."
    (if (< index (vector-length vec))
        (vector-ref vec index)
        default))

  (define (truncate-or-pad str width)
    "Truncate STR with ellipsis or right-pad it with spaces
Parameters: (STR WIDTH)
  - STR: String text to truncate.
  - WIDTH: Integer or #f target column character width
Returns fitted string."
    (cond
     ((not width)
      str)
     ((> (string-length str) width)
      (string-append (string-take str (max 0 (- width 3))) "..."))
     (else
      (string-pad-right str width #\space))))

  (define (format-column col column-index)
    "Format column value COL (cell) into a formatted pango string.
Parameters: (COL COLUMN-INDEX)
  - COL: Value for the row cell.
  - COLUMN-INDEX: Integer column index.
Returns formatted cell string, or #f if the column is hidden."
    ;; columns past `visible' default to #t; widths and colors default to #f
    (and (vec-ref visible-columns column-index #t)
         (let* ((text    (%safe-col-string col))
                (width   (vec-ref column-widths column-index #f))
                (color   (and pango
                              (vec-ref column-colors column-index #f)))
                ;; escape special characters so literal < or &
                ;; in cell text stay valid markup
                (escaped (gleui-pango-escape (truncate-or-pad text width))))
           (if (and color (not (string-null? text)))
               (format #f "<span color='~a'>~a</span>" color escaped)
               escaped))))

  (define (format-row columns)
    "Format all visible cells in COLUMNS into a single separated pango row line.
Parameters: (COLUMNS)
  - COLUMNS: List of cell values (can be single item)
Returns formatted row string."
    (let ((column-list (if (list? columns) columns (list columns))))
      ;; separate columns by 3 spaces so %col-split-rx can split them
      ;; back when scoring
      (string-join
       (filter-map format-column
                   column-list
                   (iota (length column-list)))
       "   ")))

  (map format-row options))

(define (gleui-palette-get-display-string state index)
  "Return the cached formatted display string for candidate INDEX in STATE.
Parameters: (STATE INDEX)
  - STATE: <gleui-palette-state> record.
  - INDEX: Integer candidate index.
Returns formatted display string, or empty string if INDEX is not found."
  (let* ((display-cache   (gleui-palette-state-display-candidates state))
         (candidates      (gleui-palette-state-candidates state))
         (candidate-count (if (vector? candidates)
                              (vector-length candidates)
                              (length candidates))))
    (if (or (< index 0)
            (>= index candidate-count))
        ;; not found
        ""
        ;; lazily format row strings so scrolling/redraws reuse cached text
        (or (vector-ref display-cache index)
            (let* ((item (if (vector? candidates)
                             (vector-ref candidates index)
                             (list-ref candidates index)))
                   (formatted
                    (cond
                     ((string? item) item)
                     ((list? item) (%item-formatter item))
                     (else (format #f "~a" item)))))
              (vector-set! display-cache index formatted)
              formatted)))))
