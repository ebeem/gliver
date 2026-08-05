;;; tools/generate-bindings.scm --- Wayland protocol scanner for Gliver
;;;
;;; Copyright (C) 2026 Gliver Contributors
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; Usage: guile -L . tools/generate-bindings.scm PROTO.xml ... OUTPUT-DIR
;;;
;;; Reads Wayland protocol XML files and generates Guile Scheme binding
;;; modules into OUTPUT-DIR, one .scm file per protocol.

(use-modules (sxml simple)
             (ice-9 match)
             (ice-9 format)
             (ice-9 textual-ports)
             (srfi srfi-1))

;;; sxml helpers
(define (sxml-attr elem name)
  "Get attribute NAME from SXML element ELEM, or #f."
  (let ((attrs (assq '@ (cdr elem))))
    (and attrs
         (let ((attr (assq name (cdr attrs))))
           (and attr (cadr attr))))))

(define (sxml-children elem tag)
  "Get all child elements with TAG from SXML element ELEM."
  (filter (lambda (child)
            (and (pair? child) (eq? (car child) tag)))
          (cdr elem)))

(define (sxml-child elem tag)
  "Get the first child element with TAG, or #f."
  (let ((children (sxml-children elem tag)))
    (and (pair? children) (car children))))

;;; name conversion
(define (snake->kebab s)
  "Convert snake_case to kebab-case."
  (string-map (lambda (c) (if (char=? c #\_) #\- c)) s))

(define (snake->screaming s)
  "Convert snake_case to SCREAMING_CASE."
  (string-upcase s))

(define (escape-string s)
  "Escape double-quotes and backslashes in S for use in Scheme string literals."
  (list->string
   (append-map (lambda (c)
                 (cond ((char=? c #\") (list #\\ #\"))
                       ((char=? c #\\) (list #\\ #\\))
                       (else (list c))))
               (string->list s))))

;;; protocol model extraction
(define (extract-arg sxml)
  `((name       . ,(sxml-attr sxml 'name))
    (type       . ,(sxml-attr sxml 'type))
    (interface  . ,(sxml-attr sxml 'interface))
    (allow-null . ,(string=? "true" (or (sxml-attr sxml 'allow-null) "")))
    (enum       . ,(sxml-attr sxml 'enum))
    (summary    . ,(or (sxml-attr sxml 'summary) ""))
    (since      . ,(and=> (sxml-attr sxml 'since) string->number))))

(define (extract-enum sxml)
  `((name     . ,(sxml-attr sxml 'name))
    (bitfield . ,(string=? "true" (or (sxml-attr sxml 'bitfield) "")))
    (since    . ,(or (and=> (sxml-attr sxml 'since) string->number) 1))
    (entries  . ,(map (lambda (e)
                        `((name    . ,(sxml-attr e 'name))
                          (value   . ,(string->number (sxml-attr e 'value)))
                          (summary . ,(or (sxml-attr e 'summary) ""))
                          (since   . ,(or (and=> (sxml-attr e 'since) string->number) 1))))
                      (sxml-children sxml 'entry)))))

(define (extract-message sxml)
  `((name        . ,(sxml-attr sxml 'name))
    (type        . ,(sxml-attr sxml 'type))
    (since       . ,(or (and=> (sxml-attr sxml 'since) string->number) 1))
    (args        . ,(map extract-arg (sxml-children sxml 'arg)))
    (description . ,(or (and=> (sxml-child sxml 'description)
                               (lambda (d) (sxml-attr d 'summary)))
                        ""))))

(define (extract-interface sxml)
  `((name        . ,(sxml-attr sxml 'name))
    (version     . ,(string->number (sxml-attr sxml 'version)))
    (enums       . ,(map extract-enum (sxml-children sxml 'enum)))
    (requests    . ,(map extract-message (sxml-children sxml 'request)))
    (events      . ,(map extract-message (sxml-children sxml 'event)))
    (description . ,(or (and=> (sxml-child sxml 'description)
                               (lambda (d) (sxml-attr d 'summary)))
                        ""))))

(define (extract-protocol sxml)
  (let ((p (or (sxml-child sxml 'protocol)
               (and (pair? sxml) (eq? (car sxml) 'protocol) sxml))))
    (unless p (error "No <protocol> element found in SXML"))
    `((name       . ,(sxml-attr p 'name))
      (interfaces . ,(map extract-interface (sxml-children p 'interface))))))

;;; signature computation
(define (arg-sig-char arg)
  "Return the Wayland signature character for an argument type."
  (match (assq-ref arg 'type)
    ("int"    #\i) ("uint"   #\u) ("fixed"  #\f)
    ("string" #\s) ("object" #\o) ("new_id" #\n)
    ("array"  #\a) ("fd"     #\h)
    (t (error "Unknown arg type" t))))

(define (compute-signature msg)
  "Compute the Wayland wire signature string for a message.
Follows the C wayland-scanner convention: emit version markers when
the 'since' value increases."
  (let ((msg-since (assq-ref msg 'since))
        (args      (assq-ref msg 'args)))
    (with-output-to-string
      (lambda ()
        (let ((since msg-since))
          ;; emit initial version marker if > 1
          (when (> since 1)
            (display since))
          ;; emit each arg
          (for-each
           (lambda (arg)
             (let ((arg-since (or (assq-ref arg 'since) msg-since)))
               ;; Emit version bump if arg introduces a higher version
               (when (> arg-since since)
                 (set! since arg-since)
                 (display since))
               (when (assq-ref arg 'allow-null)
                 (display "?"))
               (display (arg-sig-char arg))))
           args))))))

;;; ffi type mapping (for procedure->pointer in listeners)
(define (arg-ffi-type arg)
  "Return the FFI type expression string for an arg in a listener callback."
  (match (assq-ref arg 'type)
    ("int"    "int32")  ("uint"   "uint32") ("fixed"  "int32")
    ("string" "'*")     ("object" "'*")     ("new_id" "'*")
    ("array"  "'*")     ("fd"     "int32")
    (t (error "Unknown type for FFI" t))))

;;; marshal tag mapping (for make-wl-args)
(define (arg-marshal-tag arg)
  "Return the marshal tag expression string for an arg."
  (match (assq-ref arg 'type)
    ("int"    "'int")    ("uint"   "'uint")   ("fixed"  "'fixed")
    ("string" "'string") ("object" "'object") ("new_id" "'new-id")
    ("array"  "'array")  ("fd"     "'fd")
    (t (error "Unknown type for marshal" t))))

;;; interface dependency sorting
(define (iface-new-id-refs iface all-names)
  "Get same-protocol interface names this interface creates via new_id."
  (let ((messages (append (assq-ref iface 'requests)
                          (assq-ref iface 'events))))
    (delete-duplicates
     (filter identity
       (append-map
        (lambda (msg)
          (filter-map
           (lambda (arg)
             (and (string=? (assq-ref arg 'type) "new_id")
                  (let ((ref (assq-ref arg 'interface)))
                    (and ref
                         (not (string=? ref (assq-ref iface 'name)))
                         (member ref all-names)
                         ref))))
           (assq-ref msg 'args)))
        messages)))))

(define (sort-interfaces interfaces)
  "Topological sort: interfaces referenced via new_id come first.
Uses DFS post-order so dependencies are emitted before dependents."
  (let* ((all-names  (map (lambda (i) (assq-ref i 'name)) interfaces))
         (name->iface (map (lambda (i) (cons (assq-ref i 'name) i)) interfaces))
         (visited '())
         (result  '()))
    (define (visit name)
      (unless (member name visited)
        (set! visited (cons name visited))
        (let ((iface (assoc-ref name->iface name)))
          (when iface
            (for-each visit (iface-new-id-refs iface all-names))))
        (set! result (cons name result))))
    (for-each (lambda (i) (visit (assq-ref i 'name))) interfaces)
    ;; result is in reverse post-order; reverse gives dependencies first
    (map (lambda (name) (assoc-ref name->iface name))
         (reverse result))))

;;; naming conventions
(define (iface-var-name name)
  "Interface pointer variable: *river-layout-v3-interface*"
  (string-append "*" (snake->kebab name) "-interface*"))

(define (request-func-name iface-name req-name)
  "Request function: river-layout-v3-push-view-dimensions"
  (string-append (snake->kebab iface-name) "-" (snake->kebab req-name)))

(define (handler-var-name iface-name evt-name)
  "Handler variable: *river-layout-v3-layout-demand-handler*"
  (string-append "*" (snake->kebab iface-name) "-" (snake->kebab evt-name) "-handler*"))

(define (handler-setter-name iface-name evt-name)
  "Handler setter: set-river-layout-v3-layout-demand-handler!"
  (string-append "set-" (snake->kebab iface-name) "-" (snake->kebab evt-name) "-handler!"))

(define (listener-func-name iface-name)
  "Listener creator: make-river-layout-v3-listener"
  (string-append "make-" (snake->kebab iface-name) "-listener"))

(define (enum-const-name iface-name enum-name entry-name)
  "Enum constant: RIVER_LAYOUT_V3_ERROR_INVALID_FORMAT"
  (string-append (snake->screaming iface-name) "_"
                 (snake->screaming enum-name) "_"
                 (snake->screaming entry-name)))

;;; output helpers
(define (emit . parts)
  "Write PARTS to current output port."
  (for-each display parts))

(define (emit-line . parts)
  "Write PARTS followed by newline."
  (apply emit parts)
  (newline))

(define (emit-nl)
  (newline))

;;; code generation interface definitions
(define (emit-types-list args iface-names)
  "Emit the types list for a message's arguments.
Uses actual interface pointers for new_id args in the same protocol,
%null-pointer otherwise."
  (if (null? args)
      (emit "'()")
      (begin
        (emit "(list")
        (for-each
         (lambda (arg)
           (let ((type (assq-ref arg 'type))
                 (ref  (assq-ref arg 'interface)))
             (cond
              ;; new_id with same-protocol interface
              ((and (string=? type "new_id") ref (member ref iface-names))
               (emit " " (iface-var-name ref)))
              ;; everything else
              (else (emit " %null-pointer")))))
         args)
        (emit ")"))))

(define (emit-interface-def iface iface-names)
  "Emit a make-wl-interface-full definition for IFACE."
  (let ((name    (assq-ref iface 'name))
        (version (assq-ref iface 'version))
        (reqs    (assq-ref iface 'requests))
        (evts    (assq-ref iface 'events)))
    (emit-line "(define " (iface-var-name name))
    (emit-line "  (make-wl-interface-full \"" name "\" " version)
    ;; requests
    (if (null? reqs)
        (emit-line "    '()")
        (begin
          (emit-line "    (list")
          (for-each
           (lambda (req)
             (let ((sig  (compute-signature req))
                   (args (assq-ref req 'args)))
               (emit "      (list \"" (assq-ref req 'name) "\" \"" sig "\" ")
               (emit-types-list args iface-names)
               (emit-line ")")))
           reqs)
          (emit-line "    )")))
    ;; events
    (if (null? evts)
        (emit-line "    '()))")
        (begin
          (emit-line "    (list")
          (for-each
           (lambda (evt)
             (let ((sig  (compute-signature evt))
                   (args (assq-ref evt 'args)))
               (emit "      (list \"" (assq-ref evt 'name) "\" \"" sig "\" ")
               (emit-types-list args iface-names)
               (emit-line ")")))
           evts)
          (emit-line "    )))")))
    (emit-nl)))

;;; code generation request functions
(define (emit-request-functions iface iface-names)
  "Emit request functions for all requests in IFACE."
  (let ((iface-name (assq-ref iface 'name))
        (requests   (assq-ref iface 'requests)))
    (when (pair? requests)
      (emit-line ";;; " iface-name " requests")
      (emit-nl)
      (let loop ((reqs requests) (opcode 0))
        (when (pair? reqs)
          (let* ((req       (car reqs))
                 (req-name  (assq-ref req 'name))
                 (req-type  (assq-ref req 'type))
                 (desc      (assq-ref req 'description))
                 (args      (assq-ref req 'args))
                 (func-name (request-func-name iface-name req-name))
                 (is-destructor? (and req-type (string=? req-type "destructor")))
                 (new-id-arg (find (lambda (a) (string=? (assq-ref a 'type) "new_id")) args))
                 (param-args (filter (lambda (a) (not (string=? (assq-ref a 'type) "new_id"))) args))
                 (param-names (map (lambda (a) (snake->kebab (assq-ref a 'name))) param-args))
                 (is-constructor? (and new-id-arg (not is-destructor?))))

            (cond
             ;; destructor
             (is-destructor?
              (emit ";;; " req-name ": opcode " opcode " (destructor)\n")
              (emit-line "(define (" func-name " proxy)")
              (emit-line "  \"" (escape-string (if (string=? desc "") "Destroy the object." desc)) "\"")
              (emit-line "  (unless (null-pointer? proxy)")
              (emit-line "    (wl-marshal-request-destroy proxy " opcode ")))")
              (emit-nl))

             ;; constructor
             (is-constructor?
              (let* ((new-iface (assq-ref new-id-arg 'interface))
                     (in-protocol? (and new-iface (member new-iface iface-names))))
                (emit ";;; " req-name ": opcode " opcode " (constructor)\n")
                (emit "(define (" func-name " proxy")
                (for-each (lambda (p) (emit " " p)) param-names)
                (emit-line ")")
                (emit-line "  \"" (escape-string
                                   (if (string=? desc "")
                                       (format #f "Create a ~a object." (or new-iface "new"))
                                       desc)) "\"")
                (emit-line "  (wl-marshal-constructor proxy " opcode)
                (emit-line "    " (if in-protocol? (iface-var-name new-iface) "%null-pointer"))
                (emit-line "    (wl-proxy-get-version proxy)")
                (emit "    (list")
                (for-each
                 (lambda (arg)
                   (if (string=? (assq-ref arg 'type) "new_id")
                       (emit " (cons 'new-id 0)")
                       (emit " (cons " (arg-marshal-tag arg) " "
                             (snake->kebab (assq-ref arg 'name)) ")")))
                 args)
                (emit-line ")))")
                (emit-nl)))

             ;; regular request
             (else
              (emit ";;; " req-name ": opcode " opcode "\n")
              (emit "(define (" func-name " proxy")
              (for-each (lambda (p) (emit " " p)) param-names)
              (emit-line ")")
              (emit-line "  \"" (escape-string (if (string=? desc "")
                                                   (format #f "Send ~a request." req-name)
                                                   desc)) "\"")
              (if (null? args)
                  (emit-line "  (wl-marshal-request proxy " opcode " '()))")
                  (begin
                    (emit "  (wl-marshal-request proxy " opcode "\n")
                    (emit "    (list")
                    (for-each
                     (lambda (arg)
                       (emit " (cons " (arg-marshal-tag arg) " "
                             (snake->kebab (assq-ref arg 'name)) ")"))
                     args)
                    (emit-line ")))")))
              (emit-nl))))

          (loop (cdr reqs) (1+ opcode)))))))

;;; code generation event handlers
(define (emit-event-handlers iface)
  "Emit handler variables and setter functions for IFACE events."
  (let ((iface-name (assq-ref iface 'name))
        (events     (assq-ref iface 'events)))
    (when (pair? events)
      (emit-line ";;; " iface-name " event handlers")
      ;; Handler variables
      (for-each
       (lambda (evt)
         (emit-line "(define " (handler-var-name iface-name (assq-ref evt 'name)) " #f)"))
       events)
      (emit-nl)
      ;; Setter functions
      (for-each
       (lambda (evt)
         (let ((evt-name (assq-ref evt 'name)))
           (emit-line "(define (" (handler-setter-name iface-name evt-name) " handler)")
           (emit-line "  \"Set the handler for " evt-name " events.\"")
           (emit-line "  (set! " (handler-var-name iface-name evt-name) " handler))")
           (emit-nl)))
       events))))

;;; code generation listener creation
(define (emit-listener iface)
  "Emit a listener creation function for IFACE.
The listener takes one callback argument per event."
  (let ((iface-name (assq-ref iface 'name))
        (events     (assq-ref iface 'events)))
    (when (pair? events)
      (emit-line ";;; " iface-name " listener")
      (let ((cb-names (map (lambda (evt)
                             (string-append "on-" (snake->kebab (assq-ref evt 'name))))
                           events)))
        (emit "(define (" (listener-func-name iface-name))
        ;; Put first arg on same line, rest on continuation lines for readability
        (let loop ((names cb-names) (first? #t))
          (when (pair? names)
            (if first?
                (emit " " (car names))
                (begin
                  (emit "\n")
                  (emit (make-string (+ 10 (string-length (listener-func-name iface-name))) #\space))
                  (emit (car names))))
            (loop (cdr names) #f)))
        (emit-line ")")
        (emit-line "  \"Create a listener for " iface-name " events.")
        (emit-line "Each callback receives (data proxy ...) arguments.\"")
        (emit-line "  (make-wl-listener")
        (emit-line "   (list")
        (for-each
         (lambda (evt cb-name)
           (let ((args (assq-ref evt 'args)))
             (emit "    (procedure->pointer void " cb-name " (list '* '*")
             (for-each (lambda (a) (emit " " (arg-ffi-type a))) args)
             (emit-line "))")))
         events cb-names)
        (emit-line "   )))")
        (emit-nl)))))

;;; Code generation enums
(define (emit-enums iface)
  "Emit enum constant definitions for IFACE."
  (let ((iface-name (assq-ref iface 'name))
        (enums      (assq-ref iface 'enums)))
    (when (pair? enums)
      (for-each
       (lambda (enum)
         (let ((enum-name (assq-ref enum 'name)))
           (emit-line ";;; " iface-name " " enum-name " enum")
           (for-each
            (lambda (entry)
              (emit-line "(define " (enum-const-name iface-name enum-name
                                                     (assq-ref entry 'name))
                         " " (assq-ref entry 'value) ")"))
            (assq-ref enum 'entries))
           (emit-nl)))
       enums))))

;;; collect all export names
(define (collect-exports protocol sorted-ifaces)
  "Collect all export symbol names for the protocol module."
  (let ((first-iface-name (assq-ref (car (assq-ref protocol 'interfaces)) 'name))
        (exports '()))

    (define (add! name) (set! exports (cons name exports)))

    ;; protocol name constant
    (add! (string-append (snake->screaming first-iface-name) "_NAME"))

    ;; interface pointers
    (for-each (lambda (i) (add! (iface-var-name (assq-ref i 'name)))) sorted-ifaces)

    ;; enums
    (for-each
     (lambda (iface)
       (for-each
        (lambda (enum)
          (for-each
           (lambda (entry)
             (add! (enum-const-name (assq-ref iface 'name)
                                    (assq-ref enum 'name)
                                    (assq-ref entry 'name))))
           (assq-ref enum 'entries)))
        (assq-ref iface 'enums)))
     sorted-ifaces)

    ;; request functions
    (for-each
     (lambda (iface)
       (for-each
        (lambda (req)
          (add! (request-func-name (assq-ref iface 'name)
                                   (assq-ref req 'name))))
        (assq-ref iface 'requests)))
     sorted-ifaces)

    ;; event handler setters
    (for-each
     (lambda (iface)
       (for-each
        (lambda (evt)
          (add! (handler-setter-name (assq-ref iface 'name)
                                     (assq-ref evt 'name))))
        (assq-ref iface 'events)))
     sorted-ifaces)

    ;; listeners
    (for-each
     (lambda (iface)
       (when (pair? (assq-ref iface 'events))
         (add! (listener-func-name (assq-ref iface 'name)))))
     sorted-ifaces)

    (reverse exports)))

;;; main generator
(define (generate-binding protocol output-dir)
  "Generate a Scheme binding file for PROTOCOL into OUTPUT-DIR."
  (let* ((proto-name  (assq-ref protocol 'name))
         (interfaces  (assq-ref protocol 'interfaces))
         (sorted      (sort-interfaces interfaces))
         (iface-names (map (lambda (i) (assq-ref i 'name)) interfaces))
         (first-iface (car interfaces))
         (first-name  (assq-ref first-iface 'name))
         (filename    (string-append output-dir "/" (snake->kebab proto-name) ".scm"))
         (module-name (string-append "(gliver wayland gen "
                                     (snake->kebab proto-name) ")"))
         (exports     (collect-exports protocol sorted)))

    (format (current-error-port) "Generating ~a (~a interfaces) ...~%"
            filename (length interfaces))

    (with-output-to-file filename
      (lambda ()
        ;; header
        (emit-line ";;; " (snake->kebab proto-name) ".scm --- Generated Wayland protocol bindings")
        (emit-line ";;;")
        (emit-line ";;; Generated by tools/generate-bindings.scm from " proto-name ".xml")
        (emit-line ";;; DO NOT EDIT - regenerate with 'make gen'")
        (emit-nl)

        ;; module definition
        (emit-line "(define-module " module-name)
        (emit-line "  #:use-module (system foreign)")
        (emit-line "  #:use-module (gliver core logs)")
        (emit-line "  #:use-module (gliver wayland client)")
        (emit "  #:export (")
        (let loop ((exps exports) (first? #t))
          (when (pair? exps)
            (if first?
                (emit (car exps))
                (emit "\n            " (car exps)))
            (loop (cdr exps) #f)))
        (emit-line "))")
        (emit-nl)

        ;; %null-pointer
        (emit-line "(define %null-pointer (make-pointer 0))")
        (emit-nl)

        ;; protocol name constant
        (emit-line ";;; protocol name")
        (emit-line "(define " (snake->screaming first-name) "_NAME \"" first-name "\")")
        (emit-nl)

        ;; enums
        (for-each emit-enums sorted)

        ;; interface definitions
        (emit-line ";;; interface definitions")
        (for-each (lambda (i) (emit-interface-def i iface-names)) sorted)
        ;; requests, handlers, listeners per interface
        (emit-line ";;; requests, event handlers, and listeners")
        (for-each
         (lambda (iface)
           (emit-request-functions iface iface-names)
           (emit-event-handlers iface)
           (emit-listener iface))
         sorted)))))

;;; entry point
(let* ((args (cdr (command-line)))
       (n    (length args)))
  (when (< n 2)
    (format (current-error-port)
            "Usage: ~a PROTOCOL.xml ... OUTPUT-DIR~%"
            (car (command-line)))
    (exit 1))
  (let ((output-dir (last args))
        (xml-files  (drop-right args 1)))
    (for-each
     (lambda (xml-file)
       (format (current-error-port) "Reading ~a ...~%" xml-file)
       (let* ((port (open-input-file xml-file))
              (sxml (xml->sxml port))
              (protocol (extract-protocol sxml)))
         (close-port port)
         (generate-binding protocol output-dir)))
     xml-files)
    (format (current-error-port) "Done. Generated ~a protocol(s).~%"
            (length xml-files))))
