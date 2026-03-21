#!chezscheme
;;; (jerboa-aws xml) -- XML parsing utilities for AWS responses
;;; Parses XML into a simple SXML-like tree, then converts to hash tables.

(library (jerboa-aws xml)
  (export xml-parse sxml->hash sxml-text sxml-items
          strip-ns aws-response->hash)
  (import (except (chezscheme) filter hashtable?))

  ;; ---- Minimal XML parser ----
  ;; Parses XML string into SXML: (tag (@ (attr val) ...) child ...)

  (define (xml-parse str)
    (let ([port (open-input-string str)]
          [pos 0])
      (xml-read-document port)))

  (define (xml-read-document port)
    ;; Skip XML declaration if present
    (xml-skip-ws port)
    (let ([ch (peek-char port)])
      (cond
        [(eof-object? ch) '()]
        [(char=? ch #\<)
         (read-char port)
         (let ([ch2 (peek-char port)])
           (cond
             ;; XML declaration <?xml ... ?>
             [(char=? ch2 #\?)
              (xml-skip-until port "?>")
              (xml-read-document port)]
             ;; Comment <!-- ... -->
             [(char=? ch2 #\!)
              (read-char port)
              (cond
                [(and (char=? (peek-char port) #\-)
                      (begin (read-char port)
                             (char=? (peek-char port) #\-)))
                 (read-char port)
                 (xml-skip-until port "-->")
                 (xml-read-document port)]
                [else
                 (xml-skip-until port ">")
                 (xml-read-document port)])]
             ;; Element
             [else (xml-read-element port)]))]
        [else (xml-read-document port)])))

  (define (xml-read-element port)
    ;; Opening < already consumed
    (let ([tag (xml-read-name port)])
      (xml-skip-ws port)
      (let-values ([(attrs self-closing) (xml-read-attrs port)])
        (if self-closing
          (if (null? attrs)
            (list (string->symbol tag))
            (list (string->symbol tag) (cons '@ attrs)))
          ;; Read children until closing tag
          (let ([children (xml-read-children port tag)])
            (if (null? attrs)
              (cons (string->symbol tag) children)
              (cons (string->symbol tag) (cons (cons '@ attrs) children))))))))

  (define (xml-read-name port)
    (let ([out (open-output-string)])
      (let loop ()
        (let ([ch (peek-char port)])
          (cond
            [(or (eof-object? ch)
                 (char-whitespace? ch)
                 (char=? ch #\>)
                 (char=? ch #\/)
                 (char=? ch #\=))
             (get-output-string out)]
            [else
             (write-char (read-char port) out)
             (loop)])))))

  (define (xml-read-attrs port)
    ;; Returns (values attrs-list self-closing?)
    (let loop ([attrs '()])
      (xml-skip-ws port)
      (let ([ch (peek-char port)])
        (cond
          [(char=? ch #\>)
           (read-char port)
           (values (reverse attrs) #f)]
          [(char=? ch #\/)
           (read-char port)
           (let ([ch2 (read-char port)]) ;; consume >
             (values (reverse attrs) #t))]
          [else
           (let* ([name (xml-read-name port)]
                  [_ (xml-skip-ws port)]
                  [_ (read-char port)]  ;; consume =
                  [_ (xml-skip-ws port)]
                  [val (xml-read-attr-value port)])
             (loop (cons (list (string->symbol name) val) attrs)))]))))

  (define (xml-read-attr-value port)
    (let ([quote-char (read-char port)]
          [out (open-output-string)])
      (let loop ()
        (let ([ch (read-char port)])
          (cond
            [(or (eof-object? ch) (char=? ch quote-char))
             (get-output-string out)]
            [else (write-char ch out) (loop)])))))

  (define (xml-read-children port parent-tag)
    (let loop ([children '()])
      (let ([text (xml-read-text port)])
        (let ([children (if (> (string-length text) 0)
                          (cons text children)
                          children)])
          (let ([ch (peek-char port)])
            (cond
              [(eof-object? ch) (reverse children)]
              [else
               (read-char port) ;; consume <
               (let ([ch2 (peek-char port)])
                 (cond
                   ;; Closing tag
                   [(char=? ch2 #\/)
                    (read-char port)
                    (xml-skip-until port ">")
                    (reverse children)]
                   ;; Comment
                   [(char=? ch2 #\!)
                    (read-char port)
                    (when (char=? (peek-char port) #\-)
                      (read-char port) (read-char port))
                    (xml-skip-until port "-->")
                    (loop children)]
                   ;; CDATA
                   ;; Child element
                   [else
                    (let ([child (xml-read-element port)])
                      (loop (cons child children)))]))]))))))

  (define (xml-read-text port)
    (let ([out (open-output-string)])
      (let loop ()
        (let ([ch (peek-char port)])
          (cond
            [(or (eof-object? ch) (char=? ch #\<))
             (get-output-string out)]
            [(char=? ch #\&)
             (read-char port)
             (let ([entity (xml-read-until port #\;)])
               (cond
                 [(string=? entity "amp") (write-char #\& out)]
                 [(string=? entity "lt") (write-char #\< out)]
                 [(string=? entity "gt") (write-char #\> out)]
                 [(string=? entity "apos") (write-char #\' out)]
                 [(string=? entity "quot") (write-char #\" out)]
                 [else (write-char #\& out)
                       (put-string out entity)
                       (write-char #\; out)])
               (loop))]
            [else (write-char (read-char port) out) (loop)])))))

  (define (xml-read-until port ch)
    (let ([out (open-output-string)])
      (let loop ()
        (let ([c (read-char port)])
          (cond
            [(or (eof-object? c) (char=? c ch))
             (get-output-string out)]
            [else (write-char c out) (loop)])))))

  (define (xml-skip-ws port)
    (let loop ()
      (let ([ch (peek-char port)])
        (when (and (char? ch) (char-whitespace? ch))
          (read-char port)
          (loop)))))

  (define (xml-skip-until port end-str)
    (let ([end-len (string-length end-str)])
      (let loop ([matched 0])
        (let ([ch (read-char port)])
          (cond
            [(eof-object? ch) (void)]
            [(char=? ch (string-ref end-str matched))
             (if (= (+ matched 1) end-len)
               (void)
               (loop (+ matched 1)))]
            [else (loop 0)])))))

  ;; ---- SXML → hash table conversion ----

  ;; Known "set" element suffixes that should become lists
  (define set-suffixes '("Set" "set" "Addresses" "Groups" "Subnets"
                         "Members" "Items" "Entries" "Rules" "Tags"
                         "Permissions" "Associations" "Attachments"
                         "SecurityGroups" "Instances" "Reservations"
                         "Vpcs" "Volumes" "Snapshots" "Images"
                         "KeyPairs" "Routes" "Gateways" "Functions"
                         "Tables" "Topics" "Queues" "Stacks"))

  (define (is-set-element? name)
    (let ([name-str (if (symbol? name) (symbol->string name) name)])
      (let loop ([suffixes set-suffixes])
        (cond
          [(null? suffixes) #f]
          [(string-suffix? (car suffixes) name-str) #t]
          [else (loop (cdr suffixes))]))))

  ;; Strip namespace prefix from a symbol: ns:foo → foo
  (define (strip-ns sym)
    (let* ([str (symbol->string sym)]
           [pos (string-find-char str #\:)])
      (if pos
        (string->symbol (substring str (+ pos 1) (string-length str)))
        sym)))

  ;; Convert SXML element to hash table
  (define (sxml->hash elem)
    (cond
      [(string? elem) elem]
      [(not (pair? elem)) elem]
      [else
       (let* ([tag (strip-ns (car elem))]
              [children (sxml-children elem)]
              [text-children (filter string? children)]
              [elem-children (filter pair? children)])
         (cond
           ;; Leaf text node
           [(and (null? elem-children) (pair? text-children))
            (apply string-append text-children)]
           ;; Set element → list of child hashes
           [(is-set-element? tag)
            (map sxml->hash elem-children)]
           ;; Container element → hash table
           [else
            (let ([ht (make-hashtable symbol-hash eq?)])
              (for-each
                (lambda (child)
                  (when (pair? child)
                    (let* ([child-tag (strip-ns (car child))]
                           [child-val (sxml->hash child)]
                           [existing (hashtable-ref ht child-tag #f)])
                      (cond
                        ;; Duplicate → make/extend list
                        [existing
                         (if (and (list? existing) (not (hashtable? (car existing))))
                           (hashtable-set! ht child-tag (append existing (list child-val)))
                           (hashtable-set! ht child-tag (list existing child-val)))]
                        [else
                         (hashtable-set! ht child-tag child-val)]))))
                elem-children)
              ht)]))]))

  ;; Get text content of an SXML element
  (define (sxml-text elem)
    (cond
      [(string? elem) elem]
      [(not (pair? elem)) ""]
      [else
       (apply string-append
         (filter string? (sxml-children elem)))]))

  ;; Get child elements, optionally filtering by tag
  (define (sxml-items elem . tag)
    (let ([children (sxml-children elem)])
      (if (null? tag)
        (filter pair? children)
        (filter (lambda (c)
                  (and (pair? c)
                       (eq? (strip-ns (car c)) (car tag))))
                children))))

  ;; Get children of an SXML element (skip attributes)
  (define (sxml-children elem)
    (if (and (pair? (cdr elem))
             (pair? (cadr elem))
             (eq? (caadr elem) '@))
      (cddr elem)
      (cdr elem)))

  ;; Convert full AWS XML response to hash
  (define (aws-response->hash xml-str)
    (let ([sxml (xml-parse xml-str)])
      (if (pair? sxml)
        (sxml->hash sxml)
        (make-hashtable symbol-hash eq?))))

  ;; --- Helpers ---

  (define (string-suffix? suffix str)
    (let ([xlen (string-length suffix)]
          [slen (string-length str)])
      (and (<= xlen slen)
           (string=? suffix (substring str (- slen xlen) slen)))))

  (define (string-find-char str ch)
    (let ([len (string-length str)])
      (let loop ([i 0])
        (cond
          [(>= i len) #f]
          [(char=? (string-ref str i) ch) i]
          [else (loop (+ i 1))]))))

  (define (filter pred lst)
    (cond
      [(null? lst) '()]
      [(pred (car lst)) (cons (car lst) (filter pred (cdr lst)))]
      [else (filter pred (cdr lst))]))

  (define (hashtable? x)
    (or (eq-hashtable? x)
        (symbol-hashtable? x)
        (and (record? x) ;; catch generic hashtables
             (guard (e [#t #f])
               (hashtable-size x)
               #t))))

  ) ;; end library
