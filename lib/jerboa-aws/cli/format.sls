#!chezscheme
;;; (jerboa-aws cli format) -- Output formatting (json pretty-print)

(library (jerboa-aws cli format)
  (export format-output format-json format-text pretty-json)
  (import (chezscheme)
          (jerboa-aws json))

  ;; Dispatch to the appropriate formatter
  (define (format-output mode data)
    (cond
      ((string=? mode "json")  (format-json data))
      ((string=? mode "text")  (format-text data))
      (else (format-json data))))

  ;; JSON output -- pretty-printed
  (define (format-json data)
    (cond
      ((hashtable? data)
       (display (pretty-json data))
       (newline))
      ((list? data)
       (display (pretty-json data))
       (newline))
      (else
       (display data)
       (newline))))

  ;; Text output -- key: value pairs
  (define (format-text data)
    (cond
      ((hashtable? data)
       (format-text-hashtable data ""))
      ((list? data)
       (let loop ((items data) (first? #t))
         (unless (null? items)
           (unless first? (display "---\n"))
           (cond
             ((hashtable? (car items))
              (format-text-hashtable (car items) ""))
             (else (display (car items)) (newline)))
           (loop (cdr items) #f))))
      (else
       (display data)
       (newline))))

  (define (->string x)
    (cond
      ((string? x) x)
      ((symbol? x) (symbol->string x))
      ((number? x) (number->string x))
      (else (format "~a" x))))

  (define (format-text-hashtable ht prefix)
    (let-values (((keys vals) (hashtable-entries ht)))
      (let ((kv-list (let loop ((i 0) (acc '()))
                       (if (= i (vector-length keys))
                         (list-sort (lambda (a b) (string<? (->string (car a)) (->string (car b)))) acc)
                         (loop (+ i 1) (cons (cons (vector-ref keys i)
                                                   (vector-ref vals i))
                                             acc))))))
        (for-each
          (lambda (kv)
            (let ((k (->string (car kv))) (v (cdr kv)))
              (cond
                ((hashtable? v)
                 (display prefix) (display k) (display ":") (newline)
                 (format-text-hashtable v (string-append prefix "  ")))
                ((list? v)
                 (display prefix) (display k) (display ":") (newline)
                 (for-each
                   (lambda (item)
                     (cond
                       ((hashtable? item)
                        (display prefix) (display "  -") (newline)
                        (format-text-hashtable item (string-append prefix "    ")))
                       (else
                        (display prefix) (display "  - ") (display item) (newline))))
                   v))
                (else
                 (display prefix) (display k) (display "\t") (display v) (newline)))))
          kv-list))))

  ;; Pretty-print a JSON value with indentation
  (define (pretty-json val)
    (let ((port (open-output-string)))
      (pretty-json-value val port 0)
      (get-output-string port)))

  (define (pretty-json-value val port indent)
    (cond
      ((hashtable? val) (pretty-json-object val port indent))
      ((list? val) (pretty-json-array val port indent))
      ((vector? val) (pretty-json-array (vector->list val) port indent))
      ((string? val) (json-write-string val port))
      ((and (integer? val) (exact? val))
       (put-string port (number->string val)))
      ((number? val) (put-string port (number->string (inexact val))))
      ((eq? val #t) (put-string port "true"))
      ((eq? val #f) (put-string port "false"))
      ((eq? val 'null) (put-string port "null"))
      ((null? val) (put-string port "null"))
      (else (put-string port (format "~a" val)))))

  (define (pretty-json-object ht port indent)
    (let-values (((keys vals) (hashtable-entries ht)))
      (if (= (vector-length keys) 0)
        (put-string port "{}")
        (let ((sorted (list-sort
                        (lambda (a b) (string<? (->string (car a)) (->string (car b))))
                        (let loop ((i 0) (acc '()))
                          (if (= i (vector-length keys))
                            acc
                            (loop (+ i 1) (cons (cons (vector-ref keys i)
                                                      (vector-ref vals i))
                                                acc)))))))
          (put-string port "{\n")
          (let loop ((pairs sorted) (first? #t))
            (unless (null? pairs)
              (unless first? (put-string port ",\n"))
              (put-string port (make-string (+ indent 2) #\space))
              (json-write-string (->string (caar pairs)) port)
              (put-string port ": ")
              (pretty-json-value (cdar pairs) port (+ indent 2))
              (loop (cdr pairs) #f)))
          (put-string port "\n")
          (put-string port (make-string indent #\space))
          (put-string port "}")))))

  (define (pretty-json-array lst port indent)
    (if (null? lst)
      (put-string port "[]")
      (begin
        (put-string port "[\n")
        (let loop ((items lst) (first? #t))
          (unless (null? items)
            (unless first? (put-string port ",\n"))
            (put-string port (make-string (+ indent 2) #\space))
            (pretty-json-value (car items) port (+ indent 2))
            (loop (cdr items) #f)))
        (put-string port "\n")
        (put-string port (make-string indent #\space))
        (put-string port "]"))))

  (define (json-write-string str port)
    (put-char port #\")
    (do ((i 0 (+ i 1))) ((= i (string-length str)))
      (let ((c (string-ref str i)))
        (cond
          ((char=? c #\") (put-string port "\\\""))
          ((char=? c #\\) (put-string port "\\\\"))
          ((char=? c #\newline) (put-string port "\\n"))
          ((char=? c #\return) (put-string port "\\r"))
          ((char=? c #\tab) (put-string port "\\t"))
          ((< (char->integer c) #x20)
           (put-string port (format "\\u~4,'0x" (char->integer c))))
          (else (put-char port c)))))
    (put-char port #\"))

  ) ;; end library
