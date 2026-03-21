#!chezscheme
;;; (jerboa-aws json) -- JSON reader/writer
;;; Objects → hashtables, arrays → lists, null → 'null

(library (jerboa-aws json)
  (export read-json write-json string->json-object json-object->string)
  (import (except (chezscheme) hashtable?))

  ;; ---- Reader ----

  (define (read-json . args)
    (let ([port (if (null? args) (current-input-port) (car args))])
      (json-skip-ws port)
      (if (eof-object? (peek-char port))
        (eof-object)
        (json-read-value port))))

  (define (string->json-object str)
    (let ([port (open-input-string str)])
      (json-read-value port)))

  (define (json-skip-ws port)
    (let loop ()
      (let ([ch (peek-char port)])
        (when (and (char? ch) (char-whitespace? ch))
          (read-char port)
          (loop)))))

  (define (json-read-value port)
    (json-skip-ws port)
    (let ([ch (peek-char port)])
      (cond
        [(eof-object? ch) (error 'read-json "unexpected EOF")]
        [(char=? ch #\") (json-read-string port)]
        [(char=? ch #\{) (json-read-object port)]
        [(char=? ch #\[) (json-read-array port)]
        [(char=? ch #\t) (json-read-literal port "true" #t)]
        [(char=? ch #\f) (json-read-literal port "false" #f)]
        [(char=? ch #\n) (json-read-literal port "null" 'null)]
        [(or (char=? ch #\-) (char-numeric? ch)) (json-read-number port)]
        [else (error 'read-json "unexpected character" ch)])))

  (define (json-read-string port)
    (read-char port) ;; consume "
    (let ([out (open-output-string)])
      (let loop ()
        (let ([ch (read-char port)])
          (cond
            [(eof-object? ch) (error 'read-json "unterminated string")]
            [(char=? ch #\") (get-output-string out)]
            [(char=? ch #\\)
             (let ([esc (read-char port)])
               (cond
                 [(char=? esc #\") (write-char #\" out)]
                 [(char=? esc #\\) (write-char #\\ out)]
                 [(char=? esc #\/) (write-char #\/ out)]
                 [(char=? esc #\b) (write-char #\backspace out)]
                 [(char=? esc #\f) (write-char #\page out)]
                 [(char=? esc #\n) (write-char #\newline out)]
                 [(char=? esc #\r) (write-char #\return out)]
                 [(char=? esc #\t) (write-char #\tab out)]
                 [(char=? esc #\u)
                  (let ([hex (json-read-hex4 port)])
                    (if (and (>= hex #xD800) (<= hex #xDBFF))
                      (begin
                        (read-char port) ;; backslash
                        (read-char port) ;; u
                        (let ([low (json-read-hex4 port)])
                          (write-char (integer->char
                            (+ #x10000
                               (bitwise-arithmetic-shift-left (- hex #xD800) 10)
                               (- low #xDC00))) out)))
                      (write-char (integer->char hex) out)))]
                 [else (error 'read-json "invalid escape" esc)])
               (loop))]
            [else (write-char ch out) (loop)])))))

  (define (json-read-hex4 port)
    (let ([s (make-string 4)])
      (do ([i 0 (+ i 1)]) ((= i 4))
        (string-set! s i (read-char port)))
      (string->number s 16)))

  (define (json-read-number port)
    (let ([out (open-output-string)])
      (let loop ()
        (let ([ch (peek-char port)])
          (if (and (char? ch)
                   (or (char-numeric? ch)
                       (memv ch '(#\- #\+ #\. #\e #\E))))
            (begin (write-char (read-char port) out) (loop))
            (or (string->number (get-output-string out))
                (error 'read-json "invalid number")))))))

  (define (json-read-literal port expected value)
    (do ([i 0 (+ i 1)]) ((= i (string-length expected)) value)
      (let ([ch (read-char port)])
        (unless (char=? ch (string-ref expected i))
          (error 'read-json "expected literal" expected)))))

  (define (json-read-object port)
    (read-char port) ;; consume {
    (json-skip-ws port)
    (let ([ht (make-hashtable string-hash string=?)])
      (if (char=? (peek-char port) #\})
        (begin (read-char port) ht)
        (let loop ()
          (json-skip-ws port)
          (let ([key (json-read-string port)])
            (json-skip-ws port)
            (read-char port) ;; consume :
            (let ([val (json-read-value port)])
              (hashtable-set! ht key val)
              (json-skip-ws port)
              (let ([ch (read-char port)])
                (cond
                  [(char=? ch #\}) ht]
                  [(char=? ch #\,) (loop)]
                  [else (error 'read-json "expected , or }" ch)]))))))))

  (define (json-read-array port)
    (read-char port) ;; consume [
    (json-skip-ws port)
    (if (char=? (peek-char port) #\])
      (begin (read-char port) '())
      (let loop ([items '()])
        (let ([val (json-read-value port)])
          (json-skip-ws port)
          (let ([ch (read-char port)])
            (cond
              [(char=? ch #\]) (reverse (cons val items))]
              [(char=? ch #\,) (loop (cons val items))]
              [else (error 'read-json "expected , or ]" ch)]))))))

  ;; ---- Writer ----

  (define (write-json val . args)
    (let ([port (if (null? args) (current-output-port) (car args))])
      (json-write-value val port)))

  (define (json-object->string val)
    (let ([p (open-output-string)])
      (json-write-value val p)
      (get-output-string p)))

  (define (json-write-value val port)
    (cond
      [(string? val) (json-write-string val port)]
      [(and (integer? val) (exact? val))
       (put-string port (number->string val))]
      [(number? val) (put-string port (number->string (inexact val)))]
      [(eq? val #t) (put-string port "true")]
      [(eq? val #f) (put-string port "false")]
      [(eq? val 'null) (put-string port "null")]
      [(null? val) (put-string port "null")]
      [(list? val) (json-write-array val port)]
      [(hashtable? val) (json-write-object val port)]
      [(vector? val) (json-write-vector val port)]
      [else (error 'write-json "cannot serialize" val)]))

  (define (json-write-string str port)
    (put-char port #\")
    (do ([i 0 (+ i 1)]) ((= i (string-length str)))
      (let ([c (string-ref str i)])
        (cond
          [(char=? c #\") (put-string port "\\\"")]
          [(char=? c #\\) (put-string port "\\\\")]
          [(char=? c #\newline) (put-string port "\\n")]
          [(char=? c #\return) (put-string port "\\r")]
          [(char=? c #\tab) (put-string port "\\t")]
          [(char=? c #\backspace) (put-string port "\\b")]
          [(char=? c #\page) (put-string port "\\f")]
          [(< (char->integer c) #x20)
           (put-string port (format "\\u~4,'0x" (char->integer c)))]
          [else (put-char port c)])))
    (put-char port #\"))

  (define (json-write-object ht port)
    (put-char port #\{)
    (let-values ([(keys vals) (hashtable-entries ht)])
      (do ([i 0 (+ i 1)]) ((= i (vector-length keys)))
        (when (> i 0) (put-char port #\,))
        (json-write-string (vector-ref keys i) port)
        (put-char port #\:)
        (json-write-value (vector-ref vals i) port)))
    (put-char port #\}))

  (define (json-write-array lst port)
    (put-char port #\[)
    (let loop ([rest lst] [first? #t])
      (unless (null? rest)
        (unless first? (put-char port #\,))
        (json-write-value (car rest) port)
        (loop (cdr rest) #f)))
    (put-char port #\]))

  (define (json-write-vector vec port)
    (put-char port #\[)
    (do ([i 0 (+ i 1)]) ((= i (vector-length vec)))
      (when (> i 0) (put-char port #\,))
      (json-write-value (vector-ref vec i) port))
    (put-char port #\]))

  (define (hashtable? x)
    (guard (e [#t #f])
      (hashtable-size x)
      #t))

  ) ;; end library
