#!chezscheme
;;; (jerboa-aws request) -- HTTPS HTTP client via curl
;;; Replaces Gerbil's :std/net/request for AWS API calls

(library (jerboa-aws request)
  (export http-get http-post http-put http-delete http-head
          request-status request-text request-content
          request-headers request-close)
  (import (chezscheme))

  ;; A request result: #(status headers body)
  (define (make-request-result status headers body)
    (vector status headers body))

  (define (request-status req) (vector-ref req 0))
  (define (request-text req) (vector-ref req 2))
  (define (request-content req) (string->utf8 (vector-ref req 2)))
  (define (request-headers req) (vector-ref req 1))
  (define (request-close req) (void))

  ;; Parse keyword args from a flat list
  (define (parse-kwargs args)
    (let loop ([args args] [headers '()] [data #f])
      (cond
        [(null? args) (values headers data)]
        [(and (pair? (cdr args)) (eq? (car args) 'headers:))
         (loop (cddr args) (cadr args) data)]
        [(and (pair? (cdr args)) (eq? (car args) 'data:))
         (loop (cddr args) headers (cadr args))]
        [else (loop (cdr args) headers data)])))

  ;; Shell-quote a string
  (define (shell-quote s)
    (string-append "'"
      (let loop ([i 0] [out ""])
        (if (= i (string-length s)) out
          (let ([c (string-ref s i)])
            (if (char=? c #\')
              (loop (+ i 1) (string-append out "'\\''"))
              (loop (+ i 1) (string-append out (string c)))))))
      "'"))

  ;; Run curl and return request result
  (define (run-curl method url headers data)
    (let* ([header-args
            (apply string-append
              (map (lambda (h)
                     (string-append " -H "
                       (shell-quote (string-append (car h) ": " (cdr h)))))
                   headers))]
           [data-arg (if data
                       (string-append " -d " (shell-quote data))
                       "")]
           [cmd (string-append
                  "curl -s -S -X " method
                  " -o /dev/stdout -w '\\n%{http_code}'"
                  header-args data-arg
                  " " (shell-quote url))])
      (let-values ([(to-stdin from-stdout from-stderr pid)
                    (open-process-ports cmd 'block (native-transcoder))])
        (close-port to-stdin)
        (let* ([output (get-string-all from-stdout)]
               [_err (get-string-all from-stderr)]
               [_ (close-port from-stdout)]
               [_ (close-port from-stderr)]
               [parts (split-last-line output)]
               [body (car parts)]
               [status-str (cdr parts)]
               [status (or (string->number status-str) 0)])
          (make-request-result status '() body)))))

  (define (split-last-line s)
    (let ([len (string-length s)])
      (let loop ([i (- len 1)])
        (cond
          [(< i 0) (cons "" s)]
          [(char=? (string-ref s i) #\newline)
           (cons (substring s 0 i)
                 (substring s (+ i 1) len))]
          [else (loop (- i 1))]))))

  (define (http-get url . kwargs)
    (let-values ([(headers data) (parse-kwargs kwargs)])
      (run-curl "GET" url headers data)))

  (define (http-post url . kwargs)
    (let-values ([(headers data) (parse-kwargs kwargs)])
      (run-curl "POST" url headers data)))

  (define (http-put url . kwargs)
    (let-values ([(headers data) (parse-kwargs kwargs)])
      (run-curl "PUT" url headers data)))

  (define (http-delete url . kwargs)
    (let-values ([(headers data) (parse-kwargs kwargs)])
      (run-curl "DELETE" url headers data)))

  (define (http-head url . kwargs)
    (let-values ([(headers data) (parse-kwargs kwargs)])
      (run-curl "HEAD" url headers data)))

  ) ;; end library
