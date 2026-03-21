#!chezscheme
;;; (jerboa-aws lambda api) -- Lambda REST JSON API client
;;; Lambda uses REST-style path-based requests, not X-Amz-Target

(library (jerboa-aws lambda api)
  (export LambdaClient lambda-rest-request)
  (import (chezscheme)
          (jerboa-aws creds)
          (jerboa-aws crypto)
          (jerboa-aws sigv4)
          (jerboa-aws time)
          (jerboa-aws json)
          (jerboa-aws request))

  ;; --- Client record ---
  (define-record-type lambda-client
    (fields endpoint access-key secret-key region token)
    (protocol
      (lambda (new)
        (lambda (endpoint access-key secret-key region token)
          (new endpoint access-key secret-key region token)))))

  ;; Client factory
  (define (LambdaClient . args)
    (let ([profile (kw-ref args 'profile: #f)]
          [access-key (kw-ref args 'access-key: #f)]
          [secret-key (kw-ref args 'secret-key: #f)]
          [region (kw-ref args 'region: #f)]
          [token (kw-ref args 'token: #f)])
      (let-values ([(r-ak r-sk r-region r-token)
                    (aws-resolve-credentials profile)])
        (let ([ak (or access-key r-ak)]
              [sk (or secret-key r-sk)]
              [reg (or region r-region)]
              [tok (or token r-token)])
          (unless ak (error 'LambdaClient "access key is required"))
          (unless sk (error 'LambdaClient "secret key is required"))
          (make-lambda-client
            (string-append "lambda." reg ".amazonaws.com")
            ak sk reg tok)))))

  ;; Precomputed empty SHA256
  (define empty-sha256 (sha256 #vu8()))

  ;; Make a signed REST request to the Lambda API
  ;; Returns parsed JSON hash table
  ;; query: alist of (key . value) query parameters, or #f
  (define (lambda-rest-request client . args)
    (let ([verb (kw-ref args 'verb: "GET")]
          [path (kw-ref args 'path: "/")]
          [query (kw-ref args 'query: #f)]
          [payload (kw-ref args 'payload: #f)])
      (let* ([body-str (if payload (json-object->string payload) #f)]
             [body-bytes (if body-str (string->utf8 body-str) #f)]
             [body-hash (if body-bytes (sha256 body-bytes) empty-sha256)]
             [query-string (if (and query (pair? query))
                             (encode-query-string query)
                             "")]
             [ts (aws-timestamp)]
             [ds (aws-datestamp)]
             [host (lambda-client-endpoint client)]
             [headers (list (cons "Host" host)
                            (cons "x-amz-date" ts)
                            (cons "x-amz-content-sha256" (hex-encode body-hash)))]
             [headers (if body-str
                        (append headers (list (cons "Content-Type" "application/json")))
                        headers)]
             [headers (if (lambda-client-token client)
                        (append headers
                          (list (cons "X-Amz-Security-Token"
                                      (lambda-client-token client))))
                        headers)]
             [auth (sigv4-sign verb path query-string
                     headers body-hash
                     (lambda-client-region client)
                     "lambda"
                     (lambda-client-access-key client)
                     (lambda-client-secret-key client)
                     ts ds)]
             [all-headers (cons (cons "Authorization" auth) headers)]
             [url (if (string=? query-string "")
                    (string-append "https://" host path)
                    (string-append "https://" host path "?" query-string))]
             [req (cond
                    [(string=? verb "GET")
                     (http-get url 'headers: all-headers)]
                    [(string=? verb "POST")
                     (http-post url 'headers: all-headers 'data: (or body-str ""))]
                    [(string=? verb "PUT")
                     (http-put url 'headers: all-headers 'data: (or body-str ""))]
                    [(string=? verb "DELETE")
                     (http-delete url 'headers: all-headers)]
                    [else (error 'lambda-rest-request "unsupported verb" verb)])]
             [status (request-status req)])
        (if (and (>= status 200) (< status 300))
          (let ([content (request-text req)])
            (request-close req)
            (if (and content (> (string-length content) 0))
              (string->json-object content)
              (make-hashtable string-hash string=?)))
          (let ([content (request-text req)])
            (request-close req)
            (let ([json (guard (e [#t #f])
                          (string->json-object content))])
              (if json
                (let ([msg (or (ht-ref json "Message")
                               (ht-ref json "message")
                               content)]
                      [type (or (ht-ref json "Type")
                                (ht-ref json "__type")
                                "Unknown")])
                  (error 'LambdaClientError
                    (string-append type ": " msg) status))
                (error 'LambdaClientError
                  (string-append "HTTP " (number->string status))
                  content))))))))

  ;; --- Helpers ---

  ;; URI-encode a string per RFC 3986 (unreserved chars only)
  (define (uri-encode str)
    (let ([out (open-output-string)])
      (do ([i 0 (+ i 1)])
          ((= i (string-length str)) (get-output-string out))
        (let ([ch (string-ref str i)])
          (if (or (char-alphabetic? ch) (char-numeric? ch)
                  (char=? ch #\-) (char=? ch #\_)
                  (char=? ch #\.) (char=? ch #\~))
            (write-char ch out)
            (let ([bv (string->utf8 (string ch))])
              (do ([j 0 (+ j 1)])
                  ((= j (bytevector-length bv)))
                (let ([b (bytevector-u8-ref bv j)])
                  (put-string out (string-append
                    "%" (if (< b 16) "0" "")
                    (string-upcase
                      (number->string b 16))))))))))))

  ;; Encode an alist of (key . value) into a query string.
  ;; Keys are sorted alphabetically per SigV4 canonical query string rules.
  (define (encode-query-string params)
    (let ([sorted (list-sort
                    (lambda (a b) (string<? (car a) (car b)))
                    params)])
      (let loop ([rest sorted] [out ""])
        (if (null? rest) out
          (let* ([pair (car rest)]
                 [encoded (string-append
                            (uri-encode (car pair)) "="
                            (uri-encode (cdr pair)))])
            (loop (cdr rest)
                  (if (string=? out "")
                    encoded
                    (string-append out "&" encoded))))))))

  (define (kw-ref args key default)
    (let loop ([rest args])
      (cond
        [(null? rest) default]
        [(and (pair? (cdr rest)) (eq? (car rest) key))
         (cadr rest)]
        [else (loop (cdr rest))])))

  (define (ht-ref ht key)
    (guard (e [#t #f])
      (hashtable-ref ht key #f)))

  ) ;; end library
