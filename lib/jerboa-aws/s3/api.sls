#!chezscheme
;;; (jerboa-aws s3 api) -- S3 REST API client
;;; S3 uses REST-style requests with virtual-hosted-style addressing

(library (jerboa-aws s3 api)
  (export make-s3-client S3Client
          s3-request s3-request/xml s3-request/check
          s3-get s3-put s3-delete s3-head)

  (import (chezscheme)
          (jerboa-aws creds)
          (jerboa-aws crypto)
          (jerboa-aws sigv4)
          (jerboa-aws time)
          (jerboa-aws xml)
          (jerboa-aws request))

  ;; --- Client record ---
  (define-record-type s3-client
    (fields endpoint access-key secret-key region token)
    (protocol
      (lambda (new)
        (lambda (endpoint access-key secret-key region token)
          (new endpoint access-key secret-key region token)))))

  ;; Client factory with credential resolution
  (define (S3Client . args)
    (let ([profile (kw-ref args 'profile: #f)]
          [endpoint (kw-ref args 'endpoint: #f)]
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
          (unless ak (error 'S3Client "access key is required"))
          (unless sk (error 'S3Client "secret key is required"))
          (make-s3-client
            (or endpoint "s3.amazonaws.com")
            ak sk reg tok)))))

  ;; Precomputed empty SHA256
  (define empty-sha256 (sha256 #vu8()))

  ;; Core S3 request with SigV4 signing
  ;; verb: "GET", "PUT", "DELETE", "HEAD", "POST"
  ;; bucket: bucket name (optional)
  ;; key: object key (optional)
  ;; query: alist of query params
  ;; body: request body string (optional)
  ;; content-type: content type (optional)
  ;; extra-headers: additional headers alist
  (define (s3-request client . args)
    (let ([verb (kw-ref args 'verb: "GET")]
          [bucket (kw-ref args 'bucket: #f)]
          [key (kw-ref args 'key: #f)]
          [query (kw-ref args 'query: #f)]
          [body (kw-ref args 'body: #f)]
          [content-type (kw-ref args 'content-type: #f)]
          [extra-headers (kw-ref args 'extra-headers: '())])
      (let* ([body-hash (if body
                           (sha256 (if (string? body) (string->utf8 body) body))
                           empty-sha256)]
             [ts (aws-timestamp)]
             [ds (aws-datestamp)]
             [host (if bucket
                     (string-append bucket "." (s3-client-endpoint client))
                     (s3-client-endpoint client))]
             [path (if key (string-append "/" key) "/")]
             [headers (list (cons "Host" host)
                            (cons "x-amz-date" ts)
                            (cons "x-amz-content-sha256" (hex-encode body-hash)))]
             [headers (if content-type
                        (append headers (list (cons "Content-Type" content-type)))
                        headers)]
             [headers (if (s3-client-token client)
                        (append headers
                          (list (cons "X-Amz-Security-Token"
                                      (s3-client-token client))))
                        headers)]
             [headers (if (null? extra-headers)
                        headers
                        (append headers extra-headers))]
             [auth (sigv4-sign verb path ""
                     headers body-hash
                     (s3-client-region client)
                     "s3"
                     (s3-client-access-key client)
                     (s3-client-secret-key client)
                     ts ds)]
             [all-headers (cons (cons "Authorization" auth) headers)]
             [url (string-append "https://" host path)])
        (cond
          [(string=? verb "GET")
           (http-get url 'headers: all-headers)]
          [(string=? verb "PUT")
           (http-put url 'headers: all-headers 'data: (or body ""))]
          [(string=? verb "DELETE")
           (http-delete url 'headers: all-headers)]
          [(string=? verb "HEAD")
           (http-head url 'headers: all-headers)]
          [(string=? verb "POST")
           (http-post url 'headers: all-headers 'data: (or body ""))]
          [else (error 's3-request "unsupported verb" verb)]))))

  ;; S3 request returning parsed XML hash
  (define (s3-request/xml client . args)
    (let* ([req (apply s3-request client args)]
           [status (request-status req)])
      (if (and (>= status 200) (< status 300))
        (let ([body (request-text req)])
          (request-close req)
          (if (and body (> (string-length body) 0))
            (aws-response->hash body)
            (make-hashtable symbol-hash eq?)))
        (let ([body (request-text req)])
          (request-close req)
          (s3-error-raise 's3-request/xml status body)))))

  ;; S3 request returning raw request on success, error on failure
  (define (s3-request/check client . args)
    (let* ([req (apply s3-request client args)]
           [status (request-status req)])
      (if (and (>= status 200) (< status 300))
        req
        (let ([body (request-text req)])
          (request-close req)
          (s3-error-raise 's3-request/check status body)))))

  ;; Convenience wrappers
  (define (s3-get client bucket key)
    (let* ([req (s3-request/check client 'verb: "GET" 'bucket: bucket 'key: key)]
           [body (request-text req)])
      (request-close req)
      body))

  (define (s3-put client bucket key data . args)
    (let ([content-type (kw-ref args 'content-type: "application/octet-stream")])
      (let ([req (s3-request/check client
                   'verb: "PUT" 'bucket: bucket 'key: key
                   'body: data 'content-type: content-type)])
        (request-close req)
        (void))))

  (define (s3-delete client bucket key)
    (let ([req (s3-request/check client 'verb: "DELETE" 'bucket: bucket 'key: key)])
      (request-close req)
      (void)))

  (define (s3-head client bucket key)
    (let* ([req (s3-request client 'verb: "HEAD" 'bucket: bucket 'key: key)]
           [status (request-status req)])
      (request-close req)
      (and (>= status 200) (< status 300))))

  ;; --- Helpers ---
  (define (s3-error-raise who status body)
    (let ([parsed (guard (e [#t #f])
                    (aws-response->hash body))])
      (if parsed
        (let ([code (ht-ref parsed 'Code "Unknown")]
              [msg (ht-ref parsed 'Message body)])
          (error who (string-append code ": " msg) status))
        (error who (string-append "HTTP " (number->string status)) body))))

  (define (ht-ref ht key default)
    (guard (e [#t default])
      (let ([v (hashtable-ref ht key #f)])
        (or v default))))

  (define (kw-ref args key default)
    (let loop ([rest args])
      (cond
        [(null? rest) default]
        [(and (pair? (cdr rest)) (eq? (car rest) key))
         (cadr rest)]
        [else (loop (cdr rest))])))

  ) ;; end library
