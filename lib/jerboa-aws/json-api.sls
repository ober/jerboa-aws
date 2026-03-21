#!chezscheme
;;; (jerboa-aws json-api) -- AWS JSON API client
;;; Used by DynamoDB, Lambda, CloudWatch Logs, SQS, SSM, etc.

(library (jerboa-aws json-api)
  (export make-aws-json-client aws-json-client?
          aws-json-client-endpoint aws-json-client-access-key
          aws-json-client-secret-key aws-json-client-region
          aws-json-client-token aws-json-client-service
          aws-json-client-target-prefix aws-json-client-content-type
          aws-json-request aws-json-action
          AWSJsonClient AWSJsonClientError)

  (import (chezscheme)
          (jerboa-aws creds)
          (jerboa-aws crypto)
          (jerboa-aws sigv4)
          (jerboa-aws time)
          (jerboa-aws json)
          (jerboa-aws request))

  ;; --- Client record ---
  (define-record-type aws-json-client
    (fields endpoint access-key secret-key region token
            service target-prefix content-type)
    (protocol
      (lambda (new)
        (lambda (endpoint access-key secret-key region token
                 service target-prefix content-type)
          (new endpoint access-key secret-key region token
               service target-prefix content-type)))))

  ;; Client factory
  (define (AWSJsonClient . args)
    (let ([service (kw-ref args 'service: #f)]
          [endpoint (kw-ref args 'endpoint: #f)]
          [target-prefix (kw-ref args 'target-prefix: "")]
          [content-type (kw-ref args 'content-type: "application/x-amz-json-1.0")]
          [profile (kw-ref args 'profile: #f)]
          [access-key (kw-ref args 'access-key: #f)]
          [secret-key (kw-ref args 'secret-key: #f)]
          [region (kw-ref args 'region: #f)]
          [token (kw-ref args 'token: #f)])
      (unless service
        (error 'AWSJsonClient "service is required"))
      (let-values ([(r-ak r-sk r-region r-token)
                    (aws-resolve-credentials profile)])
        (let ([ak (or access-key r-ak)]
              [sk (or secret-key r-sk)]
              [reg (or region r-region)]
              [tok (or token r-token)])
          (unless ak (error 'AWSJsonClient "access key is required"))
          (unless sk (error 'AWSJsonClient "secret key is required"))
          (make-aws-json-client
            (or endpoint (string-append service "." reg ".amazonaws.com"))
            ak sk reg tok service target-prefix content-type)))))

  ;; Error
  (define (AWSJsonClientError message . irritants)
    (apply error 'AWSJsonClientError message irritants))

  ;; --- JSON API request ---
  (define (aws-json-request client target payload)
    (let* ([body-str (if payload (json-object->string payload) "{}")]
           [body-bytes (string->utf8 body-str)]
           [body-hash (sha256 body-bytes)]
           [ts (aws-timestamp)]
           [ds (aws-datestamp)]
           [host (aws-json-client-endpoint client)]
           [headers (list (cons "Host" host)
                          (cons "x-amz-date" ts)
                          (cons "Content-Type"
                                (aws-json-client-content-type client))
                          (cons "X-Amz-Target" target))]
           [headers (if (aws-json-client-token client)
                      (append headers
                        (list (cons "X-Amz-Security-Token"
                                    (aws-json-client-token client))))
                      headers)]
           [auth (sigv4-sign "POST" "/" ""
                   headers body-hash
                   (aws-json-client-region client)
                   (aws-json-client-service client)
                   (aws-json-client-access-key client)
                   (aws-json-client-secret-key client)
                   ts ds)]
           [all-headers (cons (cons "Authorization" auth) headers)]
           [url (string-append "https://" host "/")]
           [req (http-post url 'headers: all-headers 'data: body-str)]
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
              (let* ([type-raw (or (ht-ref json "__type")
                                   (ht-ref json "code")
                                   "Unknown")]
                     [type-str (if (string? type-raw)
                                 (let ([pos (string-find type-raw #\#)])
                                   (if pos
                                     (substring type-raw (+ pos 1)
                                                (string-length type-raw))
                                     type-raw))
                                 (format "~a" type-raw))]
                     [msg (or (ht-ref json "message")
                              (ht-ref json "Message")
                              content)])
                (error 'AWSJsonClientError
                  (string-append type-str ": " msg) status))
              (error 'AWSJsonClientError
                (string-append "HTTP " (number->string status))
                content)))))))

  ;; Execute a JSON API action
  (define aws-json-action
    (case-lambda
      [(client action)
       (aws-json-request client
         (string-append (aws-json-client-target-prefix client) "." action)
         #f)]
      [(client action payload)
       (aws-json-request client
         (string-append (aws-json-client-target-prefix client) "." action)
         payload)]))

  ;; --- Helpers ---

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

  (define (string-find str ch)
    (let ([len (string-length str)])
      (let loop ([i 0])
        (cond
          [(>= i len) #f]
          [(char=? (string-ref str i) ch) i]
          [else (loop (+ i 1))]))))

  ) ;; end library
