#!chezscheme
;;; (jerboa-aws s3 objects) -- S3 Object operations

(library (jerboa-aws s3 objects)
  (export get-object head-object put-object delete-object
          list-objects-v2 copy-object delete-objects)
  (import (chezscheme)
          (jerboa-aws request)
          (jerboa-aws s3 api))

  (define (get-object client bucket-name key)
    (let* ([req (s3-request/check client
                  'verb: "GET"
                  'bucket: bucket-name
                  'key: key)]
           [data (request-text req)])
      (request-close req)
      data))

  (define (head-object client bucket-name key)
    (let* ([req (s3-request/check client
                  'verb: "HEAD"
                  'bucket: bucket-name
                  'key: key)]
           [headers (request-headers req)])
      (request-close req)
      headers))

  (define (put-object client bucket-name key data . args)
    (let ([content-type (kw-ref args 'content-type: "application/octet-stream")])
      (let ([req (s3-request/check client
                   'verb: "PUT"
                   'bucket: bucket-name
                   'key: key
                   'body: data
                   'content-type: content-type)])
        (request-close req)
        (void))))

  (define (delete-object client bucket-name key)
    (let ([req (s3-request/check client
                 'verb: "DELETE"
                 'bucket: bucket-name
                 'key: key)])
      (request-close req)
      (void)))

  (define (list-objects-v2 client bucket-name . args)
    (let ([prefix (kw-ref args 'prefix: #f)]
          [delimiter (kw-ref args 'delimiter: #f)]
          [max-keys (kw-ref args 'max-keys: #f)]
          [continuation-token (kw-ref args 'continuation-token: #f)]
          [start-after (kw-ref args 'start-after: #f)])
      (let ([query (append
                     (list (cons "list-type" "2"))
                     (if prefix (list (cons "prefix" prefix)) '())
                     (if delimiter (list (cons "delimiter" delimiter)) '())
                     (if max-keys
                       (list (cons "max-keys"
                               (if (number? max-keys)
                                 (number->string max-keys)
                                 max-keys)))
                       '())
                     (if continuation-token
                       (list (cons "continuation-token" continuation-token))
                       '())
                     (if start-after
                       (list (cons "start-after" start-after))
                       '()))])
        (s3-request/xml client
          'verb: "GET"
          'bucket: bucket-name
          'query: query))))

  (define (copy-object client bucket-name key source)
    (let ([req (s3-request/check client
                 'verb: "PUT"
                 'bucket: bucket-name
                 'key: key
                 'extra-headers: (list (cons "x-amz-copy-source" source)))])
      (request-close req)
      (void)))

  (define (delete-objects client bucket-name keys . args)
    (let ([quiet (kw-ref args 'quiet: #t)])
      (let* ([objects-xml (apply string-append
                           (map (lambda (k)
                                  (string-append "<Object><Key>" k "</Key></Object>"))
                                keys))]
             [body (string-append
                     "<Delete>"
                     (if quiet "<Quiet>true</Quiet>" "")
                     objects-xml
                     "</Delete>")]
             [req (s3-request/check client
                    'verb: "POST"
                    'bucket: bucket-name
                    'query: (list (cons "delete" ""))
                    'body: body
                    'content-type: "application/xml")])
        (request-close req)
        (void))))

  ;; --- Helpers ---
  (define (kw-ref args key default)
    (let loop ([rest args])
      (cond
        [(null? rest) default]
        [(and (pair? (cdr rest)) (eq? (car rest) key))
         (cadr rest)]
        [else (loop (cdr rest))])))

  ) ;; end library
