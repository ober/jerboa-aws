#!chezscheme
;;; (jerboa-aws s3 buckets) -- S3 Bucket operations

(library (jerboa-aws s3 buckets)
  (export list-buckets create-bucket delete-bucket head-bucket
          get-bucket-location
          get-bucket-versioning put-bucket-versioning
          get-bucket-tagging put-bucket-tagging delete-bucket-tagging)
  (import (chezscheme)
          (jerboa-aws request)
          (jerboa-aws s3 api))

  (define (list-buckets client)
    (s3-request/xml client 'verb: "GET"))

  (define (create-bucket client bucket-name . args)
    (let ([region (kw-ref args 'region: #f)])
      (let ([body (if (and region (not (string=? region "us-east-1")))
                    (string-append
                      "<CreateBucketConfiguration xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\">"
                      "<LocationConstraint>" region "</LocationConstraint>"
                      "</CreateBucketConfiguration>")
                    #f)])
        (let ([req (s3-request/check client
                     'verb: "PUT"
                     'bucket: bucket-name
                     'body: body
                     'content-type: (and body "application/xml"))])
          (request-close req)
          (void)))))

  (define (delete-bucket client bucket-name)
    (let ([req (s3-request/check client
                 'verb: "DELETE"
                 'bucket: bucket-name)])
      (request-close req)
      (void)))

  (define (head-bucket client bucket-name)
    (let* ([req (s3-request client
                  'verb: "HEAD"
                  'bucket: bucket-name)]
           [status (request-status req)])
      (request-close req)
      (and (>= status 200) (< status 300))))

  (define (get-bucket-location client bucket-name)
    (s3-request/xml client
      'verb: "GET"
      'bucket: bucket-name
      'query: (list (cons "location" ""))))

  (define (get-bucket-versioning client bucket-name)
    (s3-request/xml client
      'verb: "GET"
      'bucket: bucket-name
      'query: (list (cons "versioning" ""))))

  (define (put-bucket-versioning client bucket-name status)
    (let* ([body (string-append
                   "<VersioningConfiguration xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\">"
                   "<Status>" status "</Status>"
                   "</VersioningConfiguration>")]
           [req (s3-request/check client
                  'verb: "PUT"
                  'bucket: bucket-name
                  'query: (list (cons "versioning" ""))
                  'body: body
                  'content-type: "application/xml")])
      (request-close req)
      (void)))

  (define (get-bucket-tagging client bucket-name)
    (s3-request/xml client
      'verb: "GET"
      'bucket: bucket-name
      'query: (list (cons "tagging" ""))))

  (define (put-bucket-tagging client bucket-name tags)
    (let* ([tag-xml (apply string-append
                     (map (lambda (t)
                            (string-append
                              "<Tag><Key>" (car t) "</Key>"
                              "<Value>" (cdr t) "</Value></Tag>"))
                          tags))]
           [body (string-append
                   "<Tagging><TagSet>" tag-xml "</TagSet></Tagging>")]
           [req (s3-request/check client
                  'verb: "PUT"
                  'bucket: bucket-name
                  'query: (list (cons "tagging" ""))
                  'body: body
                  'content-type: "application/xml")])
      (request-close req)
      (void)))

  (define (delete-bucket-tagging client bucket-name)
    (let ([req (s3-request/check client
                 'verb: "DELETE"
                 'bucket: bucket-name
                 'query: (list (cons "tagging" "")))])
      (request-close req)
      (void)))

  ;; --- Helpers ---
  (define (kw-ref args key default)
    (let loop ([rest args])
      (cond
        [(null? rest) default]
        [(and (pair? (cdr rest)) (eq? (car rest) key))
         (cadr rest)]
        [else (loop (cdr rest))])))

  ) ;; end library
