#!chezscheme
;;; (jerboa-aws lambda functions) -- Lambda Function operations

(library (jerboa-aws lambda functions)
  (export list-functions get-function invoke-function
          create-function delete-function
          update-function-code update-function-configuration)
  (import (chezscheme)
          (jerboa-aws lambda api))

  (define (list-functions client . args)
    (let ([max-items (kw-ref args 'max-items: #f)]
          [marker (kw-ref args 'marker: #f)])
      (lambda-rest-request client
        'verb: "GET"
        'path: "/2015-03-31/functions"
        'query: (append
                  (if max-items
                    (list (cons "MaxItems" (->string max-items)))
                    '())
                  (if marker (list (cons "Marker" marker)) '())))))

  (define (get-function client function-name)
    (lambda-rest-request client
      'verb: "GET"
      'path: (string-append "/2015-03-31/functions/" function-name)))

  (define (invoke-function client function-name . args)
    (let ([payload (kw-ref args 'payload: #f)]
          [invocation-type (kw-ref args 'invocation-type: #f)])
      (lambda-rest-request client
        'verb: "POST"
        'path: (string-append "/2015-03-31/functions/" function-name "/invocations")
        'query: (if invocation-type
                   (list (cons "InvocationType" invocation-type))
                   '())
        'payload: payload)))

  (define (create-function client . args)
    (let ([function-name (kw-ref args 'function-name: #f)]
          [runtime (kw-ref args 'runtime: #f)]
          [role (kw-ref args 'role: #f)]
          [handler (kw-ref args 'handler: #f)]
          [s3-bucket (kw-ref args 's3-bucket: #f)]
          [s3-key (kw-ref args 's3-key: #f)]
          [description (kw-ref args 'description: #f)]
          [timeout (kw-ref args 'timeout: #f)]
          [memory-size (kw-ref args 'memory-size: #f)]
          [environment (kw-ref args 'environment: #f)])
      (let ([payload (make-hashtable string-hash string=?)])
        (hashtable-set! payload "FunctionName" function-name)
        (hashtable-set! payload "Runtime" runtime)
        (hashtable-set! payload "Role" role)
        (hashtable-set! payload "Handler" handler)
        (let ([code (make-hashtable string-hash string=?)])
          (when s3-bucket (hashtable-set! code "S3Bucket" s3-bucket))
          (when s3-key (hashtable-set! code "S3Key" s3-key))
          (hashtable-set! payload "Code" code))
        (when description (hashtable-set! payload "Description" description))
        (when timeout (hashtable-set! payload "Timeout" timeout))
        (when memory-size (hashtable-set! payload "MemorySize" memory-size))
        (when environment
          (let ([env (make-hashtable string-hash string=?)])
            (hashtable-set! env "Variables" environment)
            (hashtable-set! payload "Environment" env)))
        (lambda-rest-request client
          'verb: "POST"
          'path: "/2015-03-31/functions"
          'payload: payload))))

  (define (delete-function client function-name)
    (lambda-rest-request client
      'verb: "DELETE"
      'path: (string-append "/2015-03-31/functions/" function-name))
    (void))

  (define (update-function-code client function-name . args)
    (let ([s3-bucket (kw-ref args 's3-bucket: #f)]
          [s3-key (kw-ref args 's3-key: #f)])
      (let ([payload (make-hashtable string-hash string=?)])
        (when s3-bucket (hashtable-set! payload "S3Bucket" s3-bucket))
        (when s3-key (hashtable-set! payload "S3Key" s3-key))
        (lambda-rest-request client
          'verb: "PUT"
          'path: (string-append "/2015-03-31/functions/" function-name "/code")
          'payload: payload))))

  (define (update-function-configuration client function-name . args)
    (let ([runtime (kw-ref args 'runtime: #f)]
          [handler (kw-ref args 'handler: #f)]
          [description (kw-ref args 'description: #f)]
          [timeout (kw-ref args 'timeout: #f)]
          [memory-size (kw-ref args 'memory-size: #f)]
          [role (kw-ref args 'role: #f)]
          [environment (kw-ref args 'environment: #f)])
      (let ([payload (make-hashtable string-hash string=?)])
        (when runtime (hashtable-set! payload "Runtime" runtime))
        (when handler (hashtable-set! payload "Handler" handler))
        (when description (hashtable-set! payload "Description" description))
        (when timeout (hashtable-set! payload "Timeout" timeout))
        (when memory-size (hashtable-set! payload "MemorySize" memory-size))
        (when role (hashtable-set! payload "Role" role))
        (when environment
          (let ([env (make-hashtable string-hash string=?)])
            (hashtable-set! env "Variables" environment)
            (hashtable-set! payload "Environment" env)))
        (lambda-rest-request client
          'verb: "PUT"
          'path: (string-append "/2015-03-31/functions/" function-name "/configuration")
          'payload: payload))))

  ;; --- Helpers ---
  (define (kw-ref args key default)
    (let loop ([rest args])
      (cond
        [(null? rest) default]
        [(and (pair? (cdr rest)) (eq? (car rest) key))
         (cadr rest)]
        [else (loop (cdr rest))])))

  ;; Convert a value to string for query parameters
  (define (->string v)
    (cond
      [(string? v) v]
      [(number? v) (number->string v)]
      [(symbol? v) (symbol->string v)]
      [else (format "~a" v)]))

  ) ;; end library
