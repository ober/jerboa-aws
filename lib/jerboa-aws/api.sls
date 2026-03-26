#!chezscheme
;;; (jerboa-aws api) -- AWS Query API client
;;; Used by EC2, STS, IAM, SNS, CloudFormation, RDS, ELBv2, CloudWatch

(library (jerboa-aws api)
  (export make-aws-client aws-client?
          aws-client-endpoint aws-client-access-key aws-client-secret-key
          aws-client-region aws-client-token aws-client-service
          aws-client-api-version
          aws-query-request aws-query-action aws-query-action/hash
          AWSClient AWSClientError)

  (import (except (chezscheme) hashtable?)
          (jerboa-aws creds)
          (jerboa-aws crypto)
          (jerboa-aws sigv4)
          (jerboa-aws uri)
          (jerboa-aws time)
          (jerboa-aws xml)
          (jerboa-aws json)
          (jerboa-aws request))

  ;; --- Client record ---
  (define-record-type aws-client
    (fields endpoint access-key secret-key region token
            service api-version)
    (protocol
      (lambda (new)
        (lambda (endpoint access-key secret-key region token
                 service api-version)
          (new endpoint access-key secret-key region token
               service api-version)))))

  ;; Client factory with credential resolution
  (define (AWSClient . args)
    (let ([service (kw-ref args 'service: #f)]
          [endpoint (kw-ref args 'endpoint: #f)]
          [api-version (kw-ref args 'api-version: "")]
          [profile (kw-ref args 'profile: #f)]
          [access-key (kw-ref args 'access-key: #f)]
          [secret-key (kw-ref args 'secret-key: #f)]
          [region (kw-ref args 'region: #f)]
          [token (kw-ref args 'token: #f)])
      (unless service
        (error 'AWSClient "service is required"))
      (let-values ([(r-ak r-sk r-region r-token)
                    (aws-resolve-credentials profile)])
        (let ([ak (or access-key r-ak)]
              [sk (or secret-key r-sk)]
              [reg (or region r-region)]
              [tok (or token r-token)])
          (unless ak (error 'AWSClient "access key is required"))
          (unless sk (error 'AWSClient "secret key is required"))
          (make-aws-client
            (or endpoint (string-append service "." reg ".amazonaws.com"))
            ak sk reg tok service api-version)))))

  ;; Error type
  (define (AWSClientError message . irritants)
    (apply error 'AWSClientError message irritants))

  ;; --- Query API request ---
  (define (aws-query-request client action params)
    (let* ([all-params (cons (cons "Action" action)
                        (if (> (string-length (aws-client-api-version client)) 0)
                          (cons (cons "Version" (aws-client-api-version client))
                                params)
                          params))]
           [body-str (form-url-encode all-params)]
           [body-bytes (string->utf8 body-str)]
           [body-hash (sha256 body-bytes)]
           [ts (aws-timestamp)]
           [ds (aws-datestamp)]
           [host (aws-client-endpoint client)]
           [headers (list (cons "Host" host)
                          (cons "x-amz-date" ts)
                          (cons "Content-Type" "application/x-www-form-urlencoded"))]
           [headers (if (aws-client-token client)
                      (append headers
                        (list (cons "X-Amz-Security-Token"
                                    (aws-client-token client))))
                      headers)]
           [auth (sigv4-sign "POST" "/" ""
                   headers body-hash
                   (aws-client-region client)
                   (aws-client-service client)
                   (aws-client-access-key client)
                   (aws-client-secret-key client)
                   ts ds)]
           [all-headers (cons (cons "Authorization" auth) headers)]
           ;; Convert (name . value) pairs to (name :: value) for chez-https
           [https-headers (map (lambda (h) (list (car h) ':: (cdr h)))
                               all-headers)]
           [url (string-append "https://" host "/")]
           [req (http-post url 'headers: https-headers 'data: body-str)]
           [status (request-status req)])
      (if (and (>= status 200) (< status 300))
        (let ([body (request-text req)])
          (request-close req)
          body)
        (let ([body (request-text req)])
          (request-close req)
          (let ([parsed (guard (e [#t #f])
                          (aws-response->hash body))])
            (if (and parsed (hashtable? parsed))
              (let ([err-code (ht-ref parsed 'Code "Unknown")]
                    [err-msg (ht-ref parsed 'Message body)])
                (error 'AWSClientError
                  (string-append err-code ": " err-msg)
                  status))
              (error 'AWSClientError
                (string-append "HTTP " (number->string status))
                body)))))))

  ;; Execute a Query API action and return raw XML text
  (define (aws-query-action client action . params)
    (aws-query-request client action
      (if (null? params) '() (car params))))

  ;; Execute a Query API action and return parsed hash table
  (define (aws-query-action/hash client action . params)
    (let ([xml-text (apply aws-query-action client action params)])
      (aws-response->hash xml-text)))

  ;; --- Helpers ---

  (define (kw-ref args key default)
    (let loop ([rest args])
      (cond
        [(null? rest) default]
        [(and (pair? (cdr rest)) (eq? (car rest) key))
         (cadr rest)]
        [else (loop (cdr rest))])))

  (define (ht-ref ht key default)
    (if (hashtable? ht)
      (let ([v (hashtable-ref ht key #f)])
        (or v default))
      default))

  (define (hashtable? x)
    (guard (e [#t #f])
      (hashtable-size x)
      #t))

  ) ;; end library
