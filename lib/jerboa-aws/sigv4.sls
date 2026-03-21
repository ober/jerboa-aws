#!chezscheme
;;; (jerboa-aws sigv4) -- AWS Signature Version 4

(library (jerboa-aws sigv4)
  (export sigv4-sign)

  (import (chezscheme)
          (jerboa-aws crypto))

  ;; Sign an AWS request using SigV4.
  ;; Returns the Authorization header value.
  ;;
  ;; Parameters:
  ;;   method      - HTTP method string ("POST", "GET", etc.)
  ;;   uri         - Request URI path
  ;;   query       - Query string (or "")
  ;;   headers     - Alist of (name . value) pairs
  ;;   body-hash   - SHA256 hash of request body (bytevector)
  ;;   region      - AWS region
  ;;   service     - AWS service name
  ;;   access-key  - AWS access key
  ;;   secret-key  - AWS secret key
  ;;   timestamp   - ISO 8601 timestamp "YYYYMMDDTHHMMSSZ"
  ;;   datestamp   - Date string "YYYYMMDD"
  (define (sigv4-sign method uri query headers body-hash
                      region service access-key secret-key
                      timestamp datestamp)
    (let* (;; Sort headers by lowercase name
           [sorted-headers
            (list-sort
              (lambda (a b) (string<? (string-downcase (car a))
                                       (string-downcase (car b))))
              headers)]
           ;; Build canonical headers
           [canonical-headers
            (apply string-append
              (map (lambda (h)
                     (string-append (string-downcase (car h)) ":"
                                    (string-trim (cdr h)) "\n"))
                   sorted-headers))]
           ;; Build signed headers list
           [signed-headers
            (string-join
              (map (lambda (h) (string-downcase (car h))) sorted-headers)
              ";")]
           ;; Build canonical request
           [body-hash-hex (if (bytevector? body-hash)
                            (hex-encode body-hash)
                            body-hash)]
           [canonical-request
            (string-append method "\n"
                           uri "\n"
                           (or query "") "\n"
                           canonical-headers "\n"
                           signed-headers "\n"
                           body-hash-hex)]
           ;; Build credential scope
           [scope (string-append datestamp "/" region "/" service "/aws4_request")]
           ;; Build string to sign
           [string-to-sign
            (string-append "AWS4-HMAC-SHA256\n"
                           timestamp "\n"
                           scope "\n"
                           (hex-encode (sha256 (string->utf8 canonical-request))))]
           ;; Derive signing key
           [date-key (hmac-sha256
                       (string->utf8 (string-append "AWS4" secret-key))
                       (string->utf8 datestamp))]
           [region-key (hmac-sha256 date-key (string->utf8 region))]
           [service-key (hmac-sha256 region-key (string->utf8 service))]
           [signing-key (hmac-sha256 service-key (string->utf8 "aws4_request"))]
           ;; Compute signature
           [signature (hex-encode
                        (hmac-sha256 signing-key (string->utf8 string-to-sign)))])
      ;; Return Authorization header value
      (string-append
        "AWS4-HMAC-SHA256 Credential=" access-key "/" scope ", "
        "SignedHeaders=" signed-headers ", "
        "Signature=" signature)))

  ;; --- Helpers ---

  (define (string-trim str)
    (let* ([len (string-length str)]
           [start (let loop ([i 0])
                    (if (and (< i len) (char-whitespace? (string-ref str i)))
                      (loop (+ i 1)) i))]
           [end (let loop ([i (- len 1)])
                  (if (and (>= i start) (char-whitespace? (string-ref str i)))
                    (loop (- i 1)) (+ i 1)))])
      (substring str start end)))

  (define (string-join strs sep)
    (if (null? strs) ""
      (let loop ([rest (cdr strs)] [out (car strs)])
        (if (null? rest) out
          (loop (cdr rest) (string-append out sep (car rest)))))))

  ) ;; end library
