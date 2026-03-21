#!chezscheme
;;; (jerboa-aws creds) -- AWS credential resolution
;;; Follows standard AWS credential chain:
;;; 1. Environment variables
;;; 2. ~/.aws/credentials file
;;; 3. ~/.aws/config file (for region)

(library (jerboa-aws creds)
  (export aws-resolve-credentials parse-ini-file ini-get getenv*)

  (import (chezscheme))

  ;; Get environment variable, returning #f if not set or empty
  (define (getenv* name)
    (let ([val (getenv name)])
      (and val (> (string-length val) 0) val)))

  ;; Parse an INI file into a hashtable of sections.
  ;; Each section is a hashtable of key-value pairs.
  (define (parse-ini-file path)
    (and (file-exists? path)
         (call-with-input-file path
           (lambda (port)
             (let ([sections (make-hashtable string-hash string=?)]
                   [current-section #f]
                   [current-hash #f])
               (let loop ()
                 (let ([line (get-line port)])
                   (unless (eof-object? line)
                     (let ([trimmed (string-trim line)])
                       (cond
                         ;; Empty or comment
                         [(or (= (string-length trimmed) 0)
                              (char=? (string-ref trimmed 0) #\#)
                              (char=? (string-ref trimmed 0) #\;))
                          (void)]
                         ;; Section header [name] or [profile name]
                         [(and (char=? (string-ref trimmed 0) #\[)
                               (char=? (string-ref trimmed (- (string-length trimmed) 1)) #\]))
                          (let* ([name (substring trimmed 1 (- (string-length trimmed) 1))]
                                 [name (if (string-prefix? "profile " name)
                                         (substring name 8 (string-length name))
                                         name)])
                            (set! current-section name)
                            (set! current-hash (make-hashtable string-hash string=?))
                            (hashtable-set! sections name current-hash))]
                         ;; Key = value
                         [current-hash
                          (let ([eq-pos (string-find trimmed #\=)])
                            (when eq-pos
                              (let ([key (string-trim (substring trimmed 0 eq-pos))]
                                    [val (string-trim (substring trimmed (+ eq-pos 1)
                                                        (string-length trimmed)))])
                                (hashtable-set! current-hash key val))))]))
                     (loop))))
               sections)))))

  ;; Get a value from parsed INI sections for a given profile
  (define (ini-get sections profile key)
    (and sections
         (let ([section (hashtable-ref sections profile #f)])
           (and section (hashtable-ref section key #f)))))

  ;; Resolve AWS credentials using the standard credential chain
  (define (aws-resolve-credentials profile)
    (let* ([profile (or profile (getenv* "AWS_PROFILE") "default")]
           [home (or (getenv "HOME") "")]
           [creds-file (string-append home "/.aws/credentials")]
           [config-file (string-append home "/.aws/config")]
           [creds (parse-ini-file creds-file)]
           [config (parse-ini-file config-file)]
           [access-key (or (getenv* "AWS_ACCESS_KEY_ID")
                           (ini-get creds profile "aws_access_key_id"))]
           [secret-key (or (getenv* "AWS_SECRET_ACCESS_KEY")
                           (getenv* "AWS_SECRET_KEY")
                           (ini-get creds profile "aws_secret_access_key"))]
           [region (or (getenv* "AWS_DEFAULT_REGION")
                       (getenv* "AWS_REGION")
                       (ini-get creds profile "region")
                       (ini-get config profile "region")
                       "us-east-1")]
           [token (or (getenv* "AWS_SESSION_TOKEN")
                      (ini-get creds profile "aws_session_token"))])
      (values access-key secret-key region token)))

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

  (define (string-prefix? prefix str)
    (let ([plen (string-length prefix)]
          [slen (string-length str)])
      (and (<= plen slen)
           (string=? prefix (substring str 0 plen)))))

  (define (string-find str ch)
    (let ([len (string-length str)])
      (let loop ([i 0])
        (cond
          [(>= i len) #f]
          [(char=? (string-ref str i) ch) i]
          [else (loop (+ i 1))]))))

  ) ;; end library
