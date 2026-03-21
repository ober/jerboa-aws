#!chezscheme
;;; (jerboa-aws uri) -- URI encoding utilities

(library (jerboa-aws uri)
  (export uri-encode form-url-encode)
  (import (chezscheme))

  (define (uri-encode str)
    (let ([out (open-output-string)])
      (do ([i 0 (+ i 1)])
          ((= i (string-length str)) (get-output-string out))
        (let ([c (string-ref str i)])
          (cond
            [(or (char-alphabetic? c) (char-numeric? c)
                 (memv c '(#\- #\_ #\. #\~)))
             (write-char c out)]
            [else
             (let ([bv (string->utf8 (string c))])
               (do ([j 0 (+ j 1)])
                   ((= j (bytevector-length bv)))
                 (let ([b (bytevector-u8-ref bv j)])
                   (put-string out "%")
                   (put-string out (string-upcase
                     (let ([s (number->string b 16)])
                       (if (< b 16) (string-append "0" s) s)))))))])))))

  ;; Encode an alist as application/x-www-form-urlencoded
  (define (form-url-encode params)
    (let ([parts (map (lambda (p)
                        (string-append
                          (uri-encode (if (symbol? (car p))
                                       (symbol->string (car p))
                                       (car p)))
                          "="
                          (uri-encode (if (string? (cdr p))
                                       (cdr p)
                                       (format "~a" (cdr p))))))
                      params)])
      (string-join parts "&")))

  (define (string-join strs sep)
    (if (null? strs) ""
      (let loop ([rest (cdr strs)] [out (car strs)])
        (if (null? rest) out
          (loop (cdr rest) (string-append out sep (car rest)))))))

  ) ;; end library
