#!chezscheme
;;; (jerboa-aws crypto) -- SHA256 and HMAC-SHA256 for AWS SigV4
;;; Uses openssl CLI for SHA256, pure Scheme HMAC.

(library (jerboa-aws crypto)
  (export sha256 hex-encode hmac-sha256 bytevector-append)
  (import (chezscheme))

  ;; SHA256 hash, returns bytevector
  (define (sha256 data)
    (let* ([bv (if (string? data) (string->utf8 data) data)]
           [cmd "openssl dgst -sha256 -binary"])
      (let-values ([(to-stdin from-stdout from-stderr pid)
                    (open-process-ports cmd 'block)])
        (put-bytevector to-stdin bv)
        (close-port to-stdin)
        (let ([result (get-bytevector-all from-stdout)])
          (close-port from-stdout)
          (close-port from-stderr)
          (if (eof-object? result)
            (make-bytevector 32 0)
            result)))))

  ;; Hex-encode a bytevector
  (define (hex-encode bv)
    (let* ([len (bytevector-length bv)]
           [hex (make-string (* len 2))])
      (do ([i 0 (+ i 1)])
          ((= i len) hex)
        (let* ([b (bytevector-u8-ref bv i)]
               [hi (fxsra b 4)]
               [lo (fxand b #xf)])
          (string-set! hex (* i 2) (string-ref "0123456789abcdef" hi))
          (string-set! hex (+ (* i 2) 1) (string-ref "0123456789abcdef" lo))))))

  ;; HMAC-SHA256
  (define (hmac-sha256 key data)
    (let* ([key-bv (if (string? key) (string->utf8 key) key)]
           [data-bv (if (string? data) (string->utf8 data) data)]
           [block-size 64]
           [key-bv (if (> (bytevector-length key-bv) block-size)
                     (sha256 key-bv)
                     key-bv)]
           [key-padded (make-bytevector block-size 0)])
      (bytevector-copy! key-bv 0 key-padded 0 (bytevector-length key-bv))
      (let ([o-key-pad (make-bytevector block-size)]
            [i-key-pad (make-bytevector block-size)])
        (do ([i 0 (+ i 1)])
            ((= i block-size))
          (bytevector-u8-set! o-key-pad i
            (fxlogxor (bytevector-u8-ref key-padded i) #x5c))
          (bytevector-u8-set! i-key-pad i
            (fxlogxor (bytevector-u8-ref key-padded i) #x36)))
        (sha256 (bytevector-append o-key-pad
                  (sha256 (bytevector-append i-key-pad data-bv)))))))

  ;; Append bytevectors
  (define (bytevector-append . bvs)
    (let* ([total (apply + (map bytevector-length bvs))]
           [result (make-bytevector total)])
      (let loop ([bvs bvs] [offset 0])
        (if (null? bvs) result
          (let ([bv (car bvs)])
            (bytevector-copy! bv 0 result offset (bytevector-length bv))
            (loop (cdr bvs) (+ offset (bytevector-length bv))))))))

  ) ;; end library
