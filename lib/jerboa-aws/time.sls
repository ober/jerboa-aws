#!chezscheme
;;; (jerboa-aws time) -- Timestamp utilities for AWS SigV4

(library (jerboa-aws time)
  (export aws-timestamp aws-datestamp)
  (import (chezscheme))

  ;; Returns ISO 8601 UTC timestamp: "YYYYMMDDTHHMMSSZ"
  (define (aws-timestamp)
    (let ([t (current-date 0)])  ;; UTC offset = 0
      (format "~4,'0d~2,'0d~2,'0dT~2,'0d~2,'0d~2,'0dZ"
        (date-year t) (date-month t) (date-day t)
        (date-hour t) (date-minute t) (date-second t))))

  ;; Returns date portion: "YYYYMMDD"
  (define (aws-datestamp)
    (let ([t (current-date 0)])
      (format "~4,'0d~2,'0d~2,'0d"
        (date-year t) (date-month t) (date-day t))))

  ) ;; end library
