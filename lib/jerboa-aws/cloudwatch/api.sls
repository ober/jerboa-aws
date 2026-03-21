#!chezscheme
;;; (jerboa-aws cloudwatch api) -- CloudWatch Monitoring Query API client

(library (jerboa-aws cloudwatch api)
  (export CloudWatchClient cw-action cw-action/hash)
  (import (chezscheme)
          (jerboa-aws api))

  (define (CloudWatchClient . args)
    (apply AWSClient
      'service: "monitoring"
      'api-version: "2010-08-01"
      args))

  (define (cw-action client action . params)
    (apply aws-query-action client action params))

  (define (cw-action/hash client action . params)
    (apply aws-query-action/hash client action params))

  ) ;; end library
