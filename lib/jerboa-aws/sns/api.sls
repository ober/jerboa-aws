#!chezscheme
;;; (jerboa-aws sns api) -- SNS Query API client

(library (jerboa-aws sns api)
  (export SNSClient sns-action sns-action/hash)
  (import (chezscheme)
          (jerboa-aws api))

  (define (SNSClient . args)
    (apply AWSClient
      'service: "sns"
      'api-version: "2010-03-31"
      args))

  (define (sns-action client action . params)
    (apply aws-query-action client action params))

  (define (sns-action/hash client action . params)
    (apply aws-query-action/hash client action params))

  ) ;; end library
