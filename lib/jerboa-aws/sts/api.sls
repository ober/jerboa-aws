#!chezscheme
(library (jerboa-aws sts api)
  (export STSClient sts-action sts-action/hash)
  (import (chezscheme)
          (jerboa-aws api))

  (define (STSClient . args)
    (apply AWSClient
      'service: "sts"
      'endpoint: "sts.amazonaws.com"
      'api-version: "2011-06-15"
      args))

  (define (sts-action client action . params)
    (apply aws-query-action client action params))

  (define (sts-action/hash client action . params)
    (apply aws-query-action/hash client action params))

  ) ;; end library
