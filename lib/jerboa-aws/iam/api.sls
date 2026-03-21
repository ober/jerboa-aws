#!chezscheme
;;; (jerboa-aws iam api) -- IAM Query API client
;;; IAM uses a global endpoint: iam.amazonaws.com

(library (jerboa-aws iam api)
  (export IAMClient iam-action iam-action/hash)
  (import (chezscheme)
          (jerboa-aws api))

  (define (IAMClient . args)
    (apply AWSClient
      'service: "iam"
      'endpoint: "iam.amazonaws.com"
      'api-version: "2010-05-08"
      args))

  (define (iam-action client action . params)
    (apply aws-query-action client action params))

  (define (iam-action/hash client action . params)
    (apply aws-query-action/hash client action params))

  ) ;; end library
