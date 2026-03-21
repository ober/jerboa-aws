#!chezscheme
;;; (jerboa-aws cfn api) -- CloudFormation Query API client

(library (jerboa-aws cfn api)
  (export CFNClient cfn-action cfn-action/hash)
  (import (chezscheme)
          (jerboa-aws api))

  (define (CFNClient . args)
    (apply AWSClient
      'service: "cloudformation"
      'api-version: "2010-05-15"
      args))

  (define (cfn-action client action . params)
    (apply aws-query-action client action params))

  (define (cfn-action/hash client action . params)
    (apply aws-query-action/hash client action params))

  ) ;; end library
