#!chezscheme
;;; (jerboa-aws elbv2 api) -- ELBv2 Query API client

(library (jerboa-aws elbv2 api)
  (export ELBv2Client elbv2-action elbv2-action/hash)
  (import (chezscheme)
          (jerboa-aws api))

  (define (ELBv2Client . args)
    (apply AWSClient
      'service: "elasticloadbalancing"
      'api-version: "2015-12-01"
      args))

  (define (elbv2-action client action . params)
    (apply aws-query-action client action params))

  (define (elbv2-action/hash client action . params)
    (apply aws-query-action/hash client action params))

  ) ;; end library
