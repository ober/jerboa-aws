#!chezscheme
;;; (jerboa-aws rds api) -- RDS Query API client

(library (jerboa-aws rds api)
  (export RDSClient rds-action rds-action/hash)
  (import (chezscheme)
          (jerboa-aws api))

  (define (RDSClient . args)
    (apply AWSClient
      'service: "rds"
      'api-version: "2014-10-31"
      args))

  (define (rds-action client action . params)
    (apply aws-query-action client action params))

  (define (rds-action/hash client action . params)
    (apply aws-query-action/hash client action params))

  ) ;; end library
