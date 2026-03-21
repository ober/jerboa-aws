#!chezscheme
;;; (jerboa-aws cost-optimization-hub api) -- Cost Optimization Hub JSON API client
;;; This service is only available in us-east-1.

(library (jerboa-aws cost-optimization-hub api)
  (export CostOptimizationHubClient cost-optimization-hub-action)
  (import (chezscheme)
          (jerboa-aws json-api))

  (define (CostOptimizationHubClient . args)
    (apply AWSJsonClient
      'service: "cost-optimization-hub"
      'target-prefix: "CostOptimizationHubService"
      'content-type: "application/x-amz-json-1.0"
      'region: "us-east-1"
      args))

  (define cost-optimization-hub-action
    (case-lambda
      [(client action)
       (aws-json-action client action)]
      [(client action payload)
       (aws-json-action client action payload)]))

  ) ;; end library
