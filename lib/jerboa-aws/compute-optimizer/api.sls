#!chezscheme
;;; (jerboa-aws compute-optimizer api) -- Compute Optimizer JSON API client

(library (jerboa-aws compute-optimizer api)
  (export ComputeOptimizerClient compute-optimizer-action)
  (import (chezscheme)
          (jerboa-aws json-api))

  (define (ComputeOptimizerClient . args)
    (apply AWSJsonClient
      'service: "compute-optimizer"
      'target-prefix: "ComputeOptimizerService"
      'content-type: "application/x-amz-json-1.0"
      args))

  (define compute-optimizer-action
    (case-lambda
      [(client action)
       (aws-json-action client action)]
      [(client action payload)
       (aws-json-action client action payload)]))

  ) ;; end library
