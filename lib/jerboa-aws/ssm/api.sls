#!chezscheme
;;; (jerboa-aws ssm api) -- AWS Systems Manager JSON API client

(library (jerboa-aws ssm api)
  (export SSMClient ssm-action)
  (import (chezscheme)
          (jerboa-aws json-api))

  (define (SSMClient . args)
    (apply AWSJsonClient
      'service: "ssm"
      'target-prefix: "AmazonSSM"
      'content-type: "application/x-amz-json-1.1"
      args))

  (define ssm-action
    (case-lambda
      [(client action)
       (aws-json-action client action)]
      [(client action payload)
       (aws-json-action client action payload)]))

  ) ;; end library
