#!chezscheme
;;; (jerboa-aws sqs api) -- SQS JSON API client
;;; SQS uses JSON protocol since 2024

(library (jerboa-aws sqs api)
  (export SQSClient sqs-action)
  (import (chezscheme)
          (jerboa-aws json-api))

  (define (SQSClient . args)
    (apply AWSJsonClient
      'service: "sqs"
      'target-prefix: "AmazonSQS"
      'content-type: "application/x-amz-json-1.0"
      args))

  (define sqs-action
    (case-lambda
      [(client action)
       (aws-json-action client action)]
      [(client action payload)
       (aws-json-action client action payload)]))

  ) ;; end library
