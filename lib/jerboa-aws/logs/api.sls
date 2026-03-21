#!chezscheme
;;; (jerboa-aws logs api) -- CloudWatch Logs JSON API client

(library (jerboa-aws logs api)
  (export LogsClient logs-action)
  (import (chezscheme)
          (jerboa-aws json-api))

  (define (LogsClient . args)
    (apply AWSJsonClient
      'service: "logs"
      'target-prefix: "Logs_20140328"
      'content-type: "application/x-amz-json-1.1"
      args))

  (define logs-action
    (case-lambda
      [(client action)
       (aws-json-action client action)]
      [(client action payload)
       (aws-json-action client action payload)]))

  ) ;; end library
