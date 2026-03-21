#!chezscheme
;;; (jerboa-aws dynamodb api) -- DynamoDB JSON API client

(library (jerboa-aws dynamodb api)
  (export DynamoDBClient dynamodb-action)
  (import (chezscheme)
          (jerboa-aws json-api))

  (define (DynamoDBClient . args)
    (apply AWSJsonClient
      'service: "dynamodb"
      'target-prefix: "DynamoDB_20120810"
      'content-type: "application/x-amz-json-1.0"
      args))

  (define dynamodb-action
    (case-lambda
      [(client action)
       (aws-json-action client action)]
      [(client action payload)
       (aws-json-action client action payload)]))

  ) ;; end library
