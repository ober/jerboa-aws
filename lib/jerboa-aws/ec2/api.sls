#!chezscheme
;;; (jerboa-aws ec2 api) -- EC2 client

(library (jerboa-aws ec2 api)
  (export EC2Client ec2-action ec2-action/hash)
  (import (chezscheme)
          (jerboa-aws api)
          (jerboa-aws ec2 xml))

  (define (EC2Client . args)
    (apply AWSClient
      'service: "ec2"
      'api-version: "2016-11-15"
      args))

  (define (ec2-action client action . params)
    (apply aws-query-action client action params))

  (define (ec2-action/hash client action . params)
    (let ([xml-text (apply aws-query-action client action params)])
      (ec2-response->hash xml-text)))

  ) ;; end library
