#!chezscheme
;;; (jerboa-aws ec2 xml) -- EC2-specific XML parsing

(library (jerboa-aws ec2 xml)
  (export ec2-parse-xml ec2-response->hash)
  (import (chezscheme)
          (jerboa-aws xml))

  (define (ec2-parse-xml xml-str)
    (xml-parse xml-str))

  (define (ec2-response->hash xml-str)
    (aws-response->hash xml-str))

  ) ;; end library
