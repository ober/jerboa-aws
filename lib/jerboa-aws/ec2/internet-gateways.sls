#!chezscheme
;;; (jerboa-aws ec2 internet-gateways) -- EC2 Internet Gateway operations

(library (jerboa-aws ec2 internet-gateways)
  (export describe-internet-gateways create-internet-gateway
          delete-internet-gateway attach-internet-gateway
          detach-internet-gateway)
  (import (chezscheme)
          (jerboa-aws ec2 api)
          (jerboa-aws ec2 params))

  (define (describe-internet-gateways client . args)
    (let ([internet-gateway-ids (kw-ref args 'internet-gateway-ids: '())]
          [filters (kw-ref args 'filters: '())])
      (ec2-action/hash client "DescribeInternetGateways"
        (params-merge
          (ec2-param-list "InternetGatewayId" internet-gateway-ids)
          (ec2-param-filters filters)))))

  (define (create-internet-gateway client . args)
    (let ([tag-specifications (kw-ref args 'tag-specifications: #f)])
      (ec2-action/hash client "CreateInternetGateway"
        (if tag-specifications (ec2-param-tags "internet-gateway" tag-specifications) '()))))

  (define (delete-internet-gateway client internet-gateway-id)
    (ec2-action client "DeleteInternetGateway"
      (list (cons "InternetGatewayId" internet-gateway-id)))
    (void))

  (define (attach-internet-gateway client internet-gateway-id vpc-id)
    (ec2-action client "AttachInternetGateway"
      (list (cons "InternetGatewayId" internet-gateway-id)
            (cons "VpcId" vpc-id)))
    (void))

  (define (detach-internet-gateway client internet-gateway-id vpc-id)
    (ec2-action client "DetachInternetGateway"
      (list (cons "InternetGatewayId" internet-gateway-id)
            (cons "VpcId" vpc-id)))
    (void))

  (define (kw-ref args key default)
    (let loop ([rest args])
      (cond
        [(null? rest) default]
        [(and (pair? (cdr rest)) (eq? (car rest) key))
         (cadr rest)]
        [else (loop (cdr rest))])))

  ) ;; end library
