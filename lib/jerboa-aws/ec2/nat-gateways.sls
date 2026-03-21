#!chezscheme
;;; (jerboa-aws ec2 nat-gateways) -- EC2 NAT Gateway operations

(library (jerboa-aws ec2 nat-gateways)
  (export describe-nat-gateways create-nat-gateway delete-nat-gateway)
  (import (chezscheme)
          (jerboa-aws ec2 api)
          (jerboa-aws ec2 params))

  (define (describe-nat-gateways client . args)
    (let ([nat-gateway-ids (kw-ref args 'nat-gateway-ids: '())]
          [filters (kw-ref args 'filters: '())])
      (ec2-action/hash client "DescribeNatGateways"
        (params-merge
          (ec2-param-list "NatGatewayId" nat-gateway-ids)
          (ec2-param-filters filters)))))

  (define (create-nat-gateway client subnet-id allocation-id . args)
    (let ([connectivity-type (kw-ref args 'connectivity-type: #f)]
          [tag-specifications (kw-ref args 'tag-specifications: #f)])
      (ec2-action/hash client "CreateNatGateway"
        (params-merge
          (list (cons "SubnetId" subnet-id)
                (cons "AllocationId" allocation-id))
          (if connectivity-type (list (cons "ConnectivityType" connectivity-type)) '())
          (if tag-specifications (ec2-param-tags "natgateway" tag-specifications) '())))))

  (define (delete-nat-gateway client nat-gateway-id)
    (ec2-action/hash client "DeleteNatGateway"
      (list (cons "NatGatewayId" nat-gateway-id))))

  (define (kw-ref args key default)
    (let loop ([rest args])
      (cond
        [(null? rest) default]
        [(and (pair? (cdr rest)) (eq? (car rest) key))
         (cadr rest)]
        [else (loop (cdr rest))])))

  ) ;; end library
