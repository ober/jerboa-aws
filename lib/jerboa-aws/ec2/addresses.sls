#!chezscheme
;;; (jerboa-aws ec2 addresses) -- EC2 Elastic IP Address operations

(library (jerboa-aws ec2 addresses)
  (export describe-addresses allocate-address release-address
          associate-address disassociate-address)
  (import (chezscheme)
          (jerboa-aws ec2 api)
          (jerboa-aws ec2 params))

  (define (describe-addresses client . args)
    (let ([allocation-ids (kw-ref args 'allocation-ids: '())]
          [public-ips (kw-ref args 'public-ips: '())]
          [filters (kw-ref args 'filters: '())])
      (ec2-action/hash client "DescribeAddresses"
        (params-merge
          (ec2-param-list "AllocationId" allocation-ids)
          (ec2-param-list "PublicIp" public-ips)
          (ec2-param-filters filters)))))

  (define (allocate-address client . args)
    (let ([domain (kw-ref args 'domain: "vpc")]
          [tag-specifications (kw-ref args 'tag-specifications: #f)])
      (ec2-action/hash client "AllocateAddress"
        (params-merge
          (list (cons "Domain" domain))
          (if tag-specifications (ec2-param-tags "elastic-ip" tag-specifications) '())))))

  (define (release-address client . args)
    (let ([allocation-id (kw-ref args 'allocation-id: #f)]
          [public-ip (kw-ref args 'public-ip: #f)])
      (ec2-action client "ReleaseAddress"
        (params-merge
          (if allocation-id (list (cons "AllocationId" allocation-id)) '())
          (if public-ip (list (cons "PublicIp" public-ip)) '())))
      (void)))

  (define (associate-address client . args)
    (let ([instance-id (kw-ref args 'instance-id: #f)]
          [allocation-id (kw-ref args 'allocation-id: #f)]
          [public-ip (kw-ref args 'public-ip: #f)]
          [network-interface-id (kw-ref args 'network-interface-id: #f)]
          [allow-reassociation (kw-ref args 'allow-reassociation: #f)])
      (ec2-action/hash client "AssociateAddress"
        (params-merge
          (if instance-id (list (cons "InstanceId" instance-id)) '())
          (if allocation-id (list (cons "AllocationId" allocation-id)) '())
          (if public-ip (list (cons "PublicIp" public-ip)) '())
          (if network-interface-id (list (cons "NetworkInterfaceId" network-interface-id)) '())
          (if allow-reassociation (list (cons "AllowReassociation" "true")) '())))))

  (define (disassociate-address client . args)
    (let ([association-id (kw-ref args 'association-id: #f)]
          [public-ip (kw-ref args 'public-ip: #f)])
      (ec2-action client "DisassociateAddress"
        (params-merge
          (if association-id (list (cons "AssociationId" association-id)) '())
          (if public-ip (list (cons "PublicIp" public-ip)) '())))
      (void)))

  (define (kw-ref args key default)
    (let loop ([rest args])
      (cond
        [(null? rest) default]
        [(and (pair? (cdr rest)) (eq? (car rest) key))
         (cadr rest)]
        [else (loop (cdr rest))])))

  ) ;; end library
