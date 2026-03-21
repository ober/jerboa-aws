#!chezscheme
;;; (jerboa-aws ec2 route-tables) -- EC2 Route Table operations

(library (jerboa-aws ec2 route-tables)
  (export describe-route-tables create-route-table delete-route-table
          create-route delete-route associate-route-table
          disassociate-route-table)
  (import (chezscheme)
          (jerboa-aws ec2 api)
          (jerboa-aws ec2 params))

  (define (describe-route-tables client . args)
    (let ([route-table-ids (kw-ref args 'route-table-ids: '())]
          [filters (kw-ref args 'filters: '())])
      (ec2-action/hash client "DescribeRouteTables"
        (params-merge
          (ec2-param-list "RouteTableId" route-table-ids)
          (ec2-param-filters filters)))))

  (define (create-route-table client vpc-id . args)
    (let ([tag-specifications (kw-ref args 'tag-specifications: #f)])
      (ec2-action/hash client "CreateRouteTable"
        (params-merge
          (list (cons "VpcId" vpc-id))
          (if tag-specifications (ec2-param-tags "route-table" tag-specifications) '())))))

  (define (delete-route-table client route-table-id)
    (ec2-action client "DeleteRouteTable"
      (list (cons "RouteTableId" route-table-id)))
    (void))

  (define (create-route client route-table-id . args)
    (let ([destination-cidr-block (kw-ref args 'destination-cidr-block: #f)]
          [destination-ipv6-cidr-block (kw-ref args 'destination-ipv6-cidr-block: #f)]
          [gateway-id (kw-ref args 'gateway-id: #f)]
          [nat-gateway-id (kw-ref args 'nat-gateway-id: #f)]
          [network-interface-id (kw-ref args 'network-interface-id: #f)]
          [instance-id (kw-ref args 'instance-id: #f)])
      (ec2-action client "CreateRoute"
        (params-merge
          (list (cons "RouteTableId" route-table-id))
          (if destination-cidr-block (list (cons "DestinationCidrBlock" destination-cidr-block)) '())
          (if destination-ipv6-cidr-block (list (cons "DestinationIpv6CidrBlock" destination-ipv6-cidr-block)) '())
          (if gateway-id (list (cons "GatewayId" gateway-id)) '())
          (if nat-gateway-id (list (cons "NatGatewayId" nat-gateway-id)) '())
          (if network-interface-id (list (cons "NetworkInterfaceId" network-interface-id)) '())
          (if instance-id (list (cons "InstanceId" instance-id)) '())))
      (void)))

  (define (delete-route client route-table-id . args)
    (let ([destination-cidr-block (kw-ref args 'destination-cidr-block: #f)]
          [destination-ipv6-cidr-block (kw-ref args 'destination-ipv6-cidr-block: #f)])
      (ec2-action client "DeleteRoute"
        (params-merge
          (list (cons "RouteTableId" route-table-id))
          (if destination-cidr-block (list (cons "DestinationCidrBlock" destination-cidr-block)) '())
          (if destination-ipv6-cidr-block (list (cons "DestinationIpv6CidrBlock" destination-ipv6-cidr-block)) '())))
      (void)))

  (define (associate-route-table client route-table-id subnet-id)
    (ec2-action/hash client "AssociateRouteTable"
      (list (cons "RouteTableId" route-table-id)
            (cons "SubnetId" subnet-id))))

  (define (disassociate-route-table client association-id)
    (ec2-action client "DisassociateRouteTable"
      (list (cons "AssociationId" association-id)))
    (void))

  (define (kw-ref args key default)
    (let loop ([rest args])
      (cond
        [(null? rest) default]
        [(and (pair? (cdr rest)) (eq? (car rest) key))
         (cadr rest)]
        [else (loop (cdr rest))])))

  ) ;; end library
