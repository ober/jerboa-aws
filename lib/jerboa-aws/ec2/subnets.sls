#!chezscheme
;;; (jerboa-aws ec2 subnets) -- EC2 Subnet operations

(library (jerboa-aws ec2 subnets)
  (export describe-subnets create-subnet delete-subnet modify-subnet-attribute)
  (import (chezscheme)
          (jerboa-aws ec2 api)
          (jerboa-aws ec2 params))

  (define (describe-subnets client . args)
    (let ([subnet-ids (kw-ref args 'subnet-ids: '())]
          [filters (kw-ref args 'filters: '())])
      (ec2-action/hash client "DescribeSubnets"
        (params-merge
          (ec2-param-list "SubnetId" subnet-ids)
          (ec2-param-filters filters)))))

  (define (create-subnet client vpc-id cidr-block . args)
    (let ([availability-zone (kw-ref args 'availability-zone: #f)]
          [tag-specifications (kw-ref args 'tag-specifications: #f)])
      (ec2-action/hash client "CreateSubnet"
        (params-merge
          (list (cons "VpcId" vpc-id)
                (cons "CidrBlock" cidr-block))
          (if availability-zone (list (cons "AvailabilityZone" availability-zone)) '())
          (if tag-specifications (ec2-param-tags "subnet" tag-specifications) '())))))

  (define (delete-subnet client subnet-id)
    (ec2-action client "DeleteSubnet"
      (list (cons "SubnetId" subnet-id)))
    (void))

  (define (modify-subnet-attribute client subnet-id . args)
    (let ([map-public-ip-on-launch (kw-ref args 'map-public-ip-on-launch: #f)])
      (ec2-action client "ModifySubnetAttribute"
        (params-merge
          (list (cons "SubnetId" subnet-id))
          (if (not (eq? map-public-ip-on-launch #f))
            (list (cons "MapPublicIpOnLaunch.Value" (if map-public-ip-on-launch "true" "false")))
            '())))
      (void)))

  (define (kw-ref args key default)
    (let loop ([rest args])
      (cond
        [(null? rest) default]
        [(and (pair? (cdr rest)) (eq? (car rest) key))
         (cadr rest)]
        [else (loop (cdr rest))])))

  ) ;; end library
