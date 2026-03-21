#!chezscheme
;;; (jerboa-aws ec2 vpcs) -- EC2 VPC operations

(library (jerboa-aws ec2 vpcs)
  (export describe-vpcs create-vpc delete-vpc modify-vpc-attribute)
  (import (chezscheme)
          (jerboa-aws ec2 api)
          (jerboa-aws ec2 params))

  (define (describe-vpcs client . args)
    (let ([vpc-ids (kw-ref args 'vpc-ids: '())]
          [filters (kw-ref args 'filters: '())])
      (ec2-action/hash client "DescribeVpcs"
        (params-merge
          (ec2-param-list "VpcId" vpc-ids)
          (ec2-param-filters filters)))))

  (define (create-vpc client cidr-block . args)
    (let ([amazon-provided-ipv6 (kw-ref args 'amazon-provided-ipv6: #f)]
          [instance-tenancy (kw-ref args 'instance-tenancy: #f)]
          [tag-specifications (kw-ref args 'tag-specifications: #f)])
      (ec2-action/hash client "CreateVpc"
        (params-merge
          (list (cons "CidrBlock" cidr-block))
          (if amazon-provided-ipv6 (list (cons "AmazonProvidedIpv6CidrBlock" "true")) '())
          (if instance-tenancy (list (cons "InstanceTenancy" instance-tenancy)) '())
          (if tag-specifications (ec2-param-tags "vpc" tag-specifications) '())))))

  (define (delete-vpc client vpc-id)
    (ec2-action client "DeleteVpc"
      (list (cons "VpcId" vpc-id)))
    (void))

  (define (modify-vpc-attribute client vpc-id . args)
    (let ([enable-dns-support (kw-ref args 'enable-dns-support: #f)]
          [enable-dns-hostnames (kw-ref args 'enable-dns-hostnames: #f)])
      (ec2-action client "ModifyVpcAttribute"
        (params-merge
          (list (cons "VpcId" vpc-id))
          (if (not (eq? enable-dns-support #f))
            (list (cons "EnableDnsSupport.Value" (if enable-dns-support "true" "false")))
            '())
          (if (not (eq? enable-dns-hostnames #f))
            (list (cons "EnableDnsHostnames.Value" (if enable-dns-hostnames "true" "false")))
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
