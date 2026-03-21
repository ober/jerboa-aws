#!chezscheme
;;; (jerboa-aws ec2 security-groups) -- EC2 Security Group operations

(library (jerboa-aws ec2 security-groups)
  (export describe-security-groups create-security-group delete-security-group
          authorize-security-group-ingress authorize-security-group-egress
          revoke-security-group-ingress revoke-security-group-egress)
  (import (chezscheme)
          (jerboa-aws ec2 api)
          (jerboa-aws ec2 params))

  (define (describe-security-groups client . args)
    (let ([group-ids (kw-ref args 'group-ids: '())]
          [group-names (kw-ref args 'group-names: '())]
          [filters (kw-ref args 'filters: '())])
      (ec2-action/hash client "DescribeSecurityGroups"
        (params-merge
          (ec2-param-list "GroupId" group-ids)
          (ec2-param-list "GroupName" group-names)
          (ec2-param-filters filters)))))

  (define (create-security-group client group-name description . args)
    (let ([vpc-id (kw-ref args 'vpc-id: #f)]
          [tag-specifications (kw-ref args 'tag-specifications: #f)])
      (ec2-action/hash client "CreateSecurityGroup"
        (params-merge
          (list (cons "GroupName" group-name)
                (cons "GroupDescription" description))
          (if vpc-id (list (cons "VpcId" vpc-id)) '())
          (if tag-specifications (ec2-param-tags "security-group" tag-specifications) '())))))

  (define (delete-security-group client . args)
    (let ([group-id (kw-ref args 'group-id: #f)]
          [group-name (kw-ref args 'group-name: #f)])
      (ec2-action client "DeleteSecurityGroup"
        (params-merge
          (if group-id (list (cons "GroupId" group-id)) '())
          (if group-name (list (cons "GroupName" group-name)) '())))
      (void)))

  (define (authorize-security-group-ingress client group-id ip-permissions)
    (ec2-action client "AuthorizeSecurityGroupIngress"
      (params-merge
        (list (cons "GroupId" group-id))
        (ec2-param-ip-permissions "IpPermissions" ip-permissions)))
    (void))

  (define (authorize-security-group-egress client group-id ip-permissions)
    (ec2-action client "AuthorizeSecurityGroupEgress"
      (params-merge
        (list (cons "GroupId" group-id))
        (ec2-param-ip-permissions "IpPermissions" ip-permissions)))
    (void))

  (define (revoke-security-group-ingress client group-id ip-permissions)
    (ec2-action client "RevokeSecurityGroupIngress"
      (params-merge
        (list (cons "GroupId" group-id))
        (ec2-param-ip-permissions "IpPermissions" ip-permissions)))
    (void))

  (define (revoke-security-group-egress client group-id ip-permissions)
    (ec2-action client "RevokeSecurityGroupEgress"
      (params-merge
        (list (cons "GroupId" group-id))
        (ec2-param-ip-permissions "IpPermissions" ip-permissions)))
    (void))

  (define (kw-ref args key default)
    (let loop ([rest args])
      (cond
        [(null? rest) default]
        [(and (pair? (cdr rest)) (eq? (car rest) key))
         (cadr rest)]
        [else (loop (cdr rest))])))

  ) ;; end library
