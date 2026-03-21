#!chezscheme
;;; (jerboa-aws ec2 network-interfaces) -- EC2 Network Interface operations

(library (jerboa-aws ec2 network-interfaces)
  (export describe-network-interfaces create-network-interface
          delete-network-interface attach-network-interface
          detach-network-interface)
  (import (chezscheme)
          (jerboa-aws ec2 api)
          (jerboa-aws ec2 params))

  (define (describe-network-interfaces client . args)
    (let ([network-interface-ids (kw-ref args 'network-interface-ids: '())]
          [filters (kw-ref args 'filters: '())])
      (ec2-action/hash client "DescribeNetworkInterfaces"
        (params-merge
          (ec2-param-list "NetworkInterfaceId" network-interface-ids)
          (ec2-param-filters filters)))))

  (define (create-network-interface client subnet-id . args)
    (let ([description (kw-ref args 'description: #f)]
          [security-group-ids (kw-ref args 'security-group-ids: '())]
          [private-ip-address (kw-ref args 'private-ip-address: #f)]
          [tag-specifications (kw-ref args 'tag-specifications: #f)])
      (ec2-action/hash client "CreateNetworkInterface"
        (params-merge
          (list (cons "SubnetId" subnet-id))
          (if description (list (cons "Description" description)) '())
          (ec2-param-list "SecurityGroupId" security-group-ids)
          (if private-ip-address (list (cons "PrivateIpAddress" private-ip-address)) '())
          (if tag-specifications (ec2-param-tags "network-interface" tag-specifications) '())))))

  (define (delete-network-interface client network-interface-id)
    (ec2-action client "DeleteNetworkInterface"
      (list (cons "NetworkInterfaceId" network-interface-id)))
    (void))

  (define (attach-network-interface client network-interface-id instance-id device-index)
    (ec2-action/hash client "AttachNetworkInterface"
      (list (cons "NetworkInterfaceId" network-interface-id)
            (cons "InstanceId" instance-id)
            (cons "DeviceIndex" device-index))))

  (define (detach-network-interface client attachment-id . args)
    (let ([force (kw-ref args 'force: #f)])
      (ec2-action client "DetachNetworkInterface"
        (params-merge
          (list (cons "AttachmentId" attachment-id))
          (if force (list (cons "Force" "true")) '())))
      (void)))

  (define (kw-ref args key default)
    (let loop ([rest args])
      (cond
        [(null? rest) default]
        [(and (pair? (cdr rest)) (eq? (car rest) key))
         (cadr rest)]
        [else (loop (cdr rest))])))

  ) ;; end library
