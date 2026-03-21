#!chezscheme
;;; (jerboa-aws ec2 instances) -- EC2 instance operations

(library (jerboa-aws ec2 instances)
  (export describe-instances run-instances start-instances
          stop-instances terminate-instances reboot-instances
          describe-instance-status get-console-output
          modify-instance-attribute describe-instance-types)
  (import (chezscheme)
          (jerboa-aws ec2 api)
          (jerboa-aws ec2 params))

  (define (describe-instances client . args)
    (let ([instance-ids (kw-ref args 'instance-ids: '())]
          [filters (kw-ref args 'filters: '())])
      (ec2-action/hash client "DescribeInstances"
        (params-merge
          (ec2-param-list "InstanceId" instance-ids)
          (ec2-param-filters filters)))))

  (define (run-instances client image-id instance-type . args)
    (let ([min-count (kw-ref args 'min-count: "1")]
          [max-count (kw-ref args 'max-count: "1")]
          [key-name (kw-ref args 'key-name: #f)]
          [security-group-ids (kw-ref args 'security-group-ids: '())]
          [subnet-id (kw-ref args 'subnet-id: #f)]
          [tags (kw-ref args 'tags: '())]
          [user-data (kw-ref args 'user-data: #f)])
      (ec2-action/hash client "RunInstances"
        (params-merge
          (list (cons "ImageId" image-id)
                (cons "InstanceType" instance-type)
                (cons "MinCount" min-count)
                (cons "MaxCount" max-count))
          (if key-name (list (cons "KeyName" key-name)) '())
          (ec2-param-list "SecurityGroupId" security-group-ids)
          (if subnet-id (list (cons "SubnetId" subnet-id)) '())
          (if (pair? tags) (ec2-param-tags "instance" tags) '())
          (if user-data (list (cons "UserData" user-data)) '())))))

  (define (start-instances client instance-ids)
    (ec2-action/hash client "StartInstances"
      (ec2-param-list "InstanceId" instance-ids)))

  (define (stop-instances client instance-ids . args)
    (let ([force (kw-ref args 'force: #f)])
      (ec2-action/hash client "StopInstances"
        (params-merge
          (ec2-param-list "InstanceId" instance-ids)
          (if force (list (cons "Force" "true")) '())))))

  (define (terminate-instances client instance-ids)
    (ec2-action/hash client "TerminateInstances"
      (ec2-param-list "InstanceId" instance-ids)))

  (define (reboot-instances client instance-ids)
    (ec2-action/hash client "RebootInstances"
      (ec2-param-list "InstanceId" instance-ids)))

  (define (describe-instance-status client . args)
    (let ([instance-ids (kw-ref args 'instance-ids: '())])
      (ec2-action/hash client "DescribeInstanceStatus"
        (params-merge
          (ec2-param-list "InstanceId" instance-ids)
          (list (cons "IncludeAllInstances" "true"))))))

  (define (get-console-output client instance-id)
    (ec2-action/hash client "GetConsoleOutput"
      (list (cons "InstanceId" instance-id))))

  (define (modify-instance-attribute client instance-id attribute value)
    (ec2-action/hash client "ModifyInstanceAttribute"
      (list (cons "InstanceId" instance-id)
            (cons (string-append attribute ".Value") value))))

  (define (describe-instance-types client . args)
    (let ([instance-types (kw-ref args 'instance-types: '())])
      (ec2-action/hash client "DescribeInstanceTypes"
        (ec2-param-list "InstanceType" instance-types))))

  (define (kw-ref args key default)
    (let loop ([rest args])
      (cond
        [(null? rest) default]
        [(and (pair? (cdr rest)) (eq? (car rest) key))
         (cadr rest)]
        [else (loop (cdr rest))])))

  ) ;; end library
