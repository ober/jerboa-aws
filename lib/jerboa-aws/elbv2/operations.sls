#!chezscheme
;;; (jerboa-aws elbv2 operations) -- ELBv2 operations

(library (jerboa-aws elbv2 operations)
  (export describe-load-balancers create-load-balancer delete-load-balancer
          describe-target-groups create-target-group delete-target-group)
  (import (chezscheme)
          (jerboa-aws elbv2 api))

  ;; Encode a list of strings as member-list parameters
  (define (encode-member-list prefix values)
    (let loop ([vs values] [i 1] [acc '()])
      (if (null? vs)
        (reverse acc)
        (loop (cdr vs) (+ i 1)
              (cons (cons (string-append prefix ".member." (number->string i))
                          (car vs))
                    acc)))))

  (define (describe-load-balancers client . args)
    (let ([load-balancer-arns (kw-ref args 'load-balancer-arns: #f)]
          [names (kw-ref args 'names: #f)]
          [marker (kw-ref args 'marker: #f)]
          [page-size (kw-ref args 'page-size: #f)])
      (elbv2-action/hash client "DescribeLoadBalancers"
        (append
          (if load-balancer-arns
            (encode-member-list "LoadBalancerArns" load-balancer-arns)
            '())
          (if names (encode-member-list "Names" names) '())
          (if marker (list (cons "Marker" marker)) '())
          (if page-size
            (list (cons "PageSize"
                    (if (number? page-size)
                      (number->string page-size)
                      page-size)))
            '())))))

  (define (create-load-balancer client name . args)
    (let ([subnets (kw-ref args 'subnets: '())]
          [security-groups (kw-ref args 'security-groups: '())]
          [scheme (kw-ref args 'scheme: #f)]
          [type (kw-ref args 'type: #f)]
          [ip-address-type (kw-ref args 'ip-address-type: #f)])
      (elbv2-action/hash client "CreateLoadBalancer"
        (append
          (list (cons "Name" name))
          (encode-member-list "Subnets" subnets)
          (encode-member-list "SecurityGroups" security-groups)
          (if scheme (list (cons "Scheme" scheme)) '())
          (if type (list (cons "Type" type)) '())
          (if ip-address-type
            (list (cons "IpAddressType" ip-address-type)) '())))))

  (define (delete-load-balancer client load-balancer-arn)
    (elbv2-action client "DeleteLoadBalancer"
      (list (cons "LoadBalancerArn" load-balancer-arn)))
    (void))

  (define (describe-target-groups client . args)
    (let ([load-balancer-arn (kw-ref args 'load-balancer-arn: #f)]
          [target-group-arns (kw-ref args 'target-group-arns: #f)]
          [names (kw-ref args 'names: #f)]
          [marker (kw-ref args 'marker: #f)]
          [page-size (kw-ref args 'page-size: #f)])
      (elbv2-action/hash client "DescribeTargetGroups"
        (append
          (if load-balancer-arn
            (list (cons "LoadBalancerArn" load-balancer-arn))
            '())
          (if target-group-arns
            (encode-member-list "TargetGroupArns" target-group-arns)
            '())
          (if names (encode-member-list "Names" names) '())
          (if marker (list (cons "Marker" marker)) '())
          (if page-size
            (list (cons "PageSize"
                    (if (number? page-size)
                      (number->string page-size)
                      page-size)))
            '())))))

  (define (create-target-group client name . args)
    (let ([protocol (kw-ref args 'protocol: #f)]
          [port (kw-ref args 'port: #f)]
          [vpc-id (kw-ref args 'vpc-id: #f)]
          [target-type (kw-ref args 'target-type: #f)]
          [health-check-protocol (kw-ref args 'health-check-protocol: #f)]
          [health-check-path (kw-ref args 'health-check-path: #f)])
      (elbv2-action/hash client "CreateTargetGroup"
        (append
          (list (cons "Name" name))
          (if protocol (list (cons "Protocol" protocol)) '())
          (if port
            (list (cons "Port"
                    (if (number? port)
                      (number->string port)
                      port)))
            '())
          (if vpc-id (list (cons "VpcId" vpc-id)) '())
          (if target-type (list (cons "TargetType" target-type)) '())
          (if health-check-protocol
            (list (cons "HealthCheckProtocol" health-check-protocol)) '())
          (if health-check-path
            (list (cons "HealthCheckPath" health-check-path)) '())))))

  (define (delete-target-group client target-group-arn)
    (elbv2-action client "DeleteTargetGroup"
      (list (cons "TargetGroupArn" target-group-arn)))
    (void))

  ;; --- Helpers ---
  (define (kw-ref args key default)
    (let loop ([rest args])
      (cond
        [(null? rest) default]
        [(and (pair? (cdr rest)) (eq? (car rest) key))
         (cadr rest)]
        [else (loop (cdr rest))])))

  ) ;; end library
