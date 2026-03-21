#!chezscheme
;;; (jerboa-aws ec2 regions) -- EC2 Region and Availability Zone operations

(library (jerboa-aws ec2 regions)
  (export describe-regions describe-availability-zones)
  (import (chezscheme)
          (jerboa-aws ec2 api)
          (jerboa-aws ec2 params))

  (define (describe-regions client . args)
    (let ([region-names (kw-ref args 'region-names: '())]
          [filters (kw-ref args 'filters: '())]
          [all-regions (kw-ref args 'all-regions: #f)])
      (ec2-action/hash client "DescribeRegions"
        (params-merge
          (ec2-param-list "RegionName" region-names)
          (ec2-param-filters filters)
          (if all-regions (list (cons "AllRegions" "true")) '())))))

  (define (describe-availability-zones client . args)
    (let ([zone-names (kw-ref args 'zone-names: '())]
          [zone-ids (kw-ref args 'zone-ids: '())]
          [filters (kw-ref args 'filters: '())]
          [all-zones (kw-ref args 'all-zones: #f)])
      (ec2-action/hash client "DescribeAvailabilityZones"
        (params-merge
          (ec2-param-list "ZoneName" zone-names)
          (ec2-param-list "ZoneId" zone-ids)
          (ec2-param-filters filters)
          (if all-zones (list (cons "AllAvailabilityZones" "true")) '())))))

  (define (kw-ref args key default)
    (let loop ([rest args])
      (cond
        [(null? rest) default]
        [(and (pair? (cdr rest)) (eq? (car rest) key))
         (cadr rest)]
        [else (loop (cdr rest))])))

  ) ;; end library
