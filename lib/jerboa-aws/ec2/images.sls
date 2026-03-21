#!chezscheme
;;; (jerboa-aws ec2 images) -- EC2 AMI operations

(library (jerboa-aws ec2 images)
  (export describe-images create-image deregister-image copy-image)
  (import (chezscheme)
          (jerboa-aws ec2 api)
          (jerboa-aws ec2 params))

  (define (describe-images client . args)
    (let ([image-ids (kw-ref args 'image-ids: '())]
          [owners (kw-ref args 'owners: '())]
          [filters (kw-ref args 'filters: '())])
      (ec2-action/hash client "DescribeImages"
        (params-merge
          (ec2-param-list "ImageId" image-ids)
          (ec2-param-list "Owner" owners)
          (ec2-param-filters filters)))))

  (define (create-image client instance-id name . args)
    (let ([description (kw-ref args 'description: #f)]
          [no-reboot (kw-ref args 'no-reboot: #f)]
          [tag-specifications (kw-ref args 'tag-specifications: #f)])
      (ec2-action/hash client "CreateImage"
        (params-merge
          (list (cons "InstanceId" instance-id)
                (cons "Name" name))
          (if description (list (cons "Description" description)) '())
          (if no-reboot (list (cons "NoReboot" "true")) '())
          (if tag-specifications (ec2-param-tags "image" tag-specifications) '())))))

  (define (deregister-image client image-id)
    (ec2-action client "DeregisterImage"
      (list (cons "ImageId" image-id)))
    (void))

  (define (copy-image client source-image-id source-region name . args)
    (let ([description (kw-ref args 'description: #f)]
          [encrypted (kw-ref args 'encrypted: #f)])
      (ec2-action/hash client "CopyImage"
        (params-merge
          (list (cons "SourceImageId" source-image-id)
                (cons "SourceRegion" source-region)
                (cons "Name" name))
          (if description (list (cons "Description" description)) '())
          (if encrypted (list (cons "Encrypted" "true")) '())))))

  (define (kw-ref args key default)
    (let loop ([rest args])
      (cond
        [(null? rest) default]
        [(and (pair? (cdr rest)) (eq? (car rest) key))
         (cadr rest)]
        [else (loop (cdr rest))])))

  ) ;; end library
