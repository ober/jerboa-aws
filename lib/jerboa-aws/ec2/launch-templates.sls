#!chezscheme
;;; (jerboa-aws ec2 launch-templates) -- EC2 Launch Template operations

(library (jerboa-aws ec2 launch-templates)
  (export describe-launch-templates create-launch-template delete-launch-template)
  (import (chezscheme)
          (jerboa-aws ec2 api)
          (jerboa-aws ec2 params))

  (define (describe-launch-templates client . args)
    (let ([launch-template-ids (kw-ref args 'launch-template-ids: '())]
          [launch-template-names (kw-ref args 'launch-template-names: '())]
          [filters (kw-ref args 'filters: '())]
          [max-results (kw-ref args 'max-results: #f)]
          [next-token (kw-ref args 'next-token: #f)])
      (ec2-action/hash client "DescribeLaunchTemplates"
        (params-merge
          (ec2-param-list "LaunchTemplateId" launch-template-ids)
          (ec2-param-list "LaunchTemplateName" launch-template-names)
          (ec2-param-filters filters)
          (if max-results (list (cons "MaxResults" max-results)) '())
          (if next-token (list (cons "NextToken" next-token)) '())))))

  (define (create-launch-template client launch-template-name . args)
    (let ([image-id (kw-ref args 'image-id: #f)]
          [instance-type (kw-ref args 'instance-type: #f)]
          [key-name (kw-ref args 'key-name: #f)]
          [security-group-ids (kw-ref args 'security-group-ids: '())]
          [user-data (kw-ref args 'user-data: #f)]
          [tag-specifications (kw-ref args 'tag-specifications: #f)])
      (ec2-action/hash client "CreateLaunchTemplate"
        (params-merge
          (list (cons "LaunchTemplateName" launch-template-name))
          (if image-id (list (cons "LaunchTemplateData.ImageId" image-id)) '())
          (if instance-type (list (cons "LaunchTemplateData.InstanceType" instance-type)) '())
          (if key-name (list (cons "LaunchTemplateData.KeyName" key-name)) '())
          (let loop ([sgs security-group-ids] [i 1] [acc '()])
            (if (null? sgs)
              (reverse acc)
              (loop (cdr sgs) (+ i 1)
                    (cons (cons (string-append "LaunchTemplateData.SecurityGroupId." (number->string i))
                                (car sgs))
                          acc))))
          (if user-data (list (cons "LaunchTemplateData.UserData" user-data)) '())
          (if tag-specifications (ec2-param-tags "launch-template" tag-specifications) '())))))

  (define (delete-launch-template client . args)
    (let ([launch-template-id (kw-ref args 'launch-template-id: #f)]
          [launch-template-name (kw-ref args 'launch-template-name: #f)])
      (ec2-action client "DeleteLaunchTemplate"
        (params-merge
          (if launch-template-id (list (cons "LaunchTemplateId" launch-template-id)) '())
          (if launch-template-name (list (cons "LaunchTemplateName" launch-template-name)) '())))
      (void)))

  (define (kw-ref args key default)
    (let loop ([rest args])
      (cond
        [(null? rest) default]
        [(and (pair? (cdr rest)) (eq? (car rest) key))
         (cadr rest)]
        [else (loop (cdr rest))])))

  ) ;; end library
