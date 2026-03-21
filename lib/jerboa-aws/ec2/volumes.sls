#!chezscheme
;;; (jerboa-aws ec2 volumes) -- EC2 EBS Volume operations

(library (jerboa-aws ec2 volumes)
  (export describe-volumes create-volume delete-volume attach-volume detach-volume)
  (import (chezscheme)
          (jerboa-aws ec2 api)
          (jerboa-aws ec2 params))

  (define (describe-volumes client . args)
    (let ([volume-ids (kw-ref args 'volume-ids: '())]
          [filters (kw-ref args 'filters: '())])
      (ec2-action/hash client "DescribeVolumes"
        (params-merge
          (ec2-param-list "VolumeId" volume-ids)
          (ec2-param-filters filters)))))

  (define (create-volume client availability-zone . args)
    (let ([size (kw-ref args 'size: #f)]
          [snapshot-id (kw-ref args 'snapshot-id: #f)]
          [volume-type (kw-ref args 'volume-type: #f)]
          [iops (kw-ref args 'iops: #f)]
          [encrypted (kw-ref args 'encrypted: #f)]
          [tag-specifications (kw-ref args 'tag-specifications: #f)])
      (ec2-action/hash client "CreateVolume"
        (params-merge
          (list (cons "AvailabilityZone" availability-zone))
          (if size (list (cons "Size" size)) '())
          (if snapshot-id (list (cons "SnapshotId" snapshot-id)) '())
          (if volume-type (list (cons "VolumeType" volume-type)) '())
          (if iops (list (cons "Iops" iops)) '())
          (if encrypted (list (cons "Encrypted" "true")) '())
          (if tag-specifications (ec2-param-tags "volume" tag-specifications) '())))))

  (define (delete-volume client volume-id)
    (ec2-action client "DeleteVolume"
      (list (cons "VolumeId" volume-id)))
    (void))

  (define (attach-volume client volume-id instance-id device)
    (ec2-action/hash client "AttachVolume"
      (list (cons "VolumeId" volume-id)
            (cons "InstanceId" instance-id)
            (cons "Device" device))))

  (define (detach-volume client volume-id . args)
    (let ([instance-id (kw-ref args 'instance-id: #f)]
          [device (kw-ref args 'device: #f)]
          [force (kw-ref args 'force: #f)])
      (ec2-action/hash client "DetachVolume"
        (params-merge
          (list (cons "VolumeId" volume-id))
          (if instance-id (list (cons "InstanceId" instance-id)) '())
          (if device (list (cons "Device" device)) '())
          (if force (list (cons "Force" "true")) '())))))

  (define (kw-ref args key default)
    (let loop ([rest args])
      (cond
        [(null? rest) default]
        [(and (pair? (cdr rest)) (eq? (car rest) key))
         (cadr rest)]
        [else (loop (cdr rest))])))

  ) ;; end library
