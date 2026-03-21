#!chezscheme
;;; (jerboa-aws ec2 snapshots) -- EC2 EBS Snapshot operations

(library (jerboa-aws ec2 snapshots)
  (export describe-snapshots create-snapshot delete-snapshot copy-snapshot)
  (import (chezscheme)
          (jerboa-aws ec2 api)
          (jerboa-aws ec2 params))

  (define (describe-snapshots client . args)
    (let ([snapshot-ids (kw-ref args 'snapshot-ids: '())]
          [owner-ids (kw-ref args 'owner-ids: '())]
          [filters (kw-ref args 'filters: '())])
      (ec2-action/hash client "DescribeSnapshots"
        (params-merge
          (ec2-param-list "SnapshotId" snapshot-ids)
          (ec2-param-list "Owner" owner-ids)
          (ec2-param-filters filters)))))

  (define (create-snapshot client volume-id . args)
    (let ([description (kw-ref args 'description: #f)]
          [tag-specifications (kw-ref args 'tag-specifications: #f)])
      (ec2-action/hash client "CreateSnapshot"
        (params-merge
          (list (cons "VolumeId" volume-id))
          (if description (list (cons "Description" description)) '())
          (if tag-specifications (ec2-param-tags "snapshot" tag-specifications) '())))))

  (define (delete-snapshot client snapshot-id)
    (ec2-action client "DeleteSnapshot"
      (list (cons "SnapshotId" snapshot-id)))
    (void))

  (define (copy-snapshot client source-snapshot-id source-region . args)
    (let ([description (kw-ref args 'description: #f)]
          [encrypted (kw-ref args 'encrypted: #f)]
          [tag-specifications (kw-ref args 'tag-specifications: #f)])
      (ec2-action/hash client "CopySnapshot"
        (params-merge
          (list (cons "SourceSnapshotId" source-snapshot-id)
                (cons "SourceRegion" source-region))
          (if description (list (cons "Description" description)) '())
          (if encrypted (list (cons "Encrypted" "true")) '())
          (if tag-specifications (ec2-param-tags "snapshot" tag-specifications) '())))))

  (define (kw-ref args key default)
    (let loop ([rest args])
      (cond
        [(null? rest) default]
        [(and (pair? (cdr rest)) (eq? (car rest) key))
         (cadr rest)]
        [else (loop (cdr rest))])))

  ) ;; end library
