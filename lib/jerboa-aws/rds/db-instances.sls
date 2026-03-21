#!chezscheme
;;; (jerboa-aws rds db-instances) -- RDS DB Instance operations

(library (jerboa-aws rds db-instances)
  (export describe-db-instances create-db-instance
          delete-db-instance modify-db-instance)
  (import (chezscheme)
          (jerboa-aws rds api))

  (define (describe-db-instances client . args)
    (let ([db-instance-identifier (kw-ref args 'db-instance-identifier: #f)]
          [max-records (kw-ref args 'max-records: #f)]
          [marker (kw-ref args 'marker: #f)])
      (rds-action/hash client "DescribeDBInstances"
        (append
          (if db-instance-identifier
            (list (cons "DBInstanceIdentifier" db-instance-identifier))
            '())
          (if max-records
            (list (cons "MaxRecords"
                    (if (number? max-records)
                      (number->string max-records)
                      max-records)))
            '())
          (if marker (list (cons "Marker" marker)) '())))))

  (define (create-db-instance client db-instance-identifier . args)
    (let ([db-instance-class (kw-ref args 'db-instance-class: #f)]
          [engine (kw-ref args 'engine: #f)]
          [master-username (kw-ref args 'master-username: #f)]
          [master-user-password (kw-ref args 'master-user-password: #f)]
          [allocated-storage (kw-ref args 'allocated-storage: #f)]
          [db-name (kw-ref args 'db-name: #f)]
          [vpc-security-group-ids (kw-ref args 'vpc-security-group-ids: '())]
          [availability-zone (kw-ref args 'availability-zone: #f)]
          [multi-az (kw-ref args 'multi-az: #f)]
          [storage-type (kw-ref args 'storage-type: #f)])
      (rds-action/hash client "CreateDBInstance"
        (append
          (list (cons "DBInstanceIdentifier" db-instance-identifier))
          (if db-instance-class
            (list (cons "DBInstanceClass" db-instance-class)) '())
          (if engine (list (cons "Engine" engine)) '())
          (if master-username
            (list (cons "MasterUsername" master-username)) '())
          (if master-user-password
            (list (cons "MasterUserPassword" master-user-password)) '())
          (if allocated-storage
            (list (cons "AllocatedStorage"
                    (if (number? allocated-storage)
                      (number->string allocated-storage)
                      allocated-storage)))
            '())
          (if db-name (list (cons "DBName" db-name)) '())
          (let loop ([sgs vpc-security-group-ids] [i 1] [acc '()])
            (if (null? sgs)
              (reverse acc)
              (loop (cdr sgs) (+ i 1)
                    (cons (cons (string-append "VpcSecurityGroupIds.member."
                                  (number->string i))
                                (car sgs))
                          acc))))
          (if availability-zone
            (list (cons "AvailabilityZone" availability-zone)) '())
          (if multi-az
            (list (cons "MultiAZ" (if multi-az "true" "false"))) '())
          (if storage-type
            (list (cons "StorageType" storage-type)) '())))))

  (define (delete-db-instance client db-instance-identifier . args)
    (let ([skip-final-snapshot (kw-ref args 'skip-final-snapshot: #f)]
          [final-db-snapshot-identifier (kw-ref args 'final-db-snapshot-identifier: #f)])
      (rds-action/hash client "DeleteDBInstance"
        (append
          (list (cons "DBInstanceIdentifier" db-instance-identifier))
          (if skip-final-snapshot
            (list (cons "SkipFinalSnapshot" "true")) '())
          (if final-db-snapshot-identifier
            (list (cons "FinalDBSnapshotIdentifier" final-db-snapshot-identifier))
            '())))))

  (define (modify-db-instance client db-instance-identifier . args)
    (let ([db-instance-class (kw-ref args 'db-instance-class: #f)]
          [allocated-storage (kw-ref args 'allocated-storage: #f)]
          [master-user-password (kw-ref args 'master-user-password: #f)]
          [multi-az (kw-ref args 'multi-az: #f)]
          [apply-immediately (kw-ref args 'apply-immediately: #f)]
          [storage-type (kw-ref args 'storage-type: #f)])
      (rds-action/hash client "ModifyDBInstance"
        (append
          (list (cons "DBInstanceIdentifier" db-instance-identifier))
          (if db-instance-class
            (list (cons "DBInstanceClass" db-instance-class)) '())
          (if allocated-storage
            (list (cons "AllocatedStorage"
                    (if (number? allocated-storage)
                      (number->string allocated-storage)
                      allocated-storage)))
            '())
          (if master-user-password
            (list (cons "MasterUserPassword" master-user-password)) '())
          (if multi-az
            (list (cons "MultiAZ" (if multi-az "true" "false"))) '())
          (if apply-immediately
            (list (cons "ApplyImmediately" "true")) '())
          (if storage-type
            (list (cons "StorageType" storage-type)) '())))))

  ;; --- Helpers ---
  (define (kw-ref args key default)
    (let loop ([rest args])
      (cond
        [(null? rest) default]
        [(and (pair? (cdr rest)) (eq? (car rest) key))
         (cadr rest)]
        [else (loop (cdr rest))])))

  ) ;; end library
