#!chezscheme
;;; (jerboa-aws iam policies) -- IAM Policy operations

(library (jerboa-aws iam policies)
  (export list-policies get-policy create-policy delete-policy
          attach-role-policy detach-role-policy
          attach-user-policy detach-user-policy)
  (import (chezscheme)
          (jerboa-aws iam api))

  (define (list-policies client . args)
    (let ([scope (kw-ref args 'scope: #f)]
          [only-attached (kw-ref args 'only-attached: #f)]
          [path-prefix (kw-ref args 'path-prefix: #f)]
          [max-items (kw-ref args 'max-items: #f)]
          [marker (kw-ref args 'marker: #f)])
      (iam-action/hash client "ListPolicies"
        (append
          (if scope (list (cons "Scope" scope)) '())
          (if only-attached (list (cons "OnlyAttached" "true")) '())
          (if path-prefix (list (cons "PathPrefix" path-prefix)) '())
          (if max-items (list (cons "MaxItems" max-items)) '())
          (if marker (list (cons "Marker" marker)) '())))))

  (define (get-policy client policy-arn)
    (iam-action/hash client "GetPolicy"
      (list (cons "PolicyArn" policy-arn))))

  (define (create-policy client policy-name policy-document . args)
    (let ([path (kw-ref args 'path: #f)]
          [description (kw-ref args 'description: #f)])
      (iam-action/hash client "CreatePolicy"
        (append
          (list (cons "PolicyName" policy-name)
                (cons "PolicyDocument" policy-document))
          (if path (list (cons "Path" path)) '())
          (if description (list (cons "Description" description)) '())))))

  (define (delete-policy client policy-arn)
    (iam-action client "DeletePolicy"
      (list (cons "PolicyArn" policy-arn)))
    (void))

  (define (attach-role-policy client role-name policy-arn)
    (iam-action client "AttachRolePolicy"
      (list (cons "RoleName" role-name)
            (cons "PolicyArn" policy-arn)))
    (void))

  (define (detach-role-policy client role-name policy-arn)
    (iam-action client "DetachRolePolicy"
      (list (cons "RoleName" role-name)
            (cons "PolicyArn" policy-arn)))
    (void))

  (define (attach-user-policy client user-name policy-arn)
    (iam-action client "AttachUserPolicy"
      (list (cons "UserName" user-name)
            (cons "PolicyArn" policy-arn)))
    (void))

  (define (detach-user-policy client user-name policy-arn)
    (iam-action client "DetachUserPolicy"
      (list (cons "UserName" user-name)
            (cons "PolicyArn" policy-arn)))
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
