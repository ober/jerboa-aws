#!chezscheme
;;; (jerboa-aws iam roles) -- IAM Role operations

(library (jerboa-aws iam roles)
  (export list-roles get-role create-role delete-role)
  (import (chezscheme)
          (jerboa-aws iam api))

  (define (list-roles client . args)
    (let ([path-prefix (kw-ref args 'path-prefix: #f)]
          [max-items (kw-ref args 'max-items: #f)]
          [marker (kw-ref args 'marker: #f)])
      (iam-action/hash client "ListRoles"
        (append
          (if path-prefix (list (cons "PathPrefix" path-prefix)) '())
          (if max-items (list (cons "MaxItems" max-items)) '())
          (if marker (list (cons "Marker" marker)) '())))))

  (define (get-role client role-name)
    (iam-action/hash client "GetRole"
      (list (cons "RoleName" role-name))))

  (define (create-role client role-name assume-role-policy-document . args)
    (let ([path (kw-ref args 'path: #f)]
          [description (kw-ref args 'description: #f)]
          [max-session-duration (kw-ref args 'max-session-duration: #f)])
      (iam-action/hash client "CreateRole"
        (append
          (list (cons "RoleName" role-name)
                (cons "AssumeRolePolicyDocument" assume-role-policy-document))
          (if path (list (cons "Path" path)) '())
          (if description (list (cons "Description" description)) '())
          (if max-session-duration
            (list (cons "MaxSessionDuration" max-session-duration))
            '())))))

  (define (delete-role client role-name)
    (iam-action client "DeleteRole"
      (list (cons "RoleName" role-name)))
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
