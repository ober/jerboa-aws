#!chezscheme
;;; (jerboa-aws iam access-keys) -- IAM Access Key operations

(library (jerboa-aws iam access-keys)
  (export list-access-keys create-access-key delete-access-key update-access-key)
  (import (chezscheme)
          (jerboa-aws iam api))

  (define (list-access-keys client . args)
    (let ([user-name (kw-ref args 'user-name: #f)]
          [max-items (kw-ref args 'max-items: #f)]
          [marker (kw-ref args 'marker: #f)])
      (iam-action/hash client "ListAccessKeys"
        (append
          (if user-name (list (cons "UserName" user-name)) '())
          (if max-items (list (cons "MaxItems" max-items)) '())
          (if marker (list (cons "Marker" marker)) '())))))

  (define (create-access-key client . args)
    (let ([user-name (kw-ref args 'user-name: #f)])
      (iam-action/hash client "CreateAccessKey"
        (if user-name (list (cons "UserName" user-name)) '()))))

  (define (delete-access-key client access-key-id . args)
    (let ([user-name (kw-ref args 'user-name: #f)])
      (iam-action client "DeleteAccessKey"
        (append
          (list (cons "AccessKeyId" access-key-id))
          (if user-name (list (cons "UserName" user-name)) '())))
      (void)))

  (define (update-access-key client access-key-id status . args)
    (let ([user-name (kw-ref args 'user-name: #f)])
      (iam-action client "UpdateAccessKey"
        (append
          (list (cons "AccessKeyId" access-key-id)
                (cons "Status" status))
          (if user-name (list (cons "UserName" user-name)) '())))
      (void)))

  ;; --- Helpers ---
  (define (kw-ref args key default)
    (let loop ([rest args])
      (cond
        [(null? rest) default]
        [(and (pair? (cdr rest)) (eq? (car rest) key))
         (cadr rest)]
        [else (loop (cdr rest))])))

  ) ;; end library
