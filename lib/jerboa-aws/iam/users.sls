#!chezscheme
;;; (jerboa-aws iam users) -- IAM User operations

(library (jerboa-aws iam users)
  (export list-users get-user create-user delete-user update-user)
  (import (chezscheme)
          (jerboa-aws iam api))

  (define (list-users client . args)
    (let ([path-prefix (kw-ref args 'path-prefix: #f)]
          [max-items (kw-ref args 'max-items: #f)]
          [marker (kw-ref args 'marker: #f)])
      (iam-action/hash client "ListUsers"
        (append
          (if path-prefix (list (cons "PathPrefix" path-prefix)) '())
          (if max-items (list (cons "MaxItems" max-items)) '())
          (if marker (list (cons "Marker" marker)) '())))))

  (define (get-user client . args)
    (let ([user-name (kw-ref args 'user-name: #f)])
      (iam-action/hash client "GetUser"
        (if user-name (list (cons "UserName" user-name)) '()))))

  (define (create-user client user-name . args)
    (let ([path (kw-ref args 'path: #f)])
      (iam-action/hash client "CreateUser"
        (append
          (list (cons "UserName" user-name))
          (if path (list (cons "Path" path)) '())))))

  (define (delete-user client user-name)
    (iam-action client "DeleteUser"
      (list (cons "UserName" user-name)))
    (void))

  (define (update-user client user-name . args)
    (let ([new-user-name (kw-ref args 'new-user-name: #f)]
          [new-path (kw-ref args 'new-path: #f)])
      (iam-action client "UpdateUser"
        (append
          (list (cons "UserName" user-name))
          (if new-user-name (list (cons "NewUserName" new-user-name)) '())
          (if new-path (list (cons "NewPath" new-path)) '())))
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
