#!chezscheme
;;; (jerboa-aws iam groups) -- IAM Group operations

(library (jerboa-aws iam groups)
  (export list-groups get-group create-group delete-group)
  (import (chezscheme)
          (jerboa-aws iam api))

  (define (list-groups client . args)
    (let ([path-prefix (kw-ref args 'path-prefix: #f)]
          [max-items (kw-ref args 'max-items: #f)]
          [marker (kw-ref args 'marker: #f)])
      (iam-action/hash client "ListGroups"
        (append
          (if path-prefix (list (cons "PathPrefix" path-prefix)) '())
          (if max-items (list (cons "MaxItems" max-items)) '())
          (if marker (list (cons "Marker" marker)) '())))))

  (define (get-group client group-name . args)
    (let ([max-items (kw-ref args 'max-items: #f)]
          [marker (kw-ref args 'marker: #f)])
      (iam-action/hash client "GetGroup"
        (append
          (list (cons "GroupName" group-name))
          (if max-items (list (cons "MaxItems" max-items)) '())
          (if marker (list (cons "Marker" marker)) '())))))

  (define (create-group client group-name . args)
    (let ([path (kw-ref args 'path: #f)])
      (iam-action/hash client "CreateGroup"
        (append
          (list (cons "GroupName" group-name))
          (if path (list (cons "Path" path)) '())))))

  (define (delete-group client group-name)
    (iam-action client "DeleteGroup"
      (list (cons "GroupName" group-name)))
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
