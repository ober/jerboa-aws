#!chezscheme
;;; (jerboa-aws ec2 tags) -- EC2 Tag operations

(library (jerboa-aws ec2 tags)
  (export describe-tags create-tags delete-tags)
  (import (chezscheme)
          (jerboa-aws ec2 api)
          (jerboa-aws ec2 params))

  (define (describe-tags client . args)
    (let ([filters (kw-ref args 'filters: '())]
          [max-results (kw-ref args 'max-results: #f)]
          [next-token (kw-ref args 'next-token: #f)])
      (ec2-action/hash client "DescribeTags"
        (params-merge
          (ec2-param-filters filters)
          (if max-results (list (cons "MaxResults" max-results)) '())
          (if next-token (list (cons "NextToken" next-token)) '())))))

  ;; Create tags on one or more resources
  ;; resources: list of resource IDs
  ;; tags: alist of (key . value) pairs
  (define (create-tags client resources tags)
    (let ([tag-params
           (let loop ([ts tags] [i 1] [acc '()])
             (if (null? ts)
               (reverse acc)
               (let ([tag (car ts)])
                 (loop (cdr ts) (+ i 1)
                       (cons (cons (string-append "Tag." (number->string i) ".Value") (cdr tag))
                             (cons (cons (string-append "Tag." (number->string i) ".Key") (car tag))
                                   acc))))))])
      (ec2-action client "CreateTags"
        (params-merge
          (ec2-param-list "ResourceId" resources)
          tag-params))
      (void)))

  ;; Delete tags from one or more resources
  ;; resources: list of resource IDs
  ;; tags: alist of (key . value) pairs (value can be "" to match any)
  (define (delete-tags client resources tags)
    (let ([tag-params
           (let loop ([ts tags] [i 1] [acc '()])
             (if (null? ts)
               (reverse acc)
               (let ([tag (car ts)])
                 (loop (cdr ts) (+ i 1)
                       (cons (cons (string-append "Tag." (number->string i) ".Value") (cdr tag))
                             (cons (cons (string-append "Tag." (number->string i) ".Key") (car tag))
                                   acc))))))])
      (ec2-action client "DeleteTags"
        (params-merge
          (ec2-param-list "ResourceId" resources)
          tag-params))
      (void)))

  (define (kw-ref args key default)
    (let loop ([rest args])
      (cond
        [(null? rest) default]
        [(and (pair? (cdr rest)) (eq? (car rest) key))
         (cadr rest)]
        [else (loop (cdr rest))])))

  ) ;; end library
