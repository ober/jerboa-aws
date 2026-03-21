#!chezscheme
;;; (jerboa-aws ec2 key-pairs) -- EC2 Key Pair operations

(library (jerboa-aws ec2 key-pairs)
  (export describe-key-pairs create-key-pair delete-key-pair import-key-pair)
  (import (chezscheme)
          (jerboa-aws ec2 api)
          (jerboa-aws ec2 params))

  (define (describe-key-pairs client . args)
    (let ([key-names (kw-ref args 'key-names: '())]
          [key-pair-ids (kw-ref args 'key-pair-ids: '())]
          [filters (kw-ref args 'filters: '())])
      (ec2-action/hash client "DescribeKeyPairs"
        (params-merge
          (ec2-param-list "KeyName" key-names)
          (ec2-param-list "KeyPairId" key-pair-ids)
          (ec2-param-filters filters)))))

  (define (create-key-pair client key-name . args)
    (let ([key-type (kw-ref args 'key-type: #f)]
          [tag-specifications (kw-ref args 'tag-specifications: #f)])
      (ec2-action/hash client "CreateKeyPair"
        (params-merge
          (list (cons "KeyName" key-name))
          (if key-type (list (cons "KeyType" key-type)) '())
          (if tag-specifications (ec2-param-tags "key-pair" tag-specifications) '())))))

  (define (delete-key-pair client . args)
    (let ([key-name (kw-ref args 'key-name: #f)]
          [key-pair-id (kw-ref args 'key-pair-id: #f)])
      (ec2-action client "DeleteKeyPair"
        (params-merge
          (if key-name (list (cons "KeyName" key-name)) '())
          (if key-pair-id (list (cons "KeyPairId" key-pair-id)) '())))
      (void)))

  (define (import-key-pair client key-name public-key-material)
    (ec2-action/hash client "ImportKeyPair"
      (list (cons "KeyName" key-name)
            (cons "PublicKeyMaterial" public-key-material))))

  (define (kw-ref args key default)
    (let loop ([rest args])
      (cond
        [(null? rest) default]
        [(and (pair? (cdr rest)) (eq? (car rest) key))
         (cadr rest)]
        [else (loop (cdr rest))])))

  ) ;; end library
