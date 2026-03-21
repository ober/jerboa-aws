#!chezscheme
;;; (jerboa-aws dynamodb operations) -- DynamoDB operations

(library (jerboa-aws dynamodb operations)
  (export list-tables describe-table create-table delete-table
          put-item get-item delete-item
          dynamodb-query dynamodb-scan)
  (import (chezscheme)
          (jerboa-aws dynamodb api))

  (define (list-tables client . args)
    (let ([limit (kw-ref args 'limit: #f)]
          [exclusive-start (kw-ref args 'exclusive-start-table-name: #f)])
      (let ([payload (make-hashtable string-hash string=?)])
        (when limit (hashtable-set! payload "Limit" limit))
        (when exclusive-start
          (hashtable-set! payload "ExclusiveStartTableName" exclusive-start))
        (dynamodb-action client "ListTables" payload))))

  (define (describe-table client table-name)
    (let ([payload (make-hashtable string-hash string=?)])
      (hashtable-set! payload "TableName" table-name)
      (dynamodb-action client "DescribeTable" payload)))

  (define (create-table client table-name . args)
    (let ([attribute-definitions (kw-ref args 'attribute-definitions: #f)]
          [key-schema (kw-ref args 'key-schema: #f)]
          [billing-mode (kw-ref args 'billing-mode: "PAY_PER_REQUEST")]
          [provisioned-throughput (kw-ref args 'provisioned-throughput: #f)])
      (let ([payload (make-hashtable string-hash string=?)])
        (hashtable-set! payload "TableName" table-name)
        (when attribute-definitions
          (hashtable-set! payload "AttributeDefinitions" attribute-definitions))
        (when key-schema
          (hashtable-set! payload "KeySchema" key-schema))
        (hashtable-set! payload "BillingMode" billing-mode)
        (when provisioned-throughput
          (hashtable-set! payload "ProvisionedThroughput" provisioned-throughput))
        (dynamodb-action client "CreateTable" payload))))

  (define (delete-table client table-name)
    (let ([payload (make-hashtable string-hash string=?)])
      (hashtable-set! payload "TableName" table-name)
      (dynamodb-action client "DeleteTable" payload)))

  (define (put-item client table-name item . args)
    (let ([condition-expression (kw-ref args 'condition-expression: #f)]
          [expression-attribute-names (kw-ref args 'expression-attribute-names: #f)]
          [expression-attribute-values (kw-ref args 'expression-attribute-values: #f)]
          [return-values (kw-ref args 'return-values: #f)])
      (let ([payload (make-hashtable string-hash string=?)])
        (hashtable-set! payload "TableName" table-name)
        (hashtable-set! payload "Item" item)
        (when condition-expression
          (hashtable-set! payload "ConditionExpression" condition-expression))
        (when expression-attribute-names
          (hashtable-set! payload "ExpressionAttributeNames" expression-attribute-names))
        (when expression-attribute-values
          (hashtable-set! payload "ExpressionAttributeValues" expression-attribute-values))
        (when return-values
          (hashtable-set! payload "ReturnValues" return-values))
        (dynamodb-action client "PutItem" payload))))

  (define (get-item client table-name key . args)
    (let ([consistent-read (kw-ref args 'consistent-read: #f)]
          [projection-expression (kw-ref args 'projection-expression: #f)]
          [expression-attribute-names (kw-ref args 'expression-attribute-names: #f)])
      (let ([payload (make-hashtable string-hash string=?)])
        (hashtable-set! payload "TableName" table-name)
        (hashtable-set! payload "Key" key)
        (when consistent-read
          (hashtable-set! payload "ConsistentRead" consistent-read))
        (when projection-expression
          (hashtable-set! payload "ProjectionExpression" projection-expression))
        (when expression-attribute-names
          (hashtable-set! payload "ExpressionAttributeNames" expression-attribute-names))
        (dynamodb-action client "GetItem" payload))))

  (define (delete-item client table-name key . args)
    (let ([condition-expression (kw-ref args 'condition-expression: #f)]
          [expression-attribute-names (kw-ref args 'expression-attribute-names: #f)]
          [expression-attribute-values (kw-ref args 'expression-attribute-values: #f)]
          [return-values (kw-ref args 'return-values: #f)])
      (let ([payload (make-hashtable string-hash string=?)])
        (hashtable-set! payload "TableName" table-name)
        (hashtable-set! payload "Key" key)
        (when condition-expression
          (hashtable-set! payload "ConditionExpression" condition-expression))
        (when expression-attribute-names
          (hashtable-set! payload "ExpressionAttributeNames" expression-attribute-names))
        (when expression-attribute-values
          (hashtable-set! payload "ExpressionAttributeValues" expression-attribute-values))
        (when return-values
          (hashtable-set! payload "ReturnValues" return-values))
        (dynamodb-action client "DeleteItem" payload))))

  (define (dynamodb-query client table-name . args)
    (let ([key-condition-expression (kw-ref args 'key-condition-expression: #f)]
          [filter-expression (kw-ref args 'filter-expression: #f)]
          [projection-expression (kw-ref args 'projection-expression: #f)]
          [expression-attribute-names (kw-ref args 'expression-attribute-names: #f)]
          [expression-attribute-values (kw-ref args 'expression-attribute-values: #f)]
          [index-name (kw-ref args 'index-name: #f)]
          [scan-index-forward (kw-ref args 'scan-index-forward: #f)]
          [limit (kw-ref args 'limit: #f)]
          [consistent-read (kw-ref args 'consistent-read: #f)]
          [exclusive-start-key (kw-ref args 'exclusive-start-key: #f)])
      (let ([payload (make-hashtable string-hash string=?)])
        (hashtable-set! payload "TableName" table-name)
        (when key-condition-expression
          (hashtable-set! payload "KeyConditionExpression" key-condition-expression))
        (when filter-expression
          (hashtable-set! payload "FilterExpression" filter-expression))
        (when projection-expression
          (hashtable-set! payload "ProjectionExpression" projection-expression))
        (when expression-attribute-names
          (hashtable-set! payload "ExpressionAttributeNames" expression-attribute-names))
        (when expression-attribute-values
          (hashtable-set! payload "ExpressionAttributeValues" expression-attribute-values))
        (when index-name
          (hashtable-set! payload "IndexName" index-name))
        (when scan-index-forward
          (hashtable-set! payload "ScanIndexForward" scan-index-forward))
        (when limit
          (hashtable-set! payload "Limit" limit))
        (when consistent-read
          (hashtable-set! payload "ConsistentRead" consistent-read))
        (when exclusive-start-key
          (hashtable-set! payload "ExclusiveStartKey" exclusive-start-key))
        (dynamodb-action client "Query" payload))))

  (define (dynamodb-scan client table-name . args)
    (let ([filter-expression (kw-ref args 'filter-expression: #f)]
          [projection-expression (kw-ref args 'projection-expression: #f)]
          [expression-attribute-names (kw-ref args 'expression-attribute-names: #f)]
          [expression-attribute-values (kw-ref args 'expression-attribute-values: #f)]
          [index-name (kw-ref args 'index-name: #f)]
          [limit (kw-ref args 'limit: #f)]
          [consistent-read (kw-ref args 'consistent-read: #f)]
          [exclusive-start-key (kw-ref args 'exclusive-start-key: #f)])
      (let ([payload (make-hashtable string-hash string=?)])
        (hashtable-set! payload "TableName" table-name)
        (when filter-expression
          (hashtable-set! payload "FilterExpression" filter-expression))
        (when projection-expression
          (hashtable-set! payload "ProjectionExpression" projection-expression))
        (when expression-attribute-names
          (hashtable-set! payload "ExpressionAttributeNames" expression-attribute-names))
        (when expression-attribute-values
          (hashtable-set! payload "ExpressionAttributeValues" expression-attribute-values))
        (when index-name
          (hashtable-set! payload "IndexName" index-name))
        (when limit
          (hashtable-set! payload "Limit" limit))
        (when consistent-read
          (hashtable-set! payload "ConsistentRead" consistent-read))
        (when exclusive-start-key
          (hashtable-set! payload "ExclusiveStartKey" exclusive-start-key))
        (dynamodb-action client "Scan" payload))))

  ;; --- Helpers ---
  (define (kw-ref args key default)
    (let loop ([rest args])
      (cond
        [(null? rest) default]
        [(and (pair? (cdr rest)) (eq? (car rest) key))
         (cadr rest)]
        [else (loop (cdr rest))])))

  ) ;; end library
