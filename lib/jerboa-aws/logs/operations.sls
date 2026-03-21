#!chezscheme
;;; (jerboa-aws logs operations) -- CloudWatch Logs operations

(library (jerboa-aws logs operations)
  (export describe-log-groups create-log-group delete-log-group
          describe-log-streams create-log-stream
          put-log-events get-log-events filter-log-events)
  (import (chezscheme)
          (jerboa-aws logs api))

  (define (describe-log-groups client . args)
    (let ([log-group-name-prefix (kw-ref args 'log-group-name-prefix: #f)]
          [limit (kw-ref args 'limit: #f)]
          [next-token (kw-ref args 'next-token: #f)])
      (let ([payload (make-hashtable string-hash string=?)])
        (when log-group-name-prefix
          (hashtable-set! payload "logGroupNamePrefix" log-group-name-prefix))
        (when limit (hashtable-set! payload "limit" limit))
        (when next-token (hashtable-set! payload "nextToken" next-token))
        (logs-action client "DescribeLogGroups" payload))))

  (define (create-log-group client log-group-name . args)
    (let ([tags (kw-ref args 'tags: #f)])
      (let ([payload (make-hashtable string-hash string=?)])
        (hashtable-set! payload "logGroupName" log-group-name)
        (when tags (hashtable-set! payload "tags" tags))
        (logs-action client "CreateLogGroup" payload))
      (void)))

  (define (delete-log-group client log-group-name)
    (let ([payload (make-hashtable string-hash string=?)])
      (hashtable-set! payload "logGroupName" log-group-name)
      (logs-action client "DeleteLogGroup" payload))
    (void))

  (define (describe-log-streams client log-group-name . args)
    (let ([log-stream-name-prefix (kw-ref args 'log-stream-name-prefix: #f)]
          [order-by (kw-ref args 'order-by: #f)]
          [descending (kw-ref args 'descending: #f)]
          [limit (kw-ref args 'limit: #f)]
          [next-token (kw-ref args 'next-token: #f)])
      (let ([payload (make-hashtable string-hash string=?)])
        (hashtable-set! payload "logGroupName" log-group-name)
        (when log-stream-name-prefix
          (hashtable-set! payload "logStreamNamePrefix" log-stream-name-prefix))
        (when order-by (hashtable-set! payload "orderBy" order-by))
        (when descending (hashtable-set! payload "descending" descending))
        (when limit (hashtable-set! payload "limit" limit))
        (when next-token (hashtable-set! payload "nextToken" next-token))
        (logs-action client "DescribeLogStreams" payload))))

  (define (create-log-stream client log-group-name log-stream-name)
    (let ([payload (make-hashtable string-hash string=?)])
      (hashtable-set! payload "logGroupName" log-group-name)
      (hashtable-set! payload "logStreamName" log-stream-name)
      (logs-action client "CreateLogStream" payload))
    (void))

  (define (put-log-events client log-group-name log-stream-name events . args)
    (let ([sequence-token (kw-ref args 'sequence-token: #f)])
      (let ([payload (make-hashtable string-hash string=?)])
        (hashtable-set! payload "logGroupName" log-group-name)
        (hashtable-set! payload "logStreamName" log-stream-name)
        (hashtable-set! payload "logEvents" (list->vector events))
        (when sequence-token
          (hashtable-set! payload "sequenceToken" sequence-token))
        (logs-action client "PutLogEvents" payload))))

  (define (get-log-events client log-group-name log-stream-name . args)
    (let ([start-time (kw-ref args 'start-time: #f)]
          [end-time (kw-ref args 'end-time: #f)]
          [limit (kw-ref args 'limit: #f)]
          [next-token (kw-ref args 'next-token: #f)]
          [start-from-head (kw-ref args 'start-from-head: #f)])
      (let ([payload (make-hashtable string-hash string=?)])
        (hashtable-set! payload "logGroupName" log-group-name)
        (hashtable-set! payload "logStreamName" log-stream-name)
        (when start-time (hashtable-set! payload "startTime" start-time))
        (when end-time (hashtable-set! payload "endTime" end-time))
        (when limit (hashtable-set! payload "limit" limit))
        (when next-token (hashtable-set! payload "nextToken" next-token))
        (when start-from-head (hashtable-set! payload "startFromHead" start-from-head))
        (logs-action client "GetLogEvents" payload))))

  (define (filter-log-events client log-group-name . args)
    (let ([log-stream-names (kw-ref args 'log-stream-names: #f)]
          [filter-pattern (kw-ref args 'filter-pattern: #f)]
          [start-time (kw-ref args 'start-time: #f)]
          [end-time (kw-ref args 'end-time: #f)]
          [limit (kw-ref args 'limit: #f)]
          [next-token (kw-ref args 'next-token: #f)])
      (let ([payload (make-hashtable string-hash string=?)])
        (hashtable-set! payload "logGroupName" log-group-name)
        (when log-stream-names
          (hashtable-set! payload "logStreamNames" (list->vector log-stream-names)))
        (when filter-pattern
          (hashtable-set! payload "filterPattern" filter-pattern))
        (when start-time (hashtable-set! payload "startTime" start-time))
        (when end-time (hashtable-set! payload "endTime" end-time))
        (when limit (hashtable-set! payload "limit" limit))
        (when next-token (hashtable-set! payload "nextToken" next-token))
        (logs-action client "FilterLogEvents" payload))))

  ;; --- Helpers ---
  (define (kw-ref args key default)
    (let loop ([rest args])
      (cond
        [(null? rest) default]
        [(and (pair? (cdr rest)) (eq? (car rest) key))
         (cadr rest)]
        [else (loop (cdr rest))])))

  ) ;; end library
