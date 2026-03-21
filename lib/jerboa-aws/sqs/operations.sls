#!chezscheme
;;; (jerboa-aws sqs operations) -- SQS operations

(library (jerboa-aws sqs operations)
  (export list-queues create-queue delete-queue
          send-message receive-message delete-message)
  (import (chezscheme)
          (jerboa-aws sqs api))

  (define (list-queues client . args)
    (let ([queue-name-prefix (kw-ref args 'queue-name-prefix: #f)]
          [max-results (kw-ref args 'max-results: #f)]
          [next-token (kw-ref args 'next-token: #f)])
      (let ([payload (make-hashtable string-hash string=?)])
        (when queue-name-prefix
          (hashtable-set! payload "QueueNamePrefix" queue-name-prefix))
        (when max-results
          (hashtable-set! payload "MaxResults" max-results))
        (when next-token
          (hashtable-set! payload "NextToken" next-token))
        (sqs-action client "ListQueues" payload))))

  (define (create-queue client queue-name . args)
    (let ([attributes (kw-ref args 'attributes: #f)]
          [tags (kw-ref args 'tags: #f)])
      (let ([payload (make-hashtable string-hash string=?)])
        (hashtable-set! payload "QueueName" queue-name)
        (when attributes
          (hashtable-set! payload "Attributes" attributes))
        (when tags
          (hashtable-set! payload "tags" tags))
        (sqs-action client "CreateQueue" payload))))

  (define (delete-queue client queue-url)
    (let ([payload (make-hashtable string-hash string=?)])
      (hashtable-set! payload "QueueUrl" queue-url)
      (sqs-action client "DeleteQueue" payload))
    (void))

  (define (send-message client queue-url message-body . args)
    (let ([delay-seconds (kw-ref args 'delay-seconds: #f)]
          [message-attributes (kw-ref args 'message-attributes: #f)]
          [message-group-id (kw-ref args 'message-group-id: #f)]
          [message-deduplication-id (kw-ref args 'message-deduplication-id: #f)])
      (let ([payload (make-hashtable string-hash string=?)])
        (hashtable-set! payload "QueueUrl" queue-url)
        (hashtable-set! payload "MessageBody" message-body)
        (when delay-seconds
          (hashtable-set! payload "DelaySeconds" delay-seconds))
        (when message-attributes
          (hashtable-set! payload "MessageAttributes" message-attributes))
        (when message-group-id
          (hashtable-set! payload "MessageGroupId" message-group-id))
        (when message-deduplication-id
          (hashtable-set! payload "MessageDeduplicationId" message-deduplication-id))
        (sqs-action client "SendMessage" payload))))

  (define (receive-message client queue-url . args)
    (let ([max-number-of-messages (kw-ref args 'max-number-of-messages: #f)]
          [wait-time-seconds (kw-ref args 'wait-time-seconds: #f)]
          [visibility-timeout (kw-ref args 'visibility-timeout: #f)]
          [attribute-names (kw-ref args 'attribute-names: #f)]
          [message-attribute-names (kw-ref args 'message-attribute-names: #f)])
      (let ([payload (make-hashtable string-hash string=?)])
        (hashtable-set! payload "QueueUrl" queue-url)
        (when max-number-of-messages
          (hashtable-set! payload "MaxNumberOfMessages" max-number-of-messages))
        (when wait-time-seconds
          (hashtable-set! payload "WaitTimeSeconds" wait-time-seconds))
        (when visibility-timeout
          (hashtable-set! payload "VisibilityTimeout" visibility-timeout))
        (when attribute-names
          (hashtable-set! payload "AttributeNames" (list->vector attribute-names)))
        (when message-attribute-names
          (hashtable-set! payload "MessageAttributeNames" (list->vector message-attribute-names)))
        (sqs-action client "ReceiveMessage" payload))))

  (define (delete-message client queue-url receipt-handle)
    (let ([payload (make-hashtable string-hash string=?)])
      (hashtable-set! payload "QueueUrl" queue-url)
      (hashtable-set! payload "ReceiptHandle" receipt-handle)
      (sqs-action client "DeleteMessage" payload))
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
